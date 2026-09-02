# Roster-aware replacement and PORP (points over replacement per week).
# Value uses a 50/50 blend of raw PORP and a locally smoothed PORP curve.
# Floor / Ceiling stay on raw source-scenario PORP so the range stays wide.

n_starters <- function(pos, settings) {
  flex_share <- list(QB = 0, RB = 0.45, WR = 0.45, TE = 0.10)
  bench_share <- list(QB = 0.15, RB = 0.40, WR = 0.35, TE = 0.10)
  
  base <- switch(pos,
                 QB = settings$qb,
                 RB = settings$rb,
                 WR = settings$wr,
                 TE = settings$te,
                 0
  )
  
  if (pos == "QB" && isTRUE(settings$superflex)) {
    base <- base + 1
  }
  
  starters_league <- settings$teams * base
  flex_league <- settings$teams * settings$flex * flex_share[[pos]]
  bench_league <- settings$teams * settings$bench * bench_share[[pos]] * 0.5
  
  as.integer(round(starters_league + flex_league + bench_league))
}

replacement_points <- function(scored, settings) {
  positions <- c("QB", "RB", "WR", "TE")
  purrr::map_dfr(positions, function(p) {
    n <- max(n_starters(p, settings), 1)
    pos_df <- scored |>
      dplyr::filter(.data$pos == p) |>
      dplyr::arrange(dplyr::desc(.data$points))
    if (nrow(pos_df) == 0) {
      return(dplyr::tibble(pos = p, repl_pts = 0, repl_n = n))
    }
    idx <- min(n, nrow(pos_df))
    repl <- pos_df$points[idx]
    if (!is.finite(repl)) repl <- 0
    dplyr::tibble(pos = p, repl_pts = repl, repl_n = n)
  })
}

adp_implied_porp <- function(adp, max_porp) {
  adp <- dplyr::if_else(is.na(adp) | adp <= 0, 160, adp)
  raw <- pmax(0, (160 - adp) / 160)
  raw * max_porp
}

clean_num <- function(x) {
  x <- tidyr::replace_na(suppressWarnings(as.numeric(x)), 0)
  x[!is.finite(x)] <- 0
  x
}

local_smooth_curve <- function(y) {
  y <- clean_num(y)
  n <- length(y)
  # Tiny groups (FB, small TE lists, etc.) cannot support loess.
  if (n < 12) {
    if (n < 3) return(y)
    half <- 1L
    return(vapply(seq_len(n), function(i) mean(y[max(1, i - half):min(n, i + half)]), numeric(1)))
  }
  idx <- seq_len(n)
  span <- max(0.4, 12 / n)
  fit <- tryCatch(
    suppressWarnings(stats::loess(y ~ idx, span = span, degree = 1, surface = "direct")),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    half <- 2L
    return(vapply(idx, function(i) mean(y[max(1, i - half):min(n, i + half)]), numeric(1)))
  }
  pred <- as.numeric(stats::predict(fit, idx))
  bad <- !is.finite(pred)
  pred[bad] <- y[bad]
  pred
}

blend_raw_smooth <- function(raw) {
  raw <- clean_num(raw)
  0.5 * raw + 0.5 * local_smooth_curve(raw)
}

rank_players <- function(stats, settings, scoring, lambda = 0.35, weeks = 17) {
  scored <- score_players(stats, scoring)
  scored$pos <- toupper(trimws(as.character(scored$pos)))
  scored$pos <- dplyr::recode(scored$pos, FB = "RB", HB = "RB", TB = "RB", .default = scored$pos)
  if (!"floor_mult" %in% names(scored)) scored$floor_mult <- 0.70
  if (!"ceiling_mult" %in% names(scored)) scored$ceiling_mult <- 1.35
  
  if (!"adp" %in% names(scored)) {
    scored$adp <- NA_real_
  } else {
    scored$adp <- suppressWarnings(as.numeric(scored$adp))
  }
  repl <- replacement_points(scored, settings)
  
  out <- scored |>
    dplyr::left_join(repl, by = "pos") |>
    dplyr::mutate(
      repl_pts = tidyr::replace_na(repl_pts, 0),
      floor_mult_x = pmin(ifelse(is.finite(floor_mult) & floor_mult > 0, floor_mult, 0.70), 0.70),
      ceiling_mult_x = pmax(ifelse(is.finite(ceiling_mult) & ceiling_mult > 0, ceiling_mult, 1.35), 1.35),
      porp_season = points - repl_pts,
      porp_week = porp_season / weeks,
      floor_week = (points * floor_mult_x - repl_pts) / weeks,
      ceiling_week = (points * ceiling_mult_x - repl_pts) / weeks
    ) |>
    dplyr::group_by(pos) |>
    dplyr::arrange(dplyr::desc(.data$porp_week), .by_group = TRUE) |>
    dplyr::mutate(porp_blend = blend_raw_smooth(.data$porp_week)) |>
    dplyr::ungroup()
  
  max_porp <- suppressWarnings(max(out$porp_blend, na.rm = TRUE))
  if (!is.finite(max_porp) || max_porp <= 0) max_porp <- 1
  
  out |>
    dplyr::mutate(
      adp_porp = adp_implied_porp(adp, max_porp),
      value = (1 - lambda) * tidyr::replace_na(porp_blend, 0) + lambda * adp_porp,
      floor = dplyr::coalesce(floor_week, value),
      ceiling = dplyr::coalesce(ceiling_week, value)
    ) |>
    dplyr::group_by(pos) |>
    dplyr::mutate(
      pos_rank_n = dplyr::dense_rank(dplyr::desc(value)),
      pos_rank = paste0(pos, pos_rank_n)
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(value)) |>
    dplyr::mutate(rank = dplyr::row_number()) |>
    dplyr::select(
      rank, pos, player, team, adp,
      floor, value, ceiling, pos_rank,
      points, porp_week
    )
}