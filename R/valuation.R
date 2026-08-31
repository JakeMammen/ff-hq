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
    dplyr::tibble(pos = p, repl_pts = pos_df$points[idx], repl_n = n)
  })
}

adp_implied_porp <- function(adp, max_porp) {
  adp <- dplyr::if_else(is.na(adp) | adp <= 0, 160, adp)
  raw <- pmax(0, (160 - adp) / 160)
  raw * max_porp
}

rank_players <- function(stats, settings, scoring, lambda = 0.35, weeks = 17) {
  scored <- score_players(stats, scoring)
  repl <- replacement_points(scored, settings)
  
  out <- scored |>
    dplyr::left_join(repl, by = "pos") |>
    dplyr::mutate(
      porp_season = points - repl_pts,
      porp_week = porp_season / weeks,
      floor_week = (floor_pts - repl_pts) / weeks,
      ceiling_week = (ceiling_pts - repl_pts) / weeks
    )
  
  max_porp <- max(out$porp_week, na.rm = TRUE)
  out |>
    dplyr::mutate(
      adp_porp = adp_implied_porp(adp, max_porp),
      value = (1 - lambda) * porp_week + lambda * adp_porp,
      floor = floor_week,
      ceiling = ceiling_week
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