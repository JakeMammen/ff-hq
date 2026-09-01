roster_spots_per_team <- function(settings) {
  qb <- settings$qb + as.integer(isTRUE(settings$superflex))
  qb + settings$rb + settings$wr + settings$te + settings$flex + settings$bench
}

dollarize <- function(weights, pot, total_w) {
  w <- pmax(tidyr::replace_na(as.numeric(weights), 0), 0)
  w[!is.finite(w)] <- 0
  out <- if (!is.finite(total_w) || total_w <= 0) {
    rep(1, length(w))
  } else {
    1 + pot * (w / total_w)
  }
  out[!is.finite(out)] <- 1
  pmax(1, round(out, 0))
}

auction_dollars <- function(available, settings, keeper_spend = 0, n_keepers = 0) {
  if (nrow(available) == 0) {
    return(dplyr::mutate(
      available,
      dollars = numeric(0),
      floor_dollars = numeric(0),
      ceiling_dollars = numeric(0)
    ))
  }
  
  budget <- ifelse(is.finite(settings$budget), settings$budget, 200)
  keeper_spend <- ifelse(is.finite(keeper_spend), keeper_spend, 0)
  n_keepers <- ifelse(is.finite(n_keepers), n_keepers, 0)
  
  spots_league <- settings$teams * roster_spots_per_team(settings)
  remaining_spots <- max(spots_league - n_keepers, 1)
  league_cap <- settings$teams * budget - keeper_spend
  pot <- max(league_cap - remaining_spots, 0)
  if (!is.finite(pot) || pot < 0) pot <- 0
  
  value_w <- pmax(tidyr::replace_na(available$porp_week, 0), 0)
  value_w[!is.finite(value_w)] <- 0
  total_w <- sum(value_w, na.rm = TRUE)
  
  dplyr::mutate(
    available,
    dollars = dollarize(porp_week, pot, total_w),
    floor_dollars = dollarize(floor, pot, total_w),
    ceiling_dollars = dollarize(ceiling, pot, total_w)
  )
}