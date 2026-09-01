preset_scoring <- function(ppr_type = c("PPR", "Half PPR", "Standard")) {
  ppr_type <- match.arg(ppr_type)
  rec <- switch(ppr_type,
                "PPR" = 1,
                "Half PPR" = 0.5,
                "Standard" = 0
  )
  list(
    pass_yd = 0.04,
    pass_td = 4,
    pass_int = -2,
    rush_yd = 0.1,
    rush_td = 6,
    rec = rec,
    rec_yd = 0.1,
    rec_td = 6,
    fumble = -2
  )
}

nz <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.finite(x), x, 0)
}

score_players <- function(stats, scoring) {
  dplyr::mutate(
    stats,
    points =
      nz(pass_yds) * scoring$pass_yd +
      nz(pass_td) * scoring$pass_td +
      nz(pass_int) * scoring$pass_int +
      nz(rush_yds) * scoring$rush_yd +
      nz(rush_td) * scoring$rush_td +
      nz(rec) * scoring$rec +
      nz(rec_yds) * scoring$rec_yd +
      nz(rec_td) * scoring$rec_td +
      nz(fumbles) * scoring$fumble,
    floor_pts = points * ifelse(is.finite(floor_mult) & floor_mult > 0, floor_mult, 0.8),
    ceiling_pts = points * ifelse(is.finite(ceiling_mult) & ceiling_mult > 0, ceiling_mult, 1.1)
  )
}