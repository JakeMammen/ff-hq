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

score_players <- function(stats, scoring) {
  dplyr::mutate(
    stats,
    points =
      pass_yds * scoring$pass_yd +
      pass_td * scoring$pass_td +
      pass_int * scoring$pass_int +
      rush_yds * scoring$rush_yd +
      rush_td * scoring$rush_td +
      rec * scoring$rec +
      rec_yds * scoring$rec_yd +
      rec_td * scoring$rec_td +
      fumbles * scoring$fumble,
    floor_pts = points * floor_mult,
    ceiling_pts = points * ceiling_mult
  )
}