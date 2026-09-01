# Refresh crowd projections from ffanalytics.
# Run from the project root on your laptop, not inside the Shiny app.
#
#   setwd("~/Documents/ff-hq")
#   source("scripts/refresh_projections.R")
#
# Sources: ESPN, CBS, FantasyPros, FantasySharks
# ADP sources skip NFL because that scrape is currently broken.

ff_sources <- c("ESPN", "CBS", "FantasyPros")
ff_positions <- c("QB", "RB", "WR", "TE")

library(ffanalytics)
library(dplyr)
library(readr)
library(tidyr)

coalesce_num <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) {
    return(rep(0, nrow(df)))
  }
  vals <- suppressWarnings(as.numeric(gsub(",", "", as.character(df[[hit[[1]]]]))))
  ifelse(is.finite(vals), vals, 0)
}

coalesce_chr <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) {
    return(rep(NA_character_, nrow(df)))
  }
  out <- trimws(as.character(df[[hit[[1]]]]))
  out[out %in% c("", "NA", "NULL")] <- NA_character_
  out
}

normalize_source <- function(df, pos_scraped) {
  names(df) <- gsub("[^A-Za-z0-9_]+", "_", names(df))
  names(df) <- tolower(names(df))
  tibble(
    player = coalesce_chr(df, c("player", "player_name", "name")),
    pos = toupper(dplyr::coalesce(coalesce_chr(df, c("pos", "position")), pos_scraped)),
    team = toupper(coalesce_chr(df, c("team", "teamabbr", "nfl_team"))),
    data_src = coalesce_chr(df, c("data_src", "src")),
    pass_yds = coalesce_num(df, c("pass_yds", "passing_yds", "pass_yards")),
    pass_td = coalesce_num(df, c("pass_td", "pass_tds", "passing_td", "passing_tds")),
    pass_int = coalesce_num(df, c("pass_int", "pass_ints", "passing_int", "ints")),
    rush_yds = coalesce_num(df, c("rush_yds", "rushing_yds", "rush_yards")),
    rush_td = coalesce_num(df, c("rush_td", "rush_tds", "rushing_td", "rushing_tds")),
    rec = coalesce_num(df, c("rec", "recs", "receptions")),
    rec_yds = coalesce_num(df, c("rec_yds", "receiving_yds", "rec_yards")),
    rec_td = coalesce_num(df, c("rec_td", "rec_tds", "receiving_td", "receiving_tds")),
    fumbles = coalesce_num(df, c("fumbles", "fumbles_lost", "fum", "fl"))
  )
}

message("Scraping ", paste(ff_sources, collapse = ", "), " ...")
raw <- scrape_data(
  src = ff_sources,
  pos = ff_positions,
  season = NULL,
  week = 0
)

source_rows <- bind_rows(
  lapply(names(raw), function(nm) normalize_source(raw[[nm]], toupper(nm)))
) |>
  filter(!is.na(player), player != "") |>
  mutate(
    player = trimws(player),
    src_pts = 0.04 * pass_yds + 4 * pass_td - 2 * pass_int +
      0.1 * rush_yds + 6 * rush_td +
      1 * rec + 0.1 * rec_yds + 6 * rec_td -
      2 * fumbles
  )

finite_min <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  min(x)
}

finite_max <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  max(x)
}

agg <- source_rows |>
  group_by(player, pos) |>
  summarise(
    team = dplyr::first(na.omit(team[team != ""]), default = NA_character_),
    n_src = dplyr::n_distinct(na.omit(data_src)),
    pass_yds = mean(pass_yds, na.rm = TRUE),
    pass_td = mean(pass_td, na.rm = TRUE),
    pass_int = mean(pass_int, na.rm = TRUE),
    rush_yds = mean(rush_yds, na.rm = TRUE),
    rush_td = mean(rush_td, na.rm = TRUE),
    rec = mean(rec, na.rm = TRUE),
    rec_yds = mean(rec_yds, na.rm = TRUE),
    rec_td = mean(rec_td, na.rm = TRUE),
    fumbles = mean(fumbles, na.rm = TRUE),
    mean_pts = mean(src_pts[is.finite(src_pts)], na.rm = TRUE),
    min_pts = finite_min(src_pts),
    max_pts = finite_max(src_pts),
    .groups = "drop"
  ) |>
  mutate(
    mean_pts = ifelse(is.finite(mean_pts), mean_pts, 0),
    min_pts = ifelse(is.finite(min_pts), min_pts, mean_pts),
    max_pts = ifelse(is.finite(max_pts), max_pts, mean_pts),
    floor_mult = ifelse(mean_pts > 0, pmin(pmax(min_pts / mean_pts, 0.5), 1), 0.8),
    ceiling_mult = ifelse(mean_pts > 0, pmin(pmax(max_pts / mean_pts, 1), 1.5), 1.1),
    adp = NA_real_
  )

pt <- tryCatch(
  {
    projections_table(raw, avg_type = "average") |>
      add_player_info()
  },
  error = function(e) {
    message("projections_table() failed: ", e$message)
    NULL
  }
)

if (!is.null(pt) && all(c("player", "pos") %in% names(pt))) {
  pt_small <- pt |>
    group_by(player, pos) |>
    summarise(
      points = mean(points, na.rm = TRUE),
      floor_pts = mean(floor, na.rm = TRUE),
      ceiling_pts = mean(ceiling, na.rm = TRUE),
      .groups = "drop"
    )
  agg <- agg |>
    left_join(pt_small, by = c("player", "pos")) |>
    mutate(
      floor_mult = case_when(
        is.finite(points) & points > 0 & is.finite(floor_pts) ~ pmin(pmax(floor_pts / points, 0.5), 1),
        TRUE ~ floor_mult
      ),
      ceiling_mult = case_when(
        is.finite(points) & points > 0 & is.finite(ceiling_pts) ~ pmin(pmax(ceiling_pts / points, 1), 1.5),
        TRUE ~ ceiling_mult
      )
    ) |>
    select(-any_of(c("points", "floor_pts", "ceiling_pts")))
}

# Skip NFL ADP. That source is failing.
if (!is.null(pt)) {
  pt_adp <- tryCatch(
    add_adp(pt, sources = c("CBS", "FFC", "MFL", "RTS")),
    error = function(e) {
      message("add_adp() failed: ", e$message)
      pt
    }
  )
  if ("adp" %in% names(pt_adp) && "player" %in% names(pt_adp)) {
    adp_map <- pt_adp |>
      group_by(player, pos) |>
      summarise(adp_new = mean(adp, na.rm = TRUE), .groups = "drop")
    agg <- agg |>
      left_join(adp_map, by = c("player", "pos")) |>
      mutate(adp = dplyr::coalesce(adp_new, adp)) |>
      select(-adp_new)
  }
}

out <- agg |>
  filter(!(pass_yds == 0 & rush_yds == 0 & rec == 0 & rec_yds == 0 &
             pass_td == 0 & rush_td == 0 & rec_td == 0)) |>
  transmute(
    player, pos, team,
    adp = round(adp, 1),
    pass_yds = round(pass_yds, 1),
    pass_td = round(pass_td, 2),
    pass_int = round(pass_int, 2),
    rush_yds = round(rush_yds, 1),
    rush_td = round(rush_td, 2),
    rec = round(rec, 2),
    rec_yds = round(rec_yds, 1),
    rec_td = round(rec_td, 2),
    fumbles = round(fumbles, 2),
    floor_mult = round(floor_mult, 3),
    ceiling_mult = round(ceiling_mult, 3)
  ) |>
  arrange(pos, desc(rec_yds + rush_yds + pass_yds))

dir.create("data", showWarnings = FALSE)
write_csv(out, "data/projections.csv")
write_csv(source_rows, "data/projections_by_source.csv")
writeLines(
  c(
    paste("refreshed_at", format(Sys.time(), tz = "America/Chicago")),
    paste("sources", paste(ff_sources, collapse = ", ")),
    paste("adp_sources CBS, FFC, MFL, RTS (NFL skipped)"),
    paste("players", nrow(out)),
    paste("source_rows", nrow(source_rows))
  ),
  "data/refresh_meta.txt"
)

message("Wrote data/projections.csv with ", nrow(out), " players.")
message("NFL ADP is skipped because that scrape is failing.")
message("Commit and push data/projections.csv, then republish.")