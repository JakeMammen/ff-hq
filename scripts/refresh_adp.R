# Pull format-specific ADP from Fantasy Football Calculator.
# FFC publishes Standard, Half PPR, and PPR as separate lists.
#
#   setwd("/Users/jakemammen/Developer/ff_hq")
#   source("scripts/refresh_adp.R")

library(ffanalytics)
library(dplyr)
library(readr)

norm_name <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- gsub("\\b(jr|sr|iii|ii|iv)\\b", "", x)
  trimws(gsub("\\s+", " ", x))
}

pick_chr <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) return(rep(NA_character_, nrow(df)))
  as.character(df[[hit[[1]]]])
}

pick_num <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) return(rep(NA_real_, nrow(df)))
  suppressWarnings(as.numeric(df[[hit[[1]]]]))
}

ffc_format_adp <- function(fmt) {
  raw <- tryCatch(
    ffc_draft(format = fmt, pos = "all", n_teams = "12", metric = "adp"),
    error = function(e) {
      message("ffc_draft(", fmt, ") failed: ", e$message)
      NULL
    }
  )
  if (is.null(raw) || !nrow(raw)) return(NULL)
  
  tibble(
    player = pick_chr(raw, c("player", "player_name", "name")),
    pos = toupper(trimws(pick_chr(raw, c("pos", "position")))),
    adp = pick_num(raw, c("adp", "ADP", "avg", "average"))
  ) |>
    filter(!is.na(player), player != "", is.finite(adp)) |>
    mutate(
      player_norm = norm_name(player),
      pos = recode(pos, PK = "K", DEF = "DST", DST = "DST", .default = pos)
    ) |>
    filter(pos %in% c("QB", "RB", "WR", "TE")) |>
    group_by(player_norm, pos) |>
    summarise(adp = mean(adp, na.rm = TRUE), .groups = "drop")
}

message("Scraping FFC PPR ADP ...")
ppr <- ffc_format_adp("ppr")
message("Scraping FFC Half PPR ADP ...")
half <- ffc_format_adp("half-ppr")
message("Scraping FFC Standard ADP ...")
std <- ffc_format_adp("standard")

if (is.null(ppr) && is.null(half) && is.null(std)) {
  stop("FFC returned no ADP. Try again later.")
}

empty_map <- function(col) {
  tibble(player_norm = character(), pos = character()) |>
    mutate(!!col := numeric())
}

adp_map <- full_join(
  if (is.null(ppr)) empty_map("adp_ppr") else rename(ppr, adp_ppr = adp),
  if (is.null(half)) empty_map("adp_half") else rename(half, adp_half = adp),
  by = c("player_norm", "pos")
) |>
  full_join(
    if (is.null(std)) empty_map("adp_std") else rename(std, adp_std = adp),
    by = c("player_norm", "pos")
  )

proj <- read_csv("data/projections.csv", show_col_types = FALSE) |>
  mutate(player_norm = norm_name(player), pos = toupper(pos)) |>
  select(-any_of(c("adp_ppr", "adp_half", "adp_std"))) |>
  left_join(adp_map, by = c("player_norm", "pos")) |>
  mutate(
    adp = dplyr::case_when(
      is.finite(adp_ppr) ~ adp_ppr,
      is.finite(adp_half) ~ adp_half,
      is.finite(adp_std) ~ adp_std,
      TRUE ~ adp
    )
  ) |>
  select(-player_norm)

write_csv(proj, "data/projections.csv")

message(
  "Wrote ADP columns. ",
  "PPR: ", sum(is.finite(proj$adp_ppr)), " | ",
  "Half: ", sum(is.finite(proj$adp_half)), " | ",
  "Std: ", sum(is.finite(proj$adp_std))
)
print(head(proj[order(proj$adp_ppr), c("player", "pos", "adp_ppr", "adp_half", "adp_std")], 12))