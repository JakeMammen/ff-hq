# Run locally only:
#   Rscript scripts/refresh_projections.R
#
# Later this will scrape crowd stats and overwrite data/projections.csv.

if (!file.exists("data/projections.csv")) {
  stop("data/projections.csv is missing. Run this from the project root.")
}

n <- nrow(read.csv("data/projections.csv"))
message("Current snapshot has ", n, " players.")