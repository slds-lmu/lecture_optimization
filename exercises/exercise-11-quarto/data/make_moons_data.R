# Generate the shared moons dataset for exercise-11 from R's mlr3.
# Both the R (sol_nlp_R.ipynb) and Python (sol_nlp_py.ipynb) solutions read the
# committed moons.csv this script produces, so the two panels fit identical data.
# Run once: Rscript data/make_moons_data.R  (re-running with the same seed
# reproduces the CSV exactly).
suppressMessages(library(mlr3))

set.seed(123)
n <- 200
moon_data <- tgen("moons")$generate(n)$data()
moon_data$y <- ifelse(moon_data$y == "A", 1, -1)
out <- moon_data[, c("x1", "x2", "y")]

args <- commandArgs(trailingOnly = FALSE)
script_dir <- dirname(sub("--file=", "", grep("--file=", args, value = TRUE)))
if (length(script_dir) == 0 || script_dir == "") script_dir <- "."
write.csv(out, file.path(script_dir, "moons.csv"), row.names = FALSE)

cat(sprintf("wrote %d rows -> moons.csv | x1 [%.3f, %.3f], x2 [%.3f, %.3f]\n",
            nrow(out), min(out$x1), max(out$x1), min(out$x2), max(out$x2)))
