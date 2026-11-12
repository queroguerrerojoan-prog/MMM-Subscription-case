# Run the whole case from start to finish:  Rscript R/run_all.R
# Packages needed: ggplot2, scales  ->  install.packages(c("ggplot2", "scales"))
for (f in c("R/01_simulate_data.R", "R/02_fit_model.R",
            "R/03_roi_and_budget.R", "R/04_charts.R")) {
  cat("\n=====", f, "=====\n")
  source(f, echo = FALSE)
}
