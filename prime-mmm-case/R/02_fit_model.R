# =============================================================================
# 02 - Fit the Marketing Mix Model
# -----------------------------------------------------------------------------
# 1. Try many adstock (theta) and saturation (K) values per channel
# 2. For each combination: fit on the first 88 weeks, predict the last 16
# 3. Keep the combination that predicts the unseen weeks best
# 4. Refit on all 104 weeks and decompose sign-ups by driver
# =============================================================================

source("R/00_functions.R")
df <- read.csv("data/weekly_spain.csv")
df$date <- as.Date(df$date)

train <- df$week <= 88
test  <- !train

# ---- 1-3. Grid search over theta and K ------------------------------------------
grid <- expand.grid(
  theta_search = c(0, 0.1, 0.2, 0.3),             # search: short memory
  K_search     = c(40000, 60000, 80000, 120000, 160000),
  theta_social = c(0, 0.2, 0.4, 0.6),             # social: may linger
  K_social     = c(25000, 50000, 75000, 100000)
)

make_features <- function(d, p) {
  d$search_t <- saturation(adstock(d$search_spend, p$theta_search), p$K_search)
  d$social_t <- saturation(adstock(d$social_spend, p$theta_social), p$K_social)
  d
}

formula_mmm <- prime_signups ~ search_t + social_t + season + black_friday

grid$test_mape <- sapply(seq_len(nrow(grid)), function(i) {
  d   <- make_features(df, grid[i, ])
  fit <- lm(formula_mmm, data = d[train, ])
  if (any(coef(fit)[c("search_t", "social_t")] < 0)) return(NA)  # ads can't hurt
  mape(d$prime_signups[test], predict(fit, d[test, ]))
})

grid <- grid[!is.na(grid$test_mape), ]
grid <- grid[order(grid$test_mape), ]
best <- grid[1, ]

# Many settings predict almost equally well. We keep every model within 10%
# of the best error as "plausible" and use them later as an uncertainty range.
plausible <- grid[grid$test_mape <= best$test_mape * 1.10, ]
cat(nrow(plausible), "of", nrow(grid), "models are plausible (within 10% of best)\n")
cat("Best hyperparameters (out-of-sample MAPE =",
    round(100 * best$test_mape, 2), "%):\n")
print(best, row.names = FALSE)

# ---- 4. Final model on all weeks --------------------------------------------------
d     <- make_features(df, best)
model <- lm(formula_mmm, data = d)
print(summary(model))

# Hold-out check with the chosen settings (for the chart)
fit_train  <- lm(formula_mmm, data = d[train, ])
d$fitted   <- fitted(model)
d$pred_test <- NA
d$pred_test[test] <- predict(fit_train, d[test, ])

# ---- Decomposition: how many sign-ups each driver produced --------------------
b <- coef(model)
d$contrib_search <- b["search_t"] * d$search_t
d$contrib_social <- b["social_t"] * d$social_t
d$contrib_base   <- d$fitted - d$contrib_search - d$contrib_social

dir.create("output", showWarnings = FALSE)
saveRDS(list(model = model, best = best, plausible = plausible, data = d,
             formula = formula_mmm),
        "output/model.rds")

# ---- Compare with the truth (only possible because data is simulated) --------------
truth <- readRDS("data/true_params.rds")
cat("\nRecovered vs true parameters:\n")
print(data.frame(
  channel   = c("search", "social"),
  theta_est = c(best$theta_search, best$theta_social), theta_true = truth$theta,
  K_est     = c(best$K_search, best$K_social),         K_true     = truth$K,
  beta_est  = round(c(b["search_t"], b["social_t"])),  beta_true  = truth$beta
), row.names = FALSE)
