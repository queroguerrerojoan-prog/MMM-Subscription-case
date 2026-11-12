# =============================================================================
# 01 - Simulate 2 years of weekly data for one market (Spain)
# -----------------------------------------------------------------------------
# Real company data is confidential, so we SIMULATE it. Bonus: because we set
# the true effects ourselves, we can check whether the model recovers them.
# =============================================================================

source("R/00_functions.R")
set.seed(1)

n    <- 104                                        # 2 years of weeks
week <- 1:n
date <- seq(as.Date("2024-01-01"), by = "week", length.out = n)

# ---- Demand drivers ------------------------------------------------------------
# Travel demand peaks in January (people plan summer holidays)
season       <- 1 + 0.25 * cos(2 * pi * (week - 3) / 52)
black_friday <- as.integer(week %in% c(47, 99))

# ---- Media spend (EUR per week) -------------------------------------------------
# Paid search: always on, and follows demand (more searches in peak season)
search <- round(60000 * season * runif(n, 0.85, 1.15))

# Paid social: 4-week campaigns at high spend, then 4 weeks at a low "always on" level
social <- round(ifelse(week %% 8 %in% 0:3,
                       runif(n, 50000, 70000),
                       runif(n, 10000, 20000)))

# ---- The "true" world (unknown in real life) ------------------------------------
TRUE_PARAMS <- data.frame(
  channel = c("search", "social"),
  theta   = c(0.1, 0.4),      # search acts immediately; social lingers
  K       = c(80000, 50000),  # half-saturation spend
  beta    = c(4000, 2000)     # max weekly sign-ups the channel could drive
)

search_t <- saturation(adstock(search, 0.1), 80000)
social_t <- saturation(adstock(social, 0.4), 50000)

prime_signups <- round(3000 * season +           # base: organic demand
                       400 * black_friday +       # Black Friday promo
                       4000 * search_t +          # paid search effect
                       2000 * social_t +          # paid social effect
                       rnorm(n, 0, 120))          # noise

df <- data.frame(date, week, prime_signups,
                 search_spend = search, social_spend = social,
                 season = round(season, 4), black_friday)

dir.create("data", showWarnings = FALSE)
write.csv(df, "data/weekly_spain.csv", row.names = FALSE)
saveRDS(TRUE_PARAMS, "data/true_params.rds")

cat("Saved", n, "weeks to data/weekly_spain.csv\n")
