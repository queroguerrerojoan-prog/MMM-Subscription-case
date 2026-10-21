# =============================================================================
# 00 - Helper functions
# The two transformations that make an MMM different from a plain regression.
# =============================================================================

# Adstock (carryover): part of last week's advertising effect carries into
# this week.  theta = share that carries over (0 = none, 0.7 = long memory).
adstock <- function(x, theta) {
  out <- numeric(length(x))
  carry <- 0
  for (t in seq_along(x)) {
    carry  <- x[t] + theta * carry
    out[t] <- carry
  }
  out
}

# Saturation (diminishing returns): output between 0 and 1.
# K = spend level at which a channel reaches half of its maximum effect.
saturation <- function(x, K) x / (x + K)

# Weekly response if we spent `s` EUR every week (steady state).
# With constant spend, adstock settles at s / (1 - theta).
weekly_response <- function(s, theta, K, beta) {
  beta * saturation(s / (1 - theta), K)
}

mape <- function(actual, predicted) mean(abs(actual - predicted) / actual)
