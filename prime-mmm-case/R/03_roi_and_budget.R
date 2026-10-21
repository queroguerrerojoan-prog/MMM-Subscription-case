# =============================================================================
# 03 - ROI, marginal cost and budget scenarios
# -----------------------------------------------------------------------------
# A Prime member is worth more than one booking: they pay a yearly fee and
# book again without new ad spend. So we value sign-ups at lifetime value (LTV).
# =============================================================================

source("R/00_functions.R")
m    <- readRDS("output/model.rds")
d    <- m$data
best <- m$best
b    <- coef(m$model)

LTV  <- 120     # EUR margin per Prime member over their lifetime (assumption)
STEP <- 10000   # "what does the next EUR 10k per week buy?"

avg_search <- mean(d$search_spend)
avg_social <- mean(d$social_spend)
total      <- avg_search + avg_social
shares     <- seq(0.30, 0.90, by = 0.01)    # search share of a fixed total budget

# ---- Everything we want to know, for ONE set of model settings -------------------
evaluate <- function(p) {
  dd <- d
  dd$search_t <- saturation(adstock(dd$search_spend, p$theta_search), p$K_search)
  dd$social_t <- saturation(adstock(dd$social_spend, p$theta_social), p$K_social)
  bb <- coef(lm(m$formula, data = dd))
  c_search <- function(s) weekly_response(s, p$theta_search, p$K_search, bb["search_t"])
  c_social <- function(s) weekly_response(s, p$theta_social, p$K_social, bb["social_t"])
  split <- c_search(total * shares) + c_social(total * (1 - shares))
  c(signups_search  = unname(sum(bb["search_t"] * dd$search_t)),
    signups_social  = unname(sum(bb["social_t"] * dd$social_t)),
    mcost_search    = unname(STEP / (c_search(avg_search + STEP) - c_search(avg_search))),
    mcost_social    = unname(STEP / (c_social(avg_social + STEP) - c_social(avg_social))),
    best_search_share = shares[which.max(split)])
}

# ---- 1. Main model: average ROI (what happened) ------------------------------------------
roi <- data.frame(
  channel        = c("Paid search", "Paid social"),
  spend_eur      = c(sum(d$search_spend), sum(d$social_spend)),
  signups_driven = c(sum(d$contrib_search), sum(d$contrib_social))
)
roi$share_of_signups <- roi$signups_driven / sum(d$prime_signups)
roi$cost_per_member  <- roi$spend_eur / roi$signups_driven
roi$roi_on_ltv       <- roi$signups_driven * LTV / roi$spend_eur
base_share <- sum(d$contrib_base) / sum(d$fitted)

# ---- 2. Main model: marginal cost (what the NEXT euro buys) ------------------------------
curve_search <- function(s) weekly_response(s, best$theta_search, best$K_search, b["search_t"])
curve_social <- function(s) weekly_response(s, best$theta_social, best$K_social, b["social_t"])

roi$avg_weekly_spend      <- c(avg_search, avg_social)
roi$extra_signups_per_10k <- c(curve_search(avg_search + STEP) - curve_search(avg_search),
                               curve_social(avg_social + STEP) - curve_social(avg_social))
roi$marginal_cost_per_member <- STEP / roi$extra_signups_per_10k

# ---- 3. Main model: budget split (same total budget) -----------------------------------
scen <- data.frame(search_share = shares,
                   weekly_signups = curve_search(total * shares) +
                                    curve_social(total * (1 - shares)))
current <- curve_search(avg_search) + curve_social(avg_social)
opt     <- scen[which.max(scen$weekly_signups), ]

# ---- 4. How sure are we? Repeat for every plausible model ------------------------------
unc <- as.data.frame(t(sapply(seq_len(nrow(m$plausible)),
                              function(i) evaluate(m$plausible[i, ]))))
unc$roi_search <- unc$signups_search * LTV / roi$spend_eur[1]
unc$roi_social <- unc$signups_social * LTV / roi$spend_eur[2]
ranges <- sapply(unc, range)

saveRDS(list(roi = roi, scen = scen, current = current, opt = opt,
             current_share = avg_search / total, total = total,
             base_share = base_share, LTV = LTV, unc = unc, ranges = ranges,
             curve_params = data.frame(channel = c("search", "social"),
                                       theta = c(best$theta_search, best$theta_social),
                                       K = c(best$K_search, best$K_social),
                                       beta = unname(b[c("search_t", "social_t")]))),
        "output/results.rds")

# ---- Print ---------------------------------------------------------------------------
cat("Base (organic + season) share of sign-ups:", round(100 * base_share, 1), "%\n\n")
print(transform(roi, spend_eur = round(spend_eur), signups_driven = round(signups_driven),
                share_of_signups = round(share_of_signups, 3),
                cost_per_member = round(cost_per_member, 1), roi_on_ltv = round(roi_on_ltv, 2),
                avg_weekly_spend = round(avg_weekly_spend),
                extra_signups_per_10k = round(extra_signups_per_10k),
                marginal_cost_per_member = round(marginal_cost_per_member)),
      row.names = FALSE)

cat("\nBudget split (total EUR", round(total), "per week):\n")
cat("  Current: search share", round(100 * avg_search / total), "% ->",
    round(current), "paid sign-ups/week\n")
cat("  Optimal: search share", round(100 * opt$search_share), "% ->",
    round(opt$weekly_signups), "paid sign-ups/week (+", round(opt$weekly_signups - current), ")\n")

cat("\nRange across", nrow(unc), "plausible models:\n")
print(round(ranges, 2))
