# =============================================================================
# 04 - Charts used in the README
# =============================================================================

suppressPackageStartupMessages({ library(ggplot2); library(scales) })
source("R/00_functions.R")
m <- readRDS("output/model.rds")
r <- readRDS("output/results.rds")
d <- m$data
cp <- r$curve_params
curve <- function(ch, s) weekly_response(s, cp$theta[cp$channel == ch], cp$K[cp$channel == ch],
                                         cp$beta[cp$channel == ch])

COL <- c("Paid search" = "#2a78d6", "Paid social" = "#eb6834",
         "Base (organic + season)" = "#c9c7c0")
INK <- "#0b0b0b"; INK2 <- "#52514e"; GRID <- "#e1e0d9"

theme_case <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = GRID, linewidth = 0.3),
        plot.title = element_text(face = "bold", colour = INK),
        plot.subtitle = element_text(colour = INK2),
        axis.text = element_text(colour = INK2),
        axis.title = element_text(colour = INK2),
        legend.position = "top", legend.justification = "left",
        legend.title = element_blank(),
        plot.background = element_rect(fill = "white", colour = NA))

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
save <- function(p, file, h = 4.2) ggsave(file.path("output/figures", file), p,
                                          width = 8, height = h, dpi = 150, bg = "white")

# ---- 1. Does the model fit? Actual vs predicted, incl. unseen weeks ------------------
test_start <- min(d$date[!is.na(d$pred_test)])
p1 <- ggplot(d, aes(date)) +
  annotate("rect", xmin = test_start, xmax = max(d$date) + 3, ymin = -Inf, ymax = Inf,
           fill = "#f1f0ec") +
  annotate("text", x = test_start + 4, y = min(d$prime_signups),
           label = "Hold-out weeks\n(not used to fit)", hjust = 0, vjust = 0, size = 3.2,
           colour = INK2) +
  geom_line(aes(y = prime_signups, colour = "Actual"), linewidth = 0.7) +
  geom_line(aes(y = ifelse(is.na(pred_test), fitted, pred_test), colour = "Model"),
            linewidth = 0.7, linetype = "22") +
  scale_colour_manual(values = c(Actual = INK, Model = "#2a78d6")) +
  scale_y_continuous(labels = comma) +
  labs(title = "The model tracks weekly Prime sign-ups closely",
       subtitle = sprintf("R² = %.2f on all weeks · %.1f%% average error on the hold-out weeks",
                          summary(m$model)$r.squared, 100 * m$best$test_mape),
       x = NULL, y = "New Prime sign-ups / week") +
  theme_case
save(p1, "01_fit.png")

# ---- 2. Where did sign-ups come from? ----------------------------------------------
dec <- rbind(
  data.frame(date = d$date, driver = "Base (organic + season)", value = d$contrib_base),
  data.frame(date = d$date, driver = "Paid social", value = d$contrib_social),
  data.frame(date = d$date, driver = "Paid search", value = d$contrib_search))
dec$driver <- factor(dec$driver, levels = names(COL)[c(3, 2, 1)])
p2 <- ggplot(dec, aes(date, value, fill = driver)) +
  geom_area(colour = "white", linewidth = 0.3) +
  scale_fill_manual(values = COL, breaks = names(COL)) +
  scale_y_continuous(labels = comma) +
  labs(title = "About half of new members would join anyway",
       subtitle = sprintf("Share of sign-ups over 2 years: base %.0f%%, paid search %.0f%%, paid social %.0f%%",
                          100 * r$base_share, 100 * r$roi$share_of_signups[1],
                          100 * r$roi$share_of_signups[2]),
       x = NULL, y = "New Prime sign-ups / week") +
  theme_case
save(p2, "02_decomposition.png")

# ---- 3. Response curves (diminishing returns) ----------------------------------------
s <- seq(0, 150000, by = 1000)
rc <- rbind(data.frame(spend = s, channel = "Paid search", signups = curve("search", s)),
            data.frame(spend = s, channel = "Paid social", signups = curve("social", s)))
now <- data.frame(channel = c("Paid search", "Paid social"),
                  spend = r$roi$avg_weekly_spend,
                  signups = c(curve("search", r$roi$avg_weekly_spend[1]),
                              curve("social", r$roi$avg_weekly_spend[2])))
p3 <- ggplot(rc, aes(spend, signups, colour = channel)) +
  geom_line(linewidth = 0.9) +
  geom_point(data = now, size = 3.2, stroke = 1.2, shape = 21, fill = "white") +
  geom_text(data = now, aes(label = paste0("today: €", round(spend / 1000), "k")),
            vjust = -1.1, hjust = 0.8, size = 3.3, colour = INK, show.legend = FALSE) +
  scale_colour_manual(values = COL) +
  scale_x_continuous(labels = label_dollar(prefix = "€", scale = 1e-3, suffix = "k")) +
  scale_y_continuous(labels = comma) +
  labs(title = "Each extra euro brings fewer members than the one before",
       subtitle = "Weekly sign-ups if this amount were spent every week (from the model)",
       x = "Weekly spend", y = "Sign-ups / week from channel") +
  theme_case
save(p3, "03_response_curves.png")

# ---- 4. The key chart: average vs marginal cost per member --------------------------
u <- r$ranges
cost <- data.frame(
  channel = rep(c("Paid search", "Paid social"), 2),
  type    = factor(rep(c("Average cost\n(all members so far)", "Marginal cost\n(next member)"), each = 2),
                   levels = c("Average cost\n(all members so far)", "Marginal cost\n(next member)")),
  value   = c(r$roi$cost_per_member, r$roi$marginal_cost_per_member),
  lo      = c(r$roi$spend_eur[1] / u[2, "signups_search"], r$roi$spend_eur[2] / u[2, "signups_social"],
              u[1, 3], u[1, 4]),
  hi      = c(r$roi$spend_eur[1] / u[1, "signups_search"], r$roi$spend_eur[2] / u[1, "signups_social"],
              u[2, 3], u[2, 4]))
p4 <- ggplot(cost, aes(channel, value, fill = channel)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.15, colour = INK2, linewidth = 0.4) +
  geom_text(aes(label = paste0("€", round(value)), y = 8), colour = "white",
            fontface = "bold", size = 4) +
  geom_hline(yintercept = r$LTV, linetype = "22", colour = INK) +
  annotate("text", x = 0.45, y = r$LTV + 7, label = paste0("Member value €", r$LTV),
           hjust = 0, size = 3.3, colour = INK) +
  facet_wrap(~type) +
  scale_fill_manual(values = COL, guide = "none") +
  scale_y_continuous(labels = label_dollar(prefix = "€"), limits = c(0, 150)) +
  labs(title = "Social looks cheaper on average, search is cheaper at the margin",
       subtitle = "Cost per Prime member · bars = main model · lines = range across plausible models",
       x = NULL, y = NULL) +
  theme_case + theme(strip.text = element_text(face = "bold", colour = INK, size = 11))
save(p4, "04_avg_vs_marginal_cost.png", h = 4.6)

# ---- 5. Budget split ----------------------------------------------------------------------
rng <- range(r$unc$best_search_share)
p5 <- ggplot(r$scen, aes(search_share, weekly_signups)) +
  annotate("rect", xmin = rng[1], xmax = rng[2], ymin = -Inf, ymax = Inf, fill = "#e3eefb") +
  annotate("text", x = mean(rng), y = min(r$scen$weekly_signups) + 10,
           label = "Best split in\nall plausible models", size = 3.1, colour = "#1c5aa3") +
  geom_line(linewidth = 0.9, colour = INK) +
  geom_point(data = data.frame(x = c(r$current_share, r$opt$search_share),
                               y = c(r$current, r$opt$weekly_signups)),
             aes(x, y), size = 3, colour = INK) +
  annotate("text", x = r$current_share + 0.005, y = r$current - 14, label = "Today", size = 3.3, hjust = 0, colour = INK) +
  annotate("text", x = r$opt$search_share + 0.01, y = r$opt$weekly_signups + 8,
           label = sprintf("Optimal (+%d / week)", round(r$opt$weekly_signups - r$current)),
           size = 3.3, hjust = 0, colour = INK) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = comma) +
  labs(title = "Same budget, more members: shift spend from social to search",
       subtitle = sprintf("Total paid budget fixed at €%sk per week", round(r$total / 1000)),
       x = "Share of budget on paid search", y = "Paid sign-ups / week") +
  theme_case
save(p5, "05_budget_split.png")

cat("Charts saved to output/figures/\n")
