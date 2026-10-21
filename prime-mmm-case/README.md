# Marketing Mix Model for a travel subscription: paid search vs paid social

**How should an online travel agency split its paid budget to win the most new subscription members?**

A Marketing Mix Model (MMM) in R that turns two years of weekly spend and sign-up data into a budget recommendation. It's inspired by the subscription model of European online travel agencies such as eDreams ODIGEO (Prime). All data is **simulated**, and this project is not affiliated with any company.

| | |
|---|---|
| **Business question** | Which channel brings new members most cheaply, and how should we split the budget? |
| **Data** | 104 weeks, one market (Spain), simulated |
| **Channels** | Paid search, paid social |
| **KPI** | New subscription members per week, valued at lifetime value (LTV) |
| **Method** | Adstock + saturation transformations, linear regression, grid search validated on hold-out weeks |
| **Tools** | R (base R + ggplot2) |

---

## 1. Business context

For a travel subscription business, the goal of paid marketing is no longer to buy a booking. It is to **acquire a member** who pays a yearly fee and then books again and again without new ad spend.

eDreams ODIGEO is a public example of this shift. Its Prime programme had 7.7M members at the end of Q3 FY26 and generates about 91% of Cash Marginal Profit, while marketing and variable costs fell from €325.5M to €277.6M in the first nine months of FY26 ([3Q FY26 results report](https://s202.q4cdn.com/903232390/files/doc_financials/2026/q3/v2/3Q-FY26_eDO-Results-Report_EN.pdf)).

So the questions marketing needs answered are:

1. How many new members does each channel **really** cause, beyond those who would join anyway?
2. What does a new member cost per channel, **on average and at the margin**?
3. With the same budget, what split between search and social gives the most members?

## 2. Data

`data/weekly_spain.csv`: one row per week.

| Column | Meaning |
|---|---|
| `prime_signups` | New members that week (KPI) |
| `search_spend` | Paid search spend, EUR. Always on, higher in peak season |
| `social_spend` | Paid social spend, EUR. 4-week campaigns, then a low always-on level |
| `season` | Travel demand index, peaking in January when people plan summer |
| `black_friday` | 1 in Black Friday weeks |

**Why simulated data?** Real data is confidential. Simulation also gives an answer key: I set the true channel effects, so I can check whether the model finds them ([section 6](#6-did-the-model-find-the-truth)).

## 3. Method

```
spend ──► adstock ──► saturation ──► regression ──► contributions, ROI, budget curves
          (carryover)  (diminishing
                        returns)
```

**Adstock: ads keep working after the week they run.** A person who sees an Instagram ad may sign up two weeks later. `theta` is the share of last week's effect that carries over.

```r
adstock <- function(x, theta) {
  out <- numeric(length(x)); carry <- 0
  for (t in seq_along(x)) { carry <- x[t] + theta * carry; out[t] <- carry }
  out
}
```

**Saturation: each extra euro buys less.** Search is an auction. Once you are top of the page, bidding more buys few extra clicks. `K` is the spend at which a channel reaches half of its maximum effect.

```r
saturation <- function(x, K) x / (x + K)
```

**Regression:**

```r
lm(prime_signups ~ search_t + social_t + season + black_friday)
```

**Choosing `theta` and `K`:** I tried 320 combinations. Each was fitted on the first 88 weeks and scored on the last 16 weeks, which the model never saw. The best one is the main model. Every model within 10% of the best error (67 in total) is kept as "plausible" to show how uncertain the results are.

## 4. Results

### The model fits well, including on unseen weeks

![Model fit](output/figures/01_fit.png)

R² = 0.97. Average error on the 16 hold-out weeks is 1.3%. Both channels have positive, significant effects.

### About half of new members would join anyway

![Decomposition](output/figures/02_decomposition.png)

| Driver | Share of sign-ups (2 years) |
|---|---|
| Base (organic + season + Black Friday) | 50% |
| Paid search | 28% |
| Paid social | 23% |

This is the first insight for the business. Last-click attribution would credit paid media with many of the members in the base, who would have joined anyway.

### Diminishing returns in both channels

![Response curves](output/figures/03_response_curves.png)

Social rises fast and flattens early. Search keeps growing at higher spend.

### The key finding: average cost and marginal cost point in different directions

![Average vs marginal cost](output/figures/04_avg_vs_marginal_cost.png)

| | Paid search | Paid social |
|---|---|---|
| Spend (2 years) | €6.28M | €3.86M |
| Members driven | 167,900 | 139,800 |
| **Average cost per member** | **€37** | **€28** |
| ROI on LTV (€120 per member) | 3.2 | 4.3 |
| Extra members per extra €10k/week | +156 | +92 |
| **Marginal cost per member** | **€64** (range €56–78) | **€108** (range €87–129) |

On average, social looks like the better channel. But budget decisions are about the **next** euro, and there search is much cheaper: €64 versus €108 for social, which is close to the €120 a member is worth. Social's cheap members have already been bought.

### Same budget, more members

![Budget split](output/figures/05_budget_split.png)

| Scenario | Search / week | Social / week | Paid members / week |
|---|---|---|---|
| Today (62% search) | €60.4k | €37.1k | 3,049 |
| Optimal (70% search) | €68.2k | €29.2k | 3,072 |

The gain is modest: +22 members a week, about +1,150 a year, worth about €140k in lifetime value at no extra cost. More importantly, **all 67 plausible models agree on the direction**: the best split is 66–76% on search, above today's 62%.

## 5. Recommendation

1. **Shift about 8 percentage points of the paid budget from social to search** (about €8k a week). Every plausible model says search should get a bigger share.
2. **Don't cut social to zero.** It's still profitable at the margin, and its effect lasts weeks after each campaign. Judging it on same-week sign-ups would undervalue it.
3. **Validate before scaling.** Run a geo holdout test: raise search and lower social in a few regions for 6–8 weeks and compare with matched control regions. Use the result to calibrate the next model.
4. **Value members at lifetime value, not first booking.** Measured on one booking's margin, both channels would look far less profitable than they are.

## 6. Did the model find the truth?

Because the data is simulated, I can compare the model with the effects I built in:

| | Model | Plausible range | Truth |
|---|---|---|---|
| Members from search | 167,900 | 138,900–317,600 | 187,800 |
| Members from social | 139,800 | 74,600–140,600 | 108,200 |
| Marginal cost, search | €64 | €56–78 | €65 |
| Marginal cost, social | €108 | €87–129 | €86 |
| Best search share | 70% | 66–76% | 67% |
| Carryover (theta), social | 0.4 | | 0.4 |

**What this shows:**
- **Carryover is well identified.** The model found exactly how long social's effect lasts.
- **Saturation is weakly identified.** Many curve shapes fit the data almost equally well, so the size of each channel's contribution is uncertain. The best single model overstates social by about 29%.
- **The decision is robust even though the sizes aren't.** The truth sits inside every plausible range, and every plausible model recommends moving budget towards search.

This is why I report ranges, not single numbers, and why I'd calibrate MMM with experiments before making large budget moves.

## 7. Limitations and next steps

- **Endogeneity:** search spend follows demand (it's higher in January). Seasonality is controlled for, but the model could still over-credit search in peaks. A search holdout test would check this.
- **Brand vs generic search:** brand searches ("edreams flights") come from people already looking for the company. With real data I would split them, as brand search usually has a much lower true incremental effect.
- **One market, two channels:** a real model would cover several countries with a hierarchical (multilevel) model, so small markets can borrow strength from big ones.
- **Lifetime value is an assumption** (€120). With real data I'd estimate LTV per acquisition channel from cohorts, since members from social might renew less than those from search.
- **Next tools:** Meta's Robyn (R) or Google's Meridian for production use, both of which support calibration with experiments.

## How to run

```r
install.packages(c("ggplot2", "scales"))
```

```bash
Rscript R/run_all.R
```

| Script | What it does |
|---|---|
| `R/00_functions.R` | Adstock, saturation and response-curve helpers |
| `R/01_simulate_data.R` | Creates `data/weekly_spain.csv` with known true effects |
| `R/02_fit_model.R` | Grid search with hold-out validation, final model, decomposition |
| `R/03_roi_and_budget.R` | ROI, marginal cost, budget scenarios, uncertainty ranges |
| `R/04_charts.R` | Creates the charts in `output/figures/` |

---

*Simulated data for demonstration only. Not affiliated with or endorsed by eDreams ODIGEO.*
