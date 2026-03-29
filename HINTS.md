# ECON 3385 — Problem Set Style & Implementation Hints

## Overview

The course centers on one core theme across all psets: **prices are endogenous (set in equilibrium), so you need instruments (IV/2SLS) to estimate demand. Estimated demand is then used for merger counterfactuals.**

Demand specifications evolve: **Linear → Loglog → Logit → Nested Logit**

---

## PSET 3 — Two-Good Market Equilibrium & 2SLS Identification

**Problems:**
- Identify valid instruments in a two-good demand-supply system
- Generate simulated equilibrium prices/quantities from a 4-equation system
- Perform 2SLS; test first-stage instrument relevance (F-stat)
- Show identification fails when you have fewer instruments than endogenous regressors

**Tools:** `numpy`, `pandas`, `statsmodels` (OLS, IV2SLS)

**Models:**
- OLS for first-stage (F-stats ~168 and ~147 → very strong instruments)
- IV2SLS for second-stage demand estimation

**Key insight:** Supply shifters (Z_a, Z_b) are valid instruments — they affect supply but not demand directly. Need at least as many instruments as endogenous variables.

---

## PSET 5 — Airline Industry: Loglog Demand & Multiple Merger Analysis

**Data:** `airlines_long.csv` — 4 airlines, route-quarter level (long format → reshape to wide)

**Problems:**
1. Reshape data from long to wide (route-quarter level pivot)
2. Estimate loglog demand: `log(Q_jct) = α_j + β_pop×log(avg_pop) + Σ βjk×log(p_jct) + ε`
3. Write Nash-Bertrand pricing FOCs for 4 airlines
4. 2SLS with 8 instruments (avg_hub + other airlines' prices)
5. Compute implied marginal costs from FOCs
6. Simulate AA+UA merger; report price/profit changes
7. Compare 3 merger scenarios: AA+UA, AA+DL, UA+DL

**Tools:** `numpy`, `pandas`, `statsmodels` (formula API), `linearmodels.iv` (IV2SLS), `matplotlib`

**Models:** 2SLS on 4 simultaneous loglog demand equations

**Key results:**
- AA+DL merger → largest price increase (~4–5%)
- UA+DL merger → smallest (~1–2%)
- AA has stronger pricing power than UA

---

## PSET 6 — Airline Industry: Logit Demand & Merger with Elasticities

**Data:** `airlines_long_2.csv` — 8 airlines, includes explicit `mkt_size`

**Problems:**
1. Specify logit demand (Type I extreme value errors); interpret utility & market share formula
2. Estimate logit demand via 2SLS (`avg_hub` as instrument; Berry-style)
3. Compute own-price elasticities: `ε_jj = α × p_j × (1 − s_j)`
4. Compute cross-price elasticities (AA–DL pairs)
5. Solve for marginal costs from Bertrand FOCs
6. Simulate AA–DL merger; compare to loglog results

**Tools:** `numpy`, `pandas`, `linearmodels.iv`, `itertools`, `matplotlib`

**Models:** IV2SLS in logit framework (Berry linearization: `ln(s_j) − ln(s_0)` as LHS)

**Key results:**
- Own-price elasticity: −1 to −2 range
- AA–DL cross-elasticity ≈ 0.016 (weak substitutes)
- Despite weak substitution, AA–DL merger → **13.5% price increase**
- Internalization of competitor effects drives the result, not product substitutability

---

## PSET 7 — Airline Industry: Nested Logit & Demand Model Comparison

**Data:** `airlines_long_2.csv` (same as PSET 6)

**Problems:**
1. Explain nested logit share structure: `s_j = s(j|G,ct) × s(G,ct)`
2. Estimate nested logit via 2SLS (same instruments as PSET 6)
3. Estimate nesting parameter λ (within-group substitution correlation)
4. Compute marginal costs using nested logit FOC
5. Simulate AA–DL merger; compare to simple logit

**Tools:** `numpy`, `pandas`, `linearmodels.iv`

**Models:** IV2SLS with nested logit LHS: `ln(s_j) − ln(s_0) = ... + (1−λ)×ln(s_{j|G}) + ξ`

**Key results:**
- λ ≈ 0.58 (moderate within-nest correlation)
- Nested logit → AA–DL price increase ~8–10% (vs logit's 13.5%)
- Demand specification materially affects merger predictions

---

## Cross-Pset Patterns

### Solution Structure (notebooks)
1. Markdown cell: `# ECON 3385 - Problem Set N` + `Anton Melnychuk`
2. Markdown cells for each question header: `### Question X`
3. Markdown with derivations/explanations (LaTeX math)
4. Code cells with implementation
5. Output shown inline

### Methodological Consistency
- IV/2SLS appears in **every** pset (solution to price endogeneity)
- Merger simulation always: estimate demand → recover marginal costs from FOC → iterate price equilibrium
- `linearmodels.iv` preferred over `statsmodels` for PSET 5–7
- Pivoting long→wide data is a recurring data prep step

### Core Libraries
| Library | Use |
|---|---|
| `numpy` | Matrix ops, numerical optimization, equilibrium iteration |
| `pandas` | Data wrangling, pivot/reshape |
| `statsmodels` | OLS, early IV (PSET 3) |
| `linearmodels.iv` | IV2SLS (PSET 5–7) |
| `matplotlib` | Demand curves, elasticity plots |

### Regression Models Used
- **OLS** — first-stage instrument testing
- **IV2SLS** — demand estimation (endogenous prices)
- **Logit** — discrete choice, Berry linearization
- **Nested Logit** — hierarchical discrete choice with nesting parameter λ

---

## Data Files Reference

| PSET | Branch | Notebook | Data |
|---|---|---|---|
| 3 | pset3 | ps3_solution.ipynb | sim_market.csv (simulated, 400 obs) |
| 5 | pset5 | ps5_solutions.ipynb | airlines_long.csv (4 airlines) |
| 6 | pset6 | ps6_solutions.ipynb | airlines_long_2.csv (8 airlines) |
| 7 | pset7 | ps7_solutions.ipynb | airlines_long_2.csv (8 airlines) |
| 8 | pset8 | ps8_solutions.ipynb | jets.csv |
