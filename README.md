# Meta-estimates of dead and wounded in the Russia-Ukraine war (2022-)

<!-- ESTIMATES-START -->

## Live estimates

As of **2025-07-23**:
- Estimated deaths: **191,000** to **359,000** (or, roughly **262,000**)
- Estimated casualties: **915,000** to **1,355,000** (or, roughly **1,113,000**)

<!-- ESTIMATES-END -->

![Meta-estimate plot](plots/meta-estimate.png)

Our estimate combines public and leaked intelligence on military casualties in the Russia–Ukraine war with geospatial data on conflict intensity derived from our war-fire system and calculations of shifts in areas of control. We fit a generalized additive model (using R’s mgcv package) with three types of predictors:

* Temporal trend: days since 24 February 2022
* Changes in territory: cumulative net changes in area controlled by each side
* War intensity: a tensor-product smooth of cumulative war-fire activity (the sum of daily logged firing events) and cloud cover, both stratified by area of control

From this GAM we derive daily point estimates, 95% confidence intervals and prediction intervals for casualties and fatalities dating back to the start of the invasion. At present we report only Russian losses, as there are too few credible data on Ukrainian fallen to support a robust meta-estimate.

## Prerequisites
- **R** ≥ 4.0  
- R packages: `tidyverse`, `lubridate`, `mgcv`, `scales`, `zoo` 

## Data Layout
```

scripts/
└─ meta\_estimate.R

source-data/
├─ Soldier\_deaths\_casualties\_estimates.csv
├─ UK\_MoD\_monthly\_estimates.csv
├─ meduza\_2024.csv
├─ meduza\_2025\_weekly.csv
├─ strikes\_by\_location\_and\_day.csv
└─ area\_assessed\_as\_controlled.csv

output-data/
├─ meta-estimate-casualties.csv
└─ meta-estimate-deaths.csv

plots/
└─ meta-estimate.png

````

## Usage
```bash
Rscript scripts/meta_estimate.R
````

* Halts if no data newer than 3 months is found
* Generates CSVs in `output-data/` and plot in `plots/`

## Workflow

1. **Load & clean** multiple source CSVs
2. **Merge** estimates (deaths vs. casualties)
3. **Load covariates** (fires, cloud cover, territory changes)
4. **Fit GAM** with splines & tensor terms
5. **Predict** through today, calculate 95 % CI & PI
6. **Export** CSVs & save plot

## Outputs

* `output-data/meta-estimate-casualties.csv`
* `output-data/meta-estimate-deaths.csv`
* `plots/meta-estimate.png`

