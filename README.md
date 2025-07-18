# Meta-estimates of dead and wounded in the Russia-Ukraine war (2022-)

<!-- ESTIMATES-START -->

## Live estimates

As of **2025-07-18**:
- Estimated deaths: **190,000** to **354,000** (or, roughly **259,000**)
- Estimated casualties: **909,000** to **1,334,000** (or, roughly **1,101,000**)

<!-- ESTIMATES-END -->

![Meta-estimate plot](plots/meta-estimate.png)

Our estimate is based on combining public and leaked intelligence estimates of military casualties and deaths in the Russia–Ukraine war with data on war-fire activity and territory shifts. Specifically, we use a GAM (using **mgcv**) with predictors being days since the invasion began, cumulative changes in territory, and tensor products of cloud coverage and war-fire activity (both cumulative--war-fire activity being the sum of daily logged activity, and broken down by area of control). This outputs an estimate, a confidence interval, and a prediction interval for casualties and dead for every day since February 24th of 2022. It currently only predicts dead and casualties for the Russian side, as there are not enough credible estimates of Ukrainian fallen available to produce such meta-estimates.

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

