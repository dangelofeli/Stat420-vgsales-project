# Video Game Sales Analysis

## Setup
Install required packages (run once in RStudio console):
```r
install.packages(c("tidyverse", "janitor", "here", "lmtest", "car", "moments", "knitr", "rmarkdown"))
```

## How to run
1. Open `vg-sales.Rproj` in RStudio
2. Open any `.Rmd` file in `reports/`
3. Hit **Knit** to generate the HTML report

## File structure
```
vg-sales/
├── vg-sales.Rproj
├── README.md
├── data/
│   └── vgsales.csv
├── R/
│   └── 01_load_clean.R
└── reports/
    ├── 01_preprocessing.Rmd
    ├── 02_modeling.Rmd
    └── 03_final_model.Rmd
```
