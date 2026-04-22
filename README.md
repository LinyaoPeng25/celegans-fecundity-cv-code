# celegans-fecundity-cv-code

# Code for automated fecundity counting analyses in C. elegans

This repository contains the R code used to generate the statistical analyses and figures reported in the manuscript on YOLO-based automated counting of C. elegans offspring.

## Contents

- `analysis_figure_scripts.R`  
  Main R script used to generate the analyses and figures.

## Input files

The script expects the following CSV files in the working directory:

- `cv_manual_count_counter_block.csv`
- `heldout_manual_vs_cv_50test.csv`

## Figures generated

The script includes code for:

- Figure 3: comparison to ground truth
- Figure 4A: linear regression of manual versus computer vision counts
- Figure 4B: Bland–Altman analysis
- Figure 5A: counter bias analysis
- Figure 5B: counter bias across experimental blocks

## Software requirements

The analyses were run in R.

### Required R packages

- `ggplot2`
- `dplyr`
- `tidyverse`
- `ggpubr`
- `patchwork`
- `multcompView`

Install missing packages with:

```r
install.packages(c("ggplot2", "dplyr", "tidyverse", "ggpubr", "patchwork", "multcompView"))
