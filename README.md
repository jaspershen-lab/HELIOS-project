# HELIOS-project

This repository contains analysis code for the HELIOS diet–ethnicity–microbiome project. The project investigates associations among ethnicity, dietary intake, gut microbiome composition, and microbiome-derived disease risk scores in the HELIOS cohort.

The repository includes code for data preparation, cohort summary, microbiome diversity analysis, differential abundance analysis, diet–microbiome association analysis, network analysis, CRC risk score analysis, and supplementary sensitivity analyses prepared during manuscript revision.

## Repository structure

```text
HELIOS-project/
├── 1-code/
│   ├── 100-tools.R
│   ├── 101-demo_code.R
│   ├── ruwen/
│   │   ├── Aitchison.Rmd
│   │   ├── balance_sample_size.Rmd
│   │   ├── crc_risk_score_adjust.Rmd
│   │   ├── figure1_summary.Rmd
│   │   ├── figure2_heatmap.Rmd
│   │   ├── figure4_heatmap.Rmd
│   │   ├── figure5_network.Rmd
│   │   ├── interaction_thosai_bifi.Rmd
│   │   ├── metabolic_adjust.Rmd
│   │   ├── network_food_nutri_spe.Rmd
│   │   ├── pcoa_eth_food.Rmd
│   │   ├── pseudomonas.Rmd
│   │   ├── volcano_plot.Rmd
│   │   └── other analysis or figure-generation scripts
│   └── xiaotao/
│       ├── 1-data-preparation/
│       ├── 2-study-summary/
│       └── 3_differntial_species/
├── HELIOS-project.Rproj
├── ignore_large_files.sh
├── LICENSE
└── README.md
```

## Project overview

The main analyses include:

1. Ethnicity-associated differences in gut microbiome composition.
2. Dietary differences across Chinese, Indian, and Malay participants.
3. Associations between traditional foods, including chapati, idli, and thosai, and microbial taxa such as *Bifidobacterium* species.
4. Microbiome diversity and beta-diversity analyses.
5. Differential abundance analyses of microbial taxa.
6. Food–microbe correlation and network analyses.
7. Associations between microbiome features and metabolic traits.
8. Microbiome-derived gastrointestinal disease risk score analyses.
9. Sensitivity analyses addressing sample size imbalance and metabolic confounding.

## Main analysis modules

### 1. Data preparation

Location:

```text
1-code/xiaotao/1-data-preparation/
```

This folder contains scripts for preparing and formatting input data, including microbiome abundance tables, metadata, dietary variables, and clinical variables.

### 2. Study summary

Location:

```text
1-code/xiaotao/2-study-summary/
1-code/ruwen/figure1_summary.Rmd
1-code/ruwen/helios_demog.Rmd
```

These scripts summarize participant characteristics, ethnicity distribution, dietary variables, and clinical variables.

### 3. Microbiome diversity and community composition

Example scripts:

```text
1-code/ruwen/Aitchison.Rmd
1-code/ruwen/pcoa_eth_food.Rmd
1-code/ruwen/balance_sample_size.Rmd
```

These scripts are used for alpha diversity, beta diversity, PCoA visualization, Aitchison distance analysis, and PERMANOVA analyses.

### 4. Differential abundance analysis

Example scripts and folders:

```text
1-code/xiaotao/3_differntial_species/
1-code/ruwen/figure2_heatmap.Rmd
1-code/ruwen/logfold_heatmap.Rmd
1-code/ruwen/volcano_plot.Rmd
1-code/ruwen/volcano_speices.Rmd
```

These scripts identify taxa associated with ethnicity and generate heatmaps, log-fold-change plots, and volcano plots.

### 5. Diet–microbiome association analysis

Example scripts:

```text
1-code/ruwen/food_normalize.Rmd
1-code/ruwen/network_food_nutri_spe.Rmd
1-code/ruwen/figure5_network.Rmd
1-code/ruwen/interaction_thosai_bifi.Rmd
```

These scripts examine associations between dietary intake and microbial taxa, including traditional food items such as chapati, idli, and thosai.

### 6. Metabolic trait and disease risk analyses

Example scripts:

```text
1-code/ruwen/metabolic_adjust.Rmd
1-code/ruwen/clinical_var.Rmd
1-code/ruwen/crc_risk_score_adjust.Rmd
1-code/ruwen/cancer_marker.Rmd
```

These scripts analyze associations between microbiome features and clinical or metabolic traits, including BMI, HbA1c, CRP, HOMA-IR, HOMA-B, obesity status, diabetes status, and microbiome-derived CRC risk scores.

## Recommended execution order

The suggested order for running the scripts is:

### Step 1: Open the project

Open the RStudio project file:

```text
HELIOS-project.Rproj
```

Alternatively, set the working directory to the repository root.

### Step 2: Load shared functions

### Step 3: Prepare input data

### Step 4: Generate study summary tables and figures

### Step 5: Run diversity and PERMANOVA analyses

### Step 6: Run differential abundance analyses

### Step 7: Run diet–microbiome analyses

### Step 8: Run metabolic and disease risk analyses

### Step 9: Generate final manuscript and supplementary figures



## Input files

The scripts require several types of input files:

| Input type                     | Description                                                                              |
| ------------------------------ | ---------------------------------------------------------------------------------------- |
| Microbiome abundance table     | Species-level or genus-level microbial abundance table                                   |
| Metadata table                 | Participant ID, ethnicity, age, sex, BMI, and other demographic variables                |
| Dietary table                  | Food frequency questionnaire or food group intake variables                              |
| Clinical table                 | HbA1c, BMI, CRP, HOMA-IR, HOMA-B, diabetes status, obesity status, and related variables |
| Curated microbial marker files | Published microbial signatures or coefficients used for disease risk score calculation   |
| SCFA species lists             | Curated acetate-, propionate-, and butyrate-producing species lists based on literature  |

Individual-level HELIOS cohort data are not included in this repository due to data privacy and governance restrictions. Users should place the required input files in the expected local directories and update file paths in the scripts where necessary.

## Output files

The scripts generate the following types of outputs:

| Output type           | Description                                                                                                                          |
| --------------------- | ------------------------------------------------------------------------------------------------------------------------------------ |
| Figures               | PCoA plots, heatmaps, volcano plots, boxplots, violin plots, network plots, and summary figures                                      |
| Tables                | Differential abundance results, correlation results, PERMANOVA results, adjusted model outputs, and risk score summaries             |
| Intermediate files    | Cleaned metadata, filtered abundance tables, normalized dietary variables, and merged analysis tables                                |
| Supplementary outputs | Matched-cohort sensitivity results, ethnicity-stratified analyses, metabolic adjustment results, and revision-related figures/tables |

Output paths may vary by script. Please check `write.csv()`, `ggsave()`, `pdf()`, `png()`, or notebook export commands inside each script.

## Software requirements

The analyses were mainly performed in R, with some notebooks or scripts potentially using Python/Jupyter.

Recommended software:

```text
R version: R 4.6.0
RStudio: recommended for running .Rmd files
Python version: Python 3.8.8
Jupyter Notebook or JupyterLab: required for .ipynb notebooks
```

Main R packages used across the analyses may include:

```text
tidyverse
dplyr
readr
ggplot2
vegan
phyloseq
DESeq2
Maaslin2
ANCOMBC
LinDA
compositions
MatchIt
pheatmap
ComplexHeatmap
ggpubr
reshape2
igraph
ggraph
Hmisc
ppcor
data.table
rmarkdown
knitr
```

Main Python packages used across notebooks may include:

```text
pandas
numpy
scipy
matplotlib
seaborn
sklearn
statsmodels
```

Because package versions may differ across machines, please record exact software versions using:

```r
sessionInfo()
```

For Python notebooks, record versions using:

```bash
pip freeze
```

or:

```bash
conda env export
```

## Running R Markdown scripts

To render an R Markdown file from the terminal, use:

```bash
Rscript -e "rmarkdown::render('1-code/ruwen/interaction_thosai_bifi.Rmd')"
```

Replace the file path with the script you want to run.

For example:

```bash
Rscript -e "rmarkdown::render('1-code/ruwen/metabolic_adjust.Rmd')"
```

## Running Jupyter notebooks

If `.ipynb` notebooks are used, start Jupyter from the repository root:

```bash
jupyter notebook
```

or:

```bash
jupyter lab
```

Then open the relevant notebook and run cells in order.

## Data availability

This repository contains analysis code only. Individual-level HELIOS cohort data are not included because they may contain sensitive participant-level information and are subject to cohort governance, ethics, and data-sharing restrictions.

Data access should follow the relevant HELIOS cohort data access policies. Public sequencing data accession numbers or processed data links should be added here when available.


## Suggested citation

If using this repository, please cite the associated preprint:

Zhou R, et al. **Distinct Gut Microbiome Signatures in Ethnically Diverse Populations within a Shared Urban Asian Geography**. *medRxiv*. 2026.  
Preprint DOI/link: https://www.medrxiv.org/content/10.64898/2026.02.06.26345736v1

Please note that this manuscript is currently available as a preprint and is under peer review.

## License

This repository is distributed under the MIT License. See `LICENSE` for details.

## Contact

For questions about the code or analyses, please contact the repository maintainers or the corresponding authors of the associated manuscript.

