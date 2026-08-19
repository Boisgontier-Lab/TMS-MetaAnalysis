# TMS-MetaAnalysis
**Authors:** Ata Farajzadeh, Ian M. Lahart, Matthieu P. Boisgontier

**Contacts:** ata.farajzadeh@uottawa.ca, matthieu.boisgontier@uottawa.ca

## Description

Code and data for the article: "Physical activity and motor cortical neurophysiology: a meta-analysis"

## Usage

1. Run the file `main.Rmd`
2. Output plots will be saved to `main.pdf`

## File Structure

- `data/` : Raw and processed data used for analyses (CSV files: `excitability.csv`, `facilitation.csv`, `inhibition.csv`). This folder also includes `Farajzadeh_2026_PhysicalActivity-TMS.csv`, which is the dataset collected by our own laboratory and refered to as "Farajzadeh A. 2026. Dataset physical activity and TMS." in the reference list of the article.
- `R/` : R scripts for package installation, preprocessing, analysis, figure generation, and the main document `main.Rmd`.
