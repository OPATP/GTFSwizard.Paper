This repository contains the scripts and data used to run the research experiment titled **"GTFSwizard: A set of tools for exploring and manipulating General Transit Feed Specification in R language"**. The study employs GTFS data and R scripts to evaluate the impact of operational changes in a public transport system.

## Repository Structure

- **`data/`**: Contains input and processed data files.
- **`scripts/`**: Includes the only R scripts used for data preparation, analysis, and visualization.
- **`figs/`**: Stores output figures and visualizations.

## Prerequisites

- R (≥ 4.0)
- Required R packages:
  - `GTFSwizard`
  - `aopdata`
  - `tidyverse`
  - `sf`
  - `data.table`
  - `parallel`
  - `viridis`

## Cloning the Repository

Clone this repository to your local machine:
```bash
git clone https://github.com/OPATP/GTFSwizard.Paper.git
cd GTFSwizard.Paper
```

Run scripts:
```bash
Rscript R/GTFSwizard2025.R
```

## Methodology

The experiment evaluates transit interventions using:
- **Corridor prioritization**: Adjusting speeds on key corridors.
- **Hub enhancements**: Reducing dwell times at major hubs.
- **Frequency improvements**: Introducing new trips to reduce headways.

Accessibility is measured using zone-to-zone travel times derived from GTFS data processed by the `GTFSwizard` package. A Gaussian decay function weighs job accessibility based on travel times. Land use data comes from the [Access to Opportunities](https://github.com/ipeaGIT/aopdata) project.

## Results

Results include:
- Maps of accessibility before and after interventions.
- Statistical comparisons using metrics such as the Palma Ratio and Gini Index.
- Scenario summaries highlighting impacts on equity and operational costs.


## References

- For GTFSwizard documentation [click here](https://CRAN.R-project.org/package=GTFSwizard)!
- Paper to be published.
