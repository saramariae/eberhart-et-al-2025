# The internationalization of renewable energy finance
Sara Eberhart, Tobias S. Schmidt, Bjarne Steffen, Florian Egli

ETH Zurich, TU Munich

Maintained by: Sara Eberhart <sara.eberhart@gess.ethz.ch>

### Summary
Mitigating climate change necessitates vast investments into clean energy technologies globally, requiring internationalization not only of knowledge, production, and policies but also finance. This paper examines 42,291 renewable energy investment deals across OECD countries from 2004 to 2022, revealing highly international capital flows. Results show that $US 45.4B annually (45%) is invested across borders, with varying degrees between countries. Further, renewable energy investments increasingly mirror general foreign direct investment (89% correlation), indicating financial mainstreaming, which can contribute to rapid deployment. These results are primarily driven by onshore wind and solar photovoltaic (PV). While all technologies experienced increased international capital mobility, offshore wind leads with 76% of capital exiting borders, whereas biomass is largely financed domestically (36% international). In sum, while international capital mobility has been crucial for growth in RE deployment, substantial country and technology differences exist. Our findings offer learnings for novel low-carbon technologies and inform future research.

### Code descripton
This repository contains the data preparation and imputation for Eberhart et al. (2025)

### Files
- `assetfinance.xlsx`: This dataset contains the merged project and finance data from BNEF. Given this is proprietary data, the data is not included but only the column headers.
- `organizations_classified.xlsx`: This dataset contains the organizations data from BNEF, including the classification into investor types as described in the paper (mapping files available in the supplementary material). Given this is proprietary data, the data is not included but only the column headers
- `US_CPI_annual.xlsx`: This file contains the inflation data for the US. This is public data and available from the World Bank, thus this file only includes the structure of the data for ease of code implementation.
- `countries_regions_match_IRENA.xlsx`: This file contains the mapping of countries into regions and specifically the OECD countries, according to the IRENA methodology.
