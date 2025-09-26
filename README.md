Lancet Countdown Europe 2026 urban scale (special report): Wildfire smoke indicator
This repository contains the code to generate the wildfire smoke indicators for the Lancet Countdown Europe 2026 urban scale special edition, as well as the figures and tables included in the report and appendix. Please refer to the appendix file for a complete description of the methods.

Analysis
The workflow to compute the indicators is structured in a sequential set of steps that can be run from a root makefile script. Note that some of the steps were performed in a High Performance Computing (HPC) cluster due to memory usage. The steps to be run are:

1. City boundary data processing: Assign city and region codes to each of the exposure geometries (HPC).
2. Mortality data processing: Clean EUROSTAT mortality time series at NUTS3, merge it with the LCDE urban scale provided annual  (constant) mortality, apply weekly trends of EUROSTAT to constant data, and disaggregation to daily counts.
3. Exposure data processing: Extract daily exposures, population and city codes and aggregate population counts (HPC).
4. Exposure analysis: Construct wildfire-PM2.5 exposure subindicator (HPC).
5. Health Impact Assessment (HIA) analysis: Construct attributable mortality subindicator (HPC).
6. Fire weather index analysis: Construct Forest Fire Weather Index (FWI) subindicator.
7. Tables and figures: Code to generate the figures and tables included in the report.


Preparation
The indicator uses a set of open source datasets (city and regional boundaries, mortality time series, gridded population estimates) that can be easily obtained from Eurostat, Urban Audit and local data sources. The city boundaries, population and time series data and its sources are further described in the report's appendix. Those should be placed in the folder data/raw/boundaries, data/raw/regions, data/raw/population, data/raw/mortality, respectively. These are not included in the repository due to large file sizes.

Furthermore, the indicator uses wildfire-PM2.5 exposure data derived from the Integrated System for wild-land Fires (IS4FIRES) and the System for Integrated modelling of Atmospheric composition (SILAM) models, as well as Forest fire Weather Index (FWI) information computed using ERA5 data. Please contact the authors of the indicator (Lara.Stucki@isglobal.org, Mikhail.Sofiev@fmi.fi, Risto.Hanninen@fmi.fi, Cathryn.Tonne@isglobal.org) for details on how to get access to the data. SILAM data should be placed in the data/raw/SILAM folder and fire danger data should be placed in the data/raw/FWI folder.

This code was originally written by Carles Milà adapted for each new report.

This code was run using R version 4.4.0 with the packages specified in the makefile script for the analysis part, as well as the visualization packages included in the tables and figures script.
