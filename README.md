# Housing Discrimination and the Pollution Exposure Gap in the United States

Data and replication files for "Housing Discrimination and the Pollution Exposure Gap in the United States" by  [Christensen](https://www.uiuc-bdeep.org/christensen),  [Sarmiento-Barbieri](https://ignaciomsarmiento.github.io/) and  [Timmins](https://sites.duke.edu/christophertimmins/)

# Abstract

Local pollution exposures disproportionately impact minority households, but the root causes remain unclear. This study conducts a correspondence experiment on a major online housing platform to test whether housing discrimination constrains minority access to housing options in markets with significant sources of airborne chemical toxics.  We find that renters with African American or Hispanic/LatinX names are 41% less likely than renters with White names to receive responses for properties in low exposure locations.  We find no evidence of discriminatory constraints in high exposure locations, indicating that discrimination increases relative access to housing choices at elevated exposure risk.  



## Data files

- toxic_discrimination_data.dta: experimental data set that includes information from 19 ZIP codes drawn at random from the set of high emissions markets. Listing location was then matched to EPA 810m grid with the 2016 aggregated microdata to obtain RSEI Toxic Concentration, RSEI Cancer Score, and RSEI Non Cancer Score. RSEI data are publicly available from the EPA's ftp server at ftp://newftp.epa.gov/RSEI/. Rent, Poverty Rate, Share of African American, Share of Whites, Share of Hispanics, Population in Block Group, Share of College Educated, Unemployment Rate come from the 2016 ACS block group data available from the US Census.
- ACS_population.dta: 2016 ACS block group data on renters 
- Potential_zips.csv: 111 ZIP codes that are above the 80th percentile of TRI stack air releases. This was calculated using 2016 TRI data available from [https://www.epa.gov/toxics-release-inventory-tri-program/tri-basic-data-files-calendar-years-1987-2018](https://www.epa.gov/toxics-release-inventory-tri-program/tri-basic-data-files-calendar-years-1987-2018)
- zipcodes.tar.gz shapefile of US ZIP codes from the US Census. 

## Code files:

- The analysis is conducted using Stata-15 version 15.1 and R version 4.0.2 (2020-06-22) software

- All the code was run on a MacBookPro 2020 running macOS Catalina Version 10.15.6

- Previous to running the command users should add the  files`disc_boot.ado` `disc_boot_logit.ado` files located in the ado folder into their  Stata local ado file location. These ado files implement the  Kline and Santos (2012) standard errors correction.
 
- main.sh: Contains the sequence of execution of dofiles and Rscripts to produce the figures and tables in the paper and appendix. Dofiles and Rscripts are named according to the function they perform
	- The folder dofiles contains the dofiles called by main.sh:
		- 1_estimates_figure2.do
		- 2_estimates_figure3.do
		- 3_estimates_figure3.do
		- 3_estimates_figure4.do
		- 4_estimates_figure5.do
		- 5_estimates_figure5.do
		- 5_estimates_figureA1.do
		- 6_estimates_figureA2.do
		- 7_Table_A2.do
		- 8_Table_A4.do
		- 9_table_A5.do
		- 10_Table_A6.do
		- 11_Table_A7.do
		- 12_Table_A8.do
		- 13_Table_A9.do
		- 14_Table_A10.do
		- 15_Table_A11.do
		- 16_Table_A12.do
	- The folder Rscripts contains the Rscripts called by main.sh:
		- 1_Figure_1a.R
		- 2_Figure_2.R
		- 3_Figures_Odds.R
		- 4_Figures_Odds_App.R
		- 4_Figures_Odds_Appendix.R
		- 5_TableA2.R
		- 6_Figure_A4.R
		- 7_Figure_A5.R
		- 8_TableA10.R
		- aux folder includes auxiliary files for plots and generating matched sample
			- gen_matched_sample.R
			- plot_fct_tox_conc_n_distance.R
	- main.sh creates:
		- temporary auxiliary data frames that are saved in stores/aux and deleted at the end of the script
		- a folder called logs is created and contains log files: .log for Stata  and .Rout for R


Figures and tables are saved in the "views" folder. 


Additional Notes:

-  Scripts to reproduce Figures 1 and A3 are not released. To preserve confidentiality and in compliance with IRB we don't release identifying information of  listings, e.g., Address, geolocation, etc.
-  Table A1 can be constructed from the file Potential_zips.csv
-  Table A2 rows of share of listings must be manually added from Stata output
-  Table A5, column (5) Stata prints to latex the incorrect significance stars, must be corrected "by hand"
-  Table A10 is printed in 2 parts: tableA10_a.tex, percentile results, and tableA10_b.tex, distance results.
