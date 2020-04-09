# Global_Soil_Moisture
Codes to reproduce results of manuscript: Gap-Free Global Annual Soil Moisture: 15km Grids for 1991–2016 by Guevara, Taufer and Vargas

Preprint: Guevara et al. Gap-Free Global Annual Soil Moisture: 15km Grids for 1991–2016: https://doi.org/10.5194/essd-2019-191
Software: R
Inputs: https://www.earth-syst-sci-data-discuss.net/essd-2019-191/
Prepared by Mario Guevara
mguevara@udel.edu

Project: CIF21 DIBBs: PD: Cyberinfrastructure Tools for Precision Agriculture in the 21st Century
PIs: Michela Taufer and Rodrigo Vargas
Funding agency: NSF award #1724843
https://www.nsf.gov/awardsearch/showAward?AWD_ID=1724843&HistoricalAwards=false


Workspace contains the following codes: 

a) ESSD_reanalysis_eco_dummy_1991_2018_yearly_v0: 

-Prediction model based on KKNN using as training data the ESA-CCI yearly means and as covariates the bioclimatic features from the FAO Agroecological areas 


b) ESSD_reanalysis_allCovs_1991_2018_yearly_v0.R: 

-Prediction model based on KKNN using as training data the ESA-CCI yearly means and as covariates terrain parameters, bioclimatic features and soil typle classes


c) ESSD_reanalysis_obliqueCOORDS_1991_2018_yearly_v0.R

-Prediction model based on KKNN using as training data the ESA-CCI yearly means and as covariates a system of n=6 tilted spatial coordinates across different angles.


d) Compressed folder (ismn_prep_example.tar.xz) containing the codes and examples to organize the data from the ISMN in a yearly basis. 

All inputs and outputs are available at: 
https://www.hydroshare.org/resource/b940b704429244a99f902ff7cb30a31f/
