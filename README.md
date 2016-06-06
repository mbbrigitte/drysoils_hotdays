# Code for papers relating dry soils and hot days
##IDL-Code from Paper at ETH Zurich: 
Hirschi M., B. Mueller, W. Dorigo and S. I. Seneviratne (2014): Using remotely sensed soil moisture for land-atmosphere coupling diagnostics: The role of surface vs. root-zone soil moisture variability. Remote Sensing of Environment,  doi:10.1016/j.rse.2014.08.030. 

##Structure:
[corr_SPI_HD.pro](https://github.com/mbbrigitte/drysoils_hotdays/blob/master/corr_SPI_NHD.pro) 
This function can read in any of the SPI-like and HotDay-like data used for both Hirschi et al. (2014) and Mueller and Seneviratne (2012). corr_SPI_HD.pro saves the correlations in NetCDF files for future reference and also creates the plots that appear in the Hirschi-paper. There is a description what the function does and how to properly call it in the header. 
The code uses the sub-functions in this directory: [read_GLDAS.pro](https://github.com/mbbrigitte/drysoils_hotdays/blob/master/read_GLDAS.pro), [read_HD.pro](https://github.com/mbbrigitte/drysoils_hotdays/blob/master/read_HD.pro) and [read_monthly_EI_temp.pro](https://github.com/mbbrigitte/drysoils_hotdays/blob/master/read_monthly_EI_temp.pro) to read in the data needed.  


##Sub-Codes - Calculating Hot Days:



##Bibliography
Hirschi M., B. Mueller, W. Dorigo and S. I. Seneviratne (2014): Using remotely sensed soil moisture for land-atmosphere coupling diagnostics: The role of surface vs. root-zone soil moisture variability. Remote Sensing of Environment,  doi:10.1016/j.rse.2014.08.030. 

Mueller, B. and S.I. Seneviratne (2012): Hot days induced by precipitation deficits at the global scale. Proceedings of the National Academy of Sciences of the United States of America, 109 (31), 12398-12403, doi: 10.1073/pnas.1204330109

