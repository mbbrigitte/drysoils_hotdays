# Code for papers relating dry soils and hot days
##IDL-Code from Paper at ETH Zurich: 
Hirschi M., B. Mueller, W. Dorigo and S. I. Seneviratne (2014): [Using remotely sensed soil moisture for land-atmosphere coupling diagnostics: The role of surface vs. root-zone soil moisture variability.](http://www.sciencedirect.com/science/article/pii/S003442571400337X) Remote Sensing of Environment,  doi:10.1016/j.rse.2014.08.030. 

##Structure:
*:arrow_right: [corr_SPI_HD.pro](https://github.com/mbbrigitte/drysoils_hotdays/blob/master/corr_SPI_NHD.pro)
  *:leftwards_arrow_with_hook: [read_GLDAS.pro](https://github.com/mbbrigitte/drysoils_hotdays/blob/master/read_GLDAS.pro)
  *:leftwards_arrow_with_hook: [read_HD.pro](https://github.com/mbbrigitte/drysoils_hotdays/blob/master/read_HD.pro)
  *:leftwards_arrow_with_hook: [read_monthly_EI_Temp.pro](https://github.com/mbbrigitte/drysoils_hotdays/blob/master/read_monthly_EI_Temp.pro)


[corr_SPI_HD.pro](https://github.com/mbbrigitte/drysoils_hotdays/blob/master/corr_SPI_NHD.pro) 
This function can read in any of the SPI-like and HotDay-like data used for both Hirschi et al. (2014) and Mueller and Seneviratne (2012). corr_SPI_HD.pro saves the correlations in NetCDF files for future reference and also creates the plots that appear in the Hirschi-paper. There is a description what the function does and how to properly call it in the header. 

The code uses the sub-functions in this directory: [read_GLDAS.pro](https://github.com/mbbrigitte/drysoils_hotdays/blob/master/read_GLDAS.pro), [read_HD.pro](https://github.com/mbbrigitte/drysoils_hotdays/blob/master/read_HD.pro) and [read_monthly_EI_Temp.pro](https://github.com/mbbrigitte/drysoils_hotdays/blob/master/read_monthly_EI_Temp.pro) to read in the data needed.  


##Sub-Codes - Calculating Hot Days:
:leftwards_arrow_with_hook: [heatw_temp_percentiles_5d.pro](https://github.com/mbbrigitte/drysoils_hotdays/blob/master/heatw_temp_percentiles_5d.pro),prepare_daily_temp.pro](https://github.com/mbbrigitte/drysoils_hotdays/blob/master/prepare_daily_temp.pro)

[heatw_temp_percentiles_5d.pro](https://github.com/mbbrigitte/drysoils_hotdays/blob/master/heatw_temp_percentiles_5d.pro) reads in the daily temperature data from different sources that you can choose with a keyword, and then calculates the number of days where the temperature is above the 90-th-percentile of the longterm 5 days-value around the date (not averaged, just the 5 values). If exceedance keyword is used, it will calculate the Temperature Exceedance in K - used in the Supplementary material of Mueller-paper - instead of the number of days.  
To prepare the downloaded ERA-Interim or other sources temperature, the code [prepare_daily_temp.pro](https://github.com/mbbrigitte/drysoils_hotdays/blob/master/prepare_daily_temp.pro) extracts the maximum daily temperature from several (usually 4) values a day and saves them in NetCDF to be read in and used in heatw_temp_percentiles_5d.pro.


##Bibliography
Hirschi M., B. Mueller, W. Dorigo and S. I. Seneviratne (2014): [Using remotely sensed soil moisture for land-atmosphere coupling diagnostics: The role of surface vs. root-zone soil moisture variability.](http://www.sciencedirect.com/science/article/pii/S003442571400337X) Remote Sensing of Environment,  doi:10.1016/j.rse.2014.08.030. 

Mueller, B. and S.I. Seneviratne (2012): [Hot days induced by precipitation deficits at the global scale.](http://www.pnas.org/content/109/31/12398.full?sid=1134f824-548e-4797-aeb9-3cfbf40f4931) Proceedings of the National Academy of Sciences of the United States of America, 109 (31), 12398-12403, doi: 10.1073/pnas.1204330109

