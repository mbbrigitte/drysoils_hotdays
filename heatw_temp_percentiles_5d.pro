PRO heatw_temp_percentiles_5d,era40=era40,lowres=lowres,CFSR=CFSR,MERRA=MERRA,EOBS=EOBS,excess=excess

;PURPOSE: reads in the data that was outputfrom prepare_daily_temp.pro, then
; 	calculates the number of days where the temperature is 
; 	above the 90-th-percentile of the longterm 5 days-value around the date
;        which is not averaged! 
;	The progamm first transforms the data: it adds NaN to all months 
;	which are shorter than 31 days
;       
;	
;KEYWORDS: ,/era40 if you want to calculate HD for ERA-40 data, 
;           default is ERA-Interim temperature data.
;	   ,/lowres for low resolution data, ,/CFSR for CFSR temperature etc.
;	   ,/excess calculates the excess temperature in K instead of the number of days.

;OUTPUT: See above, file NetCDF: /data/brmuelle/Temperature/ERA_Interim/daily/NetCDF/2m_temp_ano_stdev.nc
; 	 updated from heatw_temperature_output.pro
;	 on 16.05.2011 bm, cleaned a bit on 6.6.2016 for sharing but not much, 
; -------------------------------------START-----------------------------------------------------

CPU, TPOOL_NTHREADS=1 ; otherwise, 500% of CPU will be used and the process slows down 

startyear=1979
nm=12 ; not for 2011
lon_var = 'LON'
lat_var = 'LAT'
tim_var = 'TIME'
nyears=33 & ny=33 ;=2010-1979+1

; name of outfile
filename='/brmuelle/temp_numberof90thpercent_5d2011_a.nc' 
filenameall='/brmuelle/test_yearly_numberof90thpercent_5d2011_a.nc' 

IF keyword_set(excess) THEN BEGIN
  filename='/net/firebolt/data/brmuelle/Temperature/ERA_Interim/daily/PERCENTILE/temp_total_degreesabove90thpercent_5d.nc' 
  extrafilename='/net/firebolt/data/brmuelle/Temperature/ERA_Interim/daily/PERCENTILE/temp_degreesabove90th.nc' 

  filenameall='/net/firebolt/data/brmuelle/Temperature/ERA_Interim/daily/PERCENTILE/test_yearly_degreesaboveof90thpercent_5d.nc' 
ENDIF


IF keyword_set(lowrew) THEN filename='/net/firebolt/data/brmuelle/Temperature/ERA_Interim/daily/PERCENTILE/temp_numberof90thpercent_5d_lowres.nc'


IF keyword_set (era40) THEN BEGIN
   startyear=1959
   nm=12 ; not for 2002
   lon_var = 'LON'
   lat_var = 'LAT'
   tim_var = 'TIME'
   nyears=2001-1959+1 & ny=2001-1959+1 ;=2001-1959+1
   filenameall='/net/firebolt/data/brmuelle/Temperature/ERA-40/HD/test_yearly_numberof90thpercent_5d.nc' 
   ; name of outfile
   filename='/net/firebolt/data/brmuelle/Temperature/ERA-40/HD/temp_numberof90thpercent_5d.nc'
ENDIF ; keyword era40

IF keyword_set(CFSR) THEN BEGIN     
      ; Set Ocean to NaN in order to save disk-space:
      ; Read the land-sea mask to mask out the ocean:
      filein='/net/firebolt/data/brmuelle/CFSR_NCEP/DATA/LandCover.nc'
      cdfid=ncdf_open(filein)		        ;open the netcdf file "filein"
      varid=ncdf_varid(cdfid, 'LAND_L1')	;data read
      ncdf_varget, cdfid, varid, lsm_old
      varid=ncdf_varid(cdfid, 'lon')		;data read
      ncdf_varget, cdfid, varid, lon_grid
      varid=ncdf_varid(cdfid, 'lat')		;data read
      ncdf_varget, cdfid, varid, lat_grid
      ncdf_close,cdfid
      seaindex=WHERE(lsm_old eq 0)
      filename='/net/firebolt/data/brmuelle/CFSR_NCEP/NetCDF/NHD/CFSR_temp_no_90thpercent_5d.nc' 
      filenameall='/net/firebolt/data/brmuelle/CFSR_NCEP/NetCDF/NHD/CFSR_test_yearly_numberof90thpercent_5d.nc' 
ENDIF

IF keyword_set(MERRA) THEN BEGIN     
      filename='/net/firebolt/data/brmuelle/ET_MERRA/NetCDF_Temperature/NHD/MERRA_temp_no_90thpercent_5d.nc' 
      filenameall='/net/firebolt/data/brmuelle/ET_MERRA/NetCDF_Temperature/NHD/MERRA_test_yearly_numberof90thpercent_5d.nc' 
ENDIF

IF keyword_set(EOBS) THEN BEGIN     
      filename='/net/firebolt/data/brmuelle/Temperature/E-OBS/HotDays/E-Obs_temp_no_90thpercent_5d.nc' 
      filenameall='/net/firebolt/data/brmuelle/Temperature/E-OBS/HotDays/E-Obs_test_yearly_numberof90thpercent_5d.nc' 
ENDIF
    
IF keyword_set(EOBS) THEN BEGIN
      file='/net/firebolt/data/brmuelle/Temperature/E-OBS/DATA/tx_0.25deg_reg_v5.0.nc'
      result=file_test(file)
      if (result eq 0) then stop
      cdfid=ncdf_open(file)		;open netcdf "file"
      varid=ncdf_varid(cdfid, 'tx')	
      ncdf_varget, cdfid, varid, ev 
      varid=ncdf_varid(cdfid, 'longitude')	;readin longitude
      ncdf_varget, cdfid, varid,lon	
      varid=ncdf_varid(cdfid, 'latitude')	;readin  latitude
      ncdf_varget, cdfid, varid, lat
      varid=ncdf_varid(cdfid, 'time')	;readin  latitude
      ncdf_varget, cdfid, varid, time
      ncdf_close,cdfid
      ntall=N_ELEMENTS(time)
      nlon=N_ELEMENTS(lon)
      nlat=N_ELEMENTS(lat)
      ; cut it at 1979:
        ; Create an array of years from 1980 to 1998
      years = findgen(1979-1950) + 1950
      ndays = 365 + isleap(years)
      stind79=TOTAL(ndays)
     years = findgen(ny) + 1979   ;32 years
;    ; Compute the number of days for each year
      ndays = 365 + isleap(years)
      totdays=TOTAL(ndays)
      endind=stind79+totdays-1
      tempmax=ev(*,*,stind79:endind)
      ev=0
      ; cut out the 29 februaries
      FOR ntim=0L,32-1 DO BEGIN
	 dat=tempmax(*,*,TOTAL(ndays(0:ntim))-ndays(ntim):TOTAL(ndays(0:ntim))-1)
	 IF n_elements(dat(0,0,*)) EQ 366 THEN BEGIN
	    dat1=dat(*,*,0:31+27) & dat2=dat(*,*,31+29:365)
	    dat=[[[dat1]],[[dat2]]]
	 ENDIF
	 IF (ntim EQ 0)  THEN BEGIN ; initalizing
	    new_array=dat
	 ENDIF ELSE BEGIN ; append ev to each array
	    new_array = [[[new_array]],[[dat]]]
	 ENDELSE
      ENDFOR
    
ENDIF ELSE BEGIN ; for all not E-Obs datasets, loop and read in data:

   FOR i=0,ny-1 DO BEGIN       ;years
   FOR j=0,nm-1 DO BEGIN    ;months season
      a=startyear+i
      b=1+j
      ; -------------------------------------------------------------
      ;          read data (maximum of either 00:00,6,h 12: or 18:00h)
      ; --------------------------------------------------------------
      IF NOT keyword_set(era40) THEN file='/brmuelle/2m_highres_temp_maxofday'+string(a,format='(i4)')+'_'+string(b,format='(i2.2)')+'.nc'
      IF keyword_set(era40) THEN file='/net/firebolt/data/brmuelle/Temperature/ERA-40/NetCDF_regular/2m_highres_temp_maxofday06'+string(a,format='(i4)')+'_'+string(b,format='(i2.2)')+'.nc'
      IF keyword_set(CFSR) THEN file='/net/firebolt/data/brmuelle/CFSR_NCEP/NetCDF/MaxTemp/CFSR_maxofday'+string(a,format='(i4)')+'_'+string(b,format='(i2.2)')+'.nc'
      IF keyword_set(MERRA) THEN	file='/net/firebolt/data/brmuelle/ET_MERRA/NetCDF_Temperature/MaximumTemp/MERRA_maxofday'+string(a,format='(i4)')+'_'+string(b,format='(i2.2)')+'.nc'
      print, file
      result=file_test(file)
      if (result eq 0) then stop
      cdfid=ncdf_open(file)		;open netcdf "file"
      varid=ncdf_varid(cdfid, 'TEMP')	
      ncdf_varget, cdfid, varid, ev 
      varid=ncdf_varid(cdfid, lon_var)	;readin longitude
      ncdf_varget, cdfid, varid,lon	
      varid=ncdf_varid(cdfid, lat_var)	;readin  latitude
      ncdf_varget, cdfid, varid, lat
      varid=ncdf_varid(cdfid, tim_var)	;readin  latitude
      ncdf_varget, cdfid, varid, time
      ncdf_close,cdfid
      ntall=N_ELEMENTS(time)
      nlon=N_ELEMENTS(lon)
      nlat=N_ELEMENTS(lat)
      help, ev
      
      IF keyword_set(CFSR) THEN ev(seaindex)=!Values.F_NaN
      
      
      IF keyword_set(lowres) THEN BEGIN
      eva=FLTARR(96,72,ntall)
	 FOR timlop=0L,ntall-1 DO BEGIN
	    nlon=96 & nlat=72
	    eva(*,*,timlop)=CONGRID(ev(*,*,timlop),nlon,nlat)
	 ENDFOR
      ev=eva
      ENDIF
      
      ; cut the 29th februaries out of the data to be able to reform per year!!!
      IF ntall EQ 29 THEN ev=ev(*,*,0:ntall-2)

      IF ((i EQ 0) AND (j EQ 0)) THEN BEGIN ; initalizing
	 new_array=ev
      ENDIF ELSE BEGIN ; append ev to each array
      new_array = [[[new_array]],[[ev]]]
      ENDELSE
      
      IF  (j EQ 11) THEN print, N_elements(new_array(0,0,*))-i*365,'in year',a
      IF ((j EQ 11) AND (N_elements(new_array(0,0,*))-i*365 NE 365)) THEN stop

   ENDFOR ; month
   ENDFOR ;year 
ENDELSE ; MERRA, ERA-Interim, etc.
   
ntim=N_elements(new_array(0,0,*))
fiveavgs=FLTARR(nlon,nlat,ntim,5)
add=FLTARR(nlon,nlat,1)
add(*)=!Values.F_NaN
add=REFORM(add,nlon,nlat,1)
print, 'entering reform-loop'
   FOR tp=0L,ntim-1 DO BEGIN
      IF tp EQ 1 THEN BEGIN ; first feb
         dat=[[[new_array(*,*,tp-1:tp+2)]],[[add]]]
      ENDIF ELSE BEGIN
         IF tp EQ 0 THEN BEGIN ; first january
           dat=[[[new_array(*,*,tp:tp+2)]],[[add]],[[add]]]
         ENDIF ELSE BEGIN
            IF tp EQ ntim-2 THEN BEGIN ; second last december
              dat=[[[new_array(*,*,tp-2:tp+1)]],[[add]]]
            ENDIF ELSE BEGIN
               IF tp EQ ntim-1 THEN BEGIN
                  dat=[[[new_array(*,*,tp-2:tp)]],[[add]],[[add]]]
               ENDIF ELSE BEGIN
               dat=new_array(*,*,tp-2:tp+2)
               ENDELSE
            ENDELSE
         ENDELSE
      ENDELSE
      fiveavgs(*,*,tp,*)=dat
   ENDFOR
   
refdata=REFORM(new_array,nlon,nlat,365,ntim/365)
reffiveavgs=REFORM(fiveavgs,nlon,nlat,365,ntim/365,5)
percyesno=FLTARR(nlon,nlat,365,ntim/365)
excesstemp=FLTARR(nlon,nlat,365,ntim/365)
excesstemp(*)=!Values.F_NaN
print, 'entering nlonxnlatx365x2 loop'
FOR lo=0,nlon-1 DO BEGIN ; longitude loop
   FOR la=0,nlat-1 DO BEGIN ; latitude loop
      FOR dayl=0L,365-1 DO BEGIN ; day loop (if reffiveavgs than over the 5 day average automatically)  
         datatoper=REFORM(reffiveavgs(lo,la,dayl,*,*))
         testind=WHERE((FINITE(datatoper) EQ 1),ntest)
         IF ntest GT 4 THEN BEGIN ; at least 4 years should have this days value:
            percdata=datatoper(testind)
            percentile=prank(percdata,90); = Data((SORT(Data)) (90*N_ELEMENTS(Data)/100) )
         ENDIF ELSE BEGIN
            percentile=!Values.F_nan
         ENDELSE
         FOR o=0,ntim/365-1 DO BEGIN            ; yearsloop
               ; this then should actually be fine
          IF refdata(lo,la,dayl,o) GT percentile THEN BEGIN
           percyesno(lo,la,dayl,o)=1
           excesstemp(lo,la,dayl,o)=refdata(lo,la,dayl,o)-percentile
          ENDIF; ELSE BEGIN
         ENDFOR
      ENDFOR
   ENDFOR
ENDFOR
print, 'end of the big loop, starting writing the outfile'
IF keyword_set(excess) THEN percyesno=excesstemp


y_data=TOTAL(percyesno,3) ; lo,la,yr
var_new=create_struct('N90P',y_data)
lon_arr=lon
lat_arr=lat
tim_arr2=findgen(ny)+startyear
dim_new=create_struct('lon',float(lon_arr),'lat',float(lat_arr),'years',float(tim_arr2))
attributes=create_struct('var1',['Temperature','Number of days higher than 90thpercentile of 5 days other years'],'dim1',['longitude','degrees_east'],'dim2',['latitude','degrees_north'],'dim3',['years','starting 1979'])
put_cdf_var,filenameall,ncdim=dim_new,ncdata=var_new,attnames=attributes,/float

; Then transform the data to 31 each month*12, nyears
alldata=FLTARR(nlon,nlat,31*12,ntim/365)
alldata(*)=!Values.F_NaN

FOR yi=0L,ntim/365-1 DO BEGIN ; year loop
   daysmonth=[31,28,31,30,31,30,31,31,30,31,30,31]
   firstyear=percyesno(*,*,*,yi)
   firstyear_out=FLTARR(nlon,nlat,31*12)
   firstyear_out(*)=!Values.F_NAN
   cumdays=TOTAL(daysmonth,/cumulative)
   endind=cumdays
   ;endind=[31.0000,59.0000,90.0000,120.000,151.000,181.000,212.000,243.000,273.000,304.000,334.000,365.000]
   stind=endind-daysmonth

   FOR nf=0L, 11 DO BEGIN
      firstyear_out(*,*,(nf*31):(nf*31)+daysmonth(nf)-1)=firstyear(*,*,stind(nf):cumdays(nf)-1)
   ENDFOR
   alldata(*,*,*,yi)=firstyear_out
ENDFOR


malldata=REFORM(alldata,nlon,nlat,31,12,ntim/365)
IF keyword_set(excess) THEN BEGIN
  numberof90perc=AVERAGE(malldata,3,/NaN) ; since in the 3rd dimension, it is 1 if higher than percentile and 
  ; zero if not, so we can just sum the values up and will get the number of days with higher than 
  ; percentile in this month 
  ; if it is the temperature exceedance stuff, it will be the Kelvins higher than 90percentile added up in a month
  ; or take the maximum of the exceedance in the following line

  maxnumberof=MAX(malldata,DIMENSION=3,/NaN)
  var_new=create_struct('MaxExceedanceInThatMonth',maxnumberof)
  lon_arr=lon
  lat_arr=lat
  tim_arr1=findgen(nm)+1
  tim_arr2=findgen(ny)+startyear
  dim_new=create_struct('lon',float(lon_arr),'lat',float(lat_arr),'months',float(tim_arr1),'years',float(tim_arr2))
  attributes=create_struct('var1',['Temperature','Max Temp of hottest minus percentile every month if higher than 90thpercentile of 5 days other years'],'dim1',['longitude','degrees_east'],'dim2',['latitude','degrees_north'],'dim3',['months','starting January'],'dim4',['years','starting 1979'])
  put_cdf_var,extrafilename,ncdim=dim_new,ncdata=var_new,attnames=attributes,/float

  var_new=create_struct('Exc90P',numberof90perc)
  lon_arr=lon
  lat_arr=lat
  tim_arr1=findgen(nm)+1
  tim_arr2=findgen(ny)+startyear
  dim_new=create_struct('lon',float(lon_arr),'lat',float(lat_arr),'months',float(tim_arr1),'years',float(tim_arr2))
  attributes=create_struct('var1',['Temperature','Sum of exceedance temperature above 90percentile of 5 days other years'],'dim1',['longitude','degrees_east'],'dim2',['latitude','degrees_north'],'dim3',['months','starting January'],'dim4',['years','starting 1979'])
  put_cdf_var,filename,ncdim=dim_new,ncdata=var_new,attnames=attributes,/float
ENDIF ELSE BEGIN
  numberof90perc=TOTAL(malldata,3,/NaN) ; since in the 3rd dimension, it is 1 if higher than percentile and 
  ; zero if not, so we can just sum the values up and will get the number of days with higher than 
  ; percentile in this month 
  ; if it is the temperature exceedance stuff, it will be the Kelvins higher than 90percentile added up in a month
  ; or take the maximum of the exceedance in the following line

  var_new=create_struct('N90P',numberof90perc)
  lon_arr=lon
  lat_arr=lat
  IF keyword_set(lowres) THEN BEGIN
    lon_arr=INDGEN(96)*3.75
    lat_arr=-90+indgen(72)*2.5
  ENDIF
  tim_arr1=findgen(nm)+1
  tim_arr2=findgen(ny)+startyear
  dim_new=create_struct('lon',float(lon_arr),'lat',float(lat_arr),'months',float(tim_arr1),'years',float(tim_arr2))
  attributes=create_struct('var1',['Temperature','Number of days higher than 90thpercentile of 5 days other years'],'dim1',['longitude','degrees_east'],'dim2',['latitude','degrees_north'],'dim3',['months','starting January'],'dim4',['years','starting 1979'])
  put_cdf_var,filename,ncdim=dim_new,ncdata=var_new,attnames=attributes,/float
ENDELSE

stop
END

