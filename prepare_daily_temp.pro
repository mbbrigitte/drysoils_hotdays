PRO prepare_daily_temp,lowres=lowres,highres=highres,era40=era40,CFSR=CFSR,MERRA=MERRA

;PURPOSE: Reads the original ERA-Interim daily 2m Temperature data 
; 	  which is taken at the 12h-forecast step and at times 0 and 12'o clock
; 	  then takes always the maximum of the two times and stores it as an output netcdf
; 	  for the programm heatw_anomalies_output.pro


;KEYWORDS: ,/lowres will take the 1.5° Temperature 2-ANOMALIES
; 	   ,/highres will take the 0.25° Temperature 2m-Values (anomalies are
;	   calculated in follwoing prog anyway (heatw_anomalies_output.pro)
;	    ,/era40 if you want to prepare the era-40 temperature HD
; You do have to set a keyword!!

; HISTORY: added highres on 16 June 2011, bm
;          added era40 on 21 July 2011, bm
;          added CFSR keyword and conversion on 10 Feb 2012, bm
;          slight clean up for sharing version on 6 June 2016, bm

IF keyword_set(MERRA) THEN BEGIN
   nyr=32 ; number of years 1979-2010
   nm=12 ;  number of months
   ; read all data
  FOR i=0,nyr-1 DO BEGIN       ;years
   FOR j=0,nm-1 DO BEGIN    ;months
      a=2010+i
      b=1+j
      x=[31,28,31,30,31,30,31,31,30,31,30,31]
      IF isleap(a) eq 1 THEN x=[31,29,31,30,31,30,31,31,30,31,30,31]
      nt=x(j) ; last day of month according to actual month
      Dailytemp=FLTARR(540,361,nt) ;array to store daily values of single month

      FOR k=0L,nt-1 DO BEGIN   ; day loop
	 c=1+k
	 print, 'year',a,b,'day',c
	 file='/net/firebolt/data/brmuelle/ET_MERRA/Daily_temp_download/MERRA100.prod.assim.tavg1_2d_flx_Nx.'+string(a,format='(i4)')+string(b,format='(i2.2)')+string(c,format='(i2.2)')+'.SUB.nc'
	 
	 IF a GE 1993 THEN file='/net/firebolt/data/brmuelle/ET_MERRA/Daily_temp_download/MERRA200.prod.assim.tavg1_2d_flx_Nx.'+string(a,format='(i4)')+string(b,format='(i2.2)')+string(c,format='(i2.2)')+'.SUB.nc'
	 IF a GE 2001 THEN file='/net/firebolt/data/brmuelle/ET_MERRA/Daily_temp_download/MERRA300.prod.assim.tavg1_2d_flx_Nx.'+string(a,format='(i4)')+string(b,format='(i2.2)')+string(c,format='(i2.2)')+'.SUB.nc'
	 IF ((a GE 2010) and (b GE 6)) THEN file='/net/firebolt/data/brmuelle/ET_MERRA/Daily_temp_download/MERRA301.prod.assim.tavg1_2d_flx_Nx.'+string(a,format='(i4)')+string(b,format='(i2.2)')+string(c,format='(i2.2)')+'.SUB.nc'
	 IF ((a GE 2010) and (b GE 9)) THEN file='/net/firebolt/data/brmuelle/ET_MERRA/Daily_temp_download/MERRA300.prod.assim.tavg1_2d_flx_Nx.'+string(a,format='(i4)')+string(b,format='(i2.2)')+string(c,format='(i2.2)')+'.SUB.nc'

	 
	 missing_value = 1.e+15
	 lon_var = 'longitude'
	 lat_var = 'latitude'
	 tim_var = 'time'
	 ;test the file:
	 result=file_test(file)
	 if (result eq 0) then stop
	 cdfid=ncdf_open(file)           ;open netcdf "file"
	 varid=ncdf_varid(cdfid, 'tlml')       ;
	 ncdf_varget, cdfid, varid, ev 
	 varid=ncdf_varid(cdfid, lon_var)     ;readin longitude
	 ncdf_varget, cdfid, varid,lon        
	 varid=ncdf_varid(cdfid, lat_var)     ;readin  latitude
	 ncdf_varget, cdfid, varid, lat
	 varid=ncdf_varid(cdfid, tim_var)     ;readin  time (24h)
	 ncdf_varget, cdfid, varid, time 
	 ncdf_close,cdfid
	 index=where(ev eq missing_value,ncount)
	 if ncount gt 1 then ev(index)=!Values.F_NAN
      
	 ; nt number of days
	 nlon=N_ELEMENTS(lon)
	 nlat=N_ELEMENTS(lat)
	 ; loop over days an only consider the larger of the 24 daily values
	 
	 FOR lo=0,nlon-1 DO BEGIN
	       FOR la=0,nlat-1 DO BEGIN
		     dailytemp(lo,la,k)=MAX(ev(lo,la,*)) 
	       ENDFOR ; latitude
	 ENDFOR  ; longitude
      ENDFOR ; k (28 to 31 files per month)

       
      
     filename='/net/firebolt/data/brmuelle/ET_MERRA/NetCDF_Temperature/MaximumTemp/MERRA_maxofday'+string(a,format='(i4)')+'_'+string(b,format='(i2.2)')+'.nc'
     var_new=create_struct('Temp',dailytemp)
     lon_arr=lon
     lat_arr=lat
     tim_arr=findgen(nt)+1
     dim_new=create_struct('lon',float(lon_arr),'lat',float(lat_arr),'time',float(tim_arr))
     attributes=create_struct('var1',['2m-Daily max. temperature','K'],'dim1',['longitude','degrees_east'],'dim2',['latitude','degrees_north'],'dim3',['time','days starting at 1.January'])
     put_cdf_var,filename,ncdim=dim_new,ncdata=var_new,attnames=attributes,/float
   ENDFOR  ;months
  ENDFOR   ;years
  print, 'this is the end of the merra-loop'
ENDIF ELSE BEGIN ; end MERRA reanalysis max temp

IF keyword_set(CFSR) THEN BEGIN
   pirnt, 'now it is calculating CFSR max temp'
   nyr=32 ; number of years 1979-2010
   nm=12 ;  number of months
   ; read all data
  FOR i=0,nyr-1 DO BEGIN       ;years
   FOR j=0,nm-1 DO BEGIN    ;months
      a=1979+i
      b=1+j
      x=[31,28,31,30,31,30,31,31,30,31,30,31]
      IF isleap(a) eq 1 THEN x=[31,29,31,30,31,30,31,31,30,31,30,31]
      c1arr=[1,6,11,16,21,26] ;first day of 5-day values
      nt=x(j) ; last day of month according to actual month
      c2arr=[5,10,15,20,25,nt] ; last day of 5-day values - for name of input file

      Dailytemp=FLTARR(720,361,nt) ;array to store daily values of single month

      FOR k=0L,5 DO BEGIN   ; day loop
	 c1=c1arr(k) & c2=c2arr(k)
	 print, a,b,c1,'bis',c2
	 file='/net/firebolt/data/brmuelle/CFSR_NCEP/DATA/Temperature/pgbh06.gdas.'+string(a,format='(i4)')+string(b,format='(i2.2)')+string(c1,format='(i2.2)')+'-'+string(a,format='(i4)')+string(b,format='(i2.2)')+string(c2,format='(i2.2)')+'.grb2.nc'
	 missing_value =  3.4e+38
	 lon_var = 'lon'
	 lat_var = 'lat'
	 tim_var = 'time'
	 ;test the file:
	 result=file_test(file)
	 if (result eq 0) then stop
	 cdfid=ncdf_open(file)           ;open netcdf "file"
	 varid=ncdf_varid(cdfid, 'T_MAX_L103')       ;
	 ncdf_varget, cdfid, varid, ev 
	 varid=ncdf_varid(cdfid, lon_var)     ;readin longitude
	 ncdf_varget, cdfid, varid,lon        
	 varid=ncdf_varid(cdfid, lat_var)     ;readin  latitude
	 ncdf_varget, cdfid, varid, lat
	 varid=ncdf_varid(cdfid, tim_var)     ;readin  latitude
	 ncdf_varget, cdfid, varid, time 
	 varid=ncdf_varid(cdfid, 'valid_date_time')     ;readin  latitude
	 ncdf_varget, cdfid, varid, test
	 ncdf_close,cdfid
	 index=where(ev eq missing_value,ncount)
	 if ncount gt 1 then ev(index)=!Values.F_NAN
      
	 ; nt number of days
	 nlon=N_ELEMENTS(lon)
	 nlat=N_ELEMENTS(lat)
	 ; loop over days an only consider the larger of the 4 values (either at 00:00, 6: 12: or 18:00))
	 
	 IF ((a eq 2010) and (b eq 12) and (c1 eq 26)) THEN BEGIN ; for the last december 2010
	    FOR lo=0,nlon-1 DO BEGIN
	       FOR la=0,nlat-1 DO BEGIN
		  FOR t=0L, (c2-c1-1)  DO BEGIN
		     hourchose=ev(lo,la,t*4:t*4+3)
		     dailytemp(lo,la,t+5*k)=MAX(hourchose) 
		     IF t EQ 4 THEN BEGIN
		       dailytemp(lo,la,t+1+5*k)=MAX(hourchose)
		     ENDIF
		     IF ((lo EQ 0) and (la EQ 0)) THEN BEGIN
			print,'t from',t*4,'to',t*4+3
			print, 'outvariable-index',t+5*k
		     ENDIF
		  ENDFOR ;t 
	       ENDFOR ; latitude
	    ENDFOR  ; longitude
	 ENDIF ELSE BEGIN ; for all other cases
	    FOR lo=0,nlon-1 DO BEGIN
	       FOR la=0,nlat-1 DO BEGIN
		  FOR t=0L, (c2-c1)  DO BEGIN
		     hourchose=ev(lo,la,t*4:t*4+3)
		     dailytemp(lo,la,t+5*k)=MAX(hourchose) 
		     IF ((lo EQ 0) and (la EQ 0)) THEN BEGIN
			print,'t from',t*4,'to',t*4+3
			print, 'outvariable-index',t+5*k
		     ENDIF
		  ENDFOR ;t 
	       ENDFOR ; latitude
	    ENDFOR  ; longitude
	 ENDELSE
      ENDFOR ; k (6 files per month)
       
      
     filename='/net/firebolt/data/brmuelle/CFSR_NCEP/NetCDF/MaxTemp/CFSR_maxofday'+string(a,format='(i4)')+'_'+string(b,format='(i2.2)')+'.nc'
     var_new=create_struct('Temp',dailytemp)
     lon_arr=lon
     lat_arr=lat
     tim_arr=findgen(nt)+1
     dim_new=create_struct('lon',float(lon_arr),'lat',float(lat_arr),'time',float(tim_arr))
     attributes=create_struct('var1',['Temperature','K'],'dim1',['longitude','degrees_east'],'dim2',['latitude','degrees_north'],'dim3',['time','days starting at 1.January'])
     put_cdf_var,filename,ncdim=dim_new,ncdata=var_new,attnames=attributes,/float
   ENDFOR  ;months
  ENDFOR   ;years
print, 'This is the end of CFSR loop'
ENDIF ELSE BEGIN ; end CFSR reanalysis max temp
; For era-interim or era.interim maximum temperature:
IF KEYWORD_SET(lowres) THEN BEGIN 
   print, 'this takes ERA-Interim low resolution temperature'
   startyear=1989
   nm=12 ; not for 2011

   file='/data/brmuelle/Temperature/ERA_Interim/daily/NetCDF/2m_temp_12hfc.nc'
   missing_value = -32767
   scale_factor = 0.00203650650986529 
   add_offset = 258.948837822103 
   lon_var = 'longitude'
   lat_var = 'latitude'
   tim_var = 'time'
   ;test the file:
   result=file_test(file)
   if (result eq 0) then stop
   cdfid=ncdf_open(file)		;open netcdf "file"
   varid=ncdf_varid(cdfid, 't2m')
   ncdf_varget, cdfid, varid, ev 
   varid=ncdf_varid(cdfid, lon_var)	;readin longitude
   ncdf_varget, cdfid, varid,lon	
   varid=ncdf_varid(cdfid, lat_var)	;readin  latitude
   ncdf_varget, cdfid, varid, lat
   varid=ncdf_varid(cdfid, tim_var)	;readin  latitude
   ncdf_varget, cdfid, varid, time
   ncdf_close,cdfid

   index=where(ev eq missing_value,ncount)
   if ncount gt 1 then ev(index)=!Values.F_NAN

   dat=ev*scale_factor
   data=dat+add_offset

   ntall=N_ELEMENTS(time)
   nlon=N_ELEMENTS(lon)
   nlat=N_ELEMENTS(lat)
   nt=ntall/2.
   datafirst=FLTARR(nlon,nlat,nt)
   ; loop over days an only consider the larger of the both values (either at 00:00 or at 12:00h)
   FOR lo=0,nlon-1 DO BEGIN
      FOR la=0,nlat-1 DO BEGIN
         FOR t=0L, nt-1 DO BEGIN
            ; 0:1, 2:3,4:5,6:7,8:9 etc.-> index=INDGEN(ntall/2)+1
            allindex=indgen(nt)*2
            hourchose=data(lo,la,allindex(t):allindex(t)+1)
         ; print, 'index',allindex(t),'to',allindex(t)+1
            datafirst(lo,la,t)=MAX(hourchose)
         ENDFOR
      ENDFOR
   ENDFOR

   filename='/data/brmuelle/Temperature/ERA_Interim/daily/NetCDF/2m_temp_maxof06h.nc'
   var_new=create_struct('Temp',datafirst)
   lon_arr=lon
   lat_arr=lat
   tim_arr=findgen(nt)
   dim_new=create_struct('lon',float(lon_arr),'lat',float(lat_arr),'time',float(tim_arr))
   attributes=create_struct('var1',['Temperature','K'],'dim1',['longitude','degrees_east'],'dim2',['latitude','degrees_north'],'dim3',['time','starting at 1989 January 1st'])
   put_cdf_var,filename,ncdim=dim_new,ncdata=var_new,attnames=attributes,/float
   print, 'here ERA_Interim low res ends'
ENDIF ELSE BEGIN

IF Keyword_SET(highres) THEN BEGIN
  print, 'this takes ERA-Interim high resolution (normal) temperature'
  nyr=8 ; number of years 1989-2010
  nm=12 ;  number of months

  ; read all data
  FOR i=0,nyr-1 DO BEGIN       ;years
   FOR j=0,nm-1 DO BEGIN    ;months
      a=2011+i
      b=1+j
      print, a,b
      file='/net/firebolt/data/brmuelle/Temperature/ERA_Interim/daily/REGULAR_NETCDF/Temp_'+string(a,format='(i4)')+'_'+string(b,format='(i2.2)')+'.nc'

      missing_value =  2.e+20

      lon_var = 'lon'
      lat_var = 'lat'
      tim_var = 'time'
      ;test the file:
      result=file_test(file)
      if (result eq 0) then stop
      cdfid=ncdf_open(file)                ;open netcdf "file"
      varid=ncdf_varid(cdfid, 'T2')       ;
      ncdf_varget, cdfid, varid, ev 
      varid=ncdf_varid(cdfid, lon_var)     ;readin longitude
      ncdf_varget, cdfid, varid,lon        
      varid=ncdf_varid(cdfid, lat_var)     ;readin  latitude
      ncdf_varget, cdfid, varid, lat
      varid=ncdf_varid(cdfid, tim_var)     ;readin  latitude
      ncdf_varget, cdfid, varid, time
      ncdf_close,cdfid

      index=where(ev eq missing_value,ncount)
      if ncount gt 1 then ev(index)=!Values.F_NAN

      data=REFORM(ev) ; get rid of the spare dimension

      ntall=N_ELEMENTS(time)
      nlon=N_ELEMENTS(lon)
      nlat=N_ELEMENTS(lat)
      nt=ntall/4.
      datafirst=FLTARR(nlon,nlat,nt)
      ; loop over days an only consider the larger of the 4 values (either at 00:00, 6: 12: or 18:00))
      FOR lo=0,nlon-1 DO BEGIN
         FOR la=0,nlat-1 DO BEGIN
            FOR t=0L, nt-1 DO BEGIN
               ; 0:1, 2:3,4:5,6:7,8:9 etc.-> index=INDGEN(ntall/2)+1
               allindex=indgen(nt)*4
               hourchose=data(lo,la,allindex(t):allindex(t)+3)
            ; print, 'index',allindex(t),'to',allindex(t)+1
               datafirst(lo,la,t)=MAX(hourchose)
            ENDFOR
         ENDFOR
      ENDFOR

     filename='/net/firebolt/data/brmuelle/Temperature/ERA_Interim/daily/NetCDF/2m_highres_temp_maxofday'+string(a,format='(i4)')+'_'+string(b,format='(i2.2)')+'.nc'
     var_new=create_struct('Temp',datafirst)
     lon_arr=lon
     lat_arr=lat
     tim_arr=findgen(nt)+1
     dim_new=create_struct('lon',float(lon_arr),'lat',float(lat_arr),'time',float(tim_arr))
     attributes=create_struct('var1',['Temperature','K'],'dim1',['longitude','degrees_east'],'dim2',['latitude','degrees_north'],'dim3',['time','days starting at 1.January'])
     put_cdf_var,filename,ncdim=dim_new,ncdata=var_new,attnames=attributes,/float
   ENDFOR
  ENDFOR
print, 'era int high res ends'
ENDIF ELSE BEGIN
IF Keyword_SET(era40) THEN BEGIN
   print, 'now this calculates era40 temperature max'
   nyr=43 ; number of years 1959-2001
   nm=12 ;  number of months

   ; read all data
  FOR i=0,nyr-1 DO BEGIN       ;years
   FOR j=0,nm-1 DO BEGIN    ;months
      a=1958+i
      b=1+j
      print, a,b
      file='/net/firebolt/data/brmuelle/Temperature/ERA-40/NetCDF_regular/Temp2m_e40_'+string(a,format='(i4)')+'_'+string(b,format='(i2.2)')+'.nc'

      missing_value =  2.e+20

      lon_var = 'lon'
      lat_var = 'lat'
      tim_var = 'time'
      ;test the file:
      result=file_test(file)
      if (result eq 0) then stop
      cdfid=ncdf_open(file)                ;open netcdf "file"
      varid=ncdf_varid(cdfid, 'T2')       ;differen for VIC!!!
      ncdf_varget, cdfid, varid, ev 
      varid=ncdf_varid(cdfid, lon_var)     ;readin longitude
      ncdf_varget, cdfid, varid,lon        
      varid=ncdf_varid(cdfid, lat_var)     ;readin  latitude
      ncdf_varget, cdfid, varid, lat
      varid=ncdf_varid(cdfid, tim_var)     ;readin  latitude
      ncdf_varget, cdfid, varid, time
      ncdf_close,cdfid

      index=where(ev eq missing_value,ncount)
      if ncount gt 1 then ev(index)=!Values.F_NAN

      data=REFORM(ev) ; get rid of the spare dimension

   ntall=N_ELEMENTS(time)
   nlon=N_ELEMENTS(lon)
   nlat=N_ELEMENTS(lat)
   nt=ntall/2.
   datafirst=FLTARR(nlon,nlat,nt)
   ; loop over days an only consider the larger of the both values (either at 00:00 or at 12:00h)
   FOR lo=0,nlon-1 DO BEGIN
      FOR la=0,nlat-1 DO BEGIN
         FOR t=0L, nt-1 DO BEGIN
            ; 0:1, 2:3,4:5,6:7,8:9 etc.-> index=INDGEN(ntall/2)+1
            allindex=indgen(nt)*2
            hourchose=data(lo,la,allindex(t):allindex(t)+1)
         ; print, 'index',allindex(t),'to',allindex(t)+1
            datafirst(lo,la,t)=MAX(hourchose)
         ENDFOR
      ENDFOR
   ENDFOR

     filename='/net/firebolt/data/brmuelle/Temperature/ERA-40/NetCDF_regular/2m_highres_temp_maxofday06'+string(a,format='(i4)')+'_'+string(b,format='(i2.2)')+'.nc'
     var_new=create_struct('Temp',datafirst)
     lon_arr=lon
     lat_arr=lat
     tim_arr=findgen(nt)+1
     dim_new=create_struct('lon',float(lon_arr),'lat',float(lat_arr),'time',float(tim_arr))
     attributes=create_struct('var1',['Temperature','K'],'dim1',['longitude','degrees_east'],'dim2',['latitude','degrees_north'],'dim3',['time','days starting at 1.January'])
     put_cdf_var,filename,ncdim=dim_new,ncdata=var_new,attnames=attributes,/float
   ENDFOR
  ENDFOR
stop
ENDIF ELSE BEGIN

print, 'no keyword set - exit routine'
ENDELSE
ENDELSE
ENDELSE
ENDELSE ;MERRA

ENDELSE; different reanalyses
END