PRO read_HD,hd_data,lon,lat,months,years,startyear=startyear,exceed=exceed,era40=era40,perc10=perc10,perc99=perc99,MERRA=MERRA,CFSR=CFSR,HD_exce=HD_exce
; PURPOSE: read in   HD (Interim) data 
; KEYWORD: ,/perc10 to read data below 10 percentile instead of above 90 percentile

; HISRTORY: taken from   ... on 24 June 2011, bm
;		changes 17.2.2012, included MERRA and CFSR HotDays
;

IF keyword_set(perc99) THEN BEGIN
   file='/net/firebolt/data/brmuelle/Temperature/ERA_Interim/daily/PERCENTILE/temp_numberof99thpercent_5d_99per.nc'
   print, '99th percentile, calculated with percentile compared to 5 days'
   varia='N99P'
   result=file_test(file)
   if (result eq 0) then stop
   cdfid=ncdf_open(file)             ;open netcdf "file"
   varid=ncdf_varid(cdfid, varia)    
   ncdf_varget, cdfid, varid, evn 
   varid=ncdf_varid(cdfid, 'LON')    ;readin longitude
   ncdf_varget, cdfid, varid,lonn    
   varid=ncdf_varid(cdfid, 'LAT')    ;readin  latitude
   ncdf_varget, cdfid, varid, latn
   varid=ncdf_varid(cdfid, 'MONTHS')  ;readin  latitude
   ncdf_varget, cdfid, varid, months
   varid=ncdf_varid(cdfid, 'YEARS')  ;readin  latitude
   ncdf_varget, cdfid, varid, year
   ncdf_close,cdfid
   ntn=N_ELEMENTS(year)
   nlo=N_ELEMENTS(lonn)
   nla=N_ELEMENTS(latn)
   lo1=lonn[256:511]-360
   lo2=lonn[0:255]
   Tempmap2=evn[256:511,*,*,*]
   Tempmap3=evn[0:255,*,*,*]
   evp=[Tempmap2,Tempmap3]
   lon0temp=[lo1,lo2]
   lat0temp=latn
   index=WHERE(year EQ startyear,nc)
   if nc LT 1 THEN stop 
   HD_data=evp(*,*,*,index:ntn-1)
   years=year(index:ntn-1)
   lon=lon0temp
   lat=lat0temp
ENDIF ELSE BEGIN  ; End 10-Percentile data, start 90 percentile
   IF keyword_set(perc10) THEN BEGIN
      file='/net/firebolt/data/brmuelle/Temperature/ERA_Interim/daily/PERCENTILE/temp_numberof10thpercent_5d_10per.nc'
      print, '10th percentile, calculated with percentile compared to 5 days'
      varia='N10P'
      result=file_test(file)
      if (result eq 0) then stop
      cdfid=ncdf_open(file)             ;open netcdf "file"
      varid=ncdf_varid(cdfid, varia)    
      ncdf_varget, cdfid, varid, evn 
      varid=ncdf_varid(cdfid, 'LON')    ;readin longitude
      ncdf_varget, cdfid, varid,lonn    
      varid=ncdf_varid(cdfid, 'LAT')    ;readin  latitude
      ncdf_varget, cdfid, varid, latn
      varid=ncdf_varid(cdfid, 'MONTHS')  ;readin  latitude
      ncdf_varget, cdfid, varid, months
      varid=ncdf_varid(cdfid, 'YEARS')  ;readin  latitude
      ncdf_varget, cdfid, varid, year
      ncdf_close,cdfid
      ntn=N_ELEMENTS(year)
      nlo=N_ELEMENTS(lonn)
      nla=N_ELEMENTS(latn)
      lo1=lonn[256:511]-360
      lo2=lonn[0:255]
      Tempmap2=evn[256:511,*,*,*]
      Tempmap3=evn[0:255,*,*,*]
      evp=[Tempmap2,Tempmap3]
      lon0temp=[lo1,lo2]
      lat0temp=latn
      index=WHERE(year EQ startyear,nc)
      if nc LT 1 THEN stop 
      HD_data=evp(*,*,*,index:ntn-1)
      years=year(index:ntn-1)
      lon=lon0temp
      lat=lat0temp
   ENDIF ELSE BEGIN  ; End 10-Percentile data, start 90 percentile
      IF keyword_set(era40) THEN BEGIN
         print, 'takes era40 data'
         file='/data/brmuelle/Temperature/ERA-40/HD/temp_numberof90thpercent_5d.nc'
         varia='N90P'
         result=file_test(file)
         if (result eq 0) then stop
         cdfid=ncdf_open(file)           ;open netcdf "file"
         varid=ncdf_varid(cdfid, varia)  
         ncdf_varget, cdfid, varid, evn 
         varid=ncdf_varid(cdfid, 'LON')  ;readin longitude
         ncdf_varget, cdfid, varid,lonn  
         varid=ncdf_varid(cdfid, 'LAT')  ;readin  latitude
         ncdf_varget, cdfid, varid, latn
         varid=ncdf_varid(cdfid, 'MONTHS')  ;readin  latitude
         ncdf_varget, cdfid, varid, months
         varid=ncdf_varid(cdfid, 'YEARS')        ;readin  latitude
         ncdf_varget, cdfid, varid, year
         ncdf_close,cdfid
         ntn=N_ELEMENTS(year)
         nlo=N_ELEMENTS(lonn)
         nla=N_ELEMENTS(latn)

         lo1=lonn[160:319]-360
         lo2=lonn[0:159]
         Tempmap2=evn[160:319,*,*,*]
         Tempmap3=evn[0:159,*,*,*]
         evp=[Tempmap2,Tempmap3]
         lon0temp=[lo1,lo2]
         lat0temp=latn

         index=WHERE(year EQ startyear,nc)
         if nc LT 1 THEN stop 
         HD_data=evp(*,*,*,index:ntn-1)
         years=year(index:ntn-1)
         lon=lon0temp
         lat=lat0temp

      ENDIF ELSE BEGIN
         ;-------------------------------------------------------------------------
         ; Read Number of 90-Percentile Temp Interim 
         ;-------------------------------------------------------------------------
      print, 'will take 90 Percentile'
         IF N_elements(exceed) EQ 0 THEN BEGIN
	    IF keyword_set(MERRA) THEN BEGIN
               file='/net/firebolt/data/brmuelle/ET_MERRA/NetCDF_Temperature/NHD/MERRA_temp_no_90thpercent_5d.nc'
	       print, 'taking the 90th percentile, calculated with percentile compared to 5 days - best version (MERRA)'
	       varia='N90P'
	    ENDIF ELSE BEGIN
	       IF keyword_set(CFSR) THEN BEGIN
               file='/net/firebolt/data/brmuelle/CFSR_NCEP/NetCDF/NHD/CFSR_temp_no_90thpercent_5d.nc'
	       print, 'taking the 90th percentile, calculated with percentile compared to 5 days - best version (CFSR)'
	       varia='N90P'
	       ENDIF ELSE BEGIN
		  IF keyword_set(HD_exce) THEN BEGIN
		  file='/net/firebolt/data/brmuelle/Temperature/ERA_Interim/daily/PERCENTILE/temp_degreesabove90th.nc'
		  print, 'maximum temperature exceedance above 90th percentile, calculated with percentile compared to 5 days - ERA-Interim'
		  varia='MAXEXCEEDANCEINTHATMONTH'
	          chosenset='E-INT'
	         ENDIF ELSE BEGIN
	         file='/net/firebolt/data/brmuelle/Temperature/ERA_Interim/daily/PERCENTILE/temp_numberof90thpercent_5d.nc'
	         print, 'taking the 90th percentile, calculated with percentile compared to 5 days - best version (ERA-Interim)'
	         varia='N90P'
	         chosenset='E-INT'
		 ENDELSE
	       ENDELSE
	   ENDELSE
         ENDIF ELSE BEGIN
         file='/net/firebolt/data/brmuelle/Temperature/ERA_Interim/daily/PERCENTILE/temp_numberof'+string(exceed,format='(i2.2)')+'degreesEI_5d.nc'
         print, 'will take the exceedance days of',exceed,'K relative to 5 days'
         varia='NP'+string(exceed,format='(i2.2)')
         ENDELSE

         result=file_test(file)
         if (result eq 0) then stop
         cdfid=ncdf_open(file)		;open netcdf "file"
         varid=ncdf_varid(cdfid, varia)	
         ncdf_varget, cdfid, varid, evn 
         varid=ncdf_varid(cdfid, 'LON')	;readin longitude
         ncdf_varget, cdfid, varid,lonn	
         varid=ncdf_varid(cdfid, 'LAT')	;readin  latitude
         ncdf_varget, cdfid, varid, latn
         varid=ncdf_varid(cdfid, 'MONTHS')  ;readin  latitude
         ncdf_varget, cdfid, varid, months
         varid=ncdf_varid(cdfid, 'YEARS')	;readin  latitude
         ncdf_varget, cdfid, varid, year
         ncdf_close,cdfid

         ntn=N_ELEMENTS(year)
         nlo=N_ELEMENTS(lonn)
         nla=N_ELEMENTS(latn)

	 IF keyword_set(MERRA) THEN BEGIN
	     evp=evn
	    lon0temp=lonn  ; no changes since already form -90..90 and -180...180
	    lat0temp=latn
	 ENDIF ELSE BEGIN
	    IF keyword_set(CFSR) THEN BEGIN
	       lo1=lonn[360:719]-360
	       lo2=lonn[0:359]
	       Tempmap2=evn[360:719,*,*,*]
	       Tempmap3=evn[0:359,*,*,*]
	       evp_notrev=[Tempmap2,Tempmap3]
	       evp=REVERSE(evp_notrev,2)
	       lon0temp=[lo1,lo2]
	       lat0temp=REVERSE(latn)
	    ENDIF ELSE BEGIN ; ERA-Interim:
	       IF chosenset NE 'E-INT' THEN stop
	       lo1=lonn[256:511]-360
	       lo2=lonn[0:255]
	       Tempmap2=evn[256:511,*,*,*]
	       Tempmap3=evn[0:255,*,*,*]
	       evp=[Tempmap2,Tempmap3]
	       lon0temp=[lo1,lo2]
	       lat0temp=latn
	     ENDELSE
	 ENDELSE
	 
         index=WHERE(year EQ startyear,nc)
         if nc LT 1 THEN stop 
         HD_data=evp(*,*,*,index:ntn-1)
         years=year(index:ntn-1)
         lon=lon0temp
         lat=lat0temp
      ENDELSE ; not era40 but era-interim
   ENDELSE ; not perc10 but perc90
ENDELSE ; perc99
END
