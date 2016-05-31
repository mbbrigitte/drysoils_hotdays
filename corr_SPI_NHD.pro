PRO corr_SPI_NHD,outdata,outsig,outlon,outlat,spi_dataset=spi_dataset,HD_dataset=HD_dataset,lays=lays,yearlyhot=yearlyhot,smoothlate=smoothlate,threemonth=threemonth,GLDAS=GLDAS,CLM=CLM,anom=anom,standard=standard


; PURPOSE: read in  SPI  and HD  data (CRU, GPCP, E-Int, MERRA, CFSR, CPC) 
;	   Calculate the correlations and plot on a map

; HISRTORY: original corr_SPI_NHD moved to Martin's folder annotated and simplified  on 31 May 2016, BM
;
; KEYWORD: ,/firsttime  will plot a 2x2 plot with a) hottest month b) correla-
;           tion HD-SPI 
;	   ,/smoothlate needed since it is more correct to caclulate the
;           correlations and only smooth everything at the end

; RUN:    .compile read_HD
;         .compile read_monthly_EI_Temp
;         .compile corr_SPI_NHD
;         .compile read_GLDAS

;  corr_SPI_NHD,/yearlyhot,/firsttime,/smoothlate ; just for firsttime plots  (see above)
;  corr_SPI_NHD,spi_dataset='CRU',HD_dataset='MERRA',/smoothlate,/yearlyhot
;  corr_SPI_NHD,spi_dataset='WacmosSSSMI',HD_dataset='E-Int',/smoothlate,/yearlyhot
;  corr_SPI_NHD,spi_dataset='Wacmosorig',HD_dataset='E-Int',/smoothlate,/yearlyhot
;  corr_SPI_NHD,spi_dataset='WacmosSSWI',HD_dataset='E-Int',/smoothlate,/yearlyhot
;  corr_SPI_NHD,spi_dataset='Wacmosorig_ano',HD_dataset='E-Int',/smoothlate,/yearlyhot,lays=0
;  corr_SPI_NHD,spi_dataset='WacmosSWIorig_ano',HD_dataset='E-Int',/smoothlate,/yearlyhot
;  corr_SPI_NHD,spi_dataset='WacmosSWIorig',HD_dataset='E-Int',/smoothlate,/yearlyhot
;  corr_SPI_NHD,spi_dataset='NOAH_1layer',HD_dataset='E-Int',/smoothlate,/yearlyhot add GLDAS, CLM etc keywords
;  corr_SPI_NHD,spi_dataset='NOAH_3layers',HD_dataset='E-Int',/smoothlate,/yearlyhot add  keywords
;  corr_SPI_NHD,spi_dataset='NOAH_1layer_ano',HD_dataset='E-Int',/smoothlate,/yearlyhot add  keywords
;  corr_SPI_NHD,spi_dataset='NOAH_3layers_ano',HD_dataset='E-Int',/smoothlate,/yearlyhot add keywords
;  corr_SPI_NHD,spi_dataset='CLM_1layer',HD_dataset='E-Int',/smoothlate,/yearlyhot add keywords
;  corr_SPI_NHD,spi_dataset='CLM_8layers',HD_dataset='E-Int',/smoothlate,/yearlyhot add  keywords
;  corr_SPI_NHD,spi_dataset='CLM_1layer_ano',HD_dataset='E-Int',/smoothlate,/yearlyhot add  keywords
 ; corr_SPI_NHD,spi_dataset='CLM_8layers_ano',HD_dataset='E-Int',/smoothlate,/yearlyhot add keywords
 
; corr_SPI_NHD,spi_dataset='CLM_8layers',HD_dataset='E-Int',lays=8,/smoothlate,/yearlyhot,/GLDAS,/CLM,/anom
; corr_SPI_NHD,spi_dataset='CLM_standardized',HD_dataset='E-Int',lays=0,/smoothlate,/yearlyhot,/GLDAS,/CLM,/standard
; corr_SPI_NHD,spi_dataset='NOAH_standardized',HD_dataset='E-Int',lays=0,/smoothlate,/yearlyhot,/GLDAS,/standard
;----------------------------------------------------------------------------------------------------------------



IF keyword_set(GLDAS) THEN GLDAS=1 ELSE GLDAS=0
IF keyword_set(CLM) THEN CLM=1 ELSE CLM=0
IF keyword_set(anom) THEN anom=1 ELSE anom=0



print, HD_dataset
print, 'lays',lays


If keyword_set(yearlyhot) THEN BEGIN
   print, 'every year different hottest month taken for heat wave correlation'
ENDIF ELSE BEGIN
   print, 'seasonal cycle hottest month taken'
ENDELSE

timescale='3'
startyear=1979 ; 

endyear=2009
IF SPI_dataset EQ 'CPC' THEN endyear=2008

nm=12
nyspi=endyear+1-startyear ;SPI endyear
ny=endyear+1+1-startyear ;temperature one later as preceding temperature wanted, i.e. needs one more year for January - March of the last year
sig_lev=0.1
smoothfac=10



; -------------------------------------------------------
;       0. Set a couple of names
;--------------------------------------------------------
; This section doesn't calculate anything, got to 1. for start

outfil= string(timescale)+'mn_SPI_'+string(spi_dataset)+'_preceding_hot.nc'
IF GLDAS EQ 1 THEN outfil='SM_'+string(spi_dataset)+'_preceding_hot.nc'

; Determine the netcdf output file path
IF ((spi_dataset EQ 'Wacmosorig') OR (spi_dataset EQ 'Wacmos') OR (spi_dataset EQ 'WacmosSSSMI')OR (spi_dataset EQ 'WacmosSSWI')OR (spi_dataset EQ 'Wacmosorig_ano')OR (spi_dataset EQ 'WacmosSWIorig_ano')OR (spi_dataset EQ 'WacmosSWIorig')OR (GLDAS EQ 1)) THEN netcdfdir='/net/firebolt/data/brmuelle/ET_PLOTS/PLOT_HEATWAVE/CORRELATIONS/WACMOS-PAPER/360x1979-2009/' ELSE netcdfdir='/net/firebolt/data/brmuelle/ET_PLOTS/PLOT_HEATWAVE/CORRELATIONS/REANALYSES_CORR/DATA/'

; determine the netcdf output file name
filenc=netcdfdir+string(hd_dataset)+'_'+string(spi_dataset)+'_'+string(timescale)+'mn.nc'
IF ((spi_dataset EQ 'Wacmosorig') OR (spi_dataset EQ 'Wacmos') OR (spi_dataset EQ 'Wacmosorig_ano')OR (spi_dataset EQ 'WacmosSWIorig_ano')OR (spi_dataset EQ 'WacmosSWIorig')OR (GLDAS EQ 1)) THEN filenc=netcdfdir+string(hd_dataset)+'_'+string(spi_dataset)+'.nc'


; output filename (ps-figure) and title
dir='/net/firebolt/data/brmuelle/ET_PLOTS/PLOT_HEATWAVE/CORRELATIONS/FIGURES/Smoothatbeginning/'
IF keyword_set(smoothlate) THEN dir='/net/firebolt/data/brmuelle/ET_PLOTS/PLOT_HEATWAVE/CORRELATIONS/FIGURES/'

If keyword_set(yearlyhot) THEN BEGIN ;output file name (every year different hotm)
   psfile1=dir+'Correlation_HD'+string(hd_dataset)+'_SPI'+string(spi_dataset)+'_hot'+string(timescale)+'_sm.eps' 
   titlestr='Correlation NHD '+string(hd_dataset)+' and preceding '+string(timescale)+'mn SPI '+string(spi_dataset)
   IF HD_dataset EQ 'HD-exceedance' THEN  titlestr='Correlation max. temperature exceedance and preceding '+string(timescale)+'mn SPI '+string(spi_dataset)

   IF keyword_set(threemonth) THEN BEGIN
      psfile1=dir+'Correlation_3mHD'+string(hd_dataset)+'_SPI'+string(spi_dataset)+'_hot'+string(timescale)+'_sm.eps' 
      titlestr='Corr. 3mn-NHD '+string(hd_dataset)+' and preceding '+string(timescale)+'mn SPI '+string(spi_dataset)
   ENDIF
ENDIF ELSE BEGIN ; now only seasonal hottest month
   psfile1=dir+'Correlation_HD'+string(hd_dataset)+'_SPI'+string(spi_dataset)+'_1Hotmonth'+string(timescale)+'.eps' 
   titlestr='Corr. HD (hottest month seasonal cycle) and preceding '+timescale+'mn-SPI'
ENDELSE



; input filenames for SPI
file=0
IF SPI_dataset EQ 'CRU' THEN BEGIN
   file='/net/firebolt/data/brmuelle/Precipitation_data/CRU/SPI_CRU_'+timescale+'mn_1950-2009_reform.nc'
   stye=1950
ENDIF 
IF SPI_dataset EQ 'GPCP' THEN BEGIN
   file='/net/firebolt/data/brmuelle/Precipitation_data/GPCP/SPI_GPCP_'+timescale+'mn_1979-2010_reform.nc'
   stye=1979
ENDIF
IF SPI_dataset EQ 'CPC' THEN BEGIN
   file='/net/firebolt/data/brmuelle/Precipitation_data/CMAP/SPI_CPC_'+timescale+'mn_1979-2009_reform.nc'
   stye=1979
ENDIF 
IF SPI_dataset EQ 'WacmosSSSMI' THEN BEGIN
   file='/home/brmuelle/firebolt/SoilMoisture/WACMOS/SSM/ESACCI-L3S_SSM_1979_2010_sssmi'+string(timescale)+'m.nc'
   stye=1979
   titlestr='Correlation NHD '+string(hd_dataset)+' and preceding'+string(timescale)+string(spi_dataset)+'-SSMI'
   print, 'please use other programm where Wacmos is interpolated and not SPI'
ENDIF
IF SPI_dataset EQ 'Wacmosorig' THEN BEGIN
   file='/home/brmuelle/firebolt/SoilMoisture/WACMOS/SSM/ESACCI-L3S_SSM_1979_2010.nc'
   stye=1979
   titlestr='Correlation NHD '+string(hd_dataset)+' and preceding SSM'
   print, 'please use other programm where Wacmos is interpolated and not SPI'
ENDIF
IF SPI_dataset EQ 'WacmosSSWI' THEN BEGIN
   file='/home/brmuelle/firebolt/SoilMoisture/WACMOS/SWI/ESACCI-L3S_SWI_1979_2010_sswi'+string(timescale)+'m.nc'
   stye=1979
   titlestr='Correlation NHD '+string(hd_dataset)+' and 	preceding'+string(timescale)+string(spi_dataset)+'-SSWI'
   print, 'please use other programm where Wacmos is interpolated and not SPI'
ENDIF
IF SPI_dataset EQ 'Wacmosorig_ano' THEN BEGIN
   file='/home/brmuelle/firebolt/SoilMoisture/WACMOS/SSM/ESACCI-L3S_SSM_1979_2010_anom.nc'
   stye=1979
   titlestr='Correlation NHD '+string(hd_dataset)+' and preceding'+string(timescale)+string(spi_dataset)+'-SSM-Anomaly'
ENDIF
IF SPI_dataset EQ 'WacmosSWIorig_ano' THEN BEGIN
   file='/home/brmuelle/firebolt/SoilMoisture/WACMOS/SWI/ESACCI-L3S_SWI_1979_2010_anom.nc'
   stye=1979
   titlestr='Correlation NHD '+string(hd_dataset)+' and preceding'+string(timescale)+string(spi_dataset)
ENDIF
IF SPI_dataset EQ 'WacmosSWIorig' THEN BEGIN
   file='/home/brmuelle/firebolt/SoilMoisture/WACMOS/SWI/ESACCI-L3S_SWI_1979_2010.nc'
   stye=1979
   titlestr='Correlation NHD '+string(hd_dataset)+' and preceding'+string(timescale)+string(spi_dataset)
ENDIF


IF GLDAS EQ 1 THEN BEGIN
   print, 'GLDAS title'
   titlestr='Correlation NHD '+string(hd_dataset)+' and preceding '+string(spi_dataset)+': '+string(lays)+' Layers'
ENDIF
IF N_elements(file) EQ 0 THEN BEGIN
   print, 'no valid SPI dataset'
   stop
ENDIF



;---------------------------------------------------------
; 1. Read SPI data and do some transformations
;---------------------------------------------------------
IF GLDAS EQ 1 THEN BEGIN
   IF keyword_set(standard) THEN BEGIN
	 IF CLM EQ 1 THEN read_GLDAS, noah_data,noah_lo,noah_la,lays=lays,/CLM,/standard ELSE read_GLDAS, noah_data,noah_lo,noah_la,lays=lays,/standard
   ENDIF ELSE BEGIN
      IF CLM EQ 1 THEN BEGIN
	 IF anom EQ 1 THEN read_GLDAS,noah_data,noah_lo,noah_la,lays=lays,/anom,/CLM ELSE read_GLDAS, noah_data,noah_lo,noah_la,lays=lays,/CLM
      ENDIF ELSE BEGIN
	 IF anom EQ 1 THEN read_GLDAS,noah_data,noah_lo,noah_la,lays=lays,/anom ELSE read_GLDAS, noah_data,noah_lo,noah_la,lays=lays
      ENDELSE
   ENDELSE
   ntspi=32
   lospi=noah_lo & laspi=noah_la
   nlospi=N_ELEMENTS(lospi)
   nlaspi=N_ELEMENTS(laspi)
   SPIdata=noah_data(*,*,*,0:ntspi-2) ;79-09
   IF nyspi NE N_elements(spidata(0,0,0,*)) THEN stop
ENDIF ELSE BEGIN
   print, 'read in Wacmos or SPI data'
   result=file_test(file)
   if (result eq 0) then stop
   cdfid=ncdf_open(file)           ;open netcdf "file"
   IF SPI_dataset EQ 'WacmosSSSMI' THEN BEGIN
   varib='sssmi' & loib='lon' & laib='lat' 
   years=stye+FINDGEN(ny)
   ENDIF ELSE BEGIN
      IF ((SPI_dataset EQ 'Wacmosorig') OR (SPI_dataset EQ 'Wacmosorig_ano')) THEN BEGIN
	 varib='sm' & loib='lon' & laib='lat' 
	 years=stye+FINDGEN(ny)
      ENDIF ELSE BEGIN
	 IF ((spi_dataset EQ 'WacmosSWIorig_ano') OR (spi_dataset EQ 'WacmosSWIorig')) THEN BEGIN
	 varib='swi' & loib='lon' & laib='lat' 
	 years=stye+FINDGEN(ny)
	 ENDIF ELSE BEGIN
	    IF SPI_dataset EQ 'WacmosSSWI' THEN BEGIN
	    varib='sswi' & loib='lon' & laib='lat' 
	    years=stye+FINDGEN(ny)
	    ENDIF ELSE BEGIN
	    varib='SPI' & loib='LON' & laib='LAT'
	    ENDELSE
	 ENDELSE
      ENDELSE
   ENDELSE
   varid=ncdf_varid(cdfid, varib) 
   ncdf_varget, cdfid, varid, ev 
   varid=ncdf_varid(cdfid, loib)  ;readin longitude
   ncdf_varget, cdfid, varid,lospi  
   varid=ncdf_varid(cdfid, laib)  ;readin  latitude
   ncdf_varget, cdfid, varid, laspi
   IF ((SPI_dataset NE 'WacmosSSSMI')AND (SPI_dataset NE 'Wacmosorig')AND (SPI_dataset NE 'WacmosSSWI')AND (SPI_dataset NE 'Wacmosorig_ano')AND (SPI_dataset NE 'WacmosSWIorig_ano')AND (SPI_dataset NE 'WacmosSWIorig')) THEN BEGIN
   varid=ncdf_varid(cdfid, 'TIME')        ;readin  latitude
   ncdf_varget, cdfid, varid, months
   varid=ncdf_varid(cdfid, 'YEARS')        ;readin  latitude
   ncdf_varget, cdfid, varid, years
   ENDIF
   ncdf_close,cdfid
   IF ((SPI_dataset NE 'WacmosSSSMI')AND (SPI_dataset NE 'Wacmosorig')AND (SPI_dataset NE 'WacmosSSWI')AND (SPI_dataset NE 'Wacmosorig_ano')AND (SPI_dataset NE 'WacmosSWIorig_ano')AND (SPI_dataset NE 'WacmosSWIorig')) THEN years=stye+years
   ntspi=N_ELEMENTS(years)
   nlospi=N_ELEMENTS(lospi)
   nlaspi=N_ELEMENTS(laspi)

   IF ((SPI_dataset EQ 'WacmosSSSMI') OR (SPI_dataset EQ 'Wacmosorig') OR (SPI_dataset EQ 'WacmosSSWI')OR (SPI_dataset EQ 'Wacmosorig_ano')OR (SPI_dataset EQ 'WacmosSWIorig_ano')OR (SPI_dataset EQ 'WacmosSWIorig')) THEN BEGIN
      ev(where(ev EQ -9999.0))=!Values.F_NaN
      ev=REFORM(ev,1440,720,nm,ntspi)
      ev=REVERSE(ev,2)
      laspi=REVERSE(laspi)
   ENDIF

   stind=WHERE(years EQ startyear,nc)
   endind=WHERE(years EQ endyear)
   IF nc NE 1 THEN stop
   SPIdata=ev(*,*,*,stind:endind) ;79-09
   IF nyspi NE N_elements(spidata(0,0,0,*)) THEN stop
ENDELSE

; -------------------------------------------------
;         Reform SPI data so that all same grid
;--------------------------------------------------
IF ((SPI_dataset EQ 'GPCP') OR (SPI_dataset EQ 'CPC')) THEN BEGIN
spi1=spidata[72:143,*,*,*]
spi2=spidata[0:71,*,*,*]
spidata=0 ; reset this variable
spiref=[spi1,spi2]
lo1=lospi[72:143]-360
lo2=lospi[0:71]
loref=[lo1,lo2]
lospi=loref
SPIdata=REVERSE(spiref,2)
laspi=REVERSE(laspi)
ENDIF ; for GPCP and CPC reversed and 0-360  to -180...180 transformed
; WacmosSSSMI has been reversed already before!



;--------------------------------------
;         Read hottest month
;--------------------------------------
If keyword_set(yearlyhot) THEN BEGIN
   ; -------------------------------------------------------
   ;          Read Temperature data (Interim) to define 
   ;          hottest month nd its lowest value at 
   ;          each grid point 
   ;--------------------------------------------------------
   print, 'read in ERA-Interim Temperature for hottest month'
   read_monthly_EI_Temp,temp,lotem,latem,startyear=startyear,ny=ny ;ny=32
   
   dry_nlo=360
   dry_nla=180
   lod=-180+findgen(360)
   lad=-90+findgen(180)
   IF keyword_set(resolution) THEN BEGIN
    dry_nlo=120
    dry_nla=60
    lod=-180+3*findgen(120)
    lad=-90+3*findgen(60)
   ENDIF
   Temp_data=fltarr(dry_nlo,dry_nla,12,ny)
   FOR tem=0L,11 DO BEGIN 
    FOR yrl=0L,ny-1 DO BEGIN
       currentdat=REFORM(temp(*,*,tem,yrl))  ; 
       Temp_data(*,*,tem,yrl)=CONGRID(currentdat,dry_nlo,dry_nla,/interp)
     ENDFOR
   ENDFOR
   help, temp_data
ENDIF ELSE BEGIN
   filename='/net/firebolt/data/brmuelle/ERS/NetCDF/Hot_month_in_seasonalcycleERS.nc'
   result=file_test(filename)
   if (result eq 0) then stop
   cdfid=ncdf_open(filename)           ;open netcdf "file"
   varid=ncdf_varid(cdfid, 'MONTH') 
   ncdf_varget, cdfid, varid, d 
   varid=ncdf_varid(cdfid, 'LON')  ;readin longitude
   ncdf_varget, cdfid, varid,lod  
   varid=ncdf_varid(cdfid, 'LAT')  ;readin  latitude
   ncdf_varget, cdfid, varid,lad  
   ncdf_close,cdfid
   drym=d-1
   dry_nlo=N_elements(lod)
   dry_nla=N_elements(lad)
ENDELSE



;-------------------------------------------------------------------------
;    Read Number of 90-Percentile Temp Interim 
;-------------------------------------------------------------------------
print, 'read in HD datast',hd_dataset
IF HD_dataset EQ 'MERRA' THEN BEGIN
read_HD,hd_data,lon,lat,months,years,startyear=startyear,/MERRA
ENDIF ELSE BEGIN
   IF HD_dataset EQ 'CFSR' THEN BEGIN
    read_HD,hd_data,lon,lat,months,years,startyear=startyear,/CFSR
   ENDIF ELSE BEGIN
      IF HD_dataset EQ 'E-Int' THEN BEGIN
       read_HD,hd_data,lon,lat,months,years,startyear=startyear
      ENDIF ELSE BEGIN
	IF HD_dataset EQ 'HD-exceedance' THEN BEGIN
	 read_HD,hd_data,lon,lat,months,years,startyear=startyear,/HD_exce
	ENDIF ELSE BEGIN
	 print, 'no Reanalysis dataset chosen'
	 stop
	ENDELSE
      ENDELSE
   ENDELSE
ENDELSE
help,hd_data ; 1979-2010
nlo=N_ELEMENTS(lon)
nla=N_ELEMENTS(lat)

; -------------------------------------------------------------
;          REGRID data SPI and data HD
;--------------------------------------------------------------

print, 'regrid the SPI or Wacmos data'
plotSPIall=fltarr(dry_nlo,dry_nla,12,nySPI) ;nyspi is 31
FOR tem=0L,nySPI-1 DO BEGIN
   FOR mnt=0L,11 DO BEGIN
   currentdat1=REFORM(SPIdata(*,*,mnt,tem))  ; 
   IF ((keyword_set(smoothlate)) OR (keyword_set(resolution))) THEN BEGIN
      plotSPIall(*,*,mnt,tem)=CONGRID(currentdat1,dry_nlo,dry_nla,/interp)
      IF ((tem eq 0) and (mnt eq 0)) then print, 'only congrid done'
   ENDIF ELSE BEGIN
      currentdat2=CONGRID(currentdat1,dry_nlo,dry_nla,/interp)
      plotSPIall(*,*,mnt,tem)=SMOOTH(currentdat2,smoothfac,/NaN)
      print, 'smoothing done'
   ENDELSE
   ENDFOR
ENDFOR

print, 'regrid the HD dataset'
plotHDall=fltarr(dry_nlo,dry_nla,12,ny) ;ny is 32
FOR tem=0L,ny-1 DO BEGIN
   FOR mnt=0L,11 DO BEGIN
   currentdat1=REFORM(hd_data(*,*,mnt,tem))  ; 
   IF ((keyword_set(smoothlate)) OR (keyword_set(resolution))) THEN BEGIN
      plotHDall(*,*,mnt,tem)=CONGRID(currentdat1,dry_nlo,dry_nla,/interp)
   ENDIF ELSE BEGIN
      currentdat2=CONGRID(currentdat1,dry_nlo,dry_nla,/interp)
      plotHDall(*,*,mnt,tem)=SMOOTH(currentdat2,smoothfac,/NaN)
   ENDELSE
   ENDFOR
ENDFOR


help, plotSPIall ;1979-2009
help, plotHDall   ;1979-2010
help, drym       ;index

; -------------------------------------------------------------
;          3. Correlations
;--------------------------------------------------------------

HD_SM_plot=FLTARR(dry_nlo,dry_nla)

HD_SM=FLTARR(dry_nlo,dry_nla)
ncHD_SM=FLTARR(dry_nlo,dry_nla)
p_corr=FLTARR(dry_nlo,dry_nla)
p_corr(*)=!Values.F_nan
plotSPImonth=FLTARR(dry_nlo,dry_nla,nyspi-1)
HD_JJA=FLTARR(dry_nlo,dry_nla,nyspi-1)

hotmonth=FLTARR(dry_nlo,dry_nla,ny)
print, 'select at the hottest month'
IF keyword_set(yearlyhot) THEN BEGIN
   IF not keyword_set(threemonth) THEN BEGIN
      drymin=fltarr(dry_nlo,dry_nla,ny)
      FOR lol=0L,dry_nlo-1 DO BEGIN;longitude
	 FOR lal=0L,dry_nla-1 DO BEGIN;latitude
	    FOR tem=0L,nyspi-2 DO BEGIN ;years
	;    IF ((lol EQ 0) AND (lal EQ 0)) THEN print, '0,0 tem loop at ',tem
	    drymin=WHERE(temp_data(lol,lal,*,tem+1) EQ MAX(temp_data(lol,lal,*,tem+1),/NaN),nc);1990-2009 so tem+1
	    if nc EQ 2 then drymin=drymin(1)
	    IF nc GT 2 then stop
	    hotmonth(lol,lal,tem)=drymin ;index where hottest month is
	    HD_JJA(lol,lal,tem)=plotHDall(lol,lal,drymin,tem+1) ;1990-2009
	    IF drymin GE 1 THEN BEGIN
	    plotSPImonth(lol,lal,tem)=plotSPIall(lol,lal,drymin-1,tem+1);1990-2009 for jan-nov
	    ENDIF ELSE BEGIN
	    plotSPImonth(lol,lal,tem)=plotSPIall(lol,lal,drymin-1+12,tem);1989-2008 for december
	    ENDELSE
	    ENDFOR
	 ENDFOR
      ENDFOR
    ENDIF ELSE BEGIN ; threemonth not set end, now for threemonth:
      drymin=fltarr(dry_nlo,dry_nla,ny)
      FOR lol=0L,dry_nlo-1 DO BEGIN;longitude
	 FOR lal=0L,dry_nla-1 DO BEGIN;latitude
	    FOR tem=0L,nyspi-2 DO BEGIN ;years
	    IF ((lol EQ 0) AND (lal EQ 0)) THEN print, '0,0 tem loop at ',tem
	    ; IF ((lol EQ 0) AND (lal EQ 1)) THEN print, '0,1 tem loop at ',tem
	    drymin=WHERE(temp_data(lol,lal,*,tem+1) EQ MAX(temp_data(lol,lal,*,tem+1),/NaN),nc);1990-2009 so tem+1
	    if nc EQ 2 then drymin=drymin(1)
	    IF nc GT 2 then stop
	    IF ((drymin GE 1) AND (drymin LE 10)) THEN BEGIN ; +/1 month possible for those, normal case: 
	    HD_JJA(lol,lal,tem)=TOTAL(plotHDall(lol,lal,drymin-1:drymin+1,tem+1),/NaN) ;1990-2009
	    ENDIF ELSE BEGIN
	       IF drymin EQ 0 THEN BEGIN
	       twomonthsum=TOTAL(plotHDall(lol,lal,drymin:drymin+1,tem+1),/NaN)
	       HD_JJA(lol,lal,tem)=plotHDall(lol,lal,drymin-1+12,tem)+twomonthsum
	       ENDIF
	       IF drymin EQ 11 THEN BEGIN
	       ; not possible for last year December:
		  IF tem LE nyspi-2 THEN BEGIN
		  twomonthsum=TOTAL(plotHDall(lol,lal,drymin-1:drymin,tem+1),/NaN)
		  HD_JJA(lol,lal,tem)=plotHDall(lol,lal,drymin+1-12,tem+2)+twomonthsum
		  ENDIF ELSE BEGIN
		  IF ((lol EQ 0) and (lal EQ 0)) then print, 'This case really happened'

		  HD_JJA(lol,lal,tem) =!Values.F_NaN 
		  ENDELSE ; not if very last year december
	       ENDIF ;december
	    ENDELSE ; the abnormal cases where January or December end here
	    
	    IF drymin GE 1 THEN BEGIN
            plotSPImonth(lol,lal,tem)=plotSPIall(lol,lal,drymin-1,tem+1);1990-2009 for jan-nov
	    ENDIF ELSE BEGIN
	    plotSPImonth(lol,lal,tem)=plotSPIall(lol,lal,drymin-1+12,tem);1989-2008 for december
	    ENDELSE
	    ENDFOR
	 ENDFOR
      ENDFOR 
    ENDELSE ; keyword threemonth
ENDIF ELSE BEGIN
   FOR lol=0L,dry_nlo-1 DO BEGIN
      FOR lal=0L,dry_nla-1 DO BEGIN
      drymin=drym(lol,lal)
      IF FINITE(drymin) EQ 0 THEN BEGIN  ; only NaNs
         plotSPImonth(lol,lal,*)=!Values.F_Nan
      ENDIF ELSE BEGIN
         HD_JJA(lol,lal,*)=TOTAL(plotHDall(lol,lal,drymin,1:ny-2),3,/NaN) ;1990-2009
         IF drymin GE 1 THEN BEGIN
         plotSPImonth(lol,lal,*)=plotSPIall(lol,lal,drymin-1,1:nyspi-1) ;1990-2009 for jan-nov
         ENDIF ELSE BEGIN
         plotSPImonth(lol,lal,*)=plotSPIall(lol,lal,drymin-1,0:nyspi-2) ;1989-2008 for december
         ENDELSE
      ENDELSE
   ENDFOR
   ENDFOR
ENDELSE

print, 'arrived at second loop!'


plotSPI=plotSPImonth
plotHD=HD_JJA
; output in NetCDF files
  help,SPI_season,plotHD
   lon_arr=lod 
   lat_arr=lad
   tim_arr=FINDGEN(N_elements(plotSPI(0,0,*)))+startyear+1
   var_new=CREATE_STRUCT('SPI',plotSPI)
   dim_new=CREATE_STRUCT('lon',float(lon_arr),'lat',float(lat_arr),'time',float(tim_arr))
   attributes=CREATE_STRUCT('var1',['SPI','-'],'dim1',['longitude','degrees_east'],'dim2',['latitude','degrees_north'],'dim3',['time','time (yearly,SPI at hottest month-1)'])
   put_cdf_var,outfil,ncdim=dim_new,ncdata=var_new,attnames=attributes,/float


print, 'correlate'
IF HD_dataset EQ 'HD-exceedance' THEN BEGIN
   FOR lol=0L,dry_nlo-1 DO BEGIN
      FOR lal=0L,dry_nla-1 DO BEGIN
	 x=REFORM(plotSPI(lol,lal,*))
	 y=REFORM(plotHD(lol,lal,*))
	 index=WHERE((FINITE(x) EQ 1),nc)
	 index2=WHERE((FINITE(y) EQ 1),nc)
	 ncHD_SM(lol,lal)=nc
	 IF nc LE 2 THEN BEGIN
	    HD_SM(lol,lal)=!Values.F_nan
	 ENDIF ELSE BEGIN
	 a=x(index)
	 b=y(index)
	 a=a(index2)
	 b=b(index2)
	 ETTcorr = CORRELATE(a,b)
	 HD_SM(lol,lal)=ETTcorr
	 pvalue=corr_ttest(corr=abs(ETTcorr),N=nc,sl=sig_lev)
	    IF pvalue LT sig_lev THEN BEGIN
	 p_corr(lol,lal)=0.01
	 ENDIF ELSE BEGIN
	  p_corr(lol,lal)=9999
	 ENDELSE
	 ENDELSE
      ENDFOR
   ENDFOR
ENDIF ELSE BEGIN
   FOR lol=0L,dry_nlo-1 DO BEGIN
      FOR lal=0L,dry_nla-1 DO BEGIN
	 x=REFORM(plotSPI(lol,lal,*))
	 y=REFORM(plotHD(lol,lal,*))
	 index=WHERE((FINITE(x) EQ 1),nc)
	 ncHD_SM(lol,lal)=nc
	 IF nc LE 2 THEN BEGIN
	    HD_SM(lol,lal)=!Values.F_nan
	 ENDIF ELSE BEGIN
	    a=x(index)
	    b=y(index)
	    ETTcorr = CORRELATE(a,b)
	    HD_SM(lol,lal)=ETTcorr
	    pvalue=corr_ttest(corr=abs(ETTcorr),N=nc,sl=sig_lev)
	    IF pvalue LT sig_lev THEN BEGIN
	       p_corr(lol,lal)=0.01
	    ENDIF ELSE BEGIN
	       p_corr(lol,lal)=9999
	    ENDELSE
	 ENDELSE
      ENDFOR
   ENDFOR
ENDELSE
; -------------------------------------------------------------
;                       4. Plots
; -------------------------------------------------------------
lev=10
minlat=-60
minlon=-170
maxlat=70
maxlon=170
; 
; Read the land-sea mask to mask out the ocean:
filein='/net/firebolt/data/brmuelle/downloaded_EVAP_interim/Land_sea/landsea_regular.nc'
cdfid=ncdf_open(filein)         ;open the netcdf 
varid=ncdf_varid(cdfid, 'LSM')    
ncdf_varget, cdfid, varid, lsm_old
varid=ncdf_varid(cdfid, 'lon')        
ncdf_varget, cdfid, varid, lon_grid
varid=ncdf_varid(cdfid, 'lat')       
ncdf_varget, cdfid, varid, lat_grid
ncdf_close,cdfid
lsm1=lsm_old(256:511,*) & lsm2=lsm_old(0:255,*)
lsm=[lsm1,lsm2]
landsea256=CONGRID(lsm,dry_nlo,dry_nla) ;360x180 resolution 
index=WHERE(landsea256 eq 0)

; number of correlations
mi=0 & ma=30
data=ncHD_SM & lon1=lod & lat1=lad
data(index)=!Values.F_NaN
contour_plot_farbig,data,lon=lon1,lat=lat1,ct=33,psfilename=dir+'NumberOfCorr'+string(hd_dataset)+'_SPI'+string(spi_dataset)+'.ps',title='NumberOfCorr'+string(hd_dataset)+'_'+string(spi_dataset),mi=mi,ma=ma,regionplot=[minlat,minlon,maxlat,maxlon],limit=[minlat,minlon,maxlat,maxlon],colorbarunits='-',nlevels=lev,colorbarlowleft=[0.1,0.07],/gridme,/eps,chars=2,colorbarchars=1.5;,/whitecenter

netdata=HD_SM
netdata(where(finite(netdata) NE 1))=-9999.

var_new=create_struct('corr',netdata)
lon_arr=lon1
lat_arr=lat1
dim_new=create_struct('lon',float(lon_arr),'lat',float(lat_arr))
attributes=create_struct('var1',['Correlation','Correlation HD - 3mn SPI'],'dim1',['longitude','degrees_east'],'dim2',['latitude','degrees_north'])
put_cdf_var,filenc,ncdim=dim_new,ncdata=var_new,attnames=attributes,/float


IF keyword_set(smoothlate) THEN BEGIN
indnan1=WHERE(FINITE(HD_SM) NE 1)
data=SMOOTH(HD_SM,smoothfac,/NaN) & lon1=lod & lat1=lad
data(indnan1)=!Values.F_NaN
data(index)=!Values.F_NaN
p_corr(index)=!Values.F_NaN
ENDIF ELSE BEGIN
data=HD_SM & lon1=lod & lat1=lad
data(index)=!Values.F_NaN
p_corr(index)=!Values.F_NaN
ENDELSE


; colorbar yellow between 0 and -0.2, grey above 0 and red below -0.2.
mi=-1 & ma=0.2

print, data(4,5)
help, data, lon1,lat1
contour_plot,data,lon=lon1,lat=lat1,ct=33,psfilename=psfile1,title=titlestr,mi=mi,ma=ma,regionplot=[minlat,minlon,maxlat,maxlon],limit=[minlat,minlon,maxlat,maxlon],colorbarunits='-',cot_an=[44,43],nlevels=lev,colorbarlowleft=[0.1,0.07],p_values=p_corr,signif_levels=[0.0,0.05],/gridme,/eps,chars=2,colorbarchars=1.5;,/whitecenter

outdata=data
outsig=p_corr
outlon=lon1
outlat=lat1

IF keyword_set(firsttime) THEN BEGIN
  psfile2=dir+'Hotmonth_EI_mostcommon.ps' 
  titlestr='Hottest month'
  hot_m=FLTARR(dry_nlo,dry_nla)
  FOR lol=0L,dry_nlo-1 DO BEGIN
    FOR lal=0L,dry_nla-1 DO BEGIN
    t=transpose(hotmonth(lol,lal,*))
    binsize=1
    h=histogram(t,bins=binsize)
    mh = max(h)
    i = where(h eq max(h))
    iout = i*binsize
    hot_m(lol,lal)=iout(0)
    ENDFOR
  ENDFOR
  mi=1
  ma=13
  minlat=-60
  minlon=-170
  maxlat=70
  maxlon=170
  ; Read the land-sea mask to mask out the ocean:
  filein='/net/firebolt/data/brmuelle/downloaded_EVAP_interim/Land_sea/landsea_regular.nc'
  cdfid=ncdf_open(filein)         ;open the netcdf file "filewant"
  varid=ncdf_varid(cdfid, 'LSM')          ;data read
  ncdf_varget, cdfid, varid, lsm_old
  varid=ncdf_varid(cdfid, 'lon')          ;data read
  ncdf_varget, cdfid, varid, lon_grid
  varid=ncdf_varid(cdfid, 'lat')          ;data read
  ncdf_varget, cdfid, varid, lat_grid
  ncdf_close,cdfid
  lsm1=lsm_old(256:511,*) & lsm2=lsm_old(0:255,*)
  lsm=[lsm1,lsm2]
  landsea256=CONGRID(lsm,360,180)
  index=WHERE(landsea256 eq 0)



  data=hot_m & lon1=lod & lat1=lad
  data(index)=!Values.F_NaN
  help, data,lon1,lat1
  contour_plot,data+1,lon=lon1,lat=lat1,ct=33,psfilename=psfile2,title=titlestr,mi=mi,ma=ma,regionplot=[minlat,minlon,maxlat,maxlon],limit=[minlat,minlon,maxlat,maxlon],colorbarunits='month',colorbarlowleft=[0.1,0.07],/gridme,plotstep=1,chars=2,colorbarchars=1.5;,/whitecenter

  contour_plot,data+1,lon=lon1,lat=lat1,ct=33,psfilename=psfile2,title=titlestr,mi=mi,ma=ma,regionplot=[minlat,minlon,maxlat,maxlon],limit=[minlat,minlon,maxlat,maxlon],colorbarunits='month',colorbarlowleft=[0.1,0.07],/gridme,plotstep=1,chars=2,colorbarchars=1.5,colorbarloffset=[0.025,0],colorbarsteps=[0,1,2,3,4,5,6,7,8,9,10,11,12]

ENDIF

END

