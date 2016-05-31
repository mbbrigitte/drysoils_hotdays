PRO read_monthly_EI_Temp,temp,lotem,latem,startyear=startyear,ny=ny

; PURPOSE: read in Temp (Interim) data 
;	   ; use for corrleation stuff: lag_corr_SM_Temp etc.
; 	   ; ny= number of years
nm=12


;---------------------------------------------------------
; Read Temperature Interim and calc anomalies TInt_ano
;---------------------------------------------------------

outlon=1440  ; number of longitudes for outdata
outlat=721 ; number of latitudes for outdata
outdata=FLTARR(outlon, outlat, nm, ny)
FOR i=0,ny-1 DO BEGIN               ;loop read in years
   FOR j=0,nm-1 DO BEGIN            ;loop read in month
    a=startyear+i
    b=1+j
    IF ((i EQ 0) and (j EQ 0)) THEN print, a, b   
    IF ((i EQ ny-1) and (j EQ nm-1)) THEN print, 'Temp readin done, arrived at', a, b

      filewant_st='/net/firebolt/data/brmuelle/Temperature/ERA_Interim/NetCDF/TempInterim_'+string(a,format='(i4)')+'_'+string(b,format='(i2.2)')+'.nc'
     variable='TEMPERATURE'
     revlat=0
     revlon=0
     cent=1
     file=filewant_st           ; Read the variables
     cdfid=ncdf_open(file)      ;open netcdf "filewant"
     varid=ncdf_varid(cdfid, variable) ;data read
     ncdf_varget, cdfid, varid, ev 
     varid=ncdf_varid(cdfid, 'LON')  ;readin longitude
     ncdf_varget, cdfid, varid,lo       
     varid=ncdf_varid(cdfid, 'LAT')  ;readin latitude
     ncdf_varget, cdfid, varid, la
     ncdf_close,cdfid
     IF (i EQ 0) AND (j EQ 0) THEN print,'lon(0), lat(0) ',lo(0), la(0)

     index=where(ev EQ -9999,ncount)
     IF ncount GT 1 THEN ev(index)=!Values.F_NAN
     outdata(*,*,j,i)=ev        ; save in outdata
     filewant_st=0                     ; reset the filewant_st to avoid errors
   ENDFOR                            ;loop read in month
ENDFOR                               ; loop read year

ntn=startyear+INDGEN(ny)
nlo=N_ELEMENTS(lo)
nla=N_ELEMENTS(la)


   lo1=lo[720:1439]-360
   lo2=lo[0:719]
   Tempmap2=outdata[720:1439,*,*,*]
   Tempmap3=outdata[0:719,*,*,*]
   evp=[Tempmap2,Tempmap3]
   temp=REVERSE(evp,2) ;latitudes reverse 
   lotem=[lo1,lo2]
   latem=REVERSE(la)
; output temp, lotem, latem
END

