PRO read_GLDAS, noah_data,noah_lo,noah_la,lays=lays,anom=anom,CLM=CLM,standard=standard,Princeton=Princeton
; PURPOSE: read in   GLDAS SM NOAH

; .compile read_GLDAS
; HISRTORY: 27.7.2012

IF keyword_set(CLM) THEN code1='CLM' ELSE code1='NOAH'
IF keyword_set(Princeton) THEN code1='NOAH-PF'

IF keyword_set(anom) THEN print,'Anomalies of GLDAS '+code1+' are being read' ELSE print,'Absolute GLDAS values: '+code1

         IF keyword_set(anom) THEN file='/net/firebolt/data/brmuelle/TWS_GLDAS/'+code1+'/SM_'+string(lays,format='(i1)')+'Layer_'+code1+'_1983_07_anom.nc' ELSE file='/net/firebolt/data/brmuelle/TWS_GLDAS/'+code1+'/SM_'+string(lays,format='(i1)')+'Layer_'+code1+'_1983_07.nc'
         IF keyword_set(standard) THEN file='/net/firebolt/data/brmuelle/TWS_GLDAS/'+code1+'/Standard_SM_0Layer_'+code1+'_1983_07.nc'

	 varia='SM'
	 result=file_test(file)
	 if (result eq 0) then stop
	 cdfid=ncdf_open(file)           ;open netcdf "file"
	 varid=ncdf_varid(cdfid, varia)  
	 ncdf_varget, cdfid, varid, evn 
	 varid=ncdf_varid(cdfid, 'LON')  ;readin longitude
	 ncdf_varget, cdfid, varid,lonn  
	 varid=ncdf_varid(cdfid, 'LAT')  ;readin  latitude
	 ncdf_varget, cdfid, varid, latn
	 ncdf_close,cdfid
	 nlo=N_ELEMENTS(lonn)
	 nla=N_ELEMENTS(latn)

	 nm=12
	 ny=32
	 IF keyword_set(Princeton) THEN ny=30
	 
      noah_data=FLTARR(nlo,nla,nm,ny)
      FOR i=0L,ny-1 DO BEGIN
	 FOR j=0L,nm-1 DO BEGIN
	 a=1979+i
	 b=1+j
         IF keyword_set(anom) THEN filename='/net/firebolt/data/brmuelle/TWS_GLDAS/'+code1+'/SM_'+string(lays,format='(i1)')+'Layer_'+code1+'_'+string(a,format='(i4)')+'_'+string(b,format='(i2.2)')+'_anom.nc' ELSE filename='/net/firebolt/data/brmuelle/TWS_GLDAS/'+code1+'/SM_'+string(lays,format='(i1)')+'Layer_'+code1+'_'+string(a,format='(i4)')+'_'+string(b,format='(i2.2)')+'.nc'
	 
	 IF keyword_set(standard) THEN filename='/net/firebolt/data/brmuelle/TWS_GLDAS/'+code1+'/Standard_SM_0Layer_'+code1+'_'+string(a,format='(i4)')+'_'+string(b,format='(i2.2)')+'.nc'

	 IF ((i eq 0) and (j eq 0)) THEN print, 'reading file ', filename
	 varia='SM'
	 result=file_test(filename)
	 if (result eq 0) then stop
	 cdfid=ncdf_open(filename)           ;open netcdf "file"
	 varid=ncdf_varid(cdfid, varia)  
	 ncdf_varget, cdfid, varid, evn 
	 varid=ncdf_varid(cdfid, 'LON')  ;readin longitude
	 ncdf_varget, cdfid, varid,lonn  
	 varid=ncdf_varid(cdfid, 'LAT')  ;readin  latitude
	 ncdf_varget, cdfid, varid, latn
	 ncdf_close,cdfid
	 noah_data(*,*,j,i)=evn
	 ENDFOR
      ENDFOR
 noah_lo=lonn
 noah_la=latn
END
