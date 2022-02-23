;+
; :Function:
;     asi_skymap_geomag
;
; :Description:
;     Calculate the geomagnetic coordinates for
;     skymaps downloaded from the calgary data
;     server. 
;     
;     Calculate the center latitude and longitude
;     of each pixel in both geographic and 
;     geomagnetic coordinates.
;     
;     Require the aacgm library which ships with
;     SPEDAS.
;     
; :Calling Sequence:
;     dat = asi_Skymap_geomag(sm)
;     
; :Params:
;     sm_file - skymap file to calculate coordinates for
;
; :Keywords:
;     force_calc - if the geomagnetic file exists force
;                  recalculation of all parameters
;
; :Defaults:
;     none
;     
; :Return:
;     A structure containing the new skymap with geomagnetic
;     coordinates and pixel center coordinates and the path 
;     to the new save file
;         
; 
; :Author: krmurphy - kylemurphy.spacephys@gmail.com
; 
; :Modification:
;-
function asi_skymap_geomag, $
  sm_file, $ ; skymap file from calgary website to add coordinates to
  force_calc=force_calc ; if geomag file exists overwrite and recalculate

  
  ; call the main AACGM_V2 routine which
  ; sets environmental variables and compiles
  ; the libraries
  sv = execute('aacgmidl_v2')
  
  
  
  if sv eq 0 then begin
    print, '----------'
    print, 'The AACGM_V2 library is required'
    print, 'to convert goegraphic coordinates'
    print, 'to geomagnetic'
    print, '----------'
    print, 'Check SPEDAS dependicies to'
    print, 'ensure AACGM_V2 is installed'
    print, '----------'

    return, 0
  endif
  
  
  ; the AACGM routines in 
  ; SPEDAS need help compiling 
  ; the entire set of routines
  resolve_routine,'aacgmidl_v2',/COMPILE_FULL_FILE, /EITHER
  resolve_routine,'aacgm_v2',/COMPILE_FULL_FILE, /EITHER
  
  if keyword_set(force_calc) then force_calc=1 else force_calc=0

  ; check for the skymap
  fn = file_search(sm_file, count=fc)
  
  if fc eq 0 then begin
    print, 'Skymap file not found: '+sm_file
    return, 0
  endif

  ; restore skymap file
  restore, fn, /verbose
  
  ; check if geomagnetic file exists
  out_dir = file_dirname(sm_file)
  out_file = strjoin([skymap.project_uid,'skymap',skymap.site_uid,'geomag',strmid(skymap.generation_info[0].valid_interval_start,0,8)],'_')
  out_file = out_file+'.sav'
  
  mag_fn = file_search(out_dir+path_sep()+out_file, count=mag_c)
  if mag_c eq 1 and force_calc ne 1 then begin
    print, 'Geomagnetic skymap exists: '+mag_fn
    print, 'To force recalculation of Geomagnetic'
    print, 'coordinates use /force_calc'
    restore, mag_fn
    return, {skymap:skymap, path:mag_fn}
  endif

  year  = long(strmid(skymap.generation_info.valid_interval_start,0,4))
  month = long(strmid(skymap.generation_info.valid_interval_start,4,2))
  day   = long(strmid(skymap.generation_info.valid_interval_start,6,2))
  hour  = long(strmid(skymap.generation_info.valid_interval_start,8,2))
  
  
  sv = call_function('AACGM_v2_SetDateTime',1997,6,25)
  sv = call_function('cnvcoord_v2', 50,120,111)


  ret = call_function('AACGM_v2_SetDateTime',year,month,day,hour)
  
  geo_lat = skymap.FULL_MAP_LATITUDE
  geo_lon = skymap.FULL_MAP_LONGITUDE
  alt     = skymap.FULL_MAP_ALTITUDE/1000.
  
  mag_lat = geo_lat
  mag_lat[*] = !values.f_nan
  mag_lon    = mag_lat
  
  ;x dimensions of array
  imx_sz  = n_elements(mag_lat[*,0,0])-1
  imx_ind = imx_sz-1
  ;y dimensions of array
  imy_sz  = n_elements(mag_lat[0,*,0])-1
  imy_ind = imy_sz-1
  
  mlat = fltarr(imx_sz,imy_sz,alt.length)
  mlat[*] = !values.f_nan
  mlon = mlat
  glat = mlat
  glon = mlat

  print, 'Calculating Geomagnetic Coordinates for skymap from: '+fn
  for i=0L, alt.length-1 do begin
    print, 'Converting Geographic Coordinates for: '+string(alt[i])
    for j=0L, n_elements(geo_lat[*,0,0])-1 do begin
      alt_temp = reform(geo_lat[j,*,i])
      alt_temp[*] = alt[i]
      lat_temp = reform(geo_lat[j,*,i])
      lon_temp = reform(geo_lon[j,*,i])
      geo_mag = call_function('cnvcoord_v2',lat_temp,lon_temp,alt_temp)
      
      mag_lat[j,*,i]=geo_mag[0,*]
      mag_lon[j,*,i]=geo_mag[1,*]
    endfor
    
    ; calculate the "center"
    ; coordinates of each pixel
    mlat[*,*,i] = (mag_lat[0:imx_ind,0:imy_ind,i] + mag_lat[1:imx_ind+1,0:imy_ind,i] + mag_lat[1:imx_ind+1,1:imy_ind+1,i]+mag_lat[0:imx_ind,1:imy_ind+1,i])/4.0
    mlon[*,*,i] = (mag_lon[0:imx_ind,0:imy_ind,i] + mag_lon[1:imx_ind+1,0:imy_ind,i] + mag_lon[1:imx_ind+1,1:imy_ind+1,i]+mag_lon[0:imx_ind,1:imy_ind+1,i])/4.0
 
    glat[*,*,i] = (geo_lat[0:imx_ind,0:imy_ind,i] + geo_lat[1:imx_ind+1,0:imy_ind,i] + geo_lat[1:imx_ind+1,1:imy_ind+1,i]+geo_lat[0:imx_ind,1:imy_ind+1,i])/4.0
    glon[*,*,i] = (geo_lon[0:imx_ind,0:imy_ind,i] + geo_lon[1:imx_ind+1,0:imy_ind,i] + geo_lon[1:imx_ind+1,1:imy_ind+1,i]+geo_lon[0:imx_ind,1:imy_ind+1,i])/4.0
  endfor
  
  
  
  ; add the mag lat and lon
  ; arrays to the skymap
  ; structure
  skymap=create_struct(skymap,'full_map_mag_latitude',mag_lat,'full_map_mag_longitude',mag_lon, $
           'center_geo_latitude', glat, 'center_geo_longitude', glon, $
           'center_mag_latitude', mlat, 'center_mag_longitude', mlon)
 
 
  save, skymap, filename=out_dir+path_sep()+out_file
  
  return, {skymap:reform(skymap), path:out_dir+path_sep()+out_file}
end
