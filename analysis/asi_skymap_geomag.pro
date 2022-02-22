function asi_skymap_geomag, sm_file, force_calc=force_calc

  if keyword_set(force_calc) then force_calc=1 else force_calc=0

  ; check for the skymap
  fn = file_search(sm_file, count=fc)
  
  if fc eq 0 then begin
    print, 'Skymap file not found: '+sm_file
    return, 0
  endif

  if strlen(routine_filepath('aacgmidl_v2')) eq 0 then begin
    print, 'The AACGM_V2 library is required'
    print, 'to convert goegraphic coordinates'
    print, 'to geomagnetic'
    print, '----------'
    print, 'Check SPEDAS dependicies to'
    print, 'ensure AACGM_V2 is installed'
    print, '----------'
    
    return, 0
  endif
  
  ; restore skymap file
  restore, fn, /verbose
  
  ; check if geomagnetic file exists
  out_dir = file_dirname(sm_file)
  out_file = strjoin([skymap.project_uid,'skymap',skymap.site_uid,'geomag',skymap.generation_info[0].valid_interval_start],'_')
  out_file = out_dir+path_sep()+out_file+'.sav'
  
  mag_fn = file_search(out_file, count=mag_c)
  if mag_c eq 1 and force_calc ne 1 then begin
    print, 'Geomagnetic skymap exists: '+mag_fn
    print, 'To force recalculation of Geomagnetic'
    print, 'coordinates use /force_calc'
    return, mag_fn
  endif

  ; call the main AACGM_V2 routine which 
  ; sets environmental variables and compiles
  ; the libraries
  aacgm_v2
  
  
  
  year  = long(strmid(skymap.generation_info.valid_interval_start,0,4))
  month = long(strmid(skymap.generation_info.valid_interval_start,4,2))
  day   = long(strmid(skymap.generation_info.valid_interval_start,6,2))
  hour  = long(strmid(skymap.generation_info.valid_interval_start,8,2))
  
  ret = AACGM_v2_SetDateTime(year,month,day,hour)
  
  geo_lat = skymap.FULL_MAP_LATITUDE
  geo_lon = skymap.FULL_MAP_LONGITUDE
  alt     = skymap.FULL_MAP_ALTITUDE/1000.

  mag_lat = geo_lat
  mag_lat[*] = !values.f_nan
  mag_lon    = mag_lat

  print, 'Calculating Geomagnetic Coordinates for skymap from: '+fn
  for i=0L, alt.length-1 do begin
    print, 'Converting Geographic Coordinates for: '+string(alt[i])
    for j=0L, n_elements(geo_lat[*,0,0])-1 do begin
      alt_temp = reform(geo_lat[j,*,i])
      alt_temp[*] = alt[i]
      lat_temp = reform(geo_lat[j,*,i])
      lon_temp = reform(geo_lon[j,*,i])
      geo_mag = cnvcoord_v2(lat_temp,lon_temp,alt_temp)
      
      mag_lat[j,*,i]=geo_mag[0,*]
      mag_lon[j,*,i]=geo_mag[1,*]
    endfor
  endfor
  
  ; add the mag lat and lon
  ; arrays to the skymap
  ; structure
  skymap=create_struct(skymap,'full_map_mag_latitude',mag_lat,'full_map_mag_longitude',mag_lon)
 
  save,skymap,filename=ouf_file
  return, out_file
end


;Main
; test

fn =asi_skymap_geomag('D:\data\asi_tools\REGO\skymaps\gill\rego_skymap_gill_20141102-%2B_vXX.sav', /force_calc)

end


