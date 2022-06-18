function asi_nskeo, $
  site, $ ; ASI site to load/download
  t0, $ ; star time for loading/downloading
  dt, $ ; duration (default hours)
  n_lat, $ ; number of latitude bins
  min_lat, $ ; minimum latitude
  max_lat, $ ; maximum latitude
  min_lon, $ ; minimum longitude
  max_lon, $ ; maximum longitude
  minutes=minutes, $ ; duration in minutes
  hours=hours, $ ; duration in hours
  alt=alt, $ ; altidue to use for lat and lon values
  moon = moon, $ ; remove moon
  min_elevation = min_elevation, $ ; minimum elvation of plot, default is 10
  add_tplot=add_tplot, $
  _EXTRA=ex
  
  
  asi_init
  
  ;the number of longitude bins
  ; is always 1 for a north-south
  ; keogram
  n_lon=1
  
  ; if multiple sites are passed return
  ;a structure of strucuters where each
  ;structure within the main is the keogram
  ;for that station
  if site.length gt 1 then begin
    r_str = { }
    for i=0L, site.length-1 do begin
      
    endfor
    
    return, r_str
  endif
  

  
  keo = asi_keo(site,t0,dt, $
    n_lat,min_lat,max_lat, $
    n_lon,min_lon,max_lon, $ 
    minutes=minutes,hours=hours, $
    alt=alt,moon=moon,min_elevation=min_elevation,_EXTRA=ex)
   
  lat_axis = (keo.keo_coor.lat_low+keo.keo_coor.lat_up)/2.
  lat_range = [keo.keo_coor.lat_min,keo.keo_coor.lat_max]
  
  r_str = {asi_site:keo.asi_site, asi_array:keo.asi_array, asi_t:keo.asi_t, $
           asi_keogram:keo.asi_keogram, lat_axis:lat_axis, lat_range:lat_range}
  
  if keyword_set(add_tplot) then begin
     
    ;store the tplot data
    d = {x:r_str.asi_t, y:r_str.asi_keogram, v:r_str.lat_axis}
    data_att = {coord_sys:'Geomagnetic Latitude', units:'Counts/Intensity/Brightness'}
    dlimits = {SPEC:1,LOG:0, DATA_ATT:data_att, $
      YTITLE:r_str.asi_site+' '+r_str.asi_array +'!CNS Keogram', $
      ZTITLE:'Intensity', $
      ZLOG:1, X_NO_INTERP:1, Y_NO_INTERP:1, $
      OVERLAY:0}
    limits = {ZLOG:1, YLOG:1, YRANGE:lat_range, YSTYLE:1}

    name = r_str.asi_site+'_'+r_str.asi_array
    store_data,name, data=d, dlimits=dlimits, limits=limits 
  endif
  
  return, r_str  
end

;main
; testing

pos1 = asi_nskeo('gill_rego', '2015-02-02/10:00:00', 60, 100, 62, 68, -26, -25, /minutes, min_elevation=15, /local, /add_tplot)

end