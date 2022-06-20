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
  keo_pos = keo_pos, $ ; plot the position of the keogram and keogram bins
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
      asi_loading = string(site[i])
      
      ;check if keywords are set
      ; if they are the same size as the number of asi's passed
      ; then pass them in order with the asi loaded
      ; if they aren't the same size pass the first element
      ; if they haven't been set set the passed values to null
      
      if n_lat.length eq site.length then n_lat_p = n_lat[i] else n_lat_p=n_lat[0]
      if min_lat.length eq site.length then min_lat_p = min_lat[i] else min_lat_p=min_lat[0]
      if max_lat.length eq site.length then max_lat_p = max_lat[i] else max_lat_p=max_lat[0]
      if min_lon.length eq site.length then min_lon_p = min_lon[i] else min_lon_p=min_lon[0]
      if max_lon.length eq site.length then max_lon_p = max_lon[i] else max_lon_p=max_lon[0]
      
      if size(alt,/type) eq 0 then alt_pass=!null $
        else if n_elements(alt) eq site.length then alt_pass=alt[i] else alt_pass=alt[0]
      if size(moon,/type) eq 0 then m_pass=!null $
        else if n_elements(moon) eq site.length then m_pass=moon[i] else m_pass=moon[0]
      if size(min_elevation,/type) eq 0 then ele_pass=!null $
        else if n_elements(min_elevation) eq site.length then ele_pass=min_elevation[i] $
        else ele_pass=min_elevation[0]
      
      ;get keo
      r_dat = asi_nskeo(string(asi_loading), t0, dt, $
                n_lat_p,min_lat_p,max_lat_p, $
                min_lon_p,max_lon_p, $
                minutes=minutes, hours=hours, $
                alt=alt_pass, moon=m_pass, $
                min_elevations=ele_pass, $
                keo_pos=keo_pos, add_tplot=add_tplot, _EXTRA=ex)
              
                
      r_str = create_struct(r_str,asi_loading,r_dat)
    endfor
    
    return, r_str
  endif
  

  
  keo = asi_keo(site,t0,dt, $
    n_lat,min_lat,max_lat, $
    n_lon,min_lon,max_lon, $ 
    minutes=minutes,hours=hours, $
    alt=alt,moon=moon,min_elevation=min_elevation, $
    keo_pos=keo_pos,_EXTRA=ex)
  
  ;if no keogram was produced return 0
  if size(keo,/type) ne 8 then return, 0  
   
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
    limits = {ZLOG:1, YLOG:0, YRANGE:lat_range, YSTYLE:1}

    name = r_str.asi_site+'_'+r_str.asi_array
    store_data,name, data=d, dlimits=dlimits, limits=limits 
  endif
  
  return, r_str  
end
