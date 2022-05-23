function asi_keopix, $
  asi_skymap, $
  n_lat, $
  min_lat, $
  max_lat, $
  n_lon, $
  min_lon, $
  max_lon, $
  alt=alt
  
  
  
  if size(alt,/type) ne 0 then alt = alt else alt = 1
  if alt gt 2 or alt lt 0 then begin
    print, 'alt must be 0, 1, or 2'
    print, 'setting alt to 1 - 110 km'
    alt = 1
  endif
  
  ; setup bins to identify pixel locations
  ;for each bin identify which pixels in the 
  ;skymap belong to that bin
  lat_bin = (max_lat-min_lat)/n_lat
  lat_low = findgen(n_lat)*lat_bin+min_lat
  lat_up  = lat_low+lat_bin
  
  lon_bin = (max_lon-min_lon)/n_lon
  lon_low = findgen(n_lon)*lon_bin+min_lon
  lon_up  = lon_low+lon_bin
  
  
  pix_loc
  
  
  stop
  return, 0
  
  
end




;main
;testing


dat = asi_load_data('gill_rego', '2015-02-02/10:00:00', 40, /minutes, /local,/no_load)

pos = asi_keopix(dat.asi_skymap,10,60,70,1,-26,-25)

end
  