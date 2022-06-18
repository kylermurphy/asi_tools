function asi_keopix, $
  asi_skymap, $ ; asi skymap to find pixel locations within give cgm lat/lon bins
  n_lat, $ ; number of latitude bins
  min_lat, $ ; minimum latitude
  max_lat, $ ; maximum latitude
  n_lon, $ ; number of longitude bins
  min_lon, $ ; minimum longitude
  max_lon, $ ; maximum longitude
  alt=alt, $ ; altidue to use for lat and lon values
  mask=mask ; mask to apply to the lat and lon values
  
  
  asi_init
  
  if size(alt,/type) ne 0 then alt = alt else alt = 1
  if alt gt 2 or alt lt 0 then begin
    print, 'alt must be 0, 1, or 2'
    print, 'setting alt to 1 - 110 km'
    alt = 1
  endif
  ; get the asi sky maps
  asi_lat = reform(asi_skymap.CENTER_MAG_LATITUDE[*,*,alt])
  asi_lon = reform(asi_skymap.CENTER_MAG_LONGITUDE[*,*,alt])
  
  if keyword_set(mask) then begin
    m_size = size(mask)
    a_size = size(asi_lat)
    
    if m_size[0] eq 2 and m_size[1] eq a_size[1] and m_size[2] eq a_size[2] then begin
      asi_lat = asi_lat*mask
      asi_lon = asi_lon*mask
    endif 
  endif
   
  
  ; setup bin positions to identify pixel locations
  ;for each bin identify which pixels in the 
  ;skymap belong to that bin
  lat_bin = (max_lat-min_lat)/float(n_lat)
  lat_low = findgen(n_lat)*lat_bin+min_lat
  lat_up  = lat_low+lat_bin
  
  lon_bin = (max_lon-min_lon)/float(n_lon)
  lon_low = findgen(n_lon)*lon_bin+min_lon
  lon_up  = lon_low+lon_bin
  
  ;create a list so that 
  ;each element of the list
  ;can hold a different number of values
  
  ;not all bins will have the same number of pixels
  ; use a list to store the locations
  pix_loc = list()
  pix_num = list()
  l_lat = list() ; lower lat value for each list position
  u_lat = list() ; upper lat value for each list position
  l_lon = list() ; lower lon value for each list position
  u_lon = list() ; upper lon value for each list position
  
  ;loop over latitude and longitude positions
  x_pix = n_elements(asi_skymap.CENTER_MAG_LATITUDE[*,0,0])
  y_pix = n_elements(asi_skymap.CENTER_MAG_LATITUDE[*,1,0])
  img = intarr(x_pix,y_pix)
  c=1
  for i=0L, lat_low.length-1 do begin
    ;get the latitude pixes
    g_lat = where(asi_lat ge lat_low[i] and asi_lat lt lat_up[i],c_lat, /L64)
    if c_lat lt 1 then continue
    
    for j=0L, lon_low.length-1 do begin
      g_lon = where(asi_lon[g_lat] ge lon_low[j] and asi_lon[g_lat] lt lon_up[j], c_lon, /L64)

      if c_lon lt 1 then continue
      
      pix_loc.add, g_lat[g_lon]
      pix_num.add, c_lon
      l_lat.add, lat_low[i]
      u_lat.add, lat_up[i]
      l_lon.add, lon_low[j]
      u_lon.add, lon_up[j]
      
      img[g_lat[g_lon]]=c
      c++    
    endfor
  endfor
  
  ;unravel arrays here 
  
  
  img=bytscl(img,min=0,max=c-1)
  
  pix_num = pix_num.ToArray()
  l_lat = l_lat.ToArray()
  u_lat = u_lat.ToArray()
  l_lon = l_lon.ToArray()
  u_lon = u_lon.ToArray()
  
  ;-1 corresponds to no pixel locations
  ; because different lat/lon bins have
  ; different number of pixels
  pix_val = lonarr(pix_num.length,max(pix_num))
  pix_val[*] = -1
  for i=0L, pix_num.length-1 do pix_val[i,0:pix_num[i]-1] = pix_loc[i]
  
  coord_axis = {lat_bin:lat_bin, lat_min:min_lat, lat_max:max_lat, lat_low:lat_low, lat_up:lat_up, $
                lon_bin:lon_bin, lon_min:min_lon, lon_max:max_lon, lon_low:lon_low, lon_up:lon_up}
  
  r_str = {pix_loc:pix_val, pix_num:pix_num, lower_lat:l_lat, upper_lat:u_lat, $
          lower_lon:l_lon, upper_lon:u_lon, bin_img:img, coord_axis:coord_axis}
  
  return, r_str
  
  
end




;main
;testing


dat = asi_load_data('gill_rego', '2015-02-02/10:00:00', 40, /minutes, /local,/no_load)

pos2 = asi_keopix(dat.asi_skymap,40,60,70,1,-26,-25)
pos1 = asi_keopix(dat.asi_skymap,1,65,66,40,-30,-20)

end
  