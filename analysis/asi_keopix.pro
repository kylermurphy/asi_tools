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
  ; get the asi sky maps
  asi_lat = reform(asi_skymap.CENTER_MAG_LATITUDE[*,*,alt])
  asi_lon = reform(asi_skymap.CENTER_MAG_LONGITUDE[*,*,alt]) 
  
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
    g_lat = where(asi_lat ge lat_low[i] and asi_lat lt lat_up[i],c_lat)
    if c_lat lt 1 then continue
    
    for j=0L, lon_low.length-1 do begin
      g_lon = where(asi_lon[g_lat] ge lon_low[j] and asi_lon[g_lat] lt lon_up[j], c_lon)

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
  
  ;unravel arrays here? 
  ;use NaN for bins with fewer points?
  
  img=bytscl(img,min=0,max=c-1)

  loadct,0,/silent
  plot,[0,x_pix-1],[0,y_pix-1],/nodata,ystyle=1,xstyle=1,/isotropic
  loadct,41,/silent
  tvscale,img,/overplot,/nointerpolation
  
  r_str = {pix_loc:pix_loc, pix_num:pix_num, lower_lat:l_lat, upper_lat:u_lat, $
          lower_lon:l_lon, upper_lon:u_lon, bin_img:img}
  
  stop
  
  return, r_str
  
  
end




;main
;testing


dat = asi_load_data('gill_rego', '2015-02-02/10:00:00', 40, /minutes, /local,/no_load)

pos = asi_keopix(dat.asi_skymap,40,60,70,1,-26,-25)

end
  