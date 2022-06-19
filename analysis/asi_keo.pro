; moon doesn't do anything yet
; may need to return the pixels 
; and a single image to show 
; where the keogram comes from

; do the above here with a keyword that will be
; forwarded from nskeo and ewkeo

function asi_keo, $ 
  site, $ ; ASI site to load/download
  t0, $ ; star time for loading/downloading
  dt, $ ; duration (default hours)
  n_lat, $ ; number of latitude bins
  min_lat, $ ; minimum latitude
  max_lat, $ ; maximum latitude
  n_lon, $ ; number of longitude bins
  min_lon, $ ; minimum longitude
  max_lon, $ ; maximum longitude
  minutes=minutes, $ ; duration in minutes
  hours=hours, $ ; duration in hours
  alt=alt, $ ; altidue to use for lat and lon values
  moon = moon, $ ; remove moon
  min_elevation = min_elevation, $ ; minimum elvation of plot, default is 10
  _EXTRA=ex
  
  asi_init
  
  
  if n_lat gt 1 and n_lon gt 1 then begin
    message,'One of n_lat or n_lon must be 1 to create a keogram'
  endif

  
  ;set altitude value
  if size(alt,/type) ne 0 then alt = alt else alt = 1
  if alt gt 2 or alt lt 0 then begin
    print, 'alt must be 0, 1, or 2'
    print, 'setting alt to 1 - 110 km'
    alt = 1
  endif
  
  if keyword_set(moon) then moon=1 else moon=0
  if keyword_set(min_elevation) then min_elevation=min_elevation else min_elevation=10
  
  ;get skymap and paths to data
  dprint, dlevel=0, 'Loading Skymap and retreiving data and paths for '+site
  asi_paths = asi_load_data(site,t0,dt,minutes=minutes,hours=hours,no_load=1,_EXTRA=ex)
  
  

  ;no data returned
  if size(asi_paths,/type) eq 2 then return, 0
  
  ;create elvation mask
  gd_ele = where(asi_paths.asi_skymap.full_elevation gt min_elevation, ele_c)
  ele_mask = fltarr(asi_paths.asi_x,asi_paths.asi_y)
  ele_mask[*] = !values.f_nan
  if ele_c gt 0 then ele_mask[gd_ele]=1
  
  ;get pixel locations of the keogram from the skymap
  pix_val = asi_keopix(asi_paths.asi_skymap, n_lat, min_lat, max_lat, $
              n_lon, min_lon, max_lon, alt=alt, mask=ele_mask)
  
  
  ;loop through paths
  ; get imager time
  ; rotate images and convert two 1 d arrays
  ;   this helps with the pix_val locations 
  ;   for the keogram and speeds up averaging
  t_keo = []
  img_1d = []
  
  for i=0L, n_elements(asi_paths.asi_paths)-1 do begin
    ;read data
    trex_imager_readfile,asi_paths.asi_paths[i],img,meta, count=img_c

    t_img = time_double(meta[*].exposure_start_cdf,/epoch)
    img_temp = make_array(asi_paths.asi_x*asi_paths.asi_y,t_img.length,type=img_t)
    for j=0L, t_img.length-1 do begin
      temp = rotate(reform(img[*,*,j]),asi_paths.skymap_rotated_by)
      ; grab a single image to show where the keogram
      ;is take from
      if i eq 0 and j eq 0 then asi_img = temp
      img_temp[*,j] = temp[*]
    endfor
    
    t_keo = [t_keo,t_img]
    img_1d = [[img_1d],[img_temp]]
  endfor
  
  ;use the pix_val.pix_loc to generate the keogram
  keo_sz  = n_elements(pix_val.pix_num)
  img_keo = fltarr(t_keo.length,keo_sz)
  ;loop over pixel bins from pix_val to 
  ; generate keogram  
  for i=0L, keo_sz-1 do begin
    gd_pix = where(pix_val.pix_loc[i,*] ge 0, gc, /L64)
    if gc lt 1 then continue
    img_keo[*,i] = mean(img_1d[pix_val.pix_loc[i,gd_pix],*],dimension=1)
  endfor
  
  ;add keo plotting location here
  
  ;create return structure here
  
  r_str = {asi_site:asi_paths.asi_site, asi_array:asi_paths.asi_array, asi_t:t_keo, $ 
           asi_keogram:img_keo, keo_coor:pix_val.coord_axis}
           
  return, r_str 
end




;main
; testing

pos1 = asi_keo('gill_rego', '2015-02-02/10:00:00', 60, 100, 62, 68, 1, -26, -25, /minutes, min_elevation=15, /local)
pos1 = asi_keo('gill_themis', '2011-04-09/04:00:00', 30, 100, 62, 68, 1, -26, -25, /minutes, min_elevation=15, /local)
pos2 = asi_keo('gill_rego', '2015-02-02/10:20:00', 20, 1, 65.5, 66, 40, -30, -20, /minutes, min_elevation=15, /local)

end


