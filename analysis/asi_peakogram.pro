; need to do an minimum elvation check

; need to make -1 position values nan

; should this work on only rotated data? 
; probably yes.

function asi_peakogram, $
  site, $ ; ASI site to load/download
  t0, $ ; star time for loading/downloading
  dt, $ ; duration (default hours)
  minutes=minutes, $ ; duration in minutes
  hours=hours, $ ; duration in hours
  alt = alt, $ ; altitude to use, 0 - 90 km, 1 - 110 km, 2 - 150 km
  n_peaks = n_peaks, $ ; number of peaks to find, defaults to 2
  n_longitudes = n_longitudes, $ ; number of latitdues to use, defaults to 1
  longitudes = longidutes, $ ; longitudes to use, defaults to center FOV
  longitude_tresh = longitude_thresh, $ ; threshold for longitude difference, default 0.2
  px_smooth = px_smooth, $ ; smoothing of image, default is 5 
  moon = moon, $ ; remove moon
  min_elevation = min_elevation, $ ; minimum elvation of plot, default is 10
  add_tplot = add_tplot, $ ; add an approximate tplot
  _EXTRA=ex
  
  
  asi_init
  
  ; make the data loading quiet
  dprint, getdebug=debug0
  dprint, setdebug=0
  
  if keyword_set(alt) then alt = alt else alt = 1
  if alt gt 2 or alt lt 0 then begin
    print, 'alt must be 0, 1, or 2'
    print, 'setting alt to 1 - 110 km'
    alt = 1
  endif
  if keyword_set(n_peaks) then n_pks = n_peaks else n_pks = 2
  if n_pks lt 1 then begin
    print, 'Number of peaks must be larger then 0."
    print, 'Setting to 2'
    n_pks = 2
  endif
  
  if keyword_set(longitude_thresh) then lon_min=lonigtude_thresh else lon_min=0.2
  if keyword_set(px_smooth) then px_smth=px_smooth else px_smth=5
  if keyword_set(moon) then moon=1 else moon=0
  if keyword_set(min_elevation) then min_elevation=min_elevation else min_elevation=10
  
  ;get skymap and paths to data
  dprint, dlevel=0, 'Loading Skymap and retreiving data and paths for '+site
  asi_paths = asi_load_data(site,t0,dt,minutes=minutes,hours=hours, no_load=1,_EXTRA=ex)
    
  ;create elvation mask
  gd_ele = where(asi_paths.asi_skymap.full_elevation gt min_elevation, ele_c)
  ele_mask = fltarr(asi_paths.asi_x,asi_paths.asi_y)
  ele_mask[*] = !values.f_nan
  if ele_c gt 0 then ele_mask[gd_ele]=1
  
  
  ;determine what longitudes to use
  ; if n_longitudes is set then grab
  ; n_longitudes by seperating in percentiles
  ; which divide the data up into n_longitudes+1
  ; sections
  ;otherwise use the longitudes defined by the user 
  ; or the longitude of the center of the FOV
  
  if keyword_set(n_longitudes) and size(n_longitudes,/ type) eq 2  then begin
    n_lon = n_longitudes
    pk_pos = 1./(n_lon+1)
    pk_percentile = (findgen(n_longitudes)+1) * pk_pos
    
    lon_vals = asi_paths.asi_skymap.CENTER_MAG_LONGITUDE[*,asi_paths.asi_y/2.,alt]
    lon_vals = lon_vals[sort(lon_vals)]
    
    pk_lon = lon_vals[pk_percentile*lon_vals.length]
  endif else if keyword_set(longitudes) then begin
    pk_lon = longitudes
    n_lon = pk_lon.length 
  endif else begin
    pk_lon = median(asi_paths.asi_skymap.CENTER_MAG_LONGITUDE[*,*,alt])
    n_lon = 1   
  endelse
  
  ; need to do an elevations check here
  
  lon_arr = reform(asi_paths.asi_skymap.CENTER_MAG_LONGITUDE[*,*,alt])
  lat_arr = reform(asi_paths.asi_skymap.CENTER_MAG_LATITUDE[*,*,alt])
  
  ; loop through the y dimension and find
  ; the closest longitdue in the x direction 
  ; that is within the longitude threshold
  ; points outside this are set -1 in the x position
  
  dprint, dlevel=0, 'Calculating longitude slices for '+site
  
  x_pos = intarr(n_longitudes,asi_paths.asi_y)
  y_pos = x_pos
  for j=0L, n_lon-1 do begin
    for i=0L, asi_paths.asi_y-1 do begin
      min_val = min(abs(reform(lon_arr[*,i])-pk_lon[j]),/nan)
      if min_val lt lon_min then x_pos[j,i] = !C else x_pos[j,i] = -1
      y_pos[j,i] = i 
    endfor
  endfor
  
  ; loop through the paths and find the peaks
  ; individual files are processed to reduce
  ; the amount of data loaded
  
  dprint, dlevel=0, 'Finding peaks for '+site
  
  paths = asi_paths.asi_paths
  pk_temp = []
  for i=0L, paths.length-1 do begin
    pk_val = asi_peakogram_getpks(paths[i], $
      i_rot=asi_paths.skymap_rotated_by, x_pos=x_pos, y_pos=y_pos, $
      n_longitudes=n_lon, n_peaks=n_pks, $
      px_smooth=px_smth, moon=moon, mask=ele_mask, $
      _EXTRA=ex)
    pk_temp = [pk_temp,pk_val]
  endfor
  
  ; unravel the data
  ; pk_dat stores the peak position
  ; and amplitude
  ; [img,lon_peak,n_peaks,2]
  ; the last dimension is position
  pk_dat  = []
  pk_time_arr = []
  for i=0L, pk_temp.length-1 do begin
    pk_dat  = [pk_dat,pk_temp[i].pk_dat]
    pk_time_arr = [pk_time_arr, pk_temp[i].t_th]
  endfor
  
  ; get data in sensible arrays
  pk_lat_arr = fltarr(pk_time_arr.length,n_lon,n_pks)
  pk_lon_arr = pk_lat_arr
  pk_pos_arr = reform(pk_dat[*,*,*,0],pk_time_arr.length,n_lon,n_pks)
  pk_amp_arr = reform(pk_dat[*,*,*,1],pk_time_arr.length,n_lon,n_pks)
  
  for i=0L, n_lon-1 do begin
    for j=0L, n_pks-1 do begin
      pos_temp = pk_pos_arr[*,i,j]
      lat_temp = lat_arr[x_pos[i,pos_temp],y_pos[i,pos_temp]]
      lon_temp = lon_arr[x_pos[i,pos_temp],y_pos[i,pos_temp]]
      
      pk_lat_arr[*,i,j] = lat_temp
      pk_lon_arr[*,i,j] = lon_temp
    endfor
  endfor
  
  
 
  if keyword_set(add_tplot) then begin
  
    ; sort the time array 
    t_sort = sort(pk_time_arr)
    
    ; loop through each long slice
    for i=0L, n_lon-1 do begin
      ; get the latitdues positions
      ;of each longitude slice
      lats = asi_paths.asi_skymap.CENTER_MAG_LATITUDE[*,*,alt]*ele_mask
      lats = lats[x_pos[i,*],y_pos[i,*]]
      
      ; find the lat min and max
      ;values along each slice
      lat_min=min(lats,max=lat_max,/nan)
      lat_bin=0.2
      
      lat_min = floor(lat_min)
      lat_max = ceil(lat_max)
      
      ; create an array of latitudes
      lat_arr = findgen((lat_max-lat_min)/lat_bin)*lat_bin+lat_min
      
      ; create an array that can store the 
      ;intensity of each peak as close to 
      ;the lat array as possible
      plot_arr = fltarr(pk_time_arr.length, lat_arr.length)
      
      ; for each set of peaks find the position 
      ;in the plot_arr that corresponds to the
      ;peak and fill with the peak amplitude
      for j=0L, n_pks-1 do begin
        lat_pos = pk_lat_arr[t_sort,i,j]
        lat_ind = round((lat_pos-lat_min)/lat_bin)        
        plot_arr[indgen(lat_pos.length),lat_ind] = pk_amp_arr[t_sort,i,j]
      endfor  
      
      ;store the tplot data
      d = {x:pk_time_arr[t_sort], y:plot_arr, v:lat_arr}
      data_att = {coord_sys:'Geomagnetic Latitude', units:'Counts/Intensity/Brightness'}
      dlimits = {SPEC:1,LOG:0, DATA_ATT:data_att, $
            YTITLE:'Latitude!CLongitude Slice: '+string(pk_lon[i],format='(F7.2)'), $
            ZTITLE:'Intensity', $
            ZLOG:1, X_NO_INTERP:1, Y_NO_INTERP:1, $
            OVERLAY:0}
      limits = {ZLOG:1, YLOG:1, YRANGE:[lat_min,lat_max], YSTYLE:1}
      
      name = asi_paths.asi_site+'_'+asi_paths.asi_array+'_lon_'+strtrim(i,2)
      
      store_data,name, data=d, dlimits=dlimits, limits=limits
    endfor
  endif

  ; change back to nominal dprint level
  dprint, setdebug=debug0
 
  return, {asi_site:asi_paths.asi_site, asi_array:asi_paths.asi_array, pk_lon_val:pk_lon, $
    pk_lat:pk_lat_arr,pk_lon:pk_lon_arr, pk_amp:pk_amp_arr, $
    pk_pos:pk_pos_arr, x_pos:x_pos, y_pos:y_pos }
  
end



;MAIN
;test
fixplot
;dat = asi_peakogram('gill_themis', '2011-04-09/04:24:00', 6, /minutes,n_longitude=1)
;dat = asi_peakogram('fykn_themis', '2008-02-15/08:00:00', 120, /minutes,n_longitude=2, /add_tplot)
dat = asi_peakogram('gill_rego', '2015-02-02/10:00:00', 60, /minutes,n_longitude=1, min_elevation=25, /add_tplot)

end
