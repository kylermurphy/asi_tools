;+
; :Function:
;     asi_peakogram
;
; :Description:
;     Find peaks in brightness along fixed longitudes
;     to identify the latitudinal motion of the aurora
;     and compare motion at different latitudes. 
;     
;     Defaults to finding 2 peaks in brightness along
;     a single geomagnetic longitude.
;     
;     Relies on:
;     asi_load_data( ) 
;
; :Calling Sequence:
;     dat = asi_peakogram('site_array', t0, dt)
;     
; :Example:
; 
;     Get peakogram for the Gillam THEMIS station from a single longitude
;     dat = asi_peakogram('gill_themis', '2011-04-09/04:24:00', 6, /minutes,n_longitude=1)
;     
;     Get peakogram for the Gillam REGO station, consider only points with elevation
;     greater then 25 degrees, add a tplot variable which shows the approximate 
;     position and brightness of the peaks
;     dat = asi_peakogram('gill_rego', '2015-02-02/10:00:00', 60, /minutes,n_longitude=1, min_elevation=25, /add_tplot)
;
; :Params:
;    site - 4 character site followed by array name '????_array'
;           array can be themis, rego, rgb, blueline
;    t0 - Start time for loading. These can be string 'YYYY-MM-DD/hh:mm:ss'
;           or double (seconds; since 1970).
;    dt - Amount of time to load, defaults to hours when passed to asi_load_data( )
;
; :Keywords:
;    minutes - specify dt in minutes when passed to asi_load_data( )
;    hours - specify dt in hours when passed to asi_load_data( )
;    alt - altitude to get asi coordinates from 
;          0 - 90 km, 1 - 110 km, 2 - 150 km, defaults to 1 - 110 km
;    n_peaks - number of peaks to search for along each longitude slice, default 2
;    n_longitudes - number of longitude slices, default 1
;    longitudes - specify the longitude slices to search along, if not set
;                 the camera is divided into sections based on n_longitudes
;    longitude_tresh - max difference when defining the longitude slices along the
;                 y dimension of the camera. If the difference is larger this position
;                 is ignored
;    px_smooth - the number of pixels to smooth the asi image before searching for peaks
;    moon - flag to remove the moon from the imager using the mask returned by 
;           asi_moon_mask( ). The mask is generated from the first miniute of imager data
;    min_elevation - the minimum elevation of the camera to consider when searching for
;           peaks, creates a 2D mask [img size x, img size y] values with good elevation
;           are 1, all other are NaN
;    add_tplot - add a tplot variable which approximates the location and brightness of 
;                peaks for each longitude slice 
;    
;    _EXTRA - extra keywords that can be passed to asi_load_data( ) and asi_peakogram_getpks( )
;             e.g., /verbose, /no_update, ,force_download, /no_download
;     
; :Defaults:
;     dt - in hours
;     alt - 110 km
;     n_peaks - 2
;     n_longitudes - 1
;     longitude_thresh - 0.2
;     px_smooth - 5
;     min_elevation - 10
;     
; :Return:
;     A structure containing the geomagnetic positions of the peaks in brightness
;     for each longitude slice
;     
;     asi_site - site 
;     asi_array - array
;     pk_lon_val - geomagnetic longitude for each longitude slice
;     pk_lat - geomagnetic latitude for each peak in brightness and longitude slice
;              [time, n_longitude, n_peaks]
;     pk_lon - geomagnetic longitude for each peak, same format as pk_lat
;     pk_amp - amplitude of each peak, same format as pk_lat
;     pk_pos - position along the longitude slice of each peak, same format as pk_lat
;              this can be used with x_pos and y_pos to define peak position within the
;              image pixel cooridantes
;              
;              x and y position for time zero, first longitude and first peak
;              
;              x_pkpos = x_pos[0,pk_pos[0,0,0]]
;              y_pkpos = y_pos[0,pk_pos[0,0,0]]
;              
;     x_pos - the x position in the imager that defines the longitude slice
;             [n_longitude, pixel]
;     y_pos - the y position in the imager that defines the longitude slice
;             [n_longitude, pixel]
;             
;             You can get the slice through the imager for defining the longitude slice using
;             img_slice = asi_img[x_pos[0,*],y_pos[0.*]
;             
;             With the station skymap, x_pos and y_pos can be used to define the lat and lon
;             of the slice
;     n_lon - number of longitude slices
;     n_pk - number of peaks 
;     asi_x - image size of the asi in the x-direction
;     asi_y - image size of the asi in the y-direction
;              
;     
; :Author: krmurph1
;
; :Modification:
;
;-
function asi_peakogram, $
  site, $ ; ASI site to load/download
  t0, $ ; star time for loading/downloading
  dt, $ ; duration (default hours)
  minutes=minutes, $ ; duration in minutes
  hours=hours, $ ; duration in hours
  alt = alt, $ ; altitude to use, 0 - 90 km, 1 - 110 km, 2 - 150 km
  n_peaks = n_peaks, $ ; number of peaks to find, defaults to 2
  n_longitudes = n_longitudes, $ ; number of latitdues to use, defaults to 1
  longitudes = longitudes, $ ; longitudes to use, defaults to center FOV
  longitude_thresh = longitude_thresh, $ ; threshold for longitude difference, default 0.2
  px_smooth = px_smooth, $ ; smoothing of image, default is 5 
  moon = moon, $ ; remove moon
  min_elevation = min_elevation, $ ; minimum elvation of plot, default is 10
  add_tplot = add_tplot, $ ; add an approximate tplot
  _EXTRA=ex
  
  
  asi_init
  
  ; make the data loading quiet
  dprint, getdebug=debug0
  dprint, setdebug=0
  
  ; if multiple sites are passed return
  ;a structure of strucuters where each
  ;structure within the main is the data
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
      if size(alt,/type) eq 0 then alt_pass=!null $
        else if n_elements(alt) eq site.length then alt_pass=alt[i] else alt_pass=alt[0]
      
      if size(n_peaks,/type) eq 0 then pks_pass=!null $
        else if n_elements(n_peaks) eq site.length then pks_pass=n_peaks[i] else pks_pass=n_peaks[0]
      
      if size(n_longitudes,/type) eq 0 then nlon_pass=!null $
        else if n_elements(n_longitudes) eq site.length then nlon_pass=n_longitudes[i] else nlon_pass=n_longitudes[0] 
      
      ; if longitudes has a similar size as asi_sites
      ; pass that set of longitudes 
      
      ; this can be done two ways, via a list 
      ; with the same number of elements as asi_sites
      ; this allows different number of longitudes
      ; for each station 
      ; 
      ; or via an array where the second dimension 
      ; is the same length as asi_sites
      
      lon_sz = size(longitudes)
      if lon_sz[-2] eq 11 and lon_sz[1] eq site.length then lon_pass=longitudes[i] $
      else if lon_sz[0] eq 2 and lon_sz[2] eq site.length then lon_pass=reform(longitudes[*,i]) $
        else if lon_sz[-2] eq 0 then lon_pass=!null else lon_pass=longitudes[0]
      
      if size(longitude_thresh,/type) eq 0 then thresh_pass=!null $
        else if n_elements(longitude_thresh) eq site.length then thresh_pass=longitude_thresh[i] $
        else thresh_pass=longitude_thresh[0]

      if size(px_smooth,/type) eq 0 then px_pass=!null $
        else if n_elements(px_smooth) eq site.length then px_pass=px_smooth[i] else px_pass=px_smooth[0]
      
      if size(moon,/type) eq 0 then m_pass=!null $
        else if n_elements(moon) eq site.length then m_pass=moon[i] else m_pass=moon[0]   

      if size(min_elevation,/type) eq 0 then ele_pass=!null $
        else if n_elements(min_elevation) eq site.length then ele_pass=min_elevation[i] $ 
        else ele_pass=min_elevation[0]
      
      ;get peaks
      r_dat=asi_peakogram(asi_loading, t0, dt, minutes=minutes, hours=hours, $
              alt=alt_pass, n_peaks=pk_pass, n_longitudes=nlon_pass, longitudes=lon_pass, $
              px_smooth=px_pass, moon=m_pass, min_elevation=ele_pass, $
              add_tplot=add_tplot, _EXTRA=ex)

      r_str = create_struct(r_str,asi_loading,r_dat)
    endfor

    r_str = create_struct(r_str,'n_sites',site.length)

    return, r_str
  endif
  
  if size(alt,/type) ne 0 then alt = alt else alt = 1
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
  
  if keyword_set(longitude_thresh) then lon_min=longitude_thresh else lon_min=0.2
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
  
  x_pos = intarr(n_lon,asi_paths.asi_y)
  y_pos = x_pos
  for j=0L, n_lon-1 do begin
    for i=0L, asi_paths.asi_y-1 do begin
      min_val = min(abs(reform(lon_arr[*,i])-pk_lon[j]),/nan)
      if min_val lt lon_min then x_pos[j,i] = !C else x_pos[j,i] = -1
      y_pos[j,i] = i 
    endfor
  endfor
  
  
  if keyword_set(moon) then begin
    dprint, dlevel=0, 'Generating moon mask from first minute of data for '+site
    trex_imager_readfile, asi_paths.asi_paths[0], img, meta, count=img_c
    moon_mask = asi_moon_mask(img)
    moon_mask = rotate(moon_mask,asi_paths.skymap_rotated_by)
    
    stop
  endif
  
  ; loop through the paths and find the peaks
  ; individual files are processed to reduce
  ; the amount of data loaded
  
  dprint, dlevel=0, 'Finding peaks for '+site
  
  paths = asi_paths.asi_paths
  pk_temp = list( )
  for i=0L, paths.length-1 do begin
    pk_val = asi_peakogram_getpks(paths[i], $
      i_rot=asi_paths.skymap_rotated_by, x_pos=x_pos, y_pos=y_pos, $
      n_longitudes=n_lon, n_peaks=n_pks, $
      px_smooth=px_smth, moon=moon_mask, mask=ele_mask, $
      _EXTRA=ex)
    pk_temp.Add, pk_val
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
 
  return, {asi_site:asi_paths.asi_site, asi_array:asi_paths.asi_array, $
    pk_lon_val:pk_lon, pk_lat:pk_lat_arr,pk_lon:pk_lon_arr, $
    pk_amp:pk_amp_arr, pk_pos:pk_pos_arr, n_lon:n_lon, n_pk:n_pks, $
    x_pos:x_pos, y_pos:y_pos, asi_x:asi_paths.asi_x, asi_y:asi_paths.asi_y, asi_paths:paths}
  
end



;MAIN
;test
fixplot
;dat = asi_peakogram('gill_themis', '2011-04-09/04:24:00', 6, /minutes,n_longitude=1)
;dat = asi_peakogram('fykn_themis', '2008-02-15/08:00:00', 120, /minutes,n_longitude=2, /add_tplot)
;dat = asi_peakogram('gill_rego', '2015-02-02/10:00:00', 60, /minutes,n_longitude=1, min_elevation=25, /add_tplot)

;dat = asi_peakogram(['snkq_themis','gill_rego','fsim_themis'],'2015-02-02/10:00:00', 60, $
;  alt=[0,1,1], n_peaks=[3,2,1], n_longitudes=[7,8,9], longitudes=list([45,5],[22],[12]), $
;  px_smooth=[5,10,11], moon=[0,1,1], min_elevation=[25,10,19])

;dat = asi_peakogram(['snkq_themis','gill_rego','fsim_themis'],'2015-02-03/03:18:00', 42, /minutes, /add_tplot, /verbose)
;dat = asi_peakogram(['gill_themis','fsmi_themis','fsim_themis','fykn_themis'],'2010-02-16/07:00:00', 45, /minutes, /add_tplot, /verbose)

dat = asi_peakogram(['snkq_themis','gill_rego','fsim_themis'],'2015-02-03/03:18:00', 42, moon=[1,1,1], /minutes, /add_tplot, /verbose)


end
