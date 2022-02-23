;+
; :Function:
;     asi_load_data
;
; :Description:
;     Load ASI data for a particular time range. 
;     Check if data exists by querying the server 
;     and wheter it has alread been downloaded to the 
;     local disk. 
;     
;     Download and loads 1 minute PGM files. 
;     
; :Calling Sequence:
;     dat = asi_load_data(site, t0, dt)
;     
; :Example:
; 
;     Download Gillam REGO data
;     dat = asi_load_data('gill_rego', '2015-02-02/10:00:00', 40, /minutes)
;     dat = asi_load_data('gill', '2015-02-02/10:00:00', 40, /minutes, /rego)
;  
;     
;     
; :Params:
;    site - ASI to download and load data, sites can be passed
;            as "site_array" to define the array they are
;            associated with, e.g., 'gill_rego'
;    t0 - Start time for loading. These can be string 'YYYY-MM-DD/hh:mm:ss' 
;            or double (seconds; since 1970).  
;    dt - Amount of time to load, defaults to hours.
;
; :Keywords:
;    minutes - specify dt in minutes
;    hours - specify dt in hours
;    themis - load data from THEMIS array
;    rego - loadd data from REGO array
;    rgb - load data from TREX RGB array
;    blue_line - load data from TREX Blueline array
;    path_only - return only the local paths to the files
;    meta_data - return the meta data structure in the pgm's
;    no_load - don't load the data, only return the paths and skymap
;    _EXTRA - additional keywords for spd_download( )
;    
;     _EXTRA examples
;    
;     last_version - Flag to only download the last in file in a lexically sorted
;                    list when multiple matches are found using wildcards
;     no_update - Flag to not overwrite existing file
;     force_download - Flag to always overwrite existing file
;     no_download - Flag to not download remote files
;     
; :Defaults:
;     dt - in hours
;     array - themis
;     
; :Return:
;     A structure containg the images downloaded/loaded for the specified time.
;
; :Author: krmurphy - kylemurphy.spacephys@gmail.com
;
; :Modification:
;-
function asi_load_data, $
  site, $ ; ASI site to load/download
  t0, $ ; star time for loading/downloading
  dt, $ ; duration (default hours)
  minutes=minutes, $ ; duration in minutes
  hours=hours, $ ; duration in hours
  themis=themis, $ ; load from THEMIS array
  rego=rego, $ ; load from REGO array
  rgb=rgb, $ ; load from TREX RGB array
  blue_line=blue_line, $ ; load from TREX blue line
  path_only=path_only, $ ; return only the paths to the local files
  meta_data=meta_data, $ ; return the meta data along with the data
  no_load=no_load, $ ;don't load the data, only return paths and skymap
  _EXTRA=ex  

  asi_init
  
  ;can add a for loop here if site is an array
  
  ;make sure we use the 4 character code
  asi_site = strmid(strlowcase(site),0,4)

  ;determine if more then 4 characters
  ;are passed and set the correct array
  if strlen(site) gt 4 then begin
    s_str = strsplit(site,'_',/extract)
    array = strlowcase(s_str[1])
    if array eq 'themis' then themis=1 $
    else if array eq 'rego' then rego=1 $
    else if array eq 'rgb' then rgb=1 $
    else if array eq 'blueline' then blueline=1
  endif
  
  ;create a time_series from t0 and dt
  ; if no keyword set assume hours
  case 1 of
    keyword_set(hours):   deltat = dt * 3600.
    keyword_set(minutes): deltat = dt * 60.
    else:                 deltat = dt * 3600.
  endcase
  
  ; files are saved in minute pgm files
  ; create an array stepped in minutes in order 
  ; to define the files to download
  ; 
  ; define the start and end time
  ts = time_double(time_string(t0,tformat='YYYY-MM-DD/hh:mm'))
  te = time_double(time_string(time_double(t0)+deltat+120,tformat='YYYY-MM-DD/hh:mm'))
  ; create minute time series
  t_arr = dindgen((te-ts)/60)*60.+ts

  ; set download url and
  ; set local download directories
  if keyword_set(themis) then begin
    url = !asi_tools.themis_url
    dir = 'THEMIS'+path_sep()
    tf  = 'YYYYMMDD_hhmm'
    chk_site = asi_is_site(asi_site,/themis)
    ;f_path = asi_themis_path(site,t)
  endif else if keyword_set(rego) then begin
    url = !asi_tools.rego_url
    dir = 'REGO'+path_sep()
    tf  = 'YYYYMMDD_hhmm'
    chk_site = asi_is_site(asi_site,/rego)
  endif else if keyword_set(rgb) then begin
    url = !asi_tools.rgb_url
    dir = 'TREX'+path_sep()+'RGB'+path_sep()
    tf  = 'YYYYMMDD_hhmm'
    chk_site = asi_is_site(asi_site,/rgb)
  endif else if keyword_set(blueline) then begin
    ; no current sky maps
  endif else begin
    themis=1
    url = !asi_tools.themis_url
    dir = 'THEMIS'+path_sep()
    tf  = 'YYYYMMDD_hhmm'
    chk_site = asi_is_site(asi_site,/themis)
  endelse
  
  if chk_site.is_site eq 0 then begin
    print, 'Site not appart of input array'
    return, 0
  endif
  
  
  ;the data directories are broken down
  ; by 
  ; YYYY/MM/DD/site_?/uthh/
  ; where ? is a number and array code
  ; search for a single file to find the
  ; array code by searching for the url 
  ; of the first and last frame
  url_test = url+'stream0/'+ $
    time_string(t_arr[[0,-1]],tformat='YYYY')+'/'+ $
    time_string(t_arr[[0,-1]],tformat='MM')+'/'+ $
    time_string(t_arr[[0,-1]],tformat='DD')+'/'+ $
    asi_site+'*'
  
  ;check if directory exists
  spd_download_expand, url_test
  if strlen(url_test[0]) eq 0 then return, 0
  
  ;get the ? appended to the site
  asi_append = strsplit(url_test[0],'/_',/extract)
  asi_text = '_'+asi_append[-1]
  
  ;create path for downloading
  dir = filepath(dir,root_dir=!asi_tools.data_dir)
  paths = strarr(t_arr.length)
  
  ;loop through minute time array
  ; search url for file, download, and save
  ; path
  for i=0l, t_arr.length-1 do begin
    ;full url to file 
    full_url = url+'stream0/'+ $
      time_string(t_arr[i],tformat='YYYY')+'/'+ $
      time_string(t_arr[i],tformat='MM')+'/'+ $
      time_string(t_arr[i],tformat='DD')+'/'+ $
      asi_site+asi_text+'/ut'+ $
      time_string(t_arr[i],tformat='hh')+'/'+ $
      time_string(t_arr[i],tformat=tf)+'*.pgm.gz'
    
    
    ;full path to download directory  
    dl_dir = dir + $
      time_string(t_arr[i],tformat='YYYY')+path_sep()+ $
      time_string(t_arr[i],tformat='MM')+path_sep()+ $
      time_string(t_arr[i],tformat='DD')+path_sep()+ $
      time_string(t_arr[i],tformat='hh')+path_sep()+ $
      asi_site+path_sep()
    
    paths[i] = spd_download(remote_file=full_url,local_path=dl_dir, no_update=1, _EXTRA=ex)
  endfor
  
  ; load the skymap
  ;find all skymaps for current site/array
  skymap_path= asi_download_skymap(site=asi_site,themis=themis,rego=rego,rgb=rgb,blueline=blueline)
  ; check the type returned to make sure
  ;paths are actually returned
  skymap_type = size(skymap_path,/type)
  
  if skymap_type eq 7 then begin
    skymap_dir = file_dirname(skymap_path[0])
    skymap_date= strarr(skymap_path.length)
    for i=0L, skymap_path.length-1 do begin
      dd = strsplit(skymap_path[i],'_-',/extract)
      skymap_date[i]=dd[-3]
    endfor
    ;find skymap closest in time to data
    skymap_date = time_double(skymap_date,tformat='YYYYMMDD')
    min_diff = min(abs(skymap_date-t_arr[0]),/nan)
    skymap_pos = !C
    load_date = skymap_date[skymap_pos]
    
    ;find the geomagnetic skymap
    skymap_mag = file_search(skymap_dir, $
      chk_site.array+'_skymap_'+asi_site+'_geomag_'+time_string(load_date,tformat='YYYYMMDD')+'*.sav', $
      count=fc)
    
    ;if the geomagnetic skymap
    ; doesn't exist create and load
    ; it otherwise restore the save
    ; file  
    if fc eq 0 then begin
      skymap_dat = asi_skymap_geomag(skymap_path[skymap_pos])
      skymap = skymap_dat.skymap
    endif else begin
      restore, skymap_mag
    endelse
  endif else begin
    skymap = 0
  endelse
  
  ; return only paths
  if keyword_set(path_only) then return, {asi_paths:paths}
  
  if keyword_set(no_load) then begin
    return, {asi_paths:paths, asi_skymap:skymap}
  endif
                 
  ; read in the PGM files
  trex_imager_readfile,paths,img,meta, count=img_c

  t_img = time_double(meta[*].exposure_start_cdf,/epoch)
  
  r_dat = {asi_site:asi_site, asi_array:chk_site.array, $
            asi_img:img, asi_t:t_img, $
            asi_x:n_elements(img[*,0,0]), asi_y:n_elements(img[0,*,0]), $
            asi_frames:t_img.length,  asi_paths:paths, asi_skymap:skymap}
  
  if keyword_set(meta_data) then r_dat = create_struct(r_dat,'asi_meta',meta)

  return, r_dat
  
end


;Main 
; test


; read some themis data
;dat = asi_load_data('rank', '2017-09-15/02:30:00', 30, /minutes)
;dat = asi_load_data('gill_themis', '2007-03-07/05:52:00', 8, /minutes)

; read some rego data
;dat = asi_load_data('gill', '2015-02-02/10:00:00', 40, /minutes, /rego)
;dat = asi_load_data('gill_rego', '2018-08-01/06:00:00', 2, /minutes)

; read some TREX RGB data
dat = asi_load_data('fsmi', '2019-02-18/03:25:00', 2, /minutes, /rgb, /meta)
;dat = asi_load_data('fsmi_rgb', '2019-02-18/03:25:00', 30, /minutes, /meta)



;window, xsize=dat.asi_x, ysize=dat.asi_y
;for i=0L, dat.asi_frames do tvscl,alog10(reform((dat.asi_img[*,*,i])))

end

