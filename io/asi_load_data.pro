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
   
  ; return only paths 
  if keyword_set(path_only) then return, paths
  
  ; read in the PGM files
  themis_imager_readfile_new,paths,img,meta, count=img_c
  
  t_img = time_double(meta[*].exposure_start_cdf,/epoch)
  
  r_dat = {asi_site:asi_site, asi_array:chk_site.array, $
            asi_img:img, asi_t:t_img, $
            asi_x:n_elements(img[*,0,0]), asi_y:n_elements(img[0,*,0]), $
            asi_frames:t_img.length,  asi_paths:paths}
  
  if keyword_set(meta_data) then r_dat = create_struct(r_dat,'asi_meta',meta)

  return, r_dat
  
end



