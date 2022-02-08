; this might have to be a for loop
; might be able to call itself from within itself


function asi_load_data, site, t0, dt, minutes=minutes, hours=hours, themis=themis, rego=rego, rgb=rgb, blue_line=blue_line, path_only=path_only, $
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
  
  if chk_site eq 0 then begin
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
  return,0
  
end


;Main 
; test


;dat = asi_load_data('gill', '2013-01-01/00:01:22', 3)
;dat = asi_load_data('gill', '2018-04-07/05:00:00', 8, /minutes, /rego)
dat = asi_load_data('pina', '2019-05-05/06:40:00', 8, /minutes, /rgb)
dat = asi_load_data('pina_rgb', '2019-05-05/06:05:00', 8, /minutes)
dat = asi_load_data('gill_rego', '2018-04-07/05:22:00', 8, /minutes, /rego,/path_only)
dat = asi_load_data('gill_themis', '2018-04-07/05:22:00', 8, /minutes)

end

