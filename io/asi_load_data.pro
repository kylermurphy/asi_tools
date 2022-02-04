; this might have to be a for loop
; might be able to call itself from within itself


function asi_load_data, site, t0, dt, seconds=seconds, minutes=minutes, hours=hours, themis=themis, rego=rego, rgb=rgb, blue_line=blue_line  


  asi_init
  
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
    keyword_set(seconds): deltat = dt
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
  t_arr = dindgen((te-ts)/60)+ts
  
  stop
   



  ; set download url and
  ; set local download directories
  if keyword_set(themis) then begin
    url = !asi_tools.themis_url
    dir = 'THEMIS\'
    chk_site = asi_is_site(asi_site,/themis)
    ;f_path = asi_themis_path(site,t)
  endif else if keyword_set(rego) then begin
    url = !asi_tools.rego_url
    dir = 'REGO\'
    chk_site = asi_is_site(asi_site,/rego)
  endif else if keyword_set(rgb) then begin
    url = !asi_tools.rgb_url
    dir = 'TREX\RGB\'
    chk_site = asi_is_site(asi_site,/rgb)
  endif else if keyword_set(blueline) then begin
    ; no current sky maps
  endif else begin
    url = !asi_tools.themis_url
    dir = 'THEMIS\'
    chk_site = asi_is_site(asi_site,/themis)
  endelse
  
  if chk_site eq 0 then begin
    print, 'Site not appart of input array'
    return, 0
  endif
  
  ;finish setting up url directory
  url = url+'stream0/'
  
  
  
  return,0
  
end


;Main 
; test

;file='https://data.phys.ucalgary.ca/sort_by_instrument/all_sky_camera/TREx/RGB/stream0/2020/04/12/fsmi_rgb-??/ut04/20200412_0418_fsmi_rgb-01_full.pgm.gz'
;ld = 'D:\data\asi_tools\'
;path = spd_download(remote_file=file, local_path=ld)

dat = asi_load_data('gill', '2013-01-01/00:01:22', 3)


end

