;+
; :Function:
;     asi_download
; 
; :Description:
;     Download save file skymaps for various 
;     ASI arrays and return the paths to the
;     download files.
; 
; :Calling Sequence:
;     path = asi_download_skymap(site=site)     
;
; :Example:
;
;     Download Gillam THEMIS Skymaps
;       path=asi_download_skymap(site='gill')
;       path=asi_download_skymap(site='gill', /themis)
;   
;     Download Gillam RGB Skymaps
;       path=asi_download_skymap(site='gill', /rgb)
;
; :Input:
; 
; :Keywords:
; 
; :Optional:
;     site - ASI to download skymaps, sites can be passed
;            as "site_array" to define the array they are
;            associated with, e.g., 'gill_rego'
;     themis - download THEMIS skymaps
;     rego - download REGO Skymaps
;     rgb - download TREx RGB skymaps
;     blueline - download TREx Blue Line skymaps
;     _EXTRA - additional keywords for spd_download( )
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
;     site - gill
;     array - THEMIS
; 
; :Return:
;     Returns the local paths to the downloaded files
; 
;
; :Author: krmurphy - kylemurphy.spacephys@gmail.com
;
; :Modification:
;-
function asi_download_skymap, $
  site=site, $ ; ASI site to download 
  themis=themis, $ ; Download skymaps for THEMIS array
  rego=rego, $ ; Download skymaps for REGO (red line) array
  rgb=rgb, $ ; Download skymaps for TREx RGB
  blueline=blueline, $ ; Download skympas for TREx blue line
  _EXTRA=ex

  asi_init

  if keyword_set(site) then site = site else site = 'gill'
  
  ;determine  if more then 4 characters
  ;are passed and set the correct array
  if strlen(site) gt 4 then begin
    s_str = strsplit(site,'_',/extract)
    array = strlowcase(s_str[1])
    if array eq 'themis' then themis=1 $
      else if array eq 'rego' then rego=1 $
      else if array eq 'rgb' then rbg=1 $
      else if array eq 'blueline' then blueline=1 
  endif
  

  ; set download url and
  ; set local download directories
  if keyword_set(themis) then begin
    url = !asi_tools.themis_url+'/skymaps/'
    dir = 'THEMIS\skymaps\'
    chk_site = asi_is_site(site,/themis)
  endif else if keyword_set(rego) then begin
    url = !asi_tools.rego_url+'/skymap/'
    dir = 'REGO\skymaps\'
    chk_site = asi_is_site(site,/rego)
  endif else if keyword_set(rgb) then begin
    url = !asi_tools.rgb_url+'/skymaps/'
    dir = 'TREX\RGB\skymaps\'
    chk_site = asi_is_site(site,/rgb)
  endif else if keyword_set(blueline) then begin
    ; no current sky maps
  endif else begin
    url = !asi_tools.themis_url+'/skymaps/'
    dir = 'THEMIS\skymaps\'
    chk_site = asi_is_site(site,/themis)
  endelse
  
  if chk_site.is_site eq 0 then begin
    print, 'Site not appart of input array'
    return, 0
  endif
  
  ;get the 4 character code for directory structure
  asi_site = strmid(strlowcase(site),0,4)
  
  ; finish setting up download url
  ;skymaps are stored in individual folders
  ;by date. Since we don't know these dates
  ;find the folders first
  url=url+asi_site+'/'+asi_site+'_*'
  ; get full paths to data
  spd_download_expand,url
  
  ; setup the local download directory
  dir=filepath(dir,root_dir=!asi_tools.data_dir)
  dir=filepath(asi_site,root_dir=dir)+path_sep()
  
  ; want to add all skymaps to a common folder
  ;as opposed to individual folders
  paths = strarr(url.length)
  for i=0L, url.length-1 do begin   
    paths[i] = spd_download(remote_file=url[i]+'*.sav', local_path=dir,no_update=1, _EXTRA=ex)
  endfor

  return,paths

end


