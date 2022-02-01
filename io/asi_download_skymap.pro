pro asi_download_skymap, site=site, themis=themis, rego=rego, rgb=rgb, blue_line=blue_line, $
_EXTRA=ex

  asi_init

  if keyword_set(site) then site = site else site = 'gill'

  ; set download url and
  ; set local download directories
  if keyword_set(themis) then begin
    url = 'https://data.phys.ucalgary.ca/sort_by_project/THEMIS/asi/skymaps/'
    dir = 'THEMIS\skymaps\'
    chk_site = is_site(site,/themis)
  endif else if keyword_set(rego) then begin
    url = 'https://data.phys.ucalgary.ca/sort_by_project/GO-Canada/REGO/skymap/'
    dir = 'REGO\skymaps\'
    chk_site = is_site(site,/rego)
  endif else if keyword_set(rgb) then begin
    url = 'https://data.phys.ucalgary.ca/sort_by_project/TREx/RGB/skymaps/'
    dir = 'RGB\skymaps\'
    chk_site = is_site(site,/rgb)
  endif else if keyword_set(blue_line) then begin
    ; no current sky maps
  endif else begin
    url = 'https://data.phys.ucalgary.ca/sort_by_project/THEMIS/asi/skymaps/'
    dir = 'THEMIS\skymaps\'
    chk_site = is_site(site,/themis)
  endelse
  
  if chk_site eq 0 then begin
    print, 'Site not appart of input array'
    return
  endif
  
  ; finish setting up download url
  ;skymaps are stored in individual folders
  ;by date. Since we don't know these dates
  ;find the folders first
  url=url+site+'/'+site+'_*'
  ; get full paths to data
  spd_download_expand,url
  
  stop
  
  ; setup the local download directory
  dir=!asi_tools.data_dir+'\'+dir
  
  ; want to add all skymaps to a common folder
  ;as opposed to individual folders
  for i=0L, url.length-1 do begin
    
    paths = spd_download(remote_file=url[i]+'*.sav', local_path=dir)
  endfor
  
  
  stop 


end


;MAIN
;test

asi_download_skymap,site='gill'

end
