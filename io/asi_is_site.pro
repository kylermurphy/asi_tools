;+
; :Function:
;     asi_is_site
; 
; :Description:
;     Check if a site is apart of a
;     particular ASI array.
;     
; :Calling Sequence:
;     b = asi_is_site('gill_themis')
;     b = asi_is_site('gill', /themis)
;     b = asi_is_site('gill')
;
; :Params:
;     site - site to check for, truncates to 4 characters
;            in case "site_array" is passed
;            if "site_array" is passes "array" defines
;            the array to check for         
;
; :Keywords:
; 
; :Optional:
;     themis - check is site is in THEMIS array
;     rego - check if site is in REGO array
;     rgb - check is site is TREx RGB
;     blueline - check if site is TREx Blue line
;
; :Defualts:
;     Check if site is part of THEMIS.
;  
; :Return:
;   structure  
;   is_site:1 - yes
;   is_site:0 - no
;   array - array tested
;
; :Author: krmurphy - kylemurphy.spacephys@gmail.com
; 
; :Modification:
;-
function asi_is_site, $
  site, $
  themis=themis, $
  rego=rego, $
  rgb=rgb, $
  blueline=blueline

  asi_init
  
  ;make sure we use the 4 character code
  asi_site = strmid(strlowcase(site),0,4)
  
  ;determine array if more then 4 characters
  ;are passed and set the correct array
  if strlen(site) gt 4 then begin
    s_str = strsplit(site,'_',/extract)
    array = strlowcase(s_str[1])
    if array eq 'themis' then themis=1 $
    else if array eq 'rego' then rego=1 $
    else if array eq 'rgb' then rgb=1 $
    else if array eq 'blueline' then blueline=1
  endif

  ;themis sites
  themis_asi = ['atha', 'chbg', 'ekat', 'fsim', 'fsmi', $
                'fykn', 'gako', 'gbay', 'gill', 'inuv', $
                'kapu', 'kian', 'kuuj', 'mcgr', 'nrsq', $
                'pgeo', 'pina', 'rank', 'snap', 'snkq', $
                 'talo', 'tpas', 'whit', 'yknf']
  ;rgb sites               
  rgb_asi = ['atha','fsmi','gill','luck','pina','rabb']
  ;rego sites
  rego_asi = ['atha','fsim','fsmi','gill','kakt','luck', $
              'lyrn','rank','resu','sach','talo']
  ;blue line sites
  blue_asi = ['atha','gill','pina','rabb']
  
  ;check if the site is apart of a given array
  if keyword_set(themis) then begin
    chk_site = where(themis_asi eq asi_site, c)
    if c gt 0 then return, {is_site:1, array:'THEMIS'} else return, {is_site:0, array:'THEMIS'}
    
  endif else if keyword_set(rego) then begin
    chk_site = where(rego_asi eq asi_site, c)
    if c gt 0 then return, {is_site:1, array:'REGO'} else return, {is_site:0, array:'REGO'}
   
  endif else if keyword_set(rgb) then begin
    chk_site = where(rgb_asi eq asi_site, c)
    if c gt 0 then return, {is_site:1, array:'TREX_RGB'} else return, {is_site:0, array:'TREX_RGB'}
    
  endif else if keyword_set(blueline) then begin
    chk_site = where(blue_asi eq asi_site, c)
    if c gt 0 then return, {is_site:1, array:'TREX_Blueline'} else return, {is_site:0, array:'TREX_Blueline'}
    
  endif else begin
    ;default to themis
    chk_site = where(themis_asi eq asi_site, c)
    if c gt 0 then return, {is_site:1, array:'THEMIS'} else return, {is_site:0, array:'THEMIS'}
  endelse 

end