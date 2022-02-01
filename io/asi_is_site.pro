function asi_is_site, site, themis=themis, rego=rego, rgb=rgb, blue_line=blue_line

  asi_init
  
  asi_site = strlowcase(site)

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
    if c gt 0 then return, 1 else return, 0
    
  endif else if keyword_set(rego) then begin
    chk_site = where(rego_asi eq asi_site, c)
    if c gt 0 then return, 1 else return, 0
   
  endif else if keyword_set(rgb) then begin
    chk_site = where(rgb_asi eq asi_site, c)
    if c gt 0 then return, 1 else return, 0
    
  endif else if keyword_set(blue_line) then begin
    chk_site = where(rgb_asi eq asi_site, c)
    if c gt 0 then return, 1 else return, 0
    
  endif else begin
    ;default to themis
    chk_site = where(themis_asi eq asi_site, c)
    if c gt 0 then return, 1 else return, 0
  endelse 

end