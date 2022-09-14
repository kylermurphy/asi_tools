;function to normalize a vector between 0 and 1
function normalize_vec, vec, vmax=vmax, vmin=vmin, bscl=bscl, verbose=verbose, no_outlier=no_outlier
  
  if n_elements(vec) eq 1 then return, 0
  if keyword_set(vmin) then vmin = vmin else vmin = min(vec,/nan)
  if keyword_set(vmax)  then vmax = vmax else vmax = max(vec,/nan)
  if keyword_set(bscl) then bscl = 1 else bscl = 0
  if keyword_set(no_outlier) then no_outlier=1 else no_outlier=0
  
  ; if byte scaling everything
  ; should initially be between 
  ; 0 and 1 so set no_ourlier to 1
  if bscl eq 1 then no_outlier=1
  
  n_v = (vec-float(vmin))/(float(vmax)-float(vmin))
  
  ; remove outliers, e.g. those
  ;values less then 0 or greater
  ;then 1
  if no_outlier eq 1 then begin
    ld = where(n_v gt 1, c)
    if c gt 0 then n_v[ld]=1.
    sd = where(n_v lt 0, c)
    if c gt 0 then n_v[sd]=0.
  endif
  
  ; byte scale the data
  if bscl eq 1 then n_v = n_v*255.
  
  if keyword_set(verbose) then print, 'Vector min and max: ', vmin, vmax
  
  return, n_v
end