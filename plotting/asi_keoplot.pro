pro asi_keoplot, $
  keo_str, $ ; a structure or structure of structures containing the output from asi_nskeo or asi_ewkeo
  trange=trange, $ ; time range to plot
  imin=imin, $ ; minimum intensity
  imax=imax, $ ; maximum intesnity
  keo_ct = keo_ct, $ ; keogram color table to use
  ct_file=ct_file, $ ; file to load color table
  log=log, $ ; log 10 the intensity before plotting
  overplot=overplot, $ ; overplot on the current plot
  pkcursor=pkcursor, $ ; plot the asi image with peaks and longitude slices
  _EXTRA=ex ; extra keywords for plotting (e.g., x and y titles)
  
  !x.style=1
  !y.style=1

  asi_init
  
  if size(keo_str,/type) ne 8 then begin
    print, 'No data in the keogram structure'
    return
  endif
  
  ;set up multi plot for loop here
  ; check for tag names and if multiple
  ; sites exist
  keo_tags = tag_names(keo_str)
  ns_pos = where(keo_tags eq 'N_SITES',nc)
  if nc gt 0 then begin


    p_sites = where(keo_tags ne 'N_SITES',pc)
    n_sites = keo_tags.n_sites

    ;check if overplot keyword was set and
    ; if there are enough plots left in
    ; the current window, if not create a window
    ; for plotting
    if !p.multi[0] lt n_sites and not keyword_set(overplot) then begin
      fixplot
      xs = 600
      ys = 200*n_sites
      window, 25, xsize=xs, ysize=ys
      !p.multi = [0,1,n_sites]
      if n_sites gt 2 then !p.charsize=2.0
      !x.omargin=[0,15]
      !y.omargin=[1,0]
      !y.margin=[2,1]
    endif

    ;check if keywords are set
    ; if they are the same size as the number of asi's passed
    ; then pass them in order with the asi loaded
    ; if they aren't the same size pass the first element
    ; if they haven't been set set the passed values to null
    if size(trange,/type) eq 0 then trange_pass=!null $
    else trange_pass=trange_pass

    ;loop through stations and plot
    for i=0L, p_sites.length-1 do begin
      if size(imin,/type) eq 0 then imin_pass=!null $
        else if n_elements(imin) eq n_sites then imin_pass=imin[i] else imin_pass=imin[0]
      if size(imax,/type) eq 0 then imax_pass=!null $
        else if n_elements(imax) eq n_sites then imax_pass=imax[i] else imax_pass=imax[0]
      if size(keo_ct,/type) eq 0 then keo_ct_pass=!null $
        else if n_elements(keo_ct) eq n_sites then keo_ct_pass=keo_ct[i] else keo_ct_pass=keo_ct[0]     
      if size(ct_file,/type) eq 0 then ct_file_pass=!null $
        else if n_elements(ct_file) eq n_sites then ct_file_pass=ct_file[i] else ct_file_pass=ct_file[0]
      if size(log,/type) eq 0 then log_pass=!null $
        else if n_elements(log) eq n_sites then log_pass=log[i] else log_pass=log[0]

      ;check if yrange keyword has been passed via _EXTRA=ex
      if size(ex,/type) eq 8 then begin
        ex_tags = tag_names(ex)
        yr = where(ex_tags eq 'YRANGE',yc)
        if yc eq 1 then begin
          if n_elements(ex.yrange[0,*]) eq n_sites then yrange=ex.yrange[*,i] $
          else if n_elements(ex.yrange) eq 2 then yrange=ex.yrange[*] else yrange=!null
        endif
      endif

;      asi_keoplot, pk_str.(p_sites[i]),trange=trange_pass, imin=imin_pass, imax=imax_pass, $
;        sym_ct=sym_ct_pass, ct_file=ct_file_pass, log=log_pass, overplot=overplot, $
;        ytitle=pk_tags[p_sites[i]], yrange=yrange
    endfor

    ;once looped through all sites stop
    return
  endif
  
  if keyword_set(overplot) then op = 1 else op = 0
  if keyword_set(keo_ct) then keo_ct = keo_ct else keo_ct = 25
  
  ; get tags and find the y-axis values
  k_tags = tag_names(keo_str)
  y_pos = where(k_tags eq 'LON_AXIS' or k_tags eq 'LAT_AXIS', c)
  if c ne 1 then message, 'No y-axis found for plotting'
  y_axis = keo_str.(y_pos)
  
  ; get the lat or lon range for plotting
  r_pos = where(k_tags eq 'LON_RANGE' or k_tags eq 'LAT_RANGE', c)
  if c ne 1 then yrange = [min(keo_str.(y_pos),max=ymax,/nan),ymax] else yrange = keo_str.(r_pos)
  
  ; define the time range for plotting
  if keyword_set(trange) then xrange=time_double(trange) else begin
    x_min = min(keo_str.asi_t,max=x_max,/nan)
    xrange = [x_min,x_max]
  endelse

  ; get axis ticks
  x_tk = time_ticks(xrange, offset)
  x_tk.xtickv = x_tk.xtickv+offset
  x_tk.xrange = x_tk.xrange+offset


  ; add the tick array to the extra
  ;reference but only if the same
  ;keywords haven't been passed

  ; if the same keywords have been passed
  ;set offset to 0
  if size(ex,/type) eq 8 then begin
    ex_tags = tag_names(ex)
    ;determine if xtickvalues have been
    ;passed as _EXTRA keyword arguments
    xticks = where(ex_tags eq 'XTICKNAME' or ex_tags eq 'XTICKV' or $
      ex_tags eq 'XTICKS' or ex_tags eq 'XMINOR', tc)
    if tc eq 0 then begin
      tk_tags = tag_names(x_tk)
      for i=0l, tk_tags.length-1 do begin
        if tk_tags[i] eq 'XRANGE' or tk_tags[i] eq 'XTITLE' then continue
        ex = create_struct(ex,tk_tags[i],x_tk.(i))
      endfor
    endif else offset=0
  endif else ex = x_tk

  ; skip plotting if you are overplotting on existing plot
  ; if overplotting set offset to 0 as other ticks will be
  ; used
  if op eq 0 then plot, xrange,yrange, /nodata, _EXTRA=ex else offset=0
  
  ;get the x and y ranges of the current plot
  ; and find corresponding pixels of the 
  ; keogram
  yp = !y.crange
  xp = !x.crange
  
  y_pix = where(y_axis ge yp[0] and y_axis le yp[1],y_c)
  x_pix = where(keo_str.asi_t ge xp[0] and keo_str.asi_t le xp[1],x_c)
  if y_c lt 2 or x_c lt 2 then message, 'Y-range is to small for plotting, not enought pixels'
  
  ; get keogram plotting array
  parr = keo_str.asi_keogram 
  parr = parr[*,y_pix]
  parr = parr[x_pix,*]
 
  ; find the scales for plotting if not set
  ; and log values if log is set
  if keyword_set(imax) then cmax=imax else cmax=max(parr)
  if keyword_set(imin) then cmin=imin else cmin=min(parr)

  crange = [cmin,cmax]
  
  if keyword_set(log) then begin
    parr = alog10(parr)
    crange = alog10(crange)
    cplot = 10.^crange
  endif else begin
    cplot = crange
  endelse

  ; plot keogram and add color bar
  loadct,keo_ct,/silent, file=ct_file
  tvscale,parr, /overplot,/nointerpolation, minvalue=crange[0], maxvalue=crange[1]
  asi_colorbar,range=cplot,position=pos, log=log, yticklen=-1, ytitle='Intensity'

  
  
  
  
  
  
end
  
  
; MAIN
; testing
;


;keo_gbay = asi_ewkeo('gbay_themis', '2015-02-18/01:55:00', 25, 50, 18, 29, 61, 63, /minutes, min_elevation=15, /add_tplot,/local,/keo_pos)
;
;window, 0, xsize=750, ysize=200
;!x.margin=[15,15]
;asi_keoplot, keo_gbay,/log, ytitle='Geomagnetic Longitude', xtitle='Time - UT'

keo_3 = asi_nskeo(['kuuj_themis','snkq_themis','gill_themis'],'2011-04-09:04:00:00', 60, $
  [100,100,100],[62,62,62],[72,72,72],[12.5,-5,-30],[13.5,-3,-29], $
  /minutes, min_elevation=[15,15,15], /add_tplot, /local)
  
asi_keoplot, keo_3, /log


end