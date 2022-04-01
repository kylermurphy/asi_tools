pro asi_peakoplot, $
  pk_str, $ ; a structure or structure of structures containing the output from asi_peakogram
  trange=trange, $ ; time range to plot
  imin=imin, $ ; minimum intensity
  imax=imax, $ ; maximum intesnity
  sym_ct=sym_ct, $ ; color table to use
  ct_file=ct_file, $ ; file to load color table
  log=log, $ ; log 10 the intensity before plotting
  overplot=overplot, $ ; overplot on the current plot
   _EXTRA=ex ; extra keywords for plotting (e.g., x and y titles)

  !x.style=1
  !y.style=1
  
  asi_init
  
  ;set up multi plot for loop here
  
  
  
  if keyword_set(sym_ct) then sym_ct = sym_ct else sym_ct = [56,59,49,50,51]
  if keyword_set(sz_min) then sz_min = sz_min else sz_min = 0.1
  if keyword_set(sz_max) then sz_max = sz_max else sz_max = 1.5
  if keyword_set(overplot) then op = 1 else op = 0
  
  if sz_min gt sz_max then begin
    sz_min = 0.1
    sz_max = 1.5
  endif
  
  ;create a filled circle symbol
  phi=findgen(32)*(!PI*2/32.)
  phi = [phi, phi(0)]
  usersym, cos(phi), sin(phi), /fill
  
  if keyword_set(trange) then xrange=time_double(trange) else begin   
    x_min = min(pk_str.pk_time,max=x_max,/nan)
    xrange = [x_min,x_max] 
  endelse
  y_min = pk_str.lat_min
  y_max = pk_str.lat_max
  
  x_tk = time_ticks(xrange, offset)
  x_tk.xtickv = x_tk.xtickv
  
  
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
  if op eq 0 then plot, [x_min,x_max]-offset,[y_min,y_max], /nodata, _EXTRA=ex else offset=0
  
  ;scale symbol sizes
  sym_sz = normalize_vec(pk_str.pk_amp)*(sz_max-sz_min)+sz_min
  
  ; find the scales for plotting if not set
  ; and log values if log is set
  if keyword_set(imax) then cmax=imax else cmax=max(pk_str.pk_amp)
  if keyword_set(imin) then cmin=imin else cmin=min(pk_str.pk_amp)
  
  crange = [cmin,cmax]
  
  if keyword_set(log) then begin
    crange=alog10([cmin,cmax])
    pk_amp = alog10(pk_str.pk_amp)
    cplot=10.^crange
  endif else begin
    pk_amp = pk_str.pk_amp
    cplot=crange 
  endelse
  
  ; loop through the number of longitudes
  ; number of peaks and plot
  ct_num=0
  for i=0L, pk_str.n_lon-1 do begin
    loadct,sym_ct[ct_num],/silent, file=ct_file
    c_plot = bytscl(reform(pk_amp[*,i,*]),min=crange[0],max=crange[1],/nan)
    for j=0L, pk_str.n_pk-1 do begin
      for w=0L, n_elements(pk_str.pk_time)-1 do begin  
        if pk_str.pk_time[w] lt xrange[0] or pk_str.pk_time[w] gt xrange[1] then continue
        plots, pk_str.pk_time[w]-offset, pk_str.pk_lat[w,i,j], psym=sym(1), $
        color=c_plot[w,j],  symsize=sym_sz[w,i,j], noclip=0
      endfor
    endfor
    
    if i ne pk_str.n_lon-1 then begin
      ytickf='no_ticks'
      ytitle=' '
      yticklen=0
    endif else begin
      ytickf=!null
      ytitle='Intensity'
      yticklen=-1
    endelse
    
    if size(pos,/type) eq 0 then begin
      asi_colorbar,range=cplot,position=pos, ytickf=ytickf, ytitle=ytitle, log=log, yticklen=yticklen
    endif else begin
      xsp = pos[2]-pos[0]
      pos[0] = pos[0]+xsp
      pos[2] = pos[2]+xsp
    
      asi_colorbar,range=cplot,position=pos, ytickf=ytickf, ytitle=ytitle, log=log, yticklen=yticklen
    endelse
    
    ct_num++
    if ct_num gt sym_ct.length then ct_num=0
  endfor
  
end



; MAIN
; test


restore,'D:\asi_tools_peakotest.sav',/verbose 
dat = asi_peakogram('gill_rego', '2015-02-02/10:00:00', 60, /minutes,n_longitude=3, min_elevation=15)
fixplot
!x.omargin=[0,15]
window, 0
;asi_peakoplot, dat, yrange=[64,67],/log, imin=100, imax=10000

window, 2
fixplot
!x.omargin=[0,15]
plot, dat.pk_time,dat.pk_time, yrange=[64.75,66],/nodata

stop
asi_peakoplot, dat,/log, imin=100, imax=10000,/overplot, trange=['2015-02-02/10:20','2015-02-02/10:40']



end
