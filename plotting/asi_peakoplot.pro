pro asi_peakoplot, pk_str, imin=imin, imax=imax, overplot=overplot, sym_ct=sym_ct, _EXTRA=ex

  !x.style=1
  !y.style=1
  
  if keyword_set(sym_ct) then sym_ct = sym_ct else sym_ct = [56,59,49,50,51]
  if keyword_set(sz_min) then sz_min = sz_min else sz_min = 0.1
  if keyword_set(sz_max) then sz_max = sz_max else sz_max = 1.5
  
  if sz_min gt sz_max then begin
    sz_min = 0.1
    sz_max = 1.5
  endif
  
  ;create a filled circle symbol
  phi=findgen(32)*(!PI*2/32.)
  phi = [phi, phi(0)]
  usersym, cos(phi), sin(phi), /fill
  
  ;scale symbol sizes
  sym_sz = normalize_vec(pk_str.pk_amp)*(sz_max-sz_min)+sz_min
  
  
  x_min = min(pk_str.pk_time,max=x_max,/nan) 
  y_min = pk_str.lat_min
  y_max = pk_str.lat_max
  
  x_tk = time_ticks([x_min,x_max], offset)
  x_tk.xtickv = x_tk.xtickv
  
  ; add the tick array to the extra
  ;reference but only if the same 
  ;keywords haven't been passed
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
    endif
  endif else ex = x_tk
    
  if size(offset,/type) ne 0 then offset=offset else offset=0
  
  plot, [x_min,x_max]-offset,[y_min,y_max], /nodata, _EXTRA=ex
  
  if keyword_set(imax) then cmax=imax else cmax=max(pk_str.pk_amp)
  if keyword_set(imin) then cmin=imin else cmin=min(pk_str.pk_amp)
  
  ct_num=0
  for i=0L, pk_str.n_lon-1 do begin
    loadct,sym_ct[ct_num],/silent
    
    c_plot = bytscl(reform(pk_str.pk_amp[*,i,*]),min=cmin,max=cmax,/nan)
    for j=0L, pk_str.n_pk-1 do begin
      for w=0L, n_elements(pk_str.pk_time)-1 do $
        plots, pk_str.pk_time[w]-offset, pk_str.pk_lat[w,i,j], psym=sym(1), $
        color=c_plot[w,j],  symsize=sym_sz[w,i,j], noclip=0
    endfor
    
      
    ct_num++
    if ct_num gt sym_ct.length then ct_num=0
  endfor
  ;draw_color_scale,range=[cmin,cmax]
  ;loadct,0
  ;axis,yaxis=1,ystyle=1
  
  
end



; MAIN
; test


restore,'D:\asi_tools_peakotest.sav',/verbose 
dat = asi_peakogram('gill_rego', '2015-02-02/10:00:00', 60, /minutes,n_longitude=2, min_elevation=15)
fixplot
!x.omargin=[0,15]
asi_peakoplot, dat, yrange=[64,67]


end
