;+
; :Function:
;     asi_peakoplot
;     
; :Description:
;    Plot the auroral peaks along each longitude slice as
;    a function of time from the structure returned from 
;    asi_peakogram.
;    
;    Will plot multiple stations if multiple stations
;    where passed to asi_peakogram and the return 
;    structure contatins multiple stations.
;    
; :Calling Sequence:
; 
;   asi_peakoplot, pk_str
;    
; :Example:        
;    
;    Peakoplot for a single station and calling pkcursor to show auroral
;    images and peak locations
;        
;    dat = asi_peakogram('gill_rego', '2015-02-02/10:20:00', 20, /minutes,n_longitude=1, min_elevation=15)
;    asi_peakoplot, dat, /log, yrange=[64,67], /pkcursor
;    
;    Peakoplot for multiple stations setting keywords for each stations 
;    
;    dat = asi_peakogram(['gill_rego','fsim_themis'] '2015-02-02/10:20:00', 20, /minutes,n_longitude=1, min_elevation=15)
;    asi_peakoplot, dat, /log, imin=[700,6000], imax=[1100,10000],yrange=[[64,67],[56,70]], sym_ct=[62,63]
;
; :Params:
;    pk_str - structure returned from asi_peakogram   
;
; :Keywords:
;    trange - time range for plotting
;    imin - minimum intensity for plotting
;    imax - maximum intensity for plotting
;    sym_ct - color table for plotting auroral intensity
;    ct_file - color table file to load color table
;    log - log the intensity
;    overplot - plot the peakogram on the current plot
;    pkcursor - implement plotting of the 2D asi image and peaks in aurora
;        brightness at each time stamp. 
;               Can only be when a single station is passed. This is to
;        reduce memory ussage when loading large amounts of images.
;    _EXTRA - passed to plot procedure to set plot keywords, e.g., yrange
;    
; :Defaults:
;    None
;    
; :Return:
;    None      
;
; :Author: krmurphy - kylemurphy.spacephys@gmail.com
;-
pro asi_peakoplot, $
  pk_str, $ ; a structure or structure of structures containing the output from asi_peakogram
  trange=trange, $ ; time range to plot
  imin=imin, $ ; minimum intensity
  imax=imax, $ ; maximum intesnity
  sym_ct=sym_ct, $ ; color table to use
  ct_file=ct_file, $ ; file to load color table
  log=log, $ ; log 10 the intensity before plotting
  overplot=overplot, $ ; overplot on the current plot
  pkcursor=pkcursor, $ ; plot the asi image with peaks and longitude slices
   _EXTRA=ex ; extra keywords for plotting (e.g., x and y titles)

  !x.style=1
  !y.style=1
  
  asi_init
  
  ;set up multi plot for loop here
  ; check for tag names and if multiple
  ; sites exist
  pk_tags = tag_names(pk_str)
  ns_pos = where(pk_tags eq 'N_SITES',nc)
  if nc gt 0 then begin
    p_sites = where(pk_tags ne 'N_SITES',pc)
    n_sites = pk_str.n_sites
    
    ;check if overplot keyword was set and 
    ; if there are enough plots left in 
    ; the current window, if not create a window
    ; for plotting
    if !p.multi[0] lt n_sites and not keyword_set(overplot) then begin
      fixplot
      xs = 600
      ys = 200*n_sites
      window, 31, xsize=xs, ysize=ys
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
      if size(sym_ct,/type) eq 0 then sym_ct_pass=!null $
        else if n_elements(sym_ct) eq n_sites then sym_ct_pass=sym_ct[i] else sym_ct_pass=sym_ct[0]
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
     
      asi_peakoplot, pk_str.(p_sites[i]),trange=trange_pass, imin=imin_pass, imax=imax_pass, $
        sym_ct=sym_ct_pass, ct_file=ct_file_pass, log=log_pass, overplot=overplot, pkcursor=0, $
        ytitle=pk_tags[p_sites[i]], yrange=yrange 
    endfor
    
    ;once looped through all sites stop
    return
  endif
  ;stop
  
  if keyword_set(sym_ct) then sym_ct = sym_ct else sym_ct = [56,59,49,50,51]
  if keyword_set(sz_min) then sz_min = sz_min else sz_min = 0.1
  if keyword_set(sz_max) then sz_max = sz_max else sz_max = 1.5
  if keyword_set(overplot) then op = 1 else op = 0
  
  if size(pk_str,/type) ne 8 then begin
    print, 'No data in the peakogram structure'
    return 
  endif
  
  if sz_min gt sz_max then begin
    sz_min = 0.1
    sz_max = 1.5
  endif
    
  if keyword_set(trange) then xrange=time_double(trange) else begin   
    x_min = min(pk_str.pk_time,max=x_max,/nan)
    xrange = [x_min,x_max] 
  endelse
  y_min = pk_str.lat_min
  y_max = pk_str.lat_max
  
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
  if op eq 0 then plot, [x_min,x_max],[y_min,y_max], /nodata, _EXTRA=ex else offset=0
  
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
        plots, pk_str.pk_time[w], pk_str.pk_lat[w,i,j], psym=sym(1), $
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
  
  
  if keyword_set(pkcursor) then asi_peakocursor, pk_str
  
end



; MAIN
; test

fixplot
!x.omargin=[0,15]
window, 0, xsize=500, ysize=500
restore,'D:\data\asi_tools\peakoplot_test.sav',/verbose
asi_peakoplot, dat, /log, yrange=[64,67],/pkcursor

stop

!x.omargin=[0,15]
window, 0, xsize=500, ysize=500
dat = asi_peakogram('gill_rego', '2015-02-02/10:20:00', 20, /minutes,n_longitude=1, min_elevation=15)


fixplot
!x.omargin=[0,15]
window, 0, xsize=500, ysize=500
!p.multi=[0,1,2]
asi_peakoplot, dat, /log, imin=[700,6000], imax=[1100,10000],yrange=[[64,67],[56,70]], sym_ct=[62,63]

window, 2, xsize=500, ysize=600
!p.multi=[0,1,2]
asi_peakoplot, dat, /log, imin=[700,6000], imax=[1100,10000],yrange=[64,67]


stop
window, 2
fixplot
!x.omargin=[0,15]
plot, dat.gill_rego.pk_time,dat.gill_rego.pk_time, yrange=[62,70],/nodata

asi_peakoplot, dat,/log,/overplot, imax=[1100,10000],yrange=[[64,67],[56,70]], sym_ct=[62,63]


end
