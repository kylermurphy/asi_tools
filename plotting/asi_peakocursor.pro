pro asi_peakocursor, $
  pk_str ; a structure or structure of structures containing the output from asi_peakogram
 
  if pk_str.asi_array eq 'REGO' then l_ct=3 else l_ct=8
  
  ;get the peakogram 
  ; plotting window and
  ; setup plotting 
  pk_win = !window
  
  ;asi images are generated using a plot
  ;thus plot parameters for this sub-plot could change the overall plot
  xt = !x    ; save previous plot parameters
  yt = !y
  clipt = !p.clip
  
  !x.margin=[5,5]
  !y.margin=[5,5]
  !x.omargin=[0,0]
  !y.omargin=[0,0]

  ;load the asi data and get time
  trex_imager_readfile, pk_str.asi_paths, img, meta, count=img_c
  t_img = time_double(meta[*].exposure_start_cdf,/epoch)

  for i=0L, img_c-1 do img[*,*,i] = reform(img[*,*,i])*pk_str.ele_mask

  ;get limits for plotting
  img_br = total(total(img,1),1)
  max_br = max(img_br)
  max_p =!C
  
  ;get the brightest image
  ; set limits for plotting 
  ; using this image
  b_img = img[*,*,max_p]
  b_img = b_img[*]
  gd = where(b_img gt 0,c)
  if c gt 0 then b_img = b_img[gd]
  s_img = sort(b_img)
  
  pmin=b_img[s_img[s_img.length*0.25]]
  pmax=b_img[s_img[s_img.length*0.99]]
  if pmax gt 10000 then pmax=10000
  
  ;setup the plotting window
  window,30, xsize=400, ysize=400
  
  ;set to peakoplot window
  ; and start cursor
  cursor, xx, yy, /data, /nowait
  old_x = xx
  old_y = yy
  done = 0
  
  plot,[0,pk_str.asi_x],[0,pk_str.asi_y],/nodata,/isotropic, $
    xtickf='asi_nt',ytickf='asi_nt', xticks=1, xminor=1, yticks=1, yminor=1
  ;get position to output time 
  bl = convert_coord(!p.clip[0],!p.clip[1],/device,/to_normal)
  
  ;get asi plot parameters
  ; to move between the two
  ; plots
  x_asi = !x    
  y_asi = !y
  clip_asi = !p.clip
  
  print, '-------------'
  print, 'Starting ASI plotting'
  print, 'Move cursor on window '+strtrim(long(pk_win),2)
  print, 'Left click to exit'
  print, '-------------'
  
  while not done do begin
    wset,pk_win
    cursor,xx,yy, /data,/nowait
    change_pos = abs(old_x-xx)
    old_x = xx
    if change_pos ne 0 then begin
      ;set back to asi plotting
      wset,30
      !x = x_asi
      !y = y_asi
      !p.clip = clip_asi
      
      ; find the position from the 
      ;plot that is closest to the
      ;timestamp of the image
      t_min = min(abs(xx-t_img))
      t_pos = !C
      
      ;plot that image      
      c_image = rotate(reform(img[*,*,t_pos]),pk_str.i_rot)
      c_image = bytscl(alog10(c_image),/nan,min=alog10(pmin),max=alog10(pmax)) 
      loadct,l_ct,/silent
      tvscale, c_image,/overplot
      loadct,0,/silent
      
      ;plot the longitude slices and 
      ; peaks in the peakogram structure
      for w=0L, pk_str.n_lon-1 do begin
        gd_pos = where(pk_str.x_pos[w,*] gt 0 and pk_str.y_pos[w,*] gt 0)
        oplot, pk_str.x_pos[w,gd_pos], pk_str.y_pos[w,gd_pos]
        oplot, pk_str.x_pos[w,pk_str.pk_pos[t_pos,w,*]],pk_str.y_pos[w,pk_str.pk_pos[t_pos,w,*]], psym=sym(6)
      end
      
      xyouts, bl[0],bl[1],time_string(t_img[t_pos]),/normal
    endif
    
    ; restore previous plot parameters
    !x = xt     
    !y = yt
    !p.clip = clipt
    done = !MOUSE.button eq 1
  endwhile
  
  
end

;MAIN
;testing 
fixplot
!x.omargin=[0,15]
window, 0, xsize=500, ysize=500
restore,'D:\data\asi_tools\peakoplot_test.sav',/verbose
asi_peakoplot, dat, /log, yrange=[64,67],/pkcursor



end

