function asi_peakogram_getpks, $
  paths, $ ; path to asi data
  x_pos = x_pos, $ ; x position of image to search along for peaks
  y_pos = y_pos, $ ; y position of image to search along for peaks
  n_longitudes = n_longitudes, $ ; number of longitudes 
  n_peaks = n_peaks, $ ; number of peaks to search for
  px_smooth=px_smooth, $ ; number of pixels to smooth over
  moon=moon ; remove the moon if set
  
  
  if not keyword_set(x_pos) or not keyword_set(y_pos) $
    or not keyword_set(n_peaks) or not keyword_set(n_longitudes) $
    or not keyword_set(px_smooth) $
    then message, 'Error - keywords x_pos, y_pos, n_peaks, px_smooth must all be set.'
  
  x_temp = x_pos
  y_temp = y_pos
  npk_temp = n_peaks
  nlon_temp = n_longitudes
  px_temp = px_smooth
  
  ;load the asi data
  trex_imager_readfile, paths, img, meta, count=img_c
  
  t_img = time_double(meta[*].exposure_start_cdf,/epoch)
  
  im_sz = size(img)
  x_sz = im_sz[1]
  y_sz = im_sz[2]
  
  ; array of data to return
  ; number of images, longitude slices, peaks
  ; last two dimesions are the pk pos
  pk_dat = fltarr(img_c,nlon_temp,npk_temp,2)
  pk_dat[*] = !values.f_nan
  
  window,0
  !p.multi=[0,2,1]
  
  ; loop through the images and find the peaks
  for i=0L, img_c-1 do begin
    ; smooth the image
    im_temp = smooth(img[*,*,i],px_temp,/edge_truncate,/nan)
    ; loop through the longitude slice
    for j=0L, nlon_temp-1 do begin
      ; get the longitude slices of data
      lon_line = reform(im_temp[x_temp[j,*],y_temp[j,*]])
      ; find where there was bad data and set 
      ; those values of lon_line to NaN
      bd_pos = where(x_temp[j,*] lt 0 or y_temp[j,*] lt 0, c)
      if c gt 0 then lon_line[bd_pos] = !values.f_nan
      
      ; find the peaks
      pk_pos = sigma_peaks(lon_line,0,0,count=tot_pk)
      pk_amp = lon_line[pk_pos]
      
      ;sort in descending peak amplitude
      pk_sort = reverse(sort(pk_amp))
      pk_amp = pk_amp[pk_sort]
      pk_pos = pk_pos[pk_sort]
      
      ; fill the array
      if tot_pk gt npk_temp then begin
        pk_dat[i,j,0:npk_temp-1,0] = pk_pos[0:npk_temp-1]
        pk_dat[i,j,0:npk_temp-1,1] = pk_amp[0:npk_temp-1]
      endif else begin
        ; if the number of peaks found
        ; is less then that requested
        ; fill with found the rest stay NaN
        pk_dat[i,j,0:tot_pk-1,0] = pk_pos
        pk_dat[i,j,0:tot_pk-1,1] = pk_amp
      endelse
      
      plot,[0,x_sz-1],[0,y_sz-1],/nodata,/isotropic
      tvscale, alog10(im_temp),/overplot
      plots, x_temp[j,*],y_temp[j,*], psym=3
      plots, x_temp[j,pk_pos[0:npk_temp-1]], y_temp[j,pk_pos[0:npk_temp-1]], psym = 4
      
      plot, lon_line
      plots, pk_pos, lon_line[pk_pos], psym=2
      wait,0.01
    endfor
  endfor
  
  return, {pk_dat:pk_dat,t_th:t_img}
  
end
  
; main
;  test
; 
  
;a = asi_peakogram_getpks('hi',x_pos=1, y_pos=1, n_peaks=1, px_smooth=1, n_longitudes=1, moon=0)
dat = asi_peakogram('gill_themis', '2011-04-09/04:24:00', 6, /minutes,n_longitude=1)

end  
   