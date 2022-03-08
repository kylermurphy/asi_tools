;+
;+
; :Function:
;     asi_peakogram_getpks
;
; :Description:
;     
;     Find the locations of peak brigthness along
;     a given (x,y) slice through an imager. 
;     
;     The imager data is loaded from the paths
;     argument. 
;     
; :Calling Sequence:
;     dat = asi_load_data(site, t0, dt)
;     
; :Example:
; 
;     Download/load Gillam REGO data
;     dat = asi_load_data('gill_rego', '2015-02-02/10:00:00', 40, /minutes)
;     dat = asi_load_data('gill', '2015-02-02/10:00:00', 40, /minutes, /rego)
;     
;     Download/load Gillam THEMIS and Gillam REGO data
;     dat = asi_load_data(['gill_themis','gill_rego'], '2018-08-01/06:00:00', 2, /minutes)
;          
; :Params:
;    paths - the paths to the asi images to load
;
; :Keywords:
;    i_rot - the amount the image should be rotated to match
;            this is used so that the orientatio of all arrays
;            here match that in asi_peakogram
;    x_pos - the x positions to search for peaks
;    y_pos - the y positions to search for peaks
;    
;    ?_pos - two dimensional array, first dimension defines number
;            of slices, second dimension defines positions
;     
;    n_longitudes - number of longitudes/slices to search along, 
;            should match the number of elements in the first 
;            dimension of x_pos and y_pos
;    n_peaks - the number of peaks to return
;    px_smooth - the pixel size to smooth the image
;    moon - flag to remove the mon
;    mask - mask to apply each image loaded
;    
; :Return:
;    
;    pk_dat - 4 dimensional array
;             0 - time dimension
;             1 - number of slices searched, n_longitudes
;             2 - number of peaks found
;             3 - peak position and peak amplitude for each peak
;             
;             peak position is the index of the x_pos and
;             y_pos arrays where peaks were found
;             
;             peak amplitude is the amplitude of the peak
;     
;    t_th - a time double of the time of each image 
;
; :Author: krmurphy - kylemurphy.spacephys@gmail.com
;
; :Modification:
;
;-
function asi_peakogram_getpks, $
  paths, $ ; path to asi data
  i_rot = i_rot, $ ; what to rotate the loaded images by
  x_pos = x_pos, $ ; x position of image to search along for peaks
  y_pos = y_pos, $ ; y position of image to search along for peaks
  n_longitudes = n_longitudes, $ ; number of longitudes 
  n_peaks = n_peaks, $ ; number of peaks to search for
  px_smooth=px_smooth, $ ; number of pixels to smooth over
  moon=moon, $ ; remove the moon if set
  mask=mask, $ ; apply a mask to the image
  verbose=verbose ; plot some stuff for debugging 
  
  
  if not keyword_set(i_rot) or not keyword_set(x_pos) or not keyword_set(y_pos) $
    or not keyword_set(n_peaks) or not keyword_set(n_longitudes) $
    or not keyword_set(px_smooth) $
    then message, 'Error - keywords i_rot, x_pos, y_pos, n_peaks, px_smooth must all be set.'
  
  if keyword_set(mask) then mask=mask else mask=0
  
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
  
  if keyword_set(verbose) then begin
    window,0
    !p.multi=[0,2,1]
  endif
  
  ; loop through the images and find the peaks
  for i=0L, img_c-1 do begin
    ; rotate the image
    im_temp = rotate(reform(img[*,*,i]),i_rot)
    ; smooth the image
    im_temp = smooth(im_temp,px_temp,/edge_truncate,/nan)
    ; apply the image mask
    if keyword_set(mask) then im_temp = im_temp*mask
    
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
      
      if keyword_set(verbose) then begin
        plot,[0,x_sz-1],[0,y_sz-1],/nodata,/isotropic
        tvscale, alog10(im_temp),/overplot
        plots, x_temp[j,*],y_temp[j,*], psym=3
        plots, x_temp[j,pk_pos[0:npk_temp-1]], y_temp[j,pk_pos[0:npk_temp-1]], psym = 4
      
        plot, lon_line
        plots, pk_pos, lon_line[pk_pos], psym=2
        wait,0.01
      endif
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
   