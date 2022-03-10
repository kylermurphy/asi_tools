;+
; :Function:
;    asi_moon_mask
;
; :Description:
;    
;    Create a mask where the moon is
;    so that it can be ignored when processing
;    data 
;    
; :Calling Sequence:
;     dat = asi_load_data(asi_img)
;     
; :Example:
; 
;     Load data from snkq and create a moon
;     mask from 1 minute of data
;     
;     dat = asi_load_data('snkq_themis','2015-02-03/03:18:00', 1, /minutes)
;     mask = asi_moon_mask(dat.asi_img)
;     
; :Params:
;    
;    asi_img - an image or array of images 
;              if an array is passed the array
;              of images is used to create a 
;              minimum image
;
; :Keywords:
; 
;    outlier - define the threshold for outliers
;              other wise the outliers are defined
;              as the UQ+1.5*IQR of the minimum image
;              UQ - upper quartile
;              IQR - inter qaurtile range
;               upper quartile - lower quartile
;               
; :Return:
; 
;     a mask the same size as the image passed
;     values where the moon is are set to NaN
;     the mask can be applied by
;     
;     asi_img*mask
;      
; :Author: krmurph1
;-
function asi_moon_mask, asi_img, outlier=outlier

  ; define a quiet image from the images
  ;passed, this helps to ensure the
  ;moon is the brightest feature   


  im_sz = size(asi_img)
  im_min = reform(asi_img[*,*,0])

  ; for each pixel the intensity of
  ;the minimum intensity
  ;of all images passed  
  for i=0L, im_sz[1]-1 do begin
    for j=0L, im_sz[2]-1 do begin
      im_min[i,j] = min(asi_img[i,j,*])
    endfor
  endfor
  
  ; from the quiet image define an intensity
  ;for outlying pixels if not passed
  ;this is the upper quartile intensity plus
  ;1.5 times the inter quartile range
  if keyword_set(outlier) then outlier=outlier else begin
    im_sort = im_min[sort(im_min,/L64)]
    lq = im_sort[im_sort.length*0.25]
    uq = im_sort[im_sort.length*0.75]
    iqr = uq-lq
    outlier = uq+1.5*iqr
  endelse
  
  ; define outlying pixels 
  roi_pixels = where(im_min ge outlier, complement=gd, bc)
  ; use the outlying pixels to grow the region
  new_roi_pixels = region_grow(im_min, roi_pixels, stddev_multiplier=1.5, all_neighbors=1)
  ; use the new region to define the moon mask
  moon_mask=fltarr(im_sz[1],im_sz[2])
  moon_mask[*] = 1
  moon_mask[new_roi_pixels] = !values.f_nan
  moon_mask[roi_pixels] = !values.f_nan
  
  return, moon_mask
  
end




;MAIN
; test
 

dat = asi_load_data('snkq_themis','2015-02-03/03:18:00', 1, /minutes)

;dat

mask = asi_moon_mask(dat.asi_img)


end
