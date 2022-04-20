; Function: sigma_peaks
; Finds the peaks in a spectrum and returns their indices and the number of peaks found.
;
; Inputs:
; y: vector to find peaks from
; nsigma: # of standard deviations above the mean of all peak amplitudes for a peak to 
;   be considered significant. Maximum of 2 decimal places. Only used when t_method=1.
; thres: threshold value used when t_method=2. Maximum of 1 decimal place.
; count: if set to a named variable, returns the number of peaks found
; max_frac: if set, only local maxima whose neighboring points (in a 5-point 
;   neighborhood) are below this fraction of its amplitude are considered peaks. 
;   Valid values are from 0 to 0.999. Maximum of 3 decimal places. If not set, all 
;   local maxima (in a 3-point neighborhood) are considered peaks. Maxima on the 
;   endpoints are not considered peaks, but if set, maxima on the second (second last) 
;   element may be considered a peak if the left (right) endpoint satisfies the 
;   above condition.
; t_method: specifies the thresholding method
;   0: no threshold
;   1: calculates the mean and standard deviation of the amplitudes of all peaks, 
;     then uses a value equal to a specified number of standard deviations (ie. nsigma) 
;     above the mean as a threshold
;   2: specified fixed threshold (ie. thres)
;
; Outputs:
; Returns the indices of the input vector that correspond to peaks. If count is set to 
; a named variable, also returns the number of peaks found. If no peaks are found, 
; returns a result of '' and count=0.

function sigma_peaks, y, nsigma, thres, count=num_pk, max_frac=max_frac, t_method=t_method

; error messages
if keyword_set(max_frac) and n_elements(y) lt 5 then begin
  print, 'Input vector must have at least 5 elements.'
  stop
endif else if not keyword_set(max_frac) and n_elements(y) lt 3 then begin
  print, 'Input vector must have at least 3 elements.'
  stop
endif

pk_arr = intarr(n_elements(y))
left1 = shift(y, 1)
  ; element to the left of each element
right1 = shift(y, -1)
  ; element to the right of each element

if keyword_set(max_frac) then begin
  left2 = shift(y, 2)
    ; element located two elements to the left
  right2 = shift(y, -2)
    ; element located two elements to the right
  for i=1, n_elements(y)-2 do begin
    ; check sharpness on the left (in a 5-point neighborhood)
    if left1[i] lt y[i]*max_frac then sharp_l=1 $
      else if left2[i] lt y[i]*max_frac and left1[i] lt y[i] and i gt 1 then sharp_l=1 $
      else sharp_l=0
    ; check sharpness on the right (in a 5-point neighborhood)
    if right1[i] lt y[i]*max_frac then sharp_r=1 $
      else if right2[i] lt y[i]*max_frac and right1[i] lt y[i] and i lt n_elements(y)-2 then sharp_r=1 $
      else sharp_r=0
    if sharp_l eq 1 and sharp_r eq 1 then pk_arr[i]=1
      ; flag element as a peak
  endfor
endif else begin
  ; else check that element is a local max in a 3-point neighborhood
  for i=1, n_elements(y)-2 do begin
    if left1[i] lt y[i] then pk_l=1 else pk_l=0
    if right1[i] lt y[i] then pk_r=1 else pk_r=0
    if pk_l eq 1 and pk_r eq 1 then pk_arr[i]=1
  endfor
endelse

peak = where(pk_arr eq 1, num_pk)
if num_pk lt 1 then begin
  num_pk = 0
  return, ''
endif

; apply thresholding
if keyword_set(t_method) then begin
  amp = y[peak]
  case t_method of
    1: begin
      pk_mean = robust_mean(amp, 4)
      pk_sigma = stddev(amp)
        ; calculate mean and standard deviation of peak amplitudes
      bigind = where(amp gt pk_mean + nsigma*pk_sigma, num_pk)
        ; threshold using this mean and standard deviation
    end
    2: bigind = where(amp gt thres, num_pk)
      ; fixed-amplitude threshold
  endcase
  if num_pk lt 1 then begin
    num_pk = 0
    return, ''
  endif
  bigpeak = peak[bigind]
endif else bigpeak = peak

return, bigpeak

end