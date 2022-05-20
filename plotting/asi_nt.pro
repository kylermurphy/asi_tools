;+
; :Description:
;    Provide blank ticks to plot axis'
;
; :Params:
;    Required parameters for userdefined axis
;    formatting function
;    
;    axis - axis number, 0 for X axis, 1 for Y axis, 2 for Z axis
;    index - tick mark index (indices start at 0)
;    time - data value at the tick mark (a double-precision floating point value)
;
;
;
; :Author: krmurphy - kylemurphy.spacephys@gmail.com
;-
function asi_nt,axis,index,value
  return,' '
end
