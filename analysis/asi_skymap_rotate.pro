;+
; :Function:
; 
;     asi_skymap_rotate
;
; :Description:
;     
;     Rotate a skymap by i_rot, which defines
;     the direction in rotate( ) 
;
; :Calling Sequence:
;     dat = asi_skymap_rotate(skymap_file, i_rot)
;
;
; :Params:
;
;    skymap - a skymap 
;    i_rot - an integer specifying the roation direction
;            for roate( )
;
; :Return:
; 
;     A rotated skymap
;
; :Author: krmurphy - kylemurphy.spacephys@gmail.com
;
; :Modification:
;
;-
function asi_skymap_rotate, skymap, i_rot

  asi_init
  
  ; get the image size
  im_sz = size(skymap.full_row)
  x_sz = im_sz[1]
  y_sz = im_sz[2]
  ; get the skymap tags
  tags = tag_names(skymap)
  
  ; get the dimensions of the roated image
  rot_test = rotate(skymap.full_row,i_rot)
  rot_s = size(rot_test)
  rot_x = rot_s[1]
  rot_y = rot_s[2]
  
  sky_temp= {}
  
  for i=0L, tags.length-1 do begin
    tag_s = size(skymap.(i))
    
    ; if the tag is a structure or string 
    ; leave as is
    if tag_s[-2] eq 8 or tag_s[-2] eq 7 then begin  
      tag_temp = skymap.(i)
    
    ; if the tag is a 2D array simply rotate it  
    endif else if tag_s[0] eq 2 then begin
      tag_temp = rotate(skymap.(i),i_rot)
    
    ; if the tag is a 3D array rotate the first
    ; 2 dimensions   
    endif else if tag_s[0] eq 3 then begin
      r0 = rotate(reform(skymap.(i)[*,*,0]),i_rot)
      r0_s = size(r0)
      
      tag_temp = make_array(r0_s[1],r0_s[2],tag_s[3],type=r0_s[-2])
      for j=1L, tag_s[3]-1 do begin
        tag_temp[*,*,j] = rotate(reform(skymap.(i)[*,*,j]),i_rot)
      endfor
    ; if the tag is an aray or a 1D array leave as it
    endif else if tag_s[0] eq 1 or tag_s[0] eq 0 then begin
      print, tags[i]
      tag_temp = skymap.(i)
    endif else message, 'Cannot handle tags of dimension > 3'
    
    sky_temp = create_struct(sky_temp,tags[i], tag_temp)
  endfor


  return, sky_temp

end



; MAIN
; test


restore, 'D:\data\asi_tools\REGO\skymaps\gill\rego_skymap_gill_geomag_20180801.sav',/verbose
pos = asi_skymap_pos(skymap)
help, pos
skymap = asi_skymap_rotate(skymap, pos.i_rot)
pos = asi_skymap_pos(skymap)
help, pos

end