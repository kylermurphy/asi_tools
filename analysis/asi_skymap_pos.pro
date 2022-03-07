function asi_skymap_pos, skymap

  asi_init

  ; determine the orientation of the
  ;passed skymap and if any rotation 
  ;is required so that north is up and
  ;west is left
  
  ; get the image size
  im_size = size(skymap.FULL_MAP_LATITUDE)
  x_sz = im_size[1]
  y_sz = im_size[2]
  
  ; find key positions in the skymaps
  ; position of min/max latitude along the center x position of the array
  lat_min = min(skymap.CENTER_GEO_LATITUDE[x_sz/2.,*,1],max=lat_max,subscript_max=lat_max_pos,/nan)
  lat_min_pos = !C
  
  ; positions of min/max longitude along the centery y position of the array
  lon_min = min(skymap.CENTER_GEO_LONGITUDE[*,y_sz/2,1],max=lon_max,subscript_max=lon_max_pos,/nan)
  lon_min_pos = !C
  
  ; if the position of the min lat
  ; is greater then the position of max lat
  ; then north is down and asi_is_north_up = 0
  if lat_min_pos gt lat_max_pos then asi_is_north_up=0 else asi_is_north_up=1
  
  ; if the position of the min lon
  ; is greater then the position of
  ; the max long then west is right
  ; and asi_west_is_left = 0
  if lon_min_pos gt lon_max_pos then asi_is_west_left=0 else asi_is_west_left=1
  
  ; determine the type of rotation needed
  ; this is the rotation code in the idl
  ; function rotate() - see documentation
  ; for details
  if lat_min_pos lt lat_max_pos and lon_min_pos lt lon_max_pos then i_rot=0 $
  else if lat_min_pos gt lat_max_pos and lon_min_pos gt lon_max_pos then i_rot=2 $
  else if lat_min_pos gt lat_max_pos and lon_min_pos lt lon_max_pos then i_rot=7 $
  else i_rot=-1
  
  return, {i_rot:i_rot, asi_is_north_up:asi_is_north_up, asi_is_west_left:asi_is_west_left}
  
end  