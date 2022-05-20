pro asi_peakocursor, $
  pk_str ; a structure or structure of structures containing the output from asi_peakogram
  
  

  ;load the asi data
  ;trex_imager_readfile, pk_str.paths, img, meta, count=img_c
  
  
  stop
  cursor, xx, yy, /data, /nowait
  old_x = xx
  old_y = yy
  done = 0
  while not done do begin
    cursor,xx,yy, /data,/nowait
    change_pos = abs(old_x-xx)
    old_x = xx
    if change_pos ne 0 then print, time_string(xx), change_pos
    done = !MOUSE.button eq 1
  endwhile
  
  
  
  
  
  
  
end