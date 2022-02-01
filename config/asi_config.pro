function asi_config, cread=cread, cwrite=cwrite, config_file=config_file, config_dir=config_dir, _EXTRA=ex

  if keyword_set(config_file) then config_file = config_file else config_file = 'asi_config.txt'
  if keyword_set(config_dir) then config_dir = config_dir else begin
     config_dir = routine_filepath( )
     config_dir = strsplit(config_dir,'asi_config',/extract,/regex)
     config_dir = config_dir[0] 
  endelse
  
  ;config file name
  fn = concat_dir(config_dir,config_file)

  ; read config file
  if keyword_set(cread) then begin
    c_str = asi_config_read(fn)
  ; write config file  
  endif else if keyword_set (cwrite) then begin
    c_str = asi_config_write(fn)
  ; read config file  
  endif else begin   
    c_str = asi_config_read(fn)
  endelse


  
  return, c_str





end


;Main
;test


a = asi_config(/cread)

end


