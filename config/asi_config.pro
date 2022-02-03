function asi_config, cread=cread, cwrite=cwrite

  config_file = 'asi_config.txt'
  config_dir = routine_filepath( )
  config_dir = strsplit(config_dir,'asi_config',/extract,/regex)
  config_dir = config_dir[0] 

  
  ;search for config file
  ;if it doesn't exist create it
  fn = file_search(config_dir,config_file, count=fc)
  if fc ne 1 then begin
    c_str = asi_config_write(filepath(config_file,root_dir=config_dir))
  endif
  
  fn = file_search(config_dir,config_file, count=fc)
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


