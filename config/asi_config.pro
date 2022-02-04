;+
; :Function:
;     asi_config
; 
; :Description:
;     Read and write the asi configuration file
;     "asi_config.txt" which is stored in
;     /asi_tools/config/
;     
;     Returns a structure used for defining
;     the !asi_tools system variable
;     
;     Relies on 
;       asi_config_read()
;       asi_config_write()
;       
; :Calling Sequence:
;     dat_str = asi_config()      
;
; :Example:
;     Read the configuration file
;       asi_str = asi_config(/cread)
;     
;     Write the configuration file
;     will be prompted to eneter a new
;     data_dir
;       asi_str = asi_config(/cwrite)
;
; :Input:
;
; :Keywords:
;     cread - read configuration file
;     cwrite - write configuration file
;    
; :Defaults:
;     cread - read configuration file   
;
; :Return:
;     A structure that contains the variables in 
;     the configuratio file use for the 
;     !asi_tools system variable
;     
; 
; :Author: krmurphy - kylemurphy.spacephys@gmail.com
; 
; :Modification:
;-
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
