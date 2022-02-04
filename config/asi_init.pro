;+
; :Procedure:
;     asi_init
; 
; :Description:
;     Intialize asi_tools system variable.
;     Can be called from IDL startup to set
;     custom locations
;     
;     Configuration settings are read from
;         /asi_tools/config/asi_config.txt
;     
;     If the file does not exist the user
;     will be prompted to set the "data_dir"
;     and a config file will be created using
;     the defualt values returned from 
;         asi_config_str
;     and written using
;         asi_config_write
;   
;     !asi_tools.data_dir - Root location for all files.
;         It is expected that all data files will reside 
;         in specific subdirectories with respect to this
;         directory. 
;         
;         example: D:\data\asi_tools
;     
;     !asi_tools.*_url - URLs for the various data sets that
;         can be loaded 
;         example: https://data.phys.ucalgary.ca/sort_by_project/THEMIS/asi/
;
; :Input:
;
; :Keywords:
;     reset - reset the !asi_tools values in the environment
;
; :Author: krmurphy - kylemurphy.spacephys@gmail.com
; 
; :Modification:
;-
pro asi_init, reset=reset

  
  defsysv,'!asi_tools',exists=exists
  if not keyword_set(exists) then begin
    defsysv,'!asi_tools', asi_config_str( ) ; get the base asi_tools system variable structure
  endif
  
  if keyword_set(reset) then !asi_tools.init =0
  if !asi_tools.init eq 1 then return

  config = asi_config(/cread)

  !asi_tools.init=1
  !asi_tools.data_dir=config.data_dir
  !asi_tools.themis_url=config.themis_url
  !asi_tools.rego_url=config.rego_url
  !asi_tools.rgb_url=config.rgb_url
  !asi_tools.blue_url=config.blue_url
  

end