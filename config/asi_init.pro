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