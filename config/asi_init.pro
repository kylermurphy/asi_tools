pro asi_init

  
  defsysv,'!asi_tools',exists=exists
  if not keyword_set(exists) then begin
    defsysv,'!asi_tools', {init:0, data_dir:''} ; this could be a function that returns the necessary structure
  endif
  
  if keyword_set(reset) then !asi_tool.init =0
  if !asi_tools.init eq 1 then return
  
  
  config = asi_config(/cread)

  !asi_tools.data_dir=config.data_dir
  !asi_tools.init=1

end