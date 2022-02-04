;+
; :Function:
;     asi_config_str
; 
; :Description:
;     Return a structure the stores the 
;     defualt variables and values for
;     the !asi_tools system variable
;     
; :Calling Sequence:
;     asi_str = asi_config_str( )
;
; :Return:
;     Defualt variables and values for
;     !asi_tools system variable
;
; :Author: krmurphy - kylemurphy.spacephys@gmail.com
; 
; :Modification:
;-
function asi_config_str

  return, {init:0, $ ; determine if asi_tools has been intialized 
           data_dir:'', $ ; data directory for 
           themis_url:'https://data.phys.ucalgary.ca/sort_by_project/THEMIS/asi/', $
           rego_url:'https://data.phys.ucalgary.ca/sort_by_project/GO-Canada/REGO', $
           rgb_url:'https://data.phys.ucalgary.ca/sort_by_project/TREx/RGB', $
           blue_url:'Place Holder'}

end