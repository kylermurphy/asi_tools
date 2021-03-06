;+
; :Function:
;     asi_config_read   
;
; :Description:
;     Read the configuration file "config_file"
;    
;     Skip comment lines starting with ;
;     Read variables and values as
;     variable=value
;    
;     Store variables and values in structure
;     {variable:value}
;    
; :Calling Sequence:
;     dat_str = asi_config_read(config_file)
;
; :Input:
;
; :Params:
;     config_file - full path and name of configuration file
;
; :Return:
;     Structure containing the variables in the 
;     configuration file. Used to define the 
;     !asi_tools system variable
;     
;     {variable:value}
;
;
; :Author: krmurphy - kylemurphy.spacephys@gmail.com
; 
; :Modification:
;-
function asi_config_read, config_file

  fs = file_search(config_file,count=fc)
  if fc ne 1 then begin
    print,'Config File not found: '+fn
    cw = asi_config_write(fn) 
  endif

  openr,inlun,config_file,/get_lun
  
  in_dat = []
  line = ' '
  while not eof(inlun) do begin
    readf,inlun,line
    ; determine if we've read in a comment ';'
    ; and skip over empty lines
    if strmid(strcompress(line,/remove_all),0,1) eq ';' then continue
    if strlen(strcompress(line,/remove_all)) eq 0 then continue

    in_dat = [in_dat,strcompress(line,/remove_all)]
  endwhile
  
  close,inlun
  free_lun,inlun
  
  ;loop through the variables read in and add them to a structure
  for i=0L, in_dat.length-1 do begin
    r_dat = strsplit(in_dat[i],'=',/extract)
    
    if i eq 0 then r_str = create_struct(r_dat[0],r_dat[1]) $
      else r_str = create_struct(r_str,r_dat[0],r_dat[1])
  endfor
  

  

  return, r_str
end