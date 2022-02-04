;+
; :Function:
;     asi_config_write
; 
; :Description:
;     Write the configuration file config_file.
;     
;     Write predifined header
;     Write variable value pairs as
;     variable=value
;     
;     Prompt for the data_dir variable.
;     
;     Write defaults returned from asi_config_str()
;     
; :Calling Sequence:
;     asi_w = asi_config_write(config_file)     
;
; :Input:
;
; :Params:
;     config_file - full path and name of configuration file
;
; :Keywords:
;     current - write the !asi_tools variable
;
; :Author: krmurphy - kylemurph.spacephys@gmail.com
; 
; :Modification:
;-
function asi_config_write, config_file, current=current

  openw, outlun, config_file, /get_lun

  ;print header
  printf, outlun,'; Configuration file for asi_tools'
  printf, outlun,'; currently the only configuration'
  printf, outlun,'; variable set is the data directory'
  printf, outlun,';------------------------------------'
  printf, outlun,' '
  printf, outlun,'; comments are ignored ";"'
  printf, outlun,'; variables and values are set as'
  printf, outlun,'; variable:value'
  printf, outlun,';------------------------------------'
  printf, outlun,' '
  
  
  config_str  = asi_config_str()
  config_tags = tag_names(config_str)
  line = ' ' ; string for reading
  
  ;write the current !asi_tools variable to 
  ; the config file
  defsysv,'!asi_tools',exists=exists
  if keyword_set(current) and keyword_set(exists)  then begin
    current_tags = tag_names(!asi_tools)
    for i=0L, config_tags.length-1 do begin
      ; check if the config tag is part 
      ;of the current system variable
      ;write it to the file if it does
      ;otherwise ask user for input and write
      tag_pos = where(config_tags[i] eq current_tags, tc)
      if tc eq 1 then begin
        printf, outlun, config_tags[i]+'='+strcompress(!asi_tools.(tag_pos))
      endif else begin
        print, 'Tag not apart of current !asi_tool variable: '+config_tags[i]
        print, ' '
        read,line,prompt='Enter value for - '+config_tags[i]+':'
        printf, outlun, config_tags[i]+'='+strcompress(line)
      endelse
    endfor
  
  ; if writing new file start here  
  endif else begin
    for i=0L, config_tags.length-1 do begin
      ; only prompt for the data dir 
      ;write the other
      if strupcase(config_tags[i]) eq 'DATA_DIR' then begin 
        print, ' '
        print, ' '
        print, '!!!!!!!!!!!!!!!!!!!!!!!!!'
        read,line,prompt='Enter value for - '+config_tags[i]+':'
        printf, outlun,config_tags[i]+'='+string(line)
      endif else begin
        printf, outlun, config_tags[i]+'='+strcompress(config_str.(i))
      endelse
    endfor
  endelse
  
  close, outlun
  free_lun, outlun
  
  return,0
end
