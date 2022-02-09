FUNCTION PAM_READFILE_new,filename,LUN=lun,DEBUG=debug,COMMENTS=comments,TUPLE_TYPE=tuple_type,MAXVAL=maxval
  IF NOT KEYWORD_SET(DEBUG) THEN ON_ERROR,2  ;# return to caller on error
  ON_IOERROR,ioerror

  IF KEYWORD_SET(LUN) THEN BEGIN
    llun=lun & filename=STRING(lun,FORMAT='("LUN=",I)')
    fs= FSTAT(llun)
    IF (NOT fs.read) THEN BEGIN
      MESSAGE,'Error- cannot read from lun '+STRING(llun),/INFORMATIONAL
      RETURN,-1L
    ENDIF
  ENDIF ELSE BEGIN
    f= FINDFILE(filename[0],COUNT=nfiles)
    IF (nfiles EQ 0) THEN BEGIN
      MESSAGE,'Error- file not found: '+filename[0],/INFORM
      RETURN,-1L
    ENDIF
    compress= STREGEX(STRUPCASE(filename),'.*\.GZ$',/BOOLEAN)
    OPENR,llun,f[0],/GET_LUN,/SWAP_IF_LITTLE_ENDIAN,COMPRESS=compress[0]
  ENDELSE

  magicnumber= BYTARR(2)
  READU,llun,magicnumber
  CASE STRING(magicnumber) OF
  ;'P1':filetype=1  ;# PBM plain (bitmap ASCII)
  'P2':filetype=2  ;# PGM plain (grayscale ASCII)
  'P3':filetype=3  ;# PPM plain (3-color ASCII)
  ;'P4':filetype=4  ;# PBM raw (bitmap binary)
  'P5':filetype=5  ;# PGM raw (grayscale binary)
  'P6':filetype=6  ;# PPM raw (3-color binary)
  'P7':filetype=7  ;# PAM (N-channel binary)
  ELSE:  MESSAGE,'Error- invalid magic number: '+STRING(magicnumber)
   
  ENDCASE

  width=0 & height=0 & depth=0 & maxval=-1L & tupletype=''
  depth= ([1,1,3, 1,1,3, 0])[filetype-1]
  line='' & comments= ''
  value=LONARR(3) & nvalues=0

  REPEAT BEGIN
    READF,llun,line

    pos= STRPOS(line,'#')
    IF (pos GE 0) THEN BEGIN
      comment= STRMID(line,pos+1,999)
      line= STRMID(line,0,pos)
    ENDIF ELSE comment=''
    IF (STRLEN(comment) GT 0) THEN comments= [comments, comment]

    line= STRCOMPRESS(STRTRIM(line,2))
    IF (STRLEN(line) EQ 0) THEN CONTINUE

    token= STRSPLIT(line,' ',/EXTRACT)
    ntokens= N_ELEMENTS(token)
    IF (filetype EQ 7) THEN BEGIN
      CASE token[0] OF
       'WIDTH':width=token[1]
       'HEIGHT':height=token[1]
       'DEPTH':depth=token[1]
       'MAXVAL':maxval=token[1]
       'TUPLTYPE':tuple_type= tuple_type + ' ' + token[1]
       'ENDHDR':nvalues=3
       ELSE:MESSAGE,'Warning- unrecognized header token: '+line,/INFORMATIONAL
      ENDCASE
    ENDIF ELSE BEGIN
      value[nvalues]= token[0:(ntokens-1)<2]
      nvalues= nvalues + ntokens
    ENDELSE

  ENDREP UNTIL (nvalues GE 3) OR EOF(llun)

  IF EOF(llun) THEN BEGIN
     IF NOT KEYWORD_SET(LUN) THEN FREE_LUN,llun
     MESSAGE,'Error- end of file while reading header: '+filename,/INFORM
     RETURN,-1L
  ENDIF

  IF (N_ELEMENTS(comments) GT 1) THEN comments= comments[1:*]
  IF (filetype NE 7) THEN BEGIN
    width= value[0]
    height= value[1]
    maxval= value[2]
  ENDIF

  IF (width LE 0) THEN BEGIN & MESSAGE,'Error- invalid width:'+STRING(width) & RETURN,-1L & ENDIF
  IF (height LE 0) THEN BEGIN & MESSAGE,'Error- invalid height:'+STRING(height) & RETURN,-1L & ENDIF
  IF (depth LE 0) THEN BEGIN & MESSAGE,'Error- invalid depth:'+STRING(depth) & RETURN,-1L & ENDIF
  IF (maxval LE 0) THEN BEGIN & MESSAGE,'Error- invalid maxval:'+STRING(maxval) & RETURN,-1L & ENDIF
  IF (maxval GT 65535L) THEN BEGIN & MESSAGE,'Error- invalid maxval:'+STRING(maxval) & RETURN,-1L & ENDIF

  IF (maxval LE 255) THEN atom=0b ELSE atom=0U
  IF (depth GT 1) THEN dimensions=[depth,width,height] ELSE dimensions=[width,height]
  image= REPLICATE(atom,dimensions)
  IF (filetype LE 3) THEN READF,llun,image ELSE READU,llun,image

  IF NOT KEYWORD_SET(LUN) THEN BEGIN
    IF (EOF(llun) NE 1) THEN MESSAGE,'Note- not at end of file',/INFORMATIONAL
    FREE_LUN,llun
  ENDIF

  RETURN,image ;REFORM(image)  ;!!FIXME!!

  ioerror:FREE_LUN,llun
  PRINT,!ERROR_STATE.MSG
  RETURN,-1L
END

