
pro asi_colorbar,range=range,brange=brange, ct=ct, ct_file=ct_file, log=log,yticks=yticks,$
  position=pos,offset=offset,charsize=charsize,title=title,$
  _extra=ex

  ;@colors_com
  if keyword_set(brange) eq 0 then begin
    ;   brange=[bottom_c,top_c]
    brange=[0,!d.table_size-2]
  endif

  if not keyword_set(charsize) then charsize = !p.charsize
  if charsize eq 0. then charsize=1.0
  if not keyword_set(title)    then title=''


  if not keyword_set(pos) then begin
    if not keyword_set(offset) then offset = [1.,2]
    space = charsize * !d.x_ch_size/!d.x_size
    if !p.multi[1]*!p.multi[2] gt 4 then space=space/2
    xw = !x.window[1] + offset * space
    yw = !y.window
    pos = [xw[0],yw[0],xw[1],yw[1]]
  endif else begin
    xw = pos[[0,2]]
    yw = pos[[1,3]]
  endelse

  ;keep x/y start positions inside plot,
  ;otherwise data coords will not be established and AXIS will throw error later
  pos[0:1] = pos[0:1] < 1.

  ;color scale is generated using a plot
  ;thus plot parameters for this sub-plot could change the overall plot
  xt = !x    ; save previous plot parameters
  yt = !y
  clipt = !p.clip
  plot,[0,1],range,yrange=range,/nodata,/noerase, $
    pos=pos,xstyle=1+4,ystyle=1+4,ylog=log

  pixpos = round(convert_coord(!x.window,!y.window,/norm,/to_device))
  npx = pixpos[0,1]-pixpos[0,0] > 1  ;ensure at least one pixel
  npy = pixpos[1,1]-pixpos[1,0] > 1  ;to prevent errors
  xposition = pixpos[0,0]
  yposition = pixpos[1,0]

  if !d.name eq 'PS' then scale = 40./1000. else scale = 1.

  nypix = round(scale*npy)

  y = findgen(nypix)*(!y.crange[1]-!y.crange[0])/(nypix-1) + !y.crange[0]
  if keyword_set(log) then y = 10.^y
  y = bytescale(y,bottom=brange[0],top=brange[1],range=range,log=log)

  c_image = replicate(1b,npx) # y
  
  if keyword_set(ct) then loadct, ct,/silent, file=ct_file

  tv,c_image,xposition,yposition,xsize=npx,ysize=npy


  ;if start/end positions are equal then data coords
  ;will not be established and AXIS will throw error
  if pos[2]-pos[0] ne 0 and pos[3]-pos[1] ne 0 then begin

    ;load ct 0 then go back to the current color table
    ; or the color table passed
    tvlct, c_ct, /get
    loadct,0,/silent 
    

    ;set proper tick marks on axes.
    ;if set by user, use user param
    ;if small plot, don't use tick marks
    ;if normal plot, autogenerate
    if keyword_set(yticks) then begin
      axis, yaxis = 1, ystyle = 1, yrange = range, ylog = log, ytitle = title, charsize = charsize, yticks = yticks, ytickname=ytickname,_extra=ex
    endif else if(npy lt 50) then begin

      if not keyword_set(title) then begin
        if keyword_set(log) then begin
          title = 'Log'
        endif else begin
          title = 'Lin'
        endelse
      endif

      axis, yaxis = 1, ystyle = 1, yrange = range, ylog = log, ytitle = title, charsize = charsize, yticks = 1,_extra=ex

    endif else begin
      axis, yaxis = 1, ystyle = 1, yrange = range, ylog = log, ytitle = title, charsize = charsize,_extra=ex
    endelse

  endif

  tvlct, c_ct

  xbox = [xw[0],xw[1],xw[1],xw[0],xw[0]]
  ybox = [yw[0],yw[0],yw[1],yw[1],yw[0]]
  loadct, 0,/silent
  plots,xbox,ybox,/normal,/noclip

  !x = xt     ; restore previous plot parameters
  !y = yt
  !p.clip = clipt

end