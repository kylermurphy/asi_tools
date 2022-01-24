;+
; :Description:
;    Describe the procedure.
;
; :Params:
;    time_start   -  timespan in standard format, e.g. '2010-02-22/03:00:00'
;    time_hours   -  number of hours to add to time_start. Defines how much data to load
;
; :Keywords:
;    site            -  4 letter site name, or array, e.g., ['fsmi', 'gill'], or 'gill'
;
; :Optional:
;    longitudes      -  The longitueds to take slices from. Defaults to ~center of FOV
;    npeaks          -  Number of peaks to find. Defaults to 2
;    xavg            -  x (longitude) averaging. Default is 5
;    yavg            -  y (latitude) averaging. Default is 5
;    symsize_scale   -  percent to scale the symbols. Default is 100
;    xscale_plot     -  xrange for the plot in decimal hours, e.g. [5.2, 5.6]. Defaults to time_start + time_hours
;    yscale_plot     -  yrange for the plot. defaults to full latitude range of image
;    noload          -  don't load - assumes tplot variables already exist
;    maxfirst        -  plots the kgram symbols in order, either max or min first, to help with visualizatoin (beta)
;    decimate        -  decimate by factor of N, e.g. 5
;    min_bytscl      -  for scaling the images, must be array if site is array, e.g. min_bytscl=[3000, 4000].
;    max_bytscl      -  for scaling the images, must be array if site is array, e.g. max_bytscl=[6000, 7000]
;    eps             -  to save the kgram as an eps file
;    makemovie       -  makes a movie...
;    moon
;    supermoon
;    no_kepkogram    -  Don't create the kepkogram. Useful for quickly looking at images only
;    noplot          -  This just loads the data and returns
;    rainbow         -  For rainbow color scaling of the images
;    no_download     -  don't check server for the data (force local)
;    onset_location
;    file_prefix     -  where to save the files. Defaults to '~/'
;    denoise
;    single_plot     -  For multiple stations, puts all slices on single plot
;    rego            -  When set w/o /nowl, reads in rego + white light
;    nowl            -  Only applicable when /rego set. This flag does not read in the WL for station
;    the_asis        -  returns the_asis structure
;    save_pointers   -  should set if returning the_asis
;    square
;    diagnostic      -  plot some extra diagnostics
;    raw             -  sets minimum elevation to 0
;    histequal       -  To scale the images with built-in IDL hist_equal routine
;    nogrid          -  Don't plot the grid on the images
;    nofill          -  if set, arcgrams are open circles instead of filled circles
;    arc_rainbow     -  If set, arcgram slices are all the same color table (rainbow)
;    thm_foot        -  Flag to overplot themis footprint (currently requires auxiliary files)
;    mms_foot
;
; :Author: ekepko
;-



pro kg_plot_longitude_slices, the_asis, im_scl=im_scl, diagnostic = diagnostic, xavg = xavg, latlon_charsize=latlon_charsize,$
   char_plot = char_plot, cursor_i = cursor_i, symsize_scale = symsize_scale

   for jj = 0, the_asis.tmpnumslices - 1 do begin
      loadct, the_asis.the_slices[jj].slice_ct, /silent
      lon2plot = *the_asis.the_slices[jj].lon_points
      ;  Plot the sliced longitude line
      plots, lon2plot.x[0:lon2plot.npts] * im_scl, lon2plot.y[0:lon2plot.npts] * im_scl, color = 250, thick = 2.5, /device
      if keyword_set(diagnostic) then begin ; plot width of averaging
         plots, (lon2plot.x[0:lon2plot.npts] + xavg) * im_scl, lon2plot.y[0:lon2plot.npts] * im_scl, color = 250, thick = 1.5, /device
         plots, (lon2plot.x[0:lon2plot.npts] - xavg) * im_scl, lon2plot.y[0:lon2plot.npts] * im_scl, color = 250, thick = 1.5, /device
      endif
      ;  Plot the longitude number
      halfway = lon2plot.npts / 2
      xyouts, lon2plot.x[halfway] * im_scl, lon2plot.y[halfway] * im_scl, strtrim(fix(lon2plot.lon), 2) + '!Uo!N', /data, color = 250, $
         charsize = latlon_charsize, charthick = char_plot, /device
      ;  Plot the symbols
      psym = symcat(the_asis.the_slices[jj].slice_psym)
      for xyz123 = 0, n_elements(*(*the_asis.the_slices[jj].xpoints)[cursor_i]) - 1 do begin
         if ((*(*the_asis.the_slices[jj].xpoints)[cursor_i])[xyz123] gt 0) then begin
            plots, (*(*the_asis.the_slices[jj].xpoints)[cursor_i])[xyz123] * im_scl, $
               (*(*the_asis.the_slices[jj].ypoints)[cursor_i])[xyz123] * im_scl, $
               psym = psym, $
               symsize = (*(*the_asis.the_slices[jj].amps)[cursor_i])[xyz123] / symsize_scale, $
               color = (*(*the_asis.the_slices[jj].amps)[cursor_i])[xyz123] / 0.9, /device
         endif
      endfor   ; peaks
   endfor      ; Longitude slices
end


pro kg_plot_latlon_lines, theasi, latcolor=latcolor, eps_scale = eps_scale

   ;  Plot latitude lines

   latpoints = *theasi.lat_points
   for jj = 0, n_elements(latpoints) - 1 do begin
      plots, latpoints[jj].x[0:latpoints[jj].npts] * eps_scale, latpoints[jj].y[0:latpoints[jj].npts] * eps_scale, $
         color = latcolor, thick = 1.5, /device
   endfor

   ;  Plot longitude lines

   lonpoints = *theasi.lon_points
   for jj = 0, n_elements(lonpoints) - 1 do begin
      plots, lonpoints[jj].x[0:lonpoints[jj].npts] * eps_scale, lonpoints[jj].y[0:lonpoints[jj].npts] * eps_scale, $
         color = latcolor, thick = 1.5, /device
   endfor
end

pro kg_plot_latlon_labels, theasi, char_plot=char_plot, latlon_charsize=latlon_charsize, latcolor=latcolor,eps_scale=eps_scale

   latpoints = *theasi.lat_points
   for jj = 0, n_elements(latpoints) - 1 do begin
      halfway = latpoints[jj].npts / 2
      xyouts, latpoints[jj].x[halfway] * eps_scale, latpoints[jj].y[halfway] * eps_scale, $
         strtrim(long(latpoints[jj].lat), 2) + '!Uo!N', /data, color = latcolor, $
         charsize = latlon_charsize, charthick = char_plot, /device
   endfor

   ;  Plot longitude lines and print the numbers

   lonpoints = *theasi.lon_points
   for jj = 0, n_elements(lonpoints) - 1 do begin
      halfway = lonpoints[jj].npts / 2
      xyouts, lonpoints[jj].x[halfway] * eps_scale, lonpoints[jj].y[halfway] * eps_scale, $
         strtrim(long( lonpoints[jj].lon ), 2)+'!Uo!N', /data, color = latcolor, $
         charsize = latlon_charsize, charthick = char_plot, /device
   endfor
end


;
;
;  calculate pixel locations for the specified longitudes (for taking the kepkogram slices)
;
;
;  Added lat_range = this restricts the slice to min/max of lat_range (for speed, and localizing)

pro kg_calculate_longitude_slices, lat_hist, lon_hist, image_size, good_ele, elev_asi, plot_lon = plot_lon, lon_points = lon_points, $
   min_elevation = min_elevation, lat_range = lat_range

   print, 'Calculating longitude slices...'


   ;  Array to hold the slices. x,y are pixel locations within the image. Latitudes are the magnetic latitudes corresponding to each
   ;  (x,y) data pair, while lon is the magnetic longitude passed in and remains constant.

   lon_points = replicate({x:intarr(500), y:intarr(500), latitudes:dblarr(500), lon:0.0, npts:0.0}, n_elements(plot_lon))

   ; Step through each longitude slice passed in
   for j = 0L, n_elements(plot_lon) - 1 do begin
      print, plot_lon[j], j + 1, n_elements(plot_lon), format = '("Longitude ", F5.1, " (", I1, " of ", I1, ")")'

      start_lon  = plot_lon[j]
      plot_point = 0

      for i = 0L, n_elements(lon_hist[0, *]) - 1 do begin
         lon_arr = reform(lon_hist[*, i])
         min_diff = min(abs(lon_arr - start_lon), /nan)        ;  Subtract the desired longitude from the array and look for smallest difference
         min_pos = !C         ; get that index

         if min_diff gt 0.1 then continue                ;  too big of a difference. Rewrite with standard if thens
         if elev_asi[min_pos,i] lt min_elevation or finite(elev_asi[min_pos,i]) eq 0 then continue
         ;         if lat_hist[min_pos, i] lt lat_range[0] then continue
         ;         if lat_hist[min_pos, i] gt lat_range[1] then continue

         lon_points[j].x[plot_point] = min_pos
         lon_points[j].y[plot_point] = i
         lon_points[j].latitudes[plot_point] = lat_hist[min_pos, i]
         plot_point++
      endfor
      if plot_point ne 0 then begin                ; Store the array if non zero # of points
         lon_points[j].npts = plot_point - 1
         lon_points[j].lon = plot_lon[j]
      endif else lon_points[j].npts = 0
   endfor
   print, '....Done'
end

pro kg_convert_footpoint2asigrid, lat_hist, lon_hist, lats2find=lats2find, lons2find=lons2find, thd_lon = thd_lon, thd_lat = thd_lat

   ;  Takes ina  set of lat/lon (like satellite footpoints) and converts them to pixel locations

   thd_lon = fltarr(n_elements(lats2find))
   thd_lat = thd_lon

   for jj = 0, n_elements(lats2find) - 1 do begin
      lon_diff = abs(lon_hist - lons2find[jj])        ;  Subtract the desired longitude from the array and look for smallest difference
      lat_diff = abs(lat_hist - lats2find[jj])
      total_diff = lon_diff + lat_diff
      best_min = min(total_diff, best_index, /nan )
      best_loc = array_indices(total_diff, best_index)
      thd_lon[jj] = best_loc[0]
      thd_lat[jj] = best_loc[1]
   endfor
end

;
;
;  Create latitude and longitude grids
;
;

;pro create_mlat_grid, mlats, mlons, image_size, good_ele, elev_asi, lat_points = lat_points, lon_points = lon_points, latrange=latrange
pro kg_create_mlat_grid, mlats, mlons, good_ele, elev_asi, lat_points = lat_points, lon_points = lon_points, $
   latrange=latrange, image_size=image_size, lat_hist = lat_hist, lon_hist = lon_hist, min_elevation = min_elevation

   print, 'Creating latitude grid...'

   lat_step = 1
   lon_step = 5
   ps = 0

   ;   min_elevation = 6

   imsz = image_size - 1
   ;   print, image_size, imsz

   lat_hist = (mlats[0:imsz,0:imsz] + mlats[1:imsz+1,0:imsz] + mlats[1:imsz+1,1:imsz+1]+mlats[0:imsz,1:imsz+1])/4.0
   lon_hist = (mlons[0:imsz,0:imsz] + mlons[1:imsz+1,0:imsz] + mlons[1:imsz+1,1:imsz+1]+mlons[0:imsz,1:imsz+1])/4.0

   start_lat = long(min(lat_hist[good_ele],/nan, max=max_lat)/lat_step)*lat_step
   end_lat = max_lat
   latrange = [start_lat, round(end_lat)]
   npts = round((end_lat-start_lat)/lat_step)
   plot_lat = indgen(npts)*lat_step+start_lat
   mid_pt = round(lat_hist[(image_size/2) - 1, (image_size/2) - 1])     ; 127 for 256 sized
   min_diff = min(abs(plot_lat-mid_pt),/nan)
   if min_diff gt 0 then plot_lat = [plot_lat,mid_pt]
   lat_sort = sort(plot_lat)
   plot_lat = plot_lat[lat_sort]

   ;
   ;  Create array to hold the data

   lat_points = replicate({pp_str2, x:intarr(500), y:intarr(500), lat:0.0, npts:0}, n_elements(plot_lat))

   for j = 0L, n_elements(plot_lat) - 1 do begin
      start_lat  = plot_lat[j]
      plot_point = 0
      leg_plot   = 0
      for i = 0L, n_elements(lat_hist[*, 0]) - 1 do begin
         lat_arr = reform(lat_hist[i, *])
         min_diff = min(abs(lat_arr - start_lat), /nan)
         min_pos = !C

         if min_diff gt 0.1 then continue
         if elev_asi[i,min_pos] lt min_elevation or finite(elev_asi[i,min_pos]) eq 0 then continue

         lat_points[j].x[plot_point] = i
         lat_points[j].y[plot_point] = min_pos
         plot_point++
      endfor
      if plot_point ne 0 then begin
         lat_points[j].npts = plot_point - 1
         lat_points[j].lat = plot_lat[j]
      endif else lat_points[j].npts = 0
   endfor


   ;  Create longitude grid

   start_lon = long(min(lon_hist[good_ele], /nan, max = max_lon) / lon_step) * lon_step
   end_lon = max_lon
   print, 'max min longitude ', start_lon, end_lon
   npts = round((end_lon - start_lon) / lon_step)
   plot_lon = indgen(npts)*lon_step+start_lon
   mid_pt = round(lon_hist[(image_size/2) - 1, (image_size/2) - 1])        ; should be 127 for 256
   min_diff = min(abs(plot_lon-mid_pt),/nan)
   if min_diff gt 0 then plot_lon = [plot_lon,mid_pt]
   lon_sort = sort(plot_lon)
   plot_lon = plot_lon[lon_sort]


   lon_points = replicate({pp_str3, x:intarr(500), y:intarr(500), lon:0.0, npts:0.0}, n_elements(plot_lon))

   for j = 0L, n_elements(plot_lon) - 1 do begin
      start_lon  = plot_lon[j]
      ;     print, start_lon
      plot_point = 0
      leg_plot   = 0
      for i=0L, n_elements(lon_hist[0,*])-1 do begin
         lon_arr = reform(lon_hist[*,i])
         min_diff = min(abs(lon_arr-start_lon),/nan)
         min_pos = !C

         if min_diff gt 0.1 then continue
         if elev_asi[min_pos,i] lt min_elevation or finite(elev_asi[min_pos,i]) eq 0 then continue
         lon_points[j].x[plot_point] = min_pos
         lon_points[j].y[plot_point] = i
         plot_point++
      endfor
      if plot_point ne 0 then begin
         lon_points[j].npts = plot_point - 1
         lon_points[j].lon = plot_lon[j]
      endif else lon_points[j].npts = 0
   endfor
   print, '.... Done'
end

pro kg_single_peakfinder_mlat, ff, npeaks = npeaks, peakloc = peakloc, peakamp = peakamp

   ;  NORSTAR_PEAKFINDER
   ;
   ;  Looks though the slice (ff) and identifies the latitude and intensity of the # of peaks asked for
   ;
   ;  REQUIRED INPUT
   ;     ff : The 1-d slice of imaged data
   ;
   ;  OPTIONAL INPUT
   ;     npeaks : number of peaks to look for. Defaults to 1
   ;     peakloc : return variable for peak locations, given as indices
   ;     peakamp : return variable for peak amplitudes, given as indices
   ;
   ;

   ;ff = reform(ff)
   ;if not keyword_set(npeaks) then npeaks = 1     Set in routine above

   peakloc = intarr(npeaks)
   peakamp = fltarr(npeaks)

   ;  Loop through the data for the number of peaks asked for

   for ii = 0, npeaks - 1 do begin
      peakamp[ii] = max(ff, jj, /nan)
      peakloc[ii] = jj
      ;     print, ' -- found a peak at', peakloc[ii], 'with ampitude', peakamp[ii]
      ;  Now that we have a local maxima, look for local minima in both directions, then remove those points
      ;  Now move left (lower indices) to find a local minima

      loc_max = peakamp[ii]
      if finite(loc_max) then begin
         loc_ii_left = peakloc[ii] - 1
         ;         if loc_ii_left gt 1 then begin
         while ((loc_ii_left gt 1) and (ff[loc_ii_left] le loc_max)) do begin
            loc_max = ff[loc_ii_left]
            loc_ii_left = loc_ii_left - 1
            ;        if loc_ii_left lt 1 then break
         endwhile
         ;        endif

         ;  print, 'found minima at', loc_ii_left, 'with amplitude ', loc_max
         ;  Now move right (higher indices) to find a local minima

         loc_max = peakamp[ii]
         loc_ii_right = peakloc[ii] + 1
         if loc_ii_right lt n_elements(ff)-1 then begin
            while ff[loc_ii_right] le loc_max do begin
               loc_max = ff[loc_ii_right]
               loc_ii_right = loc_ii_right + 1
               if loc_ii_right gt n_elements(ff)-2 then break
            endwhile
         endif

         ;  print, 'found minima at', loc_ii_right, 'with amplitude ', loc_max

         if loc_ii_left lt 0 then loc_ii_left = 0
         if (loc_ii_right gt (n_elements(ff)-1)) then loc_ii_right = n_elements(ff)-1
         ff[loc_ii_left:loc_ii_right] = !VALUES.F_NAN    ; mask the peak from valley to valley
      endif
   endfor

   ;  If peak is at either top or bottom edge, get rid of it.

   ww = where(peakloc eq 0, count)
   if count ne 0 then peakloc[ww] = -999.0
   ww = where(peakloc eq (n_elements(ff)-1), count)
   if count ne 0 then peakloc[ww] = -999.0

   ww = where(peakloc le 0, count)
   if count ne 0 then peakloc[ww] = -999.0
end


pro kg_epsopen, fname0
   set_plot, 'ps', /copy
   fname = '~/' + fname0


   ;if (keyword_set(landscape)) then begin
   ;  device, filename=fname, /color, bits_per_pixel=8, /inc, /landscape
   ;endif else begin
   ;  device, filename=fname, /color, bits_per_pixel=8, xsi=8.5, ysi=11, xoff=.5, yoff=.5, /inc
   device, filename=fname, /color, bits_per_pixel=8, /inches, $
      xoff=0.0, yoff=0.0, xsi=8.5, ysi=10.2, /portrait, /encapsulated, $
      /HELVETICA, /NARROW
   ;endelse
end

pro kg_epsclose
   device, /close
   set_plot, 'x'
end


;
;  Main program
;




pro kepkogram, $
   time_start, $		         ; timespan in standard format, '2010-02-22/03:00:00'
   time_hours, $		         ; number of hours to add to time_start. Defines how much data to load
   site = site, $					; sitename
   longitudes = longitudes, $       ; The longitueds to take slices from
   npeaks = npeaks, $			; Number of peaks to find. Defaults to 2
   xavg = xavg, $					; x (longitude) averaging. Default is 5
   yavg = yavg, $					; y (latitude) averaging. Default is 5
   symsize_scale = symsize_scale, $
   xscale_plot = xscale_plot, $	; xrange for the plot. defaults to time_start + time_hours
   yscale_plot = yscale_plot, $	; yrange for the plot. defaults to full latitude range of image
   noload = noload, $            ; don't load - assumes tplot variables already exist
   maxfirst = maxfirst, $
   decimate = decimate, $        ; decimate by factor of N
   min_bytscl = min_bytscl, $    ; for scaling the images
   max_bytscl = max_bytscl, $    ; for scaling the images
   eps = eps, $                  ; to save the kepkogram as an eps file
   makemovie = makemovie, $      ; maeks a movie...
   ;		rot_angle = rot_angle, $
   moon = moon, $
   supermoon = supermoon, $
   no_kepkogram = no_kepkogram, $         ; Don't create the kepkogram. Useful for quickly looking at images only
   noplot = noplot, $                     ; This just loads the data and returns
   rainbow = rainbow, $                   ; For rainbow color scaling of the images
   no_download = no_download, $           ; don't download the data (use local)
   onset_location = onset_location, $
   file_prefix = file_prefix, $           ; where to save the files. Defaults to '~/'
   denoise = denoise, $
   single_plot = single_plot, $           ; For multiple stations, puts all slices on single plot
   rego = rego, $                         ; When set w/o /nowl, reads in rego + white light
   nowl = nowl, $                         ; Only applicable when /rego set. This flag does not read in the WL for station
   the_asis = the_asis, $                 ; returns the_asis structure
   save_pointers = save_pointers, $       ; should set if returning the_asis
   square = square, $
   diagnostic = diagnostic, $             ; plot some extra diagnostics
   raw = raw, $                           ; sets minimum elevation to 0
   histequal = histequal, $               ; To scale the images with hist_equal
   nogrid = nogrid, $
   nofill = nofill, $                     ; if set, arcgrams are open circles instead of filled circles
   arc_rainbow = arc_rainbow, $           ; If set, arcgram slices are all the same color table (rainbow)
   thm_foot = thm_foot, $
   mms_foot = mms_foot


   autoscale_min_stdev = 1.5     ;  These are for autoscale. median +/- these numbers sets bytscale range
   autoscale_max_stdev = 3.5

   QUICK_GRID = 1            ; testing this for speed. sets image pixels to white for lat/lon instead of plotting

   if keyword_set(nofill) then arcgram_psym = 9 else arcgram_psym = 16        ; filled vs open circles
   if keyword_set(arc_rainbow) then slice_color_cts = [13, 13, 13, 13, 13, 13, 13] else slice_color_cts = [56, 57, 63, 53, 62, 13]     ; make all slices same rainbow

   if not keyword_set(site) then begin
      print, 'You need to pass in a site'
      return
   endif
   site = strlowcase(site)							   	;	Make lowercase
   number_of_sites = n_elements(site)					;	Number of sites passed in by user
   print, 'number of sites passed in', number_of_sites

   n_longitudes = n_elements(longitudes)
   print, 'number of longitudes passed in', n_longitudes

   if not keyword_set(yscale_plot) then autoscale = 1 else autoscale = 0
   if not keyword_set(yscale_plot) then yscale_plot = []
   if keyword_set(min_bytscl) then usr_min_bytscl = 1 else usr_min_bytscl = 0
   if keyword_set(max_bytscl) then usr_max_bytscl = 1 else usr_max_bytscl = 0
   if keyword_set(square) then pos = [0.1, 0.3, 0.6, .8] else pos = [0.1, 0.3, 0.95, 0.55]
   if keyword_set(rainbow) then realtime_ct = 33 else realtime_ct = 0

   !y.margin = [1.5, 1.5]

   nam = strarr(n_elements(site))						;	Names of the tplot variables

   ;	Load the data, unless they don't want to

   if keyword_set(rego) then tname_prefix = 'clg_rgf_' else tname_prefix = 'thg_asf_'
   if not keyword_set(npeaks) then npeaks = intarr(number_of_sites) + 2

   if keyword_set(rego) then begin
      ;			for ii = 0, n_elements(site) - 1 do begin
      ;				load_multispectral_imager, site = site[ii], wavelength='6300', /noplot, sc_max = -1
      ;				nam[ii] = tnames(tname_prefix + site[ii])
      ;			endfor
      ;			load_multispectral_imager, site = 'gill', wavelength = '6300', sc_max=18000, rot_angle=165
      ;			load_multispectral_imager, site = site, wavelength = '6300', sc_max=28000, rot_angle=0, /noplot

      ;  Do a hack check to see if they've asked for rego and provided a single array npeaks - need to expand
      ;  npeaks array


      if not keyword_set(nowl) then npeaks = [npeaks, npeaks]        ; need to add an extra npeaks for the second station we added

      if not keyword_set(noload) then begin
         del_data, '*'
         timespan, time_start, time_hours, /hours
         thm_load_rego, site = site, no_download = no_download
         if not keyword_set(nowl) then thm_load_asi, datatype = 'asf', site = site, no_download = no_download
      endif
      if not keyword_set(nowl) then begin
         nam = tnames(tname_prefix + site)
         nam = [tnames(tname_prefix + site), tnames('thg_asf_' + site)]           ;  HACK TO ADD WL IMAGES
         site = [site, site]           ;  HACK TO ADD WL IMAGES
         number_of_sites = 2
         rego_site_flag = [1, 0]
      endif else begin
         nam = tnames(tname_prefix + site)
         rego_site_flag = 1
         number_of_sites = 1
      endelse
   endif else begin
      rego_site_flag = intarr(number_of_sites)
      if not keyword_set(noload) then begin
         del_data, '*'
         timespan, time_start, time_hours, /hours
         for ii = 0, n_elements(site) - 1 do begin
            thm_load_asi, datatype = 'asf', site = site[ii], no_download = no_download
            nam[ii] = tnames(tname_prefix + site[ii])
         endfor
      endif else begin
         ;  Check for no data
         get_data, tname_prefix + site[0], data = d
         d_sss = size(d)
         if d_sss[0] eq 0 then begin
            print, 'No data!'
            return
         endif
         nam = []
         for ii = 0, n_elements(site) - 1 do begin
            nam = [nam, tname_prefix + site[ii]]
         endfor
      endelse
   endelse

   if keyword_set(noplot) then return					;	Optional return after loading

   ;
   ;  If the user did not pass in any longitudes, default to something near the middle of the FOV
   ;

   if n_longitudes eq 0 then begin
      longitudes = fltarr(1, number_of_sites)
      n_longitudes = 1
      for jj = 0, number_of_sites - 1 do begin
         if site[jj] eq 'gill' then longitudes[0, jj] = -29.0
         if site[jj] eq 'fykn' then longitudes[0, jj] = -96
         if site[jj] eq 'inuv' then longitudes[0, jj] = -87
         if site[jj] eq 'snkq' then longitudes[0, jj] = -4
         if site[jj] eq 'kuuj' then longitudes[0, jj] = 13
         if site[jj] eq 'fsmi' then longitudes[0, jj] = -56
         if site[jj] eq 'snap' then longitudes[0, jj] = -56
         if site[jj] eq 'kian' then longitudes[0, jj] = -109
         if site[jj] eq 'fsim' then longitudes[0, jj] = -68
         if site[jj] eq 'rank' then longitudes[0, jj] = -29
         if site[jj] eq 'pina' then longitudes[0, jj] = -29
         if site[jj] eq 'yknf' then longitudes[0, jj] = -60
         if site[jj] eq 'atha' then longitudes[0, jj] = -60
         if site[jj] eq 'nrsq' then longitudes[0, jj] = 43
         if site[jj] eq 'gako' then longitudes[0, jj] = -93
         if site[jj] eq 'whit' then longitudes[0, jj] = -87
         if site[jj] eq 'tpas' then longitudes[0, jj] = -38
         if site[jj] eq 'kapu' then longitudes[0, jj] = -5
         if site[jj] eq 'mcgr' then longitudes[0, jj] = -100
         if site[jj] eq 'luck' then longitudes[0, jj] = -46
      endfor
   endif


   ;
   ;  Set some defaults
   ;

   if not keyword_set(xavg) then xavg = 5
   if not keyword_set(yavg) then yavg = 5
   if keyword_set(rego) then begin
      xavg = xavg * 2
      yavg = yavg * 2
   endif
   if not keyword_set(symsize_scale) then symsize_scale = 100.0
   if keyword_set(rego) then image_size_multiplier = [1.0, 2.0] else image_size_multiplier = 2.0  + intarr(number_of_sites)
   MOON_FLAG_VALUE = 3.0e4


   ;
   ;	the_asis holds all the data associated with each station
   ;
   ;	the_asis[number of stations]->
   ;		site = sitename
   ;		type = 3 letter string, asf = white light, clg = rego redline
   ;		*raw_images[number of images, *, *] = the original, unscaled images
   ;		*scaled_images[number of images, *, *] = the scaled images
   ;		*bad_elev = location of bad elevations in the images
   ;		*image_ut[number of images] = the universal time in tplot units (double)
   ;		*allpeaks_loc
   ;		*allpeaks_ut
   ;		*allpeaks_amp
   ;		*lat_points
   ;		*lon_points
   ;		tmpnumslices = number of longitudes that were sliced
   ;		n_images = number of images (i.e., every timestep)
   ;		foundpeaks
   ;		the_slices[number of longitudes]->
   ;			longitude = the longitude of the slice
   ;			*latitude =
   ;			*amplitude =
   ;			*ut =
   ;			*lon_points =
   ;			*xypoints_array[number of images]
   ;
   ;	Some useful references:
   ;


   slice_array = {longitude:0.0, slice_ct:0, slice_psym:0, latitude:ptr_new(), amplitude:ptr_new(), $
      ut:ptr_new(), lon_points:ptr_new(), xpoints:ptr_new(), ypoints:ptr_new(), amps:ptr_new(), ff:ptr_new()}

   foundpeak_array = {ut:0.0, imagenumber:1L, latitudes:ptr_new(), xpixel_location:ptr_new(), $
      ypixel_location:ptr_new(), amplitude:ptr_new()}

   the_asis = replicate({site:'', type:'', image_size:0, image_size_multiplier:0, image_scale:0, $
      histequal_images:ptr_new(), scaled_images:ptr_new(), bad_ele:ptr_new(), image_ut:ptr_new(), allpeaks_loc:ptr_new(), $
      allpeaks_ut:ptr_new(), allpeaks_amp:ptr_new(), lat_points:ptr_new(), lon_points:ptr_new(), $
      the_slices:replicate(slice_array, n_longitudes), tmpnumslices:0, n_images:0, $
      foundpeaks:ptr_new(), lat_hist:ptr_new(), lon_hist:ptr_new(), thm_lat:ptr_new(), thm_lon:ptr_new(), $
      mms_lat:ptr_new(), mms_lon:ptr_new()}, number_of_sites)

   single_plot_min_lat = 9999.0
   single_plot_max_lat = -9999.0

   ;if not keyword_set(xcols) then xcols = image_size / 2					;	If they don't define any N/S slices pick the middle
   ;if not keyword_set(timegap) then timegap = 10
   ;if not keyword_set(xrange) then xrange = [0, image_size - 1]
   ;	if not keyword_set(rot_angle) then rot_angle = -1
   ;if not keyword_set(amp_scale) then amp_scale = 1.0

   loadct, 0, /silent

   image_size = []

   ;---------------------------------------------------------------------
   ;
   ;  This is the main loop to go through each site and calculate slices
   ;
   ;	Loop through each station, scale the images, and create the line plots
   ;
   ;----------------------------------------------------------------------

   for jj = 0, number_of_sites - 1 do begin
      n_longitudes = n_elements(longitudes[*, jj])
      the_asis[jj].tmpnumslices = n_longitudes
      print, n_longitudes
      the_asis[jj].image_size_multiplier = image_size_multiplier[jj]

      the_asis[jj].type = strmid(nam[jj], 4, 3)
      the_asis[jj].site = site[jj]           ;  Store the sitename

      get_data, nam[jj], data = d
      exist_test = size(d)
      if exist_test[0] eq 0 then begin
         print, 'No data for ', nam[jj]
         if keyword_set(noload) then print, 'rerun and remove /noload'
         return
      endif

      ;
      ;  Decimate the data if they asked for it

      if keyword_set(decimate) then begin             ; NOTE: THIS NEEDS TO BE MODIFIED TO CHANGE IMAGE SIZE, etc
         print, 'decimating'
         sz = size(d.y)
         new_i = decimate * indgen((sz(1) - decimate) / decimate)
         d = {x:d.x(new_i), y:d.y(new_i, *, *)}
      endif

      sc_time = d.x
      sz = size(d.y)
      tot_images = sz[1]
      the_asis[jj].image_size = sz[2]
      image_size = [image_size, sz[2]]							;	This is the x dim of the image, should be same as y dimension

      ;	Calculate a useful time

      t0_base_str = time_string(sc_time[0], TFORMAT = "YYYY-MM-DD")
      t0_base = time_double(t0_base_str + '/00:00:00')
      ut_hh = (sc_time - t0_base) / 3600.0				;	ut_hh now contains decimal time HH.frachour

      ;	If xscale_plot is set, look for the indices corresponding to these times, and trim array to this

      if keyword_set(xscale_plot) then begin
         l_edge = where((ut_hh - xscale_plot[0]) ge 0)
         if (l_edge[0] gt 0) then im_1 = l_edge[0] else im_1 = 0
         r_edge = where((ut_hh - xscale_plot[1]) ge 0)
         if (r_edge[0] gt 0) then im_2 = r_edge[0] else im_2 = n_elements(ut_hh) - 1
         ut_hh = ut_hh[im_1:im_2]
      endif else begin
         xscale_plot = [ut_hh[0], ut_hh[n_elements(ut_hh) - 1]]
         im_1 = 0
         im_2 = tot_images - 1
      endelse

      sc_time = d.x[im_1:im_2]
      n_images = im_2 - im_1 + 1			;	Number of images / timesteps
      the_asis[jj].n_images = n_images
      the_asis[jj].foundpeaks = ptr_new(replicate(foundpeak_array, n_images), /allocate_heap)

      ;
      ;	Get / calculate magnetic latitude and longitude
      ;	and identify bad points (low elevation) in the images
      ;

      ;		min_elevation = 6
      ;      min_elevation = 3

      if not keyword_set(min_elevation) then min_elevation = 6
      if keyword_set(raw) then min_elevation = 0
      ;		if not keyword_set(redline) then begin

      ;      if (1) then begin
      if rego_site_flag[jj] then begin
         thm_load_asi_cal, site[jj], asi_cal, /rego						;	Load the ASI cal file
         camera_name = 'clg_rgf'
      endif else begin
         print, 'loading calibration files'
         thm_load_asi_cal, site[jj], asi_cal						;	Load the ASI cal file
         camera_name ='thg_asf'
      endelse
      ele_name = camera_name + '_' + site[jj] + '_elev'

      ; get longitude/latitude/elevation arrays
      ; 2020 change on the data pointer arrays below

      lon_name = camera_name + '_'+site[jj]+'_mlon'
      field_index=where(asi_cal.vars.name eq lon_name)
;      x=reform((*asi_cal.vars[field_index[0]].dataptr)[1, *, *])
      x=reform((*asi_cal.vars[field_index[0]].dataptr)[*, *])
      ;      mlons = reverse(x, 1)
      mlons = x

      lat_name = camera_name + '_' + site[jj]+'_mlat'
      field_index=where(asi_cal.vars.name eq lat_name)
;      y=reform((*asi_cal.vars[field_index[0]].dataptr)[1, *, *])
      y=reform((*asi_cal.vars[field_index[0]].dataptr)[*, *])
      mlats = y

      ele_name = camera_name + '_'+site[jj]+'_elev'
      field_index=where(asi_cal.vars.name eq ele_name)
      elev=*asi_cal.vars[field_index[0]].dataptr
      elev_asi=reverse(elev,1)

      stop

      ; get rid of pixels with bad elevation

      bad_ele = where(elev_asi le min_elevation or finite(elev_asi) ne 1, c_bad)
      good_ele = where(elev_asi gt min_elevation and finite(elev_asi) eq 1, c_good)
      inf_ele = where(finite(elev_asi) ne 1, c_inf)

      the_asis[jj].bad_ele = ptr_new(bad_ele, /allocate_heap)


      if not usr_min_bytscl then begin
         ;
         ;	Work with the image in the middle of the time series to determine auto-scaling
         ;

         tmp_image = double(reform(d.y[im_1 + n_images / 2, *, *]))			;	Middle image
         tmp_image[bad_ele] = !values.f_nan									;	Remove bad points
         std_flag_value_moon = mean(tmp_image, /nan) + 6.0 * stddev(tmp_image, /nan)
         MOON_FLAG_VALUE = std_flag_value_moon
         tmp_image[where(tmp_image gt MOON_FLAG_VALUE)] = !values.f_nan				;	remove moon and lights to get good scaling

         ;      min_asi = min(tmp_image, /nan)
         ;      min_asi = (min_asi / 10^(long(alog10(min_asi)))) * 10^(long(alog10(min_asi)))
         ;      max_asi = 10^(long(alog10(min_asi)) + 1)

         tt = mean(tmp_image, /nan)
         stt = stddev(tmp_image, /nan)
         min_asi = tt - autoscale_min_stdev * stt
         max_asi = tt + autoscale_max_stdev * stt
         if min_asi lt 0 then min_asi = 1

         print, 'image scales for ', site[jj], ' are ',  min_asi, max_asi
         ;			if ((not keyword_set(redline)) && (min_asi lt 2500)) then min_asi = 2500
         if keyword_set(moon) then max_asi = 0.5 * max_asi
         if keyword_set(supermoon) then max_asi = 0.3 * max_asi
         print, 'image scales for ', site[jj], ' are ',  min_asi, max_asi
      endif

      the_asis[jj].image_ut = ptr_new(sc_time, /allocate_heap)

      ;print, size(d.x)
      ;print, size(d.y)

      ;	Do some scaling of the images

      ;		if not keyword_set(redline) then begin
      if (usr_min_bytscl eq 1) then min_bytscl_apply = min_bytscl[jj] else min_bytscl_apply = min_asi
      if (usr_max_bytscl eq 1) then max_bytscl_apply = max_bytscl[jj] else max_bytscl_apply = max_asi

      print, 'applying ', min_bytscl_apply, max_bytscl_apply, ' image scaling'

      scaled_images = float(bytscl(d.y[im_1:im_2, * , *], max = max_bytscl_apply, min = min_bytscl_apply, top=254))		; Have to do this so the averaging works correctly, b/c passed in data might be in bytes
      if keyword_set(histequal) then begin
         ;         histequal_images = hist_equal(d.y[im_1:im_2, * , *])
         print, 'Applying hist_equal...'
         histequal_images = d.y[im_1:im_2, * , *]
         for ijk = 0, n_images - 1 do begin
            histequal_images[ijk, *, *] = hist_equal(reform(histequal_images[ijk,*,*]))
         endfor
         print, '....done'
      end

      the_asis[jj].scaled_images = ptr_new(scaled_images, /allocate_heap)
      the_asis[jj].histequal_images = ptr_new(histequal_images, /allocate_heap)

      ;
      ;	Calculate the pixel locations of the longitudes to take the slices from
      ;

      kg_create_mlat_grid, mlats, mlons, good_ele, elev_asi, lat_points = lat_points, $
         lon_points = lon_points, latrange = latrange, image_size = image_size[jj], lat_hist = lat_hist, $
         lon_hist = lon_hist, min_elevation = min_elevation
         

      the_asis[jj].lat_points = ptr_new(lat_points, /allocate_heap)
      the_asis[jj].lon_points = ptr_new(lon_points, /allocate_heap)
      the_asis[jj].lat_hist = ptr_new(lat_hist, /allocate_heap)
      the_asis[jj].lon_hist = ptr_new(lon_hist, /allocate_heap)

      ;  Set the yscale plot to be min & max of latitude range of the ASI
      sc_latrange = [latrange[0] + 4.0, latrange[1] - 4.0]

      ;      if not keyword_set(yscale_plot) then yscale_plot = reform(rebin(sc_latrange, 2, number_of_sites)); ANOTHER HACK TO ACCOMMODATE REGO. SHOULD FIX
      if autoscale then yscale_plot = [[yscale_plot], [sc_latrange]]
      if keyword_set(single_plot) then begin
         if latrange[0] lt single_plot_min_lat then single_plot_min_lat = yscale_plot[0]
         if latrange[1] gt single_plot_max_lat then single_plot_max_lat = yscale_plot[1]
      endif

      if rego_site_flag[jj] then $
         kg_calculate_longitude_slices, *the_asis[jj].lat_hist, *the_asis[jj].lon_hist, the_asis[jj].image_size, good_ele, elev_asi, $
         plot_lon = longitudes[*, jj], lon_points = slice_points, min_elevation = min_elevation, lat_range = yscale_plot $
      else $
         kg_calculate_longitude_slices, *the_asis[jj].lat_hist, *the_asis[jj].lon_hist, the_asis[jj].image_size, good_ele, elev_asi, $
         plot_lon = longitudes[*, jj], lon_points = slice_points, min_elevation = min_elevation, lat_range = yscale_plot

      for abc = 0, n_longitudes - 1 do begin
         the_asis[jj].the_slices[abc].lon_points = ptr_new(slice_points[abc], /allocate_heap)
         the_asis[jj].the_slices[abc].xpoints = ptr_new(ptrarr(n_images), /allocate_heap)
         the_asis[jj].the_slices[abc].ypoints = ptr_new(ptrarr(n_images), /allocate_heap)
         the_asis[jj].the_slices[abc].amps = ptr_new(ptrarr(n_images), /allocate_heap)
         the_asis[jj].the_slices[abc].ff = ptr_new(ptrarr(n_images), /allocate_heap)
      endfor

      stop

      if keyword_set(thm_foot) then begin
         foot_base_location = '~/'
         base_time_str = strmid(time_start, 0, 10) + '_thd_footpoint_mag_par1.tplot'
         tplot_restore, filename=base_time_str, verbose=0
         get_data, 'thd_ifoot_th', data = thd_ifoot_th
         get_data, 'thd_ifoot_phi', data = thd_ifoot_phi
         kg_convert_footpoint2asigrid, *the_asis[jj].lat_hist, *the_asis[jj].lon_hist, lats2find=thd_ifoot_th.y, lons2find=thd_ifoot_phi.y, $
            thd_lat = thd_lat, thd_lon = thd_lon

         ;  Now interpolate to the asi timestep and store

         thd_lat = interpol(thd_lat, thd_ifoot_th.x, sc_time)
         thd_lon = interpol(thd_lon, thd_ifoot_th.x, sc_time)

         the_asis[jj].thm_lat = ptr_new(thd_lat, /allocate_heap)
         the_asis[jj].thm_lon = ptr_new(thd_lon, /allocate_heap)
      endif


      if keyword_set(mms_foot) then begin
         foot_base_location = '~/'
         base_time_str = strmid(time_start, 0, 10) + '_mms_footpoint_mag_par1.tplot'
         tplot_restore, filename=base_time_str, verbose=0
         get_data, 'mms_ifoot_th', data = mms_ifoot_th
         get_data, 'mms_ifoot_phi', data = mms_ifoot_phi
         kg_convert_footpoint2asigrid, *the_asis[jj].lat_hist, *the_asis[jj].lon_hist, lats2find=mms_ifoot_th.y, lons2find=mms_ifoot_phi.y, $
            thd_lat = mms_lat, thd_lon = mms_lon

         ;  Now interpolate to the asi timestep and store

         mms_lat = interpol(mms_lat, mms__ifoot_th.x, sc_time)
         mms_lon = interpol(mms_lon, mms_ifoot_th.x, sc_time)

         the_asis[jj].mms_lat = ptr_new(mms_lat, /allocate_heap)
         the_asis[jj].mms_lon = ptr_new(mms_lon, /allocate_heap)
      endif


      if not keyword_set(no_kepkogram) then begin
         print, 'Looking for peaks'

         ;	Loop through the longitudes and determine peaks

         for jkl = 0, n_longitudes - 1 do begin
            x = longitudes[jkl, jj]						;	Pull out the longitude to work with [lon#, site]
            the_asis[jj].the_slices[jkl].longitude = longitudes[jkl, jj]				;	Store the longitude to this slice
            lon_points = *the_asis[jj].the_slices[jkl].lon_points
            allpeaks_loc = []
            allpeaks_amp = []
            allpeaks_ut = []
            ;            print, 'column ', x, '(', jkl + 1, 'of ', n_longitudes, ')'
            print, x, jkl + 1, n_longitudes, format = '("Longitude ", F5.1, " (", I1, " of ", I1, ")")'

            for i = 0, n_images - 1 do begin    ; Step through every image for each longitude [jkl]
               tmp_i = reform(scaled_images[i, *, *])		;	Pull out the single image
               tmp_i(bad_ele) = !values.f_nan				;	Get rid of bad elevation

               ;					if keyword_set(denoise) then tmp_i = wv_denoise(tmp_i, 'coiflet', 5,percent=99 ,threshold=1)

               ff = dblarr(lon_points.npts)				;	This is the slice
               for jjj = 0, lon_points.npts - 1 do begin		;	step through the longitude slice and widen it for analysis
                  ff[jjj] = total(tmp_i[lon_points.x[jjj] - xavg:lon_points.x[jjj] + xavg, lon_points.y[jjj]], /nan) / (2.0 * xavg + 1.0)
               endfor
               ;				ff = total(tmp_i[x - xavg:x + xavg, *], 1, /nan) / (2.0 * xavg + 1.0)
               ff = smooth(ff, yavg, /nan)
               ff_org = ff

               kg_single_peakfinder_mlat, ff, peakloc = peakloc, peakamp = peakamp, npeaks = npeaks[jj]
               allpeaks_loc = [allpeaks_loc, reform(lon_points.latitudes(peakloc))]		; Save the latitude
               allpeaks_amp = [allpeaks_amp, peakamp]
               allpeaks_ut = [allpeaks_ut, replicate(ut_hh[i], n_elements(peakamp))]

               (*the_asis[jj].the_slices[jkl].xpoints)[i] = ptr_new(reform(lon_points.x[peakloc]), /allocate_heap)
               (*the_asis[jj].the_slices[jkl].ypoints)[i] = ptr_new(reform(lon_points.y[peakloc]), /allocate_heap)
               (*the_asis[jj].the_slices[jkl].amps)[i] = ptr_new(reform(peakamp), /allocate_heap)
               (*the_asis[jj].the_slices[jkl].ff)[i] = ptr_new(reform(ff_org), /allocate_heap)
            endfor
            the_asis[jj].the_slices[jkl].latitude = ptr_new(allpeaks_loc, /allocate_heap)
            the_asis[jj].the_slices[jkl].amplitude = ptr_new(allpeaks_amp, /allocate_heap)
            the_asis[jj].the_slices[jkl].ut = ptr_new(allpeaks_ut, /allocate_heap)

            the_asis[jj].the_slices[jkl].slice_ct = slice_color_cts[jkl]       ; Pull the color table from predefined slice color array
            the_asis[jj].the_slices[jkl].slice_psym = arcgram_psym
         endfor

         print, '.... done'

         ;	Need to renormalize the amplitude peaks so they scale from 0.1 to 1.5

         slice_min = 999
         slice_max = 0
         for jkl = 0, n_longitudes - 1 do begin
            tmin = min(*the_asis[jj].the_slices[jkl].amplitude)
            tmax = max(*the_asis[jj].the_slices[jkl].amplitude)
            if tmin lt slice_min then slice_min = tmin
            if tmax gt slice_max then slice_max = tmax
         endfor
         for jkl = 0, n_longitudes - 1 do begin
            *the_asis[jj].the_slices[jkl].amplitude = 100.0 * (0.5 + 1.7 * ((*the_asis[jj].the_slices[jkl].amplitude - tmin) / (tmax - tmin)))
         endfor

      endif else begin
         peakloc = [70, 72]
      endelse

      ;
      ;  Sort the amplitudes of every slice at each UT to either plot the small or large ones first
      ;  Work in progress....

      if keyword_set(maxfirst) then begin
         merged_slice_ut = []
         merged_slice_lat = []
         merged_slice_amp = []
         for jkl = 0, n_longitudes - 1 do begin
            merged_slice_ut = [merged_slice_ut, *the_asis[jj].the_slices[jkl].ut]
            merged_slice_amp = [merged_slice_amp, *the_asis[jj].the_slices[jkl].amplitude]
            merged_slice_lat = [merged_slice_lat, *the_asis[jj].the_slices[jkl].latitude]
         endfor
         sorted = sort(merged_slice_amp)
         merged_slice_ut = reverse(merged_slice_ut[sorted])
         merged_slice_amp = reverse(merged_slice_amp[sorted])
         merged_slice_lat = reverse(merged_slice_lat[sorted])
      endif

   endfor   ; Ends the jj loop on the_asis

   ;-----------------------------------------
   ;
   ;  Make an eps of the kepkograms
   ;
   ;-----------------------------------------

   if keyword_set(eps) then begin
      print, 'Writing eps....'
      if not keyword_set(single_plot) then begin
         !p.multi = [0]
         for jj = 0, number_of_sites - 1 do begin
            if not keyword_set(file_prefix) then file_prefix = '~/'
            kg_epsopen, 'slices_ampts_' + site[jj] + '.eps'

            ;  pos = [0.1, 0.3, 0.95, 0.7]
            ;        pos = [0.1, 0.3, 0.95, 0.55]

            loadct, 0, /silent
            plot, ut_hh, peakloc, psym = 4, /nodata, yrange = yscale_plot[*,jj], xrange = xscale_plot, xstyle=1, $
               color = 0, background = 255, ystyle=1, charthick = 2.0, charsize=1.5, ytitle=site[jj], pos = pos, xtitle=time_start, font=1

            ;            mid_longitude = mlons[image_size[jj] / 2, image_size[jj] / 2]
            ;            print, mid_longitude, ' is mid longitude'
            for jjkk = 0, n_longitudes - 1 do begin         ;  Convert amplitude to color here
               which_lon = the_asis[jj].the_slices[jjkk].longitude
               ;               diff_lon = mid_longitude - which_lon
               slices2plot = the_asis[jj].the_slices[jjkk]
               psym = symcat(the_asis[jj].the_slices[jjkk].slice_psym)
               loadct, the_asis[jj].the_slices[jjkk].slice_ct, /silent
               for zz = 0, n_elements(*slices2plot.amplitude) - 1 do begin
                  plots, (*slices2plot.ut)[zz], (*slices2plot.latitude)[zz], psym = psym, $
                     symsize = (*slices2plot.amplitude)[zz] / (1.5*symsize_scale), /continue, color = (*slices2plot.amplitude)[zz]/0.9, noclip=0
               endfor
               ;  Create a legend
               xyouts, xscale_plot[0], yscale_plot[1,jj] - (jjkk + 1) * (yscale_plot[1,jj] - yscale_plot[0,jj]) / 10, string(which_lon, format='(f5.1)'), $
                  /data, color = (*slices2plot.amplitude)[0] / 1.2, font = 0
            endfor

            if keyword_set(onset_location) then $
               plots, onset_location[0], onset_location[1], psym=6, /continue, symsize = 2, color=255, thick=3

            kg_epsclose

            write_jpeg, file_prefix + site[jj] + '.jpg', tvrd(/true), /true, quality = 90
         endfor
      endif else begin   ; End of eps no combined plot block
         ;  This makes a combined plot
         if not keyword_set(file_prefix) then file_prefix = ''
         kg_epsopen, file_prefix + 'slices_ampts_combined.eps'

         plot, ut_hh, peakloc, psym = 4, /nodata, yrange = [single_plot_min_lat, single_plot_max_lat], xrange = xscale_plot, xstyle=1, $
            color = 0, background = 255, ystyle=1, charsize=1.5, ytitle='all sites', pos=pos
         ;         slice_color_cts = [56, 57, 63, 61, 62]
         for jj = 0, number_of_sites - 1 do begin
            mid_longitude = mlons[image_size[jj] / 2, image_size[jj] / 2]
            print, mid_longitude, ' is mid longitude'
            for jjkk = 0, n_longitudes - 1 do begin         ;  Convert amplitude to color here
               slices2plot = the_asis[jj].the_slices[jjkk]
               psym = symcat(the_asis[jj].the_slices[jjkk].slice_psym)
               loadct, the_asis[jj].the_slices[jjkk].slice_ct, /silent
               for zz = 0, n_elements(*slices2plot.amplitude) - 1 do begin
                  plots, (*slices2plot.ut)[zz], (*slices2plot.latitude)[zz], psym = psym, $
                     symsize = (*slices2plot.amplitude)[zz] / symsize_scale, /continue, color = (*slices2plot.amplitude)[zz]/.9, noclip=0
               endfor
            endfor
            if keyword_set(onset_location) then $
               plots, onset_location[0], onset_location[1], psym=6, /continue, symsize = 2, color=255
         endfor
         kg_epsclose
      endelse
      print, '.... done writing eps'
   endif ; end eps block


   ;
   ; If they want a movie, loop through each site and make movies
   ;

   if keyword_set(makemovie) then begin
      if not keyword_set(file_prefix) then file_prefix = ''
      latcolor = 200
      ;  Create a filename with all the sites in it
      movie_fname = '';
      for jj = 0, number_of_sites - 1 do begin
         movie_fname = movie_fname + '_' + the_asis[jj].type + '_' + the_asis[jj].site
      endfor

      fname = '/Users/ekepko/' + file_prefix + 'movie' + movie_fname + '.mp4'
      print, 'making movie ', fname

      ;  Calculate the size of the window
      im_sc = the_asis[0].image_size * the_asis[0].image_size_multiplier
      oVid = IDLffVideoWrite(fname)
      vidStream = oVid.AddVideoStream(im_sc * (number_of_sites), im_sc, 45)

      window, 10, xsize = im_sc * (number_of_sites), ysize = im_sc
      loadct, 0, /silent

      for i = 0, the_asis[0].n_images - 1 do begin
         for jj = 0, number_of_sites - 1 do begin
            if keyword_set(histequal) then $
               tmp_i = (*the_asis[jj].histequal_images)[i, *, *] $
            else  $
               tmp_i = (*the_asis[jj].scaled_images)[i, *, *]
            ;            tmp_i = reform((*the_asis[jj].scaled_images)[i, *, *])      ;  Pull out the single image
            tmp_i = reform(tmp_i)      ;  Pull out the single image
            tmp_i(*the_asis[jj].bad_ele) = 0                            ;  Get rid of bad elevation
            tt = rebin(tmp_i, im_sc, im_sc)
            loadct, realtime_ct, /silent
            tv, tt, jj

            ;  Plot latitude lines and print the numbers

            if not keyword_set(nogrid) then begin
               kg_plot_latlon_lines, the_asis[jj], latcolor=latcolor, eps_scale=the_asis[jj].image_size_multiplier
               kg_plot_latlon_labels, the_asis[jj], char_plot=char_plot, latlon_charsize=latlon_charsize, latcolor=latcolor, eps_scale=the_asis[jj].image_size_multiplier
               kg_plot_longitude_slices, the_asis[jj], im_scl=the_asis[jj].image_size_multiplier, diagnostic=0, $
                  xavg = xavg, latlon_charsize=latlon_charsize, char_plot = char_plot, cursor_i = i, symsize_scale = symsize_scale
            endif
            loadct, 0, /silent
            xyouts, 10+im_sc*jj, 10, the_asis[jj].site+'_'+the_asis[jj].type, color=255, /device, charsize = 2
            xyouts, 10, 0.94 * im_sc, time_string((*the_asis[0].image_ut)[i], tformat='hh:mm:ss'), color=255, /device, charsize = 1.75

            ;            xyouts, 10, 10, the_asis[jj].site, color=200, /device, charsize = 2
            ;            xyouts, 10, 480, time_string((*the_asis[jj].image_ut)[i], tformat='hh:mm:ss'), color=200, /device, charsize = 1.75

            time = oVid.Put(vidStream, tvrd(/true))
         endfor
      endfor
      oVid.Cleanup
   endif    ; makemovie if


   ;---------------------------------------------------
   ;
   ;  This plots the kepkogram
   ;
   ;---------------------------------------------------

   if not keyword_set(no_kepkogram) then begin

      ;  See if window is open, and if not create it

      ;      if (jj eq 0) then begin
      device, window_state = theseWindows
      if (theseWindows(1) ne 1) then window, 1, xpos = 150, ypos = 250, xsize = 1500, ysize = 400 * number_of_sites ;n_elements(site[jj])
      ;      endif

      ;plot, ut_hh, peakloc, psym = 4, /nodata, yrange=[0, xrange_indices[1]-xrange_indices[0]], xrange = xscale_plot, xstyle=1, ystyle=1

      wset, 1
      if not keyword_set(single_plot) then begin
         !p.multi = [0, 1, number_of_sites]
         for jj = 0, number_of_sites - 1 do begin
            !p.multi = [jj, 1, number_of_sites]
            loadct, 0, /silent
            plot, ut_hh, peakloc, psym = 4, /nodata, yrange = yscale_plot[*,jj], xrange = xscale_plot, xstyle=1, $
               color = 0, background = 255, ystyle=1, charsize=1.5, ytitle=the_asis[jj].site + the_asis[jj].type
            if keyword_set(maxfirst) then begin
               psym = symcat(the_asis[jj].the_slices[0].slice_psym)
               loadct, the_asis[jj].the_slices[0].slice_ct, /silent
               for zz = 0, n_elements(merged_slice_ut) - 1 do begin
                  plots, merged_slice_ut[zz], merged_slice_lat[zz], psym = psym, $
                     symsize = merged_slice_amp[zz] / symsize_scale, color = merged_slice_amp[zz]/.9, noclip=0
               endfor
            endif else begin
               for jjkk = 0, n_longitudes - 1 do begin         ;  Convert amplitude to color here
                  slices2plot = the_asis[jj].the_slices[jjkk]
                  psym = symcat(the_asis[jj].the_slices[jjkk].slice_psym)
                  loadct, the_asis[jj].the_slices[jjkk].slice_ct, /silent
                  for zz = 0, n_elements(*slices2plot.amplitude) - 1 do begin          ;  Plot each circle
                     plots, (*slices2plot.ut)[zz], (*slices2plot.latitude)[zz], psym = psym, $
                        symsize = (*slices2plot.amplitude)[zz] / symsize_scale, color = (*slices2plot.amplitude)[zz]/.9, noclip=0
                  endfor
               endfor
               if keyword_set(onset_location) then $
                  plots, onset_location[0], onset_location[1], psym=6, /continue, symsize = 2, color=255
               ;         endif
            endelse
         endfor            ; jj asi loop
      endif else begin        ; Make a single combined plot
         plot, ut_hh, peakloc, psym = 4, /nodata, yrange = [single_plot_min_lat, single_plot_max_lat], xrange = xscale_plot, xstyle=1, $
            color = 0, background = 255, ystyle=1, charsize=1.5, ytitle='all sites'
         ;         slice_color_cts = [56, 57, 63, 61, 62]
         for jj = 0, number_of_sites - 1 do begin
            for jjkk = 0, n_longitudes - 1 do begin         ;  Convert amplitude to color here
               slices2plot = the_asis[jj].the_slices[jjkk]
               psym = symcat(the_asis[jj].the_slices[jjkk].slice_psym)
               loadct, the_asis[jj].the_slices[jjkk].slice_ct, /silent
               for zz = 0, n_elements(*slices2plot.amplitude) - 1 do begin
                  plots, (*slices2plot.ut)[zz], (*slices2plot.latitude)[zz], psym = psym, $
                     symsize = (*slices2plot.amplitude)[zz] / symsize_scale, color = (*slices2plot.amplitude)[zz]/.9, noclip=0
               endfor
            endfor
            if keyword_set(onset_location) then $
               plots, onset_location[0], onset_location[1], psym=6, /continue, symsize = 2, color=255
         endfor
      endelse        ; ends single kepkogram block
   endif             ; no_kepkogram flag

   if not (keyword_set(eps) or keyword_set(makemovie)) then begin    ; skip cursor control if they wanted eps or movie
      loadct, 0, /silent

      device, window_state = theseWindows
      if (theseWindows(0) ne 1) then window, 0, xpos = 150, ypos=250, xsize = 500, ysize=300
      wset, 0
      if not keyword_set(noload) then thm_load_pseudoae, verbose=0
      store_data, 'thg_idx_aul', data=['thg_idx_au', 'thg_idx_al'], verbose=0
      tplot, 'thg_idx_aul', verbose=0

      ;      device, window_state = theseWindows
      ;      if (theseWindows(2) ne 1) then window, 2, xpos = 150, ypos=250, xsize = 300, ysize=800

      if keyword_set(diagnostic) then slice_win = window(dimensions = [300, 800], window_title="The slices")

      wset, 1

      ;
      ;  Create a window for each site
      ;

      for jj = 0, number_of_sites - 1 do begin
         device, window_state = theseWindows
         if (theseWindows(jj + 3) ne 1) then window, jj + 3, xpos = 50, ypos=50, xsize = the_asis[jj].image_size * the_asis[jj].image_size_multiplier, $
            ysize = the_asis[jj].image_size * the_asis[jj].image_size_multiplier
         !p.multi=0
      endfor

      ;------------------------------------------
      ;
      ;
      ;	Main cursor control loop
      ;
      ;
      ;-------------------------------------------


      ;	window 0 is the AU/AL plot
      ;	window 1 is the kepkogram plot
      ;	window 3-N is the image
      ;	window 2 is the ff slice

      done = 0

      print, 'ready for cursor tracking'
      print, 'left click to end, right click to save the image'

      latlon_charsize = 1.5
      latcolor = 200

      cursor, x, y, /data, /nowait
      old_x = x
      old_y = y
      make_an_eps = 0

      wset, 1
      while not done do begin
         make_an_eps = 0
         eps_scale = 1
         cursor, x, y, /data, /nowait
         changed_cursor = abs(old_x - x) ne 0
         old_x = x
         old_y = y
         done = !MOUSE.button eq 1
         if !MOUSE.button eq 4 then begin
            changed_cursor = 1
            for site_num = 0, number_of_sites - 1 do begin
               wset, site_num + 3
               write_jpeg, '~/' + the_asis[site_num].site + '_image' + strtrim(string(index[0]), 2) + '.jpg', tvrd(/true), /true, quality = 90
               print, 'wrote image'
            endfor
            wset, 1
            make_an_eps = 1
         endif

         ;		akey = get_kbrd(0)
         ;		if (akey eq ' ') then make_an_eps = 1
         ;		if (akey eq '+') then begin
         ;			akey = get_kbrd(1)
         ;			which_win = fix(akey)-2
         ;			*the_asis[which_win].scaled_images = bytscl(*the_asis[which_win].scaled_images, max = 200, min = 0, top=253)
         ;		endif


         if changed_cursor then begin
            index = where(ut_hh ge x, count)

            if count ne 0 then begin
               cursor_i = index[0]
               for site_num = 0, number_of_sites - 1 do begin
                  wset, site_num + 3
                  loadct, realtime_ct, /silent
                  if keyword_set(histequal) then $
                     image2use = (*the_asis[site_num].histequal_images)[cursor_i, *, *] $
                  else  $
                     image2use = (*the_asis[site_num].scaled_images)[cursor_i, *, *]
                  tmp_i = reform(image2use)		;	Pull out the single image
                  tmp_i(*the_asis[site_num].bad_ele) = 0			;	Get rid of bad elevation
                  tmp_i = rebin(tmp_i,  the_asis[site_num].image_size * the_asis[site_num].image_size_multiplier, $
                     the_asis[site_num].image_size * the_asis[site_num].image_size_multiplier)					;	Make twice as big
                  ;					if keyword_set(denoise) then tmp_i = wv_denoise(tmp_i, 'coiflet', 5,percent=99.5,threshold=1)
                  if make_an_eps then begin
                     kg_epsopen, the_asis[site_num].site + '_image_nogrid'+time_string((*the_asis[0].image_ut)[cursor_i], tformat='hhmmss')+'.eps'
                     tvscl, tmp_i
                     if make_an_eps then kg_epsclose
                  endif

                  if QUICK_GRID and not keyword_set(nogrid) then begin
                     ;  set pixel location of lat/lon lines to white
                     tmp_i[(*the_asis[site_num].lat_points).x * the_asis[site_num].image_size_multiplier, (*the_asis[site_num].lat_points).y * the_asis[site_num].image_size_multiplier] = 255
                     tmp_i[(*the_asis[site_num].lon_points).x * the_asis[site_num].image_size_multiplier, (*the_asis[site_num].lon_points).y * the_asis[site_num].image_size_multiplier] = 255
                  endif
                  tv, tmp_i

                  if make_an_eps then begin
                     kg_epsopen, the_asis[site_num].site + '_image_yesgrid_'+time_string((*the_asis[0].image_ut)[cursor_i], tformat='hhmmss')+'.eps'
                     image_inches = 1.5
                     eps_scale = 5.0 * image_inches
                     tvscl, tmp_i, xsize = eps_scale * the_asis[site_num].image_size_multiplier, ysize = eps_scale* the_asis[site_num].image_size_multiplier, /inches
                     ;                     eps_scale = 3.0 * image_inches * (image_size_multiplier[site_num])
                     latlon_charsize = 1.0 * (image_inches / 4.0)
                     symsize_scale = 200.0 * (4.0 / image_inches)
                  endif

                  im_scl = eps_scale * the_asis[site_num].image_size_multiplier

                  ;	Plot latitude lines and print the numbers

                  if not keyword_set(nogrid) then begin
                     if not QUICK_GRID then begin
                        kg_plot_latlon_lines, the_asis[site_num], latcolor=latcolor, eps_scale=im_scl
                     endif
                     kg_plot_latlon_labels, the_asis[site_num], char_plot=char_plot, latlon_charsize=latlon_charsize, latcolor=latcolor, eps_scale=im_scl

                     if keyword_set(diagnostic) then begin
                        ;  Plot a box showing size of averaging window
                        l_edge = .88*the_asis[site_num].image_size         ; nominally 450
                        r_edge = .9375 * the_asis[site_num].image_size
                        plots, [l_edge, l_edge + 2 * xavg, l_edge + 2 * xavg, l_edge, l_edge], [r_edge, r_edge, r_edge + 2 * yavg, r_edge + 2 * yavg, r_edge], color = latcolor, thick=1.0, /device
                     endif
                  endif

                  ;	Plot sliced longitude lines

                  if not keyword_set(no_kepkogram) then  kg_plot_longitude_slices, the_asis[site_num], im_scl=im_scl, diagnostic=diagnostic, $
                     xavg = xavg, latlon_charsize=latlon_charsize, char_plot = char_plot, cursor_i = cursor_i, symsize_scale = symsize_scale

                  ;  Plot footpoints if they asked for it

                  if keyword_set(thm_foot) then begin
                     psym = symcat(16)
                     plots, (*the_asis[site_num].thm_lon)[cursor_i] * im_scl, (*the_asis[site_num].thm_lat)[cursor_i] * im_scl, color=250, symsize=2, psym=psym, /device
                     xyouts, (*the_asis[site_num].thm_lon)[cursor_i] * im_scl, (*the_asis[site_num].thm_lat)[cursor_i] * im_scl, 'thd', color=250, /device, charsize = latlon_charsize
                  endif    ; thm_foot check
                  if keyword_set(mms_foot) then begin
                     psym = symcat(16)
                     plots, (*the_asis[site_num].mms_lon)[cursor_i] * im_scl, (*the_asis[site_num].mms_lat)[cursor_i] * im_scl, color=250, symsize=2, psym=psym, /device
                     xyouts, (*the_asis[site_num].mms_lon)[cursor_i] * im_scl, (*the_asis[site_num].mms_lat)[cursor_i] * im_scl, 'mms', color=250, /device, charsize = latlon_charsize
                  endif    ; thm_foot check

                  loadct, 0, /silent
                  xyouts, 10, 10, the_asis[site_num].site+'_'+the_asis[site_num].type, color=255, /device, charsize = latlon_charsize
                  xyouts, 10, 0.94 * the_asis[site_num].image_size * im_scl, time_string((*the_asis[0].image_ut)[cursor_i], tformat='hh:mm:ss'), color=255, /device, charsize = latlon_charsize

                  if make_an_eps then begin
                     kg_epsclose
                     symsize_scale = 100.0
                     latlon_charsize = 1.5
                  endif

                  if (keyword_set(diagnostic)) then begin
                     slice_win.setcurrent
                     slice_win.erase
                     ;                     if site_num eq 0 then !p.multi = [0, 1, number_of_sites] else !p.multi = [site_num, 1, number_of_sites]
                     aplot = plot(*((*the_asis[site_num].the_slices[0].ff)[cursor_i]), $
                        (*the_asis[site_num].the_slices[0].lon_points).latitudes[0:(*the_asis[site_num].the_slices[0].lon_points).npts-1], yrange=[60,75],/current)
                  endif       ; Diagnostic loop
               endfor		; Ends looping through each station
            endif
            wset, 1
         endif
      endwhile
   endif          ; eps / movie if statement

   if keyword_set(diagnostic) then slice_win.close

   if not keyword_set(save_pointers) then begin
      print, 'freeing the pointers'
      for jj = 0, number_of_sites - 1 do begin
         ptr_free, the_asis[jj].scaled_images
         ptr_free, the_asis[jj].histequal_images
         ptr_free, the_asis[jj].bad_ele
         ptr_free, the_asis[jj].lat_points
         ptr_free, the_asis[jj].lon_points
         ptr_free, the_asis[jj].allpeaks_amp
         ptr_free, the_asis[jj].allpeaks_ut
         ptr_free, the_asis[jj].allpeaks_loc
         ptr_free, the_asis[jj].image_ut
         ptr_free, the_asis[jj].foundpeaks
         ptr_free, the_asis[jj].lon_hist
         ptr_free, the_asis[jj].lat_hist
         if keyword_set(thm_foot) then ptr_free, the_asis[jj].thm_lat
         if keyword_set(thm_foot) then ptr_free, the_asis[jj].thm_lon
         for xx = 0, n_elements(the_asis[jj].tmpnumslices) - 1 do begin
            ptr_free, the_asis[jj].the_slices[xx].latitude
            ptr_free, the_asis[jj].the_slices[xx].amplitude
            ptr_free, the_asis[jj].the_slices[xx].ut
            ptr_free, the_asis[jj].the_slices[xx].lon_points
            ptr_free, the_asis[jj].the_slices[xx].xpoints
            ptr_free, the_asis[jj].the_slices[xx].ypoints
            ptr_free, the_asis[jj].the_slices[xx].amps
            ptr_free, the_asis[jj].the_slices[xx].ff
         endfor
      endfor
   endif
end


