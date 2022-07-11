# asi_tools
Code for analyzing ASI data and producing some nice ASI plots. 

[![DOI](https://zenodo.org/badge/451494983.svg)](https://zenodo.org/badge/latestdoi/451494983)

# Table of Contents

- [Setup](#setup)
- [Requirements](#requirements)
- [Initialization](#initialization)
- [Skymaps](#skymaps)
- [Downloading and Loading Data](#downloading-and-loading-data)
- [Generating a Peakogram](#generating-a-peakogram)
- [Generating Keograms](#generating-keograms)


# Setup

Add the asi_tools directory and subdirectories to the IDL Path.

Easiest via the IDL IDE 
```
Window -> Preferences 

IDL -> Paths
```

Running ```asi_init``` will initialize the system variable ```!asi_tools``` which is used to define the url's for downloading data and the local directory ```data_dir``` for storing and downloading data.

# Requirements 

[SPEDAS](https://spedas.org/blog/)

## Initialization

Initialize the library for the first time (below). And set idl system variable ```!asi_tools``` which specifies a number of requirements including the directory to download data and the url's where data is accessed. 

```idl
asi_init

!!!!!!!!!!!!!!!!!!!!!!!!!
Enter value for - DATA_DIR: ENTER YOUR LOCAL DATA DIRECTORY, e.g., D:\data\asi_tools

```

Running ```asi_init``` for the the first time will generate the file ```asi_tools\config\asi_config.txt```, a text file which stores the ```!asi_tools``` variables for subsequent loading.

## Skymaps

Download skymaps for a particular station apart of aparticular array. 

```idl
; download themis gill skymap
path = asi_download_skymap(site='gill',/themis)
path = asi_download_skymap(site='gill')
path = asi_download_skymap(site='gill_themis')

; download rego gillam skymap
path = asi_download_skymap(site='gill_rego')
path = asi_download_skymap(site='gill',/rego)
```

## Downloading and Loading Data

Downloading and loading data is done via ```asi_load_data.pro``` which downloads 1 minute PGM files from the [University of Calgary data server](https://data.phys.ucalgary.ca/) and then loads the data using ```trex_imager_readfile.pro``` which is included as part of the ```/external/``` libraries/code and can also be found on the [University of Calgary's GitHub repository](https://github.com/ucalgary-aurora/trex-imager-readfile). 

Download/loading examples

```idl
; read themis data
dat = asi_load_data('rank', '2017-09-15/02:30:00', 30, /minutes)

; read themis data from local directory
dat = asi_load_data('gill_themis', '2007-03-07/05:52:00', 8, /minutes, /local)

; return only the local paths to some themis data
dat = asi_load_data('gill_themis', '2007-03-07/05:52:00', 8, /minutes, /path_only)

; read rego data
dat = asi_load_data('gill', '2015-02-02/10:00:00', 40, /minutes, /rego)

; read rego data locally only
dat = asi_load_data('gill_rego', '2015-02-02/10:00:00', 40, /minutes, /no_download)

; read TREX RGB data
dat = asi_load_data('fsmi', '2019-02-18/03:25:00', 30, /minutes, /rgb, /meta)
dat = asi_load_data('fsmi_rgb', '2019-02-18/03:25:00', 30, /minutes, /meta)

; make a quick movie of last loaded data set
window, xsize=dat.asi_x, ysize=dat.asi_y
for i=0L, dat.asi_frames do tvscl,alog10(reform((dat.asi_img[*,*,i])))
```

## Generating a Peakogram

The peakogram searches for peaks in auroral brightness at fixed longitudes. It allows users to investigate the latitudinal motion and evolution of the aurora. In order to be able to run on mutliple stations and multiple arrays the functionality is slightly different then that of loading data using ```asi_load_data```. Namely stations must be passed using the 4 chracter site code followed by an underscore and the array, ```????_themis```. 

Peakograms can be easily plotted using ```asi_peakoplot, pk_str``` where ```pk_str``` is the structure returned from ```asi_peakogram( )```. ```asi_peakoplot``` will plot multiple stations, overplot on the existing plot, and (for single stations) will allow the user to plot the asi image and peak in brightness for the time corresponding to the position of the cursor in peakoplot.

### Examples

```idl
; generate a peakogram from the GILL REGO ASI at two longitudes
; and plot it using the peakogram plotting procedure
dat = asi_peakogram('gill_rego', '2015-02-02/10:00:00', 60, /minutes, n_longitude=2, min_elevation=15)
!x.omargin=[0,15]
asi_peakoplot, dat, yrange=[64,67]

; same as above but load data locally (don't search web for data)
; and plot asi images and peak locations using pkcursor
dat = asi_peakogram('gill_rego', '2015-02-02/10:00:00', 60, /minutes, n_longitude=2, min_elevation=15, /local)
asi_peakoplot, dat, yrange=[64,67], /pkcursor 

; generate a peakogram from SNKQ THEMIS, GILL REGO, FSIM THEMIS
; using different parameters for each station e.g., FOR SNKQ use 
; altitude 1, 3 peaks, 3 longitudes, smooth over 5 pixels, do not 
; remove the moon, and set minimum elevation to 10 degrees. 
; add a tplot like plot for all peakograms

dat = asi_peakogram(['snkq_themis','gill_rego','fsim_themis'],'2015-02-02/10:20:00', 20, /minutes, $
             alt=[1,2,1], n_peaks=[3,2,2], n_longitudes=[3,1,1], $
             px_smooth=[5,10,11], moon=[0,0,0], min_elevation=[10,10,10], /add_tplot)

;setup tplot for plotting
thm_init
tplot,'*'

; generate peakogram from three stations
; plot peakogram with different options for 
;  each station
; call pkcursor on one station

dat = asi_peakogram(['snkq_themis','kuuj_themis','gill_themis'], '2011-04-09/04:00:00', 45, /minutes,n_longitude=2, min_elevation=10)
asi_peakoplot, dat,/log, imin=[3000,3000,3000], imax=[10000,10000,10000],yrange=[[62,69],[63,68],[62,72]]

window, 0
asi_peakoplot, dat.gill_themis,/log, /pkcursor

```

## Generating Keograms

```asi_tools``` has the ability to generate North-South and East-West keograms where the keograms are in equally spaced geomagnetic coordiantes, e.g., North-South keograms in geomagnetic latitude and East-West keograms in geomagnetic longitude, rather then pixel coordinates. The keograms are generated using the skymap pixel positions and can be calculated for any of the skymap altitudes(90, 100, 150 km). North-South keograms are generated with ```asi_nskeo()``` and East-West keograms are generated with ```asi_ewkeo()```. Similar to ```asi_peakogram()```, in order to easily generate keograms over a fixed time frame from multiple stations apart of different arrays both keogram functions use the following format for station names ```????_array```, where ```????``` is the four letter station code and ```array``` is the array to load the station data from, e.g., ```themis```, ```rego```. The ```keo_pos``` keyword for each of the keogram functions will plot the location and bins of the keograms in the field-of-view of the all sky imager.

Both keogram functions return an IDL structure which contains metadata information (station name and array), the keogram, the y-axis (latitude or longitude), the x-axis (time) and the y-range. If multiple stations are passed then a structure of structures is returned. 

Keograms can be easily plotted using by setting the ```add_tplot``` keyword which stores the keograms as tplot variables. Keograms can also be plotted with the ```asi_keoplot``` procedure using ```asi_keoplot, keo_str```, where ```keo_str``` is the structure returned from one of the keogram functions. ```asi_keoplot``` will plot multiple stations and allow users to overplot on the existing plot.

The the advantage of ```asi_tools``` is that the keograms have an equally spaced y-axis (latitude or longitude) allowing for quantifiable analysis (e.g. power spectral density/Fourier analysis). For each function the user specifies the limits of the keogram, min and max geomagnetic latitude/longitude, and the number of bins to divide the latitudinal region (N-S keogram) or longitidinal region (E-W) into. Both keogram functions also offer the utility to plot the region of keogram and keogram bins overtop of an ASI image using the ```keo_pos``` keyword. Two examples are below. 

### Examples

```idl
; generate and plot a North-South keogram
; from the GBAY REGO asi from
; the Gilles et al. 2018 paper

gill_keo = asi_nskeo('gill_rego', '2015-02-02/10:00:00', 60, 100, 62, 68, -26, -25, /minutes, min_elevation=15, /add_tplot, /keo_pos)

window, 0, xsize=750, ysize=200
!x.margin=[15,15]
asi_keoplot, gill_keo, /log, ytitle='Geomagnetic Latitude', xtitle='Time - UT'

; generate an East-West keogram
; from the GBAY THEMIS asi from
; the Tian et al. 2022 paper

gbay_keo = asi_ewkeo('gbay_themis', '2015-02-18/01:55:00', 25, 50, 18, 29, 61, 63, /minutes, min_elevation=15, /add_tplot,/keo_pos)

window, 0, xsize=750, ysize=200
!x.margin=[15,15]
asi_keoplot, keo_gbay, /log, ytitle='Geomagnetic Longitude', xtitle='Time - UT'

```