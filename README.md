# asi_tools
Code for analyzing ASI data and producing some nice ASI plots. 

# Setup

Add the asi_tools directory (and subdirectory) to the IDL Path.

Easiest via the IDL IDE 
Windows -> Preferences
IDL -> Paths

Running ```asi_init``` will initialize the system variable ```!asi_tools``` which is used to define the url's for downloading data and the local directory ```data_dir``` for storing and downloading data.

# Requirements 

[SPEDAS](https://spedas.org/blog/)

## Initialization

Initialize the library for the first time. And set idl system variable ```!asi_tools``` which specifies a number of requirements including the directory to download data and the url's where data is accessed 

```idl
asi_init

!!!!!!!!!!!!!!!!!!!!!!!!!
Enter value for - DATA_DIR: ENTER YOUR LOCAL DATA DIRECTORY, e.g., D:\data\asi_tools

```

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

## Download and loading data

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

Peakograms can be easily plotted using ```peakoplot, pk_str``` where ```pk_str``` is the structure returned from ```asi_peakogram( )```. ```peakoplot``` will plot multiple stations, overplot on the existing plot, and (for single stations) will allow the user to plot the asi image and peak in brightness for the time corresponding to the position of the cursor in peakoplot.

Examples

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


```