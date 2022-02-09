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
dat = asi_load_data('gill_themis', '2007-03-07/05:52:00', 8, /minutes)

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

