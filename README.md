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

# Examples

Initialize the library for the first time.

```idl
asi_init

!!!!!!!!!!!!!!!!!!!!!!!!!
Enter value for - DATA_DIR: ENTER YOUR LOCAL DATA DIRECTORY, e.g., D:\data\asi_tools

```

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