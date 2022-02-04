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

```
asi_init

!!!!!!!!!!!!!!!!!!!!!!!!!
Enter value for - DATA_DIR: ENTER YOUR LOCAL DATA DIRECTORY, e.g., D:\data\asi_tools

```