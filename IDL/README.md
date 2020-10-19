
* Installation *


** Install IDL **

Register and download the IDL setup application from website: https://www.l3harrisgeospatial.com/


** Install libraries **

Thr IDL program needs following libraries


*** GAMAP ***

[Global Atmospheric Model Analysis Package](http://acmg.seas.harvard.edu/gamap/ "GAMAP home page") library must be downloaded and installed on local system.

Follow the [GAMAP Installation](http://acmg.seas.harvard.edu/gamap/doc/Chapter_2.html "GAMAP v2–19 User Guide") to download the source code via `git` and then set it up.
The GAMAP User Guide suggest to install the library under the `$HOME/IDL/` directory; I tried to install it under the directory: `d:/lib/IDL/` in MS Windows 10 system, and it looks like working.

As stated in the User Guide, I set the environment variable `IDL_STARTUP` to point to `idl_startup.pro` file stored in the `d:/lib/IDL/` directory.
The `idl_startup.pro` file stored in the latter directory is a copy of the same file stored in the directory: `gamap2/idl_startup/`, as suggested by documentation.
As last I updated my custom `idl_startup.pro` (but, in this file, cannot I use the environment variable? otherwise, why did I set it?).


**** Errors ****

***** Alert and Dialog Box *****

If GAMAP library does not find the file asked to open, it shows an alert message and then a dialog box where the end user can select the correct file.
The library function OPEN_FILE shows the alert and dialog box.


***** gamap_colors.tbl *****

If you get the following error:

% LOADCT: Error reading file: gamap2\color\tables\gamap_colors.tbl, READU: End of file encountered.
            Unit: 100, File: gamap2\color\tables\gamap_colors.tbl

Go to the directory `gamap2\color\tables\` and create a symbolic link `gamap_colors.tbl` to file `gamap_colors_idl8.tbl`.
As stated in README file stored in the same directory.
