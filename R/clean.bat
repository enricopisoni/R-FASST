@echo off

echo ATTENTION: by continuing you'll erase all output and intermediate files.
choice /C YN  /D N  /T 10  /M "Do you want to erase such files?"
if ERRORLEVEL 255 goto :EOF
if ERRORLEVEL   2 goto :EOF

del  /s /q  ..\Files\testing\Rplots.pdf
del  /s /q  ..\Files\testing\Project\*.*
del  /s /q  ..\Files\testing\prj-fasst\*.*
del  /s /q  ..\Files\testing\prj-emep\*.*

del         ..\Files\INPUT\ANCILLARY\MORTALITY\GBD_2017_BASEMORT_GRIDMAPS\*.csv
del         ..\Files\INPUT\ANCILLARY\MORTALITY\GBD_2017_BASEMORT_GRIDMAPS\*.nc
del         ..\Files\INPUT\ANCILLARY\MORTALITY\*.csv
del         ..\Files\INPUT\ANCILLARY\MORTALITY\*.nc

del  /s /q  ..\Files\INPUT\ANCILLARY\MORTALITY\EMEP\*.*
del  /s /q  ..\Files\INPUT\ANCILLARY\MORTALITY\FASST\*.*


echo All output and intermediate files erased.

:EOF

