@echo off

echo ATTENTION: by continuing you'll erase all output and intermediate files.
choice /C YN  /D N  /T 10  /M "Do you want to erase such files?"
if ERRORLEVEL 255 goto :EOF
if ERRORLEVEL   2 goto :EOF

del  /s /q  ..\Files\testing\Rplots.pdf
del  /s /q  ..\Files\testing\Project\*.*

del  /s /q  ..\Files\INPUT\ANCILLARY\MORTALITY\*.*

echo All output and intermediate files erased.

:EOF

