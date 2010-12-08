@echo off
"c:\Program Files\7-Zip\7z" a -r -x@Exclude.lst GR32PNG.7z *.pas *.inc *.dfm *.lfm *.dpr *.lpr *.lpi *.diff *.groupproj *.png  
pause