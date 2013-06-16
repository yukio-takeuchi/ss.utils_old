#call rtools6415.bat
set LANG=C
set LC_ALL=C
R CMD INSTALL  --build -l ./temp ss.utils
R CMD build ss.utils
