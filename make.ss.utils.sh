#call rtools6415.bat
set LANG=C
set LC_ALL=C
R64 CMD INSTALL  --build -l ./temp ss.utils
R64 CMD build ss.utils