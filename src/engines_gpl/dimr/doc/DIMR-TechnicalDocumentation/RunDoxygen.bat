mkdir "..\BuildLogs"
"doxygen.exe" DIMR.Doxyfile > Doxygen.log

call "latex\make.bat" >> Doxygen.log

copy "latex\refman.pdf" "..\DIMR - Technical Documentation.pdf" >> Doxygen.log
copy "Doxygen.log" "..\BuildLogs\Doxygen.log.txt"



