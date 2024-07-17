for %%a in ("*.cpp") do (
    copy "gpl.txt"+"%%~a" "%%~a.tmp" /B
    move /Y "%%~a.tmp" "%%~a"
)