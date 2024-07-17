for %%a in ("*.F90") do (
    copy "gpl.txt"+"%%~a" "%%~a.tmp" /B
    move /Y "%%~a.tmp" "%%~a"
)