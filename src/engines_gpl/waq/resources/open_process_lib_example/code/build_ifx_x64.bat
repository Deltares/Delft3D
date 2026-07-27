call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat"

ifx -c stdlib_kinds.f90 precision_basics.f90 precision.f90 waq_precision.f90 logger_helper.f90 m_extract_waq_attribute.f90
ifx decayt.f90 waq_precision.obj logger_helper.obj m_extract_waq_attribute.obj -dll

del decayt.exp decayt.lib *.obj *.mod
