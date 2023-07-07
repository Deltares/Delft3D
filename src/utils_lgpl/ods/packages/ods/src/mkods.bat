rem Build ods.dll

del *.obj

cl @nefis.rsp
cl @ods.rsp

ifort -extend-source:132 -I../include ../srcf/getlga.f ../srcf/mor_sys.f ../srcf/odsaux.f ../srcf/phidias.f ../srcf/tri_his.f ../srcf/tri_map.f *.obj -dll -exe:ods.dll

rem gcc -o testods testods.c libODS.so -I../include -g
rem gcc -o testods testods.c *.o -I../include -g -lgfortran
