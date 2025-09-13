vs.exe << +++++
use datac10.dat def datac10.def
!rm -f dispstat
!rm -f vc_10.res
disp stat to vc_10.res
let b = Real4(2,2,1) from var_1601(1,1601,1)
let c = Real4(2,2,1) from fixed(1,1601,1)
write b c to vs_c10.res
quit
+++++
vi vs_c10.res
