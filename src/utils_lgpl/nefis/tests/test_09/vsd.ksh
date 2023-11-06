vs.exe <<+++++
!rm -f dispstat
use datac9.dat def datac9.def
disp stat to dispstat
let Var_001 = ELEM_R_4        (1,1,1) from DATAGRP_TEST_1A (1,1000,1)
write Var_001 to janm.tkl
+++++
