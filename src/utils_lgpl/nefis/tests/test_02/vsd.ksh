vs.exe << +++++
use datac2.dat def datac2.def
!rm -f dispstat
disp stat to dispstat
{
let x = aa from group1
let b = ab from group2(2,2,1;5,5,1;6,6,1)
write x to conny
write b to conny
}
quit
+++++
