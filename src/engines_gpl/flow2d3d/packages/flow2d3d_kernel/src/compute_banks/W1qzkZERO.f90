SUBROUTINE W1qzkZERO(var1,var2,kmax,nmmax,nmlb,nmub,nst)
   use precision
   IMPLICIT NONE
   real(fp), dimension(nmlb:nmub, 0:kmax)       :: var1      !  Description and declaration in esm_alloc_real.f90
   real(fp), dimension(nmlb:nmub, 0:kmax)       :: var2
   integer                                      :: kmax,nmmax,nmlb,nmub,nst
!
   var1  = 0._fp
   var2  = 0._fp
!
return
end subroutine W1qzkZERO
