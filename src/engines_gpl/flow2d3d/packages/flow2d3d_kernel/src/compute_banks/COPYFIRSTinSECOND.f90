SUBROUTINE COPYFIRSTinSECOND(var1,var2,var1COPY,var2COPY,kmax,nmmax,nmlb,nmub,nst)
   use precision
   IMPLICIT NONE
   real(fp), dimension(nmlb:nmub, 0:kmax)       :: var1,var1COPY     !  Description and declaration in esm_alloc_real.f90
   real(fp), dimension(nmlb:nmub, 0:kmax)       :: var2,var2COPY
   integer                                      :: kmax,nmmax,nmlb,nmub,nst
!
   var1COPY = var1
   var2COPY = var2
!
return
end subroutine COPYFIRSTinSECOND
