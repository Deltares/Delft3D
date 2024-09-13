SUBROUTINE W1prescribed(w1,qzk,xz,yz,gsqs,agsqs_link,kmax,nmmax,nmlb,nmub,nst)
   use precision
   IMPLICIT NONE
   real(fp), dimension(nmlb:nmub, 0:kmax)       :: w1      !  Description and declaration in esm_alloc_real.f90
   real(fp), dimension(nmlb:nmub, 0:kmax)       :: qzk      !  Description and declaration in esm_alloc_real.f90
   real(fp), dimension(nmlb:nmub)               :: xz      !  Description and declaration in esm_alloc_real.f90
   real(fp), dimension(nmlb:nmub)               :: yz      !  Description and declaration in esm_alloc_real.f90
   real(fp), dimension(nmlb:nmub)               :: gsqs      !  Description and declaration in esm_alloc_real.f90
   real(fp), dimension(nmlb:nmub)               :: agsqs_link      !  Description and declaration in esm_alloc_real.f90

   integer                                      :: kmax,nmmax,nmlb,nmub,nst,nm
!
   do nm=1,nmmax
      if ( sqrt(xz(nm)**2+yz(nm)**2)   > 66._fp) then
          w1(nm,1:kmax-1) = -0.03
      elseif ( sqrt(xz(nm)**2+yz(nm)**2)   < 54._fp) then
          w1(nm,1:kmax-1) =  0.03
      endif
      qzk(nm,1:kmax-1) = w1(nm,1:kmax-1)*gsqs(nm)*agsqs_link(nm) 
   enddo

!
return
end subroutine W1prescribed
