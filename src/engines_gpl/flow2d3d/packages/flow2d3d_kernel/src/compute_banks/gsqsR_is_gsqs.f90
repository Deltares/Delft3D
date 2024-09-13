  subroutine gsqsR_is_gsqs(gsqs,gsqsR,nmlb,nmub)
!
  use precision
!
  implicit none
!
   real(fp)     , dimension(nmlb:nmub), intent(in)                             :: gsqs    !  Description and declaration in esm_alloc_real.f90
   real(fp)     , dimension(nmlb:nmub), intent(inout)                          :: gsqsR    !  Description and declaration in esm_alloc_real.f90
   integer                            , intent(in)                             :: nmlb
   integer                            , intent(in)                             :: nmub
!
     gsqsR(nmlb:nmub) = gsqs(nmlb:nmub) 
!
  return 
end subroutine gsqsR_is_gsqs
