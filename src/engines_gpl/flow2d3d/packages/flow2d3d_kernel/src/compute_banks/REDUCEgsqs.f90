  subroutine REDUCEgsqs(gsqs,agsqs,gsqsR,nmlb,nmub)
!
  use precision
!
  implicit none
!
   real(fp)     , dimension(nmlb:nmub), intent(in)                             :: gsqs    !  Description and declaration in esm_alloc_real.f90
   real(fp)     , dimension(nmlb:nmub), intent(in)                             :: agsqs    !  Description and declaration in esm_alloc_real.f90
   real(fp)     , dimension(nmlb:nmub), intent(inout)                          :: gsqsR    !  Description and declaration in esm_alloc_real.f90
   integer                            , intent(in)                             :: nmlb
   integer                            , intent(in)                             :: nmub
!
     gsqsR(nmlb:nmub) = gsqs(nmlb:nmub)*agsqs(nmlb:nmub)
!
  return 
end subroutine REDUCEgsqs
