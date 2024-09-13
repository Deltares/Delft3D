  subroutine volumNL_are_volumes(volum0,volum1,volum0NL,volum1NL,nmlb,nmub,kmax) 
!
  use precision
!
  implicit none
!
   real(fp)     , dimension(nmlb:nmub,kmax), intent(in)                             :: volum0     
   real(fp)     , dimension(nmlb:nmub,kmax), intent(in)                             :: volum1  
   real(fp)     , dimension(nmlb:nmub,kmax), intent(out)                            :: volum0NL     
   real(fp)     , dimension(nmlb:nmub,kmax), intent(out)                            :: volum1NL     
   integer                                 , intent(in)                             :: nmlb
   integer                                 , intent(in)                             :: nmub
   integer                                 , intent(in)                             :: kmax
!
     volum0NL(nmlb:nmub,1:kmax) = volum0(nmlb:nmub,1:kmax) 
     volum1NL(nmlb:nmub,1:kmax) = volum1(nmlb:nmub,1:kmax) 
!
  return 
end subroutine volumNL_are_volumes
