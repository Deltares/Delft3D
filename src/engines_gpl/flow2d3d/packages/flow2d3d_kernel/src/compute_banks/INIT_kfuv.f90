  SUBROUTINE INIT_kfuv(kfu,kfv,mmax,nmax,nlb,nub,mlb,mub,nmlb,nmub, gdp) 
!
!    initialize kfu and kfv to zero if cut edge fully dry. It is needed beacause two cells may be partially
!    active but have a common edge fully dry. Therefore, at the first time step wrong values are used 
!    in taubot for computing average vvv
!
    use globaldata
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    real(fp), dimension(:,:), pointer :: aguu
    real(fp), dimension(:,:), pointer :: agvv
      integer                                                             , intent(in)    :: mmax   !  Description and declaration in iidim.f90
      integer                                                             , intent(in)    :: nmax   !  Description and declaration in iidim.f90
      integer, dimension(nlb:nub,mlb:mub)                                , intent(out)   :: kfu   
      integer, dimension(nlb:nub,mlb:mub)                                , intent(out)   :: kfv  
      integer                                                             , intent(in)    :: nlb
      integer                                                             , intent(in)    :: nub
      integer                                                             , intent(in)    :: mlb
      integer                                                             , intent(in)    :: mub
      integer                                                             , intent(in)    :: nmlb
      integer                                                             , intent(in)    :: nmub
!
!     local variables
!    
      integer    :: n
      integer    :: m
!
!     executable statements
! 
    aguu => gdp%gdimbound%aguu
    agvv => gdp%gdimbound%agvv
      do m=1,mmax
         do n=1,nmax
            if (comparereal(aguu(n,m),0._fp)==0)  kfu(n,m) = 0
            if (comparereal(agvv(n,m),0._fp)==0)  kfv(n,m) = 0
         enddo
      enddo
!
    RETURN
    end subroutine INIT_kfuv
