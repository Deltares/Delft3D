subroutine EXACTslopeSUB(kfu,kfv,xz,yz,nmmax,icx,icy,gdp) !to print exact solution for slope    
    use precision
    use mathconsts
    use sediment_basics_module
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    integer               , pointer :: EXACTSLOPE
    real(fp), dimension(:), pointer :: dzduuCENTR
    real(fp), dimension(:), pointer :: dzdvvCENTR
!
! Global variables
!
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in)  :: xz
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in)  :: yz
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in)  :: kfu
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in)  :: kfv
    integer                                               , intent(in)  :: nmmax
    integer                                               , intent(in)  :: icx
    integer                                               , intent(in)  :: icy
!
! Local variables
!
    integer  :: m,n,nm,nmd,ndm
    real(fp) xx,yy,x2y2,dtetadx,dtetady,drdx,drdy,dzdr,dzdteta
!
! executable statements -------------------------------------------------------
!
    EXACTSLOPE => gdp%gdimbound%EXACTSLOPE
    dzduuCENTR => gdp%gdimbound%dzduuCENTR
    dzdvvCENTR => gdp%gdimbound%dzdvvCENTR
      nmd = -icx
      ndm = -icy 
      do nm=1,nmmax
         nmd = nmd+1
         ndm = ndm+1 
         if (kfu(nm)==1.or.kfv(nm)==1.or.kfu(nmd)==1.or.kfv(ndm)==1) then !since kfs is not passed
            !call nm_to_n_and_m(nm, n, m, gdp)
            ! provide dzduuCENTR,dzdvvCENTR for printing. Note that it has done twice (idir==1 and 2) but it still not enough, since both the dir for a cut cell can be wall and it is skipped 
            select case (exactSLOPE)
            case(2)
            !anular channel with 60 m of radius
               dzdr =    0._fp !0.043955483387823_fp !0.035 !
               dzdteta = 0.06_fp
               xx = xz(nm)
               yy = yz(nm)
               x2y2 = xx**2+yy**2
               drdx = xx/sqrt(x2y2)
               drdy = yy/sqrt(x2y2)
               dtetadx = - yy/x2y2
               dtetady =   xx/x2y2
               dzduuCENTR(nm) = dzdr*drdx+dzdteta*dtetadx
               dzdvvCENTR(nm) = dzdr*drdy+dzdteta*dtetady  
            case default
               write(*,*) ' exactSLOPE not admitted' 
               call d3stop(1, gdp)
            end select

         endif
      enddo
 return
end subroutine EXACTslopeSUB
