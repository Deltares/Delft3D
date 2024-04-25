subroutine dfmassc (s1        ,u1        ,qxk       ,hu        ,d0        , &
                  & dpu       ,porosu    ,gsqs      ,guu       ,tetau     , &
                  & kcs       ,kcu       ,kfu       ,thick     ,nmmax     , &
                  & kmax      ,icx       ,gdp )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  
!  
!!--description-----------------------------------------------------------------
!
!   adapts waterlevels and velocities at coupling boundaries
!   to prevent mass closure error in case of parallel runs
!
!!--pseudo code and references--------------------------------------------------
!
!   adapt waterlevel at start of row near coupling boundary using computed horizontal discharge
!   adapt waterlevel at end of row near coupling boundary using computed horizontal discharge
!   exchange waterlevel with neighbours
!   re-compute waterdepth at u-point and u-velocity at coupling boundary based on adapted waterlevel
!
!
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
    use globaldata
    !
    implicit none
    !
    type(globdat), target    :: gdp
!
! Global variables
!
    integer                                         , intent(in) :: icx    !  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                         , intent(in) :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in) :: nmmax  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfu    !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: d0     !  internal work array, explicit part of right-hand side
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: dpu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                   :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: gsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: porosu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: qxk    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                   :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: tetau  !  factor for upwind approach
    real(fp), dimension(kmax)                       , intent(in) :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: u1     !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer               :: k      ! loop variable
    integer               :: nm     ! loop variable
    real(fp),     pointer :: hdt
    real(fp)              :: qx     ! discharge in x-direction
    character(6), pointer :: momsol
    integer               :: nm_pos ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
!
!! executable statements -------------------------------------------------------
!
    momsol      => gdp%gdnumeco%momsol
    hdt         => gdp%gdnumeco%hdt
    !
    do nm = 1, nmmax
       !
       ! adapt waterlevel at start of row near coupling boundary using computed horizontal discharge
       !
       if ( kcs(nm) == 1 .and. kfu(nm - icx) /= 0 .and. kcs(nm - icx) == -1 ) then
          qx = 0.
          do k = 1, kmax
             qx = qx + qxk(nm, k) - qxk(nm - icx, k)
          enddo
          s1(nm) = hdt*(d0(nm) - qx)/gsqs(nm)
       endif
       !
       ! adapt waterlevel at end of row near coupling boundary using computed horizontal discharge
       !
       if ( kcs(nm) == 1 .and. kfu(nm) /= 0 .and. kcs(nm + icx) == -1 ) then
          qx = 0.
          do k = 1, kmax
             qx = qx + qxk(nm, k) - qxk(nm - icx, k)
          enddo
          s1(nm) = hdt*(d0(nm) - qx)/gsqs(nm)
       endif
    enddo
    !
    ! exchange s1 with neighbours
    !
    nm_pos = 1
    call dfexchg ( s1, 1, 1, dfloat, nm_pos, gdp )
    !
    ! re-compute waterdepth at u-point and u-velocity at coupling boundary based on adapted waterlevel
    !
    do nm = 1, nmmax
       !
       ! at start of row
       !
       if ( kcu(nm) == -1 .and. kcu(nm + icx) == 1 ) then
          !
          hu(nm) = tetau(nm)*s1(nm) + (1. - tetau(nm))*s1(nm + icx) + dpu(nm)
          !
          ! to-be-temporary trick in old parallel version
          !
          hu(nm)    = max(hu(nm), 0.01_fp) ! temporary solution to avoid negative hu
          !
          if (momsol /= 'flood') then
             do k = 1, kmax
                if (guu(nm)*hu(nm)*thick(k)*porosu(nm,k) == 0.0) write(gdp%gdinout%lundia,*)'WARNING divide by zero in dfmassc 1 ',nm,' ',k
                u1(nm,k) = qxk(nm,k)/(guu(nm)*hu(nm)*thick(k)*porosu(nm,k))
             enddo
          endif
       endif
       !
       ! at end of row
       !
       if ( kcu(nm) == 1 .and. kcu(nm + icx) == -1 ) then
          !
          hu(nm) = tetau(nm)*s1(nm) + (1. - tetau(nm))*s1(nm + icx) + dpu(nm)
          !
          ! to-be-temporary trick in old parallel version
          !
          hu(nm)    = max(hu(nm), 0.01_fp) ! temporary solution to avoid negative hu
          !
          !
          if (momsol /= 'flood') then
             do k = 1, kmax
                if (guu(nm)*hu(nm)*thick(k)*porosu(nm,k) == 0.0) write(gdp%gdinout%lundia,*)'WARNING divide by zero in dfmassc 2 ',nm,' ',k
                u1(nm,k) = qxk(nm,k)/(guu(nm)*hu(nm)*thick(k)*porosu(nm,k))
             enddo
          endif
       endif
    enddo
end subroutine dfmassc
