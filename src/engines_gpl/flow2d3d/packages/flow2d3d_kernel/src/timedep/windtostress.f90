subroutine windtostress(mmax      ,nmax      ,nmaxus    ,grdang    ,kcs       , &
                      & w10mag    ,windu     ,windv     ,windsu    ,windsv    , &
                      & windcd    , gdp       )
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
!    Function: Conversion from wind velocity to stresses
! Method used: -
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)               , pointer :: rhoa
    real(fp)               , pointer :: ag
    real(fp), dimension(:) , pointer :: wstcof
    integer                , pointer :: wslake
    integer                , pointer :: sdlake
!
! Global variables
!
    integer, intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer              :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    real(fp), intent(in)  :: grdang !  Description and declaration in tricom.igs
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: w10mag !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: windsu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: windsv !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: windu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: windv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: windcd !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer :: m      ! Loop variable 
    integer :: n      ! Loop variable 
    real(fp):: cd     ! Resulting windcoefficient 
    real(fp):: w1
    real(fp):: w2
    real(fp):: w3
    real(fp):: w4
    real(fp):: w5
    real(fp):: w6
    real(fp):: wangle ! Wind angle 
    real(fp):: wsp    ! Wind speed 
    real(fp):: wsp2   ! WSP*WSP 
    real(fp):: cdwl   ! Cd according to Wuest & Lorke (2003)
!
!! executable statements -------------------------------------------------------
!
    rhoa      => gdp%gdphysco%rhoa
    ag        => gdp%gdphysco%ag
    wstcof    => gdp%gdphysco%wstcof
    wslake    => gdp%gdheat%wslake
    sdlake    => gdp%gdheat%sdlake   
    !
    w1 = wstcof(1)
    w2 = wstcof(2)
    w3 = wstcof(3)
    w4 = wstcof(4)
    w5 = wstcof(5)
    w6 = wstcof(6)
    !
    do m = 1, mmax
       do n = 1, nmaxus
          if (kcs(n,m) > 0) then
             if (windu(n, m)==0. .and. windv(n, m)==0.) then
                wangle = 0.0_fp
             else
                !
                ! GRDANG allows for y-axis not pointing to the north
                ! is always 0 for sferic
                !
                wangle = atan2(windv(n, m), windu(n, m)) + grdang*degrad
             endif
             wsp2 = windu(n, m)*windu(n, m) + windv(n, m)*windv(n, m)
             wsp = sqrt(wsp2)
             !
             ! For  Windspeed <= W2  CDCOEF = W1
             ! W2 < Windspeed <  W4  CDCOEF = W1+(WSP-W2)/(W4-W2)*(W3-W1)
             !      Windspeed >= W4  CDCOEF = W3
             !
             if (wsp <= w2) then
                cd = w1
             elseif (wsp >w2 .and. wsp <=w4) then
                cd = w1 + (min(wsp,w4) - w2) / (w4 - w2) * (w3 - w1)
             else
                cd = w3 + (min (wsp,w6) - w4) / (w6 - w4) * (w5 - w3)
             endif
             !
             ! Wuest & Lorke (2003)
             !
             if(wslake == 1) then
                 ! Limit wsp to > 0.5. Stay on the safe side in case cd is copied to Stanton/Dalton, see code below
                 cdwl  = 0.0044_fp/max(wsp,0.5_fp)**1.15_fp
             else
                 cdwl = 0.0_fp
             endif
             !
             ! find the max.
             !
             windcd(n,m) = max(cdwl, cd)
             !
             windsu(n,m) = windcd(n,m) * wsp2 * cos(wangle) * rhoa
             windsv(n,m) = windcd(n,m) * wsp2 * sin(wangle) * rhoa
             w10mag(n,m) = wsp
             if((sdlake == 1) .and. (wslake == 1)) then
                gdp%gdheat%stanton = windcd(n,m) * gdp%gdheat%mulsta
                gdp%gdheat%dalton  = windcd(n,m) * gdp%gdheat%muldal
             endif
          endif
       enddo
    enddo
end subroutine windtostress
