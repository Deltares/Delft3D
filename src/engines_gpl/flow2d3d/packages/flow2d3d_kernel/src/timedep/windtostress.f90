subroutine windtostress(mmax      ,nmax      ,nmaxus    ,grdang    ,kcs       , &
                      & w10mag    ,windu     ,windv     ,windsu    ,windsv    , &
                      & gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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
!  $Id: windtostress.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/timedep/windtostress.f90 $
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
!
!! executable statements -------------------------------------------------------
!
    rhoa      => gdp%gdphysco%rhoa
    ag        => gdp%gdphysco%ag
    wstcof    => gdp%gdphysco%wstcof
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
                wangle = 0.0
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
             windsu(n, m) = cd*wsp2*cos(wangle)*rhoa
             windsv(n, m) = cd*wsp2*sin(wangle)*rhoa
             w10mag(n, m) = wsp
          endif
       enddo
    enddo
end subroutine windtostress
