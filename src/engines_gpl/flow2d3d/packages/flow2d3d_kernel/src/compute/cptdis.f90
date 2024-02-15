subroutine cptdis(lundia     ,ag         ,area       ,alfa       ,cd1        , &
                & cd2        ,cd3        ,leng       ,mann       ,coefl      , &
                & olddis     ,htculv     ,itype      ,isw        ,positc     , &
                & rdis       ,s0in       ,s0out      ,width      ,gdp        )
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
! Computes the discharge relation through a culverts for five flow regimes 
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Local parameters
!
    ! minimum depth voor H* or R* (see documentation) of 0.001 m 
    real(fp), parameter :: hmin = 0.001
!
! Global variables
!
    integer , intent(in)  :: lundia
    integer , intent(in)  :: isw    ! switch for intake/outfall location
    integer               :: itype  ! flow regime (2 to 6)
    real(fp), intent(in)  :: ag     ! acceleration due to gravity
    real(fp), intent(in)  :: alfa   ! energy loss correction coefficient of culvert
    real(fp)              :: area   ! effective area (=htculv * width)
    real(fp), intent(in)  :: cd1    ! energy loss coefficients for the three types 
    real(fp), intent(in)  :: cd2    ! energy loss coefficients for the three types
    real(fp), intent(in)  :: cd3    ! energy loss coefficients for the three types
    real(fp)              :: coefl  ! culvert loss coefficient (cd1, cd2 or cd3)
    real(fp)              :: debiet
    real(fp)              :: hhh
    real(fp), intent(in)  :: htculv ! effective height of culvert
    real(fp), intent(in)  :: leng   ! length of culvert
    real(fp), intent(in)  :: mann   ! Manning's friction coefficient of culvert
    real(fp), intent(in)  :: olddis ! discharge at previous time step
    real(fp), intent(in)  :: positc ! vertical position of culvert
    real(fp)              :: rdis   ! discharge through culvert
    real(fp), intent(in)  :: s0in   ! water elevation at intake
    real(fp), intent(in)  :: s0out  ! water elevation at outfall
    real(fp), intent(in)  :: width  ! width of culvert
!
! Local variables:
!
    integer           :: iexit
    integer           :: iter
    real(fp)          :: eps    
    real(fp)          :: hc     ! critical depth
    real(fp)          :: hster  ! value for wet cross section area
    real(fp)          :: muster ! auxiliary value
    real(fp)          :: rster
    real(fp)          :: value
    real(fp)          :: ztin   ! water level at intake
    real(fp)          :: ztout  ! water level at outfall
!
!! executable statements -------------------------------------------------------
!
    eps   = 0.001
    ztin  = max(0.0_fp, s0in - positc)
    ztout = max(0.0_fp, s0out- positc)
    !
    ! compute h_critical (based on averaged waterlevel):
    !
    debiet = olddis
    do iter=1,10
       hhh    = debiet**2.0 / (ag * width**2.0)
       hc     = hhh**(1.0 / 3.0)
       hster  = min(htculv, 0.5 * ztin + 0.5 * ztout )
       hster  = max(hster, hmin)
       rster  = (hster * width ) / max(hmin, 2.0 * hster + width)
       value  = 1.0 + (2.0 * ag * leng * (mann**2.0) / rster**(4.0 / 3.0) &
              & + alfa) * (cd2**2.0)
       muster = cd2 / max(hmin, sqrt(value) )
       debiet = muster * hster * width * &
              & sqrt(2.0 * ag * max(0.0_fp, ztin - ztout))
    enddo
    itype = -999
    if (ztin/htculv > 1.0 .and. ztout/htculv > 1.0) then
       !
       ! type 4 (submerged flow):
       !
       itype  = 4
       rster  = (htculv * width ) / max(hmin, 2.0 * htculv + 2.0 * width)
       value  = 1.0 + (2.0 * ag * leng * (mann**2.0) / rster**(4.0 / 3.0) + &
              & alfa) * (cd2**2.0)
       muster = cd2 / max(hmin, sqrt(value) )
       rdis   = muster * htculv * width * sqrt(2*ag*(ztin - ztout))
       coefl  = muster
       area   = htculv * width
    elseif (ztin/htculv < 1.5 .and. ztout/htculv <= 1.0) then
       if (ztout <= hc) then
          !
          ! type 2 (supercritical flow):
          !
          itype = 2
          do iter=1,10
             hhh    = debiet**2.0 / (ag * width**2.0)
             hc     = hhh**(1.0 / 3.0)
             hster  = min(htculv, 0.5 * hc + 0.5 * ztin )
             hster  = max(hster, hmin)
             rster  = (hster * width ) / max(hmin, 2.0 * hster + width)
             value  = 1 + (2.0 * ag * leng * (mann**2.0) / rster**(4.0 / 3.0) + &
                    & alfa) * (cd1**2.0) * (hc / max(hmin, hster))**2
             muster = cd1 / max(hmin, sqrt(value) )
             rdis   = muster * hc * width * sqrt(2.0 * ag * (max(hmin, ztin - hc)))
             debiet = rdis
          enddo
          coefl = muster
          area  = hc * width
       elseif (ztout > hc) then
          !
          ! type 3 (tranquil flow):
          !
          itype  = 3
          hster  = min(htculv, 0.5 * ztin + 0.5 * ztout )
          hster  = max(hster, hmin)
          rster  = (hster * width ) / max(hmin, 2.0 * hster + width)
          value  = 1.0 + (2.0 * ag * leng * (mann**2.0) / rster**(4.0/3.0) + &
                 & alfa) * (cd1**2.0) * (ztout / max(hmin,hster))**2.0
          muster = cd1 / max(hmin, sqrt(value) )
          rdis   = muster * ztout * width * &
                 & sqrt(2.0 * ag * (max(hmin, ztin - ztout)))
          coefl  = muster
          area   = ztout * width
       endif
    elseif ((ztin / htculv >= 1.5) .and. (ztout / htculv <= 1.0)) then
       if (ztout <= hc) then
          !
          ! type 5 (rapid flow at inlet):
          !
          itype = 5
          rdis  = cd3 * htculv * width * sqrt(2.0 * ag * ztin)
          coefl = cd3
          area  = htculv * width
       elseif (ztout > hc) then
          !
          ! type 6 (full flow free outlet):
          !
          itype  = 6
          rster  = (htculv * width ) / max(hmin, 2.0 * htculv + 2.0 * width)
          value  = 1 + (2.0 * ag * leng * (mann**2.0) / rster**(4.0 / 3.0) + &
                 & alfa) * (cd2**2.0)
          muster = cd2 / max(hmin, sqrt(value) )
          rdis   = muster * htculv * width * sqrt(2.0 * ag * (ztin - htculv))
          coefl  = cd2
          area   = htculv * width
       endif
    else
       write (lundia,*) 'none of the conditions is satisfied'
       write (lundia,*) 'ERROR'
       call d3stop(iexit     ,gdp       )      
    endif
    if (isw == 1) rdis = -rdis
end subroutine cptdis
