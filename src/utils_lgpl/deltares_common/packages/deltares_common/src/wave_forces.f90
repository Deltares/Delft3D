!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------
subroutine wave_forces(    dir       ,tp        ,fxhis                , &
                         & fyhis     ,dish      ,diss      ,wavel     , &
                         & water_is_too_shallow_or_waves_are_too_small, &
                         & fx        ,fy                              , &
                         & swdis     ,grav      ,wsbodyu   , wsbodyv   )
    !!--description-----------------------------------------------------------------
    !
    !     Input:
    !     -------
    !     DIR,TP,FXHIS,FYHIS,DISH,DISS,WAVEL,water_is_too_shallow_or_waves_are_too_small,SWDIS,grav
    !     water_is_too_shallow_or_waves_are_too_small   : logical variable, .true. when depth or waveheight is too small
    !     SWDIS  : 1: wave forces based on gradients,
    !              2: from total dissipation,
    !              3: using the 3D dissipation profile
    !
    !     Output:
    !     --------
    !     FX,FY,wsbodyu,wsbodyv
    !     FX,FY : wave forces based on gradients(1) or total dissipation(2) or 3d dissipation(3)
    !             Unit: N/m2
    !
    !!--pseudo code and references--------------------------------------------------
    ! NONE
    !!--declarations----------------------------------------------------------------
    implicit none
    !
    ! Global variables
    !
    logical, intent(in   )  :: water_is_too_shallow_or_waves_are_too_small
    integer, intent(in   )  :: swdis
    real   , intent(in   )  :: dir
    real   , intent(in   )  :: dish
    real   , intent(in   )  :: diss
    real   , intent(  out)  :: fx
    real   , intent(in   )  :: fxhis
    real   , intent(  out)  :: fy
    real   , intent(in   )  :: fyhis
    real   , intent(in   )  :: grav
    real   , intent(in   )  :: tp
    real   , intent(in   )  :: wavel
    real   , intent(  out)  :: wsbodyu
    real   , intent(  out)  :: wsbodyv
    !
    ! Local variables
    !
    real    :: frc
    real    :: tr_angle
    !
    !! executable statements -------------------------------------------------------
    !
    if (water_is_too_shallow_or_waves_are_too_small) then
        fx    = 0.0
        fy    = 0.0
    else
        if (swdis == 1) then
            !
            ! Determine wave forces based on gradients radiation stresses
            !
            fx      = fxhis
            fy      = fyhis
            wsbodyu = 0.0
            wsbodyv = 0.0
        elseif (swdis == 2) then
            !
            ! Determine wave forces based on dissipation
            !
            frc      = dish*tp/wavel
            !
            tr_angle = 0.0174533*dir
            fx       = cos(tr_angle)*frc
            fy       = sin(tr_angle)*frc
            wsbodyu  = 0.0
            wsbodyv  = 0.0
        elseif (swdis == 3) then
            !
            ! Determine wave forces based on dissipation at the free surface
            ! and a separate contribution to the water column through the
            ! remaining forces (wsbodyu/wsbodyv)
            !
            frc      = diss*tp/wavel
            !
            tr_angle = 0.0174533*dir
            fx       = cos(tr_angle)*frc
            fy       = sin(tr_angle)*frc
            wsbodyu = fxhis - fx
            wsbodyv = fyhis - fy
        else
        endif
    endif
end subroutine wave_forces