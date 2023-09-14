subroutine wapar(hrm       ,dir       ,deph      ,tp        ,fxhis     , &
               & fyhis     ,dish      ,diss      ,wavel     ,wavek     , &
               & ldep      ,fx        ,fy        ,qbsli     ,dismax    , &
               & corht     ,swdis     ,grav      ,wsbodyu   ,wsbodyv   )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2023.                                
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
!     Input:
!     -------
!     Hrm,DIR,DEPH,TP,FXHIS,FYHIS,DISH,DISS,QBH,WAVEL,WAVEK,LDEP,SWDIS
!     LDEP   : logical variable, .true. when depth or waveheight is too small
!     SWDIS  : 1: wave forces based on gradients, 
!              2: from total dissipation,
!              3: using the 3D dissipation profile
!
!     Output:
!     --------
!     FX,FY,QBSLI
!     FX,FY : wave forces based on gradients(1) or total dissipation(2) or 3d dissipation(3)
!             Unit: N/m2
!     QBSLI : 'actual' fraction breaking waves, Unit [-]
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    logical, intent(in)            :: corht
    logical, intent(in)            :: ldep
    integer, intent(in)            :: swdis
    real   , intent(in)            :: deph
    real   , intent(in)            :: dir
    real   , intent(in)            :: dish
    real   , intent(in)            :: diss
    real   , intent(in)            :: dismax
    real                           :: fx
    real   , intent(in)            :: fxhis
    real                           :: fy
    real   , intent(in)            :: fyhis
    real                           :: grav
    real   , intent(in)            :: hrm
    real   , intent(out)           :: qbsli
    real   , intent(in)            :: tp
    real   , intent(in)            :: wavek
    real   , intent(in)            :: wavel
    real                           :: wsbodyu
    real                           :: wsbodyv
!
! Local variables
!
    integer :: choice
    real    :: factor
    real    :: fmax
    real    :: frc
    real    :: ftot
    real    :: tr_angle
!
!! executable statements -------------------------------------------------------
!
    choice = 0
    if (corht) then
       choice = 1
    endif
    if (ldep) then
       fx    = 0.0
       fy    = 0.0
       qbsli = 0.0
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
          if (choice == 1) then
             ftot = fx*fx + fy*fy
             if (ftot > 0.0) then
                ftot   = sqrt(ftot)
                fmax   = dismax/sqrt(grav*deph)
                factor = min(fmax/ftot, 1.E0)
                fx     = factor*fx
                fy     = factor*fy
             endif
          endif
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
          if (choice == 1) then
             ftot = fx*fx + fy*fy
             if (ftot > 0.0) then
                ftot   = sqrt(ftot)
                fmax   = dismax/sqrt(grav*deph)
                factor = min(fmax/ftot, 1.E0)
                fx     = factor*fx
                fy     = factor*fy
             endif
          endif
          wsbodyu = fxhis - fx
          wsbodyv = fyhis - fy
       else
       endif
    endif
end subroutine wapar
