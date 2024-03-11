!> This routine implements ASMITA equilibrium relation.
subroutine asmita(zb, timhr, npar, par, sbot, cesus, t_relax)
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

!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Arguments
!
    integer                  , intent(in)  :: npar    !< length of transport parameter array
    real(fp)                 , intent(in)  :: timhr   !< time since reference date [h]
    real(fp)                 , intent(in)  :: zb      !< bed level [m]
    real(fp), dimension(npar), intent(in)  :: par     !< transport parameter array
    !
    real(fp)                 , intent(out) :: sbot        !< bed load transport [kg/s]
    real(fp)                 , intent(out) :: cesus       !< equilibrium suspended concentration [kg/m3]
    real(fp)                 , intent(out) :: t_relax     !< relaxation time scale [s]
!
! static variable
!
    logical                  , save        :: first = .true.                !< flag indicating whether asmita.wlt has been checked
!
! Local variables
!
    logical                                :: exist                        !< flag indicating whether file exists
    real(fp)                               :: cequi                        !< user-specified equilibrium concentration at equilibrium water depth [kg/m3]
    real(fp)                               :: h                            !< representative water depth [m]
    real(fp)                               :: hequi                        !< user-specified equilibrium water depth [m]
    real(fp)                               :: hmin                         !< minimum water depth-specified equilibrium water depth [m]
    real(fp)                               :: maxhh                        !< maximum value of ration of equilibrium and current water depth [-] Value should be larger than 1.
    real(fp)                               :: n                            !< transport power [-]
    real(fp)                               :: reflevel                     !< reference water level [m] for ASMITA depth definition
!
!! executable statements -------------------------------------------------------
!
    cequi    = par(11)
    hequi    = par(12)
    n        = par(13)
    maxhh    = par(14)
    reflevel = par(15)
    !
    if (first) then
       inquire (file = 'asmita.wlt', exist = exist)
       if (exist) then
          write (*, '(A)') 'Obsolete asmita.wlt file found; please use RefLevel keyword.'
          call throwexception()
       endif
       first = .false.
    endif
    !
    ! bed load
    !
    sbot  = 0.0_fp
    !
    ! equilibrium suspended concentration
    !
    if (hequi > 0.0_fp) then
        ! negative depth h may occur since we define the depth here as the difference between the
        ! reference level and the current bed level. Make sure that depth h used is always bigger
        ! than the (positive) equilibrium depth divided by maxhh such that hequi / h is limited
        ! to values less or equal to maxhh.
        !
        hmin = hequi / maxhh
        h = max(hmin, reflevel - zb)
        !
        cesus = cequi * (hequi / h)**n
        t_relax = 1.0_fp
    else
        ! negative equilibrium depth may occur if part of the model is above the reference level
        ! if so, set the equilibrium concentration to 0 and set t_relax to a huge value to switch
        ! off erosion and sedimentation.
        cesus = 0.0_fp
        t_relax = 1.0e10_fp
    endif
end subroutine asmita
