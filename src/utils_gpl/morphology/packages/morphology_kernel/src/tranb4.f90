subroutine tranb4(utot      ,d         ,chezy     ,npar      ,par       , &
                & hidexp    ,sbot      ,ssus      )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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
! computes sediment transport according to
! general formula
! -
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
!
! Arguments
!
    integer                  , intent(in)    :: npar   !< length of par array
    real(fp)                 , intent(in)    :: chezy  !< Chezy value
    real(fp)                 , intent(in)    :: d      !< grain diameter
    real(fp)                 , intent(in)    :: hidexp !< hiding & exposure factor
    real(fp), dimension(npar), intent(in)    :: par    !< sediment transport formula parameters
    real(fp)                 , intent(in)    :: utot   !< depth averaged velocity magnitude
    !
    real(fp)                 , intent(out)   :: sbot   !< bedload transport rate
    real(fp)                 , intent(out)   :: ssus   !< suspended transport rate
!
! Local variables
!
    real(fp) :: acal  ! calibration factor for bedload
    real(fp) :: acals ! calibration factor for suspended load
    real(fp) :: ag    ! gravity acceleration
    real(fp) :: b     ! exponent of Shields number for bedload
    real(fp) :: bs    ! exponent of Shields number for suspended load
    real(fp) :: cc    ! exponent of excess Shields number for bedload
    real(fp) :: ccs   ! exponent of excess Shields number for suspended load
    real(fp) :: delta ! relative density of sediment particle
    real(fp) :: f     ! help variable for excess Shields number
    real(fp) :: rmu   ! ripple factor for bedload
    real(fp) :: rmus  ! ripple factor for suspended load
    real(fp) :: th    ! Shields number
    real(fp) :: thcr  ! critical Shields number for bedload
    real(fp) :: thcrs ! critical Shields number for suspended load
!
!! executable statements -------------------------------------------------------
!
    sbot  = 0.0
    ssus  = 0.0
    !
    ag    = par( 1)
    delta = par( 4)
    acal  = par(11)
    b     = par(12)
    cc    = par(13)
    rmu   = par(14)
    thcr  = par(15)
    acals = par(16)
    bs    = par(17)
    ccs   = par(18)
    rmus  = par(19)
    thcrs = par(20)
    !
    if ((chezy<1.0e-6) .or. (utot<1.0e-6)) then
       return
    endif
    th = (utot/chezy)**2/(delta*d)
    !
    f  = rmu*th - hidexp*thcr
    if (f>1.0e-8) sbot = acal * d**1.5 * sqrt(ag*delta) * th**b * f**cc
    !
    f  = rmus*th - hidexp*thcrs
    if (f>1.0e-8) ssus = acals * d**1.5 * sqrt(ag*delta) * th**bs * f**ccs
end subroutine tranb4
