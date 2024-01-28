subroutine tranb1(utot      ,d50       ,c         ,h         ,npar       , &
                & par       ,sbot      ,ssus      )
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
! computes sediment transport according to
! engelund hansen
! -
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Arguments
!
    integer                  , intent(in)  :: npar    !< length of transport parameter array
    real(fp)                 , intent(in)  :: c       !< Chezy value
    real(fp)                 , intent(in)  :: d50     !< mean diameter
    real(fp)                 , intent(in)  :: h       !< water depth
    real(fp), dimension(npar), intent(in)  :: par     !< transport parameter array
    real(fp)                 , intent(in)  :: utot    !< velocity magnitude
    !
    real(fp)                 , intent(out) :: sbot    !< bed load transport
    real(fp)                 , intent(out) :: ssus    !< suspended load transport
!
! Local variables
! 
    real(fp)   :: acal    ! user-specified calibration coefficient
    real(fp)   :: ag      ! gravity acceleration
    real(fp)   :: delta   ! relative density of sediment particle
    real(fp)   :: suspfac ! user-specified suspended sediment factor
    real(fp)   :: temp    ! total transport (not yet split into bedload and suspended load)
    real(fp)   :: th      ! Shields number
!
!! executable statements -------------------------------------------------------
!
    sbot  = 0.0_fp
    ssus  = 0.0_fp
    !
    ag      = par(1)
    delta   = par(4)
    acal    = par(11)
    !rk      = par(12) ! obsolete
    suspfac = par(13)
    !
    ! bed load
    !
    th = (utot/c)**2 / (delta * d50)
    temp  = 0.05_fp * acal * (c**2 / ag) * d50**1.5_fp * sqrt(ag * delta) * th**2.5_fp
    sbot  = (1.0_fp-suspfac) * temp
    !
    ! suspended sediment transport
    !
    ssus = suspfac * temp
end subroutine tranb1
