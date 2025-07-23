subroutine trab14(utot      ,d50       ,chezy     ,npar      ,par       , &
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
!
! computes sediment transport according to
! generalized version of Ashida and Michiue
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
!
! Arguments
!
    integer                  , intent(in)    :: npar
    real(fp)                 , intent(in)    :: chezy  ! chezy value
    real(fp)                 , intent(in)    :: d50    ! Grain size specified as d50
    real(fp)                 , intent(in)    :: hidexp ! hiding & exposure factor
    real(fp), dimension(npar), intent(in)    :: par    ! sediment parameter list
    real(fp)                 , intent(in)    :: utot   ! flow velocity
    !
    real(fp)                 , intent(out)   :: sbot   ! bed load transport
    real(fp)                 , intent(out)   :: ssus   ! suspended sediment transport
!
! Local variables
!
    real(fp) :: a
    real(fp) :: ag
    real(fp) :: delta  ! relative density of sediment particle
    real(fp) :: m
    real(fp) :: p
    real(fp) :: q
    real(fp) :: sag
    real(fp) :: sgd
    real(fp) :: ssgd3
    real(fp) :: t
    real(fp) :: tc
    real(fp) :: tct
    real(fp) :: ustar
!
!! executable statements -------------------------------------------------------
!
    sbot  = 0.0
    ssus  = 0.0
    !
    ag    = par(1)
    delta = par(4)      ! (rhosol - rhowat) / rhowat
    a     = par(11)     ! acal: tuning constant of sediment transport
    tc    = par(12)
    m     = par(13)
    p     = par(14)
    q     = par(15)
    !
    sag   = sqrt(ag)
    sgd   = delta*ag*d50
    ssgd3 = sqrt(sgd*d50*d50)
    if (chezy >= 1.e-6) then
       if (utot >= 1.e-6) then
          ustar = sag*utot/chezy
          t     = ustar**2/sgd
          tct   = hidexp*tc/t
          if (tct < 1.0) then
             sbot = a * ssgd3 * t**m * (1.0 - tct)**p * (1.0 - sqrt(tct))**q
          endif
       endif
    endif
end subroutine trab14
