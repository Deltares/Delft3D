subroutine tranb3(utot      ,d35       ,c         ,h         ,npar      , &
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
! swanby (ackers white)
! -
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Arguments
!
    integer                  , intent(in)    :: npar
    real(fp)                 , intent(in)    :: c
    real(fp)                 , intent(in)    :: d35
    real(fp)                 , intent(in)    :: h
    real(fp), dimension(npar), intent(in)    :: par
    real(fp)                 , intent(in)    :: utot
    !
    real(fp)                 , intent(out)   :: sbot
    real(fp)                 , intent(out)   :: ssus
!
!
! Local variables
!
    real(fp)                       :: a
    real(fp)                       :: acal
    real(fp)                       :: ag                   ! gravity acceleration
    real(fp)                       :: cc
    real(fp)                       :: ccc
    real(fp)                       :: cd
    real(fp)                       :: cf
    real(fp)                       :: delta                ! relative density of sediment particle
    real(fp)                       :: dgr
    real(fp)                       :: dp                   ! depth value at depth points
    real(fp)                       :: f                    ! real help array
    real(fp)                       :: fwc
    real(fp)                       :: rk
    real(fp)                       :: rm
    real(fp)                       :: rn
    real(fp)                       :: uster
!
!
!! executable statements -------------------------------------------------------
!
    sbot = 0.0
    ssus = 0.0
    !
    ag = par(1)
    delta = par(4)
    acal = par(11)
    rk = par(12)
    !
    if ((utot<1.E-6) .or. (h<.001)) then
       return
    endif
    if (c<1.E-6) then
       cc = 18.*log10(12.*h/rk)
    else
       cc = c
    endif
    cf = ag/cc/cc
    dp = d35
    dgr = 25300*dp
    rn = 1.0 - .2432*log(dgr)
    rm = 9.66/dgr + 1.34
    a = .23/sqrt(dgr) + .14
    ccc = log(dgr)
    ccc = exp(2.86*ccc - .4343*ccc*ccc - 8.128)
    cd = 18.*log10(12.*h/dp)
    uster = sqrt(cf)*utot
    f = utot**(1. - rn)*uster**rn/cd**(1. - rn)/ag**(rn/2.)/sqrt(delta*dp)
    fwc = (f - a)/a
    if (fwc>0.) sbot = acal*utot*dp*(utot/uster)**rn*ccc*fwc**rm
    ssus = 0.0
end subroutine tranb3
