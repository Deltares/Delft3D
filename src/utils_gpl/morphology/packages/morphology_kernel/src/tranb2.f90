subroutine tranb2(utot      ,d50       ,d90       ,chezy     ,h         , &
                & npar      ,par       ,hidexp    ,sbot      ,ssus      )
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
! meyer-peter-muller (comor/rivcom)
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
    real(fp)                 , intent(in)    :: chezy
    real(fp)                 , intent(in)    :: d50
    real(fp)                 , intent(in)    :: d90
    real(fp)                 , intent(in)    :: h
    real(fp)                 , intent(in)    :: hidexp !< hiding & exposure factor
    real(fp), dimension(npar), intent(in)    :: par
    real(fp)                 , intent(in)    :: utot
    !
    real(fp)                 , intent(out)   :: sbot
    real(fp)                 , intent(out)   :: ssus
!
! Local variables
!
    real(fp) :: acal
    real(fp) :: ag     !       gravity acceleration
    real(fp) :: amurs
    real(fp) :: cgrain
    real(fp) :: delta  !   relative density of sediment particle
    real(fp) :: y
!
!! executable statements -------------------------------------------------------
!
    sbot  = 0.0_fp
    ssus  = 0.0_fp
    !
    ag    = par(1)
    delta = par(4)
    acal  = par(11)
    !
    !     bed load transport
    !
    if (chezy < 1.0e-6) then
       return
    endif
    if ((12.0*h/d90) < 1.0e-6) then
       amurs = 1.0_fp
    else
       cgrain = 18.0 * log10(12.0*h/d90)
       amurs  = (cgrain/chezy)**1.5
       amurs  = max(amurs, 1.0_fp)
    endif
    y = (utot/chezy)**2 / amurs / delta / d50
    y = max(y - hidexp * 0.047, 0.0_fp)
    if (y < 1.0e-6) then
       return
    endif
    sbot = acal*8.0*sqrt(ag*delta*d50*y)*d50*y
    ssus = 0.0_fp
end subroutine tranb2
