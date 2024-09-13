subroutine deactivateBANKS(hu, aguu, EDGEtypeBANK, zmodel, nmmax, nmlb, nmub, icx)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
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
!  $Id: deactivateBANKS.f90 6265 2016-07-07 13:56:06Z platzek $
!  $HeadURL: 
!!--description-----------------------------------------------------------------
!
!    Function: set hu to negative value (so kfu will be set to zero in checku) 
!              if one does not want to flood floodplains
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
use precision
implicit none
!
    real(fp), dimension(nmlb:nmub)                                      , intent(inout) :: hu
    real(fp), dimension(nmlb:nmub)                                      , intent(in)    :: aguu
    integer, dimension(4,nmlb:nmub)                                     , intent(in)    :: EDGEtypeBANK
    integer                                                             , intent(in)    :: nmlb
    integer                                                             , intent(in)    :: nmub
    integer                                                             , intent(in)    :: icx
    integer                                                             , intent(in)    :: nmmax
    logical                                                             , intent(in)    :: Zmodel
!
!   local variables
!
    integer  :: k
    integer  :: kAD
    integer  :: nm
    integer  :: nmu
!
!   executable statements
!
    if (zmodel) then
       write(*,*) 'should be correct, but check if aguu has to be used to set hu to zero'
    endif
    if (icx/=1) then !along x
       k   = 2
       kAD = 4 
    else !along y
       k = 3
       kAD = 1
    endif
    !    
    do nm = 1,nmmax
       nmu = nm + icx
       !  if (abs(EDGEtypeBANK(k,nm))==2.and.abs(EDGEtypeBANK(kAD,nmu))==1.or.&
       !      abs(EDGEtypeBANK(k,nm))==1.and.abs(EDGEtypeBANK(kAD,nmu))==2.or.&
       !      abs(EDGEtypeBANK(k,nm))==2.and.abs(EDGEtypeBANK(kAD,nmu))==2) then ! i exclude both 1 (both cut)
       if (abs(EDGEtypeBANK(k,nm))==2.or.abs(EDGEtypeBANK(kAD,nmu))==2.or.comparereal(aguu(nm),0._fp)==0) then
           !
           ! AGUU HAS TO BE REMOVED AND REPLACED BY AGUU_BANK. EDGEtypeBANK not needed.it is needed because you can have the 
           ! the case of two cut edges starting at the opposite vertex and giving aguu=0. It happened. 
           ! Maybe this subroutine can be removed and the check done for dpu, making sure its compueted with dpH when aguu is zero.
           ! this will become if (comparereal(aguu_bank,0._fp)==0) then
           !
           !if (hu(nm).gt.0._fp.and.comparereal(aguu(nm),0._fp)==0) then
           hu(nm) =-10.0_fp !random negative value
           !endif
       endif
    enddo
    !
end subroutine deactivateBANKS
