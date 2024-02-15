!----- AGPL --------------------------------------------------------------------
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
! 
! 

module m_restart_debug

 implicit none

 double precision, pointer      :: r_bodsed(:,:)   !< [kg m-2] Available sediment in the bed in flow cell center.            {"location": "face", "shape": ["stmpar%morlyr%settings%nfrac", "ndx"], "internal": "stmpar%morlyr%state%bodsed"}
 double precision, pointer      :: r_dpsed(:)      !< [m] Sediment thickness in the bed in flow cell center.                 {"location": "face", "shape": ["ndx"], "internal": "stmpar%morlyr%state%dpsed"}
 double precision, pointer      :: r_msed(:,:,:)   !< [kg m-2] Available sediment in a layer of the bed in flow cell center. {"location": "face", "shape": ["stmpar%morlyr%settings%nfrac", "stmpar%morlyr%settings%nlyr", "ndx"], "internal": "stmpar%morlyr%state%msed"}
 double precision, pointer      :: r_thlyr(:,:)    !< [m] Thickness of a layer of the bed in flow cell center.               {"location": "face", "shape": ["stmpar%morlyr%settings%nlyr","ndx"], "internal": "stmpar%morlyr%state%thlyr"}

 contains

 subroutine ini_m_restart_debug()
  use m_sediment
  implicit none
  if (stm_included) then 
    r_bodsed => stmpar%morlyr%STATE%bodsed
    r_dpsed => stmpar%morlyr%STATE%dpsed
    r_msed => stmpar%morlyr%STATE%msed
    r_thlyr => stmpar%morlyr%STATE%thlyr
  endif  
 end subroutine ini_m_restart_debug

end module m_restart_debug
