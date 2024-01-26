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

 subroutine initialfield2Dto3D( v2D, v3D, tr13, tr14 )
 use m_flowgeom
 use m_flow
 use m_missing
 use timespace

 implicit none

 double precision, intent(inout) :: v2D(*) , v3D(*)
 double precision, intent(in   ) :: tr13, tr14
 double precision                :: zb, zt, zz
 integer                         :: n, k, kb, kt
 !character(len=1), intent(in)    :: operand !< Operand type, valid values: 'O', 'A', '+', '*', 'X', 'N'.

 zb = -1d9 ; if (tr13 .ne. dmiss) zb = tr13
 zt =  1d9 ; if (tr14 .ne. dmiss) zt = tr14
 do n = 1,ndx
    if ( v2D(n) .ne. dmiss ) then
       if (kmx == 0) then
          call operate(v3D(n), v2D(n), operand)
       else
          kb = kbot(n) ; kt = ktop(n)
          do k = kb, kt
             zz = 0.5d0*( zws(k) + zws(k-1) )
             if (zz > zb .and. zz < zt ) then
                call operate(v3D(k), v2D(n), operand)
             endif
          enddo
       endif
    endif
 enddo
 end subroutine initialfield2Dto3D
