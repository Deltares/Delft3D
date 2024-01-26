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

 subroutine iadvecini()
 use m_flowgeom
 use m_flow
 use unstruc_messages
 implicit none
 integer :: L, jado

 jado = 0
 if (jado == 1) then
 if (cflmx > 0.9d0 )  then
    if (iadvec == 3) then
        iadvec = 5
    else if (iadvec == 4) then
        iadvec = 6
    else
        iadvec = 5
    endif
    call mess(LEVEL_INFO, 'CFLMax > 0.9, Advectype switched to semi implicit Piaczek&Williams ')
 else if (cflmx < 0.71d0) then
    if (iadvec == 5) then
        iadvec = 3
        call mess(LEVEL_INFO, 'CFLMax < 0.71 Advectype switched to explicit ')
    else if (iadvec == 6) then
        iadvec = 4
        call mess(LEVEL_INFO, 'CFLMax < 0.71 Advectype switched to explicit ')
    endif
 endif
 endif


 if (kmx > 0 .or. iadvec == 0) iadvec1D = iadvec                  ! for now, same if 3D
 do L = 1,lnx
    if (iadv(L) .ne. -1) then
       iadv(L) = iadvec
       if (L <= Lnx1D) then
          if ( iadvec .ne. 0) iadv(L) = iadvec1D ! voorlopig altijd piacz impl 4 voor 1D
       endif
    endif
 enddo

 end subroutine iadvecini
