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

subroutine updateBalance()
   use m_flow
   use m_partitioninfo
   implicit none

   integer :: ivar

   if (jampi == 1) then
      call reduce_bal(cumvolcur, MAX_IDX)
!     only need to reduce the first two entries of volcur, but we do the reduce for the whole array here
      call reduce_bal(volcur,    MAX_IDX)
   endif
   do ivar = 1,MAX_IDX
      if (ivar == IDX_STOR) then
         voltot(IDX_STOR)    = volcur(IDX_STOR) - vol1ini
      else if (ivar == IDX_VOLTOT) then
         voltot(IDX_VOLTOT)  = volcur(IDX_VOLTOT)
      else if (ivar == IDX_ICEPT) then
         voltot(IDX_ICEPT)  = volcur(IDX_ICEPT)
      else
         ! All other variables are simply cumlative total in time:
         voltot(ivar)  = voltot(ivar)  + cumvolcur(ivar)
      end if
   end do

   cumvolcur = 0d0
end subroutine updateBalance
