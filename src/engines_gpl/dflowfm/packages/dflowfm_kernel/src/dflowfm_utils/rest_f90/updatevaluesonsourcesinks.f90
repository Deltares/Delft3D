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

subroutine updateValuesOnSourceSinks(tim1)
use fm_external_forcings_data, only: qsrc, qsrcavg, vsrccum, vsrccum_pre, numsrc
use m_missing
use m_flowtimes, only: ti_his, time_his
use precision
use m_flowparameters, only: eps10
use m_alloc
implicit none
   double precision, intent(in) :: tim1 !< Current (new) time

   double precision,                 save        :: timprev = -1d0 ! TODO: save is unsafe, replace by using time1 and time0, also two other occurrences
   double precision                              :: timstep
   integer                                       :: i

   if (timprev < 0d0) then
      ! This realloc should not be needed
      call reallocsrc(numsrc)
   else
      timstep = tim1 - timprev
      ! cumulative volume from Tstart
      do i = 1, numsrc
         vsrccum(i) =vsrccum(i) + timstep*qsrc(i)
      enddo

      if (comparereal(tim1, time_his, eps10)== 0) then
         do i = 1, numsrc
            qsrcavg(i) = (vsrccum(i) - vsrccum_pre(i)) / ti_his ! average discharge in the past His-interval
            vsrccum_pre(i) = vsrccum(i)
         enddo
      endif
   end if

   timprev = tim1
end subroutine updateValuesOnSourceSinks
