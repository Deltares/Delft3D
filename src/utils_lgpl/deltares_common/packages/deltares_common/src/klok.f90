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
module m_wall_clock_time
   implicit none
contains
!> wall clock timer
   subroutine wall_clock_time(t)
   use MessageHandling

   double precision   :: t
   character(len=8)   :: date
   character(len=10)  :: time
   character(len=5)   :: zone
   integer            :: timing(8)

   character(len=128) :: mesg

   integer,          save :: ndays=0
   integer,          save :: dayprev=-999

   call date_and_time(date, time, zone, timing)

!  check for new day
   if ( dayprev == -999 ) then
      dayprev = timing(3)    ! initialization to
   else if ( timing(3) /= dayprev ) then
      ndays = ndays+1
      write(mesg, "('new wall clock day: previous day=', I2, ', new day=', I2)") dayprev, timing(3)
      call mess(LEVEL_INFO, trim(mesg))
      dayprev = timing(3)
   end if

   t = ndays*3600d0*24d0 + timing(5)*3600d0 + timing(6)*60d0 + timing(7) + dble(timing(8))/1000d0


   end subroutine wall_clock_time
end module m_wall_clock_time
