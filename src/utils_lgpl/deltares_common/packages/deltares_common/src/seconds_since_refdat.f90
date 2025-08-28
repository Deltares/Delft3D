!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2025.                                
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

 !> Calculates the relative time in seconds since refdat, given an absolute datetime.
 !! The input datetime is in separate year/month/../seconds values.
 !! \see datetimestring_to_seconds
 subroutine seconds_since_refdat(iyear, imonth, iday, ihour, imin, isec, refdat, timsec)
  use m_julday
 implicit none
 integer,          intent(in)  :: iyear, iday, imonth, ihour, imin, isec !< Input absolute date time components
 character (len=8), intent(in) :: refdat                                 !< reference date
 double precision, intent(out) :: timsec                                 !< Output seconds since refdate for the specified input datetime.

 integer :: jul, jul0, iyear0, imonth0, iday0

     read(refdat(1:4),*) iyear0
     read(refdat(5:6),*) imonth0
     read(refdat(7:8),*) iday0

     jul0 = julday(imonth0,iday0,iyear0)
     jul  = julday(imonth ,iday ,iyear )

     timsec = (jul - jul0)*24d0*3600d0      + ihour*3600d0      + imin*60d0 + isec
 end subroutine seconds_since_refdat
