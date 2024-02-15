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

 !> Calculate absolute date time values, given a time in seconds since refdat.
 !! \see seconds_to_datetimestring
 subroutine datetime_from_refdat(timsec, refdat, iyear, imonth, iday, ihour, imin, isec)
 use m_julday
 implicit none
 double precision,  intent(in)  :: timsec                                 !< Time in seconds since refdate
 character (len=8), intent(in)  :: refdat                                 !< reference date
 integer,           intent(out) :: iyear, imonth, iday, ihour, imin, isec !< Actual date, split up in year/month, etc.

 integer :: jul, jul0, iyear0, imonth0, iday0
 double precision :: tnr, tsec
 integer :: ndag


 read(refdat(1:4),*) iyear0
 read(refdat(5:6),*) imonth0
 read(refdat(7:8),*) iday0

 jul0  = julday(imonth0,iday0,iyear0)
 tnr   = timsec / 3600d0
 ndag  = tnr / 24d0

 call caldat(jul0+ndag,imonth,iday,iyear) 

 tsec  =  timsec - ndag*24d0*3600d0
 ihour =   tsec/3600d0
 imin  =  (tsec - ihour*3600d0)/60d0
 isec  =  nint(tsec - ihour*3600d0 - imin*60d0)

 end subroutine datetime_from_refdat
