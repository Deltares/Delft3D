!!  Copyright (C)  Stichting Deltares, 2012-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
module m_report_date_time

implicit none

contains


      subroutine report_date_time(lun)
!
!     reports actual date and time in file (lun)
!
      use timers

      implicit none
      integer, intent(in) :: lun
      character(len= 8)   :: datum
      character(len=10)   :: tijd
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "report_date_time", ithndl )

      call date_and_time (date=datum,time=tijd)
!
      write(lun,'(//20(a))')       &
       '  Actual date/time = ',datum(1:4),'/',datum(5:6),'/',datum(7:8), &
       '  ',tijd(1:2),':',tijd(3:4),':',tijd(5:6)
!
      if ( timon ) call timstop ( ithndl )
      end subroutine report_date_time

end module m_report_date_time
