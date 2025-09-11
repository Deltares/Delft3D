!!  Copyright (C)  Stichting Deltares, 2012-2025.
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

      subroutine dattim(rundat)
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! RUNDAT  IO CH*20                 Current date and time containing a
!                                  combination of DATE and TIME
!                                  format: 'yyyy/mm/dd hh:mm:ss '
!-----------------------------------------------------------------------
!
! declarations and specifications
!
      character(len=20) rundat
      character(len=8)  date
      character(len=10) time
      character(len=5)  zone
      integer      values(8)

      call date_and_time (date,time,zone,values)
      write (rundat,1000) values(1),values(2),values(3),values(5),values(6),values(7)
 1000 format ( i4.4,'/',i2.2,'/',i2.2,' ',i2.2,':',i2.2,':',i2.2)

      return
      end
