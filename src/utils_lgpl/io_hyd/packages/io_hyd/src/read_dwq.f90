!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2025.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
!
!

      subroutine read_dwq(file_dwq, num_columns  , num_rows, ipnt )

      !! read a dwq file and check dimensions

      use m_logger_helper, only : stop_with_error
      use m_waq_file                   ! module contains everything for the files
      implicit none

      ! declaration of the arguments

      type(t_file)                       :: file_dwq               ! aggregation-file
      integer                                :: num_columns                   ! grid cells m direction
      integer                                :: num_rows                   ! grid cells n direction
      integer                                :: ipnt(num_rows,num_columns)        ! aggregation pointer

      ! local declarations

      integer                                :: mmaxd                  ! grid cells m direction from dwq file
      integer                                :: nmaxd                  ! grid cells n direction from dwq file
      integer                                :: nmd                    ! total number of grid cells from dwq file
      integer                                :: ioptdd                 ! dido option ?
      integer                                :: idum                   ! dummy
      integer                                :: n,m                    ! loop counters
      integer                                :: ioerr                  ! error on file

      call file_dwq%open()
      read(file_dwq%unit,*,iostat=ioerr) nmaxd, mmaxd, nmd, ioptdd, idum
      if ( ioerr .ne. 0 ) then
         write(*,*) ' error reading dwq file'
         call stop_with_error()
      endif

!     If nmaxd or mmaxd is one, only check if nmd.ne.num_rows*num_columns
      if (nmaxd.eq.1.or.mmaxd.eq.1) then
        if (nmd.ne.num_rows*num_columns) then
           write(*,*) ' dimensions grid on dido file differ from input hydrodynamics'
           call stop_with_error()
        endif
      else
        if (nmaxd.ne.num_rows.or.mmaxd.ne.num_columns) then
           write(*,*) ' dimensions grid on dido file differ from input hydrodynamics'
           call stop_with_error()
        endif
      endif
      read(file_dwq%unit,*,iostat=ioerr) ((ipnt(n,m),n=1,num_rows),m=1,num_columns)
      if ( ioerr .ne. 0 ) then
         write(*,*) ' error reading dwq file'
         call stop_with_error()
      endif

      return
      end
