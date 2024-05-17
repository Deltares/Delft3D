!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
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

      subroutine read_lga(file_lga, mmax  , nmax  , nolay , nosegl, &
                          noq1    , noq2  , noq3  , lgrid )

      ! read a lga file and check dimensions
      use m_logger_helper, only : stop_with_error, get_log_unit_number

      use m_waq_file                   ! module contains everything for the files
      implicit none

      ! declaration of the arguments

      type(t_file)                       :: file_lga               ! aggregation-file
      integer                                :: mmax                   ! grid cells m direction
      integer                                :: nmax                   ! grid cells n direction
      integer                                :: nolay                  ! nolay
      integer                                :: nosegl                 ! nosegl
      integer                                :: noq1                   ! noq1
      integer                                :: noq2                   ! noq2
      integer                                :: noq3                   ! noq3
      integer                                :: lgrid(nmax,mmax)       ! active grid table

      ! local declarations

      integer                                :: mmaxd                  ! grid cells m direction from lga file
      integer                                :: nmaxd                  ! grid cells n direction from lga file
      integer                                :: ioerr                  ! error on file
      integer                                :: m                      ! loop counter
      integer                                :: n                      ! loop counter
      integer                                :: lunrep                 ! unit number report file

      call get_log_unit_number(lunrep)

      call file_lga%open()
      read(file_lga%unit,iostat=ioerr) nmaxd, mmaxd, nosegl, nolay, noq1, noq2, noq3
      if ( ioerr .ne. 0 ) then
         write(lunrep,*) ' error reading lga file'
         call stop_with_error()
      endif

      if ( nmaxd.ne.nmax .or. mmaxd.ne.mmax ) then
         write(lunrep,*) ' dimensions lga file differ from input hydrodynamics'
         write(lunrep,*) ' mmax hyd file:',mmax
         write(lunrep,*) ' nmax hyd file:',nmax
         write(lunrep,*) ' mmax lga file:',mmaxd
         write(lunrep,*) ' nmax lga file:',nmaxd
         call stop_with_error()
      endif

      read(file_lga%unit,iostat=ioerr) ((lgrid(n,m),n=1,nmax),m=1,mmax)
      if ( ioerr .ne. 0 ) then
         write(lunrep,*) ' error reading lga file'
         call stop_with_error()
      endif

      return
      end
