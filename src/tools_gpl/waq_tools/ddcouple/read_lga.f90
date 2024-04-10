!!  Copyright (C)  Stichting Deltares, 2021-2024.
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

      subroutine read_lga(file_lga, mmax  , nmax  , nolay , nosegl, &
                          noq1    , noq2  , noq3  , lgrid )

      ! function : read a lga file and check dimensions

      ! global declarations

      use m_srstop
      use m_monsys
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

      call getmlu(lunrep)

      call file_lga%open()
      read(file_lga%unit,iostat=ioerr) nmaxd, mmaxd, nosegl, nolay, noq1, noq2, noq3
      if ( ioerr .ne. 0 ) then
         write(lunrep,*) ' error reading lga file'
         call srstop(1)
      endif

      if ( nmaxd.ne.nmax .or. mmaxd.ne.mmax ) then
         write(lunrep,*) ' dimensions lga file differ from input hydrodynamics'
         call srstop(1)
      endif

      read(file_lga%unit,iostat=ioerr) ((lgrid(n,m),n=1,nmax),m=1,mmax)
      if ( ioerr .ne. 0 ) then
         write(lunrep,*) ' error reading lga file'
         call srstop(1)
      endif

      close(file_lga%unit)
      file_lga%status = FILE_STAT_UNOPENED

      return
      end
