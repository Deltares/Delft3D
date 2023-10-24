!!  Copyright (C)  Stichting Deltares, 2021-2023.
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

      subroutine read_srf(file_srf, mmax  , nmax  , nosegl, surf )

      ! function : read a srf file and check dimensions

      ! global declarations

      use m_srstop
      use m_monsys
      use filmod                   ! module contains everything for the files
      implicit none

      ! declaration of the arguments

      type(t_dlwqfile)                       :: file_srf               ! aggregation-file
      integer                                :: mmax                   ! grid cells m direction
      integer                                :: nmax                   ! grid cells n direction
      integer                                :: nosegl                 ! nosegl
      real                                   :: surf(nosegl)           ! property of the cells per layer

      ! local declarations

      integer                                :: mmaxd                  ! grid cells m direction from srf file
      integer                                :: nmaxd                  ! grid cells n direction from srf file
      integer                                :: i3,i4,i5,i6            ! nosegl from srf file
      integer                                :: ioerr                  ! error on file
      integer                                :: i                      ! loop counter
      integer                                :: lunrep                 ! unit number report file

      call getmlu(lunrep)

      call dlwqfile_open(file_srf)
      read(file_srf%unit_nr,iostat=ioerr) nmaxd, mmaxd, i3, i4, i5, i6
      if ( ioerr .ne. 0 ) then
         write(lunrep,*) ' error reading srf file'
         call srstop(1)
      endif

      if ( nmaxd.ne.nmax .or. mmaxd.ne.mmax ) then
         write(lunrep,*) ' dimensions srf file differ from input hydrodynamics'
         call srstop(1)
      endif

      read(file_srf%unit_nr,iostat=ioerr) (surf(i),i=1,nosegl)
      if ( ioerr .ne. 0 ) then
         write(lunrep,*) ' error reading srf file'
         call srstop(1)
      endif

      close(file_srf%unit_nr)
      file_srf%status = FILE_STAT_UNOPENED

      return
      end
