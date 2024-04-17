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

      subroutine write_srf ( file_srf, mmax  , nmax  , nosegl, surf  )
!
!     created             : jan van beek
!
!     function            : writes horizontal surface file.
!
!     subroutines called  : jbputi, puts an integer value to a dos binary file.
!                           jbputa, puts a real array to a dos binary file.
!                         : uxputi, puts an integer value to a unix binary file.
!                         : uxputa, puts an real array to a unix binary file.
!                         : which_operating_system, return platform type
!
      ! global declarations

      use m_waq_file                   ! module contains everything for the files
      implicit none

      ! declaration of arguments

      type(t_file)                       :: file_srf               ! surfaces-file
      integer                                :: mmax                   ! grid cells m direction
      integer                                :: nmax                   ! grid cells n direction
      integer                                :: nosegl                 ! number of segments per layer
      real                                   :: surf(nosegl)           ! surf

      ! local declarations

      integer       lun
      integer       i
      integer       idummy
      integer       irlen
      integer       plform
      integer       filtyp
      integer       filsta

      plform = which_operating_system()
      idummy = 0

      ! initialise file

      call file_srf%open()
      lun    = file_srf%unit
      filtyp = file_srf%type

      ! write surfaces file

      if ( filtyp .eq. FT_UNF .and. plform .eq. PL_UNX .or. &
           filtyp .eq. FT_BIN .and. plform .eq. PL_DOS) then
         write (lun) nmax,mmax,nosegl,nosegl,nosegl,idummy
         write (lun) (surf(i),i=1,nosegl)
      elseif ( filtyp .eq. FT_BIN .and. plform .eq. PL_UNX) then
         call jbputi(lun,nmax)
         call jbputi(lun,mmax)
         call jbputi(lun,nosegl)
         call jbputi(lun,nosegl)
         call jbputi(lun,nosegl)
         call jbputi(lun,idummy)
         call jbputa(lun,surf,nosegl)
      elseif ( filtyp .eq. FT_UNF .and. plform .eq. PL_DOS) then
         irlen = 6*4
         call uxputi(lun,irlen)
         call uxputi(lun,nmax)
         call uxputi(lun,mmax)
         call uxputi(lun,nosegl)
         call uxputi(lun,nosegl)
         call uxputi(lun,nosegl)
         call uxputi(lun,idummy)
         call uxputi(lun,irlen)
         irlen = nosegl*4
         call uxputi(lun,irlen)
         call uxputa(lun,surf,nosegl)
         call uxputi(lun,irlen)
      elseif ( filtyp .eq. FT_ASC ) then
         write (lun,'(4i8)') nmax,mmax,nosegl,nosegl,nosegl,idummy
         write (lun,'(e12.5)') (surf(i),i=1,nosegl)
      endif

      close(file_srf%unit)
      file_srf%status = FILE_STAT_UNOPENED

      return
      end
