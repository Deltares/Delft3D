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

      subroutine write_poi ( file_poi , noq   , noq1  , noq2  , noq3  , ipoint   )
!
!     created             : jan van beek
!
!     function            : writes pointers to delwaq auxiliary input file.
!
!     subroutines called  : jbputj, puts an integer array to a dos binary file.
!
!     parameters          :
!
!     name    kind     length     funct.  description
!     ----    -----    ------     ------- -----------
!     filnr   integer  1          input   index file in file administr.
!     noq1    integer  1          input   number of items 1 in file
!     noq2    integer  1          input   number of items 2 in file
!     noq3    integer  1          input   number of items 3 in file
!     ipoint  integer  *          input   pointer array
!
      ! global declarations

      use m_waq_file                   ! module contains everything for the files
      implicit none

!     declaration of arguments
!
      type(t_file)                       :: file_poi               ! pointer file
      integer       noq   , noq1  , noq2  , noq3
      integer       ipoint(*)
!
!     local declarations
!
      integer       noq12 , noq123, lun, k
      integer       irlen
      integer       plform
      character*256 filnam
      integer       filtyp
      integer       filsta

      plform = which_operating_system()
!
!     initialise file
!
      call file_poi%open()
      lun    = file_poi%unit
      filtyp = file_poi%type
      filnam = file_poi%name
!
!     write pointers
!
      noq12  = noq1  + noq2
      noq123 = noq12 + noq3
!
      if ( filtyp .eq. FT_UNF .and. plform .eq. PL_UNX .or. &
           filtyp .eq. FT_BIN .and. plform .eq. PL_DOS) then
         if ( noq1 .gt. 0 ) write (lun) (ipoint(k),k=1,4*noq1)
         if ( noq2 .gt. 0 ) write (lun) (ipoint(k),k=4*noq1+1,4*noq12)
         if ( noq3 .gt. 0 ) write (lun) (ipoint(k),k=4*noq12+1,4*noq123)
      elseif ( filtyp .eq. FT_BIN .and. plform .eq. PL_UNX) then
         if ( noq1 .gt. 0 ) call jbputj (lun,ipoint,4*noq1)
         if ( noq2 .gt. 0 ) call jbputj (lun,ipoint(4*noq1+1),4*noq2)
         if ( noq3 .gt. 0 ) call jbputj (lun,ipoint(4*noq12+1),4*noq3)
      elseif ( filtyp .eq. FT_UNF .and. plform .eq. PL_DOS) then
         if ( noq1 .gt. 0 ) then
            irlen = 4*noq1*4
            call uxputi(lun,irlen)
            call uxputj (lun,ipoint,4*noq1)
            call uxputi(lun,irlen)
         endif
         if ( noq2 .gt. 0 ) then
            irlen = 4*noq2*4
            call uxputi(lun,irlen)
            call uxputj (lun,ipoint(4*noq1+1),4*noq2)
            call uxputi(lun,irlen)
         endif
         if ( noq3 .gt. 0 ) then
            irlen = 4*noq3*4
            call uxputi(lun,irlen)
            call uxputj (lun,ipoint(4*noq12+1),4*noq3)
            call uxputi(lun,irlen)
         endif
      elseif ( filtyp .eq. FT_ASC ) then
         if ( noq1 .gt. 0 ) write (lun,'(4(i7,1x))') (ipoint(k),k=1,4*noq1)
         if ( noq2 .gt. 0 ) write (lun,'(4(i7,1x))') (ipoint(k),k=4*noq1+1,4*noq12)
         if ( noq3 .gt. 0 ) write (lun,'(4(i7,1x))') (ipoint(k),k=4*noq12+1,4*noq123)
      endif

      close(file_poi%unit)
      file_poi%status = FILE_STAT_UNOPENED

      return
      end
