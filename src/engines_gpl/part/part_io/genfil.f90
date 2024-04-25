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

module genfil_mod
!
!  module declarations
!
!
!  data definition module(s)
!
use m_waq_precision    ! single and double precision
      use timers
!
implicit none    ! force explicit typing
!
contains
      subroutine genfil(filnam,ftype,lnpath)
!
!     auteur   : antoon koster
!     date     : 4 feb.r 2003
!     function : generate part file names
!                starting from a full path of the fikle name on input,
!                this function adds a prefix at the start (file type)
!                e.g.  ../model/data/p04.inp the new file name
!                      ../model/data/map-p04
!     note     : code was extracted from part12, part13, etc routines
!


      implicit none

      character(len=*) :: filnam
      character(len=4) :: ftype
      logical          :: search_back
!
!     local scalars
!
      integer(int_wp )      :: iext , ipath , k , lenfil , lnpath
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "genfil", ithndl )

      search_back = .true.

      ipath = 0
      lenfil= len_trim(filnam)
!
!     backward search for directory separator
!     note: one directory path may include forward as well as backward separators
!           e.g. c:\my projects/test cases
!           scan function searches for both separators !
!
      ipath  = scan(filnam,'\/',search_back)
!
!     backward search for file extension
!
      iext = index(filnam,'.',search_back)
!
!     remove file-extension
!
      write(filnam(iext:iext+3), '(a4)') '    '
!
      do k=lenfil,ipath+1,-1
          filnam(k+4:k+4)=filnam(k:k)
      enddo
      write(filnam(ipath+1:ipath+4),'(a4)') ftype

      lnpath = lenfil
      if ( timon ) call timstop ( ithndl )
      return
      end subroutine
end module


