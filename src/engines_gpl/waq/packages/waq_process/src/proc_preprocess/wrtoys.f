!!  Copyright (C)  Stichting Deltares, 2012-2023.
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

      subroutine wrtoys ( lchar , lun    , notot , syname, noutp ,
     +                    ioutps, outputs)

!     Deltares Software Centre

!>/File
!>      writes altoys input files.

!     Created   : Nov   1994 by Jan van Beek
!     Modified  : Aug   2012 by Jan van Beek, use output structure, modern look and feel

      use timers         !< performance timers
      use output

      implicit none

      character(len=*)    , intent(in   ) :: lchar(*)               !< filenames
      integer             , intent(in   ) :: lun(*)                 !< unit numbers
      integer             , intent(in   ) :: notot                  !< number of substances
      character(len=20)   , intent(in   ) :: syname(*)              !< substance names
      integer             , intent(in   ) :: noutp                  !< total number of output files
      integer             , intent(in   ) :: ioutps(7,*)            !< (old) output structure
      type(outputcoll)    , intent(in   ) :: outputs                !< output structure

      ! local

      integer                             :: isys
      integer                             :: ivar
      integer                             :: ioffv
      character*255                       :: filpst
      character*255                       :: filstu
      integer                             :: lunwrk
      integer                             :: nrvar
      integer                             :: indx
      integer                             :: indx2
      integer                             :: ilen
      integer(4)                :: ithndl = 0        ! handle for performance timer
      if (timon) call timstrt( "wrtoys", ithndl )
!
      lunwrk = 81
!
!     write altoys.inp for all output in history file
!
      open ( lunwrk , file = 'altoys.inp' )
      if ( ioutps(5,3) .eq. ihi3 ) then
         do 100 isys = 1 , notot
            write(lunwrk,1000) syname(isys),syname(isys)
  100    continue
      endif
      if ( ioutps(5,3) .eq. ihi3 .or. ioutps(5,3) .eq. ihi4) then
         ioffv = ioutps(4,1) + ioutps(4,2)
         nrvar = ioutps(4,3)/2
         do 200 ivar = 1 , nrvar
            write(lunwrk,1000) outputs%names(ioffv+ivar),outputs%names(ioffv+ivar)
  200    continue
      endif
      close ( lunwrk )
!
!     write batoys.inp for all substances
!
      if ( ioutps(5,5) .eq. ibal ) then
         open ( lunwrk , file = 'batoys.inp' )
         write(lunwrk,1010) notot
         do 300 isys = 1 , notot
            write(lunwrk,1020) syname(isys)
  300    continue
         do 400 isys = 1 , notot
            write(lunwrk,1030) syname(isys),syname(isys)
  400    continue
         close ( lunwrk )
      endif
!
!     construct filename pst en stu file
!
      filpst = lchar(21)
      filstu = filpst
      indx = index(filpst,'.his ')
      if ( indx .gt. 0 ) then
         filpst(indx:) = '.pst'
         filstu(indx:) = '.stu'
      endif
      indx = indx+3
      indx = min ( indx   , 255 )
      ilen = len(lchar(36))
      indx2 = index(lchar(36),' ')
      if ( indx2 .eq. 0 ) then
         indx2 = ilen
      else
         indx2 = indx2 - 1
         indx2 = max(indx2,1)
      endif
!
!     write altoys.fil, file filetje for altoys
!
      open ( lunwrk , file = 'altoys.fil' )
      write(lunwrk,1040) lchar(36)(1:indx2)
      write(lunwrk,1040) lchar(37)(1:indx)
      write(lunwrk,1040) 'batoys.inp'
      write(lunwrk,1040) lchar(21)(1:indx)
      write(lunwrk,1040) filstu(1:indx)
      write(lunwrk,1040) filpst(1:indx)
      write(lunwrk,1040) 'altoys.mes'
      write(lunwrk,1040) 'altoys.inp'
      write(lunwrk,1040) '      .   '
      close ( lunwrk )
!
!     write altoys.ini default settings
!
      open ( lunwrk , file = 'altoys.ini' )
      write(lunwrk,1050)
      write(lunwrk,1060)
      write(lunwrk,1070)
      write(lunwrk,1070)
      close ( lunwrk )
!
      if (timon) call timstop( ithndl )
      return
 1000 format ( a20,'a',9x,a20,'1.0')
 1010 format ( i6 , ' * number of balances')
 1020 format ( a10 )
 1030 format ( a10,2x,a10,2x,'1.0')
 1040 format ( '''',a,'''' )
 1050 format ( '86400' )
 1060 format ( '''day''' )
 1070 format ( '0' )
      end
