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
module m_parttd
use m_stop_exit


implicit none

contains


      subroutine parttd ( lunin  , lunout , itime  , idtime , itime1 ,   &
                          itime2 , nftot  , nrtot  , array1 , array2 ,   &
                          result , ipnt   , lblock , luntxt , isflag ,   &
                          ifflag , update )

!     Deltares Software Centre

!>/File
!>            Steps along in a dataset with linear (volumes) or block function
!>
!>            Maintains 2 arrays:
!>            - array1 at itime1 is the lower array of the interval
!>            - array2 at itime2 is the upper array of the interval
!>            - idtime is the offset that is increased with the whole time span at rewind
!>            itime1 <= itime-idtime < itime2
!>            The result array is filled with a pointer to allow of active only hydrodynamics

!     system administration : Antoon Koster

!     created               :          1987 by Jos van Gils

!     last updated          : June     2011 by Leo Postma   : allow for active only aggregation

!     note                  : uses more array space than needed for block interpolation
!                             block interpolation done with dlwqbl

!     logical unitnumbers   : lunin  - input unit intermediate file
!                             lunout - monitor file

!     subroutines called    : stop_exit, stops execution

      use m_waq_precision         ! single/double precision
      use timers

      implicit none

!     Arguments           :

!     kind           function         name               description

      integer  (int_wp ), intent(in   ) :: lunin            !< unit number intermediate file
      integer  (int_wp ), intent(in   ) :: lunout           !< unit number report file
      integer  (int_wp ), intent(in   ) :: itime            !< current time in the model
      integer  (int_wp ), intent(inout) :: idtime           !< time offset after rewind
      integer  (int_wp ), intent(inout) :: itime1           !< lower time in file
      integer  (int_wp ), intent(inout) :: itime2           !< higher time in file
      integer  (int_wp ), intent(in   ) :: nftot            !< array size in the file
      integer  (int_wp ), intent(in   ) :: nrtot            !< array size to be delivered
      real     (sp), intent(inout) :: array1(nftot)    !< record at lower time in file
      real     (sp), intent(inout) :: array2(nftot)    !< record at higher time in file
      real     (sp), intent(inout) :: result(nrtot)    !< record as delivered to Delpar
      integer  (int_wp ), intent(in   ) :: ipnt  (nftot)    !< pointer from nftot to nrtot
      logical      , intent(in   ) :: lblock           !< if true then block function
      character( *), intent(in   ) :: luntxt           !< text with this unit number
      integer  (int_wp ), intent(in   ) :: isflag           !< if 1 then 'dddhhmmss' format
      integer  (int_wp ), intent(in   ) :: ifflag           !< if 1 then this is first invokation
      logical      , intent(  out) :: update           !< true if record is updated

!     declarations        :

      character(16), dimension(4) ::                                 &
           msgtxt(4) = (/' Rewind on      ' , ' Warning reading' ,   &
                         ' Error reading  ' , ' Divide error on' /)

!     local scalars

      integer(int_wp ) :: it2 , messge
      real   (sp) :: div , fac1  , fac2
      integer     :: i

      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "dlwqtd", ithndl )
!
      update = .false.
      messge = 0
      if ( nftot  == 0 ) goto 100
      if ( ifflag .eq. 1 ) then
         read ( lunin , end=40 , err=40 ) itime1 , array1
         read ( lunin , end=40 , err=40 ) itime2 , array2
         idtime = 0
         update = .true.
         result = 0.0
      endif

!         check for start time simulation before start time file

      if ( itime .lt. itime1 ) messge = 2

!         a new record required?

   10 do while ( itime-idtime .ge. itime2 )
         update = .true.
         array1 = array2
         itime1 = itime2
         read ( lunin , end=50 , err=40 ) itime2 , array2
      enddo

!         interpolation

      if ( itime2 .eq. itime1 ) goto 30
      result = 0.0
      if ( lblock ) then
         ! Stack overflow on large grid
         !result(ipnt(:)) = array1(:)
         do i = 1,size(array1)
             result(ipnt(i)) = array1(i)
         enddo
      else
         it2  = itime - idtime
         div  = float(itime2-itime1)
         fac1 = (itime2-it2   )/div
         fac2 = (it2   -itime1)/div
         ! Stack overflow on large grid
         !result(ipnt(:)) = fac1*array1(:) + fac2*array2(:)
         do i = 1,size(array1)
             result(ipnt(i)) = fac1 * array1(i) + fac2 * array2(i)
         enddo
      endif
      goto 100

!         normal rewind.

   20 rewind lunin
      idtime = idtime + itime1
      read ( lunin , end=40 , err=40 ) itime1 , array1
      read ( lunin , end=40 , err=40 ) itime2 , array2
      idtime = idtime - itime1
      goto 10

!         error processing

   30 messge = 4              !    ' Divide error on'
      goto 100

   40 messge = 3              !    ' Error reading  '
      goto 100

   50 messge = 1              !    ' Rewind on      '

!         write the messages

  100 if ( messge .ne. 0 ) then
         if ( isflag .ne. 1 ) then
              write( lunout, 2000 ) msgtxt(messge), lunin, trim(luntxt),     &
                                    itime, itime1
         else
              write( lunout, 2010 ) msgtxt(messge), lunin, trim(luntxt),     &
                                    itime /86400, mod(itime ,86400)/3600 ,   &
                                    mod(itime ,3600)/60, mod(itime ,60)  ,   &
                                    itime1/86400, mod(itime1,86400)/3600 ,   &
                                    mod(itime1,3600)/60, mod(itime1,60)
         endif
         if ( messge .eq. 1 ) then
            messge = 0
            goto 20
         endif
         if ( messge .ne. 2 ) call stop_exit( 1 )
      endif

      if ( timon ) call timstop ( ithndl )
      return

 2000 format (   a16          ,' unit: ',i,', reading: ',a,/              &
               ' at simulation time:',i12,' !',/,                          &
               ' time in file:      ',i12,' !')
 2010 format (   a16          ,' unit: ',i,', reading: ',a,/              &
               ' at simulation time:',i5,'d ',i2,'h ',i2,'m ',i2,'s !',/   &
               ' time in file:      ',i5,'d ',i2,'h ',i2,'m ',i2,'s !')

      end subroutine

end module m_parttd
