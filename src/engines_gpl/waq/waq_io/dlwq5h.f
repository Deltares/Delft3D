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
      module m_dlwq5h
      use m_waq_precision


      implicit none
      
      contains


      subroutine compact_usefor_list( lunut  , iar    , itmnr  , noitm  , idmnr  ,
     *                    nodim  , iorder , cnames , ioffi  , ioffc  ,
     *                             iods   , ioffd, i , icnt, ierr, iwar)
!
!
!     Deltares        Sector Waterresources And Environment
!
!     Created            : October '00  by L. Postma
!
!     Modified           :
!
!     Function           : Compacts USEFOR lists if unresolved externals
!
!     Subroutines Called : none
!
!     Logical Units      : LUN(27) = unit stripped DELWAQ input file
!                          LUN(29) = unit formatted output file
!
!     Parameters    :
!
!     Name    Kind     Length     Funct.  Description
!     ---------------------------------------------------------
!     lunut   integer    1         input   unit number for ascii output
!     iar     integer  iimax       in/out  integer   workspace
!     itmnr   integer    1         in/out  nr of items for assignment
!     noitm   integer    1         in      nr of items in computational rule
!     idmnr   integer    1         in/out  nr of subst for assignment
!     nodim   integer    1         in      nr of subst in computational rule
!     iorder  integer    1         in      1 = items first, 2 is subst first
!     cnames  char*(*)  nitm       input   items to check for presence
!     ioffi   integer    1         in/out  offset in input array
!     ioffc   integer    1         in/out  offset in character array
!     ioffd   integer    1         in/out  base offset in both arrays
!     iods    integer    1         input   shift counter ods files
!     i       integer    1         input   loop counter
!     icnt    integer    1         in/out  counter
!
!
      use timers       !   performance timers

      character*(*) cnames(*)
      dimension     iar(*)
      character*20  chulp,  message_type
      integer(kind=int_wp) ::  ithndl = 0
      integer(kind=int_wp) ::  i1, i3, i4, i5
      integer(kind=int_wp) ::  lunut, i, icnt, ioffc, iorder, ntt, idmnr, nitm, nodim
      integer(kind=int_wp) ::  itmnr, noitm, i2, iar, ioffd, ishft, ioffi, iods
       
      integer(kind=int_wp) ::  ierr, iwar
      
      
      ierr = -1
      if (timon) call timstrt( "compact_usefor_list", ithndl )
!
!       Write message
!
      write ( lunut ,   *  )

      if ( iorder == 1 ) then ! items first
          ntt  = idmnr
          nitm = nodim
      else ! subst first
          ntt  = itmnr
          nitm = noitm
      endif
!
!       look backwards
!
      i4 = 0
      do i1 = i,1,-1
         i2 = iar(i1+ioffc)
         if ( i2 > -100000 ) exit
      end do
!
!       additional messages for this sequence
      if ( i2 <= 0 .and. i2 > -100000 ) then
!       try to find the reference
         do i3 = 1 , i
            i5 = iar(i3+ioffc)
            if ( i5 > 0 )   i4 = iar(i3+ioffc)
            if ( i5 <= 0 .and. i5 > -100000 )   i4 = i4 + 1
         end do
         chulp = cnames(i4+ioffd)
         if ( cnames(i+ioffc) /= chulp ) then
            if ( iorder == 2 ) then
               write (lunut,1030) i4,chulp
            else
               write (lunut,1040) i4,chulp
            end if
         end if
      else if ( i2 > 0 .and. i2 <  100000 ) then
         i4 = i2
         chulp = cnames( i2+ioffd)
         if ( cnames(i+ioffc) == chulp ) then
             iwar = iwar + 1
             message_type = "WARNING"
             write ( lunut , 1010 ) message_type, i+icnt, cnames(i+ioffc)
         else
             message_type = "ERROR"
             write ( lunut , 1010 ) message_type, i+icnt, cnames(i+ioffc)
             ierr = 1
             if ( iorder == 2 ) then
               write (lunut,1030)  message_type, i2, chulp
            else
               write (lunut,1040)  message_type, i2, chulp
            end if
         end if
      endif
      i2 = i4
!
!     determine the shift in locations
      ishft = 1
      do i4 = i1+1,nitm
         i3 = iar(i4+ioffc)
         if ( i3 > -1000000 ) exit
         ishft = ishft + 1
      end do
!
!     shift the third array heap
      do i4 = i1, nitm
         iar   (i4+ioffi) = iar(i4+ioffi+ishft)
      end do
!
!     shift the second array heap
      do i4 = i1, nitm*2+iods
         iar   (i4+ioffc) = iar   (i4+ioffc+ishft)
         cnames(i4+ioffc) = cnames(i4+ioffc+ishft)
      end do
      nitm  = nitm  - ishft
      ioffi = ioffi - ishft
      ioffc = ioffc - 1
      ioffi = ioffi - 1
      icnt  = icnt  + ishft
!
!     shift the base array heap
      do i5 = i2+ioffd , ntt+ioffd+nitm*2+iods
         iar   (i5) = iar   (i5+1)
         cnames(i5) = cnames(i5+1)
      end do
!
!      renumber the second array heap
      do i4 = i1 , nitm
         if ( iar(i4+ioffc) > i2 ) then
             iar(i4+ioffc) = iar(i4+ioffc) -1
         end if
      end do
!
!      update totals
      if ( iorder == 1 .or.  iods > 0 ) then
         idmnr = idmnr-1
         nodim = nodim-ishft
      else if ( iorder == 2 .and. iods == 0 ) then
         itmnr = itmnr-1
         noitm = noitm-ishft
      endif
!
      if (timon) call timstop( ithndl )
      return
!
 1010 format ( ' ', A, ': Input item : ',I3,' not resolved: ',A)
 1030 format ( ' ', A, ': Item number: ',I3,' also not resolved: ',A)
 1040 format ( ' ', A, ': Substance  : ',I3,' also not resolved: ',A)
!
      end subroutine compact_usefor_list

      end module m_dlwq5h
