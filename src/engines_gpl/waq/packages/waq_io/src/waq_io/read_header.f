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

      subroutine read_header( waq_param, data_param, nocol , itfact, dtflg1,
     &                        dtflg3   , ierr      , iwar  )

!     Deltares Software Centre

!     function : Checks if column header exists

!     Global declarations

      use m_zoek
      use dlwq_data      ! for definition and storage of data
      use rd_token
      use timers       !   performance timers

      implicit none

!     declaration of arguments

      type(t_dlwq_item)     , intent(inout) :: waq_param    ! list of param items to be set in this block ( substances etc )
      type(t_dlwq_item)     , intent(inout) :: data_param   ! list of param items in the data
      integer               , intent(inout) :: nocol        ! number of columns in input
      integer               , intent(in)    :: itfact       ! factor between clocks
      logical               , intent(in)    :: dtflg1       ! true if time in 'date' format
      logical               , intent(in)    :: dtflg3       ! true if yyetc instead of ddetc
      integer               , intent(out)   :: ierr         ! error indication
      integer               , intent(inout) :: iwar         ! cumulative warning count

      ! local declaration

      integer                               :: itype         ! type of token
      character(len=256)                    :: ctoken        ! character token
      integer                               :: itoken        ! integer token
      real                                  :: rtoken        ! real token
      logical                               :: first         ! first loop indicator / .not. header exists
      integer                               :: i             ! item index
      integer                               :: k             ! shifted item index
      integer                               :: icnt          ! shift in item index
      character(len=8)                      :: strng         ! string to be printed
      integer                               :: ifound        ! index in list if found
      integer                               :: nitm         ! number of items in data
      integer                               :: ierr2        ! local error indication
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "read_header", ithndl )

      ! read loop

      first = .true.
      do
         if ( gettoken(ctoken, itoken, rtoken, itype  , ierr) .ne. 0 ) then

            goto 9999 ! a read error

         else

            !  a string has arrived, check for date string

            if ( itype .eq. 1 ) then
               call dlwq0t ( ctoken , itoken, .false., .false., ierr2 )
               if ( ierr2 .eq. 0 ) then

                  ! date string found, push back, exit input loop

                  push = .true.
                  exit

               endif
               if ( first ) then

                  ! a header exists

                  first = .false.
                  data_param%sequence = 0
                  nocol = 0
                  write ( lunut ,   *  )
               endif
               nocol = nocol + 1
               strng = 'not used'
               do i = 1 , data_param%no_item
                  call zoek(ctoken,1,data_param%name(i),20,ifound)
                  if ( ifound .ge. 1 ) then
                     strng = 'used'
                     data_param%sequence(i) = nocol
                  endif
               enddo
               write ( lunut , 1000 ) nocol, ctoken, strng
            else

               ! end of the list, push token back, conversion of time removed

               push = .true.
               exit

            endif
         endif

      enddo

      if ( .not. first ) then

         ! is everything resolved ?

         icnt = 0
         nitm = data_param%no_item
         do i = 1 , nitm
            k = i - icnt
            if ( data_param%name(k) .eq. '&$&$SYSTEM_NAME&$&$!') cycle
            if ( data_param%sequence(k) .gt. 0 ) cycle
            call compact_usefor( lunut , waq_param, data_param, k      , icnt   )
            iwar = iwar + 1
            if ( i + icnt .ge. nitm ) exit
         enddo

      endif

 9999 if (timon) call timstop( ithndl )
      return

 1000 format ( ' column:',i3,' contains: ',a40,' status: ',a8)

      end
