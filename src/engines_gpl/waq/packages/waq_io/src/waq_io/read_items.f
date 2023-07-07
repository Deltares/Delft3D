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

      subroutine read_items( lunrep    , inpfil   , ioutpt   , chkflg   , callr ,
     +                       waq_item  , data_item, name_item, type_item, noits ,
     +                       ierr      , iwar)

!     Deltares Software Centre

!     function : item name retrieval, new style of DLWQ5B using t_dlwq_item structures instead of workspace

!     global declarations

      use m_zoek
      use dlwq_data
      use rd_token
      use timers       !   performance timers

      implicit none

!     declaration of the arguments

      integer               , intent(in)    :: lunrep       ! report file
      type(inputfilestack)  , intent(inout) :: inpfil       ! input file strucure with include stack
      integer               , intent(in)    :: ioutpt       ! level of reporting to ascii output file
      integer               , intent(in)    :: chkflg       ! check on input or add items
      character(len=10)     , intent(in)    :: callr        ! calling subject
      type(t_dlwq_item)     , intent(out)   :: waq_item     ! list of items to be set in this block ( boundaries, loads, substances etc )
      type(t_dlwq_item)     , intent(out)   :: data_item    ! list of items in the data
      type(t_dlwq_item)     , intent(inout) :: name_item    ! delwaq item list
      type(t_dlwq_item)     , intent(in)    :: type_item    ! delwaq (item-) type list
      integer               , intent(out)   :: noits        ! number of scale factors to be read
      integer               , intent(inout) :: ierr         ! cummulative error count
      integer               , intent(inout) :: iwar         ! cummulative warning count

!     local declarations

      logical       usefor, setnam, comput, signon
      integer                               :: ntitm        ! number of bounds/wastes
      integer                               :: nttype       ! number of bound/waste types
      integer                               :: noitm        ! number of items read
      integer                               :: t_asked      ! type of token asked
      integer                               :: itype        ! type of token read
      integer                               :: itoken       ! real token
      character(len=256)                    :: ctoken       ! character token
      real                                  :: rtoken       ! real token
      integer       itmnr, ioffc, ioffi, nconst, ierr2, i, i2, ifound, namset

      integer(4) :: ithndl = 0
      if (timon) call timstrt( "read_items", ithndl )

!     some initialisations

      usefor = .false.
      setnam = .false.
      comput = .false.
      signon = .false.

      waq_item%no_item   = 0
      itmnr              = 0
      data_item%no_item  = 0
      noitm              = 0
      noits              = 0
      ntitm              = name_item%no_item
      nttype             = type_item%no_item

      ioffc  = 0
      ioffi  = 0
      nconst = 0

!     get a token string (and return if something wrong was found)

   10 continue
      if ( signon .or. ( usefor .and. setnam ) ) then
         ierr = gettoken(ctoken, itoken, rtoken, itype , ierr2 )
      else
         ierr = gettoken(ctoken, itoken, itype , ierr2 )
      endif
      if ( ierr .ne. 0 ) then
         push = .true.
         goto 9999
      endif

!     a keyword was met

      if ( iabs(itype) .eq. 1 .and.
     *         (  ctoken(1: 5) .eq. 'BLOCK'        .or.
     *            ctoken(1: 6) .eq. 'LINEAR'       .or.
     *            ctoken(1: 4) .eq. 'ITEM'         .or.
     *            ctoken(1:13) .eq. 'IDENTICALITEM'.or.
     *            ctoken(1:12) .eq. 'USEDATA_ITEM' .or.
     *            ctoken(1: 7) .eq. 'FORITEM'      .or.
     *            ctoken(1: 9) .eq. 'DATA_ITEM'    .or.
     *            ctoken(1: 6) .eq. 'CONCEN'       .or.
     *            ctoken(1: 6) .eq. 'DATA'         .or.
     *            ctoken(1:10) .eq. 'TIME_DELAY'   .or.
     *            ctoken(1: 8) .eq. 'ODS_FILE'     .or.
     *            ctoken(1:11) .eq. 'BINARY_FILE'  .or.
     *            ctoken(1: 8) .eq. 'ABSOLUTE'     .or.
     *            ctoken(1: 4) .eq. 'TIME'         .or.
     *            ctoken(1: 9) .eq. 'HARMONICS'    .or.
     *            ctoken(1: 8) .eq. 'FOURIERS'     .or.
     *            ctoken(1: 5) .eq. 'SCALE'        .or.
     *            ctoken(1: 8) .eq. 'DEFAULTS'     .or.
     *            ctoken(1: 3) .eq. 'ALL'          .or.
     *            ctoken(1: 8) .eq. 'SEGMENTS'     .or.
     *            ctoken(1: 9) .eq. 'CONSTANTS'    .or.
     *            ctoken(1:10) .eq. 'PARAMETERS'   .or.
     *            ctoken(1: 9) .eq. 'FUNCTIONS'    .or.
     *            ctoken(1: 9) .eq. 'INPUTGRID'    .or.
     *            ctoken(1:13) .eq. 'SEG_FUNCTIONS'     )   ) then
         if ( usefor ) then
            write ( lunut , 1035 ) ctoken
            goto 40
         else
            push = .true.
            goto 9999
         endif
      endif

!     computations

      if ( iabs(itype) .eq. 1 .and.
     *     ( ctoken .eq.  '*'  .or. ctoken .eq.  '/'  .or.
     *       ctoken .eq.  '+'  .or. ctoken .eq.  '-'  .or.
     *       ctoken .eq. 'MIN' .or. ctoken .eq. 'MAX'      ) ) then
         if ( .not. comput ) then
            write ( lunut , 1070 )
            goto 40
         endif
         if ( signon ) then
            write ( lunut , 1080 )
            goto 40
         endif
         noitm = noitm + 1
         noits = noits + 1
         ierr2 = dlwq_resize(data_item,noitm)
         data_item%no_item = noitm
         data_item%sequence(noitm) = 0
         if ( ctoken .eq.  '*'  ) data_item%ipnt(noitm) = -1000000
         if ( ctoken .eq.  '/'  ) data_item%ipnt(noitm) = -10000000
         if ( ctoken .eq.  '+'  ) data_item%ipnt(noitm) = -100000000
         if ( ctoken .eq.  '-'  ) data_item%ipnt(noitm) = -1000000000
         if ( ctoken .eq. 'MIN' ) data_item%ipnt(noitm) = -1100000000
         if ( ctoken .eq. 'MAX' ) data_item%ipnt(noitm) = -1200000000
         signon = .true.
         goto 10
      endif

!     an item used in computations

      if ( iabs(itype) .eq. 1 .and. signon ) then

         do i = 1 , itmnr-1
            if ( waq_item%ipnt(i) .eq. -1300000000 ) cycle
            call zoek ( ctoken, 1,waq_item%name(i),20,ifound)
            if ( ifound .eq. 1 ) then
               noits = noits - 1
               i2 = data_item%ipnt(noitm)
               if ( i2 .eq. -1000000 )    write(lunut,1120)i,ctoken
               if ( i2 .eq. -10000000 )   write(lunut,1110)i,ctoken
               if ( i2 .eq. -100000000 )  write(lunut,1100)i,ctoken
               if ( i2 .eq. -1000000000 ) write(lunut,1090)i,ctoken
               if ( i2 .eq. -1100000000 ) write(lunut,1092)i,ctoken
               if ( i2 .eq. -1200000000 ) write(lunut,1094)i,ctoken
               data_item%ipnt(noitm) = i2 + i
               data_item%name(noitm) = '&$&$SYSTEM_NAME&$&$!'
               signon = .false.
               goto 10
            endif
         enddo

         i2 = data_item%ipnt(noitm)
         if ( i2 .eq. -1000000 )    write(lunut,1130)ctoken
         if ( i2 .eq. -10000000 )   write(lunut,1140)ctoken
         if ( i2 .eq. -100000000 )  write(lunut,1150)ctoken
         if ( i2 .eq. -1000000000 ) write(lunut,1160)ctoken
         if ( i2 .eq. -1100000000 ) write(lunut,1162)ctoken
         if ( i2 .eq. -1200000000 ) write(lunut,1164)ctoken
         data_item%sequence(noitm) = noits
         data_item%name(noitm) = ctoken
         signon = .false.
         goto 10
      endif

!     a number is used in computations

      if ( iabs(itype) .eq. 2 .or. iabs(itype) .eq. 3 ) then
         if ( setnam .or. signon ) then
            nconst = nconst + 1
            noits = noits - 1
            i2 = data_item%ipnt(noitm)
            data_item%name(noitm) = '&$&$SYSTEM_NAME&$&$!'
            data_item%constant(noitm) = rtoken
            if ( signon ) then
               if ( i2 .eq. -1000000 )    write(lunut,1170)rtoken
               if ( i2 .eq. -10000000 )   write(lunut,1180)rtoken
               if ( i2 .eq. -100000000 )  write(lunut,1190)rtoken
               if ( i2 .eq. -1000000000 ) write(lunut,1200)rtoken
               if ( i2 .eq. -1100000000 ) write(lunut,1210)rtoken
               if ( i2 .eq. -1200000000 ) write(lunut,1220)rtoken
               data_item%ipnt(noitm) = i2 - nconst
               signon = .false.
            endif
            if ( setnam ) then
               namset = waq_item%ipnt(itmnr)
               if ( namset .gt. 0 .and. ioutpt .ge. 3 ) then
                  write ( lunut , 1001 ) callr, itmnr, callr, namset ,
     *                                   name_item%name(namset) , rtoken
               elseif ( namset .eq. 0 .and. ioutpt .ge. 3  ) then
                  write ( lunut , 1001 ) callr, itmnr, callr, namset ,
     *                                   'flow'        , rtoken
               elseif (namset .eq. -1300000000 .and. ioutpt .ge. 3) then
                  write ( lunut , 1001 ) callr, itmnr, callr, namset ,
     *                                   'Ignored'     , rtoken
               elseif ( ioutpt .ge. 3 ) then
                  write ( lunut , 1011 ) callr, itmnr, callr,-namset ,
     *                                   type_item%name(-namset) , rtoken
               endif
               data_item%ipnt(noitm) =  -nconst
               data_item%sequence(noitm) = 0
               usefor = .false.
               setnam = .false.
               comput = .true.
            endif
            goto 10
         endif
      endif
!
!          A local redirection of the name of an item or substance
!
      if ( iabs(itype) .eq. 1 .and. ctoken .eq. 'USEFOR') then
         if ( usefor ) then
            write ( lunut , 1035 ) ctoken
            goto 40
         else
            usefor = .true.
            setnam = .false.
            goto 10
         endif
      endif
!
!          Getting the items of this block
!                        waq_item(itmnr)   is the reference to the delwaq item (NAMSET)
!                        data_item(noitm)    is the order number in the series (data)
!                        namset              is the ID number of NOITMth name
!                        ANAME/ATYPE(NAMSET) is the corresponding reserved name or type (delwaq item)
!                        ctoken              is the name that should be used.
!
      if ( itype .eq. 1 ) then
         if ( usefor .and. setnam ) then
            namset = waq_item%ipnt(itmnr)
            if ( namset .gt. 0 .and. ioutpt .ge. 3 ) then
               write ( lunut , 1000 ) callr , itmnr , callr , namset ,
     *                                name_item%name(namset) , ctoken
            elseif ( namset .eq. 0 .and. ioutpt .ge. 3  ) then
               write ( lunut , 1000 ) callr , itmnr , callr , namset ,
     *                                'FLOW'        , ctoken
            elseif ( namset .eq. -1300000000 .and. ioutpt .ge. 3  ) then
               write ( lunut , 1000 ) callr , itmnr , callr , namset ,
     *                                'Ignored'     , ctoken
            elseif ( ioutpt .ge. 3 ) then
               write ( lunut , 1010 ) callr , itmnr , callr ,-namset ,
     *                                type_item%name(-namset) , ctoken
            endif
            data_item%sequence(noitm) = noits
            data_item%name(noitm) = ctoken
            usefor = .false.
            setnam = .false.
!                     it is now possible to compute
            comput = .true.
            goto 10
         endif
!
!              fill in a string value if an empty string is provided
!
         if ( chkflg      .eq. -1 .and.
     *        ctoken(1:20) .eq. '                    ' ) then
            ctoken = 'Item-'
            write ( ctoken(6:12) , '(i7)' ) noitm+1
         endif
!
!              FLOW is only valid as CONCENTR. and item number is 0
!
         call zoek(ctoken,1,'FLOW                ',20,ifound)
         if ( ifound .eq. 1 .and. callr .eq. 'CONCENTR. ' ) then
            itmnr = itmnr + 1
            ierr2 = dlwq_resize(waq_item,itmnr)
            waq_item%no_item = itmnr

            noitm = noitm + 1
            ierr2 = dlwq_resize(data_item,noitm)
            data_item%no_item = noitm

            noits = noits + 1

            waq_item%ipnt(itmnr)      = 0
            data_item%ipnt(noitm)     = itmnr
            data_item%sequence(noitm) = noits
            waq_item%name(itmnr)      = ctoken
            data_item%name(noitm)     = ctoken
            if ( usefor ) setnam = .true.
            if ( ioutpt .ge. 3 .and. .not. usefor )
     *      write ( lunut , 1020 ) callr , itmnr , callr , 0 , 'FLOW'
            goto 10
         endif
!
!              ctoken equals an item-NAME
!
         i2= dlwq_find(name_item,ctoken)
         if ( i2 .ge. 1 ) then
            itmnr = itmnr + 1
            ierr2 = dlwq_resize(waq_item,itmnr)
            waq_item%no_item = itmnr

            noitm = noitm + 1
            ierr2 = dlwq_resize(data_item,noitm)
            data_item%no_item = noitm

            noits = noits + 1

            waq_item%ipnt(itmnr)       = i2
            data_item%ipnt(noitm)      = itmnr
            data_item%sequence(noitm)  = noits
            waq_item%name(itmnr)       = ctoken
            data_item%name(noitm)      = ctoken
            if ( usefor ) setnam = .true.
            if ( ioutpt .ge. 3 .and. .not. usefor )
     +      write ( lunut , 1020 ) callr, itmnr, callr, i2, name_item%name(i2)
            goto 10
         endif

!        ctoken equals an item-TYPE. the index reference is set negative

         i2= dlwq_find(type_item,ctoken)
         if ( i2 .ge. 1 ) then
            itmnr = itmnr + 1
            ierr2 = dlwq_resize(waq_item,itmnr)
            waq_item%no_item = itmnr

            noitm = noitm + 1
            ierr2 = dlwq_resize(data_item,noitm)
            data_item%no_item = noitm

            noits = noits + 1

            waq_item%ipnt(itmnr)      = -i2
            data_item%ipnt(noitm)     = itmnr
            data_item%sequence(noitm) = noits
            waq_item%name(itmnr)      = ctoken
            data_item%name(noitm)     = ctoken

            if ( usefor ) setnam = .true.
            if ( ioutpt .ge. 3 .and. .not. usefor )
     +      write ( lunut , 1030 ) callr, itmnr, callr, i2, type_item%name(i2)
            goto 10
         endif

!        name does not exist

         if ( chkflg .eq. 1 ) then

!           ignore the data

            itmnr = itmnr + 1
            ierr2 = dlwq_resize(waq_item,itmnr)
            waq_item%no_item = itmnr

            noitm = noitm + 1
            ierr2 = dlwq_resize(data_item,noitm)
            data_item%no_item = noitm

            noits = noits + 1

            waq_item%ipnt(itmnr)      = -1300000000
            data_item%ipnt(noitm)     = 1300000000
            data_item%sequence(noitm) = noits
            waq_item%name(itmnr)      = ctoken
            data_item%name(noitm)     = ctoken

            if ( usefor ) setnam = .true.
            write ( lunut , 1040 ) callr, itmnr, trim(ctoken)
            iwar = iwar + 1
            goto 10
         else

!           now a new name is added to the list of names

            ntitm = ntitm + 1
            ierr2 = dlwq_resize(name_item,ntitm)
            name_item%no_item = ntitm
            name_item%name(ntitm) = ctoken

!           plus normal procedure

            itmnr = itmnr + 1
            ierr2 = dlwq_resize(waq_item,itmnr)
            waq_item%no_item = itmnr

            noitm = noitm + 1
            ierr2 = dlwq_resize(data_item,noitm)
            data_item%no_item = noitm

            noits = noits + 1

            waq_item%ipnt(itmnr)      = ntitm
            data_item%ipnt(noitm)     = itmnr
            data_item%sequence(noitm) = noits
            waq_item%name(itmnr)      = ctoken
            data_item%name(noitm)     = ctoken

            if ( usefor ) setnam = .true.
            if ( ioutpt .ge. 3 .and. .not. usefor )
     +                   write ( lunut , 1020 ) callr, itmnr, callr,
     +                                          ntitm, name_item%name(ntitm)
            goto 10
         endif
      endif

!     no item name was given, but an item number

      if ( itype .eq. 2 ) then
         if ( itoken .le.  ntitm .and. itoken .ge. -nttype ) then

            itmnr = itmnr + 1
            ierr2 = dlwq_resize(waq_item,itmnr)
            waq_item%no_item = itmnr

            noitm = noitm + 1
            ierr2 = dlwq_resize(data_item,noitm)
            data_item%no_item = noitm

            noits = noits + 1

            waq_item%ipnt(itmnr)      = itoken
            data_item%ipnt(noitm)     = itmnr
            data_item%sequence(noitm) = noits
            if ( callr .eq. 'segment' ) then
               if ( itoken .le. 0 ) then
                  write ( lunut , 1060 ) itoken
                  goto 40
               endif
               if ( ioutpt .ge. 3 .and. .not. usefor )
     +              write ( lunut , 1015 ) callr, itmnr, callr,  itoken
               write ( ctoken , '(''Segment '',i8)' ) itoken
            elseif ( itoken .eq. 0 .and. callr .ne. 'CONCENTR. ' ) then
               write ( lunut , 1060 ) itoken
               goto 40
            elseif ( itoken .gt. 0 ) then
               if ( ioutpt .ge. 3 .and. .not. usefor )
     +              write ( lunut , 1020 ) callr, itmnr, callr,  itoken,
     +                                                   name_item%name(itoken)
               ctoken = name_item%name(itoken)
            elseif ( itoken .eq. 0 .and. callr .eq. 'CONCENTR. ' ) then
               if ( ioutpt .ge. 3 .and. .not. usefor )
     +         write ( lunut , 1020 ) callr, itmnr, callr, itoken,
     +                                                    'FLOW'
               ctoken = 'FLOW'
            else
               if ( ioutpt .ge. 3 .and. .not. usefor )
     +         write ( lunut , 1030 ) callr, itmnr, callr, -itoken,
     +                                                   type_item%name(-itoken)
               ctoken = type_item%name(-itoken)
            endif
            waq_item%name(itmnr)   = ctoken
            data_item%name(noitm)  = ctoken
            if ( usefor ) setnam = .true.
            goto 10
         else
            write ( lunut , 1060 ) itoken
            goto 40
         endif
      endif
!
   40 ierr = 1
 9999 if (timon) call timstop( ithndl )
      return
!
 1000 FORMAT (  ' Input ',A,' nr:',I5,' is ',A,' nr:',I5,' with ID  : ',
     *          A20,' and local substitution: ',A20 )
 1001 FORMAT (  ' Input ',A,' nr:',I5,' is ',A,' nr:',I5,' with ID  : ',
     *          A20,' and local substitution: ',E15.6 )
 1010 FORMAT (  ' Input ',A,' nr:',I5,' is ',A,' type:',I5,
     *          ' with type: ',A20,' and local substitution: ',A20 )
 1011 FORMAT (  ' Input ',A,' nr:',I5,' is ',A,' type:',I5,
     *          ' with type: ',A20,' and local substitution: ',E15.6 )
 1015 FORMAT (  ' Input ',A,' nr:',I5,' is ',A,' nr:',I5 )
 1020 FORMAT (  ' Input ',A,' nr:',I5,' is ',A,' nr:',I5,' with ID  : ',
     *          A20 )
 1030 FORMAT (  ' Input ',A,' nr:',I5,' is ',A,' nr:',I5,' with type: ',
     *          A20 )
 1035 FORMAT (  ' ERROR: no reserved keyword expected: ', A20 )
 1040 FORMAT (  ' WARNING: Input ',A,' nr:',I5,' with name: ',A20,
     *          ' is not a valid ID, data ignored' )
 1050 FORMAT ( /' ERROR: string is no valid item ID: ',A )
 1060 FORMAT (  ' ERROR: number: ',I5,' is not a valid item number !' )
 1070 FORMAT (  ' ERROR: multiplication is only allowed in USEFOR',
     *          ' context !')
 1080 FORMAT (  ' ERROR: arithmetics should be separated by items !')
 1090 FORMAT (  ' Subtracted by item nr: ',I6,' Name: ',A20 )
 1092 FORMAT (  ' Minimum value is item nr: ',I6,' Name: ',A20 )
 1094 FORMAT (  ' Maximum value is item nr: ',I6,' Name: ',A20 )
 1100 FORMAT (  ' Summed with item nr: ',I6,' Name: ',A20 )
 1110 FORMAT (  ' Divided by item nr: ',I6,' Name: ',A20 )
 1120 FORMAT (  ' Multiplied by item nr: ',I6,' Name: ',A20 )
 1130 FORMAT (  ' Multiplied by local substitution: ',A20 )
 1140 FORMAT (  ' Divided by local substitution: ',A20 )
 1150 FORMAT (  ' Summed with local substitution: ',A20 )
 1160 FORMAT (  ' Subtracted by local substitution: ',A20 )
 1162 FORMAT (  ' Minimum value is local substitution: ',A20 )
 1164 FORMAT (  ' Maximum value is local substitution: ',A20 )
 1169 FORMAT (  ' Substituted by: ',E15.6 )
 1170 FORMAT (  ' Multiplied by: ',E15.6 )
 1180 FORMAT (  ' Divided by: ',E15.6 )
 1190 FORMAT (  ' Summed with: ',E15.6 )
 1200 FORMAT (  ' Subtracted by: ',E15.6 )
 1210 FORMAT (  ' Minimum value is: ',E15.6 )
 1220 FORMAT (  ' Maximum value is: ',E15.6 )
!
      END
