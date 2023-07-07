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

      subroutine dlwq01 ( lun    , syname , nosys  , notot  , nomult ,
     &                    multp  , iwidth , otime  , isfact , refday ,
     &                    vrsion , ioutpt , ierr   , iwar   )

!       Deltares Software Centre

!>\file
!>                          Reads the model identification and substances IDs
!>
!>                          This routine reads:
!>                             - the first non tokenized line with line lengthes and comment character
!>                             - the version number of the input file
!>                             - the 3 40 character strings with model documentation
!>                             - a 4th 40 character strings with the optional absolute reference time
!>                             - number of transported and passive substances
!>                             - the substance names (need not necessarily be process library reserved names)
!>                               the names may end with *nn to indicate a multiple occurence of the substance

!       Created           :    April    1988 by M.E. Sileon / L. Postma

!       Modified          :    April    1997 by R. Bruinsma: Tokenized input data file reading added
!                            9 November 2010 by L. Postma  : Multiple substances, F90 look and feel
!                           13 April    2011 by L. Postma  : Further streamlining tokenized reading

!       Subroutines called: rdvers  read version number from user input file
!                           check   for end of data block
!                           srstop  stop processing with return code
!                           zoek    to find previous occurences of the same name

!       Logical units     : lun(26) = unit user input file
!                           lun(27) = unit stripped input file
!                           lun(29) = unit formatted output file
!                           lun( 2) = unit system-intermediate file

      use m_zoek
      use m_srstop
      use m_rdvers
      use m_julian
      use rd_token     !   tokenized reading
      use dlwq0t_data
      use timers       !   performance timers
      use computeRefday

      implicit none

!     Parameters

!     kind           function         name                 description

      integer  ( 4), intent(in   ) :: lun  (*)          !< array with unit numbers
      character(20), pointer       :: syname(:)         !< array with substance names
      integer  ( 4), intent(  out) :: nosys             !< number of transported substances
      integer  ( 4), intent(  out) :: notot             !< total number of substances
      integer  ( 4), intent(  out) :: nomult            !< number of multiple substances
      integer  ( 4), pointer       :: multp (:,:)       !< multiple substance administration
      integer  ( 4), intent(  out) :: iwidth            !< width of the output file
      real     ( 8), intent(  out) :: otime             !< Offset of the system time (Julian)
      integer  ( 4), intent(  out) :: isfact            !< Units (in sec) of the system clock
      integer  ( 4), intent(  out) :: refday            !< reference day, varying from 1 till 365
      real     ( 4), intent(  out) :: vrsion            !< version number of this input
      integer  ( 4), intent(  out) :: ioutpt            !< flag for more or less output
      integer  ( 4), intent(inout) :: ierr              !< cumulative error   count
      integer  ( 4), intent(inout) :: iwar              !< cumulative warning count

!     Local

      integer  ( 4)   :: itype                             !  input type  0 = any, 1 = char, 2 = int, 3 = float
      integer  ( 4)   :: ierr2                             !  local error   accumulator
      integer  ( 4)   :: iwar2                             !  local warning accumulator
      character(40)   :: modid1, modid2,  runid1, runid2   !  model identification strings
      integer  ( 4)   :: idummy                            !  integer   read help variable
      real     ( 4)   :: adummy                            !  real      read help variable
      character(255)  :: cdummy                            !  character read help variable
      integer  ( 4)   :: isys, isys2                       !  loop counters for substances
      logical         :: intread                           !  flag for read of substance numbers
      integer  ( 4)   :: ilen                              !  length help variable
      integer  ( 4)   :: idate                             !  date of the time offset
      integer  ( 4)   :: itime                             !  time of the time offset
      integer  ( 4)   :: iyear                             !  year of the time offset
      integer  ( 4)   :: imonth                            !  month of the time offset
      integer  ( 4)   :: iday                              !  day of the time offset
      integer  ( 4)   :: ihour                             !  hour of the time offset
      integer  ( 4)   :: iminut                            !  minute of the time offset
      integer  ( 4)   :: isecnd                            !  second of the time offset
      integer  ( 4)   :: ifound                            !  help variable for name search
      integer  ( 4)   :: nosyss                            !  help variable for transported substance
      integer  ( 4)   :: notots                            !  help variable for total substance
      integer  ( 4), allocatable :: imult(:)            !  help array for number of substances
      character(20), allocatable :: sname(:)            !  help array for substance names
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "dlwq01", ithndl )


!        Initialize the read stack, output unit and error and warning help variables

      ierr2   = 0
      iwar2   = 0

!     First line not tokenized : meta data

      read ( lun(26) , * , end=110 , err=120 ) npos , iwidth , cchar
      if ( iwidth .ne. 80 .and. iwidth .ne. 132 ) then
         write ( lunut , 2000 ) iwidth
         iwidth = 80
      endif
      if ( iwidth .eq.  80 ) iwidth= 5
      if ( iwidth .eq. 132 ) iwidth=10

!     Read version number and initialize position on start of new line

      call rdvers ( ilun(1), lch(1) , lunut  , npos   , vrsion  ,
     &                                                  ioutpt  )
      iposr = 0

!     Read model documentation strings

      modid1 = ' '
      modid2 = ' '
      runid1 = ' '
      runid2 = ' '
      itype = 1
      if ( gettoken ( modid1, ierr2 ) .gt. 0 ) goto 100
      if ( gettoken ( modid2, ierr2 ) .gt. 0 ) goto 100
      write ( lun( 2)  ) modid1, modid2
      write ( lunut  , 2010 ) modid1, modid2
      if ( gettoken ( runid1, ierr2 ) .gt. 0 ) goto 100
      if ( gettoken ( runid2, ierr2 ) .gt. 0 ) goto 100
      write ( lun( 2)  ) runid1, runid2
      write ( lunut  , 2020 ) runid1, runid2

!     identify an timer offset value in the last string

      if ( runid2( 1: 3) .ne. 't0: ' .and.
     &     runid2( 1: 3) .ne. 'T0: ' .and.
     &     runid2( 1: 3) .ne. 't0= ' .and.
     &     runid2( 1: 3) .ne. 'T0= '       ) then
         write ( lunut  , 2030 )
         iwar  = iwar + 1
         isfact= 1
         otime = 0.0d+00
      else
         write ( lunut  , 2040 )
         read ( runid2( 5: 8) , '(i4)' ) iyear
         read ( runid2(10:11) , '(i2)' ) imonth
         read ( runid2(13:14) , '(i2)' ) iday
         read ( runid2(16:17) , '(i2)' ) ihour
         read ( runid2(19:20) , '(i2)' ) iminut
         read ( runid2(22:23) , '(i2)' ) isecnd
         read ( runid2(31:38) , '(i8)' ) isfact
         idate  = iyear*10000+imonth*100+iday
         itime  = ihour*10000+iminut*100+isecnd
         otime  = JULIAN ( idate , itime )
         if ( isfact .gt. 0 ) then
            write ( lunut  , 2050 )  isfact
         else
            write ( lunut  , 2060 ) -isfact
         endif
      endif

!     Compute refday
      call compute_refday(iyear, imonth, iday, refday)

!     Copy timers data to dlwqt0_data
      dlwq0t_otime  = otime 
      dlwq0t_isfact = isfact  

!     Read number of transported and number of passive systems

      if ( gettoken ( nosys , ierr2 ) .gt. 0 ) goto 100
      if ( gettoken ( idummy, ierr2 ) .gt. 0 ) goto 100
      notot = nosys + idummy

!     allocate

      allocate ( sname(notot) , imult(notot) )
      sname = ' '
      imult = 1

!        Read system numbers, identification and optional multiplication factor

      intread = .false.
      nomult  = 0
      do isys = 1, notot
         if ( .not. intread ) then                 ! read substance number
            if ( gettoken ( idummy, ierr2 ) .gt. 0 ) goto 100
         endif                                     ! read substanceID
         if ( gettoken ( cdummy, ierr2 ) .gt. 0 ) goto 100
         if ( idummy .le. 0 .or. idummy .gt. notot ) then
            write ( lunut , 2070 ) idummy, cdummy
            ierr = ierr+1
         else
            sname(idummy) = cdummy
         endif                                     ! read new substance nr or *n for multiples
         ifound = gettoken ( cdummy, idummy, adummy, itype, ierr2 )
         if ( isys .eq. notot .and. ifound .eq. 2 ) exit
         if ( ifound .ne. 0 ) goto 100
         if ( itype .eq. 1 ) then                  ! a string was found. must be *n
            if ( cdummy(1:1) .ne. '*' ) then       ! only a multiplication is accepted
               write( lunut, 2170 ) trim(cdummy)
               goto 100
            endif
            read ( cdummy(2:), * ) imult(isys)
            nomult = nomult + 1
            intread = .false.
         else                                      ! an integer is found
            intread = .true.
         endif
      enddo

!        Deal with multiple substances

      allocate ( multp(2,nomult) )
      nosyss = nosys
      notots = notot
      nosys  = 0
      notot  = 0
      nomult = 1
      do isys = 1, nosyss
         if ( imult(isys) .gt. 1 ) then
            multp(1,nomult) = nosys + 1
            multp(2,nomult) = nosys + 1 + imult(isys) - 1
            nomult = nomult + 1
         endif
         nosys = nosys + imult(isys)
         notot = notot + imult(isys)
      enddo
      do isys = nosyss+1, notots
         if ( imult(isys) .gt. 1 ) then
            multp(1,nomult) = notot + 1
            multp(2,nomult) = notot + 1 + imult(isys) - 1
            nomult = nomult + 1
         endif
         notot = notot + imult(isys)
      enddo
      allocate ( syname(notot+nomult) )

      write ( lunut  , 2080 ) nosys , notot-nosys , notot
      if ( nosys .lt. 0 .or. idummy .lt. 0 .or. notot .le. 0 ) then
           write ( lunut , 2090 )
           ierr = ierr+1
      endif

!        Fill in their names

      if ( ioutpt .ge. 1 ) then
         write ( lunut , 2100 )
      else
         write ( lunut , 2110 )
      endif
      nosyss = 1
      nomult = 0
      do isys = 1, notots

!        Get an unique name

         if ( sname(isys) .eq. ' ' ) write ( sname(isys), 2120 ) isys
         do isys2 = 1 , isys-1
            call ZOEK ( sname(isys), 1, sname(isys2:), 20, ifound )
            if ( ifound .gt. 0 ) then
               write(lunut,2130)
               ierr = ierr + 1
            endif
         enddo

!        Make the array with names

         if ( imult(isys) .eq. 1 ) then
            syname(nosyss) = sname(isys)
            if ( ioutpt .ge. 1 ) write ( lunut, 2140 ) nosyss, syname(nosyss)
            nosyss = nosyss + 1
         else
            nomult = nomult + 1
            syname(notot+nomult) = sname(isys)
            do isys2 = 1, imult(isys)
               syname(nosyss) = sname(isys)
               ilen = len_trim(syname(nosyss))
               if ( isys2 .lt.   10 ) then
                  write ( syname(nosyss)(ilen+1:), '(''0'',i1)' ) isys2
               elseif ( isys2 .lt. 100 ) then
                  write ( syname(nosyss)(ilen+1:), '(      i2)' ) isys2
               else
                  write ( syname(nosyss)(ilen+1:), '(      i3)' ) isys2
               endif
               if ( ioutpt .ge. 1 ) write ( lunut, 2140 ) nosyss, syname(nosyss)
               nosyss = nosyss + 1
            enddo
         endif
      enddo

!        Watch out !! in dlwq02.f subsequently the names of the particle tracking
!                     substances are written to lun(2)

      write ( lun(2)     ) ( syname(isys), isys=1, notot )

!        Check number of data in inputfile

  100 continue
      if ( ierr2 .ne. 0 .and. ierr2 .ne. 2 ) ierr = ierr + 1
      if ( ierr2 .eq. 3 ) call srstop(1)
      call check  ( cdummy, iwidth , 1      , ierr2  , ierr   )
      iwar = iwar + iwar2
      if ( timon ) call timstop( ithndl )
      return

  110 write ( lunut , 2150 ) ilun(1) , lch(1)
      call srstop ( 1 )
      return

  120 write ( lunut , 2160 ) ilun(1) , lch(1)
      call srstop ( 1 )
      return

!        Output formats

 2000 format ( /' Invalid width of output file',I4,'! A width of 80 is assumed!')
 2010 format (//' Model :            ',A40,/20X,A40 )
 2020 format (//' Run   :            ',A40,/20X,A40 )
 2030 format ( /' WARNING. No intermediate version absolute timer detected !',
     &         /'          Use of date/time is not supported - date/time strings',
     &         /'          may not be interpreted correctly')
 2040 format ( /' Intermediate version absolute timer detected !' )
 2050 format (  ' System clock is ',I5,' seconds !' )
 2060 format (  ' System clock is 1/',I3,'th of a second !' )
 2070 format ( /' ERROR invalid system number:',I3,5X,A20 )
 2080 format ( /' Number of active constituents   :',I3,/
     &          ' Number of inactive constituents :',I3,/
     &          ' Total number of constituents    :',I3  )
 2090 format ( /' ERROR invalid number of systems.' )
 2100 format (  ' Number   name' )
 2110 format (  ' Information on substances will be printed for',
     &          ' output option 1 or higher !' )
 2120 format (  ' Substance ',I3 )
 2130 format ( /' ERROR. system name not unique')
 2140 format (      I5 , 5X , A20 )
 2150 format ( /' ERROR. End of file on unit:',I3,/' Filename = ',A )
 2160 format ( /' ERROR reading file on unit:',I3,' - first line invalid. Please check',/' Filename = ',A )
 2170 format ( /' ERROR encountered invalid repeat count - should start with an asterisk (*): ',A )

      end
