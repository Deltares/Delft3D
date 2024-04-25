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

      subroutine gettok ( lun    , line   , str    , int    , reel   ,
     &                    itype  , iposl  , iposr  , iwidth , comc   ,
     &                                      grpsep , lineno , ierr   )

!     Deltares Software Centre

!>\file
!>       Parses a token from the input line
!>
!>               - Gets the next token from line
!>               - reads new line if no token left
!>               - returns the type of the token and the value
!>               - strips quotes from strings

!     Created             : May  - 1996 by L. Postma

!     Logical unitnumbers : LUN - input file to read lines from

!     Subroutines called  : none

      implicit none

!     Parameters

!     kind          function         name                Descriptipon

      integer  (4), intent(in   ) :: lun               !< logical unitnumber input file
      character(*), intent(inout) :: line              !< space to store line to read
      character(*), intent(  out) :: str               !< token if itype = 1
      integer  (8), intent(  out) :: int               !< token if itype = 2
      real     (8), intent(  out) :: reel              !< token if itype = 3
      integer  (4), intent(  out) :: itype             !< type of the token ( 0 = none, 1 = character, 2 = integer, 3 = real )
      integer  (4), intent(  out) :: iposl             !< left  position in LINE of token
      integer  (4), intent(  out) :: iposr             !< right position in LINE of token
      integer  (4), intent(in   ) :: iwidth            !< Line length of input lines
      character(1), intent(in   ) :: comc              !< Comment character
      character(1), intent(in   ) :: grpsep            !< Group separation character
      integer  (4), intent(inout) :: lineno            !< line number in file
      integer  (4), intent(  out) :: ierr              !< not 0 error during processing

!          local variables

      character(  1) limc        !   Limit char. for token, space ' and "
      logical        num         !   True if a number was detected
      logical        dot         !   True if a decimal point was detected
      logical        exp         !   True if an 'E','e','D' or 'd' detected
      integer        ntot        !   Repeat factor of a token
      integer        itypes      !   Stored previous type
      character(128) strs        !   Stored previous string
      integer  (  8) ints        !   Stored previous integer
      real     (  8) reels       !   Stored previous real
      integer        iposls      !   Stored previous left
      integer        iposrs      !   Stored previous right
      character(  1) ctrlz       !   Ctrl_Z character
      character(  1) chtab       !   Tab character
      character(  1) ch_cr       !   Cariage return character
      integer        i, i2, j    !   loop counters
      integer        iexp, ihlp  !   help variables in the parsing process

      save
      data        ntot / 0 /

      chtab = char(9)
      ch_cr = char(13)
      ctrlz = char(26)
      str  = ' '
      int  = 0
      reel = 0.0

!         see if we are in a repeat cycle

      if ( ntot .gt. 0 ) then
         itype = itypes
         str   = strs
         int   = ints
         reel  = reels
         iposl = iposls
         iposr = iposrs
         ntot  = ntot - 1
         return
      endif

!         read an initialisation line

      num   = .false.
      itype = 0
      ierr  = 0
   10 if ( iposr .eq. 0 ) then
         read ( lun , '(a)' , end=100 , err=110 ) line
         iposr  = 0
         ntot   = 0
         lineno = lineno + 1
      endif
      iposr = iposr + 1

!         get a non-space character

      do i = iposr , iwidth
         iposl = i
         if ( line(i:i) .ne. ' '   .and.
     &        line(i:i) .ne. ctrlz .and.
     &        line(i:i) .ne. ch_cr .and.
     &        line(i:i) .ne. chtab      ) goto 30
      enddo
      iposr = 0
      goto 10

!         character found, see if it is a ' or "

   30 if ( line(iposl:iposl) .eq. comc ) then
         iposr = 0
         goto 10
      endif
      limc  = ' '
      i = iposl
      if ( line(i:i) .eq. '''' .or. line(i:i) .eq. '"' ) then
         limc  = line(i:i)
         itype = -1
      endif

!         find the end of the token

      iposr = iposl
      do i = iposl+1 , iwidth
         if ( limc .ne. ' ' ) then
            if ( line(i:i) .eq. limc ) goto 50
         else
            if ( line(i:i) .eq. ' '   .or.
     &           line(i:i) .eq. ctrlz .or.
     &           line(i:i) .eq. ch_cr .or.
     &           line(i:i) .eq. chtab .or.
     &           line(i:i) .eq. comc       ) goto 50
         endif
         iposr = i
      enddo

!         no delimiting quote found

      if ( limc .ne. ' ' ) then
         str   = line(iposl:iposr)
         ierr  = -1
         iposr = iwidth
         ntot  =  0
         goto 95
      endif

!         detect what the token is

!     a delimited string (with optionally embedded blanks)
   50 if ( limc .ne. ' ' ) then
         iposl = iposl + 1
         str   = line(iposl:iposr)
         iposr = iposr + 1
         goto 95
      endif


!     see if it is an integer (it is allowed to start with a sign)
      str   = line(iposl:iposr)
      do i = iposl , iposr
         j = ichar(line(i:i))
         if (   i .eq. iposl .and.
     &        ( line(i:i) .eq. '+' .or. line(i:i) .eq. '-' ) ) then
             if ( iposl .eq. iposr ) goto 90
             cycle
         endif
         if ( line(i:i) .eq. '*' .and. ntot .ne. 0 ) then
            ntot  = 0
            iposl = iposls
            goto 90
         endif
         if ( line(i:i) .eq. '*' .and. num ) then
            read ( line(iposl:i-1) , '(i40)' ) ntot
            if (ntot .le. 0 ) goto 140
            iposls = iposl
            iposr  = i
            ntot   = ntot - 1
            goto 10
         endif
         if ( j .lt. ichar('0') .or. j .gt. ichar('9') ) goto 70
         num = .true.
      enddo
      read ( line(iposl:iposr) , '(i40)',err=130) int
      itype = 2
      goto 95


!     see if it is a real ( this one is the hardest )
   70 iexp =  iposl-1
      dot  = .false.
      exp  = .false.
      num  = .false.
      do i = iposl , iposr
         limc = line(i:i)
!        sign only at the beginning or right after exponent
         if (  limc .eq. '+' .or. limc .eq. '-' )  then
            if ( iexp .ne. i-1 ) goto 90
            cycle
         endif
!        only one dot allowed, before the exponent
         if ( limc .eq. '.' ) then
            if ( dot .or. exp ) goto 90
            dot = .true.
            cycle
         endif
!        only one exp allowed, after at least one numeric value
         if ( ( limc .eq. 'e' .or. limc .eq. 'E' .or.
     &          limc .eq. 'd' .or. limc .eq. 'D'      ) ) then
            if  ( exp .or. .not. num ) goto 90
            exp  = .true.
            iexp = i
            do i2 = i+1 , iposr
               j = ichar(line(i2:i2))
               if (   i2 .eq. i+1 .and.
     &              ( line(i2:i2) .eq. '+' .or.
     &                line(i2:i2) .eq. '-'      )  ) cycle
               if ( j .lt. ichar('0') .or. j .gt. ichar('9') ) goto 90
            enddo
            read ( line(i+1:iposr) , '(i40)' ) ihlp
!jvb        if ( ihlp .lt. -30 .or. ihlp .gt. +30 ) then
            if ( ihlp .gt. +30 ) then
               ierr = -3
               goto 90
            else
               exit
            endif
         endif
!        now only numerical values should remain
         j   = ichar(limc)
         if ( j .lt. ichar('0') .or. j .gt. ichar('9') ) goto 90
         num = .true.
      enddo
      read ( line(iposl:iposr) , *, err=120 ) reel
      itype = 3
      goto 95
!     it apparently is a string
   90 str   = line(iposl:iposr)
      if ( line(iposl:iposl) .eq. grpsep ) then
         ierr = -2
      else
         itype = 1
      endif
   95 itypes = itype
      strs   = str
      ints   = int
      reels  = reel
      iposls = iposl
      iposrs = iposr
      return

  100 ierr = 1
      return

  110 ierr = 2
      return

!     real overflow of underflow

  120 ierr = -3
      return

!     integer overflow of underflow

  130 ierr = -4
      return

!     negative or zero repeats(*)

  140 ierr = -5
      return

      end
