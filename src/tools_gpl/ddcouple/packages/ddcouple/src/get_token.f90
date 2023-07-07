!!  Copyright (C)  Stichting Deltares, 2021-2023.
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

      subroutine get_token ( inpfil )
!
!     Deltares     sector waterresources and environment
!
!     created: may  - 1996 by l. postma
!
!     function            : gets the next token from line
!                           reads new line if no token left
!                           returns the type of the token and the value
!                           strips quotes from strings
!
!     logical unitnumbers : lun - input file to read lines from
!
!     subroutines called  : none
!

      ! global definitions

      use dlwqdata_mod

      ! declaration of arguments

      type(inputfilestack)  , intent(inout) :: inpfil       ! input file strucure with include stack

!
!          local important aspects
!
!     lun     integer       1     input   logical unitnumber input file
!     line    char*(*)      1     in/out  space to store line to read
!     iposl   integer       1     output  left  position in line of token
!     iposr   integer       1     output  right position in line of token
!     iwidth  integer       1     input   line length of input lines
!     comc    character*1   1     input   comment character
!     grpsep  character*1   1     input   group separation character
!     ierr    integer       1     output  > 0 error during processing
!     limc    character*1   1      limit char. for token, space ' and "
!     num     logical       1      true if a number was detected
!     dot     logical       1      true if a decimal point was detected
!     exp     logical       1      true if an 'e','e','d' or 'd' detected
!
      character(len=1024) :: line
      character*1   comc, grpsep
      character   limc*1
      character*1 ctrlz , chtab , ch_cr
      logical     num , dot , exp
!
      chtab = char(9)
      ch_cr = char(13)
      ctrlz = char(26)
!
      lun    = inpfil%inplun(inpfil%inputf)
      line   = inpfil%linbuf(inpfil%inputf)
      iposl  = inpfil%iposl
      iposr  = inpfil%iposr
      iwidth = inpfil%npos
      comc   = inpfil%cchar
      grpsep = inpfil%grpsep

!    +                  inpfil%ctoken               ,
!    +                  inpfil%itoken               ,
!    +                  inpfil%rtoken               ,
!    +                  inpfil%ierr                 )

      ! see if we are in a repeat cycle

      if ( inpfil%nrepeat .ne. 0 ) then
         inpfil%nrepeat = inpfil%nrepeat - 1
         return
      endif

      ! read an initialisation line

      num   = .false.
      inpfil%t_token = 0
      ierr  = 0
   10 if ( iposr .eq. 0 ) then
         read ( lun , '(a)' , end=100 , err=110 ) line
         iposr = 0
         inpfil%nrepeat  = 0
      endif
      iposr = iposr + 1

      ! get a non-space character

      do 20 i = iposr , iwidth
         iposl = i
         if ( line(i:i) .ne. ' '   .and. &
              line(i:i) .ne. ctrlz .and. &
              line(i:i) .ne. ch_cr .and. &
              line(i:i) .ne. chtab      ) goto 30
   20 continue
      iposr = 0
      goto 10

      ! character found, see if it is a ' or "

   30 if ( line(iposl:iposl) .eq. comc ) then
         iposr = 0
         goto 10
      endif
      limc  = ' '
      i = iposl
      if ( line(i:i) .eq. '''' .or. line(i:i) .eq. '"' ) then
         limc  = line(i:i)
         inpfil%t_token = -1
      endif

      ! find the end of the token

      iposr = iposl
      do 40 i = iposl+1 , iwidth
         if ( limc .ne. ' ' ) then
            if ( line(i:i) .eq. limc .or. line(i:i) .eq. comc ) goto 50
         else
            if ( line(i:i) .eq. ' '   .or. &
                 line(i:i) .eq. ctrlz .or. &
                 line(i:i) .eq. ch_cr .or. &
                 line(i:i) .eq. chtab .or. &
                 line(i:i) .eq. comc       ) goto 50
         endif
         iposr = i
   40 continue
!         no delimiting quote found
      if ( limc .ne. ' ' ) then
         inpfil%ctoken = line(iposl:iposr)
         ierr  = -1
         iposr = iwidth
         inpfil%nrepeat  =  0
         goto 95
      endif
!
!         detect what the token is
!
!     a delimited string (with optionally embedded blanks)
   50 if ( limc .ne. ' ' ) then
         iposl = iposl + 1
         inpfil%ctoken = line(iposl:iposr)
         iposr = iposr + 1
         goto 95
      endif


!     see if it is an integer (it is allowed to start with a sign)
      inpfil%ctoken = line(iposl:iposr)
      do 60 i = iposl , iposr
         j = ichar(line(i:i))
         if (   i .eq. iposl .and. &
              ( line(i:i) .eq. '+' .or. line(i:i) .eq. '-' ) ) then
             if ( iposl .eq. iposr ) goto 90
             goto 60
         endif
         if ( line(i:i) .eq. '*' .and. inpfil%nrepeat .ne. 0 ) then
            inpfil%nrepeat  = 0
            iposl = iposls
            goto 90
         endif
         if ( line(i:i) .eq. '*' .and. num ) then
            read ( line(iposl:i-1) , '(i40)' ) inpfil%nrepeat
            iposls = iposl
            iposr  = i
            inpfil%nrepeat   = inpfil%nrepeat - 1
            goto 10
         endif
         if ( j .lt. ichar('0') .or. j .gt. ichar('9') ) goto 70
         num = .true.
   60 continue
      read ( line(iposl:iposr) , '(i40)',err=120) inpfil%itoken
      inpfil%t_token = 2
      goto 95


!     see if it is a real ( this one is the hardest )
   70 iexp =  iposl-1
      dot  = .false.
      exp  = .false.
      num  = .false.
      do 80 i = iposl , iposr
         limc = line(i:i)
!        sign only at the beginning or right after exponent
         if (  limc .eq. '+' .or. limc .eq. '-' )  then
            if ( iexp .ne. i-1 ) goto 90
            goto 80
         endif
!        only one dot allowed, before the exponent
         if ( limc .eq. '.' ) then
            if ( dot .or. exp ) goto 90
            dot = .true.
            goto 80
         endif
!        only one exp allowed, after at least one numeric value
         if ( ( limc .eq. 'e' .or. limc .eq. 'E' .or. &
                limc .eq. 'd' .or. limc .eq. 'D'      ) ) then
            if  ( exp .or. .not. num ) goto 90
            exp  = .true.
            iexp = i
            do 75 i2 = i+1 , iposr
               j = ichar(line(i2:i2))
               if (   i2 .eq. i+1 .and. &
                    ( line(i2:i2) .eq. '+' .or. &
                      line(i2:i2) .eq. '-'      )  ) goto 75
               if ( j .lt. ichar('0') .or. j .gt. ichar('9') ) goto 90
   75       continue
            read ( line(i+1:iposr) , '(i40)' ) ihlp
!jvb        wel underflow toelaten, wordt nul ofzo
!jvb        if ( ihlp .lt. -30 .or. ihlp .gt. +30 ) then
            if ( ihlp .gt. +30 ) then
               ierr = -3
               goto 90
            else
               goto 85
            endif
         endif
!        now only numerical values should remain
         j   = ichar(limc)
         if ( j .lt. ichar('0') .or. j .gt. ichar('9') ) goto 90
         num = .true.
   80 continue
   85 read ( line(iposl:iposr) , '(e40.0)' ,err=130 ) inpfil%rtoken
      inpfil%t_token = 3
      goto 95
!     it apparently is a string
   90 inpfil%ctoken = line(iposl:iposr)
      if ( line(iposl:iposl) .eq. grpsep ) then
         ierr = -2
      else
         inpfil%t_token = 1
      endif
   95 continue
      inpfil%linbuf(inpfil%inputf) = line
      inpfil%iposr  = iposr
      inpfil%iposl  = iposl
      inpfil%ierr   = ierr
      return
!
  100 inpfil%ierr = 1
      inpfil%linbuf(inpfil%inputf) = line
      inpfil%iposr  = iposr
      inpfil%iposl  = iposl
      return
!
  110 inpfil%ierr = 2
      inpfil%linbuf(inpfil%inputf) = line
      inpfil%iposr  = iposr
      inpfil%iposl  = iposl
      return
!
!     integer overflow of underflow
!
  120 inpfil%ierr = -4
      inpfil%linbuf(inpfil%inputf) = line
      inpfil%iposr  = iposr
      inpfil%iposl  = iposl
      return
!
!     real overflow of underflow
!
  130 inpfil%ierr = -3
      inpfil%linbuf(inpfil%inputf) = line
      inpfil%iposr  = iposr
      inpfil%iposl  = iposl
      return
!
      end
