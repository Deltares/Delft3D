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
      module m_gkwini
      use m_zoek

      implicit none

      contains


      subroutine gkwini ( lu , group , keywrd , value )
!
      use timers
      integer       lu
      character*(*) group, keywrd, value

      logical       gropen, grclos
      character*256 lline , groupl, keywrl, valuel
      integer       lgrpin, lkeyin, lvalin,
     j              il    , ierr  , lcomp , index
      integer(4) ithndl /0/
      if ( timon ) call timstrt( "gkwini", ithndl )

      rewind (lu)
      gropen = .false.
      grclos = .false.
      lgrpin = len(group)
      lkeyin = len(keywrd)
      lvalin = len(value)
      value  = ' '

!     Read line

   10 continue
      lline = ' '
      read ( lu , '(a)' , end = 90 , err = 90 ) lline

!     Check for group separator

      call gettko ( lline , 256 , '[' , ']' , groupl , il , ierr )
      if ( ierr .eq. 0 .and. il .gt. 0 ) then

!         Group separator found

          lcomp = min ( il , lgrpin )
          call zoek (groupl(1:lcomp), 1, group(1:lcomp), lcomp, index)
          if ( index .eq. 1 ) then

!             Group name equals requested group

              gropen = .true.
          else

!             Group name does not equal requested group

              if ( gropen ) grclos = .true.
          endif

!         If requested group is passed, finish looking

          if ( grclos ) goto 90
      else

!         Check for keyword if Group is open

          if ( gropen ) then
              call gettko (lline,256,'*','=',keywrl,il,ierr)
              if ( ierr .eq. 0 .and. il .gt. 0 ) then

!                 Keyword found

                  lcomp = min ( il , lkeyin )
                  call zoek
     j            (keywrl(1:lcomp),1,keywrd(1:lcomp),lcomp,index)
                  if ( index .eq. 1 ) then

!                     Keyword equals requested keyword

                      call gettko
     j                ( lline , 256 , '=' , '*' , valuel, il , ierr )
                      if ( ierr .eq. 0 .and. il .gt. 0 ) then

!                         Value succesfully read

                          value = ' '
                          lcomp = min ( il , lvalin )
                          value(1:lcomp) = valuel(1:lcomp)
                          goto 90
                      endif
                  endif
              endif
          endif
      endif

!     Go for next line

      goto 10

   90 continue
      if ( timon ) call timstop( ithndl )
      return
      end

      SUBROUTINE GETTKO ( LINE , IN , CL , CT , TOKEN , IL , IERR )
!
!     LINE      INPUT STRING
!     IN        LENGTH OF INPUT STRING
!     CL        LEADING SEPARATOR ( '*' IS FROM BEGIN OF STRING)
!     CT        TRAILING SEPARATOR ( '*' IS TILL END OF STRING)
!     TOKEN     STRING BETWEEN SEPARATORS
!     IL        LENGTH OF STRING BETWEEN SEPARATORS

      USE Timers
      CHARACTER*(*) LINE
      CHARACTER*(*) TOKEN
      CHARACTER*1   CL,CT
      CHARACTER*10  CFORMA
      INTEGER IN,IL,IERR
      INTEGER I1,I2

      integer(4) ithndl /0/
      if ( timon ) call timstrt( "gettko", ithndl )

      TOKEN = ' '
      IERR = 0

!     If necessary find leading separator

      I1 = 1

      IF ( CL .NE. '*' ) THEN
          I1 = 2
    1     IF ( LINE(I1-1:I1-1) .EQ. CL ) GOTO 2
          I1 = I1+1
          IF ( I1 .GT. IN ) THEN
              IERR = 1
              goto 9999
          ENDIF
          GOTO 1
    2     CONTINUE
      ENDIF

!     Skip leading blanks

    3 IF ( LINE(I1:I1) .NE. ' ' ) GOTO 4
      I1 = I1+1
      IF ( I1 .GT. IN ) THEN
          IERR = 2
          goto 9999
      ENDIF
      GOTO 3
    4 CONTINUE

!     If necessary find trailing seperator

      I2 = IN

      IF ( CT .NE. '*' ) THEN
          I2 = I1
    5     IF ( LINE(I2+1:I2+1) .EQ. CT ) GOTO 6
          I2 = I2+1
          IF ( I2 .EQ. IN ) THEN
              IERR = 3
              goto 9999
          ENDIF
          GOTO 5
    6     CONTINUE
      ENDIF

!     Skip trailing blanks

    7 IF ( LINE(I2:I2) .NE. ' ' ) GOTO 8
      I2 = I2-1
      IF ( I2 .LT. I1 ) THEN
          IERR = 4
          goto 9999
      ENDIF
      GOTO 7
    8 CONTINUE

      IL = I2 - I1 + 1
      CALL CHARFO ( CFORMA , IL )
      WRITE ( TOKEN(1:IL) , CFORMA ) LINE(I1:I2)
 9999 if ( timon ) call timstop( ithndl )
      RETURN
      END

      SUBROUTINE CHARFO ( CFORMA , IL )
      CHARACTER*(*) CFORMA
      INTEGER IL
      CFORMA = ' '
      WRITE ( CFORMA , '(''(A'',I3.3,'')'')' ) IL
      RETURN
      END
      subroutine gi_ini ( lu , group , keywrd , ivalue , found)
      integer       lu
      character*(*) group, keywrd
      logical, optional :: found
      integer       ivalue, ierr

      ! local decalarations

      character*256 value

      call gkwini ( lu , group , keywrd , value )
      if ( value .ne. ' ' ) then
         found = .true.
         read(value,'(i256)',iostat=ierr) ivalue
         if ( ierr .ne. 0 ) then
            ivalue = -999
         endif
      else
         found = .false.
         ivalue = -999
      endif

      return
      end
      subroutine gr_ini ( lu , group , keywrd , rvalue, found )
      integer       lu , ierr
      character*(*) group, keywrd
      logical, optional :: found
      real          rvalue

      ! local decalarations

      character*256 value

      call gkwini ( lu , group , keywrd , value )
      if ( value .ne. ' ' ) then
         found = .true.
         read(value,'(f256.0)',iostat=ierr) rvalue
         if ( ierr .ne. 0 ) then
            rvalue = -999.
         endif
      else
         found = .false.
         rvalue = -999.
      endif

      return
      end
      subroutine gl_ini ( lu , group , keywrd , lvalue, found )
      integer       lu
      character*(*) group, keywrd
      logical, optional :: found
      logical       lvalue

      ! local decalarations

      character*256 value
      integer       ifound

      call gkwini ( lu , group , keywrd , value )

      lvalue = .false.

      if ( value .ne. ' ' ) then

         found = .true.
         if (.not. lvalue) call zoek('true ',1,value,5,ifound)
         if ( ifound .eq. 1 ) lvalue = .true.

         if (.not. lvalue) call zoek('yes  ',1,value,5,ifound)
         if ( ifound .eq. 1 ) lvalue = .true.

         if (.not. lvalue) call zoek('1    ',1,value,5,ifound)
         if ( ifound .eq. 1 ) lvalue = .true.

      else
         found = .false.
      endif

      return
      end
      end module m_gkwini
