subroutine gkwini ( lu , group , keywrd , value )
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Parse New Model Database (V2.0)
!
! Programmer:         J.A.G. van Gils
!
! Module:             Get keyword from ini file
!
! Module description: Reads a keyword from a "windows" type ini file
!
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gkwini.pf,v $
! Revision 1.1  1998/02/13  13:23:41  kuipe_j
! Adapt to CMT
!
!
!***********************************************************************
   integer       lu
   character*(*) group, keywrd, value

   logical       gropen, grclos
   character*256 lline , groupl, keywrl, valuel
   integer       lgrpin, lkeyin, lvalin,&
   &il    , ierr  , lcomp , index

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
            call zoek&
            &(keywrl(1:lcomp),1,keywrd(1:lcomp),lcomp,index)
            if ( index .eq. 1 ) then

!                     Keyword equals requested keyword

               call gettko&
               &( lline , 256 , '=' , '*' , valuel, il , ierr )
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

   CHARACTER*(*) LINE
   CHARACTER*(*) TOKEN
   CHARACTER*1   CL,CT
   CHARACTER*10  CFORMA
   INTEGER IN,IL,IERR

   INTEGER I1,I2
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
         RETURN
      ENDIF
      GOTO 1
2     CONTINUE
   ENDIF

!     Skip leading blanks

3  IF ( LINE(I1:I1) .NE. ' ' ) GOTO 4
   I1 = I1+1
   IF ( I1 .GT. IN ) THEN
      IERR = 2
      RETURN
   ENDIF
   GOTO 3
4  CONTINUE

!     If necessary find trailing seperator

   I2 = IN

   IF ( CT .NE. '*' ) THEN
      I2 = I1
5     IF ( LINE(I2+1:I2+1) .EQ. CT ) GOTO 6
      I2 = I2+1
      IF ( I2 .EQ. IN ) THEN
         IERR = 3
         RETURN
      ENDIF
      GOTO 5
6     CONTINUE
   ENDIF

!     Skip trailing blanks

7  IF ( LINE(I2:I2) .NE. ' ' ) GOTO 8
   I2 = I2-1
   IF ( I2 .LT. I1 ) THEN
      IERR = 4
      RETURN
   ENDIF
   GOTO 7
8  CONTINUE

   IL = I2 - I1 + 1
   CALL CHARFO ( CFORMA , IL )
   WRITE ( TOKEN(1:IL) , CFORMA ) LINE(I1:I2)
   RETURN
END

SUBROUTINE CHARFO ( CFORMA , IL )
   CHARACTER*(*) CFORMA
   INTEGER IL
   CFORMA = ' '
   WRITE ( CFORMA , '(''(A'',I3.3,'')'')' ) IL
   RETURN
END
