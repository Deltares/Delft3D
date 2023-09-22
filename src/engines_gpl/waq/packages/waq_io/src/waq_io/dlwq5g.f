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
      module m_dlwq5g

      implicit none

      contains


      SUBROUTINE DLWQ5G ( LUNUT, I_ARRAY, COUNT_ITEMS_ASSIGN , COUNT_ITEMS_COMP_RULE  , COUNT_SUBS_ASSIGN,
     *                    COUNT_SUBS_COMP_RULE, INDEX_FIRST, I_MAX , NAMES_TO_CHECK , START_IN_LINE,
     *                    NPOS , ILUN  , LCH   , LSTACK , CCHAR,
     *                    CHULP, NOCOL , DTFLG1, DTFLG3 , ITFACT,
     *                    ITYPE, IHULP , RHULP , ERROR_IDX, IWAR)
!
!
!     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED            : March '00  by L. Postma
!
!     MODIFIED           :
!
!     FUNCTION           : Checks if column header exists
!
!     SUBROUTINES CALLED : none
!
!     LOGICAL UNITS      : LUN(27) = unit stripped DELWAQ input file
!                          LUN(29) = unit formatted output file
!
!     PARAMETERS    :
!
!     NAME                    KIND         LENGTH     FUNCT.  DESCRIPTION
!     ------------------------------------------------------------------------------------
!     LUNUT                  INTEGER        1            INPUT   unit number for ASCII output
!     I_ARRAY                INTEGER      I_MAX          IN/OUT  integer workspace array
!     I_MAX                  INTEGER        1            INPUT   max. integer workspace dimension
!     COUNT_ITEMS_ASSIGN     INTEGER        1            IN/OUT  number of items for assignment
!     COUNT_ITEMS_COMP_RULE  INTEGER        1            IN      number of items in computational rule
!     COUNT_ITEMS_ENTRIES    INTEGER        1            IN      number of items entries to be filled
!     COUNT_SUBS_ASSIGN      INTEGER        1            IN/OUT  number of subst for assignment
!     COUNT_SUBS_COMP_RULE   INTEGER        1            IN      number of subst in computational rule
!     COUNT_SUBS_ENTRIES     INTEGER        1            IN      number of subst entries to be filled
!     INDEX_FIRST            INTEGER        1            IN      1 = items first, 2 = substances first
!     NAMES_TO_CHECK         CHAR*(*)      COUNT_NAMES   INPUT   names of items to check for presence
!     COUNT_NAMES            INTEGER        1            IN/OUT  Start position on input line
!     START_IN_LINE          INTEGER        1            IN/OUT  Start position on input line
!     NPOS                   INTEGER        1            INPUT   number of significant characters
!     ILUN                   INTEGER       LSTACK        INPUT   unitnumb include stack
!     LCH                    CHAR*(*)      LSTACK        INPUT   file name stack, 4 deep
!     LSTACK                 INTEGER        1            INPUT   include file stack size
!     CCHAR                  CHAR*1         1            INPUT   comment character
!     CHULP                  CHAR*(*)       1            OUTPUT  space for limiting token
!     NOCOL                  INTEGER        1            OUTPUT  number of collums in matrix
!     DTFLG1                 LOGICAL        1            INPUT   True if time in 'date' format
!     DTFLG3                 LOGICAL        1            INPUT   True if YYetc instead of DDetc
!     ITFACT                 INTEGER        1            INPUT   factor between clocks
!     ITYPE                  INTEGER        1            OUTPUT  type of info at end
!     IHULP                  INTEGER        1            OUTPUT  parameter read to be transferred
!     RHULP                  REAL           1            OUTPUT  parameter read to be transferred
!     ERROR_IDX              INTEGER        1            OUTPUT  error index within current subroutine
!     IWAR                   INTEGER        1            OUTPUT  cumulative warning count
!     OFFSET_I_ARRAY         INTEGER        1            OUTPUT  offset  in I_ARRAY
!     OFFSET_NAMES           INTEGER        1            OUTPUT  offset in NAMES_TO_CHECK
!     OFFSET_COMMON          INTEGER        1            OUTPUT  comon offset in I_ARRAY and NAMES_TO_CHECK
!
!
      use m_dlwq5h
      use m_zoek
      use timers       !   performance timers
      use m_cnvtim

      INTEGER       I_MAX
      CHARACTER*(*) LCH   (LSTACK) , CHULP , NAMES_TO_CHECK(*)
      CHARACTER     CCHAR*1 , STRNG*8
      DIMENSION     I_ARRAY(*) , ILUN( LSTACK )
      LOGICAL       DTFLG1 , DTFLG3 , FIRST, MUST_READ_MORE
      integer ( 8)  ihulp8
      integer(4) :: ithndl = 0
      integer    :: I, COUNT_ITEMS_COMP_RULE, COUNT_SUBS_ASSIGN, COUNT_SUBS_COMP_RULE, INDEX_FIRST, OFFSET_NAMES, OFFSET_COMMON, notim
      integer    :: itype, lunut, ilun, START_IN_LINE, nopos, ihulp, ERROR_IDX
      integer    :: I_ARRAY, nocol, ifound, itfact, icnt, iods, k, IWAR
      integer    :: OFFSET_I_ARRAY, COUNT_ITEMS_ASSIGN, COUNT_NAMES, npos, lstack
      real       :: rhulp
      
      integer :: KKVACA
      
      
      if (timon) call timstrt( "dlwq5g", ithndl )
!
!     Array offsets
!
      OFFSET_I_ARRAY = COUNT_ITEMS_ASSIGN + COUNT_ITEMS_COMP_RULE + 
     +                 COUNT_SUBS_ASSIGN  + COUNT_SUBS_COMP_RULE
      IF ( INDEX_FIRST .EQ. 1 ) THEN ! items first
         OFFSET_NAMES  = COUNT_ITEMS_ASSIGN + COUNT_ITEMS_COMP_RULE + COUNT_SUBS_ASSIGN
         OFFSET_COMMON = COUNT_ITEMS_ASSIGN + COUNT_ITEMS_COMP_RULE
         COUNT_NAMES   = COUNT_SUBS_COMP_RULE
      ELSE IF ( INDEX_FIRST .EQ. 2 ) THEN !substances first
         OFFSET_NAMES  = COUNT_ITEMS_ASSIGN + COUNT_SUBS_COMP_RULE + COUNT_SUBS_ASSIGN 
         OFFSET_COMMON = COUNT_SUBS_COMP_RULE + COUNT_SUBS_ASSIGN
         COUNT_NAMES   = COUNT_ITEMS_COMP_RULE
      ENDIF
!
!     Read loop
!
      FIRST = .TRUE.
      MUST_READ_MORE = .TRUE.
      DO WHILE (MUST_READ_MORE)
          ITYPE = 0
          CALL RDTOK1 ( LUNUT, ILUN, LCH, LSTACK, CCHAR,
     *              START_IN_LINE, NPOS, CHULP, IHULP, RHULP,
     *              ITYPE  , ERROR_IDX)
         
          IF ( ERROR_IDX  .NE. 0 ) THEN ! Error occurred when reading
              if (timon) call timstop( ithndl )
              RETURN !exit subroutine
          END IF
         
!         No error
          IF ( ITYPE .EQ. 1 ) THEN ! A string has arrived
             ! get time (ihulp) from string (CHULP)
             CALL DLWQ0T ( CHULP , ihulp, .FALSE., .FALSE., ERROR_IDX )
             IF ( ERROR_IDX .EQ. 0 ) THEN
                ERROR_IDX = -2
                IF ( FIRST ) THEN
                   if (timon) call timstop( ithndl )
                   RETURN  !exit subroutine
                ELSE
                   EXIT !GOTO 50
                ENDIF
             ENDIF
             IF ( FIRST ) THEN
                FIRST = .FALSE.
                DO I = 1 , COUNT_NAMES
                   I_ARRAY(OFFSET_I_ARRAY + I) = 0
                END DO
                NOCOL = 0
                WRITE ( LUNUT ,   *  )
             ENDIF
             NOCOL = NOCOL + 1
             STRNG = 'NOT used'
             DO I = 1 , COUNT_NAMES
                CALL ZOEK(CHULP,1,NAMES_TO_CHECK(OFFSET_NAMES+I),20,IFOUND)
                IF ( IFOUND >= 1 ) THEN
                   STRNG = 'used'
                   I_ARRAY(I+OFFSET_I_ARRAY) = NOCOL
                ENDIF
             END DO
             WRITE ( LUNUT , 1000 ) NOCOL, CHULP, STRNG
             !GOTO 20  ! Read loop
          ELSE
             IF ( ITYPE .EQ. 2 ) THEN ! An integer has arrived
                CALL CNVTIM ( ihulp  , ITFACT, DTFLG1 , DTFLG3 )
             ENDIF
             ERROR_IDX = -1
             MUST_READ_MORE = .FALSE.
             IF ( FIRST ) THEN
                 if (timon) call timstop( ithndl )
                 RETURN !exit subroutine
             END IF
          ENDIF
      END DO
!
!     Is everything resolved ?
      ICNT = 0
      IODS = 0
!      DO I = 1, COUNT_NAMES
!          WRITE(*,*) "G->", I, NAMES_TO_CHECK(OFFSET_NAMES + I)
!      END DO
      
      DO I = 1, COUNT_NAMES
         K = I - ICNT
         IF ( (NAMES_TO_CHECK(OFFSET_NAMES + K) /= '&$&$SYSTEM_NAME&$&$!')
     *       .AND.  (I_ARRAY(OFFSET_I_ARRAY + K) <= 0) ) THEN
            CALL compact_usefor_list(LUNUT, I_ARRAY, COUNT_ITEMS_ASSIGN,
     *                COUNT_ITEMS_COMP_RULE, COUNT_SUBS_ASSIGN,
     *                COUNT_SUBS_COMP_RULE, INDEX_FIRST, NAMES_TO_CHECK,
     *                OFFSET_I_ARRAY, OFFSET_NAMES,
     *                IODS, OFFSET_COMMON, K, ICNT, ERROR_IDX, IWAR)
            !ERROR_IDX = 2
         END IF
      END DO
!
 1000 FORMAT ( ' Column:',I3,' contains: ',A40,' Status: ',A8)
!
      END SUBROUTINE DLWQ5G

      end module m_dlwq5g
