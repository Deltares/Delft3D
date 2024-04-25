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

C ======================================================================
      SUBROUTINE RDTOK1 ( LUNUT  , ILUN   , LCH    , LSTACK , CCHAR  ,
     *                    IPOSR  , NPOS   , CHULP2 , IHULP  , RHULP  ,
     *                    ITYPEX , IERR   )

      CHARACTER*1   CCHAR
      CHARACTER*(*) LCH  ( LSTACK ) , CHULP2
      DIMENSION     ILUN ( LSTACK )
      INTEGER*8     IHULP8
      REAL*8        RHULP8
      
      CALL RDTOK2 ( LUNUT  , ILUN   , LCH    , LSTACK , CCHAR  ,
     *              IPOSR  , NPOS   , CHULP2 , IHULP8 , RHULP8  ,
     *              ITYPEX , IERR   )
      
      IHULP = IHULP8
      RHULP = REAL(RHULP8)
      
      RETURN
      END
      
      SUBROUTINE RDTOK2 ( LUNUT  , ILUN   , LCH    , LSTACK , CCHAR  ,
     *                    IPOSR  , NPOS   , CHULP2 , IHULP  , RHULP  ,
     *                    ITYPEX , IERR   )
C ----------------------------------------------------------------------
C
C
C     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED            : May '96  by L. Postma
C
C     MODIFIED           : Jan '13  by M. Jeuken
C                                   - General name RDTOK1 for subroutine instead of DLWQJ1
C                                   - Open file without DLWQ subroutine
C
C     FUNCTION           : Reads a token and handles messages
C
C     SUBROUTINES CALLED : GETTOK - gets a token
C                          MESTOK - messages input errors
C
C     LOGICAL UNITS      : LUNIN   = unit stripped DELWAQ input file
C                          LUNUT   = unit formatted output file
C
C     PARAMETERS    :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ---------------------------------------------------------
C     LUNUT   INTEGER     1       INPUT   unit number output file
C     ILUN    INTEGER   LSTACK    IN/OUT  unitnumb include stack
C     LCH     CHAR*(*)  LSTACK    IN/OUT  filename include stack
C     LSTACK  INTEGER     1       INPUT   include file stack size
C     NPOS    INTEGER     1       INPUT   nr of significant characters
C     CCHAR   CHAR*1      1       INPUT   comment character
C     IPOSR   INTEGER     1       IN/OUT  start position on line
C     NPOS    INTEGER     1       INPUT   width of the input file
C     CHULP   CHAR*(*)    1       OUTPUT  string  to be delivered (for ITYPEX=-1: input!)
C     IHULP   INTEGER     1       OUTPUT  integer to be delivered
C     RHULP   REAL*8      1       OUTPUT  real    to be delivered
C     ITYPEX  INTEGER     1       INPUT   type expected
C     IERR    INTEGER     1       OUTPUT  Error code (see below)
C
C     ERROR CODES:
C
C     From GETTOK:                   From this routine:
C     -5 Negative or zero repeats(*) 1 General error/error reading etc.
C     -4 Integer overflow            1 General error/error reading etc.
C     -3 Exponent out of range       1 General error/error reading etc.
C     -2 Group separator found       2 End of data group / include stack exceeded
C     -1 No delimiting quote         1 General error/error reading etc.
C      0 Normal result               0 Normal end or (if other than expected)
C                                    4 Unexpected input data
C      1 End of file encountered     3 End of file * only if ITYPEX >0 at entry!
C      2 Read error encountered      1 General error/error reading etc.
C
C DATA ---------------------------------------------------- Arguments --

      CHARACTER*1   CCHAR
      CHARACTER*(*) LCH  ( LSTACK ) , CHULP2
      DIMENSION     ILUN ( LSTACK )
      INTEGER*8     IHULP
      REAL*8        RHULP
C
C DATA -------------------------------------------------------- Local --
C
      SAVE
      CHARACTER  LINE*1000, LINE2*80 , CHULP*1000
      INTEGER, DIMENSION(100) :: CURLINE = 0 ! Line number in the current file

      CHARACTER(  1) CTRLZ       !   Tab character
      CHARACTER(  1) CHTAB       !   Cariage return character
      CHARACTER(  1) CH_CR       !   Ctrl_Z character

      CHTAB = CHAR(9)
      CH_CR = CHAR(13)
      CTRLZ = CHAR(26)

C BEGIN ================================================================

C
C           Management of include files
C
      DO 5 I = 1 , LSTACK
         IF ( ILUN(I) .NE. 0 ) THEN
            IFL   = I
            LUNIN = ILUN(I)
         ENDIF
    5 CONTINUE

C
C           Force the opening of a new include file - special case
C
      IF ( ITYPEX .EQ. -999) THEN
         WRITE ( LUNUT , 1040 ) CHULP2
         IFL       = IFL + 1
         OPEN (NEWUNIT = LUNIN, FILE = CHULP2, STATUS = 'old', 
     *         IOSTAT = IOERR)
         IF ( IOERR .GT. 0 ) THEN
            IFL = IFL - 1
            WRITE ( LUNUT , 1050 )
            IERR = 1
            GOTO 20
         ELSE
            LCH (IFL) = CHULP2
            ILUN(IFL) = LUNIN
            IERR = 0
            RETURN
         ENDIF
      ENDIF

C
C           Get the data
C
      CHULP = ' '
      CHULP2 = ' '
   10 IERR = 0

      IF ( ITYPEX .EQ. 4) THEN
C
C           Return a string from the first non space caracter until the end of the line (could be empty)
C
         DO I = IPOSR+1 , NPOS
         IPOSL = I
         IF ( LINE(I:I) .NE. ' '   .AND.
     &        LINE(I:I) .NE. CTRLZ .AND.
     &        LINE(I:I) .NE. CH_CR .AND.
     &        LINE(I:I) .NE. CHTAB      ) EXIT
          ENDDO
          IPOSR = MAX(IPOSL,LEN_TRIM(LINE))
          CHULP2 = LINE(IPOSL:IPOSR)
          IPOSL = 0
          IPOSR = 0
          RETURN
      ENDIF

      CALL GETTOK ( LUNIN  , LINE   , CHULP  , IHULP  , RHULP   ,
     *              ITYPE  , IPOSL  , IPOSR  , NPOS   , CCHAR   ,
     *                       '#'    , CURLINE(IFL)    , IERR    )
      CHULP2 = CHULP
C
C           Deal with errors
C
C        Negative or zero repeats(*)
C
      IF ( IERR.EQ.-5 ) THEN
         LINE2= ' ERROR Negative or zero repeats at repeat sign (*)'
         CALL MESTOK ( LUNUT  , LUNIN  , LCH(IFL), LINE   , IPOSL  ,
     *                 IPOSR  , NPOS   , LINE2   , 0      , ITYPE  ,
     *                                   CURLINE(IFL)     , 0      )
         ITYPEX = 2
         IERR   = 1
         GOTO 20
      ENDIF
C        Integer overflow
      IF ( IERR.EQ.-4 .AND. ( ITYPEX.EQ. 2 .OR. ITYPEX.EQ. 0 .OR.
     *                        ITYPEX.EQ.-1 .OR. ITYPEX.EQ.-3    ) ) THEN
         LINE2= ' ERROR integer value too large or too small (OVERFLOW)'
         CALL MESTOK ( LUNUT  , LUNIN  , LCH(IFL), LINE   , IPOSL  ,
     *                 IPOSR  , NPOS   , LINE2   , 0      , ITYPE  ,
     *                                   CURLINE(IFL)     , 0      )
         ITYPEX = 2
         IERR   = 1
         GOTO 20
      ENDIF
C        Exponent out of range and real value allowed
      IF ( IERR.EQ.-3 .AND. ( ITYPEX.EQ. 3 .OR. ITYPEX.EQ. 0 .OR.
     *                        ITYPEX.EQ.-1 .OR. ITYPEX.EQ.-2    ) ) THEN
         LINE2 = ' ERROR exponent too positive or too negative'
         CALL MESTOK ( LUNUT  , LUNIN  , LCH(IFL), LINE   , IPOSL  ,
     *                 IPOSR  , NPOS   , LINE2   , 0      , ITYPE  ,
     *                                   CURLINE(IFL)     , 0      )
         ITYPEX = 3
         IERR   = 1
         GOTO 20
      ENDIF
C        End of data block found
      IF ( IERR .EQ. -2 ) THEN
         IF ( .NOT. ( ITYPEX .EQ. 0 .OR. ITYPEX .EQ. 1) ) THEN
            LINE2 = ' ERROR unexpected end of datagroup on unit'
            CALL MESTOK ( LUNUT  , LUNIN  , LCH(IFL), ' '   , 0      ,
     *                    IPOSR  , NPOS   , LINE2   , 0     , ITYPE  ,
     *                                      CURLINE(IFL)    , 0      )
            GOTO 20
         ENDIF
         IERR   = 2
         RETURN
      ENDIF

      IF ( IERR .EQ. -1 ) THEN
         LINE2 = ' No delimiting quote found !'
         CALL MESTOK ( LUNUT  , LUNIN  , LCH(IFL), LINE   , IPOSL  ,
     *                 IPOSR  , NPOS   , LINE2   , 0      , ITYPE  ,
     *                                   CURLINE(IFL)     , 0      )
         ITYPEX = 1
         IERR   = 1
         GOTO 20
      ENDIF

      IF ( IERR .EQ. 1 ) THEN

         IF ( ITYPEX .GT. 0 ) THEN

            LINE2 = ' End of file on the input unit'

            IF ( IFL .EQ. 1 )
     *      CALL MESTOK ( LUNUT  , LUNIN  , LCH(IFL), ' '   , IPOSL,
     *                    IPOSR  , NPOS   , LINE2   , 0     , ITYPE  ,
     *                                      CURLINE(IFL)    , 0      )
         ENDIF
         IERR   = 3
         GOTO 20
      ENDIF
      IF ( IERR .EQ. 2 ) THEN
         LINE2 = ' ERROR reading from the input unit'
         CALL MESTOK ( LUNUT  , LUNIN  , LCH(IFL), ' '   , IPOSL  ,
     *                 IPOSR  , NPOS   , LINE2   , 0     , ITYPE  ,
     *                                   CURLINE(IFL)    , 0      )
         ITYPEX = 0
         IERR   = 1
         GOTO 20
      ENDIF
C
C       write(*,*)  'Skip to a new file', ' ', chulp(:40)
C
      IF ( ITYPE .EQ. 1 .AND. CHULP .EQ. 'INCLUDE' ) THEN
         IF ( IFL .EQ. LSTACK ) THEN
            WRITE ( LUNUT , 1020 ) LSTACK
            IERR = 1
            GOTO 20
         ENDIF
         IERR = 0
         CALL GETTOK ( LUNIN  , LINE   , CHULP  , IHULP  , RHULP   ,
     *                 ITYPE  , IPOSL  , IPOSR  , NPOS   , CCHAR   ,
     *                          '#'    , CURLINE(IFL)    , IERR    )
         IF ( ITYPE .NE. 1 .AND. ITYPE .NE. -1 ) THEN
            CALL MESTOK ( LUNUT  , LUNIN  , LCH(IFL), LINE  , IPOSL  ,
     *                    IPOSR  , NPOS   , ' '     , 1     , ITYPE  ,
     *                                      CURLINE(IFL)    , 0      )
            WRITE ( LUNUT , 1030 )
            IERR = 1
            GOTO 20
         ENDIF
         WRITE ( LUNUT , 1040 ) CHULP
         IFL       = IFL + 1
         OPEN (NEWUNIT = LUNIN, FILE = CHULP, STATUS = 'old', 
     *         IOSTAT = IOERR)
         IF ( IOERR .GT. 0 ) THEN
            IFL = IFL - 1
            WRITE ( LUNUT , 1050 )
            IERR = 1
            GOTO 20
         ELSE
            LCH (IFL) = CHULP
            ILUN(IFL) = LUNIN
            GOTO 10
         ENDIF
      ENDIF
      IF ( (ITYPEX .EQ. 2 .AND. ITYPE.NE.2                  ) .OR.
     *     (ITYPEX .EQ. 3 .AND. ITYPE.NE.2 .AND. ITYPE.NE. 3) .OR.
     *     (ITYPEX .EQ. 1 .AND. ITYPE.NE.1 .AND. ITYPE.NE.-1) .OR.
     *     (ITYPEX .EQ.-1 .AND. ITYPE.EQ.1                  ) .OR.
     *     (ITYPEX .EQ.-2 .AND. ITYPE.EQ.2                  ) .OR.
     *     (ITYPEX .EQ.-3 .AND. ITYPE.EQ.3                  )     ) THEN
         CALL MESTOK ( LUNUT  , LUNIN  , LCH(IFL), LINE   , IPOSL  ,
     *                 IPOSR  , NPOS   , ' '     , ITYPEX , ITYPE  ,
     *                                    CURLINE(IFL)    , 0      )
         IERR = 4
      ENDIF
      IF ( ITYPE .EQ. 2 ) THEN
         RHULP  = IHULP
      ENDIF
      IF ( ITYPE .EQ. 1 ) THEN
      ENDIF
      ITYPEX = ITYPE
      IF ( ITYPEX .LT. 0 ) ITYPEX = -ITYPEX
C
      IF ( IERR .NE. 0 ) GOTO 20
      RETURN
C
   20 IF ( IERR .EQ. 3 ) THEN
         IF ( IFL .GT. 1 ) THEN
            WRITE ( LUNUT , 1000 ) LCH(IFL)(:78)
            CLOSE ( ILUN(IFL) )
            LCH (IFL)    = ' '
            ILUN(IFL)    = 0
            CURLINE(IFL) = 0
            IFL = IFL-1
            WRITE ( LUNUT , 1010 ) LCH(IFL)(:78)
            LUNIN = ILUN(IFL)
            IPOSR = 0
            GOTO 10
         ELSE
            RETURN
         ENDIF
      ELSE
         IF ( IFL .GT. 1 ) THEN
            DO 30 I = IFL , 2 , -1
               WRITE ( LUNUT , 1000 ) LCH(IFL)(:78)
               CLOSE ( ILUN(IFL) )
               LCH (IFL) = ' '
               ILUN(IFL) =  0
   30       CONTINUE
            IPOSR = 0
         ENDIF
      ENDIF
      RETURN
C
 1000 FORMAT (/' Closing file: ',A )
 1010 FORMAT (/' Continuing on file: ',A )
 1020 FORMAT (/' ERROR: nr of include stack levels (',I2,') exceeded !')
 1030 FORMAT (/' Expected character string should be a valid ',
     *                                   ' ASCII filename !' )
 1040 FORMAT (/' Including file: ',A )
 1050 FORMAT (/' ERROR: Include file does not exist !' )
      END
