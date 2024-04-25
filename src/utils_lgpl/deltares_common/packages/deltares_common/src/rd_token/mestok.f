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

      SUBROUTINE MESTOK ( LUNUT  , LUNIN  , LUNINC , LINE   , IPOSL  ,
     *                    IPOSR  , IWIDTH , LINERR , ITYPEX , ITYPE  ,
     *                                               LINENO , IERR   )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: may  - 1996 by L. Postma
C
C     FUNCTION            : Produces an error message
C
C     LOGICAL UNITNUMBERS : LUNUT - output file for the message
C
C     SUBROUTINES CALLED  : none
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     LUNUT   INTEGER       1     INPUT   logical unitnumber output file
C     LUNIN   INTEGER       1     INPUT   unit number in error
C     LUNINC  CHAR*(*)      1     INPUT   file name of unit in error
C     LINE    CHAR*(*)      1     INPUT   line read on unit in error
C     IPOSL   INTEGER       1     INPUT   left position token on error
C     IPOSR   INTEGER       1     INPUT   right position token on error
C     IWIDTH  INTEGER       1     INPUT   total width of the input line
C     LINERR  CHAR*(*)      1     INPUT   text string to produce as error
C     ITYPEX  INTEGER       1     INPUT   type of token expected
C     ITYPE   INTEGER       1     INPUT   type of token found
C     LINENO  INTEGER       1     INPUT   current line in the file
C     IERR    INTEGER       1     INPUT   error number returned from read
C
C
      CHARACTER*(*) LUNINC, LINE, LINERR
      CHARACTER     LINE2*80, CCHAR*10 , CINT*8, CREAL*5
      DATA   CCHAR / 'character '/ ,
     *       CINT  / 'integer '  / ,
     *       CREAL / 'real '     /
C
      WRITE ( LUNUT , * )
C
C         File information when LUNIN unequal to 0
C
      IF ( LUNIN .NE. 0 ) THEN
         IF ( LUNINC .NE. ' ' ) THEN
            WRITE ( LUNUT , 1000 ) LUNIN, LUNINC
         ELSE
            WRITE ( LUNUT , 1010 ) LUNIN
         ENDIF
      ENDIF
C
C         Line information when line is not blank
C
      IF ( LINE .NE. ' ' ) THEN
         WRITE ( LUNUT , 1020 ) LINENO, ' '
         IF ( IPOSL .NE. 0 .AND. IPOSR .NE. 0 ) THEN
            IWIDTH_TRIM = LEN_TRIM(LINE)
            DO 20 J = 1 , IWIDTH_TRIM , 80
               WRITE ( LUNUT , '(A)' ) LINE (J:MIN(J+79,IWIDTH_TRIM))
               LINE2 = ' '
               IF ( IPOSL-J .LT. 80 ) THEN
                  ILIM = IPOSR
                  IF ( ITYPE .EQ. -1 ) ILIM = ILIM-1
                  DO 10 I = MAX(IPOSL-J+1,1), MIN(ILIM-J+1,80)
                     LINE2(I:I) = '^'
   10             CONTINUE
               ENDIF
               WRITE ( LUNUT , '(A)' ) LINE2
   20       CONTINUE
         ENDIF
      ENDIF
C
C         Error code ?
C
      IF ( IERR .NE. 0 ) WRITE ( LUNUT , 1030 ) IERR
C
C         You can't always get what you want
C
      IF ( ITYPEX .EQ. 1 ) WRITE ( LUNUT , 1040 ) CCHAR
      IF ( ITYPEX .EQ. 2 ) WRITE ( LUNUT , 1050 ) CINT
      IF ( ITYPEX .EQ. 3 ) WRITE ( LUNUT , 1040 ) CREAL
      IF ( ITYPE  .EQ. 1 .OR. ITYPE .EQ. -1 )
     *                     WRITE ( LUNUT , 1060 ) CCHAR
      IF ( ITYPE  .EQ. 2 ) WRITE ( LUNUT , 1070 ) CINT
      IF ( ITYPE  .EQ. 3 ) WRITE ( LUNUT , 1060 ) CREAL
      IF ( ITYPEX .LT. 0 ) WRITE ( LUNUT , 1080 )
C
C         Something else to say ?
C
      IF ( LINERR .NE. ' ' ) WRITE ( LUNUT , '(A)' ) LINERR
C
      RETURN
C
C         Formats
C
 1000 FORMAT ( ' ERROR reading file on unit:',I4,', filename: ',A )
 1010 FORMAT ( ' ERROR reading file on unit:',I4,' !' )
 1020 FORMAT ( ' Line on input file was (at line no. ', I0, '):', A )
 1030 FORMAT ( ' Error code from input processor was: ',I2 )
 1040 FORMAT ( ' Expected was a ',A,'!' )
 1050 FORMAT ( ' Expected was an ',A,'!' )
 1060 FORMAT ( ' Detected was a ',A,'!' )
 1070 FORMAT ( ' Detected was an ',A,'!' )
 1080 FORMAT ( ' This item is NOT allowed at this location !' )
C
      END
