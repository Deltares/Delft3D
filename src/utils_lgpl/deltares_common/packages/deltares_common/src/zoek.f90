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
      module m_zoek

      implicit none

      contains


!     MODULE ZOEK
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED: april 1993 by Jan van Beek
!
!     FUNCTION            : Search string in character array
!
!     SYMBOLS             : ZOEK  , searches a string in an array
!                           ZOEKNS, searches not case sensetive
!                           ZOEKCS, searches case sensetive
!                           SETZMO, sets search case sensetivety mode
!                           GETZMO, gives search case sensetivety mode
!                           BCZOEK, (block data) sets default search mode
!
      SUBROUTINE ZOEK ( NAAM  , NOTOT , SYNAME, NZOEK , IAINDX)
      use m_write_error_message
      USE Timers
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED: april 1993 by Jan van Beek
!
!     FUNCTION            : searches a string in an array
!
!     SUBROUTINES CALLED  : ZOEKNS, searches not case sensitive
!                           ZOEKCS, searches case sensitive
!                           write_error_message,
!
!     COMMON's            : CZOEK , search settings
!
!     PARAMETERS          : 5
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     NAAM    CHAR*(*)      1     INPUT   string to be located
!     NOTOT   INTEGER       1     INPUT   number of elements in SYNAME
!     SYNAME  CHAR*(*)  NOTOT     INPUT   array to be searched in
!     NZOEK   INTEGER       1     INPUT   number of characters to be used
!                                         in the comparison
!     IAINDX  INTEGER       1     OUTPUT  index in SYNAME if found,
!                                         -1 if not found
!     Declaration of arguments
!
      INTEGER       NOTOT , NZOEK , IAINDX
      CHARACTER*(*) NAAM
      CHARACTER*(*) SYNAME(NOTOT)
!
!     COMMON's
!
      INTEGER          ICASEM
      COMMON / CZOEK / ICASEM
      SAVE   / CZOEK /
      integer(4) ithndl /0/
!
!     Search with case sensitivity depending on ICASEM
!
      IF ( ICASEM .EQ. 0 ) THEN
         CALL ZOEKNS (NAAM,NOTOT,SYNAME,NZOEK,IAINDX)
      ELSEIF ( ICASEM .EQ. 1 ) THEN
         CALL ZOEKCS (NAAM,NOTOT,SYNAME,NZOEK,IAINDX)
      ELSE
         IAINDX = -1
         CALL write_error_message ( 'ERROR IN ZOEK : ONBEKENDE MODE ' )
      ENDIF
!
      RETURN
      END
!
      SUBROUTINE ZOEKNS ( NAAM  , NOTOT , SYNAME, NZOEK , IAINDX)
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED: april 1993 by Jan van Beek
!
!     FUNCTION            : searches a string in an array
!                           searches not case sensetive
!                           Uses ICHAR() and expects ASCII char set
!                           a t/m z have codes 97 t/m 122
!                           A t/m Z have codes 65 t/m 90
!
!     SUBROUTINES CALLED  : -
!
!     PARAMETERS          : 5
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     NAAM    CHAR*(*)      1     INPUT   string to be located
!     NOTOT   INTEGER       1     INPUT   number of elements in SYNAME
!     SYNAME  CHAR*(*)  NOTOT     INPUT   array to be searched in
!     NZOEK   INTEGER       1     INPUT   number of characters to be used
!                                         in the comparison
!     IAINDX  INTEGER       1     OUTPUT  index in SYNAME if found,
!                                         -1 if not found
!     Declaration of arguments
!
      INTEGER       NOTOT , NZOEK , IAINDX
      CHARACTER*(*) NAAM
      CHARACTER*(*) SYNAME(NOTOT)
      integer :: K, I, I1, I2, ICASEM
!
      IAINDX = -1
      DO 100 I = 1,NOTOT
          DO 50 K = 1,NZOEK
              I1 = ICHAR(NAAM     (K:K))
              I2 = ICHAR(SYNAME(I)(K:K))
              IF (I1.GE. 97.AND.I1.LE.122) THEN
                  IF (I1.NE.I2.AND.I1.NE.(I2+32)) GOTO 100
              ELSEIF (I1.GE. 65.AND.I1.LE. 90) THEN
                  IF (I1.NE.I2.AND.I1.NE.(I2-32)) GOTO 100
              ELSE
                  IF (I1.NE.I2                  ) GOTO 100
              ENDIF
   50     CONTINUE
          IAINDX = I
          GOTO 200
  100 CONTINUE
      RETURN
  200 CONTINUE
      RETURN
      END
!
      SUBROUTINE ZOEKCS ( NAAM  , NOTOT , SYNAME, NZOEK , IAINDX)
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED: april 1993 by Jan van Beek
!
!     FUNCTION            : searches a string in an array
!                           searches case sensetive
!
!     SUBROUTINES CALLED  : -
!
!     PARAMETERS          : 5
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     NAAM    CHAR*(*)      1     INPUT   string to be located
!     NOTOT   INTEGER       1     INPUT   number of elements in SYNAME
!     SYNAME  CHAR*(*)  NOTOT     INPUT   array to be searched in
!     NZOEK   INTEGER       1     INPUT   number of characters to be used
!                                         in the comparison
!     IAINDX  INTEGER       1     OUTPUT  index in SYNAME if found,
!                                         -1 if not found
!     Declaration of arguments
!
      INTEGER       NOTOT , NZOEK , IAINDX
      CHARACTER*(*) NAAM
      CHARACTER*(*) SYNAME(NOTOT)
      integer :: I, ICASEM
!
      IAINDX = -1
!
!     Loop over the array elements
!
      DO 100 I = 1,NOTOT
!
!        Direct comparison
!
         IF (NAAM(1:NZOEK).EQ.SYNAME(I)(1:NZOEK)) THEN
            IAINDX = I
            GOTO 200
         ENDIF
  100 CONTINUE
  200 CONTINUE
!
      RETURN
      END
!
      SUBROUTINE SETZMO (ICASST)
      use m_write_error_message

!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED: april 1993 by Jan van Beek
!
!     FUNCTION            : sets search mode
!
!     SUBROUTINES CALLED  : -
!
!     COMMON's            : CZOEK , search settings
!
!     PARAMETERS          : 1
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     ICASST  INTEGER       1     INPUT   search mode to be set
!
!     Declaration of arguments
!
      INTEGER    ICASST
!
!     COMMON's
!
      INTEGER          ICASEM
      COMMON / CZOEK / ICASEM
      SAVE   / CZOEK /
!
!     set ICASEM
!
      IF ( ICASST .GE. 0 .AND. ICASST .LE. 1 ) THEN
         ICASEM = ICASST
      ELSE
         CALL write_error_message ( 'ERROR IN SETZMO : ONBEKENDE MODE ' )
      ENDIF
!
      RETURN
      END
!
!
     
      SUBROUTINE ZOEK20 ( NAAM  , NOTOT , SYNAME, NZOEK , IAINDX)
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED: april 1993 by Jan van Beek
!
!     FUNCTION            : searches a string in an CHAR*20 array
!                           by declaring that array as CHAR*20 and
!                           then calling the normal ZOEK
!
!     SUBROUTINES CALLED  : ZOEK  , searches
!
!     PARAMETERS          : 5
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     NAAM    CHAR*(*)      1     INPUT   string to be located
!     NOTOT   INTEGER       1     INPUT   number of elements in SYNAME
!     SYNAME  CHAR*20   NOTOT     INPUT   array to be searched in
!     NZOEK   INTEGER       1     INPUT   number of characters to be used
!                                         in the comparison
!     IAINDX  INTEGER       1     OUTPUT  index in SYNAME if found,
!                                         -1 if not found
!     Declaration of arguments
!
      INTEGER       NOTOT , NZOEK , IAINDX
      CHARACTER*(*) NAAM
      CHARACTER*20  SYNAME(NOTOT)
!
      CALL ZOEK ( NAAM  , NOTOT , SYNAME, NZOEK , IAINDX)
!
      RETURN
      END
      end module m_zoek

      BLOCK DATA BCZOEK
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED: april 1993 by Jan van Beek
!
!     FUNCTION            : set default search mode
!
!     COMMON's            : CZOEK , search settings
!
      INTEGER          ICASEM
      COMMON / CZOEK / ICASEM
      SAVE   / CZOEK /
!
      DATA   ICASEM / 0 /
      END
