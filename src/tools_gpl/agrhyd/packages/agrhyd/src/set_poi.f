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

      SUBROUTINE KOPPNT ( MMAX  , NMAX  , KMAX  , MNMAXK, LGRID ,
     +                    IPNT  , ILPNT , NOSEG , NOQ1  , NOQ2  ,
     +                    NOQ3  , NOBND , IPNM  , IPNN  , IPOINT,
     +                    ISAMEN)
C
C     DELFT HYDRAULICS           SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     Created : Sept. 1996 by Jan van Beek
C
C     Function            : Makes pointers in grid
C
C     Subroutines called  : -
C
C     Parameters          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     MMAX    INTEGER  1          INPUT   X,U direction, second in LGRID
C     NMAX    INTEGER  1          INPUT   Y,V direction, first in LGRID
C     KMAX    INTEGER  1          INPUT   Number of TRIWAQ layers
C     MNMAXK  INTEGER  1          INPUT   Number of WAQUA  cells
C     LGRID   INTEGER  NMAX,MMAX  INPUT   Grid table
C     IPNT    INTEGER  0:MNMAXK   INPUT   Pointer to active segments
C     ILPNT   INTEGER  KMAX       INPUT   Layer aggregation pointer
C     NOSEG   INTEGER  1          OUTPUT  Number of DELWAQ cells aggregated
C     NOQ1    INTEGER  1          OUTPUT  Number of N-exchanges
C     NOQ2    INTEGER  1          OUTPUT  Number of M-exchanges
C     NOQ3    INTEGER  1          OUTPUT  Number of K-exchanges
C     NOBND   INTEGER  1          OUTPUT  Number of boundaries
C     IPNM    INTEGER  MNMAXK     OUTPUT  Pointer to active M-exchanges
C     IPNN    INTEGER  MNMAXK     OUTPUT  Pointer to active N-exchanges
C     IPOINT  INTEGER  MNMAXK*4   OUTPUT  Delwaq pointers
C     ISAMEN  INT         1       INPUT   Flag (0/1) aggregation horizontal
C
C     Declaration of arguments
C
      INTEGER    MMAX  , NMAX  , KMAX  , MNMAXK, NOSEG ,
     +           NOQ1  , NOQ2  , NOQ3  , NOBND , ISAMEN
      INTEGER    LGRID(NMAX,*)    , IPNT(0:*)        ,
     +           ILPNT(*)         , IPNM(*)          ,
     +           IPNN(*)          , IPOINT(4,*)
C
C
C     Local declaration
C

C
C     Set NOSEG and NOBND ( top layer )
C     clear the pointers
C
      NOSEGL = 0
      NOBNDL = 0
      DO 10 ICEL=1,MNMAXK
         NOSEGL = MAX(NOSEGL,IPNT(ICEL))
         NOBNDL = MAX(NOBNDL,-IPNT(ICEL))
         IPNN(ICEL) = 0
         IPNM(ICEL) = 0
   10 CONTINUE
C
      IF ( KMAX .GT. 1 ) THEN
         NLAY   = ILPNT(KMAX)
         NOSEG  = NLAY*NOSEGL
         NOBND  = NLAY*NOBNDL
      ELSE
         NLAY   = 1
         NOSEG  = NOSEGL
         NOBND  = NOBNDL
      ENDIF
C
C         fill the active exchanges in N-direction
C
      NOQ1  = 0
      DO 30 M = 1,MMAX
      DO 30 N = 1,NMAX
         IP0 = LGRID(N,M)
         IP1 = IPNT(IP0)
         IF ( IP1 .EQ. 0 ) GOTO 30
         IP2 = 0
         IF ( N .LE. NMAX-1 ) IP2 = IPNT(LGRID(N+1,M))
         IF ( IP2 .EQ. 0 .OR.  IP1 .EQ. IP2 ) GOTO 30
         IF ( IP1 .LT. 0 .AND. IP2 .LT.   0 ) GOTO 30
C
C        look down till we find a segment with another number for the -1 pointer
C
         DO IN2 = N-1 , 1 , -1
            IP3 = IPNT(LGRID(IN2,M))
            IF ( IP3 .NE. IP1 ) GOTO 15
         ENDDO
         IP3 = 0
   15    CONTINUE
C
C        look up till we find a segment with another number for the +1 pointer
C
         DO IN2 = N+2 , NMAX
            IP4 = IPNT(LGRID(IN2,M))
            IF ( IP4 .NE. IP2 ) GOTO 16
         ENDDO
         IP4 = 0
   16    CONTINUE
C
C        Excange unique ?
C
         IF ( ISAMEN .EQ. 1 ) THEN
            DO 20  IQ1 = 1 , NOQ1
               IF ( IPOINT(1,IQ1).EQ.IP1 .AND. IPOINT(2,IQ1).EQ.IP2 )
     +         THEN
                    IPNN ( IP0 ) = IQ1
                    GOTO 30
               ELSEIF ( IPOINT(2,IQ1).EQ.IP1.AND.IPOINT(1,IQ1).EQ.IP2 )
     +         THEN
                    IPNN ( IP0 ) = -IQ1
                    GOTO 30
               ENDIF
   20       CONTINUE
         ENDIF
         NOQ1 = NOQ1 + 1
         IPOINT(1,NOQ1) = IP1
         IPOINT(2,NOQ1) = IP2
         IPOINT(3,NOQ1) = IP3
         IPOINT(4,NOQ1) = IP4
         IPNN  (IP0   ) = NOQ1
   30 CONTINUE
      NOQ1L  = NOQ1
C
C     rest of the layers
C
      IF ( KMAX .GT. 1 ) THEN
         NOQ1   = NLAY*NOQ1L
         DO 50 ILAY = 2 , NLAY
            ISOFF = (ILAY-1)*NOSEGL
            IQOFF = (ILAY-1)*NOQ1L
            IBOFF = (ILAY-1)*NOBNDL
            DO 40 IQ = 1 , NOQ1L
               IP1 = IPOINT(1,IQ)
               IP2 = IPOINT(2,IQ)
               IP3 = IPOINT(3,IQ)
               IP4 = IPOINT(4,IQ)
               IF ( IP1 .GT. 0 ) THEN
                  IPOINT(1,IQ+IQOFF) = IP1 + ISOFF
               ELSEIF( IP1 .LT. 0 ) THEN
                  IPOINT(1,IQ+IQOFF) = IP1 - IBOFF
               ENDIF
               IF ( IP2 .GT. 0 ) THEN
                  IPOINT(2,IQ+IQOFF) = IP2 + ISOFF
               ELSEIF( IP2 .LT. 0 ) THEN
                  IPOINT(2,IQ+IQOFF) = IP2 - IBOFF
               ENDIF
               IF ( IP3 .GT. 0 ) THEN
                  IPOINT(3,IQ+IQOFF) = IP3 + ISOFF
               ELSEIF( IP3 .LT. 0 ) THEN
                  IPOINT(3,IQ+IQOFF) = IP3 - IBOFF
               ELSE
                  IPOINT(3,IQ+IQOFF) = 0
               ENDIF
               IF ( IP4 .GT. 0 ) THEN
                  IPOINT(4,IQ+IQOFF) = IP4 + ISOFF
               ELSEIF( IP4 .LT. 0 ) THEN
                  IPOINT(4,IQ+IQOFF) = IP4 - IBOFF
               ELSE
                  IPOINT(4,IQ+IQOFF) = 0
               ENDIF
   40       CONTINUE
   50    CONTINUE
      ENDIF
C
C         follow the same procedure in M-direction
C
      NOQ2     = NOQ1
      DO 70 M = 1,MMAX
      DO 70 N = 1,NMAX
C
         IP0 = LGRID(N  ,M)
         IP1 = IPNT(IP0)
         IF ( IP1 .EQ. 0 ) GOTO 70
         IP2 = 0
         IF ( M .LE. MMAX-1 ) IP2 = IPNT(LGRID(N,M+1))
         IF ( IP2 .EQ. 0 .OR.  IP1 .EQ. IP2 ) GOTO 70
         IF ( IP1 .LT. 0 .AND. IP2 .LT.   0 ) GOTO 70
C
C        look left till we find a segment with another number for the -1 pointer
C
         DO IM2 = M-1 , 1 , -1
            IP3 = IPNT(LGRID(N,IM2))
            IF ( IP3 .NE. IP1 ) GOTO 55
         ENDDO
         IP3 = 0
   55    CONTINUE
C
C        look right till we find a segment with another number for the +1 pointer
C
         DO IM2 = M+2 , MMAX
            IP4 = IPNT(LGRID(N,IM2))
            IF ( IP4 .NE. IP2 ) GOTO 56
         ENDDO
         IP4 = 0
   56    CONTINUE
C
C        Is exchange unique
C
         IF ( ISAMEN .EQ. 1 ) THEN
            DO 60  IQ2 = 1 , NOQ1L
               IF ( IPOINT(1,IQ2).EQ.IP1 .AND. IPOINT(2,IQ2).EQ.IP2 )
     +         THEN
                    IPNM ( IP0 ) = IQ2
                    GOTO 70
               ELSEIF ( IPOINT(2,IQ2).EQ.IP1 .AND. IPOINT(1,IQ2).EQ.IP2)
     +         THEN
                    IPNM ( IP0 ) = -IQ2
                    GOTO 70
               ENDIF
   60       CONTINUE
            DO 65  IQ2 = NOQ1 + 1 , NOQ2
               IF ( IPOINT(1,IQ2).EQ.IP1 .AND. IPOINT(2,IQ2).EQ.IP2 )
     +         THEN
                    IPNM ( IP0 ) = IQ2
                    GOTO 70
               ELSEIF ( IPOINT(2,IQ2).EQ.IP1 .AND. IPOINT(1,IQ2).EQ.IP2)
     +         THEN
                    IPNM ( IP0 ) = -IQ2
                    GOTO 70
               ENDIF
  65        CONTINUE
         ENDIF
         NOQ2 = NOQ2 + 1
         IPOINT(1,NOQ2) = IP1
         IPOINT(2,NOQ2) = IP2
         IPOINT(3,NOQ2) = IP3
         IPOINT(4,NOQ2) = IP4
         IPNM  (IP0   ) = NOQ2
   70 CONTINUE
      NOQ2  = NOQ2 - NOQ1
      NOQ2L = NOQ2
C
C     rest of the layers
C
      IF ( KMAX .GT. 1 ) THEN
         NOQ2   = NLAY*NOQ2L
         DO 90 ILAY = 2 , NLAY
            ISOFF = (ILAY-1)*NOSEGL
            IQOFF = NOQ1 + (ILAY-1)*NOQ2L
            IBOFF = (ILAY-1)*NOBNDL
            DO 80 IQ = 1 , NOQ2L
               IP1 = IPOINT(1,IQ+NOQ1)
               IP2 = IPOINT(2,IQ+NOQ1)
               IP3 = IPOINT(3,IQ+NOQ1)
               IP4 = IPOINT(4,IQ+NOQ1)
               IF ( IP1 .GT. 0 ) THEN
                  IPOINT(1,IQ+IQOFF) = IP1 + ISOFF
               ELSEIF( IP1 .LT. 0 ) THEN
                  IPOINT(1,IQ+IQOFF) = IP1 - IBOFF
               ENDIF
               IF ( IP2 .GT. 0 ) THEN
                  IPOINT(2,IQ+IQOFF) = IP2 + ISOFF
               ELSEIF( IP2 .LT. 0 ) THEN
                  IPOINT(2,IQ+IQOFF) = IP2 - IBOFF
               ENDIF
               IF ( IP3 .GT. 0 ) THEN
                  IPOINT(3,IQ+IQOFF) = IP3 + ISOFF
               ELSEIF( IP3 .LT. 0 ) THEN
                  IPOINT(3,IQ+IQOFF) = IP3 - IBOFF
               ELSE
                  IPOINT(3,IQ+IQOFF) = 0
               ENDIF
               IF ( IP4 .GT. 0 ) THEN
                  IPOINT(4,IQ+IQOFF) = IP4 + ISOFF
               ELSEIF( IP4 .LT. 0 ) THEN
                  IPOINT(4,IQ+IQOFF) = IP4 - IBOFF
               ELSE
                  IPOINT(4,IQ+IQOFF) = 0
               ENDIF
   80       CONTINUE
   90    CONTINUE
      ENDIF
C
C     The third direction
C
      IF ( NLAY .GT. 1 ) THEN
         NOQ3 = (NLAY-1)*NOSEGL
         DO 110 ILAY = 1 , NLAY-1
            IQOFF  = NOQ1 + NOQ2 + (ILAY-1)*NOSEGL
            ISOFF1 = (ILAY-1)*NOSEGL
            ISOFF2 = (ILAY  )*NOSEGL
            ISOFF3 = (ILAY-2)*NOSEGL
            ISOFF4 = (ILAY+1)*NOSEGL
            DO 100 IQ = 1 , NOSEGL
               IPOINT(1,IQ+IQOFF) = IQ + ISOFF1
               IPOINT(2,IQ+IQOFF) = IQ + ISOFF2
               IPOINT(3,IQ+IQOFF) = MAX(0,IQ+ISOFF3)
               IF ( ILAY .EQ. NLAY-1) THEN
                  IPOINT(4,IQ+IQOFF) = 0
               ELSE
                  IPOINT(4,IQ+IQOFF) = IQ + ISOFF4
               ENDIF
  100       CONTINUE
  110    CONTINUE
      ELSE
         NOQ3 = 0
      ENDIF
C
      RETURN
      END
