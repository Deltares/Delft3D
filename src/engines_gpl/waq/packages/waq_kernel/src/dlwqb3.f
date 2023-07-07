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
      module m_dlwqb3

      implicit none

      contains


      SUBROUTINE DLWQB3 ( AREA   , FLOW   , VELO   , IPOINT , NOTOT  ,
     *                    NOQ    , NOVELO , IVPNT  , VOLUME , IOPT   ,
     *                    AMASS2 , IDT    , IAFLAG , NOSYS  , DMPQ   ,
     *                    NDMPQ  , IQDMP  )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED             : october 1995 by L.Postma
!
!     FUNCTION            : Makes new volumes for computed volumes
!
!     SUBROUTINES CALLED  : none
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     AREA    REAL       NOQ      INPUT   exchange surface area
!     FLOW    REAL       NOQ      INPUT   flows accross exchange surfs
!     VELO    REAL   NOVELO*NOQ   INPUT   additional velocity array
!     IPOINT  INTEGER   4*NOQ     INPUT   exchange pointers
!     NOTOT   INTEGER     1       INPUT   number  of total substances
!     NOQ     INTEGER     1       INPUT   total number of exchanges
!     NOVELO  INTEGER     1       INPUT   number  of additional velos.
!     IVPNT   INTEGER   NOSYS     INPUT   pointer systems to velocities
!     VOLUME  REAL      NOSEG     IN/OUT  volumes to update
!     IOPT    INTEGER     1       INPUT   = 0 or 2 DISP at zero flow
!                                         = 1 or 3 no DISP at zero flow
!     AMASS2  REAL     NOTOT*5    IN/OUT  mass balance array
!     IDT     INTEGER     1       INPUT   integration time step size
!     LUN     INTEGER     1       INPUT   unitnumber of monitoring file
!     IAFLAG  INTEGER     1       INPUT   if 1 then accumulate mass
!     NOSYS   INTEGER     1       INPUT   number  of active substances
!     DMPQ    REAL  NOSYS*NDMPQ*? IN/OUT  mass balance dumped exchange
!                                         if INTOPT > 7
!     NDMPQ   INTEGER     1       INPUT   number of dumped exchanges
!     IQDMP   INTEGER     *       INPUT   pointer dumped exchanges
!
      use timers

      INTEGER    NDMPQ, NOTOT, NOQ, NOVELO, NOSYS
      INTEGER    IOPT, IDT, IAFLAG
      INTEGER    IQDMP   (*)
      real       AREA (*) , FLOW  (*) , VELO  (*) , VOLUME(*),
     *           AMASS2(*) , DMPQ(*)
      integer    IVPNT(*) , IPOINT(4,*)
      LOGICAL    MASBAL

      integer    i, j, i4, i5, i6, iq, ipq
      real       b, q

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqb3", ithandl )
!
!         loop accross the number of exchanges
!
      I4 = 3*NOTOT+1
      I5 = 4*NOTOT+1
      I6 = NOSYS*NDMPQ
      B  = 0.0
      IF ( IAFLAG .EQ. 1 ) B = 1.0/IDT
      MASBAL = .FALSE.
      IF ( MOD(IOPT,16) .GE. 8  ) MASBAL = .TRUE.
      DO 60 IQ = 1 , NOQ
!
!         initialisations, check for transport anyhow
!
      I    = IPOINT(1,IQ)
      J    = IPOINT(2,IQ)
      IF ( I .EQ. 0 .OR. J .EQ. 0 ) GOTO 60
      Q    = FLOW(IQ)*IDT
      IF ( IVPNT(1) .GT. 0 )
     *      Q = Q + VELO((IQ-1)*NOVELO+IVPNT(1)) * AREA(IQ) * IDT
!
!     accumulate balance for dumped exchanges
!
      IF ( MASBAL ) THEN
         IF ( IQDMP(IQ) .GT. 0 ) THEN
            IPQ = (IQDMP(IQ)-1)*NOSYS + 1
            IF ( Q .GT. 0.0 ) THEN
               DMPQ(IPQ)=DMPQ(IPQ) + Q
            ELSE
               DMPQ(IPQ+I6)=DMPQ(IPQ+I6) - Q
            ENDIF
         ENDIF
      ENDIF
!
      IF ( I .LT. 0 ) GOTO 20
      IF ( J .LT. 0 ) GOTO 40
!
!         The regular case
!
      VOLUME(I) = VOLUME(I) - Q
      VOLUME(J) = VOLUME(J) + Q
      GOTO 60
!
!        The 'from' element was a boundary. Note the 2 options.
!
   20 IF ( J .LT. 0 ) GOTO 60
      VOLUME(J) = VOLUME(J) + Q
      IF ( Q .GT. 0.0 ) THEN
           AMASS2(I4) = AMASS2(I4) + Q*B
      ELSE
           AMASS2(I5) = AMASS2(I5) - Q*B
      ENDIF
      GOTO 60
!
!        The 'to' element was a boundary.
!
   40 VOLUME(I) = VOLUME(I) - Q
      IF ( Q .GT. 0.0 ) THEN
         AMASS2(I5) = AMASS2(I5) + Q*B
      ELSE
         AMASS2(I4) = AMASS2(I4) - Q*B
      ENDIF
!
!        end of the loop over exchanges
!
   60 CONTINUE
!
      if ( timon ) call timstop ( ithandl )
      RETURN
      END

      end module m_dlwqb3
