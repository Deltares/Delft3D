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
      module m_dlwq81

      implicit none

      contains


      SUBROUTINE DLWQ81 ( WASTE  , IWASTE , NOWST  , NOTOT  , CONC   ,
     *                    DERIV  , TIMER  , VOLUME , AMASS2 , IAFLAG ,
     *                    DMPS   , NDMPS  , INTOPT , ISDMP  , NOSYS  )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED: april 3, 1988 by L.Postma
!
!     FUNCTION            : Adds the wasteloads to DERIV.
!
!     LOGICAL UNITNUMBERS : none
!
!     SUBROUTINES CALLED  : none
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     WASTE   REAL  NOTOT+1*NOWST INPUT   waste masses per system clock
!     IWASTE  INTEGER    NOWST    INPUT   segment numbers of the wastes
!     NOWST   INTEGER     1       INPUT   number of wastes
!     NOTOT   INTEGER     1       INPUT   number of substances
!     CONC    REAL     NOTOT*?    INPUT   concentrations for withdrawals
!     DERIV   REAL     NOTOT*?    IN/OUT  derivative to be updated
!     TIMER   REAL     NOTOT*?    IN/OUT  time step accumulator
!     VOLUME  REAL      NOSEG     INPUT   segment volumes
!     AMASS2  REAL     NOTOT*5    IN/OUT  mass balance array
!     IAFLAG  INTEGER     1       INPUT   if 1 then accumulation
!     DMPS    REAL  NOTOT*NDMPAR*?IN/OUT  dumped segment fluxes
!                                         if INTOPT > 7
!     NDMPS   INTEGER     1       INPUT   number of dumped segments
!     INTOPT  INTEGER     1       INPUT   Integration suboptions
!     ISDMP   INTEGER     *       INPUT   pointer dumped segments
!
      use timers

      INTEGER     INTOPT, NDMPS
      INTEGER     ISDMP(*)  , IWASTE(*)
      real        WASTE (*) , CONC(*)  , DERIV(*) ,VOLUME(*),
     *            AMASS2(*) , TIMER (*), DMPS(*)
      integer     NOWST, NOTOT, IAFLAG, NOSYS

      integer     i, i1, i3, i4, i5, itel, iseg, ipb, ipq, ibflag
      real        aload, ahlp, c

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq81", ithandl )
      IF ( NOWST .EQ. 0 ) RETURN
!
!     pointers in mass balance array
!
      IF ( MOD(INTOPT,16) .GE. 8  ) THEN
         IBFLAG = 1
      ELSE
         IBFLAG = 0
      ENDIF
!
      I4 = NOTOT*NDMPS
      I5 = NOTOT*NDMPS*2
!
      ITEL = 1
      DO 40 I = 1 , NOWST
!
      I3 = ( IWASTE(I) - 1 ) * NOTOT
!
      IF ( IBFLAG .EQ. 1 ) THEN
         ISEG = IWASTE(I)
         IF ( ISDMP(ISEG) .GT. 0 ) THEN
            IPB = ISDMP(ISEG)
            IPQ = (ISDMP(ISEG)-1)*NOTOT
         ELSE
            IPB = 0
         ENDIF
      ELSE
         IPB = 0
      ENDIF
!
!         a load or a withdrawal with a flow?
!
      ALOAD = WASTE(ITEL)
      IF (     ALOAD  .LT.-1.0E-30 ) GOTO 20
      IF ( ABS(ALOAD) .LE. 1.0E-30 ) ALOAD = 1.0
!
      ITEL = ITEL + 1
      DO 10 I1=1,NOTOT
         AHLP = WASTE(ITEL)*ALOAD
         DERIV(I3+I1) = DERIV (I3+I1) + AHLP
!
!        accumulation ?
!
         IF ( IAFLAG .EQ. 1 ) THEN
            AMASS2(I1+2*NOTOT) = AMASS2(I1+2*NOTOT) + AHLP
            IF ( IPB .GT. 0  ) THEN
               IF ( AHLP .LT. 0.0 ) THEN
                  DMPS(IPQ+I1+I5)=DMPS(IPQ+I1+I5) - AHLP
               ELSE
                  DMPS(IPQ+I1+I4)=DMPS(IPQ+I1+I4) + AHLP
               ENDIF
            ENDIF
         ENDIF
!
         ITEL = ITEL + 1
   10 CONTINUE
      GOTO 40
!
   20 ITEL  = ITEL + 1
      DO 30 I1 = 1,NOTOT
         IF ( ABS(WASTE(I3+I1)) .LT. 1.0E-30 ) THEN
            IF ( I1 .LE. NOSYS ) THEN
               AHLP = CONC (I3+I1)*ALOAD
            ELSE
               AHLP = 0.0
            ENDIF
         ELSE
            AHLP = WASTE(I3+I1)*ALOAD
         ENDIF
         C = CONC(I3+I1)
         IF ( C .LT. 1.0E-30 ) C = 1.0
         TIMER (I3+I1) = TIMER (I3+I1)      - AHLP/C/VOLUME(IWASTE(I))
         DERIV (I3+I1) = DERIV (I3+I1)      + AHLP
!
!        accumulation ?
!
         IF ( IAFLAG .EQ. 1 ) THEN
            AMASS2(I1+2*NOTOT) = AMASS2(I1+2*NOTOT) + AHLP
            IF ( IPB .GT. 0  ) THEN
               IF ( AHLP .LT. 0.0 ) THEN
                  DMPS(IPQ+I1+I5)=DMPS(IPQ+I1+I5) - AHLP
               ELSE
                  DMPS(IPQ+I1+I4)=DMPS(IPQ+I1+I4) + AHLP
               ENDIF
            ENDIF
         ENDIF
         ITEL = ITEL + 1
   30 CONTINUE
!
   40 CONTINUE
!
      if ( timon ) call timstop ( ithandl )
      RETURN
      END

      end module m_dlwq81
