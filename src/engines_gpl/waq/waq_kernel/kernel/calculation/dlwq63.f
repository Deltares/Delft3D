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
      module m_dlwq63

      implicit none

      contains


      SUBROUTINE DLWQ63 ( CONC   , DERIV  , AMASS2 , NOSEG  , NOTOT  ,
     *                    ISYS   , NSYS   , DMPS   , INTOPT , ISDMP  )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED: june 1988 by L.Postma
!
!     FUNCTION            : derives concentrations from deriv
!                           zeros DERIV
!
!     LOGICAL UNITNUMBERS : none
!
!     SUBROUTINES CALLED  : none
!
!     PARAMETERS          :
!
!     NAME    KIND       LENGTH       FUNCT.  DESCRIPTION
!     ----    -----      ------       ------- -----------
!     CONC    REAL     NOTOT*NOSEG    INPUT   first order term
!     DERIV   REAL     NOTOT*NOSEG    IN/OUT  right hand side matrix
!     AMASS2  REAL     NOTOT*5        IN/OUT  mass accumulation array
!     NOSEG   INTEGER       1         INPUT   number of segments
!     NOTOT   INTEGER       1         INPUT   total number of systems
!     ISYS    INTEGER       1         INPUT   system considered
!     NSYS    INTEGER       1         INPUT   number of systems to take
!     DMPS    REAL          *         IN/OUT  dumped segment fluxes
!                                             if INTOPT > 7
!     INTOPT  INTEGER     1       INPUT   Integration suboptions
!
!     ISDMP   INTEGER  NOSEG      INPUT   pointer dumped segments
!
      use timers

      INTEGER     ISDMP(*)
      real        CONC(NOTOT,*) , DERIV(*) , AMASS2(NOTOT,*) ,
     *            DMPS(*)
      integer    notot, nsys, noseg
      integer    i, ip, i4, i5, i6, ntot, iset, intopt, iseg, isys
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq63", ithandl )
!
!         gets concentrations
!
      ISET = 1
      IF ( MOD(INTOPT,16) .LT. 8  ) THEN
         DO 10 ISEG = 1 , NOSEG
         DO 10 I = ISYS , ISYS+NSYS-1
            AMASS2(I,   2) = AMASS2( I, 2 ) + CONC (I,ISEG)*DERIV(ISET)
            CONC  (I,ISEG) = DERIV ( ISET )
            ISET = ISET+1
   10    CONTINUE
      ELSE
         DO 20 ISEG = 1 , NOSEG
            IP = ISDMP(ISEG)
            I4 = (IP-1)*NOTOT
            DO 20 I = ISYS , ISYS+NSYS-1
               AMASS2(I,   2) = AMASS2(I, 2) + CONC(I,ISEG)*DERIV(ISET)
               IF ( IP .GT. 0 ) THEN
                  DMPS(I4+I) = DMPS(I4+I) + CONC(I,ISEG)*DERIV(ISET)
               ENDIF
               CONC  (I,ISEG) = DERIV (ISET)
               ISET = ISET+1
   20    CONTINUE
      ENDIF
!
!         zero the derivative
!
      NTOT = NOTOT*NOSEG
      DO 30 I = 1 , NTOT
   30 DERIV(I) = 0.0
!
      if ( timon ) call timstop ( ithandl )
      RETURN
      END

      end module m_dlwq63
