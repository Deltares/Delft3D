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
      module m_dlwq80

      implicit none

      contains


      SUBROUTINE DLWQ80 ( DERIV  , NOTOT  , NOSEG  , ITFACT , TIMER  ,
     *                    AMASS  , AMASS2 , IAFLAG , DMPS   , INTOPT ,
     *                    ISDMP  )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED: april 3, 1988 by L.Postma
!
!     FUNCTION            : utility that scales the DERIV array after
!                           the user quality processes routine and
!                           accumulates if needed.
!
!     LOGICAL UNITNUMBERS : none
!
!     SUBROUTINES CALLED  : none
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     DERIV   REAL   NOTOT*NOSEG  IN/OUT  derivatives to be scaled
!     NOTOT   INTEGER     1       INPUT   total number of systems
!     NOSEG   INTEGER     1       INPUT   number of computational elems
!     ITFACT  INTEGER     1       INPUT   scale factor
!     TIMER   REAL   NOTOT*NOSEG  IN/OUT  timer accumulator
!     AMASS   REAL   NOTOT*NOSEG  INPUT   mass in the system
!     AMASS2  REAL     NOTOT*5    IN/OUT  mass balance array
!     IAFLAG  INTEGER     1       INPUT   if 1 then accumulation
!     DMPS    REAL        *       IN/OUT  dumped segment fluxes
!                                         if INTOPT > 7
!     INTOPT  INTEGER     1       INPUT   Integration suboptions
!     ISDMP   INTEGER  NOSEG      INPUT   pointer dumped segments
!
      use timers

      INTEGER     ISDMP(*)
      real        DERIV(*)  ,  TIMER(*) , AMASS(*) , AMASS2(*) ,
     *            DMPS(*)

      integer  NOTOT, NOSEG, IAFLAG, INTOPT, ITFACT

      integer  i1, i2, iseg, ip, isys

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq80", ithandl )
!
!         loop accross deriv
!
      DO  10 I1 = 1 , NOTOT*NOSEG
      DERIV(I1) = DERIV(I1)/ITFACT
      IF ( DERIV(I1) .LT. 0.0 ) TIMER(I1)=TIMER(I1)-DERIV(I1)/AMASS(I1)
   10 CONTINUE
      IF ( IAFLAG .EQ. 0 ) RETURN
!
!     total mass balance
!
      DO  20  I1 = 1 , NOTOT*NOSEG
      I2         = MOD   (I1-1,NOTOT) + NOTOT + 1
      AMASS2(I2) = AMASS2(I2) + DERIV(I1)
   20 CONTINUE
!
!     segment mass balance
!
      IF ( MOD(INTOPT,16) .GE. 8  ) THEN
         DO  40  ISEG = 1 , NOSEG
            IF ( ISDMP(ISEG) .GT. 0 ) THEN
               IP = (ISDMP(ISEG)-1) * NOTOT
               I1 = NOTOT*(ISEG-1)
               DO 30 ISYS = 1 , NOTOT
                  DMPS(IP+ISYS)=DMPS(IP+ISYS) + DERIV(I1+ISYS)
   30          CONTINUE
            ENDIF
   40    CONTINUE
      ENDIF
!
      if ( timon ) call timstop ( ithandl )
      RETURN
      END

      end module m_dlwq80
