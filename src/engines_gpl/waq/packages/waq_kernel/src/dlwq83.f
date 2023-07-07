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
      module m_dlwq83

      implicit none

      contains


      SUBROUTINE DLWQ83 ( CONC   , AMASS  , DERIV  , VOLUME , VOLUM2 ,
     *                    TIMER  , NOSYS  , NOTOT  , NOSEG  , ISTEP  ,
     *                                      ASTOP  , CONVER , IOUT   )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED:    april 1988 by L.Postma
!
!     FUNCTION            : Sets an explicit time step from DERIV.
!
!     UNIT NUMBERS        : IOUT , monitor file
!
!     SUBROUTINES CALLED  : none
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     CONC    REAL   NOTOT*NOSEG  OUTPUT  new concentrations
!     AMASS   REAL   NOTOT*NOSEG  IN/OUT  mass array to be updated
!     DERIV   REAL   NOTOT*NOSEG  INPUT   derivetives for time step
!     VOLUME  REAL      NOSEG     INPUT   segment volumes
!     VOLUM2  REAL      NOSEG     INPUT   closure error correction
!     TIMER   REAL   NOTOT*NOSEG  INPUT   time step size in flow units
!     NOSYS   INTEGER     1       INPUT   number of active substances
!     NOTOT   INTEGER     1       INPUT   number of total substances
!     NOSEG   INTEGER     1       INPUT   number of computational elts.
!     ISTEP   INTEGER     1       INPUT   iteration step count
!     ASTOP   INTEGER     1       INPUT   stop criterion
!     CONVER  LOGICAL     1       OUTPUT  TRUE if iteration successful
!     IOUT    INTEGER     1       INPUT   unit number monitor file
!
      use timers

      real       CONC (*) , AMASS (*) , DERIV(*) , VOLUME(*) ,
     *           TIMER(*) , VOLUM2(*) , ASTOP

      integer    NOSYS, NOTOT, NOSEG, ISTEP, IOUT

      integer    ismax, icmax, itel, iseg, i
      real       dcmax, v1, v2, dt, a, csave, cnew, dc

      LOGICAL    CONVER
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq83", ithandl )
!
!         loop accross the number of computational elements
!
      CONVER = .FALSE.
      DCMAX  = 0.0
      ISMAX  =  0
      ICMAX  =  0
      ITEL   =  1
      DO 30 ISEG=1,NOSEG
      V1 = VOLUME(ISEG)
      V2 = VOLUM2(ISEG)
      IF ( V1 .LT. 1.0E-30 ) V1 = 1.0
!
!         active substances first
!
      DO 10 I=1,NOSYS
      DT = 1.0
      IF ( TIMER(ITEL) .NE. 0.0 ) DT = 0.75/TIMER(ITEL)
      A           = AMASS(ITEL) + DT * DERIV(ITEL)
      AMASS(ITEL) = A / ( 1.0 + V2*DT/V1 )
      IF ( A .EQ. 0.0 ) GOTO 5
      IF ( ABS((CONC(ITEL)*V1-A)/A) .GT. DCMAX ) THEN
           CSAVE = CONC (ITEL)
           CNEW  = AMASS(ITEL)/V1
           IF ( CNEW .EQ. 0.0 ) CNEW = 1.0
           DCMAX = ABS( ( CSAVE - CNEW ) / CNEW )
           ISMAX = ISEG
           ICMAX = I
      ENDIF
    5 CONC (ITEL) = A / V1
      DERIV(ITEL) = 0.0
      TIMER(ITEL) = 0.0
      ITEL = ITEL+1
   10 CONTINUE
!
!         then the inactive substances
!
      DO 20 I=NOSYS+1,NOTOT
      DT = 1.0
      IF ( TIMER(ITEL) .NE. 0.0 ) DT = 0.75/TIMER(ITEL)
      AMASS(ITEL) = AMASS(ITEL) + DT*DERIV(ITEL)
      CSAVE = CONC (ITEL)
      CNEW  = AMASS(ITEL)
      IF ( CNEW .EQ. 0.0 ) GOTO 15
      DC    = ABS( ( CSAVE - CNEW ) / CNEW )
      IF ( DC .GT. DCMAX ) THEN
           DCMAX = DC
           ISMAX = ISEG
           ICMAX = I
      ENDIF
   15 CONC (ITEL) = AMASS(ITEL)
      DERIV(ITEL) = 0.0
      TIMER(ITEL) = 0.0
      ITEL = ITEL+1
   20 CONTINUE
!
!         end of the loop
!
   30 CONTINUE
!
      IF ( ICMAX .NE. 0 ) THEN
          WRITE ( IOUT , 2000 ) ISTEP, ISMAX, ICMAX, CSAVE, CNEW, DCMAX
          IF ( DCMAX .LT. ASTOP ) CONVER = .TRUE.
      ELSE
          WRITE ( IOUT , 2010 )
          CONVER = .TRUE.
      ENDIF
      if ( timon ) call timstop ( ithandl )
      return
!
 2000 FORMAT(' ITERATION STEP:',I5,' MAXIMUM ADAPTATION IN SEGMENT:',I7,
     *       '  SUBSTANCE:',I3,/
     *       ' OLD:',E15.6,'  NEW:',E15.6,'  DIFFERENCE:',E15.6 )
 2010 FORMAT(' ALL DERIVATIVES ARE ZERO, NO IMPROVEMENTS POSSIBLE')
!
      END

      end module m_dlwq83
