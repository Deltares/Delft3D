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

      subroutine varoxy ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
      use m_srstop
      use m_monsys

!>\file
!>       Variation of oxygen due to variation in primary production within day

!
!     Description of the module :
!
! Name    T   L I/O   Description                                   Units
! ----    --- -  -    -------------------                            ----

!     Logical Units : -

!     Modules called : -
      implicit none
!     Name     Type   Library
!     ------   -----  ------------

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      INTEGER  LUNREP

      INTEGER  IP1 , IP2 , IP3 , IP4 , IP5 , IP6 , IP7 , IP8 , IP9 ,
     J         IP10, IP11, IP12, I, IFLUX, ISEG
      REAL     TIMSIM, DELTAT, TIMNUL, T1MXPP, T2MXPP, DAYLEN, FPPTOT,
     J         FRESPI, DEPTHW, T1    , T2    , PPMAX , TRISE ,
     J         TSET  , TOTAL , V1    , V2
      REAL     INTEGR(0:12*24), PPLAST, RELAST, DAYLLAST
      SAVE     PPLAST, RELAST, DAYLLAST, INTEGR
      DATA     PPLAST, RELAST, DAYLLAST /-999.,-999.,-999./
      INTEGER  NR_MES
      SAVE     NR_MES
      DATA     NR_MES / 0 /

      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
      IP5  = IPOINT( 5)
      IP6  = IPOINT( 6)
      IP7  = IPOINT( 7)
      IP8  = IPOINT( 8)
      IP9  = IPOINT( 9)
      IP10 = IPOINT(10)
      IP11 = IPOINT(11)
      IP12 = IPOINT(12)

!     Check whether certain input parameters are independent of X

      IF ( (INCREM(1) .GT. 0) .OR.
     J     (INCREM(2) .GT. 0) .OR.
     J     (INCREM(3) .GT. 0) .OR.
     J     (INCREM(4) .GT. 0) .OR.
     J     (INCREM(5) .GT. 0) .OR.
     J     (INCREM(6) .GT. 0) ) THEN

          CALL GETMLU(LUNREP)
          WRITE (LUNREP,*)
     J    ' VAROXY: Time parameters function(x) not ALLOWED'
          WRITE (*,*)
     J    ' VAROXY: Time parameters function(x) not ALLOWED'
          CALL SRSTOP(1)
      ENDIF

      IFLUX = 1
      DO 9000 ISEG = 1 , NOSEG

          TIMSIM = PMSA(IP1)/PMSA(IP3)
          DELTAT = PMSA(IP4)
          TIMNUL = PMSA(IP2)
          T1MXPP = PMSA(IP5)
          T2MXPP = PMSA(IP6)
          DAYLEN = PMSA(IP7)*24.
          FPPTOT = PMSA(IP8)
          FRESPI = PMSA(IP9)
          DEPTHW = PMSA(IP10)
          TRISE  = 12.0-0.5*DAYLEN
          TSET   = 12.0+0.5*DAYLEN

!         Initialize light variation curve for present cycle
!         ONLY if fluxes have changed

          IF ( (ISEG .EQ. 1) .OR.
     J             (ABS(DAYLEN-DAYLLAST) .GT. 1E-3) .OR.
     J             (ABS(FPPTOT-PPLAST) .GT. 1E-3) .OR.
     J             (ABS(FRESPI-RELAST) .GT. 1E-3) ) THEN

              PPLAST = FPPTOT
              RELAST = FRESPI
              DAYLLAST = DAYLEN

!             Check on conditions for daylength

              IF ( T1MXPP .LT. TRISE .OR.
     J             T2MXPP .GT. TSET ) THEN
                 IF ( NR_MES .LT. 25 ) THEN
                    NR_MES = NR_MES + 1
                    WRITE(*,*) ' WARNING: VAROXY limited values of T1MXPP/T2MXPP to daylight range'
                 ENDIF
                 IF ( NR_MES .EQ. 25 ) THEN
                    NR_MES = NR_MES + 1
                    WRITE(*,*) ' 25 WARNINGS on limiting T1MXPP/T2MXPP'
                    WRITE(*,*) ' Further messages on extinction surpressed'
                 ENDIF
                 T1MXPP = MAX(T1MXPP, TRISE)
                 T2MXPP = MIN(T2MXPP, TSET )
              ENDIF

              PPMAX = 48.0/(T2MXPP-T1MXPP+DAYLEN)
!             PPMAX = 48.0*(FPPTOT+FRESPI)/(T2MXPP-T1MXPP+DAYLEN)

!             Compute normalized integral Flux.dt in of (gC/m2/d)*h
!             from t=0 to t=T every 5 minutes

              TOTAL = 0.0
              INTEGR(0) = 0.0
              T1 = 0.0
              V1 = 0.0
              DO 100 I = 1,12*24
                  T2 = REAL(I)/12.
                  IF ( T2 .LE. TRISE .OR. T2 .GE. TSET ) THEN
                      V2 = 0.0
                  ELSEIF ( T2.GT.TRISE .AND. T2.LT.T1MXPP ) THEN
                      V2 = PPMAX*(T2-TRISE)/(T1MXPP-TRISE)
                  ELSEIF ( T2.GE.T1MXPP .AND. T2.LE.T2MXPP ) THEN
                      V2 = PPMAX
                  ELSEIF ( T2.GT.T2MXPP .AND. T2.LT.TSET ) THEN
                      V2 = PPMAX*(1.0-(T2-T2MXPP)/(TSET- T2MXPP) )
                  ENDIF
                  TOTAL = TOTAL + ((V1+V2)/2.0) * (T2-T1)
                  INTEGR(I) = TOTAL
                  V1 = V2
                  T1 = T2
  100         CONTINUE
          ENDIF

          IF (BTEST(IKNMRK(ISEG),0)) THEN
!
!            Compute FLUX only if SWITCH is 1.0
!
             IF ( PMSA(IP11) .GT. 0.5 ) THEN

!               Compute relative time within day of time step to come

                T1 = (TIMSIM-INT(TIMSIM))*24.0 + TIMNUL
                IF ( T1 .GE. 24.0 ) T1 = T1 - 24.0
                T2 = T1 + DELTAT*24.0
                IF ( T2 .GT. 24.001 ) THEN
                    T2 = 24.
                    T1 = T2 - DELTAT*24.0
                ENDIF
                PMSA(IP12) = T1

!               Compute flux for interval [T1:T2] by subtracting integrals
!               for both times and dividing by time interval

                FL(IFLUX)  = (( INTEGR(NINT(T2*12.0))
     J                         -INTEGR(NINT(T1*12.0)) )
     J                       / (T2-T1)* (FPPTOT+FRESPI)
     J                       - FRESPI ) / DEPTHW

             ELSE
                FL(IFLUX)  = 0.0
             ENDIF
          ENDIF
!
          IFLUX = IFLUX + NOFLUX
!
          IP1  = IP1  + INCREM( 1)
          IP2  = IP2  + INCREM( 2)
          IP3  = IP3  + INCREM( 3)
          IP4  = IP4  + INCREM( 4)
          IP5  = IP5  + INCREM( 5)
          IP6  = IP6  + INCREM( 6)
          IP7  = IP7  + INCREM( 7)
          IP8  = IP8  + INCREM( 8)
          IP9  = IP9  + INCREM( 9)
          IP10 = IP10 + INCREM(10)
          IP11 = IP11 + INCREM(11)
          IP12 = IP12 + INCREM(12)
!
 9000 CONTINUE
!
      RETURN
!
      END
