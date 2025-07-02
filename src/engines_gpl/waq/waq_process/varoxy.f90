!!  Copyright (C)  Stichting Deltares, 2012-2025.
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
      module m_varoxy
      use m_waq_precision


      implicit none

      contains


      subroutine varoxy ( process_space_real   , fl     , ipoint , increm , num_cells  , &
                         noflux , iexpnt , iknmrk , num_exchanges_u_dir   , num_exchanges_v_dir   , &
                         num_exchanges_z_dir   , num_exchanges_bottom_dir   )
      use m_logger_helper, only : stop_with_error, get_log_unit_number

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

      REAL(kind=real_wp) ::process_space_real  ( * ) , FL    (*)
      INTEGER(kind=int_wp) ::IPOINT( * ) , INCREM(*) , num_cells , NOFLUX, &
              IEXPNT(4,*) , IKNMRK(*) , num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

      INTEGER(kind=int_wp) ::LUNREP

      INTEGER(kind=int_wp) ::IP1 , IP2 , IP3 , IP4 , IP5 , IP6 , IP7 , IP8 , IP9 , &
              IP10, IP11, IP12, I, IFLUX, ISEG
      REAL(kind=real_wp) ::TIMSIM, DELTAT, TIMNUL, T1MXPP, T2MXPP, DAYLEN, FPPTOT, &
              FRESPI, DEPTHW, T1    , T2    , PPMAX , TRISE , &
              TSET  , TOTAL , V1    , V2
      REAL(kind=real_wp) ::INTEGR(0:12*24), PPLAST, RELAST, DAYLLAST
      SAVE     PPLAST, RELAST, DAYLLAST, INTEGR
      DATA     PPLAST, RELAST, DAYLLAST /-999.,-999.,-999./
      INTEGER(kind=int_wp) ::NR_MES
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

      IF ( (INCREM(1) > 0) .OR. &
          (INCREM(2) > 0) .OR. &
          (INCREM(3) > 0) .OR. &
          (INCREM(4) > 0) .OR. &
          (INCREM(5) > 0) .OR. &
          (INCREM(6) > 0) ) THEN

          CALL get_log_unit_number(LUNREP)
          WRITE (LUNREP,*) &
         ' VAROXY: Time parameters function(x) not ALLOWED'
          WRITE (*,*) &
         ' VAROXY: Time parameters function(x) not ALLOWED'
          CALL stop_with_error()
      ENDIF

      IFLUX = 1
      DO ISEG = 1 , num_cells

          TIMSIM = process_space_real(IP1)/process_space_real(IP3)
          DELTAT = process_space_real(IP4)
          TIMNUL = process_space_real(IP2)
          T1MXPP = process_space_real(IP5)
          T2MXPP = process_space_real(IP6)
          DAYLEN = process_space_real(IP7)*24.
          FPPTOT = process_space_real(IP8)
          FRESPI = process_space_real(IP9)
          DEPTHW = process_space_real(IP10)
          TRISE  = 12.0-0.5*DAYLEN
          TSET   = 12.0+0.5*DAYLEN

!         Initialize light variation curve for present cycle
!         ONLY if fluxes have changed

          IF ( (ISEG == 1) .OR. &
                  (ABS(DAYLEN-DAYLLAST) > 1E-3) .OR. &
                  (ABS(FPPTOT-PPLAST) > 1E-3) .OR. &
                  (ABS(FRESPI-RELAST) > 1E-3) ) THEN

              PPLAST = FPPTOT
              RELAST = FRESPI
              DAYLLAST = DAYLEN

!             Check on conditions for daylength

              IF ( T1MXPP < TRISE .OR. &
                  T2MXPP > TSET ) THEN
                 IF ( NR_MES < 25 ) THEN
                    NR_MES = NR_MES + 1
                    WRITE(*,*) ' WARNING: VAROXY limited values of T1MXPP/T2MXPP to daylight range'
                 ENDIF
                 IF ( NR_MES == 25 ) THEN
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
              DO I = 1,12*24
                  T2 = REAL(I)/12.
                  IF ( T2 <= TRISE .OR. T2 >= TSET ) THEN
                      V2 = 0.0
                  ELSEIF ( T2>TRISE .AND. T2<T1MXPP ) THEN
                      V2 = PPMAX*(T2-TRISE)/(T1MXPP-TRISE)
                  ELSEIF ( T2>=T1MXPP .AND. T2<=T2MXPP ) THEN
                      V2 = PPMAX
                  ELSEIF ( T2>T2MXPP .AND. T2<TSET ) THEN
                      V2 = PPMAX*(1.0-(T2-T2MXPP)/(TSET- T2MXPP) )
                  ENDIF
                  TOTAL = TOTAL + ((V1+V2)/2.0) * (T2-T1)
                  INTEGR(I) = TOTAL
                  V1 = V2
                  T1 = T2
              end do
          ENDIF

          IF (BTEST(IKNMRK(ISEG),0)) THEN
!
!            Compute FLUX only if SWITCH is 1.0
!
             IF ( process_space_real(IP11) > 0.5 ) THEN

!               Compute relative time within day of time step to come

                T1 = (TIMSIM-INT(TIMSIM))*24.0 + TIMNUL
                IF ( T1 >= 24.0 ) T1 = T1 - 24.0
                T2 = T1 + DELTAT*24.0
                IF ( T2 > 24.001 ) THEN
                    T2 = 24.
                    T1 = T2 - DELTAT*24.0
                ENDIF
                process_space_real(IP12) = T1

!               Compute flux for interval [T1:T2] by subtracting integrals
!               for both times and dividing by time interval

                FL(IFLUX)  = (( INTEGR(NINT(T2*12.0)) &
                              -INTEGR(NINT(T1*12.0)) ) &
                            / (T2-T1)* (FPPTOT+FRESPI) &
                            - FRESPI ) / DEPTHW

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
      end do
!
      RETURN
!
      END

      end module m_varoxy
