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
module m_rear
    use m_waq_precision

    implicit none

contains


    subroutine rear   (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_zerome
        use m_logger_helper, only : stop_with_error, get_log_unit_number
        use m_extract_waq_attribute

        !>\file
        !>       Reaeration of carbon dioxide and oxygen

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                   Units
        ! ----    --- -  -    -------------------                            ----
        ! DEPTH   R*4 1 I actual depth of the water column                     [m]
        ! FCOVER  R*4 1 I fraction of water surface covered <0-1>              [-]
        ! FL (1)  R*4 1 O reaeration flux                                 [g/m3/d]
        ! HCRT    R*4 1 I critical water depth/velocity                        [m]
        ! IFREAR  I*4 1 I switch for the rearation formula                     [-]
        ! MAXRRC  R*4 1 I maximum wat trf. coef. for temp. lim.              [m/d]
        ! MINRRC  R*4 1 I minimum reaeration rate                            [m/d]
        ! O2      R*4 1 I concentration of dissolved oxygen                 [g/m3]
        ! OXSAT   R*4 1 L saturation concentration of dissolved oxygen      [g/m3]
        ! RAIN    R*4 1 L rainfall rate                                     [mm/h]
        ! REARTC  R*4 1 L reaeration temperatuur coefficient                   [-]
        ! REARKL  R*4 1 L reaeration transfer coefficient                    [m/d]
        ! REARRC  R*4 1 L reaeration rate                                    [1/d]
        ! TEMP    R*4 1 I ambient temperature                                 [xC]
        ! TEMP20  R*4 1 L ambient temperature - stand. temp (20)              [xC]
        ! VELOC   R*4 1 I streamflow velocity                                [m/s]
        ! VWIND   R*4 1 I wind velocity                                      [m/s]

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        IMPLICIT NONE

        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
        !
        !     Local declarations
        !
        INTEGER(kind = int_wp) :: LUNREP
        INTEGER(kind = int_wp) :: IFREAR, IKMRK1, IKMRK2, ISEG, IFLUX
        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10, &
                IP11, IP12, IP13, IP14, IP15, IP16, IP17, IP18, IP19, IP20, &
                IP21, IP22, IP23, IP24, IP25, IP26, IP27
        REAL(kind = real_wp) :: SAL, B_ENHA, SC, SC20, KLREAR, TOTDEP, &
                REARTC, REARRC, HCRT, VELOC, VWIND, TEMP, &
                OXSAT, O2, TEMP20, TMPCF, DEPTH, FL1, &
                FCOVER, MAXRRC, REARKL, SATPERC, MINRRC, DELT, RAIN
        REAL(kind = real_wp) :: A, B1, B2, C1, &
                C2, D1, D2, D3, D4, &
                A_O, B_O, C_O, D_O, E_O, &
                A_CO, B_CO, C_CO, D_CO, E_CO

        !   PBo3: hard coded coefficients for salt water options Wannikhof
        !     Parameters for Schmidt number calculation (Wanninkhoff and Guerin)
        !     D1-4Os = oxygen Wannikhof (seawater)
        !     D1-4Cs = CO2 Wannikhof (seawater)

        PARAMETER ( A_O = 1920.4, &
                    B_O = -135.6, &
                    C_O = 5.2122, &
                    D_O = -0.10939, &
                    E_O = 0.00093777, &
                    A_CO = 2116.8, &
                    B_CO = -136.25, &
                    C_CO = 4.7353, &
                    D_CO = -0.092307, &
                    E_CO = 0.0007555)

        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)
        IP6 = IPOINT(6)
        IP7 = IPOINT(7)
        IP8 = IPOINT(8)
        IP9 = IPOINT(9)
        IP10 = IPOINT(10)
        IP11 = IPOINT(11)
        IP12 = IPOINT(12)
        IP13 = IPOINT(13)
        IP14 = IPOINT(14)
        IP15 = IPOINT(15)
        IP16 = IPOINT(16)
        IP17 = IPOINT(17)
        IP18 = IPOINT(18)
        IP19 = IPOINT(19)
        IP20 = IPOINT(20)
        IP21 = IPOINT(21)
        IP22 = IPOINT(22)
        IP23 = IPOINT(23)
        IP24 = IPOINT(24)
        IP25 = IPOINT(25)
        IP26 = IPOINT(26)
        IP27 = IPOINT(27)

        !
        IFLUX = 0
        DO ISEG = 1, num_cells
            CALL extract_waq_attribute(1, IKNMRK(ISEG), IKMRK1)
            IF (IKMRK1==1) THEN

                !         Compute saturation percentage for all layers

                O2 = process_space_real(IP1)
                OXSAT = process_space_real(IP10)
                SATPERC = O2 / OXSAT * 100
                process_space_real (IP27) = SATPERC

                CALL extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                IF ((IKMRK2==0).OR.(IKMRK2==1)) THEN
                    !
                    DEPTH = process_space_real(IP2)
                    TEMP = process_space_real(IP3)
                    VELOC = process_space_real(IP4)
                    VWIND = process_space_real(IP5)
                    IFREAR = process_space_real(IP6) + 0.5
                    REARKL = process_space_real(IP7)
                    REARTC = process_space_real(IP8)
                    DELT = process_space_real(IP9)
                    SAL = process_space_real(IP11)
                    TOTDEP = process_space_real(IP12)
                    FCOVER = process_space_real(IP13)
                    MAXRRC = process_space_real(IP14)
                    MINRRC = process_space_real(IP15)
                    RAIN = process_space_real(IP16)
                    A = process_space_real(IP17)
                    B1 = process_space_real(IP18)
                    B2 = process_space_real(IP19)
                    C1 = process_space_real(IP20)
                    C2 = process_space_real(IP21)
                    D1 = process_space_real(IP22)
                    D2 = process_space_real(IP23)
                    D3 = process_space_real(IP24)
                    D4 = process_space_real(IP25)

                    IF (DEPTH   < 1E-30) CALL ZEROME ('DEPTH in REAR')

                    !     JvG, 1 May 2002
                    !     Current formulation was not valid for layered schematisations
                    !     Correct by using the methodology as follows:
                    !     a) compute surface transfer coefficient in m/day per method
                    !     b) compute flux by multiplying with (surface) deficit
                    !     c) convert to volumetric flux by using the (surface) layer thickness

                    select case (IFREAR)
                    case(0)
                        !
                        !         0. Unscaled user input coefficient in 1/day
                        !
                        REARRC = REARKL * TOTDEP

                    case(1)
                        !
                        !         1. User input coefficient in m/day
                        !
                        REARRC = REARKL

                    case(2)
                        !
                        !         2. Churchill [1962]
                        !
                        REARRC = 0.0
                        IF (VELOC  > 1E-30) &
                                REARRC = 5.026 * (VELOC**0.969) / (TOTDEP**0.673)

                    case(3)
                        !
                        !         3. O'Connor - Dobbins [1958]
                        !
                        REARRC = 0.0
                        IF (VELOC  > 1E-30) &
                                REARRC = 3.863 * (VELOC**0.5) / (TOTDEP**0.5)

                    case(4)
                        !
                        !         4. Scaled version of O'Connor - Dobbins [1958]
                        !
                        REARRC = 0.0
                        IF (VELOC  > 1E-30) &
                                REARRC = 3.863 * (VELOC**0.5) / (TOTDEP**0.5) * REARKL

                    case(5)
                        !
                        !         5. Owens - Edwards - Gibb [1964]
                        !
                        REARRC = 0.0
                        IF (VELOC  > 1E-30) &
                                REARRC = 5.322 * (VELOC**0.67) / (TOTDEP**0.85)

                    case(6)
                        !
                        !         6. Langbien - Durum [1967]
                        !
                        REARRC = 0.0
                        IF (VELOC  > 1E-30) &
                                REARRC = 11.23 * VELOC / (TOTDEP**0.333)

                    case(7)
                        !
                        !         7. Van Pagee[1978] and Delvigne [1980]
                        !
                        IF (VELOC  > 1E-30) THEN
                            REARRC = (REARKL * 0.065 * VWIND**2 + &
                                    3.86 * SQRT(VELOC / TOTDEP))
                        ELSE
                            REARRC = REARKL * 0.065 * VWIND**2
                        ENDIF

                    case(8)
                        !
                        !         8. Thackston - Krenkel [1966]
                        !
                        CALL get_log_unit_number(LUNREP)
                        WRITE (LUNREP, *) &
                                ' Reaeration formula 8 has not been implemented'
                        WRITE (*, *) ' Reaeration formula 8 has not been implemented'
                        CALL stop_with_error()

                    case(9)
                        !
                        !         9. DBS
                        !
                        REARRC = (0.30 + REARKL * 0.028 * VWIND**2)

                    case(10)
                        !
                        !        10. Wanninkhof Oxygen
                        !
                        IF (SAL > 5.0) THEN
                            SC = A_O + B_O * TEMP + C_O * TEMP**2 + D_O * TEMP**3 + E_O * TEMP**4
                            SC20 = A_O + B_O * 20.0 + C_O * 20.0**2 + D_O * 20.0**3 + E_O * 20.0**4
                        ELSE
                            SC = D1 - D2 * TEMP + D3 * TEMP**2 - D4 * TEMP**3
                            SC20 = D1 - D2 * 20.0 + D3 * 20.0**2 - D4 * 20.0**3
                        ENDIF
                        KLREAR = 0.31 * VWIND**2 * (SC / SC20)**(-0.5) * 24. / 100.
                        REARRC = KLREAR

                    case(11)
                        !
                        !        10. Wanninkhof CO2
                        !
                        IF (SAL > 5.0) THEN
                            SC = A_CO + B_CO * TEMP + C_CO * TEMP**2 + D_CO * TEMP**3 + E_CO * TEMP**4
                            SC20 = A_CO + B_CO * 20.0 + C_CO * 20.0**2 + D_CO * 20.0**3 + E_CO * 20.0**4
                        ELSE
                            SC = D1 - D2 * TEMP + D3 * TEMP**2 - D4 * TEMP**3
                            SC20 = D1 - D2 * 20.0 + D3 * 20.0**2 - D4 * 20.0**3
                        ENDIF
                        B_ENHA = 2.5 * (.5246 + 1.6256E-2 * TEMP + 4.9946E-4 * TEMP**2)
                        KLREAR = (B_ENHA + 0.31 * VWIND**2) * (SC / SC20)**(-0.5) * 24. / 100.
                        REARRC = KLREAR

                    case(12)

                        !     Note this option is not included in the process documentation!
                        !
                        !         12. Hybride formulation using O'Connor - Dobbins [1958]
                        !             and Owens - Edwards - Gibb [1964]
                        !
                        REARRC = 0.0
                        HCRT = 3.93 / 5.32 * TOTDEP**0.35
                        IF (VELOC  > 1E-30) THEN
                            IF (VELOC < HCRT**6) THEN
                                REARRC = 3.93 * (VELOC**0.5) / (TOTDEP**0.5)
                            ELSE
                                REARRC = 5.32 * (VELOC**0.67) / (TOTDEP**0.85)
                            ENDIF
                        ENDIF
                        REARRC = MAX(MINRRC, REARRC)

                    case(13)
                        !
                        !        13. Guerin O2  - only fresh water
                        !            Guerin CO2 - only fresh water
                        !            Guerin CH4 - only fresh water
                        !
                        SC = D1 - D2 * TEMP + D3 * TEMP**2 - D4 * TEMP**3
                        SC20 = D1 - D2 * 20.0 + D3 * 20.0**2 - D4 * 20.0**3

                        KLREAR = (A * EXP(B1 * VWIND**B2) + C1 * Rain**C2) * (SC / SC20)**(-0.67)
                        REARRC = KLREAR

                        REARTC = 1.0
                        !
                    case default
                        CALL get_log_unit_number(LUNREP)
                        WRITE (LUNREP, *) ' Invalid option for reaeration formula'
                        WRITE (*, *) ' Invalid option for reaeration formula'
                        CALL stop_with_error()
                    end select

                    process_space_real (IP26) = REARRC / DEPTH

                    !     Calculation of rearation flux ( M.L-1.DAY)
                    !     negatieve zuurstof wordt 0 gemaakt i.v.m. deficiet berekening!
                    !     Wanninkhof, don't use temperature dependency

                    O2 = MAX (O2, 0.0)
                    IF (IFREAR == 10 .OR. IFREAR == 11) THEN
                        FL1 = REARRC * (OXSAT - O2) / DEPTH
                    ELSE
                        TEMP20 = TEMP - 20.0
                        TMPCF = REARTC ** TEMP20
                        IF (REARRC<=MAXRRC) THEN
                            REARRC = REARRC * TMPCF
                        ENDIF
                        FL1 = MIN(1.0 / DELT, REARRC * (1 - FCOVER) / DEPTH) * (OXSAT - O2)
                    ENDIF

                    !     Limitation of FL(1) to amount of oxygen present
                    IF (FL1 < 0.0) THEN
                        FL1 = MAX (-1. * O2 / DELT, FL1)
                    ENDIF
                    FL(1 + IFLUX) = FL1

                ENDIF
            ENDIF
            !
            IFLUX = IFLUX + NOFLUX
            IP1 = IP1 + INCREM (1)
            IP2 = IP2 + INCREM (2)
            IP3 = IP3 + INCREM (3)
            IP4 = IP4 + INCREM (4)
            IP5 = IP5 + INCREM (5)
            IP6 = IP6 + INCREM (6)
            IP7 = IP7 + INCREM (7)
            IP8 = IP8 + INCREM (8)
            IP9 = IP9 + INCREM (9)
            IP10 = IP10 + INCREM (10)
            IP11 = IP11 + INCREM (11)
            IP12 = IP12 + INCREM (12)
            IP13 = IP13 + INCREM (13)
            IP14 = IP14 + INCREM (14)
            IP15 = IP15 + INCREM (15)
            IP16 = IP16 + INCREM (16)
            IP17 = IP17 + INCREM (17)
            IP18 = IP18 + INCREM (18)
            IP19 = IP19 + INCREM (19)
            IP20 = IP20 + INCREM (20)
            IP21 = IP21 + INCREM (21)
            IP22 = IP22 + INCREM (22)
            IP23 = IP23 + INCREM (23)
            IP24 = IP24 + INCREM (24)
            IP25 = IP25 + INCREM (25)
            IP26 = IP26 + INCREM (26)
            IP27 = IP27 + INCREM (27)
            !
        end do
        !
        RETURN
        !
    END

end module m_rear
