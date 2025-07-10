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
module m_vbgro
    use m_waq_precision

    implicit none

contains


    subroutine VBGRO      (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_logger_helper, only : write_error_message, get_log_unit_number
        use m_extract_waq_attribute

        !
        !*******************************************************************************
        !
        IMPLICIT NONE
        !
        !     Type    Name         I/O Description
        !
        real(kind = real_wp) :: process_space_real(*)     !I/O Process Manager System Array, window of routine to process library
        real(kind = real_wp) :: fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(60) ! I  Array of pointers in process_space_real to get and store the data
        integer(kind = int_wp) :: increm(60) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: num_cells       ! I  Number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux      ! I  Number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
        integer(kind = int_wp) :: num_exchanges_u_dir        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
        integer(kind = int_wp) :: num_exchanges_v_dir        ! I  Nr of exchanges in 2nd direction, num_exchanges_u_dir+num_exchanges_v_dir gives hor. dir. reg. grid
        integer(kind = int_wp) :: num_exchanges_z_dir        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: num_exchanges_bottom_dir        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
        integer(kind = int_wp) :: ipnt(60)   !    Local work array for the pointering
        integer(kind = int_wp) :: iseg        !    Local loop counter for computational element loop
        !
        !*******************************************************************************
        !
        !     Type    Name         I/O Description                                        Unit
        !
        real(kind = real_wp) :: VB1         ! I  vegetation biomass cohort 1                        (gC/m2)
        real(kind = real_wp) :: maxVB1      ! I  maximum vegetation biomass cohort 1                (Tdm/hac)
        real(kind = real_wp) :: minVB1      ! I  minimum vegetation biomass cohort 1                (Tdm/hac)
        real(kind = real_wp) :: hlfAgeVB1   ! I  age where biomass is half of maximum cohort 1      (d)
        real(kind = real_wp) :: sfVB1       ! I  shape factor growth curve cohort 1                 (-)
        real(kind = real_wp) :: dmCfVB1     ! I  dry matter carbon ratio veg. cohort 1              (dm/gC)
        real(kind = real_wp) :: iniVB1      ! I  initial veg. biomass cohort 1                      (Tdm/hac)
        real(kind = real_wp) :: iniCovVB1   ! I  initial veg. coverage cohort 1                     (hac/ha)
        !     real(kind=real_wp), allocatable ::SWini(:)
        !                         ! I  switch 0=firsttime init 1=biomass 2=fr coverage cohort 1   (-)
        real(kind = real_wp) :: Surf        ! I  horizontal surface area of a DELWAQ segment        (m2)
        real(kind = real_wp) :: DELT        ! I  timestep for processes                             (d)
        real(kind = real_wp) :: Volume      ! I  volume of computational cell                       (m3)
        real(kind = real_wp) :: ageVB1      ! O  age of vegation cohort 1                           (d)
        real(kind = real_wp) :: VB1ha       ! O  vegetation biomass cohort 1                        (TC/hac)
        real(kind = real_wp) :: VBA1ha      ! O  attainable vegetation biomass cohort 1             (TC/hac)
        real(kind = real_wp) :: rGWV1       ! O  growth rate vegetation biomass cohort 1            (1/d)
        real(kind = real_wp) :: fVB1        ! O  growth rate vegetation biomass cohort 1            (gC/m2/d)
        real(kind = real_wp) :: dVB1        ! F  growth rate vegetation biomass cohort 1            (gC/m3/d)
        real(kind = real_wp) :: SwGrowth    ! I  switch 0=no growth 1=growth                        (-)
        real(kind = real_wp) :: SwMrt       ! I  switch 0=no mortality 1=mortality                  (-)
        real(kind = real_wp) :: VBAge0ha    ! O  vegetation biomass per ha at age is zero       (gC/ha)
        integer(kind = int_wp) :: IdVB1       !    Pointer to the growth rate vegetation biomass cohort 1
        integer(kind = int_wp) :: VBType      ! I  code of vegetation type for error and warnings      (-)
        integer(kind = int_wp) :: SWiniVB1    !    0=no init, 1=init.
        integer(kind = int_wp) :: SWregro     !    0=no regrowth, 1=regrowth allowed
        logical, save :: first = .true.      !
        integer(kind = int_wp) :: ikmrk1         ! first feature
        integer(kind = int_wp) :: ikmrk2         ! second feature
        integer(kind = int_wp) :: ILUMON
        !     integer(kind=int_wp), allocatable, save ::SWDying(:)     ! keep track off whether veg. is dying
        integer(kind = int_wp) :: SWDying

        real(kind = real_wp) :: navail      ! i  available nitrogen                                 (g/m2)
        real(kind = real_wp) :: pavail      ! i  available nitrogen                                 (g/m2)
        real(kind = real_wp) :: savail      ! i  available nitrogen                                 (g/m2)
        real(kind = real_wp) :: FravailM    ! i  fraction available nutrient for uptake             (-)
        real(kind = real_wp) :: F1VB        ! I  allocation factor comp. 1 (stem) VB01              (-)
        real(kind = real_wp) :: F2VB        ! I  allocation factor comp. 2 (foliage) VB01           (-)
        real(kind = real_wp) :: F3VB        ! I  allocation factor comp. 3 (branch) VB01            (-)
        real(kind = real_wp) :: F4VB        ! I  allocation factor comp. 4 (root) VB01              (-)
        real(kind = real_wp) :: F5VB        ! I  allocation factor comp. 5 (fineroot) VB01          (-)
        real(kind = real_wp) :: CNf1VB      ! I  carbon-nitrogen ratio in stem VB01                 (gC/gN)
        real(kind = real_wp) :: CNf2VB      ! I  carbon-nitrogen ratio in foliage VB01              (gC/gN)
        real(kind = real_wp) :: CNf3VB      ! I  carbon-nitrogen ratio in branch VB01               (gC/gN)
        real(kind = real_wp) :: CNf4VB      ! I  carbon-nitrogen ratio in root VB01                 (gC/gN)
        real(kind = real_wp) :: CNf5VB      ! I  carbon-nitrogen ratio in fineroot VB01             (gC/gN)
        real(kind = real_wp) :: CPf1VB      ! I  carbon-phosporus ratio in stem VB01                (gC/gP)
        real(kind = real_wp) :: CPf2VB      ! I  carbon-phosporus ratio in foliage VB01             (gC/gP)
        real(kind = real_wp) :: CPf3VB      ! I  carbon-phosporus ratio in branch VB01              (gC/gP)
        real(kind = real_wp) :: CPf4VB      ! I  carbon-phosporus ratio in root VB01                (gC/gP)
        real(kind = real_wp) :: CPf5VB      ! I  carbon-phosporus ratio in fineroot VB01            (gC/gP)
        real(kind = real_wp) :: CSf1VB      ! I  carbon-sulphur ratio in stem VB01                  (gC/gS)
        real(kind = real_wp) :: CSf2VB      ! I  carbon-sulphur ratio in foliage VB01               (gC/gS)
        real(kind = real_wp) :: CSf3VB      ! I  carbon-sulphur ratio in branch VB01                (gC/gS)
        real(kind = real_wp) :: CSf4VB      ! I  carbon-sulphur ratio in root VB01                  (gC/gS)
        real(kind = real_wp) :: CSf5VB      ! I  carbon-sulphur ratio in fineroot                   (gC/gS)
        real(kind = real_wp) :: weighCN     ! I  n content of VB01                                     (gN)
        real(kind = real_wp) :: weighCP     ! I  p content of VB01                                     (gP)
        real(kind = real_wp) :: weighCS     ! I  s content of VB01                                     (gS)
        real(kind = real_wp) :: NutGroFac   ! I  nutrient growth factor <0=full lim, 1=no lim           (-)
        real(kind = real_wp) :: dVB1MaxNl   ! O  maximum growth rate acc to avail nutrients       (gC/m2/d)
        real(kind = real_wp) :: initAge     ! I  initial age of vegetation at start of simulation       (d)
        real(kind = real_wp) :: iniSWDying  ! I  initial status of vegetation at start of simulation    (d)
        real(kind = real_wp) :: SwWV        ! I  use wetland vegetation model (0=no,1=yes)              (-)
        real(kind = real_wp) :: Rc0GWV      ! I  wetland vegetation growth rate VB01 at 20 oC         (1/d)
        real(kind = real_wp) :: TcGWV       ! I  temperature coefficient of WV growth for VB01          (-)
        real(kind = real_wp) :: AcGWV       ! I  acceleration factor for WV growth of VB01              (-)
        real(kind = real_wp) :: MinRWV      ! I  minimum biomass ratio for VB01                         (-)
        real(kind = real_wp) :: TBmWV       ! I  target total biomass for VB01                      (tc/ha)
        real(kind = real_wp) :: TempAir     ! I  Air temperature                                       (oC)
        real(kind = real_wp) :: minVB       ! I  minimum biomass for all vegetation                 (gC/m2)

        ! Local variables
        integer(kind = int_wp) :: nrofinputs  !    Number of inputs
        real(kind = real_wp) :: Temp20      !    Air temperature minus 20                              (oC)
        real(kind = real_wp) :: TempCof     !    Temperature coefficient
        real(kind = real_wp) :: rVB1        !    Ratio between current biomass and target biomass       (-)
        integer(kind = int_wp), save :: ifirst(1:18) = 0     !    for 2x initialisation of 9 types veg
        !      integer(kind=int_wp), allocatable, save ::iknmrk_save(:) ! copy of the original feature array
        !
        !*******************************************************************************
        !
        ipnt = ipoint
        IdVB1 = 1

        CALL get_log_unit_number(ILUMON)

        do  iseg = 1, num_cells

            !        lowest water and 2d segments only (also when dry!)
            call extract_waq_attribute(1, iknmrk(iseg), ikmrk1)
            call extract_waq_attribute(2, iknmrk(iseg), ikmrk2)
            if (ikmrk1<3 .and. (ikmrk2==0).or.(ikmrk2==3)) then
                !

                VB1 = process_space_real(ipnt(1))
                maxVB1 = process_space_real(ipnt(2))
                minVB1 = process_space_real(ipnt(3))
                hlfAgeVB1 = process_space_real(ipnt(4))
                sfVB1 = process_space_real(ipnt(5))
                dmCfVB1 = process_space_real(ipnt(6))
                iniVB1 = process_space_real(ipnt(7))
                iniCovVB1 = process_space_real(ipnt(8))
                SWiniVB1 = process_space_real(ipnt(9))
                ageVB1 = process_space_real(ipnt(10))
                Surf = process_space_real(ipnt(11))
                DELT = process_space_real(ipnt(12))
                Volume = process_space_real(ipnt(13))
                SwGrowth = process_space_real(ipnt(14))
                SwMrt = process_space_real(ipnt(15))
                VBType = process_space_real(ipnt(16))
                Navail = process_space_real(ipnt(17))
                Pavail = process_space_real(ipnt(18))
                Savail = process_space_real(ipnt(19))
                FravailM = process_space_real(ipnt(20))
                F1VB = process_space_real(ipnt(21))
                F2VB = process_space_real(ipnt(22))
                F3VB = process_space_real(ipnt(23))
                F4VB = process_space_real(ipnt(24))
                F5VB = process_space_real(ipnt(25))
                CNf1VB = process_space_real(ipnt(26))
                CNf2VB = process_space_real(ipnt(27))
                CNf3VB = process_space_real(ipnt(28))
                CNf4VB = process_space_real(ipnt(29))
                CNf5VB = process_space_real(ipnt(30))
                CPf1VB = process_space_real(ipnt(31))
                CPf2VB = process_space_real(ipnt(32))
                CPf3VB = process_space_real(ipnt(33))
                CPf4VB = process_space_real(ipnt(34))
                CPf5VB = process_space_real(ipnt(35))
                CSf1VB = process_space_real(ipnt(36))
                CSf2VB = process_space_real(ipnt(37))
                CSf3VB = process_space_real(ipnt(38))
                CSf4VB = process_space_real(ipnt(39))
                CSf5VB = process_space_real(ipnt(40))
                SWRegro = process_space_real(ipnt(41))
                SWDying = process_space_real(ipnt(42))
                InitAge = process_space_real(ipnt(43))
                IniSWDying = process_space_real(ipnt(44))
                SwWV = process_space_real(ipnt(45))
                Rc0GWV = process_space_real(ipnt(46))
                TcGWV = process_space_real(ipnt(47))
                AcGWV = process_space_real(ipnt(48))
                MinRWV = process_space_real(ipnt(49))
                TBmWV = process_space_real(ipnt(50))
                TempAir = process_space_real(ipnt(51))
                MinVB = process_space_real(ipnt(52))
                nrofinputs = 52

                if (ifirst (vbtype) == 0) then
                    AgeVB1 = InitAge
                endif

                if (ifirst (vbtype + 9) == 0) then
                    SWDying = IniSWDying
                endif

                !           evaluate use initialisation 0=no, 1=yes
                !           always use iniCovVB1, 100% (default) means ha=haC, if unequal 100% ha<>haC
                IF ((SWiniVB1 == 1) .or. (SWiniVB1 == 0)) THEN
                    iniCovVB1 = iniCovVB1 / 100
                ELSE
                    CALL write_error_message ('(no valid value for SWiniVB <0,1>')
                ENDIF

                IF ((SWregro /= 1) .and. (SWregro /= 0)) THEN
                    CALL write_error_message ('(no valid value for SWregro <0,1>')
                ENDIF

                ! ---        if the volume is zero, then the calculations below should be avoided altogether

                if (volume <= 0.0) then
                    cycle
                endif


                ! ---        only process significant coverages
                if (iniCovVB1 > 0.001) then

                    if(NINT(SwWV) == 0) then

                        !                 original VEGMOD

                        !                 convert Ton DM/hac to gC/m2-cohort
                        iniVB1 = iniVB1 / dmCfVB1 * 100
                        minVB1 = minVB1 / dmCfVB1 * 100
                        maxVB1 = maxVB1 / dmCfVB1 * 100
                        nutgrofac = 1

                        !                 input shape factor (range 1-10) scaled to halfAge
                        sfVB1 = sfVB1 / hlfAgeVB1

                        !                 biomass at age=0 for later use to check if vegetation is dying after mortality
                        VBAge0ha = ((minVB1 - maxVB1) / (1 + exp(sfVB1 * (- hlfAgeVB1))) + maxVB1) / 100

                        !                 Check values growth limitation (only 0 or 1)
                        IF ((NINT(SWgrowth) /= 1) .and. (NINT(SwGrowth) /= 0)) THEN
                            CALL write_error_message ('(no valid value for SwGrowthVB <0,1>')
                        ENDIF

                        !                 Check values mort limitation (only 0 or 1)
                        IF ((NINT(SWmrt) /= 1) .and. (NINT(SwMrt) /= 0)) THEN
                            CALL write_error_message ('(no valid value for SWMrtVB <0,1>')
                        ENDIF

                        !                 Checking for nut availabilithy

                        if ((F4VB + F5VB) - 1.E-10 < 0.0) then
                            CALL write_error_message ('(no valid values for F4VB and F5VB (allocation factors vegetation  roots)')
                        else
                            !                    average Nutrient content of cohort
                            weighCN = F1VB * CNf1VB + F2VB * CNf2VB + F3VB * CNf3VB + F4VB * CNf4VB + F5VB * CNf5VB
                            weighCP = F1VB * CPf1VB + F2VB * CPf2VB + F3VB * CPf3VB + F4VB * CPf4VB + F5VB * CPf5VB
                            weighCS = F1VB * CSf1VB + F2VB * CSf2VB + F3VB * CSf3VB + F4VB * CSf4VB + F5VB * CSf5VB

                            dVB1MaxNL = FravailM * min(Navail * weighCN, Pavail * weighCP, Savail * weighCS) * IniCovVB1 / volume * surf / delt
                            dVB1MaxNL = max(0.0, dVB1MaxNL)

                        endif

                        !                 Evaluate initial conditions at start of simulation
                        IF (SWiniVB1 /= 0) THEN

                            !                    calculate age matching initial biomass
                            !                    check and maximise age to 2xhalfage

                            !                    initial biomass exceeds minimum
                            IF ((iniVB1 - minVB1) > 1.E-10) then
                                !                        initial biomass less than maximum biomass
                                IF ((iniVB1 / maxVB1) < 0.99) THEN
                                    ageVB1 = hlfAgeVB1 + LOG((minVB1 - maxVB1) / (iniVB1 - maxVB1) - 1) / sfVB1
                                ELSE
                                    WRITE (ILUMON, *) 'WARNING : Vegtype ', vbtype, ' init biom .ge. Max: ', iniVB1 * dmcfVB1 / 100, '>=', maxVB1 * dmcfVB1 / 100
                                    !                           age representing 99% of initial mass
                                    ageVB1 = hlfAgeVB1 + LOG((minVB1 - maxVB1) / ((0.99 - 1) * maxVB1) - 1) / sfVB1
                                ENDIF
                            ELSE
                                ageVB1 = 0.0
                            ENDIF

                            VBA1ha = ((minVB1 - maxVB1) / (1 + exp(sfVB1 * (ageVB1 - hlfAgeVB1))) + maxVB1) / 100

                            VB1ha = VB1 / iniCovVB1 / 100
                            dVB1 = (VBA1ha - VB1ha) * 100.0 * surf / volume / delt * iniCovVB1
                        ELSE

                            VB1ha = VB1 / iniCovVB1 / 100

                            !                    vegetation is flooded
                            if (NINT (SWmrt) == 1) then
                                SWDying = 1.0
                            endif

                            !                    check if vegetation died long enough - regrowth allowed again
                            if  ((VB1ha < VBAge0ha) .and. (SWDying == 1) .and.  SWRegro == 1)  then
                                SWDying = 0.0
                                ageVB1 = 0
                            endif

                            !                    calculate new age for decaying vegetation based on current biomass
                            !                    convert VBha (Tc/ha-cohort) to maxVB1 (gC/m2-cohort)
                            if (SWDying == 1) then
                                if (VB1ha <= VBAge0ha) then
                                    ageVB1 = 0
                                elseif ((VB1ha * 100 / maxVB1) < 0.99) then
                                    ageVB1 = hlfAgeVB1 + LOG((minVB1 - maxVB1) / (VB1ha * 100 - maxVB1) - 1) / sfVB1
                                else
                                    !                          cannot calc age
                                endif
                            endif

                            !                    calculate attainable biomass per ha using age
                            VBA1ha = ((minVB1 - maxVB1) / (1 + exp(sfVB1 * (ageVB1 - hlfAgeVB1))) + maxVB1) / 100

                            !                    no growth for standing stock either
                            IF ((NINT(SwGrowth) == 1) .and. (SWDying == 0) .and. SWRegro == 1) THEN

                                dVB1 = (VBA1ha - VB1ha) * 100.0 * surf / volume / delt * iniCovVB1
                                !                       growth reduction?
                                if (dVB1 > 1.E-20) then
                                    NutGroFac = min (1.0, (dVB1MaxNL / dVB1))
                                else
                                    NutGroFac = 1
                                endif
                                dVB1 = min(dVB1MaxNL, dVB1)

                                !                       there is (reduced?) growth
                                ageVB1 = ageVB1 + NutGroFac * DELT

                            ELSE
                                !                        no growth flux = 0
                                dVB1 = 0
                            ENDIF
                        END IF

                        ! if init then no nutrient uptake, set output to zero
                        if (SWiniVB1 /= 0) then
                            fVB1 = 0.0
                        else
                            fVB1 = dVB1 * volume / surf
                        endif

                    else

                        !                 =========================
                        !                 Wetland vegetation growth
                        !                 =========================

                        !                 Attainable biomass averaged over segment
                        VBA1ha = TBmWV / dmCfVB1 * iniCovVB1

                        if (ifirst (vbtype) == 0 .and. SWiniVB1 == 1) then
                            !                    Initialise biomass using target biomass and % coverage without nutrient uptake
                            !                    Current density within covered area
                            VB1ha = TBmWV / dmCfVB1
                            !                    Initialise by % times TBmWV
                            dVB1 = (VBA1ha * 100.0 - VB1) / volume * surf / DELT
                            fVB1 = 0.0
                            !                    flux for nutrient uptake is zero
                            NutGroFac = 1.0
                            !                    No actual growth rate can be calulated...
                            rGWV1 = 0.0
                        else if (VB1 < MinVB) then
                            !                    Very low/no biomass for species with significant coverage
                            !                    Current density within covered area
                            VB1ha = MinVB1 / 100.0 / iniCovVB1
                            !                    Initialise biomass using general minimum biomass without nutrient uptake
                            dVB1 = (MinVB - VB1) / volume * surf / DELT
                            fVB1 = 0.0
                            !                    flux for nutrient uptake is zero
                            NutGroFac = 1.0
                            !                    No actual growth rate can be calulated...
                            rGWV1 = 0.0
                        else
                            !                    Calculate actual growth
                            !                    Current density within covered area
                            VB1ha = VB1 / 100.0 / iniCovVB1

                            !                    Ratio of current biomass to attainable biomass
                            if (VBA1ha > 0.0) then
                                rVB1 = VB1 / (VBA1ha * 100.0)
                            else
                                rVB1 = merge(1.0, 0.0, VB1 > 0.0)
                            endif

                            if (rVB1 < 1.0 .and. NINT(SwGrowth) == 1 .and. NINT (SWmrt) == 0) then
                                !                       average Nutrient content of vegetation
                                weighCN = F1VB * CNf1VB + F2VB * CNf2VB + F3VB * CNf3VB + F4VB * CNf4VB + F5VB * CNf5VB
                                weighCP = F1VB * CPf1VB + F2VB * CPf2VB + F3VB * CPf3VB + F4VB * CPf4VB + F5VB * CPf5VB
                                weighCS = F1VB * CSf1VB + F2VB * CSf2VB + F3VB * CSf3VB + F4VB * CSf4VB + F5VB * CSf5VB

                                dVB1MaxNL = FravailM * min(Navail * weighCN, Pavail * weighCP, Savail * weighCS) * IniCovVB1 / volume * surf / delt
                                dVB1MaxNL = max(0.0, dVB1MaxNL)

                                TEMP20 = TempAir - 20.0
                                TempCof = TcGWV ** TEMP20

                                if (rVB1 < MinRWV) then
                                    dVB1 = VB1 * AcGWV * Rc0GWV * TempCof / volume * surf
                                else
                                    dVB1 = VB1 * Rc0GWV * TempCof / volume * surf
                                end if

                                !                       growth reduction?
                                if (dVB1 > 1.E-20) then
                                    NutGroFac = min (1.0, (dVB1MaxNL / dVB1))
                                else
                                    NutGroFac = 1.0
                                endif
                                dVB1 = min(dVB1MaxNL, dVB1)
                                !                       flux for nutrient uptake
                                fVB1 = dVB1 * volume / surf
                            else
                                dVB1 = 0.0
                                fVB1 = 0.0
                                NutGroFac = 1.0
                            end if
                        end if
                        if (VB1 > MinVB) then
                            rGWV1 = dVB1 / VB1
                        else
                            !                    No actual growth rate can be calulated...
                            rGWV1 = 0.0
                        endif
                        VBAge0ha = 0.0
                        SWDying = 0.0
                    end if
                    !
                    fl  (IdVB1) = dVB1                              ! g/m3/d
                    process_space_real(ipnt(nrofinputs + 1)) = ageVB1
                    process_space_real(ipnt(nrofinputs + 2)) = VB1ha
                    process_space_real(ipnt(nrofinputs + 3)) = VBA1ha
                    process_space_real(ipnt(nrofinputs + 4)) = rGWV1
                    process_space_real(ipnt(nrofinputs + 5)) = fVB1
                    process_space_real(ipnt(nrofinputs + 6)) = VBAge0ha
                    process_space_real(ipnt(nrofinputs + 7)) = SWDying
                    process_space_real(ipnt(nrofinputs + 8)) = NutGroFac

                    ! ---          check iniCovVB1 - only significant vegetation
                endif
                !        bottom and 2d segments only
            endif
            !
            IdVB1 = IdVB1 + noflux
            ipnt = ipnt + increm
            !
            !     segment loop
        enddo

        !     no need to initialise this cohort in next timestep
        !     SWiniVB1 = 0
        process_space_real(ipoint(9)) = 0.0
        ifirst (vbtype) = 1
        ifirst (vbtype + 9) = 1
        !
        return
    end

end module m_vbgro
