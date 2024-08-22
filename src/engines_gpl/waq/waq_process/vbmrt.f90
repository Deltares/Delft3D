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
module m_vbmrt
    use m_waq_precision

    implicit none

contains


    subroutine VBMRT      (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
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
        integer(kind = int_wp) :: ipoint(79) ! I  Array of pointers in process_space_real to get and store the data
        integer(kind = int_wp) :: increm(79) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: num_cells       ! I  Number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux      ! I  Number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
        integer(kind = int_wp) :: num_exchanges_u_dir        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
        integer(kind = int_wp) :: num_exchanges_v_dir        ! I  Nr of exchanges in 2nd direction, num_exchanges_u_dir+num_exchanges_v_dir gives hor. dir. reg. grid
        integer(kind = int_wp) :: num_exchanges_z_dir        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: num_exchanges_bottom_dir        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
        integer(kind = int_wp) :: ipnt(79)   !    Local work array for the pointering
        integer(kind = int_wp) :: iseg        !    Local loop counter for computational element loop
        !
        !*******************************************************************************
        !
        !     Type    Name         I/O Description                                        Unit
        !
        real(kind = real_wp) :: VB1         ! I  vegetation biomass cohort 1                        (gC/m2)
        real(kind = real_wp) :: F1VB01      ! I  allocation factor comp. 1 (stem) VB 01             (-)
        real(kind = real_wp) :: F2VB01      ! I  allocation factor comp. 2 (foliage) VB 01          (-)
        real(kind = real_wp) :: F3VB01      ! I  allocation factor comp. 3 (branch) VB 01           (-)
        real(kind = real_wp) :: F4VB01      ! I  allocation factor comp. 4 (root) VB 01             (-)
        real(kind = real_wp) :: F5VB01      ! I  allocation factor comp. 5 (fineroot) VB 01         (-)
        real(kind = real_wp) :: SwVB01Mrt   ! I  vegetation biomass dead (0=no,1=yes)               (-)
        real(kind = real_wp) :: SwDying     ! I  vegetation biomass dying (0=no,1=yes)              (-)
        real(kind = real_wp) :: ageVB1      ! I  age of vegation cohort 1                           (d)
        real(kind = real_wp) :: CNf1VB01    ! I  carbon-nitrogen ratio in stem VB01                 (gC/gN)
        real(kind = real_wp) :: CNf2VB01    ! I  carbon-nitrogen ratio in foliage VB01              (gC/gN)
        real(kind = real_wp) :: CNf3VB01    ! I  carbon-nitrogen ratio in branch VB01               (gC/gN)
        real(kind = real_wp) :: CNf4VB01    ! I  carbon-nitrogen ratio in root VB01                 (gC/gN)
        real(kind = real_wp) :: CNf5VB01    ! I  carbon-nitrogen ratio in fineroot VB01             (gC/gN)
        real(kind = real_wp) :: CPf1VB01    ! I  carbon-phosporus ratio in stem VB01                (gC/gP)
        real(kind = real_wp) :: CPf2VB01    ! I  carbon-phosporus ratio in foliage VB01             (gC/gP)
        real(kind = real_wp) :: CPf3VB01    ! I  carbon-phosporus ratio in branch VB01              (gC/gP)
        real(kind = real_wp) :: CPf4VB01    ! I  carbon-phosporus ratio in root VB01                (gC/gP)
        real(kind = real_wp) :: CPf5VB01    ! I  carbon-phosporus ratio in fineroot VB01            (gC/gP)
        real(kind = real_wp) :: CSf1VB01    ! I  carbon-sulphur ratio in stem VB01                  (gC/gS)
        real(kind = real_wp) :: CSf2VB01    ! I  carbon-sulphur ratio in foliage VB01               (gC/gS)
        real(kind = real_wp) :: CSf3VB01    ! I  carbon-sulphur ratio in branch VB01                (gC/gS)
        real(kind = real_wp) :: CSf4VB01    ! I  carbon-sulphur ratio in root VB01                  (gC/gS)
        real(kind = real_wp) :: CSf5VB01    ! I  carbon-sulphur ratio in fineroot                   (gC/gS)
        real(kind = real_wp) :: FfolPOC1    ! I  fraction of biomass foliage to POC1                (-)
        real(kind = real_wp) :: FfolPOC2    ! I  fraction of biomass foliage to POC2                (-)
        real(kind = real_wp) :: FfrootPOC1  ! I  fraction of biomass root to POC1                   (-)
        real(kind = real_wp) :: FfrootPOC2  ! I  fraction of biomass root to POC2                   (-)
        real(kind = real_wp) :: DELT        ! I  timestep for processes                             (d)
        real(kind = real_wp) :: Depth       ! I  depth of computational cell                        (m)

        real(kind = real_wp) :: SwWV        ! I  use wetland vegetation model (0=no,1=yes)          (-)
        real(kind = real_wp) :: Rc0MSWV     ! I  senescence mortality rate for VB01 at 20 oC        (1/d)
        real(kind = real_wp) :: TcMSWV      ! I  temperature coefficient of WV mort. for VB01       (-)
        real(kind = real_wp) :: RcMGRWV     ! I  grazing mortality pressure for VB01                (g/m2/d)
        real(kind = real_wp) :: AcMWV       ! I  acceleration factor for senescence mort VB0#       (-)
        real(kind = real_wp) :: MinRWV      ! I  maximum biomass ratio for VB0#                     (-)
        real(kind = real_wp) :: MaxRWV      ! I  maximum biomass ratio for VB0#                     (-)
        real(kind = real_wp) :: TBmWV       ! I  target total biomass for VB01                      (tC/ha)
        real(kind = real_wp) :: TempAir     ! I  Air temperature                                    (oC)
        real(kind = real_wp) :: minVB       ! I  minimum biomass for all vegetation                 (gC/m2)

        real(kind = real_wp) :: fMrtVB      ! O Total mortality flux for VB0                        (gC/m2/d)

        real(kind = real_wp) :: fMC2VB01P1  ! O  mortality foliage VB01 to POC1                     (gC/m2/d)
        real(kind = real_wp) :: fMC2VB01P2  ! O  mortality foliage VB01 to POC2                     (gC/m2/d)
        real(kind = real_wp) :: fMC2VB01P3  ! O  mortality foliage VB01 to POC3                     (gC/m2/d)
        real(kind = real_wp) :: fMN2VB01P1  ! O  mortality foliage VB01 to PON1                     (gN/m2/d)
        real(kind = real_wp) :: fMN2VB01P2  ! O  mortality foliage VB01 to PON2                     (gN/m2/d)
        real(kind = real_wp) :: fMN2VB01P3  ! O  mortality foliage VB01 to PON3                     (gN/m2/d)
        real(kind = real_wp) :: fMP2VB01P1  ! O  mortality foliage VB01 to POP1                     (gP/m2/d)
        real(kind = real_wp) :: fMP2VB01P2  ! O  mortality foliage VB01 to POP2                     (gP/m2/d)
        real(kind = real_wp) :: fMP2VB01P3  ! O  mortality foliage VB01 to POP3                     (gP/m2/d)
        real(kind = real_wp) :: fMS2VB01P1  ! O  mortality foliage VB01 to POS1                     (gS/m2/d)
        real(kind = real_wp) :: fMS2VB01P2  ! O  mortality foliage VB01 to POS2                     (gS/m2/d)
        real(kind = real_wp) :: fMS2VB01P3  ! O  mortality foliage VB01 to POS3                     (gS/m2/d)
        real(kind = real_wp) :: fMC5VB01P1  ! O  mortality fineroot VB01 to POC1                    (gC/m2/d)
        real(kind = real_wp) :: fMC5VB01P2  ! O  mortality fineroot VB01 to POC2                    (gC/m2/d)
        real(kind = real_wp) :: fMC5VB01P3  ! O  mortality fineroot VB01 to POC3                    (gC/m2/d)
        real(kind = real_wp) :: fMN5VB01P1  ! O  mortality fineroot VB01 to PON1                    (gN/m2/d)
        real(kind = real_wp) :: fMN5VB01P2  ! O  mortality fineroot VB01 to PON2                    (gN/m2/d)
        real(kind = real_wp) :: fMN5VB01P3  ! O  mortality fineroot VB01 to PON3                    (gN/m2/d)
        real(kind = real_wp) :: fMP5VB01P1  ! O  mortality fineroot VB01 to POP1                    (gP/m2/d)
        real(kind = real_wp) :: fMP5VB01P2  ! O  mortality fineroot VB01 to POP2                    (gP/m2/d)
        real(kind = real_wp) :: fMP5VB01P3  ! O  mortality fineroot VB01 to POP3                    (gP/m2/d)
        real(kind = real_wp) :: fMS5VB01P1  ! O  mortality fineroot VB01 to POS1                    (gS/m2/d)
        real(kind = real_wp) :: fMS5VB01P2  ! O  mortality fineroot VB01 to POS2                    (gS/m2/d)
        real(kind = real_wp) :: fMS5VB01P3  ! O  mortality fineroot VB01 to POS3                    (gS/m2/d)
        real(kind = real_wp) :: dMrtC1VB01  ! F  mortality stem VB01                                (gC/m3/d)
        real(kind = real_wp) :: dMrtC3VB01  ! F  mortality branch VB01                              (gC/m3/d)
        real(kind = real_wp) :: dMrtC4VB01  ! F  mortality root VB01                                (gC/m3/d)

        real(kind = real_wp) :: dMrtC2VB01  ! F  mortality foliage VB01                             (gC/m3/d)
        real(kind = real_wp) :: dMrtC5VB01  ! F  mortality fineroot VB01                            (gC/m3/d)

        real(kind = real_wp) :: dMrtN1VB01  ! F  mortality stem VB01                                (gN/m3/d)
        real(kind = real_wp) :: dMrtN3VB01  ! F  mortality branch VB01                              (gN/m3/d)
        real(kind = real_wp) :: dMrtN4VB01  ! F  mortality root VB01                                (gN/m3/d)
        real(kind = real_wp) :: dMrtP1VB01  ! F  mortality stem VB01                                (gP/m3/d)
        real(kind = real_wp) :: dMrtP3VB01  ! F  mortality branch VB01                              (gP/m3/d)
        real(kind = real_wp) :: dMrtP4VB01  ! F  mortality root VB01                                (gP/m3/d)
        real(kind = real_wp) :: dMrtS1VB01  ! F  mortality stem VB01                                (gS/m3/d)
        real(kind = real_wp) :: dMrtS3VB01  ! F  mortality branch VB01                              (gS/m3/d)
        real(kind = real_wp) :: dMrtS4VB01  ! F  mortality root VB01                                (gS/m3/d)

        real(kind = real_wp) :: fMrtC1VB01  ! O  mortality stem VB01                                (gC/m3/d)
        real(kind = real_wp) :: fMrtC3VB01  ! O  mortality branch VB01                              (gC/m3/d)
        real(kind = real_wp) :: fMrtC4VB01  ! O  mortality root VB01                                (gC/m3/d)
        real(kind = real_wp) :: fMrtN1VB01  ! O  mortality stem VB01                                (gN/m3/d)
        real(kind = real_wp) :: fMrtN3VB01  ! O  mortality branch VB01                              (gN/m3/d)
        real(kind = real_wp) :: fMrtN4VB01  ! O  mortality root VB01                                (gN/m3/d)
        real(kind = real_wp) :: fMrtP1VB01  ! O  mortality stem VB01                                (gP/m3/d)
        real(kind = real_wp) :: fMrtP3VB01  ! O  mortality branch VB01                              (gP/m3/d)
        real(kind = real_wp) :: fMrtP4VB01  ! O  mortality root VB01                                (gP/m3/d)
        real(kind = real_wp) :: fMrtS1VB01  ! O  mortality stem VB01                                (gS/m3/d)
        real(kind = real_wp) :: fMrtS3VB01  ! O  mortality branch VB01                              (gS/m3/d)
        real(kind = real_wp) :: fMrtS4VB01  ! O  mortality root VB01                                (gS/m3/d)

        real(kind = real_wp) :: rcdec       ! I  decay rate for vegetation mortality

        real(kind = real_wp) :: rcdecact    ! O  actual decay rate for vegetation mortality
        real(kind = real_wp) :: fMrt        ! O  total decay for vegetation mortality

        integer(kind = int_wp) :: IdMrtC1VB01 !    Pointer to the mortality stem VB01
        integer(kind = int_wp) :: IdMrtC3VB01 !    Pointer to the mortality branch VB01
        integer(kind = int_wp) :: IdMrtC4VB01 !    Pointer to the mortality root VB01

        integer(kind = int_wp) :: IdMrtC2VB01 !    Pointer to the mortality foliage VB01
        integer(kind = int_wp) :: IdMrtC5VB01 !    Pointer to the mortality fineroots VB01

        integer(kind = int_wp) :: IdMrtN1VB01 !    Pointer to the mortality stem VB01
        integer(kind = int_wp) :: IdMrtN3VB01 !    Pointer to the mortality branch VB01
        integer(kind = int_wp) :: IdMrtN4VB01 !    Pointer to the mortality root VB01
        integer(kind = int_wp) :: IdMrtP1VB01 !    Pointer to the mortality stem VB01
        integer(kind = int_wp) :: IdMrtP3VB01 !    Pointer to the mortality branch VB01
        integer(kind = int_wp) :: IdMrtP4VB01 !    Pointer to the mortality root VB01
        integer(kind = int_wp) :: IdMrtS1VB01 !    Pointer to the mortality stem VB01
        integer(kind = int_wp) :: IdMrtS3VB01 !    Pointer to the mortality branch VB01
        integer(kind = int_wp) :: IdMrtS4VB01 !    Pointer to the mortality root VB01
        integer(kind = int_wp) :: ikmrk1         ! first feature
        integer(kind = int_wp) :: ikmrk2         ! second feature

        ! Local variables
        integer(kind = int_wp) :: nrofinputs  !    Number of inputs
        real(kind = real_wp) :: Temp20      !    Air temperature minus 20                              (oC)
        real(kind = real_wp) :: TempCof     !    Temperature coefficient
        real(kind = real_wp) :: rVB1        !    Ratio between current biomass and target biomass       (-)
        real(kind = real_wp) :: maxfMrtVB   !    Maximum biomass flux                              (g/m2/d)
        !
        !*******************************************************************************
        !
        ipnt = ipoint
        IdMrtC1VB01 = 1
        IdMrtC3VB01 = 2
        IdMrtC4VB01 = 3

        IdMrtC2VB01 = 4
        IdMrtC5VB01 = 5

        IdMrtN1VB01 = 6
        IdMrtN3VB01 = 7
        IdMrtN4VB01 = 8
        IdMrtP1VB01 = 9
        IdMrtP3VB01 = 10
        IdMrtP4VB01 = 11
        IdMrtS1VB01 = 12
        IdMrtS3VB01 = 13
        IdMrtS4VB01 = 14
        !
        do  iseg = 1, num_cells

            !        lowest water and 2d segments only
            call extract_waq_attribute(1, iknmrk(iseg), ikmrk1)
            call extract_waq_attribute(2, iknmrk(iseg), ikmrk2)
            if (ikmrk1<3 .and. (ikmrk2==0).or.(ikmrk2==3)) then
                !
                VB1 = process_space_real(ipnt(1))
                F1VB01 = process_space_real(ipnt(2))
                F2VB01 = process_space_real(ipnt(3))
                F3VB01 = process_space_real(ipnt(4))
                F4VB01 = process_space_real(ipnt(5))
                F5VB01 = process_space_real(ipnt(6))
                SwVB01Mrt = process_space_real(ipnt(7))
                ageVB1 = process_space_real(ipnt(8))
                CNf1VB01 = process_space_real(ipnt(9))
                CNf2VB01 = process_space_real(ipnt(10))
                CNf3VB01 = process_space_real(ipnt(11))
                CNf4VB01 = process_space_real(ipnt(12))
                CNf5VB01 = process_space_real(ipnt(13))
                CPf1VB01 = process_space_real(ipnt(14))
                CPf2VB01 = process_space_real(ipnt(15))
                CPf3VB01 = process_space_real(ipnt(16))
                CPf4VB01 = process_space_real(ipnt(17))
                CPf5VB01 = process_space_real(ipnt(18))
                CSf1VB01 = process_space_real(ipnt(19))
                CSf2VB01 = process_space_real(ipnt(20))
                CSf3VB01 = process_space_real(ipnt(21))
                CSf4VB01 = process_space_real(ipnt(22))
                CSf5VB01 = process_space_real(ipnt(23))
                FfolPOC1 = process_space_real(ipnt(24))
                FfolPOC2 = process_space_real(ipnt(25))
                FfrootPOC1 = process_space_real(ipnt(26))
                FfrootPOC2 = process_space_real(ipnt(27))
                DELT = process_space_real(ipnt(28))
                Depth = process_space_real(ipnt(29))
                rcdec = process_space_real(ipnt(30))
                SWDying = process_space_real(ipnt(31))
                SwWV = process_space_real(ipnt(32))
                Rc0MSWV = process_space_real(ipnt(33))
                TcMSWV = process_space_real(ipnt(34))
                RcMGRWV = process_space_real(ipnt(35))
                AcMWV = process_space_real(ipnt(36))
                MinRWV = process_space_real(ipnt(37))
                MaxRWV = process_space_real(ipnt(38))
                TBmWV = process_space_real(ipnt(39))
                TempAir = process_space_real(ipnt(40))
                MinVB = process_space_real(ipnt(41))
                nrofinputs = 41
                !
                !
                !   *****     Insert your code here  *****
                !
                if ((NINT (SwVB01Mrt) == 1) .or. (NINT (SwDying) == 1)) then
                    !           inundation mortality
                    rcdecact = rcdec
                else
                    !           no inundation mortality
                    rcdecact = 0.0
                endif

                if (NINT(SwWV) == 1) then
                    !           if wetland vegetation model, also turnover due to senescence mortality and grazing
                    !           Ratio of current biomass to attainable biomass
                    rVB1 = VB1 / ((TBmWV + tiny(TBmWV)) * 100.0)
                    TEMP20 = TempAir - 20.0
                    TempCof = TcMSWV ** TEMP20

                    If (rVB1 > MaxRWV) then
                        !              Add accelarated senescence mortality with biomass above target biomass
                        rcdecact = rcdecact + AcMWV * Rc0MSWV * TempCof
                    else
                        !              Add normal senescence mortality
                        rcdecact = rcdecact + Rc0MSWV * TempCof
                    end if
                    maxfMrtVB = MAX (0.0, (VB1 - MinVB) / DELT)
                    fMrtVB = MIN(rcdecact * VB1 + RcMGRWV, maxfMrtVB)
                else
                    fMrtVB = rcdecact * VB1
                end if



                !        check if vegetation cohort is dead or still dying off or we use the Wetland Vegetation option
                if (((NINT (SwVB01Mrt) == 1) .or. (NINT (SwDying) == 1) .or. (NINT(SwWV) == 1)) .and. (fMrtVB > 0.0)) then

                    !           calculate 2D fluxes of vegetation compartments
                    !           seems redundant to split C-flux by veg. compartment
                    !           when different lag times per compartment are introduced this is needed
                    !           moreover useful for balance output

                    !           Fluxex for state var VB01

                    !           C-flux for stem, branch and root

                    dMrtC1VB01 = fMrtVB * F1VB01 / DEPTH
                    dMrtC3VB01 = fMrtVB * F3VB01 / DEPTH
                    dMrtC4VB01 = fMrtVB * F4VB01 / DEPTH
                    !           C-flux foliage and fine roots
                    dMrtC2VB01 = fMrtVB * F2VB01 / DEPTH
                    dMrtC5VB01 = fMrtVB * F5VB01 / DEPTH

                    !           C-outputs for stem, branch and root

                    fMrtC1VB01 = fMrtVB * F1VB01
                    fMrtC3VB01 = fMrtVB * F3VB01
                    fMrtC4VB01 = fMrtVB * F4VB01

                    !           C-Outputs for prod of POC1-3 fractions from foliage/fineroots 3D

                    fMC2VB01P1 = fMrtVB * F2VB01 * FfolPOC1
                    fMC2VB01P2 = fMrtVB * F2VB01 * FfolPOC2
                    fMC2VB01P3 = fMrtVB * F2VB01 * (1 - FfolPOC1 - FfolPOC2)
                    !           output-flux for fineroots ->POC1-3
                    fMC5VB01P1 = fMrtVB * F5VB01 * FfrootPOC1
                    fMC5VB01P2 = fMrtVB * F5VB01 * FfrootPOC2
                    fMC5VB01P3 = fMrtVB * F5VB01 * (1 - FfrootPOC1 - FfrootPOC2)

                    !           NPS-Fluxes for nutrients of stem, branch and roots: 2D -> POX5
                    fMrtN1VB01 = fMrtC1VB01 / CNf1VB01
                    fMrtN3VB01 = fMrtC3VB01 / CNf3VB01
                    fMrtN4VB01 = fMrtC4VB01 / CNf4VB01
                    fMrtP1VB01 = fMrtC1VB01 / CPf1VB01
                    fMrtP3VB01 = fMrtC3VB01 / CPf3VB01
                    fMrtP4VB01 = fMrtC4VB01 / CPf4VB01
                    fMrtS1VB01 = fMrtC1VB01 / CSf1VB01
                    fMrtS3VB01 = fMrtC3VB01 / CSf3VB01
                    fMrtS4VB01 = fMrtC4VB01 / CSf4VB01

                    !           calculate output for nutrient fluxes on POC1-3 through 3d rootzone distribution

                    !           NPS-output foliage comp=2
                    fMN2VB01P1 = fMC2VB01P1 / CNf2VB01
                    fMN2VB01P2 = fMC2VB01P2 / CNf2VB01
                    fMN2VB01P3 = fMC2VB01P3 / CNf2VB01
                    fMP2VB01P1 = fMC2VB01P1 / CPf2VB01
                    fMP2VB01P2 = fMC2VB01P2 / CPf2VB01
                    fMP2VB01P3 = fMC2VB01P3 / CPf2VB01
                    fMS2VB01P1 = fMC2VB01P1 / CSf2VB01
                    fMS2VB01P2 = fMC2VB01P2 / CSf2VB01
                    fMS2VB01P3 = fMC2VB01P3 / CSf2VB01

                    !           NPS-output fine roots comp=5
                    fMN5VB01P1 = fMC5VB01P1 / CNf5VB01
                    fMN5VB01P2 = fMC5VB01P2 / CNf5VB01
                    fMN5VB01P3 = fMC5VB01P3 / CNf5VB01
                    fMP5VB01P1 = fMC5VB01P1 / CPf5VB01
                    fMP5VB01P2 = fMC5VB01P2 / CPf5VB01
                    fMP5VB01P3 = fMC5VB01P3 / CPf5VB01
                    fMS5VB01P1 = fMC5VB01P1 / CSf5VB01
                    fMS5VB01P2 = fMC5VB01P2 / CSf5VB01
                    fMS5VB01P3 = fMC5VB01P3 / CSf5VB01
                    !
                    !        chort not dead or decaying
                else
                    dMrtC1VB01 = 0.0
                    dMrtC3VB01 = 0.0
                    dMrtC4VB01 = 0.0
                    dMrtC2VB01 = 0.0
                    dMrtC5VB01 = 0.0
                    !           dMrtN1VB01    = 0.0
                    !           dMrtN3VB01    = 0.0
                    !           dMrtN4VB01    = 0.0
                    !           dMrtP1VB01    = 0.0
                    !           dMrtP3VB01    = 0.0
                    !           dMrtP4VB01    = 0.0
                    !           dMrtS1VB01    = 0.0
                    !           dMrtS3VB01    = 0.0
                    !           dMrtS4VB01    = 0.0
                    fMC2VB01P1 = 0.0
                    fMC2VB01P2 = 0.0
                    fMC2VB01P3 = 0.0
                    fMN2VB01P1 = 0.0
                    fMN2VB01P2 = 0.0
                    fMN2VB01P3 = 0.0
                    fMP2VB01P1 = 0.0
                    fMP2VB01P2 = 0.0
                    fMP2VB01P3 = 0.0
                    fMS2VB01P1 = 0.0
                    fMS2VB01P2 = 0.0
                    fMS2VB01P3 = 0.0
                    fMC5VB01P1 = 0.0
                    fMC5VB01P2 = 0.0
                    fMC5VB01P3 = 0.0
                    fMN5VB01P1 = 0.0
                    fMN5VB01P2 = 0.0
                    fMN5VB01P3 = 0.0
                    fMP5VB01P1 = 0.0
                    fMP5VB01P2 = 0.0
                    fMP5VB01P3 = 0.0
                    fMS5VB01P1 = 0.0
                    fMS5VB01P2 = 0.0
                    fMS5VB01P3 = 0.0
                    fMrtC1VB01 = 0.0
                    fMrtC3VB01 = 0.0
                    fMrtC4VB01 = 0.0
                    fMrtN1VB01 = 0.0
                    fMrtN3VB01 = 0.0
                    fMrtN4VB01 = 0.0
                    fMrtP1VB01 = 0.0
                    fMrtP3VB01 = 0.0
                    fMrtP4VB01 = 0.0
                    fMrtS1VB01 = 0.0
                    fMrtS3VB01 = 0.0
                    fMrtS4VB01 = 0.0
                endif

                !   *****     End of your code       *****
                !

                fl  (IdMrtC1VB01) = dMrtC1VB01
                fl  (IdMrtC3VB01) = dMrtC3VB01
                fl  (IdMrtC4VB01) = dMrtC4VB01

                fl  (IdMrtC2VB01) = dMrtC2VB01
                fl  (IdMrtC5VB01) = dMrtC5VB01

                !        fl  ( IdMrtN1VB01 ) = dMrtN1VB01
                !        fl  ( IdMrtN3VB01 ) = dMrtN3VB01
                !        fl  ( IdMrtN4VB01 ) = dMrtN4VB01
                !        fl  ( IdMrtP1VB01 ) = dMrtP1VB01
                !        fl  ( IdMrtP3VB01 ) = dMrtP3VB01
                !        fl  ( IdMrtP4VB01 ) = dMrtP4VB01
                !        fl  ( IdMrtS1VB01 ) = dMrtS1VB01
                !        fl  ( IdMrtS3VB01 ) = dMrtS3VB01
                !        fl  ( IdMrtS4VB01 ) = dMrtS4VB01
                process_space_real(ipnt(nrofinputs + 1)) = rcdecact
                process_space_real(ipnt(nrofinputs + 2)) = fMrtVB
                process_space_real(ipnt(nrofinputs + 3)) = fMC2VB01P1
                process_space_real(ipnt(nrofinputs + 4)) = fMC2VB01P2
                process_space_real(ipnt(nrofinputs + 5)) = fMC2VB01P3
                process_space_real(ipnt(nrofinputs + 6)) = fMN2VB01P1
                process_space_real(ipnt(nrofinputs + 7)) = fMN2VB01P2
                process_space_real(ipnt(nrofinputs + 8)) = fMN2VB01P3
                process_space_real(ipnt(nrofinputs + 9)) = fMP2VB01P1
                process_space_real(ipnt(nrofinputs + 10)) = fMP2VB01P2
                process_space_real(ipnt(nrofinputs + 11)) = fMP2VB01P3
                process_space_real(ipnt(nrofinputs + 12)) = fMS2VB01P1
                process_space_real(ipnt(nrofinputs + 13)) = fMS2VB01P2
                process_space_real(ipnt(nrofinputs + 14)) = fMS2VB01P3
                process_space_real(ipnt(nrofinputs + 15)) = fMC5VB01P1
                process_space_real(ipnt(nrofinputs + 16)) = fMC5VB01P2
                process_space_real(ipnt(nrofinputs + 17)) = fMC5VB01P3
                process_space_real(ipnt(nrofinputs + 18)) = fMN5VB01P1
                process_space_real(ipnt(nrofinputs + 19)) = fMN5VB01P2
                process_space_real(ipnt(nrofinputs + 20)) = fMN5VB01P3
                process_space_real(ipnt(nrofinputs + 21)) = fMP5VB01P1
                process_space_real(ipnt(nrofinputs + 22)) = fMP5VB01P2
                process_space_real(ipnt(nrofinputs + 23)) = fMP5VB01P3
                process_space_real(ipnt(nrofinputs + 24)) = fMS5VB01P1
                process_space_real(ipnt(nrofinputs + 25)) = fMS5VB01P2
                process_space_real(ipnt(nrofinputs + 26)) = fMS5VB01P3
                process_space_real(ipnt(nrofinputs + 27)) = fMrtC1VB01
                process_space_real(ipnt(nrofinputs + 28)) = fMrtC3VB01
                process_space_real(ipnt(nrofinputs + 29)) = fMrtC4VB01
                process_space_real(ipnt(nrofinputs + 30)) = fMrtN1VB01
                process_space_real(ipnt(nrofinputs + 31)) = fMrtN3VB01
                process_space_real(ipnt(nrofinputs + 32)) = fMrtN4VB01
                process_space_real(ipnt(nrofinputs + 33)) = fMrtP1VB01
                process_space_real(ipnt(nrofinputs + 34)) = fMrtP3VB01
                process_space_real(ipnt(nrofinputs + 35)) = fMrtP4VB01
                process_space_real(ipnt(nrofinputs + 36)) = fMrtS1VB01
                process_space_real(ipnt(nrofinputs + 37)) = fMrtS3VB01
                process_space_real(ipnt(nrofinputs + 38)) = fMrtS4VB01

                !

                !        bottom and 2d segments only
            endif
            !
            IdMrtC1VB01 = IdMrtC1VB01 + noflux
            IdMrtC3VB01 = IdMrtC3VB01 + noflux
            IdMrtC4VB01 = IdMrtC4VB01 + noflux

            IdMrtC2VB01 = IdMrtC2VB01 + noflux
            IdMrtC5VB01 = IdMrtC5VB01 + noflux

            !        IdMrtN1VB01 = IdMrtN1VB01 + noflux
            !        IdMrtN3VB01 = IdMrtN3VB01 + noflux
            !        IdMrtN4VB01 = IdMrtN4VB01 + noflux
            !        IdMrtP1VB01 = IdMrtP1VB01 + noflux
            !        IdMrtP3VB01 = IdMrtP3VB01 + noflux
            !        IdMrtP4VB01 = IdMrtP4VB01 + noflux
            !        IdMrtS1VB01 = IdMrtS1VB01 + noflux
            !        IdMrtS3VB01 = IdMrtS3VB01 + noflux
            !        IdMrtS4VB01 = IdMrtS4VB01 + noflux
            ipnt = ipnt + increm


            !
            !     segment loop
        enddo
        !
        return
    end

end module m_vbmrt
