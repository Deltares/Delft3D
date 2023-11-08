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
module m_vbmrt

implicit none

contains


      subroutine VBMRT      ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
      use m_evaluate_waq_attribute

!
!*******************************************************************************
!
      IMPLICIT NONE
!
!     Type    Name         I/O Description
!
      real(4) pmsa(*)     !I/O Process Manager System Array, window of routine to process library
      real(4) fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
      integer ipoint( 79) ! I  Array of pointers in pmsa to get and store the data
      integer increm( 79) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! I  Number of computational elements in the whole model schematisation
      integer noflux      ! I  Number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
      integer ipnt( 79)   !    Local work array for the pointering
      integer iseg        !    Local loop counter for computational element loop
!
!*******************************************************************************
!
!     Type    Name         I/O Description                                        Unit
!
      real(4) VB1         ! I  vegetation biomass cohort 1                        (gC/m2)
      real(4) F1VB01      ! I  allocation factor comp. 1 (stem) VB 01             (-)
      real(4) F2VB01      ! I  allocation factor comp. 2 (foliage) VB 01          (-)
      real(4) F3VB01      ! I  allocation factor comp. 3 (branch) VB 01           (-)
      real(4) F4VB01      ! I  allocation factor comp. 4 (root) VB 01             (-)
      real(4) F5VB01      ! I  allocation factor comp. 5 (fineroot) VB 01         (-)
      real(4) SwVB01Mrt   ! I  vegetation biomass dead (0=no,1=yes)               (-)
      real(4) SwDying     ! I  vegetation biomass dying (0=no,1=yes)              (-)
      real(4) ageVB1      ! I  age of vegation cohort 1                           (d)
      real(4) CNf1VB01    ! I  carbon-nitrogen ratio in stem VB01                 (gC/gN)
      real(4) CNf2VB01    ! I  carbon-nitrogen ratio in foliage VB01              (gC/gN)
      real(4) CNf3VB01    ! I  carbon-nitrogen ratio in branch VB01               (gC/gN)
      real(4) CNf4VB01    ! I  carbon-nitrogen ratio in root VB01                 (gC/gN)
      real(4) CNf5VB01    ! I  carbon-nitrogen ratio in fineroot VB01             (gC/gN)
      real(4) CPf1VB01    ! I  carbon-phosporus ratio in stem VB01                (gC/gP)
      real(4) CPf2VB01    ! I  carbon-phosporus ratio in foliage VB01             (gC/gP)
      real(4) CPf3VB01    ! I  carbon-phosporus ratio in branch VB01              (gC/gP)
      real(4) CPf4VB01    ! I  carbon-phosporus ratio in root VB01                (gC/gP)
      real(4) CPf5VB01    ! I  carbon-phosporus ratio in fineroot VB01            (gC/gP)
      real(4) CSf1VB01    ! I  carbon-sulphur ratio in stem VB01                  (gC/gS)
      real(4) CSf2VB01    ! I  carbon-sulphur ratio in foliage VB01               (gC/gS)
      real(4) CSf3VB01    ! I  carbon-sulphur ratio in branch VB01                (gC/gS)
      real(4) CSf4VB01    ! I  carbon-sulphur ratio in root VB01                  (gC/gS)
      real(4) CSf5VB01    ! I  carbon-sulphur ratio in fineroot                   (gC/gS)
      real(4) FfolPOC1    ! I  fraction of biomass foliage to POC1                (-)
      real(4) FfolPOC2    ! I  fraction of biomass foliage to POC2                (-)
      real(4) FfrootPOC1  ! I  fraction of biomass root to POC1                   (-)
      real(4) FfrootPOC2  ! I  fraction of biomass root to POC2                   (-)
      real(4) DELT        ! I  timestep for processes                             (d)
      real(4) Depth       ! I  depth of computational cell                        (m)

      real(4) SwWV        ! I  use wetland vegetation model (0=no,1=yes)          (-)
      real(4) Rc0MSWV     ! I  senescence mortality rate for VB01 at 20 oC        (1/d)
      real(4) TcMSWV      ! I  temperature coefficient of WV mort. for VB01       (-)
      real(4) RcMGRWV     ! I  grazing mortality pressure for VB01                (g/m2/d)
      real(4) AcMWV       ! I  acceleration factor for senescence mort VB0#       (-)
      real(4) MinRWV      ! I  maximum biomass ratio for VB0#                     (-)
      real(4) MaxRWV      ! I  maximum biomass ratio for VB0#                     (-)
      real(4) TBmWV       ! I  target total biomass for VB01                      (tC/ha)
      real(4) TempAir     ! I  Air temperature                                    (oC)
      real(4) minVB       ! I  minimum biomass for all vegetation                 (gC/m2)

      real(4) fMrtVB      ! O Total mortality flux for VB0                        (gC/m2/d)

      real(4) fMC2VB01P1  ! O  mortality foliage VB01 to POC1                     (gC/m2/d)
      real(4) fMC2VB01P2  ! O  mortality foliage VB01 to POC2                     (gC/m2/d)
      real(4) fMC2VB01P3  ! O  mortality foliage VB01 to POC3                     (gC/m2/d)
      real(4) fMN2VB01P1  ! O  mortality foliage VB01 to PON1                     (gN/m2/d)
      real(4) fMN2VB01P2  ! O  mortality foliage VB01 to PON2                     (gN/m2/d)
      real(4) fMN2VB01P3  ! O  mortality foliage VB01 to PON3                     (gN/m2/d)
      real(4) fMP2VB01P1  ! O  mortality foliage VB01 to POP1                     (gP/m2/d)
      real(4) fMP2VB01P2  ! O  mortality foliage VB01 to POP2                     (gP/m2/d)
      real(4) fMP2VB01P3  ! O  mortality foliage VB01 to POP3                     (gP/m2/d)
      real(4) fMS2VB01P1  ! O  mortality foliage VB01 to POS1                     (gS/m2/d)
      real(4) fMS2VB01P2  ! O  mortality foliage VB01 to POS2                     (gS/m2/d)
      real(4) fMS2VB01P3  ! O  mortality foliage VB01 to POS3                     (gS/m2/d)
      real(4) fMC5VB01P1  ! O  mortality fineroot VB01 to POC1                    (gC/m2/d)
      real(4) fMC5VB01P2  ! O  mortality fineroot VB01 to POC2                    (gC/m2/d)
      real(4) fMC5VB01P3  ! O  mortality fineroot VB01 to POC3                    (gC/m2/d)
      real(4) fMN5VB01P1  ! O  mortality fineroot VB01 to PON1                    (gN/m2/d)
      real(4) fMN5VB01P2  ! O  mortality fineroot VB01 to PON2                    (gN/m2/d)
      real(4) fMN5VB01P3  ! O  mortality fineroot VB01 to PON3                    (gN/m2/d)
      real(4) fMP5VB01P1  ! O  mortality fineroot VB01 to POP1                    (gP/m2/d)
      real(4) fMP5VB01P2  ! O  mortality fineroot VB01 to POP2                    (gP/m2/d)
      real(4) fMP5VB01P3  ! O  mortality fineroot VB01 to POP3                    (gP/m2/d)
      real(4) fMS5VB01P1  ! O  mortality fineroot VB01 to POS1                    (gS/m2/d)
      real(4) fMS5VB01P2  ! O  mortality fineroot VB01 to POS2                    (gS/m2/d)
      real(4) fMS5VB01P3  ! O  mortality fineroot VB01 to POS3                    (gS/m2/d)
      real(4) dMrtC1VB01  ! F  mortality stem VB01                                (gC/m3/d)
      real(4) dMrtC3VB01  ! F  mortality branch VB01                              (gC/m3/d)
      real(4) dMrtC4VB01  ! F  mortality root VB01                                (gC/m3/d)

      real(4) dMrtC2VB01  ! F  mortality foliage VB01                             (gC/m3/d)
      real(4) dMrtC5VB01  ! F  mortality fineroot VB01                            (gC/m3/d)


      real(4) dMrtN1VB01  ! F  mortality stem VB01                                (gN/m3/d)
      real(4) dMrtN3VB01  ! F  mortality branch VB01                              (gN/m3/d)
      real(4) dMrtN4VB01  ! F  mortality root VB01                                (gN/m3/d)
      real(4) dMrtP1VB01  ! F  mortality stem VB01                                (gP/m3/d)
      real(4) dMrtP3VB01  ! F  mortality branch VB01                              (gP/m3/d)
      real(4) dMrtP4VB01  ! F  mortality root VB01                                (gP/m3/d)
      real(4) dMrtS1VB01  ! F  mortality stem VB01                                (gS/m3/d)
      real(4) dMrtS3VB01  ! F  mortality branch VB01                              (gS/m3/d)
      real(4) dMrtS4VB01  ! F  mortality root VB01                                (gS/m3/d)

      real(4) fMrtC1VB01  ! O  mortality stem VB01                                (gC/m3/d)
      real(4) fMrtC3VB01  ! O  mortality branch VB01                              (gC/m3/d)
      real(4) fMrtC4VB01  ! O  mortality root VB01                                (gC/m3/d)
      real(4) fMrtN1VB01  ! O  mortality stem VB01                                (gN/m3/d)
      real(4) fMrtN3VB01  ! O  mortality branch VB01                              (gN/m3/d)
      real(4) fMrtN4VB01  ! O  mortality root VB01                                (gN/m3/d)
      real(4) fMrtP1VB01  ! O  mortality stem VB01                                (gP/m3/d)
      real(4) fMrtP3VB01  ! O  mortality branch VB01                              (gP/m3/d)
      real(4) fMrtP4VB01  ! O  mortality root VB01                                (gP/m3/d)
      real(4) fMrtS1VB01  ! O  mortality stem VB01                                (gS/m3/d)
      real(4) fMrtS3VB01  ! O  mortality branch VB01                              (gS/m3/d)
      real(4) fMrtS4VB01  ! O  mortality root VB01                                (gS/m3/d)

      real(4) rcdec       ! I  decay rate for vegetation mortality

      real(4) rcdecact    ! O  actual decay rate for vegetation mortality
      real(4) fMrt        ! O  total decay for vegetation mortality

      integer IdMrtC1VB01 !    Pointer to the mortality stem VB01
      integer IdMrtC3VB01 !    Pointer to the mortality branch VB01
      integer IdMrtC4VB01 !    Pointer to the mortality root VB01

      integer IdMrtC2VB01 !    Pointer to the mortality foliage VB01
      integer IdMrtC5VB01 !    Pointer to the mortality fineroots VB01

      integer IdMrtN1VB01 !    Pointer to the mortality stem VB01
      integer IdMrtN3VB01 !    Pointer to the mortality branch VB01
      integer IdMrtN4VB01 !    Pointer to the mortality root VB01
      integer IdMrtP1VB01 !    Pointer to the mortality stem VB01
      integer IdMrtP3VB01 !    Pointer to the mortality branch VB01
      integer IdMrtP4VB01 !    Pointer to the mortality root VB01
      integer IdMrtS1VB01 !    Pointer to the mortality stem VB01
      integer IdMrtS3VB01 !    Pointer to the mortality branch VB01
      integer IdMrtS4VB01 !    Pointer to the mortality root VB01
      integer             :: ikmrk1         ! first feature
      integer             :: ikmrk2         ! second feature

! Local variables
      integer nrofinputs  !    Number of inputs
      real(4) Temp20      !    Air temperature minus 20                              (oC)
      real(4) TempCof     !    Temperature coefficient
      real(4) rVB1        !    Ratio between current biomass and target biomass       (-)
      real(4) maxfMrtVB   !    Maximum biomass flux                              (g/m2/d)
!
!*******************************************************************************
!
      ipnt        = ipoint
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
      do  iseg = 1 , noseg

!        lowest water and 2d segments only
         call evaluate_waq_attribute(1,iknmrk(iseg),ikmrk1)
         call evaluate_waq_attribute(2,iknmrk(iseg),ikmrk2)
         if (ikmrk1.lt.3 .and. (ikmrk2.eq.0).or.(ikmrk2.eq.3)) then
!
         VB1        = pmsa( ipnt(  1) )
         F1VB01     = pmsa( ipnt(  2) )
         F2VB01     = pmsa( ipnt(  3) )
         F3VB01     = pmsa( ipnt(  4) )
         F4VB01     = pmsa( ipnt(  5) )
         F5VB01     = pmsa( ipnt(  6) )
         SwVB01Mrt  = pmsa( ipnt(  7) )
         ageVB1     = pmsa( ipnt(  8) )
         CNf1VB01   = pmsa( ipnt(  9) )
         CNf2VB01   = pmsa( ipnt( 10) )
         CNf3VB01   = pmsa( ipnt( 11) )
         CNf4VB01   = pmsa( ipnt( 12) )
         CNf5VB01   = pmsa( ipnt( 13) )
         CPf1VB01   = pmsa( ipnt( 14) )
         CPf2VB01   = pmsa( ipnt( 15) )
         CPf3VB01   = pmsa( ipnt( 16) )
         CPf4VB01   = pmsa( ipnt( 17) )
         CPf5VB01   = pmsa( ipnt( 18) )
         CSf1VB01   = pmsa( ipnt( 19) )
         CSf2VB01   = pmsa( ipnt( 20) )
         CSf3VB01   = pmsa( ipnt( 21) )
         CSf4VB01   = pmsa( ipnt( 22) )
         CSf5VB01   = pmsa( ipnt( 23) )
         FfolPOC1   = pmsa( ipnt( 24) )
         FfolPOC2   = pmsa( ipnt( 25) )
         FfrootPOC1 = pmsa( ipnt( 26) )
         FfrootPOC2 = pmsa( ipnt( 27) )
         DELT       = pmsa( ipnt( 28) )
         Depth      = pmsa( ipnt( 29) )
         rcdec      = pmsa( ipnt( 30) )
         SWDying    = pmsa( ipnt( 31) )
         SwWV       = pmsa( ipnt( 32) )
         Rc0MSWV    = pmsa( ipnt( 33) )
         TcMSWV     = pmsa( ipnt( 34) )
         RcMGRWV    = pmsa( ipnt( 35) )
         AcMWV      = pmsa( ipnt( 36) )
         MinRWV     = pmsa( ipnt( 37) )
         MaxRWV     = pmsa( ipnt( 38) )
         TBmWV      = pmsa( ipnt( 39) )
         TempAir    = pmsa( ipnt( 40) )
         MinVB      = pmsa( ipnt( 41) )
         nrofinputs = 41
!
!
!   *****     Insert your code here  *****
!
         if ( ( NINT (SwVB01Mrt) .eq. 1) .or. ( NINT (SwDying) .eq. 1)) then
!           inundation mortality
            rcdecact = rcdec
         else
!           no inundation mortality
            rcdecact = 0.0
         endif

         if (NINT(SwWV) .eq. 1) then
!           if wetland vegetation model, also turnover due to senescence mortality and grazing
!           Ratio of current biomass to attainable biomass
            rVB1 = VB1 / ((TBmWV + tiny(TBmWV)) * 100.0)
            TEMP20 = TempAir - 20.0
            TempCof = TcMSWV ** TEMP20

            If (rVB1 .gt. MaxRWV) then
!              Add accelarated senescence mortality with biomass above target biomass
               rcdecact = rcdecact + AcMWV * Rc0MSWV * TempCof
            else
!              Add normal senescence mortality
               rcdecact = rcdecact + Rc0MSWV * TempCof
            end if
            maxfMrtVB = MAX (0.0, (VB1 - MinVB) / DELT )
            fMrtVB = MIN(rcdecact * VB1 + RcMGRWV, maxfMrtVB)
         else
            fMrtVB = rcdecact * VB1
         end if



!        check if vegetation cohort is dead or still dying off or we use the Wetland Vegetation option
         if ( ( ( NINT (SwVB01Mrt) .eq. 1) .or. ( NINT (SwDying) .eq. 1) .or. (NINT(SwWV) .eq. 1) ) .and. (fMrtVB .gt. 0.0) ) then

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
            fMC2VB01P3 = fMrtVB * F2VB01 * (1 - FfolPOC1 -FfolPOC2)
!           output-flux for fineroots ->POC1-3
            fMC5VB01P1 = fMrtVB * F5VB01 * FfrootPOC1
            fMC5VB01P2 = fMrtVB * F5VB01 * FfrootPOC2
            fMC5VB01P3 = fMrtVB * F5VB01 * (1 - FfrootPOC1 -FfrootPOC2)

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
            dMrtC1VB01    = 0.0
            dMrtC3VB01    = 0.0
            dMrtC4VB01    = 0.0
            dMrtC2VB01    = 0.0
            dMrtC5VB01    = 0.0
!           dMrtN1VB01    = 0.0
!           dMrtN3VB01    = 0.0
!           dMrtN4VB01    = 0.0
!           dMrtP1VB01    = 0.0
!           dMrtP3VB01    = 0.0
!           dMrtP4VB01    = 0.0
!           dMrtS1VB01    = 0.0
!           dMrtS3VB01    = 0.0
!           dMrtS4VB01    = 0.0
            fMC2VB01P1    = 0.0
            fMC2VB01P2    = 0.0
            fMC2VB01P3    = 0.0
            fMN2VB01P1    = 0.0
            fMN2VB01P2    = 0.0
            fMN2VB01P3    = 0.0
            fMP2VB01P1    = 0.0
            fMP2VB01P2    = 0.0
            fMP2VB01P3    = 0.0
            fMS2VB01P1    = 0.0
            fMS2VB01P2    = 0.0
            fMS2VB01P3    = 0.0
            fMC5VB01P1    = 0.0
            fMC5VB01P2    = 0.0
            fMC5VB01P3    = 0.0
            fMN5VB01P1    = 0.0
            fMN5VB01P2    = 0.0
            fMN5VB01P3    = 0.0
            fMP5VB01P1    = 0.0
            fMP5VB01P2    = 0.0
            fMP5VB01P3    = 0.0
            fMS5VB01P1    = 0.0
            fMS5VB01P2    = 0.0
            fMS5VB01P3    = 0.0
            fMrtC1VB01    = 0.0
            fMrtC3VB01    = 0.0
            fMrtC4VB01    = 0.0
            fMrtN1VB01    = 0.0
            fMrtN3VB01    = 0.0
            fMrtN4VB01    = 0.0
            fMrtP1VB01    = 0.0
            fMrtP3VB01    = 0.0
            fMrtP4VB01    = 0.0
            fMrtS1VB01    = 0.0
            fMrtS3VB01    = 0.0
            fMrtS4VB01    = 0.0
         endif

!   *****     End of your code       *****
!

         fl  ( IdMrtC1VB01 ) = dMrtC1VB01
         fl  ( IdMrtC3VB01 ) = dMrtC3VB01
         fl  ( IdMrtC4VB01 ) = dMrtC4VB01

         fl  ( IdMrtC2VB01 ) = dMrtC2VB01
         fl  ( IdMrtC5VB01 ) = dMrtC5VB01

!        fl  ( IdMrtN1VB01 ) = dMrtN1VB01
!        fl  ( IdMrtN3VB01 ) = dMrtN3VB01
!        fl  ( IdMrtN4VB01 ) = dMrtN4VB01
!        fl  ( IdMrtP1VB01 ) = dMrtP1VB01
!        fl  ( IdMrtP3VB01 ) = dMrtP3VB01
!        fl  ( IdMrtP4VB01 ) = dMrtP4VB01
!        fl  ( IdMrtS1VB01 ) = dMrtS1VB01
!        fl  ( IdMrtS3VB01 ) = dMrtS3VB01
!        fl  ( IdMrtS4VB01 ) = dMrtS4VB01
         pmsa( ipnt( nrofinputs + 1)   ) = rcdecact
         pmsa( ipnt( nrofinputs + 2)   ) = fMrtVB
         pmsa( ipnt( nrofinputs + 3)   ) = fMC2VB01P1
         pmsa( ipnt( nrofinputs + 4)   ) = fMC2VB01P2
         pmsa( ipnt( nrofinputs + 5)   ) = fMC2VB01P3
         pmsa( ipnt( nrofinputs + 6)   ) = fMN2VB01P1
         pmsa( ipnt( nrofinputs + 7)   ) = fMN2VB01P2
         pmsa( ipnt( nrofinputs + 8)   ) = fMN2VB01P3
         pmsa( ipnt( nrofinputs + 9)   ) = fMP2VB01P1
         pmsa( ipnt( nrofinputs + 10)  ) = fMP2VB01P2
         pmsa( ipnt( nrofinputs + 11)  ) = fMP2VB01P3
         pmsa( ipnt( nrofinputs + 12)   ) = fMS2VB01P1
         pmsa( ipnt( nrofinputs + 13)   ) = fMS2VB01P2
         pmsa( ipnt( nrofinputs + 14)   ) = fMS2VB01P3
         pmsa( ipnt( nrofinputs + 15)   ) = fMC5VB01P1
         pmsa( ipnt( nrofinputs + 16)   ) = fMC5VB01P2
         pmsa( ipnt( nrofinputs + 17)   ) = fMC5VB01P3
         pmsa( ipnt( nrofinputs + 18)   ) = fMN5VB01P1
         pmsa( ipnt( nrofinputs + 19)   ) = fMN5VB01P2
         pmsa( ipnt( nrofinputs + 20)   ) = fMN5VB01P3
         pmsa( ipnt( nrofinputs + 21)   ) = fMP5VB01P1
         pmsa( ipnt( nrofinputs + 22)   ) = fMP5VB01P2
         pmsa( ipnt( nrofinputs + 23)   ) = fMP5VB01P3
         pmsa( ipnt( nrofinputs + 24)   ) = fMS5VB01P1
         pmsa( ipnt( nrofinputs + 25)   ) = fMS5VB01P2
         pmsa( ipnt( nrofinputs + 26)   ) = fMS5VB01P3
         pmsa( ipnt( nrofinputs + 27)   ) = fMrtC1VB01
         pmsa( ipnt( nrofinputs + 28)   ) = fMrtC3VB01
         pmsa( ipnt( nrofinputs + 29)   ) = fMrtC4VB01
         pmsa( ipnt( nrofinputs + 30)   ) = fMrtN1VB01
         pmsa( ipnt( nrofinputs + 31)   ) = fMrtN3VB01
         pmsa( ipnt( nrofinputs + 32)   ) = fMrtN4VB01
         pmsa( ipnt( nrofinputs + 33)   ) = fMrtP1VB01
         pmsa( ipnt( nrofinputs + 34)   ) = fMrtP3VB01
         pmsa( ipnt( nrofinputs + 35)   ) = fMrtP4VB01
         pmsa( ipnt( nrofinputs + 36)   ) = fMrtS1VB01
         pmsa( ipnt( nrofinputs + 37)   ) = fMrtS3VB01
         pmsa( ipnt( nrofinputs + 38)   ) = fMrtS4VB01

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
         ipnt        = ipnt        + increm


!
!     segment loop
      enddo
!
      return
      end

end module m_vbmrt
