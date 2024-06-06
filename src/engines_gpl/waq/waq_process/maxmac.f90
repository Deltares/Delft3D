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
module m_maxmac
    use m_waq_precision

    implicit none

contains


    SUBROUTINE MAXMAC     (PMSA, FL, IPOINT, INCREM, NOSEG, &
            NOFLUX, IEXPNT, IKNMRK, NOQ1, NOQ2, &
            NOQ3, NOQ4)
        use m_evaluate_waq_attribute

        !
        !*******************************************************************************
        !
        IMPLICIT NONE
        !
        !     Type    Name         I/O Description
        !
        REAL(kind = real_wp) :: PMSA(*)     !I/O Process Manager System Array, window of routine to process library
        REAL(kind = real_wp) :: FL(*)       ! O  Array of fluxes made by this process in mass/volume/time
        INTEGER(kind = int_wp) :: IPOINT(31) ! I  Array of pointers in PMSA to get and store the data
        INTEGER(kind = int_wp) :: INCREM(31) ! I  Increments in IPOINT for segment loop, 0=constant, 1=spatially varying
        INTEGER(kind = int_wp) :: NOSEG       ! I  Number of computational elements in the whole model schematisation
        INTEGER(kind = int_wp) :: NOFLUX      ! I  Number of fluxes, increment in the FL array
        INTEGER(kind = int_wp) :: IEXPNT      ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
        INTEGER(kind = int_wp) :: IKNMRK(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
        INTEGER(kind = int_wp) :: NOQ1        ! I  Nr of exchanges in 1st direction, only horizontal dir if irregular mesh
        INTEGER(kind = int_wp) :: NOQ2        ! I  Nr of exchanges in 2nd direction, NOQ1+NOQ2 gives hor. dir. reg. grid
        INTEGER(kind = int_wp) :: NOQ3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
        INTEGER(kind = int_wp) :: NOQ4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
        INTEGER(kind = int_wp) :: IPNT(31)   !    Local work array for the pointering
        INTEGER(kind = int_wp) :: ISEG        !    Local loop counter for computational element loop
        !
        !*******************************************************************************
        !
        !     Type    Name         I/O Description                                        Unit
        !
        REAL(kind = real_wp) :: nMacrophyt  ! I  number of macrophyte species                       (-)
        REAL(kind = real_wp) :: HSIEM01     ! I  Habitat Suitability Index for EM01                 (-)
        REAL(kind = real_wp) :: PotEM01     ! I  potential biomass for EM01                         (gC/M2)
        REAL(kind = real_wp) :: HSISM01     ! I  Habitat Suitability Index for SM01                 (-)
        REAL(kind = real_wp) :: PotSM01     ! I  potential biomass for SM01                         (gC/M2)
        REAL(kind = real_wp) :: HSIEM02     ! I  Habitat Suitability Index for EM02                 (-)
        REAL(kind = real_wp) :: PotEM02     ! I  potential biomass for EM02                         (gC/M2)
        REAL(kind = real_wp) :: HSISM02     ! I  Habitat Suitability Index for SM02                 (-)
        REAL(kind = real_wp) :: PotSM02     ! I  potential biomass for SM02                         (gC/M2)
        REAL(kind = real_wp) :: HSIEM03     ! I  Habitat Suitability Index for EM03                 (-)
        REAL(kind = real_wp) :: PotEM03     ! I  potential biomass for EM03                         (gC/M2)
        REAL(kind = real_wp) :: HSISM03     ! I  Habitat Suitability Index for SM03                 (-)
        REAL(kind = real_wp) :: PotSM03     ! I  potential biomass for SM03                         (gC/M2)
        REAL(kind = real_wp) :: HSIEM04     ! I  Habitat Suitability Index for EM04                 (-)
        REAL(kind = real_wp) :: PotEM04     ! I  potential biomass for EM04                         (gC/M2)
        REAL(kind = real_wp) :: HSISM04     ! I  Habitat Suitability Index for SM04                 (-)
        REAL(kind = real_wp) :: PotSM04     ! I  potential biomass for SM04                         (gC/M2)
        REAL(kind = real_wp) :: HSIEM05     ! I  Habitat Suitability Index for EM05                 (-)
        REAL(kind = real_wp) :: PotEM05     ! I  potential biomass for EM05                         (gC/M2)
        REAL(kind = real_wp) :: HSISM05     ! I  Habitat Suitability Index for SM05                 (-)
        REAL(kind = real_wp) :: PotSM05     ! I  potential biomass for SM05                         (gC/M2)
        REAL(kind = real_wp) :: MaxEM01     ! O  maximum biomass for macrophyt emerged 01           (gC)
        REAL(kind = real_wp) :: MaxSM01     ! O  maximum biomass for macrophyt submerged 01         (gC)
        REAL(kind = real_wp) :: MaxEM02     ! O  maximum biomass for EM02                           (gC/M2)
        REAL(kind = real_wp) :: MaxSM02     ! O  maximum biomass for SM02                           (gC/M2)
        REAL(kind = real_wp) :: MaxEM03     ! O  maximum biomass for EM03                           (gC/M2)
        REAL(kind = real_wp) :: MaxSM03     ! O  maximum biomass for SM03                           (gC/M2)
        REAL(kind = real_wp) :: MaxEM04     ! O  maximum biomass for EM04                           (gC/M2)
        REAL(kind = real_wp) :: MaxSM04     ! O  maximum biomass for SM04                           (gC/M2)
        REAL(kind = real_wp) :: MaxEM05     ! O  maximum biomass for EM05                           (gC/M2)
        REAL(kind = real_wp) :: MaxSM05     ! O  maximum biomass for SM05                           (gC/M2)

        REAL(kind = real_wp) :: SumHSIEM
        REAL(kind = real_wp) :: SumHSISM
        INTEGER(kind = int_wp) :: IKMRK1
        !
        !*******************************************************************************
        !
        IPNT = IPOINT
        !
        DO ISEG = 1, NOSEG
            !
            CALL extract_waq_attribute(1, IKNMRK(ISEG), IKMRK1)
            IF (IKMRK1==1) THEN

                nMacrophyt = PMSA(IPNT(1))
                HSIEM01 = PMSA(IPNT(2))
                PotEM01 = PMSA(IPNT(3))
                HSISM01 = PMSA(IPNT(4))
                PotSM01 = PMSA(IPNT(5))
                HSIEM02 = PMSA(IPNT(6))
                PotEM02 = PMSA(IPNT(7))
                HSISM02 = PMSA(IPNT(8))
                PotSM02 = PMSA(IPNT(9))
                HSIEM03 = PMSA(IPNT(10))
                PotEM03 = PMSA(IPNT(11))
                HSISM03 = PMSA(IPNT(12))
                PotSM03 = PMSA(IPNT(13))
                HSIEM04 = PMSA(IPNT(14))
                PotEM04 = PMSA(IPNT(15))
                HSISM04 = PMSA(IPNT(16))
                PotSM04 = PMSA(IPNT(17))
                HSIEM05 = PMSA(IPNT(18))
                PotEM05 = PMSA(IPNT(19))
                HSISM05 = PMSA(IPNT(20))
                PotSM05 = PMSA(IPNT(21))
                !
                !   *****     Insert your code here  *****
                !
                SumHSIEM = HSIEM01 + HSIEM02 + HSIEM03 + HSIEM03 + HSIEM05
                IF (SumHSIEM < 1.E-20) SumHSIEM = 1.0

                SumHSISM = HSISM01 + HSISM02 + HSISM03 + HSISM03 + HSISM05
                IF (SumHSISM < 1.E-20) SumHSISM = 1.0

                MaxEM01 = HSIEM01 * PotEM01 / SumHSIEM
                MaxSM01 = HSISM01 * PotSM01 / SumHSISM
                MaxEM02 = HSIEM02 * PotEM02 / SumHSIEM
                MaxSM02 = HSISM02 * PotSM02 / SumHSISM
                MaxEM03 = HSIEM03 * PotEM03 / SumHSIEM
                MaxSM03 = HSISM03 * PotSM03 / SumHSISM
                MaxEM04 = HSIEM04 * PotEM04 / SumHSIEM
                MaxSM04 = HSISM04 * PotSM04 / SumHSISM
                MaxEM05 = HSIEM05 * PotEM05 / SumHSIEM
                MaxSM05 = HSISM05 * PotSM05 / SumHSISM
                !
                !   *****     End of your code       *****
                !
                PMSA(IPNT(22)) = MaxEM01
                PMSA(IPNT(23)) = MaxSM01
                PMSA(IPNT(24)) = MaxEM02
                PMSA(IPNT(25)) = MaxSM02
                PMSA(IPNT(26)) = MaxEM03
                PMSA(IPNT(27)) = MaxSM03
                PMSA(IPNT(28)) = MaxEM04
                PMSA(IPNT(29)) = MaxSM04
                PMSA(IPNT(30)) = MaxEM05
                PMSA(IPNT(31)) = MaxSM05

            ENDIF
            !
            IPNT = IPNT + INCREM
            !
        end do
        !
        RETURN
    END

end module m_maxmac
