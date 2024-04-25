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
module m_grzmac
    use m_waq_precision

    implicit none

contains


    SUBROUTINE GRZMAC     (PMSA, FL, IPOINT, INCREM, NOSEG, &
            NOFLUX, IEXPNT, IKNMRK, NOQ1, NOQ2, &
            NOQ3, NOQ4)
        use m_monsys
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
        INTEGER(kind = int_wp) :: IPOINT(14) ! I  Array of pointers in PMSA to get and store the data
        INTEGER(kind = int_wp) :: INCREM(14) ! I  Increments in IPOINT for segment loop, 0=constant, 1=spatially varying
        INTEGER(kind = int_wp) :: NOSEG       ! I  Number of computational elements in the whole model schematisation
        INTEGER(kind = int_wp) :: NOFLUX      ! I  Number of fluxes, increment in the FL array
        INTEGER(kind = int_wp) :: IEXPNT      ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
        INTEGER(kind = int_wp) :: IKNMRK(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
        INTEGER(kind = int_wp) :: NOQ1        ! I  Nr of exchanges in 1st direction, only horizontal dir if irregular mesh
        INTEGER(kind = int_wp) :: NOQ2        ! I  Nr of exchanges in 2nd direction, NOQ1+NOQ2 gives hor. dir. reg. grid
        INTEGER(kind = int_wp) :: NOQ3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
        INTEGER(kind = int_wp) :: NOQ4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
        INTEGER(kind = int_wp) :: IPNT(14)   !    Local work array for the pointering
        INTEGER(kind = int_wp) :: ISEG        !    Local loop counter for computational element loop
        !
        !*******************************************************************************
        !
        !     Type    Name         I/O Description                                        Unit
        !
        REAL(kind = real_wp) :: EM          ! I  macrophyt emerged                                  (gC/m2)
        REAL(kind = real_wp) :: SM          ! I  macrophyt submerged                                (gC/m2)
        REAL(kind = real_wp) :: RH          ! I  macrophyt rhizome                                  (gC/m2)
        REAL(kind = real_wp) :: NRH         ! I  nitrogen content macrophyt rhizome                 (gN/m2)
        REAL(kind = real_wp) :: PRH         ! I  phosphorus content macrophyt rhizome               (gP/m2)
        REAL(kind = real_wp) :: K0GrzEM     ! I  zeroth-order grazing flux macrophyte EM            (gC/m2/d)
        REAL(kind = real_wp) :: K1GrzEM     ! I  first order grazing rate macrophyte EM             (1/d)
        REAL(kind = real_wp) :: K0GrzSM     ! I  zeroth-order grazing flux macrophyte SM            (gC/m2/d)
        REAL(kind = real_wp) :: K1GrzSM     ! I  first order grazing rate macrophyte SM             (1/d)
        REAL(kind = real_wp) :: K0GrzRH     ! I  zeroth-order grazing flux macrophyte RH            (gC/m2/d)
        REAL(kind = real_wp) :: K1GrzRH     ! I  first order grazing rate macrophyte RH             (1/d)
        REAL(kind = real_wp) :: Volume      ! I  volume of computational cell                       (m3)
        REAL(kind = real_wp) :: Depth       ! I  depth of segment                                   (m)
        REAL(kind = real_wp) :: DELT        ! I  timestep for processes                             (d)
        REAL(kind = real_wp) :: dGrazeEM    ! F  grazing flux macrophyte EM                         (gC/m3/d)
        REAL(kind = real_wp) :: dGrazeSM    ! F  grazing flux macrophyte SM                         (gC/m3/d)
        REAL(kind = real_wp) :: dGrazeRH    ! F  grazing flux macrophyte RH                         (gC/m3/d)
        REAL(kind = real_wp) :: dGrzNRH     ! F  grazing flux macrophyte NRH                        (gC/m3/d)
        REAL(kind = real_wp) :: dGrzPRH     ! F  grazing flux macrophyte PRH                        (gC/m3/d)
        INTEGER(kind = int_wp) :: IdGrazeEM   !    Pointer to the grazing flux macrophyte EM
        INTEGER(kind = int_wp) :: IdGrazeSM   !    Pointer to the grazing flux macrophyte SM
        INTEGER(kind = int_wp) :: IdGrazeRH   !    Pointer to the grazing flux macrophyte RH
        INTEGER(kind = int_wp) :: IdGrzNRH    !    Pointer to the grazing flux macrophyte NRH
        INTEGER(kind = int_wp) :: IdGrzPRH    !    Pointer to the grazing flux macrophyte PRH
        REAL(kind = real_wp) :: SURF        ! L  surface area                                       (m2)
        INTEGER(kind = int_wp) :: IKMRK1

        INTEGER(kind = int_wp), SAVE :: NR_MSG = 0
        INTEGER(kind = int_wp) :: LUNREP
        !
        !*******************************************************************************
        !
        IPNT = IPOINT
        IdGrazeEM = 1
        IdGrazeSM = 2
        IdGrazeRH = 3
        IdGrzNRH = 4
        IdGrzPRH = 5
        !
        DO ISEG = 1, NOSEG

            CALL evaluate_waq_attribute(1, IKNMRK(ISEG), IKMRK1)
            IF (IKMRK1==1) THEN

                !
                EM = PMSA(IPNT(1))
                SM = PMSA(IPNT(2))
                RH = PMSA(IPNT(3))
                NRH = PMSA(IPNT(4))
                PRH = PMSA(IPNT(5))
                K0GrzEM = PMSA(IPNT(6))
                K1GrzEM = PMSA(IPNT(7))
                K0GrzSM = PMSA(IPNT(8))
                K1GrzSM = PMSA(IPNT(9))
                K0GrzRH = PMSA(IPNT(10))
                K1GrzRH = PMSA(IPNT(11))
                Volume = PMSA(IPNT(12))
                Depth = PMSA(IPNT(13))
                DELT = PMSA(IPNT(14))
                !
                !   *****     Insert your code here  *****
                !
                ! check input

                IF (DEPTH < 1E-20) THEN
                    NR_MSG = NR_MSG + 1
                    CALL GETMLU(LUNREP)
                    IF (NR_MSG <= 25) THEN
                        WRITE(LUNREP, *) 'GRZMAC: WARNING - depth zero or negative'
                        WRITE(LUNREP, *) '   Segment:', ISEG
                        WRITE(LUNREP, *) '   Depth:', DEPTH
                        IF (NR_MSG == 25) THEN
                            WRITE(LUNREP, *) 'GRZMAC: 25 warnings written - further warnings suppressed'
                        ENDIF
                    ENDIF
                ENDIF

                SURF = VOLUME / MAX(DEPTH, 1.0E-20)
                IF (SURF < 1E-20) THEN
                    NR_MSG = NR_MSG + 1
                    CALL GETMLU(LUNREP)
                    IF (NR_MSG <= 25) THEN
                        WRITE(LUNREP, *) 'GRZMAC: WARNING - surface zero or negative'
                        WRITE(LUNREP, *) '   Segment:', ISEG
                        WRITE(LUNREP, *) '   Surface:', SURF
                        IF (NR_MSG == 25) THEN
                            WRITE(LUNREP, *) 'GRZMAC: 25 warnings written - further warnings suppressed'
                        ENDIF
                    ENDIF
                ENDIF

                ! graze on emerged macrophyte

                dGrazeEM = K0GrzEM + EM * K1GrzEM
                IF (EM > dGrazeEM * DELT) THEN
                    dGrazeEM = dGrazeEM / DEPTH
                ELSE
                    dGrazeEM = 0.0
                ENDIF

                ! graze on submerged macrophyte

                dGrazeSM = K0GrzSM + SM * K1GrzSM
                IF (SM > dGrazeSM * DELT) THEN
                    dGrazeSM = dGrazeSM / DEPTH
                ELSE
                    dGrazeSM = 0.0
                ENDIF

                ! graze on rhizome macrophyte

                dGrazeRH = K0GrzRH + RH * K1GrzRH
                IF (RH > dGrazeRH * DELT) THEN
                    dGrazeRH = dGrazeRH / DEPTH
                ELSE
                    dGrazeRH = 0.0
                ENDIF

                ! the nitrogen content of rhizome

                IF (RH > 1e-20) THEN
                    dGrzNRH = dGrazeRH * NRH / RH
                ELSE
                    dGrzNRH = 0.0
                ENDIF

                ! the phosphorus content of rhizome

                IF (RH > 1e-20) THEN
                    dGrzPRH = dGrazeRH * PRH / RH
                ELSE
                    dGrzPRH = 0.0
                ENDIF

            ENDIF

            !
            !   *****     End of your code       *****
            !
            FL  (IdGrazeEM) = dGrazeEM
            FL  (IdGrazeSM) = dGrazeSM
            FL  (IdGrazeRH) = dGrazeRH
            FL  (IdGrzNRH) = dGrzNRH
            FL  (IdGrzPRH) = dGrzPRH
            !
            IdGrazeEM = IdGrazeEM + NOFLUX
            IdGrazeSM = IdGrazeSM + NOFLUX
            IdGrazeRH = IdGrazeRH + NOFLUX
            IdGrzNRH = IdGrzNRH + NOFLUX
            IdGrzPRH = IdGrzPRH + NOFLUX
            IPNT = IPNT + INCREM
            !
        end do
        !
        RETURN
    END

end module m_grzmac
