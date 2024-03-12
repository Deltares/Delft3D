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
module m_ulfix
    use m_waq_precision

    implicit none

contains


    subroutine ulfix  (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_dhnoseg
        use m_dhnolay
        use m_evaluate_waq_attribute

        !>\file
        !>       Fixation of BLOOM algae at the water bed (e.g. for Ulvae)

        implicit none

        !     arguments

        real(kind = real_wp) :: pmsa(*)            !I/O Process Manager System Array, window of routine to process library
        real(kind = real_wp) :: fl(*)              ! O  Array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(*)        ! I  Array of pointers in pmsa to get and store the data
        integer(kind = int_wp) :: increm(*)        ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: noseg              ! I  Number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux             ! I  Number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *)        ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)          ! I  Active-Inactive, Surface-water-bottom, see manual for use
        integer(kind = int_wp) :: noq1               ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
        integer(kind = int_wp) :: noq2               ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
        integer(kind = int_wp) :: noq3               ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: noq4               ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)

        !     pmsa array

        real(kind = real_wp) :: tau                ! I  total bottom shear stress                          (N/m2)
        real(kind = real_wp) :: taucrulva          ! I  critical shear stress for resuspension ULVA        (N/m2)
        real(kind = real_wp) :: fixgrad            ! I  gradient of fixation versus shear stress           (-)
        real(kind = real_wp) :: delt               ! I  timestep for processes                             (d)
        real(kind = real_wp) :: depth              ! I  depth of segment                                   (m)
        real(kind = real_wp) :: volume             ! I  volume of computational cell                       (m3)
        real(kind = real_wp) :: bloomalg           ! I  algae concentration                                (gC/m3)
        real(kind = real_wp) :: frfixedalg         ! O  fraction of algae fixed                            (-)

        !     fluxes

        real(kind = real_wp) :: dsedresalg  ! F  sedimentation flux algae (gC) in fl array:         (gC/m3/d)

        !     local variables

        integer(kind = int_wp), parameter :: ntyp_m = 30        ! number of algae types expected in pmsa
        integer(kind = int_wp), parameter :: nipfix = 7        ! first number of entries in pmsa independent of number of algae
        integer(kind = int_wp), parameter :: nipvar = 2        ! number of input entries in pmsa dependent of number of algae
        integer(kind = int_wp) :: ifix(ntyp_m)       ! fix flag >0 suspended <0 corresponding fixed, copied from pmsa
        integer(kind = int_wp) :: jfix               ! fix flag >0 suspended <0 corresponding fixed other algae
        integer(kind = int_wp) :: ialg_fixed(ntyp_m) ! if present number of corresponding fixed type
        integer(kind = int_wp) :: nosegw             ! number of segments in the water
        integer(kind = int_wp) :: nosegl             ! number of segments per layer
        integer(kind = int_wp) :: iseg               ! segment number
        integer(kind = int_wp) :: isegl              ! segment number top layer (=column number)
        integer(kind = int_wp) :: isegb              ! segment number of bottom segment of the column
        integer(kind = int_wp) :: nolay              ! number of layers
        integer(kind = int_wp) :: ilay               ! layer number
        integer(kind = int_wp) :: ilayb              ! layer number of bottom segment of the column
        integer(kind = int_wp) :: ialg               ! algae type (suspended)
        integer(kind = int_wp) :: ialg2              ! algae type
        integer(kind = int_wp) :: jalg               ! algae type (fixed)
        real(kind = real_wp) :: msusp              ! mass suspended over the column (gC)
        real(kind = real_wp) :: mfix               ! mass fixed over the column (gC)
        real(kind = real_wp) :: mtot               ! total mass over the column (gC)
        real(kind = real_wp) :: frac               ! fraction of suspended mass in a layer (-)
        integer(kind = int_wp) :: ikmrk2             ! second segment attribute
        integer(kind = int_wp) :: ip                 ! index pointer in pmsa
        integer(kind = int_wp) :: ipp                ! index pointer in pmsa
        integer(kind = int_wp) :: ip1, ip2, ip3, ip4, ip5! index pointers in pmsa
        integer(kind = int_wp) :: ip6, ip7            ! index pointers in pmsa
        integer(kind = int_wp) :: in1, in2, in3, in4, in5! increments in pmsa
        integer(kind = int_wp) :: in6, in7            ! increments in pmsa
        integer(kind = int_wp) :: io1                ! index pointers in pmsa first output parameter
        integer(kind = int_wp) :: ino1               ! increment first output parameter

        ! some init

        ip1 = ipoint(1)
        ip2 = ipoint(2)
        ip3 = ipoint(3)
        ip4 = ipoint(4)
        ip5 = ipoint(5)
        ip6 = ipoint(6)
        ip7 = ipoint(7)
        in1 = increm(1)
        in2 = increm(2)
        in3 = increm(3)
        in4 = increm(4)
        in5 = increm(5)
        in6 = increm(6)
        in7 = increm(7)
        io1 = ipoint(nipfix + nipvar * ntyp_m + 1)
        ino1 = increm(nipfix + 2 * ntyp_m + 1)

        call dhnoseg(nosegw)
        call dhnolay(nolay)
        nosegl = nosegw / nolay
        delt = pmsa(ipoint(4))

        ! set the ifix and ialg_fixed array (should be independent of the segment number)

        ifix = 0
        ialg_fixed = 0
        do ialg = 1, ntyp_m
            ip = nipfix + ialg + ntyp_m
            ifix(ialg) = nint(pmsa(ipoint(ip)))
            if (ifix(ialg) > 0) then

                !find corresponding type in rest of the algae

                do ialg2 = 1, ntyp_m
                    ip = nipfix + ialg2 + ntyp_m
                    jfix = nint(pmsa(ipoint(ip)))
                    if (jfix == -1 * ifix(ialg)) then
                        ialg_fixed(ialg) = ialg2
                        exit
                    endif
                enddo

            endif
        enddo

        ! loop over the columns to determine the fixing/defixing

        do isegl = 1, nosegl

            do ialg = 1, ntyp_m
                jalg = ialg_fixed(ialg)
                if (ifix(ialg) > 0 .and. jalg > 0) then

                    ! calculate total suspended up till the bottom

                    msusp = 0.0
                    do ilay = 1, nolay
                        iseg = isegl + (ilay - 1) * nosegl
                        volume = pmsa(ip7 + (iseg - 1) * in7)
                        depth = pmsa(ip5 + (iseg - 1) * in5)
                        ip = ipoint(nipfix + ialg) + (iseg - 1) * increm(nipfix + ialg)
                        bloomalg = max(pmsa(ip), 0.0)
                        msusp = msusp + bloomalg * volume  ! total mass suspended type in g
                        call evaluate_waq_attribute(2, iknmrk(iseg), ikmrk2)
                        if ((ikmrk2==0).or.(ikmrk2==3)) then

                            ip = ipoint(nipfix + jalg) + (iseg - 1) * increm(nipfix + jalg)
                            bloomalg = max(pmsa(ip), 0.0)
                            mfix = bloomalg * volume / depth     ! biomass of attached (fixed) type (convert from g/m2 to g)
                            isegb = iseg
                            ilayb = ilay
                            exit
                        endif
                    enddo

                    ! if the bottom segment is inactive exit algae loop

                    if (.not. btest(iknmrk(isegb), 0)) exit

                    ! caluclate distribution according to tau at the bottom segment

                    tau = pmsa(ip1 + (isegb - 1) * in1)
                    taucrulva = pmsa(ip2 + (isegb - 1) * in2)
                    fixgrad = pmsa(ip3 + (isegb - 1) * in3)
                    if (taucrulva > 0.) then
                        frfixedalg = min(1., max(0., fixgrad - tau / taucrulva))
                    else
                        frfixedalg = 0.
                    endif
                    pmsa(io1 + (isegb - 1) * ino1) = frfixedalg

                    mtot = msusp + mfix
                    dsedresalg = frfixedalg * mtot - mfix
                    if (dsedresalg > 0) then

                        ! rooting event decrease suspended material proportionally

                        do ilay = 1, ilayb
                            iseg = isegl + (ilay - 1) * nosegl
                            volume = pmsa(ip7 + (iseg - 1) * in7)
                            ip = ipoint(nipfix + ialg) + (iseg - 1) * increm(nipfix + ialg)
                            bloomalg = max(pmsa(ip), 0.0)
                            frac = bloomalg * volume / msusp
                            fl(ialg + (iseg - 1) * noflux) = -frac * dsedresalg / volume / delt
                        enddo
                        fl(jalg + (isegb - 1) * noflux) = dsedresalg / volume / delt

                    else

                        ! de-rooting event, all material towards suspension in lowest layer

                        volume = pmsa(ip7 + (isegb - 1) * in7)
                        fl(ialg + (isegb - 1) * noflux) = -dsedresalg / volume / delt
                        fl(jalg + (isegb - 1) * noflux) = dsedresalg / volume / delt

                    endif

                endif

            enddo ! algea type

        enddo ! columns

        ! loop to calculate concentration per m2

        ip5 = ipoint(5)
        ip7 = ipoint(7)
        do iseg = 1, noseg

            if (btest(iknmrk(iseg), 0)) then

                depth = pmsa(ip5)
                volume = pmsa(ip7)

                do ialg = 1, ntyp_m
                    ip = nipfix + ialg
                    ipp = ipoint(ip) + (iseg - 1) * increm(ip)
                    if (ifix(ialg) < 0) then
                        bloomalg = pmsa(ipp) / volume
                    else
                        bloomalg = pmsa(ipp)
                    endif
                    ip = nipfix + ialg + 2 * ntyp_m + 1
                    ipp = ipoint(ip) + (iseg - 1) * increm(ip)
                    pmsa(ipp) = bloomalg * depth
                enddo

            endif

            ip5 = ip5 + increm (5)
            ip7 = ip7 + increm (7)

        enddo

        return
    end

end module m_ulfix
