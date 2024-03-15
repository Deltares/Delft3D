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
module m_phcomb
    use m_waq_precision

    implicit none

contains


    subroutine phcomb (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        !>\file
        !>       Calculates total C, P, N, Si, Dm, Chlorophyll in algae from fractions in Bloom

        ! Calculates some algae properties BLOOM

        implicit none

        ! declaration of the arguments

        real(kind = real_wp) :: pmsa(*)     !I/O Process Manager System Array, window of routine to process library
        real(kind = real_wp) :: fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(*)   ! I  Array of pointers in PMSA to get and store the data
        integer(kind = int_wp) :: increm(*)   ! I  Increments in IPOINT for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: noseg       ! I  Number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux      ! I  Number of fluxes, increment in the FL array
        integer(kind = int_wp) :: iexpnt(4, *) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
        integer(kind = int_wp) :: noq1        ! I  Nr of exchanges in 1st direction, only horizontal dir if irregular mesh
        integer(kind = int_wp) :: noq2        ! I  Nr of exchanges in 2nd direction, NOQ1+NOQ2 gives hor. dir. reg. grid
        integer(kind = int_wp) :: noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)

        ! variables from the pmsa array

        integer(kind = int_wp) :: ntype       ! I  number of algae types                          (-)
        real(kind = real_wp) :: depth       ! I  depth of computational cell                    (m)
        real(kind = real_wp) :: biomas      ! I  concentration of algae type                (gC/m3)
        integer(kind = int_wp) :: ispec       ! I  number of the group for algae type             (-)
        real(kind = real_wp) :: ncrat       ! I  N:C ratio algae type                       (gN/gC)
        real(kind = real_wp) :: pcrat       ! I  P:C ratio algae type                       (gP/gC)
        real(kind = real_wp) :: sicrat      ! I  Si:C ratio algae type                     (gSi/gC)
        real(kind = real_wp) :: dmcf        ! I  DM:C ratio algae type 01                  (gDM/gC)
        real(kind = real_wp) :: catocl      ! I  Chlorophyll-a:C ratio per algae type    (gChla/gC)
        real(kind = real_wp) :: xncralg     ! I  N:C for heterotrophic algae types          (gN/gC)
        real(kind = real_wp) :: xpcralg     ! I  P:C for heterotrophic algae types          (gP/gC)
        real(kind = real_wp) :: fncralg     ! I  N/C for nitrogen fixing algae types        (gN/gC)
        integer(kind = int_wp) :: fixalg      ! I  benthic:<0, resuspended:>0, plankton:=0        (-)
        real(kind = real_wp) :: phyt        ! O  total carbon in phytoplankton              (gC/m3)
        real(kind = real_wp) :: algn        ! O  total nitrogen in algae                    (gN/m3)
        real(kind = real_wp) :: algp        ! O  total phosphorus in algae                  (gP/m3)
        real(kind = real_wp) :: algsi       ! O  total silica in algae                     (gSi/m3)
        real(kind = real_wp) :: algdm       ! O  total DM in algae                         (gDM/m3)
        real(kind = real_wp) :: chlfa       ! O  Chlorophyll-a concentration                (mg/m3)
        real(kind = real_wp) :: cgroup      ! O  algae group concentration                  (gC/m3)

        ! number of variables in pmsa

        integer(kind = int_wp), parameter :: nipfix = 2 !    number of fixed inputs (not per type)
        integer(kind = int_wp), parameter :: nipvar = 11 !    number of variable inputs (per type)
        integer(kind = int_wp), parameter :: nopfix = 6 !    number of fixed outputs (not per type)
        integer(kind = int_wp), parameter :: nopvar = 1 !    number of variable outputs (per group)

        ! other local declarations

        integer(kind = int_wp) :: iseg        !    loop counter for computational element loop
        integer(kind = int_wp) :: itel        !    index in pmsa array
        integer(kind = int_wp) :: itype       !    loop counter types
        integer(kind = int_wp) :: igrp        !    index algae groups
        integer(kind = int_wp) :: ispec_prev  !    previous algae group

        ntype = pmsa(ipoint(1))

        do iseg = 1, noseg

            depth = pmsa (ipoint(2) + (iseg - 1) * increm(2))

            phyt = 0.0
            algn = 0.0
            algp = 0.0
            algsi = 0.0
            algdm = 0.0
            chlfa = 0.0
            ispec_prev = 0
            igrp = 0

            do itype = 1, ntype

                itel = nipfix + itype + ntype
                ispec = nint(pmsa (ipoint(itel) + (iseg - 1) * increm(itel)))

                if (ispec > 0) then

                    itel = nipfix + itype
                    biomas = pmsa (ipoint(itel) + (iseg - 1) * increm(itel))
                    itel = nipfix + itype + ntype * 2
                    ncrat = pmsa (ipoint(itel) + (iseg - 1) * increm(itel))
                    itel = nipfix + itype + ntype * 3
                    pcrat = pmsa (ipoint(itel) + (iseg - 1) * increm(itel))
                    itel = nipfix + itype + ntype * 4
                    sicrat = pmsa (ipoint(itel) + (iseg - 1) * increm(itel))
                    itel = nipfix + itype + ntype * 5
                    dmcf = pmsa (ipoint(itel) + (iseg - 1) * increm(itel))
                    itel = nipfix + itype + ntype * 6
                    catocl = pmsa (ipoint(itel) + (iseg - 1) * increm(itel))
                    itel = nipfix + itype + ntype * 7
                    xncralg = max(0.0, pmsa (ipoint(itel) + (iseg - 1) * increm(itel)))
                    itel = nipfix + itype + ntype * 8
                    xpcralg = max(0.0, pmsa (ipoint(itel) + (iseg - 1) * increm(itel)))
                    itel = nipfix + itype + ntype * 9
                    fncralg = max(0.0, pmsa (ipoint(itel) + (iseg - 1) * increm(itel)))
                    itel = nipfix + itype + ntype * 10
                    fixalg = nint(pmsa (ipoint(itel) + (iseg - 1) * increm(itel)))

                    ! add ratios of mixotophic and n-fixing algae

                    ncrat = ncrat + xncralg + fncralg
                    pcrat = pcrat + xpcralg

                    ! sum to the totals (not for rooted types, jvg 10102012)

                    if (fixalg>=0) then
                        phyt = phyt + biomas
                        algn = algn + biomas * ncrat
                        algp = algp + biomas * pcrat
                        algsi = algsi + biomas * sicrat
                        algdm = algdm + biomas * dmcf
                        chlfa = chlfa + biomas * catocl * 1000.
                    endif

                    ! biomass group

                    if (ispec /= ispec_prev) then
                        cgroup = 0.0
                        igrp = igrp + 1
                        ispec_prev = ispec
                    endif

                    cgroup = cgroup + biomas
                    itel = nipfix + nipvar * ntype + nopfix + igrp
                    pmsa (ipoint(itel) + (iseg - 1) * increm(itel)) = cgroup

                endif

            enddo

            itel = nipfix + nipvar * ntype + 1
            pmsa (ipoint(itel) + (iseg - 1) * increm(itel)) = phyt
            itel = nipfix + nipvar * ntype + 2
            pmsa (ipoint(itel) + (iseg - 1) * increm(itel)) = algn
            itel = nipfix + nipvar * ntype + 3
            pmsa (ipoint(itel) + (iseg - 1) * increm(itel)) = algp
            itel = nipfix + nipvar * ntype + 4
            pmsa (ipoint(itel) + (iseg - 1) * increm(itel)) = algsi
            itel = nipfix + nipvar * ntype + 5
            pmsa (ipoint(itel) + (iseg - 1) * increm(itel)) = algdm
            itel = nipfix + nipvar * ntype + 6
            pmsa (ipoint(itel) + (iseg - 1) * increm(itel)) = chlfa

        enddo

        return
    end

end module m_phcomb
