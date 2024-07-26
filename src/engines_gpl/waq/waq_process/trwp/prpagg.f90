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
module m_prpagg
    use m_waq_precision

    implicit none

contains


    subroutine PRPAGG   (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_properties
        use m_extract_waq_attribute


        !>\file
        !>       Properties of aggregated particles (one TRW particle and one suspended solids particle)

        !
        !     Description of the module :
        !
        !     Calculate the properties of the combined particles and then
        !     calculate the sedimentation velocity and the critical shear stress
        !     based on the combined particle's physical properties
        !
        ! Name            T   Index   Description                                   Units
        ! ----            --- -       -------------------                            ----
        ! DIAMETER1       R*4 1   diameter of the tyre particles                       [um]
        ! DENSITY1        R*4 2   density of the tyre particles                     [kg/m3]
        ! DIAMETER2       R*4 3   diameter of the sediment particles                   [um]
        ! DENSITY2        R*4 4   density of the sediment particles                 [kg/m3]
        ! BIOFILM_THK     R*4 5   thickness of the biofilm                             [um]
        ! BIOFILM_DENSITY R*4 6   density of the biofilm                            [kg/m3]
        !
        ! SETTLE_VEL      R*4 1   settling velocity                                   [m/d]
        ! TCR_SEDIM       R*4 2   critical shear stress for sedimentation              [Pa]
        !
        ! nov 2021 Jos van Gils added a loop over the fractions, to avoid long lists of processes and to speed up ...

        implicit none

        real(kind = real_wp) :: process_space_real  (*), fl    (*)
        integer(kind = int_wp) :: ipoint(*), increm(*), num_cells, noflux, &
                iexpnt(4, *), iknmrk(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
        !
        !   local declarations
        !
        integer(kind = int_wp) :: iseg, ikmrk1, ikmrk2, itel, num_exchanges, iq, ifrom, ipp
        real(kind = real_wp) :: diameter1, density1, diameter2, density2, biofilm_thk, biofilm_density
        real(kind = real_wp) :: combined_diameter, combined_density, new_shape_factor
        real(kind = real_wp) :: settle_vel, tcr_sedim

        integer(kind = int_wp) :: ipnt(500)
        integer(kind = int_wp), parameter :: ip_nTRWP = 1
        integer(kind = int_wp), parameter :: ip_nIM = 2
        integer(kind = int_wp), parameter :: ip_BioFilmThk = 3
        integer(kind = int_wp), parameter :: ip_BioFilmDen = 4
        integer(kind = int_wp), parameter :: ip_lastsingle = 4

        integer(kind = int_wp) :: ntrwp, itrwp, nsusp, isusp, nitem, offset

        ntrwp = process_space_real(ipoint(ip_ntrwp))
        nsusp = process_space_real(ipoint(ip_nim))
        nitem = 4 + 2 * ntrwp + 2 * nsusp + 3 * ntrwp * nsusp !

        !
        !  Note: we only need to do this once, no looping over the segments
        !  as all particles of the same size class have the same properties

        ipnt(1:nitem) = ipoint(1:nitem)

        do iseg = 1, num_cells
            call extract_waq_attribute(1, iknmrk(iseg), ikmrk1)
            if (ikmrk1==1) then
                call extract_waq_attribute(2, iknmrk(iseg), ikmrk2)
                if (ikmrk2<=4) then   ! surface water

                    ! input independentt of fractions
                    biofilm_thk = process_space_real(ipnt(ip_BioFilmThk))
                    biofilm_density = process_space_real(ipnt(ip_BioFilmDen))

                    ! loop over active fractions, IM are inner loop
                    itel = 0
                    do itrwp = 1, ntrwp
                        do isusp = 1, nsusp

                            itel = itel + 1
                            diameter1 = process_space_real(ipnt(ip_lastsingle + itrwp))
                            density1 = process_space_real(ipnt(ip_lastsingle + ntrwp + itrwp))
                            diameter2 = process_space_real(ipnt(ip_lastsingle + 2 * ntrwp + isusp))
                            density2 = process_space_real(ipnt(ip_lastsingle + 2 * ntrwp + nsusp + isusp))

                            call add_biofilm(diameter1, density1, biofilm_thk, biofilm_density)
                            call combine_particles(diameter1, diameter2, density1, density2, &
                                    combined_diameter, combined_density, new_shape_factor)
                            call calculate_sedim(combined_diameter, combined_density, new_shape_factor, settle_vel, tcr_sedim)

                            process_space_real(ipnt(ip_lastsingle + 2 * ntrwp + 2 * nsusp + itel)) = settle_vel
                            process_space_real(ipnt(ip_lastsingle + 2 * ntrwp + 2 * nsusp + ntrwp * nsusp + itel)) = tcr_sedim
                        enddo
                    enddo

                endif
            endif
            ipnt(1:nitem) = ipnt(1:nitem) + increm(1:nitem)

        enddo

        ! addition for use in 3D

        num_exchanges = num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir
        offset = ip_lastsingle + 2 * ntrwp + 2 * nsusp + 2 * ntrwp * nsusp
        ipnt(1:nitem) = ipoint(1:nitem)
        do IQ = 1, num_exchanges_u_dir + num_exchanges_v_dir
            itel = 0
            do itrwp = 1, ntrwp
                do isusp = 1, nsusp
                    itel = itel + 1
                    process_space_real(ipnt(offset + itel)) = 0.0
                enddo
            enddo
            ipnt(1:nitem) = ipnt(1:nitem) + increm(1:nitem)
        enddo
        do  IQ = num_exchanges_u_dir + num_exchanges_v_dir + 1, num_exchanges
            ifrom = IEXPNT(1, IQ)
            !
            !       Sedimentation velocity from segment to exchange-area
            !
            IF (ifrom > 0) THEN
                itel = 0
                do itrwp = 1, ntrwp
                    do isusp = 1, nsusp
                        itel = itel + 1
                        ipp = ip_lastsingle + 2 * ntrwp + 2 * nsusp + itel
                        settle_vel = process_space_real(ipoint(ipp) + (ifrom - 1) * increm(ipp))
                        process_space_real(ipnt(offset + itel)) = settle_vel
                    enddo
                enddo
            ENDIF
            ipnt(1:nitem) = ipnt(1:nitem) + increm(1:nitem)
        enddo

    end


end module m_prpagg
