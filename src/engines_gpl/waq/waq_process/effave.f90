!!  Copyright (C)  Stichting Deltares, 2012-2026.
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
module m_effave
    use m_waq_precision
    use m_get_effi

    implicit none

contains


    subroutine effave (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)

        !>\file
        !>       Average efficiency for a Bloom time step (typically a day)

        !
        !     Description of the module :
        !
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        implicit none

        integer, parameter :: max_number_algae = 30
        integer, parameter :: number_pointers  = 2 + 3 * max_number_algae

        real(kind = real_wp) :: process_space_real  (*), fl    (*)
        integer(kind = int_wp) :: ipoint(number_pointers), increm(number_pointers), num_cells, noflux, &
                iexpnt(4, *), iknmrk(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

        integer(kind = int_wp) :: ip(number_pointers)
        real(kind = real_wp) :: delt, efftalg, limralg, bloom_step
        integer(kind = int_wp) :: navera
        integer(kind = int_wp) :: iseg, iflux, igro
        integer(kind = int_wp) :: nspe       ! number of bloom algae species

        real(kind = real_wp), save :: elapsed_time = 0.0_real_wp
        logical                    :: reset

        !     this is in a module/include, so we might put a flag if it was read of not.
        !     this should be a 'proto-proces', and thus needs to be added to the BLOOM.SPE

        !     Retrieve switch for averaging and nr. of steps to be averaged
        call get_nspe(nspe)

        delt = process_space_real(ipoint(1))
        bloom_step = process_space_real(ipoint(2))
        elapsed_time = elapsed_time + delt

        if ( abs(elapsed_time - bloom_step) <= 0.5 * delt ) then
            reset        = .true.
            elapsed_time = delt
        endif

        !     Loop over segments

        ip = ipoint
        iflux = 0

        do iseg = 1, num_cells

            do igro = 1, nspe
                efftalg = process_space_real(ip(2 + igro))
                limralg = process_space_real(ip(2 + max_number_algae + igro))
                if (reset) then
                    ! Store result over past period
                    process_space_real(ip(2 + 2 * max_number_algae + igro)) = efftalg
                endif

                ! Add contribution of present time step to tracer
                fl(iflux + igro) = (limralg - efftalg) / elapsed_time
            end do

            ip = ip + increm
            iflux = iflux + noflux
        enddo

        !     Update the elapsed time - if necessary, reset
        if ( abs(elapsed_time - bloom_step) <= 0.5 * delt ) then
            elapsed_time = 0.0_real_wp
        endif
    end

end module m_effave
