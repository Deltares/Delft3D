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

module larvm2_mod

contains
    subroutine larvm2 (lunrep, itime, nosegl, num_layers, nosubs, &
            conc)

        ! function            : calculate larvae per m2 over the water column, output to map file

        !
        !  data definition module(s)
        !
        use m_waq_precision               ! single/double precision
        use timers
        implicit none


        ! arguments :

        integer(int_wp), intent(in) :: lunrep              ! report file
        integer(int_wp), intent(in) :: itime               ! time in seconds
        integer(int_wp), intent(in) :: nosegl              ! number segments per layer
        integer(int_wp), intent(in) :: num_layers               ! number of layers in calculation
        integer(int_wp), intent(in) :: nosubs              ! number of substances per particle
        real   (sp), pointer :: conc  (:, :)     ! concentration array in transport grid

        ! function            : calculate larvae per m2 over the water column

        integer, save :: lunmm2
        character(len = 256) :: filmm2
        real, allocatable, save :: c_m2(:, :)
        character(len = 20), allocatable :: syname(:)
        character(len = 40) :: moname(4)
        integer, save :: num_substances_total
        integer :: iseg
        integer :: isegl
        integer(int_wp) :: ilay
        integer(int_wp) :: isys
        integer(int_wp) :: istage
        integer, save :: ifirst = 1

        if (ifirst == 1) then
            ifirst = 0
            filmm2 = 'larvae_m2.map'
            moname(1) = 'larvae per m2'
            moname(2) = ' '
            moname(3) = ' '
            moname(4) = ' '
            num_substances_total = nosubs + 1
            allocate(syname(nosubs - 3))
            allocate(c_m2(nosubs - 3, nosegl))
            syname(1) = 'sole'
            do isys = 2, nosubs - 3
                istage = isys - 1
                write(syname(isys), '(''sole'',i2.2,''stage'')') istage
            enddo
            open (newunit = lunmm2, file = filmm2, access = 'stream', form = 'unformatted')
            write(lunmm2) moname
            write(lunmm2) nosubs - 3, nosegl
            write(lunmm2) syname
        endif

        do isegl = 1, nosegl

            ! if seperate stages accumulate stages (thus skipping stage 0), also seperate per stage, num_substances_total = localdepth

            if (nosubs > 4) then

                c_m2(1, isegl) = 0.0
                do istage = 1, nosubs - 4
                    c_m2(1, isegl) = c_m2(1, isegl) + conc(4 + istage, isegl) * conc(num_substances_total, isegl)
                    c_m2(1 + istage, isegl) = conc(4 + istage, isegl) * conc(num_substances_total, isegl)
                    do ilay = 2, num_layers
                        iseg = isegl + (ilay - 1) * nosegl
                        c_m2(1, isegl) = c_m2(1, isegl) + conc(4 + istage, iseg) * (conc(num_substances_total, iseg) - conc(num_substances_total, iseg - nosegl))
                        c_m2(1 + istage, isegl) = c_m2(1 + istage, isegl) + conc(4 + istage, iseg) * (conc(num_substances_total, iseg) - conc(num_substances_total, iseg - nosegl))
                    enddo
                    iseg = isegl + (num_layers - 1) * nosegl
                    c_m2(1, isegl) = c_m2(1, isegl) + conc(4 + istage, iseg)
                    c_m2(1 + istage, isegl) = c_m2(1 + istage, isegl) + conc(4 + istage, iseg)
                enddo

            else

                ! just cummulate substance 1 per layer

                c_m2(1, isegl) = conc(1, isegl) * conc(num_substances_total, isegl)
                do ilay = 2, num_layers
                    iseg = isegl + (ilay - 1) * nosegl
                    c_m2(1, isegl) = c_m2(1, isegl) + conc(1, iseg) * (conc(num_substances_total, iseg) - conc(num_substances_total, iseg - nosegl))
                enddo
                iseg = isegl + (num_layers - 1) * nosegl
                c_m2(1, isegl) = c_m2(1, isegl) + conc(1, iseg)

            endif
        enddo
        write(lunmm2) itime, c_m2

        return
    end subroutine
end module
