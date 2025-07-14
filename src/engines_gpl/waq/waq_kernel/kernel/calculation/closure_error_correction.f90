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
module m_closure_error_correction
    use m_waq_precision
    use timers

    implicit none

    private
    public :: calculate_closure_error_correction, calculate_closure_error_correction_steady_state

contains


    !> Calculates closure error correction on masses
    !! The volume after rewind of hydrodynamics (volumn) generally
    !! does generally not correspond with the volume that is obtained
    !! with mass conserving transport in the last time step of the
    !! hydrodynamic file (voluml). This will give a jump in the the
    !! time series of concentrations after rewind since DELWAQ preserves
    !! mass. To avoid this jump, the mass can be adjusted according to
    !! the volume error made with the rewind of the dataset.
    subroutine calculate_closure_error_correction(amass, volumn, voluml, num_substances_transported, &
            num_substances_total, num_cells, file_unit_list)

        integer(kind = int_wp), intent(in) :: file_unit_list              !< Unit number of the monitroing file
        integer(kind = int_wp), intent(in) :: num_substances_transported  !< Number of transport substances
        integer(kind = int_wp), intent(in) :: num_substances_total        !< Total number of substances
        integer(kind = int_wp), intent(in) :: num_cells                   !< Number of computational volumes
        real(kind = real_wp), intent(inout) :: amass (num_substances_total, num_cells) !< Delwaq mass array to be updated
        real(kind = real_wp), intent(in) :: volumn(num_cells)              !< Volume after rewind
        real(kind = real_wp), intent(in) :: voluml(num_cells)              !< Last volume before rewind

        ! Local variables
        real(kind = dp) :: tovoll      !< total of the last volume array
        real(kind = dp) :: tovoln      !< total of the last new volume array
        real(kind = dp) :: clofac      !< closure correction factor
        integer(kind = int_wp) :: cell_i !< finite volume counter

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("calculate_closure_error_correction", ithandl)

        ! Say what you are doing
        write (file_unit_list, 1000)

        ! Loop accross the number of computational elements
        tovoll = 0.0d00
        tovoln = 0.0d00
        do cell_i = 1, num_cells
            ! Calculate closure error
            if (abs(voluml(cell_i)) > 1.0e-28) then
                clofac = volumn(cell_i) / voluml(cell_i)
            else
                clofac = 1.0
            endif
            tovoll = tovoll + voluml(cell_i)
            tovoln = tovoln + volumn(cell_i)
            ! Correct mass of transported substances
            amass(1:num_substances_transported, cell_i) = amass(1:num_substances_transported, cell_i) * clofac
        enddo
        ! Write statistics
        write (file_unit_list, 1010) tovoll
        write (file_unit_list, 1020) tovoln
        write (file_unit_list, 1030) tovoln / tovoll
        if (timon) call timstop (ithandl)

        ! Output formats
        1000 format ('Performing closure error correction')
        1010 format ('Total volume before rewind:', e24.13)
        1020 format ('Total volume after rewind :', e24.13)
        1030 format ('Total correction factor   :', e24.13)
    end subroutine calculate_closure_error_correction

    !> Makes a closure error correction for steady state computations
    subroutine calculate_closure_error_correction_steady_state(amass, volume, idt, num_cells)

        real(kind = real_wp), intent(inout) :: amass (*) !< Closure error correction
        real(kind = real_wp), intent(inout) :: volume(*) !< Volume
        integer(kind = int_wp), intent(in) :: idt       !< Time between amass and volume
        integer(kind = int_wp), intent(in) :: num_cells     !< Number of cells

        ! Local variables
        integer(kind = int_wp) :: i
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("calculate_closure_error_correction_steady_state", ithandl)

        do i = 1, num_cells
            amass(i) = (amass(i) - volume(i)) / idt
        end do

        if (timon) call timstop (ithandl)
    end subroutine calculate_closure_error_correction_steady_state

end module m_closure_error_correction
