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
module m_dlwq65
    use m_waq_precision

    implicit none

contains


    !> Makes a closure error correction
    !! for steady state computations
    subroutine dlwq65(amass, volume, idt, noseg)

        use timers

        real(kind = real_wp),   intent(inout) :: amass (*) !< Closure error correction
        real(kind = real_wp),   intent(inout) :: volume(*) !< Volume
        integer(kind = int_wp), intent(in   ) :: idt       !< Time between amass and volume
        integer(kind = int_wp), intent(in   ) :: noseg     !< Number of cells
        
        ! Local variables
        integer(kind = int_wp) :: i
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwq65", ithandl)

        do i = 1, noseg
            amass(i) = (amass(i) - volume(i)) / idt
        end do

        if (timon) call timstop (ithandl)
    end subroutine dlwq65
end module m_dlwq65
