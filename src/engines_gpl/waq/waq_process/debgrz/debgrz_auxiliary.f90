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

module m_debgrz_auxiliary
    use m_waq_precision

    implicit none

    private
    public :: debgrz_auxiliary

    type :: debgrz_auxiliary
        integer(kind=int_wp), dimension(:), allocatable :: benfood  !< Benthic foods (true/false)

        real(kind=real_wp), dimension(:), allocatable :: ccfood   !< Stoichiometry ratio of carbon to food unit
                                                                  !< (often the food unit is expressed in carbon).
        real(kind=real_wp), dimension(:), allocatable :: cfood    !< Carbon foods
        real(kind=real_wp), dimension(:), allocatable :: chlcfood !< Chlorophyll foods
        real(kind=real_wp), dimension(:), allocatable :: ncfood   !< Nitrogen foods
        real(kind=real_wp), dimension(:), allocatable :: pcfood   !< Phosphorus foods
        real(kind=real_wp), dimension(:), allocatable :: sicfood  !< Silicon foods
        real(kind=real_wp), dimension(:), allocatable :: dfil     !< Daily filtration rate for each food type [gC/ind/d]
        contains
            procedure allocate_food_arrays

    end type debgrz_auxiliary

    contains

    subroutine allocate_food_arrays(this, food_count)
        class(debgrz_auxiliary) :: this !< The auxiliary_variables instance
        integer(kind=int_wp), intent(in) :: food_count

        allocate(this%benfood(food_count))
        allocate(this%ccfood(food_count))
        allocate(this%cfood(food_count))
        allocate(this%chlcfood(food_count))
        allocate(this%ncfood(food_count))
        allocate(this%pcfood(food_count))
        allocate(this%sicfood(food_count))
        allocate(this%dfil(food_count))

    end subroutine allocate_food_arrays
end module m_debgrz_auxiliary
