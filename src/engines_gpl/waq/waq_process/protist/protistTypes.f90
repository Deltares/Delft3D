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

!!  *********************************************************************
!!  *    Modules containing all the functions for PROTIST                *
!!  *********************************************************************

!!   contains the derived types and associated functions:
!!    - protist_allocate
!!              allocate derived types
!!    - protist_initialize
!!              initialize derived types
!!    - protist_deallocate
!!              deallocate derived types



!!

module protist_types
    use m_waq_precision

    implicit none

    ! Protist arrays
    type :: protist_array
        ! prey state variables
        real(kind = real_wp), dimension(:), allocatable :: preyC, preyChl, preyN, preyP, preySi
        ! other prey input parameters
        real(kind = real_wp), dimension(:), allocatable :: CcellPrey, rPrey, motPrey, PR
        ! food quantity
        real(kind = real_wp), dimension(:), allocatable :: preyFlag                       ! protection aginst small prey conc.
        real(kind = real_wp), dimension(:), allocatable :: nrPrey                         ! prey abundance
        real(kind = real_wp), dimension(:), allocatable :: smallerVel, largerVel          ! velocities
        real(kind = real_wp), dimension(:), allocatable :: encPrey                        ! prey encounter
        real(kind = real_wp), dimension(:), allocatable :: capturedPrey                   ! prey capture
        real(kind = real_wp), dimension(:), allocatable :: propPrey, ingNC, ingPC         ! preyN and preyP proprtion in diet
        ! ingestion of prey by predator fluxes
        real(kind = real_wp), dimension(:), allocatable :: dPreyC, dPreyChl, dPreyN, dPreyP, dPreySi
    end type


contains

    ! allocate arrays
    subroutine allocate_prot_array(prot_array, nrPrey)
        type(protist_array), intent(inout) :: prot_array
        integer(kind = int_wp), intent(in) :: nrPrey

        ! allocate statements
        allocate(prot_array%preyC(nrPrey))
        allocate(prot_array%preyChl(nrPrey))
        allocate(prot_array%preyN(nrPrey))
        allocate(prot_array%preyP(nrPrey))
        allocate(prot_array%preySi(nrPrey))
        allocate(prot_array%CcellPrey(nrPrey))
        allocate(prot_array%rPrey(nrPrey))
        allocate(prot_array%motPrey(nrPrey))
        allocate(prot_array%PR(nrPrey))

        ! allocation of food quantity and quality arrays
        allocate(prot_array%nrPrey(nrPrey))
        allocate(prot_array%preyFlag(nrPrey))
        allocate(prot_array%smallerVel(nrPrey))
        allocate(prot_array%largerVel(nrPrey))
        allocate(prot_array%encPrey(nrPrey))
        allocate(prot_array%capturedPrey(nrPrey))
        allocate(prot_array%propPrey(nrPrey))
        allocate(prot_array%ingNC(nrPrey))
        allocate(prot_array%ingPC(nrPrey))
        allocate(prot_array%dPreyC(nrPrey))
        allocate(prot_array%dPreyChl(nrPrey))
        allocate(prot_array%dPreyN(nrPrey))
        allocate(prot_array%dPreyP(nrPrey))
        allocate(prot_array%dPreySi(nrPrey))

    end subroutine allocate_prot_array


    ! initialize arrays
    subroutine initialize_prot_array(prot_array, nrPrey, process_space_real, ipnt, nrIndInp, nrSpec, nrSpecInp, iSpec, nrPreyInp)
        type(protist_array), intent(inout) :: prot_array
        integer(kind = int_wp), intent(in) :: nrPrey
        real(kind = real_wp) :: process_space_real(*)
        integer(kind = int_wp), intent(in) :: ipnt(:)
        integer(kind = int_wp), intent(in) :: nrIndInp, nrSpec, nrSpecInp, iSpec, nrPreyInp
        integer(kind = int_wp) :: iPrey         ! local prey number counter
        integer(kind = int_wp) :: prInc         ! local pray process_space_real number increment

        do iPrey = 1, nrPrey
            !prey specific input
            ! independentItems + all input items of all zoo species + first prey item + current PreyNumber * total nr of prey specific items
            prInc = nrIndInp + nrSpec * nrSpecInp + (iPrey - 1) * (nrPreyInp + nrSpec)

            prot_array%preyC(iPrey) = process_space_real(ipnt(prInc + 1)) + 1.0E-20 ! C-biomass (+1.0E-20: protection against division by 0) (gC m-3)
            prot_array%preyChl(iPrey) = process_space_real(ipnt(prInc + 2))           ! Chl-biomass                                            (gC m-3)
            prot_array%preyN(iPrey) = process_space_real(ipnt(prInc + 3))           ! N-biomass                                              (gN m-3)
            prot_array%preyP(iPrey) = process_space_real(ipnt(prInc + 4))           ! P-biomass                                              (gP m-3)
            prot_array%preySi(iPrey) = process_space_real(ipnt(prInc + 5))           ! Si-biomass                                             (gP m-3)
            prot_array%CcellPrey(iPrey) = process_space_real(ipnt(prInc + 6))           ! C content of protist cell                              (pgC cell-1)
            prot_array%rPrey(iPrey) = process_space_real(ipnt(prInc + 7))           ! radius of nutrient repleted protist cell               (um)
            prot_array%motPrey(iPrey) = process_space_real(ipnt(prInc + 8))           ! swimming velocity                                      (m s-1)
            prot_array%PR(iPrey) = process_space_real(ipnt(prInc + 8 + iSpec))    !      handling index of prey 1 by pred 1             (-)


            ! if loop to protect against small preys (-)
            if (prot_array%preyC(iPrey) >= 1.0E-5) then
                prot_array%preyFlag(iPrey) = 1.0
            else
                prot_array%preyFlag(iPrey) = 0.0
            end if

        end do
    end subroutine initialize_prot_array


    ! deallocate arrays
    subroutine deallocate_prot_array(prot_array)
        type(protist_array), intent(inout) :: prot_array

        ! deallocate statements
        deallocate(prot_array%preyC)
        deallocate(prot_array%preyChl)
        deallocate(prot_array%preyN)
        deallocate(prot_array%preyP)
        deallocate(prot_array%preySi)
        deallocate(prot_array%CcellPrey)
        deallocate(prot_array%rPrey)
        deallocate(prot_array%motPrey)
        deallocate(prot_array%PR)

        ! deallocation of food quantity and quality arrays
        deallocate(prot_array%nrPrey)
        deallocate(prot_array%preyFlag)
        deallocate(prot_array%smallerVel)
        deallocate(prot_array%largerVel)
        deallocate(prot_array%encPrey)
        deallocate(prot_array%capturedPrey)
        deallocate(prot_array%propPrey)
        deallocate(prot_array%ingNC)
        deallocate(prot_array%ingPC)
        deallocate(prot_array%dPreyC)
        deallocate(prot_array%dPreyChl)
        deallocate(prot_array%dPreyN)
        deallocate(prot_array%dPreyP)
        deallocate(prot_array%dPreySi)

    end subroutine deallocate_prot_array


end module protist_types
