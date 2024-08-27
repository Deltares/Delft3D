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
module m_zlayer
    use m_waq_precision
    use m_string_utils
    use timers

    implicit none

    private
    public :: zlayer, zflows

contains


    subroutine zlayer(nosegw, num_cells, num_substances_transported, num_substances_total, num_layers, &
            volume, noq12, num_exchanges, area, num_constants, &
            coname, cons, num_spatial_parameters, paname, param, &
            num_spatial_time_fuctions, sfname, segfun, conc, mass, &
            iknmrk, iknmkv, ifrmto)

        !>      Sets feature of dry cells below bed to zero, determines position bed layer
        !>
        !>      - Only works if the constant 'Z_THRESH' exists, otherwise it only:
        !>        - copies the constant property array to the variable array
        !>        - minimizes the area array at 1.0 m2
        !>      - Only works for horizontal cells with the first layer active
        !>        - so it does not override a column specified as inactive by the user
        !>      - Tests the value of the volume in an active column from the bed
        !>        - if too low:
        !>          - it sets the property to 'inactive'
        !>          - it sets the second property (surface,middle,bed) to zero
        !>          - it modifies the second property of the layer above accordingly
        !>          - it sets the third property (saved value of the first property) to 'inactive'
        !>      - The first property of the top layer is never modified so 'active' remains 'active'
        !>      - The Z_THRESH value is the thickness of the cell to decide for too low or not
        !>      - This is a modification of the property array at start of simulation.\n
        !>      - A tricky point is that for z-layers the from- to pointer is not
        !>        adapted any more. This is because EDFs TELEMAC may produce flows below the
        !>        bed, that should be taken into account for mass conservation.
        !>      NB. This routine also initialises the variable property array. It does so for the
        !>      whole array, so inclusive of any bed cells.

        use waq_attribute_utils, only: set_feature
        use m_extract_waq_attribute

        integer(kind = int_wp), intent(in) :: nosegw               !< number of computational volumes water
        integer(kind = int_wp), intent(in) :: num_cells                !< number of computational volumes total
        integer(kind = int_wp), intent(in) :: num_substances_transported          !< number of transported substance
        integer(kind = int_wp), intent(in) :: num_substances_total                !< total number of substance
        integer(kind = int_wp), intent(in) :: num_layers                !< number of layers
        real(kind = real_wp), intent(in) :: volume(num_cells)       !< volumes at start of time step
        integer(kind = int_wp), intent(in) :: noq12                !< number of horizontal exchanges
        integer(kind = int_wp), intent(in) :: num_exchanges                  !< total number of exchanges
        real(kind = real_wp), intent(inout) :: area  (noq12)       !< areas at start of time step
        integer(kind = int_wp), intent(in) :: num_constants               !< number of constants
        character(20), intent(in) :: coname(num_constants)       !< names of the constants
        real(kind = real_wp), intent(in) :: cons  (num_constants)       !< values of the constants
        integer(kind = int_wp), intent(in) :: num_spatial_parameters                 !< number of parameters
        character(20), intent(in) :: paname(num_spatial_parameters)       !< names of the parameters
        real(kind = real_wp), intent(in) :: param (num_spatial_parameters, num_cells) !< values of the parametrs
        integer(kind = int_wp), intent(in) :: num_spatial_time_fuctions               !< number of segment functions
        character(20), intent(in) :: sfname(num_spatial_time_fuctions)       !< names of the segment functions
        real(kind = real_wp), intent(in) :: segfun(num_cells, num_spatial_time_fuctions) !< values of the constants
        real(kind = real_wp), intent(inout) :: conc  (num_substances_total, num_cells) !< model concentrations
        real(kind = real_wp), intent(inout) :: mass  (num_substances_total, num_cells) !< model masses
        integer(kind = int_wp), intent(inout) :: iknmrk(num_cells)       !< constant feature array
        integer(kind = int_wp), intent(out) :: iknmkv(num_cells)       !< time varying feature array
        integer(kind = int_wp), intent(inout) :: ifrmto(4, num_exchanges)       !< exchange pointer array

        !     Locals

        integer(kind = int_wp) :: idryfld         ! help variable to find dry_tresh constant
        integer(kind = int_wp) :: isurf           ! index to find horizontal surface area values
        real(kind = real_wp) :: threshold       ! drying and flooding value
        real(kind = real_wp) :: minarea         ! minimum exhange area of a horizontal exchange
        integer(kind = int_wp) :: nosegl          ! number of computational volumes per layer
        integer(kind = int_wp) :: cell_i            ! loop variable volumes
        integer(kind = int_wp) :: iq              ! loop variable exchanges
        integer(kind = int_wp) :: i, j            ! general loop variables
        integer(kind = int_wp) :: ivol            ! this computational volumes
        integer(kind = int_wp) :: isub            ! loop variable substances
        integer(kind = int_wp) :: ilay            ! loop variable layers
        integer(kind = int_wp) :: ikm             ! feature

        integer(kind = int_wp) :: ithandl = 0

        idryfld = index_in_array('Z_THRESH  ', coname)
        if (idryfld <= 0) then                                       ! constant not found
            iknmkv = iknmrk                                               ! set variable property to

            minarea = 1.00E-04                                            ! default value of 1.00E-04 m2 = 1 cm2
            idryfld = index_in_array('MIN_AREA', coname)
            if (idryfld > 0) minarea = cons(idryfld)                 ! or the given value
            area = max(area, minarea)                                   ! set minimum area
            return                                                        ! and return
        end if
        threshold = cons(idryfld)                                        ! apply the given value
        ! and proceed with z-layer
        if (timon) call timstrt ("zlayer", ithandl)                  ! correction
        nosegl = nosegw / num_layers
        isurf = index_in_array('SURF      ', paname)

        !        SURF is a parameter

        if (isurf > 0) then
            do cell_i = 1, nosegl
                call extract_waq_attribute(1, iknmrk(cell_i), ikm)
                if (ikm == 0) cycle                                    ! whole collumn is inactive
                do ilay = num_layers, 1, -1                                     ! from bottom to top
                    ivol = cell_i + (ilay - 1) * nosegl
                    if (volume(ivol) < param(isurf, ivol) * threshold) then
                        if (ilay > 1) then
                            iknmrk(ivol) = 0                                  ! inactive cell below the bed
                            call extract_waq_attribute(2, iknmrk(ivol - nosegl), ikm)         ! get second one of cell above
                            select case (ikm)
                            case (1)                                     ! the cell above is surface cell
                                call set_feature(2, iknmrk(ivol - nosegl), 0)     ! now it also has a bed
                            case (2)                                     ! the cell on top is middle cell
                                call set_feature(2, iknmrk(ivol - nosegl), 3)     ! now it is the bed
                            end select
                            do isub = num_substances_transported + 1, num_substances_total
                                conc(isub, ivol - nosegl) = conc(isub, ivol - nosegl) + conc(isub, ivol)
                                mass(isub, ivol - nosegl) = mass(isub, ivol - nosegl) + mass(isub, ivol)
                                conc(isub, ivol) = 0.0
                                mass(isub, ivol) = 0.0
                            end do
                        end if
                    else
                        exit
                    end if
                end do
            end do
        else
            isurf = index_in_array('SURF      ', sfname)

            !        SURF is a spatial time function (often with 1D models)

            if (isurf > 0) then
                do cell_i = 1, nosegl
                    call extract_waq_attribute(1, iknmrk(cell_i), ikm)
                    if (ikm == 0) cycle
                    do ilay = num_layers, 1, -1                                  ! from bottom to top
                        ivol = cell_i + (ilay - 1) * nosegl
                        if (volume(ivol) < segfun(ivol, isurf) * threshold) then
                            if (ilay > 1) then
                                iknmrk(ivol) = 0                               ! inactive cell below the bed
                                call extract_waq_attribute(2, iknmrk(ivol - nosegl), ikm)
                                select case (ikm)
                                case (1)                                  ! the cell on top is surface cell
                                    call set_feature(2, iknmrk(ivol - nosegl), 0)  ! now it also has a bed
                                case (2)                                  ! the cell on top is middle cell
                                    call set_feature(2, iknmrk(ivol - nosegl), 3)  ! now it is the bed
                                end select
                                do isub = num_substances_transported + 1, num_substances_total
                                    conc(isub, ivol - nosegl) = conc(isub, ivol - nosegl) + conc(isub, ivol)
                                    mass(isub, ivol - nosegl) = mass(isub, ivol - nosegl) + mass(isub, ivol)
                                    conc(isub, ivol) = 0.0
                                    mass(isub, ivol) = 0.0
                                end do
                            end if
                        else
                            exit
                        end if
                    end do
                end do
            else

                !        SURF is not found, so the default value of 1 m2 is used

                do cell_i = 1, nosegl
                    call extract_waq_attribute(1, iknmrk(cell_i), ikm)
                    if (ikm == 0) cycle
                    do ilay = num_layers, 1, -1                               ! from bottom to top
                        ivol = cell_i + (ilay - 1) * nosegl
                        if (volume(ivol) < threshold) then
                            if (ilay > 1) then
                                iknmrk(ivol) = 0                            ! inactive cell below the bed
                                call extract_waq_attribute(2, iknmrk(ivol - nosegl), ikm)
                                select case (ikm)
                                case (1)                                  ! the cell on top is surface cell
                                    call set_feature(2, iknmrk(ivol - nosegl), 0)  ! now it also has a bed
                                case (2)                                  ! the cell on top is middle cell
                                    call set_feature(2, iknmrk(ivol - nosegl), 3)  ! now it is the bed
                                end select
                                do isub = num_substances_transported + 1, num_substances_total
                                    conc(isub, ivol - nosegl) = conc(isub, ivol - nosegl) + conc(isub, ivol)
                                    mass(isub, ivol - nosegl) = mass(isub, ivol - nosegl) + mass(isub, ivol)
                                    conc(isub, ivol) = 0.0
                                    mass(isub, ivol) = 0.0
                                end do
                            end if
                        else
                            exit
                        end if
                    end do
                end do
            end if
        end if

        iknmkv = iknmrk

        minarea = 1.00E-04                                            ! default value of 1.00E-04 m2 = 1 cm2
        idryfld = index_in_array('MIN_AREA', coname)
        if (idryfld > 0) minarea = cons(idryfld)                 ! or the given value
        area = max(area, minarea)                                   ! set minimum area

        !          update the vertical exchange pointer
        !          this needs more sophisticated approach when atmosphere and bed
        !          have been attached as open boundary conditions

        do iq = 1, num_exchanges
            do i = 1, 2
                j = ifrmto(i, iq)
                if (j > 0) then
                    if (.not. btest(iknmkv(j), 0)) ifrmto(i, iq) = 0
                end if
            end do
        end do
        if (timon) call timstop (ithandl)
    end subroutine zlayer

    !> Adjusts the flow pointer to cross layers where needed for a Z-layer model
    !! If the bed crosses layer interfaces then the average bed level of a node
    !! may be in a higher layer than the actual bed level for an exchange.\n
    !! Some flow models then nevertheless give a flow to the level of the node
    !! below the bed and also create a vertical flux from below the bed to above.
    !! This routine adjusts the horizontal flow pointer to point to 1 or 2 layers
    !! higher. In the above zlayer routine the vertical flow was already masked
    !! out.
    subroutine zflows(num_exchanges, noq12, num_layers, num_constants, coname, &
            flow, ifrmto)

        integer(kind = int_wp), intent(in) :: num_exchanges            !< Number of exchanges between cells
        integer(kind = int_wp), intent(in) :: noq12          !< Number of horizontal exchanges
        integer(kind = int_wp), intent(in) :: num_layers          !< Number of Z-layers
        integer(kind = int_wp), intent(in) :: num_constants         !< Number of constants
        character(20), intent(in) :: coname(num_constants) !< Names of the constants
        real(kind = real_wp), intent(in) :: flow(num_exchanges)      !< Flows between cells
        integer(kind = int_wp), intent(inout) :: ifrmto(4, num_exchanges) !< Exchange index array

        ! Local variables
        integer(kind = int_wp) :: iq    !< Loop variable exchanges
        integer(kind = int_wp) :: ifrom !< From cell number
        integer(kind = int_wp) :: ito   !< To cell number
        integer(kind = int_wp) :: iql   !< Auxiliary variable to find lowest active cell
        integer(kind = int_wp) :: noqhl !< Number of horizontal exchanges per layer

        integer(kind = int_wp) :: ithandl = 0

        iq = index_in_array('Z_THRESH  ', coname)
        if (iq <= 0) return

        if (timon) call timstrt ("zflows", ithandl)

        noqhl = noq12 / num_layers

        do iq = 1, noq12
            ifrom = ifrmto(1, iq)
            ito = ifrmto(2, iq)
            if (ifrom /= 0 .and. ito /= 0) cycle  ! both below the bed
            if (abs(flow(iq)) < 1.0e-4) cycle      ! flow is almost zero
            if (ifrom == 0) then
                iql = iq - noqhl                         ! look at the corresponding
                do while (iql > 0)                  ! exchange one layer higher
                    if (ifrmto(1, iql) > 0) then      ! if that is real
                        ifrmto(1, iq) = ifrmto(1, iql)       ! take that cell for this flux also
                        exit
                    end if
                    iql = iql - noqhl
                end do
            end if
            if (ito   == 0) then
                iql = iq - noqhl
                do while (iql > 0)
                    if (ifrmto(2, iql) > 0) then
                        ifrmto(2, iq) = ifrmto(2, iql)
                        exit
                    end if
                    iql = iql - noqhl
                end do
            end if
        end do
        if (timon) call timstop (ithandl)
    end subroutine zflows

end module m_zlayer
