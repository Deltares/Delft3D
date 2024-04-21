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

    implicit none

contains


    subroutine zlayer (nosegw, noseg, nosys, notot, nolay, &
            volume, noq12, noq, area, nocons, &
            coname, cons, nopa, paname, param, &
            nosfun, sfname, segfun, conc, mass, &
            iknmrk, iknmkv, ifrmto)

        !     Deltares Software Centre

        !>\File
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

        !     Created             : September 2010 by Leo Postma
        !     Modified            : February  2011 by Leo Postma determine position of bedlayer
        !                           August    2011 by Leo Postma more limited functionality
        !                           February  2013 by Leo Postma expanded functionality again

        !     Files               : none

        !     Routines            : zoek  - to search the DRY_TRESH constant
        !                                     and SURF parameter/segfunction
        !                           set_feature  - to set features
        !                           evaluate_waq_attribute  - to get features

        use waq_attribute_utils, only : set_feature
        use m_evaluate_waq_attribute
        use timers
        implicit none

        !     Parameters          :

        !     kind           function         name                   description

        integer(kind = int_wp), intent(in) :: nosegw               !< number of computational volumes water
        integer(kind = int_wp), intent(in) :: noseg                !< number of computational volumes total
        integer(kind = int_wp), intent(in) :: nosys                !< number of transported substance
        integer(kind = int_wp), intent(in) :: notot                !< total number of substance
        integer(kind = int_wp), intent(in) :: nolay                !< number of layers
        real(kind = real_wp), intent(in) :: volume(noseg)       !< volumes at start of time step
        integer(kind = int_wp), intent(in) :: noq12                !< number of horizontal exchanges
        integer(kind = int_wp), intent(in) :: noq                  !< total number of exchanges
        real(kind = real_wp), intent(inout) :: area  (noq12)       !< areas at start of time step
        integer(kind = int_wp), intent(in) :: nocons               !< number of constants
        character(20), intent(in) :: coname(nocons)       !< names of the constants
        real(kind = real_wp), intent(in) :: cons  (nocons)       !< values of the constants
        integer(kind = int_wp), intent(in) :: nopa                 !< number of parameters
        character(20), intent(in) :: paname(nopa)       !< names of the parameters
        real(kind = real_wp), intent(in) :: param (nopa, noseg) !< values of the parametrs
        integer(kind = int_wp), intent(in) :: nosfun               !< number of segment functions
        character(20), intent(in) :: sfname(nosfun)       !< names of the segment functions
        real(kind = real_wp), intent(in) :: segfun(noseg, nosfun) !< values of the constants
        real(kind = real_wp), intent(inout) :: conc  (notot, noseg) !< model concentrations
        real(kind = real_wp), intent(inout) :: mass  (notot, noseg) !< model masses
        integer(kind = int_wp), intent(inout) :: iknmrk(noseg)       !< constant feature array
        integer(kind = int_wp), intent(out) :: iknmkv(noseg)       !< time varying feature array
        integer(kind = int_wp), intent(inout) :: ifrmto(4, noq)       !< exchange pointer array

        !     Locals

        integer(kind = int_wp) :: idryfld         ! help variable to find dry_tresh constant
        integer(kind = int_wp) :: isurf           ! index to find horizontal surface area values
        real(kind = real_wp) :: threshold       ! drying and flooding value
        real(kind = real_wp) :: minarea         ! minimum exhange area of a horizontal exchange
        integer(kind = int_wp) :: nosegl          ! number of computational volumes per layer
        integer(kind = int_wp) :: iseg            ! loop variable volumes
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
        endif
        threshold = cons(idryfld)                                        ! apply the given value
        ! and proceed with z-layer
        if (timon) call timstrt ("zlayer", ithandl)                  ! correction
        nosegl = nosegw / nolay
        isurf = index_in_array('SURF      ', paname)

        !        SURF is a parameter

        if (isurf > 0) then
            do iseg = 1, nosegl
                call evaluate_waq_attribute(1, iknmrk(iseg), ikm)
                if (ikm == 0) cycle                                    ! whole collumn is inactive
                do ilay = nolay, 1, -1                                     ! from bottom to top
                    ivol = iseg + (ilay - 1) * nosegl
                    if (volume(ivol) < param(isurf, ivol) * threshold) then
                        if (ilay > 1) then
                            iknmrk(ivol) = 0                                  ! inactive cell below the bed
                            call evaluate_waq_attribute(2, iknmrk(ivol - nosegl), ikm)         ! get second one of cell above
                            select case (ikm)
                            case (1)                                     ! the cell above is surface cell
                                call set_feature(2, iknmrk(ivol - nosegl), 0)     ! now it also has a bed
                            case (2)                                     ! the cell on top is middle cell
                                call set_feature(2, iknmrk(ivol - nosegl), 3)     ! now it is the bed
                            end select
                            do isub = nosys + 1, notot
                                conc(isub, ivol - nosegl) = conc(isub, ivol - nosegl) + conc(isub, ivol)
                                mass(isub, ivol - nosegl) = mass(isub, ivol - nosegl) + mass(isub, ivol)
                                conc(isub, ivol) = 0.0
                                mass(isub, ivol) = 0.0
                            enddo
                        endif
                    else
                        exit
                    endif
                enddo
            enddo
        else
            isurf = index_in_array('SURF      ', sfname)

            !        SURF is a spatial time function (often with 1D models)

            if (isurf > 0) then
                do iseg = 1, nosegl
                    call evaluate_waq_attribute(1, iknmrk(iseg), ikm)
                    if (ikm == 0) cycle
                    do ilay = nolay, 1, -1                                  ! from bottom to top
                        ivol = iseg + (ilay - 1) * nosegl
                        if (volume(ivol) < segfun(ivol, isurf) * threshold) then
                            if (ilay > 1) then
                                iknmrk(ivol) = 0                               ! inactive cell below the bed
                                call evaluate_waq_attribute(2, iknmrk(ivol - nosegl), ikm)
                                select case (ikm)
                                case (1)                                  ! the cell on top is surface cell
                                    call set_feature(2, iknmrk(ivol - nosegl), 0)  ! now it also has a bed
                                case (2)                                  ! the cell on top is middle cell
                                    call set_feature(2, iknmrk(ivol - nosegl), 3)  ! now it is the bed
                                end select
                                do isub = nosys + 1, notot
                                    conc(isub, ivol - nosegl) = conc(isub, ivol - nosegl) + conc(isub, ivol)
                                    mass(isub, ivol - nosegl) = mass(isub, ivol - nosegl) + mass(isub, ivol)
                                    conc(isub, ivol) = 0.0
                                    mass(isub, ivol) = 0.0
                                enddo
                            endif
                        else
                            exit
                        endif
                    enddo
                enddo
            else

                !        SURF is not found, so the default value of 1 m2 is used

                do iseg = 1, nosegl
                    call evaluate_waq_attribute(1, iknmrk(iseg), ikm)
                    if (ikm == 0) cycle
                    do ilay = nolay, 1, -1                               ! from bottom to top
                        ivol = iseg + (ilay - 1) * nosegl
                        if (volume(ivol) < threshold) then
                            if (ilay > 1) then
                                iknmrk(ivol) = 0                            ! inactive cell below the bed
                                call evaluate_waq_attribute(2, iknmrk(ivol - nosegl), ikm)
                                select case (ikm)
                                case (1)                                  ! the cell on top is surface cell
                                    call set_feature(2, iknmrk(ivol - nosegl), 0)  ! now it also has a bed
                                case (2)                                  ! the cell on top is middle cell
                                    call set_feature(2, iknmrk(ivol - nosegl), 3)  ! now it is the bed
                                end select
                                do isub = nosys + 1, notot
                                    conc(isub, ivol - nosegl) = conc(isub, ivol - nosegl) + conc(isub, ivol)
                                    mass(isub, ivol - nosegl) = mass(isub, ivol - nosegl) + mass(isub, ivol)
                                    conc(isub, ivol) = 0.0
                                    mass(isub, ivol) = 0.0
                                enddo
                            endif
                        else
                            exit
                        endif
                    enddo
                enddo
            endif
        endif

        iknmkv = iknmrk

        minarea = 1.00E-04                                            ! default value of 1.00E-04 m2 = 1 cm2
        idryfld = index_in_array('MIN_AREA', coname)
        if (idryfld > 0) minarea = cons(idryfld)                 ! or the given value
        area = max(area, minarea)                                   ! set minimum area

        !          update the vertical exchange pointer
        !          this needs more sophisticated approach when atmosphere and bed
        !          have been attached as open boundary conditions

        do iq = 1, noq
            do i = 1, 2
                j = ifrmto(i, iq)
                if (j > 0) then
                    if (.not. btest(iknmkv(j), 0)) ifrmto(i, iq) = 0
                endif
            enddo
        enddo

        if (timon) call timstop (ithandl)

        return
    end

    subroutine zflows (noq, noq12, nolay, nocons, coname, &
            flow, ifrmto)

        !     Deltares Software Centre

        !>\File
        !>      Adjusts the flow pointer to cross layers where needed for a Z-layer model
        !>
        !>      If the bed crosses layer interfaces then the average bed level of a node
        !>      may be in a higher layer than the actual bed level for an exchange.\n
        !>      Some flow models then nevertheless give a flow to the level of the node
        !>      below the bed and also create a vertical flux from below the bed to above.
        !>      This routine adjusts the horizontal flow pointer to point to 1 or 2 layers
        !>      higher. In the above zlayer routine the vertical flow was already masked
        !>      out.

        !     Created             : February  2013 by Leo Postma for EDF

        !     Files               : none

        !     Routines            : zoek  - to search the DRY_TRESH constant

        use timers
        implicit none

        !     Parameters          :

        !     kind           function         name                   description

        integer(kind = int_wp), intent(in) :: noq                  !< number of exchanges between cells
        integer(kind = int_wp), intent(in) :: noq12                !< number of horizontal exchanges
        integer(kind = int_wp), intent(in) :: nolay                !< number of Z-layers
        integer(kind = int_wp), intent(in) :: nocons               !< number of constants
        character(20), intent(in) :: coname(nocons)       !< names of the constants
        real(kind = real_wp), intent(in) :: flow  (noq)        !< flows between cells
        integer(kind = int_wp), intent(inout) :: ifrmto(4, noq)       !< exchange pointer array

        !     Locals

        integer(kind = int_wp) :: iq              ! loop variable exchanges
        integer(kind = int_wp) :: ifrom, ito      ! from and to cell number
        integer(kind = int_wp) :: iql             ! help variable to find lowest active cell
        integer(kind = int_wp) :: noqhl           ! number of horizontal exchanges per layer

        integer(kind = int_wp) :: ithandl = 0

        iq = index_in_array('Z_THRESH  ', coname)
        if (iq <= 0) return

        if (timon) call timstrt ("zflows", ithandl)

        noqhl = noq12 / nolay

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
                    endif
                    iql = iql - noqhl
                enddo
            endif
            if (ito   == 0) then
                iql = iq - noqhl
                do while (iql > 0)
                    if (ifrmto(2, iql) > 0) then
                        ifrmto(2, iq) = ifrmto(2, iql)
                        exit
                    endif
                    iql = iql - noqhl
                enddo
            endif
        enddo

        if (timon) call timstop (ithandl)

        return
    end

end module m_zlayer
