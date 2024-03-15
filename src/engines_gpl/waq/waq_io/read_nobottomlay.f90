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
module m_read_nobottomlay
    use m_waq_precision
    use m_error_status

    implicit none

contains


    subroutine read_nobottomlay(GridPs, status)

        !       Deltares Software Centre

        !>\file
        !>           Reads the number of bed layers per sediment column
        !>
        !>           Several options exist:
        !>           - DEFAULT followed by the number of layers to be applied for all columns
        !>           - INPUTGRID followed by:
        !>             - an existig grid name that contains the bottom columns map
        !>             - for each cell of this grid, an integer giving the number of layers
        !>           - ALL followed by an integer for each cell of the BOTTOMGRID
        !>           Using the INPUTGRID feature specifies the number of layers for anonther
        !>           grid than the BOTTOMGRID. This grid then is expanded to the BOTTOMGRID.

        !     Created            : Somewhere 2003 by Jan van Beek as layerd bed special
        !     Modified           : May       2011 by Leo Postma   merge with standard version

        !     global declarations

        use dlwqgrid_mod
        use rd_token         !   for the reading of tokens
        use timers       !   performance timers

        implicit none

        !     declaration of arguments

        type(GridPointerColl), intent(inout) :: GridPs     !< collection off all grid definitions
        type(error_status), intent(inout) :: status !< current error status

        !     local declarations

        type(GridPointer) :: aGrid                ! a single grid
        integer(kind = int_wp) :: itoken                ! integer token from input
        integer(kind = int_wp) :: idummy                ! dummy which content is not used
        real(kind = real_wp) :: adummy                ! dummy which content is not used
        character(len = 255) :: ctoken               ! character token from input
        character :: cdummy               ! dummy which content is not used
        integer(kind = int_wp) :: i_base_grid           ! index base grid in collection
        integer(kind = int_wp) :: i_bottom_grid         ! index bottom grid in collection
        integer(kind = int_wp) :: iseg                  ! segment index
        integer(kind = int_wp) :: iseg2                 ! segment index
        integer(kind = int_wp) :: input_grid            ! index input grid in collection
        integer(kind = int_wp) :: itype                 ! type of input to be needded
        integer(kind = int_wp) :: ierr2                 ! local error indication
        integer(kind = int_wp) :: iref                  ! index reference grid in collection
        integer(kind = int_wp) :: noseg_lay             ! number of segments per layer
        integer(kind = int_wp) :: noseg_input           ! number of segments in input
        integer(kind = int_wp) :: noseg_grid            ! number of segments in all layers
        integer(kind = int_wp) :: nolay                 ! number of layers
        integer(kind = int_wp) :: max_nolay             ! max number of layers in space_var_nolay
        integer(kind = int_wp) :: ilay                  ! index layers
        integer(kind = int_wp), pointer :: new_pointer(:)        ! new grid pointer on expanded bottom grid
        integer(kind = int_wp), allocatable :: bottom_matrix(:, :)    ! new grid on expanded bottom grid
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_nobottomlay", ithndl)

        !     check bottom grid

        i_base_grid = GridPs%base_grid
        i_bottom_grid = GridPs%bottom_grid

        if (i_bottom_grid == 0) then

            !        bottom grid not defined yet, add default bottom grid equals base grid 2d

            aGrid%name = 'Bottom grid'
            aGrid%noseg = 0
            aGrid%noseg_lay = GridPs%Pointers(i_base_grid)%noseg_lay
            aGrid%iref = i_base_grid
            aGrid%name_ref = GridPs%Pointers(i_base_grid)%name
            aGrid%itype = BottomGrid
            aGrid%space_var_nolay = .FALSE.
            aGrid%nolay = 0
            aGrid%nolay_var => NULL()
            Allocate (aGrid%iarray(GridPs%Pointers(i_base_grid)%noseg))
            do iseg = 1, GridPs%Pointers(i_base_grid)%noseg_lay
                aGrid%iarray(iseg) = iseg
            enddo
            i_bottom_grid = GridPointerCollAdd(GridPs, aGrid)
            GridPs%bottom_grid = i_bottom_grid

        endif

        !     read input

        input_grid = i_bottom_grid
        do

            if (gettoken(ctoken, ierr2) > 0) goto 1000
            select case (ctoken)

            case ('DEFAULT')
                if (gettoken(nolay, ierr2) > 0) goto 1000
                GridPs%Pointers(input_grid)%nolay = nolay
                write (lunut, 2050) nolay
                GridPs%Pointers(input_grid)%noseg = nolay * GridPs%Pointers(input_grid)%noseg_lay
                exit

            case ('INPUTGRID')
                if (gettoken(ctoken, ierr2) > 0) goto 1000
                write (lunut, 2010) trim(ctoken)
                input_grid = GridPointerCollFind(GridPs, ctoken)
                if (input_grid == 0) then
                    write (lunut, 2020)                       ! ERROR, input grid not defined
                    goto 1000
                endif
                noseg_input = GridPs%Pointers(input_grid)%noseg_lay
                GridPs%Pointers(input_grid)%space_var_nolay = .TRUE.
                allocate(GridPs%Pointers(input_grid)%nolay_var(noseg_input))
                noseg_grid = 0
                do iseg = 1, noseg_input
                    if (gettoken(nolay, ierr2) > 0) goto 1000
                    GridPs%Pointers(input_grid)%nolay_var(iseg) = nolay
                    noseg_grid = noseg_grid + nolay
                enddo
                GridPs%Pointers(input_grid)%noseg = noseg_grid
                exit

            case ('ALL')
                write (lunut, 2060)
                noseg_input = GridPs%Pointers(input_grid)%noseg_lay
                GridPs%Pointers(input_grid)%space_var_nolay = .TRUE.
                allocate(GridPs%Pointers(input_grid)%nolay_var(noseg_input))
                noseg_grid = 0
                do iseg = 1, noseg_input
                    if (gettoken(nolay, ierr2) > 0) goto 1000
                    GridPs%Pointers(input_grid)%nolay_var(iseg) = nolay
                    noseg_grid = noseg_grid + nolay
                enddo
                GridPs%Pointers(input_grid)%noseg = noseg_grid
                exit

            case default
                write (lunut, 2030) trim(ctoken)             ! ERROR, token not recognised
                goto 1000

            end select

        enddo

        !     if input grid .ne. bottom grid then expand to bottom grid (could be over a multiple reference?)

        if (input_grid /= i_bottom_grid) then
            max_nolay = maxval(GridPs%Pointers(input_grid)%nolay_var)
            do ! loop till the bottom grid is reached
                iref = GridPs%Pointers(input_grid)%iref
                if(iref <= 1) then
                    write (lunut, 2040)                          ! ERROR, input grid has no refrence to the bottom grid
                    goto 1000
                endif
                noseg_lay = GridPs%Pointers(iref)%noseg_lay
                GridPs%Pointers(iref)%space_var_nolay = .TRUE.
                allocate(GridPs%Pointers(iref)%nolay_var(noseg_lay))
                GridPs%Pointers(iref)%nolay_var = GridPs%Pointers(iref)%nolay
                noseg_grid = 0
                do iseg = 1, noseg_lay
                    itype = GridPs%Pointers(input_grid)%iarray(iseg)
                    if (itype > 0) then
                        GridPs%Pointers(iref)%nolay_var(iseg) = GridPs%Pointers(input_grid)%nolay_var(itype)
                        noseg_grid = noseg_grid + GridPs%Pointers(input_grid)%nolay_var(itype)
                    endif
                enddo
                GridPs%Pointers(iref)%noseg = noseg_grid

                ! expand pointers over the layers

                allocate(bottom_matrix(GridPs%Pointers(input_grid)%noseg_lay, max_nolay))
                bottom_matrix = 0
                iseg2 = 0
                do ilay = 1, max_nolay
                    do iseg = 1, GridPs%Pointers(input_grid)%noseg_lay
                        if (GridPs%Pointers(input_grid)%nolay_var(iseg) >= ilay) then
                            iseg2 = iseg2 + 1
                            bottom_matrix(iseg, ilay) = iseg2
                        endif
                    enddo
                enddo

                allocate(new_pointer(noseg_grid))
                iseg2 = 0
                do ilay = 1, max_nolay
                    do iseg = 1, noseg_lay
                        itype = GridPs%Pointers(input_grid)%iarray(iseg)
                        if (itype > 0) then
                            if (GridPs%Pointers(input_grid)%nolay_var(itype) >= ilay) then
                                iseg2 = iseg2 + 1
                                new_pointer(iseg2) = bottom_matrix(itype, ilay)
                            endif
                        endif
                    enddo
                enddo
                deallocate(GridPs%Pointers(input_grid)%iarray)
                GridPs%Pointers(input_grid)%iarray => new_pointer
                nullify(new_pointer)
                deallocate(bottom_matrix)

                ! exit if bottom grid else go to next reference

                input_grid = iref
                if (input_grid == i_bottom_grid) exit
            enddo
        endif

        if (timon) call timstop(ithndl)
        return

        1000 continue
        write(lunut, 2000)
        call status%increase_error_count()
        if (timon) call timstop(ithndl)
        return

        2000 format (/' ERROR, reading NOBOTTOMLAY information.')
        2010 format (/' Space varying number of bottom layers:', &
                /' Defined on grid: ', A)
        2020 format (/' ERROR, input grid not defined.')
        2030 format (/' ERROR, unrecognized token: ', A)
        2040 format (/' ERROR, input grid has no refrence to the bottom grid')
        2050 format (/' Default number of bottom layers:', I10)
        2060 format (/' Space varying number of bottom layers defined on bottom grid')

    end

end module m_read_nobottomlay
