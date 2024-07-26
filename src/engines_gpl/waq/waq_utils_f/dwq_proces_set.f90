!!  Copyright (C)  Stichting Deltares, 2012-2023.
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

module ProcesSet
    use m_string_utils
    use m_logger_helper, only : stop_with_error, get_log_unit_number

    ! module contains everything for proces definition and proces input-output
    !
    ! contains the following derived types:
    !       - ItemProp:
    !           poperties with respect to an item
    !       - IOitemProp:
    !            poperties with respect to an IOitem of a proces
    !       - StochiProp:
    !            properties of a stochi (how a proces output acts on a modelled substance)
    !       - PocesProp:
    !           properties with respect to one proces
    !       - PocesPropColl:
    !            a collection of proces properties

    integer, parameter :: ITEM_NAME_SIZE = 20             ! length all names
    integer, parameter :: ITEM_TEXT_SIZE = 50             ! length all desriptions
    integer, parameter :: ITEM_STDN_SIZE = 100             ! length all standard names
    integer, parameter :: ITEM_STDU_SIZE = 40             ! length all standard units
    integer, parameter :: MAX_NUM = 5             ! allocated per bunch
    integer, parameter :: MAX_NUM_MAX = 100             ! allocated per bunch

    integer, parameter :: IOTYPE_UNKNOWN = 0
    integer, parameter :: IOTYPE_SEGMENT_INPUT = 1
    integer, parameter :: IOTYPE_SEGMENT_WORK = 2
    integer, parameter :: IOTYPE_SCALAR_WORK = 3
    integer, parameter :: IOTYPE_EXCHANG_INPUT = 4
    integer, parameter :: IOTYPE_EXCHANG_WORK = 5
    integer, parameter :: IOTYPE_SEGMENT_OUTPUT = 6
    integer, parameter :: IOTYPE_EXCHANG_OUTPUT = 7
    integer, parameter :: IOTYPE_FLUX = 8

    integer, parameter :: STOCHITYPE_FLUX = 1
    integer, parameter :: STOCHITYPE_DISPERSION = 2
    integer, parameter :: STOCHITYPE_VELOCITY = 3

    integer, parameter :: WAQTYPE_UNKNOWN = -1
    integer, parameter :: WAQTYPE_NONE = 0
    integer, parameter :: WAQTYPE_SUBSTANCE = 1
    integer, parameter :: WAQTYPE_SEGFUNCTION = 2
    integer, parameter :: WAQTYPE_FUNCTION = 3
    integer, parameter :: WAQTYPE_PARAMETER = 4
    integer, parameter :: WAQTYPE_CONSTANT = 5
    integer, parameter :: WAQTYPE_LOCAL = 6
    integer, parameter :: WAQTYPE_EXTARDISP = 7
    integer, parameter :: WAQTYPE_EXTRAVELO = 8
    integer, parameter :: WAQTYPE_LOC_EXCHANG = 9
    integer, parameter :: WAQTYPE_FLUX = 10
    integer, parameter :: WAQTYPE_DEFAULT = 11
    integer, parameter :: WAQTYPE_VOLUME = 12
    integer, parameter :: WAQTYPE_AREA = 13
    integer, parameter :: WAQTYPE_FLOW = 14
    integer, parameter :: WAQTYPE_DISP = 15
    integer, parameter :: WAQTYPE_VELO = 16
    integer, parameter :: WAQTYPE_LENTO = 17
    integer, parameter :: WAQTYPE_LENFROM = 18

    integer, parameter :: PROCESTYPE_FLUX = 1
    integer, parameter :: PROCESTYPE_OUTPUT = 2
    integer, parameter :: PROCESTYPE_STAT = 3

    integer, parameter :: SFRAC_SPLITFLUX = 1
    integer, parameter :: SFRAC_DUPLICATE = 2
    integer, parameter :: SFRAC_EXPAND = 3
    integer, parameter :: SFRAC_DUPLICATED = 4
    integer, parameter :: SFRAC_DUPLICATED_ORIGINAL = 5

    integer, parameter :: ITEM_ACTION_PROCNAM = 1     ! replace old process name with new process name
    integer, parameter :: ITEM_ACTION_PROCPAR = 2     ! replace old process parameter with new name, includes substances
    integer, parameter :: ITEM_ACTION_DEFAULT = 3     ! change default value
    integer, parameter :: ITEM_ACTION_ADDPROC = 4     ! if old process present then add new process
    integer, parameter :: ITEM_ACTION_REMARKPROC = 5     ! print remark, obsolete process
    integer, parameter :: ITEM_ACTION_PROCDEF = 6     ! if old process present then set process parameter default value
    integer, parameter :: ITEM_ACTION_REMARKPAR = 7     ! print remark, process parameter possible incompatibility problem
    integer, parameter :: ITEM_ACTION_PPEQUAL = 8     ! if old parameter present set new parameter equal to this one
    integer, parameter :: ITEM_ACTION_RANGECHECK = 9     ! set new name if the value meets the range
    integer, parameter :: ITEM_ACTION_PPEQUAL2 = 10     ! for internal use, the conditions for ITEM_ACTION_PPEQUAL are met

    type ItemProp
        character(len = ITEM_NAME_SIZE) :: name            ! item name
        character(len = ITEM_TEXT_SIZE) :: text            ! item description
        character(len = ITEM_NAME_SIZE) :: unit            ! item unit
        character(len = ITEM_STDN_SIZE) :: stdn            ! item standard name
        character(len = ITEM_STDU_SIZE) :: stdu            ! item standar unit
        real :: default         ! default value
        character(len = ITEM_NAME_SIZE) :: aggrega         ! aggregation wheight variable
        character(len = ITEM_NAME_SIZE) :: disaggr         ! dis-aggregation wheight variable
        character(len = ITEM_TEXT_SIZE) :: groupid         ! groupid
        character :: segx            ! defined on segment or exchange
        character :: wk              ! wk
        integer :: waqtype         ! indication were the value can be found
        integer :: index           ! index in the array
    end type ItemProp

    type ItemPropPnt
        type(ItemProp), pointer :: pnt             ! pointer
    end type ItemPropPnt

    type ItemPropColl
        type(ItemPropPnt), pointer :: ItemPropPnts(:) ! pointer
        integer :: maxsize         ! maximum size of the current array
        integer :: current_size         ! filled up to this size
    end type ItemPropColl

    type IOitemProp
        character(len = ITEM_NAME_SIZE) :: name            ! IO item name
        integer :: type            ! IO item type
        real :: actdef          ! actual default
        integer :: indx            ! index number in io list process_space_real or flux array
        integer :: ip_val          ! pointer in the waq array space
        type(ItemProp), pointer :: item            ! general item properties
    end type IOitemProp

    type IOitemPropColl
        type(IOitemProp), pointer :: IOitemProps(:)  ! pointer
        integer :: maxsize         ! maximum size of the current array
        integer :: current_size         ! filled up to this size
    end type IOitemPropColl

    type StochiProp
        integer :: type            ! Stochi type (flux, velocity or dispersion)
        character(len = ITEM_NAME_SIZE) :: ioitem          ! this is the flux, velocity or dispersion
        character(len = ITEM_NAME_SIZE) :: substance       ! substance on which it works
        integer :: subindx         ! index number substance
        real :: scale           ! scale factor for the ioitem
    end type StochiProp

    type StochiPropColl
        type(StochiProp), pointer :: StochiProps(:)  ! pointer
        integer :: maxsize         ! maximum size of the current array
        integer :: current_size         ! filled up to this size
    end type StochiPropColl

    type ProcesProp
        character(len = ITEM_NAME_SIZE) :: name            ! proces name
        character(len = ITEM_NAME_SIZE) :: routine         ! PB fortran routine
        character(len = ITEM_TEXT_SIZE) :: text            ! proces description
        integer :: swtransp        ! switch for transport 1D,2D,3D
        integer :: grid            ! grid nummer proces
        integer :: ndt             ! time step multiplier
        logical :: linvok          ! can proces be made
        logical :: active          ! is proces activated
        integer :: type            ! type of proces : flux, output or statistical
        integer :: sfrac_type      ! how to handle fractions
        integer :: no_input        ! number of input items
        type(IOitemProp), pointer :: input_item(:)   ! array with IO items
        integer :: no_output       ! number of outputs on segment
        type(IOitemProp), pointer :: output_item(:)  ! array with IO items
        integer :: no_FluxOutput   ! number of fluxes
        type(IOitemProp), pointer :: FluxOutput(:)   ! array with IO items
        integer :: no_FluxStochi   ! number of Flux Stochis
        type(StochiProp), pointer :: FluxStochi(:)   ! Flux Stochi properties
        integer :: no_VeloStochi   ! number of Velocity Stochis
        type(StochiProp), pointer :: VeloStochi(:)   ! Velocity Stochi properties
        integer :: no_DispStochi   ! number of Dispersion Stochis
        type(StochiProp), pointer :: DispStochi(:)   ! Dispersion Stochis properties
    end type ProcesProp

    type ProcesPropColl
        type(ProcesProp), pointer :: ProcesProps(:)  ! array with proces properties
        integer :: maxsize         ! maximum size of the current array
        integer :: current_size         ! filled up to this size
    end type ProcesPropColl

    type ArrayProp
        character(len = ITEM_NAME_SIZE) :: name            ! name of varaibale in array
    end type ArrayProp

    type ArrayPropColl
        type(ArrayProp), pointer :: ArrayProps(:)   ! array with proces properties
        integer :: maxsize         ! maximum size of the current array
        integer :: current_size         ! filled up to this size
    end type ArrayPropColl

    type sfracsprop
        integer :: nsfrac          ! number of substances with fractions
        character(len = 20), pointer :: name(:)         ! base name substance fractions
        integer, pointer :: nfrac(:)        ! number of fractions
        integer, pointer :: linked(:)       ! linked with fraction if .ne. 0
        integer, pointer :: linklist(:, :)   ! linked with fraction if .ne. 0
    end type sfracsprop

    type old_item
        character(len = ITEM_NAME_SIZE) :: old_name         ! old name (if equal to new name then use old_default if target serial is less then
        character(len = ITEM_NAME_SIZE) :: new_name         ! new name
        real :: old_default      ! old default value
        character(len = ITEM_NAME_SIZE) :: configuration    ! (only use this new name if a specific configuration is used?)
        integer :: serial           ! the proces definition serial number up to where this old name, old default was used
        integer :: action_type      ! process rename, process parameter rename, default value change
    end type old_item

    type old_item_coll
        type(old_item), pointer :: old_items(:)     ! pointer
        integer :: target_serial    ! target serial number for changing certain items
        integer :: maxsize          ! maximum size of the current array
        integer :: current_size          ! filled up to this size
    end type old_item_coll

contains

    ! function to find an item in a collection of item properties

    function ItemPropCollFind(aItemPropColl, aItemProp) result (iret)

        type(ItemPropColl) :: aItemPropColl
        type(ItemProp) :: aItemProp
        integer :: iret

        iret = 0
        do i = 1, aItemPropColl%current_size         ! search by name, case insesitive
            if (string_equals(aItemProp%name, aItemPropColl%ItemPropPnts(i)%pnt%name)) then
                iret = i
                return
            endif
        end do

    end function ItemPropCollFind

    ! function to add to a collection of items

    function ItemPropCollAdd(aItemPropColl, aItemProp) result (current_size)

        type(ItemPropColl) :: aItemPropColl
        type(ItemProp) :: aItemProp
        type(ItemProp), pointer :: aPropPnt           ! should be a pointer to preserve space
        type(ItemPropPnt), pointer :: aItemPropPnts(:)   ! should be a pointer for the resize operation
        integer :: current_size
        ! this is the standard procedure to enlarge collections
        if (aItemPropColl%current_size == aItemPropColl%maxsize) then
            allocate (aItemPropPnts (aItemPropColl%maxsize + MAX_NUM_MAX))
            do i = 1, aItemPropColl%maxsize
                aItemPropPnts(i) = aItemPropColl%ItemPropPnts (i)        ! copies the pointers
            enddo
            if (aItemPropColl%maxsize /= 0) deallocate (aItemPropColl%ItemPropPnts)
            aItemPropColl%ItemPropPnts => aItemPropPnts                   ! attaches this new array of pointers
            aItemPropColl%maxsize = aItemPropColl%maxsize + MAX_NUM_MAX
        endif
        aItemPropColl%current_size = aItemPropColl%current_size + 1
        allocate (aPropPnt)                                  ! this is important, allocate space to
        aPropPnt = aItemProp                                   !                    preserve argument
        aItemPropColl%ItemPropPnts(aItemPropColl%current_size)%pnt => aPropPnt       ! put referenc eto space in array
        current_size = aItemPropColl%current_size
        return

    end function ItemPropCollAdd

    ! function to add to a collection of ProcesProp

    function ProcesPropCollAdd(aProcesPropColl, aProcesProp) result (current_size)

        type(ProcesPropColl) :: aProcesPropColl
        type(ProcesProp) :: aProcesProp

        type(ProcesProp), pointer :: aProcesProps(:)   ! should be a pointer for the resize operation
        integer :: current_size
        ! this is the standard procedure to enlarge collections
        if (aProcesPropColl%current_size == aProcesPropColl%maxsize) then
            allocate (aProcesProps (aProcesPropColl%maxsize + MAX_NUM), stat = ierr_alloc)
            if (ierr_alloc /= 0) then
                write(*, *) 'ERROR : ALLOCATING WORK ARRAY'
                call stop_with_error()
            endif
            do i = 1, aProcesPropColl%maxsize
                aProcesProps(i) = aProcesPropColl%ProcesProps (i)        ! copies the contents
            enddo
            if (aProcesPropColl%maxsize /= 0) deallocate (aProcesPropColl%ProcesProps)
            aProcesPropColl%ProcesProps => aProcesProps                   ! attaches this new array of pointers
            aProcesPropColl%maxsize = aProcesPropColl%maxsize + MAX_NUM
        endif
        aProcesPropColl%current_size = aProcesPropColl%current_size + 1
        aProcesPropColl%ProcesProps(aProcesPropColl%current_size) = aProcesProp
        current_size = aProcesPropColl%current_size
        return

    end function ProcesPropCollAdd

    function IOitemPropCollAdd(aIOitemPropColl, aIOitemProp) result (current_size)

        type(IOitemPropColl) :: aIOitemPropColl
        type(IOitemProp) :: aIOitemProp

        type(IOitemProp), pointer :: aIOitemProps(:)   ! should be a pointer for the resize operation
        integer :: current_size

        if (aIOitemPropColl%current_size == aIOitemPropColl%maxsize) then
            allocate (aIOitemProps (aIOitemPropColl%maxsize + MAX_NUM), stat = ierr_alloc)
            if (ierr_alloc /= 0) then
                write(*, *) 'ERROR : ALLOCATING WORK ARRAY'
                call stop_with_error()
            endif
            do i = 1, aIOitemPropColl%maxsize
                aIOitemProps(i) = aIOitemPropColl%IOitemProps (i)        ! copies the contents
            enddo
            if (aIOitemPropColl%maxsize /= 0) deallocate (aIOitemPropColl%IOitemProps)
            aIOitemPropColl%IOitemProps => aIOitemProps                   ! attaches this new array of pointers
            aIOitemPropColl%maxsize = aIOitemPropColl%maxsize + MAX_NUM
        endif
        aIOitemPropColl%current_size = aIOitemPropColl%current_size + 1
        aIOitemPropColl%IOitemProps(aIOitemPropColl%current_size) = aIOitemProp
        current_size = aIOitemPropColl%current_size
        return

    end function IOitemPropCollAdd

    function IOitemPropCollAddIndx(aIOitemPropColl, aIOitemProp, indx) result (current_size)

        type(IOitemPropColl) :: aIOitemPropColl
        type(IOitemProp) :: aIOitemProp
        integer :: indx
        integer :: current_size

        type(IOitemProp), pointer :: aIOitemProps(:)   ! should be a pointer for the resize operation

        if (indx <= 0) then
            call get_log_unit_number(lunrep)
            write(lunrep, *) 'ERROR : in IOitemPropCollAddIndx'
            write(lunrep, *) 'requested index not allowed :', indx
            write(*, *) 'ERROR : internal error'
            call stop_with_error()
        endif

        if (indx > aIOitemPropColl%maxsize) then

            ! reallocate

            allocate (aIOitemProps (indx + MAX_NUM), stat = ierr_alloc)
            if (ierr_alloc /= 0) then
                call get_log_unit_number(lunrep)
                write(lunrep, *) 'ERROR : allocating IOitemPropColl array'
                write(lunrep, *) 'requested size :', indx + MAX_NUM
                write(*, *) 'ERROR : ALLOCATING WORK ARRAY'
                call stop_with_error()
            endif

            ! copy the old items

            do i = 1, aIOitemPropColl%current_size
                aIOitemProps(i) = aIOitemPropColl%IOitemProps (i)        ! copies the contents
            enddo
            if (aIOitemPropColl%maxsize /= 0) deallocate (aIOitemPropColl%IOitemProps)
            aIOitemPropColl%maxsize = indx + MAX_NUM

            ! empty the newly added items

            do i = aIOitemPropColl%current_size + 1, aIOitemPropColl%maxsize
                aIOitemProps(i)%name = ' '
                aIOitemProps(i)%type = IOTYPE_UNKNOWN
                aIOitemProps(i)%actdef = -999.
                aIOitemProps(i)%indx = 0
                nullify(aIOitemProps(i)%item)
            enddo

            ! attach the new array to the collection

            aIOitemPropColl%IOitemProps => aIOitemProps

        endif

        ! add the new item at requested index

        aIOitemPropColl%IOitemProps(indx) = aIOitemProp
        if (indx > aIOitemPropColl%current_size) then
            aIOitemPropColl%current_size = indx
        endif
        current_size = aIOitemPropColl%current_size
        return

    end function IOitemPropCollAddIndx

    function StochiPropCollAdd(aStochiPropColl, aStochiProp) result (current_size)

        type(StochiPropColl) :: aStochiPropColl
        type(StochiProp) :: aStochiProp

        type(StochiProp), pointer :: aStochiProps(:)   ! should be a pointer for the resize operation
        integer :: current_size

        if (aStochiPropColl%current_size == aStochiPropColl%maxsize) then
            allocate (aStochiProps (aStochiPropColl%maxsize + MAX_NUM), stat = ierr_alloc)
            if (ierr_alloc /= 0) then
                write(*, *) 'ERROR : ALLOCATING WORK ARRAY'
                call stop_with_error()
            endif
            do i = 1, aStochiPropColl%maxsize
                aStochiProps(i) = aStochiPropColl%StochiProps (i)        ! copies the contents
            enddo
            if (aStochiPropColl%maxsize /= 0) deallocate (aStochiPropColl%StochiProps)
            aStochiPropColl%StochiProps => aStochiProps                   ! attaches this new array of pointers
            aStochiPropColl%maxsize = aStochiPropColl%maxsize + MAX_NUM
        endif
        aStochiPropColl%current_size = aStochiPropColl%current_size + 1
        aStochiPropColl%StochiProps(aStochiPropColl%current_size) = aStochiProp
        current_size = aStochiPropColl%current_size
        return

    end function StochiPropCollAdd

    function ArrayPropCollAdd(aArrayPropColl, aArrayProp) result (current_size)

        type(ArrayPropColl) :: aArrayPropColl
        type(ArrayProp) :: aArrayProp

        type(ArrayProp), pointer :: aArrayProps(:)   ! should be a pointer for the resize operation
        integer :: current_size

        if (aArrayPropColl%current_size == aArrayPropColl%maxsize) then
            allocate (aArrayProps (aArrayPropColl%maxsize + MAX_NUM), stat = ierr_alloc)
            if (ierr_alloc /= 0) then
                write(*, *) 'ERROR : ALLOCATING WORK ARRAY'
                call stop_with_error()
            endif
            do i = 1, aArrayPropColl%maxsize
                aArrayProps(i) = aArrayPropColl%ArrayProps (i)        ! copies the contents
            enddo
            if (aArrayPropColl%maxsize /= 0) deallocate (aArrayPropColl%ArrayProps)
            aArrayPropColl%ArrayProps => aArrayProps                   ! attaches this new array of pointers
            aArrayPropColl%maxsize = aArrayPropColl%maxsize + MAX_NUM
        endif
        aArrayPropColl%current_size = aArrayPropColl%current_size + 1
        aArrayPropColl%ArrayProps(aArrayPropColl%current_size) = aArrayProp
        current_size = aArrayPropColl%current_size
        return

    end function ArrayPropCollAdd

    function old_item_coll_add(a_old_item_coll, a_old_item) result (current_size)

        type(old_item_coll) :: a_old_item_coll
        type(old_item) :: a_old_item
        integer :: current_size

        type(old_item), pointer :: a_old_items(:)    ! should be a pointer for the resize operation

        if (a_old_item_coll%current_size == a_old_item_coll%maxsize) then
            allocate (a_old_items(a_old_item_coll%maxsize + MAX_NUM), stat = ierr_alloc)
            if (ierr_alloc /= 0) then
                write(*, *) 'ERROR : ALLOCATING WORK ARRAY'
                call stop_with_error()
            endif
            do i = 1, a_old_item_coll%maxsize
                a_old_items(i) = a_old_item_coll%old_items (i)        ! copies the contents
            enddo
            if (a_old_item_coll%maxsize /= 0) deallocate (a_old_item_coll%old_items)
            a_old_item_coll%old_items => a_old_items                    ! attaches this new array of pointers
            a_old_item_coll%maxsize = a_old_item_coll%maxsize + MAX_NUM
        endif
        a_old_item_coll%current_size = a_old_item_coll%current_size + 1
        a_old_item_coll%old_items(a_old_item_coll%current_size) = a_old_item
        current_size = a_old_item_coll%current_size
        return

    end function old_item_coll_add

    subroutine zoekio(name, num, ioitemsprops, lenchk, indx, type)
        character(len = 20) :: name            !< name of ioitem to be found
        integer :: num             !< number
        type(ioitemprop), pointer :: ioitemsprops(:) !< array with io items
        integer :: lenchk          !< characters to be checked
        integer :: indx            !< return value
        integer, optional :: type            !< type

        indx = -1
        do i = 1, num

            if (.not. string_equals(name(1:lenchk), ioitemsprops(i)%name)) then
                cycle
            endif

            if (present(type)) then
                if (ioitemsprops(i)%type /= type) then
                    cycle
                endif
            endif

            indx = i
            exit
        enddo

        return
    end subroutine zoekio

end module ProcesSet
