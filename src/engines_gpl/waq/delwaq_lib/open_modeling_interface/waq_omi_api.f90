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

!> @file
!! General interface routines to DELWAQ for OpenMI:
!! The routines are accessible from C/C++
!!<

!> Implementation of the routines that interface DELWAQ with OpenMI
!! see https://publicwiki.deltares.nl/display/OPENMI/Home
module waq_omi_priv

    use m_waq_precision
    use waq_omi_utils
    use delwaq2_global_data

    implicit none

    ! // here: all ei-enumerations!!!!

contains

    !> Set a value
    logical function SetValuePriv(dlwqtype, parid, locid, values, operation)

        use m_sysn          ! System characteristics
        use m_sysa          ! Pointers in real array workspace

        implicit none

        integer(kind = int_wp), intent(in) :: dlwqtype          !< Type of parameter to be set
        integer(kind = int_wp), intent(in) :: parid             !< Index of the parameter
        integer(kind = int_wp), intent(in) :: locid             !< Index of the parameter
        real(kind = real_wp), dimension(:), intent(in) :: values            !< Value to be used in the operation
        integer(kind = int_wp), intent(in) :: operation         !< Operation to apply

        integer(kind = int_wp), dimension(6) :: idx
        logical :: success

        !
        ! Check the arguments
        !
        success = .true.
        call CheckParameterId(dlwqtype, parid, success)
        call CheckLocationId(dlwqtype, locid, success)
        call CheckOperation(operation, success)

        if (.not. success) then
            SetValuePriv = .false.
            return
        end if

        idx = DetermineIndex(dlwqtype, parid, locid)

        !
        ! Sanity check:
        ! The size of the array "values" must be equal to
        ! the number of items stored in "idx"
        !
        if (size(values) /= idx(3)) then
            write (*, *) 'SetValues: Error - inconsistent number of values'
            write (*, *) '           Number of substances: ', notot
            write (*, *) '           Number of segments:   ', noseg
            write (*, *) '           Number of values:     ', size(values)
            write (*, *) '           Should be 1, ', notot * noseg, ' or ', noseg
            SetValuePriv = .false.
            return
        end if

        call StoreOperation(idx, size(values), values, operation)

        SetValuePriv = .true.

    end function SetValuePriv

    function DetermineIndex(dlwqtype, parid, locid)
        !> Determine the exact index in the overall rbuf array
        !!
        !! The return value is an array of three data: index, step
        !! and potential number of elements that is affected
        use m_sysn          ! System characteristics
        use m_sysa          ! Pointers in real array workspace

        integer(kind = int_wp), intent(in) :: dlwqtype
        integer(kind = int_wp), intent(in) :: parid
        integer(kind = int_wp), intent(in) :: locid
        integer(kind = int_wp), dimension(6) :: DetermineIndex

        integer(kind = int_wp) :: idx
        integer(kind = int_wp) :: step
        integer(kind = int_wp) :: number
        integer(kind = int_wp) :: idxmass
        integer(kind = int_wp) :: idxvol
        integer(kind = int_wp) :: volcorr

        idxmass = -1
        idxvol = -1
        volcorr = 1

        DetermineIndex(1) = idx
        DetermineIndex(2) = step
        DetermineIndex(3) = number
        DetermineIndex(4) = idxmass
        DetermineIndex(5) = idxvol
        DetermineIndex(6) = volcorr

    end function DetermineIndex

    subroutine StoreOperation(index, number_values, new_value, operation)
        !> Store the operation and the new value
        !!
        !! The operation should actually be applied within a suitable point
        !! in the computational cycle, so it is stored in a buffer for later use
        !  note: new_values should be an array of size <number_values>
        !

        integer(kind = int_wp), dimension(:), intent(in) :: index            !< Index into the rbuf array
        integer(kind = int_wp), intent(in) :: number_values    !< Number of values
        real(kind = real_wp), dimension(:), intent(in) :: new_value        !< Array of new values or modification values
        integer(kind = int_wp), intent(in) :: operation        !< Operation to be performed

        integer(kind = int_wp) :: pos

    end subroutine StoreOperation

    subroutine SetNewValue(value, new_value, operation)
        !> Set the new value, based on the operation
        !!
        !! The operation should actually be applied within a suitable point
        !! in the computational cycle, so it is stored in a buffer for later use

        real(kind = real_wp), intent(inout) :: value
        real(kind = real_wp), intent(in) :: new_value
        integer(kind = int_wp), intent(in) :: operation

    end subroutine SetNewValue

    logical function GetValuePriv(type, parid, locid, value)
        !> Retrieve a value

        integer(kind = int_wp), intent(in) :: type              !< Type of parameter to be set
        integer(kind = int_wp), intent(in) :: parid             !< Index of the parameter
        integer(kind = int_wp), intent(in) :: locid             !< Index of the location
        real(kind = real_wp), dimension(:), intent(out) :: value             !< Value to be used in the operation

        integer(kind = int_wp), dimension(6) :: idx
        logical :: success

        !
        ! Check the arguments
        !
        success = .true.
        call CheckParameterId(type, parid, success)
        call CheckLocationId(type, locid, success)

        if (.not. success) then
            GetValuePriv = .false.
            return
        end if

        idx = DetermineIndex(type, parid, locid)

        value = dlwqd%buffer%rbuf(idx(1):idx(1) + (idx(3) - 1) * idx(2):idx(2))
        GetValuePriv = .true.

    end function GetValuePriv

    subroutine CheckParameterId(dlwqtype, parid, success)
        !> Check that the parameter ID is valid
        use m_sysn          ! System characteristics

        integer(kind = int_wp), intent(in) :: dlwqtype
        integer(kind = int_wp), intent(in) :: parid
        logical, intent(inout) :: success

        success = .false.

    end subroutine CheckParameterId

    subroutine CheckLocationId(dlwqtype, locid, success)
        !> Check that the location ID is valid

        use m_sysn

        integer(kind = int_wp), intent(in) :: dlwqtype
        integer(kind = int_wp), intent(in) :: locid
        logical, intent(inout) :: success

        success = .false.

    end subroutine CheckLocationId

    subroutine CheckOperation(operation, success)
        !> Check that the operation is valid

        integer(kind = int_wp), intent(in) :: operation
        logical, intent(inout) :: success

        success = .false.

    end subroutine CheckOperation

    integer function GetLocationCountPriv(type)
        !> Retrieve number of locations (of given type)

        use m_sysn

        integer(kind = int_wp), intent(in) :: type

        GetLocationCountPriv = 0

    end function GetLocationCountPriv

    !> Retrieve indices of locations (of given type)
    integer function GetLocationIndicesPriv(type, idsSize, ids)

        use m_sysn

        integer(kind = int_wp), intent(in) :: type
        integer(kind = int_wp), intent(in) :: idsSize
        integer(kind = int_wp), dimension(:), intent(out) :: ids

        integer(kind = int_wp) :: count
        integer(kind = int_wp) :: i

        count = GetLocationCountPriv(type)

        if (idsSize /= count) then
            ids = 0
            GetLocationIndicesPriv = -1
        else
            ids = (/(i, i = 1, count)/)
            GetLocationIndicesPriv = 0
        end if

    end function GetLocationIndicesPriv

    !> Retrieve number of items (of given type)
    integer function GetItemCountPriv(type)
        use m_sysn

        integer(kind = int_wp), intent(in) :: type

        GetItemCountPriv = 0

    end function GetItemCountPriv

    !> Retrieve index of items (of given type)
    integer function GetItemIndexPriv(dlwqtype, name)

        integer, intent(in) :: dlwqtype
        character(len = *), intent(in) :: name

        integer(kind = int_wp) :: idx

        GetItemIndexPriv = 0

    end function GetItemIndexPriv

    !> Retrieve the name of items (of given type)
    integer function GetItemNamePriv(type, idx, name)

        integer, intent(in) :: type
        character(len = *), intent(out) :: name
        integer(kind = int_wp) :: idx

        GetItemNamePriv = -1
        name = '?'

    end function GetItemNamePriv

    !> Retrieve index of location (of given type)
    integer function GetLocationIndexPriv(type, name)

        integer, intent(in) :: type
        character(len = *), intent(in) :: name

        integer(kind = int_wp) :: idx

        GetLocationIndexPriv = 0

    end function GetLocationIndexPriv

    !> Get the time parameters for the computation
    subroutine GetTimeParameters(start, stop, step, current)

        use m_sysi

        implicit none

        real(kind = kind(1.0d0)), intent(out) :: start
        real(kind = kind(1.0d0)), intent(out) :: stop
        real(kind = kind(1.0d0)), intent(out) :: step
        real(kind = kind(1.0d0)), intent(out) :: current

        start = dlwqd%otime + itstrt / dlwqd%tscale
        stop = dlwqd%otime + itstop / dlwqd%tscale
        step = idt / dlwqd%tscale
        current = dlwqd%otime + dlwqd%itime / dlwqd%tscale

    end subroutine GetTimeParameters

end module waq_omi_priv

module waq_omi_api
    use m_waq_precision

contains
    integer function Count_Values(partype, parid, loctype, locid)
        !DEC$ ATTRIBUTES DLLEXPORT::Count_Values
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'COUNT_VALUES' :: Count_Values

        use waq_omi_priv
        use m_sysn          ! System characteristics
        use m_sysa          ! Pointers in real array workspace

        implicit none

        integer(kind = int_wp), intent(in) :: parid             !< Index of the parameter
        integer(kind = int_wp), intent(in) :: partype           !< Type of parameter to be set
        integer(kind = int_wp), intent(in) :: locid             !< Index of the location
        integer(kind = int_wp), intent(in) :: loctype           !< Type of the location

        integer(kind = int_wp) :: dlwqtype
        integer(kind = int_wp) :: count

        Count_Values = 1

    end function Count_Values

    !> Set the current value of a substance or process parameter:
    !! This function provides a general interface to the state variables
    !! and computational parameters.
    !!
    !! The type should be one of the following:
    !! \li DLWQ_CONSTANT: set the value of a "constant" process parameter (location-independent)
    !! \li DLWQ_PARAMETER: set the value of a "parameter" process parameter
    !! \li DLWQ_CONCENTRATION: set the value of a substance (or other state variable)
    !! \li DLWQ_DISCHARGE: set the value for the discharge (mass per time) of a substance
    !! \li DLWQ_BOUNDARYVALUE: set the value for the boundary condition of a substance
    !!
    !! The parameter ID should correspond to the correct type, as should the location ID.
    !! (For constants the location ID is ignored)
    !!
    !! The operation can be:
    !! \li DLWQ_SET: simply replace the original value by the new value
    !! \li DLWQ_ADD: add the given value to the original value
    !! \li DLWQ_MULTIPLY: multiply the original value by the given value
    !!
    !! Note: Specifically for OpenDA
    integer function Set_Values(partype, parid, loctype, locid, operation, number, values)

        !DEC$ ATTRIBUTES DLLEXPORT::Set_Values
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SET_VALUES' :: Set_Values

        use waq_omi_priv

        implicit none

        integer(kind = int_wp), intent(in) :: parid             !< Index of the parameter
        integer(kind = int_wp), intent(in) :: partype           !< Type of parameter to be set
        integer(kind = int_wp), intent(in) :: locid             !< Index of the location or ODA_ALL_SEGMENTS
        integer(kind = int_wp), intent(in) :: loctype           !< Type of location to be set
        integer(kind = int_wp), intent(in) :: operation         !< Operation to apply
        integer(kind = int_wp), intent(in) :: number            !< Number of values
        double precision, dimension(number), intent(in) :: values  !< Value(s) to be used in the operation

        real(kind = real_wp), dimension(:), allocatable :: r_values

        integer(kind = int_wp) :: idx
        integer(kind = int_wp) :: locid_
        integer(kind = int_wp) :: parid_
        integer(kind = int_wp) :: dlwqtype
        logical :: success

        Set_Values = 1
    end function Set_Values

    !> Set the current value of a substance or process parameter:
    !! This function provides a general interface to the state variables
    !! and computational parameters.
    !!
    !! The type should be one of the following:
    !! \li DLWQ_CONSTANT: set the value of a "constant" process parameter (location-independent)
    !! \li DLWQ_PARAMETER: set the value of a "parameter" process parameter
    !! \li DLWQ_CONCENTRATION: set the value of a substance (or other state variable)
    !! \li DLWQ_DISCHARGE: set the value for the discharge (mass per time) of a substance
    !! \li DLWQ_BOUNDARYVALUE: set the value for the boundary condition of a substance
    !!
    !! The parameter ID should correspond to the correct type, as should the location ID.
    !! (For constants the location ID is ignored)
    !!
    !! The operation can be:
    !! \li DLWQ_SET: simply replace the original value by the new value
    !! \li DLWQ_ADD: add the given value to the original value
    !! \li DLWQ_MULTIPLY: multiply the original value by the given value
    !!
    !! Note: this routine is meant for general interfacing - it assumes you use the
    !! DELWAQ codes for the arguments dlwqtype and operation, as well as a single-precision
    !! array for the values.
    !!
    !! Note: the number of values must match the number of values expected for the type
    !! of data.
    !!
    integer function Set_Values_General(dlwqtype, parid, locid, operation, number, values)

        !DEC$ ATTRIBUTES DLLEXPORT::Set_Values_General
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SET_VALUES_GENERAL' :: Set_Values_General

        use waq_omi_priv

        implicit none

        integer(kind = int_wp), intent(in) :: dlwqtype          !< Type of parameter/location to be set
        integer(kind = int_wp), intent(in) :: parid             !< Index of the parameter
        integer(kind = int_wp), intent(in) :: locid             !< Index of the location
        integer(kind = int_wp), intent(in) :: operation         !< Operation to apply
        integer(kind = int_wp), intent(in) :: number            !< Number of values
        real(kind = real_wp), dimension(number), intent(in) :: values            !< Value(s) to be used in the operation

        logical :: success

        success = SetValuePriv(dlwqtype, parid, locid, values, operation)
        Set_Values_General = merge(1, 0, success)
    end function Set_Values_General

    !> Retrieve the current value of a substance or process parameter:
    !! This function provides a general interface to retrieving the state variables
    !! and computational parameters.
    !!
    !! The type should be one of the following:
    !! \li DLWQ_CONSTANT: set the value of a "constant" process parameter (location-independent)
    !! \li DLWQ_PARAMETER: set the value of a "parameter" process parameter
    !! \li DLWQ_CONCENTRATION: set the value of a substance (or other state variable)
    !! \li DLWQ_DISCHARGE: set the value for the discharge (mass per time) of a substance
    !! \li DLWQ_BOUNDARYVALUE: set the value for the boundary condition of a substance
    !!
    !! The parameter ID should correspond to the correct type, as should the location ID.
    !! (For constants the location ID is ignored)
    integer function Get_Values(partype, parid, loctype, locid, number, values)

        !DEC$ ATTRIBUTES DLLEXPORT::Get_Values
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GET_VALUES' :: Get_Values

        use waq_omi_priv
        !    use m_delwaq_2_openda ! Only for current_instance

        implicit none

        integer(kind = int_wp), intent(in) :: partype           !< Type of parameter to be set
        integer(kind = int_wp), intent(in) :: parid             !< Index of the parameter
        integer(kind = int_wp), intent(in) :: loctype           !< Type of location to be set
        integer(kind = int_wp), intent(in) :: locid             !< Index of the location
        integer(kind = int_wp), intent(in) :: number            !< Size of array values
        double precision, dimension(number), intent(out) :: values         !< Value to be used in the operation

        real(kind = real_wp), dimension(:), allocatable :: r_values
        integer(kind = int_wp) :: idx
        integer(kind = int_wp) :: dlwqtype
        logical :: success
        real(kind = kind(1.0d0)) :: currentTime
        integer(kind = int_wp) :: dummy

        allocate (r_values(number))

        success = GetValuePriv(dlwqtype, parid, locid, r_values)
        values = r_values

        deallocate (r_values)

        Get_Values = merge(1, 0, success)

        dummy = GetWQCurrentTime(currentTime)

    end function Get_Values

    !> Interface to CheckParameterId in the waq_omi_priv module for testing purposes
    subroutine CheckParameterId(type, parid, success)

        !DEC$ ATTRIBUTES DLLEXPORT::CheckParameterId
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'CHECKPARAMETERID' :: CheckParameterId

        use waq_omi_priv, TestCheckParameterId => CheckParameterId

        implicit none

        integer(kind = int_wp), intent(in) :: type
        integer(kind = int_wp), intent(in) :: parid
        logical, intent(out) :: success

        call TestCheckParameterId(type, parid, success)

    end subroutine CheckParameterId

    !> Interface to SetNewValue in the waq_omi_priv module for testing purposes
    subroutine SetNewValue(value, new_value, operation)

        !DEC$ ATTRIBUTES DLLEXPORT::SetNewValue
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETNEWVALUE' :: SetNewValue

        use waq_omi_priv, TestSetNewValue => SetNewValue

        implicit none

        real(kind = real_wp), intent(inout) :: value
        real(kind = real_wp), intent(in) :: new_value
        integer(kind = int_wp), intent(in) :: operation

        call TestSetNewValue(value, new_value, operation)

    end subroutine SetNewValue

    !> Interface to get DLWQD from the library for testing purposes
    subroutine GetDlwqd(dlwqd_copy)

        use delwaq2_global_data

        !DEC$ ATTRIBUTES DLLEXPORT::GetDlwqd
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETDLWQD' :: GetDlwqd

        type(delwaq_data) :: dlwqd_copy

        dlwqd_copy = dlwqd

    end subroutine GetDlwqd

    !> Interface to fill DLWQD from the library for testing purposes
    subroutine SetDlwqd(dlwqd_copy)

        use delwaq2_global_data

        !DEC$ ATTRIBUTES DLLEXPORT::SetDlwqd
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETDLWQD' :: SetDlwqd

        type(delwaq_data) :: dlwqd_copy

        dlwqd = dlwqd_copy

    end subroutine SetDlwqd

    !> Interface to set a number of variables in the COMMON blocks for testing purposes
    !!
    !! <i>Note:</i> This is necessary because the COMMON blocks in the DLL are <i>different</i>
    !! from the ones in the test program
    subroutine SetCommonVars(icons_, iparm_, iconc_, ibset_, iwste_, nosys_, notot_, nocons_, nopa_, noseg_, nowst_, nobnd_)

        !DEC$ ATTRIBUTES DLLEXPORT::SetCommonVars
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETCOMMONVARS' :: SetCommonVars

        use m_sysn          ! System characteristics
        use m_sysa          ! Pointers in real array workspace

        implicit none

        integer(kind = int_wp) :: icons_, iparm_, iconc_, ibset_, iwste_, nosys_, notot_, nocons_, nopa_, noseg_, nowst_, nobnd_

        icons = icons_
        iparm = iparm_
        iconc = iconc_
        ibset = ibset_
        iwste = iwste_
        nosys = nosys_
        notot = notot_
        nototp = notot_   ! Particles not supported yet
        nocons = nocons_
        nopa = nopa_
        noseg = noseg_
        nowst = nowst_
        nobnd = nobnd_

    end subroutine SetCommonVars

    !> Interface to StoreOperation in the waq_omi_priv module for testing purposes
    subroutine StoreOperation(index, number, new_value, operation)

        !DEC$ ATTRIBUTES DLLEXPORT::StoreOperation
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'STOREOPERATION' :: StoreOperation

        use waq_omi_priv, TestStoreOperation => StoreOperation

        implicit none

        integer(kind = int_wp), dimension(3), intent(in) :: index
        integer(kind = int_wp), intent(in) :: number
        real(kind = real_wp), dimension(number), intent(in) :: new_value
        integer(kind = int_wp), intent(in) :: operation

        call TestStoreOperation(index, number, new_value, operation)

    end subroutine StoreOperation

    !> Interface to apply_operations in the delwaq2_data module for testing purposes
    subroutine test_apply_operations(dlwqd)

        !DEC$ ATTRIBUTES DLLEXPORT::test_apply_operations
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'TEST_APPLY_OPERATIONS' :: test_apply_operations

        use delwaq2_data

        implicit none

        type(delwaq_data) :: dlwqd

        !!call apply_operations( dlwqd )

    end subroutine test_apply_operations

    !> Retrieve number of locations (of given type)
    integer function GetLocationCount(odatype)

        !DEC$ ATTRIBUTES DLLEXPORT::GetLocationCount
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETLOCATIONCOUNT' :: GetLocationCount

        use waq_omi_priv

        implicit none

        integer(kind = int_wp), intent(in) :: odatype

        integer(kind = int_wp) :: dlwqtype

        GetLocationCount = 0

    end function GetLocationCount

    !> Retrieve number of items (of given type)
    integer function GetItemCount(type)

        !DEC$ ATTRIBUTES DLLEXPORT::GetItemCount
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETITEMCOUNT' :: GetItemCount

        use waq_omi_priv

        implicit none

        integer(kind = int_wp), intent(in) :: type

        GetItemCount = GetItemCountPriv(type)

    end function GetItemCount

    !> Retrieve index of a location (of given type)
    integer function GetLocationId(type, name)

        !DEC$ ATTRIBUTES DLLEXPORT::GetLocationId
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETLOCATIONID' :: GetLocationId

        use waq_omi_priv

        implicit none

        integer(kind = int_wp), intent(in) :: type
        character(len = *), intent(in) :: name

        GetLocationId = GetLocationIndexPriv(type, name)

    end function GetLocationId

    !> Retrieve index of a location (of given type)
    integer function GetLocationIds(type, idsSize, ids)

        !DEC$ ATTRIBUTES DLLEXPORT::GetLocationIds
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETLOCATIONIDS' :: GetLocationIds

        use waq_omi_priv

        implicit none

        integer(kind = int_wp), intent(in) :: type
        integer(kind = int_wp), intent(in) :: idsSize
        integer(kind = int_wp), dimension(idsSize), intent(out) :: ids

        GetLocationIds = GetLocationIndicesPriv(type, idsSize, ids)

    end function GetLocationIds

    !> Retrieve index of an item (of given type)
    integer function GetItemIndex(type, name)

        !DEC$ ATTRIBUTES DLLEXPORT::GetItemIndex
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETITEMINDEX' :: GetItemIndex

        use waq_omi_priv

        implicit none

        integer(kind = int_wp), intent(in) :: type
        character(len = *), intent(in) :: name

        GetItemIndex = GetItemIndexPriv(type, name)

    end function GetItemIndex

    !> Retrieve name of a location (of given type)
    integer function GetLocationName(type, idx, name)

        !DEC$ ATTRIBUTES DLLEXPORT::GetLocationName
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETLOCATIONNAME' :: GetLocationName

        use waq_omi_priv

        implicit none

        integer(kind = int_wp), intent(in) :: type             !< Type of location
        integer(kind = int_wp), intent(in) :: idx              !< Index of the location
        character(len = *), intent(out) :: name            !< Name of the location (if successful)

        ! GetLocationName = GetLocationNamePriv( type, idx, name )
        name = 'TODO'
        GetLocationName = 1

    end function GetLocationName

    !> Retrieve name of an item (of given type)
    integer function GetItemName(type, idx, name)

        !DEC$ ATTRIBUTES DLLEXPORT::GetItemName
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETITEMNAME' :: GetItemName

        use waq_omi_priv

        implicit none

        integer(kind = int_wp), intent(in) :: type             !< Type of item
        integer(kind = int_wp), intent(in) :: idx              !< Index of item
        character(len = *), intent(out) :: name            !< Name of the item (if successful)

        GetItemName = GetItemNamePriv(type, idx, name)

    end function GetItemName

    !> Get the number of active substances
    integer function GetActiveSubstancesCount()

        !DEC$ ATTRIBUTES DLLEXPORT::GetActiveSubstancesCount
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETACTIVESUBSTANCESCOUNT' :: GetActiveSubstancesCount

        use waq_omi_priv

        implicit none

        GetActiveSubstancesCount = 0

    end function GetActiveSubstancesCount

    !> Get the number of all substances
    integer function GetTotalSubstancesCount()

        !DEC$ ATTRIBUTES DLLEXPORT::GetTotalSubstancesCount
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETTOTALSUBSTANCESCOUNT' :: GetTotalSubstancesCount

        use waq_omi_priv

        implicit none

        GetTotalSubstancesCount = 0

    end function GetTotalSubstancesCount

    !> Get the name of a substance
    integer function GetSubstanceName(subid, name)

        !DEC$ ATTRIBUTES DLLEXPORT::GetSubstanceName
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETSUBSTANCENAME' :: GetSubstanceName

        use waq_omi_priv

        implicit none

        integer(kind = int_wp), intent(in) :: subid
        character(len = *), intent(out) :: name

        GetSubstanceName = 0
        name = '?'

    end function GetSubstanceName

    !> Get the index of a substance
    integer function GetSubstanceId(name)

        !DEC$ ATTRIBUTES DLLEXPORT::GetSubstanceId
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETSUBSTANCEID' :: GetSubstanceId

        use waq_omi_priv

        implicit none

        character(len = *), intent(in) :: name

        GetSubstanceId = 0

    end function GetSubstanceId

    !> Get the time period for the complete computation
    integer function GetTimeHorizon(startTime, stopTime)

        !DEC$ ATTRIBUTES DLLEXPORT::GetTimeHorizon
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETTIMEHORIZON' :: GetTimeHorizon

        use waq_omi_priv

        implicit none

        real(kind = kind(1.0d0)), intent(out) :: startTime
        real(kind = kind(1.0d0)), intent(out) :: stopTime

        real(kind = kind(1.0d0)) :: timeStep
        real(kind = kind(1.0d0)) :: currentTime

        call GetTimeParameters(startTime, stopTime, timeStep, currentTime)

        GetTimeHorizon = 0

    end function GetTimeHorizon

    !> Get the current time for the computation
    integer function GetWQCurrentTime(currentTime)

        !DEC$ ATTRIBUTES DLLEXPORT::GetWQCurrentTime
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETWQCURRENTTIME' :: GetWQCurrentTime

        use waq_omi_priv

        implicit none

        real(kind = kind(1.0d0)), intent(out) :: currentTime

        real(kind = kind(1.0d0)) :: startTime
        real(kind = kind(1.0d0)) :: stopTime
        real(kind = kind(1.0d0)) :: timeStep

        call GetTimeParameters(startTime, stopTime, timeStep, currentTime)
        GetWQCurrentTime = 0

    end function GetWQCurrentTime

    !> Get the next time in the computation
    integer function GetWQNextTime(nextTime)

        !DEC$ ATTRIBUTES DLLEXPORT::GetWQNextTime
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETWQNEXTTIME' :: GetWQNextTime

        use waq_omi_priv

        implicit none

        real(kind = dp), intent(out) :: nextTime

        real(kind = dp) :: startTime
        real(kind = dp) :: stopTime
        real(kind = dp) :: timeStep

        call GetTimeParameters(startTime, stopTime, timeStep, nextTime)
        nextTime = nextTime + timeStep

        GetWQNextTime = 0

    end function GetWQNextTime

end module waq_omi_api
