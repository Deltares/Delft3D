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

! Status: very temporary!
!-----------------------------------------------------------
!----------------------------------------------------------------

function SE_get_exchange_item_count() result(count)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_get_exchange_item_count

    include 'sysn_ff.inc'

    ! return value
    integer :: count ! # exchange items

    count = nosys*nobnd

end function SE_get_exchange_item_count

!-----------------------------------------------

function SE_get_exchange_item_id(role, location_id_c,quantity_id_c) result(id)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_get_exchange_item_id

    use m_openda_quantities

    use delwaq2_global_data

    implicit none

    include 'sysn_ff.inc'

    !arguments
    character(len=*)  :: location_id_c, quantity_id_c
    integer           :: role

    integer          :: location_id, quantity_id

    ! return value
    integer :: id ! # exchange id

    integer :: i

    quantity_id = -1
    do i = 1, notot
       if (substance_name(i) .eq. quantity_id_c) then
         quantity_id = i
       endif
    enddo

    location_id = -1
  !! todo: use boundary names
    if (role  == accepting) then
      if (location_id_c .eq. 'bnd1') location_id = 1
      if (location_id_c .eq. 'bnd2') location_id = 2
      if (location_id_c .eq. '56'  ) location_id = 56
    endif

    if (role == providing) then
    !! todo: use location names
      if (location_id_c .eq. 'links') location_id = 1
      if (location_id_c .eq. 'midden') location_id = 2
      if (location_id_c .eq. 'rechts') location_id = 3
      if (location_id_c .eq. '(31,4)') location_id = 56
    endif


    if ((quantity_id .ge. 0) .and. (location_id .ge.0)) then

      id = accepting * 10000 + location_id * 100 + quantity_id * 1
    else
      print *,'SE_get_exchange_item_id: location and/or quantity is not correct'
    endif

end function SE_get_exchange_item_id



!-----------------------------------------------
subroutine get_location_id_and_quantity_id(exchange_item_id, location_id,quantity_id)
    !arguments
    integer, intent(in) :: exchange_item_id
    integer, intent(out) :: location_id, quantity_id

    integer :: eid2

    eid2 = exchange_item_id
    if (exchange_item_id.ge. 10000) eid2 = exchange_item_id - 10000  ! providing
    location_id  = eid2 / 100
    quantity_id = mod(eid2, 100)

end subroutine get_location_id_and_quantity_id

!-------------------------------------------------

function SE_get_values_count_for_time_span(instance, exchange_item_id, start_time, end_time) result(ret_val)

    !DEC$ ATTRIBUTES DLLEXPORT :: SE_get_values_count_for_time_span


    ! return value
    integer :: ret_val

    ! arguments
    integer         , intent(in) :: instance         ! model instance
    integer         , intent(in) :: exchange_item_id ! type and location of quantity
                                                     ! (e.g. discharge or waterlevel at point M7)
    double precision, intent(in) :: start_time       ! start time of values
    double precision, intent(in) :: end_time         ! end time of values

    ! locals


    ret_val = 1
  ! TODO: better implementation

end function SE_get_values_count_for_time_span

!-----------------------------------------------------------


function SE_get_values_for_time_span(exchange_item_id, start_time, end_time, nvals,values) result(ret_val)

    !DEC$ ATTRIBUTES DLLEXPORT :: SE_get_values_for_time_span


    use m_openda_quantities
    use delwaq2_global_data

    implicit none

    include 'sysa_ff.inc' !voor iconc
    include 'sysn_ff.inc' ! voor notot

    ! return value
    integer :: ret_val

    ! arguments

    integer         , intent(in) :: exchange_item_id ! ! type and location of quantity
                                                     ! (e.g. discharge or waterlevel at point M7)
    double precision, intent(in) :: start_time       ! start time of bc values
    double precision, intent(in) :: end_time         ! end time of bc values
    integer,          intent(in) :: nvals            ! size of values array
    double precision, &
        dimension(nvals), &
                    intent(inout) :: values            ! returned values

    integer :: location_id, quantity_id

    ret_val = -1 ! indices not ok

    call get_location_id_and_quantity_id(exchange_item_id,location_id,quantity_id)

    ! note: for now, all quantities are 1D so values is always one-dimensional.

    if (quantity_id .le. notot) then

   ! note: quantity_id is the index corresponding to the substance
   ! location_id is simply the segment number
       values(1) = dlwqd%rbuf(iconc+quantity_id-1+(location_id-1)*notot)

       ret_val = 0
    endif

    if (ret_val /= 0) then
        write(*,'(A,I2)') 'Error in get_values_for_time_span: ', ret_val
    endif

end function SE_get_values_for_time_span

!------------------------------------------------------------
function SE_set_noise_for_time_span(exchange_item_id, start_time, end_time, operation, nvals,values) result(ret_val)


    !DEC$ ATTRIBUTES DLLEXPORT :: SE_set_noise_for_time_span


    use m_openda_quantities
    use m_openda_exchange_items, only : set_openda_buffer
    use delwaq2_global_data

    implicit none

    include 'sysn_ff.inc' ! voor notot

    ! return value
    integer :: ret_val

    ! arguments
    integer         , intent(in) :: exchange_item_id ! ! type and location of quantity
                                                     ! (e.g. discharge or waterlevel at point M7)
    double precision, intent(in) :: start_time       ! start time of bc values
    double precision, intent(in) :: end_time         ! end time of bc values
    integer,          intent(in) :: operation        ! operation: oper_multiply, oper_add, oper_set
    integer,          intent(in) :: nvals            ! size of values array
    double precision, &
        dimension(nvals), &
                    intent(in) :: values            ! returned values

    ! locals
    integer           :: location_id, quantity_id


    ret_val = -1 ! indices not ok

    call get_location_id_and_quantity_id(exchange_item_id,location_id,quantity_id)

    ! note: for now, all quantities are 1D so values is always one-dimensional.
    if (quantity_id .le. notot) then

       call set_openda_buffer(values(1),location_id, quantity_id, operation)
       ret_val = 0
    endif

    if (ret_val /= 0) then
        write(*,'(A,I2)') 'Error in set_values_for_time_span: ', ret_val
    else
        write(*,'(A,I4,A)') 'set_values_for_time_span:', &
                                  exchange_item_id,&
                                     '):'
        write(*,*) 'delta:   ', values
    endif

end function SE_set_noise_for_time_span

!----------------------------------------------------------------
