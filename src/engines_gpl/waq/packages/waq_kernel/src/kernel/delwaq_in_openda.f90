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

module m_openda_quantities
! quantity-id's : in (from delwaq to openDA)

integer, parameter :: accepting = 0


!quantity-id's : out (from openDA to delwaq)
integer, parameter :: providing = 1

!todo:  this should be variable
integer, parameter :: max_quantity_ids = 30   ! account for several substances
integer, parameter :: max_location_ids = 30


! possible operation for modifying the boundary values
integer, parameter :: oper_set      = 1
integer, parameter :: oper_add      = 2
integer, parameter :: oper_multiply = 3

contains


! todo: implement this!
subroutine openda_quantities_initialize(notot,nobnd)

  implicit none

  integer notot,nobnd

!  max_quantity_ids = notot
!  max_location_ids = nobnd

end subroutine openda_quantities_initialize

end module m_openda_quantities

!-----------------------------------

module m_openda_exchange_items

use m_openda_quantities

logical, save :: l_ei(max_location_ids,max_quantity_ids) = .false.

logical, save :: doLogging = .false.

double precision, save :: ei_val(max_location_ids,max_quantity_ids)
integer, save          :: ei_oper(max_location_ids,max_quantity_ids)

contains

!------------------------

  subroutine set_openda_buffer(val, location_id,quantity_id, operation)
! set the value of an exchange-item. This routine is typically called
! by an SE_setvalues routine (from OUTSIDE delft3D!) for a certain instance and exchange item (e.g. wind)
! In the case of forcings, the value can be a multiplier (1 + epsilon)
! Note that the actual adjustment INSIDE delft3d is not yet performed here.
! for the wind example, this is done in incmeteo with a call to get_openda_buffer.
! The multiplication is performed in this routine get_openda_buffer.

! For a specific boundary, the location_id is used (can be found by counting in the BND-file)
! for other boundaries, the l_ei remains false so they do not change.

  implicit none

  integer, intent(in)          :: location_id   !   location identifier
  integer, intent(in)          :: quantity_id   !   quantity identifier
  double precision, intent(in) :: val           !   value to be set
  integer, intent(in)          :: operation     !   operation: oper_multiply, oper_add, oper_set

  if (doLogging) then
     write (*,*) 'set_openda_buffer, loc-id=', location_id, ', q_id=', quantity_id, ', val=', val, ', oper:', operation
     call flush(6)
  endif
  l_ei(location_id,quantity_id) = .true.
  ei_val(location_id,quantity_id) = val
  ei_oper(location_id,quantity_id) = operation

  end subroutine set_openda_buffer
!---------------------------------------------------

  subroutine get_openda_buffer(quantity, loc_from_waq, dim1, dim2, qarray)

  use m_sysn          ! System characteristics
  
  implicit none

  integer, intent(in)    :: dim1, dim2, loc_from_waq
  integer , intent (in) :: quantity
  real    , dimension(dim1, dim2), target, intent(out) :: qarray

  ! locals
  integer          :: location_id, quantity_id
  double precision :: org_value

  location_id = loc_from_waq
  quantity_id = -1

! first check the substance

    if (quantity .le. notot) then
       quantity_id = quantity
     endif


! Now, assume that we only support the setting of boundary conditions!

!! TODO: allow other variants of get_openda_buffer

  if (location_id == -1 .or. quantity_id == -1) then
       write (*,*) 'EI get_openda_buffer, INVALID ITEM: loc-id=', location_id, ', q_id=', quantity_id
  else
    if (l_ei(location_id,quantity_id)) then
       org_value = qarray(1,1)
       select case (ei_oper(location_id,quantity_id))
       case(oper_set)
           qarray = ei_val(location_id, quantity_id)
       case(oper_add)
           qarray = qarray + ei_val(location_id, quantity_id)
       case(oper_multiply)
           qarray = qarray * ei_val(location_id, quantity_id)
       case default
          write (*,*) 'get_openda_buffer: UNKNOWN OPERATION type: ', ei_oper(location_id,quantity_id)
       endselect
       if (doLogging) then
          write (*,*) 'EI adjusted, loc-id=', location_id, ', q_id=', quantity_id, ', was: ', org_value, ', is:', qarray(1,1)
          call flush(6)
       endif
    endif
  endif

  end subroutine get_openda_buffer

! ------------------


   subroutine openda_buffer_initialize

   implicit none

   l_ei = .false.

   end subroutine openda_buffer_initialize

!--------------------------
end module m_openda_exchange_items


