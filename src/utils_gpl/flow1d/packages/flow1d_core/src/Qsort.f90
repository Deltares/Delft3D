module qsort
!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------
!  
!  
!-------------------------------------------------------------------------------

implicit none

private

public d_qsort

type      :: tp_stack
  integer low
  integer hi
end type tp_stack

type (tp_stack), allocatable        :: udt_stack(:)       ! our stack
type (tp_stack), allocatable        :: udt_stack_buf(:)   ! stack buffer for reallocation

contains

subroutine d_qsort(dbl_values, icount)

!************************************************************************************
!
! Version for Sorting Doubles Ascending
!
! Iterative QuickSort (rather than recursive) by Cornel Huth.
! Adapted to VB by André Hendriks: used VB6 types and made 'stack' size extendable.
! Tests showed that with an initial stacksize of 1024, extending the stack was not
! needed when sorting 30000 records. This implementation is 100 times faster than
! the old routine in this module (QuickSort) and is not running out of stack space
! when sorting 8000+ records.
! Translated to FORTRAN90 by Jaap Zeekant
!
! Arguments:
!
! Name                  I/O  Description
! --------------------  ---  --------------------------------------
! dbl_values             I/O  The one-dimensional array to sort.
!
!************************************************************************************
use MessageHandling

integer                             :: icount
double precision                    :: dbl_values(icount)

integer                             :: lng_stacksize     ! stack (increment) size
parameter (lng_stacksize = 1000)

integer                             :: lng_lower
integer                             :: lng_upper
integer                             :: i
integer                             :: j
integer                             :: lng_hi
integer                             :: lng_low
integer                             :: lng_mid

integer                             :: lng_sp            ! stack pointer

double precision                    :: dbl_compare

integer                             :: ierr

! Allocate
allocate(udt_stack(lng_stacksize), stat=ierr)
if ( ierr .ne. 0 ) then
  call setMessage(LEVEL_FATAL, 'Error Allocating Stack Space for Quick Sort')
endif


lng_lower = 1
lng_upper = icount

lng_sp = 1

udt_stack(lng_sp)%low = lng_lower
udt_stack(lng_sp)%hi  = lng_upper

lng_sp = lng_sp + 1

do

  lng_sp  = lng_sp - 1
  lng_low = udt_stack(lng_sp)%low
  lng_hi  = udt_stack(lng_sp)%hi

  do

    i = lng_low
    j = lng_hi

    lng_mid = (lng_low + lng_hi) / 2

    dbl_compare = dbl_values(lng_mid)

    do

      do while (dbl_values(i) < dbl_compare)
        i = i + 1
      enddo

      do while (dbl_values(j) > dbl_compare)
        j = j - 1
      enddo

      if (i <= j) then
        call d_swap(dbl_values(i), dbl_values(j))
        i = i + 1
        j = j - 1
      end if

      if (i > j) exit


    enddo   ! while i <= j

    if (j - lng_low < lng_hi - i) then

      if (i < lng_hi) then

        udt_stack(lng_sp)%low = i
        udt_stack(lng_sp)%hi = lng_hi
        lng_sp = lng_sp + 1

        if (lng_sp > lng_stacksize) then
          call increase_stack(lng_stacksize)
        endif

      endif
      lng_hi = j

    else

      if (lng_low < j) then

        udt_stack(lng_sp)%low = lng_low
        udt_stack(lng_sp)%hi = j
        lng_sp = lng_sp + 1

        if (lng_sp > lng_stacksize) then
          call increase_stack(lng_stacksize)
        end if

      end if

      lng_low = i

    end if

    if (lng_low >= lng_hi) exit

  enddo ! while (lng_low < lng_hi)

  if (lng_sp == 1) exit

enddo  ! while (lng_sp /= 1)

! Deallocate Stack
deallocate(udt_stack, stat=ierr)
if ( ierr .ne. 0 ) then
  call setMessage(LEVEL_FATAL, 'Error Deallocating Stack Space for Quick Sort')
endif

end subroutine d_qsort

! Swap Data
subroutine d_swap(a, b)

double precision   :: a, b, temp

temp = a
a = b
b = temp

end subroutine d_swap

! Increase Stack Size
subroutine increase_stack(i_stacksize)

use MessageHandling

integer                   :: i_stacksize
integer                   :: i_old_size
integer                   :: i_new_size

integer                   :: ierr

i_old_size = i_stacksize
i_new_size = 2 * i_stacksize

! Allocate Stack Buffer
allocate(udt_stack_buf(i_old_size), stat=ierr)
if ( ierr .ne. 0 ) then
  call setMessage(LEVEL_FATAL, 'Error Allocating Stack Space Buffer for Quick Sort')
endif

udt_stack_buf = udt_stack

! Deallocate Stack
deallocate(udt_stack, stat=ierr)
if ( ierr .ne. 0 ) then
  call setMessage(LEVEL_FATAL, 'Error Deallocating Stack Space for Quick Sort')
endif

! Reallocate Stack Buffer
allocate(udt_stack_buf(i_new_size), stat=ierr)
if ( ierr .ne. 0 ) then
  call setMessage(LEVEL_FATAL, 'Error Reallocating Stack Space for Quick Sort')
endif

udt_stack(:)%low = 0
udt_stack(:)%hi = 0

udt_stack(1:i_old_size) = udt_stack_buf(1:i_old_size)

! Deallocate Stack Buffer
deallocate(udt_stack_buf, stat=ierr)
if ( ierr .ne. 0 ) then
  call setMessage(LEVEL_FATAL, 'Error deallocating Stack Space Buffer for Quick Sort')
endif

! return new Stack Size
i_stacksize = i_new_size

end subroutine increase_stack

end module
