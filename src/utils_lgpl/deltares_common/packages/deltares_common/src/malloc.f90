!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2023.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!> Utility routines for memory (re)allocation.
module m_alloc
implicit none
private 

public realloc, reallocP, aerr, allocSize

! TODO: Handle nondefault kinds properly? [AvD]

!> Reallocates memory for an existing array. Arrays of most intrinsic
!! data types up to rank 4 are accepted and they may still be unallocated.
!! realloc is mainly intended for increasing array sizes, but it may also
!! be used for \e decreasing them. Use m_alloc::realloc for allocatable arrays and
!! use m_alloc::reallocP for pointer arrays<sup>1</sup>.
!!
!! The actual values in the new array depend on two optional parameters:
!! \a keepExisting and \a fill.
!! By default, where the old and new dimensions overlap, the original array
!! data is preserved (i.e., for a larger upperbound, all data is preserved).
!! This behaviour can be switched off by passing the optional argument
!! <tt>keepExisting=.false.</tt> (for example, to prevent unnecessary data copy).
!!
!! An optional fill value may be specified to set the non-overlapping
!! elements. For example: <tt>call realloc(x, newmax, stat=istat, fill=-999d0)</tt>
!! The original array elements are NOT overwritten by \a fill, unless
!! <tt>keepExisting=.false.</tt>
!!
!! When <tt>keepExisting=.false.</tt> and no fill value is specified,
!! the resulting values are unspecified<sup>2</sup>.
!!
!! <b>Example usage:</b>\code
!!   integer, allocatable :: iarr(:), itens(:,:,:)
!!   call realloc(iarr, 100)
!!   call realloc(iarr, 1000, fill = -1, keepExisting=.false.)
!!   allocate(itens(10,20,30))
!!   call realloc(itens, (/ 100, 200, 300 /), fill = 0)
!! \endcode
!! 
!! \param[in,out] arr Array (up to rank 4) to be reallocated.
!! \param[in]     uindex Desired new size (upper index) for array, scalar
!!      when arr has rank 1, or rank 1 array with size ra when arr
!!      has rank ra>1.
!! \param[in]     lindex (optional) Lower index for new array, defaults
!!      to lindex(1:ra)==1.
!! \param[out]   stat (optional) Result status of allocate command for the
!!      array.
!! \param[in]     fill (optional) Scalar value to fill any empty spots in
!!      the new array. Empty spots occur when the new size is larger than
!!      the old size, or when keepExisting==.false.
!! \param[in]     shift (optional) Shift original data by this increment in
!!      the new array, defaults to shift(1:ra)==0.
!! \param[in]     keepExisting (optional) Whether to preserve the original
!!      data in arr (defaults to .true.). When set to .false. and the
!!      parameter fill is not present, the resulting data is unspecified.
!!
!! <small>(1. Although the Intel compiler is able to
!! distinguish interfaces with allocatable and pointer arrays, the official
!! FORTRAN 2003 standard does not support distinguishing interfaces based on the
!! allocatable/pointer attribute; therefore, the two sets of routines have
!! been put into separate interfaces. The routine syntax is identical for
!! realloc and reallocP.)</small>
!!
!! <small>(2. When the array size remains identical to the original and
!! \a keepExisting is either true or false, and \a fill is not present
!! the original array is preserved anyway, to prevent unnecessary assignments.
!! This is not a guaranteed feature and is subject to change.)</small>
interface realloc
   module procedure reallocInt
   module procedure reallocInt2
   module procedure reallocInt2x
   module procedure reallocInt3
   module procedure reallocInt4
   module procedure reallocCharacter
   module procedure reallocCharacter2
   module procedure reallocCharacter2x
   module procedure reallocCharacter3
   module procedure reallocCharacter4
   module procedure reallocString
   module procedure reallocReal
   module procedure reallocReal2
   module procedure reallocReal2x
   module procedure reallocReal3
   module procedure reallocReal3x
   module procedure reallocReal4
   module procedure reallocDouble
   module procedure reallocDouble2
   module procedure reallocDouble2x
   module procedure reallocDouble3
   module procedure reallocDouble4
   module procedure reallocLogical
   module procedure reallocLogical2
   module procedure reallocLogical3
   module procedure reallocLogical4
   module procedure reallocByte2
end interface

!> Reallocates memory for an existing \a pointer array. behaviour and arguments
!! are identical to \ref m_alloc::realloc.
interface reallocP
   module procedure reallocPInt
   module procedure reallocPInt2
   module procedure reallocPInt3
   module procedure reallocPInt4
   module procedure reallocPCharacter
   module procedure reallocPCharacter2
   module procedure reallocPCharacter3
   module procedure reallocPCharacter4
   module procedure reallocPReal
   module procedure reallocPReal2
   module procedure reallocPReal3
   module procedure reallocPReal4
   module procedure reallocPDouble
   module procedure reallocPDouble2
   module procedure reallocPDouble3
   module procedure reallocPDouble4
   module procedure reallocPLogical
   module procedure reallocPLogical2
   module procedure reallocPLogical3
   module procedure reallocPLogical4
end interface

interface allocSize
   module procedure allocSizeDouble
end interface

contains
!
!
!
!===============================================================================
!> Emit an allocation error message *if* an allocation error has occurred.
!! The error message goes through the MessageHandling output channels, as configured by the calling application.
subroutine aerr(name, iostat, isize, errmsg)
   use MessageHandling, only: msgbuf, dbg_flush, err_flush
   use precision

   character(len=*), intent(in)  :: name   !< Name of the allocated array(s) or other description.
   integer,          intent(in)  :: iostat !< IO status as returned by ALLOCATE(..stat=iostat) statement. When zero, do nothing.
   integer,          intent(in)  :: isize  !< Size (nr of bytes divided by 8) of original ALLOCATE statement (i.e., for double precision arrays simply the array length).
   character(len=*), intent(in), optional :: errmsg !< Optional error message as returned by ALLOCATE(..errmsg=errormsg) statement

   real(kind=hp), save :: rmemtot = 0d0

   integer      :: i3

   if (iostat==0) then
!$OMP CRITICAL
      rmemtot = rmemtot + isize
      i3 = 8*rmemtot*1e-6   ! convert size (in double/8 byte units) to megabytes
      if (abs(isize) > 1000) then
         write (msgbuf,*) i3, isize*1e-6, ' ', trim(name)
         call dbg_flush()
      endif
!$OMP END CRITICAL
   else
      if (present(errmsg)) then
         write (msgbuf,*) ' Allocation Error: ', trim(name), ', Allocate status = ', iostat, ', Integer parameter = ', isize, '=>', trim(errmsg)
      else
         write (msgbuf,*) ' Allocation Error: ', trim(name), ', Allocate status = ', iostat, ', Integer parameter = ', isize
      end if
      call err_flush()
endif

end subroutine aerr
!
!
!
!===============================================================================
subroutine reallocReal2x(arr, u1, u2, l1, l2, stat, keepExisting)
   real, allocatable, intent(inout)             :: arr(:, :)
   integer                                      :: u1, u2
   integer, optional                            :: l1, l2
   integer                                      :: uindex(2)
   integer                                      :: lindex(2)
   integer, intent(out), optional               :: stat
   logical, intent(in), optional                :: keepExisting

   uindex = (/u1, u2/)
   if (present(l1)) then
      lindex = (/l1, l2/)
      call reallocReal2(arr, uindex, lindex, stat = stat)
   else
      call reallocReal2(arr, uindex, stat = stat)
   endif
end subroutine reallocReal2x
!
!
!
!===============================================================================
subroutine reallocDouble2x(arr, u1, u2, l1, l2, stat)
   double precision, allocatable, intent(inout)             :: arr(:, :)
   integer                                      :: u1, u2
   integer, optional                            :: l1, l2
   integer                                      :: uindex(2)
   integer                                      :: lindex(2)
   integer, intent(out), optional               :: stat

   uindex = (/u1, u2/)
   if (present(l1)) then
      lindex = (/l1, l2/)
      call reallocDouble2(arr, uindex, lindex, stat = stat)
   else
      call reallocDouble2(arr, uindex, stat = stat)
   endif
end subroutine reallocDouble2x
!
!
!
!===============================================================================
subroutine reallocInt2x(arr, u1, u2, l1, l2, stat)
   integer, allocatable, intent(inout)          :: arr(:, :)
   integer                                      :: u1, u2
   integer, optional                            :: l1, l2
   integer                                      :: uindex(2)
   integer                                      :: lindex(2)
   integer, intent(out), optional               :: stat

   uindex = (/u1, u2/)
   if (present(l1)) then
      lindex = (/l1, l2/)
      call reallocInt2(arr, uindex, lindex, stat = stat)
   else
      call reallocInt2(arr, uindex, stat = stat)
   endif
end subroutine reallocInt2x
!
!
!
!===============================================================================
subroutine reallocCharacter2x(arr, u1, u2, l1, l2, stat)
   character(len=*), allocatable, intent(inout) :: arr(:, :)
   integer                                      :: u1, u2
   integer, optional                            :: l1, l2
   integer                                      :: uindex(2)
   integer                                      :: lindex(2)
   integer, intent(out), optional               :: stat

   uindex = (/u1, u2/)
   if (present(l1)) then
      lindex = (/l1, l2/)
      call reallocCharacter2(arr, uindex, lindex, stat = stat)
   else
      call reallocCharacter2(arr, uindex, stat = stat)
   endif
end subroutine reallocCharacter2x
!
!
!
!===============================================================================
subroutine reallocReal3x(arr, u1, u2, u3, l1, l2, l3, stat)
   real, allocatable, intent(inout)             :: arr(:, :, :)
   integer                                      :: u1, u2, u3
   integer, optional                            :: l1, l2, l3
   integer                                      :: uindex(3)
   integer                                      :: lindex(3)
   integer, intent(out), optional               :: stat

   uindex = (/u1, u2, u3/)
   if (present(l1)) then
      lindex = (/l1, l2, l3/)
      call reallocReal3(arr, uindex, lindex, stat = stat)
   else
      call reallocReal3(arr, uindex, stat = stat)
   endif
end subroutine reallocReal3x
!
!
!
!===============================================================================
subroutine reallocPInt(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   integer, pointer,  intent(inout) :: arr(:)
   integer,           intent(in)    :: uindex
   integer, optional, intent(in)    :: lindex
   integer, optional, intent(out)   :: stat
   integer, optional, intent(in)    :: fill
   integer, optional, intent(in)    :: shift
   logical, optional, intent(in)    :: keepExisting

   integer, pointer :: b(:)
   integer          :: uind, lind, muind, mlind, lindex_, shift_, i
   integer          :: localErr
   logical          :: docopy
   logical          :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = 1
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = 0
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   nullify(b)
   if (associated(arr)) then
      uind = ubound(arr,1)
      lind = lbound(arr,1)
      equalSize = (uindex == uind) .and. (lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. shift_==0) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         b => arr
         nullify(arr)
      elseif (.not.equalSize) then
         deallocate(arr, stat = localErr)
      endif
   endif
   if (.not.associated(arr) .and. localErr==0) then
       allocate(arr(lindex_:uindex), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (associated(b) .and. localErr==0 .and. size(b)>0) then
      do i=mlind, muind
         arr(i) = b(i-shift_)
      end do
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocPInt
!
!
!
!===============================================================================
subroutine reallocInt(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   integer, allocatable, intent(inout)             :: arr(:)
   integer, intent(in)                          :: uindex
   integer, intent(in), optional                :: lindex
   integer, intent(out), optional               :: stat
   integer, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift
   logical, intent(in), optional                :: keepExisting

   integer, allocatable                            :: b(:)
   integer        :: uind, lind, muind, mlind, lindex_, shift_

   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = 1
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = 0
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   if (allocated(arr)) then
      uind = ubound(arr,1)
      lind = lbound(arr,1)
      equalSize = (uindex == uind) .and. (lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. shift_==0) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         allocate (b(mlind:muind))
        b(mlind:muind) = arr(mlind-shift_:muind-shift_)
      endif
      if (.not.equalSize) deallocate(arr, stat = localErr)
   endif
   if (.not.allocated(arr) .and. localErr==0) then
       allocate(arr(lindex_:uindex), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (allocated(b) .and. localErr==0 .and. size(b)>0) then
      arr(mlind:muind) = b(mlind:muind)
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocInt
!
!
!
!===============================================================================
subroutine reallocPInt2(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   integer, pointer, intent(inout)                 :: arr(:,:)
   integer, intent(in)                          :: uindex(2)
   integer, intent(in), optional                :: lindex(2)
   integer, intent(out), optional               :: stat
   integer, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(2)
   logical, intent(in), optional                :: keepExisting

   integer, pointer                                :: b(:,:)
   integer        :: uind(2), lind(2), muind(2), mlind(2), lindex_(2), shift_(2)
   integer        :: i1,i2
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   nullify(b)
   if (associated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         b => arr
         nullify(arr)
      elseif (.not.equalSize) then
         deallocate(arr, stat = localErr)
      endif
   endif
   if (.not.associated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (associated(b) .and. localErr==0 .and. size(b)>0) then
      do i2 = mlind(2),muind(2)
         do i1 = mlind(1),muind(1)
             arr(i1,i2) = b(i1-shift_(1),i2-shift_(2))
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocPInt2
!
!
!
!===============================================================================
subroutine reallocInt2(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   integer, allocatable, intent(inout)             :: arr(:,:)
   integer, intent(in)                          :: uindex(2)
   integer, intent(in), optional                :: lindex(2)
   integer, intent(out), optional               :: stat
   integer, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(2)
   logical, intent(in), optional                :: keepExisting

   integer, allocatable                            :: b(:,:)
   integer        :: uind(2), lind(2), muind(2), mlind(2), lindex_(2), shift_(2)
   integer        :: i1,i2
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   if (allocated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         allocate (b(mlind(1):muind(1),mlind(2):muind(2)))
            do i2 = mlind(2),muind(2)
               do i1 = mlind(1),muind(1)
                   b(i1,i2) = arr(i1-shift_(1),i2-shift_(2))
               enddo
            enddo
      endif
      if (.not.equalSize) deallocate(arr, stat = localErr)
   endif
   if (.not.allocated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (allocated(b) .and. localErr==0 .and. size(b)>0) then
      do i2 = mlind(2),muind(2)
         do i1 = mlind(1),muind(1)
             arr(i1,i2) = b(i1,i2)
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocInt2
!
!
!
!===============================================================================
subroutine reallocPInt3(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   integer, pointer, intent(inout)                 :: arr(:,:,:)
   integer, intent(in)                          :: uindex(3)
   integer, intent(in), optional                :: lindex(3)
   integer, intent(out), optional               :: stat
   integer, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(3)
   logical, intent(in), optional                :: keepExisting

   integer, pointer                                :: b(:,:,:)
   integer        :: uind(3), lind(3), muind(3), mlind(3), lindex_(3), shift_(3)
   integer        :: i1,i2,i3
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   nullify(b)
   if (associated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         b => arr
         nullify(arr)
      elseif (.not.equalSize) then
         deallocate(arr, stat = localErr)
      endif
   endif
   if (.not.associated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2),lindex_(3):uindex(3)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (associated(b) .and. localErr==0 .and. size(b)>0) then
      do i3 = mlind(3),muind(3)
         do i2 = mlind(2),muind(2)
            do i1 = mlind(1),muind(1)
                arr(i1,i2,i3) = b(i1-shift_(1),i2-shift_(2),i3-shift_(3))
            enddo
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocPInt3
!
!
!
!===============================================================================
subroutine reallocInt3(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   integer, allocatable, intent(inout)             :: arr(:,:,:)
   integer, intent(in)                          :: uindex(3)
   integer, intent(in), optional                :: lindex(3)
   integer, intent(out), optional               :: stat
   integer, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(3)
   logical, intent(in), optional                :: keepExisting

   integer, allocatable                            :: b(:,:,:)
   integer        :: uind(3), lind(3), muind(3), mlind(3), lindex_(3), shift_(3)
   integer        :: i1,i2,i3
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   if (allocated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         allocate (b(mlind(1):muind(1),mlind(2):muind(2),mlind(3):muind(3)))
            do i3 = mlind(3),muind(3)
               do i2 = mlind(2),muind(2)
                  do i1 = mlind(1),muind(1)
                      b(i1,i2,i3) = arr(i1-shift_(1),i2-shift_(2),i3-shift_(3))
                  enddo
               enddo
            enddo
      endif
      if (.not.equalSize) deallocate(arr, stat = localErr)
   endif
   if (.not.allocated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2),lindex_(3):uindex(3)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (allocated(b) .and. localErr==0 .and. size(b)>0) then
      do i3 = mlind(3),muind(3)
         do i2 = mlind(2),muind(2)
            do i1 = mlind(1),muind(1)
                arr(i1,i2,i3) = b(i1,i2,i3)
            enddo
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocInt3
!
!
!
!===============================================================================
subroutine reallocPInt4(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   integer, pointer, intent(inout)                 :: arr(:,:,:,:)
   integer, intent(in)                          :: uindex(4)
   integer, intent(in), optional                :: lindex(4)
   integer, intent(out), optional               :: stat
   integer, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(4)
   logical, intent(in), optional                :: keepExisting

   integer, pointer                                :: b(:,:,:,:)
   integer        :: uind(4), lind(4), muind(4), mlind(4), lindex_(4), shift_(4)
   integer        :: i1,i2,i3,i4
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1, 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0, 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   nullify(b)
   if (associated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         b => arr
         nullify(arr)
      elseif (.not.equalSize) then
         deallocate(arr, stat = localErr)
      endif
   endif
   if (.not.associated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2),lindex_(3):uindex(3),lindex_(4):uindex(4)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (associated(b) .and. localErr==0 .and. size(b)>0) then
      do i4 = mlind(4),muind(4)
         do i3 = mlind(3),muind(3)
            do i2 = mlind(2),muind(2)
               do i1 = mlind(1),muind(1)
                   arr(i1,i2,i3,i4) = b(i1-shift_(1),i2-shift_(2),i3-shift_(3),i4-shift_(4))
               enddo
            enddo
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocPInt4
!
!
!
!===============================================================================
subroutine reallocInt4(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   integer, allocatable, intent(inout)             :: arr(:,:,:,:)
   integer, intent(in)                          :: uindex(4)
   integer, intent(in), optional                :: lindex(4)
   integer, intent(out), optional               :: stat
   integer, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(4)
   logical, intent(in), optional                :: keepExisting

   integer, allocatable                            :: b(:,:,:,:)
   integer        :: uind(4), lind(4), muind(4), mlind(4), lindex_(4), shift_(4)
   integer        :: i1,i2,i3,i4
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1, 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0, 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   if (allocated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         allocate (b(mlind(1):muind(1),mlind(2):muind(2),mlind(3):muind(3),mlind(4):muind(4)))
            do i4 = mlind(4),muind(4)
               do i3 = mlind(3),muind(3)
                  do i2 = mlind(2),muind(2)
                     do i1 = mlind(1),muind(1)
                         b(i1,i2,i3,i4) = arr(i1-shift_(1),i2-shift_(2),i3-shift_(3),i4-shift_(4))
                     enddo
                  enddo
               enddo
            enddo
      endif
      if (.not.equalSize) deallocate(arr, stat = localErr)
   endif
   if (.not.allocated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2),lindex_(3):uindex(3),lindex_(4):uindex(4)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (allocated(b) .and. localErr==0 .and. size(b)>0) then
      do i4 = mlind(4),muind(4)
         do i3 = mlind(3),muind(3)
            do i2 = mlind(2),muind(2)
               do i1 = mlind(1),muind(1)
                   arr(i1,i2,i3,i4) = b(i1,i2,i3,i4)
               enddo
            enddo
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocInt4
!
!
!
!===============================================================================
subroutine reallocPCharacter(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   character(len=*), pointer,  intent(inout) :: arr(:)
   integer,                    intent(in)    :: uindex
   integer,          optional, intent(in)    :: lindex
   integer,          optional, intent(out)   :: stat
   character(len=*), optional, intent(in)    :: fill
   integer,          optional, intent(in)    :: shift
   logical,          optional, intent(in)    :: keepExisting

   character(len=len(arr)), pointer :: b(:)
   integer                          :: uind, lind, muind, mlind, lindex_, shift_, i
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = 1
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = 0
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   nullify(b)
   if (associated(arr)) then
      uind = ubound(arr,1)
      lind = lbound(arr,1)
      equalSize = (uindex == uind) .and. (lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. shift_==0) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         b => arr
         nullify(arr)
      elseif (.not.equalSize) then
         deallocate(arr, stat = localErr)
      endif
   endif
   if (.not.associated(arr) .and. localErr==0) then
       allocate(arr(lindex_:uindex), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (associated(b) .and. localErr==0 .and. size(b)>0) then
      do i=mlind, muind
         arr(i) = b(i-shift_)
      end do
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocPCharacter
!
!
!
!===============================================================================
subroutine reallocCharacter(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   character(len=*), allocatable, intent(inout)             :: arr(:)
   integer, intent(in)                          :: uindex
   integer, intent(in), optional                :: lindex
   integer, intent(out), optional               :: stat
   character(len=*), intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift
   logical, intent(in), optional                :: keepExisting

   character(len=len(arr)), allocatable                            :: b(:)
   integer        :: uind, lind, muind, mlind, lindex_, shift_

   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = 1
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = 0
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   if (allocated(arr)) then
      uind = ubound(arr,1)
      lind = lbound(arr,1)
      equalSize = (uindex == uind) .and. (lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. shift_==0) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         allocate (b(mlind:muind))
        b(mlind:muind) = arr(mlind-shift_:muind-shift_)
      endif
      if (.not.equalSize) deallocate(arr, stat = localErr)
   endif
   if (.not.allocated(arr) .and. localErr==0) then
       allocate(arr(lindex_:uindex), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (allocated(b) .and. localErr==0 .and. size(b)>0) then
      arr(mlind:muind) = b(mlind:muind)
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocCharacter
!
!
!
!===============================================================================
subroutine reallocPCharacter2(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   character(len=*), pointer, intent(inout)                 :: arr(:,:)
   integer, intent(in)                          :: uindex(2)
   integer, intent(in), optional                :: lindex(2)
   integer, intent(out), optional               :: stat
   character(len=*), intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(2)
   logical, intent(in), optional                :: keepExisting

   character(len=len(arr)), pointer                                :: b(:,:)
   integer        :: uind(2), lind(2), muind(2), mlind(2), lindex_(2), shift_(2)
   integer        :: i1,i2
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   nullify(b)
   if (associated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         b => arr
         nullify(arr)
      elseif (.not.equalSize) then
         deallocate(arr, stat = localErr)
      endif
   endif
   if (.not.associated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (associated(b) .and. localErr==0 .and. size(b)>0) then
      do i2 = mlind(2),muind(2)
         do i1 = mlind(1),muind(1)
             arr(i1,i2) = b(i1-shift_(1),i2-shift_(2))
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocPCharacter2
!
!
!
!===============================================================================
subroutine reallocCharacter2(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   character(len=*), allocatable, intent(inout)             :: arr(:,:)
   integer, intent(in)                          :: uindex(2)
   integer, intent(in), optional                :: lindex(2)
   integer, intent(out), optional               :: stat
   character(len=*), intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(2)
   logical, intent(in), optional                :: keepExisting

   character(len=len(arr)), allocatable                            :: b(:,:)
   integer        :: uind(2), lind(2), muind(2), mlind(2), lindex_(2), shift_(2)
   integer        :: i1,i2
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   if (allocated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         allocate (b(mlind(1):muind(1),mlind(2):muind(2)))
            do i2 = mlind(2),muind(2)
               do i1 = mlind(1),muind(1)
                   b(i1,i2) = arr(i1-shift_(1),i2-shift_(2))
               enddo
            enddo
      endif
      if (.not.equalSize) deallocate(arr, stat = localErr)
   endif
   if (.not.allocated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (allocated(b) .and. localErr==0 .and. size(b)>0) then
      do i2 = mlind(2),muind(2)
         do i1 = mlind(1),muind(1)
             arr(i1,i2) = b(i1,i2)
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocCharacter2
!
!
!
!===============================================================================
subroutine reallocPCharacter3(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   character(len=*), pointer, intent(inout)                 :: arr(:,:,:)
   integer, intent(in)                          :: uindex(3)
   integer, intent(in), optional                :: lindex(3)
   integer, intent(out), optional               :: stat
   character(len=*), intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(3)
   logical, intent(in), optional                :: keepExisting

   character(len=len(arr)), pointer                                :: b(:,:,:)
   integer        :: uind(3), lind(3), muind(3), mlind(3), lindex_(3), shift_(3)
   integer        :: i1,i2,i3
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   nullify(b)
   if (associated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         b => arr
         nullify(arr)
      elseif (.not.equalSize) then
         deallocate(arr, stat = localErr)
      endif
   endif
   if (.not.associated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2),lindex_(3):uindex(3)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (associated(b) .and. localErr==0 .and. size(b)>0) then
      do i3 = mlind(3),muind(3)
         do i2 = mlind(2),muind(2)
            do i1 = mlind(1),muind(1)
                arr(i1,i2,i3) = b(i1-shift_(1),i2-shift_(2),i3-shift_(3))
            enddo
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocPCharacter3
!
!
!
!===============================================================================
subroutine reallocCharacter3(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   character(len=*), allocatable, intent(inout)             :: arr(:,:,:)
   integer, intent(in)                          :: uindex(3)
   integer, intent(in), optional                :: lindex(3)
   integer, intent(out), optional               :: stat
   character(len=*), intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(3)
   logical, intent(in), optional                :: keepExisting

   character(len=len(arr)), allocatable                            :: b(:,:,:)
   integer        :: uind(3), lind(3), muind(3), mlind(3), lindex_(3), shift_(3)
   integer        :: i1,i2,i3
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   if (allocated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         allocate (b(mlind(1):muind(1),mlind(2):muind(2),mlind(3):muind(3)))
            do i3 = mlind(3),muind(3)
               do i2 = mlind(2),muind(2)
                  do i1 = mlind(1),muind(1)
                      b(i1,i2,i3) = arr(i1-shift_(1),i2-shift_(2),i3-shift_(3))
                  enddo
               enddo
            enddo
      endif
      if (.not.equalSize) deallocate(arr, stat = localErr)
   endif
   if (.not.allocated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2),lindex_(3):uindex(3)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (allocated(b) .and. localErr==0 .and. size(b)>0) then
      do i3 = mlind(3),muind(3)
         do i2 = mlind(2),muind(2)
            do i1 = mlind(1),muind(1)
                arr(i1,i2,i3) = b(i1,i2,i3)
            enddo
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocCharacter3
!
!
!
!===============================================================================
subroutine reallocPCharacter4(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   character(len=*), pointer, intent(inout)                 :: arr(:,:,:,:)
   integer, intent(in)                          :: uindex(4)
   integer, intent(in), optional                :: lindex(4)
   integer, intent(out), optional               :: stat
   character(len=*), intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(4)
   logical, intent(in), optional                :: keepExisting

   character(len=len(arr)), pointer                                :: b(:,:,:,:)
   integer        :: uind(4), lind(4), muind(4), mlind(4), lindex_(4), shift_(4)
   integer        :: i1,i2,i3,i4
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1, 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0, 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   nullify(b)
   if (associated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         b => arr
         nullify(arr)
      elseif (.not.equalSize) then
         deallocate(arr, stat = localErr)
      endif
   endif
   if (.not.associated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2),lindex_(3):uindex(3),lindex_(4):uindex(4)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (associated(b) .and. localErr==0 .and. size(b)>0) then
      do i4 = mlind(4),muind(4)
         do i3 = mlind(3),muind(3)
            do i2 = mlind(2),muind(2)
               do i1 = mlind(1),muind(1)
                   arr(i1,i2,i3,i4) = b(i1-shift_(1),i2-shift_(2),i3-shift_(3),i4-shift_(4))
               enddo
            enddo
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocPCharacter4
!
!
!
!===============================================================================
subroutine reallocCharacter4(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   character(len=*), allocatable, intent(inout)             :: arr(:,:,:,:)
   integer, intent(in)                          :: uindex(4)
   integer, intent(in), optional                :: lindex(4)
   integer, intent(out), optional               :: stat
   character(len=*), intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(4)
   logical, intent(in), optional                :: keepExisting

   character(len=len(arr)), allocatable                            :: b(:,:,:,:)
   integer        :: uind(4), lind(4), muind(4), mlind(4), lindex_(4), shift_(4)
   integer        :: i1,i2,i3,i4
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1, 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0, 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   if (allocated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         allocate (b(mlind(1):muind(1),mlind(2):muind(2),mlind(3):muind(3),mlind(4):muind(4)))
            do i4 = mlind(4),muind(4)
               do i3 = mlind(3),muind(3)
                  do i2 = mlind(2),muind(2)
                     do i1 = mlind(1),muind(1)
                         b(i1,i2,i3,i4) = arr(i1-shift_(1),i2-shift_(2),i3-shift_(3),i4-shift_(4))
                     enddo
                  enddo
               enddo
            enddo
      endif
      if (.not.equalSize) deallocate(arr, stat = localErr)
   endif
   if (.not.allocated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2),lindex_(3):uindex(3),lindex_(4):uindex(4)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (allocated(b) .and. localErr==0 .and. size(b)>0) then
      do i4 = mlind(4),muind(4)
         do i3 = mlind(3),muind(3)
            do i2 = mlind(2),muind(2)
               do i1 = mlind(1),muind(1)
                   arr(i1,i2,i3,i4) = b(i1,i2,i3,i4)
               enddo
            enddo
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocCharacter4
!
!
!
!===============================================================================
!> Reallocates a single allocatable string.
!! NOTE: Do not confuse this with an allocatable array of strings!
subroutine reallocString(string, newlen, stat, fill, shift, keepExisting)
   implicit none
   character(len=:), allocatable, intent(inout) :: string
   integer, intent(in)                          :: newlen
   integer, intent(out), optional               :: stat
   character(len=*), intent(in), optional       :: fill
   integer, intent(in), optional                :: shift
   logical, intent(in), optional                :: keepExisting

   character(len=:), allocatable                :: b
   integer        :: curlen, minlen, shift_
   integer        :: i, lenfill, numrep
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = 0
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   if (allocated(string)) then
      curlen = len(string)
      equalSize = (newlen == curlen)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. shift_==0) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         minlen = min(curlen + shift_, newlen)
         allocate(character(len=minlen) :: b)
         b(1+shift_:minlen) = string(1:minlen-shift_)
      endif
      if (.not.equalSize) deallocate(string, stat = localErr)
   endif
   if (.not.allocated(string) .and. localErr==0) then
       allocate(character(len=newlen) :: string, stat = localErr)
   endif
   if (present(fill) .and. localErr==0) then
      lenfill = len(fill)
      numrep  = int(newlen/lenfill)
      do i=1,newlen,lenfill
         string(i:max(newlen, i+lenfill-1)) = fill
      end do
   end if

   if (allocated(b) .and. localErr==0 .and. len(b)>0) then
      string(1+shift_:minlen) = b(1+shift_:minlen)
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocString
!
!
!
!===============================================================================
subroutine reallocPReal(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   real,    pointer,  intent(inout) :: arr(:)
   integer,           intent(in)    :: uindex
   integer, optional, intent(in)    :: lindex
   integer, optional, intent(out)   :: stat
   real,    optional, intent(in)    :: fill
   integer, optional, intent(in)    :: shift
   logical, optional, intent(in)    :: keepExisting

   real, pointer :: b(:)
   integer       :: uind, lind, muind, mlind, lindex_, shift_, i
   integer       :: localErr
   logical       :: docopy
   logical       :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = 1
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = 0
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   nullify(b)
   if (associated(arr)) then
      uind = ubound(arr,1)
      lind = lbound(arr,1)
      equalSize = (uindex == uind) .and. (lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. shift_==0) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         b => arr
         nullify(arr)
      elseif (.not.equalSize) then
         deallocate(arr, stat = localErr)
      endif
   endif
   if (.not.associated(arr) .and. localErr==0) then
       allocate(arr(lindex_:uindex), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (associated(b) .and. localErr==0 .and. size(b)>0) then
      do i=mlind, muind
        arr(i) = b(i-shift_)
      end do
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocPReal
!
!
!
!===============================================================================
subroutine reallocReal(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   real, allocatable, intent(inout)             :: arr(:)
   integer, intent(in)                          :: uindex
   integer, intent(in), optional                :: lindex
   integer, intent(out), optional               :: stat
   real, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift
   logical, intent(in), optional                :: keepExisting

   real, allocatable                            :: b(:)
   integer        :: uind, lind, muind, mlind, lindex_, shift_

   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = 1
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = 0
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   if (allocated(arr)) then
      uind = ubound(arr,1)
      lind = lbound(arr,1)
      equalSize = (uindex == uind) .and. (lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. shift_==0) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         allocate (b(mlind:muind))
        b(mlind:muind) = arr(mlind-shift_:muind-shift_)
      endif
      if (.not.equalSize) deallocate(arr, stat = localErr)
   endif
   if (.not.allocated(arr) .and. localErr==0) then
       allocate(arr(lindex_:uindex), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (allocated(b) .and. localErr==0 .and. size(b)>0) then
      arr(mlind:muind) = b(mlind:muind)
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocReal
!
!
!
!===============================================================================
subroutine reallocPReal2(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   real, pointer, intent(inout)                 :: arr(:,:)
   integer, intent(in)                          :: uindex(2)
   integer, intent(in), optional                :: lindex(2)
   integer, intent(out), optional               :: stat
   real, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(2)
   logical, intent(in), optional                :: keepExisting

   real, pointer                                :: b(:,:)
   integer        :: uind(2), lind(2), muind(2), mlind(2), lindex_(2), shift_(2)
   integer        :: i1,i2
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   nullify(b)
   if (associated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         b => arr
         nullify(arr)
      elseif (.not.equalSize) then
         deallocate(arr, stat = localErr)
      endif
   endif
   if (.not.associated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (associated(b) .and. localErr==0 .and. size(b)>0) then
      do i2 = mlind(2),muind(2)
         do i1 = mlind(1),muind(1)
             arr(i1,i2) = b(i1-shift_(1),i2-shift_(2))
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocPReal2
!
!
!
!===============================================================================
subroutine reallocReal2(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   real, allocatable, intent(inout)             :: arr(:,:)
   integer, intent(in)                          :: uindex(2)
   integer, intent(in), optional                :: lindex(2)
   integer, intent(out), optional               :: stat
   real, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(2)
   logical, intent(in), optional                :: keepExisting

   real, allocatable                            :: b(:,:)
   integer        :: uind(2), lind(2), muind(2), mlind(2), lindex_(2), shift_(2)
   integer        :: i1,i2
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   if (allocated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         allocate (b(mlind(1):muind(1),mlind(2):muind(2)))
            do i2 = mlind(2),muind(2)
               do i1 = mlind(1),muind(1)
                   b(i1,i2) = arr(i1-shift_(1),i2-shift_(2))
               enddo
            enddo
      endif
      if (.not.equalSize) deallocate(arr, stat = localErr)
   endif
   if (.not.allocated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (allocated(b) .and. localErr==0 .and. size(b)>0) then
      do i2 = mlind(2),muind(2)
         do i1 = mlind(1),muind(1)
             arr(i1,i2) = b(i1,i2)
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocReal2
!
!
!
!===============================================================================
subroutine reallocPReal3(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   real, pointer, intent(inout)                 :: arr(:,:,:)
   integer, intent(in)                          :: uindex(3)
   integer, intent(in), optional                :: lindex(3)
   integer, intent(out), optional               :: stat
   real, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(3)
   logical, intent(in), optional                :: keepExisting

   real, pointer                                :: b(:,:,:)
   integer        :: uind(3), lind(3), muind(3), mlind(3), lindex_(3), shift_(3)
   integer        :: i1,i2,i3
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   nullify(b)
   if (associated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         b => arr
         nullify(arr)
      elseif (.not.equalSize) then
         deallocate(arr, stat = localErr)
      endif
   endif
   if (.not.associated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2),lindex_(3):uindex(3)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (associated(b) .and. localErr==0 .and. size(b)>0) then
      do i3 = mlind(3),muind(3)
         do i2 = mlind(2),muind(2)
            do i1 = mlind(1),muind(1)
                arr(i1,i2,i3) = b(i1-shift_(1),i2-shift_(2),i3-shift_(3))
            enddo
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocPReal3
!
!
!
!===============================================================================
subroutine reallocReal3(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   real, allocatable, intent(inout)             :: arr(:,:,:)
   integer, intent(in)                          :: uindex(3)
   integer, intent(in), optional                :: lindex(3)
   integer, intent(out), optional               :: stat
   real, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(3)
   logical, intent(in), optional                :: keepExisting

   real, allocatable                            :: b(:,:,:)
   integer        :: uind(3), lind(3), muind(3), mlind(3), lindex_(3), shift_(3)
   integer        :: i1,i2,i3
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   if (allocated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         allocate (b(mlind(1):muind(1),mlind(2):muind(2),mlind(3):muind(3)))
            do i3 = mlind(3),muind(3)
               do i2 = mlind(2),muind(2)
                  do i1 = mlind(1),muind(1)
                      b(i1,i2,i3) = arr(i1-shift_(1),i2-shift_(2),i3-shift_(3))
                  enddo
               enddo
            enddo
      endif
      if (.not.equalSize) deallocate(arr, stat = localErr)
   endif
   if (.not.allocated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2),lindex_(3):uindex(3)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (allocated(b) .and. localErr==0 .and. size(b)>0) then
      do i3 = mlind(3),muind(3)
         do i2 = mlind(2),muind(2)
            do i1 = mlind(1),muind(1)
                arr(i1,i2,i3) = b(i1,i2,i3)
            enddo
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocReal3
!
!
!
!===============================================================================
subroutine reallocPReal4(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   real, pointer, intent(inout)                 :: arr(:,:,:,:)
   integer, intent(in)                          :: uindex(4)
   integer, intent(in), optional                :: lindex(4)
   integer, intent(out), optional               :: stat
   real, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(4)
   logical, intent(in), optional                :: keepExisting

   real, pointer                                :: b(:,:,:,:)
   integer        :: uind(4), lind(4), muind(4), mlind(4), lindex_(4), shift_(4)
   integer        :: i1,i2,i3,i4
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1, 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0, 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   nullify(b)
   if (associated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         b => arr
         nullify(arr)
      elseif (.not.equalSize) then
         deallocate(arr, stat = localErr)
      endif
   endif
   if (.not.associated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2),lindex_(3):uindex(3),lindex_(4):uindex(4)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (associated(b) .and. localErr==0 .and. size(b)>0) then
      do i4 = mlind(4),muind(4)
         do i3 = mlind(3),muind(3)
            do i2 = mlind(2),muind(2)
               do i1 = mlind(1),muind(1)
                   arr(i1,i2,i3,i4) = b(i1-shift_(1),i2-shift_(2),i3-shift_(3),i4-shift_(4))
               enddo
            enddo
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocPReal4
!
!
!
!===============================================================================
subroutine reallocReal4(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   real, allocatable, intent(inout)             :: arr(:,:,:,:)
   integer, intent(in)                          :: uindex(4)
   integer, intent(in), optional                :: lindex(4)
   integer, intent(out), optional               :: stat
   real, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(4)
   logical, intent(in), optional                :: keepExisting

   real, allocatable                            :: b(:,:,:,:)
   integer        :: uind(4), lind(4), muind(4), mlind(4), lindex_(4), shift_(4)
   integer        :: i1,i2,i3,i4
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1, 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0, 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   if (allocated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         allocate (b(mlind(1):muind(1),mlind(2):muind(2),mlind(3):muind(3),mlind(4):muind(4)))
            do i4 = mlind(4),muind(4)
               do i3 = mlind(3),muind(3)
                  do i2 = mlind(2),muind(2)
                     do i1 = mlind(1),muind(1)
                         b(i1,i2,i3,i4) = arr(i1-shift_(1),i2-shift_(2),i3-shift_(3),i4-shift_(4))
                     enddo
                  enddo
               enddo
            enddo
      endif
      if (.not.equalSize) deallocate(arr, stat = localErr)
   endif
   if (.not.allocated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2),lindex_(3):uindex(3),lindex_(4):uindex(4)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (allocated(b) .and. localErr==0 .and. size(b)>0) then
      do i4 = mlind(4),muind(4)
         do i3 = mlind(3),muind(3)
            do i2 = mlind(2),muind(2)
               do i1 = mlind(1),muind(1)
                   arr(i1,i2,i3,i4) = b(i1,i2,i3,i4)
               enddo
            enddo
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocReal4
!
!
!
!===============================================================================
subroutine reallocPDouble(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   double precision, pointer,  intent(inout) :: arr(:)
   integer,                    intent(in)    :: uindex
   integer,          optional, intent(in)    :: lindex
   integer,          optional, intent(out)   :: stat
   double precision, optional, intent(in)    :: fill
   integer,          optional, intent(in)    :: shift
   logical,          optional, intent(in)    :: keepExisting

   double precision, pointer :: b(:)
   integer                   :: uind, lind, muind, mlind, lindex_, shift_, i
   integer                   :: localErr
   logical                   :: docopy
   logical                   :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = 1
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = 0
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   nullify(b)
   if (associated(arr)) then
      uind = ubound(arr,1)
      lind = lbound(arr,1)
      equalSize = (uindex == uind) .and. (lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. shift_==0) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         b => arr
         nullify(arr)
      elseif (.not.equalSize) then
         deallocate(arr, stat = localErr)
      endif
   endif
   if (.not.associated(arr) .and. localErr==0) then
       allocate(arr(lindex_:uindex), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (associated(b) .and. localErr==0 .and. size(b)>0) then
      do i=mlind, muind
         arr(i) = b(i-shift_)
      end do
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocPDouble
!
!
!
!===============================================================================
subroutine reallocDouble(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   double precision, allocatable, intent(inout)             :: arr(:)
   integer, intent(in)                          :: uindex
   integer, intent(in), optional                :: lindex
   integer, intent(out), optional               :: stat
   double precision, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift
   logical, intent(in), optional                :: keepExisting

   double precision, allocatable                            :: b(:)
   integer        :: uind, lind, muind, mlind, lindex_, shift_

   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = 1
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = 0
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   if (allocated(arr)) then
      uind = ubound(arr,1)
      lind = lbound(arr,1)
      equalSize = (uindex == uind) .and. (lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. shift_==0) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         allocate (b(mlind:muind))
        b(mlind:muind) = arr(mlind-shift_:muind-shift_)
      endif
      if (.not.equalSize) deallocate(arr, stat = localErr)
   endif
   if (.not.allocated(arr) .and. localErr==0) then
       allocate(arr(lindex_:uindex), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (allocated(b) .and. localErr==0 .and. size(b)>0) then
      arr(mlind:muind) = b(mlind:muind)
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocDouble
!
!
!
!===============================================================================
subroutine reallocPDouble2(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   double precision, pointer, intent(inout)                 :: arr(:,:)
   integer, intent(in)                          :: uindex(2)
   integer, intent(in), optional                :: lindex(2)
   integer, intent(out), optional               :: stat
   double precision, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(2)
   logical, intent(in), optional                :: keepExisting

   double precision, pointer                                :: b(:,:)
   integer        :: uind(2), lind(2), muind(2), mlind(2), lindex_(2), shift_(2)
   integer        :: i1,i2
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   nullify(b)
   if (associated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         b => arr
         nullify(arr)
      elseif (.not.equalSize) then
         deallocate(arr, stat = localErr)
      endif
   endif
   if (.not.associated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (associated(b) .and. localErr==0 .and. size(b)>0) then
      do i2 = mlind(2),muind(2)
         do i1 = mlind(1),muind(1)
             arr(i1,i2) = b(i1-shift_(1),i2-shift_(2))
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocPDouble2
!
!
!
!===============================================================================
subroutine reallocDouble2(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   double precision, allocatable, intent(inout)             :: arr(:,:)
   integer, intent(in)                          :: uindex(2)
   integer, intent(in), optional                :: lindex(2)
   integer, intent(out), optional               :: stat
   double precision, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(2)
   logical, intent(in), optional                :: keepExisting

   double precision, allocatable                            :: b(:,:)
   integer        :: uind(2), lind(2), muind(2), mlind(2), lindex_(2), shift_(2)
   integer        :: i1,i2
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   if (allocated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         allocate (b(mlind(1):muind(1),mlind(2):muind(2)))
            do i2 = mlind(2),muind(2)
               do i1 = mlind(1),muind(1)
                   b(i1,i2) = arr(i1-shift_(1),i2-shift_(2))
               enddo
            enddo
      endif
      if (.not.equalSize) deallocate(arr, stat = localErr)
   endif
   if (.not.allocated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (allocated(b) .and. localErr==0 .and. size(b)>0) then
      do i2 = mlind(2),muind(2)
         do i1 = mlind(1),muind(1)
             arr(i1,i2) = b(i1,i2)
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocDouble2
!
!
!
!===============================================================================
subroutine reallocPDouble3(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   double precision, pointer, intent(inout)                 :: arr(:,:,:)
   integer, intent(in)                          :: uindex(3)
   integer, intent(in), optional                :: lindex(3)
   integer, intent(out), optional               :: stat
   double precision, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(3)
   logical, intent(in), optional                :: keepExisting

   double precision, pointer                                :: b(:,:,:)
   integer        :: uind(3), lind(3), muind(3), mlind(3), lindex_(3), shift_(3)
   integer        :: i1,i2,i3
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   nullify(b)
   if (associated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         b => arr
         nullify(arr)
      elseif (.not.equalSize) then
         deallocate(arr, stat = localErr)
      endif
   endif
   if (.not.associated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2),lindex_(3):uindex(3)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (associated(b) .and. localErr==0 .and. size(b)>0) then
      do i3 = mlind(3),muind(3)
         do i2 = mlind(2),muind(2)
            do i1 = mlind(1),muind(1)
                arr(i1,i2,i3) = b(i1-shift_(1),i2-shift_(2),i3-shift_(3))
            enddo
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocPDouble3
!
!
!
!===============================================================================
subroutine reallocDouble3(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   double precision, allocatable, intent(inout)             :: arr(:,:,:)
   integer, intent(in)                          :: uindex(3)
   integer, intent(in), optional                :: lindex(3)
   integer, intent(out), optional               :: stat
   double precision, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(3)
   logical, intent(in), optional                :: keepExisting

   double precision, allocatable                            :: b(:,:,:)
   integer        :: uind(3), lind(3), muind(3), mlind(3), lindex_(3), shift_(3)
   integer        :: i1,i2,i3
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   if (allocated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         allocate (b(mlind(1):muind(1),mlind(2):muind(2),mlind(3):muind(3)))
            do i3 = mlind(3),muind(3)
               do i2 = mlind(2),muind(2)
                  do i1 = mlind(1),muind(1)
                      b(i1,i2,i3) = arr(i1-shift_(1),i2-shift_(2),i3-shift_(3))
                  enddo
               enddo
            enddo
      endif
      if (.not.equalSize) deallocate(arr, stat = localErr)
   endif
   if (.not.allocated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2),lindex_(3):uindex(3)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (allocated(b) .and. localErr==0 .and. size(b)>0) then
      do i3 = mlind(3),muind(3)
         do i2 = mlind(2),muind(2)
            do i1 = mlind(1),muind(1)
                arr(i1,i2,i3) = b(i1,i2,i3)
            enddo
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocDouble3
!
!
!
!===============================================================================
subroutine reallocPDouble4(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   double precision, pointer, intent(inout)                 :: arr(:,:,:,:)
   integer, intent(in)                          :: uindex(4)
   integer, intent(in), optional                :: lindex(4)
   integer, intent(out), optional               :: stat
   double precision, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(4)
   logical, intent(in), optional                :: keepExisting

   double precision, pointer                                :: b(:,:,:,:)
   integer        :: uind(4), lind(4), muind(4), mlind(4), lindex_(4), shift_(4)
   integer        :: i1,i2,i3,i4
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1, 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0, 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   nullify(b)
   if (associated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         b => arr
         nullify(arr)
      elseif (.not.equalSize) then
         deallocate(arr, stat = localErr)
      endif
   endif
   if (.not.associated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2),lindex_(3):uindex(3),lindex_(4):uindex(4)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (associated(b) .and. localErr==0 .and. size(b)>0) then
      do i4 = mlind(4),muind(4)
         do i3 = mlind(3),muind(3)
            do i2 = mlind(2),muind(2)
               do i1 = mlind(1),muind(1)
                   arr(i1,i2,i3,i4) = b(i1-shift_(1),i2-shift_(2),i3-shift_(3),i4-shift_(4))
               enddo
            enddo
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocPDouble4
!
!
!
!===============================================================================
subroutine reallocDouble4(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   double precision, allocatable, intent(inout)             :: arr(:,:,:,:)
   integer, intent(in)                          :: uindex(4)
   integer, intent(in), optional                :: lindex(4)
   integer, intent(out), optional               :: stat
   double precision, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(4)
   logical, intent(in), optional                :: keepExisting

   double precision, allocatable                            :: b(:,:,:,:)
   integer        :: uind(4), lind(4), muind(4), mlind(4), lindex_(4), shift_(4)
   integer        :: i1,i2,i3,i4
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1, 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0, 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   if (allocated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         allocate (b(mlind(1):muind(1),mlind(2):muind(2),mlind(3):muind(3),mlind(4):muind(4)))
            do i4 = mlind(4),muind(4)
               do i3 = mlind(3),muind(3)
                  do i2 = mlind(2),muind(2)
                     do i1 = mlind(1),muind(1)
                         b(i1,i2,i3,i4) = arr(i1-shift_(1),i2-shift_(2),i3-shift_(3),i4-shift_(4))
                     enddo
                  enddo
               enddo
            enddo
      endif
      if (.not.equalSize) deallocate(arr, stat = localErr)
   endif
   if (.not.allocated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2),lindex_(3):uindex(3),lindex_(4):uindex(4)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (allocated(b) .and. localErr==0 .and. size(b)>0) then
      do i4 = mlind(4),muind(4)
         do i3 = mlind(3),muind(3)
            do i2 = mlind(2),muind(2)
               do i1 = mlind(1),muind(1)
                   arr(i1,i2,i3,i4) = b(i1,i2,i3,i4)
               enddo
            enddo
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocDouble4
!
!
!
!===============================================================================
subroutine reallocPLogical(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   logical, pointer,  intent(inout) :: arr(:)
   integer,           intent(in)    :: uindex
   integer, optional, intent(in)    :: lindex
   integer, optional, intent(out)   :: stat
   logical, optional, intent(in)    :: fill
   integer, optional, intent(in)    :: shift
   logical, optional, intent(in)    :: keepExisting

   logical, pointer :: b(:)
   integer          :: uind, lind, muind, mlind, lindex_, shift_, i
   integer          :: localErr
   logical          :: docopy
   logical          :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = 1
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = 0
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   nullify(b)
   if (associated(arr)) then
      uind = ubound(arr,1)
      lind = lbound(arr,1)
      equalSize = (uindex == uind) .and. (lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. shift_==0) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         b => arr
         nullify(arr)
      elseif (.not.equalSize) then
         deallocate(arr, stat = localErr)
      endif
   endif
   if (.not.associated(arr) .and. localErr==0) then
       allocate(arr(lindex_:uindex), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (associated(b) .and. localErr==0 .and. size(b)>0) then
      do i=mlind, muind
         arr(i) = b(i-shift_)
      end do
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocPLogical
!
!
!
!===============================================================================
subroutine reallocLogical(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   logical, allocatable, intent(inout)             :: arr(:)
   integer, intent(in)                          :: uindex
   integer, intent(in), optional                :: lindex
   integer, intent(out), optional               :: stat
   logical, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift
   logical, intent(in), optional                :: keepExisting

   logical, allocatable                            :: b(:)
   integer        :: uind, lind, muind, mlind, lindex_, shift_

   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = 1
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = 0
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   if (allocated(arr)) then
      uind = ubound(arr,1)
      lind = lbound(arr,1)
      equalSize = (uindex == uind) .and. (lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. shift_==0) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         allocate (b(mlind:muind))
        b(mlind:muind) = arr(mlind-shift_:muind-shift_)
      endif
      if (.not.equalSize) deallocate(arr, stat = localErr)
   endif
   if (.not.allocated(arr) .and. localErr==0) then
       allocate(arr(lindex_:uindex), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (allocated(b) .and. localErr==0 .and. size(b)>0) then
      arr(mlind:muind) = b(mlind:muind)
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocLogical
!
!
!
!===============================================================================
subroutine reallocPLogical2(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   logical, pointer, intent(inout)                 :: arr(:,:)
   integer, intent(in)                          :: uindex(2)
   integer, intent(in), optional                :: lindex(2)
   integer, intent(out), optional               :: stat
   logical, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(2)
   logical, intent(in), optional                :: keepExisting

   logical, pointer                                :: b(:,:)
   integer        :: uind(2), lind(2), muind(2), mlind(2), lindex_(2), shift_(2)
   integer        :: i1,i2
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   nullify(b)
   if (associated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         b => arr
         nullify(arr)
      elseif (.not.equalSize) then
         deallocate(arr, stat = localErr)
      endif
   endif
   if (.not.associated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (associated(b) .and. localErr==0 .and. size(b)>0) then
      do i2 = mlind(2),muind(2)
         do i1 = mlind(1),muind(1)
             arr(i1,i2) = b(i1-shift_(1),i2-shift_(2))
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocPLogical2
!
!
!
!===============================================================================
subroutine reallocLogical2(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   logical, allocatable, intent(inout)             :: arr(:,:)
   integer, intent(in)                          :: uindex(2)
   integer, intent(in), optional                :: lindex(2)
   integer, intent(out), optional               :: stat
   logical, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(2)
   logical, intent(in), optional                :: keepExisting

   logical, allocatable                            :: b(:,:)
   integer        :: uind(2), lind(2), muind(2), mlind(2), lindex_(2), shift_(2)
   integer        :: i1,i2
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   if (allocated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         allocate (b(mlind(1):muind(1),mlind(2):muind(2)))
            do i2 = mlind(2),muind(2)
               do i1 = mlind(1),muind(1)
                   b(i1,i2) = arr(i1-shift_(1),i2-shift_(2))
               enddo
            enddo
      endif
      if (.not.equalSize) deallocate(arr, stat = localErr)
   endif
   if (.not.allocated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (allocated(b) .and. localErr==0 .and. size(b)>0) then
      do i2 = mlind(2),muind(2)
         do i1 = mlind(1),muind(1)
             arr(i1,i2) = b(i1,i2)
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocLogical2
!
!
!
!===============================================================================
subroutine reallocPLogical3(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   logical, pointer, intent(inout)                 :: arr(:,:,:)
   integer, intent(in)                          :: uindex(3)
   integer, intent(in), optional                :: lindex(3)
   integer, intent(out), optional               :: stat
   logical, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(3)
   logical, intent(in), optional                :: keepExisting

   logical, pointer                                :: b(:,:,:)
   integer        :: uind(3), lind(3), muind(3), mlind(3), lindex_(3), shift_(3)
   integer        :: i1,i2,i3
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   nullify(b)
   if (associated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         b => arr
         nullify(arr)
      elseif (.not.equalSize) then
         deallocate(arr, stat = localErr)
      endif
   endif
   if (.not.associated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2),lindex_(3):uindex(3)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (associated(b) .and. localErr==0 .and. size(b)>0) then
      do i3 = mlind(3),muind(3)
         do i2 = mlind(2),muind(2)
            do i1 = mlind(1),muind(1)
                arr(i1,i2,i3) = b(i1-shift_(1),i2-shift_(2),i3-shift_(3))
            enddo
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocPLogical3
!
!
!
!===============================================================================
subroutine reallocLogical3(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   logical, allocatable, intent(inout)             :: arr(:,:,:)
   integer, intent(in)                          :: uindex(3)
   integer, intent(in), optional                :: lindex(3)
   integer, intent(out), optional               :: stat
   logical, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(3)
   logical, intent(in), optional                :: keepExisting

   logical, allocatable                            :: b(:,:,:)
   integer        :: uind(3), lind(3), muind(3), mlind(3), lindex_(3), shift_(3)
   integer        :: i1,i2,i3
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   if (allocated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         allocate (b(mlind(1):muind(1),mlind(2):muind(2),mlind(3):muind(3)))
            do i3 = mlind(3),muind(3)
               do i2 = mlind(2),muind(2)
                  do i1 = mlind(1),muind(1)
                      b(i1,i2,i3) = arr(i1-shift_(1),i2-shift_(2),i3-shift_(3))
                  enddo
               enddo
            enddo
      endif
      if (.not.equalSize) deallocate(arr, stat = localErr)
   endif
   if (.not.allocated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2),lindex_(3):uindex(3)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (allocated(b) .and. localErr==0 .and. size(b)>0) then
      do i3 = mlind(3),muind(3)
         do i2 = mlind(2),muind(2)
            do i1 = mlind(1),muind(1)
                arr(i1,i2,i3) = b(i1,i2,i3)
            enddo
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocLogical3
!
!
!
!===============================================================================
subroutine reallocPLogical4(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   logical, pointer, intent(inout)                 :: arr(:,:,:,:)
   integer, intent(in)                          :: uindex(4)
   integer, intent(in), optional                :: lindex(4)
   integer, intent(out), optional               :: stat
   logical, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(4)
   logical, intent(in), optional                :: keepExisting

   logical, pointer                                :: b(:,:,:,:)
   integer        :: uind(4), lind(4), muind(4), mlind(4), lindex_(4), shift_(4)
   integer        :: i1,i2,i3,i4
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1, 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0, 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   nullify(b)
   if (associated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         b => arr
         nullify(arr)
      elseif (.not.equalSize) then
         deallocate(arr, stat = localErr)
      endif
   endif
   if (.not.associated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2),lindex_(3):uindex(3),lindex_(4):uindex(4)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (associated(b) .and. localErr==0 .and. size(b)>0) then
      do i4 = mlind(4),muind(4)
         do i3 = mlind(3),muind(3)
            do i2 = mlind(2),muind(2)
               do i1 = mlind(1),muind(1)
                   arr(i1,i2,i3,i4) = b(i1-shift_(1),i2-shift_(2),i3-shift_(3),i4-shift_(4))
               enddo
            enddo
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocPLogical4
!
!
!
!===============================================================================
subroutine reallocLogical4(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   logical, allocatable, intent(inout)             :: arr(:,:,:,:)
   integer, intent(in)                          :: uindex(4)
   integer, intent(in), optional                :: lindex(4)
   integer, intent(out), optional               :: stat
   logical, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift(4)
   logical, intent(in), optional                :: keepExisting

   logical, allocatable                            :: b(:,:,:,:)
   integer        :: uind(4), lind(4), muind(4), mlind(4), lindex_(4), shift_(4)
   integer        :: i1,i2,i3,i4
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1, 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0, 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   if (allocated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         allocate (b(mlind(1):muind(1),mlind(2):muind(2),mlind(3):muind(3),mlind(4):muind(4)))
            do i4 = mlind(4),muind(4)
               do i3 = mlind(3),muind(3)
                  do i2 = mlind(2),muind(2)
                     do i1 = mlind(1),muind(1)
                         b(i1,i2,i3,i4) = arr(i1-shift_(1),i2-shift_(2),i3-shift_(3),i4-shift_(4))
                     enddo
                  enddo
               enddo
            enddo
      endif
      if (.not.equalSize) deallocate(arr, stat = localErr)
   endif
   if (.not.allocated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2),lindex_(3):uindex(3),lindex_(4):uindex(4)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (allocated(b) .and. localErr==0 .and. size(b)>0) then
      do i4 = mlind(4),muind(4)
         do i3 = mlind(3),muind(3)
            do i2 = mlind(2),muind(2)
               do i1 = mlind(1),muind(1)
                   arr(i1,i2,i3,i4) = b(i1,i2,i3,i4)
               enddo
            enddo
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocLogical4

!===============================================================================
subroutine reallocByte2(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   integer(kind=1), allocatable, intent(inout)  :: arr(:,:)
   integer, intent(in)                          :: uindex(2)
   integer, intent(in), optional                :: lindex(2)
   integer, intent(out), optional               :: stat
   integer, intent(in), optional                :: fill
   integer, intent(in), optional                :: shift(2)
   logical, intent(in), optional                :: keepExisting

   integer(kind=1), allocatable                 :: b(:,:)
   integer        :: uind(2), lind(2), muind(2), mlind(2), lindex_(2), shift_(2)
   integer        :: i1,i2
   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1 /)
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = (/ 0, 0 /)
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   if (allocated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      equalSize = all(uindex == uind) .and. all(lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. all(shift_==0)) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         allocate (b(mlind(1):muind(1),mlind(2):muind(2)))
            do i2 = mlind(2),muind(2)
               do i1 = mlind(1),muind(1)
                   b(i1,i2) = arr(i1-shift_(1),i2-shift_(2))
               enddo
            enddo
      endif
      if (.not.equalSize) deallocate(arr, stat = localErr)
   endif
   if (.not.allocated(arr) .and. localErr==0) then
       allocate(arr(lindex_(1):uindex(1),lindex_(2):uindex(2)), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (allocated(b) .and. localErr==0 .and. size(b)>0) then
      do i2 = mlind(2),muind(2)
         do i1 = mlind(1),muind(1)
             arr(i1,i2) = b(i1,i2)
         enddo
      enddo
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocByte2
!

!===============================================================================

!> Determines size of an allocatable array, returning 0 when it is not allocated.
function allocSizeDouble(arr) result(isize)
   implicit none
   double precision, allocatable, intent(inout) :: arr(:) !< Array for which the extent must be determined. Is allowed to be not allocated.
   integer                                      :: isize  !< Array length, 0 when it was not allocated.

   if (allocated(arr)) then
      isize = size(arr)
   else
      isize = 0
   end if
end function allocSizeDouble

end module m_alloc
