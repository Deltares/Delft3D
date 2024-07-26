!  Copyright (C)  Stichting Deltares, 2017-2024.
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

module runsum
   use precision
   implicit none

! ------------------------------------------------------------------------------
!   Class:      TRunSum
!   Purpose:    Keeps track of running sum/mean
!   Summary:    This class needs a buffer keeping a given number of values that are given in an update method
!               keeping track of a running mean.
!   Descendand: None
!   Parent:     None
! ------------------------------------------------------------------------------

   private
   public :: TRunSum

   type TRunSum
      integer                     :: ndx = 0                   ! current index in circulair buffer
      integer                     :: nstep = 0                 ! number of updates
      real(kind=sp), allocatable, dimension (:,:) :: buffer    ! circulair buffer
      real(kind=sp), allocatable, dimension (:)   :: state     ! state lives inside the instance
      real(kind=hp), pointer,     dimension (:)   :: dataPtr   ! dataPtr lives in the calling code
   contains
      procedure, pass :: init          => TRunSum_init         ! Init buffer and sum and index
      procedure, pass :: update        => TRunSum_update       ! Add a value to the list, update the sum
      procedure, pass :: update_state  => TRunSum_update_state ! Add a value to the list, update the sum
   end type TRunSum


contains


! ------------------------------------------------------------------------------
!   Method:     init
!   Purpose:    setup new buffer for running sum
!   Arguments:  newvalue
! ------------------------------------------------------------------------------
subroutine TRunSum_init(self, grid_width, nd, dataptr)
   class(TRunSum), intent(inout)                :: self
   integer, intent(in)                          :: grid_width
   integer, intent(in)                          :: nd
   real(kind=hp), dimension(:), pointer, optional  :: dataPtr

   if (allocated(self%buffer)) deallocate(self%buffer)
   allocate (self%buffer(grid_width,0:nd-1))
   self%buffer(1:grid_width,0:nd-1) = 0_hp
   if (allocated(self%state)) deallocate(self%state)
   allocate (self%state(grid_width))
   self%state(1:grid_width) = 0.d0
   self%ndx = 0
   self%nstep = 0
   self%dataPtr => null()
   if (present(dataPtr)) then
      if (associated(dataPtr)) then
         self%dataPtr => dataPtr
      end if
   end if
end subroutine TRunSum_init


! ------------------------------------------------------------------------------
!   Method:     update
!   Purpose:    add a new element to the buffer, remove an old one and update the sum
!   Arguments:  newvalue
! ------------------------------------------------------------------------------
subroutine TRunSum_update(self, newvalue)
   class(TRunSum), intent(inout)                :: self
   real(kind=hp), dimension(:), intent(in), optional, target :: newvalue

   integer :: grid_width, nd
   real(kind=hp), dimension(:), pointer :: pnew

   grid_width = size(self%state)
   nd = size(self%buffer,dim=2)
   if (present(newvalue)) then
       pnew => newvalue                  ! update value(s) explicit
   else
       pnew => self%dataPtr              ! update value(s) pointered to
   end if
   call self%update_state(pnew,grid_width,nd)
   self%ndx = mod(self%ndx+1,nd)
   self%nstep = self%nstep + 1
end subroutine TRunSum_update


subroutine TRunSum_update_state(self, pnew, grid_width, nd)
   class(TRunSum), intent(inout)        :: self
   real(kind=hp), dimension(:), pointer :: pnew
   integer, intent(in)                  :: grid_width
   integer, intent(in)                  :: nd

   self%state(1:grid_width) = self%state(1:grid_width) + pnew(1:grid_width) - self%buffer(1:grid_width, self%ndx)
   self%buffer(1:grid_width, self%ndx) = pnew(1:grid_width)
end subroutine TRunSum_update_state

end module runsum





module AR1MA
   use precision
   use runsum
   implicit none


! ------------------------------------------------------------------------------
!   Class:      TAR1MA
!   Purpose:    Keeps track of running sum/mean with a weight for each time level
!   Summary:    This class needs a buffer keeping a given number of values that are given in an update method
!               keeping track of a running mean.
!   Descendand: None
!   Parent:     None
! ------------------------------------------------------------------------------

   private
   public :: TAR1MA

   type, extends (TRunSum) :: TAR1MA
      real(kind=hp), pointer,     dimension (:)   :: weights   ! dataPtr lives in the calling code
      real(kind=hp)                               :: a1 = 0_hp   ! dataPtr lives in the calling code
      real(kind=hp)                               :: b1 = 1_hp   ! dataPtr lives in the calling code
   contains
      procedure, pass :: setpar    => TAR1MA_setpar
      procedure, pass :: update_state  => TAR1MA_update_state
   end type TAR1MA
 

contains

subroutine TAR1MA_setpar(self, pweights, a1, b1)
   class(TAR1MA), intent(inout)    :: self
   real(kind=hp), optional, pointer,  dimension (:) :: pweights   ! dataPtr lives in the calling code
   real(kind=hp), optional                          :: a1
   real(kind=hp), optional                          :: b1
   if (present(pweights)) then
      self%weights => pweights
   end if
   if (present(a1)) then
      self%a1 = a1
   end if
   if (present(b1)) then
      self%b1 = b1
   end if
end subroutine TAR1MA_setpar

subroutine TAR1MA_update_state(self, pnew, grid_width, nd)
   class(TAR1MA), intent(inout)            :: self
   real(kind=hp), dimension(:), pointer    :: pnew
   integer, intent(in)                     :: grid_width
   integer, intent(in)                     :: nd

   integer :: id, idm

   self%buffer(1:grid_width, self%ndx) = pnew(1:grid_width)
   self%state(1:grid_width) = self%a1 * self%state(1:grid_width)
   do id = 0, nd-1
      idm = mod(self%ndx - id + nd, nd)
      if (associated(self%weights)) then
         self%state(1:grid_width) = self%state(1:grid_width) + self%b1 * self%weights(id)*self%buffer(1:grid_width, idm)   ! weighted sum
      else
         self%state(1:grid_width) = self%state(1:grid_width) + self%b1 * self%buffer(1:grid_width, idm)
      end if
   end do
end subroutine TAR1MA_update_state

end module AR1MA


module AR1smooth
   use precision
   implicit none

! ------------------------------------------------------------------------------
!   Class:      TAR1smooth
!   Purpose:    Smooth signal using Auto Regressive order 1
!   Descendand: None
!   Parent:     None
! ------------------------------------------------------------------------------

   private
   public :: TAR1smooth

   type TAR1smooth
      integer        :: nvalue
      real(kind=hp)  :: a1
      real(kind=hp)  :: b
      logical        :: isFirst = .True.
      real(kind=hp), allocatable, dimension (:)   :: state     ! state lives inside the instance
      real(kind=hp), pointer,     dimension (:)   :: dataPtr   ! dataPtr lives in the calling code
   contains
      procedure, pass :: init          => TAR1smooth_init         ! 
      procedure, pass :: update_state  => TAR1smooth_update_state ! 
   end type TAR1smooth

contains


! ------------------------------------------------------------------------------
!   Method:     init
!   Purpose:    set AR1 parameters a1 and b in instance, set data pointer
!   Arguments:  a1, b, dataPtr
! ------------------------------------------------------------------------------
subroutine TAR1smooth_init(self, a1, b, dataptr)
   class(TAR1smooth), intent(inout)                :: self
   real(kind=hp), intent(in) :: a1
   real(kind=hp), intent(in) :: b
   real(kind=hp), dimension(:), pointer, optional  :: dataPtr

   self%isFirst = .True.
   self%a1 = a1
   self%b  = b
   self%dataPtr => null()
   if (present(dataPtr)) then
      if (associated(dataPtr)) then
         self%dataPtr => dataPtr
      end if
   end if
end subroutine TAR1smooth_init


! ------------------------------------------------------------------------------
!   Method:     update_state
!   Purpose:    update the state with a new value
!   Arguments:  newvalue
! ------------------------------------------------------------------------------
subroutine TAR1smooth_update_state(self, newvalue)
   class(TAR1smooth), intent(inout)                          :: self
   real(kind=hp), dimension(:), intent(in), optional, target :: newvalue

   integer :: grid_width
   real(kind=hp), dimension(:), pointer :: pnew

   grid_width = size(self%state)
   if (present(newvalue)) then
       pnew => newvalue                  ! update value(s) explicit
   else
       pnew => self%dataPtr              ! update value(s) pointered to
   end if
   if (self%isFirst) then
      self%state(1:grid_width) = pnew(1:grid_width)
      self%isFirst = .False.
   else
      self%state(1:grid_width) = self%a1 * self%state(1:grid_width) + self%b * pnew(1:grid_width)
   end if
end subroutine TAR1smooth_update_state

end module AR1smooth


