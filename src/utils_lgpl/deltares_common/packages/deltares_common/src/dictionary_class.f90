!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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

module m_dictype
   implicit none

! ------------------------------------------------------------------------------
!   Class:      TDict
!   Purpose:    Implements a key-value dictionary, (character to character)
!   Summary:    Key-value pairs can be pushed, retrieved,
!               the existance of a key-value pair can be established, by key and value substring
!               The dictionary can be traversed element by element using the nnext method
!               The implementation is based on a linked list data structure
!   Descendand: None
!   Parent:     None
! ------------------------------------------------------------------------------

   private
   public :: TDict
   public :: TDictIterator

   type TDict
      integer                     ::  count = 0
      type(TDictElement), pointer ::  elements => null()
   contains
      procedure, pass :: push    => TDict_push            ! Insert a new element
      procedure, pass :: pop     => TDict_pop             ! Remove top-level element, return the key and value 
      procedure, pass :: get     => TDict_get             ! Retrieve a value by key
      procedure, pass :: match   => TDict_match           ! Match a key-value combination
      procedure, pass :: nnext   => TDict_nnext           ! Fetch the next in line
      procedure, pass :: purge   => TDict_purge           ! Destructor/Finalizer
   end type TDict


   type TDictIterator
      type(TDict), pointer        ::  dict => null()
      type(TDictElement), pointer ::  elmptr => null()
   contains
      procedure, pass :: iterate    => TDictIterator_iterate ! Do a step, return next key-value pair
      procedure, pass :: init       => TDictIterator_init    ! Initialize with the pointer to the first element of a dictionary
   end type TDictIterator

   interface TDictIterator
      module procedure :: TDictIterator_constructor
   end interface TDictIterator


   type TDictElement                                    ! TODO: can I make this type private ?
      character(len=:), allocatable   :: key
      character(len=:), allocatable   :: val
        type(TDictElement), pointer   :: next => null()
   end type TDictElement


contains

! ------------------------------------------------------------------------------
!   Method:     purge
!   Purpose:    deletes all elements
!   Summary:    to be called as the destructor
!   Arguments:  none
! ------------------------------------------------------------------------------
subroutine TDict_purge(self)
   class(TDict), intent(inout)                :: self
   do while (self%count>0)
      call self%pop()
   enddo
end subroutine TDict_purge

! ------------------------------------------------------------------------------
!   Method:     pop
!   Purpose:    removes the top-level element and returns contents
!   Summary:    The new element is inserted in the beginning of the linked list
!   Arguments:  newkey  -  key to be written
!               newval  -  value to be written
! ------------------------------------------------------------------------------
subroutine TDict_pop(self,keystr,valstr)
   class(TDict), intent(inout)                :: self
   character(len=*), intent(out), optional    :: keystr
   character(len=*), intent(out), optional    :: valstr
   type (TDictElement), pointer               :: oldkv
   oldkv => self%elements
   if (associated(oldkv)) then
      if (present(keystr)) keystr = oldkv%key
      if (present(valstr)) valstr = oldkv%val
      self%elements => oldkv%next
      deallocate(oldkv)
      self%count =self%count - 1
   endif
end subroutine TDict_pop

! ------------------------------------------------------------------------------
!   Method:     push
!   Purpose:    stores a new key-value pair in the dictionary (inserted at position 0)
!   Summary:    The new element is inserted in the beginning of the linked list
!   Arguments:  newkey  -  key to be written
!               newval  -  value to be written
! ------------------------------------------------------------------------------
subroutine TDict_push(self,newkey,newval)
   class(TDict), intent(inout)                :: self
   character(len=*), intent(in)               :: newkey
   character(len=*), intent(in)               :: newval
   type (TDictElement), pointer               :: newkv

   allocate(newkv)
   newkv%key = newkey
   newkv%val = newval
   newkv%next => self%elements
   self%elements => newkv
   self%count = self%count + 1
end subroutine TDict_push

! ------------------------------------------------------------------------------
!   Method:     nnext
!   Purpose:    traverse the linked list, retrieving all pairs
!   Summary:    facilitates a loop over all dictionary elements like this:
!   Example:    elmptr => null()
!               do while (my_dict%nnext(keystr,valstr,elmptr))
!                  print *, keystr,'=',valstr
!               enddo
!
!               so that all elements can be processed in succession
!   Arguments:  keystr  -  returned key
!               valstr  -  returned value
!               newkv   -  in : pointer to the element to be visted
!                          out: pointer to the succeeding element
! ------------------------------------------------------------------------------
function TDict_nnext(self,keystr,valstr,newkv) result (success)
   logical                                     :: success
   class(TDict), intent(inout)                 :: self
   character(len=:), allocatable, intent(out)  :: keystr
   character(len=:), allocatable, intent(out)  :: valstr
   type (TDictElement), pointer                :: newkv
   if (associated(newkv)) then
      keystr = newkv%key
      valstr = newkv%val
      newkv => newkv%next
      success = .True.
   else
      success = .False.
   endif
end function TDict_nnext

! ------------------------------------------------------------------------------
!   Method:     match
!   Purpose:    traverse the linked list, scanning for matching key-value pair
!   Summary:    returns the value of the first element matching given key and given substring of the value.
!   Example:    my_dict%match('LOCATION','DA') matches key='LOCATION', value='DAAR', 'DATA', ... etc
!   Arguments:  keystr  -  key to match
!               substr  -  substring to match
!               newkv   -  pointer to the matching element
! ------------------------------------------------------------------------------

function TDict_match(self,keystr,substr) result (strout)
   character(len=:), allocatable              :: strout
   class(TDict), intent(inout)                :: self
   character(len=*), intent(in)               :: keystr
   character(len=*), intent(in)               :: substr
   type (TDictElement), pointer               :: newkv

   strout=''
   newkv => self%elements
   do while ((newkv%key /= keystr .or. index(newkv%val,substr) == 0) &
                                  .and. associated(newkv%next))
      newkv => newkv%next
   enddo
   if (newkv%key == keystr .and. index (newkv%val,substr) > 0) then
      strout = newkv%val
   else
      newkv => null()
   endif
end function

! ------------------------------------------------------------------------------
!   Method:     get
!   Purpose:    Gets the value given the key (first match of the key)
!               NB. The FIRST match will be in this implementation be the LAST pushed
!   Summary:    The name says it all ...
!   Example:    my_dict%get('LOCATION')
!   Arguments:  keystr  -  key to match
! ------------------------------------------------------------------------------
function TDict_get(self,keystr) result (strout)
   character(len=:), allocatable              :: strout
   class(TDict), intent(inout)                :: self
   character(len=*), intent(in)               :: keystr

   type (TDictElement), pointer               :: newkv
   newkv => self%elements
   do while (newkv%key /= keystr .and. associated(newkv%next))
      newkv => newkv%next
   enddo
   if (newkv%key == keystr) then
      strout = newkv%val
   endif
end function



! ---------------------------------------------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
!   Method:     constructor
!   Purpose:    create and initialize
!   Example:    NewIterator = TDictIterator(MyDictionary) 
!   Arguments:  dict: The TDict instance it applies to
! ------------------------------------------------------------------------------
type (TDictIterator) function TDictIterator_constructor(dict)
   type (TDict), pointer  ::  dict
   TDictIterator_constructor%dict => dict
   call TDictIterator_constructor%init()
end function TDictIterator_constructor


! ------------------------------------------------------------------------------
!   Method:     init
!   Purpose:    Sets the element pointer (back) to the first element of its TDict
!   Summary:    Initialization, also called by the constructor
!   Example:    my_iterator%init()
!   Arguments:  none
! ------------------------------------------------------------------------------
subroutine TDictIterator_init(self)
   class(TDictIterator), intent(inout)         :: self
   self%elmptr => self%dict%elements
end subroutine TDictIterator_init

! ------------------------------------------------------------------------------
!   Method:     iterate
!   Purpose:    get keys and values and do a nnext on the dictionary
!   Summary:    use the iterator to get next key and value in the list
!   Arguments:  keystr  -  returned key
!               valstr  -  returned value
! ------------------------------------------------------------------------------
function TDictIterator_iterate(self,keystr,valstr) result (success)
   logical                                     :: success
   class(TDictIterator), intent(inout)         :: self
   character(len=:), allocatable, intent(out)  :: keystr
   character(len=:), allocatable, intent(out)  :: valstr
   success = self%dict%nnext(keystr,valstr,self%elmptr)
end function TDictIterator_iterate


end module m_dictype
