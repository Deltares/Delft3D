!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
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
!

!> This module contains all the methods for the datatype tEcElementSet.
module m_ec_stringbuffer
   use m_ec_typedefs
   use m_ec_message

   implicit none

   private

   public :: ecStringbufferCreate           ! constructor
   public :: ecStringbufferFree             ! destructor
   public :: ecStringbufferSearchblk

    contains

    function ecStringbufferCreate(stringBufferId) result(stringbufferPtr)
       implicit none
       type(tEcStringbufferPtr), pointer        :: stringbufferPtr !< the new Stringbuffer, intent(out)
       integer, intent(in)                      :: stringbufferId  !< unique Stringbuffer id  (obsolete, but maintained for symmetry)
       stringbufferPtr => null()
       allocate(stringbufferPtr)
       allocate(stringbufferPtr%Ptr%lines(10))
       allocate(stringbufferPtr%Ptr%numbers(10))
       stringbufferPtr%Ptr%nlines = 0
       allocate(stringbufferPtr%Ptr%bcnames(10))
       allocate(stringbufferPtr%Ptr%from_line(10))
       allocate(stringbufferPtr%Ptr%thru_line(10))
       stringbufferPtr%Ptr%nblocks = 0
    end function ecStringbufferCreate

    function ecStringBufferFree(stringbuffer) result(success)
       implicit none
       logical                                 :: success
       type (tEcStringbuffer), intent(inout)   :: stringbuffer

       integer                                 :: istat

       success = .true.
       if (allocated(stringbuffer%lines)) then
           deallocate(stringbuffer%lines, stat = istat)
           success = (success .and. (istat == 0))
       endif
       if (allocated(stringbuffer%numbers)) then
           deallocate(stringbuffer%numbers, stat = istat)
           success = (success .and. (istat == 0))
       endif
       if (allocated(stringbuffer%bcnames)) then
           deallocate(stringbuffer%bcnames, stat = istat)
           success = (success .and. (istat == 0))
       endif
       if (allocated(stringbuffer%from_line)) then
           deallocate(stringbuffer%from_line, stat = istat)
           success = (success .and. (istat == 0))
       endif
       if (allocated(stringbuffer%thru_line)) then
           deallocate(stringbuffer%thru_line, stat = istat)
           success = (success .and. (istat == 0))
       endif
    end function ecStringBufferFree

    subroutine ecStringbufferSearchblk(strings,bcname,from_line,thru_line)
    implicit none
    type (tEcStringbuffer),     intent(in)      ::  strings
    character(len=*),           intent(in)      ::  bcname
    integer,                    intent(out)     ::  from_line
    integer,                    intent(out)     ::  thru_line
    !
    integer     ::  i
    !
    from_line=-1                ! Negative outcome of this sub for either intent-out dummies
    thru_line=-1                ! are regarded as failure, meaning block-not-found
    do i = 1, strings%nblocks
        if (trim(bcname)==trim(strings%bcnames(i))) then
           from_line=strings%from_line(i)
           thru_line=strings%thru_line(i)
        endif
    enddo
    end subroutine ecStringbufferSearchblk


end module m_ec_stringbuffer
