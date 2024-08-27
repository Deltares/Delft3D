module handles
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
!-------------------------------------------------------------------------------
!  
!  
!!--description-----------------------------------------------------------------
!
! Generic handles
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
    !
    public handletype
    public registerhandletype
    public gethandletype_integer
    public gethandletype_string
!
!! -----------------------------------------------------------------------------
!
    integer, parameter :: HTYPELENGTH = 32
    type handletype
       integer, pointer :: this => NULL()
       integer :: htype = -999
    endtype handletype

    character(HTYPELENGTH), dimension(:), pointer :: handlestring => NULL()

contains

integer function registerhandletype(handletypestring)
!
! Global variables
!
    character(*)       ,intent(in)  :: handletypestring
!
! Local variables
!
    integer                                       :: htype
    integer                                       :: i
    integer                                       :: ierr
    character(HTYPELENGTH), dimension(:), pointer :: newhandlestring
    character(HTYPELENGTH)                        :: newhandletypestring
!
!! executable statements -------------------------------------------------------
!
    !
    ! copy handletypestring to local variable to clip or extend length
    !
    newhandletypestring = handletypestring
    !
    if (associated(handlestring)) then
       htype = size(handlestring)+1
       !
       ! verify whether handle type string is unique
       !
       do i = 1,htype-1
          if (handlestring(i)==newhandletypestring) then
             !
             ! handle type string is not unique, return error code
             !
             registerhandletype = -999
             return
          endif
       enddo
       !
       ! handle type string is unique, add it to the list
       !
       allocate(newhandlestring(htype))
       do i = 1,htype-1
          handlestring(i) = newhandlestring(i)
       enddo
       deallocate(handlestring, stat=ierr)
       handlestring => newhandlestring
    else
       !
       ! create a table
       !
       allocate(handlestring(1))
       htype = 1
    endif
    !
    ! store the handle type string in the local table and return the index
    !
    handlestring(htype) = newhandletypestring
    registerhandletype  = htype
end function registerhandletype


integer function gethandletype_integer(handle)
!
! Global variables
!
    type(handletype)   ,intent(in)  :: handle
!
!! executable statements -------------------------------------------------------
!
    if (handle%htype>0) then
       gethandletype_integer = -handle%htype
       if (associated(handlestring)) then
          if (handle%htype<=size(handlestring)) then
             gethandletype_integer = handle%htype
          endif
       endif
    else
       gethandletype_integer = handle%htype
    endif
end function gethandletype_integer


character(HTYPELENGTH) function gethandletype_string(handle)
!
! Global variables
!
    type(handletype)   ,intent(in)  :: handle
!
!! executable statements -------------------------------------------------------
!
    gethandletype_string = 'INVALID HANDLE'
    if (handle%htype>0) then
       if (associated(handlestring)) then
          if (handle%htype<=size(handlestring)) then
             gethandletype_string = handlestring(handle%htype)
          endif
       endif
    endif
end function gethandletype_string

end module handles
