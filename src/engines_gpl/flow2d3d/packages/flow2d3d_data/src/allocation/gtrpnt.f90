function gtrpnt(pntnam    ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
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
!!--description-----------------------------------------------------------------
!
!   Look up a real pointer name using FSM
!   and return an index into the appropriate array.
!
!!--pseudo code and references--------------------------------------------------
!
!   - Determine the length of the pointer name.
!   - Call the FSM GETPTR routine.
!   - If this returns zero, an error occured: an error message will be printed 
!     (by ERRPNT) and the program terminates (in ERRPNT)
!
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    include 'fsm.i'
!
! Global variables
!
    integer(pntrsize) :: gtrpnt
    character(*)    :: pntnam
                                   !!  Character string containing array
                                   !!  name (hence max 6 characters).
!
! Local variables
!
    integer                        :: ind    ! Actual length of character string
!
!! executable statements -------------------------------------------------------
!
    ! Define length of pointer name (array name)
    ind = index(pntnam, ' ')
    if (ind==0) ind = len(pntnam) + 1
    !
    ! Call dynamic array declaration function GETPTR to get requested
    ! array pointer
    !
    gtrpnt = getptr(pntnam(:ind - 1))
    if (gtrpnt==0) then
       call errpnt(pntnam, 'real', 'lookup', gdp)
       ! program is terminated in errpnt
    endif
end function gtrpnt
