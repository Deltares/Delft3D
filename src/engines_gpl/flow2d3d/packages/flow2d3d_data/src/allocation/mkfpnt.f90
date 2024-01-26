function mkfpnt(pntnam    ,length    ,gdp       )
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
! Request memory space with the dynamic array
! declaration routines to create a pointer for the
! single or double real array with pointer name PNTNAM
! The requested memory is initialized
!
!!--pseudo code and references--------------------------------------------------
!
!   - Determine the global precision fp used in the application 
!   - if (fp==sp) call subroutine mkrpnt
!     else
!   - if (fp==hp) call subroutine mkdpnt
!     endif
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
!
! Global variables
!
    integer      :: length
                                   !!  Total required array length
    integer(pntrsize) :: mkfpnt
    character(*) :: pntnam
                                   !!  Character string containing array
                                   !!  name (hence max 6 characters).
!
! Local variables
!
    integer(pntrsize), external :: mkdpnt
    integer(pntrsize), external :: mkrpnt
!
!! executable statements -------------------------------------------------------
!
    ! Determine global precision
    if (fp==sp) then
       !
       ! Single precision version
       !
       mkfpnt = mkrpnt(pntnam    ,length    ,gdp       )
       !
    elseif (fp==hp) then
       !
       ! Double precision version
       !
       mkfpnt = mkdpnt(pntnam    ,length    ,gdp       )
       !
    else
    endif
end function mkfpnt
