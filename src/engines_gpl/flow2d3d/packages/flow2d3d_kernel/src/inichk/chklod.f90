subroutine chklod(lundia    ,error     ,nto       ,nsrc      , &
                & gdp       )
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
!    Function: Checks the local array dimension, which has been
!              specified in various routines. If the specified
!              input parameters (Number Total Openings and NSRC)
!              in the MD-file exceeds these dimensions then error
!              message will be written to diagnostics file
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
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
    include 'pardef.igd'
!
! Global variables
!
    integer              :: lundia !  Description and declaration in inout.igs
    integer, intent(in)  :: nsrc   !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)  :: nto    !  Description and declaration in esm_alloc_int.f90
    logical, intent(out) :: error  !!  Flag=TRUE if an error is encountered
!
!
!! executable statements -------------------------------------------------------
!
    !
    !-----test NTO from DIMRD with maximum value MXNTO
    !
    if (nto>mxnto) then
       call prterr(lundia    ,'G015'    ,' '       )
       !
       error = .true.
    endif
    !
    !-----test NSRC from DIMRD with maximum value MXNSRC
    !
    if (nsrc>mxnsrc) then
       call prterr(lundia    ,'G050'    ,' '       )
       !
       error = .true.
    endif
end subroutine chklod
