subroutine errpnt(pntnam    ,dtype     ,callty    ,gdp       )
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
!   ERRPNT is called by the GT?PNT/MK?PNT functions to print
!   an error message and terminate the program (process/thread).
!   An attempt is made to write to the diagnostic file.
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
    integer              , pointer :: lundia
!
! Global variables
!
    character(*), intent(in)       :: callty !!  Type of call 'getprt' or 'makptr'
    character(*), intent(in)       :: pntnam !!  Character string containing array name (hence max 6 characters).
    character(*), intent(in)       :: dtype  !!  Character string containing data type of the pointer to be created
!
! Local variables
!
!   NONE
!
!! executable statements -------------------------------------------------------
!
    lundia => gdp%gdinout%lundia
    if (lundia /= 0) then
       write (lundia, *) '*** ERROR in ', dtype, ' array ', callty, ' of ', pntnam
       write (lundia, *) '          Contact Deltares'
    endif
    write (*, *) '*** ERROR in ', dtype, ' array ', callty, ' of ', pntnam
    write (*, *) '          Contact Deltares'
    call d3stop(1, gdp)
end subroutine errpnt
