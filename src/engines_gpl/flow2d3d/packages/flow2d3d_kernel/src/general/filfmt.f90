subroutine filfmt(lundia    ,messag    ,fmttmp    ,error     ,gdp       )
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
!    Function: Reads the file format definition ('UN' of 'FR') of
!              an attribute file and writes the full FORTRAN
!              counterpart to var. FMTTMP.
!              FMTTMP(ibeg:iend) contains UN or FR on input
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
!
! Global variables
!
    integer                     :: lundia !  Description and declaration in inout.igs
    logical       , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    character(*)                :: messag !!  Message to print in case of error(s)
    character(11)               :: fmttmp !!  Help variable for file format
!
!
! Local variables
!
    integer           :: ifmtf     ! Index of 'FR' in file string 
    integer           :: ifmtu     ! Index of 'UN' in file string 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    !-----calculate file format definition (unformatted / freeformatted)
    !     index = 0, means tekst did not occure
    !
    ifmtu = index(fmttmp, 'UN')
    ifmtf = index(fmttmp, 'FR')
    if (ifmtu/=0 .and. ifmtf==0) then
       fmttmp = 'unformatted'
    elseif (ifmtu==0 .and. ifmtf/=0) then
       fmttmp = 'formatted'
    else
       error = .true.
       call prterr(lundia    ,'U036'    ,messag    )
    !
    endif
end subroutine filfmt
