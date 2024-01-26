subroutine delnef(filnam, gdp)
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
!    Function: Delete NEFIS and NetCDF output files if they exist
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use string_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                             , pointer :: lundia
!
! Global variables
!
    character(*), intent(in)       :: filnam !  Name of communication file xxxx-<case><label>
!
! Local variables
!
    integer         :: ind      ! Length of filenam 
    integer         :: luntmp
    integer         :: newlun
    logical         :: exists
    character(256)  :: locfnm   ! Local file name 
!
!! executable statements -------------------------------------------------------
!
    lundia              => gdp%gdinout%lundia
    !
    locfnm = ' '
    locfnm = filnam
    call remove_leading_spaces(locfnm    ,ind       )
    !
    if (locfnm(ind-2:ind) == '.nc') then
       !
       ! test files existence - NetCDF version
       !
       inquire (file = locfnm(:ind), exist = exists)
       if (exists) then
          call prterr(lundia,'G051','Removing old output file: '//locfnm(:ind))
          open (newunit=luntmp, file = locfnm(:ind))
          close (luntmp, status = 'delete')
       endif
    else
       !
       ! test files existence - NEFIS DAT file
       !
       inquire (file = locfnm(:ind) // '.dat', exist = exists)
       if (exists) then
          call prterr(lundia,'G051','Removing old output file: '//locfnm(:ind)// '.dat')
          open (newunit=luntmp, file = locfnm(:ind) // '.dat')
          close (luntmp, status = 'delete')
       endif
       !
       ! test files existence - NEFIS DEF file
       !
       inquire (file = locfnm(:ind) // '.def', exist = exists)
       if (exists) then
          call prterr(lundia,'G051','Removing old output file: '//locfnm(:ind)// '.def')
          open (newunit=luntmp, file = locfnm(:ind) // '.def')
          close (luntmp, status = 'delete')
       endif
    endif
end subroutine delnef
