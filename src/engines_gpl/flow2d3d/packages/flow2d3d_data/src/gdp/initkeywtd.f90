subroutine initkeywtd(gdp       )
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
! NONE
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
    character*20, dimension(:) , pointer :: keywrd
!
! Global variables
!
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !-----Initialize statics for keywtd
    !
    keywrd  => gdp%gdkeywtd%keywrd
    !
    keywrd(1) = 'table-name          '
    keywrd(2) = 'contents            '
    keywrd(3) = 'location            '
    keywrd(4) = 'geo-coordinates     '
    keywrd(5) = 'metric-coordinates  '
    keywrd(6) = 'frequency           '
    keywrd(7) = 'frequency-unit      '
    keywrd(8) = 'time-function       '
    keywrd(9) = 'reference-time      '
    keywrd(10) = 'time-unit           '
    keywrd(11) = 'time-step           '
    keywrd(12) = 'interpolation       '
    keywrd(13) = 'extrapolation       '
    keywrd(14) = 'parameter           '
    keywrd(15) = 'unit                '
    keywrd(16) = 'records-in-table    '
    keywrd(17) = 'xy-function         '
end subroutine initkeywtd
