subroutine tmpcheck( runid, reusetmp,  tmpexist,  gdp )
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
!    Function: Checks if all necessary temporary (unformatted) files
!              are already present.
!              These files are respectively :
!                   TMP_//runid//.bcc
!                   TMP_//runid//.bch
!                   TMP_//runid//.bcq
!                   TMP_//runid//.bct
!                   TMP_//runid//.dis
!                   TMP_//runid//.eva
!                   TMP_//runid//.tem
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use string_module
    use properties
    use system_utils, only: exifil
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer , pointer :: lundia
!
! Global variables
!
    character(*)   , intent(in)  :: runid       !!  Run identification code for the cur-
                                                !!  rent simulation (used to determine
                                                !!  the names of the in- /output files
                                                !!  used by the system)
    logical        , intent(out) :: reusetmp    !!  TRUE when temporary files will be reused
                                                !!  if possible
    logical        , intent(out) :: tmpexist    !!  TRUE when all necessary temporary files
                                                !!  are available
!
! Local variables
!
    integer                      :: lrid        ! Length of character string runid 
    logical                      :: ex          ! Flag to test if file exists 
    character(12)                :: fildef      ! Empty file name 
    character(256)               :: filnam      ! File name for the time varying data file 
    character(256)               :: filrd       ! File name read from MD FLOW file
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    lundia  => gdp%gdinout%lundia
    !
    !  
    ex       = .true. 
    reusetmp = .false.
    tmpexist = .false.
    fildef   = ' '
    !
    ! Define length of runid
    !
    call remove_leading_spaces(runid, lrid)
    !  
    ! Check keyword 'ReTMP' which defines if temporary files 
    ! have to be reused if possible        
    !
    call prop_get_logical(gdp%mdfile_ptr, '*', 'ReTMP', reusetmp)
    !
    ! Check if all necessary temporary files are available if they 
    ! need to be reused.
    !
    if ( reusetmp ) then
        !
        ! Locate 'FilbcC' record in MD-file/flow file
        ! to test temporary files defining time varying concentration data
        ! at open boundaries
        !   
        if ( ex ) then
             filrd = fildef
             call prop_get_string(gdp%mdfile_ptr, '*', 'FilbcC', filrd)
             if (filrd /= fildef) then
                filnam = 'TMP_' // runid(:lrid) // '.bcc'
                ex = exifil(filnam)
            endif
        endif
        !
        ! Locate 'FilbcH' record in MD-file/flow file
        ! to test temporary file defining harmonic boundary conditions
        !
        if ( ex ) then
             filrd = fildef
             call prop_get_string(gdp%mdfile_ptr, '*', 'FilbcH', filrd)
             if (filrd /= fildef) then
                filnam = 'TMP_' // runid(:lrid) // '.bch'
                ex = exifil(filnam)
            endif
        endif 
        !
        ! Locate 'Filana' record in MD-file/flow file
        ! to test temporary file defining astronomical boundary conditions
        !
        if ( ex ) then
             filrd = fildef
             call prop_get_string(gdp%mdfile_ptr, '*', 'Filana', filrd)
             if (filrd /= fildef) then
                filnam = 'TMP_' // runid(:lrid) // '.bch'
                ex = exifil(filnam)
            endif
        endif 
        !
        ! Locate 'FilbcQ' record in MD-file/flow file
        ! to test temporary files defining time varying concentration data
        ! at open boundaries
        !   
        if ( ex ) then
             filrd = fildef
             call prop_get_string(gdp%mdfile_ptr, '*', 'FilbcQ', filrd)
             if (filrd /= fildef) then
                filnam = 'TMP_' // runid(:lrid) // '.bcq'
                ex = exifil(filnam)
            endif
        endif
        !
        ! Locate 'FilbcT' record in MD-file/flow file
        ! to test temporary files defining time varying hydrodynamic data
        ! at open boundaries
        !    
        if ( ex ) then
             filrd = fildef
             call prop_get_string(gdp%mdfile_ptr, '*', 'FilbcT', filrd)
             if (filrd /= fildef) then
                filnam = 'TMP_' // runid(:lrid) // '.bct'
                ex = exifil(filnam)
            endif
        endif
        !
        ! Locate 'Fildis' record in MD-file/flow file
        ! to test temporary files defining time varying data at discharge
        ! locations
        !   
        if ( ex ) then
             filrd = fildef
             call prop_get_string(gdp%mdfile_ptr, '*', 'Fildis', filrd)
             if (filrd /= fildef) then
                filnam = 'TMP_' // runid(:lrid) // '.dis'
                ex = exifil(filnam)
            endif
        endif
        !
        ! Locate 'Fileva' record in MD-file/flow file
        ! to test temporary files defining time varying data for
        ! evaporations and rain
        !   
        if ( ex ) then
             filrd = fildef
             call prop_get_string(gdp%mdfile_ptr, '*', 'Fileva', filrd)
             if (filrd /= fildef) then
                filnam = 'TMP_' // runid(:lrid) // '.eva'
                ex = exifil(filnam)
            endif
        endif
        !
        ! Locate 'Filtmp' record in MD-file/flow file
        ! to test temporary files defining time varying data for heat models
        !   
        if ( ex ) then
             filrd = fildef
             call prop_get_string(gdp%mdfile_ptr, '*', 'Filtmp', filrd)
             if (filrd /= fildef) then
                filnam = 'TMP_' // runid(:lrid) // '.tem'
                ex = exifil(filnam)
            endif
        endif
        !
        if (ex) then
            tmpexist = .true.
            write (lundia,'( a)') '*** WARNING Temporary files are reused'
        else
            write (lundia,'(3a)') '*** WARNING Reusing temporary files not possible: file ', trim(filnam), ' is missing'
            write (lundia,'( a)') '***         All files will be recreated'
        endif
    endif
end subroutine tmpcheck
