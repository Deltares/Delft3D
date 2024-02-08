!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
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
!-------------------------------------------------------------------------------

!< applicatin for testing
   program dhydrology_test
    use m_cmdlargs
    use dhydrology_reader_module
    use properties
    use TREE_STRUCTURES
    use MessageHandling

    implicit none

    logical                             :: verbose, jacompare, janewref
    character(len=nameLenght)           :: configfile        !< File name + path of configuration
    character(len=nameLenght)           :: configfilename    !< File name of configuration
    character(len=nameLenght)           :: configfilepath    !< File path ofconfiguration
    type(tree_data), pointer            :: config_ptr
    character(len=100)                  :: startupDir        !< File path ofconfiguration
    character(len=500)                  :: regel
    character(len=50)                   :: testname
    character(len=50)                   :: testlistname
    integer                             :: readerr
    integer                             :: itest
    integer                             :: iostat
    logical                             :: multitest, singletest, alltest
    integer                             :: i
    
    logical                             :: opened, exist !RL666
    logical                             :: ja_custom_data_format
    logical                             :: ja_custom_time_format
    type(ec_module_query)               :: query
    
    call getcwd(startupDir)                         
    if (.not.argstring('-c','',configfilename)) then
       call mess(LEVEL_ERROR, 'No config file specified')
       stop ''
    endif
   
    !read configuration file
    readerr = dhydrologyReadConfiguration(configfilename)
    
    ! create a query
    allocate(query%x(1))
    allocate(query%y(1))
    
    query%quantityname  = 'p'
    query%varname       = 'p'
    query%x             =  448200
    query%y             =  5339200
    query%vectormax     =  1      ! Max number of outputs   
    query%inFilename    =  configurationFile%netcdfInputFileName
    query%inFiletype    =  14     ! Netcdf file type                                                                      
    !interpolation method
    query%method        =  6
    query%operand       =  2
    !for time interpolation
    query%tgt_refdate   =  20140101
    query%tgt_tunit     =  1
    query%tgt_tzone     =  0.0d0
    query%jasferic      = .false.
    query%missing_value = -999.d0
    query%npoint        =  1
    query%dtnodal       =  46800
    
    !interpolate values at specific time and space
    readerr             = dhydrologyReadMap(query)

 end program dhydrology_test
