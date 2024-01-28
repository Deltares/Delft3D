subroutine rdbcb(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
               & runid     ,filbcb    ,itstrt    ,itfinish  ,gdp       )
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
!    Function: - Reads the time dependent barrier height data for
!                use in RTC from the attribute file Filbcb.
!              - Tests the file or data consistency.
!              - Checks whether the file exists or the required
!                data is not empty.
!              - An essential assumption is that the data has to
!                be specified sequentially in time. This imply
!                that NT times NSLUV of Flow should exist in the
!                file (NT unrestricted).
!              - Writes the data to an unformatted file
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use string_module
    use system_utils, only: exifil
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
    integer         :: itfinish !  Description and declaration in inttim.igs
    integer         :: itstrt   !  Description and declaration in inttim.igs
    integer         :: lundia   !  Description and declaration in inout.igs
    integer         :: lunmd    !  Description and declaration in inout.igs
    integer         :: nrrec    !  Pointer to the record number in the MD-file
    logical         :: error    !  Flag=TRUE if an error is encountered
    character(*)    :: filbcb   !  File name for the time varying data at barriers
    character(*)    :: mdfrec   !  Standard rec. length in MD-file (300)
    character(*)    :: runid    !  Run identification code for the current simulation (used to determine
                                !  the names of the in- /output files used by the system)
!
! Local variables
!
    integer                        :: lenc                 ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer                        :: lf                   ! Help var. specifying the length of character variables for file names 
    integer                        :: lrid                 ! Length of character string runid 
    integer                        :: lunrd
    integer, external              :: newlun
    integer                        :: nlook                ! Help var.: nr. of data to look for in the MD-file 
    integer                        :: ntrec                ! Help. var to keep track of NRREC 
    logical                        :: lerror               ! Flag=TRUE if a local error is encountered 
    logical                        :: newkw                ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    character(12)                  :: fildef               ! Default file name (usually = blank) 
    character(6)                   :: keyw
    character(300)                 :: message
!
!
!! executable statements -------------------------------------------------------
!
    !
    !-----Initialize local parameters
    !
    lerror = .false.
    newkw  = .true.
    nlook  = 1
    fildef = ' '
    !
    !-----Initialize global parameters
    !
    filbcb = fildef
    !
    !-----locate 'Filbcb' record for time varying process data at
    !     barriers in extra input file
    !
    keyw = 'Filbcb'
    ntrec = nrrec
    lenc = 12
    call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
              & mdfrec    ,filbcb    ,fildef    ,lenc      ,nrrec     , &
              & ntrec     ,lundia    ,gdp       )
    !
    !
    !-----reading error?
    !
    if (lerror) then
       lerror = .false.
       filbcb = fildef
    endif
    !
    !-----read data from external file
    !
    if (filbcb/=fildef) then
       call remove_leading_spaces(runid     ,lrid      )
       !
       !-------define length of file name
       !
       call remove_leading_spaces(filbcb    ,lf        )
       !
       !-------test file existence <YES> -> open file <NO> -> error
       !
       if (exifil(filbcb, lundia)) then
          !
          !---------Open FILBCB to read data from
          !
          open (newunit=lunrd, file = filbcb(:lf), form = 'formatted', status = 'old')
          write (message, '(2a)') 'Reading Barrier Heights file ', filbcb(:lf)
          call prterr(lundia, 'G051', trim(message))
          !
          !---------Read/Write data blocks
          !
          call rwbcb(lundia    ,lunrd     ,filbcb    ,error     ,itstrt    , &
                   & itfinish  ,gdp       )
          !
          close (lunrd)
          if (error) then
          endif
       else
          error = .true.
       endif
    endif
end subroutine rdbcb
