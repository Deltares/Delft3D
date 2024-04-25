subroutine run_swan (casl)
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
!     *** Run swan; produce output file swanout with values on     ***
!     *** swan computational grid                                  ***
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use swan_input
    use wave_mpi, only: numranks, wave_mpi_bcast, wave_mpi_barrier, engine_comm_world, MPI_SUCCESS, SWAN_GO
    use system_utils, only: SCRIPT_EXTENSION
    !
    implicit none
!
! Global variables
!
    integer                 :: ierr
    integer                 :: ind
    integer                 :: strlen 
    integer                 :: nuerr
    logical                 :: ex
    character(*)            :: casl
    character(4)            :: labl     
    character(5)            :: prints
    character(6)            :: append
    character(256)          :: string
    character(1024)         :: swanCommand
!
!! executable statements -------------------------------------------------------
!
    labl   = ' '
    !
    ! SWAN execution
    !
    if (swan_run%exemode == SWAN_MODE_LIB) then
       !
       ! As built-in function.
       !
       if (swan_run%scriptname /= ' ') then
           !
           ! The name of a preprocessing script has been specified. Call it.
           ! The script will generate 'INPUT' based on 'swan.inp'.
           !
           write (*,'(2a)') 'Preprocessing SWAN input file using: ', trim(swan_run%scriptname)
           write(swanCommand, '(3a)') trim(swan_run%scriptname)
           !
           ! In debug mode, util_system wants spaces at the end...
           !
           call util_system(swanCommand(1:len_trim(swanCommand)+5))
           !
           inquire (file = 'INPUT', exist = ex)
           if (.not. ex) then
              write (*,'(3a)') '*** ERROR: preprocessing script "', trim(swan_run%scriptname), '" ran without generating ''INPUT'' file'
              call wavestop(1, '*** ERROR: preprocessing script ran without generating ''INPUT'' file')
           endif
       endif
       !
       write(*,'(a)')'>>...Start SWAN run'
       if (numranks > 1) then
          call wave_mpi_bcast(SWAN_GO, ierr)
          if ( ierr == MPI_SUCCESS ) then
             call swan(engine_comm_world)
             call wave_mpi_barrier(ierr)
          endif
          if ( ierr /= MPI_SUCCESS ) then
             write (*,'(a,i5)') 'MPI produces some internal error - return code is ',ierr
             call wavestop(1, '*** ERROR: MPI produced an internal error')
          endif
       else
          call swan(engine_comm_world)
       endif
    else
       !
       ! As executable
       !
       write(*,'(3a)')'>>...Check file swan_',SCRIPT_EXTENSION(2:),'.log'
       write(swanCommand, '(4a)') 'swan', SCRIPT_EXTENSION
       !
       ! In debug mode, util_system wants spaces at the end...
       !
       call util_system(swanCommand(1:len_trim(swanCommand)+5))
       !
       inquire (file = 'norm_end', exist = ex)
       if (.not. ex .and. SCRIPT_EXTENSION=='.sh') then
          write (*,'(a)') '*** WARNING: unable to run SWAN using "swan.sh". Trying with "swan.bat" ...'
          write(swanCommand,'(3a)') 'swan.bat'
          !
          ! In debug mode, util_system wants spaces at the end...
          !
          call util_system(swanCommand(1:len_trim(swanCommand)+5))
       endif
    endif
    write(*,'(a)')'>>...End of SWAN run'
    !
    ! Check SWAN output file norm_end
    !
    inquire (file = 'norm_end', exist = ex)
    if (.not. ex) then
       write (*,'(a)') '*** ERROR: file ''norm_end'' expected to signal a correct SWAN calculation'
       call wavestop(1, '*** ERROR: file ''norm_end'' expected to signal a correct SWAN calculation')
    endif
    !
    ! Check SWAN output file PRINT
    !
    call scan_fl(swan_run%checkVersionNumber, swan_run%versionNumberOK)
    prints        = 'PRINT'
    string(1:9)  = 'swn-diag.'
    string(10:)  = casl
    ind = index(string, ' ')
    string(ind:) = labl
    append       = 'append'
    call cp_file(prints     ,string    ,append    ,nuerr             )
    if (nuerr > 0) then
       write (*, '(a,i3)') '*** ERROR: While appending PRINT to diag file: ', nuerr
    endif
    !
    ! Remove SWAN input/output/tmp files (except SWANOUT output data file)
    !
    call rm_del('norm_end')
    call rm_del('INPUT')
    call rm_del('PRINT')
    call rm_del('source')
    call rm_del('temp')
    call rm_del('swaninit')
    call rm_del('INSTR')
    call rm_del('INSTU')
    call rm_del('BOTNOW')
    call rm_del('CURNOW')
    call rm_del('AICENOW')
    string = 'fname'
    call rm_del(string) 
end subroutine run_swan
