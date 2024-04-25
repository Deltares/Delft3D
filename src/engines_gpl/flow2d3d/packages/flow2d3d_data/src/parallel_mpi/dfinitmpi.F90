#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
subroutine dfinitmpi
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
!   Join parallel application within Delft3D-FLOW
!
!!--pseudo code and references--------------------------------------------------
!
!   enroll in MPI
!   get node number
!   determine total number of processes
!   determine whether this is a parallel run or not
!   define MPI constants for communication within Delft3D-FLOW
!   determine precision for type real in case of communication (single or double)
!
!
!!--declarations----------------------------------------------------------------
#ifdef HAVE_MPI
    use mpi
#endif
    use precision
    use dfparall
    !
    implicit none
!
! Local variables
!
    integer                            :: ierr      ! error value of MPI call
    integer                            :: len
    character(128)                     :: msgstr    ! string to pass message
    character(128)                     :: rankstr
#ifdef HAVE_MPI
    character(MPI_MAX_PROCESSOR_NAME)  :: host      ! hostname       for current MPI process
    character(MPI_MAX_PROCESSOR_NAME)  :: processor ! processor name for current MPI process
#else
    integer, parameter                 :: MPI_SUCCESS = 0
#endif
    logical                            :: mpi_is_initialized
!
!! executable statements -------------------------------------------------------
!
    ! enroll in MPI
    !
    mpi_is_initialized = .false.
    ierr = MPI_SUCCESS

    !
    ! use an environment variable to check whether MPI should be activated.
    ! unfornately only the MPI calls are standardized, not the environment variables.
    ! so, we have to check a couple of different environment variables ...
    !
    ! verify whether MPICH2 (or derived) environment is used
    !
    call get_environment_variable('PMI_RANK', rankstr, len)
    usempi = (len > 0)
    !
    ! if not, verify whether OpenMPI 1.3 (or derived) environment is used
    !
    if (.not. usempi) then
       call get_environment_variable('OMPI_COMM_WORLD_RANK', rankstr, len)
       usempi = (len > 0)
    endif
    !
    ! if not, verify whether OpenMPI 1.2 (or derived) environment is used
    !
    if (.not. usempi) then
       call get_environment_variable('OMPI_MCA_ns_nds_vpid', rankstr, len)
       usempi = (len > 0)
    endif
    !
    ! if not, verify whether MVAPICH 1.1 environment is used
    !
    if (.not. usempi) then
       call get_environment_variable('MPIRUN_RANK', rankstr, len)
       usempi = (len > 0)
    endif
    !
    ! if not, verify whether MVAPICH 1.9 environment is used
    !
    if (.not. usempi) then
       call get_environment_variable('MV2_COMM_WORLD_RANK', rankstr, len)
       usempi = (len > 0)
    endif  
    !
    ! if not, verify whether POE (IBM) environment is used
    !
    if (.not. usempi) then
       call get_environment_variable('MP_CHILD', rankstr, len)
       usempi = (len > 0)
    endif

    if (usempi) then
       !
       ! test first if MPI was initialized (to avoid boxing MPI_init in DD runs)
       !
#ifdef HAVE_MPI
       call mpi_initialized( mpi_is_initialized, ierr )
#endif
       if ( ierr /= MPI_SUCCESS ) then
          write (msgstr,'(a,i5)') 'MPI produces some internal error in mpi_initialized - return code is ',ierr
          write (6,*) trim(msgstr)
          call cstop( 1, char(0) )
       endif
       !
       ! early return if MPI was already initialized before...
       !
       if (mpi_is_initialized) then
          mpi_initialized_by_engine = .FALSE.
          ! external component should also have set engine_comm_world via BMI
       else
          mpi_initialized_by_engine = .TRUE.
#ifdef HAVE_MPI
          call mpi_init ( ierr )
          engine_comm_world = MPI_COMM_WORLD
#endif
          if ( ierr /= MPI_SUCCESS ) then
             write (msgstr,'(a,i5)') 'MPI produces some internal error in mpi_init - return code is ',ierr
             write (6,*) trim(msgstr)
             call cstop( 1, char(0) )
          endif
       endif
    endif
    !
    ! initialize common variables
    !
    inode = 0
    nproc = 1
    !
    ! get node number INODE
    !
    if (usempi) then
#ifdef HAVE_MPI
       host      = 'unknown'
       processor = 'unknown'
       call mpi_comm_rank ( engine_comm_world, inode, ierr )
       call util_getenv('HOSTNAME',host)
       call mpi_get_processor_name (processor,len,ierr)
       write (6,'(a,i3.3,4a)') 'MPI process number ', inode, ' has host ', trim(host), ' and is running on processor ', trim(processor)
#endif
    endif

    inode = inode + 1

    if (usempi) then
       if ( ierr /= MPI_SUCCESS ) then
          write (msgstr,'(a,i5,a,i3.3)') 'MPI produces some internal error - return code is ',ierr,' and node number is ',inode
          write (6,*) trim(msgstr)
          call cstop( 1, char(0) )
       endif
    !
    ! determine total number of processes
    !
#ifdef HAVE_MPI
       call mpi_comm_size ( engine_comm_world, nproc, ierr )
#endif
       if ( ierr /= MPI_SUCCESS ) then
          write (msgstr,'(a,i5,a,i3.3)') 'MPI produces some internal error - return code is ',ierr,' and node number is ',inode
          write (6,*) trim(msgstr)
          call cstop( 1, char(0) )
       endif
    endif
    !
    ! determine whether this is a parallel run or not
    !
    if ( nproc > 1 ) then
       parll = .true.
    else
       parll = .false.
    endif
    !
    ! define MPI constants for communication within Delft3D-FLOW
    !
#ifdef HAVE_MPI
    dfint  = MPI_INTEGER
    dfreal = MPI_REAL
    dfdble = MPI_DOUBLE_PRECISION
    dfchar = MPI_CHARACTER
    dfmax  = MPI_MAX
    dfmin  = MPI_MIN
    dfsum  = MPI_SUM
    !
    ! determine precision for type real in case of communication (single or double)
    !
    if ( fp == hp ) then
       dfloat = dfdble
    else
       dfloat = dfreal
    endif
    !
    if ( prec == hp ) then
       dfprec = dfdble
    else
       dfprec = dfreal
    endif
#endif

end subroutine dfinitmpi
