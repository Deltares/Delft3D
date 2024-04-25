module check_mpi_env
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!   Contains a check on running in mpi based on an environment variable
!   that can differ between different mpi implementations
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
implicit none
private
public :: running_in_mpi_environment

contains

function running_in_mpi_environment() result(usempi)
    implicit none
!
! Result variable
!
    logical                            :: usempi
!
! Local variables
!
    integer                            :: lenstr
    character(128)                     :: rankstr
!
!! executable statements -------------------------------------------------------
!
    ! use an environment variable to check whether MPI should be activated.
    ! unfortunately only the MPI calls are standardized, not the environment variables.
    ! so, we have to check a couple of different environment variables ...
    !
    ! verify whether MPICH2 (or derived) environment is used
    !
    call get_environment_variable('PMI_RANK', rankstr, lenstr)
    usempi = (lenstr > 0)
    !
    ! if not, verify whether OpenMPI 1.3 (or derived) environment is used
    !
    if (.not. usempi) then
       call get_environment_variable('OMPI_COMM_WORLD_RANK', rankstr, lenstr)
       usempi = (lenstr > 0)
    endif
    !
    ! if not, verify whether OpenMPI 1.2 (or derived) environment is used
    !
    if (.not. usempi) then
       call get_environment_variable('OMPI_MCA_ns_nds_vpid', rankstr, lenstr)
       usempi = (lenstr > 0)
    endif
    !
    ! if not, verify whether MVAPICH 1.1 environment is used
    !
    if (.not. usempi) then
       call get_environment_variable('MPIRUN_RANK', rankstr, lenstr)
       usempi = (lenstr > 0)
    endif
    !
    ! if not, verify whether MVAPICH 1.9 environment is used
    !
    if (.not. usempi) then
       call get_environment_variable('MV2_COMM_WORLD_RANK', rankstr, lenstr)
       usempi = (lenstr > 0)
    endif
    !
    ! if not, verify whether POE (IBM) environment is used
    !
    if (.not. usempi) then
       call get_environment_variable('MP_CHILD', rankstr, lenstr)
       usempi = (lenstr > 0)
    endif
end function running_in_mpi_environment

end module check_mpi_env
