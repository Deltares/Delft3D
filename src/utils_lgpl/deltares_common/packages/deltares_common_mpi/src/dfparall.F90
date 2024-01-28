module dfparall
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
!   Contains data with respect to parallel process within
!   Delft3D-FLOW based on distributed-memory apprach using MPI
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
implicit none
!
! Module parameters
!
integer, parameter :: master = 1   ! rank of master process
integer, parameter :: ired   = 1   ! integer used to colour subdomains 'red' for
                                   ! determining sequence of sweeps (=1,2,3,4)
integer, parameter :: iyelow = 2   ! integer used to colour subdomains 'yellow' for
                                   ! determining sequence of sweeps (=2,3,4,1)
integer, parameter :: igreen = 3   ! integer used to colour subdomains 'green' for
                                   ! determining sequence of sweeps (=3,4,1,2)
integer, parameter :: iblack = 4   ! integer used to colour subdomains 'black' for
                                   ! determining sequence of sweeps (=4,1,2,3)
integer, parameter :: ihalom = 3   ! width of halo area in x-direction
integer, parameter :: ihalon = 3   ! width of halo area in y-direction
!
! Module variables
!
logical, save  :: mpi_initialized_by_engine
integer, target, save  :: engine_comm_world
integer, save  :: inode  = 1       ! rank of present node
integer, save  :: idir   = 0       ! direction of domain cutting  
                                   ! 1(row n) or 2(column m)
integer, save  :: nproc  = 0       ! number of nodes
integer, save  :: dfint  = 0       ! MPI datatype for integers
integer, save  :: dfloat = 0       ! datatype for real indicating single or double precision
integer, save  :: dfreal = 0       ! MPI datatype for (single precision) reals
integer, save  :: dfdble = 0       ! MPI datatype for double precision
integer, save  :: dfchar = 0       ! MPI datatype for characters
integer, save  :: dfmax  = 0       ! MPI collective maximum operation
integer, save  :: dfmin  = 0       ! MPI collective minimum operation
integer, save  :: dfsum  = 0       ! MPI collective summation
integer, save  :: dfprec = 0       ! datatype for real indicating single or double precision (only bodsed/dps)
logical, save  :: parll  = .false. ! flag to denote run as parallel (.TRUE.) or not (.FALSE.)
logical        :: usempi = .false. ! flag to denote run as using MPI (.TRUE.) or not (.FALSE.)

end module dfparall
