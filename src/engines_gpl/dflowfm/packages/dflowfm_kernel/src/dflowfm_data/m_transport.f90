!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

!> transport of many (scalar) constituents is performed with the transport module
!!   -constituents are stored in the constituents array
!!   -salt and temperature are filled from and copied to the sa1 and tem1 arrays, respectively
!!   -salt and temperature boundary and initial conditions are applied to sa1 and tem1, not to the constituents directly
!! tracers:
!!   -tracers intial and boundary conditions are directly applied to the constituents
!!   -the tracers always appear at the end of the whole constituents array
!!   -the constituents numbers of the tracers are from "ITRA1" to "ITRAN", where ITRAN=0 (no tracers) or ITRAN=NUMCONST (tracers come last)
!!   -tracers with boundary conditions (not necessarily all tracers) have their own numbering
!!   -the tracer (with bc's) to consituent mapping is called "itrac2const"
!!   -boundary condition related information of the tracers are stored in "bndtr" of type "bndtype"

module m_transportdata
   integer, parameter                            :: NAMLEN = 128
   integer                                       :: NUMCONST       ! Total number of constituents
   integer                                       :: NUMCONST_MDU   ! number of constituents as specified in mdu/ext file
   integer, target                               :: ISALT  ! salt
   integer, target                               :: ITEMP  ! temperature
   integer                                       :: ISED1  ! first sediment fraction
   integer                                       :: ISEDN  ! last  sediment fraction
   integer                                       :: ISPIR  ! secondary flow intensity
   integer                                       :: ITRA1  ! first tracer
   integer                                       :: ITRAN  ! last  tracer, should be at the back
   integer                                       :: ITRAN0         ! back up of ITRAN

!  tracers
   integer,          dimension(:),   allocatable :: itrac2const   ! constituent number of tracers (boundary conditions only)
   integer,          dimension(:),   allocatable :: ifrac2const   ! constituent number of sediment fractions
   double precision, dimension(:,:), allocatable, target :: constituents    ! constituents, dim(NUMCONST,Ndkx)

   character(len=NAMLEN), dimension(:), allocatable, target :: const_names    ! constituent names
   character(len=NAMLEN), dimension(:), allocatable :: const_units    ! constituent units
   character(len=NAMLEN), parameter                 :: DEFTRACER = 'default_tracer'

   integer,          dimension(:,:), allocatable    :: id_const   ! consituent id's in map-file
   integer                                          :: iconst_cur ! active constituent (for visualization)
end module m_transportdata

module m_transport
 
   use m_transportdata                                       !separation to get rid of all those use only: checks

   double precision, dimension(:,:), allocatable :: fluxhor  ! horizontal fluxes
   double precision, dimension(:,:), allocatable :: fluxver  ! vertical   fluxes
   double precision, dimension(:,:), allocatable :: fluxhortot ! sum of horizontal fluxes (fluxhor) at local time stepping
   double precision, dimension(:,:), allocatable :: sinksetot    ! sum of sed sinks at local time stepping
   double precision, dimension(:,:), allocatable :: sinkftot    ! sum of  fluff sinks at local time stepping

   double precision, dimension(:),   allocatable :: thetavert  ! vertical advection fluxes explicit (0) or implicit (1)

   double precision, dimension(:),   allocatable :: difsedu  ! sum of molecular and user-specified diffusion coefficient
   double precision, dimension(:),   allocatable :: difsedw  ! sum of molecular and user-specified diffusion coefficient

   double precision, allocatable                 :: dsedx   (:,:) !< cell center constituent gradient
   double precision, allocatable                 :: dsedy   (:,:) !< cell center constituent gradient

   double precision, dimension(:,:), allocatable :: const_sour  ! sources in transport, dim(NUMCONST,Ndkx)
   double precision, dimension(:,:), allocatable :: const_sink  ! linear term of sinks in transport, dim(NUMCONST,Ndkx)

!  work arrays
   double precision, dimension(:,:), allocatable :: rhs      ! right-hand side, dim(NUMCONST,Ndkx)
   double precision, dimension(:,:), allocatable :: a,b,c,d  ! aj(i,j)*sed(j,k-1) + bj(i,j)*sed(j,k) + c(i,j)*sed(j,k+1) = d(i), i=k-kb+1
   double precision, dimension(:),   allocatable :: sol, e   ! solution and dummy array in tridag, respectively

!  for local timestepping
   double precision, dimension(:,:), allocatable :: sumhorflux    !< sum of horizontal fluxes, dim(NUMCONST,Ndkx)
   integer                                       :: nsubsteps     !< total number of substeps
   integer,          dimension(:),   allocatable :: ndeltasteps   !< cell-based number of subtimesteps between updates, dim(Ndx)
   integer,          dimension(:),   allocatable :: jaupdate      !< update cell (1) or not (0), dim(Ndx)
   integer,          dimension(:),   allocatable :: jaupdatehorflux  !< update horizontal flux (1) or not (0), dim(Lnx)
   double precision, dimension(:),   allocatable :: dtmax         !< maximum local time-step (for water columns)
   double precision                              :: dtmin_transp  !< limiting time-step
   integer                                       :: kk_dtmin      !< flownode of limiting time-step
   double precision                              :: time_dtmax    !< time for which maximum local time-step is evaluated
   integer                                       :: numnonglobal  !< number of cells not at the global time step
   double precision, dimension(:),   allocatable :: sumdifflim    !< contribution of diffusion to transport time-step limitation
   double precision, dimension(:),   allocatable :: dxiAu         !< area of horizontal diffusive flux divided by Dx

!  for sediment advection velocity
   integer,          dimension(:),   allocatable :: jaupdateconst !< update constituent (1) or not (0)
   integer,          dimension(:),   allocatable :: noupdateconst !< do not update constituent (1) or do (0)

!  time step related
   integer :: jalimitdiff
   integer :: jalimitdtdiff

   double precision :: dsum
   double precision :: maserrsed !< cumulative sediment mass error because of volume truncation in shallow areas


   double precision, dimension(:),   allocatable :: u1sed
   double precision, dimension(:),   allocatable :: q1sed
 end module m_transport
