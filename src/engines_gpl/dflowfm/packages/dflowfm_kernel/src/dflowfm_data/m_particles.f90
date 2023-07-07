!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
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

module m_particles
   integer                                        :: japart       !< particles (1) or not (0)

   integer                                        :: Npart        !< number of particles
   double precision,  dimension(:),   allocatable :: xpart, ypart !< coordinates of particles, dim(Npart)
   double precision,  dimension(:),   allocatable :: zpart        !< z-coordinates of particles, dim(Npart), for spherical models
   double precision,  dimension(:),   allocatable :: dtremaining  !< remaining time, dim(Npart)
   integer,           dimension(:),   allocatable :: kpart        !< cell (flownode) number, dim(Npart)
!   integer,           dimension(:),   allocatable :: Lpart        !< edge (netlink)  number, dim(Npart)
!   character(len=40), dimension(:),   allocatable :: namepart     !< name of particle, dim(Npart)
   integer,           dimension(:),   allocatable :: iglob        !< global number, dim(Npart)
   integer                                        :: Nglob        !< maximum global number
   integer                                        :: NpartOut     !< number of particle tracks in output

   integer                                        :: Nrpart       !< number of particles to be released
   integer                                        :: irpart       !< current number of particle to be released
   double precision,  dimension(:),   allocatable :: trpart       !< timing of to be released particles, dim(Npart)
   double precision,  dimension(:),   allocatable :: xrpart       !< x-coordinates of to be released particles, dim(Npart)
   double precision,  dimension(:),   allocatable :: yrpart       !< y-coordinates of to be released particles, dim(Npart)
   double precision,  dimension(:),   allocatable :: zrpart       !< z-coordinates of to be released particles, dim(Npart), for spherical models

   integer,           dimension(:),   allocatable :: numzero      !< number of consecutive (sub)times a particle was not displaces within a time-step

   integer                                        :: jatracer     !< add tracer with particle concentration (1) or not (0)
   double precision                               :: starttime    !< start time
   double precision                               :: timestep     !< time step (>0) or every computational timestep
   double precision                               :: timelast     !< last time of particle update
   double precision                               :: timenext     !< next time of particle update
   double precision                               :: timepart     !< time of particles
   character(len=22), parameter                   :: PART_TRACERNAME = 'particle_concentration' !< particle tracer name
   integer                                        :: part_iconst  !< particle tracer constituent number
   integer                                        :: threeDtype       !< depth averaged or 2D (0), free surface (1)

   double precision, dimension(:),    allocatable :: sbegin      !< water level at begin of time interval, dim(Ndx)
   double precision, dimension(:),    allocatable :: qpart       !< cummulative fluxes from begin of time interval, dim(Lnx)

   double precision, dimension(:),    allocatable :: qfreesurf       !< free surface flux (for threeDtype=1)

!   integer :: mfile
end module m_particles
