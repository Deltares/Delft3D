!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.!
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

module m_part_parameters
   use m_waq_precision

   implicit none
   ! parameters controlling flooding/drying/solving
   real(kind=dp) :: epshs = 0.2d-4            !< minimum waterdepth for setting cfu

end module m_part_parameters

module m_part_flow

   implicit none

   ! 3D parameters
   integer                           :: kmx               !< nr of 3d layers, increasing in positive upward direction
   !! if kmx==0 then 2D code. if kmx==1 then 3D code
   integer                           :: ndkx              !< dim of 3d flow nodes (internal + boundary)
   integer                           :: lnkx              !< dim of 3d flow links (internal + boundary)

   integer                           :: layertype         !< 1= all sigma, 2 = all z, 3 = left sigma, 4 = left z

   integer, parameter                :: LAYTP_SIGMA     = 1
   integer, parameter                :: LAYTP_Z         = 2
   integer, parameter                :: LAYTP_LEFTSIGMA = 3
   integer, parameter                :: LAYTP_LEFTZ     = 4

   ! flow arrays

   ! node related, dim = ndx
   double precision, allocatable :: h0(:)       !< [m] layer thickness at start of timestep {"location": "face", "shape": ["ndx"]}
   double precision, allocatable :: h1(:)       !< [m] layer thickness at end   of timestep {"location": "face", "shape": ["ndx"]}
   double precision, allocatable :: s0(:)       !< [m] waterlevel    at start of timestep {"location": "face", "shape": ["ndx"]}
   double precision, allocatable :: s1(:)       !< [m] waterlevel    at end   of timestep {"location": "face", "shape": ["ndx"]}
   double precision, allocatable :: vol0(:)     !< [m3] total volume at start of timestep {"location": "face", "shape": ["ndx"]}
   double precision, allocatable :: vol1(:)     !< [m3] total volume at end of timestep {"location": "face", "shape": ["ndx"]}

   double precision, allocatable     :: qw    (:)   !< vertical flux through interface (m3/s)

   double precision, allocatable     :: q0(:)   !< [m3/s] discharge     (m3/s) for current time step (makes vol0 -> vol1)
   double precision, allocatable     :: q1(:)   !< [m3/s] discharge     (m3/s) for next time time step

end module m_part_flow

module m_part_geom

   implicit none

   integer                           :: ndx            !< [-] Number of flow nodes (internal + boundary). {"rank": 0}
   integer                           :: ndxi           !< [-] Number of internal flowcells  (internal = 2D + 1D ). {"rank": 0}
   double precision, allocatable     :: ba(:)          !< [m2] bottom area, if < 0 use table in node type {"location": "face", "shape": ["ndx*kmx"]}
   double precision, allocatable     :: bl(:)          !< [m] bottom level (m) (positive upward) {"location": "face", "shape": ["ndx*kmx"]}
   integer                           :: lnxi           !< [-] nr of flow links (internal, 1D+2D    ). {"rank": 0}
   integer                           :: lnx            !< [-] nr of flow links (internal + boundary). First we have 1D links, next 2D links, next boundary links (first 1D, then 2D). {"rank": 0}
   integer,          allocatable     :: ln    (:,:)    !< [-] 1D link (2,*) node   administration, 1=nd1,  2=nd2   linker en rechter celnr {"shape": [2, "lnkx"]}
   double precision, allocatable     :: wu(:)          !< [m] link initial width (m), if < 0 pointer to convtab {"location": "edge", "shape": ["lnx"]}
   integer,          allocatable     :: lne2ln(:)      !< netlink to flowlink nr dim = numL

end module m_part_geom


!> this module contains the real flow times, only to be managed by setting times in module m_usertimes
module m_part_times
   implicit none

   character (len=8)                 :: refdat      !< Reference date (e.g., '20090101'). All times (tstart_user, tend_user, etc.) are w.r.t. to this date.
   double precision                  :: Tzone       !< Data Sources in GMT are interrogated with time in minutes since refdat-Tzone*60
   character(len=42)                 :: Tudunitstr  !< Complete UDunitstring for the time variable written as a unit attribute into various NetCDF output files

   double precision                  :: tstart_user !< User specified time start (s) w.r.t. refdat
   double precision                  :: tstop_user  !< User specified time stop  (s) w.r.t. refdat

   double precision                  :: dts         !< internal computational timestep (s)
   double precision                  :: time0       !< current   julian (s) of s0
   double precision                  :: time1       !< current   julian (s) of s1  ! and of course, time1 = time0 + dt
end module m_part_times

module m_part_transport
   integer, parameter :: NAMLEN = 128
   integer                                          :: NUMCONST       ! Total number of constituents
   double precision, dimension(:,:,:), allocatable  :: constituents    ! constituents, dim(NUMCONST,Ndkx,kmx)

   character(len=NAMLEN), dimension(:), allocatable :: const_names    ! constituent names
   character(len=NAMLEN), dimension(:), allocatable :: const_units    ! constituent unitsmodule m_particles
end module m_part_transport

module m_particles
   integer                                        :: japart       !< particles (1) or not (0)

   !! AM integer                                        :: Nopart       !< number of particles - defined twice
   integer                                        :: NopartTot    !< number of particle tracks in output
   double precision,  dimension(:),   allocatable :: xpart, ypart !< coordinates of particles, dim(Npart)
   double precision,  dimension(:),   allocatable :: xpart_prevt, ypart_prevt !< coordinates of particles, dim(Npart), previous timestep
   double precision,  dimension(:),   allocatable :: zpart        !< z-coordinates of particles, dim(Npart), for spherical models
   double precision,  dimension(:),   allocatable :: zpart_prevt  !< z-coordinates of particles, dim(Npart), for spherical models, previous timestep
   integer,           dimension(:),   allocatable :: kpart        !< layer containing the particles
   integer,           dimension(:),   allocatable :: kpart_prevt  !< layer containing the particles, previous time step
   double precision,  dimension(:),   allocatable :: hpart        !< position of the particles within the layer
   double precision,  dimension(:),   allocatable :: hpart_prevt  !< position of the particles within the layer, previous timestep


   double precision,  dimension(:),   allocatable :: dtremaining  !< remaining time, dim(Npart)
   !! AM integer,           dimension(:),   allocatable :: kpart        !< cell (flownode) number, dim(Npart)
   integer,           dimension(:),   allocatable :: mpart_prevt  !< cell (flownode) number, dim(Npart), previous timestep

   integer                                        :: Nrpart       !< number of particles to be released
   integer                                        :: irpart       !< current number of particle to be released
   double precision,  dimension(:),   allocatable :: trpart       !< timing of to be released particles, dim(Nrpart)
   double precision,  dimension(:),   allocatable :: xrpart       !< x-coordinates of to be released particles, dim(Nrpart)
   double precision,  dimension(:),   allocatable :: yrpart       !< y-coordinates of to be released particles, dim(Nrpart)
   double precision,  dimension(:),   allocatable :: zrpart       !< z-coordinates of to be released particles, dim(Nrpart), for spherical models
   integer,           dimension(:),   allocatable :: mrpart       !< cell (flownode) number of to be released particles, dim(Nrpart), for spherical models
   integer,           dimension(:),   allocatable :: krpart       !< layer of to be released particles, dim(Nrpart)
   double precision,  dimension(:),   allocatable :: hrpart       !< position within the layer of to be released particles, dim(Nrpart)

   integer,           dimension(:),   allocatable :: numzero      !< number of consecutive (sub)times a particle was not displaces within a time-step

   double precision                               :: starttime    !< start time
   double precision                               :: timestep     !< time step (>0) or every computational timestep
   double precision                               :: timelast     !< last time of particle update
   double precision                               :: timenext     !< next time of particle update
   double precision                               :: timepart     !< time of particles
   character(len=22), parameter                   :: PART_TRACERNAME = 'particle_concentration' !< particle tracer name
   integer                                        :: part_iconst  !< particle tracer constituent number
   integer                                        :: threeDtype       !< depth averaged or 2D (0), free surface (1)

   double precision, dimension(:),    allocatable :: hbegin      !< water level at begin of time interval, dim(Ndx)
   double precision, dimension(:),    allocatable :: qpart       !< cummulative fluxes from begin of time interval, dim(Lnx)

   double precision, dimension(:),    allocatable :: qfreesurf       !< free surface flux (for threeDtype=1)
end module m_particles

module m_part_recons
   integer                                        :: nqzero         ! number of zeros in qe (no flux)
   double precision,  dimension(:),   allocatable :: qe             !< fluxes at all edges, including internal, and q0 where there is no discharge
   integer,           dimension(:),   allocatable :: qbnd           !< edges where there is no discharge
   integer,           dimension(:),   allocatable :: cell_closed_edge ! cells that have a closed edge (may be temporary eg dry due to dry cells)
   double precision,  dimension(:),   allocatable :: u0x, u0y, alphafm !< reconstruction of velocity fiels in cells, dim(numcells)
   double precision,  dimension(:),   allocatable :: u0z          !< reconstruction of velocity field in cells, dim(numcells), for spherical models
   double precision,  dimension(:),   allocatable :: u0w          !< reconstruction of velocity field in cells, dim(numcells), for 3D models (represents vertical motion in water column)
   double precision                               :: xref, yref ! reference point around cartesian area if model is sferical

   integer,           dimension(:),   allocatable :: ireconst    !< sparse storage of velocity reconstructin, edges,        dim(jreconst(numcells+1)-1)
   integer,           dimension(:),   allocatable :: jreconst    !< sparse storage of velocity reconstructin, startpointer, dim(numcells+1)
   double precision,  dimension(:,:), allocatable :: Areconst    !< sparse storage of velocity reconstructin, [ucx, ucy, (ucz,) alpha] dim(jreconst(3 (4), numcells+1)-1)
end module m_part_recons

module m_part_fluxes
   integer,          dimension(:),    allocatable :: iflux2link   !< sparse storage of edge-based flux to flowlink-based flux (prescribed), flowlinks, dim(jflux2link(numedges+1)-1)
   integer,          dimension(:),    allocatable :: jflux2link   !< sparse storage of edge-based flux to flowlink-based flux (prescribed), startpointer, dim(numedges+1)
   double precision, dimension(:),    allocatable :: Aflux2link   !< sparse storage of edge-based flux to flowlink-based flux (prescribed), coefficients, dim(jflux2link(numedges+1)-1)
end module m_part_fluxes

module m_part_mesh
   !  mesh data
   integer                                         :: numnodes    !< number of nodes, >= numk
   integer                                         :: numedges    !< number of edges, >= numL
   integer                                         :: numcells    !< number of cells, >= nump
   integer                                         :: numorigedges  !< number of original "non-internal" edges (numL)

   integer,          dimension(:,:),   allocatable :: edge2node   !< edge-to-node, dim(2,numedges)
   integer,          dimension(:,:),   allocatable :: edge2cell   !< edge-to-cell, dim(2,numedges)

   double precision, dimension(:),     allocatable :: xnode       !< x-coordinate of nodes, dim(numnodes)
   double precision, dimension(:),     allocatable :: ynode       !< y-coordinate of nodes, dim(numnodes)
   double precision, dimension(:),     allocatable :: znode       !< z-coordinate of nodes, dim(numnodes), for spherical models

   double precision, dimension(:),     allocatable :: xzwcell     !< x-coordinate of cell c/g, dim(numcells)
   double precision, dimension(:),     allocatable :: yzwcell     !< y-coordinate of cell c/g, dim(numcells)
   double precision, dimension(:),     allocatable :: zzwcell     !< z-coordinate of cell c/g, dim(numcells), for spherical models
   double precision, dimension(:),     allocatable :: areacell    !< area of cell, dim(numcells)

   integer,          dimension(:),     allocatable :: icell2edge   !< sparse storage of cell-to-edge, data, dim(jcell2edge(numcells+1)-1)
   integer,          dimension(:),     allocatable :: jcell2edge   !< sparse storage of cell-to-edge, startpointer, dim(numcells+1)

   integer,          dimension(:),     allocatable :: edge2link    !< edge to "flowlink" (>0), no flowlink (0), or new inner link (<0), dim(numedges)
   !   integer,          dimension(:),     allocatable :: nod2cell     !< "flownode" to cell (>0), first new "inner" triangle (<0), dim(numcells), note: numcells can be too large for array dimension
   integer,          dimension(:),     allocatable :: cell2nod     !< cell to "flownode" (>0), new triangle (<0), dim(numcells), note: numcells can be too large for array dimension

   double precision, dimension(:,:),   allocatable :: dnn          ! cell normal vector, dim(3,numcells), for spherical models
   double precision, dimension(:,:),   allocatable :: dnx          !< x-coordinate of edge normal vector (positive outward for edge2cell(1,:), positive inward for edge2cell(2,:)), dim(1,numedges) for 2D Cartesion, dim(2,numedges) for spherical models
   double precision, dimension(:,:),   allocatable :: dny          !< y-coordinate of edge normal vector (positive outward for edge2cell(1,:), positive inward for edge2cell(2,:)), dim(1,numedges) for 2D Cartesion, dim(2,numedges) for spherical models
   double precision, dimension(:,:),   allocatable :: dnz          !< y-coordinate of edge normal vector (positive outward for edge2cell(1,:), positive inward for edge2cell(2,:)), dim(2,numedges), for spherical models
   double precision, dimension(:),     allocatable :: w            !< edge width, dim(numedges)

   integer,                            parameter :: MAXSUBCELLS=10
end module m_part_mesh

module m_partfm_trk_netcdf

   character(len=255)   :: trkncfilename
   integer              :: itrkfile
   integer              :: id_trk_timedim
   integer              :: id_trk_time
   integer              :: id_trk_partdim
   integer              :: id_trk_parttime
   integer              :: id_trk_partx
   integer              :: id_trk_party
   integer              :: id_trk_partz
   integer              :: it_trk

end module m_partfm_trk_netcdf

module m_partfm_map_netcdf

   use m_ug_mesh
   use m_ug_network

   character(len=255)   :: mapncfilename
   integer              :: imapfile
   integer              :: id_map_timedim
   integer              :: id_map_time
   integer              :: id_map_layersdim
   integer              :: it_map

   integer              :: id_map_particle_concentration
   integer, dimension(:), allocatable      :: id_map_depth_averaged_particle_concentration

   type(t_ug_mesh)      :: meshids  !< Set of NetCDF-ids for all mesh geometry variables.
   type(t_ug_network)   :: networkids !< Set of NetCDF-ids for all network variables

   double precision, allocatable, target :: work(:)   !< Work array

end module m_partfm_map_netcdf
