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

module m_partmesh
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
end module m_partmesh
