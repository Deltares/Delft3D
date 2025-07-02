module grid_dimens_module
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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
!!--module description----------------------------------------------------------
!
! This module defines the data structure for grid dimensions.
!
!!--module declarations---------------------------------------------------------
use precision
private

!
! public data types
!
public griddimtype

!
! public routines
!
public simplegrid_dimens

integer, parameter, public :: MESH_STRUCTURED = 0   ! structured (n,m) mesh
integer, parameter, public :: MESH_UNSTRUCTURED = 1 ! unstructured using single index n

integer, parameter, public :: PARTITION_CONT = 0    ! simple box/range (n,m)
integer, parameter, public :: PARTITION_NONCONT = 1 ! non-contiguous partitioning index (for unstructured grid only)

!
! collection of grid dimension properties
!
type griddimtype
    !
    ! Mesh type
    !
    integer :: meshtype = MESH_STRUCTURED
    !
    ! General grid dimensions defined for structured (n,m) meshes:
    ! ------------------------------------------------------------
    !
    !   Local (n,m) indices:                        Local linear nm indices:
    !    ______________________________________      ______________________________________ 
    ! ^ |                             (nub,mub)|    |                                  nmub|
    ! | |______________________________________|    |______________________________________|
    ! M |                                      |    |                                 nmmax|
    !   |     __________________________ _     |    |     __________________________ _     |
    !   |    |/////////////////(nmax,mmax)|    |    |    |//////////////////////////| |    |
    !   |    |//////////////////////////| |    |    |    |//////////////////////////| |    |
    !   |    |//////////////////////////| |    |    |    |//////////////////////////| |    |
    !   |    |//////////////////////////| |    |    |    |//////////////////////////| |    |
    !   |    |//////////////////////////| |    |    |    |//////////////////////////| |    |
    !   |    |__________________________|_|    |    |    |__________________________|_|    |
    !   |    (1,1)                             |    |                                      |
    !   |______________________________________|    |______________________________________|
    !   |                                      |    |1                                     |
    !   |______________________________________|    |______________________________________|
    !   (nlb,mlb)                          N->       nmlb
    !
    !   Global (n,m) indices:                 
    !    ______________________________________________ 
    !   |                               (nmaxgl,mmaxgl)|
    !   |                                              |
    !   |                                              |
    !   |          __________________________          |
    !   |         |/////////////////(nlg,mlg)|         |
    !   |         |//////////////////////////|         |
    !   |         |//////////////////////////|         |
    !   |         |//////////////////////////|         |
    !   |         |//////////////////////////|         |
    !   |         |__________________________|         |
    !   |         (nfg,mfg)                            |
    !   |                                              |
    !   |                                              |
    !   |______________________________________________|
    !   (1,1)
    !
    ! Delft3D 4 specific grid dimensions:
    ! -----------------------------------
    !
    !   Local (n,m) indices Delft3D-FLOW:           Local linear nm indices Delft3D-FLOW:
    !    ______________________________________      ______________________________________ 
    !   |                 (nmax+ddb,mmax+2+ddb)|    |           (nmax+2*ddb)*(mmax+2+2*ddb)|
    !   |______________________________________|    |______________________________________|
    !   |                   (nmax+ddb,mmax+ddb)|    |             (nmax+2*ddb)*(mmax+2*ddb)|
    !   |     __________________________ _     |    |     __________________________ _     |
    !   |    |/////////////(nmaxus,mmax)|*|    |    |    |//////////////////////////| |    |
    !   |    |//////////////////////////| |    |    |    |//////////////////////////| |    |
    !   |    |//////////////////////////| |    |    |    |//////////////////////////| |    |
    !   |    |//////////////////////////| |    |    |    |//////////////////////////| |    |
    !   |    |//////////////////////////| |    |    |    |//////////////////////////| |    |
    !   |    |__________________________|_|    |    |    |__________________________|_|    |
    !   |    (1,1)                             |    |                                      |
    !   |______________________________________|    |______________________________________|
    !   |(1-ddb,1-ddb)                         |    |1                                     |
    !   |______________________________________|    |______________________________________|
    !   (1-ddb,-1-ddb)           * = (nmax,mmax)    1-2*(nmax+2*ddb)
    !
    !
    ! The hashed area is the area of the grid assigned to the current thread.
    ! In a sequential simulation this corresponds to precisely the grid as
    ! defined by the user; in a parallel simulation it is a part thereof.
    ! The size of this grid area is nmaxus x mmax, where nmaxus and mmax are
    ! either the grid dimensions known to the user (sequential case) or those
    ! corresponding to the partition given to this thread by the master
    ! including any halo cells (parallel run). All hashed areas in local and
    ! global index spaces sketched above correspond.
    !
    ! The local grid dimensions may be bigger than the hashed area e.g. due to
    ! halo cells for domain decomposition (DD boundaries) or for numerical
    ! reasons e.g. the red-black Jacobi implementation requires nmax to be odd.
    !
    ! General grid dimensions defined for unstructured mesh using only index n:
    ! -------------------------------------------------------------------------
    !
    !   Local n index:
    !   |    |////////////////////////////|    | nub
    !   |____1_________________________nmax____|
    !   nlb                                 N->
    !
    !   Contiguous global n indices:
    !   |         |//////////////////////////|         | nmaxgl
    !   |_________nfg______________________nlg_________|
    !   1
    !
    ! Local n range
    !
    integer :: nlb    ! lower bound on n index in local array dimension
    integer :: nub    ! upper bound on n index in local array dimension
    integer :: nmax   ! active local n index range runs from 1 to nmaxus (nmax=nmaxus+1 if nmax is even)
    !
    ! local m range
    !
    integer :: mlb    ! lower bound on m index in local array dimension
    integer :: mub    ! upper bound on m index in local array dimension
    integer :: mmax   ! active local m index range runs from 1 to mmax
    !
    ! Local nm linear index range
    !
    integer :: nmlb   ! lower bound on local linear nm index
    integer :: nmub   ! upper bound on local linear nm index
    integer :: nmmax  ! active local nm indices are subset of 1 to nmmax
    !
    ! Partitioning information
    !
    integer :: parttype = PARTITION_CONT
    !
    ! Global n range
    !
    integer :: nfg    ! global n index corresponding to local n index 1
    integer :: nlg    ! global n index corresponding to local n index nmaxus
    integer :: nmaxgl ! global maximum n index as known to user
    !
    ! Global m range
    !
    integer :: mfg    ! global m index corresponding to local m index 1
    integer :: mlg    ! global m index corresponding to local m index mmax
    integer :: mmaxgl ! global maximum m index as known to user
    !
    integer , dimension(:)  , pointer :: nmglobal   => null() ! (nmlb:nmub) global linear index
    !
    integer , dimension(:,:), pointer :: aggrtable  => null() ! (nlb:nub,mlb:mub) aggrtable(i,j) = 0 no cell, nm = cell index (>0)
    integer , dimension(:)  , pointer :: celltype   => null() ! (nlb:nub,mlb:mub) 0 = inactive, 1 = active (internal), 2 = boundary, -1 = ghost
    integer , dimension(:,:), pointer :: nmbnd      => null() ! (2,nb) --> (1,:) nm boundary, (2,:) = nm internal
    integer , dimension(:)  , pointer :: cell2node  => null() ! (totnodes) node numbers associated with each cell
    integer , dimension(:)  , pointer :: ncellnodes => null() ! (nmlb:nmub) number of nodes to make up the contour of each cell
    integer , dimension(:)  , pointer :: indexnode1 => null() ! (nmlb:nmub) index in cell2node to lists the first node of the cell contour
    !
    real(fp), dimension(:)  , pointer :: xz         => null() ! (nmlb:nmub) X-coordinate of the cell
    real(fp), dimension(:)  , pointer :: yz         => null() ! (nmlb:nmub) Y-coordinate of the cell
    real(fp), dimension(:)  , pointer :: xnode      => null() ! X-coordinate of the mesh node
    real(fp), dimension(:)  , pointer :: ynode      => null() ! Y-coordinate of the mesh node
end type griddimtype

contains

subroutine simplegrid_dimens(griddim,nmax,mmax,aggrtable)
!!--description-----------------------------------------------------------------
!
! Initializes a griddim structure for a simple (1,nmax) and (1,mmax) grid
!
!!--declarations----------------------------------------------------------------
    implicit none
!
! Arguments
!
    type (griddimtype)        :: griddim
    integer                   :: nmax
    integer                   :: mmax
    integer, dimension(:,:), pointer, optional :: aggrtable
!
! Local variables
!
    integer                   :: istat
    integer                   :: nm
    integer                   :: nmmax
!
!! executable statements -------------------------------------------------------
!
    griddim%nlb    = 1
    griddim%nub    = nmax
    griddim%nmax   = nmax
    !
    griddim%mlb    = 1
    griddim%mub    = mmax
    griddim%mmax   = mmax
    !
    nmmax = nmax*mmax
    griddim%nmlb   = 1
    griddim%nmub   = nmmax
    griddim%nmmax  = nmmax
    !
    griddim%nfg    = 1
    griddim%nlg    = nmax
    griddim%nmaxgl = nmax
    !
    griddim%mfg    = 1
    griddim%mlg    = mmax
    griddim%mmaxgl = mmax
    !
    if (present(aggrtable)) then
       griddim%aggrtable => aggrtable
    else
       griddim%aggrtable => null()
    endif
    !
    allocate(griddim%nmglobal(nmmax), stat=istat)
    if (istat==0) then
       do nm = 1, nmmax
           griddim%nmglobal(nm) = nm
       enddo
    endif
    allocate(griddim%celltype(nmmax), stat=istat)
    if (istat==0) griddim%celltype = 1
    !
    griddim%nmbnd => null()
    !
    griddim%cell2node  => null()
    griddim%ncellnodes => null()
    griddim%indexnode1 => null()
    !
    griddim%xz => null()
    griddim%yz => null()
    griddim%xnode => null()
    griddim%ynode => null()
end subroutine simplegrid_dimens

end module grid_dimens_module
