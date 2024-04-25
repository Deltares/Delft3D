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
! This module prepares the structured grid into an unstructured data structure
!   and writes it to a UGRID compliant NetCDF file    
!!--pseudo code and references--------------------------------------------------
! Dependencies:
!   io_netcdf module to write to UGRID compliant NetCDF file    
!!--declarations----------------------------------------------------------------
module m_write_waqgeom_curvilinear
    use precision
    implicit none
    integer, parameter   :: missing_value = -999
    type :: edge_t
        integer               :: type    = 0             !< Can be UG_EDGETYPE_INTERNAL_CLOSED, UG_EDGETYPE_INTERNAL, UG_EDGETYPE_BND or UG_EDGETYPE_BND_CLOSED
        integer               :: nmelm   = 0             !< Count of neighbouring elements (1 or 2)
        real(hp)              :: edgex                   !< Mass centre of edge (x)
        real(hp)              :: edgey                   !< Mass centre of edge (y)
        integer, dimension(2) :: vertex  = missing_value !< an edge is a straight line between two nodes
        integer, dimension(2) :: element = missing_value !< an edge separates one or two elements
    end type
contains    
    subroutine wrwaqgeomcl ( meta     , lundia, nmax   , mmax   , kmax   , & 
                             nlb      , nub   , mlb    , mub    ,          &
                             xcor     , ycor  , xz     , yz     , dep    , &
                             kcs      , kcu   , kcv    , sferic , aggre  , &
                             isaggrl  , nto   , nambnd , mnbnd)

    use netcdf
    use io_ugrid
    use m_aggregate_waqgeom
!      
    implicit none
!
!   Global variables
!
    type(t_ug_meta)                            , intent(in) :: meta       !! metadata for grid file
    integer                                    , intent(in) :: lundia     !! logical unit of diagnostic file
    integer(4)                                 , intent(in) :: nmax       !! Dimension of first index in 2d arrays
    integer(4)                                 , intent(in) :: mmax       !! Dimension of second index in 2d arrays
    integer(4)                                 , intent(in) :: kmax       !! number of flow layers
    integer(4)                                 , intent(in) :: nlb        !! Lower bound of all n dimensions
    integer(4)                                 , intent(in) :: nub        !! Upper bound of all n dimensions
    integer(4)                                 , intent(in) :: mlb        !! Lower bound of all m dimensions
    integer(4)                                 , intent(in) :: mub        !! Upper bound of all m dimensions
    integer                                    , intent(in) :: nto        !! Number of open boundaries (tidal openings)
    real(fp)       , dimension(nlb:nub,mlb:mub), intent(in) :: xcor       !! Array with x-values corners
    real(fp)       , dimension(nlb:nub,mlb:mub), intent(in) :: ycor       !! Array with y-values corners
    real(fp)       , dimension(nlb:nub,mlb:mub), intent(in) :: xz         !! Array with x-values zeta point
    real(fp)       , dimension(nlb:nub,mlb:mub), intent(in) :: yz         !! Array with y-values zeta point
    real(fp)       , dimension(nlb:nub,mlb:mub), intent(in) :: dep        !! Array with depth-values at corners
    integer        , dimension(nlb:nub,mlb:mub), intent(in) :: kcs        !! Cell type (0=inactive, 1=internal, 2=boundary)
    integer        , dimension(nlb:nub,mlb:mub), intent(in) :: kcu        !! u-flowlink type (0=closed, 1=open)
    integer        , dimension(nlb:nub,mlb:mub), intent(in) :: kcv        !! v-flowlink type (0=closed, 1=open)
    logical                                    , intent(in) :: sferic     !! sferic grid
    integer                                    , intent(in) :: aggre      !! aggregation type (0=no-aggregation, active cells only, 1=aggregation table)
    integer        , dimension(nmax*mmax)      , intent(in) :: isaggrl    !! grid aggregation pointer (only top/bottom layer, depending on zmodel)
    character(20)  , dimension(nto)            , intent(in) :: nambnd     !! names of the open boundaries
    integer        , dimension(7,nto)          , intent(in) :: mnbnd      !! indices of the open boundaries
!
!           Local variables
!
    type(t_crs), target                 :: crs
    type(t_ug_mesh)                     :: meshids               !< Set of NetCDF-ids for all mesh geometry arrays.
    type(t_ug_network)                  :: networkids            !< Set of NetCDF-ids for all network arrays.
    type(t_ug_meshgeom)                 :: meshgeom              !< Mesh geometry to be written to the NetCDF file.
    type(t_ug_meshgeom)                 :: aggregated_meshgeom   !< Mesh geometry to be written to the NetCDF file.
                                        
    integer                             :: i, j, i1, j1, k, m, n !  loop counters
    integer                             :: md, nd, mu, nu        ! help variables
    integer                             :: m_dir, n_dir          ! help variables
    integer                             :: cellindex, pointindex ! help variables
    integer                             :: elm, elm1, elm2       ! help variables
    integer                             :: elm1_vol, elm2_vol    ! help variables

    integer                             :: nr_elems              ! number of elements
    integer                             :: nr_elems_aggr         ! number of elements after aggregation
    integer                             :: nr_nodes              ! number of nodes
    integer                             :: nr_edges              ! number of edges
    integer                             :: nr_flowlinks          ! number of open flow links
    real(hp), dimension(:)    , pointer :: nodex, nodey, nodez   ! coordinates of nodes
    real(hp), dimension(:)    , pointer :: edgex, edgey          ! coordinates of the middle of the edge (ie u-punt)
    real(hp), dimension(:)    , pointer :: facex, facey          ! coordinates of the mass centre of the elements
    integer , dimension(:)    , pointer :: node_mask             ! node pointer
    integer , dimension(:)    , pointer :: flow_vol              ! no-aggregation segment pointer
    type(edge_t), dimension(:), pointer :: edge                  ! collection of edges
    integer , dimension(:,:)  , pointer :: edge_nodes            ! nodes that define an edge
    integer , dimension(:,:)  , pointer :: face_nodes            ! nodes that define a face
    integer , dimension(:,:)  , pointer :: edge_faces            ! faces that this edge is part of
    integer , dimension(:)    , pointer :: edge_type             ! edge type variable to be written to the NetCDF file.
    integer , dimension(:)    , pointer :: aggr_edge_type(:)     ! aggregated edge type variable to be written to the NetCDF file.

    integer , dimension(:,:)  , pointer :: bnd_nr                ! boundary number
    integer                             :: nr_bnd_elm
    integer                             :: total_bnd_cells
    integer                             :: max_bnd_cells
    logical                             :: found
    real(hp), dimension(:,:,:), pointer :: open_bnd              ! open boundary coordinates
    integer , dimension(:)    , pointer :: nr_bnd_cells          
    integer                             :: lunbnd                ! logical unit boundary file
    character(len=256)                  :: bndfilename           ! boundary filename

    integer , dimension(:)    , pointer :: iapnt                 ! no-aggregation segment to aggregation segment pointer

    character(len=256)                  :: geomfilename          ! geomfilename
    integer                             :: igeomfile             ! logical unit geomfile
    integer                             :: ierr                  ! errorcode
    logical                             :: success               ! success (true/false)
!
!! executable statements -------------------------------------------------------
!
    ierr = t_ug_meshgeom_destructor(meshgeom)
    ierr = t_ug_meshgeom_destructor(aggregated_meshgeom)
    !
    ! Determine no-agregation segment pointer
    !
    allocate(node_mask((nmax-1)*(mmax-1)))
    allocate(flow_vol(mmax*nmax))
    flow_vol = 0 
    nr_elems = 0
    do m = 1, mmax
        do n = 1, nmax
            cellindex = func(m, n, nmax)
            if (kcs(n, m) == 1) then
                ! Valid cell found
                nr_elems = nr_elems + 1
                flow_vol(cellindex) = nr_elems
            else if (kcs(n, m) == 2) then
                flow_vol(cellindex) = isaggrl(cellindex)
            end if
        end do
    end do        
    !
    ! Determine no-agregation active nodes
    !
    nr_nodes = 0
    node_mask = 0
    do m = 1, mmax-1
        do n = 1, nmax-1
            if (kcs(n,m  )==1 .or. kcs(n+1,m  )==1 .or. &
                kcs(n,m+1)==1 .or. kcs(n+1,m+1)==1) then
                ! Valid point found
                nr_nodes = nr_nodes + 1
                pointindex = func(m, n, nmax-1)
                node_mask(pointindex) = nr_nodes
            end if
        end do
    end do    
    !
    ! Determine no-agregation polygons of each flow volume
    !
    allocate(face_nodes(4, nr_elems))
    face_nodes = 0
    do m = 2, mmax-1 ! outer columns have kcs==0
        do n = 2, nmax-1 ! outer rows have kcs==0
            cellindex = func(m, n, nmax)
            elm = flow_vol(cellindex)
            if (elm .gt. 0) then
                ! Active internal cell found
                face_nodes(1, elm) = node_mask(func(m-1, n-1, nmax-1))
                face_nodes(2, elm) = node_mask(func(m  , n-1, nmax-1))
                face_nodes(3, elm) = node_mask(func(m  , n  , nmax-1))
                face_nodes(4, elm) = node_mask(func(m-1, n  , nmax-1))
            end if
        end do
    end do        
    !
    ! Gather node coordinates
    !
    allocate(nodex(nr_nodes))
    allocate(nodey(nr_nodes))
    allocate(nodez(nr_nodes))
    nr_nodes = 0
    do m = 1, mmax-1
        do n = 1, nmax-1
            if (kcs(n,m  )==1 .or. kcs(n+1,m  )==1 .or. &
                kcs(n,m+1)==1 .or. kcs(n+1,m+1)==1) then
                nr_nodes = nr_nodes + 1
                nodex(nr_nodes) = xcor(n,m)
                nodey(nr_nodes) = ycor(n,m)
                nodez(nr_nodes) =-dep (n,m)
            end if
        end do
    end do    
    !
    ! Find all edges
    !
    nr_edges = 0
    allocate(edge(2*mmax*nmax))
    nr_flowlinks = 0
    do m = 1, mmax-1
        do n = 1, nmax-1
            ! u-flowlink
            elm1 = func(m  ,n  , nmax)
            elm2 = func(m+1,n  , nmax)
            elm1_vol = flow_vol(elm1)
            elm2_vol = flow_vol(elm2)
            if (elm1_vol /= elm2_vol .and. (elm1_vol.gt.0.or.elm2_vol.gt.0)) then
                nr_edges = nr_edges + 1
                edge(nr_edges)%edgex = 0.5*(xcor(n-1,m  ) + xcor(n  ,m  ))
                edge(nr_edges)%edgey = 0.5*(ycor(n-1,m  ) + ycor(n  ,m  ))
                edge(nr_edges)%vertex(1) = node_mask(func(m  , n-1, nmax-1))
                edge(nr_edges)%vertex(2) = node_mask(func(m  , n  , nmax-1))
                if (elm1_vol .gt. 0 .and. elm2_vol .gt. 0) then
                   edge(nr_edges)%nmelm = 2
                   edge(nr_edges)%element(1) = elm1_vol
                   edge(nr_edges)%element(2) = elm2_vol
                   if (kcu(n,m) == 1 ) then
                      edge(nr_edges)%type = UG_EDGETYPE_INTERNAL
                      nr_flowlinks = nr_flowlinks + 1
                   else   
                      edge(nr_edges)%type = UG_EDGETYPE_INTERNAL_CLOSED
                   endif
                else if(elm1_vol .lt. 0 .or. elm2_vol .lt. 0) then   
                   edge(nr_edges)%nmelm = 2
                   edge(nr_edges)%element(1) = elm1_vol
                   edge(nr_edges)%element(2) = elm2_vol
                   if (kcu(n,m) == 1 ) then
                      edge(nr_edges)%type = UG_EDGETYPE_BND
                      nr_flowlinks = nr_flowlinks + 1
                   else   
                      edge(nr_edges)%type = UG_EDGETYPE_BND_CLOSED
                   endif
                else if(elm2_vol .eq. 0) then   
                   edge(nr_edges)%nmelm = 1
                   edge(nr_edges)%element(1) = elm1_vol
                   edge(nr_edges)%type = UG_EDGETYPE_BND_CLOSED
                else if(elm1_vol .eq. 0) then   
                   edge(nr_edges)%nmelm = 1
                   edge(nr_edges)%element(1) = elm2_vol
                   edge(nr_edges)%type = UG_EDGETYPE_BND_CLOSED
                endif
            endif
            ! v-flowlink
            elm1 = func(m  ,n  , nmax)
            elm2 = func(m  ,n+1, nmax)
            elm1_vol = flow_vol(elm1)
            elm2_vol = flow_vol(elm2)
            if (elm1_vol /= elm2_vol .and. (elm1_vol.gt.0.or.elm2_vol.gt.0)) then
                nr_edges = nr_edges + 1 
                edge(nr_edges)%edgex = 0.5*(xcor(n  ,m-1) + xcor(n  ,m  ))
                edge(nr_edges)%edgey = 0.5*(ycor(n  ,m-1) + ycor(n  ,m  ))
                edge(nr_edges)%vertex(1) = node_mask(func(m-1, n  , nmax-1))
                edge(nr_edges)%vertex(2) = node_mask(func(m  , n  , nmax-1))
                if (elm1_vol .gt. 0 .and. elm2_vol .gt. 0) then
                   edge(nr_edges)%nmelm = 2
                   edge(nr_edges)%element(1) = elm1_vol
                   edge(nr_edges)%element(2) = elm2_vol
                   if (kcv(n,m) == 1 ) then
                      edge(nr_edges)%type = UG_EDGETYPE_INTERNAL
                      nr_flowlinks = nr_flowlinks + 1
                   else   
                      edge(nr_edges)%type = UG_EDGETYPE_INTERNAL_CLOSED
                   endif
                else if(elm1_vol .lt. 0 .or. elm2_vol .lt. 0) then   
                   edge(nr_edges)%nmelm = 2
                   edge(nr_edges)%element(1) = elm1_vol
                   edge(nr_edges)%element(2) = elm2_vol
                   if (kcv(n,m) == 1 ) then
                      edge(nr_edges)%type = UG_EDGETYPE_BND
                      nr_flowlinks = nr_flowlinks + 1
                   else   
                      edge(nr_edges)%type = UG_EDGETYPE_BND_CLOSED
                   endif
                else if(elm2_vol .eq. 0) then   
                   edge(nr_edges)%nmelm = 1
                   edge(nr_edges)%element(1) = elm1_vol
                   edge(nr_edges)%type = UG_EDGETYPE_BND_CLOSED
                else if(elm1_vol .eq. 0) then   
                   edge(nr_edges)%nmelm = 1
                   edge(nr_edges)%element(1) = elm2_vol
                   edge(nr_edges)%type = UG_EDGETYPE_BND_CLOSED
                endif
            endif
        end do
    end do  
    !
    ! Determine edge_nodes and edge_faces
    !
    allocate(edge_nodes(2, nr_edges))
    allocate(edge_faces(2, nr_edges))
    allocate(edgex(nr_edges))
    allocate(edgey(nr_edges))
    allocate(edge_type(nr_edges)) 
    edge_nodes = missing_value
    edge_faces = missing_value

    do i = 1, nr_edges
         edgex(i) = edge(i)%edgex
         edgey(i) = edge(i)%edgey
         edge_nodes(1,i) = edge(i)%vertex(1)
         edge_nodes(2,i) = edge(i)%vertex(2)
         do j =1,edge(i)%nmelm
            ! number boundary nodes just after the range of internal nodes
            if (edge(i)%element(j).lt.0) edge(i)%element(j) = missing_value ! abs(edge(i)%element(j)) + nr_elems
         end do
         edge_faces(1, i) = edge(i)%element(1)
         edge_faces(2, i) = edge(i)%element(2)
         edge_type(i) = edge(i)%type
    end do
    !
    ! Mass centre of elements
    !
    allocate(facex(nr_elems))
    allocate(facey(nr_elems))
    elm = 0
    do m = 2, mmax-1 ! outer columns have kcs==0
        do n = 2, nmax-1 ! outer rows have kcs==0
            if (kcs(n, m) == 1) then
                if (kcu(n  ,m-1)==0 .and. kcu(n  ,m  )==0 .and. &
                    kcv(n-1,m  )==0 .and. kcv(n  ,m  )==0) then ! do not count active cells defined with four thin dams
                else
                    ! Valid cell found
                    elm = elm + 1
                    facex(elm) = xz(n,m)
                    facey(elm) = yz(n,m)
                end if
            end if
        end do
    end do            
    !   
    ! Determine the boundaries
    ! 
    total_bnd_cells = 0
    max_bnd_cells = 0
    allocate(nr_bnd_cells(nto))
    do i = 1, nto
        m_dir = max(mnbnd(1,i),mnbnd(3,i)) - min(mnbnd(1,i),mnbnd(3,i)) + 1
        n_dir = max(mnbnd(2,i),mnbnd(4,i)) - min(mnbnd(2,i),mnbnd(4,i)) + 1
        nr_bnd_cells(i) = max(m_dir, n_dir)
        total_bnd_cells = total_bnd_cells + max(m_dir, n_dir)
        max_bnd_cells = max(max_bnd_cells, max(m_dir, n_dir))
    end do
    allocate(open_bnd(4,nto,max_bnd_cells))
    allocate(bnd_nr(max_bnd_cells, nto))
    nr_bnd_elm = 0
    bnd_nr = 0
    open_bnd = 0
    nr_bnd_cells = 0
    do m = 1,  mmax
        do n = 1, nmax
            if (kcs(n, m) == 2) then ! boundary element
                found = .false.
                nr_bnd_elm = nr_bnd_elm + 1
                cellindex = func(m, n, nmax)
                flow_vol(cellindex) = -nr_bnd_elm
                md = max(1   ,m-1)
                nd = max(1   ,n-1)
                mu = min(mmax,m+1)
                nu = min(nmax,n+1)
                if (kcs(nd,m) == 1) then
                   ! which boundary: upper boundary
          nto_loop1: do i = 1, nto
                        do i1 = min(mnbnd(1,i),mnbnd(3,i)), max(mnbnd(1,i),mnbnd(3,i))
                            do j1 = min(mnbnd(2,i),mnbnd(4,i)),  max(mnbnd(2,i),mnbnd(4,i))
                                if (m == i1 .and. n == j1) then
                                    found = .true.
                                    exit nto_loop1
                                end if
                            end do
                        end do
                    end do nto_loop1
                    if (found) then
                        nr_bnd_cells(i) = nr_bnd_cells(i)+1
                        bnd_nr(nr_bnd_cells(i),i) = flow_vol(cellindex)
                        open_bnd(1, i, nr_bnd_cells(i)) = xcor(n-1,m-1)
                        open_bnd(2, i, nr_bnd_cells(i)) = ycor(n-1,m-1)
                        open_bnd(3 ,i, nr_bnd_cells(i)) = xcor(n-1,m  )
                        open_bnd(4, i, nr_bnd_cells(i)) = ycor(n-1,m  )
                    endif
                else if (kcs(nu,m  ) == 1) then
                   ! which boundary: lower boundary
          nto_loop2: do i = 1, nto
                        do i1 = min(mnbnd(1,i),mnbnd(3,i)), max(mnbnd(1,i),mnbnd(3,i))
                            do j1 = min(mnbnd(2,i),mnbnd(4,i)),  max(mnbnd(2,i),mnbnd(4,i))
                                if (m == i1 .and. n == j1) then
                                    found = .true.
                                    exit nto_loop2
                                end if
                            end do
                        end do
                    end do nto_loop2
                    if (found) then
                        nr_bnd_cells(i) = nr_bnd_cells(i)+1
                        bnd_nr(nr_bnd_cells(i),i) = flow_vol(cellindex)
                        open_bnd(1, i, nr_bnd_cells(i)) = xcor(n  ,m-1)
                        open_bnd(2, i, nr_bnd_cells(i)) = ycor(n  ,m-1)
                        open_bnd(3 ,i, nr_bnd_cells(i)) = xcor(n  ,m  )
                        open_bnd(4, i, nr_bnd_cells(i)) = ycor(n  ,m  )
                    endif
                else if (kcs(n  ,md) == 1) then
                   ! which boundary: right boundary
          nto_loop3: do i = 1, nto
                        do i1 = min(mnbnd(1,i),mnbnd(3,i)), max(mnbnd(1,i),mnbnd(3,i))
                            do j1 = min(mnbnd(2,i),mnbnd(4,i)),  max(mnbnd(2,i),mnbnd(4,i))
                                if (m == i1 .and. n == j1) then
                                    found = .true.
                                    exit nto_loop3
                                end if
                            end do
                        end do
                    end do nto_loop3
                    if (found) then
                        nr_bnd_cells(i) = nr_bnd_cells(i)+1
                        bnd_nr(nr_bnd_cells(i),i) = flow_vol(cellindex)
                        open_bnd(1, i, nr_bnd_cells(i)) = xcor(n-1,m-1)
                        open_bnd(2, i, nr_bnd_cells(i)) = ycor(n-1,m-1)
                        open_bnd(3 ,i, nr_bnd_cells(i)) = xcor(n  ,m-1)
                        open_bnd(4, i, nr_bnd_cells(i)) = ycor(n  ,m-1)
                    endif
                else if (kcs(n  ,mu) == 1) then
                   ! which boundary: left boundary
          nto_loop4: do i = 1, nto
                        do i1 = min(mnbnd(1,i),mnbnd(3,i)), max(mnbnd(1,i),mnbnd(3,i))
                            do j1 = min(mnbnd(2,i),mnbnd(4,i)),  max(mnbnd(2,i),mnbnd(4,i))
                                if (m == i1 .and. n == j1) then
                                    found = .true.
                                    exit nto_loop4
                                end if
                            end do
                        end do
                    end do nto_loop4
                    if (found) then
                        nr_bnd_cells(i) = nr_bnd_cells(i)+1
                        bnd_nr(nr_bnd_cells(i),i) = flow_vol(cellindex)
                        open_bnd(1, i, nr_bnd_cells(i)) = xcor(n-1,m  )
                        open_bnd(2, i, nr_bnd_cells(i)) = ycor(n-1,m  )
                        open_bnd(3 ,i, nr_bnd_cells(i)) = xcor(n  ,m  )
                        open_bnd(4, i, nr_bnd_cells(i)) = ycor(n  ,m  )
                    end if
                endif
                
            end if
        end do
    end do
    !   
    ! Write the boundary file
    ! 
    bndfilename = trim(meta%modelname) // '.bnd'
    open(newunit = lunbnd, file= trim(bndfilename))

    if (nto > 0) then
        write(lunbnd, '(i0.0)') nto
        do i = 1, nto
            write(lunbnd, '(a)') nambnd(i)
            write(lunbnd, '(i0.0)') nr_bnd_cells(i)
            do j = 1, nr_bnd_cells(i)
                write(lunbnd, '(i0.0, 4es25.17)') bnd_nr(j,i), (open_bnd(k,i,j), k=1,4)
            end do 
        end do
    else
        write(lunbnd, '(i2)') nto
    endif
    close(lunbnd)  
    !
    !===============================================================================
    ! Write the waqgeom netcdf file
    !===============================================================================
    !   
    ierr = 0
    geomfilename = trim(meta%modelname) //'_waqgeom.nc' ! Should be equal to the name given in the hyd-file (that file is written in the routine wrwaqhyd)
    !
    ! create or open the file
    !
    ierr = nf90_create(geomfilename, 0, igeomfile); 
    call nc_check_err(lundia, ierr, "creating file", geomfilename)
    if (ierr/=0) goto 9999
    ierr = ug_addglobalatts(igeomfile, meta)
    call nc_check_err(lundia, ierr, "global attributes", geomfilename)
    if (ierr/=0) goto 9999
    !
    ! Coordinates
    !
    if (sferic) then
       crs%epsg_code = 4326    
    end if

    ierr = t_ug_meshgeom_destructor(meshgeom)
    
    meshgeom%meshName = 'mesh2d'
    meshgeom%dim = 2
    meshgeom%start_index = 1

    meshgeom%numNode = nr_nodes
    meshgeom%nodex => nodex 
    meshgeom%nodey => nodey 
    meshgeom%nodez => nodez 
    
    meshgeom%numedge = nr_edges
    meshgeom%edgex => edgex 
    meshgeom%edgey => edgey 

    meshgeom%maxnumfacenodes = 4
    meshgeom%edge_nodes => edge_nodes
    meshgeom%edge_faces => edge_faces
    
    meshgeom%numFace = nr_elems 
    meshgeom%facex => facex
    meshgeom%facey => facey
    
    meshgeom%face_nodes => face_nodes 

    !
    ! Optionally aggregate the mesh
    !
    if (aggre==1) then
        allocate(iapnt(nr_elems))
        do m = 1, mmax
            do n = 1, nmax
                cellindex = func(m, n, nmax)
                elm = flow_vol(cellindex)
                if (elm > 0) then
                    iapnt(elm) = isaggrl(cellindex)
                end if
            end do
        end do
        success = aggregate_ugrid_geometry(meshgeom, aggregated_meshgeom, edge_type, aggr_edge_type, iapnt)
        if(success) then
            meshgeom = aggregated_meshgeom
            edge_type => aggr_edge_type
        end if 
    end if
    !
    ! Write mesh as UGRID
    !
    ierr = ug_write_mesh_struct(igeomfile, meshids, networkids, crs, meshgeom)
    call nc_check_err(lundia, ierr, "writing mesh", geomfilename)
    !
    ! Write edge type variable (this is an extra variable that is not part of the UGRID standard).
    !
    call write_edge_type_variable(igeomfile, meshids, meshgeom%meshName, edge_type)
    call nc_check_err(lundia, ierr, "writing mesh", geomfilename)
    !
9999 continue
    ierr = nf90_close(igeomfile); 
    call nc_check_err(lundia, ierr, "closing file", geomfilename)
    !
    ! Deallocate temporary variables
    !
    deallocate(nodex)
    deallocate(nodey)
    deallocate(nodez)
    deallocate(edge_nodes)
    deallocate(edgex)
    deallocate(edgey)
    deallocate(facex)
    deallocate(facey)
    deallocate(node_mask)
    deallocate(flow_vol)
    deallocate(nr_bnd_cells)
    deallocate(open_bnd)
    deallocate(bnd_nr)
    
end subroutine wrwaqgeomcl

integer function func(i, j, nmax) 
    integer i, j, nmax
    func = j + (i-1)*nmax
end function func

end module m_write_waqgeom_curvilinear
