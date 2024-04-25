subroutine write_wave_grid_netcdf (i_grid, sg, gridname, filename)
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
!  Write SWAN grid to a temporary NetCDF file, to be used by ESMF_RegridWeightsGen
!  (destination grid).
!  SWAN data is located at the grid points and not at the cell centres.
!  Regridding is needed to the grid points of the SWAN grid.
!  But ESMF_RegridWeightsGen is going to regrid to the specified cell centres.
!  Workaround: create a shifted grid, size (mmax+1)*(nmax+1), having the SWAN grid points
!  as cell centres.
!
!!--pseudo code and references--------------------------------------------------
!
! http://www.earthsystemmodeling.org/esmf_releases/last_built/ESMF_refdoc/node3.html
!
!!--declarations----------------------------------------------------------------
    use wave_data
    use swan_flow_grid_maps
    use netcdf
    use dwaves_version_module
    !
    implicit none
!
! Global variables
!
    integer                   :: i_grid
    type (grid)               :: sg           ! swan grid
    character(*), intent(in)  :: gridname     ! runid
    character(*), intent(out) :: filename
!
! Local variables
!
    integer                                     :: ind
    integer                                     :: i
    integer                                     :: ij
    integer                                     :: j
    integer                                     :: n
    integer                                     :: ncell
    integer                                     :: nnode
    integer                                     :: idfile
    integer                                     :: iddim_corners
    integer                                     :: iddim_ncell
    integer                                     :: iddim_nemax
    integer                                     :: iddim_rank
    integer                                     :: idvar_coords
    integer                                     :: idvar_econn
    integer                                     :: idvar_griddims
    integer                                     :: idvar_mask
    integer                                     :: idvar_neconn
    integer                                     :: idvar_nx
    integer                                     :: idvar_ny
    integer                                     :: idvar_x
    integer                                     :: idvar_y
    integer                                     :: ierror
    integer, external                           :: nc_def_var
    real(hp)                                    :: cubeSize
    real(hp), dimension(:)    , allocatable     :: ncellarray
    real(hp), dimension(:,:)  , allocatable     :: gridcor
    real(hp), dimension(:,:)  , allocatable     :: grid_corner
    real(hp), dimension(:,:)  , allocatable     :: xshift
    real(hp), dimension(:,:)  , allocatable     :: yshift
    integer , dimension(:,:)  , allocatable     :: elemconn
    byte    , dimension(:)    , allocatable     :: nelemconn
    character(256)                              :: full_version
    character(8)                                :: cdate
    character(10)                               :: ctime
    character(5)                                :: czone
!
!! executable statements -------------------------------------------------------
!
    ! Sferic:
    ! Create shifted grid
    ! Cartesian:
    !
    !
    if (sg%sferic) then
       ncell = sg%mmax * sg%nmax
       nnode = (sg%mmax+1) * (sg%nmax+1)
       allocate(ncellarray  (ncell)  , stat=ierror)
       allocate(gridcor     (4,ncell), stat=ierror)
       allocate(xshift(sg%mmax+1,sg%nmax+1), stat=ierror)
       allocate(yshift(sg%mmax+1,sg%nmax+1), stat=ierror)
    else
       ! Bilinear: weights are generated for all grid points
       ! Use all gridpoints to define cells
       !
       ncell = (sg%mmax-1) * (sg%nmax-1)
       ! nnode: number of nodes
       nnode = sg%mmax * sg%nmax
       allocate (grid_corner(2,nnode), STAT=ierror)
       allocate ( elemconn  (4,ncell), STAT=ierror)
       allocate (nelemconn  (ncell)  , STAT=ierror)
    endif
    if (sg%sferic) then
       !
       ! Create shifted grid, having the SWAN grid points as cell centres
       !
       xshift = 0.0_hp
       yshift = 0.0_hp
       if (ierror/=0) write(*,*) "ERROR allocatoin in write_wave_grid_netcdf"
       !
       ! Internal points
       do i=2, sg%mmax
          do j=2, sg%nmax
             if (        sg%x(i-1,j-1) /= 0.0_hp &
                 & .and. sg%x(i,j-1)   /= 0.0_hp &
                 & .and. sg%x(i,j)     /= 0.0_hp &
                 & .and. sg%x(i-1,j)   /= 0.0_hp  ) then
                xshift(i,j) = ( sg%x(i-1,j-1) + sg%x(i,j-1) + sg%x(i,j) + sg%x(i-1,j) ) / 4.0_hp
             endif
             if (        sg%y(i-1,j-1) /= 0.0_hp &
                 & .and. sg%y(i,j-1)   /= 0.0_hp &
                 & .and. sg%y(i,j)     /= 0.0_hp &
                 & .and. sg%y(i-1,j)   /= 0.0_hp  ) then
                yshift(i,j) = ( sg%y(i-1,j-1) + sg%y(i,j-1) + sg%y(i,j) + sg%y(i-1,j) ) / 4.0_hp
             endif
          enddo
       enddo
       !
       ! Boundary lines (bottom, top, left, right)
       do i=2, sg%mmax
          ! bottom
          do j=1, sg%nmax-1
             if (        xshift(i,j)   == 0.0_hp &
                 & .and. sg%x(i-1,j)   /= 0.0_hp &
                 & .and. sg%x(i,j)     /= 0.0_hp &
                 & .and. xshift(i,j+1) /= 0.0_hp  ) then
                !xshift(i,j) = sg%x(i-1,j) + sg%x(i,j) - xshift(i,j+1)
                xshift(i,j) = 0.5_hp*(sg%x(i-1,j) + sg%x(i,j))! - xshift(i,j+1)
             endif
             if (        yshift(i,j)  == 0.0_hp &
                 & .and. sg%y(i-1,j)  /= 0.0_hp &
                 & .and. sg%y(i,j)    /= 0.0_hp &
                 & .and. yshift(i,j+1)/= 0.0_hp  ) then
                !yshift(i,j) = sg%y(i-1,j) + sg%y(i,j) - yshift(i,j+1)
                yshift(i,j) = 0.5_hp*(sg%y(i-1,j) + sg%y(i,j))! - yshift(i,j+1)
             endif
          enddo
          ! top
          do j= 2, sg%nmax+1
             if (        xshift(i  ,j)   == 0.0_hp &
                 & .and.   sg%x(i-1,j-1) /= 0.0_hp &
                 & .and.   sg%x(i  ,j-1) /= 0.0_hp &
                 & .and. xshift(i  ,j-1) /= 0.0_hp  ) then
                !xshift(i,j) = sg%x(i-1,j-1) + sg%x(i,j-1) - xshift(i,j-1)
                xshift(i,j) = 0.5_hp*(sg%x(i-1,j-1) + sg%x(i,j-1))! - xshift(i,j-1)
             endif
             if (        yshift(i  ,j)   == 0.0_hp &
                 & .and.   sg%y(i-1,j-1) /= 0.0_hp &
                 & .and.   sg%y(i  ,j-1) /= 0.0_hp &
                 & .and. yshift(i  ,j-1) /= 0.0_hp  ) then
                !yshift(i,j) = sg%y(i-1,j-1) + sg%y(i,j-1) - yshift(i,j-1)
                yshift(i,j) = 0.5_hp*(sg%y(i-1,j-1) + sg%y(i,j-1))! - yshift(i,j-1)
             endif
          enddo
       enddo
       do j=2, sg%nmax
          ! left
          do i=1, sg%mmax-1
             if (        xshift(i  ,j  ) == 0.0_hp &
                 & .and.   sg%x(i  ,j-1) /= 0.0_hp &
                 & .and.   sg%x(i  ,j  ) /= 0.0_hp &
                 & .and. xshift(i+1,j  ) /= 0.0_hp  ) then
                !xshift(i,j) = sg%x(i,j-1) + sg%x(i,j) - xshift(i+1,j)
                xshift(i,j) = 0.5_hp*(sg%x(i,j-1) + sg%x(i,j))! - xshift(i+1,j)
             endif
             if (        yshift(i  ,j  ) == 0.0_hp &
                 & .and.   sg%y(i  ,j-1) /= 0.0_hp &
                 & .and.   sg%y(i  ,j  ) /= 0.0_hp &
                 & .and. yshift(i+1,j  ) /= 0.0_hp  ) then
                !yshift(i,j) = sg%y(i,j-1) + sg%y(i,j) - yshift(i+1,j)
                yshift(i,j) = 0.5_hp*(sg%y(i,j-1) + sg%y(i,j))! - yshift(i+1,j)
             endif
          enddo
          ! right
          do i=2, sg%mmax+1
             if (        xshift(i  ,j  ) == 0.0_hp &
                 & .and.   sg%x(i-1,j-1) /= 0.0_hp &
                 & .and.   sg%x(i-1,j  ) /= 0.0_hp &
                 & .and. xshift(i-1,j  ) /= 0.0_hp  ) then
                !xshift(i,j) = sg%x(i-1,j-1) + sg%x(i-1,j) - xshift(i-1,j)
                xshift(i,j) = 0.5_hp*(sg%x(i-1,j-1) + sg%x(i-1,j))! - xshift(i-1,j)
             endif
             if (        yshift(i  ,j  ) == 0.0_hp &
                 & .and.   sg%y(i-1,j-1) /= 0.0_hp &
                 & .and.   sg%y(i-1,j  ) /= 0.0_hp &
                 & .and. yshift(i-1,j  ) /= 0.0_hp  ) then
                !yshift(i,j) = sg%y(i-1,j-1) + sg%y(i-1,j) - yshift(i-1,j)
                yshift(i,j) = 0.5_hp*(sg%y(i-1,j-1) + sg%y(i-1,j))! - yshift(i-1,j)
             endif
          enddo
       enddo
       !
       ! Boundary corner points
       ! Lower left corners
       do i=1, sg%mmax
          do j=1, sg%nmax
             if (xshift(i,j) == 0.0_hp .and. xshift(i,j+1) /= 0.0_hp .and. xshift(i+1,j) /= 0.0_hp) then
                !xshift(i,j) = xshift(i+1,j)
                xshift(i,j) = sg%x(i,j)
             endif
             if (yshift(i,j) == 0.0_hp .and. yshift(i+1,j) /= 0.0_hp .and. yshift(i,j+1) /= 0.0_hp) then
                !yshift(i,j) = yshift(i,j+1)
                yshift(i,j) = sg%y(i,j)
             endif
          enddo
       enddo
       ! Lower right corners
       do i=2, sg%mmax+1
          do j=1, sg%nmax
             if (xshift(i,j) == 0.0_hp .and. xshift(i-1,j) /= 0.0_hp .and. xshift(i,j+1) /= 0.0_hp) then
                !xshift(i,j) = xshift(i-1,j)
                xshift(i,j) = sg%x(i-1,j)
             endif
             if (yshift(i,j) == 0.0_hp .and. yshift(i-1,j) /= 0.0_hp .and. yshift(i,j+1) /= 0.0_hp) then
                !yshift(i,j) = yshift(i,j+1)
                yshift(i,j) = sg%y(i-1,j)
             endif
          enddo
       enddo
       ! Upper left corners
       do i=1, sg%mmax
          do j=2, sg%nmax+1
             if (xshift(i,j) == 0.0_hp .and. xshift(i+1,j) /= 0.0_hp .and. xshift(i,j-1) /= 0.0_hp) then
                !xshift(i,j) = xshift(i+1,j)
                xshift(i,j) = sg%x(i,j-1)
             endif
             if (yshift(i,j) == 0.0_hp .and. yshift(i+1,j) /= 0.0_hp .and. yshift(i,j-1) /= 0.0_hp) then
                !yshift(i,j) = yshift(i,j-1)
                yshift(i,j) = sg%y(i,j-1)
             endif
          enddo
       enddo
       ! Upper right corners
       do i=2, sg%mmax+1
          do j=2, sg%nmax+1
             if (xshift(i,j) == 0.0_hp .and. xshift(i-1,j) /= 0.0_hp .and. xshift(i,j-1) /= 0.0_hp) then
                !xshift(i,j) = xshift(i-1,j)
                xshift(i,j) = sg%x(i-1,j-1)
             endif
             if (yshift(i,j) == 0.0_hp .and. yshift(i-1,j) /= 0.0_hp .and. yshift(i,j-1) /= 0.0_hp) then
                !yshift(i,j) = yshift(i,j-1)
                yshift(i,j) = sg%y(i-1,j-1)
             endif
          enddo
       enddo
       !
       ! TO DO: Can this shifted grid further be optimised?
       !do j=1, sg%nmax+1
       !    write(77,'(a,i2,a,22(e25.17,1x))') " ETA=   ", j, "  ",(xshift(i,j),i=1,sg%mmax+1)
       !enddo
       !do j=1, sg%nmax+1
       !    write(77,'(a,i2,a,22(e25.17,1x))') " ETA=   ", j, "  ",(yshift(i,j),i=1,sg%mmax+1)
       !enddo
    else
       !
       ! Cartesian
       ! Nothing to do
    endif
    !
    call getfullversionstring_dwaves(full_version)
    call date_and_time(cdate, ctime, czone)
    !
    ! define name of output file
    !
    write(filename,'(2a)') "TMP_ESMF_RegridWeightGen_", trim(gridname)
    !
    ! gridname may contain an extension
    ! Replace the '.' by a '_', just to be safe
    !
    ind = index(filename, '.', back = .true.)
    if (ind > 0) filename(ind:ind) = '_'
    write(filename,'(a,a,i4.4,a)') trim(filename), "_destination_", i_grid, ".nc"
    !
    ! create file
    !
    ierror = nf90_create(filename, 0, idfile); call nc_check_err(ierror, "creating file", filename)
    !
    ! global attributes
    !
    ierror = nf90_put_att(idfile, nf90_global,  'institution', trim(company)); call nc_check_err(ierror, "put_att global institution", filename)
    ierror = nf90_put_att(idfile, nf90_global,  'references', trim(company_url)); call nc_check_err(ierror, "put_att global references", filename)
    ierror = nf90_put_att(idfile, nf90_global,  'source', trim(full_version)); call nc_check_err(ierror, "put_att global source", filename)
    ierror = nf90_put_att(idfile, nf90_global,  'history', &
           'Created on '//cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'//ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5)// &
           ', '//trim(product_name)); call nc_check_err(ierror, "put_att global history", filename)
    if (.not.sg%sferic) then
       ierror = nf90_put_att(idfile, nf90_global,  'gridType', 'unstructured'); call nc_check_err(ierror, "put_att global gridType", filename)
       ierror = nf90_put_att(idfile, nf90_global,  'version', '0.9'); call nc_check_err(ierror, "put_att global version", filename)
    endif
    !
    ! dimensions
    !
    if (sg%sferic) then
       ! SCRIP format
       ierror = nf90_def_dim(idfile, 'grid_size', ncell, iddim_ncell); call nc_check_err(ierror, "def_dim grid_size", filename)
       ierror = nf90_def_dim(idfile, 'grid_corners', 4, iddim_corners); call nc_check_err(ierror, "def_dim grid_corners", filename)
       ierror = nf90_def_dim(idfile, 'grid_rank', 2, iddim_rank); call nc_check_err(ierror, "def_dim grid_rank", filename)
    else
       ! ESMFgrid format
       ierror = nf90_def_dim(idfile, 'elementCount', ncell, iddim_ncell); call nc_check_err(ierror, "def_dim elementCount", filename)
       ierror = nf90_def_dim(idfile, 'nodeCount', nnode, iddim_corners); call nc_check_err(ierror, "def_dim nodeCount", filename)
       ierror = nf90_def_dim(idfile, 'maxNodePElement', 4, iddim_nemax); call nc_check_err(ierror, "def_dim maxNodePElement", filename)
       ierror = nf90_def_dim(idfile, 'coordDim', 2, iddim_rank); call nc_check_err(ierror, "def_dim coordDim", filename)
    endif
    !
    ! define vars
    !
    if (sg%sferic) then
       idvar_griddims = nc_def_var(idfile, 'grid_dims'       , nf90_int   , 1, (/iddim_rank/), '', '', '', .false., filename)
       idvar_y        = nc_def_var(idfile, 'grid_center_lat' , nf90_double, 1, (/iddim_ncell/), '', '', "degrees", .false., filename)
       idvar_x        = nc_def_var(idfile, 'grid_center_lon' , nf90_double, 1, (/iddim_ncell/), '', '', "degrees", .false., filename)
       idvar_nx       = nc_def_var(idfile, 'grid_corner_lon' , nf90_double, 2, (/iddim_corners,iddim_ncell/), '', '', "degrees", .false., filename)
       ! ierror         = nf90_def_var_fill(idfile, idvar_nx,  0, -9999); call nc_check_err(ierror, "put_att _FillValue", trim(filename))
       idvar_ny       = nc_def_var(idfile, 'grid_corner_lat' , nf90_double, 2, (/iddim_corners,iddim_ncell/), '', '', "degrees", .false., filename)
       ! ierror         = nf90_def_var_fill(idfile, idvar_ny,  0, -9999); call nc_check_err(ierror, "put_att _FillValue", trim(filename))
       idvar_mask     = nc_def_var(idfile, 'grid_imask' , nf90_double, 1, (/iddim_ncell/), '', '', '', .false., filename)
       ! ierror         = nf90_def_var_fill(idfile, idvar_mask,  0, -9999); call nc_check_err(ierror, "put_att _FillValue", trim(filename))
    else
       idvar_coords   = nc_def_var(idfile, 'nodeCoords'    , nf90_double, 2, (/iddim_rank,iddim_corners/), '', '', "meters", .false., filename)
       idvar_eConn    = nc_def_var(idfile, 'elementConn'   , nf90_int   , 2, (/iddim_nemax,iddim_ncell/)  , '', 'Node Indices that define the element connectivity', '', .false., filename)
       ierror         = nf90_put_att(idfile, idvar_eConn,  '_FillValue', -1); call nc_check_err(ierror, "put_att elementConn fillVal", filename)
       idvar_neConn   = nc_def_var(idfile, 'numElementConn', nf90_int  , 1, (/iddim_ncell/)              , '', 'Number of nodes per element', '', .false., filename)
    endif
    !
    ierror = nf90_enddef(idfile); call nc_check_err(ierror, "enddef", filename)
    !
    ! put vars
    !
    if (sg%sferic) then
       !
       ! ESMF sferic:
       ! Write x,y to destination.nc
       !
       ierror = nf90_put_var(idfile, idvar_griddims, (/sg%mmax-1,sg%nmax-1/), start=(/1/), count=(/2/))   ; call nc_check_err(ierror, "put_var griddims", filename)
       !
       do i=1, sg%mmax
          do j=1, sg%nmax
             ncellarray((i-1)*sg%nmax + j) = sg%y(i,j)
          enddo
       enddo
       ierror = nf90_put_var(idfile, idvar_y       , ncellarray   , start=(/1/), count=(/ncell/)); call nc_check_err(ierror, "put_var y", filename)
       !
       do i=1, sg%mmax
          do j=1, sg%nmax
             ncellarray((i-1)*sg%nmax + j) = sg%x(i,j)
          enddo
       enddo
       ierror = nf90_put_var(idfile, idvar_x       , ncellarray   , start=(/1/), count=(/ncell/)); call nc_check_err(ierror, "put_var x", filename)
       !
       do n=1, ncell
          i = floor(real(n-1)/real(sg%nmax)) + 1
          j = n - sg%nmax*(i-1)
          ! Counter-clockwise!
          gridcor(1,n) = xshift(i  ,j  )
          gridcor(2,n) = xshift(i+1,j  )
          gridcor(3,n) = xshift(i+1,j+1)
          gridcor(4,n) = xshift(i  ,j+1)
       enddo
       ierror = nf90_put_var(idfile, idvar_nx  , gridcor  , start=(/1,1/), count=(/4,ncell/)); call nc_check_err(ierror, "put_var corner_lon", filename)
       !
       do n=1, ncell
          i = floor(real(n-1)/real(sg%nmax)) + 1
          j = n - sg%nmax*(i-1)
          ! Counter-clockwise!
          gridcor(1,n) = yshift(i  ,j  )
          gridcor(2,n) = yshift(i+1,j  )
          gridcor(3,n) = yshift(i+1,j+1)
          gridcor(4,n) = yshift(i  ,j+1)
       enddo
       ierror = nf90_put_var(idfile, idvar_ny  , gridcor  , start=(/1,1/), count=(/4,ncell/)); call nc_check_err(ierror, "put_var corner_lat", filename)
       !
       do i=1, ncell
          ncellarray(i) = 1.0_hp
       enddo
       ierror = nf90_put_var(idfile, idvar_mask, ncellarray  , start=(/1/), count=(/ncell/)); call nc_check_err(ierror, "put_var imask", filename)
    else
       !
       ! ESMF Cartesian:
       !
       grid_corner = -999.0_hp
       do i=1, sg%mmax
          do j=1, sg%nmax
             ij = (j-1)*sg%mmax + i
             grid_corner(1,ij)         = sg%x(i,j)
             grid_corner(2,ij)         = sg%y(i,j)
          enddo
       enddo
       ierror = nf90_put_var(idfile, idvar_coords, grid_corner, start=(/1,1/), count=(/2,nnode/));   call nc_check_err(ierror, "put_var nodeCoords", filename)
       do i=1, sg%mmax - 1
          do j=1, sg%nmax - 1
             ! Counter-clockwise!
             ! cell i,j consists of the 4 nodes i,j   i+1,j    i+1,j+1    i,j+1
             !
             ij = (j-1)*(sg%mmax-1) + i
             elemconn(1,ij) = (j-1)*sg%mmax + i
             elemconn(2,ij) = (j-1)*sg%mmax + i+1
             elemconn(3,ij) = (j  )*sg%mmax + i+1
             elemconn(4,ij) = (j  )*sg%mmax + i
          enddo
       enddo
       ierror = nf90_put_var(idfile, idvar_eConn , elemconn , start=(/1,1/), count=(/4,ncell/)); call nc_check_err(ierror, "put_var elementConn", filename)
       nelemconn = 4
       ierror = nf90_put_var(idfile, idvar_neConn , nelemconn , start=(/1,1/), count=(/ncell/)); call nc_check_err(ierror, "put_var numElementConn", filename)
    endif
    !
    ierror = nf90_close(idfile); call nc_check_err(ierror, "closing file", filename)
    !
    if (sg%sferic) then
       deallocate(gridcor   , stat=ierror)
       deallocate(ncellarray, stat=ierror)
    else
       deallocate (   elemconn, STAT=ierror)
       deallocate (  nelemconn, STAT=ierror)
       deallocate (grid_corner, STAT=ierror)
    endif
    deallocate(xshift    , stat=ierror)
    deallocate(yshift    , stat=ierror)
end subroutine write_wave_grid_netcdf
