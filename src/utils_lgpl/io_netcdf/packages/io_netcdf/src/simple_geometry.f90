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

!> simple_geometry module for operations on simple geometry representation
!! in NetCDF data sets, as defined in the CF-1.8 conventions:
!! http://cfconventions.org/Data/cf-conventions/cf-conventions-1.8/cf-conventions.html#geometries
module simple_geometry
use netcdf
implicit none
!
! Error codes
!
public :: SGEOM_NOERR


!
! Subroutines
!
public :: sgeom_def_geometry_variables

private
!
! Error codes
!
integer, parameter :: SGEOM_NOERR         = 0     !< Successful

!-------------------------------------------------------------------------------
   contains
!-------------------------------------------------------------------------------

! TODO: support lat/lon in geometries.

!> Define geometry related variables, including the geometry container variable and the related coordinates variables.
!! This is currently limited to single-part geometries (i.e., without multiple parts, nor holes).
function sgeom_def_geometry_variables(ncid, geom_var_name, geom_feature_name, geom_type, num_node, id_instance_dim, id_node_count, id_node_coordx, id_node_coordy, add_latlon, id_node_lon, id_node_lat) result(ierr)
   integer,          intent(in   ) :: ncid                  !< NetCDF ID of already opened data set.
   character(len=*), intent(in   ) :: geom_var_name         !< For geometry container variable: name, will also be used as prefix for all other variables. For example: 'station_geom'.
   character(len=*), intent(in   ) :: geom_feature_name     !< Human-readable name of the feature represented by this geometry. Used in long_name strings of all other variables. (should already be trimmed)
   character(len=*), intent(in   ) :: geom_type             !< Geometry type, one of: 'point', 'line', 'polygon'
   integer,          intent(in   ) :: num_node              !< Total number of nodes in all instance geometry.
   integer,          intent(in   ) :: id_instance_dim       !< Dimension ID of instance dimension
   integer,          intent(  out) :: id_node_count         !< Variable ID of node_count variable
   integer,          intent(  out) :: id_node_coordx        !< Variable ID of node x-coordinate variable
   integer,          intent(  out) :: id_node_coordy        !< Variable ID of node y-coordinate variable
   logical,          intent(in   ), optional :: add_latlon  !< Whether or not to add extra lon/lat coordinates for the nodes
                                                            !< (only applicable when the coordx/y variables contain projected coordinates,
                                                            !< and requires id_node_lon/lat to be passed as well).
   integer,          intent(  out), optional :: id_node_lon !< Variable ID of node longitude variable
   integer,          intent(  out), optional :: id_node_lat !< Variable ID of node latitude variable
   integer                         :: ierr                  !< Result status, sgeom_noerr if successful
   
   integer :: id_var, id_node_dim, n
   character(len=256) :: string
   character(len=len_trim(geom_var_name)) :: prefix
   logical :: add_latlon_

   ierr = SGEOM_NOERR

   if (present(add_latlon)) then
      add_latlon_ = add_latlon .and. present(id_node_lon) .and. present(id_node_lat)
   else
      add_latlon_ = .false.
   end if

   prefix=trim(geom_var_name)

   ! Define the dimension of the geometry variables
   ierr = nf90_def_dim(ncid, prefix//'_nNodes', num_node, id_node_dim)
   
   ! Define the geometry container variable
   ierr = nf90_def_var(ncid, trim(geom_var_name), nf90_int, id_var)
   ierr = nf90_put_att(ncid, id_var, 'geometry_type', trim(geom_type))
   ierr = nf90_put_att(ncid, id_var, 'node_count', prefix//'_node_count')
   string = prefix//'_node_coordx' // ' ' // prefix//'_node_coordy'
   if (add_latlon_) then
      n = len_trim(string)
      string(n+1:) = ' '//prefix//'_node_lon' // ' ' // prefix//'_node_lat'
   end if

   ierr = nf90_put_att(ncid, id_var, 'node_coordinates', trim(string))
   
   ! Define variable of node_count
   ierr = nf90_def_var(ncid, prefix//'_node_count', nf90_int, id_instance_dim, id_node_count)
   ierr = nf90_put_att(ncid, id_node_count, 'long_name', 'Count of nodes per '//geom_feature_name)

   ! Define variables of node_coordinate
   ierr = nf90_def_var(ncid, prefix//'_node_coordx', nf90_double, id_node_dim, id_node_coordx)
   ierr = nf90_put_att(ncid, id_node_coordx, 'long_name', 'x-coordinate of '//geom_feature_name)
   ierr = nf90_put_att(ncid, id_node_coordx, 'units', 'm')
   ierr = nf90_put_att(ncid, id_node_coordx, 'standard_name', 'projection_x_coordinate')
   ierr = nf90_put_att(ncid, id_node_coordx, 'axis', 'X')

   ierr = nf90_def_var(ncid, prefix//'_node_coordy', nf90_double, id_node_dim, id_node_coordy)
   ierr = nf90_put_att(ncid, id_node_coordy, 'long_name', 'y-coordinate of '//geom_feature_name)
   ierr = nf90_put_att(ncid, id_node_coordy, 'units', 'm')
   ierr = nf90_put_att(ncid, id_node_coordy, 'standard_name', 'projection_y_coordinate')
   ierr = nf90_put_att(ncid, id_node_coordy, 'axis', 'Y')

   if (add_latlon_) then
      ierr = nf90_def_var(ncid, prefix//'_node_lon', nf90_double, id_node_dim, id_node_lon)
      ierr = nf90_put_att(ncid, id_node_lon, 'long_name', 'longitude of '//geom_feature_name)
      ierr = nf90_put_att(ncid, id_node_lon, 'units', 'degrees_east')
      ierr = nf90_put_att(ncid, id_node_lon, 'standard_name', 'longitude')
      ierr = nf90_put_att(ncid, id_node_lon, 'axis', 'X')

      ierr = nf90_def_var(ncid, prefix//'_node_lat', nf90_double, id_node_dim, id_node_lat)
      ierr = nf90_put_att(ncid, id_node_lat, 'long_name', 'latitude of '//geom_feature_name)
      ierr = nf90_put_att(ncid, id_node_lat, 'units', 'degrees_north')
      ierr = nf90_put_att(ncid, id_node_lat, 'standard_name', 'latitude')
      ierr = nf90_put_att(ncid, id_node_lat, 'axis', 'Y')
   end if

end function sgeom_def_geometry_variables

end module simple_geometry
