!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2026.
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

module m_read_waqgeom
   use m_alloc
   use MessageHandling
   use m_utils_waqgeom

   private

   public read_waqgeom_file

contains

   !> Reads an unstructured waqgeom grid from UGRID file format.
   !! Reads netnode coordinates, edges (netlinks), net boundary and elements (netelems).
   function read_waqgeom_file(filename, meta, crs, waqgeom, edge_type, idomain, iglobal, conv_type, conv_version) result(success)
      use netcdf
      use io_netcdf
      use io_ugrid

      implicit none

      character(len=*) :: filename
      type(t_ug_meta), intent(out) :: meta
      type(t_crs), intent(out) :: crs
      type(t_ug_meshgeom), intent(out) :: waqgeom
      integer, pointer, intent(out) :: edge_type(:)
      integer, pointer, intent(out) :: idomain(:)
      integer, pointer, intent(out) :: iglobal(:)

      logical :: success !< Result status, true if successful.

      ! NetCDF variables
      !> Dimensions   Node variables Link variables Link type Boundary variables Element variables Computational boundaries
      integer :: conv_type
      real(8) :: conv_version
      integer :: ierr
      integer :: ioncid
      integer :: ncid
      integer :: nmesh
      integer :: id_edgetypes
      integer :: id_idomain
      integer :: id_iglobal
      integer :: file_size
      integer :: i_mesh, i_netw, ifill, varid
      logical :: found_sigma, found_z
      real(kind=dp), allocatable, dimension(:) :: layer_s, interface_s, layer_z, interface_z
      integer :: end_z_layers

      success = .false.
      ierr = nf90_noerr
      conv_type = IONC_CONV_NULL
      conv_version = 0.0

      inquire (FILE=filename, SIZE=file_size)
      if (file_size == 0) then
         return
      end if

      ierr = ionc_open(trim(filename), nf90_nowrite, ioncid)
      if (ierr /= nf90_noerr) then
         call mess(LEVEL_ERROR, 'File '//trim(filename)//' could not be opened.')
         return
      end if
      !
      ierr = ionc_get_meta_data(ioncid, meta)
      ierr = ionc_get_coordinate_reference_system(ioncid, crs)

      ierr = ionc_inq_conventions(ioncid, conv_type, conv_version)
      if ((ierr == nf90_noerr .and. conv_type /= IONC_CONV_UGRID) .or. &
          (ierr == nf90_noerr .and. conv_type == IONC_CONV_UGRID .and. conv_version < 1.0)) then
         ! not a UGRID or old format grid file
         call mess(LEVEL_ERROR, 'Mesh convention should be UGRID 1.0 or higher in: '//trim(filename))
         ierr = ionc_close(ioncid)
         return
      end if
      ! It is a valid UGRID file format
      ierr = ionc_get_mesh_count(ioncid, nmesh) ! UGRID: required
      if (nmesh == 0) then
         call mess(LEVEL_ERROR, 'No mesh found in UGRID file: '//trim(filename))
         return
      end if
      if (nmesh > 1) then
         call mess(LEVEL_ERROR, 'More than one mesh found in UGRID file: '//trim(filename))
         return
      end if
      i_mesh = 1
      i_netw = -1

      ! Read the mesh
      ierr = ionc_get_meshgeom(ioncid, i_mesh, i_netw, waqgeom, includeArrays=.true.)

      if (waqgeom%numface > 0) then
         call reallocP(waqgeom%face_nodes, (/waqgeom%maxnumfacenodes, waqgeom%numface/), keepExisting=.false.)
         ierr = ionc_get_face_nodes(ioncid, i_mesh, waqgeom%face_nodes, ifill, startindex=1)
         if (ierr /= nf90_noerr) then
            call mess(LEVEL_ERROR, 'Unable to read face_nodes table in UGRID file: '//trim(filename))
            return
         end if

         call reallocP(waqgeom%face_edges, (/waqgeom%maxnumfacenodes, waqgeom%numface/), keepExisting=.false.)
         ierr = ionc_get_face_edges(ioncid, i_mesh, waqgeom%face_edges, ifill, startindex=1)
         if (ierr /= nf90_noerr) then
            call mess(LEVEL_ERROR, 'Unable to read face_edges table in UGRID file: '//trim(filename))
            return
         end if

         call reallocP(waqgeom%edge_faces, (/2, waqgeom%numedge/), keepExisting=.false.)
         ierr = ionc_get_edge_faces(ioncid, i_mesh, waqgeom%edge_faces, ifill, startindex=1)
         if (ierr /= nf90_noerr) then
            call mess(LEVEL_ERROR, 'Unable to read edge_faces table in UGRID file: '//trim(filename))
            return
         end if

         ierr = ionc_get_ncid(ioncid, ncid)

         ! note: instead of variable names, we should look at the mesh attributes (but these were not set yet)
         ierr = nf90_inq_varid(ncid, trim(waqgeom%meshname)//"_edge_type", id_edgetypes)
         if (ierr == nf90_noerr) then
            call reallocP(edge_type, waqgeom%numedge, keepExisting=.false.)
            ierr = nf90_get_var(ncid, id_edgetypes, edge_type, count=(/waqgeom%numedge/))
         end if
         if (ierr /= nf90_noerr) then
            call mess(LEVEL_ERROR, 'Unable to read edge_type table in UGRID file: '//trim(filename))
            return
         end if

         ! No check on read errors for face domain number and global number, because they only exist for parallel runs
         ierr = nf90_inq_varid(ncid, trim(waqgeom%meshname)//"_face_domain_number", id_idomain)
         if (ierr == nf90_noerr) then
            call reallocP(idomain, waqgeom%numface, keepExisting=.false.)
            ierr = nf90_get_var(ncid, id_idomain, idomain, count=(/waqgeom%numface/))
            if (ierr /= nf90_noerr) then
               call mess(LEVEL_ERROR, 'Unable to read face_domain_number table in UGRID file: '//trim(filename))
               return
            end if
         end if

         ierr = nf90_inq_varid(ncid, trim(waqgeom%meshname)//"_face_global_number", id_iglobal)
         if (ierr == nf90_noerr) then
            call reallocP(iglobal, waqgeom%numface, keepExisting=.false.)
            ierr = nf90_get_var(ncid, id_iglobal, iglobal, count=(/waqgeom%numface/))
            if (ierr /= nf90_noerr) then
               call mess(LEVEL_ERROR, 'Unable to read face_global_number table in UGRID file: '//trim(filename))
               return
            end if
         end if
      end if

      call add_facexy_waqgeom(waqgeom)
      call add_edgexy_waqgeom(waqgeom)
      call add_facelinks_waqgeom(waqgeom)

      ! Read the layer information
      ! Deternmine layer type
      found_sigma = nf90_inq_varid(ncid, trim(waqgeom%meshname)//'_layer_sigma', varid) == nf90_noerr
      found_z = nf90_inq_varid(ncid, trim(waqgeom%meshname)//'_layer_z', varid) == nf90_noerr
      if (found_sigma.and.found_z) then
         waqgeom%layertype = LAYERTYPE_OCEAN_SIGMA_Z
      else if (found_sigma) then
         waqgeom%layertype = LAYERTYPE_OCEANSIGMA
      else if (found_z) then
         waqgeom%layertype = LAYERTYPE_Z
      else
         waqgeom%layertype = -1
      end if

      ! Read layer information when available
      if(waqgeom%layertype > 0) then
         ! Read number of layers
         ierr = nf90_inq_dimid(ncid, trim(waqgeom%meshname)//'_nLayers', varid)
         if (ierr /= nf90_noerr) then
            call mess(LEVEL_ERROR, 'Unable to read layer_dimension in UGRID file: '//trim(filename))
            return
         end if
         ierr = nf90_inquire_dimension(ncid, varid, len=waqgeom%num_layers)
         if (ierr /= nf90_noerr) then
            call mess(LEVEL_ERROR, 'Unable to read layer_dimension in UGRID file: '//trim(filename))
            return
         end if
         
         ! Allocate layer arrays
         call reallocP(waqgeom%layer_zs, waqgeom%num_layers, keepExisting=.false.)
         call reallocP(waqgeom%interface_zs, waqgeom%num_layers + 1, keepExisting=.false.)
         
         ! Read layers and interfaces
         if (found_sigma) then
            call realloc(layer_s, waqgeom%num_layers, keepExisting=.false.)
            layer_s = -999.0_dp            
            ierr = nf90_inq_varid(ncid, trim(waqgeom%meshname)//"_layer_sigma", varid)
            if (ierr == nf90_noerr) then
               ierr = nf90_get_var(ncid, varid, layer_s, count=(/waqgeom%num_layers/))
            end if
            if (ierr /= nf90_noerr) then
               call mess(LEVEL_ERROR, 'Unable to read layer_sigma table in UGRID file: '//trim(filename))
               return
            end if

            call realloc(interface_s, waqgeom%num_layers + 1, keepExisting=.false.)
            ierr = nf90_inq_varid(ncid, trim(waqgeom%meshname)//"_interface_sigma", varid)
            if (ierr == nf90_noerr) then
               ierr = nf90_get_var(ncid, varid, interface_s, count=(/waqgeom%num_layers + 1/))
            end if
            if (ierr /= nf90_noerr) then
               call mess(LEVEL_ERROR, 'Unable to read interface_sigma table in UGRID file: '//trim(filename))
               return
            end if
         end if

         if (found_z) then
            call realloc(layer_z, waqgeom%num_layers, keepExisting=.false.)
            ierr = nf90_inq_varid(ncid, trim(waqgeom%meshname)//"_layer_z", varid)
            if (ierr == nf90_noerr) then
               ierr = nf90_get_var(ncid, varid, layer_z, count=(/waqgeom%num_layers/))
            end if
            if (ierr /= nf90_noerr) then
               call mess(LEVEL_ERROR, 'Unable to read layer_z table in UGRID file: '//trim(filename))
               return
            end if

            call realloc(interface_z, waqgeom%num_layers + 1, keepExisting=.false.)
            ierr = nf90_inq_varid(ncid, trim(waqgeom%meshname)//"_interface_z", varid)
            if (ierr == nf90_noerr) then
               ierr = nf90_get_var(ncid, varid, interface_z, count=(/waqgeom%num_layers + 1/))
            end if
            if (ierr /= nf90_noerr) then
               call mess(LEVEL_ERROR, 'Unable to read interface_z table in UGRID file: '//trim(filename))
               return
            end if
         end if

         ! Fill the layer arrays in waqgeom depending on layer type
         if (waqgeom%layertype == LAYERTYPE_OCEANSIGMA) then
            ! Copy the sigma layers
            waqgeom%layer_zs = layer_s
            waqgeom%interface_zs = interface_s
         else if (waqgeom%layertype == LAYERTYPE_Z) then
            ! Copy the z layers
            waqgeom%layer_zs = layer_z
            waqgeom%interface_zs = interface_z
         else if  (waqgeom%layertype == LAYERTYPE_OCEAN_SIGMA_Z) then
            ! Merge z and sigma layers
            ! Find number of sigma layers on top of z layers
            waqgeom%numtopsig = waqgeom%num_layers - count(abs(layer_s) > 1.0_dp)
            if (waqgeom%numtopsig == 0) then
               call mess(LEVEL_ERROR, 'The z-sigma model has no sigma layers on top of z layers in UGRID file: '//trim(filename))
               return
            end if
            ! Combine the arrays
            end_z_layers = waqgeom%num_layers - waqgeom%numtopsig
            waqgeom%layer_zs(1:end_z_layers) = layer_z(1:end_z_layers)
            waqgeom%layer_zs(end_z_layers + 1:waqgeom%num_layers) = layer_s(end_z_layers + 1:waqgeom%num_layers)
            waqgeom%interface_zs(1:end_z_layers + 1) = interface_z(1:end_z_layers + 1)
            waqgeom%interface_zs(end_z_layers + 2:waqgeom%num_layers + 1) = interface_s(end_z_layers + 2:waqgeom%num_layers + 1)
         end if
      end if

      success = .true.
   end function read_waqgeom_file
end module
   