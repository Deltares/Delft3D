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

module m_write_waqgeom
    use m_alloc
    use MessageHandling
    use m_utils_waqgeom
    
    private
    
    public write_waqgeom_file
    
    contains 
    
    !> Write an unstructured waqgeom grid from UGRID file format.
    !! Write netnode coordinates, edges (netlinks), net boundary and elements (netelems).
    function write_waqgeom_file(filename, meta, crs, waqgeom, edge_type, conv_type, conv_version) result (success)
        use netcdf
        use io_netcdf
        use io_ugrid

        character(len=*)   , intent(in)        :: filename
        type(t_ug_meta)    , intent(in)        :: meta      
        type(t_crs)        , intent(in)        :: crs
        type(t_ug_meshgeom), intent(in)        :: waqgeom
        integer            , intent(in)        :: edge_type(:)
        integer            , intent(in)        :: conv_type 
        real(8)            , intent(in)        :: conv_version

        logical                                :: success !< Result status, true if successful.

!   local
        type(t_ug_mesh)                        :: meshids               !< Set of NetCDF-ids for all mesh geometry arrays.
        type(t_ug_network)                     :: networkids            !< Set of NetCDF-ids for all network arrays.
    
        success = .false.

        ! Write the waqgeom netcdf file

        ierr = 0

        ! create or open the file
        ierr = nf90_create(filename, 0, igeomfile); 
!        call nc_check_err(lundia, ierr, "creating file", geomfilename)
        if (ierr/=0) goto 9999
        ierr = ug_addglobalatts(igeomfile, meta)
!        call nc_check_err(lundia, ierr, "global attributes", geomfilename)
        if (ierr/=0) goto 9999


        ! Write mesh as UGRID
        ierr = ug_write_mesh_struct(igeomfile, meshids, networkids, crs, waqgeom)
!        call nc_check_err(lundia, ierr, "writing mesh", geomfilename)
        if (ierr/=0) goto 9999

        ! Write edge type variable (this is an extra variable that is not part of the UGRID standard).
        call write_edge_type_variable(igeomfile, meshids, waqgeom%meshName, edge_type)
!        call nc_check_err(lundia, ierr, "writing mesh", geomfilename)
        if (ierr/=0) goto 9999

        success = .true.

9999    continue
        ierr = nf90_close(igeomfile); 
!        call nc_check_err(lundia, ierr, "closing file", geomfilename)

    end function write_waqgeom_file
    end module