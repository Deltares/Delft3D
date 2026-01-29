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
!
!

!> Reading/writing timeseries files in NetCDF format
module m_ec_netcdf_timeseries
   use string_module, only: find_candidates_in_targets
   use m_ec_parameters
   use m_ec_support
   use m_ec_message
   use m_ec_typedefs
   use netcdf
   use m_alloc
   use multi_file_io
   use string_module
   use io_ugrid
   use netcdf_utils, only: ncu_get_att
   implicit none

   private

   public :: ecNetCDFCreate
   public :: ecNetCDFInit
   public :: ecNetCDFFree
   public :: ecNetCDFFree1dArray
   public :: ecNetCDFScan
   public :: ecNetCDFGetTimeseriesValue
   public :: ecNetCDFGetAttrib

contains

   ! =======================================================================
   !> Construct a new netCDF instance with the specified id.
   !! Failure is indicated by returning a null pointer.
   function ecNetCDFCreate(netcdf_id) result(netcdf_ptr)
      type(tEcNetCDF), pointer :: netcdf_ptr !< the new netCDF instance
      integer, intent(in) :: netcdf_id !< unique netCDF instance id

      integer :: istat
      logical :: success

      success = .false.

      allocate (netcdf_ptr, stat=istat)
      if (istat /= 0) then
         call setECMessage("ec_netcdf_timeseries::ecNetCDFCreate: Unable to allocate additional memory.")
         netcdf_ptr => null()
         return
      end if

      ! initialization
      netcdf_ptr%id = netcdf_id
   end function ecNetCDFCreate

   ! =======================================================================
   !> NetCDF (for timeseries) destructor
   function ecNetCDFFree(netcdf) result(success)
      logical :: success
      type(tEcNetCDF), intent(inout) :: netcdf

      integer :: ierr

      success = .false.

      ierr = nf90_close(netcdf%id)
      if (ierr /= NF90_NOERR) then
         call setECMessage("ec_netcdf_timeseries::ecNetCDFFree: Unable to close file")
      end if

      if (allocated(netcdf%dimlen)) then
         deallocate (netcdf%dimlen)
      end if

      if (allocated(netcdf%standard_names)) then
         deallocate (netcdf%standard_names)
      end if

      if (allocated(netcdf%tsid)) then
         deallocate (netcdf%tsid)
      end if

      if (allocated(netcdf%vp)) then
         deallocate (netcdf%vp)
      end if

      success = .true.
   end function ecNetCDFFree

   ! =======================================================================
   !> Frees a 1D array of tEcFileReaderPtrs, after which the ptr is deallocated.
   function ecNetCDFFree1dArray(ptr, n_nc_objs) result(success)
      logical :: success
      type(tEcNetCDFPtr), dimension(:), pointer, intent(inout) :: ptr
      integer, intent(inout) :: n_nc_objs !< number of NetCDF objects

      integer :: i
      integer :: istat

      success = .true.

      if (associated(ptr)) then
         ! Free and deallocate all tEcFileReaderPtrs in the 1d array.
         do i = 1, n_nc_objs
            if (ecNetCDFFree(ptr(i)%ptr)) then
               deallocate (ptr(i)%ptr, stat=istat)
               if (istat /= 0) then
                  success = .false.
               end if
            else
               success = .false.
            end if
         end do

         ! Finally deallocate the tEcNetCDFPtr(:) pointer.
         if (success) then
            deallocate (ptr, stat=istat)
            if (istat /= 0) then
               success = .false.
            end if
         end if
      end if

      n_nc_objs = 0
   end function ecNetCDFFree1dArray

   ! =======================================================================
   !> Initialize NetCDF instance
   !! Open a netCDF file, store ncid, standard names and long names ....
   !! Open only if this file is not already opened, so check the list of nc-objects first and return a pointer ....
   function ecNetCDFInit(ncname, ncptr, iostat) result(success)
      logical :: success
      character(len=*), intent(in) :: ncname
      type(tEcNetCDF), pointer, intent(inout) :: ncptr
      integer, optional, intent(out) :: iostat

      logical :: station_id_found
      logical :: time_found
      character(len=nf90_max_name) :: name
      character(len=:), allocatable :: cf_role, positive, zunits
      integer :: i_dim, n_dims, iVars, iTims, nVars, nTims, nGlobalAtts, unlimdimid, ierr
      integer :: tslen
      integer :: dimids_tsid(2)
      integer :: len_vectordef
      logical :: isVector
      integer, dimension(:, :), allocatable :: var_dimids
      integer, dimension(:), allocatable :: var_ndims

      success = .false.
      station_id_found = .false.
      time_found = .false.
      allocate (character(len=0) :: cf_role)
      allocate (character(len=0) :: positive)
      allocate (character(len=0) :: zunits)

      ierr = nf90_open(trim(ncname), NF90_NOWRITE, ncptr%ncid)
      if (ierr /= NF90_NOERR) then
         call setECmessage("ec_netcdf_timeseries::ecNetCDFInit: Error opening "//trim(ncname))
         return
      end if

      ierr = nf90_inquire(ncptr%ncid, n_dims, nVars, nGlobalAtts, unlimdimid)
      ncptr%nDIms = n_dims
      ncptr%nVars = nVars
      allocate (ncptr%dimlen(n_dims))

      ncptr%ncfilename = ncname
      iostat = 0 ! not yet used, placeholder
      do i_dim = 1, n_dims
         ierr = nf90_inquire_dimension(ncptr%ncid, i_dim, name, ncptr%dimlen(i_dim))
      end do

      allocate (ncptr%standard_names(nVars), stat=ierr)
      if (ierr /= NF90_NOERR) then 
         return
      end if

      allocate (ncptr%long_names(nVars), stat=ierr)
      if (ierr /= NF90_NOERR) then
         return
      end if

      allocate (ncptr%variable_names(nVars), stat=ierr)
      if (ierr /= NF90_NOERR) then
         return
      end if

      allocate (ncptr%vector_definitions(nVars), stat=ierr)
      if (ierr /= NF90_NOERR) then
         return
      end if

      allocate (ncptr%fillvalues(nVars), stat=ierr)
      if (ierr /= NF90_NOERR) then
         return
      end if

      allocate (ncptr%scales(nVars), stat=ierr)
      if (ierr /= NF90_NOERR) then 
         return
      end if

      allocate (ncptr%offsets(nVars), stat=ierr)
      if (ierr /= NF90_NOERR) then
         return
      end if

      ncptr%standard_names = ' '
      ncptr%long_names = ' '
      ncptr%variable_names = ' '
      ncptr%fillvalues = -huge(dp)
      ncptr%scales = 1.0_dp
      ncptr%offsets = 0.0_dp

      allocate (var_dimids(n_dims, nVars), stat=ierr) ! NOTE: n_dims is only an upper bound here!
      if (ierr /= NF90_NOERR) then 
         return
      end if

      allocate (var_ndims(nVars), stat=ierr)
      if (ierr /= NF90_NOERR) then 
         return
      end if

      var_ndims = 0
      do iVars = 1, nVars ! Inventorize variables
         ierr = nf90_inquire_attribute(ncptr%ncid, iVars, 'vector', len=len_vectordef) ! Check if this variable is just a reference to vector
         if (ierr == NF90_NOERR) then
            isVector = .true.
            allocate (character(len=len_vectordef) :: ncptr%vector_definitions(iVars)%s, stat=ierr)
            if (ierr /= NF90_NOERR) then 
               return
            end if
            ierr = nf90_get_att(ncptr%ncid, iVars, 'vector', ncptr%vector_definitions(iVars)%s)
         else
            isVector = .false.
         end if

         ierr = nf90_inquire_variable(ncptr%ncid, iVars, name=ncptr%variable_names(iVars)) ! Variable name
         ierr = nf90_get_att(ncptr%ncid, iVars, 'standard_name', ncptr%standard_names(iVars)) ! Standard name if available
         if (ierr /= NF90_NOERR) then
            ncptr%standard_names(iVars) = ncptr%variable_names(iVars) ! Variable name as fallback for standard_name
         end if
         ierr = nf90_get_att(ncptr%ncid, iVars, 'long_name', ncptr%long_names(iVars)) ! Long name for non CF names
         ierr = nf90_get_att(ncptr%ncid, iVars, '_FillValue', ncptr%fillvalues(iVars))
         if (ierr /= NF90_NOERR) then
            ncptr%fillvalues(iVars) = -huge(dp)
         end if

         ierr = nf90_get_att(ncptr%ncid, iVars, 'scale_factor', ncptr%scales(iVars))
         if (ierr /= NF90_NOERR) then
            ncptr%scales(iVars) = 1.0_dp
         end if

         ierr = nf90_get_att(ncptr%ncid, iVars, 'add_offset', ncptr%offsets(iVars))
         if (ierr /= NF90_NOERR) then
            ncptr%offsets(iVars) = 0.0_dp
         end if
         ierr = nf90_inquire_variable(ncptr%ncid, iVars, ndims=var_ndims(iVars), dimids=var_dimids(:, iVars))

         if (isVector) then
            cycle ! vector placeholder, not a real variable with data in the file
         end if

         ! Check for important var: was it the stations?
         cf_role = ''
         ierr = ncu_get_att(ncptr%ncid, iVars, 'cf_role', cf_role)
         if (cf_role == 'timeseries_id') then ! Multiple variables might have cf_role=timeseries_id
            if (var_ndims(iVars)==2 .and. ncptr%variable_names(iVars)=='station_id') then  ! Check dims and var_name
               ! Compose an index timeseries id's 
               ierr = nf90_inquire_variable(ncptr%ncid, iVars, dimids=dimids_tsid)
               if (ierr /= NF90_NOERR) then 
                  return
               end if

               tslen = ncptr%dimlen(dimids_tsid(1)) ! timeseries ID length
               nTims = ncptr%dimlen(dimids_tsid(2)) ! number of timeseries IDs
               ncptr%nTims = nTims
               allocate (ncptr%tsid(nTims), stat=ierr)
               if (ierr /= NF90_NOERR) then 
                  return
               end if

               tslen = min(tslen, len(ncptr%tsid(1)))
               ncptr%tsid = ''
               do iTims = 1, nTims
                  ierr = nf90_get_var(ncptr%ncid, iVars, ncptr%tsid(iTims), (/1, iTims/), (/tslen, 1/))
                  if (ierr /= NF90_NOERR) then
                     return
                  end if
                  call replace_char(ncptr%tsid(iTims), 0, 32) ! Replace NULL char by whitespace: iachar(' ') == 32
               end do
               ncptr%tsidvarid = iVars ! For convenience also store the Station ID explicitly
               ncptr%tsiddimid = dimids_tsid(2) ! For convenience also store the Station's dimension ID explicitly
               station_id_found = .true.
            end if
         end if

         ! Check for important var: was it time?
         if (ncptr%standard_names(iVars) == 'time') then ! Multiple variables might have standard_name "time"
            ierr = nf90_inquire_dimension(ncptr%ncid, var_dimids(1,iVars), name = name)
            if (var_ndims(iVars) == 1 .and. name == 'time') then ! ndims must be 1 and dimName must be "time"
               ierr = nf90_get_att(ncptr%ncid, iVars, 'units', ncptr%timeunit) ! Store the unit string of the time variable
               if (ierr /= NF90_NOERR) then
                  return
               end if

               ncptr%timevarid = iVars ! For convenience also store the ID explicitly
               ncptr%timedimid = var_dimids(1, iVars)
               time_found = .true.
            end if
         end if

         ! Check for important var: was it vertical layering?
         positive = ''
         zunits = ''
         ! Dirty workaround line below for UNST-9376: disabled 3D support, will be properly fixed in a later issue
         ! ierr = ncu_get_att(ncptr%ncid, iVars, 'positive', positive)
         if (len_trim(positive) > 0) then ! Identified a layercoord variable, by its positive:up/down attribute
            ! NOTE: officially, a vertical coord var may also be identified by a unit of pressure, but we don't support that here.
            ncptr%layervarid = iVars
            ncptr%layerdimid = var_dimids(1, iVars) ! For convenience also store the dimension ID explicitly
            ncptr%nLayer = ncptr%dimlen(ncptr%layerdimid)

            allocate (ncptr%vp(ncptr%nLayer), stat=ierr)
            if (ierr /= NF90_NOERR) then
               return
            end if

            ierr = nf90_get_var(ncptr%ncid, ncptr%layervarid, ncptr%vp, [1], [ncptr%nLayer])
            if (ierr /= NF90_NOERR) then
               return
            end if

            ierr = ncu_get_att(ncptr%ncid, iVars, 'units', zunits)
            if (ierr /= NF90_NOERR) then
               return
            end if

            if (strcmpi(zunits, 'm')) then
               if (strcmpi(positive, 'up')) ncptr%vptyp = BC_VPTYP_ZDATUM ! z upward from datum, unmodified z-values
               if (strcmpi(positive, 'down')) ncptr%vptyp = BC_VPTYP_ZSURF ! z downward
            else
               if (strcmpi(positive, 'up')) ncptr%vptyp = BC_VPTYP_PERCBED ! sigma upward
               if (strcmpi(positive, 'down')) ncptr%vptyp = BC_VPTYP_PERCSURF ! sigma downward
            end if

            if (ncptr%vptyp < 1) then
               call setECMessage("ec_bcreader::ecNetCDFCreate: Unable to determine vertical coordinate system.")
            end if
         end if
      end do

      deallocate (cf_role)
      deallocate (positive)
      deallocate (zunits)

      if (station_id_found .and. time_found) then
         success = .true.
      end if
   end function ecNetCDFInit

   ! =======================================================================
   !> Scan netcdf instance for a specific quantity name and location label
   recursive function ecNetCDFScan(ncptr, quantity, location, q_id, l_id, dimids, vectormax) result(success)
      logical :: success
      type(tEcNetCDF), pointer, intent(inout) :: ncptr !< NetCDF instance
      character(len=*), intent(in) :: quantity !< Quantity to search for
      character(len=*), intent(in) :: location !< Location label to search for
      integer, dimension(:), allocatable, intent(inout) :: q_id !< Awkward recursive construction, using q_id(1), forcing the calling routine to allocate it
      integer, intent(out) :: l_id !< Location id found
      integer, dimension(:), allocatable, intent(out) :: dimids !< Dimension ids of the found quantity
      integer, intent(out), optional :: vectormax !< Maximum vector size if quantity is vectorial

      character(len=NF90_MAX_NAME), dimension(:), allocatable :: ncstdnames !< list with standard names to be filled
      character(len=NF90_MAX_NAME), dimension(:), allocatable :: ncvarnames !< list with variable names to be filled
      character(len=NF90_MAX_NAME), dimension(:), allocatable :: ncstdnames_fallback !< list with fallback standard names to be filled
      character(len=NF90_MAX_NAME) :: quantity_candidate !< quantity candidate name during search
      integer :: n_dims
      integer :: ivar
      integer :: jvar
      integer :: itim
      integer :: ltl 
      integer :: iv
      integer :: ierr
      integer :: vmax
      character(len=30), dimension(:), allocatable :: elmnames
      integer, dimension(:), allocatable :: dimids_check

      success = .false.
      vmax = 1

      ! get candidate names for the quantity
      call ecSupportNetcdfGetQuantityCandidateNames(ncptr%ncfilename, quantity, ncstdnames, ncvarnames, ncstdnames_fallback)
      
      ! search for standard_name
      if (allocated(ncptr%standard_names) .and. allocated(ncstdnames)) then
         ivar = find_candidates_in_targets(ncptr%standard_names, ncstdnames)
      end if

      ! if standard_name not found, search for long_name
      if (ivar < 0 .and. allocated(ncptr%long_names) .and. allocated(ncstdnames_fallback)) then
         ivar = find_candidates_in_targets(ncptr%long_names, ncstdnames_fallback)
      end if

      ! if also long_name not found, search for variable_name
      if (ivar < 0 .and. allocated(ncptr%variable_names) .and. allocated(ncvarnames)) then
         ivar = find_candidates_in_targets(ncptr%variable_names, ncvarnames)
      end if

      ! If quantity is found in (std,long,var)_names, set q_id and check if quantity is a vector
      if (ivar > 0) then
         q_id(1) = ivar

         ! Check if quantity is a vector
         if (allocated(ncptr%vector_definitions(ivar)%s)) then
            ! Get vector element names
            call strsplit(ncptr%vector_definitions(ivar)%s, 1, elmnames, 1, sep=",")
            vmax = size(elmnames)

            ! Reallocate q_id to the correct size
            if (allocated(q_id)) then 
               deallocate (q_id)
            end if
            allocate (q_id(vmax), stat=ierr)
            if (ierr /= 0) then 
               return
            end if

            ! Scan for all vector element variable ids
            do iv = vmax, 1, -1
               if (.not. ecNetCDFScan(ncptr, trim(elmnames(iv)), location, q_id, l_id, dimids)) then
                  return
               end if
               q_id(iv) = q_id(1)
            end do
         end if
      else
         ! If quantity not found, give error message and set q_id to -1
         call setECMessage("ec_netcdf_timeseries::ecNetCDFScan: Quantity '"//trim(quantity)//"' not found in file '"//trim(ncptr%ncfilename)//"'.")
         q_id(1) = -1
      end if

      do itim = 1, ncptr%nTims
         ltl = len_trim(location)
         if (strcmpi(ncptr%tsid(itim), location)) exit ! Found
      end do

      if (itim <= ncptr%nTims) then
         l_id = itim
      else
         l_id = -1
      end if

      if (l_id <= 0 .or. q_id(1) <= 0) then
         return ! l_id<0 means : location not found, q_id<0 means quantity not found
      end if

      ierr = nf90_Inquire_Variable(ncptr%ncid, q_id(1), ndims=n_dims)
      if (ierr /= NF90_NOERR) then
         return
      end if

      if (.not. allocated(dimids)) then
         allocate (dimids(n_dims), stat=ierr)
         if (ierr /= 0) then
            return
         end if
         
         ierr = nf90_Inquire_Variable(ncptr%ncid, q_id(1), dimids=dimids)
         if (ierr /= NF90_NOERR) then
            return
         end if
      else
         allocate (dimids_check(n_dims), stat=ierr)
         if (ierr /= NF90_NOERR) then
            return
         end if

         ierr = nf90_Inquire_Variable(ncptr%ncid, q_id(1), dimids=dimids_check)
         if (ierr /= NF90_NOERR) then
            return
         end if

         if (.not. (all(dimids == dimids_check) .and. n_dims == size(dimids))) then
            ! sanity check: all elements should have the same dimensions vector
            return ! unsuccessfully
         end if
      end if

      !  TODO: Retrieve relevant variable attributes and store them in the bc-instance this netcdf is connected to

      if (present(vectormax)) then
         vectormax = vmax
      end if

      ! Set success to true if we reached this point
      success = .true.
   end function ecNetCDFScan

   ! =======================================================================
   !> Reader of timeseries to be implemented here ....
   function ecNetCDFGetTimeseriesValue(ncptr, q_id, l_id, dims, timelevel, nctime, ncvalue, buffer) result(success)
      logical :: success
      type(tEcNetCDF), pointer, intent(inout) :: ncptr
      integer, dimension(:), intent(in) :: q_id
      integer, intent(in) :: l_id
      integer, dimension(:), intent(in) :: dims
      integer, intent(in) :: timelevel
      real(dp), dimension(:), intent(out) :: nctime
      real(dp), dimension(:), intent(out) :: ncvalue
      real(dp), dimension(:), intent(inout), allocatable :: buffer

      integer :: vectormax
      integer :: iv
      integer :: il
      integer :: ierr

      success = .false.
      vectormax = size(q_id)

      if (ncptr%nLayer < 0) then ! no 3rd dimension, get single data value, maybe should be <=0
         do iv = 1, vectormax
            ierr = nf90_get_var(ncptr%ncid, q_id(iv), ncvalue(iv:iv), (/l_id, timelevel/), (/1, 1/))
            if (ierr /= NF90_NOERR) then
               return
            end if
         end do
      else ! yes 3rd dimension, get a rank-1 vector, num. of layers
         call realloc(buffer, ncptr%nLayer)
         do iv = 1, vectormax
            ierr = nf90_get_var(ncptr%ncid, q_id(iv), buffer(1:ncptr%nLayer), (/1, l_id, timelevel/), (/ncptr%nLayer, 1, 1/))
            if (ierr /= NF90_NOERR) then
               return
            end if
            do il = 1, ncptr%nLayer
               ncvalue(iv + (il - 1) * vectormax) = buffer(il) ! interleaving three quantities from netCDF into one array
            end do
         end do
      end if

      ierr = nf90_get_var(ncptr%ncid, ncptr%timevarid, nctime(1:1), (/timelevel/), (/1/)) ! get one single time value
      if (ierr /= NF90_NOERR) then
         return
      end if

      success = .true.
   end function ecNetCDFGetTimeseriesValue

   ! =======================================================================
   !> Scan netcdf instance for a specific quantity name and location label
   !> Retrieve an arbitrary attrib from the variable's metadata (returns attribute string)
   function ecNetCDFGetAttrib(ncptr, q_id, attribute_name, attribute_value) result(success)
      logical :: success
      type(tEcNetCDF), pointer, intent(inout) :: ncptr
      integer, intent(in) :: q_id
      character(len=*), intent(in) :: attribute_name
      character(len=*), intent(out) :: attribute_value

      integer :: ierr

      success = .false.

      ierr = nf90_get_att(ncptr%ncid, q_id, trim(attribute_name), attribute_value)
      if (ierr /= NF90_NOERR) then
         return
      end if

      success = .true.
   end function ecNetCDFGetAttrib

end module m_ec_netcdf_timeseries
