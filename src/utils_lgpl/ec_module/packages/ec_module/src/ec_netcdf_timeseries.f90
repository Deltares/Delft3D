!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2023.
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


module m_ec_netcdf_timeseries
    use precision 
    use m_ec_parameters
    use m_ec_support
    use m_ec_message
    use m_ec_typedefs
    use netcdf
    use m_alloc 
    use multi_file_io
    use string_module
    implicit none 
          
    private 

    public   :: ecNetCDFCreate
    public   :: ecNetCDFInit
    public   :: ecNetCDFFree
    public   :: ecNetCDFFree1dArray
    public   :: ecNetCDFScan
    public   :: ecNetCDFGetTimeseriesValue
    public   :: ecNetCDFGetAttrib
    public   :: ecNetCDFGetVectormax

   contains 

    !> Construct a new netCDF instance with the specified id.
    !! Failure is indicated by returning a null pointer.
    function ecNetCDFCreate(netCDFId) result(netCDFPtr)
       type(tEcNetCDF), pointer            :: netCDFPtr !< the new netCDF instance, intent(out)
       integer,                 intent(in) :: netCDFId  !< unique netCDF instance id
       !
       integer :: istat   !< allocate() status
       logical :: success !< helper variable
       !
       success = .false.
       !
       ! allocation
       allocate(netCDFPtr, stat = istat)
       if (istat == 0) then
          ! see what's allocatable in this object and add it here 
          ! ...... 
       end if
       if (istat /= 0) then
          call setECMessage("ec_netcdf_timeseries::ecNetCDFCreate: Unable to allocate additional memory.")
          netCDFPtr => null()
          return
       end if
       ! initialization
       netCDFPtr%id = netCDFId
    end function ecNetCDFCreate

    ! NetCDF (for timeseries) destructor 
    function ecNetCDFFree(netcdf) result(success)
       implicit none
       logical                         :: success !< function status
       type(tEcNetCDF), intent(inout)  :: netcdf  !< intent(inout)
       integer                         :: ierr 
    
       success = .False.
       ierr = nf90_close(netcdf%id)
       if (ierr/=NF90_NOERR) then 
          continue
          ! TODO: issue a warning here 
       endif 
       if (allocated(netcdf%dimlen)) then      
          deallocate(netcdf%dimlen)
       endif 
       if (allocated(netcdf%standard_names)) then      
          deallocate(netcdf%standard_names)
       endif 
       if (allocated(netcdf%tsid)) then      
          deallocate(netcdf%tsid)
       endif 
       if (allocated(netcdf%vp)) then      
          deallocate(netcdf%vp)
       endif 

       success = .True.
    end function ecNetCDFFree
    
    ! =======================================================================
    
    !> Frees a 1D array of tEcFileReaderPtrs, after which the ptr is deallocated.
    function ecNetCDFFree1dArray(ptr, nNetCDFs) result (success)
       logical                                       :: success      !< function status
       type(tEcNetCDFPtr), dimension(:), pointer     :: ptr          !< intent(inout)
       integer, intent(inout)                        :: nNetCDFs     !< number of NetCDF objects
       !
       integer :: i      !< loop counter
       integer :: istat  !< deallocate() status
       !
       success = .true.
       !
       if (associated(ptr)) then
          ! Free and deallocate all tEcFileReaderPtrs in the 1d array.
          do i=1, nNetCDFs
             if (ecNetCDFFree(ptr(i)%ptr)) then
                deallocate(ptr(i)%ptr, stat = istat)
                if (istat /= 0) success = .false.
             else
                success = .false.
             end if
          end do
          ! Finally deallocate the tEcNetCDFPtr(:) pointer.
          if (success) then
             deallocate(ptr, stat = istat)
             if (istat /= 0) success = .false.
          end if
       end if
       nNetCDFs = 0
    end function ecNetCDFFree1dArray


    !> Initialize NetCDF instance
    function ecNetCDFInit (ncname, ncptr, iostat) result (success)
    use string_module
    use io_ugrid
    use netcdf_utils, only: ncu_get_att
    
    implicit none
!   Open a netCDF file, store ncid, standard names and long names ....
!   Open only if this file is not already opened, so check the list of nc-objects first and return a pointer ....
    logical                                        :: success
    character(len=*),              intent(in)      :: ncname
    type (tEcNetCDF),              pointer         :: ncptr
    integer, optional,             intent(out)     :: iostat
    character(len=nf90_max_name)                   :: name
    character(len=:), allocatable                  :: cf_role, positive, zunits

    integer    :: iDims, nDims, iVars, iTims, nVars, nTims, nGlobalAtts, unlimdimid, ierr 
    integer    :: tslen
    integer    :: dimids_tsid(2)
    integer, allocatable :: var_dimids(:,:)
    integer, allocatable :: var_ndims(:)
    
    success = .false.
    allocate(character(len=0) :: cf_role)
    allocate(character(len=0) :: positive)
    allocate(character(len=0) :: zunits)

    ierr = nf90_open(trim(ncname), NF90_NOWRITE, ncptr%ncid)
    if (ierr /= 0) then
        call setECmessage("Error opening " // trim(ncname))
        return
    endif
    ierr = nf90_inquire(ncptr%ncid, nDims, nVars, nGlobalAtts, unlimdimid)
    ncptr%nDIms = nDims
    ncptr%nVars = nVars
    allocate (ncptr%dimlen(nDims))

    ncptr%ncfilename =  ncname 
    iostat = 0                                                                              ! not yet used, placeholder 
    do iDims = 1, nDims
       ierr = nf90_inquire_dimension(ncptr%ncid, iDims, name, ncptr%dimlen(iDims))
    enddo
    allocate (ncptr%standard_names(nVars))
    allocate (ncptr%long_names(nVars))
    allocate (ncptr%variable_names(nVars))
    allocate (ncptr%fillvalues(nVars))
    allocate (ncptr%scales(nVars))
    allocate (ncptr%offsets(nVars))
    ncptr%standard_names = ' '
    ncptr%long_names = ' '
    ncptr%variable_names = ' '
    ncptr%fillvalues = -huge(hp)
    ncptr%scales     = 1.0_hp
    ncptr%offsets    = 0.0_hp
    allocate(var_dimids(nDims, nVars)) ! NOTE: nDims is only an upper bound here!
    allocate(var_ndims(nVars))
    var_ndims = 0
    do iVars = 1, nVars                                                                     ! Inventorize variables
       ierr = nf90_inquire_variable(ncptr%ncid,iVars,name=ncptr%variable_names(iVars))      ! Variable name
       ierr = nf90_get_att(ncptr%ncid,iVars,'standard_name',ncptr%standard_names(iVars))    ! Standard name if available
       if (ierr /= 0) ncptr%standard_names(iVars) = ncptr%variable_names(iVars)             ! Variable name as fallback for standard_name
       ierr = nf90_get_att(ncptr%ncid,iVars,'long_name',ncptr%long_names(iVars))            ! Long name for non CF names

       ierr = nf90_get_att(ncptr%ncid,iVars,'_FillValue',ncptr%fillvalues(iVars))
       if (ierr/=NF90_NOERR)   ncptr%fillvalues(iVars) = -huge(hp)
       ierr = nf90_get_att(ncptr%ncid,iVars,'scale_factor',ncptr%scales(iVars))
       if (ierr/=NF90_NOERR)   ncptr%scales(iVars) = 1.0_hp
       ierr = nf90_get_att(ncptr%ncid,iVars,'add_offset',ncptr%offsets(iVars))
       if (ierr/=NF90_NOERR)   ncptr%offsets(iVars) = 0.0_hp
       ierr = nf90_inquire_variable(ncptr%ncid,iVars,ndims=var_ndims(iVars),dimids=var_dimids(:,iVars))

       ! Check for important var: was it the stations?
       cf_role = ''
       ierr = ncu_get_att(ncptr%ncid,iVars,'cf_role',cf_role)
       if (cf_role == 'timeseries_id') then 
             nDims = 0                
             ierr = nf90_inquire_variable(ncptr%ncid, iVars, ndims = nDims)
             if (nDims==2) then                                                             ! If cf Role 'timeseries_id' found, compose an index timeseries id's 
                ierr = nf90_inquire_variable(ncptr%ncid, iVars, dimids = dimids_tsid)
                tslen = ncptr%dimlen(dimids_tsid(1))                                        ! timeseries ID length 
                nTims = ncptr%dimlen(dimids_tsid(2))                                        ! number of timeseries IDs  
                ncptr%nTims = nTims
                allocate (ncptr%tsid(nTims))
                tslen = min(tslen,len(ncptr%tsid(1)))
                ncptr%tsid = ''
                do iTims=1,nTims
                   ierr = nf90_get_var(ncptr%ncid, iVars, ncptr%tsid(iTims), (/1,iTims/),(/tslen,1/)) 
                   call replace_char(ncptr%tsid(iTims), 0, 32) ! Replace NULL char by whitespace: iachar(' ') == 32
                end do
                ncptr%tsidvarid  = iVars                                                       ! For convenience also store the Station ID explicitly 
                ncptr%tsiddimid = dimids_tsid(2)                                            ! For convenience also store the Station's dimension ID explicitly 
             else 
                ! timeseries_id has the wrong dimensionality
                return  
             endif 
       end if

       ! Check for important var: was it time?
       if (ncptr%standard_names(iVars) == 'time') then
          ierr = nf90_get_att(ncptr%ncid,iVars,'units',ncptr%timeunit)                     ! Store the unit string of the time variable 
          ncptr%timevarid    = iVars                                                       ! For convenience also store the ID explicitly 
          ncptr%timedimid = var_dimids(1,iVars)
       endif 

       ! Check for important var: was it vertical layering?
       positive = ''
       zunits = ''
       ierr = ncu_get_att(ncptr%ncid,iVars,'positive',positive)
       if (len_trim(positive) > 0) then ! Identified a layercoord variable, by its positive:up/down attribute
          ! NOTE: officially, a vertical coord var may also be identified by a unit of pressure, but we don't support that here.
          ncptr%layervarid = iVars
          ncptr%layerdimid = var_dimids(1,iVars)                                          ! For convenience also store the dimension ID explicitly 
          ncptr%nLayer = ncptr%dimlen(ncptr%layerdimid)
          allocate(ncptr%vp(ncptr%nLayer))
          ierr = nf90_get_var(ncptr%ncid,ncptr%layervarid,ncptr%vp,(/1/),(/ncptr%nLayer/))
          ierr = ncu_get_att(ncptr%ncid,iVars,'units',zunits)
          if (strcmpi(zunits,'m')) then
             if (strcmpi(positive,'up'))   ncptr%vptyp=BC_VPTYP_ZDATUM         ! z upward from datum, unmodified z-values 
             if (strcmpi(positive,'down')) ncptr%vptyp=BC_VPTYP_ZSURF          ! z downward
          else
             if (strcmpi(positive,'up'))   ncptr%vptyp=BC_VPTYP_PERCBED        ! sigma upward
             if (strcmpi(positive,'down')) ncptr%vptyp=BC_VPTYP_PERCSURF       ! sigma downward
          end if
          if (ncptr%vptyp<1) then
             call setECMessage("ec_bcreader::ecNetCDFCreate: Unable to determine vertical coordinate system.")
          end if
       end if
    enddo 

    !VECTORMAX SUPPORT WILL BE IMPLEMENTED LATER 
    !vectormax = 1 ! default
    !do iVars = 1, nVars                                                                     ! Inventorize variables 
    !   ierr = nf90_inquire_variable(ncptr%ncid,iVars,ndims=var_ndims, dimids=var_dimids)
    !   n = var_ndims(iVar)
    !   if (n <= 1) then
    !      cycle ! Timeseries should at least have the station *and* time dimension
    !   end if
    !
    !   if (var_dimids(n,iVars) == ncptr%ts_dimid) then        ! Check 1: This var is defined on the stations
    !      if (var_dimids(n-1,iVars) == ncptr%time_dimid) then ! Check 2: this var is also defined in the time dimension
    !         if (n >= 3) then
    !            if (var_dimids(n-2,iVars) == ncptr%layer_dimid) then ! Check 3: This vasr is also defined in the layering dimension
    !               idimvectcandidate = n-3
    !            else
    !               idimvectcandidate = n-2
    !            end if
    !            
    !            if (idimvectcandidate >= 1) then
    !               ! This remaining dimension is then by default interpreted as a vectormax dimension for this var.
    !               var_vectormax(iVar) = ncptr%dimlen(idimvectcandidate) ! This var has a third dimension, must be the vectormax
    !            end if
    !            ! TODO: warning if var contains even more dimensions (var_ndims>4)
    !         end if ! n>=3 dim
    !      end if ! time dim
    !   end if ! station dim
    !enddo 
    deallocate(cf_role)
    deallocate(positive)
    deallocate(zunits)
    
    success = .True.
    end function ecNetCDFInit

    !> Scan netcdf instance for a specific quantity name and location label 
    function ecNetCDFScan (ncptr, quantity, location, q_id, l_id, dimids) result (success)
    use string_module
    implicit none
    logical                          :: success
    type (tEcNetCDF),   pointer      :: ncptr
    character(len=*),   intent(in)   :: quantity
    character(len=*),   intent(in)   :: location
    integer, intent(out)             :: q_id
    integer, intent(out)             :: l_id
    integer, dimension(:), allocatable, intent(out) :: dimids
    integer    :: ndims
    integer    :: ivar, itim, ltl
    integer    :: ierr

    ! initialization
    success = .False.

    ! search for standard_name
    do ivar=1, ncptr%nVars
       ltl = len_trim(quantity)
       if (strcmpi(ncptr%standard_names(ivar), quantity, ltl)) exit
    enddo

    ! if standard_name not found, search for long_name
    if (ivar > ncptr%nVars) then
       do ivar=1, ncptr%nVars
          ltl = len_trim(quantity)
          if (strcmpi(ncptr%long_names(ivar), quantity, ltl)) exit
       enddo
    endif

    ! if also long_name not found, search for variable_name
    if (ivar > ncptr%nVars .and. allocated(ncptr%variable_names)) then
       do ivar=1, ncptr%nVars
          ltl = len_trim(quantity)
          if (strcmpi(ncptr%variable_names(ivar), quantity, ltl)) exit
       enddo
    endif

    if (ivar <= ncptr%nVars) then
       q_id = ivar
    else
       call setECMessage("Quantity '"//trim(quantity)//"' not found in file '"//trim(ncptr%ncfilename)//"'.")
       q_id = -1 
    endif 
    do itim=1,ncptr%nTims
       ltl = len_trim(location)
       if (strcmpi(ncptr%tsid(itim), location)) exit ! Found
    enddo 
    if (itim<=ncptr%nTims) then 
       l_id = itim
    else 
       l_id = -1 
    endif 
    if (l_id<=0 .or. q_id<=0) then 
       return                 ! l_id<0 means : location not found, q_id<0 means quantity not found 
    endif  
    ierr = nf90_Inquire_Variable(ncptr%ncid, q_id, ndims=ndims)
    allocate(dimids(ndims))
    ierr = nf90_Inquire_Variable(ncptr%ncid, q_id, dimids=dimids)
    !  TODO: Retrieve relevant variable attributes and store them in the bc-instance this netcdf is connected to  

    success = .True. 
    end function ecNetCDFScan
   
    ! Reader of timeseries to be implemented here .... 
    function ecNetCDFGetTimeseriesValue (ncptr, q_id, l_id, dims, timelevel, nctime, ncvalue) result (success)   
    logical                          :: success
    type (tEcNetCDF),   pointer      :: ncptr              
    integer, intent(in)              :: q_id
    integer, intent(in)              :: l_id
    integer, intent(in), dimension(:):: dims
    integer, intent(in)              :: timelevel 
    integer                          :: ierr           
!   double precision, intent(out), dimension (:)   :: nctime 
!   double precision, intent(out), dimension (:)   :: ncvalue 
    real(hp), intent(out), dimension (:)   :: nctime 
    real(hp), intent(out), dimension (:)   :: ncvalue 
    
    success = .False.
    if (ncptr%nLayer<0) then           ! no 3rd dimension, get single data value, maybe should be <=0
       ierr = nf90_get_var(ncptr%ncid,q_id,ncvalue(1:1),(/l_id,timelevel/),(/1,1/))  
    else                               ! yes 3rd dimension, get a rank-1 vector, num. of layers
       ierr = nf90_get_var(ncptr%ncid,q_id,ncvalue(1:ncptr%nLayer),(/1,l_id,timelevel/),(/ncptr%nLayer,1,1/))          
     end if
    if (ierr/=NF90_NOERR) return 
    ierr = nf90_get_var(ncptr%ncid,ncptr%timevarid,nctime(1:1),(/timelevel/),(/1/))    ! get one single time value 
    if (ierr/=NF90_NOERR) return 
    success = .True.
    end function ecNetCDFGetTimeseriesValue

    !> Acquire the vectormax, vector dimensionality, from the variable's metadata (attribute:vectormax)
    !> RL: Currently, vectormax in netcdf-timeseries is not used, one of the reasons being that vectors
    !>     are traditionally stored differently in netcdf: each component is a variable, rather than
    !>     interpreting the vector dimensionality as a dimension in netcdf .... 
    !>     This should be dealt with analogous to the ascii-bc format: a string attribute named 'vector'
    !>     with a list of names of variables to be packed into a vector.
    function ecNetCDFGetVectormax (ncptr, q_id, vectormax) result (success)   
    use netcdf_utils, only: ncu_get_att
    use io_ugrid
    
    implicit none
    logical                          :: success 
    type (tEcNetCDF),   pointer      :: ncptr              
    integer, intent(in)              :: q_id
    integer, intent(out)             :: vectormax 
    integer                          :: ierr 
    
    character(len=:), allocatable   :: str
    
    allocate(character(len=0) :: str)
    success = .False. 
    str = ''
    ierr = ncu_get_att(ncptr%ncid,q_id,'vectormax',str)
    if (ierr/=NF90_NOERR) return 
    read(str,*,iostat=ierr) vectormax
    if (ierr/=0) return 
    deallocate(str)
    success = .True. 
    end function ecNetCDFGetVectormax

    !> Scan netcdf instance for a specific quantity name and location label 
    !> Retrieve an arbitrary attrib from the variable's metadata (returns attribute string)
    function ecNetCDFGetAttrib (ncptr, q_id, attribname, attribvalue) result (success)   
    implicit none
    logical                          :: success 
    type (tEcNetCDF),   pointer      :: ncptr              
    integer, intent(in)              :: q_id
    character(len=*), intent(in)     :: attribname
    character(len=*), intent(out)    :: attribvalue 
    integer                          :: ierr 
    
    success = .False. 
    ierr = nf90_get_att(ncptr%ncid,q_id,trim(attribname),attribvalue)
    if (ierr/=0) return 
    success = .True. 
    end function ecNetCDFGetAttrib
    
end module m_ec_netcdf_timeseries
