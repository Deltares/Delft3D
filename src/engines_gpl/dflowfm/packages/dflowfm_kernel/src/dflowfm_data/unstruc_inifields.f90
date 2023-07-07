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

!> Reading + initializing of initial and parameter fields.
!! The IniFieldFile from the MDU is the successor of the old
!! *.ext file for quantities such as initialwaterlevel,
!! frictioncoefficient, etc.
module unstruc_inifields

use unstruc_messages
use properties
use string_module, only: str_lower, strcmpi

implicit none
private ! Prevent used modules from being exported

public :: init1dField, initInitialFields, spaceInit1dField, readIniFieldProvider, checkIniFieldFileVersion

!> The file version number of the IniFieldFile format: d.dd, [config_major].[config_minor], e.g., 1.03
!!
!! Note: read config_minor as a 2 digit-number, i.e., 1.1 > 1.02 (since .1 === .10 > .02).
!! Convention for format version changes:
!! * if a new format is backwards compatible with old files, only
!!   the minor version number is incremented.
!! * if a new format is not backwards compatible (i.e., old files
!!   need to be converted/updated by user), then the major version number
!!   is incremented.

! IniFieldFile current version: 2.00
integer, parameter       :: IniFieldMajorVersion = 2
integer, parameter       :: IniFieldMinorVersion = 0

! History IniFieldFile versions:

! 2.00 (2019-06-18): Added LocationType and changed ExtrapolationMethod to yes/no value.
! 1.01 (2019-03-12): First version of *.ini type initial fields and parameters file.

contains

function checkIniFieldFileVersion(inifilename, inifield_ptr) result(ierr)
   use dfm_error
   character(len=*), intent(in   ) :: inifilename         !< name of initial field file, should already be opened in inifield_ptr.
   type(tree_data),  pointer       :: inifield_ptr        !< tree of inifield-file's [Initial] or [Parameter] blocks
   integer                         :: ierr                !< Result status (DFM_NOERR on success)

   integer :: major, minor
   logical :: success

   ierr = DFM_NOERR

   major = 0
   minor = 0
   call prop_get_version_number(inifield_ptr, major = major, minor = minor, success = success)
   if (.not. success .or. major < IniFieldMajorVersion) then
      write (msgbuf, '(a,i0,".",i2.2,a,i0,".",i2.2,a)') 'Unsupported IniFieldFile format detected in '''//trim(inifilename)//''': v', major, minor, '. Current format: v',IniFieldMajorVersion,IniFieldMinorVersion,'. Ignoring this file.'
      call warn_flush()
      ierr = DFM_EFILEFORMAT
   end if
end function checkIniFieldFileVersion


!> Reads and initializes an initial field file.
!! The IniFieldFile can contain multiple [Initial] and [Parameter] blocks
!! that specify the data provider details for initial conditions and
!! model parameters/coefficients.
function initInitialFields(inifilename) result(ierr)
   use tree_data_types
   use tree_structures
   use messageHandling
   use unstruc_files, only: resolvePath
   use system_utils
   use m_ec_interpolationsettings
   use m_flow, only: s1, hs, frcu, ifrcutp, ifrctypuni
   use m_flowgeom
   use m_wind ! |TODO: AvD: reduce amount of uses 
   use m_missing
   use timespace
   use unstruc_boundaries, only: prepare_lateral_mask
   use m_flowexternalforcings, only: qid, operand, transformcoef, success
   use network_data
   use m_alloc
   use dfm_error
   use m_hydrology_data, only:  infiltcap, infiltrationmodel, DFM_HYD_INFILT_CONST, &
                                HortonMinInfCap, HortonMaxInfCap, HortonDecreaseRate, HortonRecoveryRate, &
                                InterceptThickness, interceptionmodel, DFM_HYD_INTERCEPT_LAYER, jadhyd, &
                                PotEvap, ActEvap, InterceptHs

   implicit none
   character(len=*), intent(in   ) :: inifilename         !< name of initial field file
   integer                         :: ierr                !< Result status (DFM_NOERR on success)

   type(tree_data),  pointer       :: inifield_ptr        !< tree of inifield-file's [Initial] or [Parameter] blocks
   type(tree_data),  pointer       :: node_ptr
   integer                         :: istat
   integer, parameter              :: ini_key_len   = 32
   integer, parameter              :: ini_value_len = 256
   character(len=ini_key_len)      :: groupname
   character(len=ini_value_len)    :: varname
   integer                         :: num_items_in_file
   logical                         :: retVal
   character(len=255)              :: fnam, filename
   character(len=255)              :: basedir
   integer                         :: i, ib, L, iprimpos, kc_size_store, mx, k1, k2, ja
   integer, allocatable            :: kcc(:), kc1D(:), kc2D(:)
   integer :: method, iloctype, filetype, ierr_loc
   
   logical, external :: timespaceinitialfield_mpi
   
   ierr = DFM_NOERR
   success = .true.
   
   call mess(LEVEL_INFO, 'Reading initial field file '''//trim(inifilename)//'''.')
   
   call tree_create(trim(inifilename), inifield_ptr)
   call prop_file('ini',trim(inifilename),inifield_ptr,istat) 
   
   call split_filename(inifilename, basedir, fnam)
   ierr = checkIniFieldFileVersion(inifilename, inifield_ptr)
   if (ierr /= DFM_NOERR) then
      goto 888
   end if

   num_items_in_file = 0
   if (associated(inifield_ptr%child_nodes)) then
       num_items_in_file = size(inifield_ptr%child_nodes)
   endif


   ib = 0
   !! Now loop on each block
   do i=1,num_items_in_file
   
      node_ptr => inifield_ptr%child_nodes(i)%node_ptr
      !! Step 1: Read each block
      call readIniFieldProvider(inifilename, node_ptr,groupname,qid,filename,filetype,method,iloctype,operand,transformcoef,ja,varname) !,smask, maxSearchRadius)
      if (ja == 1) then
         call resolvePath(filename, basedir, filename)
         ib = ib + 1
      else
         cycle
      end if
      if ((.not. strcmpi(groupname, 'Initial')) .and. (.not. strcmpi(groupname, 'Parameter'))) then
         cycle
      end if

      !! Step 2: operation for each block
      if (filetype == field1D) then
         ierr_loc = init1dField(filename,inifilename, qid) ! todo: underneath timespaceinitial?
         if (ierr_loc /= DFM_NOERR) then
            success = .false.
            exit ! Or, consider cycle instead, to try all remaining blocks and return with an error only at the very end.
         end if
      else
         if (strcmpi(qid, 'waterlevel')) then
            call realloc(kcsini, ndx, keepExisting=.false.)
            call prepare_lateral_mask(kcsini, iLocType)
            
            success = timespaceinitialfield(xz, yz, s1, ndx, filename, filetype, method, operand, transformcoef, 2, kcsini) ! zie meteo module
         else if (strcmpi(qid, 'waterdepth')) then
            call realloc(kcsini, ndx, keepExisting=.false.)
            call prepare_lateral_mask(kcsini, iLocType)
            
            success = timespaceinitialfield(xz, yz, hs, ndx, filename, filetype, method, operand, transformcoef, 2, kcsini)
            s1(1:ndxi) = bl(1:ndxi) + hs(1:ndxi)         
         else if (strcmpi(qid, 'InfiltrationCapacity')) then
            if (infiltrationmodel /= DFM_HYD_INFILT_CONST) then
               write (msgbuf, '(a,i0,a)') 'File '''//trim(inifilename)//''' contains quantity '''//trim(qid)//'''. This requires ''InfiltrationModel=', DFM_HYD_INFILT_CONST, ''' in the MDU file (constant).'
               call warn_flush() ! No error, just warning and continue
            end if
            call realloc(kcsini, ndx, keepExisting=.false.)
            call prepare_lateral_mask(kcsini, iLocType)
            
            success = timespaceinitialfield(xz, yz, infiltcap, ndx, filename, filetype, method, operand, transformcoef, 2, kcsini)
            if (success) then
               where (infiltcap /= dmiss)
                  infiltcap = infiltcap*1d-3/(24d0*3600d0)            ! mm/day => m/s
               end where
            end if

         else if (strcmpi(qid, 'HortonMinInfCap')) then
            call realloc(kcsini, ndx, keepExisting=.false.)
            call prepare_lateral_mask(kcsini, iLocType)
            
            success = timespaceinitialfield(xz, yz, HortonMinInfCap, ndx, filename, filetype, method, operand, transformcoef, 2, kcsini)
         else if (strcmpi(qid, 'HortonMaxInfCap')) then
            call realloc(kcsini, ndx, keepExisting=.false.)
            call prepare_lateral_mask(kcsini, iLocType)
            
            success = timespaceinitialfield(xz, yz, HortonMaxInfCap, ndx, filename, filetype, method, operand, transformcoef, 2, kcsini)
         else if (strcmpi(qid, 'HortonDecreaseRate')) then
            call realloc(kcsini, ndx, keepExisting=.false.)
            call prepare_lateral_mask(kcsini, iLocType)
            
            success = timespaceinitialfield(xz, yz, HortonDecreaseRate, ndx, filename, filetype, method, operand, transformcoef, 2, kcsini)
         else if (strcmpi(qid, 'HortonRecoveryRate')) then
            call realloc(kcsini, ndx, keepExisting=.false.)
            call prepare_lateral_mask(kcsini, iLocType)
            
            success = timespaceinitialfield(xz, yz, HortonRecoveryRate, ndx, filename, filetype, method, operand, transformcoef, 2, kcsini)
         else if (strcmpi(qid, 'InterceptionLayerThickness')) then
            call realloc(kcsini, ndx, keepExisting=.false.)
            call prepare_lateral_mask(kcsini, iLocType)

            call realloc(InterceptThickness, ndx, keepExisting=.false.)
            success = timespaceinitialfield(xz, yz, InterceptThickness, ndx, filename, filetype, method, operand, transformcoef, 2, kcsini)
            if (success) then
               call realloc(InterceptHs,        ndx, keepExisting = .false., fill = 0d0)
               interceptionmodel = DFM_HYD_INTERCEPT_LAYER
               jadhyd = 1
            end if
         else if (strcmpi(qid, 'PotentialEvaporation')) then
            call realloc(kcsini, ndx, keepExisting=.false.)
            call prepare_lateral_mask(kcsini, iLocType)

            call realloc(PotEvap, ndx, keepExisting=.true., fill = 0d0)
            success = timespaceinitialfield(xz, yz, PotEvap, ndx, filename, filetype, method, operand, transformcoef, 2, kcsini)
            if (success) then
               where (PotEvap /= dmiss)
                  PotEvap = PotEvap*1d-3/(3600d0)            ! mm/hr => m/s
               end where
               ! TODO: UNST-3875: when proper EvaporationModel is introduced: remove/refactor the lines below.
               jaevap = 1
               if (.not. allocated(evap)) then
                  call realloc(evap, ndx, keepExisting=.false., fill = 0d0)
               end if
               evap = -PotEvap ! evap and PotEvap are now still doubling

               if (.not. allocated(ActEvap)) then
                  call realloc(ActEvap, ndx, keepExisting=.false., fill = 0d0)
               end if

               ! Auto-enable hydrology module, if PotentialEvaporation was supplied.
               jadhyd = 1
            end if
         else if (strcmpi(qid, 'frictioncoefficient')) then
            ! TODO: masking u points
            success = timespaceinitialfield(xu, yu, frcu, lnx, filename, filetype, method,  operand, transformcoef, 1) ! zie meteo module
               if (success) then
                  if (transformcoef(3) .ne. -999d0 .and. int(transformcoef(3)) .ne. ifrctypuni .and. operand == 'O') then
                     do L = 1,lnx
                        if (frcu(L) .ne. dmiss) then
                            ! type array only must be used if different from uni
                            ifrcutp(L) = int( transformcoef(3) )
                        endif
                     enddo
                  endif
               endif
         else if (strcmpi(groupname, 'Initial') .and. strcmpi(qid, 'bedlevel')) then
            ! Bed level was earlier set in setbedlevelfromextfile()
            cycle
         else
            write(msgbuf, '(5a)') 'Wrong block in file ''', trim(inifilename), ''': [', trim(groupname), ']. Field ''quantity'' does not match (refer to User Manual). Ignoring this block.'
            call warn_flush()
         end if
      end if
   end do
   
   write(msgbuf,'(a,i8,a)') 'Finish initializing the initial field file '''//trim(inifilename)//''':', ib , ' blocks have been read and handled.'
   call msg_flush()

   if (.not. success) then
      ierr = DFM_EXTFORCERROR
   end if

888 continue
    ! Return with whichever ierr status was set before.

end function initInitialFields


!> Reads all key values for a data provider from an IniFieldFile block.
!! All returned values will typically be used for a call to timespaceinitialfield().
subroutine readIniFieldProvider(inifilename, node_ptr,groupname,quantity,filename,filetype,method,iloctype,operand,transformcoef,ja,varname,smask, maxSearchRadius)
   use timespace_parameters, only: inside_polygon, field1D
   use m_ec_interpolationsettings, only: RCEL_DEFAULT
   use m_wind, only: ILATTP_1D, ILATTP_2D, ILATTP_ALL
   use m_grw
   character (len=*), intent(in   )           :: inifilename         !< Name of the ini file, only used in warning messages, actual data is read from node_ptr.
   type(tree_data), pointer                   :: node_ptr            !< The tree structure containing a single ini-file chapter/block.
   character (len=*), intent(  out)           :: groupname           !< Identifier of the read chapter (e.g., 'Initial')
   character (len=*), intent(  out)           :: quantity            !< Identifier of current quantity (e.g., 'waterlevel')
   character (len=*), intent(  out)           :: filename            !< Name of data file for current quantity.
   integer,           intent(  out)           :: filetype            !< File type of current quantity.
   integer,           intent(  out)           :: method              !< Time-interpolation method for current quantity.
   integer,           intent(  out)           :: iloctype            !< The spatial type of the target locations: 1D, 2D or all.
   character (len=1), intent(  out)           :: operand             !< Operand w.r.t. previous data ('O'verride or '+'Append)
   double precision,  intent(  out)           :: transformcoef(:)    !< Transformation coefficients
   integer,           intent(  out)           :: ja                  !< Whether a block was successfully read or not.
   character (len=*), intent(  out)           :: varname             !< variable name within filename; only in case of NetCDF
   character (len=*), intent(  out), optional :: smask               !< Name of mask-file applied to source arcinfo meteo-data
   double precision,  intent(  out), optional :: maxSearchRadius     !< max search radius for method == 11

   integer                         :: istat
   integer, parameter              :: ini_key_len   = 32
   integer, parameter              :: ini_value_len = 256
   character(len=ini_value_len)    :: dataFileType
   character(len=ini_value_len)    :: interpolationMethod
   character(len=ini_value_len)    :: averagingType
   character(len=ini_value_len)    :: locationType
   integer :: iav, extrapolation, averagingNumMin
   logical :: retVal
   ja = 0
   groupname = tree_get_name(node_ptr)
   
   ! TODO: support reading from ini of varname, smask and maxSearchRadius.
   if (strcmpi(groupname,'General')) then
      ja = 1
      goto 888
   end if
   
   transformcoef = -999d0

   if ((.not. strcmpi(groupname,'Initial')) .and. (.not.(strcmpi(groupname,'Parameter')))) then
      write(msgbuf, '(5a)') 'Unrecognized block in file ''', trim(inifilename), ''': [', trim(groupname), ']. Ignoring this block.'
      call warn_flush()
      goto 888
   end if
      
   ! read quantity
   call prop_get_string(node_ptr, '', 'quantity', quantity, retVal)
   if (.not. retVal) then
      write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(inifilename), ''': [', trim(groupname), ']. Field ''quantity'' is missing. Ignoring this block.'
      call warn_flush()
      goto 888
   end if
   
   ! read datafile
   call prop_get_string(node_ptr, '', 'dataFile', filename, retVal)
   if (retVal) then
   else
      write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(inifilename), ''': [', trim(groupname), '] for quantity='//trim(quantity)//'. Field ''dataFile'' is missing. Ignoring this block.'
      call warn_flush()
      goto 888
   end if
   
   ! read dataFileType
   call prop_get_string(node_ptr, '', 'dataFileType ', dataFileType , retVal)
   if (.not. retVal) then
      write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(inifilename), ''': [', trim(groupname), '] for quantity='//trim(quantity)//'. Field ''dataFileType'' is missing. Ignoring this block.'
      call warn_flush()
      goto 888
   end if
   call fileTypeStringToInteger(dataFileType, filetype)
   if (filetype < 0) then
      write(msgbuf, '(5a)') 'Wrong block in file ''', trim(inifilename), ''': [', trim(groupname), '] for quantity='//trim(quantity)//'. Field ''dataFileType'' has invalid value '''//trim(dataFileType)//'''. Ignoring this block.'
      call warn_flush()
      goto 888
   end if
   
      
   ! if dataFileType is 1dField, then it is not necessary to read interpolationMethod, operand, averagingType,  
   ! averagingRelSize, averagingNumMin, averagingPercentile, locationType, extrapolationMethod, value
   if (filetype /= field1D) then 
      ! read interpolationMethod
      call prop_get_string(node_ptr, '', 'interpolationMethod ', interpolationMethod , retVal)
      if (.not. retVal) then
         write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(inifilename), ''': [', trim(groupname), '] for quantity='//trim(quantity)//'. Field ''interpolationMethod'' is missing. Ignoring this block.'
         call warn_flush()
         goto 888
      end if
      call methodStringToInteger(interpolationMethod, method)
      if (method < 0 .or. (method == 4 .and. filetype /= inside_polygon)) then
         write(msgbuf, '(5a)') 'Wrong block in file ''', trim(inifilename), ''': [', trim(groupname), '] for quantity='//trim(quantity)//'. Field ''interpolationMethod'' has invalid value '''//trim(interpolationMethod)//'''. Ignoring this block.'
         call warn_flush()
         goto 888
      end if
      
      if (method == 6) then ! 'averaging'
         ! read averagingType
         call prop_get_string(node_ptr, '', 'averagingType ', averagingType , retVal)
         if (.not. retVal) then
            averagingType = 'mean'
         end if
         call averagingTypeStringToInteger(averagingType, iav)
         if (iav >= 0) then
            transformcoef(4) = dble(iav)
         else
            write(msgbuf, '(5a)') 'Wrong block in file ''', trim(inifilename), ''': [', trim(groupname), '] for quantity='//trim(quantity)//'. Field ''averagingType'' has invalid value '''//trim(averagingType)//'''. Ignoring this block.'
            call warn_flush()
            goto 888
         end if


         ! read averagingRelSize
         call prop_get_double(node_ptr,'','averagingRelSize', transformcoef(5), retVal)
         if (.not. retVal) then
            transformcoef(5) = RCEL_DEFAULT
         else
            if (transformcoef(5) <= 0d0) then
               write(msgbuf, '(5a,f10.3,a,f10.3,a)') 'Wrong block in file ''', trim(inifilename), ''': [', trim(groupname), '] for quantity='//trim(quantity)//'. Field ''averagingRelSize'' has invalid value ', transformcoef(5), '. Setting to default: ', RCEL_DEFAULT, '.'
               call warn_flush()
               transformcoef(5) = RCEL_DEFAULT
            end if
         end if
            
         ! read averagingNumMin
         call prop_get_integer(node_ptr,'','averagingNumMin', averagingNumMin, retVal)
         if (.not. retVal) then
            transformcoef(8) = 1d0
         else
            if (averagingNumMin <= 0) then
               write(msgbuf, '(5a,i0,a)') 'Wrong block in file ''', trim(inifilename), ''': [', trim(groupname), '] for quantity='//trim(quantity)//'. Field ''averagingNumMin'' has invalid value ', averagingNumMin, '. Setting to default: 1.'
               call warn_flush()
               transformcoef(8) = 1d0
            else
               transformcoef(8) = dble(averagingNumMin)
            end if
         end if
            
         ! read averagingPercentile
         call prop_get_double(node_ptr,'','averagingPercentile', transformcoef(7), retVal)
         if (.not. retVal) then
            transformcoef(7) = 0d0
         else
            if (transformcoef(7) < 0d0) then
               write(msgbuf, '(5a,f10.3,a)') 'Wrong block in file ''', trim(inifilename), ''': [', trim(groupname), '] for quantity='//trim(quantity)//'. Field ''averagingPercentile'' has invalid value ', transformcoef(7), '. Setting to default: 0.0.'  
               call warn_flush()
               transformcoef(7) = 0d0
            end if
         end if
      end if
         
      call prop_get_string(node_ptr, '', 'locationType ', locationType , retVal)
      if (.not. retVal) then
         ilocType = ILATTP_ALL
      else
         call str_lower(locationType)
         select case (trim(locationType))
            case ('1d')
               ilocType = ILATTP_1D
            case ('2d')
               ilocType = ILATTP_2D
            case ('1d2d')
               ilocType = ILATTP_ALL
            case default
               ilocType = ILATTP_ALL
         end select
      end if

      ! if the infiltrationmodel is not horton, but a horton quantity is detected, then send a error message
      if (infiltrationmodel /= DFM_HYD_INFILT_HORTON .and. &
         strcmpi(quantity,'Horton', 6)) then
         write (msgbuf, '(a,i0,a)') 'File '''//trim(inifilename)//''' contains quantity '''//trim(quantity)//'''. This requires ''InfiltrationModel=',DFM_HYD_INFILT_HORTON, ''' in the MDU file (Horton).'
         call warn_flush()
      end if
         
      ! read extrapolationMethod
      call prop_get_integer(node_ptr,'','extrapolationMethod', extrapolation, retVal)
      if (.not. retVal) then
         extrapolation = 0
      end if
      method = method + 100 * extrapolation
         
      ! read value
      if (filetype == inside_polygon) then
         call prop_get_double(node_ptr,'','value', transformcoef(1), retVal)
         if (.not. retVal) then
            write(msgbuf, '(5a)') 'Wrong block in file ''', trim(inifilename), ''': [', trim(groupname), '] for quantity='//trim(quantity)//'. Field ''value'' is missing. Ignore this block.'
            call warn_flush()
            goto 888
         end if
      end if
   end if ! .not. strcmpi(dataFileType, '1dField'))

   ! read operand, for any filetype
   call prop_get_string(node_ptr, '', 'operand ', operand , retVal)
   if (.not. retVal) then
      operand = 'O'
   else
      if ((.not.strcmpi(operand, 'O')) .and. (.not.strcmpi(operand, 'A')) .and. (.not.strcmpi(operand, '+')) .and. (.not.strcmpi(operand, '*')) .and. (.not.strcmpi(operand, 'X')) .and. (.not.strcmpi(operand, 'N'))) then
         write(msgbuf, '(5a)') 'Wrong block in file ''', trim(inifilename), ''': [', trim(groupname), '] for quantity='//trim(quantity)//'. Field ''operand'' has invalid value '''//trim(operand)//'''. Ignoring this block.'
         call warn_flush()
         goto 888
      end if
   end if

   varname = ''  ! TODO: Support reading varname for NetCDF files as well.

   ! We've made it to here, success!
   ja = 1
   return

888 continue
   ! Some error occurred, return without setting ja=1
   return
   
end subroutine readIniFieldProvider


!> Reads and initializes a 1d Field file (*.ini). 
function init1dField(filename, inifieldfilename, quant) result (ierr)
   use tree_data_types
   use tree_structures
   use messageHandling
   use unstruc_files, only: resolvePath
   use system_utils
   use m_alloc
   use m_missing
   use m_flow
   use m_flowgeom
   use network_data
   use dfm_error
   implicit none
   
   character(len=*), intent(in) :: filename            !< file name for 1dField file
   character(len=*), intent(in) :: inifieldfilename    !< file name of iniField file (only for messages)
   character(len=*), intent(in) :: quant               !< quantity that is specified in iniField file
   integer                      :: ierr                !< Result status (DFM_NOERR on success)
   
   type(tree_data), pointer     :: field_ptr           !< tree of inifield-file's [Initial] or [Parameter] blocks
   type(tree_data), pointer     :: node_ptr            !
   integer                      :: istat               !
   !logical                      :: success
   
   integer, parameter           :: ini_key_len   = 32  !
   integer, parameter           :: ini_value_len = 256 !
   character(len=ini_key_len)   :: groupname           !
   character(len=ini_value_len) :: quantity            ! 
   character(len=ini_value_len) :: unit                !
   character(len=ini_value_len) :: branchId            !            
   
   double precision, allocatable:: values(:)           !
   integer                      :: numLocations        !
   double precision, allocatable:: chainage(:)         ! 
   
   integer                      :: num_items_in_file   !
   logical                      :: retVal
   character(len=ini_value_len) :: fnam
   integer                      :: ib, jaglobal, i, j, numerr
   double precision             :: mchainage
   
   ierr = DFM_NOERR

   call tree_create(trim(filename), field_ptr)
   call prop_file('ini',trim(filename),field_ptr,istat) 

   if (istat /= 0) then
      write(msgbuf, '(3a)') 'Error opening 1D field file ''', trim(filename), '''. Is the file path correct?'
      call warn_flush()
      goto 888
   end if

   num_items_in_file = 0
   if (associated(field_ptr%child_nodes)) then
       num_items_in_file = size(field_ptr%child_nodes)
   endif
   
   ib = 0
   jaglobal = 0
   numLocations = 0
   numerr = 0
   
   
   ! loop on each block
   do i=1,num_items_in_file
   
      node_ptr => field_ptr%child_nodes(i)%node_ptr
      groupname = tree_get_name(node_ptr)
      
      ! Step 1: read the block
      if (strcmpi(groupname, 'General')) then
         cycle
      end if
      if (strcmpi(groupname, 'Global')) then
         if (jaglobal == 0) then
            ! read quantity
            call prop_get_string(node_ptr, '', 'quantity', quantity, retVal)
            if (.not. retVal) then
               numerr = numerr + 1
               write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''quantity'' is missing.'
               call warn_flush()
               cycle
            end if
            if (.not. strcmpi(quantity, quant)) then
               numerr = numerr + 1
               write(msgbuf, '(7a)') 'Wrong block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''quantity'' does not match the "quantity" which is specified in iniField file ''', trim(inifieldfilename), '''.'
               call warn_flush()
               cycle
            end if
            if ((.not. strcmpi(quantity, 'bedlevel')) .and. (.not.strcmpi(quantity, 'waterlevel')) .and. (.not. strcmpi(quantity,'waterdepth')) .and. (.not. strcmpi(quantity, 'frictioncoefficient'))) then
               numerr = numerr + 1
               write(msgbuf, '(7a)') 'Wrong block in file ''', trim(filename), ''': [', trim(groupname), ']. Quantity ''', trim(quantity), ''' is unknown.'
               call warn_flush()
               cycle
            end if
            ! read unit
            call prop_get_string(node_ptr, '', 'unit', unit, retVal)
            if (.not. retVal) then
               numerr = numerr + 1
               write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''unit'' is missing.'
               call warn_flush()
               cycle
            end if
            
            call realloc(values, 1, keepExisting=.false., fill = dmiss)
            call prop_get_double(node_ptr, '', 'value', values(1), retVal)
            if (.not. retVal) then
               numerr = numerr + 1
               write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''value'' is missing.'
               call warn_flush()
               cycle
            end if
            branchId = ''
            call realloc(chainage, 1, keepExisting = .false., fill=dmiss)
            jaglobal = 1
         else
            write(msgbuf, '(5a)') 'In file ''', trim(filename), ''': [', trim(groupname), ']. Only the first [Global] block is read, other [Global] blocks are ignored.'
            call warn_flush()
            cycle
         end if
      else if (strcmpi(groupname, 'Branch')) then
         call prop_get_string(node_ptr, '', 'branchId', branchId, retVal)
         if (.not. retVal) then
            numerr = numerr + 1
            write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''branchId'' is missing.'
            call warn_flush()
            cycle
         end if
         
         call prop_get_integer(node_ptr, '', 'numLocations', numLocations, retVal)
         if (.not. retVal) then
            numLocations = 0
         end if
         
         if (numLocations > 0) then
            call realloc(chainage, numLocations, keepExisting = .false.)
            call prop_get_doubles(node_ptr, '', 'chainage', chainage, numLocations, retVal)
            if (.not. retVal) then
               numerr = numerr + 1
               write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''chainage'' could not be read.'
               call warn_flush()
               cycle
            end if
            
            ! check if the locations are sorted by increasing chainage
            mchainage = chainage(1)
            do j = 2, size(chainage)
               if (chainage(j) > mchainage) then
                  mchainage = chainage(j)
               else
                  numerr = numerr + 1
                  write (msgbuf, '(3a)') 'Invalid data in file ''', trim(filename), ''': the locations are not sorted by increasing chainage.'
                  call warn_flush()
                  cycle
               end if
            end do
                          

            call realloc(values, numLocations, keepExisting = .false.)
            call prop_get_doubles(node_ptr, '', 'values', values, numLocations, retVal)
            if (.not. retVal) then
               numerr = numerr + 1
               write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''values'' could not be read.'
               call warn_flush()
               cycle
            end if
         else
            call realloc(values, 1, keepExisting = .false.)
            call prop_get_double(node_ptr, '', 'values', values(1),retVal)
            if (.not. retVal) then
               numerr = numerr + 1
               write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''values'' could not be read.'
               call warn_flush()
               cycle
            end if
         end if
         ib = ib + 1
      else
         write(msgbuf, '(5a)') 'Unrecognized block in file ''', trim(filename), ''': [', trim(groupname), ']. Ignoring this block.'
         call warn_flush()
         cycle
      end if
      
      
      ! Step 2: operations
      if (strcmpi(quantity, 'waterlevel')) then
         call spaceInit1dfield(branchId, chainage, values, 2, s1)
         hs(1:ndxi) = s1(1:ndxi) - bl(1:ndxi)
      else if (strcmpi(quantity, 'waterdepth')) then
         call spaceInit1dfield(branchId, chainage, values, 2, hs)
         s1(1:ndxi) = bl(1:ndxi) + hs(1:ndxi)
      else if (strcmpi(quantity, 'frictioncoefficient')) then
         call spaceInit1dfield(branchId, chainage, values, 1, frcu)
      else if (strcmpi(quantity, 'bedlevel')) then
         !call spaceInit1dfield(branchId, chainage, values, 2, zk)
         ! TODO: UNST-2694, Reading bedlevel from 1dField file type is not yet supported.
         numerr = numerr + 1
         write(msgbuf, '(5a)') 'Unsupported block in file ''', trim(filename), ''': [', trim(groupname), ']. Reading bedlevel from 1dField file type is not yet supported.'
         call warn_flush()
         cycle
      end if 
   end do

   if (numerr > 0) then
      goto 888
   end if

   ! No errors
   write(msgbuf,'(a, i10,a)') 'Finish initializing 1dField file '''//trim(filename)//''':', ib , ' [Branch] blocks have been read and handled.'
   call msg_flush()    
   return

888 continue
   ! There were errors
   ierr = DFM_WRONGINPUT
   return

      

end function init1dField


!> Converts fileType string to an integer.
!! Returns -1 when an invalid type string is given.
subroutine fileTypeStringToInteger(sFileType, iFileType)
   use timespace_parameters 
   implicit none
   character(len=*), intent(in   ) :: sFileType        !< file type string
   integer,          intent(  out) :: iFileType        !< file type integer
   
   call str_lower(sFileType)
   select case (trim(sFileType))
      case ('arcinfo')
         iFileType = arcinfo
      case ('sample')
         iFileType = triangulation
      case ('1dfield')
         iFileType = field1D
      case ('polygon')
         iFileType = inside_polygon
      case ('geotiff')
         iFileType = geotiff
      case default
         iFileType = -1
   end select
   return

end subroutine fileTypeStringToInteger


!> Converts interpolationMethod string to an integer.
!! Returns -1 when an invalid type string is given.
subroutine methodStringToInteger(sMethod, imethod)
   implicit none
   character(len=*), intent(in   ) :: sMethod        !< method string
   integer,          intent(  out) :: imethod        !< method integer

   call str_lower(sMethod)
   select case (trim(sMethod))
      case ('constant')
         imethod = 4
      case ('triangulation')
         imethod = 5
      case ('averaging')
         imethod = 6
      case default
         imethod = -1
   end select
   return

end subroutine methodStringToInteger


!> Converts averaging type string to an integer value.
!! Returns -1 when an invalid type string is given.
subroutine averagingTypeStringToInteger(sAveragingType, iAveragingType)
   use m_ec_interpolationsettings
   implicit none
   character(len=*), intent(in   ) :: sAveragingType        ! averaging type string
   integer,          intent(  out) :: iAveragingType        ! averaging type integer
   
   call str_lower(sAveragingType)
   select case (trim(sAveragingType))
   case ('mean')
      iAveragingType = AVGTP_MEAN
   case ('nearestnb')
      iAveragingType = AVGTP_NEARESTNB
   case ('max')
      iAveragingType = AVGTP_MAX
   case ('min')
      iAveragingType = AVGTP_MIN
   case ('invdist')
      iAveragingType = AVGTP_INVDIST
   case ('minabs')
      iAveragingType = AVGTP_MINABS
   case ('median')
      iAveragingType = AVGTP_MEDIAN
   case default
      iAveragingType = -1
   end select

   return

end subroutine averagingTypeStringToInteger


!> Initialize the values based on the given sample values
!! The method is: 
!! 1) When one sample value is given:
!!    if it is from a [Global] block, then this value will be set on all branches.
!!    if it is from a [Branch] block, then this value will be set on a this branch.
!! 2) if more than one sample values are given, then on this branch:
!!          *
!!         / \
!!        /   *----
!!   ----*
!! between two samples use linear interpolation,                             
!! on the left side of the most left sample, use constant value of this sample,
!! on the right side of the most right sample, use constant value of this sample.
subroutine spaceInit1dField(sBranchId, sChainages, sValues, ipos, res)
   use m_alloc
   use m_network
   use m_inquire_flowgeom
   use unstruc_channel_flow
   use m_flowparameters, only: eps10
   use precision_basics
   use m_hash_search
   use dfm_error
   
   implicit none
   character(len=*), intent(in   ) :: sBranchId     !< Sample branchId
   double precision, intent(in   ) :: sChainages(:) !< Sample chainages
   double precision, intent(in   ) :: sValues(:)    !< Sample values
   integer,          intent(in   ) :: ipos          !< position: 1= u point location, 2= 1d flownode(netnode) location
   double precision, intent(  out) :: res(:)        !< result
   
   integer                 :: nbrstart, nbrend, ibr, k, j, i, ierr, ipre, ns, ncount
   integer                 :: is, ip1, ip2, ipe
   type(t_branch), pointer :: pbr
   double precision        :: chai, sChaiPrev, sChai, sValPrev, sVal, minsChai, maxsChai
   character(len=256)      :: brId
   
  
   if (size(sValues) == 1) then 
   ! assign sValues(1) on all branches, or on a certain branch
      if (len_trim(sBranchId) == 0) then ! [Global] block, for all branches
         nbrstart = 1
         nbrend   = network%brs%Count
      else ! [Branch] block with a uniform value, for a certain branch
         nbrstart = hashsearch(network%brs%hashlist, sBranchId)
         nbrend   = nbrstart
      end if
      
      ! assign sValues to res
      do ibr = nbrstart, nbrend
         pbr => network%brs%branch(ibr)
         brId = pbr%id
         do is=1,pbr%gridpointsseqcount
            ip1 = pbr%k1gridpointsseq(is)
            ip2 = pbr%k2gridpointsseq(is)
            if (ipos == 1) then
               ipe = ip2 - 1 ! upoints loop
            else if (ipos == 2) then
               ipe = ip2 ! grid points loopt
            end if

            do i=ip1,ipe
               if (ipos == 1) then
                  k = pbr%lin(i)
               else if (ipos == 2) then
                  k = pbr%grd(i)
               end if
         
               res(k) = sValues(1)
            end do
         end do
      end do
            
   else 
   ![Branch] block with numLocations > 1, and needs interpolations
      ns = size(sChainages)
      minsChai = sChainages(1)
      maxsChai = sChainages(ns)
      
      ibr = hashsearch(network%brs%hashlist, sBranchId)
      pbr => network%brs%branch(ibr)
      
      if (ipos == 1) then
         ncount = pbr%uPointsCount
      else if (ipos == 2) then
         ncount = pbr%gridPointsCount
      end if
      
      ipre = 2
      do j = 1, ncount
         if (ipos == 1) then
            chai = pbr%uPointsChainages(j)
            k = pbr%lin(j)
         else if (ipos == 2) then
            chai = pbr%gridPointsChainages(j)
            k = pbr%grd(j)
         end if
         ! Constant value before the first data segment and after the last data segment.
         if (comparereal(chai, minsChai, eps10) <= 0) then
            res(k) = sValues(1)
            cycle
         else if (comparereal(chai, maxsChai, eps10) >= 0) then
            res(k) = sValues(ns)
            cycle
         end if
               
         ! Linear interpolation, find the data segment in which the current position k lies.
         do i = ipre, ns
            sChaiPrev = sChainages(i-1)
            sChai     = sChainages(i)
            sValPrev  = sValues(i-1)
            sVal      = sValues(i)
               
            if (comparereal(chai, sChaiPrev, eps10) >= 0 .and. comparereal(chai, sChai, eps10) < 0) then
               if (comparereal(sChai, sChaiPrev, eps10)/=0) then
                  res(k) = sValPrev + (sVal-sValPrev)/(sChai-sChaiPrev)*(chai-sChaiPrev)
               else
                  res(k) = (sVal + sValPrev)/2
               end if
               ipre = i
               exit
            end if  
         end do
      end do
   end if
end subroutine spaceInit1dField

end module unstruc_inifields
