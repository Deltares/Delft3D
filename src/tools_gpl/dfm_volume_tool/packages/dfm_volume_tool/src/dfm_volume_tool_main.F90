!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
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

!> DFMOUTPUT - A postprocessing tool for output files from D-Flow Flexible Mesh.
!! Combines several commands/operations into a single program.
!!
!! Available commands:
!!
!! 
program dfm_volume_tool
!-----------------------------------------------------------------------------------------------------------------------------------

use IR_Precision                                                        ! Integers and reals precision definition.
use precision 
use iso_c_utils
use string_module
use messagehandling
use dfm_volume_tool_version_module
use Data_Type_Command_Line_Interface, only: Type_Command_Line_Interface ! Definition of Type_Command_Line_Interface.
use m_netcdf_output
use m_VolumeTables
use m_network
use m_StorageTable
use unstruc_channel_flow, only: tableincrement
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none

character(len=Idlen) :: dll_name
character(len=Idlen) :: mdufile
integer(pntrsize)    :: dfm
integer              :: ret_val, dia
integer              :: i, j, L
integer              :: idindex
integer              :: numids
integer              :: numbranches
integer              :: ioutput, ncid
double precision     :: increment
double precision     :: bedlevel, toplevel
character(len=Idlen) :: output_type
character(len=Idlen) :: output_file, grid_output_file
character(len=Idlen) :: var_name
type(c_ptr)          :: xptr
integer              :: numpoints, numlinks, lnx, numbnd
integer, pointer     :: integer_pointer
logical              :: computeTotal
logical              :: computeOnBranches
logical              :: computeOnGridpoints


real(c_double), pointer, dimension(:,:) :: volume
real(c_double), pointer, dimension(:,:) :: surface
real(c_double), pointer, dimension(:,:) :: storage
real(c_double), pointer, dimension(:,:) :: deadstorage
real(c_double), pointer, dimension(:)   :: levels
real(C_double), pointer, dimension(:)   :: help

character(kind=c_char)                          :: c_output_type(15)
character(len=idlen), allocatable, dimension(:) :: ids
integer :: numlevels
type(t_voltable), dimension(:),   pointer       :: volumetable
type(t_voltable), dimension(:,:), pointer       :: volumetableOnLinks
type(t_network),                  pointer       :: network
type(t_branch),   dimension(:),   pointer       :: branches
integer,          dimension(:),   allocatable   :: mask, tablecount
integer,          dimension(:,:), pointer       :: bndindex
integer,          dimension(:,:), allocatable   :: ln2nd
integer,          dimension(:,:), pointer       :: lnog !Don't modify this!!!
integer,          dimension(:)  , pointer       :: kcu2, shapearray
integer,                          pointer       :: ndx2d
double precision, dimension(:),   pointer       :: bndvalues
double precision, dimension(:,:), pointer       :: inslevtube
double precision, dimension(:,:), pointer       :: gridvolume, gridsurface, griddeadstorage, gridstorage
double precision, dimension(:),   allocatable   :: wl_deadstorage, bedlevels, topheights
integer, parameter                              :: maxdims = 6
! externals

integer, external  :: open_shared_library, bmi_initialize

type(Type_Command_Line_Interface) :: cli          !< Command Line Interface (CLI).
integer(I4P)                      :: ierr         !< Error trapping flag.

!-----------------------------------------------------------------------------------------------------------------------------------
open(newunit = dia, file='dfm_volume_tool.dia')
call SetMessageHandling(write2screen = .true., lunmessages = dia)

call getfullversionstring_dfm_volume_tool(msgbuf)
call msg_flush()
call getbranch_dfm_volume_tool(msgbuf)
call msg_flush()

numids = 0
!-----------------------------------------------------------------------------------------------------------------------------------
!! initializing Command Line Interface
call cli%init(progname    = base_name,                                            &
              version     = version,                                             &
              description = 'Tool for generating volume output for D-Flow FM') 
!
!! setting Command Line Arguments
call cli%add(switch='--mdufile',  switch_ab='-f', help='Name of the input mdu file.',required=.true.,act='store',def=char(0),valname='FILE')
call cli%add(switch='--increment',switch_ab='-i', help='required interval for the volume tables',required=.true.,act='store',def='',valname='INCREMENT')
call cli%add(switch='--output',switch_ab='-o', help='output type: "Total", "Branches", "Gridpoints", "All"',required=.false.,act='store',def='',valname='OUTPUT_TYPE')
call cli%add(switch='--outputfile',switch_ab='-p', help='name of the output file',required=.false.,act='store',def='',valname='OUTPUT_FILE')
call cli%add(switch='--gridoutputfile',switch_ab='-g', help='name of the per-gridpoint output file',required=.false.,act='store',def='',valname='GRID_OUTPUT_FILE')

! parsing Command Line Interface
call cli%parse(error=ierr)
call cli%errored(error=ierr)

if (ierr /= 0) then
   call SetMessage(LEVEL_ERROR, 'Error reading input parameters')
endif

! Check options
output_type = 'Total'
output_file = ''
if (ierr==0) call cli%get(switch='--mdufile', val = mdufile, error=ierr)
if (ierr==0) call cli%get(switch='--increment', val = increment, error=ierr)
if (ierr==0) call cli%get(switch='--output', val = output_type, error=ierr)
if (ierr==0) then
   output_file = ''
   call cli%get(switch='--outputfile', val = output_file, error=ierr)
   if (len_trim(output_file)==0) then
      i = index(mdufile, '.', .true.)
      output_file = mdufile(1:i-1)//'.nc'
   endif
endif

if (ierr==0) then
   grid_output_file = ''
   call cli%get(switch='--gridoutputfile', val = grid_output_file, error=ierr)
   if (len_trim(grid_output_file)==0) then
      grid_output_file ="PerGridpoint_"//output_file
   endif
endif

#ifdef WIN32
   dll_name = 'dflowfm'
#else
   dll_name = 'libdflowfm.so'
#endif
if (ierr==0) then
   ierr = open_shared_library(dfm, dll_name)
   if (ierr /= 0) then
      msgbuf = 'Cannot open'//trim(dll_name)//'.'
      call msg_flush()
   endif
   
endif

if (ierr==0) then
   ! Initialise the model 
   ierr = bmi_initialize(dfm, mdufile)
   if (ierr /= 0) then
      msgbuf = 'Cannot initialize '''// trim(mdufile)//'''.'
      call msg_flush()
   endif

   ! Generate the volume tables
   call dfm_generate_volume_tables(dfm, increment)
   
   ! Retrieve data from the D-FLOW FM dll
   call BMI_GET_VAR_POINTER(dfm, string_to_char_array('lnx1D'), xptr)
   call c_f_pointer(xptr, integer_pointer)
   numlinks = integer_pointer
   
   call BMI_GET_VAR_POINTER(dfm, string_to_char_array('lnx'), xptr)
   call c_f_pointer(xptr, integer_pointer)
   lnx = integer_pointer

   call BMI_GET_VAR_SHAPE(dfm, string_to_char_array('zbndz'), xptr)  
   allocate(shapearray(MAXDIMS)) 
   call c_f_pointer(xptr, shapearray)
   numbnd = shapearray(1)
   
   call BMI_GET_VAR_POINTER(dfm, string_to_char_array('ndx2d'), xptr)
   call c_f_pointer(xptr, ndx2d)
   
   call BMI_GET_VAR_POINTER(dfm, string_to_char_array('ndx'), xptr)
   call c_f_pointer(xptr, integer_pointer)
   
   numpoints = integer_pointer - ndx2d
   
   allocate(inslevtube(2,lnx), ln2nd(2,lnx), lnog(2,lnx), bndvalues(numbnd), bndindex(6,numbnd),kcu2(lnx))
   call bmi_get_var(dfm, 'bob', inslevtube, 2*lnx)
   if (numbnd > 0) then
      call bmi_get_var(dfm, 'zbndz', bndvalues, numbnd)
      !call get_variable_pointer(dfm, string_to_char_array('kbndz'), xptr)
   endif
   call BMI_GET_VAR_POINTER(dfm, string_to_char_array('kcu'), xptr)
   call c_f_pointer(xptr, kcu2, (/ lnx /))
   
   call BMI_GET_VAR_POINTER(dfm, string_to_char_array('ln'), xptr)
   call c_f_pointer(xptr, lnog, (/2, lnx/))
   
   call BMI_GET_VAR_POINTER(dfm, string_to_char_array('kbndz'), xptr)
   call c_f_pointer(xptr, bndindex, (/MAXDIMS, numbnd/))
   
   call BMI_GET_VAR_POINTER(dfm, string_to_char_array('vltb'), xptr)
   call c_f_pointer(xptr, volumetable, (/numpoints/))
   call BMI_GET_VAR_POINTER(dfm, string_to_char_array('vltbOnLinks'), xptr)
   call c_f_pointer(xptr, volumetableOnLinks, (/2, numlinks/))
   call BMI_GET_VAR_POINTER(dfm, string_to_char_array('network'), xptr)
   call c_f_pointer(xptr, network)
   
   ! Determine the number of levels for the aggregated volume tables
   call getBedToplevel(volumetable, numpoints, toplevel,bedlevel)

   ln2nd = lnog
   
   do l = 1, lnx
     if(abs(kcu2(L)) == 1) then
         ln2nd(:,L) = lnog(:,L) - ndx2d
    endif
   enddo
   
   
   numlevels = 0
   do i = 1, numpoints
      numlevels = max(numlevels,volumetable(i)%count)
   enddo
   
   computeTotal        = .false.
   computeOnBranches   = .false.
   computeOnGridpoints = .false.
   numbranches = network%brs%Count
   if (strcmpi(output_type, 'Total')) then
      computeTotal = .true.
      numids = 1
   else if (strcmpi(output_type, 'Branches')) then
      computeOnBranches = .true.
      numids = numbranches
   else if (strcmpi(output_type, 'Gridpoints')) then
      computeOnGridpoints = .true.
   else if (strcmpi(output_type, 'All')) then
      computeTotal = .true.
      computeOnBranches = .true.
      computeOnGridpoints = .true.
      numids = numbranches + 1
   endif
      
   allocate(surface(numlevels,numids),volume(numlevels,numids), storage(numlevels,numids), deadstorage(numlevels,numids))
   allocate(levels(numlevels), ids(numids), mask(numpoints), wl_deadstorage(numpoints))
   
   surface = 0d0
   volume  = 0d0

   do i = 1, numlevels
      levels(i) = (i-1)*increment
   enddo
   tableincrement = increment
   
   call calculateDeadStorage(wl_deadstorage, network, bndvalues, inslevtube, bndindex, ln2nd, kcu2, numlinks, numpoints, numbnd)

   idindex = 0
   if (computeTotal) then
      idIndex = 1
      !> setup the mask array for the complete model
      do i = 1, numpoints
         mask(i) = i
      enddo
      
      !> Calculate the total volumes and surfaces for this model
      ids(1) = 'Total'
      call generateTotalVolumeTable(volume(:,1), surface(:,1), storage(:,1), deadstorage(:,1), &
                  wl_deadstorage, volumetable, bedlevel, increment, numpoints, numlevels, mask)
                  
   endif
   
   if (computeOnBranches) then
      branches => network%brs%branch
      do i = 1, numbranches
         idindex = idindex + 1
         ids(idindex) = branches(i)%id
         call generateVolumeTableOnBranches(volume(:,idindex), surface(:,idindex), storage(:,idindex), deadstorage(:,idindex), &
                        wl_deadstorage, volumetableOnLinks, bedlevel, &
                        increment, branches(i)%uPointsCount, numlevels, branches(i)%lin, ln2nd)
      enddo
   endif
   
   ioutput = nc_create(output_file)
   call write_volume_surface_arrays(ioutput, ids, levels, volume, surface, storage, deadstorage, numlevels, numids)
         
   if (computeOnGridpoints) then !Gridpoints are written separately
      !compute maximum volume table length
      
      allocate(gridsurface(numlevels,numpoints),griddeadstorage(numlevels,numpoints),gridvolume(numlevels,numpoints),gridstorage(numlevels,numpoints),bedlevels(numpoints),topheights(numpoints),tablecount(numpoints))
   
      do i = 1, numpoints   
         bedlevels(i)   = volumetable(i)%bedlevel
         topheights(i)  = volumetable(i)%topheight
         tablecount(i)  = volumetable(i)%count
         call AddVolumeAndSurface(volume(:,1), surface(:,1), griddeadstorage(:,i), wl_deadstorage(i), volumetable(i), volumetable(i)%bedlevel, increment, numlevels)
         do j = 1,volumetable(i)%count
            gridsurface(j,i) = volumetable(i)%sur(j)
            gridvolume (j,i) = volumetable(i)%vol(j)
            gridstorage(j,i) = gridvolume(j,i) - griddeadstorage(j,i)
         enddo
      enddo
      
      ncid = nc_create(grid_output_file)
      ! Write the output to the netcdf file
      call write_1d_flowgeom_ugrid(dfm, ncid)
      
      call write_volume_table_gridpoint_data(ncid,bedlevels,topheights,gridvolume,gridsurface, &
           gridstorage,griddeadstorage,wl_deadstorage,tablecount,numpoints,increment)
   endif
            
   deallocate(surface,volume, levels, ids, mask)
endif
      
end program dfm_volume_tool
