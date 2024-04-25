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

!> Module for long culvert data in a dflowfm model.
!! Long culverts are read from the structures.ini file(s), and converted into
!! new netlinks and prof1D definitions.
module m_longculverts
   use MessageHandling
   use m_missing
   use iso_c_binding

   private
   public realloc

   public default_longculverts
   public reset_longculverts
   public loadLongCulvertsAsNetwork
   public convertLongCulvertsAsNetwork
   public finalizeLongCulvertsInNetwork
   public LongCulvertsToProfs
   public setFrictionForLongculverts
   public reduceFlowAreaAtLongculverts
   public get_valve_relative_opening_c_loc
   public find1d2dculvertlinks
   public setlongculvert1d2dlinkangles

   !> Type definition for longculvert data.
   type, public :: t_longculvert
      character(len=IdLen)                           :: id
      character(len=IdLen)                           :: branchid                   !< if newculverts, corresponding network branch
      integer                                        :: numlinks                   !< Number of links of the long culvert
      integer, dimension(:), allocatable             :: netlinks                   !< Net link numbers of the long culvert
      integer, dimension(:), allocatable             :: flowlinks                  !< Flow link numbers of the long culvert
      integer                                        :: ifrctyp         = -999     !< Friction type
      integer                                        :: allowed_flowdir            !< Allowed flowdir:
                                                                                   !< 0 all directions
                                                                                   !< 1 only positive flow
                                                                                   !< 2 only negative flow
                                                                                   !< 3 no flow allowed
      double precision                               :: friction_value  = -999d0   !< Friction value
      double precision, dimension(:), allocatable    :: xcoords                    !< X-coordinates of the numlinks+1 points
      double precision, dimension(:), allocatable    :: ycoords                    !< Y-coordinates of the numlinks+1 points
      double precision, dimension(:), allocatable    :: bl                         !< Bed level on numlinks+1 points
      double precision                               :: width                      !< Width of the rectangular culvert
      double precision                               :: height                     !< Height of the rectangular culvert
      double precision                               :: valve_relative_opening     !< Relative valve opening: 0 = fully closed, 1 = fully open
      integer                                        :: flownode_up     = 0        !< Flow node index at upstream
      integer                                        :: flownode_dn     = 0        !< Flow node index at downstream
   end type
   type(t_longculvert), dimension(:), allocatable, public     :: longculverts               !< Array containing long culvert data (size >= nlongculverts)
   integer, public                                    :: nlongculverts             !< Number of longculverts
   logical, public                                    :: newculverts
   interface realloc
      module procedure reallocLongCulverts
   end interface


contains

   !> Sets ALL (scalar) variables in this module to their default values.
   !! For a reinit prior to flow computation, call reset_longculverts() instead.
   subroutine default_longculverts()
      if (allocated(longculverts)) then
         deallocate(longculverts)
      end if

      nlongculverts = 0             !< Number of longculverts

      ! Remaining of variables is handled in reset_longculverts()
      call reset_longculverts()
   end subroutine default_longculverts


   !> Resets only long culverts variables intended for a restart of flow simulation.
   !! Upon loading of new model/MDU, use default_longculverts() instead.
   subroutine reset_longculverts()
      ! NOT: intentionally not resetting nlongculverts counter here, because that is part of model loading.
   end subroutine reset_longculverts

   !> Loads the long culverts from a structures.ini file and
   !! creates extra netnodes+links for them.
   subroutine convertLongCulvertsAsNetwork(structurefile, jaKeepExisting,culvertprefix,structures_output,crsdef_output,ierr)
   !use network_data
   use dfm_error

   use string_module, only: strcmpi
   use m_polygon
   use m_missing
   use m_Roughness
   use m_readstructures
   use messageHandling
   use properties
   use unstruc_channel_flow
   use m_save_ugrid_state
   use system_utils

   implicit none

   character(len=*),              intent(inout) :: structurefile  	  !< File name of the structure.ini file.
   integer,                       intent(in   ) :: jaKeepExisting     !< Whether or not (1/0) to keep the existing already read long culverts.
   character(len=*),              intent(in   ) :: culvertprefix      !< Command line argument prefix to add to the converted files
   character(len=:), allocatable, intent(  out) :: structures_output  !< structures ini output file ( = culvertprefix // structurefile )
   character(len=:), allocatable, intent(  out) :: crsdef_output      !< crs def ini output file
   integer,               intent(  out) :: ierr           !< Result status, DFM_NOERR in case of success.
   
   character(len=128) :: filename =  'crsdef.ini'
   character(len=:),    allocatable :: line
   type(tree_data), pointer :: prop_ptr
   type(tree_data), pointer :: block_ptr
   type(tree_data), pointer :: node_ptr
   type(tree_data), pointer :: strs_ptr
   type(tree_data), pointer :: str_ptr
   character(len=IdLen) :: typestr
   character(len=IdLen) :: st_id
   character(len=IdLen) :: csDefId
   character(len=IdLen) :: txt
   integer :: readerr, nstr, i, numcoords
   integer, allocatable, dimension(:)    :: links
   logical :: success
   integer :: istart
   integer :: nlongculverts0
   integer :: mout
   integer :: longculvertindex
   character(len=IdLen) :: temppath, tempname, tempext 
   
   ierr = DFM_NOERR
   filename = trim(culvertprefix)//filename

   call split_filename(structurefile, temppath, tempname, tempext)
   tempname = trim(culvertprefix)//tempname
   structures_output = cat_filename(temppath, tempname, tempext)
   
   allocate(character(maxlen)::line)

   nlongculverts0 = nlongculverts ! Remember any old longculvert count

   if (jaKeepExisting == 0) then
      nlongculverts = 0
      if (allocated(longculverts)) then
         deallocate(longculverts)
      end if
   end if
   call savepol()
   xpl = dmiss
   ypl = dmiss
   zpl = dmiss
   npl = 0

   call tree_create(trim(filename), prop_ptr)
   call tree_create_node(prop_ptr,'General',block_ptr)
   call tree_create_node(block_ptr,'fileVersion',node_ptr)
   call tree_put_data(node_ptr,transfer('3.00',node_value),'STRING') ! fileVersion           = 3.00

   call tree_create_node(block_ptr,'fileType',node_ptr)
   call tree_put_data(node_ptr,transfer("crossDef",node_value),'STRING')  !fileType = crossDef

   ! Temporarily put structures.ini file into a property tree
   call tree_create(trim(structurefile), strs_ptr)
   call prop_inifile(structurefile, strs_ptr, readerr)
   !check if file was successfully opened
   if ( readerr /= 0 ) then
      ierr = DFM_WRONGINPUT
      call mess(LEVEL_ERROR, 'Error opening file ''', trim(structurefile), ''' for loading the long culverts.')
   endif

   !tree_get_name(md_ptr%child_nodes(1)%node_ptr, 'Global')
   nstr = tree_num_nodes(strs_ptr)
   call realloc(longculverts, nlongculverts + nstr)
   do i=1,nstr
      str_ptr => strs_ptr%child_nodes(i)%node_ptr

      success = .true.

      if (.not. strcmpi(tree_get_name(str_ptr), 'Structure')) then
         ! Only read [Structure] blocks, skip any other (e.g., [General]).
         cycle
      end if

      typestr = ' '
      call prop_get_string(str_ptr, '', 'type',         typestr, success)
      if (.not. success .or. .not. strcmpi(typestr, 'longCulvert')) then
         cycle
      end if

      nlongculverts = nlongculverts + 1
      if (allocated(nbranchids)) then
         longculvertindex = meshgeom1d%nbranches
      else
         longculvertindex = 0
      endif

      call prop_get(str_ptr, '',  'id', st_id, success)
      if (.not. success) then
         write (msgbuf, '(a,i0,a)') 'Error Reading Structure #', i, ' from '''//trim(structurefile)//''', id is missing.'
         call err_flush()
      endif
      if (success) call prop_get(str_ptr, '', 'numCoordinates', numcoords, success)
      if (success) then

         call tree_create_node(prop_ptr,'Definition',block_ptr)
         csDefId = 'CsDef_longCulvert_'//trim(st_id)
         call prop_set(block_ptr, '', 'id'  , csDefId)
         call prop_set(str_ptr, '', 'csDefId'  , csDefId) ! Directly refer to this new csdef in the converted structure.
         call prop_set(block_ptr  , '', 'type', 'rectangle')  
         
         longculverts(nlongculverts)%id = st_id
         longculverts(nlongculverts)%numlinks = numcoords-1
         allocate(longculverts(nlongculverts)%netlinks(numcoords-1))
         allocate(longculverts(nlongculverts)%flowlinks(numcoords-1))
         longculverts(nlongculverts)%flowlinks = -999
         allocate(longculverts(nlongculverts)%bl(numcoords))
         call increasepol(numcoords, 0)
         call prop_get(str_ptr, '', 'xCoordinates', xpl(npl+1:), numcoords, success)
         if (.not. success) then
            call SetMessage(LEVEL_ERROR, 'xCoordinates not found for long culvert: '// st_id )
         else
            longculverts(nlongculverts)%xcoords = xpl(npl+1:npl+numcoords)
         endif
         call prop_get(str_ptr, '', 'yCoordinates', ypl(npl+1:), numcoords, success)
         if (.not. success) then
            call SetMessage(LEVEL_ERROR, 'yCoordinates not found for long culvert: '// st_id )
         else
            longculverts(nlongculverts)%ycoords = ypl(npl+1:npl+numcoords)
         endif
         call prop_get(str_ptr, '', 'zCoordinates', zpl(npl+1:), numcoords, success)
         if (.not. success) then
            call SetMessage(LEVEL_ERROR, 'zCoordinates not found for long culvert: '// st_id )
         endif
         longculverts(nlongculverts)%bl = zpl(npl+1:npl+numcoords)
         npl = npl+numcoords+1 ! TODO: UNST-4328: success1 checking done later in readStructureFile().
       
         call prop_get(str_ptr, '', 'allowedFlowdir', txt, success)
         if (.not. success) then
            TXT = 'both'
         endif
         longculverts(nlongculverts)%allowed_flowdir = allowedFlowDirToInt(txt)

         call prop_get(str_ptr, '', 'width', longculverts(nlongculverts)%width, success)
         if (.not. success) then
            call SetMessage(LEVEL_ERROR, 'width not found for long culvert: '// st_id )
         endif
         call prop_get_string(str_ptr, '', 'width', typestr)
         call prop_set(block_ptr  , '', 'width', trim(typestr))  
         
         call prop_get(str_ptr, '', 'height', longculverts(nlongculverts)%height, success)
         if (.not. success) then
            call SetMessage(LEVEL_ERROR, 'height not found for long culvert: '// st_id )
         endif       
         call prop_get_string(str_ptr, '', 'height', typestr)        
         call prop_set(block_ptr  , '', 'height', trim(typestr))   
    
         call prop_set(block_ptr  , '', 'closed', 'yes')    
         
         call prop_get(str_ptr, '', 'frictionType', typestr, success)
         if (.not. success) then
            longculverts(nlongculverts)%ifrctyp = -999
         else
            call frictionTypeStringToInteger(typestr, longculverts(nlongculverts)%ifrctyp)
         endif
         call tree_create_node(block_ptr,'frictionType',node_ptr)
         call tree_put_data(node_ptr,transfer(typestr,node_value),'STRING')

         call prop_get(str_ptr, '', 'frictionValue', longculverts(nlongculverts)%friction_value, success)
         if (.not. success) then
            call SetMessage(LEVEL_ERROR, 'frictionValue not found for long culvert: '// st_id )
         endif
         
         call prop_get_string(str_ptr, '', 'frictionValue', typestr)
         call prop_set(block_ptr  , '', 'frictionValue', trim(typestr))    
        
         call get_value_or_addto_forcinglist(str_ptr, 'valveRelativeOpening', longculverts(nlongculverts)%valve_relative_opening, st_id, &
            ST_LONGCULVERT,network%forcinglist, success)
         if (.not. success) then
            call SetMessage(LEVEL_ERROR, 'valveRelativeOpening not found for long culvert: '// st_id )
         endif
      else
         call SetMessage(LEVEL_ERROR, 'numCoordinates not found for long culvert '//st_id)
      end if

      
      call tree_remove_child_by_name( str_ptr, 'frictionValue', istart)
      call tree_remove_child_by_name( str_ptr, 'frictionType', istart)
      call tree_remove_child_by_name( str_ptr, 'height', istart)
      call tree_remove_child_by_name( str_ptr, 'width', istart)
      
      if (.not. success) then
         ! Some error during reading: decrement counter to ignore this long culvert.
         nlongculverts = nlongculverts - 1
      end if
   end do

   allocate(links(npl))
   call convert1D2DLongCulverts(xpl, ypl, zpl, npl, links)
   call restorepol()
   istart = 1
   do i = nlongculverts0+1, nlongculverts
      longculverts(i)%netlinks = links(istart:istart+longculverts(i)%numlinks-1)
      istart = istart+longculverts(i)%numlinks+2
   enddo

   ! Loop all structures once again, and for long culverts: add the newly created branchids.
   do i=1,nstr
      str_ptr => strs_ptr%child_nodes(i)%node_ptr

      success = .true.

      if (.not. strcmpi(tree_get_name(str_ptr), 'Structure')) then
         ! Only read [Structure] blocks, skip any other (e.g., [General]).
         cycle
      end if
      typestr = ' '
      call prop_get_string(str_ptr, '', 'type',         typestr, success)
      if (.not. success .or. .not. strcmpi(typestr, 'longCulvert')) then
         cycle
      end if

      call prop_get(str_ptr, '',  'id', st_id, success)
      if (.not. success) then
         write (msgbuf, '(a,i0,a)') 'Error Reading Structure #', i, ' from '''//trim(structurefile)//''', id is missing.'
         call err_flush()
      else
         longculvertindex = longculvertindex+1
         call prop_set(str_ptr  , '', 'branchId', nbranchids(longculvertindex))    
      endif
   enddo

   call newfil(mout,filename)
   crsdef_output = filename
   call prop_write_inifile(mout, prop_ptr, ierr)
   call newfil(mout,structures_output)
   call prop_write_inifile(mout, strs_ptr, ierr)
   call tree_destroy(strs_ptr)
   call tree_destroy(prop_ptr)
   
   end subroutine convertLongCulvertsAsNetwork
   !> Loads the long culverts from a structures.ini file and
   !! creates extra netnodes+links for them.
   subroutine loadLongCulvertsAsNetwork(structurefile, jaKeepExisting, ierr)
       !use network_data
       use dfm_error

       use string_module, only: strcmpi
       use m_polygon
       use m_missing
       use m_Roughness
       use m_readstructures
       use m_network
       use messageHandling
       use properties
       use unstruc_channel_flow

       implicit none

       character(len=*),      intent(in   ) :: structurefile  !< File name of the structure.ini file.
       integer,               intent(in   ) :: jaKeepExisting !< Whether or not (1/0) to keep the existing already read long culverts.
       integer,               intent(  out) :: ierr           !< Result status, DFM_NOERR in case of success.

       type(tree_data), pointer :: strs_ptr
       type(tree_data), pointer :: str_ptr
       character(len=IdLen) :: typestr
       character(len=IdLen) :: st_id
       character(len=IdLen) :: csDefId
       character(len=IdLen) :: txt
       integer :: readerr, nstr, i, numcoords
       integer, allocatable, dimension(:)    :: links
       logical :: success
       integer :: istart
       integer :: nlongculverts0, iref

       ierr = DFM_NOERR

       nlongculverts0 = nlongculverts ! Remember any old longculvert count

       msgbuf = 'Reading long culverts from '//trim(structurefile)//'.'
       call msg_flush()
       
       if (jaKeepExisting == 0) then
          nlongculverts = 0
          if (allocated(longculverts)) then
             deallocate(longculverts)
          end if
       end if
       call savepol()
       xpl = dmiss
       ypl = dmiss
       zpl = dmiss
       npl = 0

       ! Temporarily put structures.ini file into a property tree
       call tree_create(trim(structurefile), strs_ptr)
       call prop_inifile(structurefile, strs_ptr, readerr)
       ! check if file was successfully opened
       if ( readerr /= 0 ) then
         ierr = DFM_WRONGINPUT
         call mess(LEVEL_ERROR, 'Error opening file ''', trim(structurefile), ''' for loading the long culverts.')
       endif

       nstr = tree_num_nodes(strs_ptr)
       call realloc(longculverts, nlongculverts + nstr)
       newculverts = .false.
       do i=1,nstr
         str_ptr => strs_ptr%child_nodes(i)%node_ptr

         success = .true.

         if (.not. strcmpi(tree_get_name(str_ptr), 'Structure')) then
           ! Only read [Structure] blocks, skip any other (e.g., [General]).
           cycle
         end if

         typestr = ' '
         call prop_get_string(str_ptr, '', 'type',         typestr, success)
         if (.not. success .or. .not. strcmpi(typestr, 'longCulvert')) then
           cycle
         end if

         nlongculverts = nlongculverts + 1

         call prop_get(str_ptr, '',  'id', st_id, success)
         if (.not. success) then
           write (msgbuf, '(a,i0,a)') 'Error Reading Structure #', i, ' from '''//trim(structurefile)//''', id is missing.'
           call err_flush()
         endif
         if (success) call prop_get(str_ptr, '', 'numCoordinates', numcoords, success)
         if (success) then
           longculverts(nlongculverts)%id = st_id

           allocate(longculverts(nlongculverts)%bl(numcoords))
           call increasepol(numcoords, 0)

           call prop_get(str_ptr, '', 'xCoordinates', xpl(npl+1:), numcoords, success)
           if (.not. success) then
             call SetMessage(LEVEL_ERROR, 'xCoordinates not found for long culvert: '// trim(st_id) )
           else
             longculverts(nlongculverts)%xcoords = xpl(npl+1:npl+numcoords)
           endif
           call prop_get(str_ptr, '', 'yCoordinates', ypl(npl+1:), numcoords, success)
           if (.not. success) then
             call SetMessage(LEVEL_ERROR, 'yCoordinates not found for long culvert: '// trim(st_id) )
           else
             longculverts(nlongculverts)%ycoords = ypl(npl+1:npl+numcoords)
           endif
           call prop_get(str_ptr, '', 'zCoordinates', zpl(npl+1:), numcoords, success)
           if (.not. success) then
             call SetMessage(LEVEL_ERROR, 'zCoordinates not found for long culvert: '// trim(st_id) )
           endif
           longculverts(nlongculverts)%bl = zpl(npl+1:npl+numcoords)
           npl = npl+numcoords+1 ! TODO: UNST-4328: success1 checking done later in readStructureFile().

           call get_value_or_addto_forcinglist(str_ptr, 'valveRelativeOpening', longculverts(nlongculverts)%valve_relative_opening, st_id, &
             ST_LONGCULVERT,network%forcinglist, success)
           if (.not. success) then
             call SetMessage(LEVEL_ERROR, 'valveRelativeOpening not found for long culvert: '// trim(st_id) )
           endif
           
           call prop_get(str_ptr, '', 'branchId', longculverts(nlongculverts)%branchId, success)
           if (success) then
              call prop_get(str_ptr, '', 'csDefId', csDefId, success)
              if (.not. success) then
                call SetMessage(LEVEL_ERROR, 'csDefId not found for long culvert: '// trim(st_id) )
              endif
   
              newculverts = .true.
           end if

           if ( newculverts) then
           longculverts(nlongculverts)%numlinks = numcoords+1
           allocate(longculverts(nlongculverts)%netlinks(numcoords+1))
           allocate(longculverts(nlongculverts)%flowlinks(numcoords+1))
           longculverts(nlongculverts)%flowlinks = -999
           
             call addlongculvertcrosssections(network, longculverts(nlongculverts)%branchid, csDefId, longculverts(nlongculverts)%bl, iref)  
             if (iref > 0) then
              ! Use top (#2) of tabulated cross section definition to derive width and height
              longculverts(nlongculverts)%width          = network%CSDefinitions%Cs(iref)%totalwidth(2)
              longculverts(nlongculverts)%height         = network%CSDefinitions%Cs(iref)%height(2)
              longculverts(nlongculverts)%ifrctyp        = network%CSDefinitions%Cs(iref)%frictiontype(1)
              longculverts(nlongculverts)%friction_value = network%CSDefinitions%Cs(iref)%frictionvalue(1)
             endif
           else !these values are no longer in the structures.ini after conversion
           longculverts(nlongculverts)%numlinks = numcoords-1
           allocate(longculverts(nlongculverts)%netlinks(numcoords-1))
           allocate(longculverts(nlongculverts)%flowlinks(numcoords-1))
           longculverts(nlongculverts)%flowlinks = -999
             call prop_get(str_ptr, '', 'allowedFlowdir', txt, success)
             if (.not. success) then
               TXT = 'both'
             endif
             longculverts(nlongculverts)%allowed_flowdir = allowedFlowDirToInt(txt)

             call prop_get(str_ptr, '', 'width', longculverts(nlongculverts)%width, success)
             if (.not. success) then
               call SetMessage(LEVEL_ERROR, 'width not found for long culvert: '// st_id )
             endif
             call prop_get(str_ptr, '', 'height', longculverts(nlongculverts)%height, success)
             if (.not. success) then
               call SetMessage(LEVEL_ERROR, 'height not found for long culvert: '// st_id )
             endif
             call prop_get(str_ptr, '', 'frictionType', typestr, success)
             if (.not. success) then
               longculverts(nlongculverts)%ifrctyp = -999
             else
               call frictionTypeStringToInteger(typestr, longculverts(nlongculverts)%ifrctyp)
             endif
             call prop_get(str_ptr, '', 'frictionValue', longculverts(nlongculverts)%friction_value, success)
             if (.not. success) then
               call SetMessage(LEVEL_ERROR, 'frictionValue not found for long culvert: '// st_id )
             endif
           end if
             if (.not. success) then
               ! Some error during reading: decrement counter to ignore this long culvert.
               nlongculverts = nlongculverts - 1
             end if

         end if

       end do
       if (.not. newculverts) then

         allocate(links(npl))
         call make1D2DLongCulverts(xpl, ypl, zpl, npl, links)
         call restorepol()
         istart = 1
         do i = nlongculverts0+1, nlongculverts
           longculverts(i)%netlinks = links(istart:istart+longculverts(i)%numlinks-1)
           istart = istart+longculverts(i)%numlinks+2
         enddo
       endif

       call tree_destroy(strs_ptr)

   end subroutine loadLongCulvertsAsNetwork


   !> Finalizes some necessary network administration after all long culverts have been read.
   !! Actual reading is done in other subroutine loadLongCulvertsAsNetwork().
   subroutine finalizeLongCulvertsInNetwork()
      use network_data
      use gridoperations
      integer :: Lnet, i, ilongc

      ! NOTE: IF setnodadm() is again called after this subroutine has completed, with more netlink permutations,
      !! Then the longculvert()%netlinks array is incorrect. This can be fixed if we change our approach
      !! to always using closeto1dnetlink() calls in the longCulvertsToProfs() subroutine, instead. For now, we are safe, though.

      call setnodadm(0)
      !call findcells(0)
      !call find1dcells()
      ! Netlink numbers have probably been permuted by setnodadm, so also update netlinks.
      do ilongc = 1, nlongculverts
         do i = 1, longculverts(ilongc)%numlinks
            ! Netlink numbers have probably been permuted after the initial long culvert reading, so also update netlinks.
            Lnet = Lperminv(longculverts(ilongc)%netlinks(i))
            longculverts(ilongc)%netlinks(i) = Lnet
         enddo
      enddo
   end subroutine finalizeLongCulvertsInNetwork


   !> Reallocates a given longculvert array to larger size.
   !! Any existing longculvert data is copied into the new array.
   subroutine reallocLongCulverts(lcs, newsize)
      ! Modules

      implicit none

      ! Input/output parameters
      type(t_longculvert), allocatable, intent(inout) :: lcs(:)  !< The existing longculvert array.
      integer,                          intent(in   ) :: newsize !< The desired new size.

      ! Local variables
      type(t_longculvert), allocatable :: oldlcs(:)
      integer :: oldsize, i

      ! Program code

      if (allocated(lcs)) then
         oldsize = size(lcs)
      else
         oldsize = 0
      end if

      if (newsize > oldsize) then
         allocate(oldlcs(oldsize))
         do i=1,oldsize
            oldlcs(i) = lcs(i)
         end do

         if (allocated(lcs)) then
            deallocate(lcs)
         end if
         allocate(lcs(newsize))
         do i=1,oldsize
            lcs(i) = oldlcs(i)
         end do
      endif

   end subroutine reallocLongCulverts


   !> Initializes the cross section administration for long culverts in prof1d and other relevant flow geometry arrays.
   !! * Sets netlink numbers and flowlink numbers.
   !> * Fills for the corresponding flow links the bedlevels, bobs and prof1d data.
   subroutine longculvertsToProfs(skiplinks)
      use network_data
      use m_flowgeom

      logical, intent(in   ) :: skiplinks     !< Skip determining the flow links or not

      integer :: Lnet, Lf, i, ilongc, k1, k2

      !
      ! If we have retrieved the flowlinks and so on via the cache file,
      ! skip this loop
      !
      if ( .not. skiplinks ) then
         do ilongc = 1, nlongculverts
            do i = 1, longculverts(ilongc)%numlinks
               if (longculverts(ilongc)%flowlinks(i) < 0) then
                  ! Flow links have not yet been initialized, this is the first call.
                  ! Netlink numbers have been set correctly in finalizeLongCulvertsInNetwork() already.
                  Lnet = longculverts(ilongc)%netlinks(i)
                  longculverts(ilongc)%flowlinks(i) = lne2ln(Lnet)
               endif
            enddo

            if (longculverts(ilongc)%numlinks <= 0) then
               ! Skip this long culvert when it is not active on this grid
               cycle
            end if

            ! Set upstream flow node
            Lf = abs(longculverts(ilongc)%flowlinks(1))
            if (ln(1,Lf) <= ndx2d) then
               longculverts(ilongc)%flownode_up = ln(1,Lf)
            else
               longculverts(ilongc)%flownode_up = ln(2,Lf)
            end if
            ! Set downstream flow node
            Lf = abs(longculverts(ilongc)%flowlinks(longculverts(ilongc)%numlinks))
            if (ln(2,Lf) <= ndx2d) then
               longculverts(ilongc)%flownode_dn = ln(2,Lf)
            else
               longculverts(ilongc)%flownode_dn = ln(1,Lf)
            end if
         enddo
      endif

      if (newculverts) then
         do ilongc = 1, nlongculverts
            do i = 2, longculverts(ilongc)%numlinks-1
               Lf = abs(longculverts(ilongc)%flowlinks(i))
               if (Lf > 0) then
                  k1 = ln(1,Lf)
                  k2 = ln(2,Lf)
                  bob(1, Lf) = longculverts(ilongc)%bl(i-1)
                  bob(2, Lf) = longculverts(ilongc)%bl(i)
                  if (k1 > ndx2d) then !if 1d link
                     bl(k1) = bob(1,Lf)
                  else
                     bl(k1) = min(bl(k1), bob(1,Lf)) 
                  end if
                  if (k2 > ndx2d) then
                     bl(k2) = bob(2,Lf)
                  else
                     bl(k2) = min(bl(k2), bob(2,Lf))
                  end if
               end if
            enddo
            Lf = abs(longculverts(ilongc)%flowlinks(1))
            if (Lf > 0) then
               wu(Lf) = longculverts(ilongc)%width
               prof1D(1,Lf)  = wu(Lf)
               prof1D(2,Lf)  = longculverts(ilongc)%height
               prof1D(3,Lf)  =  -2 
               bob(1, Lf) = longculverts(ilongc)%bl(1)
               bob(2, Lf) = bl(ln(2,Lf))
            end if
      
            Lf = abs(longculverts(ilongc)%flowlinks(longculverts(ilongc)%numlinks))
            if (Lf > 0) then
               wu(Lf) = longculverts(ilongc)%width
               prof1D(1,Lf)  = wu(Lf)
               prof1D(2,Lf)  = longculverts(ilongc)%height
               prof1D(3,Lf)  =  -2 
               bob(1, Lf) = longculverts(ilongc)%bl(longculverts(ilongc)%numlinks-1)
               bob(2, Lf) = bl(ln(2,Lf))
            end if
         enddo
      else !voor nu houden we de oude implementatie intact
         do ilongc = 1, nlongculverts
            do i = 1, longculverts(ilongc)%numlinks
               Lf = abs(longculverts(ilongc)%flowlinks(i))
               !if (kcu(lf) == 1) then ! TODO: UNST-5433: change when 1d2d links are *extra* in addition to culvert polyline
               k1 = ln(1,Lf)
               k2 = ln(2,Lf)
               
               bob(1, Lf) = longculverts(ilongc)%bl(i)
               bob(2, Lf) = longculverts(ilongc)%bl(i+1)
               if (k1 > ndx2d) then
                  bl(k1) = bob(1,Lf)
               else ! k1 = 2d point
               bl(k1) = min(bl(k1), bob(1,Lf))
               end if
               
               if (k2 > ndx2d) then
                  bl(k2) = bob(2,Lf)
               else
               bl(k2) = min(bl(k2), bob(2,Lf))
               end if
                
               wu(Lf) = longculverts(ilongc)%width
               prof1D(1,Lf)  = wu(Lf)
               prof1D(2,Lf)  = longculverts(ilongc)%height
               prof1D(3,Lf)  =  -2                                      ! for now, simple rectan            
            enddo
         enddo
      endif

   end subroutine longculvertsToProfs

   !> Fill frcu and icrctyp for the corresponding flow link numbers of the long culverts
   subroutine setFrictionForLongculverts()
      use m_flow
      implicit none

      integer :: LL, ilongc, Lf

      do ilongc = 1, nlongculverts
         do LL = 1, longculverts(ilongc)%numlinks
            Lf = abs(longculverts(ilongc)%flowlinks(LL))
            if (Lf > 0) then
               if (longculverts(ilongc)%ifrctyp > 0) then
                  ifrcutp(Lf) = longculverts(ilongc)%ifrctyp
                  if (longculverts(ilongc)%friction_value > 0) then
                     frcu(Lf) = longculverts(ilongc)%friction_value
                  endif
               end if
            end if
         enddo
      enddo

   end subroutine setFrictionForLongculverts

   !> In case  the valve_relative_area < 1 the flow area
   !! at the first link is reduced by valve_relative_area
   subroutine reduceFlowAreaAtLongculverts()
      use m_flow

      implicit none

      integer i, L

      do i = 1, nlongculverts
         if (longculverts(i)%numlinks > 0) then
           if (newculverts) then
              L = abs(longculverts(i)%flowlinks(2))
           else
              L = abs(longculverts(i)%flowlinks(1))
           endif
           if (L > 0) then
              au(L) = longculverts(i)%valve_relative_opening * au(L)
           end if
         end if
      enddo

   end subroutine reduceFlowAreaAtLongculverts

   !> Gets the pointer of the valve opening height for a given culvert structure.
   !! If the type of the given structure is not culvert, then it gets a null pointer
   !! This pointer points directly to the %culvert%valveOpening.
   type(c_ptr) function get_valve_relative_opening_c_loc(lculv)
      type(t_longculvert), intent(in), target :: lculv

      get_valve_relative_opening_c_loc = c_loc(lculv%valve_relative_opening)

  end function get_valve_relative_opening_c_loc
  
     !> Generates 1D netlinks and 1D2D connections for a (multiple) new long culvert(s).
   !! The new net links get added to the active network_data.
   !! The culvert(s) must be specified by a polyline with x/y/z coordinates.
   !! In case of multiple culverts, the coordinate arrays must have missing value
   !! (dmiss) separators between each polyline.
   subroutine make1D2DLongCulverts(xplCulv, yplCulv, zplCulv, nplCulv, linksCulv)
   use m_missing
   use m_polygon
   use geometry_module
   use m_alloc
   use network_data
   use m_cell_geometry
   use m_samples
   use m_sferic, only: jsferic, jasfer3D
   use gridoperations
   use unstruc_channel_flow, only: network
   implicit none
   
   double precision, intent(in   ) :: xplCulv(:) !< x-coordinates of the polyline of one or more culverts.
   double precision, intent(in   ) :: yplCulv(:) !< y-coordinates of the polyline of one or more culverts.
   double precision, intent(in   ) :: zplCulv(:) !< z-coordinates of the polyline of one or more culverts.
   integer,          intent(in   ) :: nplCulv    !< Number of points in the culvert polyline.
   integer,          intent(  out) :: linksCulv(:) !< Resulting netlink numbers of one or more culverts.
   
   integer :: j, jpoint, jstart, jend, k1, k2, L, ipoly
   double precision :: x1, y1, z1, x2, y2, z2

   ipoly  = 0
   jpoint = 1
   do while (jpoint < nplCulv)

      ! Find next start and end point in pli set:
      call get_startend(nplCulv-jpoint+1, xplCulv(jpoint:nplCulv), yplCulv(jpoint:nplCulv), jstart, jend, dmiss)
      jstart = jstart+jpoint-1
      jend   = jend  +jpoint-1

      if (jstart >= jend) then
         call mess(LEVEL_WARN, 'generateLongCulverts: No valid start+end point found in polyline.')
         !goto 888
      end if

      ipoly = ipoly+1

      ! Starting point:
      x1 = xplCulv(jstart)
      y1 = yplCulv(jstart)
      z1 = zplCulv(jstart)
      call setnewpoint(x1, y1, z1, k1)
      zk(k1) = z1

      do j=jstart+1,jend
         x2 = xplCulv(j)
         y2 = yplCulv(j)
         z2 = zplCulv(j)
         call setnewpoint(x2, y2, z2, k2)
         zk(k2) = z2

         if (j == jstart+1 .or. j == jend) then
            kn3typ = 5 ! 1D2D netlink type for entry-side and exit-side.
         else
            kn3typ = 1 ! purely 1D netlink type for inner pipe pieces (if any).
         end if
         call connectdbn(k1,k2,linksCulv(j-1))
         k1 = k2
      end do

      !           advance pointer
      jpoint = jend+2
   end do

   
   ! NOTE: here we do not explicitly check whether end points lie inside
   ! 2D grid cells, for performance reasons.

   ! TODO: UNST-4334: Detect whether link is already there
        !xc = 0.5d0*(x1+x2)
        !yc = 0.5d0*(y1+y2)
        !CALL CLOSETO1Dnetlink(Xc,Yc,LS,XLS,YLS,dum, 0)
        !if (Ls > 0) then
   
   ! Successful exit
   return

888 continue
   ! Something went wrong.

  end subroutine make1D2DLongCulverts
      !> Generates 1D netlinks and 1D2D connections for a (multiple) new long culvert(s).
   !! The new net links get added to the active network_data.
   !! The culvert(s) must be specified by a polyline with x/y/z coordinates.
   !! In case of multiple culverts, the coordinate arrays must have missing value
   !! (dmiss) separators between each polyline.
   subroutine convert1D2DLongCulverts(xplCulv, yplCulv, zplCulv, nplCulv, linksCulv)
   use m_missing
   use m_polygon
   use geometry_module
   use m_alloc
   use network_data
   use precision_basics, only: comparereal
   use m_samples
   use m_save_ugrid_state
   use m_sferic, only: jsferic, jasfer3D
   use gridoperations
   use unstruc_channel_flow, only: network
   use m_cell_geometry, only: xz, yz
   use m_GlobalParameters, only: flow1d_eps10
   
   implicit none

   double precision, intent(in   ) :: xplCulv(:) !< x-coordinates of the polyline of one or more culverts.
   double precision, intent(in   ) :: yplCulv(:) !< y-coordinates of the polyline of one or more culverts.
   double precision, intent(in   ) :: zplCulv(:) !< z-coordinates of the polyline of one or more culverts.
   integer,          intent(in   ) :: nplCulv    !< Number of points in the culvert polyline.
   integer,          intent(  out) :: linksCulv(:) !< Resulting netlink numbers of one or more culverts.

   integer :: j, jpoint, jstart, jend, k1, k2, L, ipoly, numculvertpoints, currentbranchindex, newnodeindex, newedgeindex, newgeomindex, newnetnodeindex, node1d2d
   integer, allocatable :: temp
   double precision :: x1, y1, z1, x2, y2, z2, pathlength, pathdiff
   character(len=5) :: ipolychar, nodechar
   
   if (len_trim(meshgeom1d%meshname) > 0) then
      ! This is to allow more than one call to loadNetwork/unc_read_net_ugrid. Remove any previously read network state.
      call default_save_ugrid_state()
      meshgeom1d%nbranches = 0
      meshgeom1d%ngeometry = 0
      meshgeom1d%nnodes  = 0
      meshgeom1d%numedge = 0
      meshgeom1d%numnode = 0   
   endif  
   
   !mesh niveau
   newedgeindex = meshgeom1d%numedge +1
   newnodeindex = meshgeom1d%numnode +1
   !network niveau
   newnetnodeindex = meshgeom1d%nnodes + 1
   currentbranchindex = meshgeom1d%nbranches
   !geometry niveau
   newgeomindex = meshgeom1d%ngeometry + 1

   !First determine number of branches that require a culvert. 
   ipoly  = 0
   jpoint = 1
   do while (jpoint < nplCulv)
      ! Find next start and end point in pli set:
      call get_startend(nplCulv-jpoint+1, xplCulv(jpoint:nplCulv), yplCulv(jpoint:nplCulv), jstart, jend, dmiss)
      jstart = jstart+jpoint-1
      jend   = jend  +jpoint-1
      jpoint = jend+2
      ipoly = ipoly+1
      !minimum of 2 1d2d links = 4 points, so 1d network only exists with 5 points or more
      !if(jend-jstart+1 >= 4) then
         meshgeom1d%numnode = meshgeom1d%numnode+jend-jstart+1
         meshgeom1d%numedge = meshgeom1d%numedge+jend-jstart
         meshgeom1d%ngeometry = meshgeom1d%ngeometry+jend-jstart+1
         meshgeom1d%nbranches = meshgeom1d%nbranches + 1
         meshgeom1d%nnodes    = meshgeom1d%nnodes + 2 ! only 2 network nodes per branch       
      !endif
   enddo
   
   call reallocP(meshgeom1d%nbranchorder          , meshgeom1d%nbranches,          keepexisting = .true., fill = -999)
   call reallocP(meshgeom1d%nbranchgeometrynodes  , meshgeom1d%nbranches,          keepexisting = .true., fill = -999)
   call reallocP(meshgeom1d%nedge_nodes           ,(/ 2, meshgeom1d%nbranches /),  keepexisting = .true.)
   call reallocP(meshgeom1d%nbranchlengths        , meshgeom1d%nbranches,          keepexisting = .true., fill = -999d0)
   call realloc (nbranchids                       , meshgeom1d%nbranches,          keepexisting = .true., fill = '')

   call reallocP(meshgeom1d%nnodex        ,meshgeom1d%nnodes,  keepexisting = .true., fill = -999d0)
   call reallocP(meshgeom1d%nnodey        ,meshgeom1d%nnodes,  keepexisting = .true., fill = -999d0)
   allocate(nnodeids(meshgeom1d%nnodes))
   call reallocP(meshgeom1d%nodebranchidx ,meshgeom1d%numnode,   keepexisting = .true., fill = -999)
   call reallocP(meshgeom1d%nodeoffsets   ,meshgeom1d%numnode,   keepexisting = .true., fill = -999d0)
   call reallocP(meshgeom1d%edgebranchidx ,meshgeom1d%numedge,   keepexisting = .true., fill = -999)
   call reallocP(meshgeom1d%edgeoffsets   ,meshgeom1d%numedge,   keepexisting = .true., fill = -999d0)
   call reallocP(meshgeom1d%ngeopointx    ,meshgeom1d%ngeometry, keepexisting = .true., fill = -999d0)
   call reallocP(meshgeom1d%ngeopointy    ,meshgeom1d%ngeometry, keepexisting = .true., fill = -999d0)

   jpoint = 1
   ipoly  = 0
   do while (jpoint < nplCulv)

      ! Find next start and end point in pli set:
      call get_startend(nplCulv-jpoint+1, xplCulv(jpoint:nplCulv), yplCulv(jpoint:nplCulv), jstart, jend, dmiss)
      jstart = jstart+jpoint-1
      jend   = jend  +jpoint-1
      if (jstart >= jend) then
         call mess(LEVEL_WARN, 'generateLongCulverts: No valid start+end point found in polyline.')
         !goto 888
      end if

      ipoly = ipoly+1
      numculvertpoints = jend+1 - jstart

     ! if (jend-jstart+1 >= 4) then 
         currentbranchindex = currentbranchindex+1
         write(ipolychar, '(I0)') currentbranchindex
         nbranchids(currentbranchindex) = 'BR_longCulvert_'//trim(ipolychar)
         !net nodes are start + end points of 1d branch
         meshgeom1d%nnodex(newnetnodeindex:newnetnodeindex+1) = (/ xplCulv(jstart), xplCulv(jend) /) 
         meshgeom1d%nnodey(newnetnodeindex:newnetnodeindex+1) = (/ yplCulv(jstart), yplCulv(jend) /)
         meshgeom1d%nedge_nodes(1:2, currentbranchindex) = (/ newnetnodeindex, newnetnodeindex+1 /) 
         write(nodechar, '(I0)') newnetnodeindex
         nnodeids(newnetnodeindex)   = 'BR_longCulvert_'//trim(ipolychar)//'_node_'//trim(nodechar)
         write(nodechar, '(I0)') newnetnodeindex+1
         nnodeids(newnetnodeindex+1) = 'BR_longCulvert_'//trim(ipolychar)//'_node_'//trim(nodechar)
         meshgeom1d%nbranchgeometrynodes(currentbranchindex) = numculvertpoints
         meshgeom1d%ngeopointx(newgeomindex:newgeomindex+numculvertpoints-1) = xplCulv(jstart:jend)
         meshgeom1d%ngeopointy(newgeomindex:newgeomindex+numculvertpoints-1) = yplCulv(jstart:jend)
         newgeomindex = newgeomindex + numculvertpoints
         newnetnodeindex = newnetnodeindex + 2
     ! end if

      ! Starting point:
      call incells(xpl(jstart),ypl(jstart), node1d2d)
      x1 = XZ(node1d2d)
      y1 = YZ(node1d2d)
      z1 = zpl(jstart)
      call setnewpoint(x1, y1, z1, k1)
      zk(k1) = z1

      pathlength = 0d0
      pathdiff = 0d0
      do j=jstart,jend
         x2 = xplCulv(j)
         y2 = yplCulv(j)
         z2 = zplCulv(j)
         call setnewpoint(x2, y2, z2, k2)
         zk(k2) = z2  
            
        if (j == jstart ) then
            kn3typ = 5 ! 1D2D netlink type for entry-side and exit-side.
        else
        !edge      
        pathdiff = dbdistance( x2, y2,xplCulv(j-1), yplCulv(j-1),jsferic, jasfer3D, dmiss)
        kn3typ = 1 ! purely 1D netlink type for inner pipe pieces (if any).
        meshgeom1d%edgebranchidx(newedgeindex) = currentbranchindex
        meshgeom1d%edgeoffsets(newedgeindex) =pathlength + pathdiff/2 
        newedgeindex = newedgeindex + 1 
        endif
        !node             
        meshgeom1d%nodebranchidx(newnodeindex) = currentbranchindex     
        pathlength = pathlength +pathdiff
        meshgeom1d%nodeoffsets(newnodeindex) = pathlength
        newnodeindex = newnodeindex + 1
        call connectdbn(k1,k2,linksCulv(j))
        k1 = k2
      end do
      
      ! end point:
      meshgeom1d%nbranchlengths(currentbranchindex) = pathlength
      kn3typ = 5 
      call incells(xpl(jend),ypl(jend), node1d2d)
      
      x1 = XZ(node1d2d)
      y1 = YZ(node1d2d)
      z1 = zpl(jend)
      if (comparereal(xpl(jend),xz(node1d2d),flow1d_eps10)==0 .and. comparereal(ypl(jend),yz(node1d2d),flow1d_eps10)==0) then
        x1 = x1 + .01
      endif
      call setnewpoint(x1, y1, z1, k1)
      zk(k1) = z1
      call connectdbn(k2,k1,linksCulv(jend+1))
      
      !advance pointer
      jpoint = jend+2
   end do
   return

888 continue
   ! Something went wrong.

   end subroutine convert1D2DLongCulverts
   
   !> Add new cross section locations on a particular branch in the network.
   !! The cross section definition (defining the long culvert's shape)
   !! must already have been read from file.
   subroutine addlongculvertcrosssections(network, branchId, csdefId, zpl, iref)
   use m_hash_search
   use m_readCrossSections
   use m_network
   type(t_network), intent(inout)   :: network   !< Network structure
   character(len=IdLen),          intent(in   ) :: branchId  !< Branch id on which to place the cross section
   character(len=IdLen),          intent(in   ) :: csdefId   !< Id of cross section definition
   double precision, allocatable, intent(in   ) :: zpl(:)    !< (numlinks+1) Bed level on the long culvert support points
   integer,                       intent(  out) :: iref      !< Index of reference cross section definition (if csdefId was found)

   integer                        :: istat
   integer                        :: numstr
   integer                        :: i, j, k
   integer                        :: inext
   integer                        :: indx
   logical                        :: success
   type(t_CrossSection), pointer  :: pCrs
   type(t_CSType), pointer        :: pCrsDef
   ! character(len=Charln)          :: binfile
   logical                        :: file_exist
   integer                        :: pos, ibin
   integer                        :: numcrs
   character(len=5)               :: kchar

       indx = hashsearch(network%brs%hashlist, branchId)
       iref = hashsearch(network%CSDefinitions%hashlist, csdefId)
   if (indx > 0 .and. iref > 0 ) then
     ! This code assumes 1 gridpoint per culvert coordinate,
     ! which means the culvert network branches cannot be modified after converting
       do k = 1, network%brs%branch(indx)%gridpointscount

       if (network%crs%count+1 > network%crs%size) then
         call realloc(network%crs)
       endif
       inext = network%crs%count+1
       pCrs => network%crs%cross(inext)
       write(kchar, '(I0)') k
       pCrs%csid = trim(branchId)//'_'//trim(kchar)
       pCrs%branchid = indx
       pCrs%bedLevel = 0.0d0
       pCrs%shift = zpl(k) !number of gridpoints in branch should match zpl+2!!
       pCrs%chainage = network%brs%branch(indx)%gridpointschainages(k)
       call finalizeCrs(network,pCrs,iref,inext)
     enddo
   endif

   end subroutine addlongculvertcrosssections

   !> Fills in flowlink numbers for a given longculvert.
   !! Note 1: This long culvert is considered invalid if its starting node, or ending node, is outside the global network.
   !! Note 2: In a parallel simulation, the flowlink number gets 0 if the flowlink is not on the current subdomain.
   !! Note 3: In a parallel simulation, it can happen that the starting (ending) node of the polylin of the long culvert is
   !! not on the current subdomain. In this case, the starting (ending) node ON the current subdomain is
   !! found firstly, and then search flowlinks for the interior polyline points.
   !! TODO (UNST-6073): currently it does not support the situation when, in a parallel simulation, the polyline enters
   !! one subdomain, then leaves, and then enters again.
   subroutine find1d2dculvertlinks(network,longculvert, numcoords)

   use m_cell_geometry, only: xz, yz
   use m_network
   use m_flowgeom
   use m_GlobalParameters, only: INDTP_1D, INDTP_2D, INDTP_ALL
   use precision_basics, only: comparereal
   use m_flowparameters, only: eps10
   use m_partitioninfo, only: jampi,reduce_int_max
   use kdtree2Factory
   use m_hash_search
   
   type(t_network) , intent(inout)    :: network      !< Network structure
   integer, intent(in)                :: numcoords    !< number of polyline coordinates
   type(t_longculvert), intent(inout) :: longculvert  !< A givin long culvert
   integer :: i, j, othernode, nodenum, linknum, linkabs, is, ie, jafounds, jafounde
   integer, allocatable :: inode(:), inodeGlob(:), jnode(:)


   associate(xpl => longculvert%xcoords, ypl => longculvert%ycoords)

     longculvert%flowlinks = 0
     jafounds = 0   ! Found the starting node or not
     jafounde = 0   ! Found the ending node or not
     is = 1         ! the starting node of the polyline
     ie = numcoords ! the ending node of the polyline

     ! Find the flownode numbers for the starting and ending points of the long culvert polyline
     call realloc(inode,     2, keepExisting=.false.,fill=0)
     call realloc(inodeGlob, 2, keepExisting=.false.,fill=0)

     i = hashsearch(network%brs%hashlist, longculvert%branchId)
     !Find the last 1D node of the branch
     if (i > 0 .and. network%BRS%size >= i) then
        inode(1) = network%BRS%Branch(i)%FROMNODE%GRIDNUMBER
        inode(2) = network%BRS%Branch(i)%TONODE%GRIDNUMBER

        !find Flownode connected to this node by 1D2D link
        do j = 1,2
           do i = 1, nd(inode(j))%lnx
              linknum = nd(inode(j))%ln(i)
              linkabs = abs(linknum)
              if (kcu(linkabs) == 5) then
                 inode(j) = ln(1,linkabs) + ln(2,linkabs) - inode(j)
                 exit
              endif
           enddo
        enddo
     endif
     
     inodeGlob(1:2) = inode(1:2)
     if (jampi > 0) then
        ! Communicate inode in parallel run to get inodeGlob
        call reduce_int_max(2, inodeGlob)
     end if

     if (inodeGlob(1) <= 0 .or. inodeGlob(2) <= 0) then 
        ! The long culvert is not valid if its starting or ending node is outside the global network
        longculvert%numlinks = 0
        call mess(LEVEL_WARN, 'find1d2dculvertlinks: a long culvert is not valid if its starting or ending node is outside the global network.')
        return
     else ! This long culvert is valid on the current domain
        ! check the starting node
        if (inode(1) > 0) then ! The starting node is inside the current domain
           longculvert%flownode_up = inode(1)
           nodenum = inode(1) ! For the later search
           jafounds = 1
        else
           ! Find the first known flow node in the current partition (if 2D flow node was not found outside of the loop already)
           call realloc(jnode, 1, keepExisting=.false.,fill=0)
           do j = is+1, ie-1
              call find_flowcells_kdtree(treeglob,1,xpl(j), ypl(j),jnode,1,INDTP_1D, ierror)
              if (ierror == 0 .and. jnode(1) > 0) then
                 nodenum = jnode(1) ! For the later search
                 is = j ! this will be the starting node of the long culvert in current domain
                 jafounds = 1
                 exit
              end if
           end do
        end if

        ! check the ending node
        if (inode(2) > 0) then ! The ending node is inside the current domain
           longculvert%flownode_dn = inode(2)
           jafounde = 1
        else
           ! Find the last known flow node in the current partition (if 2D flow ndoe was not found outside of the loop already)
           call realloc(jnode, 1, keepExisting=.false.,fill=0)
           do j = ie-1, is+1, -1
              call find_flowcells_kdtree(treeglob,1,xpl(j), ypl(j),jnode,1,INDTP_1D, ierror)
              if (ierror == 0 .and. jnode(1) > 0) then
                 ie = j ! this will be the ending node of the long culvert in current domain
                 jafounde = 1
                 exit
              end if
           end do
        end if
     end if

     if (jafounds == 1 .and. jafounde == 1) then
         ! For the interior polyline points
         do j = is, ie+1 ! j is link index, or , right node index
            if (j > is) then
               nodenum = othernode
            endif
            if (nodenum > 0) then
               do i = 1, nd(nodenum)%lnx
                  linknum = nd(nodenum)%ln(i)
                  linkabs = abs(linknum)
                  othernode = ln(1,linkabs) + ln(2,linkabs) - nodenum

                  if (j <= ie) then
                     if ((kcu(linkabs) == 1 .or. kcu(linkabs) == 5) .and. ( comparereal(xz(othernode),xpl(j),eps10) == 0 .and. comparereal(yz(othernode),ypl(j),eps10) == 0)) then
                        longculvert%flowlinks(j) = -1*linknum
                        exit
                     end if
                  else if (kcu(linkabs) == 5) then ! 1D2D link
                     longculvert%flowlinks(j) = -1*linknum
                     exit
                  endif
               enddo
            end if
         enddo
     else
        continue
     end if
   end associate
  end subroutine

  subroutine setLongCulvert1D2DLinkAngles(i)
  use m_flowgeom, only: csu, snu
  integer, intent(in) :: i !index of current long culvert (this function is called in a loopt)
  
  integer :: L 
  
  if (longculverts(i)%numlinks >= 3) then
    L = abs(longculverts(i)%flowlinks(1))
    if (L > 0) then
      csu(L) = csu(abs(longculverts(i)%flowlinks(2)))
      snu(L) = snu(abs(longculverts(i)%flowlinks(2)))
    endif
    L = abs(longculverts(i)%flowlinks(longculverts(i)%numlinks))
    if (L > 0) then
      csu(L) = csu(abs(longculverts(i)%flowlinks(longculverts(i)%numlinks-1)))
      snu(L) = snu(abs(longculverts(i)%flowlinks(longculverts(i)%numlinks-1)))
    endif
  endif

  end subroutine

end module m_longculverts
