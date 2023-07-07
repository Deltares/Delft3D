module m_readSpatialData
!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
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
!-------------------------------------------------------------------------------
   
   use m_GlobalParameters
   use m_network
   use m_spatial_data
   use m_tables
   use m_branch
   use properties
   use m_hash_search
   
   implicit none

   private

   public spatial_data_reader
 
contains
!> Read spatial data from input file
   subroutine spatial_data_reader(isp, spData, brs, inputfile, default, itype, interpolateOverBranches)

      type(t_spatial_dataSet) , intent(inout)  :: spData                   !< Spatial data set
      type(t_branchSet)       , intent(in   )  :: brs                      !< Branches
      character(len=*)        , intent(in   )  :: inputfile                !< Name of the input file
      integer                 , intent(in   )  :: itype                    !< Quantity type
      logical                 , intent(in   )  :: interpolateOverBranches  !< Flag indicates whether interpolation over branches is required
      double precision        , intent(inout)  :: default                  !< Default/global value
      integer                 , intent(  out)  :: isp                      !< Index in spatial data set
      
      type(t_spatial_data), pointer             :: pspData
      type(tree_data), pointer                  :: md_ptr
      integer                                   :: i
      integer                                   :: ind
      integer                                   :: ibr
      integer                                   :: istat
      integer                                   :: count
      integer, allocatable                      :: numLevels(:)
      integer                                   :: maxLevels
      integer                                   :: numlevs
      integer                                   :: length
      logical                                   :: success
      character(len=idlen)                      :: branchid
      double precision, allocatable             :: levels(:,:)
      double precision, allocatable             :: rough(:)
      logical                                   :: branch_error = .false.

      type(t_ptable), dimension(:), allocatable :: tbls
   
      spData%count = spData%count + 1
      isp = spData%count 
      if (spData%count > spData%size) then
         call realloc(spData)
      endif

      pspData => spData%quant(isp)
      if (itype > 0) then
         pspData%quantity = itype
      else
         pspData%quantity = -1
      endif

      call tree_create(trim(inputfile), md_ptr, maxlenpar)
      call prop_file('ini',trim(inputfile),md_ptr,istat)
      ! look for global value in spatial data file. If available use this value for 
      ! default, otherwise use default value as global value
      call prop_get_double(md_ptr, 'Content', 'globalValue', default, success)
      pspData%default = default
   
      count = 0
      maxLevels = 1
   
      if (associated(md_ptr%child_nodes)) then
         length = size(md_ptr%child_nodes)
         do i = 1, length
            if (tree_get_name(md_ptr%child_nodes(i)%node_ptr) .ne. 'branchproperties') then
            
               if(tree_get_name(md_ptr%child_nodes(i)%node_ptr) .eq. 'definition') then
                  count = count+1
               endif
               cycle
            endif
            numLevs = 1
            call prop_get_integer(md_ptr%child_nodes(i)%node_ptr, '', 'numLevels', numlevs, success)
            if (numLevs > maxLevels) then
               maxLevels = numLevs
            endif
         enddo
      end if
   
      ! Get the different levels for each branch
      allocate (levels(maxLevels, brs%Count), rough(maxLevels))
      allocate (numLevels(brs%Count))
      allocate (tbls(count))
      do i = 1, count
         tbls(i)%p => null()
      enddo
   
      numLevels = -1
      levels = 0d0
   
      if (maxLevels /=1) then
         levels = 0d0
         do i = 1, length
            call prop_get_integer(md_ptr%child_nodes(i)%node_ptr, '', 'numLevels', numLevs, success)
            if (success .and. numLevs > 1) then
               call prop_get_string(md_ptr%child_nodes(i)%node_ptr, '', 'branchid', branchid, success)
               ibr = hashsearch(brs%hashlist, branchid)
               if (ibr < 1 .or. ibr > brs%count) then
                  msgbuf = 'Incorrect branchId found in file: '//trim(inputfile)//' branchid = '// trim(branchid)
                  call err_flush()
                  branch_error = .true.
                  cycle
               endif
               numLevels(ibr) = numLevs
               call prop_get_doubles(md_ptr%child_nodes(i)%node_ptr, '', 'levels', levels(1:numLevs,ibr), numLevs, success)
            endif
         enddo
      endif
 
      ! These arrays will be deallocated after retrieving roughness values for YZ-Profiles
      allocate(pspData%brIndex(count), pspData%chainage(count), pspData%valuesOnLocation(count))

      if (count > 0) then
         ind = 1
         do i = 1, length
            call prop_get_double(md_ptr%child_nodes(i)%node_ptr, '', 'chainage', pspData%chainage(ind), success)
            if (.not. success) then
               cycle
            endif
            call prop_get_string(md_ptr%child_nodes(i)%node_ptr, '', 'branchid', branchid, success)
            ibr = hashsearch(brs%hashlist, branchid)
            if (ibr < 1 .or. ibr > brs%count) then
               msgbuf = 'Incorrect branchId found in file: '//trim(inputfile)//' branchid = '// trim(branchid)
               call err_flush()
               branch_error = .true.
               cycle
            endif
            pspData%brIndex(ind) = ibr
            if (success) call prop_get_double(md_ptr%child_nodes(i)%node_ptr, '', 'value', pspData%valuesOnLocation(ind), success)
            if (.not.success) then
               numlevs = numLevels(ibr)
               if (numlevs <= 0) then
                  call SetMessage(LEVEL_ERROR, 'Inconsistent input found. On branch '//trim(branchid)//' numlevels is undefined, while in definition VALUE is missing.')
                  cycle
               endif
               call prop_get_doubles(md_ptr%child_nodes(i)%node_ptr, '', 'values', rough, numLevs, success)
               if (success) then
                  call settable(tbls(ind)%p, 0, levels(1:numLevs,ibr), rough, numlevs)
                  pspData%valuesOnLocation(ind) = rough(1)  ! fill with roughness from roughness table - to be updated later with waterlevel and discharge dependent rouhgness
               else
                  call SetMessage(LEVEL_ERROR, 'Inconsistent input found. On branch '//trim(branchid)//' key value and values is missing.')
               endif
            endif
            if (success) then
               ind = ind+1
            endif
         enddo
      endif
   
      pspData%numValues = max(0, count)
   
      if (branch_error) then
         call SetMessage(LEVEL_FATAL, 'Branch Error(s) found during reading Spatial Data.')
      endif
      
      call ValuesToGridPoints(spData%quant(isp), brs, tbls, interpolateOverBranches)
   
      ! cleaning up time
      if (allocated(levels))    deallocate(levels)
      if (allocated(numLevels)) deallocate(numLevels)
      if (allocated(rough))     deallocate(rough)
   
      if (allocated(tbls)) then
         do i = 1, count
            if (associated(tbls(i)%p)) then
               deallocate(tbls(i)%p)
            endif
         enddo
         deallocate(tbls)
      endif
   
   end subroutine spatial_data_reader

end module m_readSpatialData   
