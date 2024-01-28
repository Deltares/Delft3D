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

!> Finds indices of netcells that relate to structures.
!! The indices will be used when partitioning the mesh with METIS, by giving a special weight on the netcells.
!! As a result, structures will not intercross the partition boundaries
!! NOTE: This functionality ONLY supports when using "polylinefile" to specify the structure location
!! TODO: extend it to support other ways of specifying the structure location.
subroutine find_netcells_for_structures(size_istrucells, nstrucells, istrucells)
   use m_cell_geometry, only: xz, yz
   use m_structures
   use string_module
   use timespace_parameters
   use unstruc_files, only: resolvePath
   use TREE_STRUCTURES
   use MessageHandling
   use unstruc_model, only: md_structurefile_dir, md_structurefile
   use network_data
   use timespace, only: read1polylin
   use kdtree2Factory
   use m_sferic, only: jsferic
   use m_missing, only: dmiss
   use m_alloc
   implicit none

   integer,                             intent(in)    :: size_istrucells !< size of istrucells array
   integer,                             intent(inout) :: nstrucells      !< Number of the netcells that are related to structures
   integer, dimension(size_istrucells), intent(inout) :: istrucells      !< Indices of the netcells that are related to structures

   character(len=256)            :: plifile, qid, strid
   type(TREE_DATA), pointer      :: str_ptr
   logical                       :: success
   character(len=:), allocatable :: str_buf
   integer,          allocatable :: istrulinks(:), ipol_tmp(:)
   double precision, allocatable :: xpl_tmp(:), ypl_tmp(:), DSL_tmp(:)
   integer                       :: i, L, k, ii, nstr, loc_spec_type, nstrulinks, &
                                    npl_tmp, minp_tmp, maxpol_tmp, ierror, nstru_read1, nstru_read2

   nstrucells = 0
   if (len_trim(md_structurefile) == 0) then
      return
   end if

   call mess(LEVEL_INFO, 'Prepare for partitioning by METIS: Finding netcells that relate to structures...' &
                         //'The purpose is to avoid structures intercross partition boundaries. ' &
                         //'Currently only support for structures that are specified by polylines.')

   allocate(xpl_tmp(MAXPOLY))
   allocate(ypl_tmp(MAXPOLY))
   allocate(istrulinks(numl))
   allocate(ipol_tmp(numl))
   allocate(dSL_tmp(numl))
   xpl_tmp = 0d0
   ypl_tmp = 0d0
   istrulinks = 0
   ipol_tmp = 0
   dSL_tmp = 0d0
   minp_tmp = 0
   npl_tmp = 0
   nstrulinks = 0
   nstru_read1 = 0
   nstru_read2 = 0

   !! Get the structure location infomation from the structure file and put them in polyline arrays
   nstr = tree_num_nodes(strs_ptr) ! structure file tree
   do i=1,nstr
      plifile = ''
      qid = ''
      success = .true.

      str_ptr => strs_ptr%child_nodes(i)%node_ptr

      if (.not. strcmpi(tree_get_name(str_ptr), 'Structure')) then
         ! Only read [Structure] blocks, skip any other (e.g., [General]).
         cycle
      end if
      nstru_read1 = nstru_read1 + 1

      strid = ' '
      call prop_get_string(str_ptr, '', 'id', strid, success)
      if (.not. success .or. len_trim(strid) == 0) then
         write(msgbuf, '(a,i0,a)') 'Required field ''id'' missing in structure #', i, '.'
         call msg_flush()
         cycle
      end if

      call prop_get_alloc_string(str_ptr, '', 'polylinefile', str_buf, success)
      if (success) then
         loc_spec_type = LOCTP_POLYLINE_FILE
         plifile = str_buf
         call resolvePath(plifile, md_structurefile_dir)
      else
         write(msgbuf, '(a,a,A)') 'Field ''polylinefile'' missing in structure ''', trim(strid), '''. Skip this structure.'
         call msg_flush()
         cycle
      end if

      ! Fill in arrays of polyline points coordinates
      if (loc_spec_type == LOCTP_POLYLINE_FILE) then
        call oldfil(minp_tmp, plifile)
        call read1polylin(minp_tmp,xpl_tmp,ypl_tmp,npl_tmp)
      end if
      nstru_read2 = nstru_read2 + 1

      call find_crossed_links_kdtree2(treeglob,npl_tmp,xpl_tmp,ypl_tmp,3,numL,1, nstrulinks, istrulinks, iPol_tmp, dSL_tmp, ierror)
      if (ierror.ne.0) then
         write(msgbuf, '(a, a, a)')'Error occurs when finding crossed polyline of structure ''', trim(strid), '''.'
         call warn_flush()
         goto 888
      end if

      !! Set netcell indices
      do L = 1, nstrulinks
         do ii = 1,2
            k = lne(ii, istrulinks(L))
            if (k > 0) then
               nstrucells = nstrucells + 1
               istrucells(nstrucells) = k
            end if
         end do
      end do
   end do

   if (nstru_read2 == 0) then
      write(msgbuf, '(a)') 'No structures are defined by polylines. The function of avoiding structures intercrosee partition boundaries ' &
                            //'currently does NOT support other ways of defining structure locations yet.'
      call warn_flush()
      goto 888
   else if (nstru_read1 > nstru_read2) then
      write(msgbuf, '(a, I5,a, I5, a, I5, a)')'There are ', nstru_read1, '[structure] blocks, but only ', nstru_read2, 'blocks use polylines to define the structure locations. ' &
                                             //'The function of avoiding structures intercrosee partition boundaries currently ' &
                                             //'does NOT support other ways of defining structure locations yet. So '&
                                             //'still ', nstru_read1-nstru_read2, ' structures are not handled in the prepartion and they might intercross partition boundaries.'
      call warn_flush()
      goto 888
   end if

   call mess(LEVEL_INFO, 'Prepare for partitioning: Done.')
   return

888 continue
    write(msgbuf, '(a)') 'Failed in preparation for partitioning by METIS. As a result, it might happen that some structures intercross the partition boundaries, ' &
                       //'and the parallel simulation results might not be reliable. To avoid this problem, please check '&
                       //'the partition results to see if any structure intercrosses partition boundaries. If so, try to change the number of partition domains '&
                       //'and partition again, or use polyline to define structures.'
    call warn_flush()
end subroutine find_netcells_for_structures
