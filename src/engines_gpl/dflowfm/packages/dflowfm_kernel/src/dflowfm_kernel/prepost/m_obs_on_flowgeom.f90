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

module m_obs_on_flowgeom
   
   implicit none
   
   private
   
   public :: obs_on_flowgeom
   
   contains

!> Snap observation stations to nearest flow nodes and links.
!! Results are stored in `m_observations` state arrays.
subroutine obs_on_flowgeom(iobstype)

   use m_observations, only: numobs, nummovobs, kobs, namobs
   use unstruc_messages, only: loglevel_StdOut, LEVEL_DEBUG, LEVEL_INFO, msgbuf, mess
   use m_flowgeom, only: ndx2D, ndxi
   use unstruc_caching, only: cacheRetrieved, copyCachedObservations
   
   implicit none

   integer, intent(in) :: iobstype !< Which obs stations to update: 0=normal, 1=moving, 2=both
   
   integer                    :: n1, n2, iobs
   logical                    :: cache_success

   if (iobstype == 0) then
      ! Only normal stations
      n1 = 1
      n2 = numobs
   elseif (iobstype == 1) then
      ! Only moving stations
      n1 = numobs+1
      n2 = numobs+nummovobs
   else
      ! All stations
      n1 = 1
      n2 = numobs+nummovobs
   end if

   ! Try to read normal (non-moving) stations from cache file
   cache_success = .false.
   if (iobstype == 0 .or. iobstype == 2) then
      if ( cacheRetrieved() ) then
         call copyCachedObservations( cache_success )
      endif
   end if

   if (cache_success) then
      ! When necessary, process also the moving observation points (which are not in cache file)
      if ((iobstype == 1 .or. iobstype == 2) .and. nummovobs > 0) then
         call find_flownodes_and_links_for_all_observation_stations(numobs+1, numobs+nummovobs)
      end if
   else
      ! No cache, so process all requested observation points.
      call find_flownodes_and_links_for_all_observation_stations(n1, n2)
   endif

   if (loglevel_StdOut == LEVEL_DEBUG) then
      do iobs = n1, n2
         if (kobs(iobs) < ndx2D) then
            write(msgbuf, '(a,i0,a,i0,a)') "Obs #",iobs,":"//trim(namobs(iobs))//" on node ",kobs(iobs)," (2D)"
         elseif (kobs(iobs) <= ndxi) then
            write(msgbuf, '(a,i0,a,i0,a)') "Obs #",iobs,":"//trim(namobs(iobs))//" on node ",kobs(iobs)," (1D)"
         end if
         call mess(LEVEL_INFO, msgbuf)
      end do
   end if

end subroutine obs_on_flowgeom

!> Finds the flow nodes/cell numbers for all observation points. There are four kinds of obs, treated differently:
!! obs that are defined in *.xyn file, to be snaped to 1D+2D flow nodes (Locationtype == 0), use kdtree
!! obs that are defined in *.ini file by xy coordinate, to be snaped to only 1D flow node (Locationtype == 1), use kdtree
!! obs that are defined in *.ini file by xy coordinate, to be snaped to only 2D flow node (Locationtype == 2), use kdtree
!! obs that are defined in *.ini file by branchID and chainage, to be snaped to only 1D flow node (Locationtype == 3), do not use kdtree
subroutine find_flownodes_and_links_for_all_observation_stations(nstart, nend)
   use MessageHandling
   use m_network
   use m_ObservationPoints
   use m_observations
   use unstruc_channel_flow
   use m_inquire_flowgeom
   use m_GlobalParameters, only: INDTP_1D, INDTP_2D, INDTP_ALL
   use dfm_error
   use m_alloc
   use m_flowgeom
   use m_find_flownode, only: find_nearest_flownodes
   use m_find_flowlink, only: find_nearest_flowlinks
   
   implicit none
   
   integer, intent(in)               :: nstart ! starting index of obs for snapping to a flow node
   integer, intent(in)               :: nend   ! ending index of obs for snapping to a flow node
   integer                           :: i, nodenr, branchIdx, ntotal, nobsini, ierr, jakdtree, jabybranch
   integer, allocatable              :: ixy2obs0(:), ixy2obs1(:), ixy2obs2(:)
   integer, allocatable              :: kobs_tmp0(:), kobs_tmp1(:), kobs_tmp2(:)
   integer, allocatable              :: lobs_tmp0(:), lobs_tmp1(:), lobs_tmp2(:)
   double precision, allocatable     :: xobs_tmp0(:), xobs_tmp1(:), xobs_tmp2(:)
   double precision, allocatable     :: yobs_tmp0(:), yobs_tmp1(:), yobs_tmp2(:)
   character(len=IdLen), allocatable :: namobs_tmp0(:), namobs_tmp1(:), namobs_tmp2(:)
   integer                           :: nloctype1D, nloctype2D, nloctypeAll
   type(t_ObservationPoint), pointer :: pOPnt

   ntotal = nend - nstart + 1
   if (ntotal <= 0) then
      return
   end if


   ! realloc temperary arrays for searching
   call realloc(ixy2obs0,    ntotal, keepExisting=.false.)
   call realloc(xobs_tmp0,   ntotal, keepExisting=.false.)
   call realloc(yobs_tmp0,   ntotal, keepExisting=.false.)
   call realloc(kobs_tmp0,   ntotal, keepExisting=.false.)
   call realloc(lobs_tmp0,   ntotal, keepExisting=.false.)
   call realloc(namobs_tmp0, ntotal, keepExisting=.false.)

   nobsini = network%obs%Count
   call realloc(ixy2obs1,    nobsini, keepExisting=.false.)
   call realloc(xobs_tmp1,   nobsini, keepExisting=.false.)
   call realloc(yobs_tmp1,   nobsini, keepExisting=.false.)
   call realloc(kobs_tmp1,   nobsini, keepExisting=.false.)
   call realloc(lobs_tmp1,   nobsini, keepExisting=.false.)
   call realloc(namobs_tmp1, nobsini, keepExisting=.false.)

   call realloc(ixy2obs2,    nobsini, keepExisting=.false.)
   call realloc(xobs_tmp2,   nobsini, keepExisting=.false.)
   call realloc(yobs_tmp2,   nobsini, keepExisting=.false.)
   call realloc(kobs_tmp2,   nobsini, keepExisting=.false.)
   call realloc(lobs_tmp2,   nobsini, keepExisting=.false.)
   call realloc(namobs_tmp2, nobsini, keepExisting=.false.)

   nloctype1D = 0
   nloctype2D = 0
   nloctypeAll = 0

   ! loop over obs
   do i = nstart, nend
      if (locTpObs(i) == INDTP_ALL) then ! obs to be snapped to a nearest 1D or 2D flow node (obs that are defined in *.xyn file)
         if (ndx <= 0) then
               write(msgbuf, '(a)') "Observation point "//trim(namobs(i))//" requires to snap to a flow node, but there is no flow node to be snapped to."
               call mess(LEVEL_ERROR, msgbuf)
         end if
         nloctypeAll = nloctypeAll + 1
         ixy2obs0(nloctypeAll)    = i
         xobs_tmp0(nloctypeAll)   = xobs(i)
         yobs_tmp0(nloctypeAll)   = yobs(i)
         namobs_tmp0(nloctypeAll) = namobs(i)
      else if (locTpObs(i) == INDTP_1D) then ! obs to be snapped to only 1D flow node (obs that are defined in *.ini file (either by branchid+chainage, or xy coordinate), and locationtype ==1)
         if (ndx - ndx2d <= 0) then
            write(msgbuf, '(a)') "Observation point "//trim(namobs(i))//" requires to snap to a 1D flow node, but there is no 1D flow node to be snapped to."
            call mess(LEVEL_ERROR, msgbuf)
         end if
         jabybranch = 0
         ! 1D, option a: Try to handle branchid+chainage input directly:
         if (obs2OP(i) > 0) then
            pOPnt => network%obs%OPnt(obs2OP(i))
            branchIdx = pOPnt%branchIdx
            if (branchIdx > 0) then
               jabybranch = 1
               ierr = findnode(branchIdx, pOPnt%chainage, nodenr) ! find flow node given branchIDx and chainage
               if (ierr == DFM_NOERR) then
                  kobs(i)   = nodenr
               else
                  call SetMessage(LEVEL_ERROR, 'Error when snapping Observation Point '''//trim(namobs(i))//''' to a 1D flow node.')
               end if
            end if
         end if

         ! 1D, option b: via x/y coords, prepare input
         if (jabybranch == 0) then
            nloctype1D = nloctype1D + 1
            ixy2obs1(nloctype1D)    = i
            xobs_tmp1(nloctype1D)   = xobs(i)
            yobs_tmp1(nloctype1D)   = yobs(i)
            namobs_tmp1(nloctype1D) = namobs(i)
         end if
      else if (locTpObs(i) == INDTP_2D) then ! obs to be snapped to only 2D flow node (obs that are defined in *.ini file by xy coordinate, and locationtype ==2)
         if (ndx2d <= 0) then
            write(msgbuf, '(a)') "Observation point "//trim(pOPnt%name)//" requires to snap to a 2D flow node, but there is no 2D flow node to be snapped to."
            call mess(LEVEL_ERROR, msgbuf)
         end if
         nloctype2D = nloctype2D + 1
         ixy2obs2(nloctype2D)    = i
         xobs_tmp2(nloctype2D)   = xobs(i)
         yobs_tmp2(nloctype2D)   = yobs(i)
         namobs_tmp2(nloctype2D) = namobs(i)
      end if
   end do


   ! find flow nodes
   jakdtree = 1
   if (nloctypeAll > 0) then
      call find_nearest_flownodes(nloctypeAll, xobs_tmp0(1:nloctypeAll), yobs_tmp0(1:nloctypeAll), namobs_tmp0(1:nloctypeAll), kobs_tmp0(1:nloctypeAll), jakdtree, 1, INDTP_ALL)
      call find_nearest_flowlinks(xobs_tmp0(1:nloctypeAll), yobs_tmp0(1:nloctypeAll), lobs_tmp0(1:nloctypeAll))
      do i = 1, nloctypeAll
         kobs(ixy2obs0(i)) = kobs_tmp0(i)
         lobs(ixy2obs0(i)) = lobs_tmp0(i)
      end do
   end if

   jakdtree = 1
   if (nloctype1D > 0) then
      call find_nearest_flownodes(nloctype1D, xobs_tmp1(1:nloctype1D), yobs_tmp1(1:nloctype1D), namobs_tmp1(1:nloctype1D), kobs_tmp1(1:nloctype1D), jakdtree, 0, INDTP_1D)
      call find_nearest_flowlinks(xobs_tmp1(1:nloctype1D), yobs_tmp1(1:nloctype1D), lobs_tmp1(1:nloctype1D))
      do i = 1, nloctype1D
         kobs(ixy2obs1(i)) = kobs_tmp1(i)
         lobs(ixy2obs1(i)) = lobs_tmp1(i)
      end do
   end if

   jakdtree = 1
   if (nloctype2D > 0) then
      call find_nearest_flownodes(nloctype2D, xobs_tmp2(1:nloctype2D), yobs_tmp2(1:nloctype2D), namobs_tmp2(1:nloctype2D), kobs_tmp2(1:nloctype2D), jakdtree, 0, INDTP_2D)
      call find_nearest_flowlinks(xobs_tmp2(1:nloctype2D), yobs_tmp2(1:nloctype2D), lobs_tmp2(1:nloctype2D))
       do i = 1, nloctype2D
         kobs(ixy2obs2(i)) = kobs_tmp2(i)
         lobs(ixy2obs2(i)) = lobs_tmp2(i)
      end do
   end if

end subroutine find_flownodes_and_links_for_all_observation_stations

end module m_obs_on_flowgeom