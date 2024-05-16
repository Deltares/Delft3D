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

module m_find_flownode
   
   implicit none
   
   private
   
   public :: find_flownode, find_flownode_for_obs, find_flowcells_kdtree
   
   contains

!> Finds the flow nodes/cell numbers for all observation points. There are four kinds of obs, treated differently:
!! obs that are defined in *.xyn file, to be snaped to 1D+2D flow nodes (Locationtype == 0), use kdtree
!! obs that are defined in *.ini file by xy coordinate, to be snaped to only 1D flow node (Locationtype == 1), use kdtree
!! obs that are defined in *.ini file by xy coordinate, to be snaped to only 2D flow node (Locationtype == 2), use kdtree
!! obs that are defined in *.ini file by branchID and chainage, to be snaped to only 1D flow node (Locationtype == 3), do not use kdtree
subroutine find_flownode_for_obs(nstart, nend)
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
   
   implicit none
   
   integer, intent(in)               :: nstart ! starting index of obs for snapping to a flow node
   integer, intent(in)               :: nend   ! ending index of obs for snapping to a flow node
   integer                           :: i, nodenr, branchIdx, ntotal, nobsini, ierr, jakdtree, jabybranch
   integer, allocatable              :: ixy2obs0(:), ixy2obs1(:), ixy2obs2(:)
   integer, allocatable              :: kobs_tmp0(:), kobs_tmp1(:), kobs_tmp2(:)
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
   call realloc(namobs_tmp0, ntotal, keepExisting=.false.)

   nobsini = network%obs%Count
   call realloc(ixy2obs1,    nobsini, keepExisting=.false.)
   call realloc(xobs_tmp1,   nobsini, keepExisting=.false.)
   call realloc(yobs_tmp1,   nobsini, keepExisting=.false.)
   call realloc(kobs_tmp1,   nobsini, keepExisting=.false.)
   call realloc(namobs_tmp1, nobsini, keepExisting=.false.)

   call realloc(ixy2obs2,    nobsini, keepExisting=.false.)
   call realloc(xobs_tmp2,   nobsini, keepExisting=.false.)
   call realloc(yobs_tmp2,   nobsini, keepExisting=.false.)
   call realloc(kobs_tmp2,   nobsini, keepExisting=.false.)
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
      call find_flownode(nloctypeAll, xobs_tmp0(1:nloctypeAll), yobs_tmp0(1:nloctypeAll), namobs_tmp0(1:nloctypeAll), kobs_tmp0(1:nloctypeAll), jakdtree, 1, INDTP_ALL)
      do i = 1, nloctypeAll
         kobs(ixy2obs0(i)) = kobs_tmp0(i)
      end do
   end if

   jakdtree = 1
   if (nloctype1D > 0) then
      call find_flownode(nloctype1D, xobs_tmp1(1:nloctype1D), yobs_tmp1(1:nloctype1D), namobs_tmp1(1:nloctype1D), kobs_tmp1(1:nloctype1D), jakdtree, 0, INDTP_1D)
      do i = 1, nloctype1D
         kobs(ixy2obs1(i)) = kobs_tmp1(i)
      end do
   end if

   jakdtree = 1
   if (nloctype2D > 0) then
      call find_flownode(nloctype2D, xobs_tmp2(1:nloctype2D), yobs_tmp2(1:nloctype2D), namobs_tmp2(1:nloctype2D), kobs_tmp2(1:nloctype2D), jakdtree, 0, INDTP_2D)
       do i = 1, nloctype2D
         kobs(ixy2obs2(i)) = kobs_tmp2(i)
      end do
   end if


   if (allocated(ixy2obs0))    deallocate(ixy2obs0)
   if (allocated(xobs_tmp0))   deallocate(xobs_tmp0)
   if (allocated(yobs_tmp0))   deallocate(yobs_tmp0)
   if (allocated(yobs_tmp0))   deallocate(yobs_tmp0)
   if (allocated(namobs_tmp0)) deallocate(namobs_tmp0)

   if (allocated(ixy2obs1))    deallocate(ixy2obs1)
   if (allocated(xobs_tmp1))   deallocate(xobs_tmp1)
   if (allocated(yobs_tmp1))   deallocate(yobs_tmp1)
   if (allocated(yobs_tmp1))   deallocate(yobs_tmp1)
   if (allocated(namobs_tmp1)) deallocate(namobs_tmp1)

   if (allocated(ixy2obs2))    deallocate(ixy2obs2)
   if (allocated(xobs_tmp2))   deallocate(xobs_tmp2)
   if (allocated(yobs_tmp2))   deallocate(yobs_tmp2)
   if (allocated(yobs_tmp2))   deallocate(yobs_tmp2)
   if (allocated(namobs_tmp2)) deallocate(namobs_tmp2)

   return
end subroutine find_flownode_for_obs

!> Find the flownodes nearest to the set of points [xx,yy]
subroutine find_flownode(n, xobs, yobs, namobs, kobs, jakdtree, jaoutside, iLocTp)
   use unstruc_messages
   use m_partitioninfo
   use m_flowgeom
   use m_GlobalParameters, only: INDTP_1D, INDTP_2D, INDTP_ALL
   use kdtree2Factory
   use geometry_module, only: dbdistance
   use m_missing, only: dmiss
   use m_sferic, only: jsferic, jasfer3D

   implicit none

   integer,                            intent(in   )  :: n           !< number of points
   double precision,     dimension(n), intent(in   )  :: xobs, yobs  !< points coordinates
   character(len=IdLen), dimension(n), intent(in   )  :: namobs      !< names of points
   integer,              dimension(n), intent(inout)  :: kobs        !< associated flow nodes, if found.
   integer,                            intent(inout)  :: jakdtree    !< use kdtree (1) or not (other)
   integer,                            intent(in   )  :: jaoutside   !< allow outside cells (for 1D) (1) or not (0)
   integer,                            intent(in   )  :: iLocTp      !< Node type, one of INDTP_1D/2D/ALL.
   integer                                            :: ierror      !< error (1) or not (0)
   integer                                            :: i, k, k1b
   integer,           dimension(1)                    :: idum
   double precision                                   :: d1, d2

   ierror = 1

   if (jakdtree == 1) then
      call find_flowcells_kdtree(treeglob, n, xobs, yobs, kobs, jaoutside, iLocTp, ierror)

      if (jampi == 1) then
         ! globally reduce ierror
         idum(1) = ierror
         call reduce_int_max(1, idum)
         ierror = idum(1)
      end if

      if (ierror /= 0) then
         jakdtree = 0   ! retry without kdtree
      end if

      ! disable observation stations without attached flowlinks
      do i = 1, n
         k = kobs(i)
         if (k > 0) then
            if (nd(k)%lnx < 1) then
               kobs(i) = 0
            end if
         end if
      end do
   
   else
      
      do i = 1, n
         call inflowcell(xobs(i), yobs(i), k, jaoutside, iLocTp)
         if (jaoutside == 1 .and. (iLocTp == INDTP_1D .or. iLocTp == INDTP_ALL)) then
            call find_1D_or_boundary_flownode_bruteforce(xobs(i), yobs(i), k1b)
            if (k /= 0 .and. k1b /= 0) then
                d1 = dbdistance(xz(k1b), yz(k1b), xobs(i), yobs(i), jsferic, jasfer3D, dmiss)
                d2 = dbdistance(xz(k  ), yz(k  ), xobs(i), yobs(i), jsferic, jasfer3D, dmiss)
                if (d1 < d2) then
                   k = k1b
                end if
            else if (k1b /= 0) then
                k = k1b
            end if
         end if
         kobs(i) = 0
         if (k /= 0) then
            if ( nd(k)%lnx > 0 ) then
               kobs(i) = k
            end if
         end if
      end do
   end if

   if (jampi == 1 .and. n > 0) then
      ! check if this subdomain owns the observation station
      call reduce_kobs(n, kobs, xobs, yobs, jaoutside)
   end if

   do i = 1, n
      if (kobs(i) == 0) then
          write(msgbuf, '(a,i0,a,a,a)') 'Could not find flowcell for point #', i, ' (', trim(namobs(i)), '). Discarding.'
          call msg_flush()
      end if
   end do

   ierror = 0
1234 continue

end subroutine find_flownode

!> Find the flownodes nearest to the set of points [xx,yy]
!! Uses the k-d tree routines
subroutine find_flowcells_kdtree(treeinst, Ns, xs, ys, inod, jaoutside, iLocTp, ierror)

   use m_missing
   use m_flowgeom
   use m_GlobalParameters, only: INDTP_1D, INDTP_2D, INDTP_ALL
   use kdtree2Factory
   use m_sferic
   use unstruc_messages
   use gridoperations
   use geometry_module, only: dbdistance, pinpok

   implicit none

   type(kdtree_instance),           intent(inout) :: treeinst
   integer,                         intent(in)    :: Ns         !< number of samples
   double precision, dimension(Ns), intent(in)    :: xs, ys     !< observation coordinates
   integer,          dimension(Ns), intent(out)   :: inod       !< flow nodes
   integer,                         intent(in)    :: jaoutside  !< allow outside cells (for 1D) (1) or not (0)
   integer,                         intent(in)    :: iLocTp     !< (0) not for obs, or obs with locationtype==0, (1) for obs with locationtype==1, (2) for obs with locationtype==2
   integer,                         intent(out)   :: ierror     !< error (>0), or not (0)

   double precision, dimension(:),  allocatable   :: xx, yy     !< unique station coordinates
   integer,          dimension(:),  allocatable   :: iperm      !< permutation array
   integer,          dimension(:),  allocatable   :: invperm    !< inverse array
     
   character(len=128)                             :: mesg, FNAM

   integer,          parameter                    :: Msize = 10

   double precision, dimension(Msize)             :: xloc, yloc
   integer,          dimension(Msize)             :: Lorg
   integer,          dimension(Msize)             :: LnnL

   double precision                               :: dmaxsize, R2search, t0, t1, zz

   integer                                        :: i, ip1, isam, in, k, N, NN
   integer                                        :: inum, num, jj
   integer                                        :: in3D, j, fid
   integer                                        :: nstart, nend
   logical                                        :: jadouble
   double precision                               :: dist_old, dist_new

   ierror = 1

   inod = 0

   call klok(t0)

   ! build kdtree
   call build_kdtree(treeinst, Ns, xs, ys, ierror, jsferic, dmiss)

   if (ierror /= 0) then
      goto 1234
   end if

   ! define the searching range, this is especially for the purpose of snapping obs to 1D, 2D or 1D+2D flownodes.
   ! For other purpose it should stay as before
   select case(iLocTp)
   case (INDTP_ALL)
      nstart = 1
      nend   = ndx
   case(INDTP_1D) ! 1d flownodes coordinates
      nstart = ndx2D+1
      nend   = ndx
   case(INDTP_2D) ! 2d flownodes coordinates
      nstart = 1
      nend   = ndx2D
   end select

   call mess(LEVEL_INFO, 'Finding flow nodes...')

   ! loop over flownodes
   do k = nstart, nend
        
      ! fill query vector
      call make_queryvector_kdtree(treeinst, xz(k), yz(k), jsferic)

      ! compute maximum flowcell dimension
      dmaxsize = 0d0
      N = size(nd(k)%x)
      do i = 1, N
         ip1 = i+1
         if (ip1 > N) then
            ip1 = ip1 - N
         end if
         dmaxsize = max(dmaxsize, dbdistance( nd(k)%x(i), nd(k)%y(i), nd(k)%x(ip1), nd(k)%y(ip1), jsferic, jasfer3D, dmiss))
      end do

      ! determine square search radius
      R2search = 1.1d0*dmaxsize**2  ! 1.1d0: safety

      ! get the cell polygon that is safe for periodic, spherical coordinates, inluding poles
      call get_cellpolygon(k, Msize, N, 1d0, xloc, yloc, LnnL, Lorg, zz)

      if (N < 1) then
         if (k <= Ndxi) then
            continue
         end if
         cycle
      end if

      ! count number of points in search area
      NN = kdtree2_r_count(treeinst%tree,treeinst%qv,R2search)

      if (NN == 0) cycle ! no links found

      ! reallocate if necessary
      call realloc_results_kdtree(treeinst,NN)

      ! find nearest NN samples
      call kdtree2_n_nearest(treeinst%tree, treeinst%qv, NN, treeinst%results)

      ! check if samples are in cell
      do i = 1, NN
         isam = treeinst%results(i)%idx

         if (k > ndx2D .and. k < ndxi+1 .and. jaoutside == 1) then  ! For 1D nodes, skip point-in-cell check
            in = 1 ! These are always accepted if closest.
         else
            call pinpok(xs(isam), ys(isam), N, xloc, yloc, in, jins, dmiss)
         end if
         if (in == 1) then
            if (inod(isam) /= 0) then            ! should not happen, but it can: for example in case of overlapping 1D branches
               write(mesg, "('find_flowcells_kdtree: sample/point ', I0, ' in cells ', I0, ' and ', I0)") isam, inod(isam), k
               call mess(LEVEL_INFO, mesg  )
               ! goto 1234
               if (k > ndx2D .and. k < ndxi+1 .and. jaoutside == 1) then  ! ONLY in case of a 1D node, consider replacing, if the 1D node is closer
                  dist_old = dbdistance( xs(isam), ys(isam), xz(inod(isam)), yz(inod(isam)), jsferic, jasfer3D, dmiss)
                  dist_new = dbdistance( xs(isam), ys(isam), xz(k         ), yz(k         ), jsferic, jasfer3D, dmiss)
                  if (dist_new<dist_old) then            ! if the new candidate is nearer to the observation station  ...
                     inod(isam) = k                      ! ... adopt the new candidate as primary candidate
                  end if
                  write(mesg, "('   selected : ',I0,' based on distance comparison.')")  inod(isam)
                  call mess(LEVEL_INFO, mesg  )
               end if
            else
               inod(isam) = k
            end if
         end if
      end do
   end do

   call klok(t1)

   write(mesg, "('done in ', F12.5, ' sec.')") t1-t0
   call mess(LEVEL_INFO, trim(mesg))

   ierror = 0
1234 continue

   ! deallocate
   if (treeinst%itreestat /= ITREE_EMPTY) call delete_kdtree2(treeinst)

end subroutine find_flowcells_kdtree

!> Find the 1-D or boundary flownode with the shortest distance to the point [x,y]
!! Brute-force approach: simply check all flowlinks in the entire grid
subroutine find_1D_or_boundary_flownode_bruteforce(x, y, node_id_closest)
   use stdlib_kinds, only: dp
   use m_find_flowlink, only: find_1D_or_boundary_flowlink_bruteforce
   use m_flowgeom, only: ln, xz, yz
   use geometry_module, only: dbdistance
   use m_sferic, only: jsferic, jasfer3D
   use m_missing, only: dmiss

   implicit none
      
   real(dp), intent(in   ) :: x, y
   integer,  intent(  out) :: node_id_closest
      
   integer  :: link_id_closest, ka, kb
   
   node_id_closest = 0
   
   call find_1D_or_boundary_flowlink_bruteforce(x, y, link_id_closest)
   
   if (link_id_closest /= 0) then
      ka = ln(1,link_id_closest)
      kb = ln(2,link_id_closest)
      if (dbdistance(x, y, xz(ka), yz(ka), jsferic, jasfer3D, dmiss) < &
          dbdistance(x, y, xz(kb), yz(kb), jsferic, jasfer3D, dmiss) ) THEN
         node_id_closest = ka
      else
         node_id_closest = kb
      end if
   end if

end subroutine find_1D_or_boundary_flownode_bruteforce

end module m_find_flownode
