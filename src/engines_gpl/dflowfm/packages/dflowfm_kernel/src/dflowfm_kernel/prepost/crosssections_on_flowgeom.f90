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

!!> Converts the current polylines to flux cross sections.
!!! This means, finding shortest path of connected netlinks
!!! between the active polyline points. The latter are first
!!! snapped to the closest net nodes.
!!!
!!! Set jaExisting to 1, if the existing polylines in crs()%xp
!!! have to be used (when called from flow flow_modelinit/obsinit)
!subroutine poltocrs(jaExisting, jaKeepPol)
!use m_polygon
!use m_crosssections
!use m_missing
!use network_data
!use unstruc_colors
!use m_alloc
!use unstruc_messages
!implicit none
!
!integer :: jaExisting !< Use existing crs%xp instead of active polygon in xpl
!integer :: jaKeepPol  !< Leave the CRS polylines active on screen (only useful in interactive mode, i.e., jaExisting==0)
!
!integer :: i, i1, ip, ip0, IPL, np, k, k1, k2, kmin, L, ja, ntrc, numnam
!integer, allocatable :: kp(:), kk(:), crstp(:)
!double precision :: dis, rmin, x0, y0, xx1, yy1, xkn, ykn, xn, yn, dpr, dprmin, sl
!double precision :: dbdistance, dprodin
!double precision, allocatable :: xtrc(:), ytrc(:) !< The cross sections that are traced along netnodes
!character(len=64) :: namcrs
!
!if (allocated(nampli)) then
!    numnam = size(nampli)
!else
!    numnam = 0
!end if
!
!! for jaExisting, copy existing cross section polylines to active polylines in xpl
!if (jaExisting == 1) then
!    call savepol() ! xph now contains the onscreen polylines
!    allocate(crstp(ncrs))
!    ! Back up types of existing crss
!    do i=1,ncrs
!        crstp(i) = crs(i)%type
!    end do
!    call copycrosssectionstopol(1) ! overwrite existing crs
!end if
!
!! No crss to be traced, return
!if (npl == 0) then
!    if (jaExisting == 1) then
!        ! Restore original onscreen polylines when needed.
!        call restorepol()
!    end if
!    return
!end if
!
!! xpl contains the polylines that need to be traced as a crs.
!! xtrc will contain the traced crs path after each iteration
!allocate(kp(npl))
!allocate(kk(maxpol)) ! Save net node nrs of traversed sequence (same pace as xk, yk).
!allocate(xtrc(maxpol), ytrc(maxpol)) ! Save net node nrs of traversed sequence (same pace as xk, yk).
!kp = -1
!! First, snap the polyline points to the closest net nodes.
!do i=1,npl
!    if (xpl(i) == dmiss .or. ypl(i) == dmiss) cycle
!
!    rmin = huge(rmin)
!    kmin = -1
!    do k=1,numk
!        ! AvD: todo, disable/skip 1D nodes?
!        dis = dbdistance(xk(k), yk(k), xpl(i), ypl(i))
!        if (dis < rmin) then
!            rmin = dis
!            kmin = k
!        end if
!    end do
!    kp(i) = kmin
!    !call cirr(xk(kmin), yk(kmin), ncolcrs)
!end do
!
!ntrc = 0         ! xtrc(:) is used as placeholder for crs(ncrs)%xk(:), ntrc is growing nr of nodes
!ip   = 0         ! position in array of coarse polylines xpl, if xpl(ip+1)==dmiss, start a new crs.
!ipl  = 0         ! Nr of current polyline (to acces nampol)
!do              ! Loop across polylines.
!    ip0 = ip
!
!    ip  = ip+1
!    if (ip > npl) exit    ! Last polyline was finished
!    if (kp(ip) < 0) cycle ! Superfluous dmiss separator, skip to next point
!
!    kc = 0      ! Mark which net nodes are in the new crs.
!    IPL = IPL + 1
!    ntrc = 1
!
!    if (ipl > numnam) then
!        namcrs = ' '
!    else
!        namcrs = nampli(ipl)
!    end if
!
!    k        = kp(ip)
!    kc(k)    = 1
!    xtrc(ntrc) = xk(k)
!    ytrc(ntrc) = yk(k)
!    kk(ntrc )  = k
!    ! For each polyline segment, find closest connected path of net links.
!pli:do          ! Loop across the segments of current polyline. i=1,np-1
!        if (ip == npl) exit
!        k  = kp(ip)
!        k1 = kp(ip+1)
!        if (k1 <= 0) exit ! End of current polyline, jump to next
!
!        x0  = xk(k)  ! Segment's start point
!        y0  = yk(k)
!        xx1 = xk(k1) ! Segment's end point
!        yy1 = yk(k1)
!
!  path: do      ! Loop until path of net links has reached node k1 (k==k1).
!            if (k==k1) exit
!
!            ! Search line from current point to next polyline point.
!            dis = dbdistance(xk(k), yk(k), xk(k1), yk(k1))
!            dis = dis*dis
!
!            ! Next, find the link with smallest angle to line towards xx1,yy1
!            dprmin = huge(dprmin)
!            kmin   = 0
!            ! Projected the current point k on line 0-1, that is the start of new search axis.
!            call dlinedis(xk(k), yk(k), x0, y0, xx1, yy1,ja,dpr,xkn,ykn)
!            do L=1,nmk(k)
!                call othernode(k, nod(k)%lin(L), k2)
!                if (kc(k2) == 1) then
!                    if (ip == npl-1 .and. k2 == kk(1)) then
!                        continue ! Point was used before, but allow it anyway (closing point of close polygon)
!                    else
!                        cycle ! Node already in crs
!                    end if
!                end if
!
!                ! Distance to line between projected previous point xkn and
!                ! endpoint xx1 should be small, i.e., close to original polyline segment.
!                call dlinedis2(xk(k2), yk(k2), xkn, ykn, xx1, yy1,ja,dpr,xn,yn, sl)
!
!                if (sl < -.01d0 .or. sl > 1.01d0) cycle ! Hardly allow backwards or beyond motion.
!
!                if (dpr < dprmin) then
!                    dprmin = dpr
!                    kmin   = k2
!                end if
!            end do
!            if (kmin == 0) then
!                ! Could not find a link leading any further towards xx1,yy1,
!                ! Discard this entire polyline
!                !npl = 0 ! Reset, to detect faulty cross section below
!                do  ! Traverse current polyline to end, after that, exit to outer loop.
!                    ip = ip + 1
!                    if (ip == npl) exit pli
!                    if (kp(ip) < 0) exit pli
!                end do
!            end if
!
!            ! Select the best direction.
!            k     = kmin
!            kc(k) = 1
!            ! Add the current node to the new polyline.
!            ntrc       = ntrc+1
!            xtrc(ntrc) = xk(k)
!            ytrc(ntrc) = yk(k)
!            kk(ntrc)   = k
!            !call cirr(xk(kmin), yk(kmin), ncolcrs)
!        end do path ! net link path for a single polyline segment
!        ! AvD: TODO: xtrc etc are now allocated at maxpol, potential overflow.
!
!        ip = ip + 1
!    end do pli ! one polyline becomes one cross section
!
!    ! Create the new rai
!    if (ntrc <= 1) then
!        write(msgbuf, '(a,i2,a,a,a)') 'Cross section path incomplete or too short. Discarding #', ipl, ' (''', trim(namcrs), ''').'
!        call msg_flush()
!    else
!        np = ip-ip0
!        call newCrossSection(namcrs, np=np)
!        if (jaExisting == 1) then
!            crs(ncrs)%type = crstp(ipl)
!        end if
!
!        crs(ncrs)%xp(1:np) = xpl(ip0+1:ip)
!        crs(ncrs)%yp(1:np) = ypl(ip0+1:ip)
!        crs(ncrs)%np = np
!
!        call allocCRSLinks(ncrs, ntrc-1)
!        crs(ncrs)%xk  = xtrc(1:ntrc)
!        crs(ncrs)%yk  = ytrc(1:ntrc)
!        crs(ncrs)%kk  = kk(1:ntrc)
!        crs(ncrs)%len = ntrc-1
!    end if
!    xtrc = dmiss
!    ytrc = dmiss
!    ntrc = 0
!    mp   = 0
!    mps  = 0
!
!    ! Proceed to next polyline/next cross section
!end do ! multiple polylines/cross sections possible
!
!! Restore original onscreen polylines if existing crs's plis had overwritten them.
!if (jaExisting == 1) then
!    deallocate(crstp)
!end if
!if (jaKeepPol /= 1)  then
!    call restorepol()
!end if
!
!nampli = ' ' ! Reset names (to prevent them from being reused in subsequent interactive polylines)
!deallocate(kp, kk, xtrc, ytrc)
!
!end subroutine poltocrs
!> Put the polyline cross sections on flow links.
!! The resulting link administration in the crspath structures is later
!! used when computing cumulative data across the cross sections.
!!
!! \see update_values_on_cross_sections, fixedweirs_on_flowgeom, thindams_on_netgeom
subroutine crosssections_on_flowgeom()
   use m_monitoring_crosssections
   use m_flowgeom, only: Lnx
   use m_missing
   use kdtree2Factory
   use unstruc_messages
   use dfm_error
   use unstruc_channel_flow
   use m_inquire_flowgeom
   use unstruc_caching, only: copyCachedCrossSections, saveLinkList
   use m_partitioninfo, only: jampi
   use m_readyy
   use m_wall_clock_time
   use m_find_crossed_links_kdtree2
   use m_crspath_on_flowgeom
   implicit none

   integer :: ic, icmod

   double precision, dimension(:), allocatable :: xx, yy
   double precision, dimension(:), allocatable :: dSL
   integer, dimension(:), allocatable :: iLink, ipol, istartcrs, numlist
   integer, dimension(:, :), allocatable :: linklist
   integer, dimension(:), allocatable :: idum

   integer :: i, num, numcrossedlinks, ierror
   integer :: istart, iend

   integer :: jakdtree = 1
   double precision :: t0, t1
   character(len=128) :: mesg
   integer :: linknr, ii, branchIdx
   type(t_observCrossSection), pointer :: pCrs
   logical :: success

   if (ncrs < 1) return

   numcrossedlinks = 0

!   allocate
   allocate (istartcrs(ncrs + 1))
   istartcrs = 1

   allocate (idum(1))
   idum = 0

   if (jakdtree == 1) then
      call wall_clock_time(t0)

      call copyCachedCrossSections(iLink, ipol, success)

      if (success) then
         numcrossedlinks = size(iLink)
         ierror = 0
      else
         num = 0
!           determine polyline size
         do ic = 1, ncrs
            if (crs(ic)%loc2OC == 0) then ! only for crs which are polyline-based
               num = num + crs(ic)%path%np + 1 ! add space for missing value
               istartcrs(ic + 1) = num + 1
            end if
         end do

!           allocate
         allocate (xx(num), yy(num))

!           determine paths to single polyline map
         num = 0
         do ic = 1, ncrs
            if (crs(ic)%loc2OC == 0) then
               do i = 1, crs(ic)%path%np
                  num = num + 1
                  xx(num) = crs(ic)%path%xp(i)
                  yy(num) = crs(ic)%path%yp(i)
               end do
!              add missing value
               num = num + 1
               xx(num) = DMISS
               yy(num) = DMISS
            end if
         end do

!           allocate
         allocate (iLink(Lnx))
         iLink = 0
         allocate (ipol(Lnx))
         ipol = 0
         allocate (dSL(Lnx))
         dSL = 0d0
         call find_crossed_links_kdtree2(treeglob, num, xx, yy, 2, Lnx, 1, numcrossedlinks, iLink, ipol, dSL, ierror)

         call saveLinklist(numcrossedlinks, iLink, ipol)
      end if

      if (ierror == 0 .and. numcrossedlinks > 0) then

!          determine crossed links per cross-section
         allocate (numlist(ncrs))
         numlist = 0
         allocate (linklist(numcrossedlinks, ncrs))
         linklist = 0

         do i = 1, numcrossedlinks
            do ic = 1, ncrs
               if (crs(ic)%loc2OC == 0) then
                  istart = istartcrs(ic)
                  iend = istartcrs(ic + 1) - 1
                  if (ipol(i) >= istart .and. ipol(i) <= iend) then
                     numlist(ic) = numlist(ic) + 1
                     linklist(numlist(ic), ic) = iLink(i)
                  end if
               end if
            end do
         end do

      else
!          disable kdtree
         jakdtree = 0
         ! allocate(idum(1))
         ! idum = 0

!          deallocate
         if (allocated(iLink)) deallocate (iLink)
         if (allocated(ipol)) deallocate (ipol)
         if (allocated(dSL)) deallocate (dSL)
      end if

!       deallocate
      if (allocated(istartcrs)) deallocate (istartcrs)
      if (allocated(xx)) deallocate (xx, yy)

      call wall_clock_time(t1)
      write (mesg, "('cross sections with kdtree2, elapsed time: ', G15.5, 's.')") t1 - t0
      call mess(LEVEL_INFO, trim(mesg))
   end if

   icMOD = max(1, ncrs / 100)

   call realloc(numlist, ncrs, keepExisting=.true., fill=0) ! In case pli-based cross sections have not allocated this yet.
   call realloc(linklist, (/max(numcrossedlinks, 1), ncrs/), keepExisting=.true., fill=0) ! In addition to pli-based cross sections (if any), also support 1D branchid-based cross sections.

   call copyCachedCrossSections(iLink, ipol, success)

   call READYY('Enabling cross sections on grid', 0d0)
   do ic = 1, ncrs
      if (mod(ic, icMOD) == 0) then
         call READYY('Enabling cross sections on grid', dble(ic) / dble(ncrs))
      end if
      if (crs(ic)%loc2OC == 0) then
         if (.not. success) then
            if (jakdtree == 0) then
               call crspath_on_flowgeom(crs(ic)%path, 0, 0, 1, idum, 0, 1)
            else
               call crspath_on_flowgeom(crs(ic)%path, 0, 1, numlist(ic), linklist(1, ic), 0, 1)
            end if
         end if
      else ! snap to only 1d flow link
         ii = crs(ic)%loc2OC
         pCrs => network%observcrs%observcross(ii)
         branchIdx = pCrs%branchIdx
         ierror = findlink(branchIdx, pCrs%chainage, linknr) ! find flow link given branchIdx and chainage
         if (linknr == -1) then
            if (ierror /= DFM_NOERR) then
               call SetMessage(LEVEL_ERROR, 'Error occurs when snapping Observation cross section '''//trim(crs(ic)%name)//''' to a 1D flow link.')
               call SetMessage(LEVEL_ERROR, 'Possibly wrong branchId? Given branchId is: '''//trim(pCrs%branchid)//'''.')
            else if (jampi > 0) then
               continue ! Most probably on another domain
            else
               ! Sequential model with correct branchId, but still no link: possibly chainage outside length range of branch? Warning only.
               call SetMessage(LEVEL_WARN, 'Error occurs when snapping Observation cross section '''//trim(crs(ic)%name)//''' to a 1D flow link.')
               write (msgbuf, '(a,g10.3,a,g10.3,a)') 'Possibly wrong chainage? Given chainage is: ', pCrs%chainage, &
                  ' on branchId '''//trim(pCrs%branchId)//''' (length = ', network%brs%branch(branchIdx)%length, ').'
               call SetMessage(LEVEL_WARN, trim(msgbuf))
            end if
         else ! valid flowlink found
            numlist(ic) = 1
            linklist(1, ic) = linknr
            call crspath_on_flowgeom(crs(ic)%path, 0, 1, numlist(ic), linklist(1, ic), 1, 1)
         end if
      end if
   end do

   call READYY('Enabling cross sections on grid', -1d0)

1234 continue

!   deallocate
   if (jakdtree == 1) then
      if (allocated(iLink)) deallocate (iLink)
      if (allocated(iPol)) deallocate (iPol)
      if (allocated(dSL)) deallocate (dSL)
      if (allocated(numlist)) deallocate (numlist)
      if (allocated(linklist)) deallocate (linklist)
   end if

   if (allocated(idum)) deallocate (idum)

   return
end subroutine crosssections_on_flowgeom
