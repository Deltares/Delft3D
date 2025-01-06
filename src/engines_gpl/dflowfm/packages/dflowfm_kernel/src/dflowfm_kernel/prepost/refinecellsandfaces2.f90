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

!> refine cells by splitting links
module m_refinecellsandfaces2
use m_delete_dry_points_and_areas, only: delete_dry_points_and_areas
use m_tidysamples, only: tidysamples
use m_inworld, only: inworld
use m_remove_isolated_hanging_nodes, only: remove_isolated_hanging_nodes

implicit none

private

public :: refinecellsandfaces2

contains

subroutine refinecellsandfaces2()
   use m_rearrange_worldmesh, only: rearrange_worldmesh
   use m_prepare_samplehessian, only: prepare_samplehessian
   use m_get_meshbounds, only: get_meshbounds
   use precision, only: dp
   use m_deallocate_samplehessian, only: deallocate_samplehessian
   use m_connect_hanging_nodes, only: connect_hanging_nodes
   use m_write_illegal_cells_to_pol, only: write_illegal_cells_to_pol
   use m_confrm
   use m_change_samples_refine_param
   use m_netw
   use m_samples
   use m_samples_refine
   use m_ec_interpolationsettings
   use m_missing
   use m_alloc
   use unstruc_messages
   use m_gui
   use kdtree2Factory
   use m_sferic
   use gridoperations
   use timespace
   use m_polygon
   use m_arcinfo
   use m_qnerror
   use m_get_samples_boundingbox
   use m_dlinklength, only: dlinklength

   integer, dimension(:), allocatable :: jarefine ! refine cell (1) or not (0) or cell outside polygon (-1), dim(nump)
   integer, dimension(:), allocatable :: jalink ! refine link (>0) or not (<=0),
   integer, dimension(:), allocatable :: linkbrother ! brotherlink, that shares a (hanging) node, dim: numL

   integer, dimension(:), allocatable :: kc_sav ! save of kc

   real(kind=dp) :: xboundmin, xboundmax, dxxmax, dxxmin, dl

   integer :: ierror ! error (1) or not (0)
   integer :: ja, jaCourantnetwork
   integer :: ic, k, kk, kkm1, kkp1, L
   integer :: numL_old
   integer :: numrefine ! number of cells to be refined
   integer :: nump_virtual
   integer :: interpolationtype_old, IAV_old, jacancelled
   integer :: jakdtree = 1 ! use kdtree (1) or not (0)

   integer :: ilevel ! refinement level
   integer :: k2
   integer :: num ! number of removed isolated hangning noded

   real(kind=dp), external :: getdy

   character(len=64) :: tex

!  determine if the refinement needs to adapt to sample values
!   jaCourantnetwork = 1
!   if ( Ns.lt.3 .or. MXSAM*MYSAM.ne.NS ) jacourantnetwork = 0

!  store original interpolation settings
   interpolationtype_old = interpolationtype
   IAV_old = IAV

   jaCourantnetwork = 0

   if (mxsam == 0) mxsam = mca ! mca was there first

   if (npl > 0) then
      irefinetype = ITYPE_MESHWIDTH ! Polygon refinement:
      numrefcycles = 0 !    make sure interactive
   else if (jaGUI == 0) then ! Commandline: only wave courant for now
      irefinetype = ITYPE_WAVECOURANT
   else ! Interactive:
      call change_samples_refine_param(jacancelled) ! get the settings from a parameter screen
      if (jacancelled == 1) goto 1234
   end if

   if ((ns > 0 .or. mxsam > 0) .and. (irefinetype == ITYPE_WAVECOURANT .or. irefinetype == ITYPE_RIDGE)) then
      jacourantnetwork = 1 ! use samples or arcinfo

      if (MXSAM * MYSAM == NS) then ! bilinarc, so no need for samplekdtree
         interpolationtype = 4
         jakdtree = 0
      else !  if ( IPSTAT.ne.IPSTAT_OK ) then
         write (6, "('tidysamples')")
         call tidysamples(xs, ys, zs, IPSAM, NS, MXSAM, MYSAM) ! uses global kdtree
         call get_samples_boundingbox()
         IPSTAT = IPSTAT_OK
         call build_kdtree(treeglob, Ns, xs, ys, ierror, jsferic, dmiss)
         if (ierror /= 0) then !         disable kdtree
            call delete_kdtree2(treeglob)
            jakdtree = 0
         end if
      end if

      if (irefinetype == ITYPE_RIDGE) then
         call prepare_sampleHessian(ierror)
         if (ierror /= 0) goto 1234
      end if

   end if

   ! if ( netstat.ne.netstat_OK .or. NPL.gt.0 ) then ! in case of selecting polygon: always call findcells
   call findcells(0)
   ! end if

   !if ( Ns.ge. 1 .and. jakdtree.eq.1 .and. interpolationtype == 2) then

   !IPSTAT                = IPSTAT_NOTOK

!  allocate
   allocate (jarefine(nump), stat=ierror)
   call aerr('jarefine(nump)', ierror, nump)
   if (ierror /= 0) goto 1234

   allocate (jalink(numL), stat=ierror)
   call aerr('jalink(numL)', ierror, numL)
   if (ierror /= 0) goto 1234

   allocate (linkbrother(numL), stat=ierror)
   call aerr('linkbrother(numL)', ierror, numL)
   if (ierror /= 0) goto 1234
   linkbrother = 0

!  get mesh bounds for spherical, periodic coordinates
   if (jsferic == 1) then
      call get_meshbounds(xboundmin, xboundmax)
   end if

!  take dry cells into account (after findcells)
   call delete_dry_points_and_areas()

!  try to find brother links in the original net
   linkbrother = 0
   call find_linkbrothers(linkbrother)

!  set initial node mask to ensure connection with the net outside the selecting polygon
   call set_initial_mask(jarefine, linkbrother)

   nump_virtual = nump
   do ilevel = 1, MAXLEVEL
      numL_old = numL

      jalink = 0

      if (jaCourantnetwork == 1) then
         !     compute cell refinement mask (cells outside polygon will be marked by -1)
         call compute_jarefine(jarefine, jalink, jakdtree, ierror)
         jalink = -jalink ! unset but remember link refinement mask
         if (ierror == 1) goto 1234
         !     smooth the cell refinement mask
         call smooth_jarefine(jarefine, jalink, linkbrother)
      else
         jarefine = 1 ! refine all cells
         jalink = -1 ! refine all links
      end if

      !  disable direct refinement of cells with one or more inactive nodes
      if (ilevel > 1) then
         do ic = 1, nump
            do kk = 1, netcell(ic)%N
               if (kc(netcell(ic)%nod(kk)) /= 1) then
                  jarefine(ic) = 0
                  !call cirr(xzw(ic), yzw(ic), 204)
                  exit
               end if
            end do
         end do
      else ! first level: disable cells with all nodes inactive only
         iclp: do ic = 1, nump
            do kk = 1, netcell(ic)%N
               k = netcell(ic)%nod(kk)
               if (kc(k) /= 0 .and. kc(k) /= -2) cycle iclp ! active node found: discard this cell and continue with next
            end do
            jarefine(ic) = 0
            !call cirr(xzw(ic), yzw(ic), 204)
         end do iclp
      end if

!      call qnerror(' ', ' ', ' ')

      call comp_jalink(jarefine, linkbrother, jalink)

!     also refine cells and link to avoid links with more than one hanging node
      call split_cells(jarefine, jalink, linkbrother)

      numrefine = 0
      do k = 1, nump
         if (jarefine(k) /= 0) numrefine = numrefine + 1
      end do

      if (numrefine == 0) then
         exit ! done
      end if

!     perform the actual refinement
      nump_virtual = nump_virtual * 4
      call refine_cells(jarefine, jalink, linkbrother, ierror)
      netstat = netstat_cells_dirty

!     rearrange worldmesh
      call rearrange_worldmesh(xboundmin, xboundmax)

      if (ierror == 1) goto 1234

!     update cell administration
      if (NPL > 0) call store_and_set_kc()
      call findcells(1000) ! do not update node mask (kc)
      if (NPL > 0) call restore_kc()
!      call findcells(0) ! update node mask (kc)
      call mess(LEVEL_INFO, 'refinement efficiency factor', real(dble(nump_virtual) / dble(max(nump, 1))))

!     take dry cells into account (after findcells)
      call delete_dry_points_and_areas()

      if (jagui == 1) then
         ja = 1
         dxxmax = -huge(1d0); dxxmin = -dxxmax
         do L = 1, numL
            dl = dlinklength(L)
            dxxmin = min(dxxmin, dl)
            dxxmax = max(dxxmax, dl)
         end do
         write (tex, '(2F14.3,I14)') dxxmin, dxxmax, nump
         if (numrefcycles == 0) then
            call confrm('Smallest and largest netlinks and number of cells: '//trim(tex)//' Continue? ', ja)
            if (ja /= 1) exit ! done
         else
            numrefcyc = numrefcyc + 1
            if (numrefcyc == numrefcycles) exit
         end if
      end if

!     reallocate
      call realloc(jarefine, nump, stat=ierror, keepExisting=.false.)
      if (ierror /= 0) goto 1234
      call realloc(jalink, numL, stat=ierror, keepExisting=.false.)
      if (ierror /= 0) goto 1234
!      call realloc(linkbrother, numL, stat=ierror, keepExisting=.true., fill=0)
!      if ( ierror.ne.0 ) gofto 1234

      linkbrother = 0
      call find_linkbrothers(linkbrother)
   end do ! do ilevel=1,MAXLEVEL

!  connect hanging nodes
   if (jagui == 1) then
      jaconnect = 1
      if (numrefcycles == 0) then ! interactive
         call confrm('connect hanging nodes?', jaconnect)
      end if
   end if
   if (jaconnect == 1) then
      where (kc == -1) kc = 1
      if (NPL > 0) call store_and_set_kc()
      call findcells(1000) !     take dry cells into account (after findcells)
      call delete_dry_points_and_areas()
      if (NPL > 0) call restore_kc()

!     remove isolated hanging nodes and update netcell administration (no need for setnodadm)
      call remove_isolated_hanging_nodes(linkbrother, num)

!     check if illegal cells have been created by removing isolated hanging nodes, or by dry/cut-cells
      call write_illegal_cells_to_pol(0)

      call connect_hanging_nodes(linkbrother)

      netstat = netstat_cells_dirty
      keepcircumcenters = 0 ! do not keep circumcenters
   else
      keepcircumcenters = 0 ! keep circumcenters
      call confrm('Keep circumcenters?', keepcircumcenters)
   end if

   ierror = 0

!  error handling

1234 continue

!  deallocate
   if (allocated(jarefine)) deallocate (jarefine)
   if (allocated(jalink)) deallocate (jalink)
   if (allocated(linkbrother)) deallocate (linkbrother)
   if (allocated(zss)) then
      call deallocate_sampleHessian()
      iHesstat = iHesstat_DIRTY
   end if

!  restore original interpolation settings
   interpolationtype = interpolationtype_old
   IAV = IAV_old

!  deallocate kdtree if it was created
   if (Ns >= 1 .and. treeglob%itreestat /= ITREE_EMPTY) call delete_kdtree2(treeglob)

   return

contains

   subroutine store_and_set_kc()
      implicit none

      integer :: k

      if (allocated(kc_sav)) deallocate (kc_sav)

      if (numk < 1) goto 1234

      allocate (kc_sav(numk))

      do k = 1, numk
         kc_sav(k) = kc(k)
         if (kc(k) /= 0) kc(k) = 1
      end do

1234  continue

      return
   end subroutine store_and_set_kc

   subroutine restore_kc()
      implicit none

      integer k

      if (.not. allocated(kc_sav)) goto 1234
      if (.not. allocated(kc)) goto 1234

      if (ubound(kc_sav, 1) < numk) goto 1234
      if (ubound(kc, 1) < numk) goto 1234

      do k = 1, numk
         kc(k) = kc_sav(k)
      end do

      deallocate (kc_sav)

1234  continue

      return
   end subroutine restore_kc

!> do not refine, based on the criterion:
!>    cells with hanging nodes
!>    cells that are crossed by the selecting polygon
!>
!> ensure that no crossed cells have hanging nodes
   subroutine set_initial_mask(jarefine, linkbrother)
      use m_netw

      implicit none

      integer, dimension(:), intent(inout) :: jarefine
      integer, dimension(:), intent(in) :: linkbrother

      integer :: ic, k, kk, kkp1, L, Lp1, N
      integer :: jahang, jacross, jarepeat

      jarepeat = 1

      do while (jarepeat == 1)
         jarepeat = 0
         do ic = 1, nump
!            if ( jarefine(ic).ne.0 ) cycle

            N = netcell(ic)%N
            jahang = 0
            jacross = 0
            do kk = 1, N
               k = netcell(ic)%nod(kk)
               kkp1 = kk + 1; if (kkp1 > N) kkp1 = kkp1 - N
               L = netcell(ic)%lin(kk)
               Lp1 = netcell(ic)%lin(kkp1)

               if ((linkbrother(L) == Lp1 .or. linkbrother(Lp1) == L)) then
                  jahang = 1
               end if

               if (kc(k) == 0) then
                  jacross = 1
               end if
            end do

            if (jacross == 1) then
               jarefine(ic) = 0
               do kk = 1, N
                  k = netcell(ic)%nod(kk)
                  if (kc(k) == 1) then
                     kc(k) = -2
                     jarepeat = 1
                     !call cirr(xk(k), yk(k), 31)
                  end if
               end do
            end if

         end do ! do ic=1,nump

      end do ! do while ( jarepeat.eq.1 )

!      call qnerror(' ', ' ', ' ')

      return
   end subroutine set_initial_mask

!> compute refinement criterion from sample data
   subroutine compute_jarefine(jarefine, jalink, jakdtree, ierror)
      use precision, only: dp
      use m_netw
      use m_samples
      use m_ec_interpolationsettings
      use m_missing

      implicit none

      integer, dimension(:), intent(out) :: jarefine !< refine cell (1) or not (0), dim(nump)
      integer, dimension(:), intent(out) :: jalink !< refine link (1) or not (0), dim(numL)
      integer, intent(inout) :: jakdtree !< use kdtree (1) or not (0)
      integer, intent(out) :: ierror !< error (1) or not (0)

      integer, parameter :: M = 10 ! maximum number of nodes in cell
      integer, parameter :: MAXSUB = 4 ! maxinum number of subcells in cell

      real(kind=dp), dimension(M) :: xloc, yloc ! node list
      integer, dimension(M) :: jarefinelink ! refine links (1) or not (0)
      integer, dimension(M) :: LnnL, Lorg ! not used
      real(kind=dp) :: zz ! not used

      integer :: ic, kk, N
      integer :: L, LL

      integer, parameter :: NDIM = 4 ! sample vector dimension

      integer, parameter :: MMAX = 6
      logical, dimension(MMAX) :: Lhang
      integer, dimension(MMAX) :: ishangingnod

      integer :: numhang, numhangnod, numrefine

      ierror = 1

!     default
      jarefine = 0
      jalink = 0
!      numdots  = 0

      !if ( IPSTAT.ne.IPSTAT_OK ) then
      !   write (6,"('tidysamples')")
      !   call tidysamples(xs,ys,zs,IPSAM,NS,MXSAM,MYSAM) ! uses global kdtree
      !   call get_samples_boundingbox()
      !   IPSTAT = IPSTAT_OK
      !end if

!     cell masking
      do ic = 1, nump
!         N = netcell(ic)%N
!         if ( N.gt.M ) goto 1234
!
!!        check if the whole cell is inside the selecting polygon and store node coordinates
!         jain = 1
!         do kk=1,N
!            k = netcell(ic)%nod(kk)
!            xloc(kk) = xk(k)
!            yloc(kk) = yk(k)
!         end do

!        get the cell polygon that is safe for periodic, spherical coordinates, inluding poles
         call get_cellpolygon(ic, M, N, 1d0, xloc, yloc, LnnL, Lorg, zz)

!        compute orientation vectors of netcell
!         call orthonet_compute_orientation(aspect, u(1), v(1), u(2), v(2), ic)

!        get hanging nodes (jalink, numrefine not used)
         call find_hangingnodes(ic, jalink, linkbrother, numhang, Lhang, numhangnod, ishangingnod, numrefine)

!        compute refinement criterion
         call compute_jarefine_poly(ic, N, xloc, yloc, jarefine(ic), jarefinelink, jakdtree, Lhang)

!        fill jalink from jarefinelink
         do kk = 1, N
            if (jarefinelink(kk) == 1) then
               LL = Lorg(kk)
               if (LL > 0) then
                  L = netcell(ic)%lin(LL)
                  jalink(L) = 1
!                  call adddot(0.5d0*(xk(kn(1,L)) + xk(kn(2,L))), 0.5d0*(yk(kn(1,L)) + yk(kn(2,L))))
               end if
            end if
         end do

!         N = netcell(ic)%N
!         do kk=1,N
!            kkp1 = kk+1; if ( kkp1.gt.N ) kkp1=kkp1-N
!!           find link
!            k1 = netcell(ic)%nod(kk)
!            k2 = netcell(ic)%nod(kkp1)
!            do kkk=1,N
!               L = netcell(ic)%lin(kkk)
!               if ( ( kn(1,L).eq.k1 .and. kn(2,L).eq.k2 ) .or.  &
!                    ( kn(1,L).eq.k2 .and. kn(2,L).eq.k1 ) ) then
!                  if ( jarefinelink(kkk).eq.1 ) then
!                     jalink(L) = 1
!                  end if
!                  exit
!               end if
!            end do
!         end do
      end do

      ierror = 0

!     error handling
1234  continue

!     restore samples
!      call restoresam()

      return

   end subroutine compute_jarefine

!> compute refinement criterion in a polygon
!>    always based on averaging2 or bilinarc
   subroutine compute_jarefine_poly(ic, N, x, y, jarefine, jarefinelink, jakdtree, Lhang)
      use precision, only: dp

      use m_ec_interpolationsettings
      use m_samples, only: NS, xs, ys, zs
      use m_arcinfo
      use m_samples_refine
      use m_physcoef
      use kdtree2Factory
      use m_missing, only: dmiss, JINS
      use m_polygon, only: NPL, xpl, ypl, zpl
      use m_ec_basic_interpolation, only: averaging2, TerrorInfo, triinterp2
      use m_sferic, only: jsferic, jasfer3D
      use geometry_module, only: dbdistance, comp_masscenter

      implicit none

      integer, intent(in) :: ic !< polygon nr
      integer, intent(in) :: N !< polygon size
      integer :: nnn(1) !< polygon size

      real(kind=dp), intent(in) :: x(:), y(:) !< polygon coordinates
      real(kind=dp) :: z(8) !< idem, z

!      real(kind=dp), dimension(N),     intent(in)    :: x, y      !< polygon coordinates
!      real(kind=dp), dimension(2),     intent(in)    :: u, v      !< orientation vectors of polygon
      integer, intent(out) :: jarefine !< refine (1) or not (0)
      integer, dimension(:), intent(out) :: jarefinelink !< refine link (1) or not (0)
      integer, intent(inout) :: jakdtree !< use kdtree (1) or not (0)
      logical, dimension(:), intent(in) :: Lhang !< link with hanging node (.true.) or not (.false.)

      real(kind=dp), dimension(1) :: xc, yc !  polygon center coordinates
      real(kind=dp), dimension(NDIM) :: zc !  interpolated sample vector [zs, DzsDx, DzsDy]
      real(kind=dp) :: zsloc, DzsDx, DzsDy
      real(kind=dp) :: dsize, dcellsize_wanted, dcellsize, dmincellsize, dmaxcellsize
      real(kind=dp), dimension(N) :: dlinklength ! link lengths

      integer, dimension(1) :: isam

      real(kind=dp) :: dval, C, Courant, dlinklengthnew, zmn, zmx

      integer :: ivar, k, kp1, num, ierror, jdla
      integer :: landsea ! cell: 0=sea, 1=landsea, 2=land
      integer :: linkcourant ! link oriented courant icw cell landsea
      real(kind=dp), parameter :: FAC = 1d0
      real(kind=dp), dimension(6) :: transformcoef = 0
      integer :: mxsam, mysam, m
      type(TerrorInfo) :: errorInfo
!      real(kind=dp), parameter                       :: dtol = 1d-8

      jarefine = 0
      jarefinelink = 0
      landsea = 0

      nnn(1) = N

!     compute cell center
      !call comp_masscenter(N, x, y, xc(1), yc(1), area, jacounterclockwise, jsferic, jasfer3D, dmiss) ! try cheap alternative below

!     initialization
      zc = DMISS

      dmincellsize = 1d99
      dmaxcellsize = 0d0
      xc(1) = 0d0; yc(1) = 0d0
      do k = 1, N
         kp1 = k + 1; if (kp1 > N) kp1 = kp1 - N
         dsize = dbdistance(x(k), y(k), x(kp1), y(kp1), jsferic, jasfer3D, dmiss)
         dlinklength(k) = dsize
         dmincellsize = min(dmincellsize, dsize)
         dmaxcellsize = max(dmaxcellsize, dsize)
         xc(1) = xc(1) + x(k)
         yc(1) = yc(1) + y(k)
      end do
      dcellsize = dmaxcellsize

      xc(1) = xc(1) / dble(N)
      yc(1) = yc(1) / dble(N)

      if (irefinetype == ITYPE_RIDGE) then
!------------------------------------------------------------------------
!         ridge detection
!------------------------------------------------------------------------

         if (interpolationtype /= INTP_AVG .or. IAV /= AVGTP_MAX) then
            !         call qnerror('Interpolation type is set to averaging and averaging type to maximum', ' ', ' ')
            interpolationtype = INTP_AVG
            IAV = AVGTP_MAX
         end if

         zc = DMISS
         call averaging2(NDIM, NS, xs, ys, zss, ipsam, xc, yc, zc, 1, x, y, N, nnn, jakdtree, &
                         dmiss, jsferic, jasfer3D, JINS, NPL, xpl, ypl, zpl, errorInfo)

         do ivar = 1, 3
            if (zc(ivar) == DMISS) goto 1234
         end do

         zsloc = zc(1)
         DzsDx = zc(2)
         DzsDy = zc(3)

!          diff = max(abs(DzsDx*u(1)+DzsDy*u(2)), abs(DzsDx*v(1)+DzsDy*v(2)))
!          dcellsize = sqrt(max(u(1)*u(1), u(2)*u(2)))

         dcellsize_wanted = threshold / (abs(zc(4)) + 1d-8)

         if (dcellsize > dcellsize_wanted .and. dcellsize > 2d0 * hmin .and. abs(zc(4)) > thresholdmin) then
!          if ( abs(zc(4)).gt.thresholdmin ) then
            jarefine = 1
            jarefinelink = 1
         else
            jarefine = 0
            jarefinelink = 0
         end if

      else if (irefinetype == ITYPE_WAVECOURANT .or. &
               irefinetype == ITYPE_MESHWIDTH) then
!------------------------------------------------------------------------
!        wave Courant number
!------------------------------------------------------------------------

         if (interpolationtype /= INTP_AVG .or. IAV /= 6) then
            !         call qnerror('Interpolation type is set to averaging and averaging type to minabs', ' ', ' ')
            !interpolationtype = 2
            IAV = AVGTP_MINABS ! minabs
         end if

!        only interpolate samples if necessary
         if (Dt_maxcour > 0d0 .or. irefinetype == ITYPE_MESHWIDTH) then
            zc = DMISS
            if (interpolationtype == 1) then
               if (ic == 1) then
                  jdla = 1; jakdtree = 1
               else
                  jdla = 0; jakdtree = 2
               end if
               mxsam = mca; mysam = nca
               call triinterp2(Xc, Yc, zc, 1, JDLA, & !attemp to allow for smooth refinement used to be ok through jdla, does not work anymore icw kdtree
                               XS, YS, ZS, NS, dmiss, jsferic, jins, jasfer3D, NPL, MXSAM, MYSAM, XPL, YPL, ZPL, transformcoef)
            else if (interpolationtype == 2) then
               call averaging2(1, NS, xs, ys, zs, ipsam, xc, yc, zc, 1, x, y, N, nnn, jakdtree, &
                               dmiss, jsferic, jasfer3D, JINS, NPL, xpl, ypl, zpl, errorInfo)
            else if (interpolationtype == 4) then
               !landsea = 0

               zmx = -1d9; zmn = 1d9
               do m = 1, N
                  call bilinarcinfo(x(m), y(m), z(m))
                  zmx = max(zmx, z(m))
                  zmn = min(zmn, z(m))
               end do
               if (zmn > 0d0) then ! land
                  zc(1) = 9d9; landsea = 3 ! no refine
               else if (zmx >= 0d0 .and. zmn <= 0d0) then ! land/sea
                  zc(1) = 0d9; landsea = 1 ! always refine
               else
                  call bilinarcinfo(xc(1), yc(1), zc(1)); landsea = 2
               end if
            end if

!           check if a value is found, use nearest sample from cell center if not so
            if (zc(1) == DMISS .and. jakdtree == 1 .and. jaoutsidecell == 1) then
               call find_nearest_sample_kdtree(treeglob, Ns, 1, xs, ys, zs, xc(1), yc(1), 1, isam, ierror, jsferic, dmiss)
               if (ierror /= 0) then
                  jakdtree = 0
               else
                  if (isam(1) > 0 .and. isam(1) < Ns + 1) zc(1) = zs(isam(1))
               end if
            end if
            dval = zc(1)
         else
            dval = 0d0
         end if

         if (dval == DMISS) then
            goto 1234
         end if

         jarefine = 0

         num = 0 ! number of links in cell to be refined
         do k = 1, N
            if (dlinklength(k) < tooclose) then
               jarefinelink(k) = 0
               num = num + 1
               cycle
            end if

            dlinklengthnew = 0.5d0 * dlinklength(k)

            if (irefinetype == ITYPE_WAVECOURANT) then
!              compute wave speed
               !if (dval > 2d0 ) dval = 10000d0
               ! C = sqrt(AG*max(-dval,0d0))

               linkcourant = 1 ! checking land sea mask of arcinfo grid and Courant based upon links instead of cells
               if (linkcourant == 1) then
                  if (landsea == 1) then ! coast always refine
                     C = 0d0
                  else if (landsea == 3) then ! land no refine
                     C = 9d9
                  else
                     if (k < N) then
                        k2 = k + 1
                     else
                        k2 = 1
                     end if

                     if (z(k) * z(k2) < 0d0) then
                        C = 0d0
                     else
                        dval = 0.5d0 * (z(k) + z(k2))
                        C = sqrt(AG * abs(dval))
                     end if
                  end if
               else
                  if (dval == 0d0) then
                     C = 0d0
                  else
                     C = sqrt(AG * abs(dval))
                  end if
               end if

!              compute wave Courant number
               Courant = C * Dt_maxcour / dlinklength(k)
               !if ( Courant.lt.1 .and. 0.5d0*dlinklength(k).gt.FAC*hmin ) then
               !if ( Courant.lt.1 .and. abs(dlinklengthnew-Dx_mincour).lt.abs(dlinklength(k)-Dx_mincour) ) then
               if (Courant < 1d0 .and. dlinklengthnew > Dx_mincour) then
                  num = num + 1
                  jarefinelink(k) = 1
               else
                  jarefinelink(k) = 0
               end if
            else if (irefinetype == ITYPE_MESHWIDTH) then
               if (dlinklength(k) > dval .and. dlinklengthnew >= hmin) then
                  num = num + 1
                  jarefinelink(k) = 1
               else
                  jarefinelink(k) = 0
               end if
            end if
         end do

!        check if at least one link needs to be refined
         if (num > 0) then
!           count number of links to be refined, or that are already refined (i.e. have a hanging node)
            num = 0
            do k = 1, N
               if (jarefinelink(k) == 1 .or. Lhang(k)) then
                  num = num + 1
               end if
            end do
         end if

!        check for non-directional refinement and refine all links without hanging nodes if so
         if (jadirectional == 0) then
            if (num == N) then
!               jarefinelink = 1
!              also refine links without hanging nodes
               do k = 1, N
                  if (.not. Lhang(k)) then
                     jarefinelink(k) = 1
                  end if
               end do
            else
               num = 0
               jarefinelink = 0
            end if
         end if

         if (num == 0) then
            jarefine = 0
         else if (num == 1) then
!           do not refine cell with only one link that needs to be, or is already, refined
            jarefine = 0
            jarefinelink = 0
         else
            jarefine = 1
         end if
      else
!------------------------------------------------------------------------
!        undefined type
!------------------------------------------------------------------------
         jarefine = 0
      end if

1234  continue

      return
   end subroutine compute_jarefine_poly

!> smooth the cell refinement mask
   subroutine smooth_jarefine(jarefine, jalink, linkbrother)
      use m_netw

      implicit none

      integer, dimension(:), intent(inout) :: jarefine !< refine cell (1) or not (0), dim: nump
      integer, dimension(:), intent(inout) :: jalink !< refine link (1) or not (0), dim(numL)
      integer, dimension(:), intent(in) :: linkbrother !< brotherlink, that shares a (hanging) node

!      integer, dimension(:), allocatable   :: janode        !  refine around node (1) or not (0), dim: numk
      integer, dimension(:), allocatable :: jalin !  refine at link (1) or not (0), dim: numL

      integer :: iter, ic, kk, L, N

      if (NUMITCOURANT < 1) return ! nothing to do

      if (jadirectional /= 0) then
         call qnerror('directional refinement not allowed in combination with smoothing', ' ', ' ')
         jadirectional = 0
      end if

!     allocate
!      allocate(janode(numk)
      allocate (jalin(numL))

      do iter = 1, NUMITCOURANT
!        determine node refinement mask
!         janode = 0

!        determine link refinement mask
         jalin = 0

         do ic = 1, nump
            if (jarefine(ic) /= 1) cycle

            N = netcell(ic)%N
!            do kk=1,N
!               k = netcell(ic)%nod(kk)
!               janode(k) = 1
!            end do

            do kk = 1, N
               L = netcell(ic)%lin(kk)

!              do not pass on mask to already refined cells
               kkp1 = kk + 1; if (kkp1 > N) kkp1 = kkp1 - N
               kkm1 = kk - 1; if (kkm1 < 1) kkm1 = kkm1 + N

               if (linkbrother(L) /= netcell(ic)%lin(kkp1) .and. linkbrother(L) /= netcell(ic)%lin(kkm1)) then
                  jalin(L) = 1
               end if
            end do
         end do

!        update cell refinement mask
         do ic = 1, nump
            do kk = 1, netcell(ic)%N
!               k = netcell(ic)%nod(kk)
!               if ( janode(k).eq.1 ) then
!                  jarefine(ic) = 1
!                  exit
!               end if
               L = netcell(ic)%lin(kk)
               if (jalin(L) == 1) then
                  jarefine(ic) = 1
                  exit
               end if
            end do
         end do
      end do

!     update link refinement mask
      do ic = 1, nump
         if (jarefine(ic) == 1) then
            N = netcell(ic)%N
            do kk = 1, N
               kkm1 = kk - 1; if (kkm1 < 1) kkm1 = kkm1 + N
               kkp1 = kk + 1; if (kkp1 > N) kkp1 = kkp1 - N

               L = netcell(ic)%lin(kk)

               if (linkbrother(L) == netcell(ic)%lin(kkm1) .or. linkbrother(L) == netcell(ic)%lin(kkp1)) then
                  ! link already refined with hanging node
               else
                  jalink(L) = 1
               end if
            end do
         end if
      end do

!     deallocate
!      if ( allocated(janode) ) deallocate(janode)
      if (allocated(jalin)) deallocate (jalin)

      return
   end subroutine smooth_jarefine

!> refine the cells, based on a cell and link refinement mask
   subroutine refine_cells(jarefine, jalink, linkbrother, ierror)
      use precision, only: dp

      use m_comp_middle_latitude
      use geometry_module, only: getcircumcenter
      use m_find_common_node
      use m_new_link

      implicit none

      integer, dimension(:), intent(inout) :: jarefine !< refine cell (>0) or not (0), dim(nump)
      integer, dimension(:), intent(inout) :: jalink !< refine link (>0) or not (0), dim(numL)
      integer, allocatable, dimension(:), intent(inout) :: linkbrother !< brotherlink, that shares a (hanging) node
      integer, intent(out) :: ierror !< error (1) or not (0)

      real(kind=dp) :: xnew, ynew
      real(kind=dp) :: ymin, ymax
      integer, parameter :: MMAX = 10 ! maximum number of links per netcell
      integer, dimension(MMAX) :: nods ! list with new nodes
      integer :: i, k, k2, k3, kk, kkm1, kkp1, kkk
      integer :: knew, k1
      integer :: L, Lm1, Lp1, Lnew, N
      integer :: num, numbrothers
      integer :: Lsize
      integer :: ierr

      logical :: Lparentcross ! original parent cell crossed by selecting polygon (.true.) or not (.false.)
      logical :: Lrefine ! refine cell (.true.) or not (.false.)
      integer :: Np
      integer, dimension(MMAX) :: ishanging
      real(kind=dp) :: xz, yz
      real(kind=dp), dimension(MMAX) :: xp, yp

!     for spherical, periodic coordinates
      real(kind=dp), dimension(MMAX) :: xv, yv
      integer, dimension(MMAX) :: LnnL
      integer, dimension(MMAX) :: Lorg
      real(kind=dp) :: zz
      integer :: nn
      logical :: Lpole1, Lpole2

      ierror = 1

!     first add new nodes at the links that need to be refined (jalink will contain new node number),
!     then make the internal links in the cells, and
!     then split the original links

      KN3TYP = 2 ! 2d links only

!     add new nodes
      do L = 1, numL
         if (jalink(L) /= 0) then
            k1 = kn(1, L)
            k2 = kn(2, L)
            xnew = 0.5d0 * (xk(k1) + xk(k2))
            ynew = 0.5d0 * (yk(k1) + yk(k2))

            if (jsferic == 1) then
               call comp_middle_latitude(yk(k1), yk(k2), ynew, ierr)
            end if

!           fix for spherical, periodic coordinates
            if (jsferic == 1) then
               if (abs(xk(k1) - xk(k2)) > 180d0) then
                  xnew = xnew + 180d0
               end if

!              fix at the poles (xk can have any value at the pole)
               Lpole1 = abs(abs(yk(k1)) - 90d0) < dtol_pole
               Lpole2 = abs(abs(yk(k2)) - 90d0) < dtol_pole
               if (Lpole1 .and. .not. Lpole2) then
                  xnew = xk(k2)
               else if (.not. Lpole1 .and. Lpole2) then
                  xnew = xk(k1)
               end if
            end if

            call dsetnewpoint(xnew, ynew, knew)
            jalink(L) = knew
            if (kc(k1) == 0 .and. kc(k2) == 0) then
               kc(knew) = 0
            else if (kc(k1) /= 1 .or. kc(k2) /= 1) then
               kc(knew) = -1
            else
               kc(knew) = 1
            end if
         end if
      end do

!     connect new nodes in cells
      do k = 1, nump
!         write(6,*) k, jacell(k)

         if (jarefine(k) == 0) cycle

         N = netcell(k)%N

!        determine if the oldest parent of this cell is crossed by the selecting polygon
         Lparentcross = .false.
         do kk = 1, N
            if (kc(netcell(k)%nod(kk)) /= 1) then
               Lparentcross = .true.
               exit
            end if
         end do

!        fix for global. spherical coordinates
         call get_cellpolygon(k, MMAX, nn, 1d0, xv, yv, LnnL, Lorg, zz)

!          BEGIN DEBUG
!           call tekpoly(nn,xv,yv,31)
!           call toemaar()
!           call tekpoly(nn,xv,yv,1)
!          END DEBUG

!        find the number of hanging nodes
         num = 0 ! number of non-hanging nodes
         numbrothers = 0 ! number of hanging nodes
         nods = 0 ! all-nodes list
         Lrefine = .true. ! refine cell (.true.) or not (.false.)
         ishanging = 0
!    kklp:do kk=1,N
         do kk = 1, nn
!            kkm1 = kk-1; if ( kkm1.lt.1 ) kkm1 = kkm1+N
            kkm1 = kk - 1; if (kkm1 < 1) kkm1 = kkm1 + nn
!            kkp1 = kk+1; if ( kkp1.gt.N ) kkp1 = kkp1-N
            kkp1 = kk + 1; if (kkp1 > nn) kkp1 = kkp1 - nn
!            L  = netcell(k)%lin(kk)
!            Lm1 = netcell(k)%lin(kkm1)
!            Lp1 = netcell(k)%lin(kkp1)
            L = Lorg(kk)
            if (L > 0) L = netcell(k)%lin(L)
            Lm1 = Lorg(kkm1)
            if (Lm1 > 0) Lm1 = netcell(k)%lin(Lm1)
            Lp1 = Lorg(kkp1)
            if (Lp1 > 0) Lp1 = netcell(k)%lin(Lp1)

            if (L == 0) then
!              fictitious link at pole
               cycle
            end if

            if (linkbrother(L) == Lp1 .and. Lp1 > 0) then
               numbrothers = numbrothers + 1
               call find_common_node(L, linkbrother(L), knew)
               num = num + 1
               nods(num) = knew
            else if (Linkbrother(L) /= Lm1 .or. Linkbrother(L) == 0) then
               if (jalink(L) == 0) then
                  !Lrefine = .false.
               else
                  num = num + 1
                  nods(num) = jalink(L)
               end if
            end if
            if (num > MMAX) goto 1234

!           check if start of this link is hanging
            if (Linkbrother(L) == Lm1 .and. Lm1 > 0) then
               ishanging(kk) = 1
            end if
         end do

!        check if this cell needs to be refined
         if (.not. Lrefine) cycle

!        compute new center node: circumcenter without hanging nodes for quads, c/g otherwise
         Np = 0
!         do kk=1,N
!         kk = 0
         do kkk = 1, nn
            L = Lorg(kkk)
!             if ( L.eq.0 ) then
!!               next attached link is fictitious link at pole
!                Lhanging = .false.
!             else
!                L = netcell(k)%lin(L)
!                kk = kk+1
!
!                if ( kk.gt.N ) then ! something wrong
!                   call qnerror('refine_cells: numbering error', ' ', ' ')
!                end if
!
!                kp = netcell(k)%nod(kk)
!                Lhanging = .false.
!                do i=1,num
!                    if ( kp.eq.nods(i) ) then
!                    Lhanging = .true.
!                    exit
!                    end if
!                end do
!             end if

!             if ( .not.Lhanging ) then

            if (ishanging(kkk) == 0) then
               Np = Np + 1
!                 xp(Np) = xk(kp)
!                 yp(Np) = yk(kp)
!                 LnnL(Np) = 2

               xp(Np) = xv(kkk)
               yp(Np) = yv(kkk)
               if (L > 0) then
                  LnnL(Np) = lnn(L)
               else
                  LnnL(Np) = 1
               end if
            end if
         end do

         if (Np == 4) then
            if (jsferic == 1) then
               ymin = 1d99
               ymax = -ymin
               do i = 1, Np
                  if (yp(i) < ymin) then
                     ymin = yp(i)
                  end if
                  if (yp(i) > ymax) then
                     ymax = yp(i)
                  end if
               end do
            end if

!           compute circumcenter without hanging nodes
            call getcircumcenter(Np, xp, yp, LnnL, xz, yz, jsferic, jasfer3D, jglobe, jins, dmiss, dxymis, dcenterinside)

            if (jsferic == 1) then
               call comp_middle_latitude(ymin, ymax, ynew, ierr)
               if (ierr == 0 .and. ymax - ymin > 1d-8) then
                  yz = ymin + 2d0 * (ynew - ymin) / (ymax - ymin) * (yz - ymin)
               end if
            end if
         else
!           use masscenter
            xz = xzw(k)
            yz = yzw(k)
         end if

         !call cirr(xz, yz, 211)

!        split the cell
         if (Np >= 4) then
            if (num > 2) then
               call dsetnewpoint(xz, yz, knew)
               do kk = 1, num
                  call newlink(nods(kk), knew, Lnew)
               end do
               if (.not. Lparentcross) then
                  kc(knew) = 1
               else
                  kc(knew) = -1 ! deactive nodes in cells crossed by polygon
               end if
            else if (num == 2) then
               call newlink(nods(1), nods(2), Lnew)
            end if
         else
            do kk = 1, num
               kkp1 = kk + 1; if (kkp1 > num) kkp1 = kkp1 - num
               call newlink(nods(kk), nods(kkp1), Lnew)
            end do
         end if

      end do

!     split original links
      do L = 1, numL_old
         if (jalink(L) > 0) then
            k1 = kn(1, L)
            k2 = kn(2, L)
            k3 = kn(3, L)
            knew = jalink(L)
            kn(2, L) = knew
            call newlink(knew, k2, Lnew)
            kn(3, Lnew) = k3
!           set the brother links
            Lsize = ubound(linkbrother, 1)
            if (numL > ubound(linkbrother, 1)) then
               Lsize = ceiling(1.2d0 * dble(numL + 1))
               call realloc(linkbrother, Lsize, keepExisting=.true., fill=0)
            end if
            linkbrother(Lnew) = L
            linkbrother(L) = Lnew
         end if
      end do

      ierror = 0

!     error handling
1234  continue

      if (jsferic > 0) then
         do k = 1, numk
            call inworld(xk(k))
         end do
      end if

      return
   end subroutine refine_cells

!> split cells before refinement that:
!>   have more nodes than allowed
!>   have links with hanging nodes that need to be refined
!>
!> note: linkmask is set and the actual splitting is performed by refine_cells
!> it is assumed that findcells leaves the link numbering intact
   subroutine split_cells(cellmask, linkmask, linkbrother)
      ! we can't use m_netw because it overwrites cellmask, redefine variable before using m_netw
      !use m_netw
      use m_plotdots

      implicit none

      integer, dimension(:), intent(inout) :: linkmask !< new nodes on links
      integer, dimension(:), intent(inout) :: cellmask !< refine cell without hanging nodes (1), refine cell with hanging nodes (2), do not refine cell at all (0) or refine cell outside polygon (-2)
      integer, dimension(:), intent(in) :: linkbrother !< brotherlink, that shares a (hanging) node

      character(len=128) :: FNAM

      integer :: jawritten

      integer :: ic, L, kk, N
      integer :: jasplit, num, numrefine, numhang, numhangnod, N_eff
      integer :: ierror

      integer :: iter
      integer, parameter :: MAXITER = 1000

      integer, parameter :: MMAX = 6
      logical, dimension(MMAX) :: Lhang
      integer, dimension(MMAX) :: ishangingnod

      ierror = 1

      iter = 0
      num = 1
      do while (num /= 0)
         iter = iter + 1
         if (iter > MAXITER) goto 1234

         num = 0
         do ic = 1, nump
            if (cellmask(ic) /= 0 .and. cellmask(ic) /= -1) cycle

            call find_hangingnodes(ic, linkmask, linkbrother, numhang, Lhang, numhangnod, ishangingnod, numrefine)

            jasplit = 0

            N = netcell(ic)%N
            if (N > MMAX) goto 1234

            do kk = 1, N
               L = netcell(ic)%lin(kk)
               !           check if the link has a brother link and needs to be refined
               if (Lhang(kk) .and. linkmask(L) > 0) then
                  jasplit = 1
               end if
            end do

!           compute the effective cell type
            N = netcell(ic)%N
            N_eff = N - numhang / 2
            if (2 * (N - N_eff) /= numhang) then
               call qnerror('splitcell: uneven number of brotherlinks', ' ', ' ')
               call adddot(xzw(ic), yzw(ic))
            end if

!           prevent certain cell types
            if (N + numrefine > MMAX) jasplit = 1 ! would result in unsupported cells after refinement
            if (N - numhang - numrefine <= 1) jasplit = 1 ! cells with only one unrefined edge
            if (N_eff == numrefine) jasplit = 1 ! all links refined

            if (jasplit == 1) then
               if (cellmask(ic) /= -1) then
                  cellmask(ic) = 2
               else
                  cellmask(ic) = -2
               end if

               do kk = 1, N
                  L = netcell(ic)%lin(kk)
                  if (linkmask(L) == 0 .and. .not. Lhang(kk)) then
                     linkmask(L) = 1
                     num = num + 1

                     if (iter == MAXITER) then
!                        call adddot(xzw(ic),yzw(ic))
                        call adddot(0.5d0 * (xk(kn(1, L)) + xk(kn(2, L))), 0.5d0 * (yk(kn(1, L)) + yk(kn(2, L))))
                     end if

                  end if
               end do
            end if
         end do ! do ic=1,nump

         write (6, *) iter, num

      end do ! do while ( num.ne.0 )

      ierror = 0

!     error handling
1234  continue

      if (ierror /= 0) then
         FNAM = 'split_cells_errors.xyz'
         call write_dots(trim(FNAM), jawritten)
         if (jawritten == 1) then
            call qnerror('split_cells: no convergence, output written to'//trim(FNAM), ' ', ' ')
         else
            call qnerror('split_cells: no convergence', ' ', ' ')
         end if
      end if

      return
   end subroutine split_cells

!> find the brother links
!>    hanging nodes are assumed to have two consecutive brother links
   subroutine find_linkbrothers(linkbrother)
      use precision, only: dp
      use m_comp_middle_latitude
      use m_netw
      use m_sferic
      use geometry_module, only: dbdistance
      use m_missing, only: dmiss
      use m_tek_link

      implicit none

      integer, dimension(:), intent(inout) :: linkbrother !< brother links, that share a common hanging node, dim(numL)

      real(kind=dp) :: xkc, ykc, dtol
      real(kind=dp) :: xmn, xmx ! for periodic coordinates

      integer :: k, k1, k2, kk, kkp1, L, Lp1
      integer :: ic1L, ic1R, ic2L, ic2R
      integer :: ierr

      logical :: Lpole1, Lpole2

      do k = 1, numk
!       check if the node is a hanging node
!        if ( nmk(k).ne.3 ) cycle

!       check all combinations of connected links
         do kk = 1, nmk(k)
            kkp1 = kk + 1; if (kkp1 > nmk(k)) kkp1 = kkp1 - nmk(k)

            L = nod(k)%lin(kk)
            if (lnn(L) < 1) cycle
!           if ( lnn(L).ne.2 )         cycle  ! inner links only
!           if ( linkbrother(L).ne.0 ) cycle  ! allready has a brother link

            Lp1 = nod(k)%lin(kkp1)
            if (lnn(Lp1) < 1) cycle
!           if ( lnn(L).ne.2 )           cycle   ! inner links only
!           if ( linkbrother(Lp1).ne.0 ) cycle   ! allready has a brother link

!          both links have to share a common cell
            ic1L = lne(1, L)
            ic1R = lne(min(lnn(L), 2), L)
            ic2L = lne(1, Lp1)
            ic2R = lne(min(lnn(Lp1), 2), Lp1)
            if (ic1L /= ic2L .and. ic1L /= ic2R .and. ic1R /= ic2L .and. ic1R /= ic2R) then
               cycle
            end if

!          check if node k is in the middle
            k1 = kn(1, L) + kn(2, L) - k
            k2 = kn(1, Lp1) + kn(2, Lp1) - k
            xkc = 0.5d0 * (xk(k1) + xk(k2))
            ykc = 0.5d0 * (yk(k1) + yk(k2))

            if (jsferic == 1) then
               call comp_middle_latitude(yk(k1), yk(k2), ykc, ierr)
            end if

!          check for periodic, spherical coordinates
            if (jsferic == 1) then

!             check for poles
               Lpole1 = .false.
               Lpole2 = .false.
               if (abs(abs(yk(k1)) - 90d0) < dtol_pole) then
                  Lpole1 = .true.
               end if
               if (abs(abs(yk(k2)) - 90d0) < dtol_pole) then
                  Lpole2 = .true.
               end if

               if (Lpole1 .and. .not. Lpole2) then
                  xkc = xk(k2)
               else if (Lpole2 .and. .not. Lpole1) then
                  xkc = xk(k1)
               else
                  xmn = min(xk(k1), xk(k2))
                  xmx = max(xk(k1), xk(k2))

                  if (xmx - xmn > 180d0) then
                     xkc = xkc + 180d0
                  end if
               end if
            end if

!          compute tolerance
            dtol = 1d-4 * max(dbdistance(xk(k1), yk(k1), xk(k), yk(k), jsferic, jasfer3D, dmiss), &
                              dbdistance(xk(k2), yk(k2), xk(k), yk(k), jsferic, jasfer3D, dmiss))
            if (dbdistance(xk(k), yk(k), xkc, ykc, jsferic, jasfer3D, dmiss) < dtol) then ! brother links found
               linkbrother(L) = Lp1
               linkbrother(Lp1) = L
               call teklink(L, 210)
               call teklink(Lp1, 210)
!              call toemaar()
!              call teklink(L,1)
!              call teklink(Lp1,1)
               exit ! brother links found for this node, continue with next node
            end if
         end do

      end do

      return
   end subroutine find_linkbrothers

! compute link refinement mask
   subroutine comp_jalink(jarefine, linkbrother, jalink)
      use m_netw
      use m_plotdots
      use messagehandling
      use m_plot_dots
      implicit none

      integer, dimension(:), intent(inout) :: jarefine !< refine cell (>0), or not
      integer, dimension(:), intent(in) :: linkbrother !< brotherlink, that shares a (hanging) node
      integer, dimension(:), intent(inout) :: jalink !< in: refine link (<0) or not (0), out: refine link (1) or not (0)

      integer, parameter :: MMAX = 10 ! maximum number of nodes and links per netcell

      integer :: numhang ! number of links with hanging node
      logical, dimension(MMAX) :: Lhang ! link with hanging node (true) or not (false)
      integer :: numhangnod ! number of hanging nodes
      integer, dimension(MMAX) :: ishangingnod ! hanging node (1) or not (0)
      integer :: numrefine ! number of links to be refined

      integer, dimension(MMAX) :: numlink ! link identifier for quads
      integer, dimension(5) :: jaquadlink ! refine quad edge (<>0) or not (0)
      ! was 4
      integer :: num, N_eff

      integer :: k, kk, kkm1, kkp1, L, N
      integer :: numfirst, numnext
      integer :: jarepeat, ja_doall

      integer :: iter
      integer, parameter :: MAXITER = 6

      !  compute the link refinement mask
      jarepeat = 1
      iter = 0
!      numdots=0
      do while (jarepeat == 1 .and. iter < MAXITER)
         iter = iter + 1
         jarepeat = 0
         do k = 1, nump
            if (jarefine(k) /= 0) then
               N = netcell(k)%N

               call find_hangingnodes(k, jalink, linkbrother, numhang, Lhang, numhangnod, ishangingnod, numrefine)
               N_eff = N - numhangnod

               if (N_eff /= 4) then ! non-quads
                  do kk = 1, N
                     kkp1 = kk + 1; if (kkp1 > N) kkp1 = kkp1 - N
                     kkm1 = kk - 1; if (kkm1 < 1) kkm1 = kkm1 + N

                     L = netcell(k)%lin(kk)

                     num = numlink(kk)
                     if (linkbrother(L) == netcell(k)%lin(kkm1) .or. linkbrother(L) == netcell(k)%lin(kkp1)) then
                        ! link already refined with hanging node
                     else
                        jalink(L) = 1
                     end if
                  end do
               else ! quads

!                 number the links in the cell, links that share a hanging node will have the same number
                  num = 1
                  jaquadlink = 0
                  do kk = 1, N
                     L = netcell(k)%lin(kk)
                     numlink(kk) = num
                     if (jalink(L) /= 0) jaquadlink(num) = jalink(L)

                     kkp1 = kk + 1; if (kkp1 > N) kkp1 = kkp1 - N
                     if (kk /= N .and. linkbrother(L) /= netcell(k)%lin(kkp1)) then
                        num = num + 1
                     else if (linkbrother(L) == netcell(k)%lin(kkp1)) then
                        jaquadlink(num) = 1 ! already refined (with hanging node)
                     end if
                  end do

!                 check if we found all quad edges
                  if (num /= 4) then
                     if (numrefcycles == 0) then
                        call qnerror('comp_jalink: numbering error numrefcycles=0', ' ', ' ')
                     else
                        call mess(level_warn, 'comp_jalink: numbering error numrefcycles>0', ' ', ' ')
                     end if
                     goto 1234
                  end if

!                 quads may only be refined horizontally, vertically or both
                  numrefine = 0
                  numfirst = 0
                  numnext = 0
                  do num = 1, 4
                     if (jaquadlink(num) /= 0) then
                        numrefine = numrefine + 1
                        if (numfirst == 0) then
                           numfirst = num
                        else if (numnext == 0) then
                           numnext = num
                        end if
                     end if
                  end do

                  num = numnext - numfirst
                  ja_doall = 0
                  if (numrefine == 2 .and. (num == 1 .or. num == 3)) then
                     jarepeat = 1
                     ja_doall = 1
                     if (iter == MAXITER) then
!                        call adddot(xzw(k),yzw(k))
                     end if
                  end if

                  do kk = 1, N
                     kkp1 = kk + 1; if (kkp1 > N) kkp1 = kkp1 - N
                     kkm1 = kk - 1; if (kkm1 < 1) kkm1 = kkm1 + N

                     L = netcell(k)%lin(kk)

                     if (jalink(L) > 0) cycle ! link already marked for refinement

                     if (ja_doall /= 1 .and. jalink(L) /= -1) cycle

                     num = numlink(kk)
                     if (num == numlink(kkm1) .or. num == numlink(kkp1)) then
                        ! link already refined with hanging node
                     else
                        jalink(L) = 1
                     end if

                  end do
               end if
            end if
         end do
      end do

      if (jarepeat == 1) then
         call plotdots()
!         write(6,*) "numdots=", numdots
!         write(6,*) "iter=", iter
         call qnerror('comp_jalink: no convergence', ' ', ' ')
      end if

!     only keep jalink=1, set other values to 0
      do L = 1, numL
         if (jalink(L) /= 1) jalink(L) = 0
      end do

1234  continue

      return
   end subroutine comp_jalink

   subroutine find_hangingnodes(ic, linkmask, linkbrother, numhang, Lhang, numhangnod, ishangingnod, numrefine)
      use m_netw
      use m_find_common_node
      implicit none

      integer, parameter :: MMAX = 6 ! maximum number of nodes and links per netcell

      integer, intent(in) :: ic !< cell number

      integer, dimension(:), intent(inout) :: linkmask !< new nodes on links
      integer, dimension(:), intent(in) :: linkbrother !< brotherlink, that shares a (hanging) node
      integer, intent(out) :: numhang !< number of links with hanging node
      logical, dimension(:), intent(out) :: Lhang !< link with hanging node (true) or not (false)
      integer, intent(out) :: numhangnod !< number of hanging nodes
      integer, dimension(:), intent(out) :: ishangingnod !< hanging node (1) or not (0)
      integer, intent(out) :: numrefine !< number of links to be refined

      integer :: i, k, kk, kkm1, kkp1, kknod
      integer :: L, Lm1, Lp1, N

      N = netcell(ic)%N
      if (N > MMAX) goto 1234

      Lhang = .false.
      ishangingnod = 0
      numhang = 0
      numhangnod = 0
      kknod = 0 ! pointer to node in cell
      numrefine = 0
      do kk = 1, N
         L = netcell(ic)%lin(kk)
         if (linkmask(L) /= 0) numrefine = numrefine + 1
!        check if the link has a brother link and needs to be refined
         if (linkbrother(L) /= 0) then
!        check if the brother link is in the cell
            kkm1 = kk - 1; if (kkm1 < 1) kkm1 = kkm1 + N
            kkp1 = kk + 1; if (kkp1 > N) kkp1 = kkp1 - N
            Lm1 = netcell(ic)%lin(kkm1)
            Lp1 = netcell(ic)%lin(kkp1)

!           find hanging node
            if (linkbrother(L) == Lm1) then
               call find_common_node(L, Lm1, k)
            else if (linkbrother(L) == Lp1) then
               call find_common_node(L, Lp1, k)
            else
               k = 0
            end if

            if (k /= 0) then
!              hanging node found
               Lhang(kk) = .true.
               numhang = numhang + 1

!              find node pointer in cell
               do i = 1, N
                  kknod = kknod + 1; if (kknod > N) kknod = kknod - N

                  if (netcell(ic)%nod(kknod) == k .and. ishangingnod(kknod) == 0) then
                     numhangnod = numhangnod + 1
                     ishangingnod(kknod) = 1
                     exit
                  end if
               end do

            end if

         end if
      end do

1234  continue

      return
   end subroutine find_hangingnodes

end subroutine refinecellsandfaces2

end module m_refinecellsandfaces2
