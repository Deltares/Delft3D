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

!> net orthogonalisation and smoothing
module m_orthogonalisenet
   use m_spher2loc, only: spher2loc
   use m_loc2spher, only: loc2spher
   use m_svdcmp, only: svdcmp
   use m_sortlinks, only: sortlinks
   use m_snap_to_landboundary, only: snap_to_landboundary
   use m_orthonet_project_on_boundary, only: orthonet_project_on_boundary
   use m_orthonet_compute_aspect, only: orthonet_compute_aspect

   implicit none

   private

   public :: orthogonalisenet

contains

   subroutine ORTHOGONALISENET(jarerun)
      use m_fliplinks, only: fliplinks
      use m_find_nearest_meshline, only: find_nearest_meshline
      use precision, only: dp
      use m_teknet
      use m_copynetnodestosam
      use m_halt3
      use m_confrm
      use m_netw
      use m_flowgeom, only: ndx, lnx
      use m_sferic
      use m_orthosettings
      use m_missing, only: jins, dmiss
      use m_samples
      use m_inverse_map
      use unstruc_colors, only: ncolhl
      use m_ec_basic_interpolation, only: triinterp2
      use geometry_module, only: dbpinpol
      use fm_external_forcings_data, only: transformcoef
      use gridoperations
      use m_drawthis
      use m_readyy
      use m_qnerror
      use m_delsam
      use m_makenetnodescoding
      use m_cirr
      use m_orthonet_admin

      integer :: jarerun !< rerun, no=1, yes=1

      real(kind=dp), dimension(:, :), allocatable :: ww, rhs ! weights and right-hand sides
      real(kind=dp), dimension(:), allocatable :: aspect ! aspect ratio of the links
      real(kind=dp), dimension(:), allocatable :: smp_mu ! coefficients in Laplacian smoother
      real(kind=dp), dimension(:), allocatable :: xkb, ykb ! copy for original boundary nodes

      integer, dimension(:, :), allocatable :: kk1 ! neighboring nodes
      integer, dimension(:), allocatable :: k_bc ! maps nodes to nearest initial-boundary node
      integer, dimension(:), allocatable :: ic, jc ! netnode indices in the curvi-grid

      integer, dimension(2) :: ibounds

      real(kind=dp) :: x0, y0, ATPF1, relaxin, relax1

!  used for sferical
      real(kind=dp) :: x00, y00, y1, DUM(2), w0(2)
      real(kind=dp), dimension(2) :: righthandside

      integer :: i, k, kk, k1, L, n, no, npl_old
      integer :: JSFERICold, ja, ja1, ja3

      integer, save :: idir = -999 ! direction of mesh adaptation

      logical :: Lteknet

      real(kind=dp), parameter :: EPS = 1d-4

      real(kind=dp) :: mu, mumin, mumax, mumat, wwx, wwy

      real(kind=dp), allocatable, dimension(:, :) :: ww2, ww2x, ww2y ! weights

      integer :: ierror ! 0: no error, 1: error

      type(tops), allocatable, dimension(:) :: ops ! array of structure with operators of unique topologies

      real(kind=dp) :: atpf_loc, atpf1_loc
      real(kind=dp), dimension(4) :: J ! Jacobian matrix
      logical :: Lcopymu ! copy initial-mesh data to smp_mu (.true.) or not (.false.)
      real(kind=dp), allocatable, dimension(:) :: zk_bak ! backup copy of zk
      real(kind=dp) :: smpminn, smpmaxx
      real(kind=dp) :: atpf_min
      logical :: Ltemp
      real(kind=dp) :: circormass_bak
      integer :: ik

      real(kind=dp) :: Dx0, Dy0
      real(kind=dp), allocatable, dimension(:) :: xloc, yloc ! local coordinates
      integer, allocatable, dimension(:) :: iloc ! startpointers in local coordinate arrays, dim(numk+1)
      real(kind=dp), dimension(1) :: dumx, dumy

      jarerun = 0

! return if the network comprises three nodes or less
      if (numk < 4) return

      if (jaswan == 1 .and. atpf <= 0.8d0) then
         call fliplinks()
      end if

! store original settings
      JSFERICold = JSFERIC
      allocate (zk_bak(size(zk)))
      zk_bak = zk
      circormass_bak = circumormasscenter

!   ATPF = 0d0
      mumax = (1d0 - smoothorarea) * 0.5d0
      mumin = 1d-2
      mumin = min(mumin, mumax)

      ATPF1 = 1d0 - ATPF

!  make the node mask
      kc = 0
      ik = -1
      do k = 1, numk
         call dbpinpol(xk(k), yk(k), ik, dmiss, JINS, NPL, xpl, ypl, zpl)
         if (ik > 0) kc(k) = ik
      end do

!   if ( netstat.ne.netstat_OK ) call findcells(100)
!  always call findcells
      call findcells(100)

!  account for folded cells
      call sortlinks()

      call makenetnodescoding() ! No relinking at the moment: makenetnodescoding outside of numortho loop

!  mark nodes outside polygon as stationary
      do k = 1, numk
         if (kc(k) == 0) nb(k) = 3
      end do

!  snap to nearest land boundary
      if (JAPROJECT > 1) then
         ja = 1
         if (jaswan /= 1) then
            call confrm('Refresh net-to-land administration?', ja)
         else
            ja = 1
         end if
!      if ( ja.eq.1 ) call find_nearest_meshline(JAPROJECT) ! net boundaries only

         if (ja == 1) then
            call find_nearest_meshline(JAPROJECT)
         end if

         Ltemp = (ja == 1)
         if (ja == 0 .and. allocated(lanseg_map)) then
            if (ubound(lanseg_map, 1) == numk) then
               Ltemp = .true.
            end if
         end if

         if (.not. Ltemp) then
            call qnerror('net-to-land administration out of date: falling back to ''netbound to orig. netbound''', ' ', ' ')
            JAPROJECT = 1
         else
            !if (japroject <= 4) call snap_to_landboundary()
         end if
      end if

! Mark flow geometry as reset to prevent any crashes on redrawing with incomplete xz/yz arrays:
! Moreover: xz ordering is here still by netcell order, and *before* the flow node renumbering.
      ndx = 0
      lnx = 0

! nmkx is max nr of neighbouring netnodes for any node.
      nmkx = 0
      do k = 1, numk
         nmkx = max(nmkx, nmk(k))
      end do

!-------------------------------------------------
!  allocate arrays
      nmkx = nmkx + 1 ! Possibly one additional dummy point at boundary nodes.
      if (allocated(xk1)) deallocate (xk1, yk1)
      if (allocated(ww)) deallocate (ww, kk1)

      allocate (xk1(numk), yk1(numk), ww(nmkx, numk), kk1(nmkx, numk))
      allocate (rhs(2, numk), aspect(numL), smp_mu(numk), k_bc(numk))
      allocate (xkb(numk), ykb(numk))
!   allocate(theta_save(nmkx,1), xi_save(nmkx,1), eta_save(nmkx,1), ktopo(numk))
!   allocate(nmk_save(1), nmk2_save(1))
      allocate (ktopo(numk))
!   allocate(atpf_nodes(numk))

!----------------------
!  for smoother
!----------------------
      allocate (nmk2(numk))
      nmk2 = 0
      allocate (kk2(nmkx2, numk))
      kk2 = 0
      allocate (ww2(nmkx2, numk))
      ww2 = 0d0

!-------------------------------------------------
! initialise
      xkb = xk(1:numk)
      ykb = yk(1:numk)
      rhs = 0d0
      aspect = DMISS
      smp_mu = 1d0
      ja = 1
      ja3 = 0
      Lteknet = .true.
      mu = mumin
      numtopo = 0
      ktopo = 0
      Lcopymu = .true.
      atpf_min = 0.8d0

      if (idir == -999) idir = 0

!  k_bc stores nearest original netnode
      do k = 1, numk
         k_bc(k) = k
      end do

!-------------------------------------------------
! determine node neighbors
      kk1 = 0
      do k = 1, numk ! kk admin
         if (nb(k) == 1 .or. nb(k) == 2 .or. nb(k) == 3 .or. nb(k) == 4) then ! kc(k) == 1) then
            do kk = 1, nmk(k)
               L = nod(k)%lin(kk)
               call othernode(k, L, kk1(kk, k))
            end do
         end if
      end do

!-------------------------------------------------
!  get the netnode indices ic and jc in the curvi-grid
!   if ( Ns.lt.0 ) then
!      allocate(ic(numk), jc(numk))
!      ic = IMISS
!      jc = IMISS
!
!      do k1=1,numk
!         if ( kc(k1).ne.1 ) cycle
!         x0 = xk(k1)
!         y0 = yk(k1)
!         call assign_icjc(x0,y0, ic, jc, iexit)
!         if ( ic(k1).ne.IMISS .and. jc(k1).ne.IMISS ) exit
!      end do
!   end if
!-------------------------------------------------

!-------------------------
!  for local coordinates
!-------------------------
      allocate (iloc(numk + 1))

      if (jsferic == 1 .and. jasfer3D == 1) then
         if (ATPF < 1d0) then
            ierror = 1
            call orthonet_comp_ops(ops, ierror) ! will make kk2 administration
            if (ierror /= 0) goto 1234
         end if

!     make startpointers
         iloc(1) = 1
         do k = 1, numk
            iloc(k + 1) = iloc(k) + max(nmk(k) + 1, nmk2(k)) ! include own node
         end do

!     allocate local coordinate arrays
         N = iloc(numk + 1) - 1
         allocate (xloc(N), yloc(N))
      end if

!----------------------
!  iterations
!----------------------
      call readyy('Orthogonalising net', 0d0)
      tp: do no = 1, itatp
         !     call removesmalllinks()

         xk1(1:numk) = xk(1:numk)
         yk1(1:numk) = yk(1:numk)

!     compute local coordinates
         if (jsferic == 1 .and. jasfer3D == 1) then
            call comp_local_coords(iloc, kk1, xk, yk, iloc(numk + 1) - 1, xloc, yloc)
         end if

         call readyy('Orthogonalising net', dble(no - 1 + .35d0) / itatp)

!------------------------------------------------------------------------
!     mesh adaptation
!------------------------------------------------------------------------
         if (Ns == 0 .and. Lcopymu .and. ATPF < 1d0 .and. adapt_beta > 0d0) then
!        use old mesh data: set smp_mu to 1/(determinant of Jacobian), scaled
!        only once, use initial mesh

!        compute the operators
            if (.not. allocated(ops)) then
               ierror = 1
               call orthonet_comp_ops(ops, ierror)
               if (ierror /= 0) goto 1234
            end if

            J = 0d0
            smpminn = 0d0
            smpmaxx = -1d99
!        compute Jacobian matrices and assign intended sample values to netnodes
            do k = 1, Numk
               if (nb(k) /= 1 .and. nb(k) /= 2 .and. nb(k) /= 3 .and. nb(k) /= 4) cycle
!            if ( nb(k).ne.1 .and. nb(k).ne.2 ) cycle

               if (jsferic == 1 .and. jasfer3D == 1) then
                  J(1) = sum(ops(ktopo(k))%Jxi(1:nmk2(k)) * xloc(iloc(k):iloc(k + 1) - 1))
                  J(2) = sum(ops(ktopo(k))%Jxi(1:nmk2(k)) * yloc(iloc(k):iloc(k + 1) - 1))
                  J(3) = sum(ops(ktopo(k))%Jeta(1:nmk2(k)) * xloc(iloc(k):iloc(k + 1) - 1))
                  J(4) = sum(ops(ktopo(k))%Jeta(1:nmk2(k)) * yloc(iloc(k):iloc(k + 1) - 1))
               else
                  J(1) = sum(ops(ktopo(k))%Jxi(1:nmk2(k)) * xk(kk2(1:nmk2(k), k)))
                  J(2) = sum(ops(ktopo(k))%Jxi(1:nmk2(k)) * yk(kk2(1:nmk2(k), k)))
                  J(3) = sum(ops(ktopo(k))%Jeta(1:nmk2(k)) * xk(kk2(1:nmk2(k), k)))
                  J(4) = sum(ops(ktopo(k))%Jeta(1:nmk2(k)) * yk(kk2(1:nmk2(k), k)))
               end if

!           set z-value at netnodes to intended sample value
               zk(k) = abs(J(1) * J(2) - J(3) * J(4))
               smpminn = min(zk(k), smpminn)
               smpmaxx = max(zk(k), smpmaxx)
            end do

            if (smpmaxx == smpminn) then ! really weird
               smpmaxx = smpminn + 1d0
            end if
            where (zk /= DMISS) zk = (smpmaxx - zk) / (smpmaxx - smpminn)

!        create the samples at the netnodes locations
            call copynetnodestosam(0)

!        not to be repeated hereafter
            Lcopymu = .false.

!        recover the original net values
            zk = zk_bak
         end if

!     interpolate samples to network for grid refinement
         if (Ns > 0 .and. adapt_beta > 0d0) then
            if (Ns > kmax) then
               call qnerror('ORTHOGONALISENET: Ns.gt.kmax', ' ', ' ')
               goto 1234
            end if
            smp_mu = dmiss
            npl_old = npl
            call triinterp2(xk, yk, smp_mu, numk, ja, &
                            XS, YS, ZS, NS, dmiss, jsferic, jins, jasfer3D, NPL, MXSAM, MYSAM, XPL, YPL, ZPL, transformcoef) ! ,0) hk: 0 is not used
            ja = 0 !     hk: triangulation only needed in first cycle
            where (smp_mu == dmiss) smp_mu = 0d0
         else
            smp_mu = 0d0
         end if

!     for post-processing, copy smp_mu to zk
         zk(1:numk) = dble(smp_mu(1:numk))

!-------------------------------------------------
!     compute the weights and right-hand sides
         ww = 0d0
         rhs = 0d0

!     orthogonaliser
         call orthonet_compute_aspect(aspect)
         call orthonet_compweights(nmkx, kk1, aspect, ww, rhs)

!     inverse-map smoother
         if (ATPF < 1d0 .or. smoothorarea < 1d0) then ! we also need administration for volume-based smoother

            if (jsferic == 1 .and. jasfer3D == 1) then
!           compute local coordinates
               call comp_local_coords(iloc, kk1, xk, yk, iloc(numk + 1) - 1, xloc, yloc)
            end if

            call orthonet_compweights_smooth(ops, smp_mu, ww2, ierror)
         else
            ierror = 0
         end if

!     for post-processing, copy ktopo to zk
!      zk(1:numk) = dble(ktopo(1:numk))

         if (ierror == 1) then
            call qnerror('orthonet: orthonet_compweights_smooth gave error', ' ', ' ')
            jarerun = 0
            goto 1234
         end if

!     volume-based smoother
         ibounds = (/nmkx2, numk/)
         call realloc(ww2x, ibounds, fill=0d0)
         call realloc(ww2y, ibounds, fill=0d0)
         if (smoothorarea /= 1d0) then
            call orthonet_compweights_vol(nmkx2, nmk2, kk2, ww2x, ww2y, ierror)
            if (ierror == 1) then
               call qnerror('orthonet: orthonet_compweights_vol gave error', ' ', ' ')
               jarerun = 0
               goto 1234
            end if
         else
            ww2x = 0d0
            ww2y = 0d0
         end if

         call readyy('Orthogonalising net', dble(no - 1 + .8d0) / itatp)
!-------------------------------------------------
! 3. Solve the 'Laplacian' for orthogonalization/Move all points in a few iteration steps.

         relaxin = 0.75d0
         relax1 = 1d0 - relaxin

         do i = 1, itbnd
            do n = 1, itin
!      !     determine atpf    ***INOPERATIVE***
!            atpf_nodes = 1d0
!            do k=1,numk
!               if ( (nb(k).ne.1 .and. nb(k).ne.2) .or. nmk(k).lt.2 ) cycle
!
!      !        compute the residuals
!      !         res_sm(1) = sum(ww2(1:nmk2(k),k)*xk(kk2(1:nmk2(k),k))) / (maxval(xk(kk2(1:nmk2(k),k)))-minval(xk(kk2(1:nmk2(k),k))))
!      !         res_sm(2) = sum(ww2(1:nmk2(k),k)*xk(kk2(1:nmk2(k),k))) / (maxval(yk(kk2(1:nmk2(k),k)))-minval(yk(kk2(1:nmk2(k),k))))
!               res_or(1) = (sum(ww(1:nmk(k),k)*(xk(kk1(1:nmk(k),k))-xk(k))) + rhs(1,k)) / (maxval(xk(kk2(1:nmk2(k),k)))-minval(xk(kk2(1:nmk2(k),k))))
!               res_or(2) = (sum(ww(1:nmk(k),k)*(yk(kk1(1:nmk(k),k))-yk(k))) + rhs(2,k)) / (maxval(xk(kk2(1:nmk2(k),k)))-minval(xk(kk2(1:nmk2(k),k))))
!
!               kk = ktopo(k)
!               res_sm(1) = sum(ops(kk)%ww2*xk(kk2(1:nmk2(k),k))) / (maxval(xk(kk2(1:nmk2(k),k)))-minval(xk(kk2(1:nmk2(k),k))))
!               res_sm(2) = sum(ops(kk)%ww2*yk(kk2(1:nmk2(k),k))) / (maxval(yk(kk2(1:nmk2(k),k)))-minval(yk(kk2(1:nmk2(k),k))))
!
!      !        determine atpf from residuals
!!               atpf_nodes(k) = min(max(get_atpf(res_sm), atpf_min),ATPF)
!
!      !        for post-processing
!      !         zk(k) = sqrt(sum(res_sm**2))
!      !         zk(k) = atpf_nodes(k)
!            end do

               ndki: do k = 1, numk
                  if ((nb(k) /= 1 .and. nb(k) /= 2) .or. nmk(k) < 2) cycle ndki

!              quadtree refinement
                  if (keepcircumcenters /= 0 .and. (nmk(k) /= 3 .or. nb(k) /= 1)) cycle ! hanging nodes only

!               if ( (nb(k).ne.1) .or. nmk(k).lt.2 ) cycle ndki

!              boundary conditions: orthogonal -> set atpf to >0 locally
                  atpf_loc = ATPF
                  if (nb(k) == 2) then
                     atpf_loc = max(ATPF_B, ATPF) ! we need some smoothing
                  end if
                  atpf1_loc = 1d0 - atpf_loc

                  x0 = 0d0; y0 = 0d0
                  Dx0 = 0d0; Dy0 = 0d0
                  x00 = xk1(k); y00 = yk1(k)
                  w0 = 0d0
                  DUM = 0d0

!              determine atpf    ***INOPERATIVE***
!               atpf_loc  = minval((/ atpf_nodes(kk2(1:nmk2(k),k)) /) )
!               atpf_loc  = atpf_nodes(k)
!               if ( (nb(k).eq.1) .and. (nmk(k).ne.4) ) atpf_loc = ATPF   ! only for quads
!
!               atpf1_loc = 1d0 - atpf_loc

!              for post-processing, set zk to atpf_loc
!               zk(k) = atpf_loc

!              determine ratio inverse-map/volume-based smoother
                  if (ww2x(1, k) /= 0d0 .or. ww2y(1, k) /= 0d0) then
                     mumat = mu * ww2(1, k) / max(ww2x(1, k), ww2y(1, k))
                  else
                     mumat = mu
                  end if

!               if ( mumat.eq.0d0 ) mumat = 1d0
                  do kk = 2, max(nmk2(k), nmk(k) + 1) ! do not include center node
!                 combine the weights
                     wwx = 0d0
                     wwy = 0d0
!                 smoother
                     if (ATPF1_loc > 0d0) then
                        if (nb(k) == 1) then ! inner points only
                           wwx = ATPF1_loc * (mumat * ww2x(kk, k) + ww2(kk, k))
                           wwy = ATPF1_loc * (mumat * ww2y(kk, k) + ww2(kk, k))
                        else
                           wwx = ATPF1_loc * ww2(kk, k)
                           wwy = ATPF1_loc * ww2(kk, k)
                        end if
                     end if

!                 orthogonaliser
                     if (kk <= nmk(k) + 1) then
                        wwx = wwx + ATPF_loc * ww(kk - 1, k)
                        wwy = wwy + ATPF_loc * ww(kk - 1, k)
                        k1 = kk1(kk - 1, k)
                     else
                        k1 = kk2(kk, k)
                     end if

!                  if (ww2(kk,k) .ne. 0) then

                     if (JSFERIC == 1) then
                        if (jasfer3D == 1) then
                           DUM(1) = wwx * Ra * dg2rd
                           DUM(2) = wwy * Ra * dg2rd
                        else
                           y1 = yk(k1)
                           DUM(1) = wwx * cos(0.5d0 * (y00 + y1) * dg2rd) * Ra * dg2rd
                           DUM(2) = wwy * Ra * dg2rd
                        end if
                     else
                        DUM(1) = wwx
                        DUM(2) = wwy
                     end if
                     w0 = w0 + DUM

                     if (jsferic == 1 .and. jasfer3D == 1) then
                        Dx0 = Dx0 + DUM(1) * xloc(iloc(k) + kk - 1) ! (xk(k1)-x0)
                        Dy0 = Dy0 + DUM(2) * yloc(iloc(k) + kk - 1) ! (yk(k1)-y0)
                     else
                        Dx0 = Dx0 + DUM(1) * (xk(k1) - xk(k)) ! (xk(k1)-x0)
                        Dy0 = Dy0 + DUM(2) * (yk(k1) - yk(k)) ! (yk(k1)-y0)
                     end if
!                  endif
                  end do

!              combine the rhs
                  righthandside = ATPF_loc * rhs(:, k)
!----------------------

                  if ((abs(w0(1)) > 1e-8) .and. (abs(w0(2)) > 1e-8)) then
                     Dx0 = (Dx0 + righthandside(1)) / w0(1)
                     Dy0 = (Dy0 + righthandside(2)) / w0(2)

                     if (jsferic == 1 .and. jasfer3D == 1) then
                        dumx(1) = relaxin * Dx0
                        dumy(1) = relaxin * Dy0
                        call loc2spher(xk(k), yk(k), 1, dumx, dumy, xk1(k), yk1(k))
                     else
                        x0 = xk(k) + Dx0
                        y0 = yk(k) + Dy0

                        xk1(k) = relaxin * x0 + relax1 * xk(k)
                        yk1(k) = relaxin * y0 + relax1 * yk(k)
                     end if
                  else
                     call cirr(xk1(k), yk1(k), ncolhl)
!                  call qnerror('orthogonalisenet: w0=0', ' ', ' ')
                     cycle ndki
!                  goto 1234
!                  iexit = 1
                  end if
               end do ndki

               xk(1:numk) = xk1(1:numk)
               yk(1:numk) = yk1(1:numk)

!           project boundary nodes back to the boundary
               if (JAPROJECT >= 1) then
!               call adddot(xk1(11),yk1(11),dble(no))
                  call orthonet_project_on_boundary(nmkx, kk1, k_bc, xkb, ykb)
               end if

!           snap to nearest land boundary
               if (JAPROJECT >= 2) call snap_to_landboundary()

!           update local coordinates
               if (jsferic == 1 .and. jasfer3D == 1) then
!              compute local coordinates
                  call comp_local_coords(iloc, kk1, xk, yk, iloc(numk + 1) - 1, xloc, yloc)
               end if

            end do

!  press left or middle mouse button to toggle net plotting on/off;
!  press right mouse button to terminate orthogonizenet
            call halt3(ja3)
            if (Lteknet) then
               ja1 = -1234
               call teknet(ja1) ! whipe out previous net image
!            call teknetcells(NDRAW(33), -1234, 0)
!            xk(1:numk) = xk1(1:numk)
!            yk(1:numk) = yk1(1:numk)

               !        snap to nearest land boundary
               !           if ( JAPROJECT.ge.2 ) call snap_to_landboundary() ! JAPROJECT)

               ja1 = -1234
               call teknet(ja1)
!            call teknetcells(NDRAW(33), -1234, 1)
               !NDRAW(10) = 1
               !CALL PLOT(NDRAW(10))
            else
!            xk(1:numk) = xk1(1:numk)
!            yk(1:numk) = yk1(1:numk)

               !        snap to nearest land boundary
!            if ( JAPROJECT.ge.2 ) call snap_to_landboundary() ! JAPROJECT)
            end if

            if (ja3 == 1 .or. ja3 == 2) then
               call teknet(ja1) ! whipe out net image
               Lteknet = .not. Lteknet
            end if

            if (ja3 == 3) then
               if (keepcircumcenters /= 1) call update_cell_circumcenters()

               call readyy('Orthogonalising net', dble(no) / itatp)

               exit tp
            end if

         end do !itbnd

         mu = min(2d0 * mu, mumax)

!-------------------------------------------------
!     compute the new cell centers
         if (keepcircumcenters /= 1) call update_cell_circumcenters()

!     increase atpf_min for next cycle
         atpf_min = 1d0 - (1d0 - atpf_min) * 0.99d0

         call readyy('Orthogonalising net', dble(no) / itatp)

      end do tp !itatp

1234  continue

      call readyy('Orthogonalising net', -1d0)

!-------------------------------------------------
!   call removesmalllinks()

!-------------------------------------------------
!  deallocate dummy arrays
!   deallocate(nb) ! AvD: TODO: this is for showing node codes (during ortho), but also introduces memleak.
      deallocate (ww, kk1, rhs, aspect, smp_mu, k_bc, xk1, yk1)
      deallocate (ktopo)
!   deallocate(atpf_nodes)

      if (allocated(ops)) call orthonet_dealloc_ops(ops)
      if (allocated(ops)) deallocate (ops)
      if (allocated(nmk2)) deallocate (nmk2)
      if (allocated(kk2)) deallocate (kk2)
      if (allocated(ww2)) deallocate (ww2)
      if (allocated(ww2x)) deallocate (ww2x, ww2y)

      if (allocated(ic)) deallocate (ic)
      if (allocated(jc)) deallocate (jc)

      if (allocated(iloc)) deallocate (iloc)
      if (allocated(xloc)) deallocate (xloc)
      if (allocated(yloc)) deallocate (yloc)

!-------------------------------------------------
!  restore original settings
      JSFERIC = JSFERICold
      circumormasscenter = circormass_bak

      if (allocated(zk_bak)) then
         zk = zk_bak ! this line should be uncommented
         deallocate (zk_bak)
      end if

!  if Lcopy is false, then samples have been created and need to be removed
      if (.not. Lcopymu) then
         call delsam(0)
      end if

!-------------------------------------------------
!  wrong adaptation direction:
!      restore, switch orientation and start over
!-------------------------------------------------
      jarerun = 0
!   if ( Ns.gt.0 ) then
!      jac = 1
!      call confrm('Was this the right adaptation direction? ',jac)
!      if ( jac .ne. 1 ) then
!         call teknet(0,ja1)                        ! whipe out net image
!         idir   = 1 - idir
!         jarerun = 1
!         CALL RESTORE()
!      end if
!   end if

   contains

      subroutine comp_local_coords(iloc, kk1, x, y, Nloc, xloc, yloc)
         use precision, only: dp
         use m_sferic
         use network_data, only: numk, nmk
         use m_inverse_map
         implicit none

         integer, dimension(numk + 1), intent(in) :: iloc !< start pointer in local coordinate arrays
         integer, dimension(nmkx, numk), intent(in) :: kk1 !< link-connected nodes
         real(kind=dp), dimension(numk), intent(in) :: x, y !< node coordinates
         integer, intent(in) :: Nloc !< size of arrays with local coordinates = iloc(numk+1)-1
         real(kind=dp), dimension(Nloc), intent(out) :: xloc, yloc !< local coordinates

         real(kind=dp), dimension(:), allocatable :: xx, yy

         integer :: k, k0, kk, N

         N = max(nmkx + 1, nmkx2)
         allocate (xx(N), yy(N))

         do k0 = 1, numk
!        store coordinates of nodes in stencil
            xx(1) = x(k0)
            yy(1) = y(k0)
            do kk = 1, nmk(k0)
               k = kk1(kk, k0)
               if (k > 0) then
                  xx(kk + 1) = xk(k)
                  yy(kk + 1) = yk(k)
               else ! safety
                  xx(kk + 1) = xk(k0)
                  yy(kk + 1) = yk(k0)
               end if
            end do
            do kk = nmk(k0) + 2, nmk2(k0)
               k = kk2(kk, k0)
               if (k > 0) then
                  xx(kk) = xk(k)
                  yy(kk) = yk(k)
               else ! safety
                  xx(kk) = xk(k0)
                  yy(kk) = yk(k0)
               end if
            end do

            N = max(nmk(k0) + 1, nmk2(k0))

            if (jsferic == 1 .and. jasfer3D == 1) then
               call spher2loc(x(k0), y(k0), N, xx, yy, xloc(iloc(k0):), yloc(iloc(k0):))
            else
               do kk = 1, N
                  xloc(iloc(k0) + k - 1) = xx(kk) - x(k0)
                  yloc(iloc(k0) + k - 1) = yy(kk) - y(k0)
               end do
            end if

         end do

         if (allocated(xx)) deallocate (xx)
         if (allocated(yy)) deallocate (yy)

         return
      end subroutine comp_local_coords

!> compute weights ww and right-hand side rhs in orthogonizer:
!!   sum_kk ww(kk,k0) * (x1(kk1(kk,k0)) - x1(k0)) = rhs(1,k0)
!!   sum_kk ww(kk,k0) * (y1(kk1(kk,k0)) - y1(k0)) = rhs(2,k0)
      subroutine orthonet_compweights(nmkx, kk1, aspect, ww, rhs)
         use precision, only: dp
         use m_netw
         use m_sferic
         use m_flowgeom
         use m_orthosettings
         use m_missing
         use geometry_module, only: dbdistance, normaloutchk, dcosphi

         implicit none

         integer, intent(in) :: nmkx !< maximum number of neighboring link-connected nodes
         integer, dimension(nmkx, numk), intent(in) :: kk1 !< neighboring link-connected nodes

         real(kind=dp), dimension(numL), intent(in) :: aspect !< aspect ratio of the links
         real(kind=dp), dimension(nmkx, numk), intent(inout) :: ww !< weights
         real(kind=dp), dimension(2, numk), intent(out) :: rhs !< right-hand sides

         real(kind=dp) :: r01, slr
         real(kind=dp) :: mu
         real(kind=dp) :: x0, y0, x1, y1, x3, y3, xn, yn
         real(kind=dp) :: atpf1

         integer :: kk, k0, k1l, kl, l, ja
         integer :: nn

         real(kind=dp) :: dummy ! used for debug purposes only
         real(kind=dp) :: SfR ! SLR/R01
         real(kind=dp) :: factor
         real(kind=dp), parameter :: EPS = 1d-4

         dummy = ATPF
         !  ATPF needs to be set to 1d0, since smoothing is performed seperately
         ATPF = 1d0

         ATPF1 = 1d0 - ATPF

         rhs = 0d0
         ww = 0d0

         do k0 = 1, numk ! attraction parameters

            if ((nb(k0) /= 1) .and. (nb(k0) /= 2)) cycle

            x0 = xk(k0)
            y0 = yk(k0)

            numkk: do kk = 1, nmk(k0) ! loop over all links of node k0
               L = nod(k0)%lin(kk) ! link number

               SfR = aspect(L)

               mu = 1d0

               if (SfR /= DMISS) then
                  !-------------------------------------------------------------------------
                  !           internal nodes
                  ww(kk, k0) = atpf * SfR + atpf1 * mu
                  !-------------------------------------------------------------------------

                  if (lnn(L) == 1) then
                     !-------------------------------------------------------------------------
                     !           boundary nodes
                     k1L = kk1(kk, k0)
                     x1 = xk(k1L)
                     y1 = yk(k1L)

                     !              compute the link lengths R01 and SLR
                     R01 = dbdistance(x0, y0, x1, y1, jsferic, jasfer3D, dmiss)
                     SLR = SfR * R01

                     !              find a point inside cell kL and compute outward normal
                     kL = lne(1, L) ! left cell w.r.t. link L
                     nn = netcell(kL)%n
!                  x3 = SUM( xk(netcell(kL)%nod(1:nn)) ) / nn
!                  y3 = SUM( yk(netcell(kL)%nod(1:nn)) ) / nn

                     x3 = xzw(kL)
                     y3 = yzw(kL)

                     call normaloutchk(x0, y0, x1, y1, x3, y3, xn, yn, ja, jsferic, jasfer3D, dmiss, dxymis)
                     if (JSFERIC == 1 .and. jasfer3D == 0) xn = xn * cos(dg2rd * 0.5d0 * (y0 + y1)) ! normal vector needs to be in Cartesian coordinates

                     rhs(1, k0) = rhs(1, k0) + (atpf * R01 * xn / 2 + &
                                                atpf1 * SLR * xn * 0.5d0 / mu)
                     rhs(2, k0) = rhs(2, k0) + (atpf * R01 * yn / 2 + &
                                                atpf1 * SLR * yn * 0.5d0 / mu)

                     ww(kk, k0) = atpf * 0.5d0 * SfR + &
                                  atpf1 * 0.5d0 * mu
                     !-------------------------------------------------------------------------
                  end if
               else
                  ! R01 -> 0
                  ww(kk, k0) = 0d0
               end if

            end do numkk

!        normalise
            factor = sum(ww(:, k0))
            if (abs(factor) > 1e-14) then
               factor = 1d0 / factor
               ww(:, k0) = factor * ww(:, k0)
               rhs(1, k0) = factor * rhs(1, k0)
               rhs(2, k0) = factor * rhs(2, k0)
            end if

         end do ! k0

         ATPF = dummy

      end subroutine orthonet_compweights

!> smoother that strives to optimize the cell area distribution
      subroutine orthonet_compweights_vol(nmkx2, nmk2, kk2, ww2x, ww2y, ierror)
         use precision, only: dp
         use m_netw
         use m_sferic
         use m_orthosettings
         use m_missing
         use m_alloc
         use unstruc_messages
         use unstruc_colors, only: ncolhl
         use m_tek_link

         implicit none

         integer, intent(in) :: nmkx2 !< maximum number of nodes in stencil
         integer, allocatable, dimension(:), intent(in) :: nmk2 !< number of nodes in stencil
         integer, allocatable, dimension(:, :), intent(in) :: kk2 !< node administration; first row, i.e. kk2(1,:), points to center nodes

         real(kind=dp), allocatable, dimension(:, :), intent(inout) :: ww2x, ww2y !< weights

         integer, intent(out) :: ierror !< 0: no error, 1: error

         real(kind=dp), allocatable, dimension(:, :) :: Vx, Vy ! D vol/Dx and D vol/Dy matrices

         integer :: icell, ilink
         integer :: k0, k1
         integer :: icL, icR
         integer :: kk0L, kk0R, kk1L, kk1R
         integer :: kdum
         integer :: N, kk0, kk1, kkk0, kkk1

         real(kind=dp) :: x0, y0, x1, y1, xL, yL, xR, yR
         real(kind=dp) :: DvolL, DvolR, xdum
!     Matlab output only
!      integer, dimension(Numl)                                     :: lne1, lne2, kn1, kn2
!      integer, parameter                                           :: ioutfile

!      logical, save                                                :: Lsavematlab = .false.

         return

         ierror = 1

!     allocate
         allocate (Vx(nmkx2, Nump), Vy(nmkx2, Nump))

!     initialize
         Vx = 0d0
         Vy = 0d0

!     for Matlab output
!      lne1 = Nump+1
!      lne2 = Nump+1

         do ilink = 1, Numl
            if (lnn(ilink) < 1) cycle

            k0 = kn(1, ilink) ! first node of link
            k1 = kn(2, ilink) ! second node of link

            icL = lne(1, ilink) ! left neighboring cell

!        find index of nodes k0 and k1 w.r.t. cell kL in netcell: kk0L and kk1L resp.
            kk0L = 1; do while (netcell(icL)%nod(kk0L) /= k0); kk0L = kk0L + 1; end do
            kk1L = 1; do while (netcell(icL)%nod(kk1L) /= k1); kk1L = kk1L + 1; end do

            N = netcell(icL)%N
            xL = sum(xk(netcell(icL)%nod(1:N))) / dble(max(N, 1))
            yL = sum(yk(netcell(icL)%nod(1:N))) / dble(max(N, 1))

            x0 = xk(k0); y0 = yk(k0); 
            x1 = xk(k1); y1 = yk(k1); 
!        contribution to the volume of the left cell
            DvolL = 0.5d0 * ((x0 - xL) * (y1 - yL) - (x1 - xL) * (y0 - yL))

!        Get the (0-1)/(L-R) frame in the right orientation by swapping nodes 0 and 1 if necessary
!        the contribution to volume of cell L needs to be positive
            if (DvolL < 0d0) then ! swap nodes 0 and 1
               kdum = k0; k0 = k1; k1 = kdum
               kdum = kk0L; kk0L = kk1L; kk1L = kdum

               xdum = x0; x0 = x1; x1 = xdum
               xdum = y0; y0 = y1; y1 = xdum

               DvolL = -DvolL
            end if

            Vx(kk0L, icL) = Vx(kk0L, icL) + 0.5d0 * y1
            Vx(kk1L, icL) = Vx(kk1L, icL) - 0.5d0 * y0

            Vy(kk0L, icL) = Vy(kk0L, icL) - 0.5d0 * x1
            Vy(kk1L, icL) = Vy(kk1L, icL) + 0.5d0 * x0

!        for Matlab output
!         kn1(ilink)  = k0
!         kn2(ilink)  = k1
!         lne1(ilink) = lne(1,ilink)

!        same for the left cell, if it exists
            if (lnn(ilink) > 1) then
               icR = lne(2, ilink)

               N = netcell(icR)%N
               xR = sum(xk(netcell(icR)%nod(1:N))) / dble(max(N, 1))
               yR = sum(yk(netcell(icR)%nod(1:N))) / dble(max(N, 1))

!           contribution to the volume of the left cell
               DvolR = 0.5d0 * ((x1 - xR) * (y0 - yR) - (x0 - xR) * (y1 - yR))

!           DvolR should be larger then zero
               if (DvolR < 0d0) then

                  call qnerror('orthonet_compweights_vol: DvolR<0', ' ', ' ')
                  call teklink(ilink, ncolhl)
                  if (kn(3, ilink) == 2) then
                     call cirr(xk(kn(1, ilink)), yk(kn(1, ilink)), ncolhl)
                     call cirr(xk(kn(2, ilink)), yk(kn(2, ilink)), ncolhl)
                  end if
                  goto 1234
               end if

!           find index of nodes k0 and k1 w.r.t. cell kL in netcell: kk0L and kk1L resp.
               kk0R = 1; do while (netcell(icR)%nod(kk0R) /= k0 .and. kk0R < N); kk0R = kk0R + 1; end do
               kk1R = 1; do while (netcell(icR)%nod(kk1R) /= k1 .and. kk1R < N); kk1R = kk1R + 1; end do

               if (netcell(icR)%nod(kk0R) /= k0 .or. netcell(icR)%nod(kk1R) /= k1) then
                  call qnerror('orthonet_compweights_vol: node not found', ' ', ' ')
                  goto 1234
               end if

               Vx(kk0R, icR) = Vx(kk0R, icR) - 0.5d0 * y1
               Vx(kk1R, icR) = Vx(kk1R, icR) + 0.5d0 * y0

               Vy(kk0R, icR) = Vy(kk0R, icR) + 0.5d0 * x1
               Vy(kk1R, icR) = Vy(kk1R, icR) - 0.5d0 * x0

!           for Matlab output
!            lne2(ilink) = lne(2,ilink)
            end if
         end do

!     compute the weights:
!       [ ww2x    0] = [-Vx'*Vx       0]
!       [    0 ww2y] = [      0 -Vy' Vy]

         ww2x = 0d0
         ww2y = 0d0

         do icell = 1, Nump
            N = netcell(icell)%N

            do kk0 = 1, N
               k0 = netcell(icell)%nod(kk0)

               if (nmk2(k0) == 0) cycle

!            if ( nb(k0).ne.1 ) cycle   ! internal nodes only

               do kk1 = 1, N
                  k1 = netcell(icell)%nod(kk1)
!               if ( nb(k1).ne.1 ) cycle   ! internal nodes only

                  kkk1 = 1
                  do while (kk2(kkk1, k0) /= k1 .and. kkk1 < nmk2(k0))
                     kkk1 = kkk1 + 1
                  end do
                  if (kk2(kkk1, k0) /= k1) then
                     call qnerror('orthonet_compweights_vol: node not found', ' ', ' ')
                     goto 1234
                  end if
                  ww2x(kkk1, k0) = ww2x(kkk1, k0) + Vx(kk0, icell) * Vx(kk1, icell)
                  ww2y(kkk1, k0) = ww2y(kkk1, k0) + Vy(kk0, icell) * Vy(kk1, icell)

                  kkk0 = 1
                  do while (kk2(kkk0, k0) /= k0 .and. kkk0 < nmk2(k0))
                     kkk0 = kkk0 + 1
                  end do
                  if (kk2(kkk0, k0) /= k0) then
                     call qnerror('orthonet_compweights_vol: node not found', ' ', ' ')
                     goto 1234
                  end if
                  ww2x(kkk0, k1) = ww2x(kkk0, k1) + Vx(kk0, icell) * Vx(kk1, icell)
                  ww2y(kkk0, k1) = ww2y(kkk0, k1) + Vy(kk0, icell) * Vy(kk1, icell)

                  if (kkk1 > nmk2(k0) .or. kkk0 > nmk2(k0)) then
                     call qnerror('orthonet_compweights_vol: node not found', ' ', ' ')
                     goto 1234
                  end if
               end do
            end do
         end do

         do k0 = 1, Numk
            if (nb(k0) == 1 .or. nb(k0) == 4) cycle ! non-internal cells only
            ww2x(1, k0) = 1d0
            ww2y(1, k0) = 1d0
            ww2x(2:nmk2(k0), k0) = 0d0
            ww2y(2:nmk2(k0), k0) = 0d0
         end do

         ierror = 0

!      if ( Lsavematlab) then
!         open(newunit=ioutfile, file='c:\cygwin\home\pijl\develop\test\testww2x.m')
!
!         call matlab_write_int(ioutfile, 'nmkx2', (/ nmkx2 /), 1,    1)
!         call matlab_write_int(ioutfile, 'nmk2',  (/ nmk2  /), Numk, 1)
!         call matlab_write_int(ioutfile, 'kk2',   kk2,     nmkx2, Numk)
!         call matlab_write_double(ioutfile, 'ww2x', ww2x, nmkx2, Numk)
!         call matlab_write_double(ioutfile, 'ww2y', ww2y, nmkx2, Numk)
!
!         call matlab_write_double(ioutfile, 'xk', xk, Numk, 1)
!         call matlab_write_double(ioutfile, 'yk', yk, Numk, 1)
!         call matlab_write_int(ioutfile, 'lne1', lne1, Numl, 1)
!         call matlab_write_int(ioutfile, 'lne2', lne2, Numl, 1)
!         call matlab_write_int(ioutfile, 'kn1', kn1, Numl, 1)
!         call matlab_write_int(ioutfile, 'kn2', kn2, Numl, 1)
!
!         close(ioutfile)
!
!         call qnerror('Matlab file saved', ' ', ' ')
!
!         Lsavematlab = .false.
!
!!         ierror = 1
!      end if
1234     continue

!     deallocate
         deallocate (Vx, Vy)

      end subroutine

!> inverse-mapping elliptic smoother
!! computes weight ww in:
!!   sum_kk ww(kk,k0) * x1(kk2(kk,k0)) = 0
!!   sum_kk ww(kk,k0) * y1(kk2(kk,k0)) = 0
      subroutine orthonet_compweights_smooth(ops, u, ww2, ierror)
         use m_matlab_write_int, only: matlab_write_int
         use m_matlab_write_double, only: matlab_write_double
         use precision, only: dp
         use m_netw
         use m_sferic
         use m_flowgeom
         use m_orthosettings
         use m_missing
         use m_alloc
         use m_inverse_map
         use unstruc_colors

         implicit none
         type(tops), allocatable, dimension(:), intent(inout) :: ops !< operators for each unique topology
         real(kind=dp), allocatable, dimension(:), intent(in) :: u !< 'physcial solution' whose gradient will attract the mesh
         real(kind=dp), allocatable, dimension(:, :), intent(inout) :: ww2 !< weights

         integer, intent(out) :: ierror !< node number that gave error

         !---------------------------
         !  mesh adaptation arrays
         !---------------------------
         real(kind=dp), allocatable, dimension(:, :) :: Ginv ! mesh monitor matrices
         real(kind=dp), dimension(4) :: Adum
         !---------------------------

         real(kind=dp), allocatable, dimension(:, :) :: J ! Jacobian matrices at the nodes

         integer :: k, kk, k0
         real(kind=dp), dimension(2) :: a1, a2 ! contravariant base vectors
         real(kind=dp) :: det

         integer, allocatable, dimension(:) :: Mcell ! for matlab output
         real(kind=dp), allocatable, dimension(:, :) :: x, y ! for matlab output

         integer :: imat ! matlab file unit number

         logical, save :: Lsavematlabfile = .false.

         real(kind=dp) :: UU(2, 2), VV(2, 2), S(2), uu1, vv1, uu2, vv2
         real(kind=dp) :: aspect ! Jacobian at link positions
         integer :: ierror_, kcheck
         type(tops) :: op ! structure with operators
         type(tadm) :: adm ! structure with administration
         real(kind=dp), allocatable, dimension(:) :: xi, eta ! node coordinates (xi,eta)

         real(kind=dp) :: alpha ! used for monotonicity correction
         real(kind=dp) :: dcosfac
         real(kind=dp), dimension(4) :: DGinvDxi, DGinvDeta
         real(kind=dp), parameter :: EPS = 1e-8

         ierror_ = 1

         kcheck = 5

         ierror = ierror_

         !  compute operators
         if (.not. allocated(ops)) then
            call orthonet_comp_ops(ops, ierror_)
            if (ierror_ /= 0) goto 1234
         end if

         allocate (xi(nmkx2), eta(nmkx2))
         allocate (J(4, Numk), Ginv(4, Numk))

         J = 0d0
!     compute Jacobian matrices
         do k0 = 1, Numk
            dcosfac = 1d0
            if (jsferic == 1) then
               dcosfac = cos(yk(k0) * dg2rd)
            end if

!         if ( nb(k0).ne.1 .and. nb(k0).ne.2 .and. nb(k0).ne.3 ) cycle
            if (nb(k0) /= 1 .and. nb(k0) /= 2 .and. nb(k0) /= 4) cycle
            op = ops(ktopo(k0))
            !        J(1,k0) = sum( op%Jxi( 1:nmk2(k0)) * xk(kk2(1:nmk2(k0),k0)) )
            !        J(2,k0) = sum( op%Jxi( 1:nmk2(k0)) * yk(kk2(1:nmk2(k0),k0)) )
            !        J(3,k0) = sum( op%Jeta(1:nmk2(k0)) * xk(kk2(1:nmk2(k0),k0)) )
            !        J(4,k0) = sum( op%Jeta(1:nmk2(k0)) * yk(kk2(1:nmk2(k0),k0)) )

            if (jsferic == 1 .and. jasfer3D == 1) then
               J(1, k0) = sum(op%Jxi(1:nmk2(k0)) * xloc(iloc(k0):iloc(k0 + 1) - 1))
               J(2, k0) = sum(op%Jxi(1:nmk2(k0)) * yloc(iloc(k0):iloc(k0 + 1) - 1))
               J(3, k0) = sum(op%Jeta(1:nmk2(k0)) * xloc(iloc(k0):iloc(k0 + 1) - 1))
               J(4, k0) = sum(op%Jeta(1:nmk2(k0)) * yloc(iloc(k0):iloc(k0 + 1) - 1))
            else
               J(1, k0) = sum(op%Jxi(1:nmk2(k0)) * xk(kk2(1:nmk2(k0), k0))) * dcosfac
               J(2, k0) = sum(op%Jxi(1:nmk2(k0)) * yk(kk2(1:nmk2(k0), k0)))
               J(3, k0) = sum(op%Jeta(1:nmk2(k0)) * xk(kk2(1:nmk2(k0), k0))) * dcosfac
               J(4, k0) = sum(op%Jeta(1:nmk2(k0)) * yk(kk2(1:nmk2(k0), k0)))
            end if
         end do

!     compose the inverse monitor matrices for mesh adaptation
         if (Ns > 0) then
            call orthonet_comp_Ginv(u, ops, J, Ginv)
         else
            Ginv(1, :) = 1d0
            Ginv(2:3, :) = 0d0
            Ginv(4, :) = 1d0
         end if

!     reallocate memory for weights ww2 if necessary
         if (ubound(ww2, 1) < nmkx2) call realloc(ww2, (/nmkx2, numk/))

!     compose the discretization
         do k0 = 1, numk ! attraction parameters

            if (k0 == kcheck) then
               continue
            end if

            if (nmk(k0) < 2) cycle

            !-------------------------------------------------------------------------
            !     internal nodes and boundary nodes
            !-------------------------------------------------------------------------
!         if ( (nb(k0) .eq. 1) ) then
            if ((nb(k0) == 1) .or. (nb(k0) == 2)) then

               op = ops(ktopo(k0))

               if (nmk(k0) > size(op%Divxi)) then
                  continue
               end if

               !        compute the Jacobian
               !         J(1,k0) = sum( op%Jxi( 1:nmk2(k0)) * xk(kk2(1:nmk2(k0),k0)) )
               !         J(2,k0) = sum( op%Jxi( 1:nmk2(k0)) * yk(kk2(1:nmk2(k0),k0)) )
               !         J(3,k0) = sum( op%Jeta(1:nmk2(k0)) * xk(kk2(1:nmk2(k0),k0)) )
               !         J(4,k0) = sum( op%Jeta(1:nmk2(k0)) * yk(kk2(1:nmk2(k0),k0)) )
               if (jsferic == 1 .and. jasfer3D == 1) then
                  J(1, k0) = sum(op%Jxi(1:nmk2(k0)) * xloc(iloc(k0):iloc(k0 + 1) - 1))
                  J(2, k0) = sum(op%Jxi(1:nmk2(k0)) * yloc(iloc(k0):iloc(k0 + 1) - 1))
                  J(3, k0) = sum(op%Jeta(1:nmk2(k0)) * xloc(iloc(k0):iloc(k0 + 1) - 1))
                  J(4, k0) = sum(op%Jeta(1:nmk2(k0)) * yloc(iloc(k0):iloc(k0 + 1) - 1))
               else
                  J(1, k0) = sum(op%Jxi(1:nmk2(k0)) * xk(kk2(1:nmk2(k0), k0))) * dcosfac
                  J(2, k0) = sum(op%Jxi(1:nmk2(k0)) * yk(kk2(1:nmk2(k0), k0)))
                  J(3, k0) = sum(op%Jeta(1:nmk2(k0)) * xk(kk2(1:nmk2(k0), k0))) * dcosfac
                  J(4, k0) = sum(op%Jeta(1:nmk2(k0)) * yk(kk2(1:nmk2(k0), k0)))
               end if

               !        compute the contravariant base vectors
               det = J(1, k0) * J(4, k0) - J(3, k0) * J(2, k0)

               if (det == 0d0) then
!               call qnerror('orthonet_compweights_smooth: det=0', ' ', ' ')
                  call cirr(xk(k0), yk(k0), ncolhl)
                  cycle
!               return
               end if

               a1 = (/J(4, k0), -J(3, k0)/) / det
               a2 = (/-J(2, k0), J(1, k0)/) / det

!--------------------------------------------------------------
!           compute the Singular Value Decomposition of the Jacobian matrix
!--------------------------------------------------------------
               if (Lsavematlabfile) then
                  UU(1, :) = (/J(1, k0), J(3, k0)/)
                  UU(2, :) = (/J(2, k0), J(4, k0)/)
                  call svdcmp(UU, 2, 2, 2, 2, S, VV)

                  aspect = min(S(1) / (S(2) + EPS), S(2) / (S(1) + EPS))
                  uu1 = UU(1, 1) * S(1)
                  vv1 = UU(2, 1) * S(1)
                  uu2 = UU(1, 2) * S(2)
                  vv2 = UU(2, 2) * S(2)
               end if

!--------------------------------------------------------------
!        compose the discretization
!--------------------------------------------------------------

               ww2(:, k0) = 0d0

               DGinvDxi = matmul(Ginv(:, kk2(1:nmk2(k0), k0)), op%Jxi)
               DGinvDeta = matmul(Ginv(:, kk2(1:nmk2(k0), k0)), op%Jeta)

               Adum = Ginv(:, k0)

               ww2(1:nmk2(k0), k0) = - &
                                     ( &
                                     Anorm(a1, a1, DGinvDxi) * op%Jxi + &
                                     Anorm(a1, a2, DGinvDeta) * op%Jxi + &
                                     Anorm(a2, a1, DGinvDxi) * op%Jeta + &
                                     Anorm(a2, a2, DGinvDeta) * op%Jeta &
                                     ) + &
                                     ( &
                                     Anorm(a1, a1, Adum) * matmul(op%Gxi, op%Divxi) + &
                                     Anorm(a1, a2, Adum) * matmul(op%Gxi, op%Diveta) + &
                                     Anorm(a2, a1, Adum) * matmul(op%Geta, op%Divxi) + &
                                     Anorm(a2, a2, Adum) * matmul(op%Geta, op%Diveta) &
                                     )

               !        monotonicity: all off-diagonal elements should be >= 0
               alpha = 0d0
               do k = 2, nmk2(k0)
!               alpha = max(alpha, -ww2(k,k0))
                  alpha = max(alpha, -ww2(k, k0) / max(1d0, op%ww2(k)))
               end do

!           firstly, correct with the node-average with some threshold
!              the threshold is intended to prevent mesh folding
!            if ( alpha.gt.-ww2(1,k0)/dble(nmk2(k0)) ) then
!!               call cirr(xk(k0), yk(k0), ncolhl)
!               ww2(:,k0) = ww2(:,k0) + alpha
!            end if

!           03-08-11: threshold set to zero
!            ww2(2:nmk2(k0),k0) = ww2(2:nmk2(k0),k0)+alpha
!           04-08-11:
               ww2(2:nmk2(k0), k0) = ww2(2:nmk2(k0), k0) + alpha * max(op%ww2(2:nmk2(k0)), 1d0)

!           then, set the remaining negative off-diagonal weights to zero
!            do k=2,nmk2(k0)
!               if ( ww2(k, k0).lt.0d0 ) ww2(k, k0) = 0d0
!            end do

               ww2(1, k0) = -sum(ww2(2:nmk2(k0), k0))

               !        normalise
               ww2(:, k0) = -ww2(:, k0) / (ww2(1, k0) + 1d-8)

!         else if ( nb(k0).eq.2 ) then  ! will never be reached
!-------------------------------------------------------------------------
!     boundary nodes
!-------------------------------------------------------------------------
!            op = ops(ktopo(k0))
!
!            ww2(:, k0) = 0d0
!            do k=1,nmk(k0)
!               do knode=1,nmk2(k0)
!                  ww2(knode, k0) = ww2(knode, k0) +   &
!                     op%Divxi( k)*op%Gxi( knode, k) +  &
!                     op%Diveta(k)*op%Geta(knode, k)
!               end do
!            end do
!
!   !        normalise
!            ww2(:, k0) = -ww2(:,k0)/ww2(1,k0)
!
            end if

            !-------------------------------------------------------------------------
            !     boundary nodes
            !     see W. Huang, 'Practical Aspects of Formulation and Solution of
            !     Moving Mesh Partial Differential Equations',
            !     J. of Comp. Phys., 2001, sect. 3.3
            !-------------------------------------------------------------------------
!         else if ( nb(k0).eq.2 ) then
!            num       = 1
!            kk2(1,k0) = k0
!            ww2(:,k0) = 0d0
!            do k=1,nmk(k0)
!               L = nod(k0)%lin(k)
!               kk2(k+1,k0) = kn(1,L) + kn(2,L) - k0
!               if ( lnn(L).eq.1 ) then
!                  num         = num+1
!                  ww2(K+1,k0) = 1d0
!               end if
!            end do
!            if ( nmk2(k0).eq.0 ) nmk2(k0)  = nmk(k0)+1
!            ww2(1,k0) = -sum(ww2(2:nmk2(k0),k0))
!
!   !        normalise
!            ww2(:, k0) = -ww2(:,k0)/ww2(1,k0)
!         end if

!        debug: Matlab output
            if (k0 == kcheck .and. Lsavematlabfile) then
               call orthonet_admin(k0, adm, ierror)
               call orthonet_assign_xieta(k0, adm, xi, eta, ierror_)

               allocate (Mcell(nmk(k0)), x(M, nmk2(k0)), y(M, nmk2(k0)))

               do kk = 1, nmk(k0)
                  if (adm%icell(kk) >= 1) then
                     Mcell(kk) = netcell(adm%icell(kk))%n
                  else
                     Mcell(kk) = -1234
                  end if
               end do

               if (Lsavematlabfile) then
                  open (newunit=imat, file='test.m')

                  call matlab_write_double(imat, 'xi', xi(1:nmk2(k0)), nmk2(k0), 1)
                  call matlab_write_double(imat, 'eta', eta(1:nmk2(k0)), nmk2(k0), 1)
                  call matlab_write_double(imat, 'Gxi', op%Gxi(1:nmk2(k0), 1:nmk(k0)), nmk2(k0), nmk(k0))
                  call matlab_write_double(imat, 'Geta', op%Geta(1:nmk2(k0), 1:nmk(k0)), nmk2(k0), nmk(k0))
                  call matlab_write_double(imat, 'Divxi', op%Divxi(1:nmk(k0)), nmk(k0), 1)
                  call matlab_write_double(imat, 'Diveta', op%Diveta(1:nmk(k0)), nmk(k0), 1)
                  call matlab_write_double(imat, 'a1', a1, 2, 1)
                  call matlab_write_double(imat, 'a2', a2, 2, 1)
                  call matlab_write_int(imat, 'kkc', adm%kkc(1:M, 1:nmk(k0)), M, nmk(k0))
                  call matlab_write_int(imat, 'Mcell', Mcell(1:nmk(k0)), nmk(k0), 1)
                  call matlab_write_double(imat, 'x', xk(adm%kk2(1:nmk2(k0))), nmk2(k0), 1)
                  call matlab_write_double(imat, 'y', yk(adm%kk2(1:nmk2(k0))), nmk2(k0), 1)
                  call matlab_write_double(imat, 'ww2', ww2(1:nmk2(k0), k0), nmk2(k0), 1)
                  call matlab_write_double(imat, 'Az', op%Az(1:nmk2(k0), 1:nmk(k0)), nmk2(k0), nmk(k0))
                  call matlab_write_double(imat, 'Jxi', op%Jxi(1:nmk2(k0)), nmk2(k0), 1)
                  call matlab_write_double(imat, 'Jeta', op%Jeta(1:nmk2(k0)), nmk2(k0), 1)

                  call matlab_write_double(imat, 'u1', (/uu1, vv1/), 2, 1)
                  call matlab_write_double(imat, 'u2', (/uu2, vv2/), 2, 1)
                  call matlab_write_double(imat, 's', S, 2, 1)

                  close (imat)
                  call qnerror('Matlab file saved', ' ', ' ')
               end if

               Lsavematlabfile = .false.

               deallocate (Mcell, x, y)
            end if

         end do ! k0

         ierror_ = 0

1234     continue

         !  deallocate arrays
         if (allocated(adm%icell)) then
            deallocate (adm%icell, adm%kk2, adm%kkc)
         else
            continue
         end if
         if (allocated(xi)) deallocate (xi, eta)
         if (allocated(op%Az)) then
            deallocate (op%Az, op%Gxi, op%Geta, op%Divxi, op%Diveta, op%Jxi, op%Jeta)
         else
            continue
         end if
         if (allocated(Ginv)) deallocate (Ginv)
         if (allocated(J)) deallocate (J)

         ierror = ierror_

      end subroutine orthonet_compweights_smooth

!> compute the inverse monitor function for mesh adaptation
!!     see W. Huang, 'Practical Aspects of Formulation and Solution of
!!     Moving Mesh Partial Differential Equations',
!!     J. of Comp. Phys., 2001, sects. 3.4 and 3.5
      subroutine orthonet_comp_Ginv(u, ops, J, Ginv)
         use m_matlab_write_double, only: matlab_write_double
         use precision, only: dp
         use m_netw
         use m_orthosettings

         implicit none

         real(kind=dp), dimension(:) :: u !< 'physcial solution' whose gradient will attract the mesh
         type(tops), dimension(:) :: ops !< array of structure with operators of unique topologies
         real(kind=dp), dimension(:, :) :: J !< Jacobian matrices at the nodes

         real(kind=dp), dimension(:, :) :: Ginv !< inverse mesh monitor matrix at net-nodes

         real(kind=dp), allocatable, dimension(:) :: u_smooth ! smoothed solution
         real(kind=dp), allocatable, dimension(:, :) :: vdir ! refinement direction vector
         real(kind=dp), allocatable, dimension(:) :: Phi !
         real(kind=dp), dimension(2) :: a1, a2 ! contravariant base vectors

         real(kind=dp), allocatable, dimension(:, :) :: G ! mesh monitor matrix at net-nodes
         real(kind=dp), allocatable, dimension(:, :) :: G_tmp ! temporary mesh monitor matrix at net-nodes

         real(kind=dp) :: det, dudxi, dudeta
         real(kind=dp) :: alpha, lambda1, lambda2, lfac
         real(kind=dp) :: Phi_ave, vol

         integer :: k0, imat

         logical, save :: Lsavematlab = .false.

!     allocate
         allocate (u_smooth(Numk), vdir(2, Numk), Phi(Numk), G(4, Numk), G_tmp(4, Numk))

         G_tmp = 0d0
         Phi = 0d0
         vdir = 0d0
         Phi_ave = 0d0
         vol = 0d0

         call orthonet_smooth_u(u, adapt_niter_u, u_smooth) ! <1: no smoothing

!     compute Phi
         do k0 = 1, Numk
            if (nb(k0) /= 1 .and. nb(k0) /= 2 .and. nb(k0) /= 4) cycle ! internal and boundary nodes only

!        compute the contravariant base vectors
            det = J(1, k0) * J(4, k0) - J(3, k0) * J(2, k0) + 1d-9
            a1 = (/J(4, k0), -J(3, k0)/) / det
            a2 = (/-J(2, k0), J(1, k0)/) / det

            dudxi = sum(ops(ktopo(k0))%Jxi(1:nmk2(k0)) * u_smooth(kk2(1:nmk2(k0), k0)))
            dudeta = sum(ops(ktopo(k0))%Jeta(1:nmk2(k0)) * u_smooth(kk2(1:nmk2(k0), k0)))

!        compute the physical gradient of mu
            vdir(:, k0) = a1 * dudxi + a2 * dudeta

            Phi(k0) = sqrt(sum(vdir(:, k0)**2)) ! temporarily, will be redefined hereafter

            if (Phi(k0) > 1d-14) then
               vdir(:, k0) = vdir(:, k0) / Phi(k0)
            else
               vdir(:, k0) = (/1d0, 0d0/)
            end if

!         Phi(k0) = sqrt( 1d0 + Phi(k0)**2 ) - 1d0        ! refinement based on gradients of u
            Phi(k0) = sqrt(1d0 + u_smooth(k0)**2) - 1d0 ! refinement based on smoothed u
!         Phi(k0) = sqrt( 1d0 + u(k0)**2 ) - 1d0   ! refinement based on u itself

            Phi_ave = Phi_ave + Phi(k0) * abs(det)
            vol = vol + abs(det)
         end do

         Phi_ave = Phi_ave / vol

         alpha = 1d0
         adapt_beta = min(adapt_beta, 0.99d0)
         if (Phi_ave /= 0d0) alpha = adapt_beta / (Phi_ave * (1d0 - adapt_beta))

         select case (adapt_method)
         case (1) ! arc-length
            do k0 = 1, Numk
               lambda1 = 1d0 + alpha * Phi(k0)

               lambda2 = 1d0

               lfac = lambda1 / lambda2 - 1d0
               G_tmp(1, k0) = 1d0 + lfac * vdir(1, k0) * vdir(1, k0)
               G_tmp(2, k0) = lfac * vdir(2, k0) * vdir(1, k0)
               G_tmp(3, k0) = lfac * vdir(1, k0) * vdir(2, k0)
               G_tmp(4, k0) = 1d0 + lfac * vdir(2, k0) * vdir(2, k0)
               G_tmp(:, k0) = G_tmp(:, k0) * lambda2
            end do

         case (2) ! Harmonic map
            do k0 = 1, Numk
               lambda1 = 1d0 + alpha * Phi(k0)

               lambda2 = 1d0 / lambda1

               lfac = lambda1 / lambda2 - 1d0
               G_tmp(1, k0) = 1d0 + lfac * vdir(1, k0) * vdir(1, k0)
               G_tmp(2, k0) = lfac * vdir(2, k0) * vdir(1, k0)
               G_tmp(3, k0) = lfac * vdir(1, k0) * vdir(2, k0)
               G_tmp(4, k0) = 1d0 + lfac * vdir(2, k0) * vdir(2, k0)
               G_tmp(:, k0) = G_tmp(:, k0) * lambda2
            end do

         case default ! Winslow
            do k0 = 1, Numk
               G_tmp(:, k0) = (/1d0, 0d0, 0d0, 1d0/) * (1d0 + alpha * Phi(k0))
            end do
         end select

!     smooth G
         do k = 1, 4
            call orthonet_smooth_u(G_tmp(k, :), adapt_niter_G, G(k, :))
         end do

         do k0 = 1, Numk
            if (nb(k0) == 1 .or. nb(k0) == 2 .or. nb(k0) == 4) then
               Ginv(1:4, k0) = (/G(4, k0), -G(2, k0), -G(3, k0), G(1, k0)/)
               Ginv(1:4, k0) = Ginv(:, k0) / (G(1, k0) * G(4, k0) - G(3, k0) * G(2, k0))
            else
               Ginv(1:4, k0) = (/1d0, 0d0, 0d0, 1d0/)
            end if
         end do

         if (Lsavematlab) then
            open (newunit=imat, file='c:\cygwin\home\pijl\develop\refinement.m')

            call matlab_write_double(imat, 'xk', xk, Numk, 1)
            call matlab_write_double(imat, 'yk', yk, Numk, 1)
            call matlab_write_double(imat, 'G_tmp', G_tmp, 4, Numk)
            call matlab_write_double(imat, 'G', G, 4, Numk)
            call matlab_write_double(imat, 'Ginv', Ginv, 4, Numk)
            call matlab_write_double(imat, 'vdir', vdir, 2, Numk)
            call matlab_write_double(imat, 'Phi', Phi, Numk, 1)
            call matlab_write_double(imat, 'Phi_ave', (/Phi_ave/), 1, 1)
            call matlab_write_double(imat, 'u', u, Numk, 1)
            call matlab_write_double(imat, 'u_smooth', u_smooth, Numk, 1)

            close (imat)

            call qnerror('Matlab file saved', ' ', ' ')

            Lsavematlab = .false.
         end if

!     deallocate
         deallocate (u_smooth, vdir, Phi, G, G_tmp)

      end subroutine

!> compute the operators for each unique topology in the
!!    inverse-map elliptic smoother
      subroutine orthonet_comp_ops(ops, ierror)
         use precision, only: dp
         use m_netw
         use m_alloc
         use m_inverse_map
         use unstruc_colors

         implicit none

         type(tops), allocatable, dimension(:), intent(out) :: ops !< per-topology operators
         integer, intent(out) :: ierror !< 0: no error, 1: error

         integer :: k0
         integer :: itopo

         real(kind=dp), allocatable, dimension(:) :: xi, eta ! node coordinates (xi,eta)

         logical :: lisnew ! new topology (.true.) or not (.false.)
         logical, allocatable, dimension(:) :: lnewtopo

         type(tadm) :: adm ! structure with administration
         type(ttop) :: top ! structure with topology arrays

         ierror = 1

         adm%Ncell = 2
         allocate (adm%icell(adm%Ncell))
         adm%icell = 0
         allocate (adm%kk2(nmkx2))
         adm%kk2 = 0
         allocate (adm%kkc(M, adm%Ncell))
         adm%kkc = 0
         allocate (xi(nmkx2), eta(nmkx2))
         xi = 0d0
         eta = 0d0

!     allocate saved arrays
         allocate (top%nmk(1), top%nmk2(1))
         allocate (top%xi(nmkx, 1), top%eta(nmkx, 1))

         if (.not. allocated(kk2)) allocate (kk2(nmkx, numk))

         !  firstly, perform the administration
         do k0 = 1, numk
            if (nmk(k0) < 2) cycle

!        perform the node-to-node-through-cells connectivity administration
            call orthonet_admin(k0, adm, ierror)
            if (ierror == 1) then
               call qnerror('orthonet_compweights_smooth: orthonet_admin gave error', ' ', ' ')
               goto 1234
            end if

!        resize kk2 array if necessary
            if (adm%nmk2 > nmkx2) then
               nmkx2 = adm%nmk2
               call realloc(kk2, (/nmkx2, numk/), fill=0)
            end if

!        fill global administration arrays with local
            nmk2(k0) = adm%nmk2
            kk2(1:nmkx2, k0) = adm%kk2(1:nmkx2)

!        resize xi and eta arrays if necessary
            if (adm%nmk2 > ubound(xi, 1)) then
               call realloc(xi, adm%nmk2, fill=0d0)
               call realloc(eta, adm%nmk2, fill=0d0)
            end if

!     assign (xi, eta) and find and save the unique topologies

!        assign the node indices xi and eta
            ierror = 1234
            call orthonet_assign_xieta(k0, adm, xi, eta, ierror)
            if (ierror == 1) then
               call qnerror('orthonet_comp_ops: orthonet_assignxieta gave error', ' ', ' ')
               goto 1234
            end if

!        save unique topology, based on xi and eta
            call orthonet_save_topo(k0, adm, xi, eta, top, lisnew)
         end do

         !  deallocate memory
         deallocate (top%xi, top%eta, top%nmk, top%nmk2)

         !  allocate memory
         allocate (ops(numtopo))
         allocate (lnewtopo(numtopo))
         lnewtopo = .true.

         !  compute and save operators for new, unique topologies
         do k0 = 1, numk
            if (nb(k0) /= 1 .and. nb(k0) /= 2 .and. nb(k0) /= 3 .and. nb(k0) /= 4) cycle ! we need the adminstration for corner nodes, hence corner nodes are not excluded
            itopo = ktopo(k0)

            if (itopo < 1) cycle ! really needs to be checked

            !     determine if this node has a new unique topology
            if (lnewtopo(itopo)) then
               lnewtopo(itopo) = .false.

               !        allocate memory
               call orthonet_alloc_op(ops(itopo), nmk(k0), nmk2(k0))

               !        perform administration
               call orthonet_admin(k0, adm, ierror)

               !        assign node coordinates (xi, eta)
               call orthonet_assign_xieta(k0, adm, xi, eta, ierror)

               !        compute operators
!            if ( nb(k0).ne.3 ) then ! corner-node operators are excluded
               call orthonet_comp_operators(k0, adm, xi, eta, ops(itopo), ierror)
               if (ierror == 1) then
                  call qnerror('orthonet_compweights_smooth: orthonet_comp_operators gave error', ' ', ' ')
                  goto 1234
               end if
!            end if
            end if

         end do

         ierror = 0

1234     continue
         !  error handling
!      call cirr(xk(k0), yk(k0), ncolhl)

!     deallocate saved arrays
         if (allocated(top%xi)) deallocate (top%xi, top%eta, top%nmk, top%nmk2)

         !  deallocate arrays
         if (allocated(adm%icell)) deallocate (adm%icell, adm%kk2, adm%kkc)
         if (allocated(xi)) deallocate (xi, eta)
         if (allocated(lnewtopo)) deallocate (lnewtopo)

      end subroutine orthonet_comp_ops

!>     allocate operator structure
      subroutine orthonet_alloc_op(op, nmkloc, nmk2loc)

         use m_inverse_map

         implicit none

         type(tops) :: op !< structure with operators
         integer :: nmkloc !< number of link-connected neighboring nodes
         integer :: nmk2loc !< number of nodes in stencil

         allocate (op%Az(nmk2loc, nmkloc))
         allocate (op%Gxi(nmk2loc, nmkloc))
         allocate (op%Geta(nmk2loc, nmkloc))
         allocate (op%Divxi(nmkloc))
         allocate (op%Diveta(nmkloc))
         allocate (op%Jxi(nmk2loc))
         allocate (op%Jeta(nmk2loc))
         allocate (op%ww2(nmk2loc))

         if (.not. allocated(op%Az)) then
            continue
         end if

      end subroutine orthonet_alloc_op

!>    deallocate op
      subroutine orthonet_dealloc_op(op)

         use m_inverse_map

         implicit none

         type(tops) :: op !< structure with operators

         if (allocated(op%Az)) deallocate (op%Az)
         if (allocated(op%Gxi)) deallocate (op%Gxi)
         if (allocated(op%Geta)) deallocate (op%Geta)
         if (allocated(op%Divxi)) deallocate (op%Divxi)
         if (allocated(op%Diveta)) deallocate (op%Diveta)
         if (allocated(op%Jxi)) deallocate (op%Jxi)
         if (allocated(op%Jeta)) deallocate (op%Jeta)
         if (allocated(op%ww2)) deallocate (op%ww2)

      end subroutine orthonet_dealloc_op

!>     deallocate ops
      subroutine orthonet_dealloc_ops(ops)

         use m_inverse_map

         implicit none

         type(tops), allocatable, dimension(:) :: ops !< operators for each unique topology

         integer :: itopo

         if (.not. allocated(ops)) return

         do itopo = 1, size(ops)
            call orthonet_dealloc_op(ops(itopo))
         end do

      end subroutine orthonet_dealloc_ops

!>  determine and store unique topologies, based on xi and eta
!!    topology is defined by the node angels w.r.t. center node: theta
      subroutine orthonet_save_topo(k0, adm, xi, eta, top, lisnew)
         use precision, only: dp
         use m_netw
         use m_missing
         use m_inverse_map

         implicit none

         integer :: k0 !< center node
         type(tadm) :: adm !< structure with administration
         real(kind=dp), allocatable, dimension(:) :: xi, eta !< node coordinates (xi,eta)
         type(ttop) :: top !< structure with topology arrays
         logical :: lisnew !< new topology (.true.) or not (.false.)

         real(kind=dp), dimension(adm%nmk2) :: theta ! atan(eta/xi)

         real(kind=dp) :: theta_sav

         integer :: k
         integer :: itopo, idum

         integer, dimension(2) :: newbound

         real(kind=dp), parameter :: TOL = 1e-4

!     compute the angle theta of the nodes connected to center node k0
         theta = DMISS
         do k = 1, nmk2(k0)
            theta(k) = atan2(eta(k), xi(k))
         end do

         !    determine if the topology is new
         lisnew = .true.
         topo: do idum = 1, 1
            if (adm%nmk2 > ubound(top%xi, 1)) cycle topo

            do itopo = 1, numtopo

               if (adm%nmk /= top%nmk(itopo) .or. adm%nmk2 /= top%nmk2(itopo)) cycle

               lisnew = .false.
               do k = 2, adm%nmk2 ! center node (k=0) not considered
                  theta_sav = atan2(top%eta(k, itopo), top%xi(k, itopo))
                  if (abs(theta(k) - theta_sav) > TOL) then
                     lisnew = .true.
                     exit
                  end if

               end do

               if (.not. lisnew) then
                  !             match found
                  ktopo(k0) = itopo
                  exit
               end if
            end do

         end do topo

         ! save the node angles and coordinates (xi,eta)
         if (lisnew) then
            numtopo = numtopo + 1

            !    check array size and increase if necessary
            newbound = ubound(top%xi)
            if (adm%nmk2 > newbound(1) .or. numtopo > newbound(2)) then
               newbound = (/max(adm%nmk2, newbound(1)), max(numtopo, newbound(2))/)
               call realloc(top%xi, newbound, fill=DMISS)
               call realloc(top%eta, newbound, fill=DMISS)
               call realloc(top%nmk, newbound(2))
               call realloc(top%nmk2, newbound(2))
            end if
            !    fill arrays
            top%xi(1:adm%nmk2, numtopo) = xi(1:adm%nmk2)
            top%eta(1:adm%nmk2, numtopo) = eta(1:adm%nmk2)
            top%nmk(numtopo) = adm%nmk
            top%nmk2(numtopo) = adm%nmk2
            ktopo(k0) = numtopo
         end if

      end subroutine orthonet_save_topo

!>  Anorm = (Ax,y)
      real(kind=dp) function Anorm(x, y, A)
         use precision, only: dp

         implicit none

         real(kind=dp), dimension(2) :: x, y !< 2-dim. vectors
         real(kind=dp), dimension(2, 2) :: A !< 2x2 matrix

         Anorm = (A(1, 1) * x(1) + A(1, 2) * x(2)) * y(1) + &
                 (A(2, 1) * x(1) + A(2, 2) * x(2)) * y(2)

      end function

!> compute operators
!!    compute coefficientmatrix G of gradient at link
!!      (d Phi / d xi)_l  = sum_{k=1}^nmk2 Gxi_k,l  Phi_k
!!      (d Phi / d eta)_l = sum_{k=1}^nmk2 Geta_k,l Phi_k
!!    compute coefficientmatrix Div of gradient in node
!!      d Phi / d xi  = sum_{l=1}^nmk Divxi_l Phi_l
!!      d Phi / d eta = sum_{l=1}^nmk Diveta_l Phi_l
!!    compute coefficientmatrix Az of cell-center in cell
!!      Phi_c = sum_{l-1}^nmk Az_l Phi_l
!!    Gxi, Geta, Divxi, Diveta and Az are stored in (type tops) op
      subroutine orthonet_comp_operators(k0, adm, xi, eta, op, ierror)
         use precision, only: dp
         use m_netw
         use m_sferic
         use m_inverse_map
         use unstruc_colors
         use m_tek_link

         implicit none

         integer :: k0 !< center node
         type(tadm) :: adm !< structure with administration
         real(kind=dp), allocatable, dimension(:) :: xi, eta !< node coordinates (xi,eta)
         type(tops) :: op !< structure with operaters
         integer, optional :: ierror !< 0: no error, 1: error

         integer :: ierror_
         integer :: kknode1, kknode0
         integer :: klink, L
         integer :: icellL, icellR
         integer :: kcellL, kcellR
         integer :: NL, NR
         integer :: k, k1

         real(kind=dp) :: I_LR_SWAP ! either 1d0 or -1d0

         real(kind=dp) :: xi1, eta1, xiL, etaL, xiR, etaR
         real(kind=dp) :: xi_bc, eta_bc
         real(kind=dp) :: exiLR, eetaLR, exi01, eeta01
         real(kind=dp) :: fac, alpha, alphaL, alphaR, alpha_x
         real(kind=dp) :: facxiL, facxiR, facetaL, facetaR
         real(kind=dp) :: facxi0, facxi1, faceta0, faceta1
         real(kind=dp) :: volxi

         real(kind=dp), dimension(nmkx2) :: xinodes, etanodes
         integer :: ic, N

         real(kind=dp), dimension(nmkx) :: xL, yL, xR, yR ! for Jacobian
         real(kind=dp) :: x_bc, y_bc

         integer, dimension(2) :: kbound ! boundary links
         real(kind=dp), dimension(nmkx) :: xis, etas

         integer :: kL, kR, linkL, linkR

         real(kind=dp) :: RlinkL, RlinkR, cDPhi
         real(kind=dp) :: volwwxi

         ierror_ = 1
         if (present(ierror)) ierror = ierror_

!     initialize
         op%Az = 0d0
         op%Gxi = 0d0
         op%Geta = 0d0
         op%Divxi = 0d0
         op%Diveta = 0d0
         op%Jxi = 0d0
         op%Jeta = 0d0

         volxi = 0d0

         kbound = 0
         xis = 0d0
         etas = 0d0

         xinodes = 0d0
         etanodes = 0d0

         volwwxi = 0d0

         if (k0 == 81) then
            continue
         end if

!     fill the averaging matrix
         do ic = 1, adm%Ncell
            if (adm%icell(ic) < 1 .or. nb(k0) == 3) cycle

!        note: linkL and linkR refer to the directly connected left and right nodes
            linkL = ic + 1 ! by construction
            linkR = linkL + 1; if (linkR > adm%Ncell + 1) linkR = linkR - adm%Ncell
            RlinkL = sqrt(xi(linkL)**2 + eta(linkL)**2 + 1d-16)
            RlinkR = sqrt(xi(linkR)**2 + eta(linkR)**2 + 1d-16)
            cDPhi = (xi(linkR) * xi(linkL) + eta(linkR) * eta(linkL)) / (RlinkL * RlinkR)

            N = netcell(adm%icell(ic))%n
            k = 1; do while (netcell(adm%icell(ic))%nod(k) /= k0 .and. k < N); k = k + 1; end do

            kL = k - 1; if (kL < 1) kL = kL + N
            kR = k + 1; if (kR > N) kR = kR - N
            if (N == 3) then ! triangles: circumcenter
               alpha = 1d0 / (1d0 - cDphi**2 + 1e-8)
               alphaL = 0.5d0 * (1d0 - RlinkL / RlinkR * cDphi) * alpha
               alphaR = 0.5d0 * (1d0 - RlinkR / RlinkL * cDphi) * alpha
               op%Az(adm%kkc(k, ic), ic) = 1d0 - (alphaL + alphaR)
               op%Az(adm%kkc(kL, ic), ic) = alphaL
               op%Az(adm%kkc(kR, ic), ic) = alphaR
            else
               op%Az(adm%kkc(1:N, ic), ic) = 1d0 / dble(N)
            end if
         end do

         do klink = 1, adm%Ncell
!        find link
            L = nod(k0)%lin(klink)

!        find node 1
            k1 = kn(1, L) + kn(2, L) - k0
            icellL = lne(1, L) ! not necessarily, but only to find (xi,eta) of node1
            kcellL = 1
            do while (adm%icell(kcellL) /= icellL .and. kcellL < adm%Ncell)
               kcellL = kcellL + 1
            end do

            if (adm%icell(kcellL) /= icellL) then ! cell not found, this happens when the cell is outside of the polygon
!            call qnerror( 'orthonet_comp_operators: cell not found', ' ', ' ')
               call cirr(xk(k0), yk(k0), ncolhl)
               ierror_ = 0
               goto 1234
            end if

            xi1 = xi(klink + 1) ! by construction
            eta1 = eta(klink + 1)

            I_LR_SWAP = 1d0 ! Left and Right are swapped when the boundary is at the left

            if (lnn(L) == 1) then
!-----------------------------------------------------------------------------------
!        boundary condition
!-----------------------------------------------------------------------------------
               !           remember the boundary links
               if (kbound(1) < 1) then ! first  boundary link
                  kbound(1) = klink
               else ! second boundary link
                  kbound(2) = klink
               end if

!           find the boundary cell in the icell array
!           assume boundary at the right
!           swap Left and Right if the boundary is at the left with I_SWAP_LR
               if (klink /= kcellL) I_LR_SWAP = -1d0

               xiL = sum(xi(1:adm%nmk2) * op%Az(1:adm%nmk2, kcellL))
               etaL = sum(eta(1:adm%nmk2) * op%Az(1:adm%nmk2, kcellL))

!           compute the cell center coordinates (x, y)
               xL(klink) = sum(xk(adm%kk2(1:adm%nmk2)) * op%Az(1:adm%nmk2, kcellL))
               yL(klink) = sum(yk(adm%kk2(1:adm%nmk2)) * op%Az(1:adm%nmk2, kcellL))

!-----------------------------------------
!           boundary conditions appear here
!           note: non-orthogonal boundary conditions!
!                 orthogonal boundary conditions by setting ATPF>0 at the boundary
!-----------------------------------------
               alpha = xiL * xi1 + etaL * eta1
               alpha = alpha / (xi1**2 + eta1**2)

               alpha_x = alpha; 
               if (alpha_x /= 0.5d0) then
                  continue
               end if

               xi_bc = alpha * xi1
               eta_bc = alpha * eta1

               xiR = 2d0 * xi_bc - xiL
               etaR = 2d0 * eta_bc - etaL

!           compute the cell center coordinates (x, y)
               x_bc = (1d0 - alpha_x) * xk(k0) + alpha_x * xk(k1)
               y_bc = (1d0 - alpha_x) * yk(k0) + alpha_x * yk(k1)
               xR(klink) = 2d0 * x_bc - xL(klink)
               yR(klink) = 2d0 * y_bc - yL(klink)
            else

!           find the left- and right-hand-side cells with respect to the link
               kcellL = klink ! by construction
               kcellR = kcellL - 1
               if (kcellR < 1) kcellR = kcellR + adm%Ncell

               if (kcellR < 1) then
                  continue
               end if

               icellL = adm%icell(kcellL)
               icellR = adm%icell(kcellR)

!           check if right cells are found
               if ((icellL /= lne(1, L) .and. icellL /= lne(2, L)) .or. &
                   (icellR /= lne(1, L) .and. icellR /= lne(2, L))) then
                  call teklink(L, ncolhl)
                  call qnerror('orthonet_comp_operators: wrong/no cells found', ' ', ' ')
                  return
               end if

               NL = netcell(icellL)%n
               NR = netcell(icellR)%n

!           compute the cell center coordinates (xi, eta)
               xiL = sum(xi(1:adm%nmk2) * op%Az(1:adm%nmk2, kcellL))
               etaL = sum(eta(1:adm%nmk2) * op%Az(1:adm%nmk2, kcellL))
               xiR = sum(xi(1:adm%nmk2) * op%Az(1:adm%nmk2, kcellR))
               etaR = sum(eta(1:adm%nmk2) * op%Az(1:adm%nmk2, kcellR))

!           compute the cell center coordinates (x, y)
               xL(klink) = sum(xk(adm%kk2(1:adm%nmk2)) * op%Az(1:adm%nmk2, kcellL))
               yL(klink) = sum(yk(adm%kk2(1:adm%nmk2)) * op%Az(1:adm%nmk2, kcellL))
               xR(klink) = sum(xk(adm%kk2(1:adm%nmk2)) * op%Az(1:adm%nmk2, kcellR))
               yR(klink) = sum(yk(adm%kk2(1:adm%nmk2)) * op%Az(1:adm%nmk2, kcellR))
            end if

!        compute the halfway link coordinates for Divxi and Diveta
            xis(klink) = 0.5d0 * (xiL + xiR)
            etas(klink) = 0.5d0 * (etaL + etaR)

!        compute link vectors eLR and e01
            exiLR = (xiR - xiL)
            eetaLR = (etaR - etaL)
            exi01 = xi1
            eeta01 = eta1

            fac = 1d0 / abs(exi01 * eetaLR - eeta01 * exiLR + 1d-16)
            facxi1 = -eetaLR * fac * I_LR_SWAP
            facxi0 = -facxi1
            faceta1 = exiLR * fac * I_LR_SWAP
            faceta0 = -faceta1
            facxiR = eeta01 * fac * I_LR_SWAP
            facxiL = -facxiR
            facetaR = -exi01 * fac * I_LR_SWAP
            facetaL = -facetaR

!        boundary link
            if (lnn(L) == 1) then
               facxi1 = facxi1 - facxiL * 2d0 * alpha_x
               facxi0 = facxi0 - facxiL * 2d0 * (1 - alpha_x)
               facxiL = facxiL + facxiL
!           note that facxiR does not exist
               faceta1 = faceta1 - facetaL * 2d0 * alpha_x
               faceta0 = faceta0 - facetaL * 2d0 * (1 - alpha_x)
               facetaL = facetaL + facetaL
!           note that facetaR does not exist
            end if

!        get the nodes in the kk2 numbering
            kknode1 = klink + 1
            kknode0 = 1

            op%Gxi(:, klink) = 0d0
            op%Geta(:, klink) = 0d0

!        fill the weights
            op%Gxi(:, klink) = facxiL * op%Az(:, kcellL)
            op%Geta(:, klink) = facetaL * op%Az(:, kcellL)

            if (lnn(L) == 2) then
               op%Gxi(:, klink) = op%Gxi(:, klink) + facxiR * op%Az(:, kcellR)
               op%Geta(:, klink) = op%Geta(:, klink) + facetaR * op%Az(:, kcellR)
            end if

            op%Gxi(kknode1, klink) = op%Gxi(kknode1, klink) + facxi1
            op%Geta(kknode1, klink) = op%Geta(kknode1, klink) + faceta1

            op%Gxi(kknode0, klink) = op%Gxi(kknode0, klink) + facxi0
            op%Geta(kknode0, klink) = op%Geta(kknode0, klink) + faceta0

!        fill the node-based gradient matrix
            op%Divxi(klink) = -eetaLR * I_LR_SWAP
            op%Diveta(klink) = exiLR * I_LR_SWAP

            if (lnn(L) == 1) then ! boundary link
!            op%Divxi( klink) = 0.5d0*op%Divxi( klink)
!            op%Diveta(klink) = 0.5d0*op%Diveta(klink)

               op%Divxi(klink) = 0.5d0 * op%Divxi(klink) + eta_bc * I_LR_SWAP
               op%Diveta(klink) = 0.5d0 * op%Diveta(klink) - xi_bc * I_LR_SWAP
            end if

            xinodes(klink + 1) = xi1
            etanodes(klink + 1) = eta1
         end do

         xinodes(1) = 0d0
         etanodes(1) = 0d0

!     Add boundary contribution to node-based gradient
!      if ( kbound(2).gt.0 ) then
!         op%Divxi( kbound) = op%Divxi( kbound) - 0.5d0*sum(op%Divxi)
!         op%Diveta(kbound) = op%Diveta(kbound) - 0.5d0*sum(op%Diveta)
!      end if

         volxi = 0d0
         do klink = 1, nmk(k0)
            volxi = volxi + 0.5 * (op%Divxi(klink) * xis(klink) + op%Diveta(klink) * etas(klink))
         end do
         if (volxi == 0d0) volxi = 1d0

         op%Divxi = op%Divxi / volxi
         op%Diveta = op%Diveta / volxi

!     compute the node-to-node gradients
         do k = 1, adm%Ncell
            if (lnn(nod(k0)%lin(k)) == 2) then ! internal link
               kR = k - 1 ! right neighboring cell, left one is k by construction
               if (kR < 1) kR = kR + nmk(k0)
               op%Jxi(1:nmk2(k0)) = op%Jxi(1:nmk2(k0)) + op%Divxi(k) * 0.5d0 * (op%Az(1:nmk2(k0), k) + op%Az(1:nmk2(k0), kR))
               op%Jeta(1:nmk2(k0)) = op%Jeta(1:nmk2(k0)) + op%Diveta(k) * 0.5d0 * (op%Az(1:nmk2(k0), k) + op%Az(1:nmk2(k0), kR))
            else ! boundary link, 1: center node, k+1: connected node through link k
!            op%Jxi(  1)  = op%Jxi(  1)  + op%Divxi(k)  * 0.5d0
!            op%Jxi(k+1)  = op%Jxi(k+1)  + op%Divxi(k)  * 0.5d0
!            op%Jeta(  1) = op%Jeta(  1) + op%Diveta(k) * 0.5d0
!            op%Jeta(k+1) = op%Jeta(k+1) + op%Diveta(k) * 0.5d0

               op%Jxi(1) = op%Jxi(1) + op%Divxi(k) * 0.5d0
               op%Jxi(k + 1) = op%Jxi(k + 1) + op%Divxi(k) * 0.5d0
               op%Jeta(1) = op%Jeta(1) + op%Diveta(k) * 0.5d0
               op%Jeta(k + 1) = op%Jeta(k + 1) + op%Diveta(k) * 0.5d0
            end if
         end do

!     compute the weights in the Laplacian smoother
         op%ww2(:) = 0d0
         do k = 1, nmk(k0)
            op%ww2(1:nmk2(k0)) = op%ww2(1:nmk2(k0)) + &
                                 op%Divxi(k) * op%Gxi(:, k) + op%Diveta(k) * op%Geta(:, k)
         end do

         ierror_ = 0

1234     continue

         if (present(ierror)) ierror = ierror_

      end subroutine orthonet_comp_operators

!>    assign xi and eta to all nodes in the stencil
      subroutine orthonet_assign_xieta(k0, adm, xi, eta, ierror)
         use precision, only: dp
         use m_netw
         use m_sferic
         use m_missing
         use unstruc_messages
         use m_inverse_map
         use unstruc_display
         use unstruc_colors

         implicit none

         integer :: k0 !< center node
         type(tadm) :: adm !< structure with administration
         real(kind=dp), allocatable, dimension(:) :: xi, eta !< node coordinates (xi,eta)
         integer, optional :: ierror !< 0: no error, 1: error

         real(kind=dp) :: Phi0, DPhi0, DPhitot ! angles in the (xi,eta) frame
         real(kind=dp) :: theta, Dtheta ! angles in the (xi',eta') frame, attached to a cell
         real(kind=dp) :: xip, etap ! coordinates in (xi', eta') frame, attached to a cell
         real(kind=dp) :: R0, dmu, aspect

         integer :: k, kk, ic, L, N, kL, kR, k1, kk1, L1, kcell

         real(kind=dp) :: FAC = 1d0 ! part of the full circle that needs to be filled

         integer :: Nnodes, Ntri, Ntri_square, Nquad, icL, icR
         real(kind=dp) :: DPhi, DPhitri, DPhitri_square, DPhiquad, DPhimin
         real(kind=dp) :: dmutri, dmutri_square
         integer :: ilink_first_quad
         integer :: kp1, km1
         real(kind=dp), dimension(adm%Ncell) :: Philink
         real(kind=dp), dimension(adm%nmk2) :: theta_square ! 'square' angles
         logical, dimension(adm%Ncell) :: L_is_square_cell
         logical :: L_is_square
         integer :: KCHECK, ierror_
         logical :: lblink ! boundary link (.true.) or not (.false.)

         KCHECK = 93

         ierror_ = 1

         if (present(ierror)) ierror = ierror_

         FAC = 1d0
         if (nb(k0) == 2) FAC = 0.5d0 ! boundary node
         if (nb(k0) == 3) FAC = 0.25d0 ! corner node

!     initialize xi and eta to zero
         xi = 0d0
         eta = 0d0

!     first, determine the 'skewness' factor dmu
!     only skew triangles: discriminate between triangles and non-triangels
         DPhimin = 15d0 / 180d0 * pi
         DPhitot = 0d0
         Dphitri_square = 0d0
         Dphitri = 0d0
         Dphiquad = 0d0
         Ntri = 0
         Ntri_square = 0
         Nquad = 0

         ilink_first_quad = 0

         if (k0 == KCHECK) then
            continue
         end if
!-------------------------------------------------------------------------------------------------
!     find the square angles of the directly connected links, i.e. 2, ..., Ncell+1
         theta_square = DMISS
         L_is_square_cell = .false.
         do kk = 1, adm%Ncell
!        find node k1 connected to the center node k0 by kk-th link L
            L = nod(k0)%lin(kk)
            k1 = adm%kk2(kk + 1)
!        get the cells icL and icR connected to both nodes
            icL = lne(1, L)
            icR = icL
            if (lnn(L) == 2) icR = lne(2, L)

!        if:
!           all other cells connected to node k1 are quads, and
!           the total numbers of quads attached to the node is less than four,ernal node
!        then:
!           the angle is 'square' (pi/2, pi or 3pi/2)
            L_is_square = .true.
!        loop over all cells through links
            do kk1 = 1, nmk(k1)
               L1 = nod(k1)%lin(kk1)
               do kcell = 1, lnn(L1)
                  ic = lne(kcell, L1)
                  if (ic /= icL .and. ic /= icR) &
                     L_is_square = L_is_square .and. (netcell(ic)%n == 4)
               end do
               if (.not. L_is_square) exit
            end do

!         if ( nmk(k1).eq.4 .and. nb(k1).eq.1 ) L_is_square=.true.

!        compute the optimal angle theta_square, if applicable
            kL = kk - 1; if (kL < 1) kL = kL + adm%Ncell
!        Nquad is the number of quads not connected to k1
            if (L_is_square) then
               if (nb(k1) == 1 .or. nb(k1) == 4) then ! inner node
                  Nquad = nmk(k1) - 2
                  theta_square(kk + 1) = (2d0 - dble(Nquad) * 0.5d0) * pi
               else if (nb(k1) == 2) then ! boundary node
                  Nquad = nmk(k1) - 1 - lnn(L)
                  theta_square(kk + 1) = (1d0 - dble(Nquad) * 0.5d0) * pi
               else if (nb(k1) == 3) then ! corner node
                  theta_square(kk + 1) = 0.5d0 * pi
               end if

!           check the total number of quads connected
!             by adding the square cells that are in the stencil
               if (adm%icell(kk) > 1) then
                  if (netcell(adm%icell(kk))%n == 4) Nquad = Nquad + 1
               end if
               if (adm%icell(kL) > 1) then
                  if (netcell(adm%icell(kL))%n == 4) Nquad = Nquad + 1
               end if
               if (Nquad > 3) L_is_square = .false.
            end if

!        mark the left and right neighboring cells as square
            L_is_square_cell(kk) = L_is_square_cell(kk) .or. L_is_square
            L_is_square_cell(kL) = L_is_square_cell(kL) .or. L_is_square

         end do

!     continue with the indirectly connected links, from Ncell+2 to adm&nmk2
!     find the 'square' angles belonging to quads
         do ic = 1, adm%Ncell
            if (adm%icell(ic) < 1) cycle ! fictitious boundary cell
            Nnodes = netcell(adm%icell(ic))%n
            if (Nnodes == 4) then
               do kk = 1, Nnodes
                  if (adm%kkc(kk, ic) <= adm%Ncell + 1) cycle ! center and directly-connected cells
                  theta_square(adm%kkc(kk, ic)) = 0.5d0 * pi
               end do
            end if
         end do

         if (k0 == KCHECK) then
            continue
         end if
!-------------------------------------------------------------------------------------------------
!     compute the internal link angle Phi
         Nquad = 0
         do kk = 1, adm%Ncell
            if (adm%icell(kk) < 1) cycle ! fictitious boundary cell
            Nnodes = netcell(adm%icell(kk))%n
!        DPhi    = pi - 2d0*pi/dble(Nnodes)
            Dphi = opt_angle(Nnodes)

!        account for 'square' angles
            if (L_is_square_cell(kk) .or. Nnodes == 4) then
               kR = kk + 2; if (kR > adm%Ncell + 1) kR = kR - adm%Ncell
               lblink = (lnn(nod(k0)%lin(kk)) == 1)
               Dphi = opt_angle(Nnodes, theta_square(kk + 1), theta_square(kR), lblink)
               if (Nnodes == 3) then
                  Ntri_square = Ntri_square + 1
                  DPhitri_square = Dphitri_square + DPhi
               else if (Nnodes == 4) then
                  Nquad = Nquad + 1
                  DPhiquad = Dphiquad + DPhi
               end if
            else
               Ntri = Ntri + 1
               DPhitri = Dphitri + DPhi
            end if
            DPhitot = DPhitot + DPhi

!        if (Nnodes.eq.3) then
!            Ntri    = Ntri+1
!            DPhitri = Dphitri + DPhi
!         else if ( Nnodes.eq.4) then
!           if (ilink_first_quad.eq.0 ) &
!              ilink_first_quad = kk+1; if ( ilink_first_quad.gt.Ncell ) ilink_first_quad = ilink_first_quad - Ncell
!           Nquad   = Nquad + 1
!        end if
         end do

         dmu = 1d0
         dmutri_square = 1d0
         dmutri = 1d0
         if (Ntri > 0) then
!         dmutri = ( FAC*2d0*pi - max(DPhitot-DPhitri, dble(Ntri)*DPhimin) ) / DPhitri
            dmutri = (FAC * 2d0 * pi - (DPhitot - DPhitri)) / DPhitri
            dmutri = max(dmutri, dble(Ntri) * Dphimin / DPhitri)
            if (dmutri < 1e-4) then
               continue
            end if
         else
            if (Ntri_square > 0) then
               dmutri_square = max(FAC * 2d0 * pi - (DPhitot - DPhitri_square), dble(Ntri_square) * DPhimin) / DPhitri_square
            end if
         end if

!     if ( (abs(dmutri-0.75d0).lt.1E-2 .or. abs(dmutri-1.5d0).lt.1E-2) ) then
!        L_is_square = .true.
!     else
!!       dmutri = 1d0
!        L_is_square = .false.
!      end if

         L_is_square = .true.
         if (Nquad < 2) then
!        dmutri = 1d0
            L_is_square = .false.
         end if

!     L_is_square = .true.
         if (Dphitot > 1e-18) then
            dmu = FAC * 2d0 * pi / (Dphitot - (1 - dmutri) * DPhitri - (1 - dmutri_square) * DPhitri_square)
         else if (adm%Ncell > 0) then
            call qnerror('orthonet_assign_xieta: Dphitot=0', ' ', ' ')
            call cirr(xk(k0), yk(k0), ncolhl)
            ierror_ = 0
            goto 1234
         end if

         if (k0 == KCHECK) then
            continue
         end if
!-------------------------------------------------------------------------------------------------
!     loop over the cells
         Phi0 = 0d0
         Dphi0 = 0d0
         Dphi = 0d0
         do ic = 1, adm%Ncell
!        add half the angle of the previous cell
            Phi0 = Phi0 + 0.5d0 * Dphi

            Philink(ic) = Phi0
            if (adm%icell(ic) < 1) then ! fictitious boundary cell
               if (nb(k0) == 2) then ! boundary node
                  Dphi = pi
               else
                  if (nb(k0) == 3) then ! corner node
                     DPhi = 1.5d0 * pi
                  else ! inappropriate fictitious boundary cell
                     call qnerror('orthonet_assign_xieta: inappropriate fictitious boundary cell', ' ', ' ')
                     call cirr(xk(k0), yk(k0), ncolhl)
                     return
                  end if
               end if
               Phi0 = Phi0 + 0.5d0 * DPhi
               cycle
            end if

            N = netcell(adm%icell(ic))%n
            if (N > M) then
               call qnerror('orthonet_assign_xiet: N>M', ' ', ' ')
               return
            end if

!        compute the optimal angle between the next two links
!         Dphi0 = pi - 2d0*pi/dble(netcell(icell(ic))%n)
            Nnodes = netcell(adm%icell(ic))%n
            Dphi0 = opt_angle(Nnodes)
            if (L_is_square_cell(ic)) then
               kR = ic + 2; if (kR > adm%Ncell + 1) kR = kR - adm%Ncell
               lblink = (lnn(nod(k0)%lin(ic)) == 1)
               Dphi0 = opt_angle(Nnodes, theta_square(ic + 1), theta_square(kR), lblink)
               if (Nnodes == 3) Dphi0 = dmutri_square * Dphi0
            else if (Nnodes == 3) then
               Dphi0 = dmutri * Dphi0
            end if

!        compute the skewed angle
            DPhi = dmu * Dphi0
!        if ( .not.L_is_square_cell(ic) ) Dphi = dmutri*DPhi

!        add half the angle of the current cell
            Phi0 = Phi0 + 0.5d0 * DPhi

!        in this cell: find the node k that corresponds to the center node
            k = 1
            do while (netcell(adm%icell(ic))%nod(k) /= k0 .and. k < N)
               k = k + 1
            end do
            if (netcell(adm%icell(ic))%nod(k) /= k0) then
               call qnerror('orthonet_assign_xieta: center node not found in cell', ' ', ' ')
               return
            end if

!        compute the optimal angle
            Dtheta = 2d0 * pi / dble(netcell(adm%icell(ic))%n)

!        determine the orientation of the cell (necessary for folded cells)
            kp1 = k + 1; if (kp1 > Nnodes) kp1 = kp1 - Nnodes
            km1 = k - 1; if (km1 < 1) km1 = km1 + Nnodes
            if (adm%kkc(km1, ic) - adm%kkc(kp1, ic) == -1 .or. &
                adm%kkc(km1, ic) - adm%kkc(kp1, ic) == adm%nmk - 1) then
               Dtheta = -Dtheta
            end if

!        compute the aspect ratio
            aspect = (1d0 - cos(Dtheta)) / sin(abs(Dtheta)) * tan(0.5d0 * dPhi)

!        compute the radius
            R0 = cos(0.5d0 * dPhi) / (1d0 - cos(Dtheta))

!        loop over all nodes comprising the netcell
            do kk = 1, N
               theta = Dtheta * (kk - k)
               xip = R0 - R0 * cos(theta)
               etap = -R0 * sin(theta)

               xi(adm%kkc(kk, ic)) = xip * cos(Phi0) - aspect * etap * sin(Phi0)
               eta(adm%kkc(kk, ic)) = xip * sin(Phi0) + aspect * etap * cos(Phi0)
            end do

         end do

         ierror_ = 0

1234     continue

         if (present(ierror)) ierror = ierror_

      end subroutine orthonet_assign_xieta

!>  compute the optimal angle between two links
      real(kind=dp) function opt_angle(Nnodes, theta1, theta2, lblink)
         use precision, only: dp
         implicit none

         integer :: Nnodes !< number of nodes in the netcell
         real(kind=dp), optional :: theta1, theta2 !< optionally: link angles
         logical, optional :: lblink !< optionally: is boundary link (.true.) or not (.false.)

         logical :: lblink_

         lblink_ = .false.
         if (present(lblink)) lblink_ = lblink

         opt_angle = pi * (1 - 2d0 / dble(Nnodes))

         if (present(theta1)) then ! 'square' angle
            if (present(theta2)) then
               if (Nnodes == 3) then
                  opt_angle = 0.25d0 * pi
                  if (theta1 + theta2 == pi .and. .not. lblink_) opt_angle = 0.5d0 * pi
               else if (Nnodes == 4) then
                  opt_angle = 0.5d0 * pi
               end if
            end if
         end if

         return

      end function opt_angle

!>  smooth the node-based variable u
      subroutine orthonet_smooth_u(u, ITAPSM, u_smooth)
         use precision, only: dp
         use m_netw
         use m_orthosettings
         use unstruc_messages
         use m_alloc
         use m_inverse_map

         implicit none

         real(kind=dp), dimension(:), intent(in) :: u !< node-based solution
         integer, intent(in) :: ITAPSM !< number of smoothing iterations
         real(kind=dp), dimension(:), intent(out) :: u_smooth !< smoothed node-based solution

         real(kind=dp), dimension(nmkx2) :: ww2
         real(kind=dp), dimension(numk) :: u_temp
         integer :: iter, k0

         real(kind=dp) :: alpha, alpha1

         alpha = 0.5d0

         alpha1 = 1d0 - alpha

         u_temp = u
         u_smooth = u

         do iter = 1, ITAPSM
            do k0 = 1, Numk
               if (nb(k0) /= 1 .and. nb(k0) /= 2 .and. nb(k0) /= 4) cycle

               !        get the Laplacian weights
               ww2 = 0d0
!            ww2(1:nmk2(k0)) = ops(ktopo(k0))%ww2(1:nmk2(k0))

               ww2(1:nmk2(k0)) = 1d0; 
               ww2(1) = -sum(ww2(2:nmk2(k0)))

               u_temp(k0) = -sum(ww2(2:nmk2(k0)) * u_smooth(kk2(2:nmk2(k0), k0))) / ww2(1)
            end do

            u_smooth = alpha * u_temp + alpha1 * u_smooth

         end do

      end subroutine orthonet_smooth_u

   end subroutine orthogonalisenet

end module m_orthogonalisenet
