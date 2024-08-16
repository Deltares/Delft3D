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

 subroutine flow_geominit(iphase) ! initialise flow geometry
    use m_netw
    use m_flowgeom
    use m_GlobalParameters, only: INDTP_2D
    use unstruc_model
    use fm_external_forcings_data
    use m_physcoef
    use m_flowparameters
    use m_flowtimes, only: ti_waq
    use m_sferic
    use m_missing
    use m_alloc
    use unstruc_files, only: basename
    use m_orthosettings
    use m_xbeach_data, only: itheta_view
    use m_heatfluxes
    use fm_external_forcings
    use m_partitioninfo
    use dfm_error
    use m_ship
    use kdtree2Factory
    use unstruc_display, only: jagui
    use unstruc_messages
    use string_module
    use m_plotdots
    use geometry_module, only: getdx, getdy, dbdistance, normalin, normalout, half, duitpl, dlinedis
    use stdlib_sorting, only: sort_index
    use m_flowtimes, only: ti_waq
    use gridoperations
    use m_flow, only: numlimdt, numlimdt_baorg
    use m_oned_functions
    use unstruc_channel_flow, only: network
    use m_sediment, only: stm_included
    use m_flowtimes, only: handle_extra
    use Timers
    use m_structures
    use unstruc_messages
    use m_find_flownode, only: find_nearest_flownodes_kdtree
    use m_turbulence, only: ln0

    implicit none

    integer, intent(in) :: iphase ! phase in geominit, 0 (all), 1 (first) or 2 (second)

    character(len=100) :: fnam

    ! locals
    integer :: m, n, k, k1, k2, k3, k4, L, Lf, LL, LLL, ierr, nn, ja, kh, numswap, n12, La
    integer :: n1, n2, n1a, n2a, ja1D, ka, kb, k1n, k2n
    integer :: nc1, nc2, nex
    double precision :: sig ! for bottom level help
    double precision :: dxn1e ! node 1 - edge distance
    double precision :: dxn2e ! node 2 - edge distance
    double precision :: x12, y12 ! link center coordinates
    double precision :: rn, rt ! for link L, normal and tangent base vectors
    double precision :: rnl, rtl ! for other links LL, normal and tangent base vectors
    double precision :: ortho, avortho ! inner product of link and face
    double precision :: af ! only for subr readyy
    double precision :: xx(6), yy(6) ! for skewness
    logical :: isbadlink ! Bad link (e.g. too short)
    logical :: noncrossinglink ! At least 1 1D2D link fails to cross a 2D cell face
    character(len=5) :: txt
    integer, allocatable :: inonlin(:) ! Array of nonLin1D, nonLin2D and nonLin, used for MPI communication

    integer :: nw, L1, L2, LLA ! wall stuff
    integer :: icn ! corner stuff
    integer :: kk1, kk2, kk3 ! banf stuff
    double precision :: dlength, dlenmx, dxorgL
    double precision :: rrr, cs, sn, dis, xn, yn, xt, yt, rl, sf, hdx, alfa, dxlim, dxlink
    double precision :: phase
    double precision :: xref, yref
    integer :: jaend
    double precision :: weirheight, weirlength

    double precision, allocatable :: banh(:), rr(:) ! temp
    integer, allocatable :: nbanh(:, :), nr(:) ! temp

    integer, dimension(:), allocatable :: nw_temp
    integer :: nwx

! character(len=200), dimension(:), allocatable :: fnames
! integer                                       :: ifil

    double precision :: xh, yh

    integer :: jaidomain, jaiglobal_s, ierror
    integer :: numl2D

    double precision, external :: cosphiu
    integer :: ndraw
    common / DRAWTHIS / ndraw(50)

    numl2D = numl - numl1D

    if (numk < 2 .or. (numl1D == 0 .and. numl2D < 3) .or. (numl2D > 0 .and. numl2D < 3)) then
       call mess(LEVEL_WARN, 'A valid network requires at least 2 computational grid points (net nodes) and at least 1 netlink for 1D or 3 netlinks for 2D.')
       return ! only continue for sufficient network
    end if

    noncrossinglink = .false.

    do L = 1, numL ! isolated 1D netlink not allowed for now, crash in parallel, please check and repair
       k1 = kn(1, L); k2 = kn(2, L); k3 = kn(3, L)
       if (k3 == 1 .or. k3 == 6) then
          if (nmk(k1) == 1 .and. nmk(k2) == 1) then
             call dumpnetlink('isolated_1Dnetlink ', L)
          end if
       end if
    end do

    call readyy('geominit', 0d0)

    if (jsferic == 0) then
       jasfer3D = 0
    end if

    if (iphase == 2) then
       !  skip to start of second phase
       goto 9002
    end if

    call inisferic() ! initialise spherical parameters

    if (bedslope /= 0d0 .or. bedwaveamplitude /= 0d0) then
       do k = 1, numk
          if (zk(K) == dmiss) then
             zk(k) = zkuni + xk(k) * bedslope
             if (bedwavelength /= 0d0) then
                phase = twopi * xk(k) / bedwavelength
                zk(k) = zk(k) + bedwaveamplitude * cos(phase)
             end if
          end if
       end do
    end if

    do k = 1, numk
       if (kc(k) /= 0) kc(k) = 1 ! all active grid nodes are now kc = 1 : only to cure old net files
    end do

    call timstrt('Findcells/preparecells', handle_extra(46)) ! findcells/preparecells

!see if subomain numbers should be read from file
    jaidomain = 0
    jaiglobal_s = 0
    if (jampi == 1 .and. npartition_pol < 1) then
       jaidomain = 1
       jaiglobal_s = 1
    end if

    ierr = DFM_GENERICERROR ! no error if cell information was read from file
    if (md_findcells == 0 .or. jaidomain == 1 .or. jaiglobal_s == 1) then
       ! read cells from file, to avoid find cells
       ! and, ONLY read idomain from file if MPI is on AND no polygon enforced.
!    call preparecells(md_netfile, min(jampi, 1-md_genpolygon), ierr)
       call preparecells(md_netfile, jaidomain, jaiglobal_s, ierr)
    end if

    if (md_findcells == 1 .or. ierr /= DFM_NOERR) then ! Either force findcells, or if cells have not been found in file.
       if (ierr == DFM_NOERR) call mess(LEVEL_WARN, 'Domain numbers read from file, but overwriting cell numbering by enforcing findcells.', my_rank)

       call findcells(0) ! shortest walks in network (0 means: look for all shapes, tris, quads, pentas, hexas)
       call find1dcells()
    end if
    call timstop(handle_extra(46)) ! findcells/preparecells

    if (jaidomain == 1 .and. .not. allocated(idomain)) then
       call mess(LEVEL_ERROR, 'Domain numbers could not be read. Either the PartitionFile is missing in the MDU file, or the network file misses domain numbers, in subdomain number', my_rank)
    end if

    if (jaiglobal_s == 1 .and. .not. allocated(iglobal_s)) then
       call mess(LEVEL_INFO, 'Global cell numbers could not be read. Subdomain number:', my_rank)
    end if

    if (jampi == 1) then
       call reduce_int_min(jaiglobal_s)
    end if

    !if (len_trim(md_dryptsfile) > 0) then
    !   call strsplit(md_dryptsfile,1,fnames,1)
    !   do ifil=1,size(fnames)
    !      call delete_drypoints_from_netgeom(fnames(ifil),0)
    !   enddo
    !   deallocate(fnames)
    !end if
    call delete_dry_points_and_areas()

! also disabled isolated cells due to cutcells and store masks
    call cutcell_list(6, 1)

    if (strip_mesh > 0) then
       if (numl1d > 0) then
          call mess(LEVEL_WARN, 'Stripping mesh not yet supported when including 1D segments.')
       else
          call remove_unused_nodes_and_links()
       end if
    end if

    ! if (makeorthocenters .gt. 0 .and. jglobe == 0) then
    if (makeorthocenters > 0) then
       call make_orthocenters(0.5d-2, makeorthocenters)
    end if

    call thindams_on_netgeom() ! Convert thin dam-type cross sections to real thin dams in network kn.

    ! AvD: NOTE: We could also place this cosphiunetcheck *after* the nsmalllink
    !  check (see some blocks below). But then again based on flow links. Such
    !  that too small flow link lengths do *not* lead to large cosphiunet values. (TODO?)
    call cosphiunetcheck(1) ! Check for bad orthogonality on netlinks
    if (nlinkbadortho > 0) then
       call checknetwork() ! If badortho, check entire network for net link crossings.
       lnx = 0
       ndx = 0
       return
    end if

! move partition domain ghostcells back in array
! call partition_arrange_ghostcells(numpi,nump1d2di)

    NDX2D = NUMP ! NR OF 2d CELLS=NUMP
    NDX = NUMP1d2d ! NR OF 1d and 2d CELLS, = ndxi
    LNX1D = NUML1D

    ! so after this loop the only points with kc = 1 are 1D points
    call readyy('geominit-FINDLINKS', 0.1d0)

    ! Renumber (internal) flow nodes.
    ! Based on link data in lne (ln not yet available), which is almost
    ! correct, except for the links that will be eliminated if distance
    ! between circumcenters is very small.
    !if (jased > 0) jarenumber = 0
    if (jarenumber == 1 .and. nump > 00 .and. (.not. ti_waq > 0d0)) then
       call timstrt('Renumber flownodes', handle_extra(47)) ! renumberFlowNodes
       call renumberFlowNodes()
       call timstop(handle_extra(47)) ! renumberFlowNodes
    end if

    do n = 1, nump
       do m = 1, netcell(n)%n
          k = netcell(n)%NOD(m)
          kc(k) = 2 ! all corners of cells are now 2, 1D nodes are still 1
       end do
    end do

    ! TODO add elemnode and flowelemnode here...

! note: findexternalboundarypoints reads ext-file for the first time, and not only counts open boundaries
    call findexternalboundarypoints() ! total nr of closed boundaries to be opened

    ndxi = NDX ! total nr of 2D cellS (tris, quads, pentas and hexas) plus 1D cells
    ndx = ndxi + numbnp ! add open boundaries

! increase netcell admin. to include boundary nodes (safety)
    call add_boundarynetcells()

    if (allocated(kcs)) then
       deallocate (nd, bl, bai, kcs, bai_mor, ba_mor) ! and allocate geometry related node arrays
    end if
    allocate (nd(ndx), bl(ndx), bai(ndx), bai_mor(ndx), ba_mor(ndx), kcs(ndx), stat=ierr)
    call aerr('nd(ndx), bl(ndx), bai(ndx), bai_mor(ndx), ba_mor(ndx), kcs(ndx)', ierr, 8 * ndx); kcs = 1
    bl = dmiss
    ba_mor = 0d0

    ! for 1D only
    if (network%loaded .and. ndxi - ndx2d > 0) then
       call realloc(groundLevel, ndxi - ndx2d, keepExisting=.false., fill=dmiss, stat=ierr)
       call aerr('groundLevel(ndxi-ndx2d)', ierr, ndxi - ndx2d)

       call realloc(groundStorage, ndxi - ndx2d, keepExisting=.false., fill=0, stat=ierr)
       call aerr('groundStorage(ndxi-ndx2d)', ierr, ndxi - ndx2d)

       call realloc(volMaxUnderground, ndxi - ndx2d, keepExisting=.false., fill=dmiss, stat=ierr)
       call aerr('volMaxUnderground(ndxi-ndx2d)', ierr, ndxi - ndx2d)

    end if

    if (stm_included .and. ndx2d > ndxi) then
       call realloc(bl_ave, ndx, keepExisting=.false., fill=dmiss, stat=ierr)
       call aerr('bl_ave(ndx)', ierr, ndx)
    end if

    if (allocated(kfs)) deallocate (kfs)
    allocate (kfs(ndx)); kfs = 0

    ! Reallocate circumcenters with extra space for 1D nodes, but keep existing 2D data.
    call realloc(xz, ndx)
    call realloc(yz, ndx)
    call realloc(xzw, ndx)
    call realloc(yzw, ndx)
    call realloc(ba, ndx); ba = 0d0

    do k = 1, ndx
       nd(k)%lnx = 0
    end do

    M = max(ndx2d / 100, 1)
    sarea = 0d0
    !jacenterinside = 1
    do n = 1, ndx2d ! get cell center coordinates 2D

       kcs(n) = 2
       if (mod(n, M) == 0) then
          af = 0.2d0 + 0.6d0 * dble(n) / dble(ndx2d)
          call readyy('geominit-cell areas ba', af)
       end if

       ! Cell circumcenters for ndx2d were already determined in findcells.

       call getcellsurface(n, ba(n), xzw(n), yzw(n))
       sarea = sarea + ba(n)
       call allocateandset2Dnodexyarrays(n) ! only for plotting...
    end do
    ! jacenterinside = 0

    fwind = (5d6 / max(sarea, 1d4))**0.05d0 ! Only for jatem == 3, excess model.

    do L = 1, NUML1D ! get cell center coordinates 1D
       if (KN(3, L) == 1 .or. KN(3, L) >= 3 .and. KN(3, L) <= 7) then
          K1n = KN(1, L); K2n = KN(2, L)
          nc1 = lne(1, L); nc2 = lne(2, L)
          N1 = abs(NC1); N2 = abs(NC2)
          if (n1 == 0) then
             call dumpnetlink('flownode 1 not found for netlink = ', L)
          end if
          if (n2 == 0) then
             call dumpnetlink('flownode 2 not found for netlink = ', L)
          end if

          if (nc1 < 0) then
             k1 = netcell(n1)%nod(1)
             ! TODO: duplicated codes: xz, yz, xzw, yzw are already computed in subroutine find1Dcells.
             xz(N1) = xk(k1); yz(N1) = yk(k1); BL(N1) = ZK(K1); xzw(n1) = xz(n1); yzw(n1) = yz(n1)
             if (.not. allocated(nd(n1)%nod)) then
                allocate (nd(n1)%nod(1), stat=ierr) ! Store original net node with this flow node
                call aerr('nd(n1)%nod(1)', ierr, 1)
             else
                if (nd(n1)%nod(1) /= k1) then
                   call mess(LEVEL_ERROR, '1D numbering error')
                end if
             end if
             nd(n1)%nod(1) = k1
          end if

          if (nc2 < 0) then
             k2 = netcell(n2)%nod(1)
             xz(n2) = xk(k2); yz(n2) = yk(k2); BL(N2) = ZK(K2); xzw(n2) = xz(n2); yzw(n2) = yz(n2)
             if (.not. allocated(nd(n2)%nod)) then
                allocate (nd(n2)%nod(1), stat=ierr)
                call aerr('nd(n2)%nod(1)', ierr, 1)
             else
                if (nd(n2)%nod(1) /= k2) then
                   call mess(LEVEL_ERROR, '1D numbering error')
                end if
             end if
             nd(n2)%nod(1) = k2
          end if
       end if
    end do

    lnxi = 0
    lnx1D = 0
    nlinktoosmall = 0

    do L = 1, numl ! count nr of edges that connect cells, ie. have nd1 and nd2
       n1 = abs(lne(1, L)); n2 = abs(lne(2, L))
       if (n1 /= 0 .and. n2 /= 0 .and. KN(3, L) /= 0) then ! so that you know the nr of lins to be allocated

          isbadlink = .false.
          ! Check on too short flow links. Only for 2D. 1D is always considered 'good'.
          if (KN(3, L) == 2) then
             dxlim = 0.9d0 * removesmalllinkstrsh * 0.5d0 * (sqrt(ba(n1)) + sqrt(ba(n2)))
             dxlink = dbdistance(xz(n1), yz(n1), xz(n2), yz(n2), jsferic, jasfer3D, dmiss)
             if (dxlink < dxlim) then
                isbadlink = .true.
             end if
          end if

          if (.not. isbadlink) then
             lnxi = lnxi + 1 ! prevents connection between overlying identical elements
             if (KN(3, L) == 1 .or. KN(3, L) >= 3 .and. KN(3, L) <= 7) then ! Also recount 1D flow links (in case some are
                lnx1D = lnx1D + 1 ! thrown away by this distance check)
             end if
          else
             nlinktoosmall = nlinktoosmall + 1
             if (nlinkbadortho + nlinktoosmall > size(linkbadqual)) then
                call realloc(linkbadqual, ceiling(1.2 * nlinkbadortho + nlinktoosmall))
             end if
             linkbadqual(nlinkbadortho + nlinktoosmall) = L
             lne(1, L) = 0; lne(2, L) = 0; LNN(L) = 0
          end if
       else
          continue
       end if
    end do
    if (nlinktoosmall > 0) then

!    copy bad link coordinates to dots
       numdots = 0
       do k = 1, nlinkbadortho + nlinktoosmall
          L = linkbadqual(k)
          k1 = kn(1, L)
          k2 = kn(2, L)
          call half(xk(k1), yk(k1), xk(k2), yk(k2), xref, yref, jsferic, jasfer3D)
          call adddot(xref, yref)
       end do

       write (txt, '(i5)') nlinktoosmall
       if (jagui /= 1) then
          call qnerror(txt//' small flow links discarded. Run ''merge circumcenters'' to remove small flow links or increase threshold', ' ', ' ')
       else
          call qnerror(txt//' small flow links discarded. Run ''remove small flowlinks'' to remove small flow links or increase threshold', ' ', ' ')
       end if
       NDRAW(2) = 5 !< Automatically set 'Display > Network + crossing/quality checks'
    end if
    if (lnxi == 0 .and. numbnp == 0) return

    lnx = lnxi + numbnp ! add open boundary points

    call readyy('geominit-NODELINKS         ', 0.5d0)

    if (allocated(ln)) deallocate (ln, lncn, bob, bob0, dx, dxi, wu, wui, kcu, csu, snu, acl, iadv, teta, wu_mor, wu1D2D, hh1D2D)
    if (allocated(ibot)) deallocate (ibot)
    allocate (ln(2, lnx), stat=ierr)
    call aerr('ln   (2,lnx)', ierr, 2 * lnx)
    allocate (lncn(2, lnx), stat=ierr)
    call aerr('lncn (2,lnx)', ierr, 2 * lnx)
    allocate (bob(2, lnx), stat=ierr)
    call aerr('bob  (2,lnx)', ierr, 2 * lnx)
    bob = 0d0
    allocate (bob0(2, lnx), stat=ierr)
    call aerr('bob0  (2,lnx)', ierr, 2 * lnx)
    bob0 = 0d0
    allocate (dx(lnx), stat=ierr)
    call aerr('dx   (  lnx)', ierr, lnx)
    allocate (dxi(lnx), stat=ierr)
    call aerr('dxi  (  lnx)', ierr, lnx)
    allocate (wu(lnx), stat=ierr)
    call aerr('wu   (  lnx)', ierr, lnx)
    allocate (wu_mor(lnx), stat=ierr)
    call aerr('wu_mor (  lnx)', ierr, lnx)
    allocate (wui(lnx), stat=ierr)
    call aerr('wui  (  lnx)', ierr, lnx)
    allocate (wu1D2D(lnx1D), stat=ierr)
    call aerr('wu1D2D(lnx1D)', ierr, lnx1D)
    allocate (hh1D2D(lnx1D), stat=ierr)
    call aerr('hh1D2D(lnx1D)', ierr, lnx1D)
    allocate (kcu(lnx), stat=ierr)
    call aerr('kcu  (  lnx)', ierr, lnx)
    kcu = 0
    allocate (csu(lnx), stat=ierr)
    call aerr('csu  (  lnx)', ierr, lnx)
    allocate (snu(lnx), stat=ierr)
    call aerr('snu  (  lnx)', ierr, lnx)
    allocate (acl(lnx), stat=ierr)
    call aerr('acl  (  lnx)', ierr, lnx)
    allocate (acn(2, lnx), stat=ierr) ! will be deallocated after cornerweights
    call aerr('acn  (2,lnx)', ierr, lnx)
    allocate (iadv(lnx), stat=ierr)
    call aerr('iadv   (lnx)', ierr, lnx)
    iadv = 0
    allocate (teta(lnx), stat=ierr)
    call aerr('teta   (lnx)', ierr, lnx)
    teta = 0
    allocate (ibot(lnx), stat=ierr)
    call aerr('ibot   (lnx)', ierr, lnx)
    ibot = 0

    call realloc(ln0, [2, lnx])
    call realloc(onlyWetLinks, lnx, keepExisting=.false., fill=0)

    if (allocated(xu)) deallocate (xu, yu, blu)
    allocate (xu(lnx), yu(lnx), blu(lnx), stat=ierr)
    call aerr('xu(lnx), yu(lnx) , blu(lnx)', ierr, 3 * lnx)
    blu = dmiss
    if (jafullgridoutput > 0) then
       call realloc(blup, lnx, keepExisting=.false., fill=dmiss, stat=ierr)
       call aerr('blup(lnx)', ierr, lnx)
    end if

    if (allocated(ln2lne)) deallocate (ln2lne, lne2ln)
    nex = max(lnx, numl)
    allocate (ln2lne(nex), stat=ierr) ! local array
    call aerr('ln2lne (nex)', ierr, nex)
    ln2lne = 0
    allocate (lne2ln(nex), stat=ierr) ! local array
    call aerr('lne2ln (nex)', ierr, nex)
    lne2ln = 0

    call readyy('geominit', 0.86d0)
    Lf = 0

    do L = 1, numl ! again count nr of edges and fill in links
       n1 = lne(1, L); n2 = lne(2, L)
       n1a = abs(n1); n2a = abs(n2)
!    if (n1 .ne. 0 .and. n2 .ne. 0) then              ! L=net, Lf=flow
       if (n1 /= 0 .and. n2 /= 0 .and. KN(3, L) /= 0) then ! L=net, Lf=flow
          Lf = Lf + 1
          ln(1, LF) = n1a
          ln(2, LF) = n2a
          ln2lne(LF) = L
          lne2ln(L) = LF
          if (kn(3, L) == 1 .or. kn(3, L) == 6) then ! 1D link
             kcu(Lf) = 1
          else if (kn(3, L) == 4) then
             k1 = kn(1, L); k2 = kn(2, L)
             jaend = 0
             if (nmk(k1) == 1 .or. nmk(k2) == 1) then
                jaend = 1
             end if
             if (jaend == 1 .and. n1a > ndx2d .and. n2a <= ndx2d .or. &
                 jaend == 1 .and. n2a > ndx2d .and. n1a <= ndx2d) then
                kcu(Lf) = 4 ! 1D2D longitudinal link
                nc2 = n2a
                if (n1a <= ndx2d) then
                   nc2 = n1a
                end if
                call WHICH2DNETLINKWASCROSSED(nc2, k1, k2, LL) ! TEMP STORE CROSSED 2d NETLINK IN LC
                if (LL > 0) then
                   if (lnn(LL) == 2) then
                      call dumpnetlink('netlink kn3==4 not on border of 2D net ', LL)
                   end if
                   ln2lne(LF) = LL ! refer back to crossed 2D netlink instead of to 1D netlink
                else
                   write (msgbuf, '(a,i0)') 'Could not find crossing with 2D cell boundary for 1D2D link ', L
                   call mess(LEVEL_WARN, trim(msgbuf))
                   noncrossinglink = .true.
                end if
             else ! 1D link
                kcu(Lf) = 1
             end if
          else if (kn(3, L) == 3 .or. kn(3, L) == 7) then
             if (n1a > ndx2d .and. n2a <= ndx2d .or. &
                 n2a > ndx2d .and. n1a <= ndx2d) then
                kcu(Lf) = kn(3, L) ! 1D2D internal link
                if (n1a <= ndx2d) then
                   kcs(n1a) = 21
                end if
                if (n2a <= ndx2d) then
                   kcs(n2a) = 21
                end if
                if (kcs(n2a) * kcs(n1a) /= 21) then
                   write (msgbuf, '(a,i0,a)') '(netlink L=', L, ')'
                   call qnerror('1d2d link kn3 = 3 or 5 or 7 not connected between kcs=21 and kcs=1 ', trim(msgbuf), ' ')
                end if
             else
                write (msgbuf, '(a,i0,a)') '(netlink L=', L, ')'
                call qnerror('1d2d link kn3 = 3 or 7 not connected between 1D node and 2D cell ', trim(msgbuf), ' ')
             end if
          else if (kn(3, L) == 5) then
             if (n1a > ndx2d .and. n2a <= ndx2d .or. &
                 n2a > ndx2d .and. n1a <= ndx2d .or. &
                 n2a <= ndx2d .and. n1a <= ndx2d) then
                kcu(Lf) = kn(3, L) ! 1D2D internal link, now also between 2 2D pts
                if (n1a <= ndx2d) then
                   kcs(n1a) = 21
                end if
                if (n2a <= ndx2d) then
                   kcs(n2a) = 21
                end if
             else
                write (msgbuf, '(a,i0,a)') '(netlink L=', L, ')'
                call qnerror('1d2d link kn3 = 5 not connected to 2D cell ', trim(msgbuf), ' ')
             end if
          else if (kn(3, L) == 2) then ! 2D link
             kcu(Lf) = 2
          end if
       else if (n1 == 0) then ! if negative, refer back to attached node
          lne2ln(L) = -n2
       else if (n2 == 0) then
          lne2ln(L) = -n1
       end if
    end do

    if (noncrossinglink) then
       call mess(LEVEL_ERROR, 'One or more 1D2D links found that do not cross a 2D cell face.')
    end if

    call addexternalboundarypoints() ! add links due to open boundaries

    ln0 = ln

    numswap = 0
    do L = 1, lnx ! for all 2d links, check positivity

       k1 = ln(1, L) ! o---4---o  1,2: flow nodes, 3,4: net nodes
       k2 = ln(2, L) ! | 1 | 2 |  L: 1--2  Ln=ln2lne(L): 3--4
       k3 = kn(1, ln2lne(L)) ! o---3---o  lncn(:,L) = 3--4, or 4--3 if
       k4 = kn(2, ln2lne(L)) !            ||3--4 X 1--2|| < 0,
       ! i.e., flux is 'to the right' through net link 3--4.

       ja1D = 0
       if (kcu(L) == 1 .or. kcu(L) == -1 .or. kcu(L) == 4) then
          ja1D = 1
       end if

       if (ja1D == 1) then
          call half(xz(k1), yz(k1), xz(k2), yz(k2), xu(L), yu(L), jsferic, jasfer3D)
       else
          call half(xk(k3), yk(k3), xk(k4), yk(k4), xu(L), yu(L), jsferic, jasfer3D)
       end if

       if (abs(kcu(L)) == 2) then
          call normalin(xz(k1), yz(k1), xz(k2), yz(k2), rnl, rtl, xu(L), yu(L), jsferic, jasfer3D, dxymis) ! in pos L direction
          call normalin(xk(k3), yk(k3), xk(k4), yk(k4), rn, rt, xu(L), yu(L), jsferic, jasfer3D, dxymis) ! edge

          if (rnl * rt - rtl * rn < 0) then ! checking/ensuring positive local axis orientation
             kh = k4
             k4 = k3
             k3 = kh
             numswap = numswap + 1
          end if ! indeed, now this may occur

          lncn(1, L) = k3 ! used in eddy visc terms
          lncn(2, L) = k4
       else if (kcu(L) == 1) then ! keep natural reference
          lncn(1, L) = k3
          lncn(2, L) = k4
       else if (kcu(L) == -1) then ! refer twice to last netnode
          if (nmk(k3) == 1) then
             lncn(1, L) = k3
             lncn(2, L) = k3
          else if (nmk(k4) == 1) then
             lncn(1, L) = k4
             lncn(2, L) = k4
          end if

       else if (kcu(L) == 3 .or. kcu(L) == 4 .or. kcu(L) == 5 .or. kcu(L) == 7) then ! 1D2D, inherit 2D keep natural reference

          lncn(1, L) = k3
          lncn(2, L) = k4

       end if
    end do

    call readyy('geominit', 0.88d0)

    do L = 1, lnx ! for all links, count nr of links attached to a node
       k1 = ln(1, L)
       nd(k1)%lnx = nd(k1)%lnx + 1
       k2 = ln(2, L)
       nd(k2)%lnx = nd(k2)%lnx + 1
    end do

    call readyy('geominit', 0.90d0)

    do k = 1, ndx ! for all nodes, allocate linknrs
! GD: memory leak
!    if(allocated(nd(k)%ln)) deallocate(nd(k)%ln)

       allocate (nd(k)%ln(nd(k)%lnx), stat=ierr)
       call aerr('nd(k)%ln ( nd(k)%lnx', ierr, nd(k)%lnx)
       nd(k)%ln = 0 ! set to zero for recount
       nd(k)%lnx = 0
    end do

    do L = 1, lnx ! for all links, recount nr of links attached to a node
       k1 = ln(1, L)
       nd(k1)%lnx = nd(k1)%lnx + 1
       nd(k1)%ln(nd(k1)%lnx) = -L ! outflowing, negative indexnr
       k2 = ln(2, L)
       nd(k2)%lnx = nd(k2)%lnx + 1
       nd(k2)%ln(nd(k2)%lnx) = L ! inflowing, positive indexnr
    end do

! sort flowlinks
    call sort_flowlinks_ccw()

! start of second phase
9002 continue

    call readyy('geominit-METRICS               ', 0.92d0)

    if (allocated(cn)) deallocate (cn, ucnx, ucny, ban) ! vort

    allocate (cn(numk), stat=ierr) ! some cell corner related stuff
    call aerr(' cn (numk)', ierr, numk)
    allocate (ucnx(numk), stat=ierr)
    call aerr('ucnx(numk)', ierr, numk)
    allocate (ucny(numk), stat=ierr)
    call aerr('ucny(numk)', ierr, numk)
    allocate (ban(numk), stat=ierr) ! for keeps, netnode area
    call aerr('ban (numk)', ierr, numk)

    cn(1:numk)%lnx = 0
    cn(1:numk)%nwx = 0
    ucnx(1:numk) = 0
    ucny(1:numk) = 0
    ban(1:numk) = 0

    do L = 1, lnx ! for all links, set dx and coordinates
       k1 = ln(1, L)
       k2 = ln(2, L)
       k3 = lncn(1, L)
       k4 = lncn(2, L)

       ja1D = 0
       if (kcu(L) == 1 .or. kcu(L) == -1 .or. kcu(L) == 4) then
          ja1D = 1
       end if

       if (ja1D == 1) then
          call half(xz(k1), yz(k1), xz(k2), yz(k2), xu(L), yu(L), jsferic, jasfer3D)
       else
          call half(xk(k3), yk(k3), xk(k4), yk(k4), xu(L), yu(L), jsferic, jasfer3D)
       end if

       dx(L) = dbdistance(xz(k1), yz(k1), xz(k2), yz(k2), jsferic, jasfer3D, dmiss) ! set link length
       ! Optionally, override dx(L) with netlink length as read from file (typically 1D with user-defined branch lengths).
       if (allocated(dxe) .and. ja1D == 1) then
          LL = ln2lne(L)
          if (dxe(LL) /= dmiss) then
             dx(L) = dxe(LL) ! NOTE: also see izbndpos correction after this loop
          end if
       end if

       if (kcu(L) == 4) then ! 1D2D lateral link, normal to 2D netlink
          call normalout(xk(k3), yk(k3), xk(k4), yk(k4), xn, yn, jsferic, jasfer3D, dmiss, dxymis)
          call normalin(xz(k1), yz(k1), xz(k2), yz(k2), xt, yt, xu(L), yu(L), jsferic, jasfer3D, dxymis)
          if (xn * xt + yn * yt < 0d0) then
             lncn(1, L) = k4
             lncn(2, L) = k3
          end if
          dx(L) = dx(L) * abs(xn * xt + yn * yt)
       else if (kcu(L) == 3 .or. kcu(L) == 5 .or. kcu(L) == 7) then ! 1D2D internal link, some averaged 2D length
          k = 0
          if (kcs(k1) == 21) k = k1
          if (kcs(k2) == 21) k = k2
          if (k == 0) then
             write (msgbuf, '(a,i0,a)') '(netlink L=', ln2lne(L), ')'
             call qnerror('1d2d link kcu=3 or 5 not connected to kcs=21 ', trim(msgbuf), ' ')
          else
             dx(L) = max(dx(L), 0.5d0 * sqrt(ba(k)))
          end if
          if (kcu(L) == 3 .and. fixedweirtopwidth > 0d0) then
             weirheight = fixedweirtopwidth ! we don't have bl nor bobs yet !max(0d0, 0.5d0*(bob(1,L) + bob(2,L)) - 0.5d0*(bl(k1) + bl(k2)) )
             weirlength = fixedweirtopwidth
             dx(L) = min(dx(L), max(weirlength + 2d0 * weirheight * fixedweirtalud, 0.5d0 * sqrt(ba(k))))
          end if
       end if

!   for partition_init: compute temporary csu, snu (will be overwritten in phase 2), based on xzw, yzw (instead of xz, yz)
       if (kcu(L) /= 4) then
          call normalin(xzw(k1), yzw(k1), xzw(k2), yzw(k2), rn, rt, xu(L), yu(L), jsferic, jasfer3D, dxymis) ! = normalin (k1,k2)
       else
          call normalout(xk(k3), yk(k3), xk(k4), yk(k4), rn, rt, jsferic, jasfer3D, dmiss, dxymis) ! 1D2D
       end if

       csu(L) = rn; snu(L) = rt
    end do

    if (allocated(dxe) .and. izbndpos /= 0) then
       ! Optionally fix dx for waterlevel type boundaries affected by izbndpos
       do k = 1, nbndz
          L = kez(k)
          Lf = lne2ln(L)
          if (dxe(L) /= dmiss .and. L <= numl1d) then ! 1D Boundary
             if (izbndpos == 0) then ! full grid cell outward
                dx(Lf) = dxe(L)
             else if (izbndpos == 1) then ! half a grid cell outward
                dx(Lf) = .5d0 * dxe(L)
             else ! izbndpos==2                           ! on specified boundary polyline
                continue ! nowhere supported yet
             end if
          end if
       end do
    end if

    ! end of first phase
    if (iphase == 1) then
       return
    end if

    call timstrt('Set bedlevel from ext-file', handle_extra(48)) ! setbedlevelfromextfile
    call setbedlevelfromextfile() ! set bl bathymetry if specified through file, so ibedlevtype must be 1
    call timstop(handle_extra(48)) ! setbedlevelfromextfile

    ! Default parameters for 1D2D links
    do L = 1, lnx1D
       if (kcu(L) == 5) then
          wu1D2D(L) = wu1Duni5
          hh1D2D(L) = hh1Duni5
       else if (kcu(L) == 7) then
          wu1D2D(L) = wu1Duni7
          hh1D2D(L) = hh1Duni7
       else
          wu1D2D(L) = wu1Duni
          hh1D2D(L) = hh1Duni
       end if
    end do

    ! Custom parameters for 1D2D links
    if (len_trim(md_1d2dlinkfile) > 0) then
       call load1D2DLinkFile(md_1d2dlinkfile)
    end if

    call set_1d_indices_in_network()

    if (allocated(prof1D)) deallocate (prof1D)
    allocate (prof1D(3, lnx1D), stat=ierr)
    call aerr('prof1D(3,lnx1D)', ierr, 2 * lnx1D)
    do L = 1, lnx1D
       prof1D(1, L) = wu1D2D(L) !  prof1d(1,*) > 0 : width   or prof1d(1,*) < 0 : ka ref
       prof1D(2, L) = hh1D2D(L) !  prof1d(2,*) > 0 : height  or prof1d(2,*) < 0 : kb ref
       if (kcu(L) == 5) then !  restricting dimensions of streetinlet
          prof1D(3, L) = iproftypuni5 !  prof1d(3,*) > 0 : ityp    or prof1d(3,*) < 0 : alfa tussen a en b .
       else if (kcu(L) == 7) then !  restricting dimensions of roofgutterpipe
          prof1D(3, L) = iproftypuni7 !  prof1d(3,*) > 0 : ityp    or prof1d(3,*) < 0 : alfa tussen a en b .
       else
          prof1D(3, L) = iproftypuni !  prof1d(3,*) > 0 : ityp    or prof1d(3,*) < 0 : alfa tussen a en b .
       end if
    end do

    if (allocated(Lbnd1D)) deallocate (Lbnd1D)
    allocate (Lbnd1D(lnxi + 1:lnx), stat=ierr); Lbnd1D = 0
    call aerr('Lbnd1D(lnxi+1:lnx)', ierr, lnx - lnxi + 1)

    if (allocated(grounlay)) deallocate (grounLay)
    if (lnx1D > 0) then
       allocate (grounLay(lnx1D), stat=ierr); grounLay = dmiss
       call aerr('grounLay(lnx1D)', ierr, Lnx1D)
    end if

    teta = abs(teta0) ! set spatially constant teta. Override only in setdt for ivariableteta = 2
    if (teta0 == 1d0) then
       ivariableteta = 0 ! fully implicit
       teta = 1d0
    else if (teta0 < 0) then
       ivariableteta = 2 ! variable teta
    else
       ivariableteta = 1 ! constant teta
       teta = teta0
    end if
    call setprofs1D() ! get prof1D by reading and interpolating profiles

    do L = 1, lnx ! for all links, set link width
       k1 = ln(1, L)
       k2 = ln(2, L)
       k3 = lncn(1, L)
       k4 = lncn(2, L)

       if (kcu(L) == 1 .or. kcu(L) == -1 .or. kcu(L) == 3 .or. kcu(L) == 4 .or. kcu(L) == 5 .or. kcu(L) == 7) then
          LL = L
          if (kcu(L) == -1) then ! 1D boundary link, find attached regular link
             if (abs(nd(k2)%ln(1)) == L) then
                LBND1D(L) = abs(nd(k2)%ln(2))
             end if
             if (abs(nd(k2)%ln(2)) == L) then
                LBND1D(L) = abs(nd(k2)%ln(1)) ! and store in LBND1D
             end if
             LL = LBND1D(L) ! LL refers to prof1D
          end if
          if (kcu(L) == 4) then ! 1D2D lateral link inherits 2D
             wu(L) = dbdistance(xk(k3), yk(k3), xk(k4), yk(k4), jsferic, jasfer3D, dmiss) ! set 2D link width
          else if (kcu(L) == 3) then ! 1D2D internal link 3  flows over side of attached 1D channel
             call getdxofconnectedkcu1(L, wu(L)) !  dbdistance ( xk(k3), yk(k3), xk(k4), yk(k4) )  ! set 2D link width
          else
             if (prof1D(1, LL) >= 0) then
                wu(L) = prof1d(1, LL) ! todo, wu1DUNI from max width of profile interpolations
             else
                KA = -PROF1D(1, LL); KB = -PROF1D(2, LL); ALFA = PROF1D(3, LL)
                WU(L) = (1d0 - ALFA) * PROFILES1D(KA)%WIDTH + ALFA * PROFILES1D(KB)%WIDTH
             end if
          end if
          hdx = 0.5d0 * dx(L)
          if (kcu(L) /= 3) then
             ! TODO: UNST-6592: consider excluding ghost links here and do an mpi_allreduce sum later
             if (k1 > ndx2d) ba(k1) = ba(k1) + hdx * wu(L) ! todo, on 1d2d nodes, choose appropriate wu1DUNI = min ( wu1DUNI, intersected 2D face)
             if (k2 > ndx2d) ba(k2) = ba(k2) + hdx * wu(L)
          end if
       else
          wu(L) = dbdistance(xk(k3), yk(k3), xk(k4), yk(k4), jsferic, jasfer3D, dmiss) ! set 2D link width
       end if
    end do

    if (jampi > 0) then
       ! WU of orphan 1D2D links must come from neighbouring partition.
       call update_ghosts(ITYPE_U, 1, lnx, wu, ierror, ignore_orientation=.true.)
    end if

    do L = lnxi + 1, Lnx
       k1 = ln(1, L); k2 = ln(2, L)
       ba(k1) = ba(k2) ! set bnd ba to that of inside point
    end do

    k = 0 ! count MAX nr of 1D endpoints, dir zijn dead ends
    do L = 1, lnx
       if (kcu(L) == 1) then
          k1 = ln(1, L); k2 = ln(2, L)
          if (nd(k1)%lnx == 1) then
             k = k + 1
          end if
          if (nd(k2)%lnx == 1) then
             k = k + 1
          end if
       end if
    end do
    mx1Dend = k

    if (allocated(n1Dend)) deallocate (n1Dend)
    allocate (n1Dend(mx1Dend), stat=ierr); n1Dend = 0
    call aerr('n1Dend(mx1Dend)', ierr, mx1Dend)

    k = 0
    do L = 1, lnx
       if (kcu(L) == 1) then
          k1 = ln(1, L); k2 = ln(2, L)
          if (nd(k1)%lnx == 1) then
             k = k + 1
             n1Dend(k) = k1
          end if
          if (nd(k2)%lnx == 1) then
             k = k + 1
             n1Dend(k) = k2
          end if
       end if
    end do
    mx1Dend = k

    do k = 1, mx1Dend
       k1 = n1Dend(k)
       ba(k1) = 2d0 * ba(k1)
    end do

    ! fraction of dist(nd1->edge) to link lenght dx
    call readyy('geominit', 0.94d0)

    acl = 0.5d0; acn = 0.5d0 ! for pipes
    do L = 1, lnx ! for all links,
       k1 = ln(1, L)
       k2 = ln(2, L)
       k3 = lncn(1, L)
       k4 = lncn(2, L)

       if (abs(kcu(L)) == 2 .or. abs(kcu(L)) == 4) then ! override for 2D

          !   x34   = 0.5d0*( xk(k3) + xk(k4) )
          !   y34   = 0.5d0*( yk(k3) + yk(k4) )
          !   dxn1e = dbdistance(xz(k1),yz(k1),x34,y34)        !
          !   dxn2e = dbdistance(xz(k2),yz(k2),x34,y34)        !

          call DLINEDIS2(xz(k1), yz(k1), xk(k3), yk(k3), xk(k4), yk(k4), JA, dxn1e, XN, YN, RL)
          call DLINEDIS2(xz(k2), yz(k2), xk(k3), yk(k3), xk(k4), yk(k4), JA, dxn2e, XN, YN, RL)

          if (abs(dxn1e + dxn2e) < 1d-15) then
             dxn1e = 5d-16
             dxn2e = 5d-16
          end if

          acl(L) = dxn1e / (dxn1e + dxn2e) ! weight factor of nd1

!       ! SPvdP: the following may cause problems for periodic sperical coordinates
!       x12   = 0.5d0*( xz(k1) + xz(k2) )
!       y12   = 0.5d0*( yz(k1) + yz(k2) )
          call half(xz(k1), yz(k1), xz(k2), yz(k2), x12, y12, jsferic, jasfer3D)
          dxn1e = dbdistance(xk(k3), yk(k3), x12, y12, jsferic, jasfer3D, dmiss) !
          dxn2e = dbdistance(xk(k4), yk(k4), x12, y12, jsferic, jasfer3D, dmiss) !

          acn(1, L) = dxn1e / (dxn1e + dxn2e) ! weight factor of nd1
          acn(2, L) = dxn2e / (dxn1e + dxn2e) ! weight factor of nd2, sum = 1d0 !
       end if

       if (kcu(L) /= 4 .and. (iNormalMethod == 0 .or. L <= Lnx1D)) then
          call normalin(xz(k1), yz(k1), xz(k2), yz(k2), rn, rt, xu(L), yu(L), jsferic, jasfer3D, dxymis) ! = normalin (k1,k2)
       else
          call normalout(xk(k3), yk(k3), xk(k4), yk(k4), rn, rt, jsferic, jasfer3D, dmiss, dxymis) ! 1D2D
       end if

       csu(L) = rn; snu(L) = rt !
    end do

    do L = 1, lnx
       ! the max func after setting dx1 fraction
       dxi(L) = 1d0 / dx(L) ! dxi to minimise nr. of divisions
       if (wu(L) > 0) then
          wui(L) = 1d0 / wu(L)
       else
          write (msgbuf, '(a,i0,a)') 'flow_geominit(): wu(', L, ') = 0'
          call qnerror(trim(msgbuf), ' ', ' ')
       end if
    end do

    do n = 1, ndx
       if (ba(n) > 0d0) then
          bai(n) = 1d0 / ba(n) ! initially, ba based on 'max wet envelopes', take bai used in linktocentreweights
       end if
    end do

    ! call message ('cutcell call 4',' ',' ')
    if (allocated(kfs)) deallocate (kfs)
    fnam = '*.cut'
    n12 = 4
    allocate (kfs(ndx)); kfs = 0
    call cutcell_list(n12, 2) ! CUT CELLS, N12 = 4, flag cells to be cut in kfs, prior to setlinktocenter/CORNERweights calls below

    call setcentertolinkorientations()

    ! call setlinktocenterweights()

    call setcornertolinkorientations()

    call setlinktocornerweights()

    do n = ndx2D + 1, ndxi
       call allocateandset1Dnodexyarrays(n) ! na  csu en snu
    end do

    call readyy('geominit', 0.98d0)

    call iadvecini() ! set desired advection for non (-1) links

    avortho = 0d0
    do L = lnx1D + 1, lnx ! for all links, check link orthogonality
       if (abs(kcu(L)) == 1) cycle
       k1 = ln(1, L)
       k2 = ln(2, L)
       k3 = lncn(1, L)
       k4 = lncn(2, L)
       call normalin(xz(k1), yz(k1), xz(k2), yz(k2), rnl, rtl, xu(L), yu(L), jsferic, jasfer3D, dxymis) ! in pos LL direction
       call normalin(xk(k3), yk(k3), xk(k4), yk(k4), rn, rt, xu(L), yu(L), jsferic, jasfer3D, dxymis) ! = normalin (k1,k2)
       ortho = rnl * rn + rtl * rt
       avortho = avortho + ortho
    end do
    avortho = avortho / lnx

    n12 = 5; fnam = '*.cut'
    ! call message ('cutcell call 5',' ',' ')

    if (allocated(numlimdt)) deallocate (numlimdt)
    allocate (numlimdt(ndx), stat=ierr); numlimdt = 0
    call aerr('numlimdt(ndx)', ierr, ndx)
    if (numlimdt_baorg > 0) then ! if prev_numlimdt(k) > numlimdt_baorg then ba(k) = baorg(k) in cutcell
       call reanumlimdt()
    end if
    call cutcell_list(n12, 3)  ! CUT CELLS, N12 = 5, WU AND BA ADAPTATION
    numlimdt = 0
! deallocate(kfs) ; allocate(kfs(ndx)) ! SPvdP: removed, since (1) uninitialized and (2) kfs needed in "setlinktocenterweights" later

    call readyy('geominit', -1d0)

    if (isimplefixedweirs == 0) call fixedweirs_on_flowgeom() ! Impose fixed weirs paths on all crossed flow links.

    jaupdbndbl = 1
    if (network%loaded) then
       jaupdbobbl1d = 1
    else
       jaupdbobbl1d = 0
    end if

    if (jampi > 0) then
       ! MPI communication of nonLin, nonLin1D and nonLin2D
       call realloc(inonLin, 3, keepExisting=.false., fill=0)
       inonLin(1) = nonLin1D
       inonLin(2) = nonLin2D
       inonLin(3) = nonLin
       call reduce_int_max(3, inonLin)
       nonLin1D = inonLin(1)
       nonLin2D = inonLin(2)
       nonLin = inonLin(3)
    end if

    if (japure1D == 1 .or. japure1D == 2) then
       call setisnbnodisnblin() ! set signarray isnbnod for left and rightneighbouring uc1d.
    end if

    if (network%loaded .and. ndxi - ndx2d > 0 .and. (jamapTimeWetOnGround > 0 .or. jamapFreeboard > 0 .or. jamapDepthOnGround > 0 .or. jamapVolOnGround > 0)) then
       call set_ground_level_for_1d_nodes(network) ! set ground level for 1d nodes
    end if

    call setbobs()
    call setbobsonroofs()

    if (jawindpartialdry == 1) then
       if (ibedlevtyp /= 3) then
          jawindpartialdry = 0
       end if
    end if

    !-------------------------------------------------- CLOSED WALL (STRESS) RELATED -----------------------------------------------

    ! add some closed boundary administration for stress terms

    nw = 0
    do L = lnx1D + 1, numl ! first count nr of closed walls
       if (lne2ln(L) < 0) then
          nw = nw + 1
       end if
    end do

    if (allocated(walls)) deallocate (walls)
    allocate (walls(17, nw), stat=ierr); walls = 0
    call aerr('walls(17,nw)', ierr, nw * 17)

    nw = 0 ! number of closed walls
    do L = lnx1D + 1, numl
       if (lne2ln(L) < 0) then
          nw = nw + 1
          k1 = abs(lne2ln(L))
          k3 = kn(1, L)
          k4 = kn(2, L)

          walls(1, nw) = k1 ! waterlevel point on the inside
          walls(2, nw) = k3 ! first wall corner
          walls(3, nw) = k4 ! second wall corner

          if (iPerot == -1) then
             nwx = nd(k1)%nwx
             if (nd(k1)%nwx == 0) then
                allocate (nd(k1)%nw(1))
                nd(k1)%nw(1) = nw
                nd(k1)%nwx = 1
             else
                allocate (nw_temp(nwx))
                nw_temp = nd(k1)%nw
                deallocate (nd(k1)%nw)
                allocate (nd(k1)%nw(nwx + 1))
                nd(k1)%nw(1:nwx) = nw_temp(1:nwx)
                nd(k1)%nw(nwx + 1) = nw
                nd(k1)%nwx = nd(k1)%nwx + 1
                deallocate (nw_temp)
             end if
          end if

          call duitpl(xzw(k1), yzw(k1), xk(k3), yk(k3), xzw(k1), yzw(k1), xk(k4), yk(k4), sig, jsferic)
          call dlinedis(xzw(k1), yzw(k1), xk(k3), yk(k3), xk(k4), yk(k4), JA, DIS, XN, YN, jsferic, jasfer3D, dmiss)
!
!       dxx = getdx( xk(k3), yk(k3), xk(k4), yk(k4) )  ! xk(k4) - xk(k3)
!       dyy = getdy( xk(k3), yk(k3), xk(k4), yk(k4) )  ! yk(k4) - yk(k3)
!       rrr = sqrt(dxx*dxx + dyy*dyy)
!       cs  = 0 ; sn = 0
!       if (rrr .ne. 0) then
!          cs = sig*dxx/rrr
!          sn = sig*dyy/rrr
!       endif

          rrr = dbdistance(xk(k3), yk(k3), xk(k4), yk(k4), jsferic, jasfer3D, dmiss)
          call half(xk(k3), yk(k3), xk(4), yk(k4), xh, yh, jsferic, jasfer3D)
          call normalin(xk(k3), yk(k3), xk(k4), yk(k4), cs, sn, xh, yh, jsferic, jasfer3D, dxymis)

          if (wall_z0 > 0) then
             sf = vonkar / log(c9of1 + dis / wall_z0)
             ! us  = utangential, un = unormal
             ! us  = ucx*cs + ucy*sn, ustar = sf*us
             walls(6, nw) = sf ! sux = -cs*ustar
          end if

          walls(7, nw) = cs ! suy = -sn*ustar    ( )
          walls(8, nw) = sn ! sinus              ( )
          walls(9, nw) = rrr ! length of the wall (m)

          L1 = 0; L2 = 0
          do LL = 1, nd(k1)%lnx
             LLL = nd(k1)%ln(LL); LLA = abs(LLL)
             if (lncn(1, LLA) == k3 .or. lncn(2, LLA) == k3) then
                L1 = LLA
                walls(4, nw) = L1 ! link 1 to which this wall contributes
                if (LLL < 0) then ! outflowing link: use alfa1
                   walls(10, nw) = acl(L1)
                else
                   walls(10, nw) = (1d0 - acl(L1))
                end if
             end if
             if (lncn(1, LLA) == k4 .or. lncn(2, LLA) == k4) then
                L2 = LLA
                walls(5, nw) = L2 ! link 2 to which this wall contributes
                if (LLL < 0) then
                   walls(11, nw) = acl(L2)
                else
                   walls(11, nw) = (1d0 - acl(L2))
                end if
             end if
          end do

          walls(12, nw) = 0.5d0 * dis ! half of distance circumcentre to the wall (m)
          walls(13, nw) = bl(k1) ! cell bottom level (m)
          walls(14, nw) = abs(zk(k4) - zk(k3)) ! bottom level difference (m)
          walls(15, nw) = walls(14, nw) / walls(9, nw) ! bottom level inclination()
          walls(17, nw) = walls(12, nw) * walls(9, nw) ! ba part of wall
       end if
    end do
    mxwalls = nw

    call setwallorientations()
    call setlinktocenterweights()

!-------------------------------------------------- CELL CORNER RELATED -----------------------------------------------

    do L = lnx1D + 1, lnx ! for all links,
       k3 = lncn(1, L)
       k4 = lncn(2, L)
       cn(k3)%lnx = cn(k3)%lnx + 1 ! count nr of links attached to a cell corner
       cn(k4)%lnx = cn(k4)%lnx + 1
    end do

    do k = 1, numk
       m = cn(k)%lnx
       if (m > 0) then
! GD: memory leak
!       if(allocated(cn(k)%ln)) deallocate(cn(k)%ln)
          allocate (cn(k)%ln(m), stat=ierr) ! allocate nr of links attached to a cell corner
          call aerr('cn(k)%ln (m)', ierr, m)
          cn(k)%lnx = 0
       end if
    end do

    do L = lnx1D + 1, lnx ! for all links,
       k3 = lncn(1, L)
       k4 = lncn(2, L)

       cn(k3)%lnx = cn(k3)%lnx + 1
       m = cn(k3)%lnx
       cn(k3)%ln(m) = L ! set attached linknrs

       cn(k4)%lnx = cn(k4)%lnx + 1
       m = cn(k4)%lnx
       cn(k4)%ln(m) = -L ! set attached linknrs
    end do

    do nw = 1, mxwalls ! for all closed walls
       k3 = walls(2, nw) ! first wall corner
       k4 = walls(3, nw) ! first wall corner
       cn(k3)%nwx = cn(k3)%nwx + 1 ! count nr of walls attached to a cell corner
       cn(k4)%nwx = cn(k4)%nwx + 1
    end do

    do k = 1, numk
       m = cn(k)%nwx
       if (m > 0) then
          allocate (cn(k)%nw(m), stat=ierr) ! allocate nr of links attached to a cell corner
          call aerr('cn(k)%nw (m)', ierr, m)
          cn(k)%nwx = 0
       end if
    end do

    do nw = 1, mxwalls ! for all closed walls
       k3 = walls(2, nw) ! first wall corner
       k4 = walls(3, nw) ! first wall corner

       cn(k3)%nwx = cn(k3)%nwx + 1 ! count nr of walls attached to a cell corner
       m = cn(k3)%nwx
       cn(k3)%nw(m) = nw ! set attached wallnrs

       cn(k4)%nwx = cn(k4)%nwx + 1 ! count nr of walls attached to a cell corner
       m = cn(k4)%nwx
       cn(k4)%nw(m) = -nw ! set attached wallnrs
    end do

    nwalcnw = 0
    do icn = 1, nrcnw ! attach closed walls to closed corners
       k = kcnw(icn)
       if (cn(k)%nwx == 0) then
          m = icn
       else
          if (cn(k)%nwx > 0) then
             nwalcnw(1, icn) = cn(k)%nw(1)
          end if
          if (cn(k)%nwx > 1) then
             nwalcnw(2, icn) = cn(k)%nw(2)
          end if
       end if
    end do

    do icn = 1, nrcnw
       sf = 0d0; n = 0
       if (abs(nwalcnw(1, icn)) > 0) then
          sf = walls(6, abs(nwalcnw(1, icn))); n = n + 1
       end if
       if (abs(nwalcnw(2, icn)) > 0) then
          sf = walls(6, abs(nwalcnw(2, icn))) + sf; n = n + 1
       end if
       if (sf > 0d0) then
          sfcnw(icn) = sf / dble(n) ! averaged
       end if
    end do

    dx = max(dx, dxmin)
    dxi = 1d0 / dx

    if (allocated(jaduiktmp)) then
       do L = 1, Lnx1D
          if (jaduiktmp(L) /= 1) then
             dx(L) = max(dx(L), dxmin1D)
          end if
       end do
       deallocate (jaduiktmp)
    else
       do L = 1, Lnx1D
          dx(L) = max(dx(L), dxmin1D)
       end do
    end if

    do L = Lnx1D + 1, Lnx
       dx(L) = max(dx(L), dxmin2D)
    end do

    if (Lnx1D < -1) then
       kc = 0 ! allocate(inodes(Ndxi-ndx2D)) ; inodes = 0
       call find_nearest_flownodes_kdtree(treeglob, Ndxi - ndx2D, xz(ndx2D + 1), yz(ndx2D + 1), kc, 0, INDTP_2D, ierr)
       do k1 = ndx2D + 1, ndxi
          k2 = kc(k1 - ndx2D)
          if (k2 > 0) then
             if (ba(k1) < ba(k2)) then
                !  ba(k2) = ba(k2) - ba(k1)
             else
                continue
             end if
          end if
       end do
    end if

    do n = 1, ndxi ! after all metrics, maximise ba and dx for better conditioning
       if (n > ndx2D) then ! reset ba on strictly 1D nodes, to bamin1D or to specified manhole area's (todo)
          ba(n) = max(ba(n), bamin1D) ! 2D handled in cutcelwu
       end if
    end do
    do L = lnxi + 1, Lnx
       ba(ln(1, L)) = ba(ln(2, L)) ! set bnd ba to that of inside point
    end do

    if (allocated(banf)) then
       deallocate (banf, nban)
    end if

    if (nump > 0) then

       mxban = 0
       do k = 1, ndx2D ! nump
          mxban = mxban + netcell(k)%n
       end do
       allocate (banf(mxban), stat=ierr) ! for keeps, netnode/flownode subarea
       call aerr('banf(mxban)', ierr, mxban); banf = 0d0
       allocate (nban(4, mxban), stat=ierr) ! for keeps, banf admin
       call aerr('nban(4,mxban)', ierr, mxban); nban = 0

       allocate (rr(mxban), nr(mxban), stat=ierr) ! for temp
       call aerr('rr(mxban), nr(mxban)', ierr, mxban)
       allocate (banh(mxban), stat=ierr)
       call aerr('banh (mxban)', ierr, mxban); banh = 0d0
       allocate (nbanh(4, mxban), stat=ierr)
       call aerr('nbanh(4,mxban)', ierr, mxban); nbanh = 0

       ka = 0 ! set netnode/flownode subarea array ban
       do k = 1, ndx2D ! nump

          nn = netcell(k)%n
          do kk2 = 1, nn ! walk in netcells
             ka = ka + 1 ! subarea nr

             kk1 = kk2 - 1; if (kk1 < 1) kk1 = nn
             kk3 = kk2 + 1; if (kk3 > nn) kk3 = 1

             K1 = netcell(k)%nod(kk1) ! k1 , k2, k3 subsequent netcell nrs
             K2 = netcell(k)%nod(kk2)
             K3 = netcell(k)%nod(kk3)

             xx(1) = xz(k); yy(1) = yz(k)
!         xx(2) = 0.5d0*( xk(k1)+xk(k2) )  ;  yy(2) = 0.5d0*( yk(k1)+yk(k2) )
             call half(xk(k1), yk(k1), xk(k2), yk(k2), xx(2), yy(2), jsferic, jasfer3D)
             xx(3) = xk(k2); yy(3) = yk(k2)
!         xx(4) = 0.5d0*( xk(k2)+xk(k3) )  ;  yy(4) = 0.5d0*( yk(k2)+yk(k3) )
             call half(xk(k2), yk(k2), xk(k3), yk(k3), xx(4), yy(4), jsferic, jasfer3D)

             call dAREAN(XX, YY, 4, banh(ka), DLENGTH, DLENMX) ! area and length of enclosed subpolygon
             nbanh(1, ka) = k2 ! netnode nr
             nbanh(2, ka) = k ! flownode nr
             rr(ka) = k2

             do kk3 = 1, nd(k)%lnx
                L = abs(nd(k)%ln(kk3))
                La = abs(L)
                if (lncn(1, La) == k2 .or. lncn(2, La) == k2) then
                   if (nbanh(3, ka) == 0) then
                      nbanh(3, ka) = La
                   else if (nbanh(4, ka) == 0) then
                      nbanh(4, ka) = La
                   end if
                end if
             end do

          end do
       end do

       call sort_index(rr, nr)
       do k = 1, mxban
          ka = nr(k)
          nban(1, k) = nbanh(1, ka)
          nban(2, k) = nbanh(2, ka)
          nban(3, k) = nbanh(3, ka)
          nban(4, k) = nbanh(4, ka)
          banf(k) = banh(ka)
       end do

       ban = 0d0
       do k = 1, mxban ! netnode area
          n = nban(1, k)
          ban(n) = ban(n) + banf(k)
       end do

       deallocate (banh, nbanh, rr, nr)

    end if

    if (jaconveyance2D >= 1) then ! set link based bed skewness array aifu

       ! bed skewness is not intended for bedlevel type < 3
       if (ibedlevtyp < 3) then
          call qnerror('bed-level type and conveyance type do not match', ' ', ' ')
       else

          if (allocated(aifu)) deallocate (aifu, bz)
          allocate (aifu(lnx), bz(ndx))

          call setaifu()

       end if ! if ( ibedlevtyp.lt.3 )

    end if

!JRE
    if (jawave == 4) then
       call xbeach_makethetagrid()
       call makethindamadmin()
       ! now that ntheta is determined:
       itheta_view = max(ntheta / 2, 1)
    end if

    blmin = minval(bl)

    if (dxwuimin2D > 0d0) then
       do L = lnx1D + 1, lnxi
          if (dx(L) < dxwuimin2D * wu(L)) then
             dxorgL = dx(L)
             dx(L) = dxwuimin2D * wu(L)
             dxi(L) = 1d0 / dx(L)
             write (Msgbuf, '(A,4F15.6)') 'Circumcentre distance dx(L)  < dxwuimin2D*wu(L) : xu, yu, old dx, new dx: ', xu(L), yu(L), dxorgL, dx(L); call msg_flush()
          end if
       end do
    end if

 end subroutine flow_geominit
