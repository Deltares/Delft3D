module m_fm_handle_mesh
    use m_waq_precision          ! single/double precision
    use m_fm_reconst_vel
    use m_fm_particles_in_grid
    use m_hydmod

    implicit none

contains

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

subroutine part_fill_networkdata(hyd, waqgeom,openbndsect_coll)
    use network_data, only: kn, xk, yk, zk, xzw, yzw, numk, numL, nump, netcell, lnn, lne
    use m_part_geom, only: Ndxi, Ndx, Lnx, ba, bl, lne2ln
    use m_part_flow
    use m_part_transport
    use m_alloc
    use m_missing
    use m_part_mesh
    use m_sferic, only: jsferic, jasfer3D
    use geometry_module, only: dbdistance, sphertocart3D, normaloutchk, comp_masscenter
    use timers

    type(t_hydrodynamics),    intent(in) :: hyd                    !< description of the hydrodynamics
    type(t_ug_meshgeom),      intent(in) :: waqgeom                !< model geometry
    type(t_openbndsect_coll), intent(in) :: openbndsect_coll       ! collection of openbndsects

    integer :: ierr, i, j, k, L, ln, N, Nmax
    real(8) :: bottomlevel

    integer(4) ithndl              ! handle to time this subroutine
    data ithndl / 0 /
    if ( timon ) call timstrt( "part_fill_networkdata", ithndl )

    if (hyd%crs%epsg_code == 4326) then ! epsg 4326 is assumed spherical, WGS84 system.
        jsferic = 1
    else
        jsferic = 0
    endif

    numk = waqgeom%numnode
    call realloc(xk, numk, keepExisting=.false., fill = dmiss)
    call realloc(yk, numk, keepExisting=.false., fill = dmiss)
    call realloc(zk, numk, keepExisting=.false., fill = 0.0d0)
    do k = 1, numk
        xk(k) = waqgeom%nodex(k)
        yk(k) = waqgeom%nodey(k)
        if (waqgeom%nodez(k) /= dmiss) then
            zk(k) = waqgeom%nodez(k)
        endif
    enddo

    numl = waqgeom%numedge
    call realloc(kn, [3, numl], keepExisting=.false., fill = 0)
    call realloc(lne2ln, numl, keepExisting=.false., fill = 0)
    call realloc(lnn, numl, keepExisting=.false., fill = 0)
    call realloc(lne, [2, numl], keepExisting=.false., fill = 0)
    Lnx = 0
    do L = 1, numl
        kn(1,L) = waqgeom%edge_nodes(1,L)
        kn(2,L) = waqgeom%edge_nodes(2,L)
        if (hyd%edge_type(L) == 1 .or. hyd%edge_type(L) == 2 .or. hyd%edge_type(L) == 21 .or. hyd%edge_type(L) == 22) then
            Lnx = Lnx + 1
            lne2ln(L) = Lnx
            lnn = 2
        else
            lne2ln(L) = 0
            lnn = 1
        endif
        lne(1, L) = max(waqgeom%edge_faces(1,L),0)
        lne(2, L) = max(waqgeom%edge_faces(2,L),0)
    enddo
    call realloc(q0, hyd%noq, keepExisting=.false., fill=0d0)
    call realloc(q1, hyd%noq, keepExisting=.false., fill=0d0)

    nump = waqgeom%numface
    Ndx = hyd%noseg! waqgeom%numface
    call realloc(xzw, nump, keepExisting=.false., fill = dmiss)
    call realloc(yzw, nump, keepExisting=.false., fill = dmiss)
    call realloc(ba, Ndx, keepExisting=.false., fill = 0.0d0)
    call realloc(bl, Ndx, keepExisting=.false., fill = 0.0d0)
    call realloc(h0, Ndx, keepExisting=.false., fill=0d0)
    call realloc(h1, Ndx, keepExisting=.false., fill=0d0)
    call realloc(vol0, Ndx, keepExisting=.false., fill=0d0)
    call realloc(vol1, Ndx, keepExisting=.false., fill=0d0)

    allocate(netcell(nump), stat = ierr)
    Nmax = waqgeom%maxnumfacenodes
    do k=1,nump

        ! Determine the average z-coordinate for the nodes
        ! to get a representative z-coordinate for the cell as a whole

        N = 0
        bottomlevel = 0.0
        do i = 1, Nmax
            if (waqgeom%face_nodes(i, k) > 0) then
                N = i
                bottomlevel = bottomlevel + zk(waqgeom%face_nodes(i, k))
            else
                exit
            endif
        enddo
        netcell(k)%N = N
        allocate(netcell(k)%nod(N))
        netcell(k)%nod = waqgeom%face_nodes(1:N, k)
        allocate(netcell(k)%lin(N))
        netcell(k)%lin = waqgeom%face_edges(1:N, k)

        xzw(k) = waqgeom%facex(k)
        yzw(k) = waqgeom%facey(k)

        do i = 1,hyd%nolay
            j     = k + (i-1) * hyd%nosegl
            ba(j) = hyd%surf(k)
            bl(j) = bottomlevel / N
        end do
    end do

    if ( timon ) call timstop ( ithndl )
end subroutine


subroutine part_setmesh()
    use m_fm_aux_routines
    use network_data, only: kn, xk, yk, xzw, yzw, numk, numL, nump, netcell, lnn, lne
    use m_part_geom, only: lne2ln, ba
    use m_alloc
    use m_missing
    use m_part_mesh
    use m_sferic, only: jsferic, jasfer3D
    use geometry_module, only: dbdistance, sphertocart3D, normaloutchk, comp_masscenter
    use timers

    integer                        :: i, node1, node2, icell, j, k, L, N
    integer                        :: im1, ip1, L1, L2
    integer                        :: newnode, newedge
    integer                        :: isign, ja, k1, k2, k3, kL, kR
    integer                        :: jaswap

    integer                        :: numnontris
    integer                        :: numaddedges

    real(kind=dp), dimension(3)    :: xv, yv
    real(kind=dp), dimension(3)    :: t, t1, t2    ! edge tangential vector

    integer(4) ithndl              ! handle to time this subroutine
    data ithndl / 0 /
    if ( timon ) call timstrt( "part_setmesh", ithndl )

    numnodes = numk
    numedges = numL
    numcells = nump

    ! count number of non-triangles and additional (internal) edges
    numnontris=0
    numaddedges=0
    do k=1,nump
        N = netcell(k)%N
        if ( N > 3 ) then
            numnontris = numnontris+1
            numaddedges = numaddedges+N
        end if
    end do

    ! compute sizes
    numnodes = numk + numnontris                 ! add centers of non-triangles
    numedges = numL + numaddedges                ! add internal edges of non-triangles
    numcells = nump - numnontris + numaddedges   ! add internal triangles of non-triangles

    ! (re)allocate
    call realloc_partmesh()

    ! nodes
    if ( jsferic==0 ) then
        do k=1,numk
            xnode(k) = xk(k)
            ynode(k) = yk(k)
        end do
    else
        do k=1,numk
            call sphertocart3D(xk(k),yk(k),xnode(k),ynode(k),znode(k))
        end do
    end if

     ! edges
    do L=1,numL
        edge2node(1,L) = kn(1,L)
        edge2node(2,L) = kn(2,L)
        if ( lne2ln(L) >= 0 ) then
            edge2link(L) = lne2ln(L)
        end if
    end do

    ! set jcell2edge startpointers and add new triangles
    icell=0
    jcell2edge(1) = 1
    do k=1,nump
        N = netcell(k)%N
        if ( N==3 ) then
            icell = icell+1
            jcell2edge(icell+1) = jcell2edge(icell) + 3
            cell2nod(icell) = k
        else if ( N > 3 ) then
            do j=1,N
                icell = icell+1
                jcell2edge(icell+1) = jcell2edge(icell) + 3
                cell2nod(icell) = -k
            end do
        else  ! should not happen -- AM: in that case, why the "continue", that is a no-op
            continue
        end if
    end do

    ! allocate jcell2edge
    N = jcell2edge(numcells+1)-1
    call realloc(icell2edge, N, fill=0, keepExisting=.false.)

    ! copy netcell data into cell2edge and add new triangles (nodes, edges)
    icell = 0
    newnode = numk
    numorigedges = numL
    newedge = numorigedges
    do k=1,nump
        N = netcell(k)%N
        if ( N==3 )then
            icell = icell+1   ! add existing triangle
            i = 0
            do j=jcell2edge(icell),jcell2edge(icell+1)-1
                i = i+1
                L = netcell(k)%lin(i)
                icell2edge(j) = L
            end do
            if ( jsferic==0 ) then
                xzwcell(icell) = xzw(k)
                yzwcell(icell) = yzw(k)
            else
                call sphertocart3D(xzw(k),yzw(k),xzwcell(icell),yzwcell(icell),zzwcell(icell))
            end if
            areacell(icell) = ba(k)
        else
            ! add node
            newnode = newnode+1
            if ( jsferic==0 ) then
                xnode(newnode) = xzw(k)
                ynode(newnode) = yzw(k)
            else
                call sphertocart3D(xzw(k),yzw(k),xnode(newnode),ynode(newnode),znode(newnode))
            end if

            xv(1) = xzw(k)
            yv(1) = yzw(k)

           do i=1,N ! add new edges and triangles
               im1 = i-1; if ( im1 < 1 ) im1=im1+N
               ip1 = i+1; if ( ip1 > N ) ip1=ip1-N
               L = netcell(k)%lin(i)
               call find_common_node(netcell(k)%lin(im1),L,node1)
               call find_common_node(netcell(k)%lin(ip1),L,node2)

               icell = icell+1
               ! add edges
               newedge = newedge+1
               edge2node(1,newedge) = newnode
               edge2node(2,newedge) = node1
               edge2link(newedge) = -1

               ! add edges to cell2edge
               j = jcell2edge(icell)
               icell2edge(j) = newedge
               icell2edge(j+1) = L
               icell2edge(j+2) = newedge+1
               if ( i==N ) then
                   icell2edge(j+2) = icell2edge(j+2) - N
               end if

               ! compute mass center
               xv(2) = xk(node1)
               yv(2) = yk(node1)
               xv(3) = xk(node2)
               yv(3) = yk(node2)
               call comp_masscenter(3, xv, yv, xzwcell(icell), yzwcell(icell), areacell(icell), ja, jsferic, jasfer3D, dmiss)

               if ( jsferic==1 ) then
                   xv(1) = xzwcell(icell)  ! reuse xv
                   yv(1) = yzwcell(icell)   ! reuse yv
                   call sphertocart3D(xv(1), yv(1),xzwcell(icell),yzwcell(icell),zzwcell(icell))
               end if
            end do
        end if
    end do

    ! set edge2cell
    edge2cell = 0
    do icell=1,numcells
        do j=jcell2edge(icell),jcell2edge(icell+1)-1
            L = icell2edge(j)
            if ( edge2cell(1,L)==0 ) then
                edge2cell(1,L) = icell
            else
                edge2cell(2,L) = icell
            end if
       end do
    end do

    !  fix orientation of edge2cell
    do L=1,numL ! exclude new edges
        k1 = edge2cell(1,L)
        if ( k1==0 ) cycle

        k1 = abs(cell2nod(k1))

        jaswap = 0

        if ( lnn(L)==1 ) then  !     boundaries: inward normal
            jaswap = 1
        else if ( k1==lne(2,L) .and. lnn(L) > 1 ) then
            jaswap = 1
        end if

        if ( jaswap==1 ) then
            icell = edge2cell(1,L)
            edge2cell(1,L) = edge2cell(2,L)
            edge2cell(2,L) = icell
        end if
    end do

    if ( jsferic /= 0 ) then
        ! compute cell normal vectors from first two edges
        do k=1,numcells
            L1 = icell2edge(jcell2edge(k))
            L2 = icell2edge(jcell2edge(k)+1)
            k1 = edge2node(1,L1)
            k2 = edge2node(2,L1)
            k3 = edge2node(1,L2)
            if ( k3==k1 .or. k3==k2 ) then
                k3 = edge2node(2,L2)
            end if
            t1 = (/ xnode(k2)-xnode(k1), ynode(k2)-ynode(k1), znode(k2)-znode(k1)/)
            t2 = (/ xnode(k3)-xnode(k2), ynode(k3)-ynode(k2), znode(k3)-znode(k2)/)

            dnn(:,k) = (/ t1(2)*t2(3) - t1(3)*t2(2), t1(3)*t2(1) - t1(1)*t2(3), t1(1)*t2(2) - t1(2)*t2(1) /)
            dnn(:,k) = dnn(:,k) / sqrt( dnn(1,k)**2 + dnn(2,k)**2 + dnn(3,k)**2 )

            ! fix orientation
            if ( dnn(1,k)*xnode(k1) + dnn(2,k)*ynode(k1) + dnn(3,k)*znode(k1) < 0d0 ) then
                dnn(:,k) = -dnn(:,k)
            end if
        end do
    end if

    ! nx, ny, w
    do L=1,numedges
        k1 = edge2node(1,L)
        k2 = edge2node(2,L)

        kL = edge2cell(1,L)
        kR = edge2cell(2,L)

        k = kL   ! outward positive

        isign = 1
        if ( k <= 0 ) then
            k = edge2cell(2,L)   ! inward positive
            isign = -1
        end if

        if ( k==0 ) cycle  ! isolated edge

        ! compute normal vector (outward positive)
        if ( jsferic==0 ) then
            call normaloutchk(xnode(k1),ynode(k1),xnode(k2),ynode(k2),xzwcell(k),yzwcell(k),dnx(1,L),dny(1,L),ja, jsferic, jasfer3D, dmiss, dxymis)

            if ( isign==-1 ) then
                dnx(1,L) = -dnx(1,L)
                dny(1,L) = -dny(1,L)
            end if

            w(L) = dbdistance(xnode(k1),ynode(k1),xnode(k2),ynode(k2),jsferic, jasfer3D, dmiss)
        else
            ! compute outward normal with respect to the left cell
            t = (/ xnode(k2)-xnode(k1), ynode(k2)-ynode(k1), znode(k2)-znode(k1) /)
            w(L) = sqrt( t(1)**2 + t(2)**2 + t(3)**2)

            t = t/w(L)

            if ( kL > 0 ) then
                ! left cell normal : nn X t
                dnx(1,L) = dnn(2,kL) * t(3) - dnn(3,kL) * t(2)
                dny(1,L) = dnn(3,kL) * t(1) - dnn(1,kL) * t(3)
                dnz(1,L) = dnn(1,kL) * t(2) - dnn(2,kL) * t(1)

                ! fix orientation
                if ( (xnode(k1)-xzwcell(kL))*dnx(1,L) + (ynode(k1)-yzwcell(kL))*dny(1,L) + (znode(k1)-zzwcell(kL))*dnz(1,L) < 0d0 ) then
                    dnx(1,L) = -dnx(1,L)
                    dny(1,L) = -dny(1,L)
                    dnz(1,L) = -dnz(1,L)
                end if
            end if

            if ( kR > 0 ) then
                ! left cell normal : nn X t
                dnx(2,L) = dnn(2,kR) * t(3) - dnn(3,kR) * t(2)
                dny(2,L) = dnn(3,kR) * t(1) - dnn(1,kR) * t(3)
                dnz(2,L) = dnn(1,kR) * t(2) - dnn(2,kR) * t(1)

                ! fix orientation
                if ( (xnode(k1)-xzwcell(kR))*dnx(2,L) + (ynode(k1)-yzwcell(kR))*dny(2,L) + (znode(k1)-zzwcell(kR))*dnz(2,L) < 0d0 ) then
                    dnx(2,L) = -dnx(2,L)
                    dny(2,L) = -dny(2,L)
                    dnz(2,L) = -dnz(2,L)
                end if
            end if
        end if
    end do

    if ( timon ) call timstop ( ithndl )
end subroutine part_setmesh


!> (re)allocate partmesh data
subroutine realloc_partmesh()
    use m_part_mesh
    use m_alloc
    use m_missing
    use m_sferic, only: jsferic

    integer :: N

    call realloc(edge2node, (/2, numedges/), fill=0, keepExisting=.false.)
    call realloc(edge2cell, (/2, numedges/), fill=0, keepExisting=.false.)
    call realloc(xnode, numnodes, fill=0d0, keepExisting=.false.)
    call realloc(ynode, numnodes, fill=0d0, keepExisting=.false.)
    if ( jsferic==1 ) then
        call realloc(znode, numnodes, fill=0d0, keepExisting=.false.)
    end if

    call realloc(xzwcell, numcells, fill=DMISS, keepExisting=.false.)
    call realloc(yzwcell, numcells, fill=DMISS, keepExisting=.false.)
    if ( jsferic==1 ) then
        call realloc(zzwcell, numcells, fill=DMISS, keepExisting=.false.)
    end if
    call realloc(areacell, numcells, fill=DMISS, keepExisting=.false.)

    if ( jsferic==0 ) then
        call realloc(dnx, (/1, numedges/), fill=DMISS, keepExisting=.false.)
        call realloc(dny, (/1, numedges/), fill=DMISS, keepExisting=.false.)
    else
        call realloc(dnx, (/2, numedges/), fill=DMISS, keepExisting=.false.)
        call realloc(dny, (/2, numedges/), fill=DMISS, keepExisting=.false.)
        call realloc(dnz, (/2, numedges/), fill=DMISS, keepExisting=.false.)
        call realloc(dnn, (/3, numcells/), fill=DMISS, keepExisting=.false.)
    end if
    call realloc(w, numedges, fill=DMISS, keepExisting=.false.)

    call realloc(edge2link, numedges, fill=0, keepExisting=.false.)
    call realloc(cell2nod, numcells, fill=0, keepExisting=.false.)

    call realloc(jcell2edge, numcells+1, fill=1, keepExisting=.false.)
    N = jcell2edge(numcells+1)-1
    call realloc(icell2edge, N, fill=0, keepExisting=.false.)

    return
end subroutine realloc_partmesh

!> deallocate particle mesh data
subroutine dealloc_partmesh()
    use m_part_mesh

    if ( allocated(edge2node ) ) deallocate(edge2node )
    if ( allocated(edge2cell ) ) deallocate(edge2cell )
    if ( allocated(xnode     ) ) deallocate(xnode     )
    if ( allocated(ynode     ) ) deallocate(ynode     )
    if ( allocated(znode     ) ) deallocate(znode     )

    if ( allocated(xzwcell   ) ) deallocate(xzwcell   )
    if ( allocated(yzwcell   ) ) deallocate(yzwcell   )
    if ( allocated(zzwcell   ) ) deallocate(zzwcell   )
    if ( allocated(areacell  ) ) deallocate(areacell  )

    if ( allocated(dnn       ) ) deallocate(dnn       )
    if ( allocated(dnx       ) ) deallocate(dnx       )
    if ( allocated(dny       ) ) deallocate(dny       )
    if ( allocated(dnz       ) ) deallocate(dnz       )
    if ( allocated(w         ) ) deallocate(w         )

    if ( allocated(edge2link ) ) deallocate(edge2link )
    !  if ( allocated(nod2cell  ) ) deallocate(nod2cell  )
    if ( allocated(cell2nod  ) ) deallocate(cell2nod  )

    if ( allocated(icell2edge) ) deallocate(icell2edge)
    if ( allocated(jcell2edge) ) deallocate(jcell2edge)
end subroutine dealloc_partmesh

! determine the lowest active layer
subroutine fill_laybot_via_attributes(hyd, laybot)

    type(t_hydrodynamics),    intent(in)  :: hyd           !< description of the hydrodynamics
    integer(int_wp), pointer, intent(out) :: laybot(:,:)   !< lowest active layer

    integer :: i, k, idcell

    allocate( laybot(1,hyd%nosegl) )

    laybot = 0
    do i = 1,hyd%nosegl
        do k = 1,hyd%nolay
            idcell = i + (k-1) * hyd%nosegl
            if ( mod( hyd%attributes(idcell), 10 ) == 1 ) then
                laybot(1,i) = k
            endif
        enddo
    enddo

end subroutine fill_laybot_via_attributes

!  initialise grid and hydro data
subroutine ini_part_grid(hyd)
    use partmem, only: ihdel, layt, nolayp, tcktot, nmaxp, mmaxp, mnmax2, mnmaxk, laybot
    use m_part_flow
    use m_part_transport, only: constituents, numconst
    use m_missing
    use m_alloc
    use MessageHandling
    use timers

    type(t_hydrodynamics)                    :: hyd           !< description of the hydrodynamics

    integer             :: minp, ilay, lun
    logical             :: lexist

    integer(4) ithndl              ! handle to time this subroutine
    data ithndl / 0 /
    if ( timon ) call timstrt( "ini_part_grid", ithndl )

    !  deallocate
    call dealloc_partmesh()
    call dealloc_partfluxes()
    call dealloc_partrecons()
    call dealloc_particles()
    call dealloc_auxfluxes()

    kmx = hyd%nolay

    !     fill network_data
    call part_fill_networkdata(hyd, hyd%waqgeom, hyd%openbndsect_coll)

    !     set pointers with mesh connectivity etc.
    call part_setmesh()

    !     examine the attributes and store the bottom-most active layer
    call fill_laybot_via_attributes( hyd, laybot )

    !     set flux coeffecients
    call comp_fluxcoeffs()

    call realloc_partrecons()

    call reconst_vel_coeffs_fmx()

    call alloc_auxfluxes()

    ihdel = hyd%cnv_step_sec
    layt = hyd%nolay
    nolayp = hyd%nolay
    allocate ( tcktot(hyd%nolay))
    do ilay = 1, hyd%nolay
        tcktot(ilay) = hyd%waq_layers(ilay)
    enddo
    mnmax2 = hyd%nosegl
    mnmaxk = hyd%nosegl*hyd%nolay

    if ( timon ) call timstop ( ithndl )
end subroutine ini_part_grid

end module m_fm_handle_mesh
