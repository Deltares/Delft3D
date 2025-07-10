module m_fm_reconst_vel

    implicit none

contains

    !----- AGPL --------------------------------------------------------------------
    !
    !  Copyright (C)  Stichting Deltares, 2017-2025.
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


    !> Construct the coefficients for reconstructing the flow velocity in the triangular (inner) cells
    !! It calculates the coefficients for the normal velocities on the three sides as well as the
    !! slope of the velocity in the normal direction. As in the case of spherical coordinates
    !! 3D cartesian coordinates are used to determine the position on the globe, the number of
    !! coefficients differs for the plane and the spherical cases.
    !!
    !! See partfmmem.f90 for an explanation of the variables.
    !!
    !! The method is described in the memo by Mart Borsboom, dd. december 2015 (reference 1220026.014)
    !!
    !! Areconst - coefficients per internal cell, output
    !!
    subroutine reconst_vel_coeffs_fmx()

        use m_part_recons
        use m_part_mesh
        use m_alloc
        use m_sferic
        use geometry_module, only: dbdistance, gaussj
        use timers

        implicit none

        integer, parameter :: N = 4

        double precision, dimension(N, N) :: Amat
        double precision, dimension(N) :: rhs

        double precision :: cs, sn

        integer :: i, icell, j, jj, k, L, NN
        integer :: k1, k2
        integer :: i12, isign

        integer(4) ithndl              ! handle to time this subroutine
        data ithndl / 0 /
        if (timon) call timstrt("reconst_vel_coeffs_fm", ithndl)


        ! allocate startpointers
        call realloc(jreconst, numcells + 1, keepExisting = .false., fill = 0)

        ! set startpointers
        jreconst(1) = 1
        do icell = 1, numcells
            jreconst(icell + 1) = jcell2edge(icell) + jcell2edge(icell + 1) - jcell2edge(icell)
        end do

        ! allocate column indices and matrix entries
        NN = jreconst(numcells + 1) - 1
        call realloc(ireconst, NN, keepExisting = .false., fill = 0)
        if (jsferic==0) then
            call realloc(Areconst, (/3, NN /), keepExisting = .false., fill = 0d0)
        else
            call realloc(Areconst, (/4, NN /), keepExisting = .false., fill = 0d0)
        end if

        ! dummy rhs
        rhs = 0d0

        ! loop over internal cells
        jj = 0
        do icell = 1, numcells
            ! get flownode number (for s, bl)
            k = abs(cell2nod(icell))

            ! fill system for (ux,uy) = (ux0, uy0) + alpha (x-x0, y-y0)
            !
            ! loop over edges of the internal cells (netlinks)
            !
            i = 0
            do j = jcell2edge(icell), jcell2edge(icell + 1) - 1
                i = i + 1
                L = icell2edge(j) ! netlink

                k1 = edge2node(1, L)
                k2 = edge2node(2, L)

                if (jsferic==0) then
                    cs = dnx(1, L)
                    sn = dny(1, L)

                    ! add to system
                    Amat(i, 1) = cs
                    Amat(i, 2) = sn
                    Amat(i, 3) = (0.5d0 * (xnode(k1) + xnode(k2)) - xzwcell(icell)) * cs + (0.5d0 * (ynode(k1) + ynode(k2)) - yzwcell(icell)) * sn
                else
                    i12 = 1
                    isign = 1
                    if (edge2cell(2, L)==icell) then
                        i12 = 2
                        isign = -1d0
                    end if

                    Amat(i, 1) = dnx(i12, L) * isign
                    Amat(i, 2) = dny(i12, L) * isign
                    Amat(i, 3) = dnz(i12, L) * isign
                    Amat(i, 4) = ((0.5d0 * (xnode(k1) + xnode(k2)) - xzwcell(icell)) * dnx(i12, L) + &
                            (0.5d0 * (ynode(k1) + ynode(k2)) - yzwcell(icell)) * dny(i12, L) + &
                            (0.5d0 * (znode(k1) + znode(k2)) - zzwcell(icell)) * dnz(i12, L)) * isign
                end if

                jj = jj + 1
                ireconst(jj) = L
            end do

            if (jsferic==0) then
                ! solve system
                call gaussj(Amat, 3, N, rhs, 1, 1)

                do i = 1, 3
                    L = icell2edge(jcell2edge(icell) + i - 1)
                    Areconst(1:3, jreconst(icell) + i - 1) = Amat(1:3, i)
                end do
            else
                ! impose zero cell normal velocity
                Amat(4, 1:3) = dnn(:, icell)
                Amat(4, 4) = 0d0

                ! solve system
                call gaussj(Amat, 4, N, rhs, 1, 1)

                do i = 1, 3
                    L = icell2edge(jcell2edge(icell) + i - 1)
                    Areconst(1:4, jreconst(icell) + i - 1) = Amat(1:4, i)
                end do

            end if
        end do

        if (timon) call timstop (ithndl)

    end subroutine reconst_vel_coeffs_fmx


    !> Reconstruct velocity in (internal) cells
    !! The velocities are velocities normal to the edges and there is linear gradient off the edge.
    !!
    !! u0x, u0y, u0z - cartesian components of the velocity in the cell, output
    !! alphafm - coefficient for linear gradient
    !!
    subroutine reconst_vel(q, h0, h1)
        use partmem, only: hyd
        use m_part_geom, only: Ndx, Lnx, bl
        use m_part_parameters, only: epshs
        use m_part_recons
        use m_part_mesh
        use m_sferic
        use geometry_module, only: dbdistance
        use timers

        implicit none

        double precision, dimension(Lnx), intent(in) :: q    !< flowlink-based discharge (m3/s)
        double precision, dimension(Ndx), intent(in) :: h0   !< layer thickness (m) at begin of interval
        double precision, dimension(Ndx), intent(in) :: h1   !< layer thickness (m) at end of interval

        integer, parameter :: N = 4

        integer :: icell, icell3d, j, k, L, lay, L3d

        double precision :: hk0, hk1, h, un

        double precision, parameter :: DTOL = 1d-10

        integer(4) ithndl              ! handle to time this subroutine
        data ithndl / 0 /
        if (timon) call timstrt("reconst_vel", ithndl)

        ! get fluxes at all edges, including internal
        call set_fluxes(Lnx, q, qe)

        ! initialize
        u0x = 0d0
        u0y = 0d0
        u0w = 0d0
        if (jsferic==1) then
            u0z = 0d0
        end if
        alphafm = 0d0

        do lay = 1, hyd%num_layers
            do icell = 1, numcells
                ! get flownode number (for s, bl)
                k = abs(cell2nod(icell)) + (lay - 1) * hyd%nosegl
                icell3d = icell + (lay - 1) * numcells

                !
                ! Get water depth - take the change from the start to the end into account
                !
                hk0 = h0(k) !s0(k)-bl(k)
                hk1 = h1(k) !s1(k)-bl(k)
                if (abs(hk1 - hk0)>DTOL) then
                    if (hk0>epshs .and. hk1>epshs) then
                        h = (hk1 - hk0) / (log(hk1) - log(hk0))
                    else if (hk0>epshs .or. hk1>epshs) then
                        h = 0.5d0 * (hk0 + hk1)
                    else
                        h = 0d0
                    end if
                else
                    h = 0.5d0 * (hk0 + hk1)
                end if

                if (h<=epshs) cycle

                if (jsferic==0) then
                    !
                    ! Get the link index for this edge - first in the upper layer,
                    ! then in the actual layer.
                    ! un is the velocity normal to the edge
                    ! u0x, u0y and u0z are the cartesian components of that velocity
                    ! alphafm is the linear gradient off the edge
                    !
                    ! As qe is the flow rate through the edge, we need to divide by the
                    ! surface area (width times water height in that layer)
                    !
                    do j = jreconst(icell), jreconst(icell + 1) - 1
                        L = ireconst(j)
                        L3d = L + (lay - 1) * numedges
                        un = qe(L3d) / (h * w(L))
                        u0x(icell3d) = u0x(icell3d) + Areconst(1, j) * un
                        u0y(icell3d) = u0y(icell3d) + Areconst(2, j) * un
                        alphafm(icell3d) = alphafm(icell3d) + Areconst(3, j) * un
                    end do
                else
                    do j = jreconst(icell), jreconst(icell + 1) - 1
                        L = ireconst(j)
                        L3d = L + (lay - 1) * numedges
                        un = qe(L3d) / (h * w(L))
                        u0x(icell3d) = u0x(icell3d) + Areconst(1, j) * un
                        u0y(icell3d) = u0y(icell3d) + Areconst(2, j) * un
                        u0z(icell3d) = u0z(icell3d) + Areconst(3, j) * un
                        alphafm(icell3d) = alphafm(icell3d) + Areconst(4, j) * un
                    end do
                end if
            end do
        end do

        !
        ! Fill the array for the vertical motion. It has a different unit (1/s) than
        ! the others, as we use relative coordinates for the position within a layer
        ! Note:
        ! The array u0w holds the relative vertical velocity for the interface
        ! between layer L and L+1. The flow array holds the flow rate between
        ! these layers L and L+1. The calculation of the indices and the loop
        ! bounds are influenced by this.
        !
        u0w = 0.0
        do lay = 1, hyd%num_layers - 1
            do icell = 1, numcells
                k = abs(cell2nod(icell)) + (lay - 1) * hyd%nosegl
                L3d = hyd%num_exchanges_u_dir + k
                icell3d = icell + (lay - 1) * numcells

                u0w(icell3d) = hyd%flow(L3d) / hyd%volume(k)
            end do
        end do

        if (timon) call timstop (ithndl)
    end subroutine reconst_vel


    !> Set all fluxes, including internal
    !!
    subroutine set_fluxes(Lnx, q, qe)
        use partmem, only: hyd
        use m_part_mesh
        use m_part_fluxes
        use timers

        implicit none

        integer, intent(in) :: Lnx        !< Number of flow links
        double precision, dimension(*), intent(in) :: q          !< Flow rate for each edge in original FM grid

        double precision, dimension(*), intent(out) :: qe         !< Flow rate (flux) for all edges (including internal)

        integer :: j, L, Lf, lay, l3d

        integer(4) ithndl              ! handle to time this subroutine
        data ithndl / 0 /
        if (timon) call timstrt("set_fluxes", ithndl)

        do lay = 1, hyd%num_layers
            do L = 1, numedges
                L3d = L + (lay - 1) * numedges
                qe(L3d) = 0d0
                do j = jflux2link(L), jflux2link(L + 1) - 1
                    Lf = iflux2link(j) + (lay - 1) * lnx
                    if (Lf>0) then
                        qe(L3d) = qe(L3d) + Aflux2link(j) * q(Lf)
                    end if
                end do
            end do
        end do

        if (timon) call timstop (ithndl)
    end subroutine set_fluxes


    !> Compute mapping from prescribed (at flowlinks) to all fluxes (at all edges, including "internal")
    !! Part of the grid administration
    !!
    subroutine comp_fluxcoeffs()
        use m_part_mesh
        use m_part_fluxes
        use m_alloc
        use MessageHandling
        use geometry_module, only: gaussj
        use timers

        implicit none

        integer :: N         ! number of subtriangles

        integer, dimension(MAXSUBCELLS) :: L1        ! "internal" edges
        integer, dimension(MAXSUBCELLS) :: L2        ! original "non-internal" edges
        integer, dimension(MAXSUBCELLS) :: icell     ! subcells
        integer, dimension(MAXSUBCELLS) :: isign1    ! orientation of "internal: edges L1, all cw or
        integer, dimension(MAXSUBCELLS) :: isign2    ! orientation of "non-internal: edges L2, outward normal
        double precision, dimension(MAXSUBCELLS) :: circ      ! weight of edge L1 in computation of circulation

        double precision, dimension(MAXSUBCELLS, MAXSUBCELLS) :: A, B

        double precision :: areasum, dlen

        integer :: i, im1, ip1, j, j2, k, L, L3, Lf

        integer(4) ithndl              ! handle to time this subroutine
        data ithndl / 0 /
        if (timon) call timstrt("comp_fluxcoeffs", ithndl)

        ! allocate
        call realloc_partfluxes()

        ! set startpointers
        jflux2link(1) = 1
        L = 1
        do while (L<=numedges)
            Lf = edge2link(L)
            if (Lf>0) then
                ! original flowlink
                jflux2link(L + 1) = jflux2link(L) + 1

                ! proceed to next edge
                L = L + 1
            else if (L>numorigedges) then
                ! internal edge: find number of subtriangels
                N = 0
                do while (icommonval(edge2cell(:, L + N), edge2cell(:, L + N + 1))/=0)
                    N = N + 1
                    if (L + N>=numedges) exit
                end do

                if (N>0) then
                    ! check connection first and last subtriangle
                    if (icommonval(edge2cell(:, L), edge2cell(:, L + N))/=0) then
                        N = N + 1

                        do i = 1, N
                            jflux2link(L + 1) = jflux2link(L) + N
                            ! proceed to next edge
                            L = L + 1
                        end do
                    else
                        call mess(LEVEL_ERROR, 'comp_fluxcoeffs: numbering error')
                    end if

                else  ! should not happen
                    call mess(LEVEL_ERROR, 'comp_fluxcoeffs: numbering error')
                end if

                if (L>=numedges) exit
            else  ! other
                jflux2link(L + 1) = jflux2link(L)
                ! proceed to next edge
                L = L + 1
            end if
        end do

        ! reallocate
        call realloc(iflux2link, jflux2link(numedges + 1) - 1, fill = 0, keepExisting = .false.)
        call realloc(Aflux2link, jflux2link(numedges + 1) - 1, fill = 0d0, keepExisting = .false.)

        ! fill iflux2link (first in edge-numbers) and compute coefficients
        L = 1
        do while (L<=numedges)
            j = jflux2link(L)
            N = jflux2link(L + 1) - j

            if (N==1) then
                ! original "non-internal" link
                iflux2link(j) = L
                Aflux2link(j) = 1d0
                ! proceed to next edge
                L = L + 1
            else if (N>1) then
                ! "internal" link
                do i = 1, N
                    ip1 = i + 1; if (ip1>N) ip1 = ip1 - N
                    L1(i) = L + i - 1
                    L3 = L1(i) + ip1

                    ! find subcell
                    icell(i) = icommonval(edge2cell(:, L + i - 1), edge2cell(:, L + ip1 - 1))

                    ! find original edge
                    j2 = jcell2edge(icell(i)) + 1   ! order of cell edges: "left" internal (L1), orginal (L2), "right internal (L3)
                    L2(i) = icell2edge(j2)

                    ! get orientation of "internal" edges (i-dir positive)
                    ! note: will always be (/ -1, 1, 1, ... /), due to generation of edge2cells in part_setmesh
                    isign1(i) = 1
                    if (edge2cell(1, L1(i))==icell(i)) isign1(i) = -1

                    ! get orientation of original "non-internal" edges (outward normal)
                    isign2(i) = 1
                    if (edge2cell(2, L2(i))==icell(i)) isign2(i) = -1
                end do

                ! compute summed area and circulation weights
                areasum = 0d0
                do i = 1, N
                    im1 = i - 1; if (im1<1) im1 = im1 + N
                    areasum = areasum + areacell(icell(i))
                    dlen = 0.25 * (areacell(icell(im1)) + areacell(icell(i))) / w(L1(i))
                    circ(i) = dlen / w(L1(i))
                end do

                ! build system: A qe = B qlink
                A = 0d0
                B = 0d0
                ! continuity equations
                do i = 1, N - 1
                    A(i, i) = -isign1(i)   ! outward
                    A(i, i + 1) = isign1(i + 1) ! outward
                    B(i, i) = -isign2(i)   ! outward, rhs
                    do k = 1, N
                        B(i, k) = B(i, k) + areacell(icell(i)) * isign2(k) / areasum
                    end do
                end do
                ! circulation
                do k = 1, N
                    A(N, k) = circ(k) * isign1(k)
                end do

                ! invert matrix: qe = inv(A) B qlink
                call gaussj(A, N, MAXSUBCELLS, B, N, MAXSUBCELLS)

                ! fill data
                do i = 1, N
                    iflux2link(j:j + N - 1) = L2(1:N)
                    Aflux2link(j:j + N - 1) = B(i, 1:N)

                    ! proceed to next edge
                    j = j + N
                    L = L + 1
                end do
            else
                ! closed boundary: proceed to next edge
                L = L + 1
            end if
        end do

        ! convert to link number
        do j = 1, jflux2link(numedges + 1) - 1
            L = iflux2link(j)
            Lf = edge2link(L)
            if (Lf>0) then
                iflux2link(j) = Lf
            else  ! closed boundary
                iflux2link(j) = 0
            end if
        end do

        if (timon) call timstop (ithndl)
    end subroutine comp_fluxcoeffs


    !> (Re)allocate arrays for storing flux coefficients
    !!
    subroutine realloc_partfluxes()
        use m_part_mesh
        use m_part_fluxes
        use m_alloc
        use m_missing
        implicit none

        integer :: N

        call realloc(jflux2link, numedges + 1, keepExisting = .false., fill = 1)
        N = jflux2link(numedges + 1) - 1
        call realloc(iflux2link, N, keepExisting = .false., fill = 0)
        call realloc(Aflux2link, N, keepExisting = .false., fill = 0d0)
    end subroutine realloc_partfluxes


    !> Deallocate arrays for flux coefficients
    !!
    subroutine dealloc_partfluxes()
        use m_part_fluxes
        implicit none

        if (allocated(iflux2link)) deallocate(iflux2link)
        if (allocated(jflux2link)) deallocate(jflux2link)
        if (allocated(Aflux2link)) deallocate(Aflux2link)
    end subroutine dealloc_partfluxes


    !> (Re)allocate arrays for flux coefficients, flow rates, velocity components
    !!
    subroutine realloc_partrecons()
        use partmem, only: hyd
        use m_part_mesh
        use m_part_recons
        use m_alloc
        use m_missing
        use m_sferic, only: jsferic
        implicit none

        call realloc(qe, numedges * hyd%num_layers, keepExisting = .false., fill = DMISS)
        call realloc(qbnd, numedges * hyd%num_layers, keepExisting = .false., fill = 0)
        call realloc(cell_closed_edge, numedges, keepExisting = .false., fill = 0)
        call realloc(u0x, numcells * hyd%num_layers, keepExisting = .false., fill = DMISS)
        call realloc(u0y, numcells * hyd%num_layers, keepExisting = .false., fill = DMISS)
        call realloc(u0w, numcells * hyd%num_layers, keepExisting = .false., fill = DMISS) ! AM: question: num_layers or num_layers-1
        if (jsferic==1) then
            call realloc(u0z, numcells * hyd%num_layers, keepExisting = .false., fill = DMISS)
        end if
        call realloc(alphafm, numcells * hyd%num_layers, keepExisting = .false., fill = DMISS)

        call realloc(ireconst, numcells + 1, keepExisting = .false., fill = 0)

    end subroutine realloc_partrecons


    !> Deallocate flux_coeffs
    subroutine dealloc_partrecons()
        use m_part_recons
        implicit none

        if (allocated(qe)) deallocate(qe)

        if (allocated(u0x)) deallocate(u0x)
        if (allocated(u0y)) deallocate(u0y)
        if (allocated(u0z)) deallocate(u0z)
        if (allocated(u0w)) deallocate(u0w)
        if (allocated(alphafm)) deallocate(alphafm)

        if (allocated(ireconst)) deallocate(ireconst)
        if (allocated(jreconst)) deallocate(jreconst)
        if (allocated(Areconst)) deallocate(Areconst)
    end subroutine dealloc_partrecons


    !> Find common value of two arrays i1 and i2 of size 2.
    !! It is assumed there is at most one common value.
    !! Return the common values
    integer function icommonval(i1, i2)
        implicit none

        integer, dimension(2), intent(in) :: i1
        integer, dimension(2), intent(in) :: i2

        icommonval = 0
        if (i1(1)==i2(1) .or. i1(1)==i2(2)) then
            icommonval = i1(1)
        else if (i1(2)==i2(1) .or. i1(2)==i2(2)) then
            icommonval = i1(2)
        end if
    end function icommonval


    !> Allocate auxiliary fluxes
    !!
    subroutine alloc_auxfluxes()
        use m_particles
        use m_part_geom, only: Ndx, Lnx
        use m_alloc
        implicit none

        call realloc(hbegin, Ndx, fill = 0d0, keepExisting = .false.)
        call realloc(qpart, Lnx, fill = 0d0, keepExisting = .false.)
    end subroutine alloc_auxfluxes


    !> Deallocate auxiliary fluxes
    !!
    subroutine dealloc_auxfluxes()
        use m_particles
        implicit none

        if (allocated(hbegin)) deallocate(hbegin)
        if (allocated(qpart)) deallocate(qpart)

        if (allocated(qfreesurf)) deallocate(qfreesurf)
    end subroutine dealloc_auxfluxes

end module m_fm_reconst_vel
