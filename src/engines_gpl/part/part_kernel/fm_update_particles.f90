module m_fm_update_particles

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

    subroutine update_particles(q, h0, h1, Dt)
        use m_fm_reconst_vel
        use m_waq_precision
        use partmem, only: nopart, mpart
        use m_particles
        use m_part_geom, only: Ndx, Lnx
        use m_sferic
        use m_sferic_part, only: ptref
        use geometry_module, only: Cart3Dtospher
        use MessageHandling
        use timers

        implicit none

        double precision, dimension(Lnx), intent(in) :: q  !< fluxes
        double precision, dimension(Ndx), intent(in) :: h0 !< layer thickness (m) at start of time interval
        double precision, dimension(Ndx), intent(in) :: h1 !< layer thickness (m) at end of time interval
        double precision, intent(in) :: Dt !< time interval
        integer, parameter :: wp = kind(q)

        integer, dimension(1) :: numremaining ! number of remaining particles to be updated

        double precision :: xx, yy

        integer :: i
        integer :: iter
        integer :: ierror

        integer, parameter :: MAXITER = 1000 ! maximum number of substeps

        integer(4), save :: ithndl = 0    ! handle to time this subroutine
        if (timon) call timstrt("update_particles", ithndl)

        ! reconstruct velocity field
        call reconst_vel(q, h0, h1)

        if (Nopart > 0) then
            ! set remaining time to time step
            dtremaining = Dt
            numzero = 0
        end if

        do iter = 1, MAXITER
            ! update particles in cells
            call update_particles_in_cells(numremaining(1), ierror)
            if (ierror /= 0) then
                if (timon) call timstop (ithndl)
                return
            endif

            write(*, *) 'iter=', iter, 'numremaining=', numremaining(1)
            if (numremaining(1) == 0) then
                write(*, *) 'iter=', iter
                exit
            end if
        end do

        ! check for remaining particles
        if (numremaining(1) > 0) then
            ! plot remaining particles
            do i = 1, Nopart
                if (dtremaining(i) > 0.0_wp .and. mpart(i) > 0) then
                    if (jsferic == 0) then
                        xx = xpart(i)
                        yy = ypart(i)
                    else
                        call Cart3Dtospher(xpart(i), ypart(i), zpart(i), xx, yy, ptref)
                    end if
                    write(*, "(I0, ':', 2E25.15, ', ', I0)") i, xx, yy, mpart(i)
                end if
            end do
            call mess(LEVEL_WARN, 'update_particles: iter>MAXITER')
            if (timon) call timstop (ithndl)
            return
        end if

        if (timon) call timstop (ithndl)
    end subroutine update_particles


    !> update positions of particles within triangles
    subroutine update_particles_in_cells(numremaining, ierror)
        use partmem, only: nopart, mpart, hyd
        use m_fm_aux_routines
        use m_particles, laypart => kpart
        use m_part_recons
        use m_part_mesh
        use MessageHandling
        use m_sferic, only: jsferic
        use m_part_geom, only: lnx
        use timers

        implicit none

        integer, intent(out) :: numremaining !< number of remaining particles to be updated
        integer, intent(out) :: ierror       !< error (1) or not (0)

        integer :: ipart
        integer :: i, k, k1, k2, L, kl
        integer :: ja
        integer :: Lexit

        double precision :: d, un
        double precision :: t, tex, dt
        double precision :: ux0, uy0, uz0, cs, sn
        double precision :: xn, yn, zn, rl
        double precision :: dvar, dis, dn

        double precision, dimension(3) :: ddn

        logical :: isboundary

        double precision, parameter :: DTOL = 1d-4
        double precision, parameter :: DTOLd = 1d-4
        double precision, parameter :: DTOLun_rel = 1d-4
        double precision, parameter :: DTOLun = 1e-14

        integer, parameter :: MAXNUMZERO = 10
        integer, parameter :: wp = kind(dtol)

        double precision :: uw0up, uw0low
        double precision :: alpha, beta, beta1, beta2
        double precision :: time_low, time_up
        integer :: new_layer

        integer(4), save :: ithndl = 0             ! handle to time this subroutine
        if (timon) call timstrt("update_particles_in_cells", ithndl)

        ierror = 0
        numremaining = 0

        dt = 0.0

        !$OMP PARALLEL DO PRIVATE (i, k, kl, k1, k2, L, ja, Lexit, d, un, t, tex, dt,      &
        !$OMP                      ux0, uy0, uz0, cs, sn, xn, yn, zn, rl, dvar, dis, dn,   &
        !$OMP                      ddn, isboundary, alpha, beta, uw0low, uw0up, time_low, time_up, &
        !$OMP                      beta1, beta2, new_layer),                               &
        !$OMP           REDUCTION ( +   : numremaining) ,                                  &
        !$OMP           REDUCTION ( MAX : ierror ),                                        &
        !$OMP           SCHEDULE  ( DYNAMIC, max(Nopart/100,1)           )
        do ipart = 1, Nopart
            ! check if this particle needs to be updated
            if (dtremaining(ipart) == 0.0_wp .or. mpart(ipart) < 1 .or. laypart(ipart) > hyd%num_layers) cycle
            ! get cell (flownode) particle in in
            k = mpart(ipart)
            kl = k + (laypart(ipart) - 1) * numcells

            ! compute exit time <= dtremaining
            tex = dtremaining(ipart)

            Lexit = 0   ! exit edge (flowlink)

            ! compute velocity at current position
            ux0 = u0x(kl) + alphafm(kl) * (xpart(ipart) - xzwcell(k))
            uy0 = u0y(kl) + alphafm(kl) * (ypart(ipart) - yzwcell(k))

            !
            ! First determine at what time the particle may leave
            ! the layer it is in
            !
            new_layer = 0
            uw0low = 0.0d0
            if (laypart(ipart) > 1) then
                uw0low = u0w(kl - numcells)
            endif

            uw0up = 0.0d0
            if (laypart(ipart) < hyd%num_layers) then
                uw0up = u0w(kl)
            endif

            time_up = huge(time_up)
            time_low = huge(time_low)
            alpha = uw0up - uw0low
            beta = 0.0d0

            if (abs(alpha) > dtol) then
                beta = uw0low / alpha

                beta1 = beta / (hpart(ipart) + beta)
                if (beta1 > 0.0_wp) then
                    time_low = log(beta1) / alpha
                endif

                beta2 = (1.0_wp + beta) / (hpart(ipart) - beta)
                if (beta2 > 0.0_wp) then
                    time_up = log(beta2) / alpha
                endif
            else
                if (uw0low > 0.0_wp) then
                    time_up = (1.0_wp - hpart(ipart)) / uw0low
                else
                    time_low = -hpart(ipart) / uw0low
                endif
            endif

            !
            ! If the time left in the time step is larger than the time
            ! required to pass through either the interface aobve or below,
            ! then adjust the exit time.
            !
            ! Make sure the particles do not move beyond the first layer
            ! or the last.
            !
            ! TODO: Take care of z-layers
            !
            if (tex > time_low .and. time_low > 0.0_wp) then
                tex = time_low
                if (laypart(ipart) > 1) then
                    new_layer = -1
                endif
            endif

            if (tex > time_up .and. time_up > 0.0_wp) then
                tex = time_up
                if (laypart(ipart) < hyd%num_layers) then
                    new_layer = +1
                endif
            endif

            !
            ! Now compute the same for the horizontal motion
            !
            if (jsferic /= 0) then
                uz0 = u0z(kl) + alphafm(kl) * (zpart(ipart) - zzwcell(k))
            end if

            ! loop over edges (netlinks) of cells
            do i = jcell2edge(k), jcell2edge(k + 1) - 1
                L = icell2edge(i)   ! edge

                k1 = edge2node(1, L)
                k2 = edge2node(2, L)

                if (jsferic == 0) then
                    cs = dnx(1, L)
                    sn = dny(1, L)
                    if (edge2cell(2, L) == k) then
                        cs = -cs
                        sn = -sn
                    end if
                else
                    if (edge2cell(1, L) == k) then
                        ddn = (/ dnx(1, L), dny(1, L), dnz(1, L) /)
                    else
                        ddn = (/ dnx(2, L), dny(2, L), dnz(2, L) /)
                    end if
                end if

                ! check for boundary edge
                isboundary = (edge2cell(1, L) == 0 .or. edge2cell(2, L) == 0)

                ! compute normal distance to edge
                if (jsferic == 0) then
                    if (isboundary) then ! boundary: add tolerance
                        call dlinedis2(xpart(ipart), ypart(ipart), xnode(k1) + cs * DTOLd, ynode(k1) + sn * DTOLd, xnode(k2) + cs * DTOLd, ynode(k2) + sn * DTOLd, ja, d, xn, yn, rl)
                    else
                        call dlinedis2(xpart(ipart), ypart(ipart), xnode(k1), ynode(k1), xnode(k2), ynode(k2), ja, d, xn, yn, rl)
                    end if
                    dis = (xn - xpart(ipart)) * cs + (yn - ypart(ipart)) * sn

                else
                    if (isboundary) then ! boundary: add tolerance
                        call dlinedis3D(xpart(ipart), ypart(ipart), zpart(ipart), xnode(k1) + DTOLd * ddn(1), &
                                ynode(k1) + DTOLd * ddn(2), &
                                znode(k1) + DTOLd * ddn(3), &
                                xnode(k2) + DTOLd * ddn(1), &
                                ynode(k2) + DTOLd * ddn(2), &
                                znode(k2) + DTOLd * ddn(3), &
                                ja, d, xn, yn, zn, rl)
                    else
                        call dlinedis3D(xpart(ipart), ypart(ipart), zpart(ipart), xnode(k1), ynode(k1), znode(k1), xnode(k2), ynode(k2), znode(k2), ja, d, xn, yn, zn, rl)
                    end if
                    dis = (xn - xpart(ipart)) * ddn(1) + (yn - ypart(ipart)) * ddn(2) + (zn - zpart(ipart)) * ddn(3)
                end if

                ! check inside or outside triangle
                if (dis < -DTOLd .and. .not.isboundary) then
                    ! outside triangle
                    Lexit = L
                    exit
                else
                    ! inside triangle
                    ! compute normal velocity to edge (outward positive)
                    if (jsferic == 0) then
                        un = ux0 * cs + uy0 * sn
                    else
                        un = ux0 * ddn(1) + uy0 * ddn(2) + uz0 * ddn(3)
                    end if

                    if (un > max(DTOLun_rel * d, DTOLun)) then   ! normal velocity does not change sign: sufficient to look at u0.n
                        ! compute exit time for this edge: ln(1+ d/un alpha) / alpha
                        dvar = alphafm(kl) * dis / un
                        if (dvar > -1.0_wp) then
                            t = dis / un
                            if (abs(dvar) >= DTOL) then
                                t = t * log(1d0 + dvar) / dvar
                            end if
                        else
                            t = huge(1d0)
                        end if

                        ! update exit time/edge (flowlink)
                        if (t <= tex) then

                            tex = t
                            Lexit = L
                        end if
                    end if

                end if
            end do

            if (dtremaining(ipart) == 0.0_wp) then
                !continue
                cycle
            end if

            ! compute timestep in cell (flownode)
            dt = min(dtremaining(ipart), tex)

            ! update particle
            if (abs(alphafm(kl)) < DTOL) then
                dvar = dt
            else
                dvar = (exp(alphafm(kl) * dt) - 1d0) / alphafm(kl)
            end if

            xpart(ipart) = xpart(ipart) + dvar * ux0
            ypart(ipart) = ypart(ipart) + dvar * uy0

            if (jsferic /= 0) then
                zpart(ipart) = zpart(ipart) + dvar * uz0
            end if

            ! if the particle does not move to another layer, update
            ! the relative position, otherwise we know exactly where
            ! it ends up.
            if (new_layer == 0) then
                if (abs(alpha) > dtol) then
                    hpart(ipart) = (hpart(ipart) + beta) * exp(alpha * dt) - beta
                else
                    hpart(ipart) = hpart(ipart) + uw0low * dt
                endif
            else
                laypart(ipart) = laypart(ipart) + new_layer
                hpart(ipart) = merge(0.0d0, 1.0d0, new_layer > 0)
                numremaining = numremaining + 1  ! number of remaining particles for next substep
            endif

            ! handle spurious errors - they should not occur, of course
            laypart(ipart) = min(max(laypart(ipart), 1), hyd%num_layers)

            ! update the time that is left within this time step
            dtremaining(ipart) = max(dtremaining(ipart) - dt, 0.0_wp)

            if (dt == 0.0_wp) then
                numzero(ipart) = numzero(ipart) + 1
            end if

            if (numzero(ipart) > MAXNUMZERO) then
                ! disable particle that is not moving
                mpart(ipart) = 0
                dtremaining(ipart) = 0.0_wp

                ! proceed to neighboring cell (if applicable)
            else if (Lexit > 0) then
                numremaining = numremaining + 1  ! number of remaining particles for next substep
                if (edge2cell(1, Lexit) > 0 .and. edge2cell(2, Lexit) > 0) then   ! internal edge (netlink)
                    mpart(ipart) = edge2cell(1, Lexit) + edge2cell(2, Lexit) - k

                    if (mpart(ipart) == 0) then
                        continue
                    else
                        if (jsferic == 1) then
                            ! project node on triangle
                            k = mpart(ipart)
                            k1 = edge2node(1, Lexit)
                            k2 = edge2node(2, Lexit)
                            xn = 0.5d0 * (xnode(k1) + xnode(k2))
                            yn = 0.5d0 * (ynode(k1) + ynode(k2))
                            zn = 0.5d0 * (znode(k1) + znode(k2))
                            dn = (xpart(ipart) - xn) * dnn(1, k) + &
                                    (ypart(ipart) - yn) * dnn(2, k) + &
                                    (zpart(ipart) - zn) * dnn(3, k)
                            xpart(ipart) = xpart(ipart) - dn * dnn(1, k)
                            ypart(ipart) = ypart(ipart) - dn * dnn(2, k)
                            zpart(ipart) = zpart(ipart) - dn * dnn(3, k)
                        end if
                    end if
                else  ! on boundary
                    mpart(ipart) = 0
                end if
            elseif (new_layer == 0) then
                ! safety check - new_layer /= 0 has already been taken care of
                if (dtremaining(ipart) /= 0.0_wp) then
                    ierror = 1
                end if
            end if

        end do
        !$OMP END PARALLEL DO

        if (ierror == 1) then
            call mess(LEVEL_ERROR, 'update_particles_in_cells: dtremaining <> 0', ' ', ' ')
        endif

        if (timon) call timstop (ithndl)
    end subroutine update_particles_in_cells

end module m_fm_update_particles
