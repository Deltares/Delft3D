!!  Copyright (C)  Stichting Deltares, 2012-2025.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

module fm_vert_disp_mod

    implicit none

contains

    subroutine fm_vert_disp (lunpr, itime)


        !       Deltares Software Centre

        !>\file
        !>         Does all the process kinetics associated with oil
        !>
        !>         <ul><li> Initial gravity spreading through radius
        !>         Oil released through dye releases will have an initial gravity spreading at the
        !>         water surface where it floats on. This routine is able (optrad(id) == 1) to
        !>         compute this radius using the Fay-Hoult formula. The actual release, using these
        !>         radius values, takes place in the dye release routine part09.f90.\n
        !>         Estimate of initial radius from adios user's manual (p4.9), NOAA 1994\n
        !>         ref: fay,j. and d.hoult, 1971. 'physical processes in the spread of oil on
        !>         a water surface',report dot-cg-01 381-a. Washington, D.C.: U.S. Coast Guard.
        !>         <li> Volatilisation and emulsification through changes of weigth
        !>         All oil particles have always 3 weight factors<ol>
        !>         <li> floating on the water surface
        !>         <li> dispersed over the water column
        !>         <li> sticking at the bed</ol>
        !>         Depending on the location of the weight of the particle it is succeptible to
        !>         wind and water driven transport, transport in the water only or it is laying on
        !>         the bed.
        !>         <li> Different oil fractions with different characteristics
        !>         It is possible to release different oil fractions that behave differently
        !>         with one particle. The code has had a maximum of 4 fractions. During subsequent
        !>         changes it is tried to remove that maximum and to let it be up to the user.
        !>         If 2 fractions are used, each particle has 6 weight factors, 3 for each fraction.
        !>         Note that only the particles move, so the fractions in a particle move always the
        !>         same. It is therefore recommended to specify multiple batches of particles, one only
        !>         with fraction 1, .. etc.
        !>         <li> Entrainment (emulsification) of oil through sophisticated techniques
        !>         The entrainment of particles from the water surface to the watercolumn is computed
        !>         here. It is possible to specify a constant entrainment factor per day (ioptd(ifrac) == 0).
        !>         It is also possible to use the advanced formula of Delvigne and Sweeny (ioptd(ifrac) == 1).
        !>         Steady state oil distribution approximation from Adios used with maximum droplet
        !>         size of 70 micron. See Adios User's Manual p 4-12.\n
        !>         If a random number is lower than the fraction entrained, the whole floating mass is
        !>         migrated to the watercolumn weight. For enough particles, the net effect is that indeed
        !>         the correct fraction is entrained.\n
        !>         Note that for entrainment unpredictable results are reached if the particle really has
        !>         multiple fractions and one fraction wants to entrain whereas the other wants be remain floating.\n
        !>         Volatilisation only takes place for floating oil. A constant volatilisation rate per day
        !>         is specified for that. This reduces the weight of the particle. The amount of volatised
        !>         oil is also accumulated (like many other characteristics).\n
        !>         <li> Sticking of oil at the water bed through sticking probability
        !>         Whether submerged oil sticks is determined by the stickyness probability. The actual sticking
        !>         takes place in the advection diffusion routine (part10.f90), together with the migration
        !>         of the weight from the dispersed box towards the sticking box.\n
        !>         <li>The 10 coefficients for each fraction of oil are read from the input file and read:<ol>
        !>         <li>evaporating fraction per day
        !>         <li>dispersion option (0=fraction per day; 1=delvigne/sweeny formula)
        !>         <li>dispersion rate per day (if dispersion option = 0)
        !>         <li>stickyness probability [0,1]
        !>         <li>volatile fraction [0,1]
        !>         <li>emulsification parameter c1
        !>         <li>maximum water content c2  [0,1]
        !>         <li>evaporated share at which emulsification starts
        !>         <li>oil density of the fraction
        !>         <li>kinematic viscosity of the fraction</ol>
        !>         <li> More Background:<ul>
        !>         <li> oil dispersion from Delvigne, Roelvink and Sweeney:\n
        !>             'Reseach on vertical turbulent dispersion of oil droplets and oiled particles',\n
        !>              OCS study MMS 86-0029 Anchorage, US Department of the Interior'
        !>         <li> G.A.L. Delvigne and L.J.M.hulsen, AMOP 1994, Vancouver, Canada\n
        !>             'Simplified laboratory measurements of oil dispersion coefficient-application in
        !>              computations of natural oil dispersion' - whitecapping:\n
        !>              Holthuysen and Herbers: J. Phys. Ocean 16,290-7,[1986]
        !>         </ol></ol>

        use m_part_flow, only: h0, h1, num_layers => kmx
        use m_part_times
        use m_part_geom
        use m_part_parameters
        use m_particles, laypart => kpart
        use partmem
        use m_waq_precision
        use m_part_mesh
        use random_generator
        use m_partvs
        use m_part_recons, only: u0x, u0y

        integer(int_wp), intent(in) :: itime                 !< current time in the model
        integer(int_wp), intent(in) :: lunpr
        ! local variables
        real(kind = dp) :: ddfac
        real(kind = dp) :: dran1
        real(kind = dp) :: abuac
        real(kind = dp) :: tp
        real(kind = dp) :: c2g, uscrit, uecrit, ubstar_b  ! critical shear stress parameters
        real(kind = dp), parameter :: gravity = 9.81_dp

        real(kind = dp) :: thicknessl, depthp
        real(kind = dp) :: totdep, reldep
        real(kind = dp) :: dvz, vz
        real(kind = dp) :: rseed = 0.5_dp  ! The function rnd() changes its argument
        real(kind = dp) :: random_step

        integer(int_wp) :: ipart, ilay             ! counters
        integer(int_wp) :: itdelt                  ! delta-t of the particle for smooth loading
        integer(int_wp) :: isub
        integer(int_wp) :: partcel, partlay, cellid, newcell, cellid_in_layer
        integer(int_wp) :: top_layer        ! Index of the highest active layer
        integer(int_wp) :: bottom_layer     ! Index of the lowest active layer
        integer(int_wp), save :: nopart_sed = 0      ! accumulative number of particles in the sediment layer
        integer(int_wp), save :: nopart_sed_old = 0      ! number of particles in the sediment layer (previous timestep)
        real(kind = dp), dimension(noslay) :: totdepthlay             ! total depth (below water surface) of bottom of layers
        real(kind = dp) :: totdepth                ! total water depth at the location of the particles
        real(kind = dp) :: dhpart
        logical :: rise, sink, neutral     ! has the particle a rising or setting speed?

        ddfac = 2.0_dp
        dran1 = drand(1)
        ipart = 1
        nopart_sed_old = nopart_sed
        tp = real(iptime(ipart), kind = kind(tp))
        abuac = abuoy(ipart)
        dran1 = drand(1)
        wsettl = 1.0_dp
        itdelt = idelt
        ! calculate shearstress parameters for sedimentation and erosion
        uscrit = sqrt(taucs / rhow)
        uecrit = sqrt(tauce / rhow)

        ! calculate settling velocity, check what happens if we change wsettl externally, then we do not need to calculate.
        call partvs(lunpr, itime, nosubs, nopart, ivtset, &
                ivtime, vsfour, vsfact, wpart, wsettl, &
                modtyp, 0, ndxi, lgrid3, num_layers, &
                mpart, mpart, laypart, nosegp, noseglp, &
                rhopart, rhowatc, spart, iptime)
        do ipart = 1, nopart
            ! set depth at bottom of layer for all layers)
            ! cellnumaer of particle
            if (mpart(ipart) <= 0 .or. (use_settling .and. laypart(ipart) == noslay)) then
                cycle
            endif
            partcel = abs(cell2nod(mpart(ipart)))  ! the segment number of the layer 1

            !layer number
            partlay = laypart(ipart)

            if (zmodel) then
                cellid_in_layer = 1 + mod(mpart(ipart) - 1, hyd%nosegl)
                top_layer = laytop(1, cellid_in_layer)
                bottom_layer = laybot(1, cellid_in_layer)
            else
                top_layer = 1
                bottom_layer = num_layers
            endif

            totdepthlay(1) = h0(partcel)
            cellid = partcel + (partlay - 1) * hyd%nosegl
            ubstar_b = sqrt(gravity / chezy**2 * (u0x(cellid)**2 + u0y(cellid)**2))
            do ilay = 2, noslay
                if (ilay <= num_layers) then
                    totdepthlay(ilay) = totdepthlay(ilay - 1) + h0(partcel + (ilay - 1) * hyd%nosegl)
                    totdepth = totdepthlay(ilay)
                else
                    totdepthlay(ilay) = totdepthlay(ilay - 1) + 1.0_dp  ! unit depth for the bed layer
                end if
            enddo

            hpart_prevt(ipart) = hpart(ipart)
            vz = wsettl(ipart) !settling is positive
            thicknessl = h0(cellid)
            ! depth of the particle from water surface
            if (laypart(ipart) == 1) then
                depthp = thicknessl * hpart(ipart)
            else
                depthp = totdepthlay(laypart(ipart) - 1) + thicknessl * hpart(ipart)
            endif

            tp = real(iptime(ipart), kind = kind(tp))
            random_step = 2.0_dp * (rnd(rseed) - 0.5_dp)
            if (tp < 0.0_dp) then           !   adaptations because of smooth loading
                tp = 0.0_dp
                itdelt = dts + iptime(ipart)
                ddfac = real(itdelt) / dts
                dran1 = dran1 * sqrt(ddfac)
                abuac = abuac * sqrt(ddfac)
            endif
            dvz = sqrt(6.0_dp * cdisp * itdelt) * random_step + vz * itdelt

            ! note that negative value is now sinking (against the direction of the local h coordinate)
            ! for testing use a fixed downward dispersion displacement (>1)
            ! dvz is the total vertical movement (setting plus diffusion)
            depthp = depthp + dvz  ! depth is positive downwards,and dvz is the increase in depth

            ! new depth is now calculated and now set the layer and hpart or reached top/bottom
            rise = dvz < 0.0_dp
            sink = dvz > 0.0_dp
            neutral = dvz == 0_dp
            dhpart = (dvz - vz) / h0(cellid)
            if (depthp <= 0.0) then
                call  v_part_bounce(ipart, depthp, totdepth, dhpart, top_layer, bottom_layer)
            elseif (depthp >= totdepth) then
                ! this is when the particle hits the bed, but here the bouncing comes in,
                ! if the particle settles then it should become inactive, we are not introducing erosion in FM (for now)
                if (use_settling .and. ubstar_b < uscrit .and. wsettl(ipart) > 0.0_dp) then
                    hpart(ipart) = 1.0_dp
                    laypart(ipart) = noslay  !problem is the z-coordinate of the particle here and the mass in the correct grid cell
                    nopart_sed = nopart_sed + 1
                else
                    call v_part_bounce(ipart, depthp, totdepth, dhpart, top_layer, bottom_layer)   !particles do not settle, but bounce off the bottom.
                endif
            else
                ! find layer starting from partlay and look down if sink or up if rise
                ilay = partlay
                if (sink) then
                    do while (depthp > totdepthlay(ilay))
                        ilay = ilay + 1
                    end do
                elseif (rise) then
                    do while (depthp < totdepthlay(ilay) - h0(cellid))
                        ilay = ilay - 1
                    end do
                endif
                laypart(ipart) = ilay ! new layer number
                newcell = partcel + (ilay - 1) * hyd%nosegl
                hpart(ipart) = 1.0_dp - (totdepthlay(ilay) - depthp) / h0(newcell) ! new relative height in layer
            end if
        end do

        if (use_settling) then
            write(*, 1010) nopart_sed - nopart_sed_old, nopart_sed
            write(lunpr, 1010) nopart_sed - nopart_sed_old, nopart_sed
            1010    format('Settling this timestep:', i6, ', total number of settled particles: ', i6)
        endif
    end subroutine

    subroutine v_part_bounce(ipart, depthp, totdepth, dhpart, top_layer, bottom_layer)

        use m_part_flow, only: h0
        use m_waq_precision       ! single/real(dp)
        use partmem
        use spec_feat_par
        use m_particles, laypart => kpart
        use m_part_geom, only: bl
        use random_generator
        !**      vertically bouncing particles - this routine is adapted from the relevant part of part10

        !**      boundary conditions, check here also settling and erosion
        !**      of particles with critical velocities at the bed

        integer(int_wp), intent(in) :: ipart            ! Particle ID
        real(kind = dp), intent(in) :: depthp           ! Depth at which the particle resides
        real(kind = dp), intent(in) :: dhpart           ! Relative change of the particle's vertical position
        real(kind = dp), intent(in) :: totdepth         ! Total depth of the column of segments
        integer(int_wp), intent(in) :: top_layer        ! Index of the highest active layer
        integer(int_wp), intent(in) :: bottom_layer     ! Index of the lowest active layer

        if (depthp >= totdepth) then
            laypart(ipart) = bottom_layer ! since it hits the bed, the bouncing assumes the layer above the bed
            if (vertical_bounce .and. dhpart > 0.0_dp) then
                ! now it bounces, but only if the diffusion (incl. settling) is directed upward ie dvz<0.
                hpart(ipart) = 1.0_dp + dhpart                                !  now it bounces, but without the settling velocity
            else
                hpart(ipart) = 0.9990_dp                                       !  now it stays near the bottom (no bounce)
            endif
        endif
        if (depthp <= 0.0) then  ! need to bounce at the surface if total displacement is pointed down
            laypart(ipart) = top_layer ! since it hits the surface, surf
            if (vertical_bounce .and. dhpart < 0.0_dp) then
                hpart(ipart) = 0.0_dp - dhpart      !  now it bounces
            else
                hpart(ipart) = 0.0001_dp            !  now it stays near the surface (no bounce)
            endif
        endif

        if (hpart(ipart) == 0.0_dp) then
            hpart(ipart) = 0.0001_dp
        endif
        if (hpart(ipart) == 1.0_dp) then
            hpart(ipart) = 0.9999_dp
        endif

    end subroutine
end module
