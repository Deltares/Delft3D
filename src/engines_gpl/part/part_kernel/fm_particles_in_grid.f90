module m_fm_particles_in_grid

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

    !> add particles
    subroutine add_particles(Nadd, xadd, yadd)
        use partmem, only: nopart, mpart
        use m_particles, laypart => kpart
        use m_alloc
        use m_sferic, only: jsferic
        use geometry_module, only: sphertocart3D
        use timers

        implicit none

        integer, intent(in) :: Nadd       !< number of particles to be added
        double precision, dimension(Nadd), intent(in) :: xadd       !< x-coordinates of particles to be added
        double precision, dimension(Nadd), intent(in) :: yadd       !< y-coordinates of particles to be added

        call calculate_position_in_grid(nadd, xadd, yadd, nopart, xpart, ypart, zpart, mpart)
    end subroutine add_particles


    !> calculate where the particles are within the grid
    subroutine calculate_position_in_grid(Nadd, xadd, yadd, np, xcrd, ycrd, zcrd, kcrd)
        use m_part_mesh
        use m_alloc
        use m_sferic, only: jsferic
        use geometry_module, only: sphertocart3D
        use timers

        implicit none

        integer, intent(in) :: Nadd       !< number of particles to be added
        double precision, dimension(Nadd), intent(in) :: xadd       !< x-coordinates of particles to be added
        double precision, dimension(Nadd), intent(in) :: yadd       !< y-coordinates of particles to be added
        integer, intent(inout) :: np         !< offset in coordinate arrays
        double precision, dimension(*), intent(inout) :: xcrd       !< x-coordinates wrt grid
        double precision, dimension(*), intent(inout) :: ycrd       !< y-coordinates wrt grid
        double precision, dimension(*), intent(inout) :: zcrd       !< z-coordinates wrt grid
        integer, dimension(*), intent(inout) :: kcrd       !< cell number for the particle

        integer, dimension(:), allocatable :: kadd       !< cell numbers

        integer :: i, ipoint, Nsize
        integer :: ierror
        integer :: Nopartnew
        integer :: Nreplace
        integer :: Nloc

        double precision :: xn, yn, zn, dn
        integer :: k, k1

        integer(4) ithndl              ! handle to time this subroutine
        data ithndl / 0 /
        if (timon) call timstrt("calculate_position_in_grid", ithndl)

        ! get new particle cell number
        allocate(kadd(Nadd))
        kadd = 0
        call part_findcell(Nadd, xadd, yadd, kadd, ierror)

        !
        ! Convert to the definitive coordinates. If the location is not in the active grid,
        ! only store the grid cell number as zero, as the error should be handled elsewhere
        !
        do i = 1, Nadd
            Np = Np + 1
            if (kadd(i) > 0) then
                if (jsferic==0) then
                    xcrd(Np) = xadd(i)
                    ycrd(Np) = yadd(i)
                    zcrd(Np) = 0.0d0
                else
                    call sphertocart3D(xadd(i), yadd(i), xcrd(Np), ycrd(Np), zcrd(Np))
                    if (jsferic==1) then
                        ! project particle on triangle
                        k = kadd(i)
                        if (k>0) then
                            k1 = edge2node(1, icell2edge(jcell2edge(k)))
                            xn = xnode(k1)
                            yn = ynode(k1)
                            zn = znode(k1)
                            dn = (xcrd(Np) - xn) * dnn(1, k) + &
                                    (ycrd(Np) - yn) * dnn(2, k) + &
                                    (zcrd(Np) - zn) * dnn(3, k)
                            xcrd(Np) = xcrd(Np) - dn * dnn(1, k)
                            ycrd(Np) = ycrd(Np) - dn * dnn(2, k)
                            zcrd(Np) = zcrd(Np) - dn * dnn(3, k)
                        end if
                    end if

                end if
                kcrd(Np) = kadd(i)
            else
                xcrd(Np) = 0.0
                ycrd(Np) = 0.0
                zcrd(Np) = 0.0
                kcrd(Np) = 0
            end if
        end do

        !  deallocate
        if (allocated(kadd)) deallocate(kadd)

        if (timon) call timstop (ithndl)
    end subroutine calculate_position_in_grid


    !> find in which cells particles are located
    subroutine part_findcellsingle(xxpart, yypart, kpart, ierror)
        use m_part_mesh
        use MessageHandling
        use kdtree2Factory
        use m_alloc
        use m_sferic, only: jsferic, jasfer3D
        use m_missing, only: jins, dmiss
        use geometry_module, only: pinpok, dbdistance, pinpok3D, cart3Dtospher
        implicit none

        double precision, intent(in) :: xxpart   !< particle x-coordinates, 2D Cartexsion or spherical coordinates (not 3D Cartesian)
        double precision, intent(in) :: yypart   !< particle x-coordinates, 2D Cartexsion or spherical coordinates (not 3D Cartesian)
        integer, intent(out) :: kpart    !< cell numbers

        integer, intent(out) :: ierror   !< error (1) or not (0)

        double precision, dimension(:), allocatable, save :: xxzwcell, yyzwcell
        type(kdtree_instance), save :: kdtreecell
        logical, save :: initkdtreecell = .false.

        double precision, dimension(3) :: xv, yv

        double precision :: dmaxsize
        double precision :: R2search
        double precision :: xx, yy

        integer :: i, ip1, j, k, knode, L, Lp1, N, NN, nres
        integer :: inside

        ierror = 1

        ! build kdtree
        if (.not.initkdtreecell) then
            if (jsferic==0) then
                call build_kdtree(kdtreecell, numcells, xzwcell, yzwcell, ierror, 0, dmiss)
            else
                call realloc(xxzwcell, numcells, keepExisting = .false., fill = DMISS)
                call realloc(yyzwcell, numcells, keepExisting = .false., fill = DMISS)
                do i = 1, numcells
                    call Cart3Dtospher(xzwcell(i), yzwcell(i), zzwcell(i), xxzwcell(i), yyzwcell(i), 0d0)
                end do
                call build_kdtree(kdtreecell, numcells, xxzwcell, yyzwcell, ierror, 0, dmiss)
            end if
            if (ierror/=0) then
                return
            end if

            initkdtreecell = .true.
        end if

        kpart = 0

        ! fill query vector
        call make_queryvector_kdtree(kdtreecell, xxpart, yypart, 0)

        ! reallocate if necessary
        NN = min(numcells, 100) ! Was: 100)
        call realloc_results_kdtree(kdtreecell, NN)

        ! find nearest NN samples
        call kdtree2_n_nearest(kdtreecell%tree, kdtreecell%qv, NN, kdtreecell%results)

        ! check if samples are in cell
        !  loop over cells
        do nres = 1, NN
            k = kdtreecell%results(nres)%idx

            ! check cell size
            N = jcell2edge(k + 1) - jcell2edge(k)
            if (N/=3) then
                call mess(LEVEL_ERROR, 'part_findcellsingle: non-triangle')
                return
            end if

            ! get cell polygon
            i = 0
            do j = jcell2edge(k), jcell2edge(k + 1) - 1
                i = i + 1
                L = icell2edge(j)
                ip1 = i + 1; if (ip1>3) ip1 = ip1 - 3
                Lp1 = icell2edge(j - i + ip1)
                !        find common node of L and Lp1
                if (edge2node(1, L)==edge2node(1, Lp1) .or. edge2node(1, L)==edge2node(2, Lp1)) then
                    knode = edge2node(1, L)
                else if (edge2node(2, L)==edge2node(1, Lp1) .or. edge2node(2, L)==edge2node(2, Lp1)) then
                    knode = edge2node(2, L)
                else  ! should not happen
                    continue
                end if
                if (jsferic==0) then
                    xv(i) = xnode(knode)
                    yv(i) = ynode(knode)
                else
                    call Cart3Dtospher(xnode(knode), ynode(knode), znode(knode), xv(i), yv(i), 0d0)
                end if
            end do

            if (jsferic==0) then
                call pinpok(xxpart, yypart, 3, xv, yv, inside, jins, dmiss)
            else
                call pinpok3D(xxpart, yypart, 3, xv, yv, inside, dmiss, jins, jsferic, jasfer3D)
            end if

            if (inside==1) then
                kpart = k
                exit
            end if
        end do

        ierror = 0
    end subroutine part_findcellsingle


    !> find in which cells particles are located
    subroutine part_findcell(Nopart, xxpart, yypart, mpart, ierror)
        use timers

        implicit none

        integer, intent(in) :: Nopart   !< number of particles
        double precision, dimension(Nopart), intent(in) :: xxpart   !< particle x-coordinates, 2D Cartexsion or spherical coordinates (not 3D Cartesian)
        double precision, dimension(Nopart), intent(in) :: yypart   !< particle x-coordinates, 2D Cartexsion or spherical coordinates (not 3D Cartesian)
        integer, dimension(Nopart), intent(out) :: mpart    !< cell numbers

        integer, intent(out) :: ierror   !< error (1) or not (0)

        integer :: i

        integer(4) ithndl              ! handle to time this subroutine
        data ithndl / 0 /
        if (timon) call timstrt("part_findcell", ithndl)

        ierror = 0
        do i = 1, nopart
            call part_findcellsingle(xxpart(i), yypart(i), mpart(i), ierror)
            if (ierror /= 0) then
                exit
            endif
        enddo

        if (timon) call timstop (ithndl)
    end subroutine part_findcell


    !> (re)allocate
    subroutine realloc_particles(Nsize, LkeepExisting, ierror)
        use partmem, only: mpart
        use m_particles, laypart => kpart, laypart_prevt => kpart_prevt
        use m_alloc
        use m_missing
        use m_sferic, only: jsferic
        implicit none

        integer, intent(in) :: Nsize          !< array sizes
        logical, intent(in) :: LkeepExisting  !< keep existing data (1) or not (0)
        integer, intent(out) :: ierror         !< error (1) or not

        ! local
        integer :: npmargin

        ierror = 1
        npmargin = Nsize / 100 + 1 + Nsize
        !  reallocate
        call realloc(xpart, npmargin, keepExisting = LkeepExisting, fill = DMISS)
        call realloc(ypart, npmargin, keepExisting = LkeepExisting, fill = DMISS)
        call realloc(xpart_prevt, npmargin, keepExisting = LkeepExisting, fill = DMISS)
        call realloc(ypart_prevt, npmargin, keepExisting = LkeepExisting, fill = DMISS)
        call realloc(zpart, npmargin, keepExisting = LkeepExisting, fill = DMISS)
        call realloc(zpart_prevt, npmargin, keepExisting = LkeepExisting, fill = DMISS)
        call realloc(dtremaining, npmargin, keepExisting = LkeepExisting, fill = 0d0)
        call reallocp(mpart, npmargin, keepExisting = LkeepExisting, fill = 0)
        call realloc(mpart_prevt, npmargin, keepExisting = LkeepExisting, fill = 0)
        call realloc(laypart, npmargin, keepExisting = LkeepExisting, fill = 0)
        call realloc(laypart_prevt, npmargin, keepExisting = LkeepExisting, fill = 0)
        call realloc(hpart, npmargin, keepExisting = LkeepExisting, fill = DMISS)
        call realloc(hpart_prevt, npmargin, keepExisting = LkeepExisting, fill = DMISS)

        call realloc(numzero, npmargin, keepExisting = LkeepExisting, fill = 0)
        numzero = 0
        ierror = 0
    end subroutine realloc_particles


    !> deallocate particle data
    subroutine dealloc_particles()
        use partmem, only: nopart, mpart
        use m_particles
        implicit none

        if (allocated(xpart)) deallocate(xpart)
        if (allocated(ypart)) deallocate(ypart)
        if (allocated(zpart)) deallocate(zpart)
        if (allocated(dtremaining)) deallocate(dtremaining)
        if (associated(mpart)) deallocate(mpart)
        if (allocated(kpart)) deallocate(kpart)
        if (allocated(hpart)) deallocate(hpart)

        if (allocated(numzero)) deallocate(numzero)

        Nopart = 0

        if (allocated(trpart)) deallocate(trpart)
        if (allocated(xrpart)) deallocate(xrpart)
        if (allocated(yrpart)) deallocate(yrpart)
        if (allocated(zrpart)) deallocate(zrpart)
        if (allocated(krpart)) deallocate(krpart)
        if (allocated(hrpart)) deallocate(hrpart)
        Nrpart = 0
    end subroutine dealloc_particles


    !> initialize particles
    subroutine ini_part(partfile, partrelfile, starttime_loc, timestep_loc, threeDtype_loc)
        use m_fm_aux_routines
        use m_hydmod
        use partmem, only: xwaste, ywaste, zwaste, &
                radius, wparm, iwtime, ictime, amassd, amassc, nodye, nocont, ndprt, nopart, npmax, nosubs, noslay

        use m_particles
        use m_samples
        use m_part_flow
        use m_part_geom, only: Ndx
        use m_part_transport, only: constituents !, numconst
        use m_part_times, only: tstart_user
        use m_missing
        use m_alloc
        use MessageHandling
        use timers
        use m_meteo1temphelpers

        implicit none

        character(len = 255), intent(in) :: partfile      !< initial particle file
        character(len = 255), intent(in) :: partrelfile   !< particle release file
        double precision, intent(in) :: starttime_loc !< start time (>0) or not (0)
        double precision, intent(in) :: timestep_loc  !< time step (>0) or every computational time step (0)
        integer, intent(in) :: threeDtype_loc    !< depth averaged (0) or free surface (1)

        integer :: minp, ierror
        logical :: lexist

        integer(4) ithndl              ! handle to time this subroutine
        data ithndl / 0 /
        if (timon) call timstrt("ini_part", ithndl)

        !  add particle tracer (when tracers are initialized)
        part_iconst = 1
        call realloc(constituents, (/ nosubs, Ndx / kmx, noslay /), keepExisting = .false., fill = 0d0)

        timenext = 0d0
        timelast = DMISS

        !  start time
        if (starttime_loc>0d0) then
            starttime = starttime_loc
            timenext = starttime
        end if

        !  time step
        if (timestep_loc>0d0) then
            timestep = timestep_loc
        end if

        if (len_trim(partfile)>0) then
            !     read initial samples from inputfile
            inquire(FILE = trim(partfile), exist = lexist)
            if (lexist) then
                call oldfil(minp, partfile)
                call savesam()
                call reasam(minp, 0)
                NopartTot = NopartTot + Ns
            else
                call mess(LEVEL_ERROR, 'the specified initial particle locations file could not be found: ', trim(partfile))
            end if
        end if
        if (len_trim(partrelfile)>0) then
            !     read initial samples from inputfile
            inquire(FILE = trim(partrelfile), exist = lexist)
            if (lexist) then
                call read_particles_release_file(partrelfile)
            else
                call mess(LEVEL_ERROR, 'the specified particle release file could not be found: ', trim(partfile))
            end if
        end if

        call realloc_particles(npmax, .true., ierror)
        if (Ns>0) then
            call add_particles(Ns, xs, ys)
            call delsam(0)
        end if
        timepart = tstart_user

        if (timon) call timstop (ithndl)
    end subroutine ini_part


    !> read particles release file
    subroutine read_particles_release_file(partrelfile)
        use m_particles
        use m_missing
        use m_alloc
        use MessageHandling
        use m_meteo1temphelpers
        implicit none

        character(len = 255), intent(in) :: partrelfile   !< release particle file
        character(len = 1000) :: line
        character(len = 1) :: char
        integer :: lun, ios, ipart, linenr, ierror
        double precision :: tr, xr, yr, zr

        call oldfil(lun, partrelfile)

        Nrpart = 0
        linenr = 0
        ios = 0

        do while (ios==0)
            read(lun, '(a1000)', iostat = ios) line
            linenr = linenr + 1
            if (ios==0) then
                read(line, '(a)', iostat = ios) char
                if (char/='*'.and.char/='#'.and.char/='!') then
                    read(line, *, iostat = ios) tr, xr, yr, zr
                    if (ios==0) then
                        Nrpart = Nrpart + 1
                    endif
                endif
            endif
        end do

        if (Nrpart>0) then
            ipart = 0
            linenr = 0
            ios = 0

            rewind (lun)
            call realloc(trpart, Nrpart)
            call realloc(xrpart, Nrpart)
            call realloc(yrpart, Nrpart)
            call realloc(zrpart, Nrpart)
            call realloc(mrpart, Nrpart)

            do while (ios==0)
                read(lun, '(a1000)', iostat = ios) line
                linenr = linenr + 1
                if (ios==0) then
                    read(line, '(a)', iostat = ios) char
                    if (char/='*'.and.char/='#'.and.char/='!') then
                        ipart = ipart + 1
                        read(line, *, iostat = ios) trpart(ipart), xrpart(ipart), yrpart(ipart), zrpart(ipart)
                        if (ios/=0 .or. trpart(ipart)==dmiss .or. xrpart(ipart)==dmiss .or.  yrpart(ipart)==dmiss .or. zrpart(ipart)==dmiss) then
                            call mess(LEVEL_ERROR, 'error reading particle release file ''' // trim(partrelfile) // ''' at line', linenr)
                        endif
                        if (ipart>1) then
                            if (trpart(ipart)<trpart(ipart - 1)) then
                                call mess(LEVEL_ERROR, 'timing in particle release file ''' // trim(partrelfile) // ''' is not incremental at line', linenr)
                            endif
                        endif
                    endif
                endif
            end do
            irpart = 1
            call part_findcell(Nrpart, xrpart, yrpart, mrpart, ierror)
            do ipart = 1, Nrpart
                if (mrpart(ipart)>0) then
                    NopartTot = NopartTot + 1
                endif
            enddo
        endif

        close(lun)
    end subroutine read_particles_release_file


    !> add released particles
    subroutine add_particles_from_release_file(time0)
        use m_waq_precision
        use partmem, only: nopart, mpart
        use partmem, only: iptime
        use m_particles
        use m_part_mesh
        use m_sferic, only: jsferic
        use geometry_module, only: sphertocart3D
        use timers

        implicit none

        double precision, intent(in) :: time0       !< current   julian (s) of h0
        integer :: k, k1
        real(real_wp) :: xn, yn, zn, dn

        integer(4) ithndl              ! handle to time this subroutine
        data ithndl / 0 /

        if (irpart==0 .or. irpart>Nrpart) return

        if (timon) call timstrt("add_particles_from_release_file", ithndl)

        do while (irpart<=Nrpart)
            if (trpart(irpart)>time0) exit  ! assume that the timestamp in the file is in seconds
            if(mrpart(irpart)>0) then
                Nopart = Nopart + 1
                if (jsferic==0) then
                    xpart(Nopart) = xrpart(irpart)
                    ypart(Nopart) = yrpart(irpart)
                else
                    call sphertocart3D(xrpart(irpart), yrpart(irpart), xpart(Nopart), ypart(Nopart), zpart(Nopart))
                    ! project particle on triangle
                    k = mrpart(irpart)
                    if (k>0) then
                        k1 = edge2node(1, icell2edge(jcell2edge(k)))
                        xn = xnode(k1)
                        yn = ynode(k1)
                        zn = znode(k1)
                        dn = (xpart(Nopart) - xn) * dnn(1, k) + &
                                (ypart(Nopart) - yn) * dnn(2, k) + &
                                (zpart(Nopart) - zn) * dnn(3, k)
                        xpart(Nopart) = xpart(Nopart) - dn * dnn(1, k)
                        ypart(Nopart) = ypart(Nopart) - dn * dnn(2, k)
                        zpart(Nopart) = zpart(Nopart) - dn * dnn(3, k)
                    end if
                end if
                mpart(Nopart) = mrpart(irpart)
            end if
            iptime(irpart) = 0.0
            irpart = irpart + 1
        end do

        if (timon) call timstop (ithndl)
    end subroutine add_particles_from_release_file


    subroutine part06fm (lun, nodye, nocont, xwaste, &
            ywaste, zwaste, nwaste, mwaste)

        !>\file
        !>            Determines the grid cells and relative coordinates of waste locations
        !>
        !>            The wastelocations are given by the user in global x,y coordinates.\n
        !>            This routine determines the n,m grid indices and the local x,y coordinates.\n
        !>            The local x,y coordinates are 0< .. <1 and are store in the old x,y locations
        !
        !     Note : we need to be careful about the names nwaste and mwaste, nwaste
        !            is actually a dummy in the case of unstructured grids

        use m_waq_precision
        use timers

        implicit none

        !     Arguments

        !     kind            function         name                   description

        integer  (int_wp), intent(in) :: lun                  !< unit number output log file
        integer  (int_wp), intent(in) :: nodye                !< number of dye releases
        integer  (int_wp), intent(in) :: nocont               !< number of continuous release
        real     (real_wp), intent(inout) :: xwaste(nodye + nocont) !< x of wasteload location
        real     (real_wp), intent(inout) :: ywaste(nodye + nocont) !< y of wasteload location
        real     (real_wp), intent(inout) :: zwaste(nodye + nocont) !< z of wasteload location
        integer  (int_wp), intent(out) :: nwaste(nodye + nocont) !< first grid index wasteloads
        integer  (int_wp), intent(out) :: mwaste(nodye + nocont) !< second grid index wasteloads

        real     (dp) :: xwasted(nodye + nocont) !< x of wasteload location
        real     (dp) :: ywasted(nodye + nocont) !< y of wasteload location
        real     (dp) :: xcrd(nodye + nocont)    !< x coordinate in grid
        real     (dp) :: ycrd(nodye + nocont)    !< y coordinate in grid
        real     (dp) :: zcrd(nodye + nocont)    !< z coordinate in grid
        integer  (int_wp) :: kwaste(nodye + nocont) !< grid index wasteloads

        !     Locals

        integer  (int_wp) :: id      ! loop counter wasteloads
        integer  (int_wp) :: ierror  ! error variable of part07
        real     (real_wp) :: xnloc   ! input x coordinate for the grid search
        real     (real_wp) :: ynloc   ! input y coordinate for the grid search
        real     (real_wp) :: xmloc   ! output x coordinate for the grid search
        real     (real_wp) :: ymloc   ! output y coordinate for the grid search
        integer  (int_wp) :: nmloc   ! output n index of the wasteload point
        integer  (int_wp) :: mmloc   ! output m index of the wasteload point
        integer  (int_wp) :: noerr   ! local error accumulator
        integer  (int_wp) :: np      ! index for storing particle information

        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("part06", ithndl)

        noerr = 0
        kwaste = -1
        np = 0
        xwasted = xwaste
        ywasted = ywaste

        call calculate_position_in_grid(nodye + nocont, xwasted, ywasted, np, xcrd, ycrd, zcrd, kwaste)

        ! Convert to single-precision

        do id = 1, nodye + nocont
            if (kwaste(id) <= 0) then
                if (id > nodye) then
                    write (lun, 1010) id - nodye, xwaste(id), ywaste(id)
                else
                    write (lun, 1000) id, xwaste(id), ywaste(id)
                endif
                noerr = noerr + 1
            endif

            xwaste(id) = xcrd(id)
            ywaste(id) = ycrd(id)
            zwaste(id) = zcrd(id)
            mwaste(id) = kwaste(id)
            nwaste(id) = 1 ! Dummy
        enddo

        if (noerr /= 0) then
            write (lun, 1020) noerr
            stop 1
        endif

        !     end of routine

        if (timon) call timstop (ithndl)

        1000 format('  Error 4901. Dye release', i0, ' at (x,y): (', &
                f9.2, ',', f9.2, ') not on active grid cell.')
        1010 format('  Error 4902. Continuous release', i0, ' at (x,y): (', &
                f9.2, ',', f9.2, ') not on active grid cell.')
        1020 format('  Total number of errors regarding waste loads: ', i0)

    end subroutine part06fm

end module m_fm_particles_in_grid
