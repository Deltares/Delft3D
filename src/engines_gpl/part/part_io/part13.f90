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

module part13_mod

    use m_stop_exit
    use m_part11
    use m_array_manipulation, only: fill_element_dimensions
    use openfl_mod

contains
    subroutine part13 (lun1, lname, lun2, title, subst, &
            lgrid2, num_rows, volume, area, npart, &
            mpart, xpart, ypart, wpart, nopart, &
            itime, idelt, ipset, iptmax, xa, &
            ya, xb, yb, pg, recovr, &
            atotal, iyear, imonth, iofset, npwndw, &
            lgrid, pblay, modtyp, apeak, adepth, &
            num_layers, nosubs, rbuffr, kpart, itrack, &
            nplot, mapsub, ntrack, isfile, num_columns, &
            nfract, use_settling, mstick, elt_names, elt_types, &
            elt_dims, elt_bytes, locdep, zpart, za, &
            dps, tcktot, nosub_max, bufsize)

        !     CREATING PLO FILE FOR PLOT GRID
        !       (Nefis and binary files / per time step)

        !     note                  : delpar sort different from partplot
        !                             partplot:  first layers
        !                             delpar  :  first substances

        !     logical unit numbers  : lun1 - plot grid file
        !                             lun2 - output log file

        !     subroutines called    : part11 - converts model coordinates to
        !                                      national grid and plot coordinates,
        !                             putget
        !                             putget_chars

        !     functions   called    : findcell

        use m_waq_precision          ! single and double precision
        use m_part_modeltypes       ! part model definitions
        use timers
        use putget_mod         ! explicit interface
        use genfil_mod         ! explicit interface
        use delete_file_mod    ! explicit interface
        use grid_search_mod    ! explicit interface
        use filldm_mod         ! explicit interface
        use openfl_mod         ! explicit interface
        use typos

        implicit none

        !     Arguments

        !     kind           function         name                       description

        integer  (int_wp), intent(in) :: lun1                    !< unit nr of the map file
        character(*), intent(in) :: lname                   !< name of the plotgrid-file
        integer  (int_wp), intent(in) :: lun2                    !< unit nr of the log file
        character(40), intent(in) :: title (4)               !< model and run titles
        integer  (int_wp), intent(in) :: nosubs                  !< actual number of substances
        integer  (int_wp), intent(in) :: nosub_max               !< maximum number of substances
        character(*), intent(in) :: subst (nosub_max)       !< substance name and unit specs
        integer  (int_wp), intent(in) :: num_rows                    !< first dimension of lgrid2
        integer  (int_wp), intent(in) :: num_columns                    !< sec. dimension of lgrid2
        integer  (int_wp), intent(in) :: lgrid2(num_rows, num_columns)       !< model grid layout (total)
        integer  (int_wp), intent(in) :: num_layers                   !< actual number of layers
        real     (real_wp), intent(in) :: volume(num_rows * num_columns * num_layers) !< volumes of the lgrid2 cells
        real     (real_wp), intent(in) :: area  (num_rows * num_columns * num_layers) !< horizontal surface areas of the lgrid2 cells
        integer  (int_wp), intent(in) :: nopart                  !< nr of particles
        integer  (int_wp), intent(in) :: npart (nopart)          !< n-values of particles
        integer  (int_wp), intent(in) :: mpart (nopart)          !< m-values of particles
        real     (real_wp), intent(in) :: xpart (nopart)          !< x-values of particles
        real     (real_wp), intent(in) :: ypart (nopart)          !< y-values of particles
        real     (real_wp), intent(in) :: wpart (nosubs, nopart)   !< weights of the particles
        integer  (int_wp), intent(in) :: itime                   !< simulation time
        integer  (int_wp), intent(in) :: idelt                   !< simulation time step
        integer  (int_wp), intent(in) :: iptmax                  !< nr of plot grids
        integer  (int_wp), intent(in) :: ipset (iptmax)          !< plot grid times
        real     (real_wp), intent(out) :: xa    (nopart)          !< national coordinates of parts
        real     (real_wp), intent(out) :: ya    (nopart)          !< national coordinates of parts
        real     (real_wp), intent(in) :: xb    (num_rows * num_columns)       !< x-values of bottom points
        real     (real_wp), intent(in) :: yb    (num_rows * num_columns)       !< y-values of bottom points
        type(PlotGrid), intent(in) :: pg                      !< plot grid information
        real     (real_wp), intent(in) :: recovr(iptmax)          !< recovery for the plots
        real     (real_wp), intent(out) :: atotal(num_layers, nosubs)    !< total per mass per subst/per layer
        integer  (int_wp), intent(in) :: iyear                   !< year offset to real time
        integer  (int_wp), intent(in) :: imonth                  !< month offset to real time
        integer  (int_wp), intent(in) :: iofset                  !< day offset in seconds to real time
        integer  (int_wp), intent(in) :: npwndw                  !< start of active nopart number
        integer  (int_wp), intent(in) :: lgrid (num_rows, num_columns)       !< active grid numbers
        real     (real_wp), intent(in) :: pblay                   !< relative thickness lower layer
        integer  (int_wp), intent(in) :: modtyp                  !< model type
        real     (real_wp), intent(out) :: apeak (nosubs, num_layers)    !< max mass per subst/per layer
        real     (real_wp), intent(out) :: adepth(nosubs, num_layers)    !< depth for max mass
        integer  (int_wp), intent(in) :: bufsize                 !< size of rbuffr
        real     (real_wp) :: rbuffr(bufsize)         !< work storage
        integer  (int_wp), intent(in) :: kpart (nopart)          !< k-values of particles
        integer  (int_wp), intent(in) :: itrack                  !< substance number for tracks
        integer  (int_wp), intent(in) :: ntrack                  !< nr of particles to track
        integer  (int_wp), intent(in) :: nplot (ntrack)          !< particle nr's for particle tracks
        integer  (int_wp), intent(in) :: mapsub(nosubs)          !< substances numbers in map
        integer  (int_wp), intent(in) :: isfile(nosubs)          !< when 1 then from conc array
        integer  (int_wp), intent(in) :: nfract                  !< number of oil fractions
        logical, intent(in) :: use_settling                  !< if .true. then settling in an extra layer
        integer  (int_wp), intent(in) :: mstick(nosubs)          !< sticking oil material if < 0 then sticky
        character(*), pointer :: elt_names(:)            !<  NEFIS
        character(*), pointer :: elt_types(:)            !<  NEFIS
        integer  (int_wp), pointer :: elt_dims (:, :)          !<  NEFIS
        integer  (int_wp), pointer :: elt_bytes(:)            !<  NEFIS
        real     (real_wp) :: locdep(num_rows * num_columns, num_layers)
        real     (real_wp), intent(in) :: zpart (nopart)          !< z-values of particles
        real     (real_wp), intent(out) :: za    (nopart)          !< national coordinates of parts
        real     (real_wp), intent(in) :: dps   (num_rows * num_columns)       !< depth
        real     (real_wp), intent(in) :: tcktot(num_layers + 1)         !< layer thickness
        !
        !     parameters            :
        !
        !     name    kind     length     funct.  description
        !     ====    ====     ======     ======  ===========
        !     amap    real  num_layers*nosubs* in/out  plot grid to be dumped
        !                    nmap*mmap
        !     atrack  real num_layers*nmap*mmap in/out array for particle track
        !     imask   integer  nmap*mmap  input   when 1 then water/when 0 then land
        !     lfimsk  logical     1       local   switch  land mask
        !     mmap    integer     1       input   dimension of amap
        !     nbin    integer     1       in/out  array for number of particles/bin
        !     nmap    integer     1       input   dimension of amap
        !     nosubc  integer     1       input   leading dimension conc-array
        !     surf    real        1       input   surface of a plot grid cell
        !     window  real        4       input   plot grid window
        !     ----    ----     ------     ------  -----------
        !     am      real        1       local   help variable
        !     depthl  real        1       local   depth of layer
        !     fvolum  real        1       local   help var. for volume
        !     first   logical     1       local   switch
        !     first1  logical     1       local   switch 1; general variables
        !     first2  logical     1       local   switch 2; nefis
        !     i       integer     1       local   help variable
        !     i1      integer     1       local   help variable 1
        !     i2      integer     1       local   help variable 2
        !     i3      integer     1       local   help variable 3
        !     i4      integer     1       local   help variable 4
        !     ilay    integer     1       local   layer index
        !     imonth  integer     1       local   month
        !     iofset  integer     1       local   offset in time
        !     ipos    integer     1       local   pointer in layers
        !     ippl    integer     1       local   pointer in layers according to partplot
        !     ipsetx  integer     1       local   plot grid times index to ipset
        !     isub    integer     1       local   substance index
        !     ix      integer     1       local   grid pointer for x dir.
        !     iy      integer     1       local   grid pointer for y dir.
        !     iyear   integer     1       local   year
        !     thickn  real        2       local   ptlay and pblay
        !     windw1  real        1       local   help var for window (speed)
        !     windw3  real        1       local   help var for window (speed)
        !     subnam  char.* 6    1       local   name of this program/file
        !     xpf     real        1       local   delta x/cell for amap
        !     ypf     real        1       local   delta y/cell for amap
        !     zparti  real        1       local   help var. for zpart (speed)
        !
        !
        !     save values between invocations: specified precisely those needed!!
        !
        save           first, type, dname, itoff, ipsetx, &
                windw1, windw3, xpf, ypf, thickn
        !
        !     declarations
        !

        character(len = 20), dimension(1) :: filvers
        character(len = 20), dimension(1) :: dname
        character(len = 20) :: filver

        !
        !     parameters
        !
        character(6), parameter :: subnam = 'part13'
        !
        !     dimensioning
        !
        integer(4), pointer, dimension(:, :) :: imask
        integer(4), pointer, dimension(:, :, :) :: nbin
        real   (4)                                  thickn(2)
        real   (4), pointer, dimension(:, :, :) :: atrack
        real   (4), pointer, dimension(:, :, :, :) :: amap
        !3d
        !
        character(len = 256) :: filnam
        character(len = 20) :: type
        !
        !     declarations for in order to use putget
        !
        integer(4), parameter :: itofmx = 7
        !
        character(len = 4) :: hlptx2
        character(len = 16) :: grnam1, grnam2, grnam3

        integer(4), dimension(6) :: nosize
        integer(4), dimension(itofmx) :: itoff
        real     (sp) :: window(4)              !< plot window

        integer(4), dimension(100) :: nplay
        !
        save          grnam1, grnam2, grnam3, &
                nosize, first1, nefis, &
                celid1, celid2, filnam
        !
        real(4) :: default = 999.999
        !
        !     r.j. vos , following 3 statements copied from code
        !     data
        !
        logical :: wrswch = .true.
        logical :: first = .true.
        logical :: first1 = .true.
        logical :: first2 = .true.
        logical :: nefis = .true.
        integer(4) :: celid1 = 1
        integer(4) :: celid2 = 1
        !
        !     local scalars
        !
        integer(4) :: i, ierr, ierrem, iland, indx, ipsetx, novers
        integer(4) :: i0, i3, i4, iredtq, ilay
        integer(4) :: jsub, i1, i2, iplot, ipos, iseg !   , ippl
        integer(4) :: isub, ix, iy, mod, nelmax, noparm
        integer(4) :: ixc, iyc, mnmapk, mmap
        integer(4) :: nmap
        real   (4) :: ac, act, am, depthl, fvolum
        real   (4) :: surf, windw1, windw3, xpf, ypf
        !
        !     automatic arrays
        !
        character(len = 20), dimension(num_layers) :: units
        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("part13", ithndl)

        !     For the time being

        mmap = pg%mmap
        nmap = pg%nmap
        window(1) = pg%xlow
        window(2) = pg%xhigh
        window(3) = pg%ylow
        window(4) = pg%yhigh
        surf = pg%surf
        amap => pg%amap
        atrack => pg%atrack
        nbin => pg%nbin
        imask => pg%imask
        !
        noparm = 8
        !
        elt_names(1) = 'TYPE'
        elt_names(2) = 'TITLE'
        elt_names(3) = 'SUBST_NAMES'
        elt_names(4) = 'LOCATION_NAMES'
        elt_names(5) = 'SIZES'
        elt_names(6) = 'PLOT_WINDOW'
        elt_names(7) = 'TIME_OFFSET'
        elt_names(8) = 'TIME'
        elt_types(1) = 'CHARACTER'
        elt_types(2) = 'CHARACTER'
        elt_types(3) = 'CHARACTER'
        elt_types(4) = 'CHARACTER'
        elt_types(5) = 'INTEGER'
        elt_types(6) = 'REAL'
        elt_types(7) = 'INTEGER'
        elt_types(8) = 'INTEGER'
        elt_bytes(1) = 20
        elt_bytes(2) = 40
        elt_bytes(3) = 20
        elt_bytes(4) = 20
        elt_bytes(5) = 4
        elt_bytes(6) = 4
        elt_bytes(7) = 4
        elt_bytes(8) = 4
        !
        mnmapk = nmap * mmap * num_layers
        !
        !     nosubs+2 : local depths (per layer) and number of particles
        !
        do i = 1, nosubs + 2
            write (hlptx2, '(i4)') i + 1000
            elt_names(i + noparm) = 'SUBST_' // hlptx2(2:4)
            elt_types(i + noparm) = 'REAL'
            elt_bytes(i + noparm) = 4
        enddo
        !
        !
        if (first1) then
            !
            !       adapt dimensions
            !
            nelmax = noparm + (nosubs + 2) + 1
            !
            !       first inquire file name (via monitoring file)
            !
            inquire(unit = lun2, name = filnam)
            !
            call genfil(filnam, 'plo-', indx)
            call delete_file(filnam(:indx) // '.dat', ierr)
            call delete_file(filnam(:indx) // '.def', ierr)
            first1 = .false.
        endif
        !
        !     end of (former) crdef.inc file
        !
        !     initialize ierrem
        !
        ierrem = 0
        !
        do i = 1, iptmax
            if (ipset(i)  >=  itime) then
                if (ipset(i) - idelt  < itime) then
                    ipsetx = i
                    goto 20
                endif
            endif
        enddo
        !
        goto 9999
        !
        !     compute particle coordinate
        !
        20 call part11(lgrid, xb, yb, num_rows, npart, mpart, &
                xpart, ypart, xa, ya, nopart, npwndw, &
                lgrid2, kpart, zpart, za, locdep, dps, &
                num_layers, num_columns, tcktot)

        !
        !..  rj vos, 25 /11 /1996
        !..  this array should be intilaized at leats the first and second time
        !..  that routine part13 is called. we now do it always
        !
        thickn(1) = 1.0 - pblay
        thickn(2) = pblay
        !
        if (first) then
            !
            !         set logical switch
            !
            first = .false.
            !
            !       open ordinary plo output unit
            !
            write (lun2, *) ' Writing to new plotgrid file:', lname(1:len_trim(lname))
            call openfl (lun1, lname, 1)
            write(lun1) title
            write(lun1) -1, nosubs + 2, nmap, mmap, num_layers, &
                    iyear, imonth, iofset
            write(lun1) window, surf
            write(lun1) (subst(i), i = 1, nosubs + 2)
            !
            !       initialize some variables for within the loop
            !
            windw1 = window(1)
            windw3 = window(3)
            xpf = (window(2) - windw1) / mmap
            ypf = (window(4) - windw3) / nmap
            !
            !..     make a mask array
            !          1 = water
            !          0 = land or outside curvilinear grid
            !
            iland = 0
            do ixc = 1, mmap
                do iyc = 1, nmap
                    if (pg%nmcell(iyc, ixc) /= 0) then
                        imask (iyc, ixc) = 1
                    else
                        imask (iyc, ixc) = 0
                        iland = iland + 1
                    endif
                end do
            end do

            !
            write(lun2, '(6x,a,es15.7,a,es15.7,a,es15.7)')  &
                    'zoom grid resolution: dx = ', xpf, '; dy=', ypf, &
                    ' surface = ', surf
            !
            atrack = 0.0  ! whole array assignment
            !
            if (nefis) then
                !
                !         initialize nefis var's
                !
                !         group names etc.
                !
                grnam1 = 'DELPAR_PARAMS'
                grnam2 = 'DELPAR_RESULTS'
                grnam3 = 'PLO-VERSION'
                type = 'PLO-FILE[PART]'
                !
                !         dummy
                !
                dname(1) = ' '
                !
                !
                !         time off-set
                !
                itoff(1) = iyear
                itoff(2) = imonth
                itoff(3) = iofset
                itoff(4) = ipset(1)
                itoff(5) = ipset(iptmax)
                itoff(6) = 0
                itoff(itofmx) = 0
                !
                !         initialize sizes; 1 - nosubs+2
                !                           2 - mnmaxk
                !                           3 - nodmp (0 for .map)
                !                           4 - num_layers
                !                           5 - nocol (.plo)
                !                           6 - norow (.plo)
                !
                nosize(1) = nosubs + 2
                nosize(2) = 0
                nosize(3) = iptmax
                nosize(4) = num_layers
                nosize(5) = nmap
                nosize(6) = mmap
                !
                mnmapk = nmap * mmap * num_layers
                !
                !
                !         set up the element dimensions
                !
                !         group 1
                !
                call fill_element_dimensions (elt_dims, 1, 1, 1, 0, 0, 0, 0)
                call fill_element_dimensions (elt_dims, 2, 1, 4, 0, 0, 0, 0)
                call fill_element_dimensions (elt_dims, 3, 1, nosubs + 2, 0, 0, 0, 0)
                call fill_element_dimensions (elt_dims, 4, 1, 1, 0, 0, 0, 0)
                call fill_element_dimensions (elt_dims, 5, 1, 6, 0, 0, 0, 0)
                call fill_element_dimensions (elt_dims, 6, 1, 4, 0, 0, 0, 0)
                call fill_element_dimensions (elt_dims, 7, 1, itofmx, 0, 0, 0, 0)
                !
                !         group 2
                !         element time
                call fill_element_dimensions (elt_dims, noparm, 1, 1, 0, 0, 0, &
                        0)
                !         per substance
                do i = 1, nosubs
                    call fill_element_dimensions (elt_dims, noparm + i, 1, mnmapk, 0, &
                            0, 0, 0)
                enddo
                !
                !         local depths per layer
                !
                call fill_element_dimensions (elt_dims, noparm + (nosubs + 1), 1, mnmapk, 0, &
                        0, 0, 0)
                !
                !         number of particles
                !

                call fill_element_dimensions (elt_dims, noparm + (nosubs + 2), 1, mnmapk, 0, &
                        0, 0, 0)
                !
                !         group 3 (version)
                !
                novers = noparm + (nosubs + 2) + 1
                call fill_element_dimensions (elt_dims, novers, 1, 1, 0, 0, 0, 0)
                !
                !         now write nefis header
                !
                !         write all elements to file; all definition and creation of files,
                !         data groups, cells and elements is handled by putget.
                !

                call putget      (filnam, grnam1, noparm - 1, elt_names, elt_dims, &
                        elt_types, elt_bytes, elt_names(1), celid1, wrswch, &
                        ierr, type)
                if (ierr  /=  0) go to 110
                !
                call putget      (filnam, grnam1, noparm - 1, elt_names, elt_dims, &
                        elt_types, elt_bytes, elt_names(2), celid1, wrswch, &
                        ierr, title)
                if (ierr  /=  0) go to 110
                call putget      (filnam, grnam1, noparm - 1, elt_names, elt_dims, &
                        elt_types, elt_bytes, elt_names(3), celid1, wrswch, &
                        ierr, subst)
                if (ierr  /=  0) go to 110
                call putget      (filnam, grnam1, noparm - 1, elt_names, elt_dims, &
                        elt_types, elt_bytes, elt_names(4), celid1, wrswch, &
                        ierr, dname)
                if (ierr  /=  0) go to 110
                call putget     (filnam, grnam1, noparm - 1, elt_names, elt_dims, &
                        elt_types, elt_bytes, elt_names(5), celid1, wrswch, &
                        ierr, nosize)
                if (ierr  /=  0) go to 110
                call putget      (filnam, grnam1, noparm - 1, elt_names, elt_dims, &
                        elt_types, elt_bytes, elt_names(6), celid1, wrswch, &
                        ierr, window)
                if (ierr  /=  0) go to 110
                call putget      (filnam, grnam1, noparm - 1, elt_names, elt_dims, &
                        elt_types, elt_bytes, elt_names(7), celid1, wrswch, &
                        ierr, itoff)
                !
                !         add version record (group 3)
                !
                filver = '2.00.00'
                elt_names(novers) = 'FILE-VERSION'
                elt_types(novers) = 'CHARACTER'
                elt_bytes(novers) = 20
                filvers(1) = filver
                call putget      (filnam, grnam3, 1, &
                        elt_names(novers:), elt_dims(:, novers:), &
                        elt_types(novers:), elt_bytes(novers:), &
                        elt_names(novers), celid1, wrswch, &
                        ierr, filvers(:))
                !
                if (ierr  /=  0) go to 110
                goto 120
                110     ierrem = ierr
            endif

        endif
        !
        !     zero plot grid for all substances
        !
        120 amap = 0.0   ! whole array assignment

        units = ' kg/m3'
        if (use_settling) then
            units(num_layers) = ' kg/m2 (bed layer)'  ! extra bed layer
        endif
        !
        write(*, '(7x,a)', advance = 'no') '  [Writing plo-file ...'
        !
        !     initialize some variables for within the loop
        !
        adepth = 0.0  ! whole array assignment
        atotal = 0.0  ! whole array assignment
        nbin = 0    ! whole array assignment
        !
        do ilay = 1, num_layers
            nplay(ilay) = 0
        end do
        !
        iredtq = mapsub(1)
        !
        iplot = 1
        !
        do i1 = npwndw, nopart
            !
            !       determine if pointers fit in the grid
            !
            ix = int((xa(i1) - windw1) / xpf) + 1
            if (ix  >  0 .and. ix  <=  mmap) then
                iy = int((ya(i1) - windw3) / ypf) + 1
                if (iy  >  0 .and. iy  <=  nmap) then
                    !
                    !           look in active grid layout
                    !
                    i2 = lgrid(npart(i1), mpart(i1))
                    if (i2  >  1) then
                        !
                        !             cell is active
                        !
                        if (area(i2)  /=  0.0) then
                            !
                            !               determine the appropriate layer
                            !
                            ilay = kpart(i1)
                            if(ilay > 0.and.ilay <= num_layers) then
                                nplay(ilay) = nplay(ilay) + 1
                            else
                                write(lun2, *) ' i1, ilay '
                                write(lun2, *) i1, ilay
                                write(lun2, *) 'Error: ilay out of range in PART13'
                                call stop_exit(1)
                            endif
                            !
                            if (modtyp == model_two_layer_temp) then
                                depthl = volume(i2) / area(i2)
                                fvolum = surf * thickn(ilay) * depthl
                            elseif (use_settling .and. ilay == num_layers) then
                                !
                                !.. take here the last but one layer to check active segments
                                !
                                iseg = (ilay - 2) * num_rows * num_columns + i2
                                depthl = volume(iseg) / area(i2)
                                fvolum = surf * depthl
                            else
                                iseg = (ilay - 1) * num_rows * num_columns + i2
                                depthl = volume(iseg) / area(i2)
                                fvolum = surf * depthl
                            endif
                            !
                            if (fvolum  /=  0.0) then
                                !
                                !                 put concentration in it's appropriate layer
                                !
                                do isub = 1, nosubs
                                    if(isfile(isub) /= 1) then
                                        ipos = ilay + (isub - 1) * num_layers
                                        !.. for partplot
                                        am = recovr(ipsetx) * wpart(isub, i1)
                                        !                   atotal(ipos) = atotal(ipos) + am
                                        atotal(ilay, isub) = atotal(ilay, isub) + am
                                        ac = am / fvolum
                                        !
                                        !.. in oil model plot some substances are floating..
                                        !.. also for deposited substances at the bed when use_settling =.true.
                                        !.. then an extra layer is created hereto..
                                        !.. also for sticking materials (mstick(isub) > 0)
                                        !
                                        if(modtyp == model_oil .and. isub <(3 * nfract)) then
                                            !.. oil module
                                            jsub = mod(isub, 3)
                                            if(jsub==0) jsub = 3
                                            !
                                            if(jsub==2) then
                                                !
                                                !.. dispersed
                                                !
                                                if (use_settling .and. ilay == num_layers) then
                                                    ac = am / surf
                                                else
                                                    ac = am / fvolum
                                                endif
                                            elseif(mstick(isub) <0) then
                                                !
                                                !.. sticky always per m2
                                                !
                                                ac = am / surf
                                            else
                                                !
                                                !.. these ones are floating: concentrations per m2
                                                !
                                                ac = am / surf
                                                !
                                            endif
                                            !
                                            !.. settling
                                        elseif (use_settling .and. ilay == num_layers) then
                                            ac = am / surf
                                            !.. stickyness
                                        elseif(mstick(isub) <0) then
                                            ac = am / surf
                                        endif
                                        !
                                        amap(isub, ilay, iy, ix) = amap(isub, ilay, iy, ix) + ac

                                        if (isub == itrack) then
                                            if (i1 == nplot(iplot)) then
                                                act = iplot
                                                !.. only when particle goes to empty space..
                                                if(atrack(ilay, iy, ix) < 1.0e-10) then
                                                    atrack(ilay, iy, ix) = atrack(ilay, iy, ix) + act
                                                endif
                                                write (lun2, 99005) iplot, i1, ix, iy, ilay, act
                                                iplot = iplot + 1
                                            endif
                                        endif
                                        !.. always done
                                        if(isub==1) then
                                            nbin  (ilay, iy, ix) = &
                                                    nbin  (ilay, iy, ix) + 1
                                        endif
                                        if (amap(isub, ilay, iy, ix)  >  apeak(isub, ilay)) then
                                            if(modtyp /= model_red_tide .or. isub /= iredtq) then
                                                if(isub /= ntrack.or.isub /= itrack) then
                                                    apeak(isub, ilay) = amap(isub, ilay, iy, ix)
                                                    adepth(isub, ilay) = depthl
                                                endif
                                            endif
                                        endif
                                    endif
                                end do
                                ! also count number of particles
                                amap(nosubs + 2, ilay, iy, ix) = amap(nosubs + 2, ilay, iy, ix) + 1
                            endif
                        endif
                    endif
                endif
            endif
        end do
        !
        write(lun2, '(/6x,a)') 'Zoom grid: particle distribution per layer'
        do i = 1, num_layers
            write(lun2, '(10x,a,i4,a,i10)')      &
                    'layer ', i, ': Number of particles = ', nplay(i)
        enddo
        !
        !.. tracks???/ bin numbers
        !
        do isub = 1, nosubs
            if(isub==itrack.or.isub==ntrack) then
                do ix = 1, mmap
                    do iy = 1, nmap
                        do ilay = 1, num_layers
                            ipos = ilay + (isub - 1) * num_layers
                            if(isub==ntrack) then
                                amap(isub, ilay, iy, ix) = nbin  (ilay, iy, ix)
                            else
                                amap(isub, ilay, iy, ix) = atrack(ilay, iy, ix)
                            endif
                            if (amap(isub, ilay, iy, ix)  >  apeak(isub, ilay)) then
                                apeak(isub, ilay) = amap(isub, ilay, iy, ix)
                            endif
                        end do
                    end do
                end do
            endif
        end do
        !
        !     add local depths to amap (as extra substance nosub+1)
        !
        do ilay = 1, num_layers
            do ix = 1, mmap
                do iy = 1, nmap
                    i0 = pg%nmcell(iy, ix)
                    if (i0  >  0) then
                        amap(nosubs + 1, ilay, iy, ix) = locdep(i0, ilay)
                    endif
                enddo
            enddo
        enddo
        !
        !     mask concentrations in the inactive grid cells and
        !     grid cells outside the curvilinear grid
        !
        do isub = 1, nosubs + 2
            do ilay = 1, num_layers
                do ix = 1, mmap
                    do iy = 1, nmap
                        if (imask(iy, ix)==0) then
                            amap(isub, ilay, iy, ix) = default
                        endif
                    enddo
                enddo
            enddo
        enddo
        !
        !     check amap array (temp.)
        !
        !     do isub = 1, nosubs+1
        !        write(lun2,'(a,i6)') ' stof ',isub
        !        do ilay = 1, num_layers
        !        write(lun2,'(a,i6)') ' laag ',ilay
        !           ippl = isub  + (ilay - 1) * (nosubs+1)
        !           do ix = 1, mmap
        !              write(lun2,'(a,i6)') ' ix(m) = ',ix
        !              write(lun2,'(4x,10f10.2)') (amap(ippl,iy,ix),iy=1,nmap)
        !           enddo
        !        enddo
        !     enddo
        !
        !     produce a plo record
        !
        write(lun1) itime, &
                ((((amap(isub, ilay, iy, ix), isub = 1, nosubs + 2), &
                        iy = 1, nmap), ix = 1, mmap), ilay = 1, num_layers)

        !    ** test data **
        !     write(lun2,'(a)') ' Part13 - amap array '
        !     do isub = 1,nosubs+2
        !        do iy = 1, nmap
        !           write(lun2,'(2i6,4x,20e12.5)') isub,iy,(amap(isub, num_layers, iy, ix),ix = 1,min(mmap,20))
        !         end do
        !     end do
        !
        !     echo to logging file
        !
        do isub = 1, nosubs
            write(lun2, '(6x,a,a)') 'Zoom grid: results for ', subst(isub)
            do ilay = 1, num_layers
                ipos = ilay + (isub - 1) * num_layers
                write(lun2, '(10x,a,i4,2(a,es15.7),a)')                   &
                        'Layer ', ilay, ': Total mass=', atotal (ilay, isub), &
                        ' kg: Peak conc. (overall) =', apeak(isub, ilay), units(ilay)
            enddo
        enddo

        !
        if (nefis) then
            !
            !       produce a map record for nefis
            !
            if (ierrem == 0) then
                !
                !         no previous nefis errors
                !
                itoff(itofmx) = celid1
                call putget     (filnam, grnam1, noparm - 1, elt_names, elt_dims, &
                        elt_types, elt_bytes, elt_names(7), 1, wrswch, &
                        ierr, itoff)
                if (ierr  /=  0) go to 310
                !
                !         write all elements to file; all definition and creation of
                !         files, data groups, cells and elements is handled by putget.
                !

                call putget    (filnam, grnam2, (nosubs + 2) + 1, &
                        elt_names(noparm:), elt_dims(:, noparm:), elt_types(noparm:), &
                        elt_bytes(noparm:), elt_names(noparm), celid1, &
                        wrswch, ierr, itime)
                if  (ierr  /=  0) go to 310
                celid1 = celid1 + 1
                !
                !         (single loop unrolled into substances and layers - ak )
                !
                do i1 = 1, nosubs + 2
                    i4 = 0
                    do ilay = 1, num_layers
                        do i3 = 1, mmap
                            do i2 = 1, nmap
                                i4 = i4 + 1
                                rbuffr(i4) = amap(i1, ilay, i2, i3)
                            enddo
                        enddo
                    enddo
                    !
                    call putget (filnam, grnam2, (nosubs + 2) + 1, &
                            elt_names(noparm:), elt_dims(:, noparm:), elt_types(noparm:), &
                            elt_bytes(noparm:), elt_names(noparm + i1), celid2, &
                            wrswch, ierr, rbuffr)
                    if (ierr  /=  0) go to 310
                enddo
                celid2 = celid2 + 1
            endif
        endif
        310  ierrem = ierr
        !
        !     check possible nefis errors
        !
        if (ierrem  /=  0) then
            !
            !       echo error to logging file
            !
            write (lun2, 99003)ierrem, subnam, itime / 86400, mod(itime, 86400)   &
                    / 3600, mod(itime, 3600) / 60, mod(itime, 60)
        endif
        !
        !     end of subroutine
        !
        write(*, '(a)') '  ready]'
        9999 if (timon) call timstop (ithndl)
        return
        !

        99003 format(' Error 4802. Writing NEFIS file', i10, '; in ', a6, &
                ' at simulation time :'              &
                , i3, 'd ', i2.2, 'h ', i2.2, 'm ', i2.2, 's !')
        99005 format(' Track no. ', i3, ' part= ', i6, ' at : ', 3(i4, 1x), 'mass = ', &
                e11.4, /)
        !
    end subroutine
end module part13_mod
