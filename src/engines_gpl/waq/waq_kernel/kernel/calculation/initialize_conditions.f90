!!  Copyright (C)  Stichting Deltares, 2012-2024.
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

module initialize_conditions
   use m_waq_precision
   use timers
   use m_string_utils
   use m_zlayer, only: zlayer
   use m_time_dependent_variables, only: update_time_dependent_external_forcing
   use workspace
   use m_delpar00
   use m_attribute_array, only: change_attribute_array
   use m_array_manipulation, only: initialize_real_array, copy_real_array_elements, create_pointer_table
   use m_open_waq_files

   private
   public :: initialize_all_conditions

contains

   !> Initializes all start conditions for simulation
   subroutine initialize_all_conditions(buffer, num_files, max_real_arr_size, max_int_arr_size, max_char_arr_size, &
                                        page_length, file_unit_list, file_name_list, filtype, gridps, dlwqd, ierr)

      !> Performs:
      !>      - calls SPACE to allocate space for all arrays
      !>      - calls initialize_fixed_conditions to initialize all fixed conditions
      !>      - calls initialize_processes to initialize all processes subsystem
      !>      - calls initialize_variables for unclear reasons
      !>      - calls initialize_output to initialize the output system
      !>      - imports all grid informations and exchange tables
      !>      - calls update_time_dependent_external_forcing to initialize all time dependent variables
      !>      - calls expands_vol_area_for_bottom_cells to initialize the water bed layers
      !>      - imports initial conditions

      use m_grid_utils_external
      use variable_declaration
      use delwaq2_data

      use workspace, only: set_array_indexes
      use string_module ! string manipulation tools
      use m_waq_memory_dimensions ! System characteristics
      use m_timer_variables ! Timer characteristics
      use m_real_array_indices ! Pointers in real array workspace
      use m_integer_array_indices ! Pointers in integer array workspace
      use m_character_array_indices ! Pointers in character array workspace

      type(waq_data_buffer), intent(inout) :: buffer !< System total array space
      integer(kind=int_wp), intent(in) :: num_files !< Number of files
      integer(kind=int_wp), intent(inout) :: max_real_arr_size !< dimension   A-array
      integer(kind=int_wp), intent(inout) :: max_int_arr_size !< dimension   J-array
      integer(kind=int_wp), intent(inout) :: max_char_arr_size !< dimension   C-array
      integer(kind=int_wp), intent(in) :: page_length !< pagelength of the output file
      integer(kind=int_wp), intent(inout) :: file_unit_list(num_files) !< array with unit numbers
      character(*), intent(in) :: file_name_list(num_files) !< filenames
      integer(kind=int_wp), intent(in) :: filtype(num_files) !< type of file
      type(gridpointercoll), intent(out) :: gridps !< collection off all grid definitions
      type(delwaq_data), intent(inout) :: dlwqd !< derived type for persistent storage
      integer(kind=int_wp), intent(inout) :: ierr !< error count

      real(kind=real_wp) :: RDUMMY(1)
      logical LDUMMY, UPDATR
      character(len=200) FINAM
      integer(kind=int_wp) :: SENDBUF(3)
      character(len=4) cext ! inital conditions file extention

      integer(kind=int_wp) :: IERRIO, new_lun

      logical propor
      integer(kind=int_wp) :: ithandl = 0

      if (timon) call timstrt("initialize_all_conditions", ithandl)

      !         initialise the system
      ftype = filtype
      call set_array_indexes(file_unit_list(19), .true., buffer%rbuf, buffer%ibuf, buffer%chbuf, &
                             max_real_arr_size, max_int_arr_size, max_char_arr_size)

      associate (a => buffer%rbuf, j => buffer%ibuf, c => buffer%chbuf)
         !
         !     copy common to (possible) shared array to share these values with
         !     other processes (domain decomposition)
         !
         call DHISYS(J(ISYSI:), J(ISYSN:))
         !
         J(ILP) = page_length
         J(ILP + 1) = 10
         J(ILP + 4) = page_length
         J(ILP + 5) = 20

         !     number of all segments - including segments at the bed

         nosss = num_cells + num_cells_bottom ! num_cells_bottom are bed-volumes
         noqtt = num_exchanges + num_exchanges_bottom_dir
         !
         !         initialisation of info from the system file
         !
         call open_waq_files(file_unit_list(2), file_name_list(2), 2, 2, IERRD)
         call initialize_fixed_conditions(file_unit_list, C(IMNAM), C(ISNAM), J(IDUMP:), C(IDNAM), &
                                          J(IDPNT:), J(IVPNT:), A(IDISP:), J(IBPNT:), C(IBNID), &
                                          C(IBNAM), C(IBTYP), J(INTYP:), J(IWAST:), iwstkind, &
                                          C(IWSID), C(IWNAM), C(IWTYP), A(ILENG:), A(ICONS:), &
                                          A(IPARM:), J(INRFT:), J(INRH2:), C(ICNAM), C(IPNAM), &
                                          C(IFNAM), C(ISFNA), C(IDINA), C(IVNAM), J(IKNMR:), &
                                          C(IDANA), J(IPDMP:), J(IQDMP:), J(ISDMP:), C(IRNAM), &
                                          J(IORAA:), J(NQRAA:), J(IQRAA:), J(IGNOS:), J(IGREF:), &
                                          J(IGSEG:), gridps, J(IDMPB:), dlwqd)
         close (file_unit_list(2))
         !
         !     open binary system files for new input processing, if any
         !
         call open_waq_files(file_unit_list(41), file_name_list(41), 41, 1, IERRD)
         if (IERRD == 0) then
            do I = 1, num_unformat_files
               read (file_unit_list(41), *) iftyp, FINAM
               new_lun = 800 + I
               call open_waq_files(new_lun, FINAM, 3, 2 + iftyp, IOERR)
               if (IOERR /= 0) then
                  write (file_unit_list(19), '(A,I3,A,A)') &
                     ' ERROR opening file on unit: ', 800 + I, ' filename: ', FINAM
                  call stop_with_error()
               end if
               ICLEN = len(FINAM)
               do IC = 1, min(ICLEN, 200)
                  C(ILUNT + (I - 1) * 200 + IC - 1) = FINAM(IC:IC)
               end do
            end do
            close (file_unit_list(41))
         end if
         !
         !     initialisation of PROCES subsytem
         !
         if (num_processes_activated > 0) then
            call open_waq_files(file_unit_list(24), file_name_list(24), 24, 2, IERRD)
            call initialize_processes(file_unit_list(24), file_name_list(24), file_unit_list(19), num_substances_total, process_space_int_len, &
                                      num_processes_activated, num_local_vars, num_fluxes, num_defaults, J(INSVA:), &
                                      J(IIFLU:), J(IPVAR:), J(IPTYP:), A(IDEFA:), A(ISTOC:), &
                                      C(IPRNA:), J(IIMOD:), IERR, bloom_status_ind, &
                                      bloom_ind, num_substances_transported, num_dispersion_arrays_extra, num_velocity_arrays_extra, &
                                      A(IDSTO:), A(IVSTO:), num_dispersion_arrays_new, J(IDPNW:), num_velocity_arrays_new, &
                                      J(IVPNW:), num_local_vars_exchange, J(IPGRD:), J(IPNDT:), num_vars, &
                                      J(IVARR:), J(IVIDX:), J(IVTDA:), J(IVDAG:), J(IVTAG:), &
                                      J(IVAGG:), num_input_ref, J(ipror:), j(iprvpt:))
            close (file_unit_list(24))
         end if
         !
         !     Set variable "structure"
         !
         call initialize_variables(file_unit_list(19), num_constants, num_spatial_parameters, num_time_functions, num_spatial_time_fuctions, &
                                   num_substances_transported, num_substances_total, num_dispersion_arrays, num_velocity_arrays, num_defaults, &
                                   num_local_vars, num_dispersion_arrays_extra, num_velocity_arrays_extra, num_local_vars_exchange, num_fluxes, &
                                   NOPRED, num_vars, J(IVARR:), J(IVIDX:), J(IVTDA:), &
                                   J(IVDAG:), J(IVTAG:), J(IVAGG:), num_grids, J(IVSET:))
         !
         !     initialisation of OUTPUT subsytem
         !

         if (num_output_files > 0) then
            call open_waq_files(file_unit_list(25), file_name_list(25), 25, 2, IERRD)
            call initialize_output(file_unit_list(25), file_name_list(25), file_unit_list(19), num_output_files, num_output_variables_extra, &
                                   output_buffer_len, J(IIOUT:), J(IIOPO:), C(IONAM), C(IOSNM), &
                                   C(IOUNI), C(IODSC), num_substances_total, C(ISSNM), C(ISUNI), &
                                   C(ISDSC), file_unit_list, file_name_list, IERR)
            close (file_unit_list(25))
         end if
         !
         !         initialisation of the grid layout
         !
         if (num_cells_u_dir * num_cells_v_dir > 0) then
            call open_waq_files(file_unit_list(6), file_name_list(6), 6, 2, IERRD)
            read (file_unit_list(6)) (J(K), K=IGRID, IGRID + num_cells_u_dir * num_cells_v_dir - 1)
            close (file_unit_list(6))
         end if
         !
         !         initialisation of exchange pointers
         !
         call open_waq_files(file_unit_list(8), file_name_list(8), 8, 2 + ftype(8), IERRD)

         if (num_rows * num_columns > 0) then

            !        read grid, make pointer table

            i1 = ilgra - 1
            read (file_unit_list(8)) nmax2, mmax2, noseg2, kmax2, noq1d, noq2d, noq3d
            read (file_unit_list(8)) (j(i1 + k), k=1, num_columns * num_rows)
            i2 = ikbnd - 1

            call create_pointer_table(num_rows, num_columns, num_layers_grid, num_cells, num_boundary_conditions, &
                                      num_exchanges, num_exchanges_u_dir, num_exchanges_v_dir, j(ilgra:), j(ixpnt:), &
                                      cellpnt, flowpnt)
            finam = file_name_list(8) (1:index(file_name_list(8), '.', .true.))//'cco'
            call open_waq_files(file_unit_list(8), finam, 8, 2 + ftype(8), ierrd)
            read (file_unit_list(8))
            read (file_unit_list(8)) mmax2, nmax2, x0, y0, beta, np2, nlay
            do i = 1, 2 * np2 + 9
               read (file_unit_list(8)) dummy
            end do
            read (file_unit_list(8)) cell_x
            read (file_unit_list(8)) cell_y
         else

            ! Read pointer table with first index 4
            I1 = IXPNT - 1
            do i = 1, noqtt
               read (file_unit_list(8)) (J(I1 + K + (i - 1) * 4), K=1, 4)
            end do
         end if
         close (file_unit_list(8))

         IBFLAG = 0
         if (mod(INTOPT, 16) >= 8) IBFLAG = 1

         !
         ! locally/per processor adapt the feature array:
         !   feature 1 == segment is active segment of own subdomain or not
         !   feature 2 == position w.r.t. the vertical direction
         !   feature 3 == segment is active segment of global domain or not
         !   feature 4 == segment belongs to own processor
         call change_attribute_array(file_unit_list(19), nosss, J(IKNMR:))

         ! determine top of the vertical columns
         call set_cell_top_of_column(nosss, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir, &
                                     j(ixpnt:), j(iknmr:), isegcol)

         ! initial conditions
         propor = .false.
         call open_waq_files(file_unit_list(18), file_name_list(18), 18, 2, ierrd)
         ! look for the file type
         ig = scan(file_name_list(18), '.', back=.true.)
         cext = file_name_list(18) (ig:ig + 3)
         call str_lower(cext)
         ! if .rmp or .rm2 (Sobek) or .map, it is a map-file
         if (cext == '.map' .or. cext == '.rmp' .or. &
             cext == '.rm2') then
            ! read title of simulation
            read (file_unit_list(18), iostat=ierrio) finam(1:160)
            if (ierrio /= 0) goto 50
            !  at end of third line ...
            if (finam(114:120) == 'mass/m2' .or. &
                finam(114:120) == 'MASS/M2') propor = .true.
            ! should be nr. of substance
            read (file_unit_list(18)) idummy
            if (idummy /= num_substances_total) then
               write (file_unit_list(19), '(a,a,/,a,i10)') &
                  ' ERROR reading initial conditions - filename: ', file_name_list(18), &
                  ' Number of substances does not match : ', idummy
               call stop_with_error()
            end if
            ! should be nr. of comp. volumes
            read (file_unit_list(18)) idummy
            if (idummy /= nosss) then
               write (file_unit_list(19), '(a,a,/,a,i10)') &
                  ' ERROR reading initial conditions - filename: ', file_name_list(18), &
                  ' Number of computational volumes does not match : ', idummy
               call stop_with_error()
            end if
            do i = 1, num_substances_total
               read (file_unit_list(18)) finam(1:20)
            end do
         end if
         read (file_unit_list(18), iostat=ierrio) & ! like the .ini, the .res and .wrk file
            idummy, (a(k), k=iconc, iconc + num_substances_total * nosss - 1)
50       if (ierrio /= 0) then
            write (file_unit_list(19), '(a,a)') &
               ' ERROR reading initial conditions - filename: ', &
               file_name_list(18), &
               ' Too few data - file contents does not match current '// &
               'model'
            call stop_with_error()
         else
            read (file_unit_list(18), iostat=ierrio) idummy
            if (ierrio == 0) then
               write (file_unit_list(19), '(a,a)') &
                  ' ERROR reading initial conditions - filename: ', &
                  file_name_list(18), &
                  ' Too many data - file contents does not match '// &
                  'current model'
               call stop_with_error()
            end if
         end if
         close (file_unit_list(18))

         ! first read of relevant time varying arrays
         IFFLAG = 1

         call update_time_dependent_external_forcing(file_unit_list, ITSTRT, ITIMEL, A(IHARM:), A(IFARR:), &
                                                     J(INRHA:), J(INRH2:), J(INRFT:), IDT, A(IVOL:), &
                                                     A(IDIFF:), A(IAREA:), A(IFLOW:), A(IVELO:), A(ILENG:), &
                                                     A(IWSTE:), A(IBSET:), A(ICONS:), A(IPARM:), A(IFUNC:), &
                                                     A(ISFUN:), J(IBULK:), file_name_list, C(ILUNT), ftype, &
                                                     INTSRT, ISFLAG, IFFLAG, IVFLAG, ILFLAG, &
                                                     UPDATR, J(IKTIM:), J(IKNMR:), J(INISP:), A(INRSP:), &
                                                     J(INTYP:), J(IWORK:), .false., LDUMMY, RDUMMY, &
                                                     .true., gridps, DLWQD)

         ! Particle tracking
         call delpar00(file_unit_list(19), file_name_list(45), num_cells, num_exchanges, a(ivol:), a(iflow:), &
                       num_spatial_time_fuctions, c(isfna:), a(isfun:))

         ! New bottomlayer processing
         if (num_exchanges_bottom_dir > 0) &
            call expands_vol_area_for_bottom_cells(file_unit_list, num_cells, num_cells_bottom, num_layers, num_grids, &
                                                   num_exchanges, num_exchanges_bottom_dir, J(IGREF:), J(IGSEG:), num_constants, &
                                                   num_spatial_parameters, num_time_functions, num_spatial_time_fuctions, A(ICONS:), C(ICNAM:), &
                                                   A(IPARM:), C(IPNAM:), A(IFUNC:), C(IFNAM:), A(ISFUN:), &
                                                   C(ISFNA:), J(IXPNT:), A(IVOL:), A(IAREA:), A(IFLOW:), &
                                                   A(ILENG:))
         !

         if (INTSRT == 6 .or. INTSRT == 7) then
            NOSUBz = num_substances_total
         else
            NOSUBz = num_substances_transported
         end if
         call copy_real_array_elements(A(IBSET:), A(IBOUN:), num_boundary_conditions * NOSUBz)
         call copy_real_array_elements(A(IBSET:), A(IBSAV:), num_boundary_conditions * NOSUBz)
         call initialize_real_array(A(IDERV:), num_substances_total * NOSSS)
         call initialize_real_array(A(IMAS2:), num_substances_total * 5)
         call initialize_real_array(A(IWDMP:), num_substances_total * num_waste_loads * 2)
         if (mod(INTOPT, 16) > 7) then
            call initialize_real_array(A(IDMPQ:), num_substances_transported * NDMPQ * 2)
            call initialize_real_array(A(IDMPS:), num_substances_total * num_monitoring_cells * 3)
            call initialize_real_array(A(ISMAS:), num_substances_total * NDMPAR * 6)
            call initialize_real_array(A(IFLXI:), NDMPAR * num_fluxes)
            call initialize_real_array(A(IFLXD:), num_monitoring_cells * num_fluxes)
            call initialize_real_array(A(ITRRA:), num_substances_transported * num_transects)
         end if

         !         make start masses for dynamic and iterative computation

         if (intsrt == 6 .or. intsrt == 7 .or. &
             intsrt == 17 .or. intsrt == 18) goto 40

         !         initial conditions coflowing substances

         do cell_i = 0, nosss - 1
            volume = a(ivol + cell_i)
            do i1 = cell_i * num_substances_total, cell_i * num_substances_total + num_substances_transported - 1
               a(imass + i1) = a(iconc + i1) * volume
            end do
         end do

         !         initial conditions passive substances

         if (num_substances_transported /= num_substances_total) then ! if there are bed-substances
            indx = index_in_array('SURF      ', buffer%create_strings_20_array(ipnam, num_spatial_parameters))
            if (indx > 0) then ! and if SURF is found
               call inact(nosss, num_substances_transported, num_substances_total, a(iconc:), a(imass:), &
                          num_spatial_parameters, indx, a(iparm:), c(imnam + 113), propor, &
                          .true.)
            else ! routine inact is at end of this file !
               indx = index_in_array('SURF      ', buffer%create_strings_20_array(isfna, num_spatial_time_fuctions))
               if (indx > 0) then ! and if SURF is found
                  call inact(nosss, num_substances_transported, num_substances_total, a(iconc:), a(imass:), &
                             num_spatial_time_fuctions, indx, a(isfun:), c(imnam + 113), propor, &
                             .false.)
               else
                  write (file_unit_list(19), '(a,a)') & !   not found
                     ' Error reading initial conditions: ', &
                     ' horizontal surface area not found! '
                  call stop_with_error()
               end if
            end if
         end if

         !         deal with z-layers (inactive cells at the bottom side of the water column
         call zlayer(num_cells, nosss, num_substances_transported, num_substances_total, num_layers, &
                     a(ivol:), num_exchanges_u_dir + num_exchanges_v_dir, num_exchanges, a(iarea:), num_constants, &
                     c(icnam:), a(icons:), num_spatial_parameters, c(ipnam:), a(iparm:), &
                     num_spatial_time_fuctions, c(isfna:), a(isfun:), a(iconc:), a(imass:), &
                     j(iknmr:), iknmkv, j(ixpnt:))

         !     temporary for closure error

40       INDX = index_in_array('CLOSE_ERR ', buffer%create_strings_20_array(ICNAM, num_constants))
         if (INDX > 0) then
            ICFLAG = 1
            write (file_unit_list(19), *) ' Closure error correction enabled'
         else
            ICFLAG = 0
            write (file_unit_list(19), *) ' Closure error correction disabled'
         end if

      end associate

      if (timon) call timstop(ithandl)
      return
   end subroutine initialize_all_conditions

   !> Makes mass/gridcell from mass/m2 for the passive substances
   subroutine inact(num_cells, num_substances_transported, num_substances_total, conc, amass, &
                    num_spatial_parameters, iparm, parm, string, propor, direct)

      integer(kind=int_wp), intent(in) :: num_cells !< number of computational volumes
      integer(kind=int_wp), intent(in) :: num_substances_transported !< number of transported substances
      integer(kind=int_wp), intent(in) :: num_substances_total !< total number of substances
      real(kind=real_wp), intent(inout) :: conc(num_substances_total, num_cells) !< the concentration values
      real(kind=real_wp), intent(out) :: amass(num_substances_total, num_cells) !< the mass values
      integer(kind=int_wp), intent(in) :: num_spatial_parameters !< number of parameters or segment functions
      integer(kind=int_wp), intent(in) :: iparm !< selected parameter
      real(kind=real_wp), intent(in) :: parm(num_spatial_parameters * num_cells) !< parameter or segment function array
      character(1), intent(out) :: string(7) !< model docu substring
      logical, intent(in) :: propor !< if .true. then /m2 in the input
      logical, intent(in) :: direct !< if .false. segments is first index

      integer(kind=int_wp) :: cell_i ! loop counter computational volumes
      integer(kind=int_wp) :: substance_i ! loop counter modelled substances
      real(kind=real_wp) :: surf ! help variable
      integer(kind=int_wp) :: indx ! index

      string(1:7) = ['m', 'a', 's', 's', '/', 'm', '2'] ! always in the output and keep debugger happy
      if (direct) then
         indx = iparm ! parameter
      else
         indx = (iparm - 1) * num_cells + 1 ! segment function
      end if
      do cell_i = 1, num_cells
         surf = parm(indx)
         do substance_i = num_substances_transported + 1, num_substances_total
            if (propor) then ! input / m2
               amass(substance_i, cell_i) = conc(substance_i, cell_i) * surf
            else ! input / gridcell
               amass(substance_i, cell_i) = conc(substance_i, cell_i)
               conc(substance_i, cell_i) = conc(substance_i, cell_i) / surf ! conc  / m2
            end if
         end do
         if (direct) then
            indx = indx + num_spatial_parameters
         else
            indx = indx + 1
         end if
      end do

      return
   end subroutine inact

   !> Reads the DelwaQ binary system file
   !> Initialises all fixed conditions and names for the simulation from the binary system
   subroutine initialize_fixed_conditions(file_unit_list, modid, sysid, idump, dumpid, &
                                          idpnt, ivpnt, disp, ibpnt, bndid, &
                                          bndnam, bndtyp, inwtyp, iwaste, iwsknd, &
                                          wastid, wstnam, wsttyp, aleng, const, &
                                          param, nrftot, nrharm, coname, paname, &
                                          funame, sfname, diname, vename, iknmrk, &
                                          danam, ipdmp, iqdmp, isdmp, ranam, &
                                          ioraai, nqraai, iqraai, grdnos, grdref, &
                                          grdseg, gridps, dmpbal, dlwqd)

      !>                          file at file_unit_list(2).
      !     LOGICAL UNITNUMBERS : file_unit_list( 2) - system intermediate file
      !                           file_unit_list(19) - monitoring output file

      use m_grid_utils_external
      use delwaq2_data
      use m_waq_memory_dimensions ! System characteristics
      use m_timer_variables ! Timer characteristics

      !     PARAMETERS          :
      !
      !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
      !     ----    -----    ------     ------- -----------
      !     file_unit_list     INTEGER       *     INPUT   logical unitnumbers
      !     MODID   CHAR*40       4     OUTPUT  Model and run-ID
      !     SYSID   CHAR*20   num_substances_total     OUTPUT  Systems ID
      !     IDUMP   INTEGER  num_monitoring_points     OUTPUT  Dump segment numbers
      !     DUMPID  CHAR*20  num_monitoring_points     OUTPUT  Dump-segment ID
      !     IDPNT   INTEGER   num_substances_transported     OUTPUT  Pointers to dispersion array
      !     IVPNT   INTEGER   num_substances_transported     OUTPUT  Pointers to velocity array
      !     DISP    REAL          3     OUTPUT  dispersion in 3 directions
      !     IBPNT   INTEGER  4*num_boundary_conditions    OUTPUT  1,* = timelag
      !                                         2,* = flow pointer
      !                                         3,* = segment pointer
      !                                         4,* = time on timelag
      !     BNDID   CHAR*20   num_boundary_conditions     OUTPUT  Open boundary ID's
      !     BNDNAM  CHAR*40   num_boundary_conditions     OUTPUT  Open boundary names
      !     BNDTYP  CHAR*20   num_boundary_conditions     OUTPUT  Open boundary types
      !     INWTYP  INTEGER       *     OUTPUT  Types of items
      !     IWASTE  INTEGER   num_waste_loads     OUTPUT  waste load segment numbers
      integer(kind=int_wp), intent(out) :: iwsknd(*) !  wasteload processing
      !     WASTID  CHAR*20   num_waste_loads     OUTPUT  Waste location ID
      !     WSTNAM  CHAR*40   num_waste_loads     OUTPUT  Waste location names
      !     WSTTYP  CHAR*20   num_waste_loads     OUTPUT  Waste location types
      !     ALENG   REAL        3       OUTPUT  Lengthes in 3 directions
      !     CONST   REAL     num_constants     OUTPUT  value of constants
      !     PARAM   REAL    num_spatial_parameters,num_cells  OUTPUT  value of parameters
      !     NRFTOT  INTEGER  num_items_time_fn     OUTPUT  file lengthes per item
      !     NRHARM  INTEGER  num_items_time_fn     OUTPUT  nr of harmonics per item
      !     CONAME  CHAR*20  num_constants     OUTPUT  Constant names
      !     PANAME  CHAR*20  num_spatial_parameters       OUTPUT  Parameter names
      !     FUNAME  CHAR*20  num_time_functions      OUTPUT  Function names
      !     SFNAME  CHAR*20  num_spatial_time_fuctions     OUTPUT  Segment function names
      !     DINAME  CHAR*20  num_dispersion_arrays     OUTPUT  Dispersion array names
      !     VENAME  CHAR*20  num_velocity_arrays     OUTPUT  Velocity array names
      !     DANAM   CHAR*20  NDMPAR     OUTPUT  Dump-area    ID
      !     IPDMP   INTEGER       *     OUTPUT  pointer structure dump area's
      !     IQDMP   INTEGER       *     OUTPUT  Exchange to dumped exchange pointer
      !     ISDMP   INTEGER       *     OUTPUT  Segment to dumped segment pointer
      !     RANAM   CHAR*20       *     OUTPUT  transects names
      !     IORAAI  INTEGER       *     OUTPUT  option output transects
      !     NQRAAI  INTEGER       *     OUTPUT  number of exch. per transect
      !     IQRAAI  INTEGER       *     OUTPUT  exchange nunbers transect
      !
      !
      !     IN COMMON BLOCK     :
      !
      !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
      !     ----    -----    ------     ------- -----------
      !     num_cells   INTEGER       1     INPUT
      !     num_substances_transported   INTEGER 1   INPUT
      !     num_substances_total   INTEGER       1   INPUT
      !     num_dispersion_arrays  INTEGER       1   INPUT
      !     num_velocity_arrays  INTEGER       1     INPUT
      !     num_exchanges     INTEGER       1     INPUT
      !     num_monitoring_points  INTEGER       1     INPUT
      !     num_boundary_conditions   INTEGER       1     INPUT
      !     num_boundary_types  INTEGER       1     INPUT
      !     num_waste_loads   INTEGER       1     INPUT
      !     num_waste_load_types  INTEGER       1     INPUT   Number of waste load types
      !     num_constants  INTEGER       1     INPUT   Number of constants used
      !     num_spatial_parameters    INTEGER       1     INPUT   Number of parameters
      !     num_time_functions   INTEGER       1     INPUT   Number of functions ( user )
      !     num_spatial_time_fuctions  INTEGER       1     INPUT   Number of segment functions
      !     num_items_time_fn  INTEGER       1     INPUT   Number possible functions
      !     NDMPAR  INTEGER       1     INPUT   Number of dump area's
      !     NTDMPQ  INTEGER       1     INPUT   total number exchanges in dump area
      !     NTDMPS  INTEGER       1     INPUT   total number segments in dump area
      !     num_transects  INTEGER       1     INPUT
      !     num_transect_exchanges  INTEGER       1     INPUT

      integer(kind=int_wp) :: IPDMP(*), IQDMP(*), ISDMP(*), IORAAI(*), &
                              NQRAAI(*), IQRAAI(*), GRDNOS(*), GRDREF(*), &
                              IDUMP(*), IDPNT(*), IVPNT(*), IBPNT(4, *), &
                              IWASTE(*), NRFTOT(*), NRHARM(*), file_unit_list(*), &
                              IKNMRK(*), INWTYP(*)
      integer(kind=int_wp) :: GRDSEG(num_cells + num_cells_bottom, num_grids)
      character(len=40) MODID(4), BNDNAM(*), WSTNAM(*)
      character(len=20) SYSID(*), DUMPID(*), BNDID(*), BNDTYP(*), &
         WASTID(*), WSTTYP(*), CONAME(*), PANAME(*), &
         FUNAME(*), SFNAME(*), DINAME(*), VENAME(*), &
         DANAM(*), RANAM(*)
      real(kind=real_wp) :: DISP(*), ALENG(*), CONST(*), PARAM(*)
      character(len=40) FILLER
      type(GridPointerColl), intent(inout) :: GridPs !< definitions of the grids
      type(delwaq_data), intent(inout) :: dlwqd !< derived type for persistent storage
      integer(kind=int_wp) :: dmpbal(*) !< indicates if dump area is included in the balance
      type(t_grid) :: aGrid ! a single grid

      integer(kind=int_wp) :: it, noqtt, nosss, k, igrid, iin, cell_i, ierror, i_grid
      integer(kind=int_wp) :: substance_i, ix, i, idummy
      integer(kind=int_wp) :: ithandl = 0
      if (timon) call timstrt("initialize_fixed_conditions", ithandl)

      IT = 0
      ! read from the system file
      NOQTT = num_exchanges + num_exchanges_bottom_dir
      NOSSS = num_cells + num_cells_bottom
      IIN = file_unit_list(2)
      read (IIN, end=40, ERR=40) MODID(1), MODID(2)
      read (IIN, end=40, ERR=40) MODID(3), MODID(4)
      read (IIN, end=40, ERR=40) (SYSID(K), K=1, num_substances_total)
      if (num_monitoring_points > 0) &
         read (IIN, end=40, ERR=40) (IDUMP(K), DUMPID(K), K=1, num_monitoring_points)
      if (NDMPAR > 0) &
         read (IIN, end=40, ERR=40) (DANAM(K), K=1, NDMPAR)
      if (NDMPAR > 0) &
         read (IIN, end=40, ERR=40) (DMPBAL(K), K=1, NDMPAR)
      if (num_transects > 0) &
         read (IIN, end=40, ERR=40) (RANAM(K), K=1, num_transects)

      ! sub-grid
      do IGRID = 1, num_grids
         read (IIN, end=40, ERR=40) GRDNOS(IGRID), GRDREF(IGRID), &
            (GRDSEG(cell_i, IGRID), cell_i=1, NOSSS)
      end do
      !     the grid structures
      do IGRID = 1, num_grids
         ierror = aGrid%read(iin, nosss)
         if (ierror /= 0) goto 40
         i_grid = GridPs%add(aGrid)
      end do
      read (IIN, end=40, ERR=40) (IDUMMY, substance_i=1, num_substances_total)
      read (IIN, end=40, ERR=40) (IDUMMY, substance_i=1, num_substances_total)
      read (IIN, end=40, ERR=40) (IKNMRK(K), K=1, NOSSS)
      if (num_dispersion_arrays > 0) &
         read (IIN, end=40, ERR=40) (DINAME(K), K=1, num_dispersion_arrays)
      if (num_velocity_arrays > 0) &
         read (IIN, end=40, ERR=40) (VENAME(K), K=1, num_velocity_arrays)
      read (IIN, end=40, ERR=40) (IDPNT(K), K=1, num_substances_transported)
      read (IIN, end=40, ERR=40) (IVPNT(K), K=1, num_substances_transported)
      if (num_boundary_conditions > 0) then
         read (IIN, end=40, ERR=40) (IBPNT(2, K), K=1, num_boundary_conditions)
         read (IIN, end=40, ERR=40) (IBPNT(3, K), K=1, num_boundary_conditions)
      end if
      if (NDMPAR > 0) then
         read (IIN, end=40, ERR=40) (IPDMP(K), K=1, NDMPAR + NTDMPQ)
         IX = NDMPAR + NTDMPQ
         read (IIN, end=40, ERR=40) (IPDMP(IX + K), K=1, NDMPAR + NTDMPS)
      end if
      if (num_transects > 0) then
         read (IIN, end=40, ERR=40) (IORAAI(K), K=1, num_transects)
         read (IIN, end=40, ERR=40) (NQRAAI(K), K=1, num_transects)
         read (IIN, end=40, ERR=40) (IQRAAI(K), K=1, num_transect_exchanges)
      end if
      if (num_transects > 0 .or. NDMPAR > 0) then
         read (IIN, end=40, ERR=40) (IQDMP(K), K=1, NOQTT)
      end if
      if (NDMPAR > 0) then
         read (IIN, end=40, ERR=40) (ISDMP(K), K=1, NOSSS)
      end if
      read (IIN, end=40, ERR=40) IDUMMY, (DISP(K), K=1, 3)
      read (IIN, end=40, ERR=40) IDUMMY, (ALENG(K), K=1, 3)
      if (num_boundary_conditions > 0) then
         do I = 1, num_boundary_conditions
            read (IIN, end=40, ERR=40) BNDID(I), BNDNAM(I)
         end do
         read (IIN, end=40, ERR=40) (BNDTYP(K), K=1, num_boundary_types)
         read (IIN, end=40, ERR=40) (INWTYP(K + IT), K=1, num_boundary_conditions)
         IT = IT + num_boundary_conditions
         !          read time lags
         read (IIN, end=40, ERR=40) (IBPNT(1, K), K=1, num_boundary_conditions)
      end if
      if (num_waste_loads > 0) then
         do I = 1, num_waste_loads
            read (IIN, end=40, ERR=40) IWASTE(I), iwsknd(i), &
               WASTID(I), WSTNAM(I)
         end do
         read (IIN, end=40, ERR=40) (WSTTYP(K), K=1, num_waste_load_types)
         read (IIN, end=40, ERR=40) (INWTYP(K + IT), K=1, num_waste_loads)
         IT = IT + num_waste_loads
      end if
      if (num_constants > 0) then
         read (IIN, end=40, ERR=40) (CONAME(K), K=1, num_constants)
      end if
      if (num_spatial_parameters > 0) then
         read (IIN, end=40, ERR=40) (PANAME(K), K=1, num_spatial_parameters)
      end if
      if (num_time_functions > 0) then
         read (IIN, end=40, ERR=40) (FUNAME(K), K=1, num_time_functions)
      end if
      if (num_spatial_time_fuctions > 0) then
         read (IIN, end=40, ERR=40) (SFNAME(K), K=1, num_spatial_time_fuctions)
      end if
      !
      !     Time function info
      !
      read (IIN, end=40, ERR=40) (NRFTOT(K), K=1, num_items_time_fn)
      read (IIN, end=40, ERR=40) (NRHARM(K), K=1, num_items_time_fn)
      !
      !         boundary timings greater then timelag
      !
      do I = 1, num_boundary_conditions
         IBPNT(4, I) = IBPNT(1, I) + 1
      end do
      !
      !         extract reference date and time
      !
      call MODIFIED_JULIAN(MODID(4))
      dlwqd%otime = otime
      dlwqd%tscale = tscale
      !
      !         completion successful
      !
      write (file_unit_list(19), 2000) (MODID(K), K=1, 4)
      if (timon) call timstop(ithandl)
      return
      !
      !         unsuccessful read
      !
40    write (file_unit_list(19), 2010)
      call stop_with_error()
      !
      !         output formats
      !
2000  format(' ', 20x, A40 / 21x, A40//21x, A40 / 21x, A40// &
             21x, 'Initialisation from system file completed.')
2010  format('   ERROR reading binary system file !!'/ &
             '   initialisation NOT successful    !!'/ &
             '   simulation impossible            !!')
      !
   contains
      subroutine MODIFIED_JULIAN(T0STRING)

         character(LEN=*) :: T0STRING

         integer(kind=int_wp) :: IYEAR, IMONTH, IDAY, IHOUR, IMIN, ISEC, ISCALE
         integer(kind=int_wp) :: IERR
         real(kind=dp) :: TEMP1, TEMP2

         real(kind=dp), parameter :: MODIFICATION_OFFSET = 2400000.5d0

         TSCALE = 1.0d0

         read (T0STRING, '(4x,i4.4,x,i2.2,x,i2.2,x,i2.2,x,i2.2,x,i2.2,7x,i8)', IOSTAT=IERR) &
            IYEAR, IMONTH, IDAY, IHOUR, IMIN, ISEC, ISCALE

         if (IERR /= 0) then
            IYEAR = 1900
            IMONTH = 1
            IDAY = 1
            IHOUR = 0
            IMIN = 0
            ISEC = 0
            ISCALE = 1
         end if

         TEMP1 = int((IMONTH - 14.0d0) / 12.0d0)
         TEMP2 = IDAY - 32075.0d0 + &
                 int(1461.0d0 * (IYEAR + 4800.0d0 + TEMP1) / 4.0d0) + &
                 int(367.0d0 * (IMONTH - 2.0d0 - TEMP1 * 12.0d0) / 12.0d0) - &
                 int(3.0d0 * int((IYEAR + 4900.0d0 + TEMP1) / 100.0d0) / &
                     4.0)
         TEMP1 = FLOAT(IHOUR) * 3600.0 + &
                 FLOAT(IMIN) * 60.0 + &
                 FLOAT(ISEC) - 43200.0
         OTIME = TEMP2 + (TEMP1 / 86400.0) - MODIFICATION_OFFSET
         TSCALE = 86400.0d0 / ISCALE
      end subroutine MODIFIED_JULIAN

   end subroutine initialize_fixed_conditions

    !! Initialisation of PROCES system.
   subroutine initialize_processes(lunwrp, lch, lurep, num_substances_total, process_space_int_len, &
                                   num_processes_activated, num_local_vars, num_fluxes, num_defaults, prvnio, &
                                   iflux, prvvar, prvtyp, defaul, stochi, &
                                   pronam, imodu, ierr, bloom_status_ind, &
                                   bloom_ind, num_substances_transported, num_dispersion_arrays_extra, num_velocity_arrays_extra, &
                                   dsto, vsto, num_dispersion_arrays_new, idpnw, num_velocity_arrays_new, &
                                   ivpnw, num_local_vars_exchange, progrd, prondt, num_vars, &
                                   vararr, varidx, vartda, vardag, vartag, &
                                   varagg, num_input_ref, proref, prvpnt)

      !
      !     FILES               : LUNWRP, Proces work file
      !                           LUREP , Monitoring file
      !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
      !     ----    -----    ------     ------- -----------
      !     LUNWRP  INTEGER       1     INPUT   Proces work file
      !     LCH     CHA*(*)       1     INPUT   Name proces work file
      !     LUREP   INTEGER       1     INPUT   Monitoring file
      !     num_substances_total   INTEGER       1     INPUT   Number of substances
      !     process_space_int_len  INTEGER       1     INPUT   Length process_space_int
      !     num_processes_activated   INTEGER       1     INPUT   Number of called processes
      !     num_local_vars   INTEGER       1     INPUT   Number of local proces params
      !     num_fluxes   INTEGER       1     INPUT   total number of fluxes
      !     num_defaults   INTEGER       1     INPUT   Number of used defaults
      !     PRVNIO  INTEGER       *     OUTPUT  Number of variables per proces
      !     IFLUX   INTEGER       *     OUTPUT  Pointer in FLUX per proces inst.
      !     process_space_int   INTEGER       *     OUTPUT  Pointer in SSA per proces inst.
      !     IPSSA   INTEGER       *     OUTPUT  Pointer to SSA per proces inst.
      !     DEFAUL  REAL          *     OUTPUT  Default proces parameters
      !     STOCHI  REAL          *     OUTPUT  Proces stochiometry
      !     PRONAM  CHA*(*)       *     OUTPUT  Name of called module
      !     IMODU   INTEGER       *     OUTPUT  Module number proces
      !     IERR    INTEGER       1     IN/OUT  Error count
      !     bloom_status_ind  INTEGER       1     INPUT   Number of Bloom module (if >0)
      !     bloom_ind  INTEGER       1     INPUT   Offset in process_space_int for Bloom
      !     num_substances_transported   INTEGER       1     INPUT   Number of active substances
      !     num_dispersion_arrays_extra   INTEGER      1     INPUT
      !     num_velocity_arrays_extra   INTEGER       1     INPUT
      !     DSTO    INTEGER num_substances_transported,*     OUTPUT  dispersion stochi matrix
      !     VSTO    INTEGER num_substances_transported,*     OUTPUT  velocity stochi matrix
      !     num_dispersion_arrays_new   INTEGER       1     INPUT   Number of new dispersion array
      !     IDPNW   INTEGER   num_substances_transported     OUTPUT  Pointers to new dispersion array
      !     num_velocity_arrays_new   INTEGER       1     INPUT
      !     IVPNW   INTEGER   num_substances_transported     OUTPUT  Pointers to new velocity array
      !     PROGRD  INTEGER   num_processes_activated     OUTPUT  Grid number for process
      !     PRONDT  INTEGER   num_processes_activated     OUTPUT  Fractional step for process

      use process_registration

      integer(kind=int_wp) :: LUNWRP, LUREP, num_substances_total, process_space_int_len, num_processes_activated, &
                              num_local_vars, num_fluxes, num_defaults, bloom_status_ind, &
                              bloom_ind, num_substances_transported, num_dispersion_arrays_extra, num_velocity_arrays_extra, &
                              num_dispersion_arrays_new, num_velocity_arrays_new, num_vars, num_input_ref
      integer(kind=int_wp) :: PRVNIO(*), IFLUX(*), PRVVAR(*), &
                              PRVTYP(*), IMODU(*), IDPNW(*), &
                              IVPNW(*), PROGRD(*), PRONDT(*), &
                              VARARR(*), VARIDX(*), VARTDA(*), &
                              VARDAG(*), VARTAG(*), VARAGG(*), &
                              proref(*), prvpnt(*)
      real(kind=real_wp) :: DEFAUL(*), STOCHI(*), DSTO(*), &
                            VSTO(*)
      character(len=*) LCH
      character(len=10) PRONAM(*)
      !
      !     Local declarations
      integer(kind=int_wp) :: NIPMSD, NPROCD, NOLOCD, NFLUXD, NODEFD, &
                              NOTOTD, IOFF, NOSYSD, num_dispersion_arrays_extraD, num_velocity_arrays_extraD, &
                              num_local_vars_exchangeD, num_dispersion_arrays_newD, num_velocity_arrays_newD, NOVARD, nrrefD
      real(kind=real_wp) :: VERSIO

      integer(kind=int_wp) :: k, ierr, num_local_vars_exchange, iproc, ifracs, ipdgrd

      ! Store fractional step flag in common CFRACS
      common / CFRACS / IFRACS
      integer(kind=int_wp) :: ithandl = 0
      if (timon) call timstrt("initialize_processes", ithandl)

      ! read and check version number
      read (LUNWRP, ERR=900, end=900) VERSIO
      !
      !     read and check dimensions
      !
      read (LUNWRP, ERR=900, end=900) NIPMSD, NPROCD, NFLUXD, &
         NOLOCD, NODEFD, NOTOTD, &
         NOSYSD, num_dispersion_arrays_extraD, num_velocity_arrays_extraD, &
         num_local_vars_exchangeD, num_dispersion_arrays_newD, num_velocity_arrays_newD, &
         NOVARD, nrrefD
      if (NIPMSD /= process_space_int_len) then
         write (LUREP, 2020) NIPMSD, process_space_int_len
         IERR = IERR + 1
      end if
      if (NPROCD /= num_processes_activated) then
         write (LUREP, 2030) NPROCD, num_processes_activated
         IERR = IERR + 1
      end if
      if (NFLUXD /= num_fluxes) then
         write (LUREP, 2040) NFLUXD, num_fluxes
         IERR = IERR + 1
      end if
      if (NOLOCD /= num_local_vars) then
         write (LUREP, 2050) NOLOCD, num_local_vars
         IERR = IERR + 1
      end if
      if (NODEFD /= num_defaults) then
         write (LUREP, 2060) NODEFD, num_defaults
         IERR = IERR + 1
      end if
      if (NOTOTD /= num_substances_total) then
         write (LUREP, 2070) NOTOTD, num_substances_total
         IERR = IERR + 1
      end if
      if (NOSYSD /= num_substances_transported) then
         write (LUREP, 2120) NOSYSD, num_substances_transported
         IERR = IERR + 1
      end if
      if (num_dispersion_arrays_extraD /= num_dispersion_arrays_extra) then
         write (LUREP, 2130) num_dispersion_arrays_extraD, num_dispersion_arrays_extra
         IERR = IERR + 1
      end if
      if (num_velocity_arrays_extraD /= num_velocity_arrays_extra) then
         write (LUREP, 2140) num_velocity_arrays_extraD, num_velocity_arrays_extra
         IERR = IERR + 1
      end if
      if (num_local_vars_exchangeD /= num_local_vars_exchange) then
         write (LUREP, 2150) num_local_vars_exchangeD, num_local_vars_exchange
         IERR = IERR + 1
      end if
      if (num_dispersion_arrays_newD /= num_dispersion_arrays_new) then
         write (LUREP, 2160) num_dispersion_arrays_newD, num_dispersion_arrays_new
         IERR = IERR + 1
      end if
      if (num_velocity_arrays_newD /= num_velocity_arrays_new) then
         write (LUREP, 2170) num_velocity_arrays_newD, num_velocity_arrays_new
         IERR = IERR + 1
      end if
      if (NOVARD /= num_vars) then
         write (LUREP, 2190) NOVARD, num_vars
         IERR = IERR + 1
      end if
      if (nrrefD /= num_input_ref) then
         write (LUREP, 2200) nrrefd, num_input_ref
         IERR = IERR + 1
      end if
      if (IERR > 0) goto 910
      !
      read (LUNWRP, ERR=900, end=900) (PRVNIO(K), K=1, num_processes_activated)
      read (LUNWRP, ERR=900, end=900) (IFLUX(K), K=1, num_processes_activated)
      read (LUNWRP, ERR=900, end=900) (PRVVAR(K), K=1, process_space_int_len)
      read (LUNWRP, ERR=900, end=900) (PRVTYP(K), K=1, process_space_int_len)
      read (LUNWRP, ERR=900, end=900) (DEFAUL(K), K=1, num_defaults)
      read (LUNWRP, ERR=900, end=900) (STOCHI(K), K=1, num_substances_total * num_fluxes)
      read (LUNWRP, ERR=900, end=900) (DSTO(K), K=1, num_substances_transported * num_dispersion_arrays_extra)
      read (LUNWRP, ERR=900, end=900) (VSTO(K), K=1, num_substances_transported * num_velocity_arrays_extra)
      if (num_dispersion_arrays_new > 0) then
         read (LUNWRP, ERR=900, end=900) (IDPNW(K), K=1, num_substances_transported)
      end if
      if (num_velocity_arrays_new > 0) then
         read (LUNWRP, ERR=900, end=900) (IVPNW(K), K=1, num_substances_transported)
      end if
      read (LUNWRP, ERR=900, end=900) (PRONAM(K), K=1, num_processes_activated)
      read (LUNWRP, ERR=900, end=900) (PROGRD(K), K=1, num_processes_activated)
      read (LUNWRP, ERR=900, end=900) (PRONDT(K), K=1, num_processes_activated)
      read (LUNWRP, ERR=900, end=900) (VARARR(K), K=1, num_vars)
      read (LUNWRP, ERR=900, end=900) (VARIDX(K), K=1, num_vars)
      read (LUNWRP, ERR=900, end=900) (VARTDA(K), K=1, num_vars)
      read (LUNWRP, ERR=900, end=900) (VARDAG(K), K=1, num_vars)
      read (LUNWRP, ERR=900, end=900) (VARTAG(K), K=1, num_vars)
      read (LUNWRP, ERR=900, end=900) (VARAGG(K), K=1, num_vars)
      read (lunwrp, err=900, end=900) (proref(k), k=1, num_processes_activated * num_input_ref)
      k = 1
      do iproc = 1, num_processes_activated
         prvpnt(iproc) = k
         k = k + prvnio(iproc)
      end do
      !
      !     Set module numbers
      !
      do K = 1, num_processes_activated
         call PRONRS(PRONAM(K), IMODU(K))
      end do
      !
      !     Report on process decomposition
      !
      IFRACS = 0
      IPDGRD = 0
      do K = 1, num_processes_activated
         if (PRONDT(K) > 1) then
            IFRACS = 1
         end if
         if (PROGRD(K) > 1) then
            IPDGRD = 1
         end if
      end do
      if (IFRACS == 0 .and. IPDGRD == 0) then
         write (LUREP, 3010)
      else
         write (LUREP, 3020)
         do K = 1, num_processes_activated
            write (LUREP, 3000) PRONAM(K), PROGRD(K), PRONDT(K)
         end do
      end if
      !
      !     Check for Bloom connection
      !
      bloom_status_ind = 0
      bloom_ind = 0
      IOFF = 1
      do K = 1, num_processes_activated
         if (PRONAM(K) (1:6) == 'D40BLO') then
            bloom_status_ind = K
            bloom_ind = IOFF
            write (LUREP, 2100)
         end if
         IOFF = IOFF + PRVNIO(K)
      end do
      !
      goto 9999 !    RETURN
      !
      !     unsuccessful read
      !
900   continue
      write (LUREP, 2090) LCH, LUNWRP
      IERR = IERR + 1
      !
910   continue
9999  if (timon) call timstop(ithandl)
      return

2020  format(' ERROR  : Proces work file doesn''t match dimensions in' &
             /'          DELWAQ boot file for process_space_int_len', &
             /'          ', I6, ' in proces,', I6, ' in boot file.')
2030  format(' ERROR  : Proces work file doesn''t match dimensions in' &
             /'          DELWAQ boot file for num_processes_activated ', &
             /'          ', I6, ' in proces,', I6, ' in boot file.')
2040  format(' ERROR  : Proces work file doesn''t match dimensions in' &
             /'          DELWAQ boot file for num_fluxes ', &
             /'          ', I6, ' in proces,', I6, ' in boot file.')
2050  format(' ERROR  : Proces work file doesn''t match dimensions in' &
             /'          DELWAQ boot file for num_local_vars ', &
             /'          ', I6, ' in proces,', I6, ' in boot file.')
2060  format(' ERROR  : Proces work file doesn''t match dimensions in' &
             /'          DELWAQ boot file for num_defaults ', &
             /'          ', I6, ' in proces,', I6, ' in boot file.')
2070  format(' ERROR  : Proces work file doesn''t match dimensions in' &
             /'          DELWAQ boot file for num_substances_total ', &
             /'          ', I6, ' in proces,', I6, ' in boot file.')
2090  format(' ERROR  : Reading proces work file;', A, &
             /'          on unit number ', I3)
2100  format(' MESSAGE: Bloom fractional step switched on')
2120  format(' ERROR  : Proces work file doesn''t match dimensions in' &
             /'          DELWAQ boot file for num_substances_transported ', &
             /'          ', I6, ' in proces,', I6, ' in boot file.')
2130  format(' ERROR  : Proces work file doesn''t match dimensions in' &
             /'          DELWAQ boot file for num_dispersion_arrays_extra ', &
             /'          ', I6, ' in proces,', I6, ' in boot file.')
2140  format(' ERROR  : Proces work file doesn''t match dimensions in' &
             /'          DELWAQ boot file for num_velocity_arrays_extra ', &
             /'          ', I6, ' in proces,', I6, ' in boot file.')
2150  format(' ERROR  : Proces work file doesn''t match dimensions in' &
             /'          DELWAQ boot file for num_local_vars_exchange ', &
             /'          ', I6, ' in proces,', I6, ' in boot file.')
2160  format(' ERROR  : Proces work file doesn''t match dimensions in' &
             /'          DELWAQ boot file for num_dispersion_arrays_new ', &
             /'          ', I6, ' in proces,', I6, ' in boot file.')
2170  format(' ERROR  : Proces work file doesn''t match dimensions in' &
             /'          DELWAQ boot file for num_velocity_arrays_new ', &
             /'          ', I6, ' in proces,', I6, ' in boot file.')
2190  format(' ERROR  : Proces work file doesn''t match dimensions in' &
             /'          DELWAQ boot file for num_vars ', &
             /'          ', I6, ' in proces,', I6, ' in boot file.')
2200  format(' ERROR  : Proces work file doesn''t match dimensions in' &
             /'          DELWAQ boot file for num_input_ref ', &
             /'          ', I6, ' in proces,', I6, ' in boot file.')
3000  format(/' MODULE :', A, ' on grid ', I3, ', timestep multiplier:', I3)
3010  format(/' No process decomposition active')
3020  format(/' Process decomposition active')
      !
   end subroutine initialize_processes

    !! Initialisation of OUTPUT system.
   subroutine initialize_output(lunwro, lch, lurep, num_output_files, num_output_variables_extra, &
                                output_buffer_len, ioutps, iopoin, ounam, ousnm, &
                                ouuni, oudsc, num_substances_total, sysnm, syuni, &
                                sydsc, file_unit_list, file_name_list, ierr)

      !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
      !     ----    -----    ------     ------- -----------
      !     LUNWRO  INTEGER       1     INPUT   Output work file
      !     LCH     CHA*(*)       1     INPUT   Name output work file
      !     LUREP   INTEGER       1     INPUT   Monitoring file
      !     num_output_files   INTEGER       1     INPUT   Number of output files
      !     num_output_variables_extra  INTEGER       1     INPUT   Number of extra output vars
      !     output_buffer_len  INTEGER       1     INPUT
      !     IOUTPS  INTEGER 7*num_output_files    OUTPUT   Output structure
      !                                            index 1 = start time
      !                                            index 2 = stop time
      !                                            index 3 = time step
      !                                            index 4 = number of vars
      !                                            index 5 = kind of output
      !                                            index 6 = format of output
      !                                            index 7 = initialize flag
      !     IOPOIN  INTEGER  num_output_variables_extra    OUTPUT   Pointer to DELWAQ array's
      !     OUNAM   CHAR*(*) num_output_variables_extra    OUTPUT   name of output variable
      !     OUSNM   CHAR*(*) num_output_variables_extra    OUTPUT   standard name of output variable
      !     OUUNI   CHAR*(*) num_output_variables_extra    OUTPUT   unit of output variable
      !     OUDSC   CHAR*(*) num_output_variables_extra    OUTPUT   description of output variable
      !     OSSNM   CHAR*(*) num_output_variables_extra    OUTPUT   standard name of substance
      !     OSUNI   CHAR*(*) num_output_variables_extra    OUTPUT   unit of substance
      !     OSDSC   CHAR*(*) num_output_variables_extra    OUTPUT   description of substance
      !     file_unit_list     INTEGER    *        INPUT   array with unit numbers
      !     file_name_list   CHAR*(*)   *        INPUT   filenames
      !     IERR    INTEGER       1    IN/OUT   cummulative error count

      use m_open_waq_files
      use results

      integer(kind=int_wp) :: lunwro, lurep, num_output_files, num_output_variables_extra, output_buffer_len, num_substances_transported, &
                              ierr, num_substances_total
      integer(kind=int_wp) :: ioutps(7, *), iopoin(*), file_unit_list(*)
      character(len=*) lch, file_name_list(*)
      character(len=20) ounam(*)
      character(len=100) ousnm(*), sysnm(*)
      character(len=40) ouuni(*), syuni(*)
      character(len=60) oudsc(*), sydsc(*)

      ! local declarations
      integer(kind=int_wp), parameter :: luoff = 18
      integer(kind=int_wp), parameter :: luoff2 = 36
      integer(kind=int_wp) :: noutpd, nrvard, nbufmd
      real(kind=real_wp) :: versio

      integer(kind=int_wp) :: k, isrtou, ifi, idum

      integer(kind=int_wp) :: ithandl = 0
      if (timon) call timstrt("initialize_output", ithandl)

      ! read and check version number
      read (lunwro, err=900, end=900) versio

      ! read and check dimensions
      read (lunwro, err=900, end=900) noutpd, nrvard, nbufmd, ncopt
      if (noutpd /= num_output_files) then
         write (lurep, 2020) noutpd, num_output_files
         ierr = ierr + 1
      end if
      if (nrvard /= num_output_variables_extra) then
         write (lurep, 2030) nrvard, num_output_variables_extra
         ierr = ierr + 1
      end if
      if (nbufmd /= output_buffer_len) then
         write (lurep, 2040) nbufmd, output_buffer_len
         ierr = ierr + 1
      end if
      if (ierr > 0) goto 910

      read (lunwro, err=900, end=900) (ioutps(1, k), k=1, num_output_files)
      read (lunwro, err=900, end=900) (ioutps(2, k), k=1, num_output_files)
      read (lunwro, err=900, end=900) (ioutps(3, k), k=1, num_output_files)
      read (lunwro, err=900, end=900) (ioutps(4, k), k=1, num_output_files)
      read (lunwro, err=900, end=900) (ioutps(5, k), k=1, num_output_files)
      read (lunwro, err=900, end=900) (ioutps(6, k), k=1, num_output_files)
      if (num_output_variables_extra > 0) then
         read (lunwro, err=900, end=900) (iopoin(k), k=1, num_output_variables_extra)
         read (lunwro, err=900, end=900) (ounam(k), k=1, num_output_variables_extra)
         read (lunwro, err=900, end=900) (ousnm(k), k=1, num_output_variables_extra)
         read (lunwro, err=900, end=900) (ouuni(k), k=1, num_output_variables_extra)
         read (lunwro, err=900, end=900) (oudsc(k), k=1, num_output_variables_extra)
      end if
      if (num_substances_total > 0) then
         read (lunwro, err=900, end=900) (sysnm(k), k=1, num_substances_total)
         read (lunwro, err=900, end=900) (syuni(k), k=1, num_substances_total)
         read (lunwro, err=900, end=900) (sydsc(k), k=1, num_substances_total)
      end if

      ! Set initialize flag, open files: only on first subdomain
      do K = 1, num_output_files
         ISRTOU = IOUTPS(5, K)
         if (K <= 4) then
            IFI = K + LUOFF
         elseif (K <= 7) then
            IFI = K + LUOFF2 - 4
         else
            IFI = K + LUOFF2 - 2
         end if

         ! Open the output-file in the correct way, depending on type of output
         IOUTPS(7, K) = 1
         if (ISRTOU == IMON .or. ISRTOU == IMO2 .or. &
             ISRTOU == IMO3 .or. ISRTOU == IMO4) then

            ! Do not open the normal monitor file
            if (K /= 1) then
               call open_waq_files(file_unit_list(IFI), file_name_list(IFI), 19, 1, IDUM)
            end if
         elseif (ISRTOU == IDMP .or. ISRTOU == IDM2) then
            call open_waq_files(file_unit_list(IFI), file_name_list(IFI), 20, 1, IDUM)
         elseif (ISRTOU == IHIS .or. ISRTOU == IHI2 .or. &
                 ISRTOU == IHI3 .or. ISRTOU == IHI4) then
            call open_waq_files(file_unit_list(IFI), file_name_list(IFI), 21, 1, IDUM)
         elseif (ISRTOU == IMAP .or. ISRTOU == IMA2) then
            call open_waq_files(file_unit_list(IFI), file_name_list(IFI), 22, 1, IDUM)
         elseif (ISRTOU == IBAL .or. ISRTOU == IBA2) then
            call open_waq_files(file_unit_list(IFI), file_name_list(IFI), 37, 1, IDUM)
         end if
      end do

      if (timon) call timstop(ithandl)
      return

      ! unsuccessful read
900   continue
      write (LUREP, 2050) LCH, LUNWRO
      IERR = IERR + 1

910   continue
      if (timon) call timstop(ithandl)

      return
2020  format(' ERROR  : Output work file doesn''t match dimensions in' &
             /'          DELWAQ boot file for num_output_files', &
             /'          ', I6, ' in output,', I6, ' in boot file.')
2030  format(' ERROR  : Output work file doesn''t match dimensions in' &
             /'          DELWAQ boot file for num_output_variables_extra', &
             /'          ', I6, ' in output,', I6, ' in boot file.')
2040  format(' ERROR  : Output work file doesn''t match dimensions in' &
             /'          DELWAQ boot file for output_buffer_len', &
             /'          ', I6, ' in output,', I6, ' in boot file.')
2050  format(' ERROR  : Reading output work file;', A, &
             /'          on unit number ', I3)

   end subroutine initialize_output

    !! Initialise constant array from common block
   subroutine dhisys(isysi, isysn)

      !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
      !     ----    -----    ------     ------- -----------
      !     ISYSI   INTEGER       *     OUTPUT  copy of the SYSI common block
      !     ISYSN   INTEGER       *     OUTPUT  copy of the SYSI common block

      use m_array_manipulation, only: copy_integer_array_elements
      use m_waq_memory_dimensions ! System characteristics
      use m_timer_variables ! Timer characteristics

      integer(kind=int_wp) :: isysi(:), isysn(:)

      ! Fill the array's
      call copy_integer_array_elements(ii, isysi, iisize)
      call copy_integer_array_elements(in, isysn, insize)

   end subroutine dhisys

    !! Expands volume, area etc. for bottom cells
   subroutine expands_vol_area_for_bottom_cells(file_unit_list, num_cells, num_cells_bottom, num_layers, num_grids, &
                                                num_exchanges, num_exchanges_bottom_dir, igref, igseg, num_constants, &
                                                num_spatial_parameters, num_time_functions, num_spatial_time_fuctions, const, coname, &
                                                param, paname, funcs, funame, sfuncs, &
                                                sfname, ipoint, volume, area, flow, &
                                                aleng)

      !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
      !     ----    -----    ------     ------- -----------
      !     num_cells   INTEGER    1        INPUT   Number of water segments
      !     num_cells_bottom   INTEGER    1        INPUT   Number of bottom segments
      !     num_layers   INTEGER    1        INPUT   Number of water layers
      !     num_grids  INTEGER    1        INPUT   Nunber of grids
      !     num_exchanges     INTEGER    1        INPUT   Nunber of water exchanges
      !     num_exchanges_bottom_dir    INTEGER    1        INPUT   Nunber of bottom exchanges
      !     IGREF   INTEGER  num_grids     INPUT   Ref, neg = nr of bottom layers
      !     IGSEG   INTEGER num_cells,num_grids INPUT  pointer from water to bottom
      !     num_constants  INTEGER    1        INPUT   Number of constants used
      !     num_spatial_parameters    INTEGER    1        INPUT   Number of parameters
      !     num_time_functions   INTEGER    1        INPUT   Number of functions ( user )
      !     num_spatial_time_fuctions  INTEGER    1        INPUT   Number of segment functions
      !     CONST   REAL     num_constants     INPUT   value of constants
      !     CONAME  CHAR*20  num_constants     INPUT   Constant names
      !     PARAM   REAL    num_spatial_parameters,num_cells  INPUT   value of parameters
      !     PANAME  CHAR*20  num_spatial_parameters       INPUT   Parameter names
      !     FUNCS   REAL     num_time_functions      INPUT   Function values
      !     FUNAME  CHAR*20  num_time_functions      INPUT   Function names
      !     SFUNCS  REAL   num_cells,num_spatial_time_fuctions INPUT   Segment function values
      !     SFNAME  CHAR*20  num_spatial_time_fuctions     INPUT   Segment function names
      !     IPOINT  INTEGER   4,NOQT    INPUT   All exchange pointers
      !     VOLUME  REAL   num_cells+num_cells_bottom  IN/OUT  Segment volumes
      !     AREA    REAL    num_exchanges+num_exchanges_bottom_dir    IN/OUT  Exchange surfaces
      !     FLOW    REAL    num_exchanges+num_exchanges_bottom_dir    IN/OUT  Exchange flows
      !     ALENG   REAL   2,num_exchanges+num_exchanges_bottom_dir   IN/OUT  Diffusion lengthes
      use m_logger_helper, only: stop_with_error
      use m_grid_utils_external
      use m_exchange_values, only: exchange_values

      integer(kind=int_wp) :: file_unit_list(*), IGREF(num_grids), IGSEG(num_cells, num_grids), &
                              IPOINT(4, num_exchanges + num_exchanges_bottom_dir)
      real(kind=real_wp) :: CONST(num_constants), PARAM(num_spatial_parameters, num_cells), &
                            FUNCS(num_time_functions), SFUNCS(num_cells, num_spatial_time_fuctions), &
                            VOLUME(num_cells + num_cells_bottom), AREA(num_exchanges + num_exchanges_bottom_dir), &
                            ALENG(2, num_exchanges + num_exchanges_bottom_dir), FLOW(num_exchanges + num_exchanges_bottom_dir)
      character(len=20) CONAME(num_constants), PANAME(num_spatial_parameters), &
         FUNAME(num_time_functions), SFNAME(num_spatial_time_fuctions)
      integer(kind=int_wp) :: num_cells, num_cells_bottom, num_layers, num_grids, num_exchanges, num_exchanges_bottom_dir, num_constants
      integer(kind=int_wp) :: num_time_functions, num_spatial_time_fuctions, num_spatial_parameters

      !     local
      logical LGET
      logical :: first_q_column
      real(kind=real_wp), allocatable :: Horsurf(:), Thickn(:)
      character(len=20) CTAG
      integer(kind=int_wp) :: ierr, iq, cell_i, nosss

      integer(kind=int_wp) :: ithandl = 0
      if (timon) call timstrt("expands_vol_area_for_bottom_cells", ithandl)
      !
      NOSSS = num_cells + num_cells_bottom
      !
      !     Set up the horizontal surfaces
      !
10    CTAG = 'SURF'
      LGET = .true.
      allocate (Horsurf(NOSSS))
      call exchange_values(CTAG, NOSSS, Horsurf, num_constants, num_spatial_parameters, &
                           num_time_functions, num_spatial_time_fuctions, CONST, CONAME, PARAM, &
                           PANAME, FUNCS, FUNAME, SFUNCS, SFNAME, &
                           LGET, IERR)
      if (IERR /= 0) then
         write (file_unit_list(19), *) ' ERROR: Variabele SURF not found !'
         call stop_with_error()
      end if

      ! set surface of the first layer of sediment bed

      horsurf(num_cells + 1:nosss) = 0.0
      first_q_column = .true.
      do iq = 1, num_exchanges_bottom_dir
         if (first_q_column) then
            if (ipoint(1, num_exchanges + iq) <= num_cells) then
               if (ipoint(2, num_exchanges + iq) > 0) then
                  horsurf(ipoint(2, num_exchanges + iq)) = horsurf(ipoint(2, num_exchanges + iq)) + horsurf(ipoint(1, num_exchanges + iq))
               end if
            end if
         end if
         if (ipoint(2, num_exchanges + iq) < 0) then
            first_q_column = .not. first_q_column
         end if
      end do

      ! set surface of the rest of the sediment layers

      first_q_column = .true.
      do iq = 1, num_exchanges_bottom_dir
         if (first_q_column) then
            if (ipoint(1, num_exchanges + iq) > num_cells) then
               if (ipoint(2, num_exchanges + iq) > 0) then
                  horsurf(ipoint(2, num_exchanges + iq)) = horsurf(ipoint(2, num_exchanges + iq)) + horsurf(ipoint(1, num_exchanges + iq))
               end if
            end if
         end if
         if (ipoint(2, num_exchanges + iq) < 0) then
            first_q_column = .not. first_q_column
         end if
      end do

      ! store the surface areas

      LGET = .false.
      call exchange_values(CTAG, NOSSS, Horsurf, num_constants, num_spatial_parameters, &
                           num_time_functions, num_spatial_time_fuctions, CONST, CONAME, PARAM, &
                           PANAME, FUNCS, FUNAME, SFUNCS, SFNAME, &
                           LGET, IERR)

      ! Expand the volumes
      CTAG = 'FIXTH'
      LGET = .true.
      allocate (Thickn(NOSSS))
      call exchange_values(CTAG, NOSSS, Thickn, num_constants, num_spatial_parameters, &
                           num_time_functions, num_spatial_time_fuctions, CONST, CONAME, PARAM, &
                           PANAME, FUNCS, FUNAME, SFUNCS, SFNAME, &
                           LGET, IERR)
      if (IERR /= 0) then
         write (file_unit_list(19), *) ' ERROR: Variabele FIXTH not found !'
         call stop_with_error()
      end if
      do cell_i = num_cells + 1, num_cells + num_cells_bottom
         volume(cell_i) = Horsurf(cell_i) * Thickn(cell_i)
      end do

      ! Expand the areas, lengthes and flows
      do iq = 1, num_exchanges_bottom_dir
         area(num_exchanges + iq) = Horsurf(IPOINT(1, num_exchanges + iq))
         aleng(1, num_exchanges + iq) = 1.0
         aleng(2, num_exchanges + iq) = 1.0
         flow(num_exchanges + iq) = 0.0
      end do

      deallocate (Horsurf, Thickn)

      if (timon) call timstop(ithandl)

   end subroutine expands_vol_area_for_bottom_cells

    !! sets the top of the column for every segment
   subroutine set_cell_top_of_column(num_cells, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, &
                                     num_exchanges_bottom_dir, ipoint, iknmrk, isegcol)

      use m_extract_waq_attribute

      integer(kind=int_wp), intent(in) :: num_cells ! total number of segments
      integer(kind=int_wp), intent(in) :: num_exchanges_u_dir ! number of exchange pointers in first direction
      integer(kind=int_wp), intent(in) :: num_exchanges_v_dir ! number of exchange pointers in first direction
      integer(kind=int_wp), intent(in) :: num_exchanges_z_dir ! number of exchange pointers in first direction
      integer(kind=int_wp), intent(in) :: num_exchanges_bottom_dir ! number of exchange pointers in first direction
      integer(kind=int_wp), intent(in) :: ipoint(4, *) ! exchange pointers
      integer(kind=int_wp), intent(in) :: iknmrk(*) ! segment attributes
      integer(kind=int_wp), intent(out) :: isegcol(*) ! pointer from segment to top of column

      ! local declarations
      integer(kind=int_wp) :: cell_i ! segment index
      integer(kind=int_wp) :: iq ! exchange index
      integer(kind=int_wp) :: ifrom ! from segment in pointer
      integer(kind=int_wp) :: ito ! to segment in pointer
      integer(kind=int_wp) :: ikmrkv ! first attribute from segment
      integer(kind=int_wp) :: num_exchanges, z_dir_start_index ! total number of exchange pointers

      do cell_i = 1, num_cells
         isegcol(cell_i) = cell_i
      end do

      z_dir_start_index = num_exchanges_u_dir + num_exchanges_v_dir + 1
      num_exchanges = num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir

      do iq = z_dir_start_index, num_exchanges
         ifrom = ipoint(1, iq)
         ito = ipoint(2, iq)

         if (ifrom > 0 .and. ito > 0) then
            isegcol(ito) = isegcol(ifrom)
         end if
      end do

      do iq = num_exchanges + 1, num_exchanges + num_exchanges_bottom_dir

         ifrom = ipoint(1, iq)
         ito = ipoint(2, iq)

         ! only positive segments
         if (ifrom <= 0 .or. ito <= 0) cycle

         ! only if from segment is not a water segment
         call extract_waq_attribute(1, iknmrk(ifrom), ikmrkv)
         if (ikmrkv /= 3) cycle

         isegcol(ito) = isegcol(ifrom)
      end do

   end subroutine set_cell_top_of_column
end module initialize_conditions
