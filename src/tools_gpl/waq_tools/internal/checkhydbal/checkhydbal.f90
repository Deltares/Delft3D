!!  Copyright (C)  Stichting Deltares, 2021-2024.
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

program checkhydbal
   use m_alloc
   use m_hydmod
   use string_module
   use MessageHandling
   use m_file_path_utils
   use m_date_time_utils_external, only: write_date_time
   use m_cli_utils, only: store_command_arguments, is_command_arg_specified, &
                          get_command_argument_by_name, get_argument_by_index
   use checkhydbal_version_module
   implicit none

   type(t_hydrodynamics) :: hyd ! description of the input hydrodynamics
   integer :: noseg
   integer :: iseg
   integer :: ip1
   integer :: ip2
   integer :: noq
   integer :: noqh
   integer :: noqv
   integer, allocatable :: noqhs(:)
   integer, allocatable :: noqvs(:)
   integer, allocatable :: iqhs(:)
   integer, allocatable :: iqvs(:)
   integer :: maxnoqhs
   integer :: maxnoqvs
   integer, allocatable :: ipoinths(:, :)
   integer, allocatable :: ipointvs(:, :)
   integer, allocatable :: iexchhs(:, :)
   integer, allocatable :: iexchvs(:, :)
   integer :: iq
   integer :: iqs
   integer :: i

   integer :: itimet0
   integer :: itimedt
   integer :: iend
   integer :: ntime
   real(kind=8) :: dt
   real(kind=8) :: mimimum_reference_flow = 1.d-3 ! minimum flow (m3/s)
   real(kind=8) :: mimimum_reference_waterdepth = 1.d-4 ! minimum reference waterdepth (m)
   real(kind=8) :: mimimum_reference_surface = 1.d0 ! minimum reference surface (m2)
   real(kind=8) :: reference_volume
   real(kind=8) :: qmax

   integer :: nosegfb
   integer, allocatable :: segfb(:)
   character(20), allocatable :: segnam(:)

   real(kind=8), allocatable :: mimimum_reference_volume(:)
   real(kind=8), allocatable :: volume0(:)
   real(kind=8), allocatable :: volume1(:)
   real(kind=4), allocatable :: volume0r(:)
   real(kind=4), allocatable :: volume1r(:)
   real(kind=4), allocatable :: dvolumer(:)
   real(kind=8), allocatable :: flow(:)
   real(kind=4), allocatable :: flowhs(:, :)
   real(kind=4), allocatable :: flowvs(:, :)

   real(kind=8), allocatable :: inflow(:)
   real(kind=8), allocatable :: outflow(:)

   real(kind=4), allocatable :: errv(:)
   real(kind=4), allocatable :: errq(:)
   real(kind=4), allocatable :: relerrv(:)
   real(kind=4), allocatable :: relerrq(:)
   real(kind=4), allocatable :: restim(:)

   real(kind=4), allocatable :: errvavg(:)
   real(kind=4), allocatable :: errvmax(:)
   real(kind=4), allocatable :: relerrvavg(:)
   real(kind=4), allocatable :: relerrvmax(:)
   real(kind=4), allocatable :: errqavg(:)
   real(kind=4), allocatable :: errqmax(:)
   real(kind=4), allocatable :: relerrqavg(:)
   real(kind=4), allocatable :: relerrqmax(:)
   real(kind=4), allocatable :: restimavg(:)
   real(kind=4), allocatable :: restimmin(:)

   real(kind=4) :: maxerrv
   integer :: maxerrvs
   real(kind=4) :: maxrelerrv
   integer :: maxrelerrvs
   real(kind=4) :: maxerrq
   integer :: maxerrqs
   real(kind=4) :: maxrelerrq
   integer :: maxrelerrqs
   real(kind=4) :: minrestim
   integer :: minrestims

   character(:), allocatable :: file_name
   character(255) :: input_hyd_file
   type(t_file) :: report
   type(t_file) :: balerr
   type(t_file) :: balerrfb
   type(t_file) :: balsum
   type(t_file) :: ascrelerrvavg
   type(t_file) :: ascrelerrvmax
   character(255) :: filext
   character(3) :: sfb
   integer :: starttime
   integer :: endtime
   type(t_file) :: subset_file

   character(len=40) :: moname(4)

   integer, parameter :: nout_fb = 3
   character(20) :: names_full_balance(nout_fb)
   character(20), allocatable :: names_exchanges_horizontal(:)
   character(20), allocatable :: names_exchanges_vertical(:)
   character(20), allocatable :: syname_segh(:)
   character(20), allocatable :: syname_segv(:)

   integer, parameter :: nout_err = 5
   character(20) :: names_errors(nout_err)
   integer, parameter :: nout_sum = 10
   character(20) :: syname_sum(nout_sum)
   character(20) :: rundat

   integer :: extpos
   integer :: extlen

   logical :: exist_hyd
   logical :: full_balance ! write full balance output
   logical :: subset_only ! but only for a subset of segments

   data names_full_balance/'Volume(t)           ', 'Volume(t+dt)        ', 'dVolume             '/
   data names_errors/'V-error-m3          ', 'V-rel-error         ', 'Q-error-m3/s        ', &
      'Q-rel-error         ', 'Res-tim-s           '/
   data syname_sum/'V-error-Ave-m3      ', 'V-error-Max-m3      ', 'V-rel-error-Ave     ', 'V-rel-error-Max     ', &
      'Q-error-Ave-m3/s    ', 'Q-error-Max-m3/s    ', 'Q-rel-error-Ave     ', 'Q-rel-error-Max     ', &
      'Res-tim-Ave-s       ', 'Res-tim-Min-s       '/

   call store_command_arguments()
   write (*, *)
   write (*, '(a,a)') '(c) ', checkhydbal_version_full
   write (*, *)

   if (is_command_arg_specified('--help') .or. &
       is_command_arg_specified('-h') .or. &
       is_command_arg_specified('/?')) then
      write (*, '(a)') 'Checkhydbal'
      write (*, '(a)') '==========='
      write (*, *)
      write (*, '(a)') 'Tool to check for water balance errors in a hyd-file data set, and optionaly extract the full'
      write (*, '(a)') 'water balance per segment for the whole model or for a subset of segments to trace errors.'
      write (*, *)
      write (*, '(a)') 'Usage:'
      write (*, '(a)') 'checkhydbal -hydfile <input_hyd_file> -fullbalance -subset <subsetfile> '
      write (*, '(a)') '            -starttime <starttime> -endtime <endtime> '
      write (*, *)
      write (*, '(a)') '-hydfile <input hyd file> Name of the hyd-file to check'
      write (*, '(a)') '-fullbalance              Write full water balance output'
      write (*, '(a)') '-subsetfile <subset file> File to limit the full balance output to a subset of segments.'
      write (*, '(a)') '                          format of the subset file:'
      write (*, '(a)') '                          <number of segments>'
      write (*, '(a)') '                          <segment #1>'
      write (*, '(a)') '                          ...'
      write (*, '(a)') '                          <segment #n>'
      write (*, '(a)') '-starttime <starttime>    Start of period to check'
      write (*, '(a)') '-endtime <endtime>        End of period to check'
      write (*, '(a)') '-h, --help, /?            Print help about Checkhydbal'
      write (*, *)
      stop
   end if

   if (is_command_arg_specified('--version') .or. &
       is_command_arg_specified('-v')) then
      write (*, '(a,a)') ' (c) ', checkhydbal_version_full
      stop
   end if

! Set default values
   input_hyd_file = ' '
   full_balance = .false.
   subset_only = .false.
   starttime = -huge(starttime)
   endtime = huge(endtime)

   if (is_command_arg_specified('-hydfile')) then
      if (get_command_argument_by_name('-hydfile', file_name)) then
         input_hyd_file = file_name
      else
         write (*, '(a)') 'Incorrect use of -hydfile. Use -h, --help, /? to print help for Checkhydbal'
      end if
   else
      ! For backwards compatibility on TeamCity, accept first argument as input_hyd_file
      if (get_argument_by_index(1, file_name)) then
         input_hyd_file = file_name
      end if
   end if

   if (input_hyd_file == ' ') then
      write (*, '(a)') 'No arguments given. Use -h, --help, /? to print help for Checkhydbal'
      stop 1
   end if
   write (*, '(a)') 'Input hyd-file name: '//trim(input_hyd_file)

   if (is_command_arg_specified('-fullbalance')) then
      full_balance = .true.
      write (*, '(a)') 'Writing of full balance switched on.'
      if (is_command_arg_specified('-subsetfile')) then
         if (get_command_argument_by_name('-subsetfile', file_name)) then
            write (*, '(a)') 'Only writing full balance for a subset of segments.'
            write (*, '(a)') 'Subset file name: '//trim(file_name)
            subset_file%name = file_name
            subset_only = .true.
            subset_file%type = ft_asc
            subset_file%status = FILE_STAT_UNOPENED
         else
            write (*, '(a)') 'Incorrect use of -subsetfile. Use -h, --help, /? to print help for Checkhydbal'
            stop 1
         end if
      end if
   end if

   if (is_command_arg_specified('-starttime')) then
      if (get_command_argument_by_name('-starttime', starttime)) then
         write (*, '(a,i0)') 'Skipping data before specified start time in seconds since T0: ', starttime
      else
         write (*, '(a)') 'Incorrect use of -starttime. Use -h, --help, /? to print help for Checkhydbal'
         stop 1
      end if
   end if

   if (is_command_arg_specified('-endtime')) then
      if (get_command_argument_by_name('-endtime', endtime)) then
         write (*, '(a,i0)') 'Skipping data after  specified end time in seconds since T0: ', endtime
      else
         write (*, '(a)') 'Incorrect use of -endtime. Use -h, --help, /? to print help for Checkhydbal'
         stop 1
      end if
   end if
   write (*, *)

   call extract_file_extension(input_hyd_file, filext, extpos, extlen)
   report%name = input_hyd_file(1:extpos - 1)//'-checkhydbal.rep'
   report%type = ft_asc
   report%status = FILE_STAT_UNOPENED

   balerr%name = input_hyd_file(1:extpos - 1)//'-checkhydbal-err.map'
   balerr%type = ft_bin
   balerr%status = FILE_STAT_UNOPENED

   if (full_balance) then
      if (subset_only) then
         balerrfb%name = input_hyd_file(1:extpos - 1)//'-checkhydbal-errfb.his'
      else
         balerrfb%name = input_hyd_file(1:extpos - 1)//'-checkhydbal-errfb.map'
      end if
      balerrfb%type = ft_bin
      balerrfb%status = FILE_STAT_UNOPENED
   end if

   balsum%name = input_hyd_file(1:extpos - 1)//'-checkhydbal-sum.map'
   balsum%type = ft_bin
   balsum%status = FILE_STAT_UNOPENED

   ascrelerrvavg%name = input_hyd_file(1:extpos - 1)//'-relerrvavg.txt'
   ascrelerrvavg%type = ft_asc
   ascrelerrvavg%status = FILE_STAT_UNOPENED

   ascrelerrvmax%name = input_hyd_file(1:extpos - 1)//'-relerrvmax.txt'
   ascrelerrvmax%type = ft_asc
   ascrelerrvmax%status = FILE_STAT_UNOPENED

   call report%open()
   call SetMessageHandling(lunMessages=report%unit)

   if (filext /= 'hyd') then
      write (*, *) 'error: input file is not a hyd-file:', trim(input_hyd_file)
      write (report%unit, *) 'error: input file is not a hyd-file:', trim(input_hyd_file)
   end if

   write (report%unit, '(a,a)') ' (c) ', checkhydbal_version_full
   call write_date_time(rundat)
   write (report%unit, '(a,a)') ' execution start: ', rundat
   write (report%unit, *) 'input hyd-file name          : ', trim(input_hyd_file)

   inquire (file=input_hyd_file, exist=exist_hyd)
   if (.not. exist_hyd) then
      write (*, '(a,a)') ' ERROR hyd-file not found: ', trim(input_hyd_file)
      write (report%unit, '(a,a)') ' ERROR hyd-file not found: ', trim(input_hyd_file)
      stop 1
   end if

   ! read hydrodynamic description file
   write (*, '(a)') 'Reading hyd-file ...'
   write (report%unit, '(a)') 'Reading hyd-file ...'
   hyd%file_hyd%name = input_hyd_file
   call read_hyd(hyd)

   ! read administration
   write (*, '(a)') 'Reading administration ...'
   write (report%unit, '(a)') 'Reading administration ...'
   call read_hyd_init(hyd)

!     get the dimensions and allocate arrays
   noseg = hyd%num_cells
   noq = hyd%num_exchanges
   noqh = hyd%num_exchanges_u_dir + hyd%num_exchanges_v_dir
   noqv = hyd%num_exchanges_z_dir

   write (*, *) 'Number of segments           : ', noseg
   write (*, *) 'Number of exchanges          : ', noq
   write (*, *) 'Number of segments per layer : ', hyd%nosegl
   write (*, *) 'Number of layers             : ', hyd%num_layers
   write (report%unit, *) 'Number of segments           : ', noseg
   write (report%unit, *) 'Number of exchanges          : ', noq
   write (report%unit, *) 'Number of segments per layer : ', hyd%nosegl
   write (report%unit, *) 'Number of layers             : ', hyd%num_layers

   call realloc(mimimum_reference_volume, noseg, keepExisting=.false., fill=0.0d0)
   call realloc(volume0, noseg, keepExisting=.false., fill=0.0d0)
   call realloc(volume1, noseg, keepExisting=.false., fill=0.0d0)
   call realloc(flow, noq, keepExisting=.false., fill=0.0d0)

   do iseg = 1, noseg
      mimimum_reference_volume(iseg) = max(hyd%surf(iseg), mimimum_reference_surface) * mimimum_reference_waterdepth
   end do

   if (full_balance) then
      if (subset_only) then
         call subset_file%open()
         read (subset_file%unit, *) nosegfb
         call realloc(segfb, nosegfb, keepExisting=.false., fill=0)
         read (subset_file%unit, *) segfb(1:nosegfb)
         call realloc(segnam, nosegfb, keepExisting=.false., fill=' ')
         do i = 1, nosegfb
            write (segnam(i), '("segment",I10.10)'), segfb(i)
         end do
      else
         nosegfb = noseg
      end if

      !     determine maximum number of exchanges for a segment
      call realloc(noqhs, noseg, keepExisting=.false., fill=0)
      call realloc(noqvs, noseg, keepExisting=.false., fill=0)
      call realloc(iqhs, noseg, keepExisting=.false., fill=0)
      call realloc(iqvs, noseg, keepExisting=.false., fill=0)
      noqhs = 0
      noqvs = 0
      do iq = 1, noqh
         ip1 = hyd%ipoint(1, iq)
         ip2 = hyd%ipoint(2, iq)
         if (ip1 /= 0 .and. ip2 /= 0) then
            if (ip1 > 0) then
               noqhs(ip1) = noqhs(ip1) + 1
            end if
            if (ip2 > 0) then
               noqhs(ip2) = noqhs(ip2) + 1
            end if
         end if
      end do
      do iq = noqh + 1, noq
         ip1 = hyd%ipoint(1, iq)
         ip2 = hyd%ipoint(2, iq)
         if (ip1 /= 0 .and. ip2 /= 0) then
            if (ip1 > 0) then
               noqvs(ip1) = noqvs(ip1) + 1
            end if
            if (ip2 > 0) then
               noqvs(ip2) = noqvs(ip2) + 1
            end if
         end if
      end do
      maxnoqhs = maxval(noqhs)
      maxnoqvs = maxval(noqvs)

      call realloc(ipoinths, (/noseg, maxnoqhs/), keepExisting=.false., fill=0)
      call realloc(ipointvs, (/noseg, maxnoqvs/), keepExisting=.false., fill=0)
      call realloc(iexchhs, (/noseg, maxnoqhs/), keepExisting=.false., fill=0)
      call realloc(iexchvs, (/noseg, maxnoqvs/), keepExisting=.false., fill=0)

      call realloc(volume0r, nosegfb, keepExisting=.false., fill=0.0)
      call realloc(volume1r, nosegfb, keepExisting=.false., fill=0.0)
      call realloc(dvolumer, nosegfb, keepExisting=.false., fill=0.0)
      call realloc(flowhs, (/nosegfb, maxnoqhs/), keepExisting=.false., fill=0.0)
      call realloc(flowvs, (/nosegfb, maxnoqvs/), keepExisting=.false., fill=0.0)

      call realloc(names_exchanges_horizontal, maxnoqhs, keepExisting=.false., fill=' ')
      call realloc(names_exchanges_vertical, maxnoqvs, keepExisting=.false., fill=' ')
      call realloc(syname_segh, maxnoqhs, keepExisting=.false., fill=' ')
      call realloc(syname_segv, maxnoqvs, keepExisting=.false., fill=' ')
      do i = 1, maxnoqhs
         write (names_exchanges_horizontal(i), '("horexch",I2.2)'), i
         write (syname_segh(i), '("hor_seg",I2.2)'), i
      end do
      do i = 1, maxnoqvs
         write (names_exchanges_vertical(i), '("verexch",I2.2)'), i
         write (syname_segv(i), '("ver_seg",I2.2)'), i
      end do

      do iq = 1, noqh
         ip1 = hyd%ipoint(1, iq)
         ip2 = hyd%ipoint(2, iq)
         if (ip1 /= 0 .and. ip2 /= 0) then
            if (ip1 > 0) then
               iqhs(ip1) = iqhs(ip1) + 1
               ipoinths(ip1, iqhs(ip1)) = ip2
               iexchhs(ip1, iqhs(ip1)) = -iq
            end if
            if (ip2 > 0) then
               iqhs(ip2) = iqhs(ip2) + 1
               ipoinths(ip2, iqhs(ip2)) = ip1
               iexchhs(ip2, iqhs(ip2)) = iq
            end if
         end if
      end do
      do iq = noqh + 1, noq
         ip1 = hyd%ipoint(1, iq)
         ip2 = hyd%ipoint(2, iq)
         if (ip1 /= 0 .and. ip2 /= 0) then
            if (ip1 > 0) then
               iqvs(ip1) = iqvs(ip1) + 1
               ipointvs(ip1, iqvs(ip1)) = ip2
               iexchvs(ip1, iqvs(ip1)) = -iq
            end if
            if (ip2 > 0) then
               iqvs(ip2) = iqvs(ip2) + 1
               ipointvs(ip2, iqvs(ip2)) = ip1
               iexchvs(ip2, iqvs(ip2)) = iq
            end if
         end if
      end do
      write (report%unit, '(/" Overview of exchanges and connected segments"/)')
      write (report%unit, '(" - A positive exchange number means flow is going to this segment.")')
      write (report%unit, '(" - A negative exchange number means flow is coming from this segment.")')
      write (report%unit, '(" - A positive segment number means there is a connection to an internal segment.")')
      write (report%unit, '(" - A negative segment number means there is a connection to an boundary."/)')
      write (report%unit, '("segmentid noqh noqv  ",100A10)') names_exchanges_horizontal(1:maxnoqhs) (1:10), &
         names_exchanges_vertical(1:maxnoqvs) (1:10), syname_segh(1:maxnoqhs) (1:10), syname_segv(1:maxnoqvs) (1:10)
      do iseg = 1, noseg
         write (report%unit, '(I9,I5,I5,X,100I10)') iseg, noqhs(iseg), noqvs(iseg), iexchhs(iseg, 1:maxnoqhs), &
            iexchvs(iseg, 1:maxnoqvs), ipoinths(iseg, 1:maxnoqhs), ipointvs(iseg, 1:maxnoqvs)
      end do
   end if

!     variable dimension arrays

   call realloc(inflow, noseg, keepExisting=.false., fill=0.0d0)
   call realloc(outflow, noseg, keepExisting=.false., fill=0.0d0)

   call realloc(errv, noseg, keepExisting=.false., fill=0.0)
   call realloc(relerrv, noseg, keepExisting=.false., fill=0.0)
   call realloc(errq, noseg, keepExisting=.false., fill=0.0)
   call realloc(relerrq, noseg, keepExisting=.false., fill=0.0)
   call realloc(restim, noseg, keepExisting=.false., fill=0.0)

   call realloc(errvavg, noseg, keepExisting=.false., fill=0.0)
   call realloc(errvmax, noseg, keepExisting=.false., fill=-1.0e20)
   call realloc(relerrvavg, noseg, keepExisting=.false., fill=0.0)
   call realloc(relerrvmax, noseg, keepExisting=.false., fill=-1.0e20)
   call realloc(errqavg, noseg, keepExisting=.false., fill=0.0)
   call realloc(errqmax, noseg, keepExisting=.false., fill=-1.0e20)
   call realloc(relerrqavg, noseg, keepExisting=.false., fill=0.0)
   call realloc(relerrqmax, noseg, keepExisting=.false., fill=-1.0e20)
   call realloc(restimavg, noseg, keepExisting=.false., fill=0.0)
   call realloc(restimmin, noseg, keepExisting=.false., fill=1.0e20)

   ntime = 0

   call balerr%open()
   if (full_balance) then
      call balerrfb%open()
   end if
   call balsum%open()

!     Write output map-file headers
   moname(1) = 'Checkhydbal: Water balance checking tool'
   moname(2) = 'Water balance & errors of hyd-file data'
   moname(3) = checkhydbal_version_full(10:)
   moname(4) = 'T0: '//hyd%cnv_ref(1:4)//'.'//hyd%cnv_ref(5:6)//'.'//hyd%cnv_ref(7:8)//' '// &
               hyd%cnv_ref(9:10)//':'//hyd%cnv_ref(11:12)//':'//hyd%cnv_ref(13:14)//'  (scu=       1s)'

   write (balerr%unit) moname
   write (balerr%unit) nout_err, noseg
   write (balerr%unit) names_errors

   if (full_balance) then
      write (balerrfb%unit) moname
      write (balerrfb%unit) nout_fb + maxnoqhs + maxnoqvs + nout_err, nosegfb
      write (balerrfb%unit) names_full_balance, names_exchanges_horizontal(1:maxnoqhs), &
         names_exchanges_vertical(1:maxnoqvs), names_errors
      if (subset_only) then
         write (balerrfb%unit) (i, segnam(i), i=1, nosegfb)
      end if
   end if

   write (balsum%unit) moname
   write (balsum%unit) nout_sum, noseg
   write (balsum%unit) syname_sum

!     Read first records

   call read_hyd_step(hyd, itimet0, iend)
   do iseg = 1, noseg
      volume0(iseg) = hyd%volume(iseg)
   end do
   do iq = 1, noq
      flow(iq) = hyd%flow(iq)
   end do
   call read_hyd_step(hyd, itimedt, iend)
   do iseg = 1, noseg
      volume1(iseg) = hyd%volume(iseg)
   end do

   write (report%unit, '(/a)') " Overview of maximum absolute and relative volume errors, discharge"
   write (report%unit, '(a/)') " errors and minimum residence time (in seconds) and their locations"
   write (report%unit, '(a)') "     istep    time(s)    segment     maxvolerr    segment"// &
      "  maxrelvolerr    segment     maxdiserr    segment  maxreldiserr    segment     minrestim"

!----------------------------------------------------------------------
!     START TIME LOOP
!----------------------------------------------------------------------

   do while (iend == 0)

      if (itimedt > endtime) then
         write (*, *) 'Stopped because end time (', endtime, ')was reached'
         exit
      else if (itimet0 < starttime) then
         write (*, *) 'Time ', ntime, itimet0, '(skipped)'
         goto 9999
      end if
      ntime = ntime + 1
      write (*, *) 'Time ', ntime, itimet0
      dt = itimedt - itimet0

!     Initialize inflow/outflow per segment

      inflow = 0.0
      outflow = 0.0

! Compute expected volume change per segment internal flows

      do iq = 1, noq
         if (hyd%ipoint(1, iq) > 0) then
            iseg = hyd%ipoint(1, iq)
            if (flow(iq) > 0.0) then
               outflow(iseg) = outflow(iseg) + flow(iq)
            else
               inflow(iseg) = inflow(iseg) - flow(iq)
            end if
         end if
         if (hyd%ipoint(2, iq) > 0) then
            iseg = hyd%ipoint(2, iq)
            if (flow(iq) < 0.0) then
               outflow(iseg) = outflow(iseg) - flow(iq)
            else
               inflow(iseg) = inflow(iseg) + flow(iq)
            end if
         end if
      end do

!     Compute balance errors and residence time

      do iseg = 1, noseg
         errv(iseg) = abs((volume1(iseg) - volume0(iseg)) - ((inflow(iseg) - outflow(iseg)) * dt))
         ! For releative error, find the higest volume as a reference, with a minimum of mimimum_reference_volume
         reference_volume = max(volume0(iseg), volume1(iseg), mimimum_reference_volume(iseg))
         relerrv(iseg) = errv(iseg) / reference_volume
         errq(iseg) = abs((volume1(iseg) - volume0(iseg)) / dt - (inflow(iseg) - outflow(iseg)))
         qmax = max(outflow(iseg), inflow(iseg), mimimum_reference_flow)
         relerrq(iseg) = errq(iseg) / qmax
         restim(iseg) = max(volume0(iseg), mimimum_reference_volume(iseg)) / qmax

!     Keep track of average, minimum and maximum

         errvavg(iseg) = errvavg(iseg) + errv(iseg)
         errvmax(iseg) = max(errvmax(iseg), errv(iseg))
         relerrvavg(iseg) = relerrvavg(iseg) + relerrv(iseg)
         relerrvmax(iseg) = max(relerrvmax(iseg), relerrv(iseg))
         errqavg(iseg) = errqavg(iseg) + errq(iseg)
         errqmax(iseg) = max(errqmax(iseg), errq(iseg))
         relerrqavg(iseg) = relerrqavg(iseg) + relerrq(iseg)
         relerrqmax(iseg) = max(relerrqmax(iseg), relerrq(iseg))
         restimavg(iseg) = restimavg(iseg) + restim(iseg)
         if (ntime == 1) then
            restimmin(iseg) = restim(iseg)
         else
            restimmin(iseg) = min(restimmin(iseg), restim(iseg))
         end if
      end do

!     Gather full balance when requested
      if (full_balance) then
         if (subset_only) then
            do i = 1, nosegfb
               iseg = segfb(i)
               volume0r(i) = volume0(iseg)
               volume1r(i) = volume1(iseg)
               do iqs = 1, noqhs(iseg)
                  iq = iexchhs(iseg, iqs)
                  if (iq > 0) then
                     flowhs(i, iqs) = flow(iq) * dt
                  else
                     flowhs(i, iqs) = -flow(-iq) * dt
                  end if
               end do
               do iqs = 1, noqvs(iseg)
                  iq = iexchvs(iseg, iqs)
                  if (iq > 0) then
                     flowvs(i, iqs) = flow(iq) * dt
                  else
                     flowvs(i, iqs) = -flow(-iq) * dt
                  end if
               end do
            end do
         else
            volume0r = volume0
            volume1r = volume1
            do iseg = 1, noseg
               do iqs = 1, noqhs(iseg)
                  iq = iexchhs(iseg, iqs)
                  if (iq > 0) then
                     flowhs(iseg, iqs) = flow(iq) * dt
                  else
                     flowhs(iseg, iqs) = -flow(-iq) * dt
                  end if
               end do
               do iqs = 1, noqvs(iseg)
                  iq = iexchvs(iseg, iqs)
                  if (iq > 0) then
                     flowvs(iseg, iqs) = flow(iq) * dt
                  else
                     flowvs(iseg, iqs) = -flow(-iq) * dt
                  end if
               end do
            end do
         end if
         dvolumer = volume1r - volume0r
      end if

! Determine summary of this time step
      maxerrv = maxval(errv)
      maxerrvs = maxloc(errv, 1)
      maxrelerrv = maxval(relerrv)
      maxrelerrvs = maxloc(relerrv, 1)
      maxerrq = maxval(errq)
      maxerrqs = maxloc(errq, 1)
      maxrelerrq = maxval(relerrq)
      maxrelerrqs = maxloc(relerrq, 1)
      minrestim = minval(restim)
      minrestims = minloc(restim, 1)

! Write the data
      if (full_balance) then
         write (balerrfb%unit) itimet0
         if (subset_only) then
            do i = 1, nosegfb
               iseg = segfb(i)
               write (balerrfb%unit) volume0r(i), volume1r(i), dvolumer(i), flowhs(i, 1:maxnoqhs), &
                  flowvs(i, 1:maxnoqvs), errv(iseg), relerrv(iseg), errq(iseg), relerrq(iseg), restim(iseg)
            end do
         else
            do iseg = 1, nosegfb
               write (balerrfb%unit) volume0r(iseg), volume1r(iseg), dvolumer(iseg), flowhs(iseg, 1:maxnoqhs), &
                  flowvs(iseg, 1:maxnoqvs), errv(iseg), relerrv(iseg), errq(iseg), relerrq(iseg), restim(iseg)
            end do
         end if
      end if

      write (balerr%unit) itimet0
      do iseg = 1, noseg
         write (balerr%unit) errv(iseg), relerrv(iseg), errq(iseg), relerrq(iseg), restim(iseg)
      end do

!        Log summary over this time step
      write (report%unit, '(I10,X,I10,5(X,I10,X,ES13.5))') ntime, itimet0, maxerrvs, maxerrv, &
         maxrelerrvs, maxrelerrv, maxerrqs, maxerrq, maxrelerrqs, maxrelerrq, minrestims, minrestim

9999  continue
      do iseg = 1, noseg
         volume0(iseg) = volume1(iseg)
      end do
      do iq = 1, noq
         flow(iq) = hyd%flow(iq)
      end do
      itimet0 = itimedt
      call read_hyd_step(hyd, itimedt, iend)
      do iseg = 1, noseg
         volume1(iseg) = hyd%volume(iseg)
      end do
   end do

!----------------------------------------------------------------------
!     END OF TIME LOOP
!----------------------------------------------------------------------

   if (ntime == 0) then
      write (*, '(/a)') 'No time steps found, so no summary could be produced.'
      write (report%unit, '(/a)') 'No time steps found, so no summary could be produced.'
      stop 1
   end if
   
   itimet0 = 0
   do iseg = 1, noseg
      errvavg(iseg) = errvavg(iseg) / real(ntime)
      relerrvavg(iseg) = relerrvavg(iseg) / real(ntime)
      errqavg(iseg) = errqavg(iseg) / real(ntime)
      relerrqavg(iseg) = relerrqavg(iseg) / real(ntime)
      restimavg(iseg) = restimavg(iseg) / real(ntime)
   end do
   write (balsum%unit) itimet0
   do iseg = 1, noseg
      write (balsum%unit) errvavg(iseg), errvmax(iseg), relerrvavg(iseg), relerrvmax(iseg), errqavg(iseg), &
         errqmax(iseg), relerrqavg(iseg), relerrqmax(iseg), restimavg(iseg), restimmin(iseg)
   end do

!     write ascii output for testbank
   call ascrelerrvavg%open()
   write (ascrelerrvavg%unit, '(ES15.7)') (relerrvavg(iseg), iseg=1, noseg)
   call ascrelerrvmax%open()
   write (ascrelerrvmax%unit, '(ES15.7)') (relerrvmax(iseg), iseg=1, noseg)

   write (*, '(/)')
   write (*, *) ' Number of time steps analysed     (-)   : ', ntime
   write (*, *) ' Maximum volume error              (m3)  : ', maxval(errvmax)
   write (*, *) ' Maximum relative volume error     (-)   : ', maxval(relerrvmax)
   write (*, *) ' Maximum discharge error           (m3/s): ', maxval(errqmax)
   write (*, *) ' Maximum relative discharge error  (-)   : ', maxval(relerrqmax)
   write (*, *) ' Minimum residence time            (s)   : ', minval(restimmin)
   write (*, *) ' Minimum residence time in segment       : ', minloc(restimmin)
   write (report%unit, '(/)')
   write (report%unit, *) ' Number of time steps analysed     (-)   : ', ntime
   write (report%unit, *) ' Maximum volume error              (m3)  : ', maxval(errvmax)
   write (report%unit, *) ' Maximum relative volume error     (-)   : ', maxval(relerrvmax)
   write (report%unit, *) ' Maximum discharge error           (m3/s): ', maxval(errqmax)
   write (report%unit, *) ' Maximum relative discharge error  (-)   : ', maxval(relerrqmax)
   write (report%unit, *) ' Minimum residence time            (s)   : ', minval(restimmin)
   write (report%unit, *) ' Minimum residence time in segment       : ', minloc(restimmin)

   write (*, '(/)')
   write (*, *) ' Checkhydbal finished normally'
   write (report%unit, '(/)')
   write (report%unit, *) ' Checkhydbal finished normally'

end program checkhydbal
