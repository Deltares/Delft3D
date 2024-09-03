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
module wave_boundary_update_module
   use wave_boundary_datastore
   !
   implicit none
   private
   public generate_wave_boundary_surfbeat
   !
   type spectrum ! These are related to input spectra
      real(dp), dimension(:, :), pointer :: S ! 2D variance density spectrum
      real(dp), dimension(:), pointer :: f, ang ! 1D frequency and direction vectors for S
      real(dp), dimension(:), pointer :: Sf ! S integrated over all directions
      real(dp), dimension(:), pointer :: Sd ! S integrated over all frequencies
      integer :: nf, nang ! number of frequencies and angles
      real(dp) :: df, dang ! frequency and angle step size
      real(dp) :: hm0, fp, dir0, scoeff ! imposed significant wave height, peak frequency,
      ! main wave angle and spreading coefficient
      real(dp) :: trep, dirm ! representative period and mean wave direction
   end type spectrum
   type shortspectrum
      real(dp), dimension(:), pointer :: Sf ! S integrated over all directions
   end type shortspectrum
   type waveparamsnew ! These are related to the generated bc series
      real(dp) :: h0 ! average water depth on offshore boundary
      integer :: K ! number of wave train components
      real(dp) :: rtbc, dtbc ! duration and time step to be written to boundary condition file
      real(dp) :: rtin, dtin ! duration and time step for the internal time axis, based on Fourier
      ! limitations and the number of wave train components (wp%K)
      real(dp), dimension(:), pointer :: tin ! internal time axis
      real(dp), dimension(:), pointer :: taperf, taperw ! internal taper function for flow and waves
      integer :: tslen ! internal time axis length (is always even, not odd)
      integer :: tslenbc ! time axis length for boundary condition file
      logical :: dtchanged ! quick check to see if dtin == dtbc (useful for interpolation purpose)
      real(dp), dimension(:), pointer :: fgen, thetagen, phigen, kgen, wgen ! frequency, angle, phase, wave number, radian frequency
      ! of wavetrain components for boundary signal
      real(dp) :: dfgen ! frequency grid size in the generated components
      type(shortspectrum), dimension(:), pointer :: vargen ! This is where the variance for each wave train at each spectrum location
      ! is stored
      type(shortspectrum), dimension(:), pointer :: vargenq ! This is where the variance for each wave train at each spectrum location
      ! is stored, which is not scaled and is used for the generation of bound waves
      real(dp), dimension(:, :), pointer :: A ! Amplitude, per wave train component, per offshore grid point A(ny+1,K)
      real(dp), dimension(:, :), pointer :: Sfinterp ! S integrated over all directions at frequency locations of fgen,
      ! per offshore grid point Sfinterp(npb,K)
      real(dp), dimension(:, :), pointer :: Sfinterpq ! S integrated over all directions at frequency locations of fgen,
      ! per offshore grid point Sfinterpq(npb,K), uncorrected for generation of bound waves
      real(dp), dimension(:), pointer :: Hm0interp ! Hm0 per offshore point, based on intergration of Sfinterp and used to scale
      ! final time series
      integer, dimension(:), pointer :: Findex ! Index of wave train component locations on frequency/Fourier axis
      integer, dimension(:), pointer :: WDindex ! Index of wave train component locations on wave directional bin axis
      integer, dimension(:), pointer :: PRindex ! Index of wave train components to be phase-resolved (rather than
      ! using energy balance)
      double complex, dimension(:, :), pointer :: CompFn ! Fourier components of the wave trains
      character(1024) :: Efilename, qfilename, nhfilename
      real(dp), dimension(:, :), pointer :: zsits ! time series of total surface elevation for nonhspectrum==1
      real(dp), dimension(:, :), pointer :: uits ! time series of depth-averaged horizontal east velocity nonhspectrum==1 or swkhmin>0
      real(dp), dimension(:, :), pointer :: vits ! time series of depth-averaged horizontal north velocity nonhspectrum==1 or swkhmin>0
      real(dp), dimension(:, :), pointer :: wits ! time series of depth-averaged vertical velocity for nonhspectrum==1  ??
   end type waveparamsnew
   !
   ! These parameters control a lot how the spectra are handled. They could be put in params.txt,
   ! but most users will want to keep these at their default values anyway
   integer, parameter :: nfint = 801 ! size of standard 2D spectrum in frequency dimension
   integer, parameter :: naint = 401 ! size of standard 2D spectrum in angular dimension
   integer, parameter :: Kmin = 200 ! minimum number of wave train components
   real(dp), parameter :: wdmax = 5.d0 ! maximum depth*reliable angular wave frequency that can be resolved by
   ! nonhydrostatic wave model. All frequencies above this are removed
   ! from nonhspectrum generation
   ! Shortcut pointers to commonly used parameters
   integer :: nspectra, bccount, npb
   integer :: ntheta
   integer :: ntheta_s
   integer :: singledir
   real(dp) :: hb0
   ! Physical constants
   real(dp), parameter :: par_pi = 4.d0 * atan(1.d0)
   real(dp), parameter :: par_g = 9.81d0
   complex(kind(0.0d0)), parameter :: par_compi = (0.0d0, 1.0d0)
   ! others
   integer, save :: ind_end_taper
   real(dp), dimension(:, :), allocatable, save :: lastwaveelevation ! wave height at the end of the last spectrum
contains

   subroutine generate_wave_boundary_surfbeat(ibnd, durationlength)

      use m_xbeach_filefunctions
      use m_xbeach_typesandkinds
      use m_xbeach_data

      use wave_boundary_datastore
      implicit none
      ! Input and output variables
      integer, intent(in) :: ibnd
      real(dp), intent(out) :: durationlength
      !
      !
      ! Internal variables
      type(spectrum), dimension(:), allocatable :: specin, specinterp
      type(spectrum) :: combspec
      type(waveparamsnew) :: wp ! Most will be deallocated, but some variables useful to keep?
      integer :: iloc
      real(dp) :: fmax
      real(dp), dimension(:), allocatable :: wL, wR
      integer, dimension(:), allocatable :: kL, kR

      type(filenames), save :: waveSpectrumFileName

      ! Shortcuts, could be pointers?
      nspectra = waveSpectrumAdministration(ibnd)%nspectra
      bccount = waveSpectrumAdministration(ibnd)%bccount
      ntheta = waveBoundaryParameters(ibnd)%ntheta
      ! Offshore water depth, which is used in various computations in this module
      hb0 = waveBoundaryParameters(ibnd)%hboundary
      npb = waveBoundaryParameters(ibnd)%np
      ntheta_s = waveBoundaryParameters(ibnd)%ntheta_s
      singledir = waveBoundaryParameters(ibnd)%singledir
      if (.not. allocated(wL)) then
         allocate (wR(1:npb), wL(1:npb), kR(1:npb), kL(1:npb))
      end if
      kL = waveSpectrumAdministration(ibnd)%kL ! spectrum number in nspectra
      kR = waveSpectrumAdministration(ibnd)%kR
      wR = waveSpectrumAdministration(ibnd)%wR ! associated weight
      wL = waveSpectrumAdministration(ibnd)%wL
      !
      !
      ! Start Wave boundary condition time series generation
      if (waveSpectrumAdministration(ibnd)%repeatwbc) then
         ! Return wave boundary conditions that have already been computed in a previous
         ! call to this subroutine. Modify time axis to reflect shift in time since the
         ! previous call
         waveBoundaryTimeSeries(ibnd)%tbc = min(waveBoundaryTimeSeries(ibnd)%tbc, &
                                                huge(0.d0) - waveBoundaryAdministration(ibnd)%startComputeNewSeries) + &
                                            waveBoundaryAdministration(ibnd)%startComputeNewSeries

      else

         call writelog('l', '', '--------------------------------')
         call writelog('ls', '', 'Calculating spectral wave boundary conditions ')
         call writelog('l', '', '--------------------------------')

         ! allocate temporary input storage
         if (.not. allocated(specin)) then
            allocate (specin(nspectra))
            allocate (specinterp(nspectra))
         end if

         ! Read through input spectra files
         fmax = 1.d0 ! assume 1Hz as maximum frequency. Increase in loop below if needed.
         do iloc = 1, nspectra

            call writelog('sl', '(a,i0)', 'Reading spectrum at location ', iloc)

            ! Read input file
            waveSpectrumFileName%fname = waveSpectrumAdministration(ibnd)%bcfiles(iloc)%fname
            waveSpectrumFileName%listline = wavespectrumadministration(ibnd)%bcfiles(iloc)%listline

            call read_spectrum_input(ibnd, wp, waveSpectrumFileName, specin(iloc))

            wavespectrumadministration(ibnd)%bcfiles(iloc)%listline = waveSpectrumFileName%listline

            write (6, *) 'Spectrum read from: ', trim(waveSpectrumAdministration(ibnd)%bcfiles(iloc)%fname)

            fmax = max(fmax, maxval(specin(iloc)%f))
         end do
         do iloc = 1, nspectra

            call writelog('sl', '(a,i0)', 'Interpreting spectrum at location ', iloc)

            ! Interpolate input 2D spectrum to standard 2D spectrum
            call interpolate_spectrum(ibnd, specin(iloc), specinterp(iloc), fmax)

            call writelog('sl', '', 'Values calculated from interpolated spectrum:')
            call writelog('sl', '(a,f0.2,a)', 'Hm0       = ', specinterp(iloc)%hm0, ' m')
            call writelog('sl', '(a,f0.2,a)', 'Trep      = ', specinterp(iloc)%trep, ' s')
            call writelog('sl', '(a,f0.2,a)', 'Mean dir  = ', mod(specinterp(iloc)%dirm, 360.d0), ' degN')

         end do

         ! Determine whether all the spectra are to be reused, which implies that the global repeatwbc should be
         ! set to true (no further computations required in future calls)
         ! JRE TO DO: check this
         call set_repeatwbc(ibnd)
         !
         ! calculate the mean combined spectra (used for combined Trep, determination of wave components, etc.)
         ! now still uses simple averaging, but could be improved to use weighting for distance etc.
         !
         ! JRE TO DO: weighted interpolation, ref to nspectrumloc>1
         !
         call generate_combined_spectrum(ibnd, specinterp, combspec)
         !call generate_combined_spectrum_weighted(ibnd, npb, kL, kR, wL, wR, specinterp, combspec)
         !
         ! Store these data in wave administration for exchange with outer
         ! XBeach or D3D/FM models
         waveSpectrumAdministration(ibnd)%Hbc = combspec%hm0
         waveSpectrumAdministration(ibnd)%Tbc = combspec%trep
         waveSpectrumAdministration(ibnd)%Dbc = combspec%dir0

         call writelog('sl', '(a,f0.2,a)', 'Overall Trep from all spectra calculated: ', &
                       waveSpectrumAdministration(ibnd)%Tbc, ' s')

         if (waveboundaryParameters(ibnd)%singledir > 0) then
            call set_stationary_spectrum(ibnd, combspec)
         end if
         ! Wave trains that are used by XBeach. The number of wave trains, their frequencies and directions
         ! are based on the combined spectra of all the locations to ensure all wave conditions are
         ! represented in the XBeach model
         call generate_wavetrain_components(ibnd, combspec, wp)

         ! We can now apply a correction to the wave train components if necessary. This section can be
         ! improved later
         if (nspectra == 1 .and. specin(1)%scoeff > 1000.d0) then
            ! this can be used both for Jonswap and vardens input
            wp%thetagen = mod(specin(1)%dir0, 2 * par_pi)
         end if

         ! Set up time axis, including the time axis for output to boundary condition files and an
         ! internal time axis, which may differ in length to the output time axis
         call generate_wave_time_axis(ibnd, wp)

         ! Determine the variance for each wave train component, at every spectrum location point
         call generate_wave_train_variance(wp, specinterp)

         ! Determine the amplitude of each wave train component, at every point along the
         ! offshore boundary
         ! ROBERT: to fix after input sorted
         call generate_wave_train_properties_per_offshore_point(ibnd, wp)

         ! Generate Fourier components for all wave train component, at every point along
         ! the offshore boundary
         call generate_wave_train_Fourier(ibnd, wp)

         ! Time series of short wave energy or surface elevation
         ! Distribute all wave train components among the wave direction bins. Also rearrage
         ! the randomly drawn wave directions to match the centres of the wave bins if the
         ! user-defined nspr is set on.
         call distribute_wave_train_directions(ibnd, wp, waveBoundaryParameters(ibnd)%nspr)
         if (.not. waveBoundaryParameters(ibnd)%nonhspectrum) then
            ! if we want to send some low-frequency swell waves into the model in the NLSWE then
            ! separate here into a mix of wave action balance and NLSWE components
            if (waveBoundaryParameters(ibnd)%swkhmin > 0.d0) then
               ! recalculate Trep
               call tpDcalc(sum(wp%Sfinterp, DIM=1) / (npb) * (1 - wp%PRindex), wp%fgen, waveSpectrumAdministration(ibnd)%Tbc, &
                            & waveBoundaryParameters(ibnd)%trepfac, waveBoundaryParameters(ibnd)%Tm01switch)
               call writelog('sl', '', 'Trep recomputed to account only for components in wave action balance.')
               call writelog('sl', '(a,f0.2,a)', 'New Trep in wave action balance: ', waveSpectrumAdministration(ibnd)%Tbc, ' s')
               !
               ! Calculate the wave energy envelope per offshore grid point and write to output file
               call generate_ebcf(ibnd, wp) ! note, this subroutine will account for only non-phase-resolved components
               ! Generate time series of surface elevation and horizontal velocity, only for phase-resolved components
               call generate_swts(ibnd, wp)
            else
               ! Only do wave action balance stuff
               !
               ! Calculate the wave energy envelope per offshore grid point and write to output file
               call generate_ebcf(ibnd, wp)
            end if ! swkhmin>0.d0
         else
            ! Generate time series of surface elevation, horizontal velocity and vertical velocity
            call generate_swts(ibnd, wp)
         end if

         ! Calculate the bound long wave from the wave train components and write to output file
         call generate_qbcf(ibnd, wp)

         ! Write non-hydrostatic time series of combined short and long waves if necessary
         if (waveBoundaryParameters(ibnd)%nonhspectrum) then
            call generate_nhtimeseries_file(ibnd, wp)
         end if

         ! Deallocate a lot of memory
         deallocate (specin, specinterp)
         deallocate (wp%tin, wp%taperf, wp%taperw)
         deallocate (wp%fgen, wp%thetagen, wp%phigen, wp%kgen, wp%wgen)
         deallocate (wp%vargen)
         deallocate (wp%vargenq)
         deallocate (wp%Sfinterp)
         deallocate (wp%Sfinterpq)
         deallocate (wp%Hm0interp)
         deallocate (wp%A)
         deallocate (wp%Findex)
         deallocate (wp%CompFn)
         deallocate (wp%PRindex)
         if (.not. waveBoundaryParameters(ibnd)%nonhspectrum) then
            deallocate (wp%WDindex)
            if (waveBoundaryParameters(ibnd)%swkhmin > 0.d0) then
               deallocate (wp%zsits)
               deallocate (wp%uits)
            end if
         else
            deallocate (wp%zsits)
            deallocate (wp%uits)
         end if
         ! Send message to screen and log

         call writelog('l', '', '--------------------------------')
         call writelog('ls', '', 'Spectral wave boundary conditions complete ')
         call writelog('l', '', '--------------------------------')

      end if ! repeatwbc
      !
      ! Return the duration length of the current boundary conditions
      durationlength = wp%rtbc
      !
      ! Update the number of boundary conditions generated by this module
      bccount = bccount + 1

   end subroutine generate_wave_boundary_surfbeat

   ! --------------------------------------------------------------
   ! ---------------- Read input spectra files --------------------
   ! --------------------------------------------------------------
   subroutine read_spectrum_input(ibnd, wp, fn, specin)

      use m_xbeach_filefunctions
      use m_xbeach_data

      implicit none
      ! Interface
      integer, intent(in) :: ibnd
      type(waveparamsnew), intent(inout) :: wp
      type(filenames), intent(inout) :: fn
      type(spectrum), intent(inout) :: specin
      ! internal
      integer :: fid
      character(8) :: testline
      character(1024) :: readfile
      logical :: filelist
      integer :: i, ier

      ! check the first line of the boundary condition file for FILELIST keyword
      open (newunit=fid, file=fn%fname, status='old', form='formatted')
      read (fid, *, iostat=ier) testline
      if (ier /= 0) then
         call report_file_read_error(fn%fname)
      end if
      if (trim(testline) == 'FILELIST') then
         filelist = .true.
         ! move listline off its default position of zero to the first row
         if (fn%listline == 0) then
            fn%listline = 1
         end if
         fn%repeat = .false.
      else
         filelist = .false.
         if (trim(instat) /= 'jons_table') then
            fn%repeat = .true.
         else
            fn%repeat = .false.
         end if
      end if
      close (fid)
      ! If file has list of spectra, read through the lines to find the correct
      ! spectrum file name and rtbc and dtbc. Else the filename and rtbc, dtbc are in
      ! params.txt
      if (filelist) then
         open (newunit=fid, file=fn%fname, status='old', form='formatted')
         do i = 1, fn%listline
            read (fid, *) testline ! old stuff, not needed anymore
         end do
         read (fid, *, iostat=ier) wp%rtbc, wp%dtbc, readfile ! new boundary condition
         if (ier /= 0) then
            call report_file_read_error(fn%fname)
         end if
         ! we have to adjust this to morphological time, as done in params.txt
         !if (     morfacopt==1) then
         !   wp%rtbc = wp%rtbc/max(     morfac,1.d0)
         !endif
         fn%listline = fn%listline + 1 ! move one on from the last time we opened this file
         close (fid)
      else
         wp%rtbc = rt ! already set to morphological time
         wp%dtbc = dtbc
         readfile = fn%fname
      end if

      ! based on the value of instat, we need to read either Jonswap, Swan or vardens files
      ! note: jons_table is also handled by read_jonswap_file subroutine
      select case (trim(instat))
         !case ('jons')
      case ('jons', 'jons_table')
         ! wp type sent in to receive rtbc and dtbc from jons_table file
         ! fn%listline sent in to find correct row in jons_table file
         call read_jonswap_file(wp, readfile, fn%listline, specin)
      case ('swan')
         call read_swan_file(ibnd, readfile, specin)
      case ('vardens')
         call read_vardens_file(readfile, specin)
      end select

   end subroutine read_spectrum_input

   ! --------------------------------------------------------------
   ! ------------------- Read JONSWAP files -----------------------
   ! --------------------------------------------------------------
   subroutine read_jonswap_file(wp, readfile, listline, specin)

      use m_xbeach_filefunctions
      use m_xbeach_data
      use m_xbeach_readkey
      use m_xbeach_errorhandling

      implicit none

      ! Input / output variables
      type(waveparamsnew), intent(inout) :: wp
      character(len=*), intent(IN) :: readfile
      integer, intent(INOUT) :: listline
      type(spectrum), intent(inout) :: specin

      ! Internal variables
      integer :: i, ii, ier, ip, ind
      integer :: nmodal
      integer :: fid
      integer :: forcepartition
      integer, dimension(2) :: indvec
      integer, dimension(:), allocatable :: tma
      real(dp), dimension(:), allocatable :: x, y, Dd, tempdir
      real(dp), dimension(:), allocatable :: Hm0, fp, gam, mainang, scoeff
      real(dp) :: dfj, fnyq
      real(dp) :: Tp
      character(len=80) :: dummystring
      type(spectrum), dimension(:), allocatable :: multinomalspec, scaledspec
      logical :: cont
      real(dp), dimension(:), allocatable :: scalefac1, scalefac2, tempmax, avgscale
      real(dp), dimension(:), allocatable :: oldvariance, newvariance
      real(dp) :: newconv, oldconv
      real(dp) :: LL, LL0, sigmatma, k, nn, hh

      ! First part: read JONSWAP parameter data

      ! Check whether spectrum characteristics or table should be used
      if (trim(instat) /= 'jons_table') then
         ! Use spectrum characteristics
         call writelog('sl', '', 'Reading jonswap info from ', trim(readfile), ' ...')
         !
         ! First read if the spectrum is multinodal, and how many peaks there should be
         !
         nmodal = readkey_int(readfile, 'nmodal', 1, 1, 4)
         if (nmodal < 1) then
            call writelog('lswe', '', 'Error: number of spectral partions may not be less than 1 in ', trim(readfile))
            call xbeach_errorhandler()
         end if
         !
         ! Allocate space for all spectral parameters
         !
         allocate (Hm0(nmodal))
         allocate (fp(nmodal))
         allocate (gam(nmodal))
         allocate (mainang(nmodal))
         allocate (scoeff(nmodal))
         allocate (tma(nmodal))
         !
         ! Read the spectral parameters for all spectrum components
         !
         ! Wave height (required)
         Hm0 = readkey_dblvec(readfile, 'Hm0', nmodal, nmodal, 0.0d0, 0.0d0, 5.0d0, bcast=.false., required=.true.)
         !
         ! Wave period (required)
         ! allow both Tp and fp specification to bring in line with params.txt
         if (isSetParameter(readfile, 'Tp', bcast=.false.) .and. .not. isSetParameter(readfile, 'fp', bcast=.false.)) then
            fp = 1.d0 / readkey_dblvec(readfile, 'Tp', nmodal, nmodal, 12.5d0, 2.5d0, 20.0d0, bcast=.false.)
         elseif (isSetParameter(readfile, 'fp', bcast=.false.) .and. .not. isSetParameter(readfile, 'Tp', bcast=.false.)) then
            fp = readkey_dblvec(readfile, 'fp', nmodal, nmodal, 0.08d0, 0.0625d0, 0.4d0, bcast=.false.)
         elseif (.not. isSetParameter(readfile, 'fp', bcast=.false.) .and. .not. isSetParameter(readfile, 'Tp', bcast=.false.)) then
            call writelog('lswe', '', 'Error: missing required value for parameter ''Tp'' or ''fp'' in ', trim(readfile))
            call xbeach_errorhandler()
         else
            fp = 1.d0 / readkey_dblvec(readfile, 'Tp', nmodal, nmodal, 12.5d0, 2.5d0, 20.0d0, bcast=.false.)
            call writelog('lsw', '', 'Warning: selecting to read peak period (Tp) instead of frequency (fp) in ', trim(readfile))
         end if
         !
         ! Wave spreading in frequency domain (peakedness)
         !
         gam = readkey_dblvec(readfile, 'gammajsp', nmodal, nmodal, 3.3d0, 1.0d0, 5.0d0, bcast=.false.)
         !
         ! Wave spreading in directional domain
         !
         scoeff = readkey_dblvec(readfile, 's', nmodal, nmodal, 10.0d0, 1.0d0, 1000.0d0, bcast=.false.)
         !
         ! TMA
         !
         tma = readkey_intvec(readfile, 'tma', nmodal, nmodal, 0, 0, 1, bcast=.false.)
         !
         ! Main wave direction
         ! allow both mainang and dir0 specification to bring in line with params.txt
         if (isSetParameter(readfile, 'mainang', bcast=.false.) .and. &
             .not. isSetParameter(readfile, 'dir0', bcast=.false.)) then
            mainang = readkey_dblvec(readfile, 'mainang', nmodal, nmodal, &
                                     270.0d0, 0.0d0, 360.0d0, bcast=.false.)
         elseif (isSetParameter(readfile, 'dir0', bcast=.false.) .and. &
                 .not. isSetParameter(readfile, 'mainang', bcast=.false.)) then
            mainang = readkey_dblvec(readfile, 'dir0', nmodal, nmodal, &
                                     270.0d0, 0.0d0, 360.0d0, bcast=.false.)
         elseif (.not. isSetParameter(readfile, 'dir0', bcast=.false.) .and. &
                 .not. isSetParameter(readfile, 'mainang', bcast=.false.)) then
            mainang = 270.d0
         else
            mainang = readkey_dblvec(readfile, 'mainang', nmodal, nmodal, 270.0d0, 0.0d0, 360.0d0, bcast=.false.)
            call writelog('lsw', '', 'Warning: selecting to read ''mainang'' instead of ''dir0'' in ', trim(readfile))
         end if
         !
         ! Nyquist parameters used only in this subroutine
         ! are not read individually for each spectrum partition
         if (oldnyq == 1) then
            fnyq = readkey_dbl(readfile, 'fnyq', 0.3d0, 0.2d0, 1.0d0, bcast=.false.)
         else
            fnyq = readkey_dbl(readfile, 'fnyq', max(0.3d0, 3.d0 * maxval(fp)), 0.2d0, 1.0d0, bcast=.false.)
         end if
         dfj = readkey_dbl(readfile, 'dfj', fnyq / 200, fnyq / 1000, fnyq / 20, bcast=.false.)
         !
         ! Finally check if XBeach should accept even the most stupid partioning (sets error level to warning
         ! level when computing partition overlap
         if (nmodal > 1) then
            forcepartition = readkey_int(readfile, 'forcepartition', 0, 0, 1, bcast=.false.)
         end if
         ! check for other strange values in this file
         call readkey(readfile, 'checkparams', dummystring)
      else
         nmodal = 1
         allocate (Hm0(nmodal))
         allocate (fp(nmodal))
         allocate (gam(nmodal))
         allocate (mainang(nmodal))
         allocate (scoeff(nmodal))
         allocate (tma(nmodal))
         ! Use spectrum table
         call writelog('sl', '', 'Reading jonswap info from table ', trim(readfile), ' ...')
         open (newunit=fid, file=readfile, status='old', form='formatted')
         ! read junk up to the correct line in the file
         do i = 1, listline ! must be reset when reading second spectrum locfile
            read (fid, *, iostat=ier) dummystring
            if (ier /= 0) then
               call report_file_read_error(readfile)
            end if
         end do
         read (fid, *, iostat=ier) Hm0(1), Tp, mainang(1), gam(1), scoeff(1), wp%rtbc, wp%dtbc
         if (ier /= 0) then
            call writelog('lswe', '', 'Error reading file ', trim(readfile))
            close (fid)
            call xbeach_errorhandler()
         end if
         ! move the line pointer in the file
         listline = listline + 1
         ! convert to morphological time
         !if (     morfacopt==1) then
         !   wp%rtbc = wp%rtbc/max(     morfac,1.d0)
         !endif
         fp(1) = 1.d0 / Tp
         fnyq = 3.d0 * fp(1)
         dfj = fp(1) / 50
         tma(1) = 0
         close (fid)
      end if
      !
      ! Second part: generate 2D spectrum from input parameters
      !
      ! Define number of frequency bins by defining an array of the necessary length
      ! using the Nyquist frequency and frequency step size
      specin%nf = ceiling((fnyq - dfj) / dfj)
      specin%df = dfj
      !
      ! Define array with actual eqidistant frequency bins
      allocate (specin%f(specin%nf))
      do i = 1, specin%nf
         specin%f(i) = i * dfj
      end do
      !
      ! we need a normalised frequency and variance vector for JONSWAP generation
      allocate (x(size(specin%f)))
      allocate (y(size(specin%f)))
      !
      ! Define 200 directions relative to main angle running from 0 to 2*pi
      ! we also need a temporary vector for direction distribution
      specin%nang = naint ! = 401, standard value
      allocate (tempdir(specin%nang))
      allocate (Dd(specin%nang))
      allocate (specin%ang(specin%nang))
      specin%dang = 2 * par_pi / dble(naint - 1)
      do i = 1, specin%nang
         specin%ang(i) = (i - 1) * specin%dang
      end do
      !
      ! Generate density spectrum for each spectrum partition
      !
      allocate (multinomalspec(nmodal))
      !
      do ip = 1, nmodal
         ! relative frequenct vector
         x = specin%f / fp(ip)
         ! Calculate unscaled and non-directional JONSWAP spectrum using
         ! peak-enhancement factor and pre-determined frequency bins
         call jonswapgk(x, gam(ip), y)
         ! Determine scaled and non-directional JONSWAP spectrum using the JONSWAP
         ! characteristics
         if (tma(ip) == 1) then
            do ii = 1, specin%nf
               LL0 = par_g * (1 / specin%f(ii))**2 / 2d0 / par_pi ! deep water wave length
               LL = iteratedispersion(LL0, LL0, par_pi, hb0)
               if (LL < 0.d0) then
                  call writelog('lsw', '', 'No dispersion convergence found for wave train ', i, &
                                ' in boundary condition generation')
                  LL = -LL
               end if
               k = 2 * par_pi / LL
               hh = hb0
               nn = 0.5d0 * (1 + k * hh * ((1 - tanh(k * hh)**2) / (tanh(k * hh))))
               sigmatma = ((1 / 2.d0 / nn) * tanh(k * hh)**2)
               y(ii) = y(ii) * sigmatma
            end do
         end if
         y = (Hm0(ip) / (4.d0 * sqrt(sum(y) * dfj)))**2 * y
         ! Convert main angle from degrees to radians and from nautical convention to
         ! internal grid
         mainang(ip) = (1.5d0 * par_pi) - mainang(ip) * par_pi / 180
         ! Make sure the main angle is defined between 0 and 2*pi
         do while (mainang(ip) > 2 * par_pi .or. mainang(ip) < 0.d0) !Robert en Ap
            if (mainang(ip) > 2 * par_pi) then
               mainang(ip) = mainang(ip) - 2 * par_pi
            elseif (mainang(ip) < 0.d0) then
               mainang(ip) = mainang(ip) + 2 * par_pi
            end if
         end do
         ! Convert 200 directions relative to main angle to directions relative to
         ! internal grid
         ! Bas: apparently division by 2 for cosine law happens already here
         tempdir = (specin%ang - mainang(ip)) / 2d0
         ! Make sure all directions around the main angle are defined between 0 and 2*pi
         do while (any(tempdir > 2d0 * par_pi) .or. any(tempdir < 0.d0))
            where (tempdir > 2d0 * par_pi)
               tempdir = tempdir - 2d0 * par_pi
            elsewhere(tempdir < 0.d0)
               tempdir = tempdir + 2d0 * par_pi
            end where
         end do
         ! Calculate directional spreading based on cosine law
         Dd = cos(tempdir)**(2 * nint(scoeff(ip))) ! Robert: apparently nint is needed here, else MATH error
         ! Scale directional spreading to have a surface of unity by dividing by its
         ! own surface
         Dd = Dd / (sum(Dd) * specin%dang)
         ! Define two-dimensional variance density spectrum array and distribute
         ! variance density for each frequency over directional bins
         allocate (multinomalspec(ip)%S(specin%nf, specin%nang))
         do i = 1, specin%nang
            do ii = 1, specin%nf
               multinomalspec(ip)%S(ii, i) = y(ii) * Dd(i)
            end do
         end do
      end do ! 1,nmodal
      do ip = 1, nmodal
         write (*, *) 'Hm0 = ', 4 * sqrt(sum(multinomalspec(ip)%S) * specin%dang * specin%df)
      end do
      !
      ! Combine spectrum partitions so that the total spectrum is correct
      !
      if (nmodal == 1) then
         ! Set all the useful parameters and arrays
         specin%Hm0 = Hm0(1)
         specin%fp = fp(1)
         specin%dir0 = mainang(1)
         specin%scoeff = scoeff(1)
         allocate (specin%S(specin%nf, specin%nang))
         specin%S = multinomalspec(1)%S
      else
         specin%Hm0 = sqrt(sum(Hm0**2)) ! total wave height
         ind = maxval(maxloc(Hm0)) ! spectrum that is largest
         specin%fp = fp(ind) ! not really used in further calculation
         specin%dir0 = mainang(ind) ! again not really used in further calculation
         ! if all scoeff>1000 then all waves should be in the same direction exactly
         if (all(scoeff >= 1024.d0) .and. all(mainang == mainang(ind))) then
            specin%scoeff = 1024.d0
         else
            specin%scoeff = min(scoeff(ind), 999.d0)
         end if
         ! Now we have to loop over all partitioned spectra. Where two or more spectra
         ! overlap, only the largest is counted, and all others are set to zero. Afterwards
         ! all spectra are scaled so that the total energy is maintained. Since the scaling
         ! affects where spectra overlap, this loop is repeated until only minor changes
         ! in the scaling occur. Warnings or errors are given if the spectrum is scaled too
         ! much, i.e. spectra overlap too much.
         !
         ! Allocate space
         allocate (scaledspec(nmodal)) ! used to store scaled density spectra
         do ip = 1, nmodal
            allocate (scaledspec(ip)%S(specin%nf, specin%nang))
            scaledspec(ip)%S = multinomalspec(ip)%S
         end do
         allocate (scalefac1(nmodal)) ! this is the scaling factor required to maintain Hm0
         ! including that parts of the spectrum are set to zero
         scalefac1 = 1.d0
         allocate (scalefac2(nmodal)) ! this is the scaling factor required to maintain Hm0
         ! in the previous iteration
         scalefac2 = 1.d0
         allocate (avgscale(nmodal))
         avgscale = 1.d0
         ! these are convergence criteria
         newconv = 0.d0
         oldconv = huge(0.d0)
         cont = .true.
         allocate (tempmax(nmodal)) ! used to store maximum value in f,theta space
         allocate (oldvariance(nmodal)) ! used to store the sum of variance in the original partitions
         do ip = 1, nmodal
            oldvariance(ip) = sum(multinomalspec(ip)%S)
         end do
         allocate (newvariance(nmodal)) ! used to store the sum of variance in the new partitions
         !
         ! Start convergence loop
         do while (cont)
            avgscale = (avgscale + scalefac1) / 2
            ! First scale the spectra
            do ip = 1, nmodal
               ! scale the spectrum using less than half the additional scale factor, this to
               ! ensure the method does not become unstable
               ! scaledspec(ip)%S = multinomalspec(ip)%S*(0.51d0+scalefac1(ip)*0.49d0)
               ! scaledspec(ip)%S = multinomalspec(ip)%S*(scalefac1(ip)+scalefac2(ip))/2
               scaledspec(ip)%S = multinomalspec(ip)%S * avgscale(ip)

            end do
            do i = 1, specin%nang
               do ii = 1, specin%nf
                  ! vector of variance densities at this point in f,theta space
                  do ip = 1, nmodal
                     tempmax(ip) = scaledspec(ip)%S(ii, i)
                  end do
                  ! All spectra other than the one that is greatest in this point
                  ! is set to zero. In case multiple spectra are equal largest,
                  ! the first spectrum in the list is chosen (minval(maxloc))
                  ind = minval(maxloc(tempmax))
                  do ip = 1, nmodal
                     if (ip /= ind) then
                        scaledspec(ip)%S(ii, i) = 0.d0
                     end if
                  end do
               end do
            end do
            !
            ! Now rescale adjusted partition spectra so that they match the incident Hm0
            scalefac2 = scalefac1 ! keep previous results
            do ip = 1, nmodal
               newvariance(ip) = sum(scaledspec(ip)%S)
               if (newvariance(ip) > 0.01d0 * oldvariance(ip)) then
                  scalefac1(ip) = oldvariance(ip) / newvariance(ip) ! want to maximise to a factor 2
                  ! else can generate rediculous results
               else
                  scalefac1(ip) = 0.d0 ! completely remove this spectrum
               end if
            end do
            !
            ! check convergence criteria (if error is increasing, we have passed best fit)
            newconv = maxval(abs(scalefac2 - scalefac1) / scalefac1)
            if (newconv < 0.0001d0 .or. abs(newconv - oldconv) < 0.0001d0) then
               cont = .false.
            end if
            oldconv = newconv
         end do
         ! Ensure full energy conservation by scaling now according to full scalefac, not just the
         ! half used in the iteration loop
         do ip = 1, nmodal
            scaledspec(ip)%S = multinomalspec(ip)%S * scalefac1(ip)
         end do
         !
         ! Now check the total scaling that has taken place across the spectrum, also accounting
         ! for the fact that some parts of the spectra are set to zero. This differs from the
         ! scalefac1 and scalefac2 vectors in the loop that only adjust the part of the spectrum
         ! that is not zero.
         do ip = 1, nmodal
            write (*, *) 'Hm0m = ', 4 * sqrt(sum(scaledspec(ip)%S) * specin%dang * specin%df)
         end do
         do ip = 1, nmodal
            indvec = maxloc(scaledspec(ip)%S)
            scalefac1(ip) = scaledspec(ip)%S(indvec(1), indvec(2)) / &
                            multinomalspec(ip)%S(indvec(1), indvec(2)) - 1.d0
         end do
         !
         ! Warning and/or error criteria here if spectra overlap each other too much
         do ip = 1, nmodal
            if (scalefac1(ip) > 0.5d0) then
               if (forcepartition == 1) then
                  call writelog('lsw', '(a,f0.0,a,i0,a,a,a)', &
                                'Warning: ', scalefac1(ip) * 100, '% of energy in spectrum partition ''', ip, &
                                ''' in  ', trim(readfile), ' is overlapped by other partitions')
                  call writelog('lsw', '', ' Check spectral partitioning in ', trim(readfile))
               else
                  call writelog('lswe', '(a,f0.0,a,i0,a,a,a)', &
                                'Error: ', scalefac1(ip) * 100, '% of energy in spectrum partition ''', ip, &
                                ''' in  ', trim(readfile), ' is overlapped by other partitions')
                  call writelog('lswe', '', 'This spectrum overlaps too much with another spectrum partition.', &
                                ' Check spectral partitioning in ', trim(readfile))
                  call writelog('lswe', '', 'If the partitioning should be carried out regardles of energy loss, ', &
                                'set ''forcepartition = 1'' in ', trim(readfile))
                  call xbeach_errorhandler()
               end if
            elseif (scalefac1(ip) > 0.2d0 .and. scalefac1(ip) <= 0.5d0) then
               call writelog('lsw', '(a,f0.0,a,i0,a,a,a)', &
                             'Warning: ', scalefac1(ip) * 100, '% of energy in spectrum partition ''', ip, &
                             ''' in  ', trim(readfile), ' is overlapped by other partitions')
               call writelog('lsw', '', ' Check spectral partitioning in ', trim(readfile))
            elseif (scalefac1(ip) < 0.d0) then
               call writelog('lsw', '(a,i0,a,a,a)', 'Warning: spectrum partition ''', ip, ''' in  ', trim(readfile), &
                             ' has been removed')
               call writelog('lsw', '', 'This spectrum is entirely overlapped by another spectrum partition.', &
                             ' Check spectral partitioning in ', trim(readfile))
            end if
         end do
         !
         ! Now set total spectrum
         allocate (specin%S(specin%nf, specin%nang))
         specin%S = 0.d0
         do ip = 1, nmodal
            specin%S = specin%S + scaledspec(ip)%S
         end do

         deallocate (scaledspec)
         deallocate (tempmax)
         deallocate (scalefac1, scalefac2)
         deallocate (newvariance, oldvariance)
      end if

      ! We need frequency spectrum to ensure Sf remains correct between interpolation
      ! routines
      allocate (specin%Sf(specin%nf))
      specin%Sf = 0.d0
      do i = 1, specin%nf
         do ii = 1, specin%nang
            if (ii == 1) then
               specin%Sf(i) = specin%Sf(i) + specin%S(i, ii) * abs(specin%ang(2) - specin%ang(1))
            elseif (ii == specin%nang) then
               specin%Sf(i) = specin%Sf(i) + specin%S(i, ii) * abs(specin%ang(ii) - specin%ang(ii - 1))
            else
               specin%Sf(i) = specin%Sf(i) + specin%S(i, ii) * abs(specin%ang(ii + 1) - specin%ang(ii - 1)) / 2
            end if
         end do
      end do

      deallocate (Dd)
      deallocate (Hm0, fp, gam, mainang, scoeff, tma)
      deallocate (x, y)
      deallocate (tempdir)
      ! TODO dereference pointers first....
      do ip = 1, nmodal
         deallocate (multinomalspec(ip)%S)
      end do
      deallocate (multinomalspec)

   end subroutine read_jonswap_file

   ! -----------------------------------------------------------
   ! --------- JONSWAP  unscaled JONSWAP spectrum --------------
   ! -------------(used by read_jonswap_files)------------------
   subroutine jonswapgk(x, gam, y)

      implicit none
      ! Required input: - x           : nondimensional frequency, divided by the peak frequency
      !                 - gam         : peak enhancement factor, optional parameter (DEFAULT 3.3)
      !                 - y is output : nondimensional relative spectral density, equal to one at the peak

      real(dp), intent(IN) :: gam
      real(dp), dimension(:), intent(IN) :: x
      real(dp), dimension(:), intent(INOUT) :: y

      ! Internal variables
      real(dp), dimension(size(x)) :: xa, sigma, fac1, fac2, fac3, temp

      xa = abs(x)

      where (xa == 0d0)
         xa = 1e-20
      end where

      sigma = xa

      where (sigma < 1d0)
         sigma = 0.07
      end where

      where (sigma >= 1d0)
         sigma = 0.09
      end where

      temp = 0d0 * xa + 1d0

      fac1 = xa**(-5d0)
      fac2 = exp(-1.25 * (xa**(-4d0)))
      fac3 = (gam * temp)**(exp(-((xa - 1)**2d0) / (2.*(sigma**2d0))))

      y = fac1 * fac2 * fac3
      y = y / maxval(y)

      return

   end subroutine jonswapgk
   !
   !
   ! --------------------------------------------------------------
   ! ----------------------Read SWAN files ------------------------
   ! --------------------------------------------------------------
   subroutine read_swan_file(ibnd, readfile, specin)

      use m_xbeach_filefunctions
      use m_xbeach_errorhandling, only: xbeach_errorhandler
      use math_tools, only: flipv, flipa
      use m_xbeach_data, only: DTHETAS_XB

      implicit none

      ! Input / output variables
      integer, intent(in) :: ibnd
      character(len=*), intent(IN) :: readfile
      type(spectrum), intent(inout) :: specin

      ! Internal variables
      character(6) :: rtext
      real(dp) :: factor, exc
      integer :: fid, switch
      integer :: i, ii, ier, ier2, ier3
      logical :: flipped
      integer :: nt, Ashift
      real(dp), dimension(:), allocatable :: temp
      real(dp), dimension(:, :), allocatable :: tempA

      flipped = .false.
      switch = 0

      call writelog('sl', '', 'Reading from SWAN file ', trim(readfile), ' ...')
      open (newunit=fid, file=readfile, form='formatted', status='old')

      ! Read file until RFREQ or AFREQ is found
      do while (switch == 0)
         read (fid, '(a)', iostat=ier) rtext
         if (ier /= 0) then
            call report_file_read_error(readfile)
         end if
         if (rtext == 'RFREQ ') then
            switch = 1
         elseif (rtext == 'AFREQ ') then
            switch = 2
         end if
      end do

      ! Read nfreq and f
      ! Note f is not monotonically increasing in most simulations
      read (fid, *, iostat=ier) specin%nf
      if (ier /= 0) then
         call report_file_read_error(readfile)
      end if
      allocate (specin%f(specin%nf))
      do i = 1, specin%nf
         read (fid, *, iostat=ier) specin%f(i)
         if (ier /= 0) then
            call report_file_read_error(readfile)
         end if
      end do

      ! Convert to absolute frequencies:
      ! STILL TO BE DONE
      if (switch == 1) then
         specin%f = specin%f
      else
         specin%f = specin%f
      end if

      ! Read CDIR or NDIR
      read (fid, '(a)', iostat=ier) rtext
      if (ier /= 0) then
         call report_file_read_error(readfile)
      end if
      if (rtext == 'NDIR  ') then
         switch = 1
      elseif (rtext == 'CDIR  ') then
         switch = 2
      else
         call writelog('ewls', '', 'SWAN directional bins keyword not found')
         call xbeach_errorhandler()
      end if

      ! Read ndir, theta
      read (fid, *, iostat=ier) specin%nang
      if (ier /= 0) then
         call report_file_read_error(readfile)
      end if
      allocate (specin%ang(specin%nang))
      do i = 1, specin%nang
         read (fid, *, iostat=ier) specin%ang(i)
         if (ier /= 0) then
            call report_file_read_error(readfile)
         end if
      end do

      ! Convert angles to cartesian degrees relative to East
      if (switch == 1) then
         ! nautical to cartesian East
         specin%ang = 270.d0 - specin%ang
      else
         ! cartesian to cartesian East
         specin%ang = specin%ang - dthetaS_XB
         ! dthetaS_XB is the (counter-clockwise) angle in the degrees to rotate from the grid x-axis in SWAN to the
         ! x-axis pointing East
      end if

      ! Ensure angles are increasing instead of decreasing
      if (specin%ang(2) < specin%ang(1)) then
         call flipv(specin%ang, size(specin%ang))
         flipped = .true.
      end if

      nt = 0
      Ashift = 0
      ! Make sure that all angles are in range of 0 to 360 degrees
      if (minval(specin%ang) < 0.d0) then
         allocate (temp(specin%nang))
         Ashift = -1
         temp = 0.d0
         do i = 1, specin%nang
            if (specin%ang(i) < 0.d0) then
               specin%ang(i) = specin%ang(i) + 360.0d0
               nt = nt + 1
            end if
         end do
         temp(1:specin%nang - nt) = specin%ang(nt + 1:specin%nang)
         temp(specin%nang - nt + 1:specin%nang) = specin%ang(1:nt)
         specin%ang = temp
         deallocate (temp)
      elseif (maxval(specin%ang) > 360.0d0) then
         allocate (temp(specin%nang))
         Ashift = 1
         temp = 0.d0
         do i = 1, specin%nang
            if (specin%ang(i) > 360.d0) then
               specin%ang(i) = specin%ang(i) - 360.0d0
               nt = nt + 1
            end if
         end do
         temp(nt + 1:specin%nang) = specin%ang(1:specin%nang - nt)
         temp(1:nt) = specin%ang(specin%nang - nt + 1:specin%nang)
         specin%ang = temp
         deallocate (temp)
      end if

      ! convert to radians
      specin%ang = specin%ang * par_pi / 180
      specin%dang = specin%ang(2) - specin%ang(1)

      ! Skip Quant, next line, read VaDens or EnDens
      read (fid, '(a)', iostat=ier) rtext
      read (fid, '(a)', iostat=ier2) rtext
      read (fid, '(a)', iostat=ier3) rtext
      if (ier + ier2 + ier3 /= 0) then
         call report_file_read_error(readfile)
      end if
      if (rtext == 'VaDens') then
         switch = 1
      elseif (rtext == 'EnDens') then
         switch = 2
      else
         call writelog('slwe', '', 'SWAN VaDens/EnDens keyword not found')
         call xbeach_errorhandler()
      end if
      read (fid, '(a)', iostat=ier) rtext
      read (fid, *, iostat=ier2) exc
      if (ier + ier2 /= 0) then
         call report_file_read_error(readfile)
      end if

      i = 0
      ! Find FACTOR keyword
      do while (i == 0)
         read (fid, '(a)', iostat=ier) rtext
         if (ier /= 0) then
            call report_file_read_error(readfile)
         end if
         if (rtext == 'FACTOR') then
            i = 1
         elseif (rtext == 'ZERO  ') then
            call writelog('lswe', '', 'Zero energy density input for this point')
            call xbeach_errorhandler()
         elseif (rtext == 'NODATA') then
            call writelog('lwse', '', 'SWAN file has no data for this point')
            call xbeach_errorhandler()
         end if
      end do
      read (fid, *, iostat=ier) factor
      if (ier /= 0) then
         call report_file_read_error(readfile)
      end if

      ! Read 2D S array
      allocate (specin%S(specin%nf, specin%nang))
      do i = 1, specin%nf
         read (fid, *, iostat=ier) (specin%S(i, ii), ii=1, specin%nang)
         if (ier /= 0) then
            call report_file_read_error(readfile)
         end if
      end do

      ! Finished reading file
      close (fid)

      ! Replace exception value
      where (specin%S == exc)
         specin%S = 0.d0
      end where

      ! If angles were decreasing, flip S_array as also dir is flipped
      if (flipped) then
         call flipa(specin%S, specin%nf, specin%nang, 2)
      end if

      ! If the order of the angles in specin%ang was reordered, so the same in
      ! specin%S array
      if (Ashift == -1) then
         allocate (tempA(specin%nf, specin%nang))
         tempA = 0
         tempA(:, 1:specin%nang - nt) = specin%S(:, nt + 1:specin%nang)
         tempA(:, specin%nang - nt + 1:specin%nang) = specin%S(:, 1:nt)
         specin%S = tempA
         deallocate (tempA)
      elseif (Ashift == 1) then
         allocate (tempA(specin%nf, specin%nang))
         tempA = 0
         tempA(:, nt + 1:specin%nang) = specin%S(:, 1:specin%nang - nt)
         tempA(:, 1:nt) = specin%S(:, specin%nang - nt + 1:specin%nang)
         specin%S = tempA
         deallocate (tempA)
      end if

      ! multiply by SWAN output factor
      specin%S = specin%S * factor

      ! Convert from energy density to variance density
      if (switch == 2) then
         specin%S = specin%S / (waveBoundaryParameters(ibnd)%rho * par_g)
      end if

      ! Convert to m2/Hz/rad
      specin%S = specin%S * 180 / par_pi

      ! We need a value for spreading. The assumption is that it is less than 1000
      ! This way, wp%fgen will not be set to just one angle.
      specin%scoeff = -1.d0
      ! We need to know if hm0 was set explicitly, not the case for Swan files
      specin%hm0 = -1.d0

      ! We need frequency spectrum to ensure Sf remains correct between interpoloation
      ! routines
      allocate (specin%Sf(specin%nf))
      specin%Sf = 0.d0
      do i = 1, specin%nf
         do ii = 1, specin%nang
            if (ii == 1) then
               specin%Sf(i) = specin%Sf(i) + specin%S(i, ii) * abs(specin%ang(2) - specin%ang(1))
            elseif (ii == specin%nang) then
               specin%Sf(i) = specin%Sf(i) + specin%S(i, ii) * abs(specin%ang(ii) - specin%ang(ii - 1))
            else
               specin%Sf(i) = specin%Sf(i) + specin%S(i, ii) * abs(specin%ang(ii + 1) - specin%ang(ii - 1)) / 2
            end if
         end do
      end do

   end subroutine read_swan_file

   ! --------------------------------------------------------------
   ! -----------------Read variance density files -----------------
   ! --------------------------------------------------------------
   subroutine read_vardens_file(readfile, specin)

      use m_xbeach_filefunctions
      use m_xbeach_errorhandling, only: xbeach_errorhandler
      use math_tools, only: flipv, flipa

      implicit none

      ! Input / output variables
      character(len=*), intent(IN) :: readfile
      type(spectrum), intent(inout) :: specin

      ! Internal variables
      integer :: fid, i, ii, nnz, ier

      ! Open file to start read
      call writelog('sl', '', 'Reading from vardens file: ', trim(readfile), ' ...')
      open (newunit=fid, file=readfile, form='formatted', status='old')

      ! Read number of frequencies and frequency vector
      read (fid, *, iostat=ier) specin%nf
      if (ier /= 0) then
         call report_file_read_error(readfile)
      end if
      allocate (specin%f(specin%nf))
      do i = 1, specin%nf
         read (fid, *, iostat=ier) specin%f(i)
         if (ier /= 0) then
            call report_file_read_error(readfile)
         end if
      end do

      ! Read number of angles and angles vector
      read (fid, *, iostat=ier) specin%nang
      if (ier /= 0) then
         call report_file_read_error(readfile)
      end if
      allocate (specin%ang(specin%nang))
      do i = 1, specin%nang
         read (fid, *, iostat=ier) specin%ang(i)
         if (ier /= 0) then
            call report_file_read_error(readfile)
         end if
      end do

      ! Convert from degrees to rad
      specin%ang = specin%ang * par_pi / 180
      specin%dang = specin%ang(2) - specin%ang(1)

      ! Read 2D S array
      allocate (specin%S(specin%nf, specin%nang))
      do i = 1, specin%nf
         read (fid, *, iostat=ier) (specin%S(i, ii), ii=1, specin%nang)
         if (ier /= 0) then
            call report_file_read_error(readfile)
         end if
      end do

      ! Finished reading file
      close (fid)

      ! Convert to m2/Hz/rad
      specin%S = specin%S * 180 / par_pi

      ! We need a value for spreading. The assumption is that it is less than 1000
      ! if more than one direction column has variance. If only one column has variance
      ! we assume the model requires long crested waves, and wp%thetagen should be exactly
      ! equal to the input direction. We then also need to find the dominant angle.
      ! First flatten spectrum to direction only, then determine if all but one are zero,
      ! use the direction of the non-zero as the main wave direction, set spreading to
      ! a high value (greater than 1000). Else set spreading to a negative value.
      allocate (specin%Sd(specin%nang))
      specin%Sd = sum(specin%S, DIM=1)
      nnz = count(specin%Sd > 0.d0)
      if (nnz == 1) then
         specin%scoeff = 1024.d0
         specin%dir0 = specin%ang(minval(maxloc(specin%Sd)))
      else
         specin%scoeff = -1.d0
      end if

      ! We need frequency spectrum to ensure Sf remains correct between interpoloation
      ! routines
      allocate (specin%Sf(specin%nf))
      specin%Sf = 0.d0
      do i = 1, specin%nf
         do ii = 1, specin%nang
            if (ii == 1) then
               specin%Sf(i) = specin%Sf(i) + specin%S(i, ii) * abs(specin%ang(2) - specin%ang(1))
            elseif (ii == specin%nang) then
               specin%Sf(i) = specin%Sf(i) + specin%S(i, ii) * abs(specin%ang(ii) - specin%ang(ii - 1))
            else
               specin%Sf(i) = specin%Sf(i) + specin%S(i, ii) * abs(specin%ang(ii + 1) - specin%ang(ii - 1)) / 2d0
            end if
         end do
      end do

      ! We need to know if hm0 was set explicitly, not the case for vardens files
      specin%hm0 = -1.d0

   end subroutine read_vardens_file

   ! --------------------------------------------------------------
   ! ------------- Interpolate to standard spectrum ---------------
   ! --------------------------------------------------------------
   subroutine interpolate_spectrum(ibnd, specin, specinterp, fmax)

      use interp

      implicit none

      ! input/output
      integer, intent(in) :: ibnd
      type(spectrum), intent(in) :: specin
      type(spectrum), intent(inout) :: specinterp
      real(dp), intent(in) :: fmax
      ! internal
      integer :: i, j, dummy
      real(dp) :: m0, df, dang
      real(dp) :: hm0pre, hm0post, Sfnow, factor, tempt0
      real(dp), dimension(:, :), allocatable :: Stemp

      double precision, parameter :: dtol = 1d-16

      double precision :: xcycle

      ! allocate size of f,ang,Sf and S arrays in specinterp
      allocate (specinterp%f(nfint))
      allocate (specinterp%ang(naint))
      allocate (specinterp%Sf(nfint))
      allocate (specinterp%Sd(naint))
      allocate (specinterp%S(nfint, naint))
      allocate (Stemp(specin%nf, naint))

      ! fill f and ang arrays
      specinterp%nf = nfint
      specinterp%df = fmax / (nfint - 1) ! this usually gives a range of 0-1Hz,
      ! with 1/800Hz resolution
      do i = 1, nfint
         specinterp%f(i) = (i - 1) * specinterp%df
      end do
      specinterp%nang = naint
      specinterp%dang = 2d0 * par_pi / (naint - 1) ! this is exactly the same as in the JONSWAP construction
      do i = 1, specinterp%nang
         specinterp%ang(i) = (i - 1) * specinterp%dang
      end do

      ! If hm0 was set explicitly, then use that, else calculate hm0
      if (specin%hm0 > 0.d0) then
         hm0pre = specin%hm0
      else
         ! pre-interpolation hm0 value (can be on a non-monotonic f,ang grid)
         m0 = 0
         do j = 1, specin%nang
            do i = 1, specin%nf
               df = specin%f(max(2, i)) - specin%f(max(1, i - 1))
               dang = specin%ang(max(2, j)) - specin%ang(max(1, j - 1))
               m0 = m0 + specin%S(i, j) * df * dang
            end do
         end do
         hm0pre = 4d0 * sqrt(m0)
      end if

      ! interpolation (no extrapolation) of input 2D spectrum to standard 2D spectrum
      xcycle = 2.d0 * par_pi
      do i = 1, specin%nf
         call interp_in_cyclic_function(specin%ang, specin%S(i, :), specin%nang, xcycle, specinterp%ang, naint, Stemp(i, :))
      end do
      do j = 1, naint
         do i = 1, nfint
            if (specinterp%f(i) > specin%f(specin%nf) .or. specinterp%f(i) < specin%f(1)) then
               specinterp%S(i, j) = 0.d0
            else
               call linear_interp(specin%f, Stemp(:, j), specin%nf, specinterp%f(i), specinterp%S(i, j), dummy)
            end if
         end do
      end do

      deallocate (Stemp)
      ! hm0 post (is always on a monotonic f,ang grid)
      m0 = sum(specinterp%S) * specinterp%df * specinterp%dang
      hm0post = 4d0 * sqrt(m0)

      ! calculate 1D spectrum, summed over directions
      specinterp%Sf = sum(specinterp%S, DIM=2) * specinterp%dang

      ! correct the wave energy by setting Sfpost == Sfpre
      do i = 1, nfint
         if (specinterp%f(i) >= minval(specin%f) .and. specinterp%f(i) <= maxval(specin%f)) then
            call LINEAR_INTERP(specin%f, specin%Sf, specin%nf, specinterp%f(i), Sfnow, dummy)
            if (specinterp%Sf(i) > 0.d0 .and. Sfnow > 0.d0) then
               factor = Sfnow / specinterp%Sf(i)
               specinterp%Sf(i) = specinterp%Sf(i) * factor
               specinterp%S(i, :) = specinterp%S(i, :) * factor
            elseif (Sfnow == 0.d0) then
               specinterp%Sf(i) = 0.d0
               specinterp%S(i, :) = 0.d0
            else
               specinterp%Sf(i) = Sfnow
               dummy = maxval(maxloc(specin%S(i, :)))
               tempt0 = specin%ang(dummy)
               dummy = maxval(minloc(abs(specinterp%ang - tempt0)))
               specinterp%S(i, dummy) = Sfnow / specinterp%dang
            end if
         end if
      end do

      ! calculate other wave statistics from interpolated spectrum
      m0 = sum(specinterp%Sf) * specinterp%df
      specinterp%hm0 = 4d0 * sqrt(m0)
      specinterp%Sd = sum(specinterp%S, DIM=1) * specinterp%df
      i = maxval(maxloc(specinterp%Sd))
      specinterp%dir0 = 270.d0 - specinterp%ang(i) * 180d0 / par_pi ! converted back into nautical degrees
      i = maxval(maxloc(specinterp%Sf))
      specinterp%fp = specinterp%f(i)
      call tpDcalc(specinterp%Sf, specinterp%f, specinterp%trep, waveBoundaryParameters(ibnd)%trepfac, waveBoundaryParameters(ibnd)%Tm01switch)
      specinterp%dirm = 270.d0 - 180.d0 / par_pi * atan2(sum(sin(specinterp%ang) * specinterp%Sd) / sum(specinterp%Sd), &
                                                         sum(cos(specinterp%ang) * specinterp%Sd) / sum(specinterp%Sd))

      specinterp%dirm = mod(specinterp%dirm, 360.d0)

   end subroutine interpolate_spectrum

   ! --------------------------------------------------------------
   ! ----------- Small subroutine to determine if the  ------------
   ! ---------- global repeatwbc should be true or false ----------
   subroutine set_repeatwbc(ibnd)

      implicit none
      ! input
      integer, intent(in) :: ibnd
      ! internal
      integer :: i

      ! set default to repeat all boundary conditions
      waveSpectrumAdministration(ibnd)%repeatwbc = .true.
      ! find any spectrum that cannot be repeated, then change global
      ! repeatwbc to false
      do i = 1, nspectra
         if (.not. waveSpectrumAdministration(ibnd)%bcfiles(i)%repeat) then
            waveSpectrumAdministration(ibnd)%repeatwbc = .false.
         end if
      end do

   end subroutine set_repeatwbc

   ! --------------------------------------------------------------
   ! ----------- Small subroutine to set the filenames ------------
   ! ----------- of the boundary condition output files -----------
   subroutine set_bcfilenames(ibnd, wp)

      implicit none
      ! input/output
      integer, intent(in) :: ibnd
      type(waveparamsnew), intent(inout) :: wp
      ! internal
      integer :: i1, i2, i3, i4, i5

      if (waveSpectrumAdministration(ibnd)%repeatwbc) then
         write (wp%Efilename, "('E_reuse_bnd_', I0, '.bcf')") ibnd
         write (wp%qfilename, "('q_reuse_bnd_', I0, '.bcf')") ibnd
         write (wp%nhfilename, "('nh_reuse_bnd_', I0, '.bcf')") ibnd
      else
         i1 = floor(real(bccount) / 10000)
         i2 = floor(real(bccount - i1 * 10000) / 1000)
         i3 = floor(real(bccount - i1 * 10000 - i2 * 1000) / 100)
         i4 = floor(real(bccount - i1 * 10000 - i2 * 1000 - i3 * 100) / 10)
         i5 = bccount - i1 * 10000 - i2 * 1000 - i3 * 100 - i4 * 10
         wp%Efilename = 'E_series'//char(48 + i1)//char(48 + i2)//char(48 + i3)//char(48 + i4)//char(48 + i5)//'.bcf'
         wp%qfilename = 'q_series'//char(48 + i1)//char(48 + i2)//char(48 + i3)//char(48 + i4)//char(48 + i5)//'.bcf'
         wp%nhfilename = 'nh_series'//char(48 + i1)//char(48 + i2)//char(48 + i3)//char(48 + i4)//char(48 + i5)//'.bcf'
      end if

   end subroutine set_bcfilenames

   ! --------------------------------------------------------------
   ! ----------- Merge all separate spectra into one --------------
   ! -------------- average spectrum for other use ----------------
   subroutine generate_combined_spectrum(ibnd, specinterp, combspec)

      implicit none
      integer, intent(in) :: ibnd
      type(spectrum), dimension(nspectra), intent(in) :: specinterp
      type(spectrum), intent(inout) :: combspec
      integer :: iloc
      integer :: ifn
      real(dp), dimension(3) :: peakSd, peakang
      real(dp), dimension(:), allocatable :: tempSf

      double precision, parameter :: dtol = 1d-16

      allocate (combspec%f(nfint))
      allocate (combspec%Sf(nfint))
      allocate (combspec%Sd(naint))
      allocate (combspec%ang(naint))
      allocate (combspec%S(nfint, naint))
      combspec%f = specinterp(1)%f
      combspec%nf = nfint
      combspec%df = specinterp(1)%df
      combspec%ang = specinterp(1)%ang
      combspec%nang = naint
      combspec%dang = specinterp(1)%dang
      combspec%trep = 0.d0
      combspec%S = 0.d0
      combspec%Sf = 0.d0
      combspec%Sd = 0.d0
      combspec%hm0 = 0.d0

      do iloc = 1, nspectra
         do ifn = 1, nfint
            combspec%S(ifn, :) = combspec%S(ifn, :) + specinterp(iloc)%S(ifn, :) / nspectra
            combspec%Sf(ifn) = combspec%Sf(ifn) + specinterp(iloc)%Sf(ifn) / nspectra
         end do
      end do
      !
      ! Calculate peak wave angle
      !
      ! frequency integrated variance array
      combspec%Sd = sum(combspec%S, DIM=1) * combspec%df
      ! peak location of f-int array
      iloc = maxval(maxloc(combspec%Sd))
      ! pick two neighbouring directional bins, including effect of closing circle at
      ! 0 and 2pi rad
      if (iloc > 1 .and. iloc < naint) then
         peakSd = (/combspec%Sd(iloc - 1), combspec%Sd(iloc), combspec%Sd(iloc + 1)/)
         peakang = (/combspec%ang(iloc - 1), combspec%ang(iloc), combspec%ang(iloc + 1)/)
      elseif (iloc == 1) then
         peakSd = (/combspec%Sd(naint), combspec%Sd(1), combspec%Sd(2)/)
         peakang = (/combspec%ang(naint) - 2d0 * par_pi, combspec%ang(1), combspec%ang(2)/)
      elseif (iloc == naint) then
         peakSd = (/combspec%Sd(naint - 1), combspec%Sd(naint), combspec%Sd(1)/)
         peakang = (/combspec%ang(naint - 1), combspec%ang(naint), combspec%ang(1) + 2d0 * par_pi/)
      end if
      ! dir0 calculated as mean over peak and two neighbouring cells
      combspec%dir0 = sum(peakSd * peakang) / max(sum(peakSd), dtol)
      ! return to 0<=dir0<=2*par_pi
      if (combspec%dir0 > 2 * par_pi) then
         combspec%dir0 = combspec%dir0 - 2d0 * par_pi
      elseif (combspec%dir0 < 0) then
         combspec%dir0 = combspec%dir0 + 2d0 * par_pi
      end if
      !
      ! Compute peak wave frequency
      !
      allocate (tempSf(size(combspec%Sf)))
      tempSf = 0d0 * combspec%Sf
      ! find frequency range of 95% wave energy around peak
      where (combspec%Sf > 0.95d0 * maxval(combspec%Sf))
         tempSf = combspec%Sf
      end where
      ! smoothed peak is weighted mean frequency of 95% range
      combspec%fp = sum(tempSf * combspec%f) / max(sum(tempSf), dtol)
      !
      ! Compute representative wave period
      !
      call tpDcalc(combspec%Sf, combspec%f, combspec%trep, waveBoundaryParameters(ibnd)%trepfac, &
                   waveBoundaryParameters(ibnd)%Tm01switch)
      !
      ! Compute combined wave height
      do iloc = 1, nspectra
         combspec%hm0 = combspec%hm0 + specinterp(iloc)%hm0**2 / dble(nspectra)
      end do
      combspec%hm0 = sqrt(combspec%hm0)

   end subroutine generate_combined_spectrum

   subroutine generate_combined_spectrum_weighted(ibnd, npb, kL, kR, wL, wR, specinterp, combspec)

      implicit none
      integer, intent(in) :: ibnd
      integer, intent(in) :: npb
      integer, dimension(npb), intent(in) :: kL, kR
      real(dp), dimension(npb), intent(in) :: wL, wR
      type(spectrum), dimension(nspectra), intent(in) :: specinterp
      type(spectrum), intent(inout) :: combspec
      integer :: iloc
      real(dp), dimension(3) :: peakSd, peakang
      real(dp), dimension(:), allocatable :: tempSf

      double precision, parameter :: dtol = 1d-16

      allocate (combspec%f(nfint))
      allocate (combspec%Sf(nfint))
      allocate (combspec%Sd(naint))
      allocate (combspec%ang(naint))
      allocate (combspec%S(nfint, naint))
      combspec%f = specinterp(1)%f
      combspec%nf = nfint
      combspec%df = specinterp(1)%df
      combspec%ang = specinterp(1)%ang
      combspec%nang = naint
      combspec%dang = specinterp(1)%dang
      combspec%trep = 0.d0
      combspec%S = 0.d0
      combspec%Sf = 0.d0
      combspec%Sd = 0.d0
      combspec%hm0 = 0.d0

      do iloc = 1, npb
         combspec%S = combspec%S + wL(iloc) * specinterp(kL(iloc))%S + wR(iloc) * specinterp(kR(iloc))%S
         combspec%Sf = combspec%Sf + wL(iloc) * specinterp(kL(iloc))%Sf + wR(iloc) * specinterp(kR(iloc))%Sf
      end do
      combspec%S = combspec%S / dble(npb)
      combspec%Sf = combspec%Sf / dble(npb)
      !
      ! Calculate peak wave angle
      !
      ! frequency integrated variance array
      combspec%Sd = sum(combspec%S, DIM=1) * combspec%df
      ! peak location of f-int array
      iloc = maxval(maxloc(combspec%Sd))
      ! pick two neighbouring directional bins, including effect of closing circle at
      ! 0 and 2pi rad
      if (iloc > 1 .and. iloc < naint) then
         peakSd = (/combspec%Sd(iloc - 1), combspec%Sd(iloc), combspec%Sd(iloc + 1)/)
         peakang = (/combspec%ang(iloc - 1), combspec%ang(iloc), combspec%ang(iloc + 1)/)
      elseif (iloc == 1) then
         peakSd = (/combspec%Sd(naint), combspec%Sd(1), combspec%Sd(2)/)
         peakang = (/combspec%ang(naint) - 2d0 * par_pi, combspec%ang(1), combspec%ang(2)/)
      elseif (iloc == naint) then
         peakSd = (/combspec%Sd(naint - 1), combspec%Sd(naint), combspec%Sd(1)/)
         peakang = (/combspec%ang(naint - 1), combspec%ang(naint), combspec%ang(1) + 2d0 * par_pi/)
      end if
      ! dir0 calculated as mean over peak and two neighbouring cells
      combspec%dir0 = sum(peakSd * peakang) / max(sum(peakSd), dtol)
      ! return to 0<=dir0<=2*par_pi
      if (combspec%dir0 > 2 * par_pi) then
         combspec%dir0 = combspec%dir0 - 2d0 * par_pi
      elseif (combspec%dir0 < 0) then
         combspec%dir0 = combspec%dir0 + 2d0 * par_pi
      end if
      !
      ! Compute peak wave frequency
      !
      allocate (tempSf(size(combspec%Sf)))
      tempSf = 0d0 * combspec%Sf
      ! find frequency range of 95% wave energy around peak
      where (combspec%Sf > 0.95d0 * maxval(combspec%Sf))
         tempSf = combspec%Sf
      end where
      ! smoothed peak is weighted mean frequency of 95% range
      combspec%fp = sum(tempSf * combspec%f) / max(sum(tempSf), dtol)
      !
      ! Compute representative wave period
      !
      call tpDcalc(combspec%Sf, combspec%f, combspec%trep, waveBoundaryParameters(ibnd)%trepfac, &
                   waveBoundaryParameters(ibnd)%Tm01switch)
      !
      ! Compute combined wave height
      do iloc = 1, nspectra
         combspec%hm0 = combspec%hm0 + specinterp(iloc)%hm0**2 / dble(nspectra)
      end do
      combspec%hm0 = sqrt(combspec%hm0)

   end subroutine generate_combined_spectrum_weighted

   ! --------------------------------------------------------------
   ! ----------- Choose wave train components based on ------------
   ! --------------------- combined spectrum ----------------------
   subroutine generate_wavetrain_components(ibnd, combspec, wp)

      use m_xbeach_filefunctions
      use interp

      use wave_boundary_datastore
      use math_tools

      implicit none
      ! input/output
      integer, intent(in) :: ibnd
      type(spectrum), intent(in) :: combspec
      type(waveparamsnew), intent(inout) :: wp
      ! internal
      integer :: i, ii
      integer :: ind1, ind2, dummy
      real(dp), dimension(:), allocatable :: randnums, pdflocal, cdflocal
      real(dp) :: L0, L, kmax, fmax

      ! If we are running non-hydrostatic boundary conditions, we want to remove
      ! very high frequency components, as these will not be computed well anyway
      if (waveBoundaryParameters(ibnd)%nonhspectrum) then
         kmax = wdmax / hb0
         fmax = par_g * kmax * tanh(kmax * hb0) / 2 / par_pi
      else
         ! this is really already taken into account
         ! by specifying waveBoundaryParameters%sprdthr
         fmax = 2d0 * maxval(combspec%f)
      end if

      ! Determine frequencies around peak frequency of one-dimensional
      ! non-directional variance density spectrum, based on factor sprdthr, which
      ! should be included in the determination of the wave boundary conditions
      call frange(ibnd, combspec%f, combspec%Sf, fmax, ind1, ind2)

      ! Calculate number of wave components to be included in determination of the
      ! wave boundary conditions based on the wave record length and width of the
      ! wave frequency range
      wp%K = ceiling(wp%rtbc * (combspec%f(ind2) - combspec%f(ind1)) + 1)
      ! also include minimum number of components
      wp%K = max(wp%K, Kmin)

      ! Allocate space in waveparams for all wave train components
      allocate (wp%fgen(wp%K))
      allocate (wp%thetagen(wp%K))
      allocate (wp%phigen(wp%K))
      allocate (wp%kgen(wp%K))
      allocate (wp%wgen(wp%K))

      ! Select equidistant wave components between the earlier selected range of
      ! frequencies around the peak frequency in the frange subfunction
      wp%dfgen = (combspec%f(ind2) - combspec%f(ind1)) / (wp%K - 1)
      do i = 1, wp%K
         wp%fgen(i) = combspec%f(ind1) + (i - 1) * wp%dfgen
      end do

      ! This subroutine needs to generate random phase / directions for the individual
      ! wave trains. Due to some strange Fortran properties, it is better to select
      ! one long vector with random numbers for both the phase and the direction, than
      ! one vector for each.
      ! Update random seed, if requested
      allocate (randnums(2 * wp%K))
      ! Spin up random number generator, else all low seed numbers mean random sequence
      ! start at ~0. Don't know how much spin up is required (strictly no spin-up needed
      ! if random integer given as seed)

      if (waveBoundaryParameters(ibnd)%randomseed /= -999) then
         randnums(1) = random(waveBoundaryParameters(ibnd)%randomseed)
      else
         randnums(1) = random(100)
      end if
      !
      do i = 2, 2 * wp%K
         randnums(i) = random(0)
      end do

      ! Determine a random phase for each wave train component between 0 and 2pi
      wp%phigen = randnums(1:wp%K) * 2 * par_pi

      ! Determine random directions for each wave train component, based on the CDF of
      ! the directional spectrum. For each wave train we will interpolate the directional
      ! distribution at that frequency, generate a CDF, and then interpolate the wave
      ! train direction from a random number draw and the CDF.
      allocate (pdflocal(naint))
      allocate (cdflocal(naint))
      do i = 1, wp%K
         ! interpolate spectrum at this frequency per angle in the spectrum
         do ii = 1, naint
            call LINEAR_INTERP(combspec%f, combspec%S(:, ii), combspec%nf, wp%fgen(i), pdflocal(ii), dummy)
         end do
         ! convert to pdf by ensuring total integral == 1, assuming constant directional bin size
         pdflocal = pdflocal / sum(pdflocal)
         ! convert to cdf by trapezoidal integration, which is not biased for integration
         ! direction.
         ! Boundary condition, which may be nonzero:
         cdflocal(1) = pdflocal(1)
         do ii = 2, naint
            cdflocal(ii) = cdflocal(ii - 1) + (pdflocal(ii) + pdflocal(ii - 1)) / 2d0
            ! Note: this only works if the directional
            ! bins are constant in size. Assumed multiplication
            ! by one.
         end do
         ! interpolate random number draw across the cdf. Note that cdf(1) is not assumed to be zero and
         ! there is a posibility of drawing less than cdf(1). Therefore, if the random number is .lt. cdf(1),
         ! interpolation should take place across the back of the angle spectrum
         ! (i.e., from theta(end)-2pi : theta(1)).
         ! Note that we are using the second half of randnums now (K+1:end)
         if (randnums(wp%K + i) >= cdflocal(1)) then
            call LINEAR_INTERP(cdflocal, combspec%ang, naint, randnums(wp%K + i), wp%thetagen(i), dummy)
         else
            call LINEAR_INTERP((/0.d0, cdflocal(1)/), (/combspec%ang(naint) - 2 * par_pi, combspec%ang(1)/), &
                               2, randnums(wp%K + i), wp%thetagen(i), dummy)
         end if
         ! ensure wave direction 0<=theta<2pi
         wp%thetagen(i) = mod(wp%thetagen(i), 2 * par_pi)
      end do

      ! Angular frequency
      wp%wgen = 2 * par_pi * wp%fgen

      ! determine wave number for each wave train component, using standard dispersion relation
      ! solver from wave_functions module. This function returns a negative wave length if the
      ! solver did not converge.
      do i = 1, wp%K
         L0 = par_g * (1 / wp%fgen(i))**2 / 2d0 / par_pi ! deep water wave length
         L = iteratedispersion(L0, L0, par_pi, hb0)
         if (L < 0.d0) then
            call writelog('lsw', '', 'No dispersion convergence found for wave train ', i, &
                          ' in boundary condition generation')
            L = -L
         end if
         wp%kgen(i) = 2 * par_pi / L
      end do

      ! Free memory
      deallocate (pdflocal, cdflocal, randnums)

   end subroutine generate_wavetrain_components

   elemental function iteratedispersion(L0, Lestimate, px, h) result(L)

      implicit none
      ! input
      real(dp), intent(in) :: L0
      real(dp), intent(in) :: Lestimate
      real(dp), intent(in) :: px
      real(dp), intent(in) :: h
      ! output
      real(dp) :: L
      ! internal
      real(dp) :: L1, L2
      integer :: iter
      real(dp) :: err
      real(dp), parameter :: aphi = 1.d0 / (((1.0d0 + sqrt(5.0d0)) / 2) + 1)
      real(dp), parameter :: bphi = ((1.0d0 + sqrt(5.0d0)) / 2) / (((1.0d0 + sqrt(5.0d0)) / 2) + 1)
      integer, parameter :: itermax = 150
      real(dp), parameter :: errmax = 0.00001d0

      err = huge(0.0d0)
      iter = 0
      L1 = Lestimate
      do while (err > errmax .and. iter < itermax)
         iter = iter + 1
         L2 = L0 * tanh(2 * px * h / L1)
         L1 = (L1 * aphi + L2 * bphi) ! Golden ratio
         err = abs(L2 - L1)
      end do

      if (iter <= itermax) then
         L = L1
      else
         ! signal this went wrong
         L = -L1
      end if

   end function iteratedispersion

   ! -----------------------------------------------------------
   ! ----------- Small subroutine to determine -----------------
   ! ------------ representative wave period -------------------
   subroutine tpDcalc(Sf, f, Trep, trepfac, switch)

      implicit none

      real(dp), dimension(:), intent(in) :: Sf, f
      real(dp), intent(out) :: Trep
      real(dp), intent(in) :: trepfac
      integer, intent(in) :: switch

      real(dp), dimension(:), allocatable :: temp

      double precision, parameter :: dtol = 1d-16

      allocate (temp(size(Sf)))
      temp = 0.d0
      where (Sf >= trepfac * maxval(Sf))
         temp = 1.d0
      end where

      if (switch == 1) then
         Trep = sum(temp * Sf) / max(sum(temp * Sf * f), dtol) ! Tm01
      else
         Trep = sum(temp * Sf / max(f, 0.001d0)) / max(sum(temp * Sf), dtol) ! Tm-1,0
      end if

      deallocate (temp)

   end subroutine tpDcalc

   ! -----------------------------------------------------------
   ! ---- Small subroutine to determine f-range round peak -----
   ! -----------------------------------------------------------
   subroutine frange(ibnd, f, Sf, fmax, firstp, lastp, findlineout)

      use wave_boundary_datastore

      implicit none

      integer, intent(in) :: ibnd
      real(dp), dimension(:), intent(in) :: f, Sf
      real(dp), intent(in) :: fmax

      integer, intent(out) :: firstp, lastp

      real(dp), dimension(:), intent(out), allocatable, optional :: findlineout
      real(dp), dimension(:), allocatable :: temp, findline
      integer :: i = 0

      ! find frequency range around peak
      allocate (findline(size(Sf)))
      findline = 0 * Sf
      where (Sf > waveBoundaryParameters(ibnd)%sprdthr * maxval(Sf))
         findline = 1
      end where

      ! find frequency range below fmax
      where (f > fmax)
         findline = 0
      end where

      firstp = maxval(maxloc(findline)) ! Picks the first "1" in temp

      allocate (temp(size(findline)))
      temp = (/(i, i=1, size(findline))/)
      lastp = maxval(maxloc(temp * findline)) ! Picks the last "1" in temp

      if (present(findlineout)) then
         allocate (findlineout(size(Sf)))
         findlineout = findline
      end if
      deallocate (temp, findline)

   end subroutine frange

   ! --------------------------------------------------------------
   ! ---------------- Setup time axis for waves -------------------
   ! --------------------------------------------------------------
   subroutine generate_wave_time_axis(ibnd, wp)

      use wave_boundary_datastore

      implicit none
      ! input/output
      integer, intent(in) :: ibnd
      type(waveparamsnew), intent(inout) :: wp
      ! internal
      integer :: ntaper, indend
      integer :: i

      ! First assume that internal and bc-writing time step is the same
      wp%dtin = wp%dtbc
      wp%dtchanged = .false.
      wp%tslenbc = nint(wp%rtbc / wp%dtbc) + 1

      ! Check whether the internal frequency is high enough to describe the highest frequency
      ! wave train returned from frange (which can be used in the boundary conditions)
      if (.not. waveBoundaryParameters(ibnd)%nonhspectrum) then
         if (wp%dtin > 0.5d0 / wp%fgen(wp%K)) then
            wp%dtin = 0.5d0 / wp%fgen(wp%K)
            wp%dtchanged = .true.
         end if
      else
         if (wp%dtin > 0.1d0 / wp%fgen(wp%K)) then
            wp%dtin = 0.1d0 / wp%fgen(wp%K)
            wp%dtchanged = .true.
         end if
      end if

      ! The length of the internal time axis should be even (for Fourier transform) and
      ! depends on the internal time step needed and the internal duration (~1/dfgen):
      wp%tslen = ceiling(1 / wp%dfgen / wp%dtin) + 1
      if (mod(wp%tslen, 2) /= 0) then
         wp%tslen = wp%tslen + 1
      end if

      ! Now we can make the internal time axis
      wp%rtin = wp%tslen * wp%dtin
      allocate (wp%tin(wp%tslen))
      do i = 1, wp%tslen
         wp%tin(i) = (i - 1) * wp%dtin
      end do

      ! Make a taper function to slowly increase and decrease the boundary condition forcing
      ! at the start and the end of the boundary condition file (including any time beyond
      ! the external rtbc
      allocate (wp%taperf(wp%tslen))
      allocate (wp%taperw(wp%tslen))
      ! fill majority with unity
      wp%taperf = 1.d0
      wp%taperw = 1.d0
      if (waveBoundaryParameters(ibnd)%nonhspectrum) then
         ! begin taper by building up the wave conditions over 2 wave periods
         ntaper = nint((2.0d0 * waveSpectrumAdministration(ibnd)%Tbc) / wp%dtin)
      else
         ntaper = nint((5.d0 * waveSpectrumAdministration(ibnd)%Tbc) / wp%dtin)
      end if
      do i = 1, min(ntaper, size(wp%taperf))
         wp%taperf(i) = tanh(5.d0 * i / ntaper) ! multiplied by five because tanh(5)=~1
         wp%taperw(i) = tanh(5.d0 * i / ntaper)
      end do
      ! We do not want to taperw the end anymore. Instead we pass the wave height at the end of rtbc to
      ! the next wave generation iteration.
      ! end taper by finding where tin=rtbc, taper before that and set everything to zero after
      ! that.
      if (wp%tin(wp%tslen) > wp%rtbc) then
         indend = minval(minloc(wp%tin, MASK=wp%tin >= wp%rtbc))
      else
         indend = wp%tslen
      end if
      do i = 1, min(ntaper, indend)
         wp%taperf(indend + 1 - i) = min(wp%taperf(indend + 1 - i), tanh(5.d0 * i / ntaper))
      end do
      wp%taperf(indend:wp%tslen) = 0.d0
      ind_end_taper = indend
   end subroutine generate_wave_time_axis

   ! --------------------------------------------------------------
   ! -------- Calculate variance at each spectrum location --------
   ! --------------------------------------------------------------
   subroutine generate_wave_train_variance(wp, specinterp)

      use interp

      implicit none
      ! input/output
      type(waveparamsnew), intent(inout) :: wp
      type(spectrum), dimension(nspectra), intent(in) :: specinterp
      ! internal
      integer :: i, ii, dummy
      real(dp) :: hm0post

      ! allocate space for the variance arrays
      allocate (wp%vargen(nspectra))
      allocate (wp%vargenq(nspectra))

      ! Determine variance at each spectrum location
      do i = 1, nspectra
         allocate (wp%vargen(i)%Sf(wp%K))
         allocate (wp%vargenq(i)%Sf(wp%K))
         do ii = 1, wp%K
            ! In order to maintain energy density per frequency, an interpolation
            ! is carried out on the input directional variance density spectrum
            ! at the frequency and direction locations in fgen.
            call linear_interp(specinterp(i)%f, &
                               specinterp(i)%Sf, &
                               nfint, &
                               wp%fgen(ii), &
                               wp%vargen(i)%Sf(ii), dummy)
         end do
         ! Correct variance to ensure the total variance remains the same as the total variance in the
         ! (interpolated) input spectrum
         hm0post = 4 * sqrt(sum(wp%vargen(i)%Sf) * wp%dfgen)
         wp%vargen(i)%Sf = (specinterp(i)%hm0 / hm0post)**2 * wp%vargen(i)%Sf
         ! For the generation of long waves we cannot use wp%vargen%Sf, because it contains an overestimation
         ! of energy in the peak frequencies. We can also not use the standard directionally-integrated spectrum
         ! because this stores all energy at fgen(K), where it is possible that for this current spectrum, there
         ! is no energy at S(f(K),theta(K0))
         ! The current solution is to take the minimum of both methods
         do ii = 1, wp%K
            ! Map Sf input to fgen
            call LINEAR_INTERP(specinterp(i)%f, specinterp(i)%Sf, specinterp(i)%nf, wp%fgen(ii), wp%vargenq(i)%Sf(ii), dummy)
         end do
         wp%vargenq(i)%Sf = min(wp%vargen(i)%Sf, wp%vargenq(i)%Sf)
      end do

   end subroutine generate_wave_train_variance

   ! --------------------------------------------------------------
   ! ------ Calculate amplitudes at each spectrum location --------
   ! ------------ for every wave train component ------------------
   subroutine generate_wave_train_properties_per_offshore_point(ibnd, wp)
      use interp
      use wave_boundary_datastore, only: waveSpectrumAdministration

      implicit none
      ! input/output
      integer, intent(in) :: ibnd
      type(waveparamsnew), intent(inout) :: wp
      ! internal
      integer :: i, ii
      integer :: kkL, kkR
      real(dp) :: sfnow, sfimp
      real(dp) :: wwL, wwR

      ! allocate space for the amplitude array and representative integration angle
      allocate (wp%A(npb, wp%K))
      allocate (wp%Sfinterp(npb, wp%K))
      allocate (wp%Sfinterpq(npb, wp%K))
      allocate (wp%Hm0interp(npb))

      ! where necessary, interpolate Sf of each spectrum location
      ! to the current grid cell, and use this to calculate A
      do i = 1, npb
         kkL = waveSpectrumAdministration(ibnd)%kL(i)
         kkR = waveSpectrumAdministration(ibnd)%kR(i)
         wwL = waveSpectrumAdministration(ibnd)%wL(i)
         wwR = waveSpectrumAdministration(ibnd)%wR(i)

         do ii = 1, wp%K
            wp%Sfinterp(i, ii) = wwR * wp%vargen(kkR)%Sf(ii) + wwL * wp%vargen(kkL)%Sf(ii)
            wp%Sfinterpq(i, ii) = wwR * wp%vargenq(kkR)%Sf(ii) + wwL * wp%vargenq(kkL)%Sf(ii)
         end do
       !! Ensure that the total amount of variance has not been reduced/increased
       !! during interpolation for short wave energy only. This is not done for
       !! Sf for bound long wave generation
         sfnow = sum(wp%Sfinterp(i, :)) ! integration over fixed bin df size unnecessary
         sfimp = wwR * sum(wp%vargen(kkR)%Sf) + wwL * sum(wp%vargen(kkL)%Sf)
         wp%Sfinterp(i, :) = wp%Sfinterp(i, :) * sfimp / sfnow

         wp%A(i, :) = sqrt(2 * wp%Sfinterp(i, :) * wp%dfgen)
         wp%Hm0interp(i) = 4 * sqrt(sum(wp%Sfinterp(i, :)) * wp%dfgen)

         ! determine if these components will be phase-resolved, or resolved in the wave energy balance
         allocate (wp%PRindex(wp%K))
         if (waveBoundaryParameters(ibnd)%nonhspectrum) then
            ! all components phase-resolved
            wp%PRindex = 1
         else
            if (waveBoundaryParameters(ibnd)%swkhmin > 0.d0) then
               ! some components resolved, some not
               where (wp%kgen * hb0 >= waveBoundaryParameters(ibnd)%swkhmin)
                  wp%PRindex = 0
               elsewhere
                  wp%PRindex = 1
               end where
            else
               ! no components phase-resolved
               wp%PRindex = 0
            end if
         end if

      end do

   end subroutine generate_wave_train_properties_per_offshore_point

   ! --------------------------------------------------------------
   ! --------- Calculate Fourier componets for each wave ----------
   ! ---------- train component on the offshore boundary ----------
   subroutine generate_wave_train_Fourier(ibnd, wp)

      use interp
      use m_xbeach_filefunctions

      use math_tools
      use wave_boundary_datastore

      implicit none
      ! input/output
      integer, intent(in) :: ibnd
      type(waveparamsnew), intent(inout) :: wp
      ! internal
      integer :: i, ii, tempi
      complex(fftkind), dimension(:), allocatable :: tempcmplx

      ! Determine indices of wave train components in frequency axis and
      ! Fourier transform result
      allocate (wp%Findex(wp%K))
      tempi = floor(wp%fgen(1) / wp%dfgen)
      do i = 1, wp%K
         wp%Findex(i) = tempi + i
      end do

      ! Allocate Fourier coefficients for each y-position at the offshore boundary and
      ! each time step
      allocate (wp%CompFn(npb, wp%tslen))
      wp%CompFn = 0.d0
      allocate (tempcmplx(wp%tslen / 2 - 1))

      call writelog('ls', '', 'Calculating Fourier components')
      call progress_indicator(.true., 0.d0, 5.d0, 2.d0)

      do i = 1, wp%K

         call progress_indicator(.false., dble(i) / wp%K * 100, 5.d0, 2.d0)

         do ii = 1, npb
            ! Determine first half of complex Fourier coefficients of wave train
            ! components using random phase and amplitudes from sampled spectrum
            ! until Nyquist frequency. The amplitudes are represented in a
            ! two-sided spectrum, which results in the factor 1/2.
            ! Unroll Fourier components along offshore boundary, assuming all wave trains
            ! start at x(1,1), y(1,1).
            wp%CompFn(ii, wp%Findex(i)) = wp%A(ii, i) / 2 * exp(par_compi * wp%phigen(i)) * & ! Bas: wp%Findex used in time dimension because t=j*dt in frequency space
                                          exp(-par_compi * wp%kgen(i) * &
                                              (sin(wp%thetagen(i)) * (waveBoundaryParameters(ibnd)%yb(ii) - waveBoundaryParameters(ibnd)%y0) &
                                               + cos(wp%thetagen(i)) * (waveBoundaryParameters(ibnd)%xb(ii) - waveBoundaryParameters(ibnd)%x0)))

            ! Determine Fourier coefficients beyond Nyquist frequency (equal to
            ! coefficients at negative frequency axis) of relevant wave components for
            ! first y-coordinate by mirroring
            tempcmplx = conjg(wp%CompFn(ii, 2:wp%tslen / 2))
            call flipiv(tempcmplx, size(tempcmplx))
            wp%CompFn(ii, wp%tslen / 2 + 2:wp%tslen) = tempcmplx
         end do
      end do

      ! Free memory
      deallocate (tempcmplx)

   end subroutine generate_wave_train_Fourier

   ! --------------------------------------------------------------
   ! --------- Calculate in which computational wave bin ----------
   ! ------------- each wave train component belongs --------------
   subroutine distribute_wave_train_directions(ibnd, wp, nspr)

      use m_xbeach_filefunctions
      use m_sferic
      use wave_boundary_datastore

      implicit none

      ! input/output
      type(waveparamsnew), intent(inout) :: wp
      integer, intent(in) :: ibnd, nspr
      ! internal
      integer :: i, itheta
      real(dp), dimension(:, :), allocatable :: binedges
      real(dp) :: lostvar, keptvar, perclost
      integer :: lntheta ! local copies that can be changed without
      real(dp) :: ldtheta ! damage to the rest of the model

      ! Calculate the bin edges of all the computational wave bins in the
      ! XBeach model (not the input spectrum)
      lntheta = waveBoundaryParameters(ibnd)%ntheta
      ldtheta = waveBoundaryParameters(ibnd)%dtheta
      allocate (binedges(lntheta, 2))
      do itheta = 1, lntheta
         binedges(itheta, 1) = mod((waveBoundaryParameters(ibnd)%theta(itheta) - ldtheta / 2) + twopi, twopi)
         binedges(itheta, 2) = mod((waveBoundaryParameters(ibnd)%theta(itheta) + ldtheta / 2) + twopi, twopi)
      end do

      ! All generated wave components are in the rang 0<=theta<2pi.
      ! We link wave components to a wave direction bin if the direction falls
      ! within the bin boundaries. Note the >= and <=, ranther than >= and <. This
      ! is not necessarily a problem, but solves having to make an exception for the
      ! highest wave direction bin, in which >= and <= should be applicable.
      ! In the case of a central bin and a wave direction exactly (!) on the bin
      ! interface, the wave will be linked to the first wave bin in the ordering,
      ! rather than the higher of the two bins.
      !
      ! Initially set WDindex to zero. This marks a wave direction outside the
      ! computational wave bins. In case it does not fit in a directional wave
      ! bin, it remains zero at the end of the loops.
      ! Note: this does not ensure all energy is included in the wave bins,
      ! as wave energy may still fall outside the computational domain.
      allocate (wp%WDindex(wp%K))
      wp%WDindex = 0
      do i = 1, wp%K
         do itheta = 1, lntheta
            ! special case if this bin spans 0 degrees
            if (binedges(itheta, 1) > binedges(itheta, 2)) then
               ! Need to check both above and below zero degrees
               if ((wp%thetagen(i) >= binedges(itheta, 1) .and. wp%thetagen(i) <= 2 * par_pi) .or. &
                   (wp%thetagen(i) >= 0.d0 .and. wp%thetagen(i) <= binedges(itheta, 2))) then
                  wp%WDindex(i) = itheta
                  ! We now have the correct wave bin, move to next wave component K
                  exit
               end if
            else
               if (wp%thetagen(i) >= binedges(itheta, 1) .and. wp%thetagen(i) <= binedges(itheta, 2)) then
                  wp%WDindex(i) = itheta
                  ! We now have the correct wave bin, move to next wave component K
                  exit
               end if
            end if
         end do
      end do

      ! If the user has set nspr == 1 then the randomly drawn wave directions
      ! should be set to the centres of the wave directional bins.
      ! Also move all wave energy falling outside the computational bins, into
      ! the computational domain (in the outer wave direction bins)
      if (nspr == 1) then
         do i = 1, wp%K
            if (wp%WDindex(i) > 0) then
               ! reset the direction of this wave train to the centre of the bin
               wp%thetagen(i) = waveBoundaryParameters(ibnd)%theta(wp%WDindex(i))
            end if
         end do
      end if

      ! Check the amount of energy lost to wave trains falling outside the computational
      ! domain
      lostvar = 0.d0
      keptvar = 0.d0
      do i = 1, wp%K
         if (wp%WDindex(i) == 0) then
            lostvar = lostvar + sum(wp%A(:, i)**2)
         else
            keptvar = keptvar + sum(wp%A(:, i)**2)
         end if
      end do
      perclost = 100 * (lostvar / (lostvar + keptvar))

      if (perclost > 5.0d0) then
         call writelog('lsw', '(a,f0.1,a)', 'Large amounts of energy (', perclost, &
                       '%) fall outside computational domain at the offshore boundary')
         call writelog('lsw', '', 'Check specification of input wave angles and wave directional grid')
      else
         call writelog('ls', '(a,f0.1,a)', 'Wave energy outside computational domain at offshore boundary: ', perclost, '%')
      end if

      ! Free memory
      deallocate (binedges)

   end subroutine distribute_wave_train_directions

   ! --------------------------------------------------------------
   ! --------- Calculate energy envelope time series from ---------
   ! -------- Fourier components, and write to output file --------
   subroutine generate_ebcf(ibnd, wp)
      use interp
      use m_xbeach_filefunctions
      use math_tools

      implicit none

      ! input/output
      integer, intent(in) :: ibnd
      type(waveparamsnew), intent(inout) :: wp
      ! internal
      integer :: itheta, iy, iwc, it
      integer :: index, status
      integer :: nInBoundaryFile
      integer, dimension(:), allocatable :: nwc
      real(dp), dimension(:, :, :), allocatable :: zeta, Ampzeta, E_tdir
      real(dp), dimension(:, :), allocatable :: eta, Amp
      complex(fftkind), dimension(:), allocatable :: Gn, tempcmplx, tempcmplxhalf
      integer, dimension(:), allocatable :: tempindex, tempinclude
      real(dp) :: stdzeta, stdeta, etot, perc, emean
      real(dp), dimension(:), allocatable :: E_t

      ! Allocate variables for water level exitation and amplitude with and without
      ! directional spreading dependent envelope
      allocate (zeta(npb, wp%tslen, ntheta))
      allocate (Ampzeta(npb, wp%tslen, ntheta))
      zeta = 0.d0
      Ampzeta = 0.d0

      allocate (eta(npb, wp%tslen))
      allocate (Amp(npb, wp%tslen))
      eta = 0.d0
      Amp = 0.d0

      ! Calculate wave energy for each y-coordinate along seaside boundary for
      ! current computational directional bin
      allocate (nwc(ntheta))
      allocate (Gn(wp%tslen))
      allocate (tempinclude(wp%K))
      allocate (tempcmplx(wp%tslen))
      allocate (tempcmplxhalf(size(Gn(wp%tslen / 2 + 2:wp%tslen))))

      do itheta = 1, ntheta

         call writelog('ls', '(A,I0,A,I0)', 'Calculating short wave time series for theta bin ', itheta, ' of ', ntheta)

         ! Select wave components that are in the current computational
         ! directional bin
         tempinclude = 0
         where (wp%WDindex == itheta .and. wp%PRindex == 0) ! only include components in this bin that are not to be phase-resolved
            tempinclude = 1
         end where
         !
         ! Determine number of wave components that are in the current
         ! computational directional bin
         nwc(itheta) = sum(tempinclude)

         ! Determine for each wave component in the current computational
         ! directional bin its index in the Fourier coefficients array
         ! ordered from high to low frequency
         allocate (tempindex(nwc(itheta)))
         tempindex = 0
         do iwc = 1, nwc(itheta)
            ! find highest
            index = maxval(maxloc(tempinclude))
            ! reset that one so that the next highest in found in next iteration
            tempinclude(index) = 0
            tempindex(iwc) = wp%Findex(index)
         end do

         ! Check whether any wave components are in the current computational
         ! directional bin
         if (nwc(itheta) > 0) then
            do iy = 1, npb
               ! Reset
               Gn = 0

               ! Determine Fourier coefficients of all wave components for current
               ! y-coordinate in the current computational directional bin
               Gn(tempindex) = wp%CompFn(iy, tempindex)
               tempcmplxhalf = conjg(Gn(2:wp%tslen / 2))
               call flipiv(tempcmplxhalf, size(tempcmplxhalf))
               Gn(wp%tslen / 2 + 2:wp%tslen) = tempcmplxhalf

               ! Inverse Discrete Fourier transformation to transform back to time
               ! domain from frequency domain
               tempcmplx = Gn
               status = 0
               tempcmplx = fft(tempcmplx, inv=.true., stat=status)

               ! Scale result
               tempcmplx = tempcmplx / sqrt(dble(size(tempcmplx)))

               ! Superimpose gradual increase and decrease of energy input for
               ! current y-coordinate and computational diretional bin on
               ! instantaneous water level excitation
               !
               ! Robert: use final wave elevation from last iteration to startup
               ! this boundary condition
               zeta(iy, :, itheta) = dble(tempcmplx * wp%tslen)
               ! The first time this function is called in a simulation, lastwaveelevation is unknown,
               ! so would be set to zero. However, this artificially increases the taper time, and is
               ! not useful if repeatwbc = .true., as this zero level is repeated every boundary condition
               ! Instead, we select zeta at the end of the first boundary condition time series to
               ! initialise lastwaveelevation
               if (bccount == 0) then
                  waveSpectrumAdministration(ibnd)%lastwaveelevation(iy, itheta) = zeta(iy, ind_end_taper, itheta)
               end if
               zeta(iy, :, itheta) = zeta(iy, :, itheta) * wp%taperw + (1 - wp%taperw) * &
                                     waveSpectrumAdministration(ibnd)%lastwaveelevation(iy, itheta)
               ! note that taperw is only zero at the start, not the end of the time series, so we
               ! can safely copy zeta at the point in time where t=rtbc to lastwaveelevation for use
               ! in the generation of the next time series
               waveSpectrumAdministration(ibnd)%lastwaveelevation(iy, itheta) = zeta(iy, ind_end_taper, itheta)
            end do ! iy=1,npb
         end if ! nwc>0
         deallocate (tempindex)
      end do ! itheta = 1,ntheta
      deallocate (tempinclude)
      deallocate (Gn)
      deallocate (tempcmplxhalf)
      !
      ! Calculate energy envelope amplitude
      call writelog('ls', '(A,I0)', 'Calculating wave energy envelope at boundary ', ibnd)
      call progress_indicator(.true., 0.d0, 5.d0, 2.d0)

      do iy = 1, npb
         ! Integrate instantaneous water level excitation of wave
         ! components over directions
         eta(iy, :) = sum(zeta(iy, :, :), 2)
         tempcmplx = eta(iy, :)

         ! Hilbert transformation to determine envelope of all total
         ! non-directional wave components
         call hilbert(tempcmplx, size(tempcmplx))

         ! Determine amplitude of water level envelope by calculating
         ! the absolute value of the complex wave envelope descriptions
         Amp(iy, :) = abs(tempcmplx)

         ! Calculate standard deviation of non-directional
         ! instantaneous water level excitation of all
         ! wave components to be used as weighing factor
         stdeta = sqrt(sum(eta(iy, :)**2) / (size(eta(iy, :)) - 1))
         do itheta = 1, ntheta
            if (nwc(itheta) > 0) then
               ! Calculate standard deviations of directional
               ! instantaneous water level excitation of all
               ! wave components to be used as weighing factor
               stdzeta = sqrt(sum(zeta(iy, :, itheta)**2) / (size(zeta(iy, :, itheta)) - 1))

               ! Calculate amplitude of directional wave envelope
               Ampzeta(iy, :, itheta) = Amp(iy, :) * stdzeta / stdeta
            else !  nwc==0
               ! Current computational directional bin does not contain any wave
               ! components
               Ampzeta(iy, :, itheta) = 0.d0
            end if ! nwc>0
         end do ! 1:ntheta
         ! Print status message to screen

         call progress_indicator(.false., dble(iy) / (npb) * 100, 5.d0, 2.d0)

      end do ! 1:npb
      !

      ! free memory
      deallocate (tempcmplx)
      deallocate (nwc)

      ! Allocate memory for energy time series
      allocate (E_tdir(npb, wp%tslen, ntheta))
      E_tdir = 0.0d0
      E_tdir = 0.5d0 * waveBoundaryParameters(ibnd)%rho * par_g * Ampzeta**2
      E_tdir = E_tdir / waveBoundaryParameters(ibnd)%dtheta
      !
      ! Ensure we scale back to the correct Hm0
      if (waveBoundaryParameters(ibnd)%wbcScaleEnergy == 1) then
         nInBoundaryFile = nint(wp%rtbc / wp%dtin) ! number of total time series in bcf (independent of dtin / dtbc differences)
         do iy = 1, npb
            stdeta = sum(E_tdir(iy, 1:nInBoundaryFile, :)) * waveBoundaryParameters(ibnd)%dtheta ! sum energy
            stdeta = stdeta / nInBoundaryFile ! mean energy

            stdzeta = (wp%Hm0interp(iy) / sqrt(2.d0))**2 * (waveBoundaryParameters(ibnd)%rho * par_g / 8d0)

            E_tdir(iy, :, :) = E_tdir(iy, :, :) * stdzeta / stdeta
         end do
      end if
      !
      if (waveBoundaryParameters(ibnd)%wbcEvarreduce < 1.d0 - 1d-10) then
         do itheta = 1, ntheta
            do iy = 1, npb
               emean = sum(E_tdir(iy, :, itheta)) / wp%tslen
               E_tdir(iy, :, itheta) = waveBoundaryParameters(ibnd)%wbcEvarreduce * (E_tdir(iy, :, itheta) - emean) + emean
            end do
         end do
      end if

      ! Print directional energy distribution to screen
      etot = sum(E_tdir)
      do itheta = 1, ntheta
         perc = sum(E_tdir(:, :, itheta)) / etot * 100

         call writelog('ls', '(a,i0,a,f0.2,a)', 'Wave bin ', itheta, ' contains ', perc, '% of total energy')

      end do
      !
      !
      ! Store time series in internal memory
      !
      ! First deallocate arrays if necessary
      if (allocated(waveBoundaryTimeSeries(ibnd)%eebct)) deallocate (waveBoundaryTimeSeries(ibnd)%eebct)
      if (allocated(waveBoundaryTimeSeries(ibnd)%tbc)) deallocate (waveBoundaryTimeSeries(ibnd)%tbc)
      !
      ! Allocate in the correct size
      allocate (waveBoundaryTimeSeries(ibnd)%eebct(npb, wp%tslenbc + 2, ntheta))
      allocate (waveBoundaryTimeSeries(ibnd)%tbc(wp%tslenbc + 2))
      !
      ! Store time vector
      do it = 2, wp%tslenbc + 1
         waveBoundaryTimeSeries(ibnd)%tbc(it) = (it - 2) * wp%dtbc + waveBoundaryAdministration(ibnd)%startCurrentSeries
      end do
      ! add 'inifinite ends to the time series in case of mismatch at the end or start of the time
      ! series generation and interpolation at each time step
      waveBoundaryTimeSeries(ibnd)%tbc(1) = -1.d0 * huge(0.d0)
      waveBoundaryTimeSeries(ibnd)%tbc(wp%tslenbc + 2) = 1.d0 * huge(0.d0)
      if (.not. allocated(E_t)) allocate (E_t(wp%tslen))
      if (wp%dtchanged) then
         ! Interpolate from internal time axis to output time axis
         do itheta = 1, ntheta
            do it = 2, wp%tslenbc + 1
               do iy = 1, npb
                  E_t = E_tdir(iy, :, itheta)
                  call linear_interp(wp%tin, E_t, wp%tslen, &
                                     (it - 1) * wp%dtbc, waveBoundaryTimeSeries(ibnd)%eebct(iy, it, itheta), status)
               end do
            end do
         end do
      else
         ! no need for interpolation
         waveBoundaryTimeSeries(ibnd)%eebct(:, 2:wp%tslenbc + 1, :) = E_tdir
      end if
      !
      ! add 'inifinite ends to the time series in case of mismatch at the end or start of the time
      ! series generation and interpolation at each time step
      waveBoundaryTimeSeries(ibnd)%eebct(:, 1, :) = waveBoundaryTimeSeries(ibnd)%eebct(:, 2, :)
      waveBoundaryTimeSeries(ibnd)%eebct(:, wp%tslenbc + 2, :) = waveBoundaryTimeSeries(ibnd)%eebct(:, wp%tslenbc + 1, :)
      !
      !
      ! Free memory
      deallocate (zeta, Ampzeta, E_tdir, Amp, eta)

   end subroutine generate_ebcf

   ! --------------------------------------------------------------
   ! ------ Calculate time series of short wave at offshore -------
   ! --------------------------- boundary -------------------------
   subroutine generate_swts(ibnd, wp)

      use m_xbeach_filefunctions

      use wave_boundary_datastore

      implicit none

      ! input/output
      integer, intent(in) :: ibnd
      type(waveparamsnew), intent(inout) :: wp
      ! internal
      integer :: j, it, ik
      real(dp) :: U
      real(dp), dimension(npb) :: distx, disty

      ! allocate memory for time series of data
      allocate (wp%zsits(npb, wp%tslen))
      allocate (wp%uits(npb, wp%tslen))
      allocate (wp%vits(npb, wp%tslen))
      wp%zsits = 0.d0
      wp%uits = 0.d0
      wp%vits = 0.d0
      !
      ! distance of each grid point to reference point
      do j = 1, npb
         distx(j) = waveBoundaryParameters(ibnd)%xb(j) - waveBoundaryParameters(ibnd)%x0
         disty(j) = waveBoundaryParameters(ibnd)%yb(j) - waveBoundaryParameters(ibnd)%y0
      end do
      !
      ! total surface elevation

      call writelog('ls', '', 'Calculating short wave elevation time series')
      call progress_indicator(.true., 0.d0, 5.d0, 2.d0)

      do it = 1, wp%tslen

         call progress_indicator(.false., dble(it) / wp%tslen * 100, 5.d0, 2.d0)

         do ik = 1, wp%K
            if (wp%PRindex(ik) == 1) then
               do j = 1, npb
                  wp%zsits(j, it) = wp%zsits(j, it) + wp%A(j, ik) * sin( &
                                    +wp%wgen(ik) * wp%tin(it) &
                                    - wp%kgen(ik) * (sin(wp%thetagen(ik)) * disty(j) &
                                                     + cos(wp%thetagen(ik)) * distx(j) &
                                                     ) &
                                    + wp%phigen(ik) &
                                    )
               end do
            end if
         end do
      end do
      ! depth-averaged velocity

      call writelog('ls', '', 'Calculating short wave velocity time series')
      call progress_indicator(.true., 0.d0, 5.d0, 2.d0)

      do it = 1, wp%tslen

         call progress_indicator(.false., dble(it) / wp%tslen * 100, 5.d0, 2.d0)

         do ik = 1, wp%K
            if (wp%PRindex(ik) == 1) then
               do j = 1, npb
                  ! Depth-average velocity in wave direction:
                  U = 1.d0 / hb0 * wp%wgen(ik) * wp%A(j, ik) * &
                      sin(wp%wgen(ik) * wp%tin(it) &
                           - wp%kgen(ik) * (sin(wp%thetagen(ik)) * disty(j) &
                                            + cos(wp%thetagen(ik)) * distx(j)) &
                           + wp%phigen(ik) &
                           ) * &
                      1.d0 / wp%kgen(ik)

                  ! Eastward component:
                  wp%uits(j, it) = wp%uits(j, it) + cos(wp%thetagen(ik)) * U
                  ! Northward component:
                  wp%vits(j, it) = wp%vits(j, it) + sin(wp%thetagen(ik)) * U
               end do
            end if
         end do
      end do
      !
      !
      ! Apply tapering to time series
      do j = 1, npb
         wp%uits(j, :) = wp%uits(j, :) * wp%taperf
         wp%vits(j, :) = wp%vits(j, :) * wp%taperf
         wp%zsits(j, :) = wp%zsits(j, :) * wp%taperf
      end do
   end subroutine generate_swts

   ! --------------------------------------------------------------
   ! ----------------------- Bound long wave ----------------------
   ! --------------------------------------------------------------
   subroutine generate_qbcf(ibnd, wp)

      use m_xbeach_filefunctions
      use interp

      use math_tools
      use wave_boundary_datastore

      implicit none

      ! input/output
      integer, intent(in) :: ibnd
      type(waveparamsnew), intent(inout) :: wp
      ! internal
      integer :: j, m, iq, it ! counters
      integer :: K ! copy of K
      integer :: halflen, status
      integer :: nInBoundaryFile
      logical :: firsttime ! used for output message only
      real(dp) :: deltaf ! difference frequency
      real(dp) :: qmean
      real(dp), dimension(:), allocatable :: term1, term2, term2new, dif, chk1, chk2, qinterp
      real(dp), dimension(npb) :: distx, disty
      real(dp), dimension(:, :), allocatable :: Eforc, D, deltheta, KKx, KKy, dphi3, k3, cg3, theta3, Abnd, D_sign
      real(dp), dimension(:, :, :), allocatable :: q
      complex(fftkind), dimension(:), allocatable :: Comptemp, Comptemp2, Gn
      complex(fftkind), dimension(:, :, :), allocatable :: Ftemp

      ! This function has changed with respect to previous versions of XBeach, in that
      ! the bound long wave has to be calculated separately at each longshore point,
      ! owing to longshore varying incident wave spectra

      ! shortcut variable
      K = wp%K

      ! Print message to screen

      call writelog('sl', '', 'Calculating primary wave interaction')

      ! Allocate two-dimensional variables for all combinations of interacting wave
      ! components to be filled triangular
      allocate (Eforc(K - 1, K))
      allocate (D(K - 1, K))
      allocate (deltheta(K - 1, K))
      allocate (KKx(K - 1, K))
      allocate (KKy(K - 1, K))
      allocate (dphi3(K - 1, K))
      allocate (k3(K - 1, K))
      allocate (cg3(K - 1, K))
      allocate (theta3(K - 1, K))
      ! Allocate variables for amplitude and Fourier coefficients of bound long wave
      allocate (Gn(wp%tslen))
      allocate (Abnd(K - 1, K))
      allocate (Ftemp(K - 1, K, 4)) ! Jaap qx, qy, qtot, zeta
      ! Storage for output discharge
      allocate (q(npb, wp%tslen, 4)) ! qx qy qtot, zeta
      allocate (qinterp(wp%tslen))
      !
      ! Initialize variables as zero
      Eforc = 0.
      D = 0.0
      deltheta = 0.0
      KKx = 0.0
      KKy = 0.0
      dphi3 = 0.0
      k3 = 0.0
      cg3 = 0.0
      theta3 = 0.0
      Gn = 0.0 * par_compi
      Abnd = 0.0
      Ftemp = 0.0 * par_compi
      q = 0.0

      ! First time is set true for each time new wave bc are generated
      firsttime = .true.

      ! upper half of frequency space
      halflen = wp%tslen / 2

      ! Run loop over wave-wave interaction components

      call progress_indicator(.true., 0.d0, 5.d0, 2.d0)

      do m = 1, K - 1

         call progress_indicator(.false., dble(m) / (K - 1) * 100, 5.d0, 2.d0)

         ! Allocate memory
         allocate (term1(K - m), term2(K - m), term2new(K - m), dif(K - m), chk1(K - m), chk2(K - m))

         ! Determine difference frequency
         deltaf = m * wp%dfgen

         ! Determine difference angles (pi already added)
         deltheta(m, 1:K - m) = abs(wp%thetagen(m + 1:K) - wp%thetagen(1:K - m)) + par_pi

         ! Determine x- and y-components of wave numbers of difference waves
         KKy(m, 1:K - m) = wp%kgen(m + 1:K) * sin(wp%thetagen(m + 1:K)) - wp%kgen(1:K - m) * sin(wp%thetagen(1:K - m))
         KKx(m, 1:K - m) = wp%kgen(m + 1:K) * cos(wp%thetagen(m + 1:K)) - wp%kgen(1:K - m) * cos(wp%thetagen(1:K - m))

         ! Determine difference wave numbers according to Van Dongeren et al. 2003
         ! eq. 19
         k3(m, 1:K - m) = sqrt(wp%kgen(1:K - m)**2 + wp%kgen(m + 1:K)**2 + &
                               2 * wp%kgen(1:K - m) * wp%kgen(m + 1:K) * cos(deltheta(m, 1:K - m)))

         ! Determine group velocity of difference waves
         cg3(m, 1:K - m) = 2.d0 * par_pi * deltaf / k3(m, 1:K - m)

         ! Modification Robert + Jaap: make sure that the bound long wave amplitude does not
         !                             explode when offshore boundary is too close to shore,
         !                             by limiting the interaction group velocity
         cg3(m, 1:K - m) = min(cg3(m, 1:K - m), waveBoundaryParameters(ibnd)%nmax * sqrt(par_g / k3(m, 1:K - m) * tanh(k3(m, 1:K - m) * hb0)))

         ! Determine difference-interaction coefficient according to Okihiro et al eq. 4a
         ! Instead of Herbers 1994 this coefficient is derived for the surface elavtion in stead of the bottom pressure.
         term1 = (-wp%wgen(1:K - m)) * wp%wgen(m + 1:K)
         term2 = (-wp%wgen(1:K - m)) + wp%wgen(m + 1:K)
         term2new = cg3(m, 1:K - m) * k3(m, 1:K - m)
         dif = (abs(term2 - term2new))
         if (any(dif > 0.01 * term2) .and. firsttime) then
            firsttime = .false.
         end if
         chk1 = cosh(wp%kgen(1:K - m) * hb0)
         chk2 = cosh(wp%kgen(m + 1:K) * hb0)

         D(m, 1:K - m) = -par_g * wp%kgen(1:K - m) * wp%kgen(m + 1:K) * cos(deltheta(m, 1:K - m)) / 2.d0 / term1 + &
                         +term2**2 / (par_g * 2) + par_g * term2 / &
                         ((par_g * k3(m, 1:K - m) * tanh(k3(m, 1:K - m) * hb0) - (term2new)**2) * term1) * &
                         (term2 * ((term1)**2 / par_g / par_g - wp%kgen(1:K - m) * wp%kgen(m + 1:K) * cos(deltheta(m, 1:K - m))) &
                          - 0.50d0 * ((-wp%wgen(1:K - m)) * wp%kgen(m + 1:K)**2 / (chk2**2) + wp%wgen(m + 1:K) * wp%kgen(1:K - m)**2 / (chk1**2)))

         ! Exclude interactions with components smaller than or equal to current
         ! component according to lower limit Herbers 1994 eq. 1
         where (wp%fgen <= deltaf) D(m, :) = 0.d0

         ! Exclude interactions with components that are cut-off by the fcutoff
         ! parameter
         if (deltaf <= waveBoundaryParameters(ibnd)%fcutoff) D(m, :) = 0.d0

         ! Determine phase of bound long wave assuming a local equilibrium with
         ! forcing of interacting primary waves according to Van Dongeren et al.
         ! 2003 eq. 21 (the angle is the imaginary part of the natural log of a
         ! complex number as long as the complex number is not zero)
         allocate (Comptemp(K - m), Comptemp2(K - m))
         Comptemp = conjg(wp%CompFn(1, wp%Findex(1) + m:wp%Findex(1) + K - 1))
         Comptemp2 = conjg(wp%CompFn(1, wp%Findex(1):wp%Findex(1) + K - m - 1))
         !dphi3(m,1:K-m) = par_pi+imag(log(Comptemp))-imag(log(Comptemp2))
         dphi3(m, 1:K - m) = aimag(log(Comptemp)) - aimag(log(Comptemp2))
         deallocate (Comptemp, Comptemp2)
         !
         ! Determine angle of bound long wave according to Van Dongeren et al. 2003 eq. 22
         theta3 = atan2(KKy, KKx)
         !
         ! free memory
         deallocate (term1, term2, term2new, dif, chk1, chk2)
      end do ! m=1,K-1
      !
      ! Output to screen

      if (.not. firsttime) then
         call writelog('lws', '', 'Warning: shallow water so long wave variance is reduced using nmax')
      end if
      call writelog('sl', '', 'Calculating flux at boundary')

      !
      ! Allocate temporary arrays for upcoming loop
      allocate (Comptemp(halflen - 1))
      allocate (Comptemp2(wp%tslen))
      !
      !
      ! distance of each grid point to reference point
      do j = 1, npb
         distx(j) = waveBoundaryParameters(ibnd)%xb(j) - waveBoundaryParameters(ibnd)%x0
         disty(j) = waveBoundaryParameters(ibnd)%yb(j) - waveBoundaryParameters(ibnd)%y0
      end do
      !
      ! Run a loop over the offshore boundary
      call progress_indicator(.true., 0.d0, 5.d0, 2.d0)
      do j = 1, npb
         ! Determine energy of bound long wave according to Herbers 1994 eq. 1 based
         ! on difference-interaction coefficient and energy density spectra of
         ! primary waves
         ! Robert: E = 2*D**2*S**2*dtheta**2*df can be rewritten as
         !         E = 2*D**2*Sf**2*df
         Eforc = 0
         do m = 1, K - 1
            Eforc(m, 1:K - m) = 2 * D(m, 1:K - m)**2 * wp%Sfinterpq(j, 1:K - m) * wp%Sfinterpq(j, m + 1:K) * wp%dfgen
         end do
         !
         ! Calculate bound wave amplitude for this offshore grid point
         ! Abnd = sqrt(2*Eforc*wp%dfgen)

         ! Menno: add the sign of the interaction coefficient in the amplitude. Large dtheta can result in a positive D.
         ! The phase of the bound wave is now only determined by phi1+phi2
         allocate (D_sign(K - 1, K))
         D_sign = 1
         ! Menno: put the sign of D in front of D_sign
         D_sign = sign(D_sign, D)
         ! Multiply amplitude with the sign of D
         Abnd = sqrt(2d0 * Eforc * wp%dfgen) * D_sign
         deallocate (D_sign)
         !
         ! Determine complex description of bound long wave per interaction pair of
         ! primary waves for first y-coordinate along seaside boundary
         Ftemp(:, :, 1) = Abnd / 2d0 * exp(-1 * par_compi * dphi3) * cg3 * cos(theta3) ! qx
         Ftemp(:, :, 2) = Abnd / 2d0 * exp(-1 * par_compi * dphi3) * cg3 * sin(theta3) ! qy
         Ftemp(:, :, 3) = Abnd / 2d0 * exp(-1 * par_compi * dphi3) * cg3 ! qtot
         Ftemp(:, :, 4) = Abnd / 2d0 * exp(-1 * par_compi * dphi3) ! eta
         !
         ! loop over qx,qy and qtot
         do iq = 1, 4
            ! Unroll wave component to correct place along the offshore boundary
            Ftemp(:, :, iq) = Ftemp(:, :, iq) * &
                              exp(-1 * par_compi * (KKy * disty(j) + KKx * distx(j)))
            ! Determine Fourier coefficients
            Gn(2:K) = sum(Ftemp(:, :, iq), DIM=2)
            Comptemp = conjg(Gn(2:halflen))
            call flipiv(Comptemp, halflen - 1)
            Gn(halflen + 2:wp%tslen) = Comptemp
            !
            ! Print status message to screen
            if (iq == 3) then
               call progress_indicator(.false., dble(j) / (npb) * 100, 5.d0, 2.d0)
            end if

            !
            ! Inverse Discrete Fourier transformation to transform back to time space
            ! from frequency space
            Comptemp2 = fft(Gn, inv=.true.)
            !
            ! Determine mass flux as function of time and let the flux gradually
            ! increase and decrease in and out the wave time record using the earlier
            ! specified window
            Comptemp2 = Comptemp2 / sqrt(dble(wp%tslen))
            q(j, :, iq) = real(Comptemp2 * wp%tslen) * wp%taperf
         end do ! iq=1,3
      end do ! j=1,npb
      !
      ! free memory
      deallocate (Comptemp, Comptemp2, Ftemp)
      !
      if (waveBoundaryParameters(ibnd)%wbcRemoveStokes == 1) then
         nInBoundaryFile = nint(wp%rtbc / wp%dtin) ! number of total time series in bcf (independent of dtin / dtbc differences)
         do iq = 1, 4
            do j = 1, npb
               qmean = sum(q(j, 1:nInBoundaryFile, iq)) / nInBoundaryFile
               q(j, :, iq) = q(j, :, iq) - qmean
            end do
         end do
      end if
      !
      if (waveBoundaryParameters(ibnd)%wbcQvarreduce < 1.d0 - 1d-10) then
         do iq = 1, 4
            do j = 1, npb
               qmean = sum(q(j, :, iq)) / wp%tslen
               q(j, :, iq) = waveBoundaryParameters(ibnd)%wbcQvarreduce * (q(j, :, iq) - qmean) + qmean
            end do
         end do
      end if
      !
      if (.not. waveBoundaryParameters(ibnd)%nonhspectrum) then
         ! If doing combined wave action balance and swell wave flux with swkhmin>0 then we need to add short wave velocity
         ! to time series of q here:
         if (waveBoundaryParameters(ibnd)%swkhmin > 0.d0) then
            do j = 1, npb
               q(j, :, 1) = q(j, :, 1) + wp%uits(j, :) * hb0 ! x, y flux
               q(j, :, 2) = q(j, :, 2) + wp%vits(j, :) * hb0
               q(j, :, 4) = q(j, :, 4) + wp%zsits(j, :) ! eta
            end do
         end if
         !
         !
         if (allocated(waveBoundaryTimeSeries(ibnd)%qxbct)) deallocate (waveBoundaryTimeSeries(ibnd)%qxbct)
         if (allocated(waveBoundaryTimeSeries(ibnd)%qybct)) deallocate (waveBoundaryTimeSeries(ibnd)%qybct)
         allocate (waveBoundaryTimeSeries(ibnd)%qxbct(npb, wp%tslenbc + 2))
         allocate (waveBoundaryTimeSeries(ibnd)%qybct(npb, wp%tslenbc + 2))
         if (wp%dtchanged) then
            ! Interpolate from internal time axis to output time axis
            do it = 2, wp%tslenbc + 1
               do j = 1, npb
                  qinterp = q(j, :, 1) ! speed-up
                  call linear_interp(wp%tin, qinterp, wp%tslen, &
                                     (it - 1) * wp%dtbc, waveBoundaryTimeSeries(ibnd)%qxbct(j, it), status)
                  qinterp = q(j, :, 2)
                  call linear_interp(wp%tin, qinterp, wp%tslen, &
                                     (it - 1) * wp%dtbc, waveBoundaryTimeSeries(ibnd)%qybct(j, it), status)
               end do
            end do
         else
            ! no need for interpolation
            waveBoundaryTimeSeries(ibnd)%qxbct(:, 2:wp%tslenbc + 1) = q(:, :, 1)
            waveBoundaryTimeSeries(ibnd)%qybct(:, 2:wp%tslenbc + 1) = q(:, :, 2)
         end if
         waveBoundaryTimeSeries(ibnd)%qxbct(:, 1) = waveBoundaryTimeSeries(ibnd)%qxbct(:, 2)
         waveBoundaryTimeSeries(ibnd)%qxbct(:, wp%tslenbc + 2) = waveBoundaryTimeSeries(ibnd)%qxbct(:, wp%tslenbc + 1)
         waveBoundaryTimeSeries(ibnd)%qybct(:, 1) = waveBoundaryTimeSeries(ibnd)%qybct(:, 2)
         waveBoundaryTimeSeries(ibnd)%qybct(:, wp%tslenbc + 2) = waveBoundaryTimeSeries(ibnd)%qybct(:, wp%tslenbc + 1)
      else
         do j = 1, npb
            ! add to velocity time series
            wp%uits(j, :) = wp%uits(j, :) + q(j, :, 1) / hb0
            wp%vits(j, :) = wp%vits(j, :) + q(j, :, 2) / hb0
            ! add to surface elevation time series
            wp%zsits(j, :) = wp%zsits(j, :) + q(j, :, 4)
         end do
      end if !      nonhspectrum==1

      ! Free memory
      deallocate (Eforc, D, deltheta, KKx, KKy, dphi3, k3, cg3, theta3, Gn, Abnd, q)

   end subroutine generate_qbcf

   ! --------------------------------------------------------------
   ! --------------- Non-hydrostatic wave time --------------------
   ! ---------------- series file generation ----------------------
   subroutine generate_nhtimeseries_file(ibnd, wp)

      use m_xbeach_filefunctions

      use wave_boundary_datastore

      implicit none

      ! input/output
      integer, intent(in) :: ibnd
      type(waveparamsnew), intent(inout) :: wp
      ! internal

      call writelog('ls', '', 'Writing short wave time series to ', wp%nhfilename)

      if (allocated(waveBoundaryTimeSeries(ibnd)%zsbct)) deallocate (waveBoundaryTimeSeries(ibnd)%zsbct)
      if (allocated(waveBoundaryTimeSeries(ibnd)%ubct)) deallocate (waveBoundaryTimeSeries(ibnd)%ubct)
      allocate (waveBoundaryTimeSeries(ibnd)%zsbct(npb, wp%tslen + 2))
      allocate (waveBoundaryTimeSeries(ibnd)%ubct(npb, wp%tslen + 2))

      waveBoundaryTimeSeries(ibnd)%zsbct(:, 2:wp%tslen + 1) = wp%zsits
      waveBoundaryTimeSeries(ibnd)%ubct(:, 2:wp%tslen + 1) = wp%uits

      waveBoundaryTimeSeries(ibnd)%zsbct(:, 1) = waveBoundaryTimeSeries(ibnd)%zsbct(:, 2)
      waveBoundaryTimeSeries(ibnd)%zsbct(:, wp%tslen + 2) = waveBoundaryTimeSeries(ibnd)%zsbct(:, wp%tslen + 1)
      waveBoundaryTimeSeries(ibnd)%ubct(:, 1) = waveBoundaryTimeSeries(ibnd)%ubct(:, 2)
      waveBoundaryTimeSeries(ibnd)%ubct(:, wp%tslen + 2) = waveBoundaryTimeSeries(ibnd)%ubct(:, wp%tslen + 1)

   end subroutine generate_nhtimeseries_file

   subroutine set_stationary_spectrum(ibnd, combspec)
      use m_sferic
      use m_physcoef
      use interp

      implicit none

      integer, intent(in) :: ibnd
      type(spectrum), intent(in) :: combspec

      double precision :: xcycle
      double precision, dimension(:), allocatable :: angcart, Sdcart, eet
      integer :: j

      allocate (angcart(combspec%nang))
      allocate (Sdcart(combspec%nang))
      allocate (eet(ntheta_s)); eet = 0d0

      xcycle = 2d0 * pi
      ! combspec%ang contains nautical directions from 0 to 2 pi; convert to cartesian from -3/2pi to 1/2 pi
      ! in reverse order

      ! Dit verwacht een 1:ntheta ding voor ee_s
      call interp_using_trapez_rule(combspec%ang, combspec%Sd, combspec%nang, xcycle, waveBoundaryParameters(ibnd)%theta_s, eet, waveBoundaryParameters(ibnd)%ntheta_s)
      do j = 1, npb
         waveSpectrumAdministration(ibnd)%ee_s(:, j) = eet ! need mapping to correct waveboundary ghostcells here
      end do

      waveSpectrumAdministration(ibnd)%ee_s(:, 1:npb) = waveSpectrumAdministration(ibnd)%ee_s(:, 1:npb) * rhomean * ag

   end subroutine set_stationary_spectrum

end module
