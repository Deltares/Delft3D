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

module m_fourier_analysis

! TODO:
!     * ucx en ucy on (1:#nods) is altijd de diepte gemiddelde, ook in 2d (dan is het de enige snelheid)

   use precision
   use string_module, only: str_lower, strsplit
   use unstruc_netcdf
   use m_flow, only: kmx
   use m_alloc
   use m_flowtimes, only: dt_user, irefdate, Tzone
   use m_sferic, only: jsferic
   use m_missing, only: dmiss
   use running_mean_wrapper
   implicit none

   !> struct to enable different sizes of suma and sumb
   type fdata
      real(kind=fp), allocatable :: rdata(:) !< actual data for suma and sumb of fourier analysis
      real(kind=sp), allocatable :: sdata(:) !< actual data for min/max calculations
      type(TRunningMean), pointer :: running => null() !< struct holding data for running mean calculations
   end type

   type fourier_type
      !
      ! integers
      !
      real(kind=fp) :: fouwrt !< Time to write fourier file
      integer :: nofouvar = 0 !< Number of parameters to write to NetCDF file
      integer :: idf !< Number of requests for data where initial dry only
      integer :: ibleh !< Number of energy height quantities in Fourier config
      integer :: ibluc !< Number of cell-centre velocity magnitude quantities in Fourier config
      integer :: iblws !< Number of wind speed (on links) quantities in Fourier config
      integer :: iblsul !< Number of water level on links quantities in Fourier config
      integer :: iblfb !< Number of freeboard quantities in Fourier config
      integer :: iblwdog !< Number of water depth on ground quantities in Fourier config
      integer :: iblvog !< Number of volume on ground quantities in Fourier config
      !
      ! pointers
      !
      integer, dimension(:), pointer :: fconno => null() !< Constituent number for Fourier analysis
      integer, dimension(:), pointer :: fnumcy => null() !< Number of cycles for fourier analysis
      real(kind=fp), dimension(:), pointer :: ftmsto => null() !< stop time for fourier analysis
      real(kind=fp), dimension(:), pointer :: ftmstr => null() !< start time for fourier analysis
      integer, dimension(:), pointer :: foumask => null() !< 0: no additional mask, 1: initial dry points only
      integer, dimension(:, :), pointer :: idvar => null() !< Ids of the variables in UGRID format
      integer, dimension(:, :), pointer :: fouref => null() !< Reference table: (ifou,1): fouid (line in input file)
                                                                     !!                  (ifou,2): fouvarstart (first index in fouvarnam/idvar to be used by this ifou
      !
      real(kind=fp), dimension(:), pointer :: fknfac => null() !< Fourier amplitude correction factor
      real(kind=fp), dimension(:), pointer :: foufas => null() !< Frequency for fourier analysis
      type(fdata), dimension(:), pointer :: fousma => null() !< Suma of fourier analysis
      type(fdata), dimension(:), pointer :: fousmb => null() !< Sumb of fourier analysis
      real(kind=fp), dimension(:), pointer :: fv0pu => null() !< Fourier phase correction
      real(kind=fp), dimension(:), pointer :: fthresh => null() !< Threshold for time analysis
      !
      character(len=1), dimension(:), pointer :: foutyp => null() !< n  : normal Fourier analysis
                                                                               !! x/i: max (x) or min (i) values
                                                                               !! r/u: running max (r) or min (u) values
                                                                               !! R/U: running max (R) or min (U) values with associated time
                                                                               !! >/<: time that values are above (>) or below (<) fthresh
                                                                               !! l  : values at last time step
                                                                               !! a  : average values
                                                                               !! c  : q1 when q2 reaches maximum
                                                                               !! C  : q1 when q2 reaches minimum
      character(len=16), dimension(:), pointer :: founam => null() !< Names of variables for fourier analysis
      character(len=16), dimension(:), pointer :: founamc => null() !< Names of variables for combined min/max
      logical, dimension(:), pointer :: withTime => null() !< With extra output of time of min/max
      character(len=50), dimension(:), pointer :: fouvarnam => null() !< Names of variables for fourier analysis as written to NetCDF file
      character(len=50), dimension(:), pointer :: fouvarnamstd => null() !< Standard name of variables for fourier analysis as written to NetCDF file
      character(len=50), dimension(:), pointer :: fouvarnamlong => null() !< Part of the long names of variables for fourier analysis as written to NetCDF file
      character(len=50), dimension(:), pointer :: fouvarunit => null() !< Unit of variables for fourier analysis as written to NetCDF file
   end type fourier_type
!-------------------------------------------------------------------------------------------------------

   type(fourier_type), target :: gdfourier
   character(len=:), allocatable :: FouOutputFile

   private

   integer, allocatable, target :: initial_wet_mask(:) !< [-] copy of the initial dry/wet flag (kfs)
   real(kind=fp), allocatable, target :: sul(:) !< [m] water level on flow links
   real(kind=fp), allocatable, target :: eh(:) !< [m] energy height (m) at end of time step

   integer :: nofou !< Number of fourier components to be analyzed
   type(t_unc_mapids) :: fileids !< Set of file and variable ids for this file.

   real(kind=fp) :: time_unit_factor

   real(kind=fp), parameter :: defaultd = -999.0_fp ! Default value for doubles
   real(kind=fp), parameter :: dmiss_minmax = 1e30_fp ! Default values for min/max calculations
   real(kind=fp), parameter :: tol_time = 1d-9 ! tolerance for comparing times

   public :: fouini
   public :: alloc_fourier_analysis_arrays
   public :: reafou
   public :: postpr_fourier

   public :: fourierIsActive, fourierWithMask, fourierWithEh, fourierWithUc, fourierWithFb, fourierWithWdog, fourierWithVog, fourierWithSul
   public :: nofou
   public :: FouOutputFile

   public :: initial_wet_mask, sul, eh

contains

   !> fourier is (still) active
   logical function fourierIsActive()
      fourierIsActive = (nofou > 0)
   end function fourierIsActive

   !> do fourier with energy height or not
   logical function fourierWithMask()
      fourierWithMask = (gdfourier%idf > 0)
   end function fourierWithMask

   !> do fourier with energy height or not
   logical function fourierWithEh()
      fourierWithEh = (gdfourier%ibleh > 0)
   end function fourierWithEh

   !> do fourier with flow magnitude or not
   logical function fourierWithUc()
      fourierWithUc = (gdfourier%ibluc > 0)
   end function fourierWithUc

   !> do fourier with freeboard or not
   logical function fourierWithFb()
      fourierWithFb = (gdfourier%iblfb > 0)
   end function fourierWithFb

   !> do fourier with waterdepth on ground or not
   logical function fourierWithWdog()
      fourierWithWdog = (gdfourier%iblwdog > 0)
   end function fourierWithWdog

   !> do fourier with volume on ground or not
   logical function fourierWithVog()
      fourierWithVog = (gdfourier%iblvog > 0)
   end function fourierWithVog

   !> do fourier with water levels on links or not
   logical function fourierWithSul()
      fourierWithSul = (gdfourier%iblsul > 0)
   end function fourierWithSul

   !> count the number of fourier/min/max quantities
   subroutine count_fourier_variables
      implicit none

      integer :: ivar !< fourier quantity
      integer :: i !< name index
      character(len=16) :: names(2) !< temporary array containing the primary and optional secondary quantity

      gdfourier%idf = 0
      gdfourier%ibleh = 0
      gdfourier%iblws = 0
      gdfourier%ibluc = 0
      gdfourier%iblfb = 0
      gdfourier%iblwdog = 0
      gdfourier%iblvog = 0
      gdfourier%iblsul = 0
      do ivar = 1, nofou
         !
         names(1) = gdfourier%founam(ivar)
         names(2) = gdfourier%founamc(ivar)
         if (gdfourier%foumask(ivar) > 0) then
            gdfourier%idf = gdfourier%idf + 1
         end if
         do i = 1, 2
            select case (names(i))
            case ('eh')
               gdfourier%ibleh = gdfourier%ibleh + 1
            case ('ws')
               gdfourier%iblws = gdfourier%iblws + 1
            case ('sul')
               gdfourier%iblsul = gdfourier%iblsul + 1
            case ('uc')
               gdfourier%ibluc = gdfourier%ibluc + 1
            case ('fb')
               gdfourier%iblfb = gdfourier%iblfb + 1
            case ('wdog')
               gdfourier%iblwdog = gdfourier%iblwdog + 1
            case ('vog')
               gdfourier%iblvog = gdfourier%iblvog + 1
            end select
         end do
      end do
   end subroutine count_fourier_variables

   !> allocate the arrays for fourier and min/max
   subroutine alloc_fourier_analysis_arrays()
      use m_flowgeom, only: bl, bl_min, ndx
      use m_flow, only: s1, s1max, kmx, laytyp, numtopsig
      use precision
      implicit none

      integer :: istat
      integer, parameter :: imissval = -1
      real(kind=dp), parameter :: rmissval = -999.999_fp

      istat = 0
      !
      ! Arrays for Fourier analysis (fourier.igs)
      !
      if (istat == 0) call reallocp(gdfourier%fconno, nofou, stat=istat, keepExisting=.false.)
      if (istat == 0) call reallocp(gdfourier%fnumcy, nofou, stat=istat, keepExisting=.false.)
      if (istat == 0) call reallocp(gdfourier%ftmsto, nofou, stat=istat, keepExisting=.false.)
      if (istat == 0) call reallocp(gdfourier%ftmstr, nofou, stat=istat, keepExisting=.false.)
      if (istat == 0) call reallocp(gdfourier%foumask, nofou, stat=istat, keepExisting=.false.)
      if (istat == 0) call reallocp(gdfourier%idvar, [MAX_ID_VAR, gdfourier%nofouvar], stat=istat, keepExisting=.false.)
      if (istat == 0) call reallocp(gdfourier%fouref, [nofou, 2], stat=istat, keepExisting=.false.)
      !
      if (istat == 0) call reallocp(gdfourier%fknfac, nofou, stat=istat, keepExisting=.false.)
      if (istat == 0) call reallocp(gdfourier%foufas, nofou, stat=istat, keepExisting=.false.)
      if (istat == 0) allocate (gdfourier%fousma(nofou), stat=istat)
      if (istat == 0) allocate (gdfourier%fousmb(nofou), stat=istat)
      if (istat == 0) call reallocp(gdfourier%fv0pu, nofou, stat=istat, keepExisting=.false.)
      if (istat == 0) call reallocp(gdfourier%fthresh, nofou, stat=istat, keepExisting=.false.)
      !
      if (istat == 0) call reallocp(gdfourier%foutyp, nofou, stat=istat, keepExisting=.false.)
      if (istat == 0) call reallocp(gdfourier%founam, nofou, stat=istat, keepExisting=.false.)
      if (istat == 0) call reallocp(gdfourier%founamc, nofou, stat=istat, keepExisting=.false.)
      if (istat == 0) call reallocp(gdfourier%withTime, nofou, stat=istat, fill=.false.)
      if (istat == 0) call reallocp(gdfourier%fouvarnam, gdfourier%nofouvar, stat=istat, keepExisting=.false.)
      if (istat == 0) call reallocp(gdfourier%fouvarnamstd, gdfourier%nofouvar, stat=istat, keepExisting=.false.)
      if (istat == 0) call reallocp(gdfourier%fouvarnamlong, gdfourier%nofouvar, stat=istat, keepExisting=.false.)
      if (istat == 0) call reallocp(gdfourier%fouvarunit, gdfourier%nofouvar, stat=istat, keepExisting=.false.)

      if (istat /= 0) then
         ! Exception handling for allocation of fourier arrays
         msgbuf = 'Allocation error in alloc_fourier_analysis_arrays; Fourier disabled.'
         call err_flush()
         nofou = 0
      else
         ! Initialise arrays for Fourier analysis
         gdfourier%fnumcy = imissval
         !
         gdfourier%ftmsto = rmissval
         gdfourier%ftmstr = rmissval
         gdfourier%fknfac = rmissval
         gdfourier%foufas = rmissval
         gdfourier%fv0pu = rmissval
         gdfourier%fthresh = rmissval
         !
         gdfourier%founam = ' '
         gdfourier%founamc = ' '
         gdfourier%fouvarnam = ' '
         gdfourier%fouvarnamstd = ' '
         gdfourier%fouvarnamlong = ' '
         gdfourier%fouvarunit = ' '
         !
      end if

      ! Create fourier specific bl and s1 array, only if required
      if (kmx > 0 .and. (laytyp(1) == 1 .or. numtopsig > 0)) then
         call realloc(bl_min, ndx, keepExisting=.false., stat=istat)
         call realloc(s1max, ndx, keepExisting=.false., stat=istat)
         bl_min = bl
         s1max = s1
      end if
   end subroutine alloc_fourier_analysis_arrays

   !> - Read fourier input file and stores the
   !! variables necessary for the analysis in arrays.
   subroutine reafou(lunfou, filfou, kmax, lstsc, lsal, ltem, tstart, tstop, ti_fou, success)
      use precision
      use mathconsts
      use string_module
      use unstruc_messages
      use m_flowtimes, only: Tudunitstr
      implicit none
      !
      ! Global variables
      !
      integer, intent(in) :: lsal !< Description and declaration in dimens.igs
      integer, intent(in) :: lstsc !< Description and declaration in dimens.igs
      integer, intent(in) :: ltem !< Description and declaration in dimens.igs
      integer, intent(in) :: lunfou !< Unit number fourier input file
      character(*), intent(in) :: filfou !< File name for fourier analysis input
      integer, intent(in) :: kmax !< number of vertical layers
      real(kind=fp), intent(in) :: tstart !< simulation start time
      real(kind=fp), intent(in) :: tstop !< simulation stop time
      real(kind=fp), intent(in) :: ti_fou !< interval updating fou file ( < 0.0 in case of each computational time step)
      logical, intent(out) :: success !< function result
      !
      ! Local variables
      !
      integer, dimension(:), pointer :: fconno
      integer, dimension(:), pointer :: fnumcy
      integer, dimension(:), pointer :: foumask
      integer, dimension(:, :), pointer :: idvar
      integer, dimension(:, :), pointer :: fouref
      real(kind=fp), pointer :: fouwrt
      integer, pointer :: nofouvar
      real(kind=fp), dimension(:), pointer :: ftmsto
      real(kind=fp), dimension(:), pointer :: ftmstr
      real(kind=fp), dimension(:), pointer :: fknfac
      real(kind=fp), dimension(:), pointer :: foufas
      type(fdata), dimension(:), pointer :: fousma
      type(fdata), dimension(:), pointer :: fousmb
      real(kind=fp), dimension(:), pointer :: fv0pu
      real(kind=fp), dimension(:), pointer :: fthresh
      character(len=1), dimension(:), pointer :: foutyp
      character(len=16), dimension(:), pointer :: founam
      character(len=50), dimension(:), pointer :: fouvarnam
      character(len=50), dimension(:), pointer :: fouvarnamstd
      character(len=50), dimension(:), pointer :: fouvarnamlong
      character(len=50), dimension(:), pointer :: fouvarunit

      integer :: fouid ! Counter linenumber-commentlines
      integer :: i ! Counter
      integer :: ifou ! Counter
      integer :: ivar ! Counter
      integer :: flayno ! layer number (ignored)
      integer :: sizea, sizeb ! size of the fourier arrays a and b
      integer :: icol
      integer :: icol1
      integer :: linenumber ! Line number in Fourier input file
      integer :: ncols ! actual number of fields encountered on a line
      real(kind=fp) :: rstart ! Start time for fourier analysis
      real(kind=fp) :: rstop ! Stop  time for fourier analysis
      real(kind=fp) :: rstart_last ! Start time for 'last' averaging
      real(kind=fp) :: fillValue ! value to initialize array
      character(len=:), allocatable :: cdummy ! Help string to read foutyp
      ! The message depends on the error.
      character(len=300) :: message
      character(len=132) :: line ! Used for format free reading
      character(len=30), allocatable :: columns(:) ! each line is split into separate fields (columns)
      character(len=:), allocatable :: cref ! ref. number converted into a string
      character(len=1) :: foutyp_new ! new (Fourier) analysis flag
      integer :: iostat ! error code file io
      integer :: ierr ! error code allocate
      logical :: isRunningMean ! min/max is based on running mean
      type(TRunningMeanMeta) :: RMmeta ! meta data in case of running mean calculation
      !
      !! executable statements -------------------------------------------------------
      !
      fknfac => gdfourier%fknfac
      foufas => gdfourier%foufas
      fv0pu => gdfourier%fv0pu
      fthresh => gdfourier%fthresh
      fconno => gdfourier%fconno
      fnumcy => gdfourier%fnumcy
      foumask => gdfourier%foumask
      idvar => gdfourier%idvar
      fouref => gdfourier%fouref
      fouwrt => gdfourier%fouwrt
      nofouvar => gdfourier%nofouvar
      ftmsto => gdfourier%ftmsto
      ftmstr => gdfourier%ftmstr
      foutyp => gdfourier%foutyp
      founam => gdfourier%founam
      fouvarnam => gdfourier%fouvarnam
      fouvarnamstd => gdfourier%fouvarnamstd
      fouvarnamlong => gdfourier%fouvarnamlong
      fouvarunit => gdfourier%fouvarunit
      fousma => gdfourier%fousma
      fousmb => gdfourier%fousmb

      !
      success = .false.
      ifou = 1
      do i = 1, nofou
         fconno(i) = 1
         foumask(i) = 0
         fouref(i, :) = -1
         foutyp(i) = 'n'
      end do
      do i = 1, nofouvar
         idvar(:, i) = 0
      end do
      !
      cdummy = ' '
      !
      linenumber = 0
      fouid = 0
      call init_running_mean_admin()
      !
      ! reading file
      !
      ! -->
20    continue
      read (lunfou, '(a)', iostat=iostat) line
      if (iostat /= 0) then
         write (msgbuf, '(a)') 'Error reading data line from .fou file.'
         call warn_flush()
         goto 6666
      end if
      !
      linenumber = linenumber + 1
      if (line(1:1) == '*' .or. line == ' ') goto 20
      !
      call str_lower(line, 132)
      if (allocated(columns)) deallocate (columns)
      call strsplit(line, 1, columns, 1)
      ncols = size(columns)

      fouid = fouid + 1
      !
      ! determine array names and type (scalar or vectorial) for fourier analysis
      !
      founam(ifou) = columns(1)
      !
      select case (founam(ifou))
      case ('wl')
         founam(ifou) = 's1'
         fouref(ifou, 1) = fouid
      case ('ws') ! absolute wind-speed
         founam(ifou) = 'ws'
         fouref(ifou, 1) = fouid
      case ('ux', 'uxa')
         fouref(ifou, 1) = fouid
      case ('uy', 'uya')
         fouref(ifou, 1) = fouid
      case ('eh')
         fouref(ifou, 1) = fouid
      case ('wd')
         fouref(ifou, 1) = fouid
      case ('uc') ! absolute cell-centre velocity magnitude
         founam(ifou) = 'uc'
         fouref(ifou, 1) = fouid
      case ('qf') ! interpolated cell-centre velocities (vector)
         founam(ifou) = 'qxk' ! ucx
         fouref(ifou, 1) = fouid
      case ('bs')
         founam(ifou) = 'ta'
         fouref(ifou, 1) = fouid
      case ('ct') ! constituent: temperature (scalar)
         if (ltem /= 0) then
            founam(ifou) = 'r1'
            fconno(ifou) = ltem
            fouref(ifou, 1) = fouid
         else
            ! Exception: no temperature
            msgbuf = 'Temperature specified in .fou file, but no temperature available. '
            call warn_flush()
            goto 6666
         end if
      case ('cs') ! constituent: salinity (scalar)
         if (lsal /= 0) then
            founam(ifou) = 'r1'
            fconno(ifou) = lsal
            fouref(ifou, 1) = fouid
         else
            ! Exception: no salt
            msgbuf = 'Salinity specified in .fou file, but no salinity available.'
            call warn_flush()
            goto 6666
         end if
      case ('fb')
         founam(ifou) = 'fb'
         fouref(ifou, 1) = fouid
      case ('wdog')
         founam(ifou) = 'wdog'
         fouref(ifou, 1) = fouid
      case ('vog')
         founam(ifou) = 'vog'
         fouref(ifou, 1) = fouid
      case ('q1')
         founam(ifou) = 'q1'
         fouref(ifou, 1) = fouid
      case ('sul')
         founam(ifou) = 'sul'
         fouref(ifou, 1) = fouid
      case default ! constituent, anything else
         if (.not. ReadConstituentNumber(founam(ifou), fconno(ifou))) then
            msgbuf = 'Unable to initialize fourier analysis for '''//trim(founam(ifou))//'''.'
            call warn_flush()
            goto 6666
         end if
         fconno(ifou) = fconno(ifou) + max(lsal, ltem)
         if (fconno(ifou) > lstsc) then
            msgbuf = 'Unable to initialize fourier analysis for constituent '''//trim(founam(ifou))//'''.'
            call warn_flush()
            goto 6666
         end if
         founam(ifou) = 'r1'
         fouref(ifou, 1) = fouid
      end select
      !
      ! read start time, stop time, number of cycles
      !       determine corresponding integer time step numbers and frequency
      !
      read (columns(2), *, err=6666) rstart
      rstart = rstart * time_unit_factor ! convert to kernel time unit
      !
      if (rstart < 0.0_fp) then
         rstart = tstart
      else if (rstart < tstart) then
         msgbuf = 'Fourier sample interval start preceeds simulation start TStart.'
         call warn_flush()
         goto 6666
      end if
      ftmstr(ifou) = rstart
      !
      read (columns(3), *, err=6666) rstop
      rstop = rstop * time_unit_factor ! convert to kernel time unit
      !
      if (rstop < 0.0_fp) then
         rstop = tstop
      else if (rstop > tstop) then
         msgbuf = 'Fourier sample interval stop exceeds simulation end TStop.'
         call warn_flush()
         goto 6666
      end if
      if (rstart > rstop) then
         msgbuf = 'Fourier sample interval start > Fourier sample interval stop.'
         call warn_flush()
         goto 6666
      end if
      ftmsto(ifou) = rstop
      !
      ! Fouwrt catches the end of all fourier analyses
      !
      fouwrt = max(fouwrt, ftmsto(ifou))
      !
      read (columns(4), *, err=6666) fnumcy(ifou)
      !
      foufas(ifou) = twopi * real(fnumcy(ifou), fp) / (ftmsto(ifou) - ftmstr(ifou))
      !
      ! read nodal amplifications and phase shifts for comparison
      !       with cotidal maps
      !
      read (columns(5), *, err=6666) fknfac(ifou)
      !
      read (columns(6), *, err=6666) fv0pu(ifou)
      !
      if (fv0pu(ifou) < 0.0_fp) fv0pu(ifou) = fv0pu(ifou) + 360.0_fp
      fv0pu(ifou) = mod(fv0pu(ifou), 360.0_fp)
      !
      icol = 7
      !
      if (founam(ifou) /= 's1' .and. &
          founam(ifou) /= 'eh' .and. &
          founam(ifou) /= 'wd' .and. &
          founam(ifou) /= 'sul' .and. &
          founam(ifou) /= 'ta' .and. &
          founam(ifou) /= 'uxa' .and. &
          founam(ifou) /= 'uya' .and. &
          founam(ifou) /= 'ws' .and. &
          founam(ifou) /= 'fb' .and. &
          founam(ifou) /= 'wdog' .and. &
          founam(ifou) /= 'vog') then
         !
         read (columns(7), *, iostat=iostat) flayno
         if (iostat /= 0) then
            write (msgbuf, '(a,i0,2a)') 'Could not read layer number in line ', linenumber, ' and column 7 of file ', trim(filfou)
            call warn_flush()
            goto 6666
         else if (flayno > kmax) then
            write (msgbuf, '(a,i0,a,i0,a)') 'Requested layer exceeds,', flayno, ' exceeds max layer, ', kmax, '.'
            call warn_flush()
            goto 6666
         end if
         icol = icol + 1
      end if
      !
      do while (ncols >= icol)
         cdummy = trim(columns(icol))
         foutyp_new = ' '
         !
         select case (cdummy)
         case ('inidryonly')
            select case (founam(ifou))
            case ('s1', 'eh', 'wd')
               if (foutyp(ifou) /= 'x') then
                  write (msgbuf, '(a,i0)') "The inidryonly option is only supported when computing the maximum. Setting ignored for line ", linenumber
               else
                  foumask(ifou) = 1
                  write (msgbuf, '(3a,i0,a)') 'in file ', trim(filfou), ' line ', linenumber, &
                        & ': Fourier analysis only for initially dry points'
               end if
            case default
               write (msgbuf, '(3a,i0)') "The inidryonly option is not supported for '", trim(founam(ifou)), "'. Setting ignored for line ", linenumber
            end select
            call warn_flush()

         case ('last')
            rstart_last = rstop - (fnumcy(ifou) - 1) * ti_fou
            if (rstart_last < rstart) then
               write (msgbuf, '(a,i0)') "Not enough steps between tstart and tstop for 'last' in line ", linenumber
               call warn_flush()
               goto 6666
            else
               rstart = rstart_last
            end if
            ftmstr(ifou) = rstart
            fnumcy(ifou) = 0
            foufas(ifou) = 0.0_fp
            foutyp_new = 'l'

         case ('max', 'min', 'avg')
            isRunningMean = .false.
            if (fnumcy(ifou) > 0) then
               if (cdummy /= 'avg') then
                  RMmeta = fillMetaRM(founam(ifou), fnumcy(ifou), rstart, rstop, ti_fou)
                  call init_running_mean(fousma(ifou)%running, RMmeta)
                  icol1 = min(icol + 1, ncols)
                  if (cdummy == 'max') then
                     if (columns(icol1) == 'time') then
                        foutyp_new = 'R'
                        icol = icol + 1
                     elseif (icol < ncols) then
                        write (msgbuf, '(3a,i0,3a)') 'in file ', trim(filfou), ' line ', linenumber, &
                              & ': invalid quantity ', trim(columns(icol + 1)), ' after max; only time supported for running mean quantities.'
                        call warn_flush()
                        goto 6666
                     else
                        foutyp_new = 'r'
                     end if
                  elseif (cdummy == 'min') then
                     if (columns(icol1) == 'time') then
                        foutyp_new = 'U'
                        icol = icol + 1
                     elseif (icol < ncols) then
                        write (msgbuf, '(3a,i0,3a)') 'in file ', trim(filfou), ' line ', linenumber, &
                              & ': invalid quantity ', trim(columns(icol + 1)), ' after min; only time supported for running mean quantities.'
                        call warn_flush()
                        goto 6666
                     else
                        foutyp_new = 'u'
                     end if
                  end if
                  isRunningMean = .true.
               end if
               fnumcy(ifou) = 0
               foufas(ifou) = 0.0_fp
            end if
            if (.not. isRunningMean) then
               select case (cdummy)
               case ('max')
                  foutyp_new = 'x'
                  if (icol < ncols) then
                     if (columns(icol + 1) == 'time') then
                        gdfourier%withTime(ifou) = .true.
                        icol = icol + 1
                     else if (valid_combi(founam(ifou), columns(icol + 1))) then
                        foutyp_new = 'c'
                        gdfourier%founamc(ifou) = columns(icol + 1)
                        icol = icol + 1
                     else
                        write (msgbuf, '(3a,i0,3a)') 'in file ', trim(filfou), ' line ', linenumber, &
                              & ': invalid quantity ', trim(columns(icol + 1)), ' after max; maybe its location isn''t consistent with that of the primary quantity.'
                        call warn_flush()
                        goto 6666
                     end if
                  end if
               case ('min')
                  foutyp_new = 'i'
                  if (icol < ncols) then
                     if (columns(icol + 1) == 'time') then
                        gdfourier%withTime(ifou) = .true.
                        icol = icol + 1
                     else if (valid_combi(founam(ifou), columns(icol + 1))) then
                        foutyp_new = 'C'
                        gdfourier%founamc(ifou) = columns(icol + 1)
                        icol = icol + 1
                     else
                        write (msgbuf, '(3a,i0,3a)') 'in file ', trim(filfou), ' line ', linenumber, &
                              & ': invalid quantity ', trim(columns(icol + 1)), ' after min; maybe its location isn''t consistent with that of the primary quantity.'
                        call warn_flush()
                        goto 6666
                     end if
                  end if
               case ('avg')
                  foutyp_new = 'a'
               end select
            end if

         case ('time')
            fnumcy(ifou) = 0
            foufas(ifou) = 0.0_fp

            if (icol < ncols - 1) then
               icol = icol + 1
               cdummy = trim(columns(icol))
               select case (cdummy)
               case ('above')
                  foutyp_new = '>'
               case ('below')
                  foutyp_new = '<'
               case default
                  write (msgbuf, '(3a,i0,2a)') 'in file ', trim(filfou), ' line ', linenumber, &
                        & ': expecting above or below after "time", instead of ', trim(cdummy)
                  call warn_flush()
                  goto 6666
               end select

               icol = icol + 1
               read (columns(icol), *, err=6666) fthresh(ifou)
            else
               write (msgbuf, '(3a,i0,2a)') 'in file ', trim(filfou), ' line ', linenumber, &
                     & ': incomplete "time above/below <threshold>" flag found.'
               call warn_flush()
               goto 6666
            end if

         case default
            if (cdummy /= 'mean') then
               write (msgbuf, '(3a,i0,2a)') 'in file ', trim(filfou), ' line ', linenumber, &
                     & ': expecting avg, min, max, time or mean, instead of ', trim(cdummy)
               call warn_flush()
               goto 6666
            end if
         end select

         icol = icol + 1
         if (foutyp_new /= ' ') then
            if (foutyp(ifou) /= 'n') then
               write (msgbuf, '(a,i0,2a)') "Multiple flags specified on line ", linenumber, " : ", line
               call warn_flush()
               goto 6666
            else
               foutyp(ifou) = foutyp_new
            end if
         end if
      end do
      !
      ifou = ifou + 1
      !
      if (ifou <= nofou) goto 20
      ! <--
      !
      success = .true.
      !
      ! Define all variable names to be written
      ! Add the (start-)index ivar to fouref(..,2)
      !
      ivar = 0
      do ifou = 1, nofou
         fouref(ifou, 2) = ivar + 1
         cref = fouref2string(fouref(ifou, 1))
         select case (foutyp(ifou))
         case ('x', 'r', 'R')
            ivar = ivar + 1
            if (foutyp(ifou) /= 'x') then
               fouvarnam(ivar) = "running_mean"//cref//"_max"
            else
               fouvarnam(ivar) = "fourier"//cref//"_max"
            end if
            fouvarnamlong(ivar) = "maximum value"
            if (index('ux|uy|uxa|uya', trim(founam(ifou))) == 0) then
               call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
            end if
            call setfoustandardname(founam(ifou), fouvarnamstd(ivar))
            if (founam(ifou) == 's1' .and. foutyp(ifou) == 'x' .and. .not. gdfourier%withTime(ifou)) then
               ivar = ivar + 1
               fouvarnam(ivar) = "fourier"//cref//"_max_depth"
               fouvarnamlong(ivar) = "maximum depth value"
               call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
               fouvarnamstd(ivar) = "sea_floor_depth_below_sea_surface"
            else if (foutyp(ifou) == 'R' .or. (foutyp(ifou) == 'x' .and. gdfourier%withTime(ifou))) then
               ivar = ivar + 1
               fouvarnam(ivar) = "maximum"//cref//"_time"
               fouvarnamlong(ivar) = "time of maximum value"
               fouvarunit(ivar) = trim(Tudunitstr)
               fouvarnamstd(ivar) = "time_max"
            end if
         case ('i', 'u', 'U')
            ivar = ivar + 1
            if (foutyp(ifou) /= 'i') then
               fouvarnam(ivar) = "running_mean"//cref//"_min"
            else
               fouvarnam(ivar) = "fourier"//cref//"_min"
            end if
            fouvarnamlong(ivar) = "minimum value"
            call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
            if (index('ux|uy|uxa|uya', trim(founam(ifou))) == 0) then
               call setfoustandardname(founam(ifou), fouvarnamstd(ivar))
            end if
            if (founam(ifou) == 's1' .and. foutyp(ifou) == 'i' .and. .not. gdfourier%withTime(ifou)) then
               ivar = ivar + 1
               fouvarnam(ivar) = "fourier"//cref//"_min_depth"
               fouvarnamlong(ivar) = "minimum depth value"
               call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
               fouvarnamstd(ivar) = "sea_floor_depth_below_sea_surface"
            else if (foutyp(ifou) == 'U' .or. (foutyp(ifou) == 'i' .and. gdfourier%withTime(ifou))) then
               ivar = ivar + 1
               fouvarnam(ivar) = "minimum"//cref//"_time"
               fouvarnamlong(ivar) = "time of minimum value"
               fouvarunit(ivar) = trim(Tudunitstr)
               fouvarnamstd(ivar) = "time_min"
            end if
         case ('c', 'C')
            ivar = ivar + 1
            fouvarnam(ivar) = "min_max_combined"//cref
            fouvarnamlong(ivar) = "combined"
            call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
            call setfoustandardname(founam(ifou), fouvarnamstd(ivar))
         case ('>')
            ivar = ivar + 1
            write (message, '(G12.3)') fthresh(ivar)
            call remove_leading_spaces(message)
            fouvarnam(ivar) = "time_"//cref//"_above"
            fouvarnamlong(ivar) = "cumulative time above "//trim(message)
            fouvarunit(ivar) = "s"
            fouvarnamstd(ivar) = ""
         case ('<')
            ivar = ivar + 1
            write (message, '(G12.3)') fthresh(ivar)
            call remove_leading_spaces(message)
            fouvarnam(ivar) = "time_"//cref//"_below"
            fouvarnamlong(ivar) = "cumulative time below "//trim(message)
            fouvarunit(ivar) = "s"
            fouvarnamstd(ivar) = ""
         case ('l')
            ivar = ivar + 1
            fouvarnam(ivar) = "last"//cref
            fouvarnamlong(ivar) = "last values"
            call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
            call setfoustandardname(founam(ifou), fouvarnamstd(ivar))
         case ('a')
            ivar = ivar + 1
            fouvarnam(ivar) = "average"//cref//"_avg"
            fouvarnamlong(ivar) = "average value"
            call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
            call setfoustandardname(founam(ifou), fouvarnamstd(ivar))
         case default
            if (fnumcy(ifou) == 0) then ! zero fourier mode without further notice means 'MEAN'
               ivar = ivar + 1
               fouvarnam(ivar) = "fourier"//cref//"_mean"
               fouvarnamlong(ivar) = "average value"
               call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
               call setfoustandardname(founam(ifou), fouvarnamstd(ivar))
            else ! non-zero fourier mode
               ivar = ivar + 1
               fouvarnam(ivar) = "fourier"//cref//"_amp"
               fouvarnamlong(ivar) = "Fourier amplitude"
               call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
               ivar = ivar + 1
               fouvarnam(ivar) = "fourier"//cref//"_phs"
               fouvarnamlong(ivar) = "Fourier phase"
               fouvarunit(ivar) = "degree"
            end if
         end select
         !
         if (foumask(ifou) == 1) then
            fouvarnam(ivar) = trim(fouvarnam(ivar))//"_inidryonly"
            fouvarnamlong(ivar) = trim(fouvarnamlong(ivar))//", initially dry points only"
         end if
      end do

      ! init fourier arrays
      ierr = 0
      do ifou = 1, nofou
         call getsizes(ifou, sizea, sizeb)
         select case (foutyp(ifou))
         case ('x', 'r', 'R', 'c')
            fillValue = -dmiss_minmax
         case ('i', 'u', 'U', 'C')
            fillValue = dmiss_minmax
         case default
            fillValue = 0.0_fp
         end select
         select case (foutyp(ifou))
         case ('x', 'i', 'r', 'u', 'c', 'C')
            if (ierr == 0) call realloc(fousma(ifou)%sdata, sizea, stat=ierr, fill=real(fillValue, sp))
            if (ierr == 0) call realloc(fousmb(ifou)%sdata, sizeb, stat=ierr, fill=real(fillValue, sp))
         case ('R', 'U')
            if (ierr == 0) call realloc(fousma(ifou)%sdata, sizea, stat=ierr, fill=real(fillValue, sp))
            if (ierr == 0) call realloc(fousmb(ifou)%sdata, sizeb, stat=ierr, fill=real(dmiss, sp))
         case default
            if (ierr == 0) call realloc(fousma(ifou)%rdata, sizea, stat=ierr, fill=fillValue)
            if (ierr == 0) call realloc(fousmb(ifou)%rdata, sizeb, stat=ierr, fill=fillValue)
         end select
      end do
      if (ierr /= 0) then
         msgbuf = 'allocation error in reafou'
         call err_flush()
      end if

      call count_fourier_variables()

      return
6666  continue ! abort fourier analysis, something went wrong
      msgbuf = 'Invalid input for Fourier analysis, line = '''//trim(line)//''''
      call warn_flush()
      msgbuf = 'Switching off fourier analysis......'
      call err_flush()
      nofou = 0
   end subroutine reafou

   function fillMetaRM(name, nd, rstart, rstop, ti_fou) result(RMmeta)
      character(len=*), intent(in) :: name
      integer, intent(in) :: nd
      real(kind=hp), intent(in) :: rstart, rstop, ti_fou
      type(TRunningMeanMeta) :: RMmeta ! function result

      RMmeta%fieldname = trim(name)
      RMmeta%nd = nd
      RMmeta%nx = name_dependent_size(name)
      RMmeta%tstart = rstart
      RMmeta%tstop = rstop
      RMmeta%ti_fou = ti_fou
   end function fillMetaRM

   !> helper routine to get the size of suma and sumb arrays
   subroutine getsizes(ifou, sizea, sizeb)
      integer, intent(in) :: ifou !< counter for fourier quantities
      integer, intent(out) :: sizea !< size of suma
      integer, intent(out) :: sizeb !< size of sumb

      sizea = name_dependent_size(trim(gdfourier%founam(ifou)))

      select case (gdfourier%foutyp(ifou))
      case ('a', 'l')
         ! avg and last: sumb is not used (in avg only for time, so 1 element)
         sizeb = 1
      case ('r', 'u')
         ! in running mean sumb is not used
         sizeb = 0
      case ('<', '>')
         ! when accumulating time sumb is not used
         sizeb = 0
      case ('R', 'U', 'c', 'C')
         ! in running mean with time, sumb is used for time
         ! and combined min/max, sumb is used for 2nd field
         sizeb = sizea
      case ('x', 'i')
         ! min and max: sumb is only used for waterdepth or time
         sizeb = merge(sizea, 1, gdfourier%founam(ifou) == 's1' .or. gdfourier%withTime(ifou))
      case default
         ! real fourier analyse: sumb has the same size as suma
         sizeb = sizea
      end select
   end subroutine getsizes

   function valid_combi(name1, name2) result(same_size)
      character(len=*), intent(in) :: name1, name2
      logical :: same_size
      integer :: size1, size2

      size1 = name_dependent_size(name1)
      size2 = name_dependent_size(name2)

      same_size = (size1 == size2)
   end function valid_combi

   !> helper routine to get the size of suma arrays
   function name_dependent_size(fourier_name) result(nmaxus)
      use m_flowgeom, only: ndx, lnx, ndx2d, ndxi ! actually from m_cell_geometry
      use m_flow, only: ndkx, lnkx
      character(len=*), intent(in) :: fourier_name !< name of the fourier quantity
      integer :: nmaxus !< function result: size of suma array

      ! The name of the variable for Fourier analysis fully specified the number of elements for the fourier loop
      select case (fourier_name)
      case ('s1', 'wl', 'wd', 'eh')
         nmaxus = ndx
      case ('ws', 'sul')
         nmaxus = lnx
      case ('u1', 'q1')
         nmaxus = lnkx
      case ('qxk')
         nmaxus = lnx
      case ('ta')
         nmaxus = ndx
      case ('ux', 'uy', 'uc')
         nmaxus = ndkx
      case ('uxa', 'uya')
         nmaxus = ndx
      case ('r1')
         nmaxus = ndkx
      case ('fb', 'wdog', 'vog')
         nmaxus = ndxi - ndx2d
      case default
         nmaxus = max(ndkx, lnkx)
      end select
   end function name_dependent_size

   subroutine setfouunit(founam, lsal, ltem, fconno, fouvarunit)
      !
      ! parameters
      character(*), intent(in) :: founam
      integer, intent(in) :: lsal
      integer, intent(in) :: ltem
      integer, intent(in) :: fconno
      character(*), intent(out) :: fouvarunit
      !
      ! body
      select case (founam)
      case ('s1', 'sul')
         fouvarunit = 'm'
      case ('ws', 'u1', 'ux', 'uy', 'uc', 'uxa', 'uya')
         fouvarunit = 'm s-1'
      case ('ta')
         fouvarunit = 'N m-2'
      case ('r1')
         if (fconno == ltem) then
            fouvarunit = 'degrees_Celsius'
         elseif (fconno == lsal) then
            fouvarunit = 'ppt'
         else
            fouvarunit = 'kg m-3'
         end if
      case ('q1')
         fouvarunit = 'm3 s-1'
      case default
         fouvarunit = ' '
      end select
   end subroutine setfouunit

   subroutine setfoustandardname(founam, foustdname)
      !
      ! parameters
      character(*), intent(in) :: founam
      character(*), intent(out) :: foustdname
      !
      ! body
      select case (founam)
      case ('s1')
         foustdname = 'sea_surface_height'
      case ('ws')
         foustdname = 'wind_speed'
      case ('uc')
         foustdname = 'sea_water_speed'
      case ('uxa', 'ux')
         foustdname = merge('sea_water_x_velocity       ', &
                            'eastward_sea_water_velocity', jsferic == 0)
      case ('uya', 'uy')
         foustdname = merge('sea_water_y_velocity        ', &
                            'northward_sea_water_velocity', jsferic == 0)
      case ('fb')
         foustdname = 'freeboard'
      case ('wdog')
         foustdname = 'waterdepth_on_ground'
      case ('vog')
         foustdname = 'volume_on_ground'
      case default
         foustdname = ' '
      end select
   end subroutine setfoustandardname

   !> performs fourier analysis i.e. computes suma and sumb
   !! - calculates MAX or MIN value
   subroutine fouana(ifou, time0, rarray, bl, dts, dtw, nfou, rarray2)
      use precision
      use m_flowgeom, only: kfs
      !
      implicit none
      !
      ! Global variables
      !
      integer, intent(in) :: ifou !<  Counter
      real(kind=fp), intent(in) :: time0 !<  Current time
      real(kind=fp), dimension(:), intent(in) :: rarray !<  Array for fourier analysis
      real(kind=fp), intent(in) :: dts !<  time step [s]
      real(kind=fp), intent(in) :: dtw !<  weight for time step
      real(kind=prec), dimension(:), intent(in) :: bl !<  bottom level
      integer, intent(inout) :: nfou !< counter for update fou quantities
      real(kind=fp), dimension(:), intent(in), optional :: rarray2 !<  Array for combined min/max
      !
      ! The following list of pointer parameters is used to point inside the gdfourier structure
      !
      real(kind=fp) :: ftmsto
      real(kind=fp) :: ftmstr
      real(kind=fp) :: foufas
      real(kind=fp) :: fthresh
      real(kind=fp), pointer :: fousma(:)
      real(kind=fp), pointer :: fousmb(:)
      real(kind=sp), pointer :: fousmas(:)
      real(kind=sp), pointer :: fousmbs(:)
      character(len=16), pointer :: founam
      integer :: nmaxus
      !
      ! Local variables
      !
      integer :: n ! Loop counter over NMAXUS
      real(kind=fp) :: angl, cosangl_dtw, sinangl_dtw
      !
   !! executable statements -------------------------------------------------------
      !
      ftmsto = gdfourier%ftmsto(ifou)
      ftmstr = gdfourier%ftmstr(ifou)
      foufas = gdfourier%foufas(ifou)
      fthresh = gdfourier%fthresh(ifou)
      fousma => gdfourier%fousma(ifou)%rdata
      fousmb => gdfourier%fousmb(ifou)%rdata
      fousmas => gdfourier%fousma(ifou)%sdata
      fousmbs => gdfourier%fousmb(ifou)%sdata
      founam => gdfourier%founam(ifou)

      ! Perform fourier analysis, every timestep as long as time0 value
      ! lies in requested time interval FTMSTR and FTMSTO
      !
      ! The name of the variable for Fourier analysis fully specified the number of elements for the fourier loop
      nmaxus = name_dependent_size(founam)
      if (comparereal(time0, ftmstr, tol_time) /= -1 .and. comparereal(time0, ftmsto, tol_time) /= 1) then
         nfou = nfou + 1
         select case (gdfourier%foutyp(ifou))
         case ('x')
            !
            ! Calculate MAX value
            !
            if (gdfourier%foumask(ifou) /= 0) then
               do n = 1, nmaxus
                  if (kfs(n) == 1 .and. initial_wet_mask(n) == 0) then
                     fousmas(n) = max(fousmas(n), real(rarray(n), kind=sp))
                  end if
               end do
            else
               do n = 1, nmaxus
                  fousmas(n) = max(fousmas(n), real(rarray(n), kind=sp))
               end do
            end if
            if (gdfourier%withTime(ifou)) then
               do n = 1, nmaxus
                  ! time of maximum
                  if (rarray(n) > fousmas(n)) fousmbs(n) = time0
               end do
            else if (founam == 's1') then
               do n = 1, nmaxus
                  ! waterdepth (fousmb)
                  fousmbs(n) = max(fousmbs(n), real(rarray(n) - bl(n), sp))
               end do
            else
            end if
         case ('i')
            !
            ! Calculate MIN value
            !
            do n = 1, nmaxus
               fousmas(n) = min(fousmas(n), real(rarray(n), sp))
            end do
            if (gdfourier%withTime(ifou)) then
               do n = 1, nmaxus
                  ! time of minimum
                  if (rarray(n) < fousmas(n)) fousmbs(n) = time0
               end do
            else
               if (founam == 's1') then
                  do n = 1, nmaxus
                     ! waterdepth (fousmb)
                     fousmbs(n) = min(fousmbs(n), real(rarray(n) - bl(n), sp))
                  end do
               end if
            end if
         case ('a', 'l')
            !
            ! Calculate AVG value
            !
            do n = 1, nmaxus
               fousma(n) = fousma(n) + dtw * rarray(n)
            end do
            fousmb(1) = fousmb(1) + dtw
         case ('<')
            !
            ! Accumulate time when condition is met
            !
            do n = 1, nmaxus
               if (rarray(n) < fthresh) then
                  fousma(n) = fousma(n) + dts
               end if
            end do
         case ('>')
            !
            ! Accumulate time when condition is met
            !
            do n = 1, nmaxus
               if (rarray(n) > fthresh) then
                  fousma(n) = fousma(n) + dts
               end if
            end do
         case ('c')
            !
            ! combined max value
            !
            if (.not. present(rarray2)) then
               msgbuf = 'rarray2 not given for combined max value'
               call err_flush()
            end if
            do n = 1, nmaxus
               if (rarray2(n) > fousmas(n)) then
                  fousmas(n) = rarray2(n)
                  fousmbs(n) = rarray(n)
               end if
            end do
         case ('C')
            !
            ! combined min value
            !
            if (.not. present(rarray2)) then
               msgbuf = 'rarray2 not given for combined min value'
               call err_flush()
            end if
            do n = 1, nmaxus
               if (rarray2(n) < fousmas(n)) then
                  fousmas(n) = rarray2(n)
                  fousmbs(n) = rarray(n)
               end if
            end do
         case ('r', 'u', 'R', 'U')
            call update_running_means(rarray, ifou, time0)
         case default
            !
            ! Calculate total for fourier analyse
            !
            angl = (time0 - ftmstr) * foufas
            cosangl_dtw = cos(angl) * dtw
            sinangl_dtw = sin(angl) * dtw
            do n = 1, nmaxus
               fousma(n) = fousma(n) + rarray(n) * cosangl_dtw
               fousmb(n) = fousmb(n) + rarray(n) * sinangl_dtw
            end do
         end select
      end if
   end subroutine fouana

   !> update min/max values that are based on a running mean
   subroutine update_running_means(rarray, ifou, time0)
      integer, intent(in) :: ifou !<  Counter
      real(kind=hp), intent(in) :: time0 !<  Current time
      real(kind=hp), intent(in) :: rarray(:) !<  Array for fourier analysis

      type(TRunningMean), pointer :: running
      real(kind=sp), pointer :: fousmas(:)
      real(kind=sp), pointer :: fousmbs(:)
      integer :: n, nmaxus
      logical :: ready2use
      real(kind=sp) :: sum2mean, nwminmax
      character :: foutyp
      character(len=:), allocatable :: founam

      fousmas => gdfourier%fousma(ifou)%sdata
      fousmbs => gdfourier%fousmb(ifou)%sdata
      running => gdfourier%fousma(ifou)%running
      founam = trim(gdfourier%founam(ifou))
      foutyp = gdfourier%foutyp(ifou)

      call update_runsum(running, rarray, ready2use, sum2mean, time0)
      if (ready2use) then
         nmaxus = name_dependent_size(founam)
         select case (foutyp)
         case ('r')
            do n = 1, nmaxus
               fousmas(n) = max(fousmas(n), sum2mean * running%runsum%state(n))
            end do
         case ('u')
            do n = 1, nmaxus
               fousmas(n) = min(fousmas(n), sum2mean * running%runsum%state(n))
            end do
         case ('R')
            do n = 1, nmaxus
               nwminmax = sum2mean * running%runsum%state(n)
               if (nwminmax > fousmas(n)) then
                  fousmas(n) = nwminmax
                  fousmbs(n) = time0 ! TODO : may take mean of times in buffer
               end if
            end do
         case ('U')
            do n = 1, nmaxus
               nwminmax = sum2mean * running%runsum%state(n)
               if (nwminmax < fousmas(n)) then
                  fousmas(n) = nwminmax
                  fousmbs(n) = time0 ! TODO : may take mean of times in buffer
               end if
            end do
         end select
      end if

   end subroutine update_running_means

!> - Checks if fourier analysis are requested
!!  and determines the number of variables for which a fourier analysis is requested
   subroutine fouini(lunfou, success, time_unit_user, time_unit_kernel)
   !!--declarations----------------------------------------------------------------
      use precision
      use unstruc_messages
      !
      implicit none
      !
      ! input / output variables
      !
      integer, intent(in) :: lunfou !<  Unit number for fourier input file
      character(len=*), intent(in) :: time_unit_user !<  time unit in mdu and fou-file
      character(len=*), intent(in) :: time_unit_kernel !<  time unit kernel
      logical :: success !<  Flag=True if no error is encountered

      !
      ! Local variables
      !
      integer :: icol !< column index
      integer :: icol1 !< next column index
      integer :: ncols !< number of columns
      character(len=300) :: line ! Used for format free reading 300 = 256 + a bit (field, =, ##, etc.)
      character(len=30), allocatable :: columns(:) ! each line is split into separate fields (columns)
      integer :: numcyc
      integer :: nofouvarstep
      integer :: number
      logical :: withMinMax
      logical :: withTime
      integer, pointer :: nofouvar
      !
   !! executable statements -------------------------------------------------------
      !
      time_unit_factor = 1.0
      select case (time_unit_user)
      case ('D')
         time_unit_factor = 3600.*24.
      case ('H')
         time_unit_factor = 3600.
      case ('M')
         time_unit_factor = 60.
      case ('S')
         time_unit_factor = 1.
      end select

      select case (time_unit_kernel)
      case ('D')
         time_unit_factor = time_unit_factor / (3600.*24.)
      case ('H')
         time_unit_factor = time_unit_factor / (3600.)
      case ('M')
         time_unit_factor = time_unit_factor / (60.)
      end select

      ! The user specified times in the .fou files need to be multiplied by the time_unit_factor, to correspond with the kernel times

      nofouvar => gdfourier%nofouvar
      ! initialisation
      !
      nofou = 0
      nofouvar = 0
      !
      ! cycle through file, while reading lines
      !
      ! -->
      !
10    continue
      read (lunfou, '(a)', end=999) line
      if (line(1:1) == '*' .or. line == ' ') goto 10
      !
      ! reset line in smaller case characters and define contents
      !
      call str_lower(line, 300)
      if (allocated(columns)) deallocate (columns)
      call strsplit(line, 1, columns, 1)
      ncols = size(columns)
      !
      ! test for continuation line
      !
      read (columns(4), *, err=999) numcyc

      withMinMax = .false.
      do icol = 7, ncols
         if (columns(icol) == 'max' .or. columns(icol) == 'min') then
            withMinMax = .true.
            exit
         end if
      end do

      icol1 = min(icol + 1, ncols)
      withTime = (columns(icol1) == 'time')
      if (withMinMax .and. withTime) then
         nofouvarstep = 2
      else if (numcyc == 0) then
         nofouvarstep = 1
      else
         nofouvarstep = 2
      end if

      if (columns(1) (1:2) == 'wl') then
         ! requested fourier analysis water-level
         nofou = nofou + 1
         if (withMinMax) then
            !
            ! max and min: also write max depth
            !
            nofouvar = nofouvar + 2
         else
            nofouvar = nofouvar + nofouvarstep
         end if
         !
         ! requested fourier analysis constituent
         !
      elseif (ReadConstituentNumber(columns(1), number)) then
         nofou = nofou + 1
         nofouvar = nofouvar + nofouvarstep
         !
         ! remaining quantities
         !
      else
         select case (columns(1))
         case ('ws', 'ct', 'ux', 'uy', 'uxa', 'uya', 'uc', 'cs', 'bs', 'fb', 'wdog', 'vog', 'q1', 'sul', 'wd', 'eh')
            nofou = nofou + 1
            nofouvar = nofouvar + nofouvarstep
         case default
            !
            ! requested fourier analysis undefined
            !
            msgbuf = 'Fourier analysis: variable keyword '''//trim(columns(1))//''' not recognized, ignored'
            call msg_flush()
         end select
      end if
      !
      goto 10
      !
      ! <-- next line
      !
      !
999   continue
      fileids%ncid = 0
      success = .true.
      rewind (lunfou)
   end subroutine fouini

   !> read constituent number from quantity name after 'c' (e.g. 'c2')
   !! return true if found 'c' at the start and a number > 0
   function ReadConstituentNumber(quantity, number) result(success)
      character(len=*), intent(in) :: quantity
      integer, intent(out) :: number
      logical :: success

      integer :: ierr

      number = -999
      success = .false.
      if (quantity(1:1) == 'c') then
         read (quantity(2:), *, iostat=ierr) number
         success = (ierr == 0 .and. number > 0)
      end if
   end function ReadConstituentNumber

   !> do the actual fourier and min/max update
   !! write to file after last update
   subroutine postpr_fourier(time0, dts)
      use precision, only: dp
      use m_transport, only: constituents
      use m_flowgeom, only: bl, lnx, bl_min
      use m_flow
      implicit none

      real(kind=fp), intent(in) :: time0 !< Current time [seconds]
      real(kind=fp), intent(in) :: dts !< Internal time step [seconds]

      ! NOTE: In DELFT3D depth is used, but bl is passed (positive bottomlevel). Defined direction is different

      !
      ! Perform analysis and write to Fourier file
      !
      integer :: ifou
      integer :: ierr
      integer :: n
      real(kind=fp), pointer :: fouwrt
      integer :: nfou ! counter for updated fou quantities
      character(len=16), dimension(:), pointer :: founam
      character(len=20) :: cnum ! string to hold a number
      character(len=20) :: cquant ! 'quantity' or 'quantities'
      real(kind=dp), pointer :: fieldptr1(:), fieldptr2(:)
      real(kind=dp), allocatable, target :: wmag(:) ! [m/s] wind magnitude    (m/s) at u point {"location": "edge", "shape": ["lnx"]}
      real(kind=fp) :: dtw

      fouwrt => gdfourier%fouwrt
      founam => gdfourier%founam
      dtw = dts / dt_user

      !Update fourier specific bl and s1 array, only if required
      if (kmx > 0 .and. (laytyp(1) == 1 .or. numtopsig > 0)) then
         bl_min = min(bl, bl_min) ! lowest bed level
         s1max = max(s1, s1max) ! highest water level
      end if

      if (gdfourier%iblws > 0) then
         allocate (wmag(lnx), stat=ierr)
         if (ierr /= 0) then
            msgbuf = 'Allocation error in postpr_fourier; Fourier disabled.'
            call err_flush()
            nofou = 0
         else
            do n = 1, lnx
               wmag(n) = hypot(wx(n), wy(n))
            end do
         end if
      end if

      if (nofou > 0) then
         nfou = 0
         do ifou = 1, nofou
            !
            ! Perform Fourier analysis, as long TIME0 lies in requested time interval FTMSTR and FTMSTO
            !
            ! Scalar type Fourier component
            !
            ! Get Fourier component pointer
            !
            call find_field_pointer(fieldptr1, founam(ifou))
            if (gdfourier%foutyp(ifou) == 'c' .or. gdfourier%foutyp(ifou) == 'C') then
               call find_field_pointer(fieldptr2, gdfourier%founamc(ifou))
               call fouana(ifou, time0, fieldptr1, bl, dts, dtw, nfou, fieldptr2)
            else
               call fouana(ifou, time0, fieldptr1, bl, dts, dtw, nfou)
            end if
         end do

         if (allocated(wmag)) deallocate (wmag)

         if (nfou > 0) then
            write (cnum, *) real(time0)
            cquant = merge('quantities', 'quantity  ', nfou > 1)
            write (msgbuf, '(a,i0,4a)') 'Updated ', nfou, ' Fourier/max/min/average ', trim(cquant), ' for t = ', adjustl(cnum)
            call dbg_flush()
         end if
         !
         ! Write results of fourier analysis to data file
         ! only once when all fourier periods are complete
         !
         if (comparereal(time0, fouwrt, tol_time) /= -1) then
            if (fileids%ncid == 0) then
               call wrfou()
               !
               ! clean up large fourier arrays
               !
               do ifou = 1, nofou
                  call cleanUpFousmaIfou(gdfourier%fousma(ifou))
                  call cleanUpFousmaIfou(gdfourier%fousmb(ifou))
               end do
               deallocate (gdfourier%fousma, gdfourier%fousmb)
               nullify (gdfourier%fousma)
               nullify (gdfourier%fousmb)
               nofou = 0 ! this stops calling postpr_fourier
            end if
         end if
      end if

   contains

      subroutine find_field_pointer(fieldptr, fieldname)
         use precision, only: dp
         use m_gettaus
         use m_gettauswave
         real(kind=dp), pointer :: fieldptr(:)
         character(len=*), intent(in) :: fieldname

         select case (fieldname)
         case ('s1', 'wl') ! water level
            fieldptr => s1
         case ('wd') ! water depth
            fieldptr => hs
         case ('eh') ! energy height
            fieldptr => eh
         case ('ws') ! absolute wind magnitude
            fieldptr => wmag
         case ('ux')
            fieldptr => ucx
         case ('uy')
            fieldptr => ucy
         case ('uxa')
            fieldptr => ucxq
         case ('uya')
            fieldptr => ucyq
         case ('uc') ! ucmag, velocity magnitude
            fieldptr => ucmag
         case ('r1')
            fieldptr => constituents(gdfourier%fconno(ifou), :)
         case ('ta')
            if (jawave == 0 .or. flowWithoutWaves) then
               call gettaus(1, 1)
            else
               call gettauswave(jawaveswartdelwaq)
            end if
            fieldptr => taus
         case ('fb')
            fieldptr => freeboard ! freeboard
         case ('wdog')
            fieldptr => hsOnGround ! waterdepth above ground
         case ('vog')
            fieldptr => volOnGround ! volume above ground
         case ('q1')
            fieldptr => q1
         case ('sul')
            fieldptr => sul
         case default
            fieldptr => null() ! Unknown FourierVariable exception
         end select

      end subroutine find_field_pointer

   end subroutine postpr_fourier

   !> helper function for clean up in postpr_fourier
   subroutine cleanUpFousmaIfou(fousmi)
      type(fdata), intent(inout) :: fousmi

      if (allocated(fousmi%rdata)) deallocate (fousmi%rdata)
      if (allocated(fousmi%sdata)) deallocate (fousmi%sdata)
      if (associated(fousmi%running)) then
         if (associated(fousmi%running%runsum)) then
            deallocate (fousmi%running%runsum)
            nullify (fousmi%running%runsum)
            deallocate (fousmi%running%RMmeta%fieldname)
         end if
      end if
   end subroutine cleanUpFousmaIfou

   !> - open fourier analysis output file
   !! - writes results of fourier analysis to output file
   !! - closes fourier analysis output file
   subroutine wrfou()
      use precision, only: fp
      use mathconsts, only: raddeg
      use netcdf
      use unstruc_netcdf
      use m_sferic, only: jsferic
      use m_transport, only: namcon => const_names
      use fm_location_types
      !
      implicit none
      !
      integer :: nofouvar
      character(len=1), dimension(:), pointer :: foutyp
      character(len=16), dimension(:), pointer :: founam
      character(len=50), dimension(:), pointer :: fouvarnam
      character(len=50), dimension(:), pointer :: fouvarnamstd
      character(len=50), dimension(:), pointer :: fouvarnamlong
      character(len=50), dimension(:), pointer :: fouvarunit
      integer, dimension(:, :), pointer :: fouref
      real(kind=fp), dimension(:), pointer :: foufas
      integer, dimension(:), pointer :: fnumcy
      integer, dimension(:, :), pointer :: idvar
      !
      ! Local variables
      !
      integer :: ierr
      integer :: ifou ! Local counter for fourier functions
      integer :: ivar ! Local counter for fourier functions
      real(kind=fp) :: freqnt ! Frequency in degrees per hour
      real(kind=fp) :: tfasto ! Stop time in minutes
      real(kind=fp) :: tfastr ! Start time in minutes
      character(len=:), allocatable :: namfun ! Local name for fourier function
      character(len=:), allocatable :: namfunlong ! Local name for fourier function, including reference to the line in the fourier input file
      integer, parameter :: imissval = -1
      integer :: unc_loc
      integer, allocatable :: all_unc_loc(:)
      logical :: is_min_max_avg
      character(len=:), allocatable :: analyseTypeShort
      character(len=:), allocatable :: analyseTypeLong
      character(len=:), allocatable :: analyseType

      integer, parameter :: REQUESTTYPE_DEFINE = 1
      integer, parameter :: REQUESTTYPE_WRITE = 2

      !
      !! executable statements -------------------------------------------------------
      !
      nofouvar = gdfourier%nofouvar
      foutyp => gdfourier%foutyp
      founam => gdfourier%founam
      fouvarnam => gdfourier%fouvarnam
      fouvarnamstd => gdfourier%fouvarnamstd
      fouvarnamlong => gdfourier%fouvarnamlong
      fouvarunit => gdfourier%fouvarunit
      fouref => gdfourier%fouref
      foufas => gdfourier%foufas
      fnumcy => gdfourier%fnumcy
      idvar => gdfourier%idvar

      !
      ierr = unc_create(trim(FouOutputFile), 0, fileids%ncid)
      if (ierr == NF90_NOERR) ierr = ug_addglobalatts(fileids%ncid, ug_meta_fm)
      if (ierr /= NF90_NOERR) goto 99

      ierr = unc_meta_add_user_defined(fileids%ncid)

      call unc_write_flowgeom_filepointer_ugrid(fileids%ncid, fileids%id_tsp, jafou=.true.)

      call realloc(all_unc_loc, nofou)
      !
      ifou = 1
      do ivar = 1, nofouvar
         if (fouvarnam(ivar) == ' ') cycle
         if (ifou < nofou) then
            if (fouref(ifou + 1, 2) <= ivar) then
               ifou = ifou + 1
            end if
         end if
         freqnt = foufas(ifou) * raddeg * 3600.0_fp
         tfastr = gdfourier%ftmstr(ifou) / 60.0_fp
         tfasto = gdfourier%ftmsto(ifou) / 60.0_fp
         !
         select case (founam(ifou))
         case ('s1')
            unc_loc = UNC_LOC_S
            namfun = 'water level'
         case ('eh')
            unc_loc = UNC_LOC_S
            namfun = 'energy height'
         case ('wd')
            unc_loc = UNC_LOC_S
            namfun = 'water depth'
         case ('ws')
            unc_loc = UNC_LOC_U
            namfun = 'wind speed'
         case ('ux')
            unc_loc = merge(UNC_LOC_S3D, UNC_LOC_S, kmx > 0)
            namfun = 'U-component of cell-centre velocity'
         case ('uy')
            unc_loc = merge(UNC_LOC_S3D, UNC_LOC_S, kmx > 0)
            namfun = 'V-component of cell-centre velocity'
         case ('uxa')
            unc_loc = UNC_LOC_S
            namfun = 'U-component velocity, column average'
         case ('uya')
            unc_loc = UNC_LOC_S
            namfun = 'V-component velocity, column average'
         case ('uc')
            unc_loc = merge(UNC_LOC_S3D, UNC_LOC_S, kmx > 0)
            namfun = 'velocity magnitude'
         case ('r1')
            unc_loc = merge(UNC_LOC_S3D, UNC_LOC_S, kmx > 0)
            namfun = trim(namcon(gdfourier%fconno(ifou)))
         case ('u1')
            unc_loc = UNC_LOC_U
            namfun = 'velocity'
         case ('qx')
            unc_loc = UNC_LOC_U
            namfun = 'unit discharge'
         case ('ta')
            unc_loc = UNC_LOC_S
            namfun = 'bed stress'
         case ('fb')
            unc_loc = UNC_LOC_S
            namfun = 'freeboard'
         case ('wdog')
            unc_loc = UNC_LOC_S
            namfun = 'waterdepth_on_ground'
         case ('vog')
            unc_loc = UNC_LOC_S
            namfun = 'volume_on_ground'
         case ('q1')
            unc_loc = merge(UNC_LOC_U3D, UNC_LOC_U, kmx > 0)
            namfun = 'discharge through flow link'
         case ('sul')
            unc_loc = UNC_LOC_U
            namfun = 'water level at flow link'
         end select

         is_min_max_avg = .true.
         select case (gdfourier%foutyp(ifou))
         case ('l')
            AnalyseTypeShort = 'Last '
            AnalyseTypeLong = AnalyseTypeShort
         case ('a')
            AnalyseTypeShort = 'Average '
            AnalyseTypeLong = AnalyseTypeShort
         case ('x')
            AnalyseTypeShort = 'Maximum '
            AnalyseTypeLong = AnalyseTypeShort
         case ('c')
            AnalyseTypeShort = 'Maximum '
            AnalyseTypeLong = 'Maximum based on '//trim(gdfourier%founamc(ifou))//'; '
         case ('r', 'R')
            AnalyseTypeShort = 'Maximum '
            AnalyseTypeLong = 'Maximum (Running Mean) '
         case ('i')
            AnalyseTypeShort = 'Minimum '
            AnalyseTypeLong = AnalyseTypeShort
         case ('C')
            AnalyseTypeShort = 'Minimum '
            AnalyseTypeLong = 'Minimum based on '//trim(gdfourier%founamc(ifou))//'; '
         case ('u', 'U')
            AnalyseTypeShort = 'Minimum '
            AnalyseTypeLong = 'Minimum (Running Mean) '
         case ('<', '>')
            AnalyseTypeShort = 'Time '
            AnalyseTypeLong = 'Time '
         case default
            is_min_max_avg = .false.
            AnalyseTypeShort = 'Fourier analysis '
            AnalyseTypeLong = AnalyseTypeShort
         end select

         all_unc_loc(ifou) = unc_loc
         namfunlong = fouref2string(fouref(ifou, 1))//": "//namfun
         !
         idvar(:, ivar) = imissval
         if (index(founam(ifou), 'fb') > 0 .or. index(founam(ifou), 'wdog') > 0 .or. index(founam(ifou), 'vog') > 0) then
            ! Freeboard, waterdepth on ground and volume on ground are only for 1D
            ierr = unc_def_var_map(fileids%ncid, fileids%id_tsp, idvar(:, ivar), NF90_DOUBLE, unc_loc, trim(fouvarnam(ivar)), trim(fouvarnamstd(ivar)), &
                                   AnalyseTypeShort//namfunlong//', '//trim(fouvarnamlong(ivar)), fouvarunit(ivar), is_timedep=0, which_meshdim=1)
         else
            ierr = unc_def_var_map(fileids%ncid, fileids%id_tsp, idvar(:, ivar), NF90_DOUBLE, unc_loc, trim(fouvarnam(ivar)), trim(fouvarnamstd(ivar)), &
                                   AnalyseTypeShort//namfunlong//', '//trim(fouvarnamlong(ivar)), fouvarunit(ivar), is_timedep=0)
         end if
         if (ierr == NF90_NOERR) ierr = unc_put_att(fileids%ncid, idvar(:, ivar), 'long_name', AnalyseTypeLong//namfunlong//', '//trim(fouvarnamlong(ivar)))
         if (ierr == NF90_NOERR) ierr = unc_put_att(fileids%ncid, idvar(:, ivar), 'units', fouvarunit(ivar))
         if (ierr == NF90_NOERR) ierr = unc_put_att(fileids%ncid, idvar(:, ivar), 'reference_date_in_yyyymmdd', irefdate)

         analyseType = merge('min_max', 'fourier', is_min_max_avg)
         if (ierr == NF90_NOERR) ierr = unc_put_att(fileids%ncid, idvar(:, ivar), 'starttime_'//analyseType//'_analysis_in_minutes_since_reference_date', tfastr)
         if (ierr == NF90_NOERR) ierr = unc_put_att(fileids%ncid, idvar(:, ivar), 'stoptime_'//analyseType//'_analysis_in_minutes_since_reference_date', tfasto)

         if (ierr == NF90_NOERR) ierr = unc_add_gridmapping_att(fileids%ncid, idvar(:, ivar), jsferic)
         if (.not. is_min_max_avg) then
            if (ierr == NF90_NOERR) ierr = unc_put_att(fileids%ncid, idvar(:, ivar), 'number_of_cycles', fnumcy(ifou))
            if (ierr == NF90_NOERR) ierr = unc_put_att(fileids%ncid, idvar(:, ivar), 'frequency_degrees_per_hour', freqnt)
         end if

         if (ierr /= NF90_NOERR) goto 99
         !
      end do
      !

      ! END DEFINITION MODE
      ierr = nf90_enddef(fileids%ncid)
99    continue
      if (ierr /= NF90_NOERR) then
         msgbuf = 'Error in defining the Fourier output file: '//nf90_strerror(ierr)
         call err_flush()
      end if

      ! START DATA MODE
      !
      ! Write requested fourier function output
      !
      do ifou = 1, nofou
         !
         unc_loc = all_unc_loc(ifou)
         !
         call wrfous(ifou, fileids, unc_loc)
      end do
      !
      ! Close fourier output file
      !
      ierr = nf90_close(fileids%ncid) ! ncid NOT set to zero, to avoid writing the fourier output file again.
      if (ierr /= NF90_NOERR) then
         msgbuf = 'Error in closing the Fourier output file: '//nf90_strerror(ierr)
         call err_flush()
      end if
   end subroutine wrfou

   !> - writes results of fourier analysis to output
   subroutine wrfous(ifou, fileids, iloc)
      use precision
      use netcdf
      use unstruc_netcdf
      !
      implicit none
      !
      ! Global variables
      !
      integer, intent(in) :: ifou !< Fourier counter
      type(t_unc_mapids), intent(in) :: fileids !< Set of file and variable ids for this file.
      integer, intent(in) :: iloc
      !
      ! Local variables
      !
      integer, dimension(:, :), pointer :: idvar
      real(kind=fp), dimension(:), pointer :: fousma
      real(kind=fp), dimension(:), pointer :: fousmb
      real(kind=sp), dimension(:), pointer :: fousmas
      real(kind=sp), dimension(:), pointer :: fousmbs

      integer :: nmaxus
      integer :: ierror
      integer :: fouvar
      integer :: sizeb
      integer :: n ! Loop counter over NMAXUS

      !
      !! executable statements -------------------------------------------------------
      !
      idvar => gdfourier%idvar
      fousma => gdfourier%fousma(ifou)%rdata
      fousmb => gdfourier%fousmb(ifou)%rdata
      fousmas => gdfourier%fousma(ifou)%sdata
      fousmbs => gdfourier%fousmb(ifou)%sdata

      !
      call getsizes(ifou, nmaxus, sizeb)

      !
      ! Write data for user defined dimensions, hence NMAXUS
      !
      fouvar = gdfourier%fouref(ifou, 2)
      select case (gdfourier%foutyp(ifou))
      case ('x', 'i')
         ! First for Maximum or Minimum
         if (gdfourier%founam(ifou) == 'fb' .or. gdfourier%founam(ifou) == 'wdog' .or. gdfourier%founam(ifou) == 'vog') then
            if (idvar(1, fouvar) > 0 .and. nmaxus > 0) then
               ierror = nf90_put_var(fileids%ncid, idvar(1, fouvar), fousmas, start=(/1, fileids%id_tsp%idx_curtime/))
            end if
         else
            ierror = unc_put_var_map(fileids%ncid, fileids%id_tsp, idvar(:, fouvar), iloc, fousmas)
            if (gdfourier%founam(ifou) == 's1' .or. gdfourier%withTime(ifou)) then
               ! min/max water depth
               ierror = unc_put_var_map(fileids%ncid, fileids%id_tsp, idvar(:, fouvar + 1), iloc, fousmbs)
            end if
         end if

      case ('<', '>')
         ! for time above/below threshold
         if (gdfourier%founam(ifou) == 'fb' .or. gdfourier%founam(ifou) == 'wdog' .or. gdfourier%founam(ifou) == 'vog') then
            if (idvar(1, fouvar) > 0 .and. nmaxus > 0) then
               ierror = nf90_put_var(fileids%ncid, idvar(1, fouvar), fousmas, start=(/1, fileids%id_tsp%idx_curtime/))
            end if
         else
            ierror = unc_put_var_map(fileids%ncid, fileids%id_tsp, idvar(:, fouvar), iloc, fousma)
         end if

      case ('a', 'l')
         ! For average
         if (fousmb(1) > 0d0) then
            do n = 1, nmaxus
               fousma(n) = fousma(n) / fousmb(1)
            end do
         else
            fousma = defaultd
         end if
         if (gdfourier%founam(ifou) == 'fb' .or. gdfourier%founam(ifou) == 'wdog' .or. gdfourier%founam(ifou) == 'vog') then
            if (idvar(1, fouvar) > 0 .and. nmaxus > 0) then
               ierror = nf90_put_var(fileids%ncid, idvar(1, fouvar), fousmas, start=(/1, fileids%id_tsp%idx_curtime/))
            end if
         else
            ierror = unc_put_var_map(fileids%ncid, fileids%id_tsp, idvar(:, fouvar), iloc, fousma)
         end if

      case ('r', 'u')
         ! Maximum or Minimum based on running mean
         call replace_dummy(fousmas)
         if (gdfourier%founam(ifou) == 'fb' .or. gdfourier%founam(ifou) == 'wdog' .or. gdfourier%founam(ifou) == 'vog') then
            if (idvar(1, fouvar) > 0 .and. nmaxus > 0) then
               ierror = nf90_put_var(fileids%ncid, idvar(1, fouvar), fousmas, start=(/1, fileids%id_tsp%idx_curtime/))
            end if
         else
            ierror = unc_put_var_map(fileids%ncid, fileids%id_tsp, idvar(:, fouvar), iloc, fousmas)
         end if
      case ('R', 'U')
         ! Maximum or Minimum based on running mean including time of maximum
         call replace_dummy(fousmas)
         if (gdfourier%founam(ifou) == 'fb' .or. gdfourier%founam(ifou) == 'wdog' .or. gdfourier%founam(ifou) == 'vog') then
            if (idvar(1, fouvar) > 0 .and. nmaxus > 0) then
               ierror = nf90_put_var(fileids%ncid, idvar(1, fouvar), fousmas, start=(/1, fileids%id_tsp%idx_curtime/))
            end if
            if (idvar(1, fouvar + 1) > 0 .and. nmaxus > 0) then
               ierror = nf90_put_var(fileids%ncid, idvar(1, fouvar + 1), fousmas, start=(/1, fileids%id_tsp%idx_curtime/))
            end if
         else
            ierror = unc_put_var_map(fileids%ncid, fileids%id_tsp, idvar(:, fouvar), iloc, fousmas)
            ierror = unc_put_var_map(fileids%ncid, fileids%id_tsp, idvar(:, fouvar + 1), iloc, fousmbs)
         end if

      case ('c', 'C')
         ! combined maximum or minimum
         call replace_dummy(fousmbs)
         if (gdfourier%founam(ifou) == 'fb' .or. gdfourier%founam(ifou) == 'wdog' .or. gdfourier%founam(ifou) == 'vog') then
            if (idvar(1, fouvar) > 0 .and. nmaxus > 0) then
               ierror = nf90_put_var(fileids%ncid, idvar(1, fouvar), fousmas, start=(/1, fileids%id_tsp%idx_curtime/))
            end if
         else
            ierror = unc_put_var_map(fileids%ncid, fileids%id_tsp, idvar(:, fouvar), iloc, fousmbs)
         end if
      case default
         ! Fourier
         !
         call fourier_final(ifou, nmaxus)
         if (gdfourier%founam(ifou) == 'fb' .or. gdfourier%founam(ifou) == 'wdog' .or. gdfourier%founam(ifou) == 'vog') then
            if (idvar(1, fouvar) > 0 .and. nmaxus > 0) then
               ierror = nf90_put_var(fileids%ncid, idvar(1, fouvar), fousmas, start=(/1, fileids%id_tsp%idx_curtime/))
            end if
            if (idvar(1, fouvar + 1) > 0 .and. nmaxus > 0) then
               ierror = nf90_put_var(fileids%ncid, idvar(1, fouvar + 1), fousmas, start=(/1, fileids%id_tsp%idx_curtime/))
            end if
         else
            ierror = unc_put_var_map(fileids%ncid, fileids%id_tsp, idvar(:, fouvar), iloc, fousma)
            ierror = unc_put_var_map(fileids%ncid, fileids%id_tsp, idvar(:, fouvar + 1), iloc, fousmb)
         end if
      end select
   end subroutine wrfous

   !> replace initial value by missing value
   subroutine replace_dummy(field)
      real, intent(inout) :: field(:)
      integer :: i

      do i = 1, size(field)
         if (abs(field(i)) == real(dmiss_minmax, sp)) then
            field(i) = dmiss
         end if
      end do

   end subroutine replace_dummy

   !> final update of fousma and fousmb
   subroutine fourier_final(ifou, nmaxus)
      use mathconsts, only: raddeg
      integer, intent(in) :: ifou !< Fourier counter
      integer, intent(in) :: nmaxus !< dimension of current quantity
      real(kind=fp) :: freqnt !< Frequency in degrees per hour
      real(kind=fp) :: shift !< Phase shift
      integer :: n !< loop counter
      logical :: ltest !< Help variable for atan2 function test
      real(kind=fp) :: amp !< Fourier amplitude
      real(kind=fp) :: fas !< Fourier phase
      real(kind=fp) :: wdt !< weight factor for time interval
      real(kind=fp) :: fas_term !< term to add to phase
      integer :: fnumcy
      real(kind=fp) :: ftmsto
      real(kind=fp) :: ftmstr
      real(kind=fp) :: fknfac
      real(kind=fp) :: foufas
      real(kind=fp), pointer :: fousma(:)
      real(kind=fp), pointer :: fousmb(:)
      real(kind=fp) :: fv0pu
      !
   !! executable statements -------------------------------------------------------
      !
      fnumcy = gdfourier%fnumcy(ifou)
      ftmsto = gdfourier%ftmsto(ifou)
      ftmstr = gdfourier%ftmstr(ifou)
      fknfac = gdfourier%fknfac(ifou)
      foufas = gdfourier%foufas(ifou)
      fousma => gdfourier%fousma(ifou)%rdata
      fousmb => gdfourier%fousmb(ifou)%rdata
      fv0pu = gdfourier%fv0pu(ifou)

      ! Frequention := 360 degree / period
      ! where period = [ (FTMSTO - FMTSTR) ] / [ FNUMCY * 3600 ]
      ! FOUFAS is defined in RDFOUR as
      ! FOUFAS =  2 * PI * FNUMCY / [(FTMSTO - FMTSTR) ]
      ! so FREQNT = FOUFAS * RADDEG * 3600 is OK
      !
      shift = ftmstr * foufas
      freqnt = foufas * raddeg * 3600.0_fp
      wdt = 2.0_fp * dt_user / (ftmsto - ftmstr)
      fas_term = fv0pu - tzone * freqnt
      do n = 1, nmaxus
         ltest = (fousma(n) == 0.0_fp .and. fousmb(n) == 0.0_fp)
         !
         ! Test for active point and non-zero values
         !
         if (.not. ltest) then
            fousma(n) = fousma(n) * wdt
            fousmb(n) = fousmb(n) * wdt
            amp = hypot(fousma(n), fousmb(n))
            fas = atan2(fousmb(n), fousma(n)) + shift
            if (fnumcy == 0) then
               amp = 0.5_fp * amp * cos(fas)
               fas = 0.0_fp
            end if
            !
            ! Timezone correction added timezone*phase [degrees/hr].
            ! foufas       is in [rad/timestep]
            ! halftimestep is in [sec/timestep]
            ! => timezonecorr = tzone [-] * foufas [rad] * raddeg [deg/rad] * [sec/hr]
            !
            fas = fas * raddeg + fas_term
            !
            ! To define FAS between 0 and 360. add 720. to the MOD of
            ! FAS and re-use the MOD function
            !
            fousmb(n) = mod(mod(fas, 360.0_fp) + 720.0_fp, 360.0_fp)
            fousma(n) = amp / fknfac
         else
            !
            ! Inactive point (not inside grid, can be open boundary)
            !
            fousma(n) = defaultd
            fousmb(n) = defaultd
         end if
      end do
   end subroutine fourier_final

   !> convert a fourier variable reference number into a string
   function fouref2string(ifourier) result(string)

      integer, intent(in) :: ifourier !< reference number of fourier variable

      character(len=10) :: string10
      character(len=:), allocatable :: string

      if (ifourier < 1000) then
         write (string10, '(i3.3)') ifourier
      else
         write (string10, '(i0)') ifourier
      end if

      string = trim(string10)

   end function fouref2string

end module m_fourier_analysis
