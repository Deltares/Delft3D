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

module m_itdate
   use precision, only: dp
   implicit none
   private

   character(len=8), public :: refdat
   integer, public :: itdate !< should be user specified for (asc routines)
   integer, public :: jul0, imonth0, iday0, iyear0
   real(kind=dp), public :: Tzone ! doubling with "use m_flowtimes, only : tzone"
end module m_itdate

! ==========================================================================

!>
module timespace_read
!!--description-----------------------------------------------------------------
!
!!--pseudo code and references--------------------------------------------------
!
!!--declarations----------------------------------------------------------------
   use precision, only: dp
   implicit none

   integer, parameter :: maxnamelen = 256
   real(kind=dp), parameter :: dmiss_default = -999.0_dp ! Default missing value in meteo arrays
   real(kind=dp), parameter :: xymiss = -999.0_dp ! Default missing value in elementset
   character(300), target :: errormessage = ' ' ! When an error occurs, a message is set in message.
   ! function getmeteoerror returns the message

   real(kind=dp) :: pi ! pi
   real(kind=dp) :: d2r ! degrees to radials
   real(kind=dp) :: r2d ! degrees to radials
   real(kind=dp), private, parameter :: earthrad = 6378137.0_dp ! Mathworld, IUGG

contains
   !
   !
   ! ==========================================================================
   !>
   !> Parses an UDUnit-conventions datetime unit string.
   !! TODO: replace this by calling C-API from UDUnits(-2).
   function parse_ud_timeunit(timeunitstr, iunit, iyear, imonth, iday, ihour, imin, isec) result(ierr)
      character(len=*), intent(in) :: timeunitstr !< Time unit by UDUnits conventions, e.g. 'seconds since 2012-01-01 00:00:00.0 +0000'.
      integer, intent(out) :: iunit !< Unit in seconds, i.e. 'hours since..' has iunit=3600.
      integer, intent(out) :: iyear !< Year in reference datetime.
      integer, intent(out) :: imonth !< Month in reference datetime.
      integer, intent(out) :: iday !< Day in reference datetime.
      integer, intent(out) :: ihour !< Hour in reference datetime.
      integer, intent(out) :: imin !< Minute in reference datetime.
      integer, intent(out) :: isec !< Seconds in reference datetime.
      integer :: ierr !< Error status, only 0 when successful.

      integer :: i, n, ifound, iostat
      character(len=7) :: unitstr

      ierr = 0
      unitstr = ' '

      n = len_trim(timeunitstr)
      ifound = 0
      do i = 1, n
         if (timeunitstr(i:i) == ' ') then ! First space found
            if (timeunitstr(i + 1:min(n, i + 5)) == 'since') then
               unitstr = timeunitstr(1:i - 1)
               ifound = 1
            else
               ierr = 1
            end if
            exit ! Found or error, look no further.
         end if
      end do

      if (ifound == 1) then
         select case (trim(unitstr))
         case ('seconds')
            iunit = 1
         case ('minutes')
            iunit = 60
         case ('hours')
            iunit = 3600
         case ('days')
            iunit = 86400
         case ('weeks')
            iunit = 604800
         case default
            iunit = -1
         end select

         read (timeunitstr(i + 7:n), '(I4,1x,I2,1x,I2,1x,I2,1x,I2,1x,I2)', iostat=iostat) iyear, imonth, iday, ihour, imin, isec

      end if
   end function parse_ud_timeunit
end module timespace_read
!
!
!
! ==========================================================================
! ==========================================================================
! ==========================================================================
!>
!> Deze module doet ruimte/tijdinterpolatie
!! Voor een gegeven quantity met ruimtedefinitie in een elementset,
!! worden de bijdragen van alle dataproviders aan die quantity gesommeerd.
!! Hierbij heeft iedere dataprovider een eigen tijd/ruimtedefinitie.
!! Zitten meerdere quantities of dezelfde tijd/ruimtedefinitie dan hoeft de tijd/ruimteinterpolatie
!! maar 1 keer uitgevoerd te worden.
!! De gevraagde grootheid moet dan niet als scalair maar als vector aangeboden worden.
module timespace_data
!!--description-----------------------------------------------------------------
!
!!--pseudo code and references--------------------------------------------------
!
!!--declarations----------------------------------------------------------------
   use precision
   use timespace_read
   use timespace_parameters
   implicit none

   real(kind=dp) :: timelast = -1e10_dp ! time of most recent value requested
   ! if time =< timelast, no updates

   real(kind=dp) :: t01ini = -1e10_dp ! initial time for dataproviders t0 and t1 fields

   ! AvD: NOTE
   ! De pointers in alle onderstaande types worden puur gebruikt om dynamisch
   ! te kunnen alloceren. In Fortran 95 mag je namelijk geen allocatables in
   ! user-defined types opnemen. In Fortran 2003 mag dit wel, dus waarom
   ! binnenkort niet overstappen?
   ! Naar allocatables mag je ook pointeren (xyen => provider%xyen), en verder
   ! gebruiken we uberhaupt geen pointer(omleg-)functionaliteit. Performance
   ! schijnt ook slechter te zijn van pointers.
   ! allocables hoef je ook niet te nullifyen om de allocated check te laten
   ! slagen. Dit geldt wel voor de associated check van pointers.

contains
   !
   !
   ! ==========================================================================
   !>
   !> Read the next quantity block that is found in a file.
   !! The (external forcing) file is opened elsewhere and read block-by-block
   !! by consecutive calls to this routine.
   subroutine readprovider(minp, qid, filename, filetype, method, operand, transformcoef, ja, varname, smask, maxSearchRadius)
      use fm_external_forcings_data, only: NTRANSFORMCOEF
      use MessageHandling, only: LEVEL_WARN, LEVEL_INFO, mess
      use m_qnerror
      use m_filez, only: readandchecknextrecord, readerror, zoekja, zoekopt

      integer, intent(in) :: minp !< File handle to already opened input file.
      integer, intent(out) :: filetype !< File type of current quantity.
      integer, intent(out) :: method !< Time-interpolation method for current quantity.
      character(len=*), intent(out) :: filename !< Name of data file for current quantity.
      character(len=*), intent(out) :: qid !< Identifier of current quantity (i.e., 'waterlevelbnd')
      character(len=1), intent(out) :: operand !< Operand w.r.t. previous data ('O'verride or '+'Append)
      real(kind=dp), intent(out) :: transformcoef(:) !< Transformation coefficients
      integer, intent(out) :: ja !< Whether a block was successfully read or not.
      character(len=*), intent(out) :: varname !< variable name within filename; only in case of NetCDF
      character(len=*), intent(out), optional :: smask !< Name of mask-file applied to source arcinfo meteo-data
      real(kind=dp), intent(out), optional :: maxSearchRadius !< max search radius for method == 11

      ! locals
      character(len=maxnamelen) :: rec, keywrd
      integer :: l1, l2, jaopt, k, extrapolation
      logical, save :: alreadyPrinted = .false. !< flag to avoid printing the same message many times

      integer, parameter :: NUMGENERALKEYWRD_OLD = 26
      character(len=256) :: generalkeywrd_old(NUMGENERALKEYWRD_OLD) = [character(len=256) :: &
                                                                        'widthleftW1', & ! ( 1)
                                                                        'levelleftZb1', & ! ( 2)
                                                                        'widthleftWsdl', & ! ( 3)
                                                                        'levelleftZbsl', & ! ( 4)
                                                                        'widthcenter', & ! ( 5)
                                                                        'levelcenter', & ! ( 6)
                                                                        'widthrightWsdr', & ! ( 7)
                                                                        'levelrightZbsr', & ! ( 8)
                                                                        'widthrightW2', & ! ( 9)
                                                                        'levelrightZb2', & ! (10)
                                                                        'gateheight', & ! (11)
                                                                        'gateheightintervalcntrl', & ! (12)
                                                                        'pos_freegateflowcoeff', & ! (13)
                                                                        'pos_drowngateflowcoeff', & ! (14)
                                                                        'pos_freeweirflowcoeff', & ! (15)
                                                                        'pos_drownweirflowcoeff', & ! (16)
                                                                        'pos_contrcoeffreegate', & ! (17)
                                                                        'neg_freegateflowcoeff', & ! (18)
                                                                        'neg_drowngateflowcoeff', & ! (19)
                                                                        'neg_freeweirflowcoeff', & ! (20)
                                                                        'neg_drownweirflowcoeff', & ! (21)
                                                                        'neg_contrcoeffreegate', & ! (22)
                                                                        'extraresistance', & ! (23)
                                                                        'dynstructext', & ! (24)
                                                                        'gatedoorheight', & ! (25)
                                                                        'door_opening_width' & ! (26)
                                                                        ]

      if (minp == 0) then
         ja = 0
         return
      end if

      keywrd = 'QUANTITY'
      call zoekja(minp, rec, keywrd, ja)
      if (ja == 1) then
         l1 = index(rec, '=') + 1
         call checkForSpacesInProvider(rec, l1, l2) ! l2 = l1 + #spaces after the equal-sign
         read (rec(l2:), '(a)', err=990) qid
      else
         return
      end if

      keywrd = 'FILENAME'
      call zoekja(minp, rec, keywrd, ja)
      if (ja == 1) then
         l1 = index(rec, '=') + 1
         call checkForSpacesInProvider(rec, l1, l2) ! l2 = l1 + #spaces after the equal-sign
         read (rec(l2:), '(a)', err=990) filename
      else
         return
      end if

      keywrd = 'VARNAME'
      call zoekopt(minp, rec, keywrd, jaopt)
      if (jaopt == 1) then
         varname = adjustl(rec)
      else
         varname = ' '
      end if

      if (present(smask)) then ! todo: shouldn't this argument be compulsory ? .....
         keywrd = 'SOURCEMASK'
         call zoekopt(minp, rec, trim(keywrd), jaopt)
         if (jaopt == 1) then
            read (rec, *) smask
         else
            smask = ''
         end if
      end if

      keywrd = 'FILETYPE'
      call zoekja(minp, rec, keywrd, ja)
      if (ja == 1) then
         l1 = index(rec, '=') + 1
         call checkForSpacesInProvider(rec, l1, l2) ! l2 = l1 + #spaces after the equal-sign
         read (rec(l2:), *, err=990) filetype
      else
         return
      end if

      keywrd = 'METHOD'
      method = spaceandtime ! default : spaceandtime
      call zoekja(minp, rec, keywrd, ja)
      if (ja == 1) then
         l1 = index(rec, '=') + 1
         call checkForSpacesInProvider(rec, l1, l2) ! l2 = l1 + #spaces after the equal-sign
         read (rec(l2:), *, err=990) method
      else
         return
      end if

      if (method == 11) then
         if (.not. alreadyPrinted) then
            call mess(LEVEL_INFO, 'METHOD=11 is obsolete; use METHOD=3 and EXTRAPOLATION_METHOD=1')
            alreadyPrinted = .true.
         end if
         method = 100 + weightfactors
      else
         keywrd = 'EXTRAPOLATION_METHOD'
         call zoekopt(minp, rec, keywrd, jaopt)
         if (jaopt == 1) then
            read (rec, *, err=990) extrapolation
            method = method + 100 * extrapolation
         end if
      end if

      if (present(maxSearchRadius)) then
         keywrd = 'MAXSEARCHRADIUS'
         call zoekopt(minp, rec, keywrd, jaopt)
         if (jaopt == 1) then
            read (rec, *, err=990) maxSearchRadius
         else
            maxSearchRadius = -1.0_hp
         end if
      end if

      keywrd = 'OPERAND'
      OPERAND = 'O' ! hk : default =O
      call zoekja(minp, rec, keywrd, ja)
      if (ja == 1) then
         l1 = index(rec, '=') + 1
         call checkForSpacesInProvider(rec, l1, l2) ! l2 = l1 + #spaces after the equal-sign
         read (rec(l2:l2), '(a1)', err=990) operand
      else
         return
      end if

      call readTransformcoefficients(minp, transformcoef)

      if (qid == 'generalstructure') then
         call mess(LEVEL_WARN, 'Keyword [generalstructure] is not supported in the external forcing file. &
 &                               Please use a structure file <*.ini> instead.')
         if (NUMGENERALKEYWRD_OLD < NTRANSFORMCOEF) then
            call mess(LEVEL_WARN, 'Not all expected keywords are provided.')
         end if
         if (NUMGENERALKEYWRD_OLD > NTRANSFORMCOEF) then
            call mess(LEVEL_WARN, 'More keywords provided than expected.')
         end if
         do k = 1, NUMGENERALKEYWRD_OLD
            call readandchecknextrecord(minp, rec, generalkeywrd_old(k), jaopt)
            if (jaopt == 1) then
               L1 = index(rec, '=') + 1
               read (rec(L1:), *) transformcoef(k)
            else
               call qnerror('Looking for '//trim(generalkeywrd_old(k))//', but getting ', rec, ' ')
            end if
         end do
      end if

      return

990   call readerror('reading '//trim(keywrd)//' but getting ', rec, minp)

   end subroutine readprovider
   !
   subroutine readTransformcoefficients(minp, transformcoef)
      use m_filez, only: readerror, zoekopt

      integer, intent(in) :: minp
      real(kind=dp), intent(out) :: transformcoef(:)

      type tKeyInt
         character(len=32) :: key
         integer :: value
      end type tKeyInt

      character(len=maxnamelen) :: rec
      integer :: jaopt, i, ierr
      type(tKeyInt) :: pairs(21)

      ! constant keywrd = 'DISCHARGE'/'SALINITY'/'TEMPERATURE' removed, now always via time series, in future also via new ext [discharge]

      transformcoef = -999.0_dp

      pairs(1)%key = 'VALUE'
      pairs(1)%value = 1
      pairs(2)%key = 'FACTOR'
      pairs(2)%value = 2
      pairs(3)%key = 'LAYER'
      pairs(3)%value = 3
      pairs(4)%key = 'IFRCTYP'
      pairs(4)%value = 3
      pairs(5)%key = 'AVERAGINGTYPE'
      pairs(5)%value = 4
      pairs(6)%key = 'TRACERFALLVELOCITY'
      pairs(6)%value = 24
      pairs(7)%key = 'TRACERDECAYTIME'
      pairs(7)%value = 25
      pairs(8)%key = 'RELATIVESEARCHCELLSIZE'
      pairs(8)%value = 5
      pairs(9)%key = 'EXTRAPOLTOL'
      pairs(9)%value = 6
      pairs(10)%key = 'PERCENTILEMINMAX'
      pairs(10)%value = 7
      pairs(11)%key = 'AREA' ! Area for source-sink pipe
      pairs(11)%value = 4
      pairs(12)%key = 'TREF' ! relaxation time for riemann boundary
      pairs(12)%value = 7
      pairs(13)%key = 'NUMMIN' ! minimum number of points in averaging
      pairs(13)%value = 8
      pairs(14)%key = 'startlevelsuctionside'
      pairs(14)%value = 4
      pairs(15)%key = 'stoplevelsuctionside'
      pairs(15)%value = 5
      pairs(16)%key = 'startleveldeliveryside'
      pairs(16)%value = 6
      pairs(17)%key = 'stopleveldeliveryside'
      pairs(17)%value = 7
      pairs(18)%key = 'UNIFORMSALINITYABOVEZ'
      pairs(18)%value = 3
      pairs(19)%key = 'UNIFORMSALINITYBELOWZ'
      pairs(19)%value = 4
      pairs(20)%key = 'UNIFORMVALUEABOVEZ'
      pairs(20)%value = 13
      pairs(21)%key = 'UNIFORMVALUEBELOWZ'
      pairs(21)%value = 14

      do i = 1, size(pairs)
         call zoekopt(minp, rec, trim(pairs(i)%key), jaopt)
         if (jaopt == 1) then
            read (rec, *, iostat=ierr) transformcoef(pairs(i)%value)
            if (ierr /= 0) then
               call readerror('reading '//trim(pairs(i)%key)//' but getting ', rec, minp)
            end if
         end if
      end do

   end subroutine readTransformcoefficients

   !
   ! ==========================================================================
   !>
   subroutine checkForSpacesInProvider(rec, eqsign, eqsignsp)
      ! I/O
      character(len=256), intent(in) :: rec !< Name of record that includes the keyword and record
      integer, intent(in) :: eqsign !< Location of the equal-sign in the entire record string
      integer, intent(out) :: eqsignsp !< Location of the equal-sign plus first spaces after equal-sign

      ! Locals
      integer :: i ! Counter

      eqsignsp = eqsign
      do i = 0, 256 - eqsign
         if (rec(eqsign + i:eqsign + i) == ' ') then
            eqsignsp = eqsignsp + 1
         else
            exit
         end if
      end do
   end subroutine checkForSpacesInProvider
   !
   !
   ! ==========================================================================
   !> Reads a single polyline from an open file.
   !! Assumes two-column data with x,y pairs.
   subroutine read1polylin(minp, xs, ys, ns, pliname, has_more_records)
      use m_alloc
      use m_filez, only: readerror, doclose, eoferror

      integer, intent(inout) :: minp !< Unit number of poly file (already opened), will be closed after successful read.
      real(kind=dp), allocatable, intent(out) :: xs(:) !< x-coordinates read from file
      real(kind=dp), allocatable, intent(out) :: ys(:) !< y-coordinates read from file
      integer, intent(out) :: ns !< Number of pli-points read
      character(len=:), allocatable, optional, intent(out) :: pliname !< (Optional) Name (identifier) of the polyline read
      logical, optional, intent(out) :: has_more_records !< (Optional) Whether or not more polyline data exists in the remainder of the file, after reading this one polyline.

      character(len=maxnamelen) :: rec
      integer :: k

      ns = 0

      if (present(has_more_records)) then
         has_more_records = .false.
      end if

10    read (minp, '(a)', end=999) rec
      if (rec(1:1) == '*') then
         goto 10
      end if
      if (present(pliname)) then
         pliname = trim(rec)
      end if

      read (minp, '(a)', end=999) rec
      read (rec, *, err=888) ns

      if (.not. allocated(xs)) then
         allocate (xs(ns))
      else if (ns > size(xs)) then
         call realloc(xs, ns, keepExisting=.false.)
      end if

      if (.not. allocated(ys)) then
         allocate (ys(ns))
      else if (ns > size(ys)) then
         call realloc(ys, ns, keepExisting=.false.)
      end if

      do k = 1, ns
         read (minp, '(a)', end=999) rec
         read (rec, *, err=777) xs(k), ys(k)
      end do

      if (present(has_more_records)) then
         has_more_records = polyfile_has_more_records(minp)
      end if

      call doclose(minp)

      return

999   call eoferror(minp)

      return

888   call readerror('reading nrows but getting ', rec, minp)
      return

777   call readerror('reading x, y  but getting ', rec, minp)
      return

   end subroutine read1polylin

   !> Determine whether there is more data still left in the open polyfile.
   !! Returns .true. when more non-whitepace/non-comment lines exist beyond
   !! the current file pointer position.
   !! After checking, 'backspaces' the filepointer to the first new data position.
   function polyfile_has_more_records(minp) result(has_more)
      integer, intent(inout) :: minp !< Unit number of poly file, already open, filepointer can be anywhere in the file.
      logical :: has_more !< Result, whether or not more polyline data may exist in the remainder of the file.

      character(len=maxnamelen) :: rec

      has_more = .false.

      do
         read (minp, '(a)', end=999) rec
         if (rec(1:1) == '*') then
            cycle
         end if

         if (len_trim(rec) == 0) then
            cycle
         else
            ! We encountered a non-comment line, non-whitespace line before EOF
            has_more = .true.
            backspace (minp)
            exit
         end if
      end do

      return

999   continue
      ! EOF reached, simply return (.false.)
      return

   end function polyfile_has_more_records
   !
   !
   ! ==========================================================================
   !>
   subroutine settimespacerefdat(refda, jul00, tz, timjan)
      use m_itdate
      use m_julday
      character(len=8) :: refda
      integer :: jul00
      real(kind=dp) :: tz, timjan

      integer :: juljan

      refdat = refda
      read (refdat, *) itdate

      read (refdat(1:4), *) iyear0
      read (refdat(5:6), *) imonth0
      read (refdat(7:8), *) iday0

      jul0 = julday(imonth0, iday0, iyear0)
      jul00 = jul0

      Tzone = tz

      juljan = julday(1, 1, iyear0)
      timjan = (jul0 - juljan) * 24.0_dp

   end subroutine settimespacerefdat
   !
   !
   ! ==========================================================================
   !>
   function getmeteoerror() result(retval)
      implicit none
      character(300), pointer :: retval
      retval => errormessage
   end function getmeteoerror
   !
   !
   ! ==========================================================================
   !>
   subroutine meteo_tidepotential(jul0, TIME, dstart, dstop, eps)
      use m_sferic
      use m_flowparameters, only: jatidep, jaselfal, jamaptidep
      use m_partitioninfo
      use m_flow
      use m_flowgeom
      integer :: jul0 ! interpolate results in ndx
      integer :: Np !< number of potentials in tidep

      real(kind=dp) :: time, dstart, dstop, eps, dxx, dyy
      real(kind=dp) :: xx(4), yy(4) !, DAREA, DLENGTH, DLENMX

      real(kind=dp), allocatable, save :: xz2(:, :), yz2(:, :), td2(:, :), self(:, :), avhs(:, :) !, area(:,:)
      real(kind=dp) :: xmn, xmx, ymn, ymx, di, dj, f11, f21, f12, f22

      real(kind=dp), allocatable, save :: td2_x(:, :), td2_y(:, :)

      integer :: i, j, n, ierr, m1, m2, n1, n2, L
      integer, save :: ndx2
      integer, save :: i1
      integer, save :: i2
      integer, save :: j1
      integer, save :: j2
      integer, save :: INI = 0

      np = size(tidep, 1)

      if (INI == 0) then
         INI = 1

         XMN = 1e30_dp
         YMN = 1e30_dp
         XMX = -1e30_dp
         YMX = -1e30_dp
         do I = 1, ndx
            xmn = min(xz(i), xmn)
            xmx = max(xz(i), xmx)
            ymn = min(yz(i), ymn)
            ymx = max(yz(i), ymx)
         end do

         i1 = floor(xmn)
         i2 = floor(xmx) + 1
         j1 = floor(ymn)
         j2 = floor(ymx) + 1
         if (jatidep == 2) then ! gradient intp., one extra
            i1 = i1 - 1
            i2 = i2 + 1
            j1 = j1 - 1
            j2 = j2 + 1
         end if

         if (jaselfal == 1 .and. jampi == 1) then
!        globally reduce i1, i2, j1, j2
            i1 = -i1
            j1 = -j1
            call reduce_int4_max(i1, i2, j1, j2)
            i1 = -i1
            j1 = -j1
         end if

         if (allocated(XZ2)) then
            deallocate (XZ2, YZ2, TD2)
         end if
         allocate (xz2(i1:i2, j1:j2), stat=ierr) ! tot aerr
         allocate (yz2(i1:i2, j1:j2), stat=ierr)
         allocate (td2(i1:i2, j1:j2), stat=ierr)

         if (jatidep > 1) then ! gradient intp.
            if (allocated(td2_x)) then
               deallocate (td2_x, td2_y)
            end if
            allocate (td2_x(i1:i2, j1:j2), stat=ierr)
            allocate (td2_y(i1:i2, j1:j2), stat=ierr)
         end if

         td2 = 0.0_dp

         if (jaselfal > 0) then
!         if (allocated(self) ) deallocate ( self, avhs, area ) MVL ask Camille
            if (allocated(self)) then
               deallocate (self, avhs)
            end if
            allocate (self(i1:i2, j1:j2), stat=ierr)
            allocate (avhs(i1:i2, j1:j2), stat=ierr)
!         allocate ( area(i1:i2,j1:j2), stat=ierr)
            do i = i1, i2
               do j = j1, j2
                  xx(1) = real(i, kind=dp) - 0.5_dp
                  yy(1) = real(j, kind=dp) - 0.5_dp
                  xx(2) = real(i, kind=dp) + 0.5_dp
                  yy(2) = real(j, kind=dp) - 0.5_dp
                  xx(3) = real(i, kind=dp) + 0.5_dp
                  yy(3) = real(j, kind=dp) + 0.5_dp
                  xx(4) = real(i, kind=dp) - 0.5_dp
                  yy(4) = real(j, kind=dp) + 0.5_dp

!                call dAREAN( XX, YY, 4, DAREA, DLENGTH, DLENMX )
!                area(i,j) = darea
               end do
            end do
         end if

         do i = i1, i2
            do j = j1, j2
               xz2(i, j) = i * dg2rd
               yz2(i, j) = j * dg2rd
            end do
         end do

         ndx2 = (i2 - i1 + 1) * (j2 - j1 + 1)

      end if

      if (jatidep > 0) then
         call tforce(jul0, TIME, xz2, yz2, Td2, ndx2, dstart, dstop, eps)
      else
         td2 = 0.0_dp ! safety
      end if

      if (jaselfal > 0) then
         call aggregatewaterlevels(avhs, i1, i2, j1, j2)

         call selfattraction(avhs, self, i1, i2, j1, j2, jaselfal)
      end if

      do n = 1, ndx
         m1 = floor(xz(n))
         m2 = m1 + 1
         n1 = floor(yz(n))
         n2 = n1 + 1
         di = xz(n) - m1
         dj = yz(n) - n1
         f11 = (1.0_dp - di) * (1.0_dp - dj)
         f21 = (di) * (1.0_dp - dj)
         f22 = (di) * (dj)
         f12 = (1.0_dp - di) * (dj)

         if (jaselfal > 0) then

            tidep(1, n) = (td2(m1, n1) + self(m1, n1)) * f11 + &
                          (td2(m2, n1) + self(m2, n1)) * f21 + &
                          (td2(m2, n2) + self(m2, n2)) * f22 + &
                          (td2(m1, n2) + self(m1, n2)) * f12

!        for output only
            if (jamaptidep > 0 .and. Np > 1) then ! store SAL potential seperately
               tidep(2, n) = (self(m1, n1)) * f11 + &
                             (self(m2, n1)) * f21 + &
                             (self(m2, n2)) * f22 + &
                             (self(m1, n2)) * f12
            end if
         else
            tidep(1, n) = td2(m1, n1) * f11 + &
                          td2(m2, n1) * f21 + &
                          td2(m2, n2) * f22 + &
                          td2(m1, n2) * f12
         end if
      end do

      if (jatidep > 1) then ! gradient intp., get gradient

         dyy = 2.0_dp * ra * dg2rd
         do j = j1 + 1, j2 - 1
            dxx = dyy * cos(yz2(i1, j))
            do i = i1 + 1, i2 - 1
               td2_x(i, j) = (td2(i + 1, j) - td2(i - 1, j)) / dxx
               td2_y(i, j) = (td2(i, j + 1) - td2(i, j - 1)) / dyy
               if (jaselfal > 0) then
                  td2_x(i, j) = td2_x(i, j) + (self(i + 1, j) - self(i - 1, j)) / dxx
                  td2_y(i, j) = td2_y(i, j) + (self(i, j + 1) - self(i, j - 1)) / dyy
               end if
            end do
         end do

         do L = 1, Lnx
            m1 = floor(xu(L))
            m2 = m1 + 1
            n1 = floor(yu(L))
            n2 = n1 + 1
            di = xu(L) - m1
            dj = yu(L) - n1
            f11 = (1.0_dp - di) * (1.0_dp - dj)
            f21 = (di) * (1.0_dp - dj)
            f22 = (di) * (dj)
            f12 = (1.0_dp - di) * (dj)

            tidef(L) = csu(L) * (td2_x(m1, n1) * f11 + &
                                 td2_x(m2, n1) * f21 + &
                                 td2_x(m2, n2) * f22 + &
                                 td2_x(m1, n2) * f12) &
                       + snu(L) * (td2_y(m1, n1) * f11 + &
                                   td2_y(m2, n1) * f21 + &
                                   td2_y(m2, n2) * f22 + &
                                   td2_y(m1, n2) * f12)
         end do

      end if

   end subroutine meteo_tidepotential

   subroutine aggregatewaterlevels(avhs, i1, i2, j1, j2)
      !Compute the water level from the unstructured to the one degree regular grid
      use m_flow
      use m_flowgeom
      use m_GlobalParameters, only: INDTP_2D
      use m_partitioninfo
      use kdtree2Factory
      use messagehandling, only: LEVEL_INFO, mess
      use m_find_flownode, only: find_nearest_flownodes_kdtree
      use m_wall_clock_time
      use m_in_flowcell, only: in_flowcell

      implicit none

      integer :: i1, i2, j1, j2, k, k1, LL, i, j, iL, iR, ierr
      integer, save :: ini = 0
      real(kind=dp) :: alf, x, y
      real(kind=dp) :: avhs(i1:i2, j1:j2), area(i1:i2, j1:j2)

      real(kind=dp), dimension(:, :), allocatable :: xx, yy
      integer, dimension(:, :), allocatable :: kk
      real(kind=dp), dimension(:, :, :), allocatable, save :: workin, workout ! work arrays for parallel communication

      integer :: Ni, Nj
      integer :: jakdtree = 1
      integer :: ierror

      character(len=1024) :: str

      real(kind=dp) :: t0, t1
      real(kind=dp) :: wo
      real(kind=dp) :: Ds

      real(kind=dp), allocatable, save :: jasea(:, :)

      Ni = i2 - i1 + 1
      Nj = j2 - j1 + 1

      if (ini == 0) then
         call wall_clock_time(t0)

         allocate (jasea(i1:i2, j1:j2), stat=ierr)

         if (jakdtree == 1) then
            call realloc(xx, [Ni, Nj], keepExisting=.false., fill=0.0_dp)
            call realloc(yy, [Ni, Nj], keepExisting=.false., fill=0.0_dp)
            call realloc(kk, [Ni, Nj], keepExisting=.false., fill=0)
            do j = j1, j2
               do i = i1, i2
                  xx(i - i1 + 1, j - j1 + 1) = real(i, kind=dp)
                  yy(i - i1 + 1, j - j1 + 1) = real(j, kind=dp)
               end do
            end do
            call find_nearest_flownodes_kdtree(treeglob, Ni * Nj, xx, yy, kk, jakdtree, INDTP_2D, ierror)
            if (ierror /= 0) then
               jakdtree = 0
            end if

            if (allocated(xx)) then
               deallocate (xx)
            end if
            if (allocated(yy)) then
               deallocate (yy)
            end if
         end if

         if (jampi == 0) then ! sequential
            do j = j1, j2
               do i = i1, i2
                  if (jakdtree == 1) then
                     k = kk(i - i1 + 1, j - j1 + 1)
                  else
                     x = real(i, kind=dp)
                     y = real(j, kind=dp)
                     call in_flowcell(x, y, K)
                  end if

                  if (k > 0) then
                     jasea(i, j) = 1
                  else
                     jasea(i, j) = 0
                  end if
               end do
            end do
         else
!        allocate work arrays
            if (allocated(workin)) then
               deallocate (workin)
            end if
            allocate (workin(2, Ni, Nj))
            workin = 0.0_dp
            if (allocated(workout)) then
               deallocate (workout)
            end if
            allocate (workout(2, Ni, Nj))

            do j = j1, j2
               do i = i1, i2
                  if (jakdtree == 1) then
                     k = kk(i - i1 + 1, j - j1 + 1)
                  else
                     x = real(i, kind=dp)
                     y = real(j, kind=dp)
                     call in_flowcell(x, y, K)
                  end if

                  if (k <= Ndxi) then
                     k1 = k
                  else ! boundary nodes: take connected internal node for domain number (boundary nodes are always in own domain)
                     LL = abs(nd(k)%ln(1)) !< only one link connected to boundary node
                     k1 = ln(1, LL) + ln(2, LL) - k
                  end if

                  if (k > 0) then
                     if (idomain(k1) == my_rank) then
!                   jasea(i,j) = 1
                        workin(1, i - i1 + 1, j - j1 + 1) = 1.0_dp
                        workin(2, i - i1 + 1, j - j1 + 1) = 0.0_dp ! dummy
                     else
                        workin(1, i - i1 + 1, j - j1 + 1) = 0.0_dp
                        workin(2, i - i1 + 1, j - j1 + 1) = 0.0_dp ! dummy
                     end if
                  else
!                   jasea(i,j) = 0
                     workin(1, i - i1 + 1, j - j1 + 1) = 0.0_dp
                     workin(2, i - i1 + 1, j - j1 + 1) = 0.0_dp ! dummy
                  end if
               end do
            end do
            call reduce_double_sum(2 * Ni * Nj, workin, workout) ! workin too large, but only once
            do j = j1, j2
               do i = i1, i2
                  wo = workout(1, i - i1 + 1, j - j1 + 1)
                  if (wo == 0) then
                     jasea(i, j) = 0
                  else
                     jasea(i, j) = 1
                  end if
               end do
            end do
         end if

         call wall_clock_time(t1)
         write (str, "('SAL/aggregate waterlevels, elapsed time: ', G15.5, 's.')") t1 - t0
         call mess(LEVEL_INFO, trim(str))

         ini = 1
      end if

      if (allocated(kk)) then
         deallocate (kk)
      end if

      jasea = 1

      avhs = 0.0_dp
      area = 0.0_dp

      if (jampi == 0) then
         do k = 1, ndx
            i = nint(xz(k))
            j = nint(yz(k))

            Ds = 0.0_dp
            if (jaSELFALcorrectWLwithIni == 1) then
!           water level rise
               Ds = s1init(k)
            end if

            if (hs(k) > 0.0) then

               avhs(i, j) = avhs(i, j) + (s1(k) - Ds) * ba(k)
               area(i, j) = area(i, j) + ba(k)
            end if
         end do
      else ! parallel
         workin = 0.0_dp

         do k = 1, Ndx
            i = nint(xz(k))
            j = nint(yz(k))

            if (k <= Ndxi) then ! internal nodes
               k1 = k
            else ! boundary nodes: take connected internal node for domain number (boundary nodes are always in own domain)
               LL = abs(nd(k)%ln(1)) !< only one link connected to boundary node
               k1 = ln(1, LL) + ln(2, LL) - k
            end if

            Ds = 0.0_dp
            if (jaSELFALcorrectWLwithIni == 1) then
!           water level rise
               Ds = s1init(k)
            end if

            if (hs(k) > 0.0 .and. idomain(k1) == my_rank) then
!                    avhs(i,j) = avhs(i,j) + s1(k)*ba(k)
!                    area(i,j) = area(i,j) + ba(k)

               workin(1, i - i1 + 1, j - j1 + 1) = workin(1, i - i1 + 1, j - j1 + 1) + (s1(k) - Ds) * ba(k)
               workin(2, i - i1 + 1, j - j1 + 1) = workin(2, i - i1 + 1, j - j1 + 1) + ba(k)
            end if
         end do
         call reduce_double_sum(2 * Ni * Nj, workin, workout)

         do j = j1, j2
            do i = i1, i2
               avhs(i, j) = workout(1, i - i1 + 1, j - j1 + 1)
               area(i, j) = workout(2, i - i1 + 1, j - j1 + 1)
            end do
         end do
      end if

      do j = j1, j2
         do i = i1, i2
            if (area(i, j) > 0.0) then
               avhs(i, j) = avhs(i, j) / area(i, j)
            end if
         end do
      end do

      do j = j1, j2
         do i = i1, i2
            if (area(i, j) == 0.0 .and. jasea(i, j) == 1) then
               call findleftright(area, i, j, i1, i2, j1, j2, iL, iR, alf)
               avhs(i, j) = (1.0_dp - alf) * avhs(iL, j) + alf * avhs(iR, j)
            end if
         end do
      end do

      !Used for testing
      !avhs=1.0_dp

      !Create output file
      ! open (newunit=lunfil, file='d:\output_avhs2.txt',status='unknown',position='append')
      ! write(lunfil,fmt=*) i1
      ! write(lunfil,fmt=*) i2
      ! write(lunfil,fmt=*) j1
      ! write(lunfil,fmt=*) j2
      ! do i=i1,i2
      !   do j=j1,j2
      !       write(lunfil,fmt=*) avhs(i,j)
      !   enddo
      ! enddo
      ! close(lunfil)

   end subroutine aggregatewaterlevels

   subroutine findleftright(area, ii, ji, i1, i2, j1, j2, iL, iR, alf)
      implicit none
      integer, intent(in) :: i1, i2, ii, j1, j2, ji
      real(kind=dp), intent(in) :: area(i1:i2, j1:j2)

      integer, intent(out) :: iL, iR
      real(kind=dp), intent(out) :: alf
      integer :: i, dr, dl, findr, findl, disR, disL, stopsearch

      stopsearch = 0

      findr = 0
      dr = 0
      i = ii
      do while (findr == 0)
         if (area(i, ji) /= 0) then
            iR = i
            findr = 1
         end if
         if (i == i1) then
            i = i2
            dr = 1
         else
            i = i - 1
         end if
         if (i == ii) then
            findr = 1
            iR = ii
            iL = ii
            alf = 0
            stopsearch = 1
         end if
      end do

      if (stopsearch == 0) then

         findl = 0
         dl = 0
         i = ii
         do while (findl == 0)
            if (area(i, ji) /= 0) then
               iL = i
               findl = 1
            end if
            if (i == i2) then
               i = i1
               dl = 1
            else
               i = i + 1
            end if
         end do

         if (dl == 0) then
            disL = ii - iL
         else
            disL = (ii - i2) - (iL - i1)
         end if
         if (dr == 0) then
            disR = ii - iR
         else
            disR = (ii - i1) - (iR - i2)
         end if

         alf = real(disL, kind=dp) / real(disL - disR, kind=dp)

      end if
   end subroutine findleftright

   subroutine selfattraction(avhs, self, i1, i2, j1, j2, jaselfal)
      use spherepack, only: shaec, shaeci, shsec, shseci
      implicit none

      ! Input\Output parameter
      integer, intent(in) :: i1, i2, j1, j2, jaselfal
      real(kind=dp), intent(in) :: avhs(i1:i2, j1:j2)
      real(kind=dp), intent(out) :: self(i1:i2, j1:j2)

      ! Local parameters
      real(kind=dp), parameter :: Me = 5.9726e24_dp, R = 6371e3_dp, g = 9.81_dp, pi = 4.0_dp * atan(1.0_dp), rhow = 1.0240164e3_dp, rhoe = 3.0_dp * Me / (4.0_dp * pi * R * R * R)
      integer :: nlat, nlon, lsave
      integer :: i, j, ierror, isym, nt, l, mdab, ndab, k1
      real(kind=dp), dimension(:), allocatable :: llnh, llnk
      real(kind=dp), dimension(:), allocatable :: wshaec, wshsec
      real(kind=dp), dimension(:, :), allocatable :: a, b
      real(kind=dp), dimension(:, :), allocatable :: avhs1, self1

      ! Initialisation
      nlat = 181
      nlon = 360
      lsave = nlat * (nlat + 1) + 3 * ((nlat - 2) * (2 * nlat - nlat - 1) + nlon + 15)
      mdab = nlat
      ndab = nlat

!  allocate
      allocate (wshaec(1:lsave))
      allocate (wshsec(1:lsave))
      allocate (a(1:mdab, 1:ndab))
      allocate (b(1:mdab, 1:ndab))

      allocate (llnh(0:1024))
      allocate (llnk(0:1024))
      allocate (avhs1(0:180, 0:359))
      allocate (self1(0:180, 0:359))

      !Water level need to be defined in an array avhs1,
      ! where avhs1(i,j) contains the waterlevel on the point with longitude phi(j)=(j-1)*2*pi/nlon
      ! and colatitude theta(i)=(i-1)*pi/(nlat)
      !For a one degree grid, we have nlon=360 and nlat=181
      !If avhs is smaller then 0 is chosen at the location of the missing values
      avhs1 = 0.0_dp
      k1 = 0
      do i = i1, min(i2, i1 + 360 - 1)
         do j = j1, j2
            avhs1(j + 90, k1) = avhs(i, j)
         end do
         k1 = k1 + 1
      end do

      !Load Love numbers
      call loadlovenumber(llnh, llnk)

      !Computation
      isym = 0
      nt = 1
      !Spherical harmonic analysis
      call shaeci(nlat, nlon, wshaec, ierror)
      call shaec(nlat, nlon, isym, nt, avhs1, nlat, nlon, a, b, mdab, ndab, wshaec, ierror)

      !Multiplication in spherical harmonic space (=convolution)
      if (jaselfal == 2) then
         do l = 1, ndab
            a(1:mdab, l) = 3 * g * rhow / rhoe / (2 * l - 1) * a(1:mdab, l)
            b(1:mdab, l) = 3 * g * rhow / rhoe / (2 * l - 1) * b(1:mdab, l)
         end do
      end if
      if (jaselfal == 1) then
         do l = 1, ndab
            a(1:mdab, l) = 3 * g * rhow * (1 + llnk(l - 1) - llnh(l - 1)) / rhoe / (2 * l - 1) * a(1:mdab, l)
            b(1:mdab, l) = 3 * g * rhow * (1 + llnk(l - 1) - llnh(l - 1)) / rhoe / (2 * l - 1) * b(1:mdab, l)
         end do
      end if

      !Spherical harmonic synthesis
      call shseci(nlat, nlon, wshsec, ierror)
      call shsec(nlat, nlon, isym, nt, self1, nlat, nlon, a, b, mdab, ndab, &
                 wshsec, ierror)

      !self1 is defined on the same grid than avhs1, we put it back in the same grid than avhs
      self = 0.0_dp
      k1 = 0
      do i = i1, i2
         if (k1 >= 360) then
            k1 = 0
         end if
         do j = j1, j2
            if (j + 90 >= 0 .and. j - 90 <= 180) then
               self(i, j) = self1(j + 90, k1)
            end if
         end do
         k1 = k1 + 1
      end do
   end subroutine selfattraction

   subroutine loadlovenumber(llnh, llnk)
      !Define the second load Love number h' and k' up to degree 1024
      implicit none

      ! Input\Output parameter
      real(kind=dp), dimension(0:1024), intent(out) :: llnh, llnk

      !Fill arrays
      llnh(0) = 0.0000000000e+00_dp
      llnh(1) = -0.1285877758e+01_dp
      llnh(2) = -0.9915810331e+00_dp
      llnh(3) = -0.1050767745e+01_dp
      llnh(4) = -0.1053393012e+01_dp
      llnh(5) = -0.1086317605e+01_dp
      llnh(6) = -0.1143860336e+01_dp
      llnh(7) = -0.1212408459e+01_dp
      llnh(8) = -0.1283943275e+01_dp
      llnh(9) = -0.1354734845e+01_dp
      llnh(10) = -0.1423282851e+01_dp
      llnh(11) = -0.1489094554e+01_dp
      llnh(12) = -0.1552074997e+01_dp
      llnh(13) = -0.1612273740e+01_dp
      llnh(14) = -0.1669763369e+01_dp
      llnh(15) = -0.1724635488e+01_dp
      llnh(16) = -0.1776963521e+01_dp
      llnh(17) = -0.1826825601e+01_dp
      llnh(18) = -0.1874298467e+01_dp
      llnh(19) = -0.1919461416e+01_dp
      llnh(20) = -0.1962393632e+01_dp
      llnh(21) = -0.2003182253e+01_dp
      llnh(22) = -0.2041915786e+01_dp
      llnh(23) = -0.2078680486e+01_dp
      llnh(24) = -0.2113573061e+01_dp
      llnh(25) = -0.2146680270e+01_dp
      llnh(26) = -0.2178105661e+01_dp
      llnh(27) = -0.2207927152e+01_dp
      llnh(28) = -0.2236242846e+01_dp
      llnh(29) = -0.2263132641e+01_dp
      llnh(30) = -0.2288687940e+01_dp
      llnh(31) = -0.2312991757e+01_dp
      llnh(32) = -0.2336112443e+01_dp
      llnh(33) = -0.2358128831e+01_dp
      llnh(34) = -0.2379107893e+01_dp
      llnh(35) = -0.2399120761e+01_dp
      llnh(36) = -0.2418226351e+01_dp
      llnh(37) = -0.2436482905e+01_dp
      llnh(38) = -0.2453948379e+01_dp
      llnh(39) = -0.2470670195e+01_dp
      llnh(40) = -0.2486697757e+01_dp
      llnh(41) = -0.2502076334e+01_dp
      llnh(42) = -0.2516847401e+01_dp
      llnh(43) = -0.2531050008e+01_dp
      llnh(44) = -0.2544719530e+01_dp
      llnh(45) = -0.2557890739e+01_dp
      llnh(46) = -0.2570594319e+01_dp
      llnh(47) = -0.2582859779e+01_dp
      llnh(48) = -0.2594714216e+01_dp
      llnh(49) = -0.2606182782e+01_dp
      llnh(50) = -0.2617289738e+01_dp
      llnh(51) = -0.2628056023e+01_dp
      llnh(52) = -0.2638502978e+01_dp
      llnh(53) = -0.2648649164e+01_dp
      llnh(54) = -0.2658512061e+01_dp
      llnh(55) = -0.2668109142e+01_dp
      llnh(56) = -0.2677455130e+01_dp
      llnh(57) = -0.2686564655e+01_dp
      llnh(58) = -0.2695451439e+01_dp
      llnh(59) = -0.2704127764e+01_dp
      llnh(60) = -0.2712605707e+01_dp
      llnh(61) = -0.2720896238e+01_dp
      llnh(62) = -0.2729009765e+01_dp
      llnh(63) = -0.2736955903e+01_dp
      llnh(64) = -0.2744743969e+01_dp
      llnh(65) = -0.2752382423e+01_dp
      llnh(66) = -0.2759879282e+01_dp
      llnh(67) = -0.2767242102e+01_dp
      llnh(68) = -0.2774478021e+01_dp
      llnh(69) = -0.2781593811e+01_dp
      llnh(70) = -0.2788595709e+01_dp
      llnh(71) = -0.2795489680e+01_dp
      llnh(72) = -0.2802281343e+01_dp
      llnh(73) = -0.2808976028e+01_dp
      llnh(74) = -0.2815578704e+01_dp
      llnh(75) = -0.2822094093e+01_dp
      llnh(76) = -0.2828526669e+01_dp
      llnh(77) = -0.2834880683e+01_dp
      llnh(78) = -0.2841160150e+01_dp
      llnh(79) = -0.2847368769e+01_dp
      llnh(80) = -0.2853510163e+01_dp
      llnh(81) = -0.2859587939e+01_dp
      llnh(82) = -0.2865604931e+01_dp
      llnh(83) = -0.2871564378e+01_dp
      llnh(84) = -0.2877469169e+01_dp
      llnh(85) = -0.2883322045e+01_dp
      llnh(86) = -0.2889125648e+01_dp
      llnh(87) = -0.2894882413e+01_dp
      llnh(88) = -0.2900594702e+01_dp
      llnh(89) = -0.2906264743e+01_dp
      llnh(90) = -0.2911894687e+01_dp
      llnh(91) = -0.2917486512e+01_dp
      llnh(92) = -0.2923042145e+01_dp
      llnh(93) = -0.2928563403e+01_dp
      llnh(94) = -0.2934052041e+01_dp
      llnh(95) = -0.2939509674e+01_dp
      llnh(96) = -0.2944937877e+01_dp
      llnh(97) = -0.2950338132e+01_dp
      llnh(98) = -0.2955711880e+01_dp
      llnh(99) = -0.2961060436e+01_dp
      llnh(100) = -0.2966385090e+01_dp
      llnh(101) = -0.2971687056e+01_dp
      llnh(102) = -0.2976967512e+01_dp
      llnh(103) = -0.2982227536e+01_dp
      llnh(104) = -0.2987468183e+01_dp
      llnh(105) = -0.2992690446e+01_dp
      llnh(106) = -0.2997895290e+01_dp
      llnh(107) = -0.3003083596e+01_dp
      llnh(108) = -0.3008256228e+01_dp
      llnh(109) = -0.3013413998e+01_dp
      llnh(110) = -0.3018557697e+01_dp
      llnh(111) = -0.3023688044e+01_dp
      llnh(112) = -0.3028805745e+01_dp
      llnh(113) = -0.3033911465e+01_dp
      llnh(114) = -0.3039005849e+01_dp
      llnh(115) = -0.3044089483e+01_dp
      llnh(116) = -0.3049162947e+01_dp
      llnh(117) = -0.3054226779e+01_dp
      llnh(118) = -0.3059281508e+01_dp
      llnh(119) = -0.3064327612e+01_dp
      llnh(120) = -0.3069365560e+01_dp
      llnh(121) = -0.3074395793e+01_dp
      llnh(122) = -0.3079418737e+01_dp
      llnh(123) = -0.3084434789e+01_dp
      llnh(124) = -0.3089444323e+01_dp
      llnh(125) = -0.3094447698e+01_dp
      llnh(126) = -0.3099445739e+01_dp
      llnh(127) = -0.3104437859e+01_dp
      llnh(128) = -0.3109424785e+01_dp
      llnh(129) = -0.3114406817e+01_dp
      llnh(130) = -0.3119384228e+01_dp
      llnh(131) = -0.3124357299e+01_dp
      llnh(132) = -0.3129326253e+01_dp
      llnh(133) = -0.3134291331e+01_dp
      llnh(134) = -0.3139252753e+01_dp
      llnh(135) = -0.3144210746e+01_dp
      llnh(136) = -0.3149165487e+01_dp
      llnh(137) = -0.3154117170e+01_dp
      llnh(138) = -0.3159065971e+01_dp
      llnh(139) = -0.3164012071e+01_dp
      llnh(140) = -0.3168955611e+01_dp
      llnh(141) = -0.3173896746e+01_dp
      llnh(142) = -0.3178835615e+01_dp
      llnh(143) = -0.3183772364e+01_dp
      llnh(144) = -0.3188707103e+01_dp
      llnh(145) = -0.3193639953e+01_dp
      llnh(146) = -0.3198571025e+01_dp
      llnh(147) = -0.3203500433e+01_dp
      llnh(148) = -0.3208428262e+01_dp
      llnh(149) = -0.3213354609e+01_dp
      llnh(150) = -0.3218279559e+01_dp
      llnh(151) = -0.3223203202e+01_dp
      llnh(152) = -0.3228125600e+01_dp
      llnh(153) = -0.3233046832e+01_dp
      llnh(154) = -0.3237966958e+01_dp
      llnh(155) = -0.3242886050e+01_dp
      llnh(156) = -0.3247804156e+01_dp
      llnh(157) = -0.3252721331e+01_dp
      llnh(158) = -0.3257637624e+01_dp
      llnh(159) = -0.3262553244e+01_dp
      llnh(160) = -0.3267467917e+01_dp
      llnh(161) = -0.3272381835e+01_dp
      llnh(162) = -0.3277295035e+01_dp
      llnh(163) = -0.3282207554e+01_dp
      llnh(164) = -0.3287119415e+01_dp
      llnh(165) = -0.3292030647e+01_dp
      llnh(166) = -0.3296941273e+01_dp
      llnh(167) = -0.3301851314e+01_dp
      llnh(168) = -0.3306760804e+01_dp
      llnh(169) = -0.3311669742e+01_dp
      llnh(170) = -0.3316578148e+01_dp
      llnh(171) = -0.3321486032e+01_dp
      llnh(172) = -0.3326393422e+01_dp
      llnh(173) = -0.3331300520e+01_dp
      llnh(174) = -0.3336207644e+01_dp
      llnh(175) = -0.3341113629e+01_dp
      llnh(176) = -0.3346019149e+01_dp
      llnh(177) = -0.3350924191e+01_dp
      llnh(178) = -0.3355828764e+01_dp
      llnh(179) = -0.3360732862e+01_dp
      llnh(180) = -0.3365636499e+01_dp
      llnh(181) = -0.3370539656e+01_dp
      llnh(182) = -0.3375442336e+01_dp
      llnh(183) = -0.3380344530e+01_dp
      llnh(184) = -0.3385246241e+01_dp
      llnh(185) = -0.3390147452e+01_dp
      llnh(186) = -0.3395048158e+01_dp
      llnh(187) = -0.3399948348e+01_dp
      llnh(188) = -0.3404848021e+01_dp
      llnh(189) = -0.3409747153e+01_dp
      llnh(190) = -0.3414645740e+01_dp
      llnh(191) = -0.3419543764e+01_dp
      llnh(192) = -0.3424441221e+01_dp
      llnh(193) = -0.3429338088e+01_dp
      llnh(194) = -0.3434234352e+01_dp
      llnh(195) = -0.3439129995e+01_dp
      llnh(196) = -0.3444025009e+01_dp
      llnh(197) = -0.3448919371e+01_dp
      llnh(198) = -0.3453813064e+01_dp
      llnh(199) = -0.3458706066e+01_dp
      llnh(200) = -0.3463598369e+01_dp
      llnh(201) = -0.3468489946e+01_dp
      llnh(202) = -0.3473380779e+01_dp
      llnh(203) = -0.3478270847e+01_dp
      llnh(204) = -0.3483160133e+01_dp
      llnh(205) = -0.3488048612e+01_dp
      llnh(206) = -0.3492936263e+01_dp
      llnh(207) = -0.3497823067e+01_dp
      llnh(208) = -0.3502708995e+01_dp
      llnh(209) = -0.3507594040e+01_dp
      llnh(210) = -0.3512478162e+01_dp
      llnh(211) = -0.3517361345e+01_dp
      llnh(212) = -0.3522243562e+01_dp
      llnh(213) = -0.3527124799e+01_dp
      llnh(214) = -0.3532005020e+01_dp
      llnh(215) = -0.3536884206e+01_dp
      llnh(216) = -0.3541762329e+01_dp
      llnh(217) = -0.3546639373e+01_dp
      llnh(218) = -0.3551515301e+01_dp
      llnh(219) = -0.3556390096e+01_dp
      llnh(220) = -0.3561263723e+01_dp
      llnh(221) = -0.3566136170e+01_dp
      llnh(222) = -0.3571007398e+01_dp
      llnh(223) = -0.3575877387e+01_dp
      llnh(224) = -0.3580747513e+01_dp
      llnh(225) = -0.3585615132e+01_dp
      llnh(226) = -0.3590481365e+01_dp
      llnh(227) = -0.3595346263e+01_dp
      llnh(228) = -0.3600209792e+01_dp
      llnh(229) = -0.3605071936e+01_dp
      llnh(230) = -0.3609932657e+01_dp
      llnh(231) = -0.3614791931e+01_dp
      llnh(232) = -0.3619649732e+01_dp
      llnh(233) = -0.3624506035e+01_dp
      llnh(234) = -0.3629360805e+01_dp
      llnh(235) = -0.3634214026e+01_dp
      llnh(236) = -0.3639065656e+01_dp
      llnh(237) = -0.3643915683e+01_dp
      llnh(238) = -0.3648764066e+01_dp
      llnh(239) = -0.3653610782e+01_dp
      llnh(240) = -0.3658455800e+01_dp
      llnh(241) = -0.3663299103e+01_dp
      llnh(242) = -0.3668140651e+01_dp
      llnh(243) = -0.3672980417e+01_dp
      llnh(244) = -0.3677818461e+01_dp
      llnh(245) = -0.3682654761e+01_dp
      llnh(246) = -0.3687489030e+01_dp
      llnh(247) = -0.3692321405e+01_dp
      llnh(248) = -0.3697151858e+01_dp
      llnh(249) = -0.3701980361e+01_dp
      llnh(250) = -0.3706806936e+01_dp
      llnh(251) = -0.3711631455e+01_dp
      llnh(252) = -0.3716453939e+01_dp
      llnh(253) = -0.3721274358e+01_dp
      llnh(254) = -0.3726092689e+01_dp
      llnh(255) = -0.3730908892e+01_dp
      llnh(256) = -0.3735722950e+01_dp
      llnh(257) = -0.3740534821e+01_dp
      llnh(258) = -0.3745344490e+01_dp
      llnh(259) = -0.3750151923e+01_dp
      llnh(260) = -0.3754957088e+01_dp
      llnh(261) = -0.3759759956e+01_dp
      llnh(262) = -0.3764560505e+01_dp
      llnh(263) = -0.3769358699e+01_dp
      llnh(264) = -0.3774154515e+01_dp
      llnh(265) = -0.3778947920e+01_dp
      llnh(266) = -0.3783738888e+01_dp
      llnh(267) = -0.3788527389e+01_dp
      llnh(268) = -0.3793313390e+01_dp
      llnh(269) = -0.3798096866e+01_dp
      llnh(270) = -0.3802877791e+01_dp
      llnh(271) = -0.3807656137e+01_dp
      llnh(272) = -0.3812431870e+01_dp
      llnh(273) = -0.3817204958e+01_dp
      llnh(274) = -0.3821975383e+01_dp
      llnh(275) = -0.3826743110e+01_dp
      llnh(276) = -0.3831508110e+01_dp
      llnh(277) = -0.3836271873e+01_dp
      llnh(278) = -0.3841032150e+01_dp
      llnh(279) = -0.3845788986e+01_dp
      llnh(280) = -0.3850542997e+01_dp
      llnh(281) = -0.3855294157e+01_dp
      llnh(282) = -0.3860042438e+01_dp
      llnh(283) = -0.3864787807e+01_dp
      llnh(284) = -0.3869530239e+01_dp
      llnh(285) = -0.3874269702e+01_dp
      llnh(286) = -0.3879006179e+01_dp
      llnh(287) = -0.3883739635e+01_dp
      llnh(288) = -0.3888470049e+01_dp
      llnh(289) = -0.3893197379e+01_dp
      llnh(290) = -0.3897921616e+01_dp
      llnh(291) = -0.3902642720e+01_dp
      llnh(292) = -0.3907360668e+01_dp
      llnh(293) = -0.3912075438e+01_dp
      llnh(294) = -0.3916786993e+01_dp
      llnh(295) = -0.3921495314e+01_dp
      llnh(296) = -0.3926200363e+01_dp
      llnh(297) = -0.3930902125e+01_dp
      llnh(298) = -0.3935600573e+01_dp
      llnh(299) = -0.3940295676e+01_dp
      llnh(300) = -0.3944987404e+01_dp
      llnh(301) = -0.3949675741e+01_dp
      llnh(302) = -0.3954360653e+01_dp
      llnh(303) = -0.3959042119e+01_dp
      llnh(304) = -0.3963720107e+01_dp
      llnh(305) = -0.3968394592e+01_dp
      llnh(306) = -0.3973065547e+01_dp
      llnh(307) = -0.3977732958e+01_dp
      llnh(308) = -0.3982396782e+01_dp
      llnh(309) = -0.3987056997e+01_dp
      llnh(310) = -0.3991713583e+01_dp
      llnh(311) = -0.3996366510e+01_dp
      llnh(312) = -0.4001015766e+01_dp
      llnh(313) = -0.4005661299e+01_dp
      llnh(314) = -0.4010303104e+01_dp
      llnh(315) = -0.4014941156e+01_dp
      llnh(316) = -0.4019575415e+01_dp
      llnh(317) = -0.4024205867e+01_dp
      llnh(318) = -0.4028832484e+01_dp
      llnh(319) = -0.4033455241e+01_dp
      llnh(320) = -0.4038074113e+01_dp
      llnh(321) = -0.4042689078e+01_dp
      llnh(322) = -0.4047300107e+01_dp
      llnh(323) = -0.4051907177e+01_dp
      llnh(324) = -0.4056510264e+01_dp
      llnh(325) = -0.4061109341e+01_dp
      llnh(326) = -0.4065704388e+01_dp
      llnh(327) = -0.4070295381e+01_dp
      llnh(328) = -0.4074882284e+01_dp
      llnh(329) = -0.4079465087e+01_dp
      llnh(330) = -0.4084043760e+01_dp
      llnh(331) = -0.4088618279e+01_dp
      llnh(332) = -0.4093188619e+01_dp
      llnh(333) = -0.4097757745e+01_dp
      llnh(334) = -0.4102319954e+01_dp
      llnh(335) = -0.4106878131e+01_dp
      llnh(336) = -0.4111431810e+01_dp
      llnh(337) = -0.4115981211e+01_dp
      llnh(338) = -0.4120526303e+01_dp
      llnh(339) = -0.4125067085e+01_dp
      llnh(340) = -0.4129603517e+01_dp
      llnh(341) = -0.4134135580e+01_dp
      llnh(342) = -0.4138663260e+01_dp
      llnh(343) = -0.4143186527e+01_dp
      llnh(344) = -0.4147705363e+01_dp
      llnh(345) = -0.4152219750e+01_dp
      llnh(346) = -0.4156729818e+01_dp
      llnh(347) = -0.4161235270e+01_dp
      llnh(348) = -0.4165736154e+01_dp
      llnh(349) = -0.4170232508e+01_dp
      llnh(350) = -0.4174724295e+01_dp
      llnh(351) = -0.4179211503e+01_dp
      llnh(352) = -0.4183694105e+01_dp
      llnh(353) = -0.4188172090e+01_dp
      llnh(354) = -0.4192645419e+01_dp
      llnh(355) = -0.4197114075e+01_dp
      llnh(356) = -0.4201578059e+01_dp
      llnh(357) = -0.4206037324e+01_dp
      llnh(358) = -0.4210491862e+01_dp
      llnh(359) = -0.4214941658e+01_dp
      llnh(360) = -0.4219386679e+01_dp
      llnh(361) = -0.4223826915e+01_dp
      llnh(362) = -0.4228262339e+01_dp
      llnh(363) = -0.4232692981e+01_dp
      llnh(364) = -0.4237118729e+01_dp
      llnh(365) = -0.4241539614e+01_dp
      llnh(366) = -0.4245955590e+01_dp
      llnh(367) = -0.4250366666e+01_dp
      llnh(368) = -0.4254772819e+01_dp
      llnh(369) = -0.4259174029e+01_dp
      llnh(370) = -0.4263570269e+01_dp
      llnh(371) = -0.4267961523e+01_dp
      llnh(372) = -0.4272347781e+01_dp
      llnh(373) = -0.4276728990e+01_dp
      llnh(374) = -0.4281105174e+01_dp
      llnh(375) = -0.4285476289e+01_dp
      llnh(376) = -0.4289842325e+01_dp
      llnh(377) = -0.4294203252e+01_dp
      llnh(378) = -0.4298559055e+01_dp
      llnh(379) = -0.4302909733e+01_dp
      llnh(380) = -0.4307255251e+01_dp
      llnh(381) = -0.4311595601e+01_dp
      llnh(382) = -0.4315930729e+01_dp
      llnh(383) = -0.4320260674e+01_dp
      llnh(384) = -0.4324585375e+01_dp
      llnh(385) = -0.4328909213e+01_dp
      llnh(386) = -0.4333223786e+01_dp
      llnh(387) = -0.4337533117e+01_dp
      llnh(388) = -0.4341837175e+01_dp
      llnh(389) = -0.4346135928e+01_dp
      llnh(390) = -0.4350429387e+01_dp
      llnh(391) = -0.4354717544e+01_dp
      llnh(392) = -0.4359000351e+01_dp
      llnh(393) = -0.4363277792e+01_dp
      llnh(394) = -0.4367549884e+01_dp
      llnh(395) = -0.4371816587e+01_dp
      llnh(396) = -0.4376077888e+01_dp
      llnh(397) = -0.4380333776e+01_dp
      llnh(398) = -0.4384584234e+01_dp
      llnh(399) = -0.4388829242e+01_dp
      llnh(400) = -0.4393068797e+01_dp
      llnh(401) = -0.4397302881e+01_dp
      llnh(402) = -0.4401531470e+01_dp
      llnh(403) = -0.4405754548e+01_dp
      llnh(404) = -0.4409972120e+01_dp
      llnh(405) = -0.4414184146e+01_dp
      llnh(406) = -0.4418390628e+01_dp
      llnh(407) = -0.4422591551e+01_dp
      llnh(408) = -0.4426786883e+01_dp
      llnh(409) = -0.4430976616e+01_dp
      llnh(410) = -0.4435160749e+01_dp
      llnh(411) = -0.4439339273e+01_dp
      llnh(412) = -0.4443512157e+01_dp
      llnh(413) = -0.4447679385e+01_dp
      llnh(414) = -0.4451840955e+01_dp
      llnh(415) = -0.4455997001e+01_dp
      llnh(416) = -0.4460147471e+01_dp
      llnh(417) = -0.4464291999e+01_dp
      llnh(418) = -0.4468430796e+01_dp
      llnh(419) = -0.4472563895e+01_dp
      llnh(420) = -0.4476691221e+01_dp
      llnh(421) = -0.4480812807e+01_dp
      llnh(422) = -0.4484928622e+01_dp
      llnh(423) = -0.4489038649e+01_dp
      llnh(424) = -0.4493142872e+01_dp
      llnh(425) = -0.4497241313e+01_dp
      llnh(426) = -0.4501333913e+01_dp
      llnh(427) = -0.4505420682e+01_dp
      llnh(428) = -0.4509501609e+01_dp
      llnh(429) = -0.4513576672e+01_dp
      llnh(430) = -0.4517645867e+01_dp
      llnh(431) = -0.4521709169e+01_dp
      llnh(432) = -0.4525766592e+01_dp
      llnh(433) = -0.4529818089e+01_dp
      llnh(434) = -0.4533863683e+01_dp
      llnh(435) = -0.4537903321e+01_dp
      llnh(436) = -0.4541938182e+01_dp
      llnh(437) = -0.4545970060e+01_dp
      llnh(438) = -0.4549992311e+01_dp
      llnh(439) = -0.4554008601e+01_dp
      llnh(440) = -0.4558018930e+01_dp
      llnh(441) = -0.4562023294e+01_dp
      llnh(442) = -0.4566021693e+01_dp
      llnh(443) = -0.4570014092e+01_dp
      llnh(444) = -0.4574000505e+01_dp
      llnh(445) = -0.4577980892e+01_dp
      llnh(446) = -0.4581955270e+01_dp
      llnh(447) = -0.4585923636e+01_dp
      llnh(448) = -0.4589885948e+01_dp
      llnh(449) = -0.4593842227e+01_dp
      llnh(450) = -0.4597792434e+01_dp
      llnh(451) = -0.4601736614e+01_dp
      llnh(452) = -0.4605674684e+01_dp
      llnh(453) = -0.4609606692e+01_dp
      llnh(454) = -0.4613532603e+01_dp
      llnh(455) = -0.4617452421e+01_dp
      llnh(456) = -0.4621366116e+01_dp
      llnh(457) = -0.4625273717e+01_dp
      llnh(458) = -0.4629175176e+01_dp
      llnh(459) = -0.4633070515e+01_dp
      llnh(460) = -0.4636959727e+01_dp
      llnh(461) = -0.4640842768e+01_dp
      llnh(462) = -0.4644719655e+01_dp
      llnh(463) = -0.4648590372e+01_dp
      llnh(464) = -0.4652454903e+01_dp
      llnh(465) = -0.4656313259e+01_dp
      llnh(466) = -0.4660165448e+01_dp
      llnh(467) = -0.4664011409e+01_dp
      llnh(468) = -0.4667851182e+01_dp
      llnh(469) = -0.4671684743e+01_dp
      llnh(470) = -0.4675512056e+01_dp
      llnh(471) = -0.4679333175e+01_dp
      llnh(472) = -0.4683148017e+01_dp
      llnh(473) = -0.4686956657e+01_dp
      llnh(474) = -0.4690759028e+01_dp
      llnh(475) = -0.4694555138e+01_dp
      llnh(476) = -0.4698344998e+01_dp
      llnh(477) = -0.4702128578e+01_dp
      llnh(478) = -0.4705905867e+01_dp
      llnh(479) = -0.4709676879e+01_dp
      llnh(480) = -0.4713441602e+01_dp
      llnh(481) = -0.4717200013e+01_dp
      llnh(482) = -0.4720952106e+01_dp
      llnh(483) = -0.4724697898e+01_dp
      llnh(484) = -0.4728437390e+01_dp
      llnh(485) = -0.4732170536e+01_dp
      llnh(486) = -0.4735897325e+01_dp
      llnh(487) = -0.4739617794e+01_dp
      llnh(488) = -0.4743331898e+01_dp
      llnh(489) = -0.4747041742e+01_dp
      llnh(490) = -0.4750747006e+01_dp
      llnh(491) = -0.4754442562e+01_dp
      llnh(492) = -0.4758131783e+01_dp
      llnh(493) = -0.4761815020e+01_dp
      llnh(494) = -0.4765491574e+01_dp
      llnh(495) = -0.4769161760e+01_dp
      llnh(496) = -0.4772825594e+01_dp
      llnh(497) = -0.4776483086e+01_dp
      llnh(498) = -0.4780134175e+01_dp
      llnh(499) = -0.4783778934e+01_dp
      llnh(500) = -0.4787417346e+01_dp
      llnh(501) = -0.4791049345e+01_dp
      llnh(502) = -0.4794674951e+01_dp
      llnh(503) = -0.4798294201e+01_dp
      llnh(504) = -0.4801907068e+01_dp
      llnh(505) = -0.4805513523e+01_dp
      llnh(506) = -0.4809113591e+01_dp
      llnh(507) = -0.4812707285e+01_dp
      llnh(508) = -0.4816294557e+01_dp
      llnh(509) = -0.4819875456e+01_dp
      llnh(510) = -0.4823449917e+01_dp
      llnh(511) = -0.4827017975e+01_dp
      llnh(512) = -0.4830579617e+01_dp
      llnh(513) = -0.4834134846e+01_dp
      llnh(514) = -0.4837683664e+01_dp
      llnh(515) = -0.4841226070e+01_dp
      llnh(516) = -0.4844762004e+01_dp
      llnh(517) = -0.4848291562e+01_dp
      llnh(518) = -0.4851814662e+01_dp
      llnh(519) = -0.4855331349e+01_dp
      llnh(520) = -0.4858841570e+01_dp
      llnh(521) = -0.4862345359e+01_dp
      llnh(522) = -0.4865842727e+01_dp
      llnh(523) = -0.4869333622e+01_dp
      llnh(524) = -0.4872818085e+01_dp
      llnh(525) = -0.4876296108e+01_dp
      llnh(526) = -0.4879767676e+01_dp
      llnh(527) = -0.4883232788e+01_dp
      llnh(528) = -0.4886691441e+01_dp
      llnh(529) = -0.4890143646e+01_dp
      llnh(530) = -0.4893589383e+01_dp
      llnh(531) = -0.4897028646e+01_dp
      llnh(532) = -0.4900461459e+01_dp
      llnh(533) = -0.4903887800e+01_dp
      llnh(534) = -0.4907307694e+01_dp
      llnh(535) = -0.4910721080e+01_dp
      llnh(536) = -0.4914128000e+01_dp
      llnh(537) = -0.4917528444e+01_dp
      llnh(538) = -0.4920922416e+01_dp
      llnh(539) = -0.4924309950e+01_dp
      llnh(540) = -0.4927690936e+01_dp
      llnh(541) = -0.4931065502e+01_dp
      llnh(542) = -0.4934433516e+01_dp
      llnh(543) = -0.4937797571e+01_dp
      llnh(544) = -0.4941156429e+01_dp
      llnh(545) = -0.4944505566e+01_dp
      llnh(546) = -0.4947848246e+01_dp
      llnh(547) = -0.4951184474e+01_dp
      llnh(548) = -0.4954514217e+01_dp
      llnh(549) = -0.4957837546e+01_dp
      llnh(550) = -0.4961154421e+01_dp
      llnh(551) = -0.4964464816e+01_dp
      llnh(552) = -0.4967768764e+01_dp
      llnh(553) = -0.4971066281e+01_dp
      llnh(554) = -0.4974357343e+01_dp
      llnh(555) = -0.4977641966e+01_dp
      llnh(556) = -0.4980920106e+01_dp
      llnh(557) = -0.4984191792e+01_dp
      llnh(558) = -0.4987457081e+01_dp
      llnh(559) = -0.4990715853e+01_dp
      llnh(560) = -0.4993968216e+01_dp
      llnh(561) = -0.4997214147e+01_dp
      llnh(562) = -0.5000453623e+01_dp
      llnh(563) = -0.5003686648e+01_dp
      llnh(564) = -0.5006913204e+01_dp
      llnh(565) = -0.5010133371e+01_dp
      llnh(566) = -0.5013347076e+01_dp
      llnh(567) = -0.5016554357e+01_dp
      llnh(568) = -0.5019755183e+01_dp
      llnh(569) = -0.5022949569e+01_dp
      llnh(570) = -0.5026137554e+01_dp
      llnh(571) = -0.5029319047e+01_dp
      llnh(572) = -0.5032494140e+01_dp
      llnh(573) = -0.5035663150e+01_dp
      llnh(574) = -0.5038825376e+01_dp
      llnh(575) = -0.5041981220e+01_dp
      llnh(576) = -0.5045130584e+01_dp
      llnh(577) = -0.5048273555e+01_dp
      llnh(578) = -0.5051410090e+01_dp
      llnh(579) = -0.5054540239e+01_dp
      llnh(580) = -0.5057663933e+01_dp
      llnh(581) = -0.5060781203e+01_dp
      llnh(582) = -0.5063892095e+01_dp
      llnh(583) = -0.5066996544e+01_dp
      llnh(584) = -0.5070094560e+01_dp
      llnh(585) = -0.5073186208e+01_dp
      llnh(586) = -0.5076271426e+01_dp
      llnh(587) = -0.5079350236e+01_dp
      llnh(588) = -0.5082422642e+01_dp
      llnh(589) = -0.5085488664e+01_dp
      llnh(590) = -0.5088548256e+01_dp
      llnh(591) = -0.5091601476e+01_dp
      llnh(592) = -0.5094648290e+01_dp
      llnh(593) = -0.5097688683e+01_dp
      llnh(594) = -0.5100722685e+01_dp
      llnh(595) = -0.5103750327e+01_dp
      llnh(596) = -0.5106771540e+01_dp
      llnh(597) = -0.5109786384e+01_dp
      llnh(598) = -0.5112799670e+01_dp
      llnh(599) = -0.5115803150e+01_dp
      llnh(600) = -0.5118799395e+01_dp
      llnh(601) = -0.5121789292e+01_dp
      llnh(602) = -0.5124772858e+01_dp
      llnh(603) = -0.5127750116e+01_dp
      llnh(604) = -0.5130721005e+01_dp
      llnh(605) = -0.5133685604e+01_dp
      llnh(606) = -0.5136643859e+01_dp
      llnh(607) = -0.5139595762e+01_dp
      llnh(608) = -0.5142541391e+01_dp
      llnh(609) = -0.5145480724e+01_dp
      llnh(610) = -0.5148413704e+01_dp
      llnh(611) = -0.5151340435e+01_dp
      llnh(612) = -0.5154260814e+01_dp
      llnh(613) = -0.5157174876e+01_dp
      llnh(614) = -0.5160082667e+01_dp
      llnh(615) = -0.5162984187e+01_dp
      llnh(616) = -0.5165879425e+01_dp
      llnh(617) = -0.5168768373e+01_dp
      llnh(618) = -0.5171651043e+01_dp
      llnh(619) = -0.5174527467e+01_dp
      llnh(620) = -0.5177397602e+01_dp
      llnh(621) = -0.5180261493e+01_dp
      llnh(622) = -0.5183119109e+01_dp
      llnh(623) = -0.5185970497e+01_dp
      llnh(624) = -0.5188815635e+01_dp
      llnh(625) = -0.5191654520e+01_dp
      llnh(626) = -0.5194487149e+01_dp
      llnh(627) = -0.5197313595e+01_dp
      llnh(628) = -0.5200133751e+01_dp
      llnh(629) = -0.5202947738e+01_dp
      llnh(630) = -0.5205755473e+01_dp
      llnh(631) = -0.5208556983e+01_dp
      llnh(632) = -0.5211352324e+01_dp
      llnh(633) = -0.5214141419e+01_dp
      llnh(634) = -0.5216924343e+01_dp
      llnh(635) = -0.5219701054e+01_dp
      llnh(636) = -0.5222471563e+01_dp
      llnh(637) = -0.5225235884e+01_dp
      llnh(638) = -0.5227994025e+01_dp
      llnh(639) = -0.5230746030e+01_dp
      llnh(640) = -0.5233491832e+01_dp
      llnh(641) = -0.5236231461e+01_dp
      llnh(642) = -0.5238964942e+01_dp
      llnh(643) = -0.5241692271e+01_dp
      llnh(644) = -0.5244413412e+01_dp
      llnh(645) = -0.5247128424e+01_dp
      llnh(646) = -0.5249837269e+01_dp
      llnh(647) = -0.5252540015e+01_dp
      llnh(648) = -0.5255236616e+01_dp
      llnh(649) = -0.5257927067e+01_dp
      llnh(650) = -0.5260611388e+01_dp
      llnh(651) = -0.5263289623e+01_dp
      llnh(652) = -0.5265961712e+01_dp
      llnh(653) = -0.5268633068e+01_dp
      llnh(654) = -0.5271293569e+01_dp
      llnh(655) = -0.5273947990e+01_dp
      llnh(656) = -0.5276596206e+01_dp
      llnh(657) = -0.5279238426e+01_dp
      llnh(658) = -0.5281874571e+01_dp
      llnh(659) = -0.5284504655e+01_dp
      llnh(660) = -0.5287128701e+01_dp
      llnh(661) = -0.5289746751e+01_dp
      llnh(662) = -0.5292358757e+01_dp
      llnh(663) = -0.5294964725e+01_dp
      llnh(664) = -0.5297564727e+01_dp
      llnh(665) = -0.5300158699e+01_dp
      llnh(666) = -0.5302746668e+01_dp
      llnh(667) = -0.5305328668e+01_dp
      llnh(668) = -0.5307904687e+01_dp
      llnh(669) = -0.5310474675e+01_dp
      llnh(670) = -0.5313038743e+01_dp
      llnh(671) = -0.5315596857e+01_dp
      llnh(672) = -0.5318148939e+01_dp
      llnh(673) = -0.5320695164e+01_dp
      llnh(674) = -0.5323235360e+01_dp
      llnh(675) = -0.5325769682e+01_dp
      llnh(676) = -0.5328298064e+01_dp
      llnh(677) = -0.5330820487e+01_dp
      llnh(678) = -0.5333337033e+01_dp
      llnh(679) = -0.5335847660e+01_dp
      llnh(680) = -0.5338352356e+01_dp
      llnh(681) = -0.5340851158e+01_dp
      llnh(682) = -0.5343344088e+01_dp
      llnh(683) = -0.5345831113e+01_dp
      llnh(684) = -0.5348312329e+01_dp
      llnh(685) = -0.5350787595e+01_dp
      llnh(686) = -0.5353257028e+01_dp
      llnh(687) = -0.5355720597e+01_dp
      llnh(688) = -0.5358178333e+01_dp
      llnh(689) = -0.5360630227e+01_dp
      llnh(690) = -0.5363076250e+01_dp
      llnh(691) = -0.5365516440e+01_dp
      llnh(692) = -0.5367950847e+01_dp
      llnh(693) = -0.5370379412e+01_dp
      llnh(694) = -0.5372802182e+01_dp
      llnh(695) = -0.5375219136e+01_dp
      llnh(696) = -0.5377630301e+01_dp
      llnh(697) = -0.5380035657e+01_dp
      llnh(698) = -0.5382435244e+01_dp
      llnh(699) = -0.5384829094e+01_dp
      llnh(700) = -0.5387217124e+01_dp
      llnh(701) = -0.5389599434e+01_dp
      llnh(702) = -0.5391975954e+01_dp
      llnh(703) = -0.5394346756e+01_dp
      llnh(704) = -0.5396711759e+01_dp
      llnh(705) = -0.5399071078e+01_dp
      llnh(706) = -0.5401424638e+01_dp
      llnh(707) = -0.5403772479e+01_dp
      llnh(708) = -0.5406119229e+01_dp
      llnh(709) = -0.5408456158e+01_dp
      llnh(710) = -0.5410787342e+01_dp
      llnh(711) = -0.5413112829e+01_dp
      llnh(712) = -0.5415432725e+01_dp
      llnh(713) = -0.5417746886e+01_dp
      llnh(714) = -0.5420055451e+01_dp
      llnh(715) = -0.5422358379e+01_dp
      llnh(716) = -0.5424655695e+01_dp
      llnh(717) = -0.5426947346e+01_dp
      llnh(718) = -0.5429233415e+01_dp
      llnh(719) = -0.5431513881e+01_dp
      llnh(720) = -0.5433788773e+01_dp
      llnh(721) = -0.5436058059e+01_dp
      llnh(722) = -0.5438321777e+01_dp
      llnh(723) = -0.5440579909e+01_dp
      llnh(724) = -0.5442832493e+01_dp
      llnh(725) = -0.5445079538e+01_dp
      llnh(726) = -0.5447321025e+01_dp
      llnh(727) = -0.5449556959e+01_dp
      llnh(728) = -0.5451787383e+01_dp
      llnh(729) = -0.5454012244e+01_dp
      llnh(730) = -0.5456231645e+01_dp
      llnh(731) = -0.5458445490e+01_dp
      llnh(732) = -0.5460653885e+01_dp
      llnh(733) = -0.5462856815e+01_dp
      llnh(734) = -0.5465054208e+01_dp
      llnh(735) = -0.5467246209e+01_dp
      llnh(736) = -0.5469432635e+01_dp
      llnh(737) = -0.5471613649e+01_dp
      llnh(738) = -0.5473789425e+01_dp
      llnh(739) = -0.5475959565e+01_dp
      llnh(740) = -0.5478124308e+01_dp
      llnh(741) = -0.5480283586e+01_dp
      llnh(742) = -0.5482437461e+01_dp
      llnh(743) = -0.5484585919e+01_dp
      llnh(744) = -0.5486728976e+01_dp
      llnh(745) = -0.5488866669e+01_dp
      llnh(746) = -0.5490998937e+01_dp
      llnh(747) = -0.5493125867e+01_dp
      llnh(748) = -0.5495247398e+01_dp
      llnh(749) = -0.5497363593e+01_dp
      llnh(750) = -0.5499474396e+01_dp
      llnh(751) = -0.5501579935e+01_dp
      llnh(752) = -0.5503680091e+01_dp
      llnh(753) = -0.5505774872e+01_dp
      llnh(754) = -0.5507864419e+01_dp
      llnh(755) = -0.5509948597e+01_dp
      llnh(756) = -0.5512027498e+01_dp
      llnh(757) = -0.5514101090e+01_dp
      llnh(758) = -0.5516169407e+01_dp
      llnh(759) = -0.5518232431e+01_dp
      llnh(760) = -0.5520290171e+01_dp
      llnh(761) = -0.5522342671e+01_dp
      llnh(762) = -0.5524389920e+01_dp
      llnh(763) = -0.5526435070e+01_dp
      llnh(764) = -0.5528472978e+01_dp
      llnh(765) = -0.5530504862e+01_dp
      llnh(766) = -0.5532531569e+01_dp
      llnh(767) = -0.5534553070e+01_dp
      llnh(768) = -0.5536569379e+01_dp
      llnh(769) = -0.5538580560e+01_dp
      llnh(770) = -0.5540586509e+01_dp
      llnh(771) = -0.5542587346e+01_dp
      llnh(772) = -0.5544583018e+01_dp
      llnh(773) = -0.5546573556e+01_dp
      llnh(774) = -0.5548558957e+01_dp
      llnh(775) = -0.5550539231e+01_dp
      llnh(776) = -0.5552514398e+01_dp
      llnh(777) = -0.5554484460e+01_dp
      llnh(778) = -0.5556449473e+01_dp
      llnh(779) = -0.5558409336e+01_dp
      llnh(780) = -0.5560364185e+01_dp
      llnh(781) = -0.5562313913e+01_dp
      llnh(782) = -0.5564258612e+01_dp
      llnh(783) = -0.5566198240e+01_dp
      llnh(784) = -0.5568132825e+01_dp
      llnh(785) = -0.5570062431e+01_dp
      llnh(786) = -0.5571986955e+01_dp
      llnh(787) = -0.5573906479e+01_dp
      llnh(788) = -0.5575821017e+01_dp
      llnh(789) = -0.5577730521e+01_dp
      llnh(790) = -0.5579635046e+01_dp
      llnh(791) = -0.5581534623e+01_dp
      llnh(792) = -0.5583429246e+01_dp
      llnh(793) = -0.5585318843e+01_dp
      llnh(794) = -0.5587203534e+01_dp
      llnh(795) = -0.5589083215e+01_dp
      llnh(796) = -0.5590958026e+01_dp
      llnh(797) = -0.5592827913e+01_dp
      llnh(798) = -0.5594692837e+01_dp
      llnh(799) = -0.5596552884e+01_dp
      llnh(800) = -0.5598408013e+01_dp
      llnh(801) = -0.5600258261e+01_dp
      llnh(802) = -0.5602103603e+01_dp
      llnh(803) = -0.5603944084e+01_dp
      llnh(804) = -0.5605779692e+01_dp
      llnh(805) = -0.5607610469e+01_dp
      llnh(806) = -0.5609436368e+01_dp
      llnh(807) = -0.5611257443e+01_dp
      llnh(808) = -0.5613073624e+01_dp
      llnh(809) = -0.5614885059e+01_dp
      llnh(810) = -0.5616691617e+01_dp
      llnh(811) = -0.5618493398e+01_dp
      llnh(812) = -0.5620290369e+01_dp
      llnh(813) = -0.5622082588e+01_dp
      llnh(814) = -0.5623870004e+01_dp
      llnh(815) = -0.5625652635e+01_dp
      llnh(816) = -0.5627430515e+01_dp
      llnh(817) = -0.5629203619e+01_dp
      llnh(818) = -0.5630973564e+01_dp
      llnh(819) = -0.5632739215e+01_dp
      llnh(820) = -0.5634498385e+01_dp
      llnh(821) = -0.5636252874e+01_dp
      llnh(822) = -0.5638002692e+01_dp
      llnh(823) = -0.5639747990e+01_dp
      llnh(824) = -0.5641488433e+01_dp
      llnh(825) = -0.5643224238e+01_dp
      llnh(826) = -0.5644955371e+01_dp
      llnh(827) = -0.5646681860e+01_dp
      llnh(828) = -0.5648403688e+01_dp
      llnh(829) = -0.5650120894e+01_dp
      llnh(830) = -0.5651833499e+01_dp
      llnh(831) = -0.5653541488e+01_dp
      llnh(832) = -0.5655244889e+01_dp
      llnh(833) = -0.5656943648e+01_dp
      llnh(834) = -0.5658637872e+01_dp
      llnh(835) = -0.5660327454e+01_dp
      llnh(836) = -0.5662012541e+01_dp
      llnh(837) = -0.5663693098e+01_dp
      llnh(838) = -0.5665369003e+01_dp
      llnh(839) = -0.5667040432e+01_dp
      llnh(840) = -0.5668707291e+01_dp
      llnh(841) = -0.5670369673e+01_dp
      llnh(842) = -0.5672027532e+01_dp
      llnh(843) = -0.5673680874e+01_dp
      llnh(844) = -0.5675329711e+01_dp
      llnh(845) = -0.5676974102e+01_dp
      llnh(846) = -0.5678614009e+01_dp
      llnh(847) = -0.5680249399e+01_dp
      llnh(848) = -0.5681880401e+01_dp
      llnh(849) = -0.5683506937e+01_dp
      llnh(850) = -0.5685128983e+01_dp
      llnh(851) = -0.5686746614e+01_dp
      llnh(852) = -0.5688359836e+01_dp
      llnh(853) = -0.5689968657e+01_dp
      llnh(854) = -0.5691573040e+01_dp
      llnh(855) = -0.5693173037e+01_dp
      llnh(856) = -0.5694768667e+01_dp
      llnh(857) = -0.5696359900e+01_dp
      llnh(858) = -0.5697946740e+01_dp
      llnh(859) = -0.5699529279e+01_dp
      llnh(860) = -0.5701107407e+01_dp
      llnh(861) = -0.5702681211e+01_dp
      llnh(862) = -0.5704250662e+01_dp
      llnh(863) = -0.5705815820e+01_dp
      llnh(864) = -0.5707376645e+01_dp
      llnh(865) = -0.5708933156e+01_dp
      llnh(866) = -0.5710485372e+01_dp
      llnh(867) = -0.5712033286e+01_dp
      llnh(868) = -0.5713576887e+01_dp
      llnh(869) = -0.5715116238e+01_dp
      llnh(870) = -0.5716651343e+01_dp
      llnh(871) = -0.5718182133e+01_dp
      llnh(872) = -0.5719708718e+01_dp
      llnh(873) = -0.5721231370e+01_dp
      llnh(874) = -0.5722752079e+01_dp
      llnh(875) = -0.5724266184e+01_dp
      llnh(876) = -0.5725776091e+01_dp
      llnh(877) = -0.5727281862e+01_dp
      llnh(878) = -0.5728783383e+01_dp
      llnh(879) = -0.5730280793e+01_dp
      llnh(880) = -0.5731774031e+01_dp
      llnh(881) = -0.5733263026e+01_dp
      llnh(882) = -0.5734747963e+01_dp
      llnh(883) = -0.5736228729e+01_dp
      llnh(884) = -0.5737705360e+01_dp
      llnh(885) = -0.5739177941e+01_dp
      llnh(886) = -0.5740646299e+01_dp
      llnh(887) = -0.5742110595e+01_dp
      llnh(888) = -0.5743570865e+01_dp
      llnh(889) = -0.5745026960e+01_dp
      llnh(890) = -0.5746479033e+01_dp
      llnh(891) = -0.5747927018e+01_dp
      llnh(892) = -0.5749370943e+01_dp
      llnh(893) = -0.5750810827e+01_dp
      llnh(894) = -0.5752246701e+01_dp
      llnh(895) = -0.5753678496e+01_dp
      llnh(896) = -0.5755106289e+01_dp
      llnh(897) = -0.5756530074e+01_dp
      llnh(898) = -0.5757949836e+01_dp
      llnh(899) = -0.5759365629e+01_dp
      llnh(900) = -0.5760777429e+01_dp
      llnh(901) = -0.5762185269e+01_dp
      llnh(902) = -0.5763589050e+01_dp
      llnh(903) = -0.5764989015e+01_dp
      llnh(904) = -0.5766384906e+01_dp
      llnh(905) = -0.5767776906e+01_dp
      llnh(906) = -0.5769164942e+01_dp
      llnh(907) = -0.5770549045e+01_dp
      llnh(908) = -0.5771929251e+01_dp
      llnh(909) = -0.5773305592e+01_dp
      llnh(910) = -0.5774678021e+01_dp
      llnh(911) = -0.5776046522e+01_dp
      llnh(912) = -0.5777411181e+01_dp
      llnh(913) = -0.5778771903e+01_dp
      llnh(914) = -0.5780128791e+01_dp
      llnh(915) = -0.5781481778e+01_dp
      llnh(916) = -0.5782830973e+01_dp
      llnh(917) = -0.5784176296e+01_dp
      llnh(918) = -0.5785517731e+01_dp
      llnh(919) = -0.5786855440e+01_dp
      llnh(920) = -0.5788189236e+01_dp
      llnh(921) = -0.5789519318e+01_dp
      llnh(922) = -0.5790845543e+01_dp
      llnh(923) = -0.5792167984e+01_dp
      llnh(924) = -0.5793486673e+01_dp
      llnh(925) = -0.5794801520e+01_dp
      llnh(926) = -0.5796112641e+01_dp
      llnh(927) = -0.5797420004e+01_dp
      llnh(928) = -0.5798723616e+01_dp
      llnh(929) = -0.5800025860e+01_dp
      llnh(930) = -0.5801322181e+01_dp
      llnh(931) = -0.5802614805e+01_dp
      llnh(932) = -0.5803903742e+01_dp
      llnh(933) = -0.5805188958e+01_dp
      llnh(934) = -0.5806470481e+01_dp
      llnh(935) = -0.5807748369e+01_dp
      llnh(936) = -0.5809022517e+01_dp
      llnh(937) = -0.5810293038e+01_dp
      llnh(938) = -0.5811559908e+01_dp
      llnh(939) = -0.5812823154e+01_dp
      llnh(940) = -0.5814082757e+01_dp
      llnh(941) = -0.5815338711e+01_dp
      llnh(942) = -0.5816591047e+01_dp
      llnh(943) = -0.5817839765e+01_dp
      llnh(944) = -0.5819084925e+01_dp
      llnh(945) = -0.5820326443e+01_dp
      llnh(946) = -0.5821564407e+01_dp
      llnh(947) = -0.5822798792e+01_dp
      llnh(948) = -0.5824029580e+01_dp
      llnh(949) = -0.5825256834e+01_dp
      llnh(950) = -0.5826480495e+01_dp
      llnh(951) = -0.5827700679e+01_dp
      llnh(952) = -0.5828917318e+01_dp
      llnh(953) = -0.5830130356e+01_dp
      llnh(954) = -0.5831339955e+01_dp
      llnh(955) = -0.5832546036e+01_dp
      llnh(956) = -0.5833748572e+01_dp
      llnh(957) = -0.5834947646e+01_dp
      llnh(958) = -0.5836143208e+01_dp
      llnh(959) = -0.5837335326e+01_dp
      llnh(960) = -0.5838523975e+01_dp
      llnh(961) = -0.5839709120e+01_dp
      llnh(962) = -0.5840890822e+01_dp
      llnh(963) = -0.5842069117e+01_dp
      llnh(964) = -0.5843243948e+01_dp
      llnh(965) = -0.5844415380e+01_dp
      llnh(966) = -0.5845583345e+01_dp
      llnh(967) = -0.5846747902e+01_dp
      llnh(968) = -0.5847909107e+01_dp
      llnh(969) = -0.5849066883e+01_dp
      llnh(970) = -0.5850221245e+01_dp
      llnh(971) = -0.5851372233e+01_dp
      llnh(972) = -0.5852519933e+01_dp
      llnh(973) = -0.5853664111e+01_dp
      llnh(974) = -0.5854805079e+01_dp
      llnh(975) = -0.5855942632e+01_dp
      llnh(976) = -0.5857076840e+01_dp
      llnh(977) = -0.5858207705e+01_dp
      llnh(978) = -0.5859335263e+01_dp
      llnh(979) = -0.5860459528e+01_dp
      llnh(980) = -0.5861580446e+01_dp
      llnh(981) = -0.5862698053e+01_dp
      llnh(982) = -0.5863812452e+01_dp
      llnh(983) = -0.5864923445e+01_dp
      llnh(984) = -0.5866033059e+01_dp
      llnh(985) = -0.5867137708e+01_dp
      llnh(986) = -0.5868239098e+01_dp
      llnh(987) = -0.5869337290e+01_dp
      llnh(988) = -0.5870432164e+01_dp
      llnh(989) = -0.5871523895e+01_dp
      llnh(990) = -0.5872612338e+01_dp
      llnh(991) = -0.5873697645e+01_dp
      llnh(992) = -0.5874779683e+01_dp
      llnh(993) = -0.5875858528e+01_dp
      llnh(994) = -0.5876934214e+01_dp
      llnh(995) = -0.5878006676e+01_dp
      llnh(996) = -0.5879076002e+01_dp
      llnh(997) = -0.5880142202e+01_dp
      llnh(998) = -0.5881205128e+01_dp
      llnh(999) = -0.5882264991e+01_dp
      llnh(1000) = -0.5883321683e+01_dp
      llnh(1001) = -0.5884375272e+01_dp
      llnh(1002) = -0.5885425704e+01_dp
      llnh(1003) = -0.5886473023e+01_dp
      llnh(1004) = -0.5887517204e+01_dp
      llnh(1005) = -0.5888558321e+01_dp
      llnh(1006) = -0.5889596323e+01_dp
      llnh(1007) = -0.5890631250e+01_dp
      llnh(1008) = -0.5891663079e+01_dp
      llnh(1009) = -0.5892691857e+01_dp
      llnh(1010) = -0.5893717531e+01_dp
      llnh(1011) = -0.5894740186e+01_dp
      llnh(1012) = -0.5895759785e+01_dp
      llnh(1013) = -0.5896776265e+01_dp
      llnh(1014) = -0.5897789840e+01_dp
      llnh(1015) = -0.5898800343e+01_dp
      llnh(1016) = -0.5899807814e+01_dp
      llnh(1017) = -0.5900812292e+01_dp
      llnh(1018) = -0.5901813724e+01_dp
      llnh(1019) = -0.5902812170e+01_dp
      llnh(1020) = -0.5903807651e+01_dp
      llnh(1021) = -0.5904800116e+01_dp
      llnh(1022) = -0.5905789619e+01_dp
      llnh(1023) = -0.5906776200e+01_dp
      llnh(1024) = -0.5907759788e+01_dp

      llnk(0) = -1.0000000000e+00_dp
      llnk(1) = -0.1000000000e+01_dp
      llnk(2) = -0.3054020195e+00_dp
      llnk(3) = -0.1960294041e+00_dp
      llnk(4) = -0.1336652689e+00_dp
      llnk(5) = -0.1047066267e+00_dp
      llnk(6) = -0.9033564429e-01_dp
      llnk(7) = -0.8206984804e-01_dp
      llnk(8) = -0.7655494644e-01_dp
      llnk(9) = -0.7243844815e-01_dp
      llnk(10) = -0.6913401466e-01_dp
      llnk(11) = -0.6635869819e-01_dp
      llnk(12) = -0.6395689877e-01_dp
      llnk(13) = -0.6183296641e-01_dp
      llnk(14) = -0.5992172201e-01_dp
      llnk(15) = -0.5817772516e-01_dp
      llnk(16) = -0.5656704205e-01_dp
      llnk(17) = -0.5506474110e-01_dp
      llnk(18) = -0.5365205058e-01_dp
      llnk(19) = -0.5231479422e-01_dp
      llnk(20) = -0.5104204279e-01_dp
      llnk(21) = -0.4982562670e-01_dp
      llnk(22) = -0.4865919131e-01_dp
      llnk(23) = -0.4753764652e-01_dp
      llnk(24) = -0.4645728556e-01_dp
      llnk(25) = -0.4541483359e-01_dp
      llnk(26) = -0.4440818528e-01_dp
      llnk(27) = -0.4343487603e-01_dp
      llnk(28) = -0.4249347823e-01_dp
      llnk(29) = -0.4158232061e-01_dp
      llnk(30) = -0.4070034557e-01_dp
      llnk(31) = -0.3984645832e-01_dp
      llnk(32) = -0.3901937759e-01_dp
      llnk(33) = -0.3821827509e-01_dp
      llnk(34) = -0.3744217649e-01_dp
      llnk(35) = -0.3669034206e-01_dp
      llnk(36) = -0.3596186474e-01_dp
      llnk(37) = -0.3525594412e-01_dp
      llnk(38) = -0.3457187576e-01_dp
      llnk(39) = -0.3390882631e-01_dp
      llnk(40) = -0.3326609909e-01_dp
      llnk(41) = -0.3264299923e-01_dp
      llnk(42) = -0.3203883174e-01_dp
      llnk(43) = -0.3145293280e-01_dp
      llnk(44) = -0.3088463896e-01_dp
      llnk(45) = -0.3033334191e-01_dp
      llnk(46) = -0.2979842108e-01_dp
      llnk(47) = -0.2927929373e-01_dp
      llnk(48) = -0.2877539004e-01_dp
      llnk(49) = -0.2828615868e-01_dp
      llnk(50) = -0.2781108192e-01_dp
      llnk(51) = -0.2734963353e-01_dp
      llnk(52) = -0.2690133637e-01_dp
      llnk(53) = -0.2646570945e-01_dp
      llnk(54) = -0.2604229418e-01_dp
      llnk(55) = -0.2563066630e-01_dp
      llnk(56) = -0.2523039452e-01_dp
      llnk(57) = -0.2484107774e-01_dp
      llnk(58) = -0.2446233127e-01_dp
      llnk(59) = -0.2409377795e-01_dp
      llnk(60) = -0.2373506405e-01_dp
      llnk(61) = -0.2338584530e-01_dp
      llnk(62) = -0.2304579309e-01_dp
      llnk(63) = -0.2271459030e-01_dp
      llnk(64) = -0.2239193633e-01_dp
      llnk(65) = -0.2207753919e-01_dp
      llnk(66) = -0.2177111937e-01_dp
      llnk(67) = -0.2147240908e-01_dp
      llnk(68) = -0.2118115170e-01_dp
      llnk(69) = -0.2089710139e-01_dp
      llnk(70) = -0.2062002059e-01_dp
      llnk(71) = -0.2034968220e-01_dp
      llnk(72) = -0.2008586807e-01_dp
      llnk(73) = -0.1982836895e-01_dp
      llnk(74) = -0.1957698319e-01_dp
      llnk(75) = -0.1933151728e-01_dp
      llnk(76) = -0.1909178544e-01_dp
      llnk(77) = -0.1885760922e-01_dp
      llnk(78) = -0.1862881706e-01_dp
      llnk(79) = -0.1840524275e-01_dp
      llnk(80) = -0.1818672781e-01_dp
      llnk(81) = -0.1797312161e-01_dp
      llnk(82) = -0.1776427277e-01_dp
      llnk(83) = -0.1756004164e-01_dp
      llnk(84) = -0.1736029175e-01_dp
      llnk(85) = -0.1716489163e-01_dp
      llnk(86) = -0.1697371498e-01_dp
      llnk(87) = -0.1678663942e-01_dp
      llnk(88) = -0.1660354744e-01_dp
      llnk(89) = -0.1642432560e-01_dp
      llnk(90) = -0.1624886478e-01_dp
      llnk(91) = -0.1607705911e-01_dp
      llnk(92) = -0.1590880688e-01_dp
      llnk(93) = -0.1574400976e-01_dp
      llnk(94) = -0.1558257306e-01_dp
      llnk(95) = -0.1542440486e-01_dp
      llnk(96) = -0.1526941673e-01_dp
      llnk(97) = -0.1511752309e-01_dp
      llnk(98) = -0.1496864146e-01_dp
      llnk(99) = -0.1482269168e-01_dp
      llnk(100) = -0.1467959656e-01_dp
      llnk(101) = -0.1453928135e-01_dp
      llnk(102) = -0.1440167387e-01_dp
      llnk(103) = -0.1426670401e-01_dp
      llnk(104) = -0.1413430411e-01_dp
      llnk(105) = -0.1400440861e-01_dp
      llnk(106) = -0.1387695416e-01_dp
      llnk(107) = -0.1375187918e-01_dp
      llnk(108) = -0.1362912416e-01_dp
      llnk(109) = -0.1350863142e-01_dp
      llnk(110) = -0.1339034516e-01_dp
      llnk(111) = -0.1327421107e-01_dp
      llnk(112) = -0.1316017669e-01_dp
      llnk(113) = -0.1304819106e-01_dp
      llnk(114) = -0.1293820488e-01_dp
      llnk(115) = -0.1283017014e-01_dp
      llnk(116) = -0.1272404038e-01_dp
      llnk(117) = -0.1261977050e-01_dp
      llnk(118) = -0.1251731678e-01_dp
      llnk(119) = -0.1241663664e-01_dp
      llnk(120) = -0.1231768886e-01_dp
      llnk(121) = -0.1222043336e-01_dp
      llnk(122) = -0.1212483127e-01_dp
      llnk(123) = -0.1203084476e-01_dp
      llnk(124) = -0.1193843710e-01_dp
      llnk(125) = -0.1184757260e-01_dp
      llnk(126) = -0.1175821983e-01_dp
      llnk(127) = -0.1167033892e-01_dp
      llnk(128) = -0.1158389997e-01_dp
      llnk(129) = -0.1149887118e-01_dp
      llnk(130) = -0.1141522155e-01_dp
      llnk(131) = -0.1133292103e-01_dp
      llnk(132) = -0.1125194019e-01_dp
      llnk(133) = -0.1117225053e-01_dp
      llnk(134) = -0.1109382428e-01_dp
      llnk(135) = -0.1101663454e-01_dp
      llnk(136) = -0.1094065488e-01_dp
      llnk(137) = -0.1086585976e-01_dp
      llnk(138) = -0.1079222424e-01_dp
      llnk(139) = -0.1071972412e-01_dp
      llnk(140) = -0.1064833570e-01_dp
      llnk(141) = -0.1057803597e-01_dp
      llnk(142) = -0.1050880250e-01_dp
      llnk(143) = -0.1044061352e-01_dp
      llnk(144) = -0.1037344765e-01_dp
      llnk(145) = -0.1030728418e-01_dp
      llnk(146) = -0.1024210288e-01_dp
      llnk(147) = -0.1017788410e-01_dp
      llnk(148) = -0.1011460854e-01_dp
      llnk(149) = -0.1005225749e-01_dp
      llnk(150) = -0.9990812687e-02_dp
      llnk(151) = -0.9930256346e-02_dp
      llnk(152) = -0.9870571027e-02_dp
      llnk(153) = -0.9811739803e-02_dp
      llnk(154) = -0.9753746115e-02_dp
      llnk(155) = -0.9696573861e-02_dp
      llnk(156) = -0.9640207245e-02_dp
      llnk(157) = -0.9584630905e-02_dp
      llnk(158) = -0.9529829829e-02_dp
      llnk(159) = -0.9475790189e-02_dp
      llnk(160) = -0.9422496073e-02_dp
      llnk(161) = -0.9369934305e-02_dp
      llnk(162) = -0.9318091242e-02_dp
      llnk(163) = -0.9266953580e-02_dp
      llnk(164) = -0.9216508278e-02_dp
      llnk(165) = -0.9166742633e-02_dp
      llnk(166) = -0.9117644217e-02_dp
      llnk(167) = -0.9069200890e-02_dp
      llnk(168) = -0.9021400853e-02_dp
      llnk(169) = -0.8974232435e-02_dp
      llnk(170) = -0.8927684326e-02_dp
      llnk(171) = -0.8881745447e-02_dp
      llnk(172) = -0.8836405029e-02_dp
      llnk(173) = -0.8791653407e-02_dp
      llnk(174) = -0.8747481667e-02_dp
      llnk(175) = -0.8703874181e-02_dp
      llnk(176) = -0.8660824198e-02_dp
      llnk(177) = -0.8618321971e-02_dp
      llnk(178) = -0.8576358051e-02_dp
      llnk(179) = -0.8534923154e-02_dp
      llnk(180) = -0.8494008256e-02_dp
      llnk(181) = -0.8453604422e-02_dp
      llnk(182) = -0.8413702992e-02_dp
      llnk(183) = -0.8374295452e-02_dp
      llnk(184) = -0.8335373508e-02_dp
      llnk(185) = -0.8296928978e-02_dp
      llnk(186) = -0.8258953895e-02_dp
      llnk(187) = -0.8221440446e-02_dp
      llnk(188) = -0.8184381010e-02_dp
      llnk(189) = -0.8147768058e-02_dp
      llnk(190) = -0.8111594272e-02_dp
      llnk(191) = -0.8075852456e-02_dp
      llnk(192) = -0.8040535598e-02_dp
      llnk(193) = -0.8005636771e-02_dp
      llnk(194) = -0.7971149225e-02_dp
      llnk(195) = -0.7937066335e-02_dp
      llnk(196) = -0.7903381636e-02_dp
      llnk(197) = -0.7870088750e-02_dp
      llnk(198) = -0.7837181442e-02_dp
      llnk(199) = -0.7804653597e-02_dp
      llnk(200) = -0.7772499254e-02_dp
      llnk(201) = -0.7740712516e-02_dp
      llnk(202) = -0.7709287634e-02_dp
      llnk(203) = -0.7678218959e-02_dp
      llnk(204) = -0.7647500968e-02_dp
      llnk(205) = -0.7617128214e-02_dp
      llnk(206) = -0.7587095379e-02_dp
      llnk(207) = -0.7557397242e-02_dp
      llnk(208) = -0.7528028665e-02_dp
      llnk(209) = -0.7498984668e-02_dp
      llnk(210) = -0.7470260263e-02_dp
      llnk(211) = -0.7441850636e-02_dp
      llnk(212) = -0.7413751027e-02_dp
      llnk(213) = -0.7385956807e-02_dp
      llnk(214) = -0.7358463369e-02_dp
      llnk(215) = -0.7331266234e-02_dp
      llnk(216) = -0.7304360991e-02_dp
      llnk(217) = -0.7277743343e-02_dp
      llnk(218) = -0.7251409005e-02_dp
      llnk(219) = -0.7225353835e-02_dp
      llnk(220) = -0.7199573711e-02_dp
      llnk(221) = -0.7174064656e-02_dp
      llnk(222) = -0.7148822685e-02_dp
      llnk(223) = -0.7123843938e-02_dp
      llnk(224) = -0.7099129566e-02_dp
      llnk(225) = -0.7074666551e-02_dp
      llnk(226) = -0.7050455306e-02_dp
      llnk(227) = -0.7026492490e-02_dp
      llnk(228) = -0.7002774540e-02_dp
      llnk(229) = -0.6979298005e-02_dp
      llnk(230) = -0.6956059431e-02_dp
      llnk(231) = -0.6933055469e-02_dp
      llnk(232) = -0.6910282814e-02_dp
      llnk(233) = -0.6887738228e-02_dp
      llnk(234) = -0.6865418504e-02_dp
      llnk(235) = -0.6843320529e-02_dp
      llnk(236) = -0.6821441187e-02_dp
      llnk(237) = -0.6799777491e-02_dp
      llnk(238) = -0.6778326425e-02_dp
      llnk(239) = -0.6757085071e-02_dp
      llnk(240) = -0.6736050549e-02_dp
      llnk(241) = -0.6715220049e-02_dp
      llnk(242) = -0.6694590762e-02_dp
      llnk(243) = -0.6674159952e-02_dp
      llnk(244) = -0.6653925199e-02_dp
      llnk(245) = -0.6633883871e-02_dp
      llnk(246) = -0.6614032563e-02_dp
      llnk(247) = -0.6594369232e-02_dp
      llnk(248) = -0.6574891356e-02_dp
      llnk(249) = -0.6555596461e-02_dp
      llnk(250) = -0.6536482218e-02_dp
      llnk(251) = -0.6517546015e-02_dp
      llnk(252) = -0.6498785614e-02_dp
      llnk(253) = -0.6480198683e-02_dp
      llnk(254) = -0.6461782835e-02_dp
      llnk(255) = -0.6443535971e-02_dp
      llnk(256) = -0.6425455846e-02_dp
      llnk(257) = -0.6407540263e-02_dp
      llnk(258) = -0.6389786969e-02_dp
      llnk(259) = -0.6372194033e-02_dp
      llnk(260) = -0.6354759310e-02_dp
      llnk(261) = -0.6337480778e-02_dp
      llnk(262) = -0.6320356275e-02_dp
      llnk(263) = -0.6303384010e-02_dp
      llnk(264) = -0.6286561980e-02_dp
      llnk(265) = -0.6269888285e-02_dp
      llnk(266) = -0.6253360871e-02_dp
      llnk(267) = -0.6236978078e-02_dp
      llnk(268) = -0.6220737994e-02_dp
      llnk(269) = -0.6204638851e-02_dp
      llnk(270) = -0.6188678733e-02_dp
      llnk(271) = -0.6172856062e-02_dp
      llnk(272) = -0.6157169047e-02_dp
      llnk(273) = -0.6141616004e-02_dp
      llnk(274) = -0.6126195168e-02_dp
      llnk(275) = -0.6110905001e-02_dp
      llnk(276) = -0.6095743854e-02_dp
      llnk(277) = -0.6080714363e-02_dp
      llnk(278) = -0.6065808661e-02_dp
      llnk(279) = -0.6051025499e-02_dp
      llnk(280) = -0.6036365150e-02_dp
      llnk(281) = -0.6021826113e-02_dp
      llnk(282) = -0.6007406839e-02_dp
      llnk(283) = -0.5993105878e-02_dp
      llnk(284) = -0.5978921847e-02_dp
      llnk(285) = -0.5964853301e-02_dp
      llnk(286) = -0.5950898825e-02_dp
      llnk(287) = -0.5937057006e-02_dp
      llnk(288) = -0.5923326579e-02_dp
      llnk(289) = -0.5909706151e-02_dp
      llnk(290) = -0.5896194416e-02_dp
      llnk(291) = -0.5882789995e-02_dp
      llnk(292) = -0.5869491718e-02_dp
      llnk(293) = -0.5856298304e-02_dp
      llnk(294) = -0.5843208448e-02_dp
      llnk(295) = -0.5830220904e-02_dp
      llnk(296) = -0.5817334546e-02_dp
      llnk(297) = -0.5804548161e-02_dp
      llnk(298) = -0.5791860577e-02_dp
      llnk(299) = -0.5779270522e-02_dp
      llnk(300) = -0.5766776986e-02_dp
      llnk(301) = -0.5754378816e-02_dp
      llnk(302) = -0.5742074899e-02_dp
      llnk(303) = -0.5729864025e-02_dp
      llnk(304) = -0.5717745257e-02_dp
      llnk(305) = -0.5705717481e-02_dp
      llnk(306) = -0.5693779662e-02_dp
      llnk(307) = -0.5681930652e-02_dp
      llnk(308) = -0.5670169557e-02_dp
      llnk(309) = -0.5658495321e-02_dp
      llnk(310) = -0.5646906981e-02_dp
      llnk(311) = -0.5635403426e-02_dp
      llnk(312) = -0.5623983856e-02_dp
      llnk(313) = -0.5612647214e-02_dp
      llnk(314) = -0.5601392619e-02_dp
      llnk(315) = -0.5590219039e-02_dp
      llnk(316) = -0.5579125640e-02_dp
      llnk(317) = -0.5568111507e-02_dp
      llnk(318) = -0.5557175755e-02_dp
      llnk(319) = -0.5546317422e-02_dp
      llnk(320) = -0.5535535712e-02_dp
      llnk(321) = -0.5524829768e-02_dp
      llnk(322) = -0.5514198739e-02_dp
      llnk(323) = -0.5503641734e-02_dp
      llnk(324) = -0.5493157962e-02_dp
      llnk(325) = -0.5482746632e-02_dp
      llnk(326) = -0.5472406939e-02_dp
      llnk(327) = -0.5462138054e-02_dp
      llnk(328) = -0.5451939170e-02_dp
      llnk(329) = -0.5441809594e-02_dp
      llnk(330) = -0.5431748532e-02_dp
      llnk(331) = -0.5421755212e-02_dp
      llnk(332) = -0.5411828857e-02_dp
      llnk(333) = -0.5401975569e-02_dp
      llnk(334) = -0.5392181719e-02_dp
      llnk(335) = -0.5382453176e-02_dp
      llnk(336) = -0.5372788210e-02_dp
      llnk(337) = -0.5363186748e-02_dp
      llnk(338) = -0.5353648059e-02_dp
      llnk(339) = -0.5344171504e-02_dp
      llnk(340) = -0.5334756310e-02_dp
      llnk(341) = -0.5325401926e-02_dp
      llnk(342) = -0.5316107680e-02_dp
      llnk(343) = -0.5306872920e-02_dp
      llnk(344) = -0.5297696936e-02_dp
      llnk(345) = -0.5288579215e-02_dp
      llnk(346) = -0.5279519432e-02_dp
      llnk(347) = -0.5270516389e-02_dp
      llnk(348) = -0.5261569543e-02_dp
      llnk(349) = -0.5252678535e-02_dp
      llnk(350) = -0.5243842709e-02_dp
      llnk(351) = -0.5235061516e-02_dp
      llnk(352) = -0.5226334273e-02_dp
      llnk(353) = -0.5217660524e-02_dp
      llnk(354) = -0.5209039665e-02_dp
      llnk(355) = -0.5200471169e-02_dp
      llnk(356) = -0.5191954431e-02_dp
      llnk(357) = -0.5183488963e-02_dp
      llnk(358) = -0.5175074221e-02_dp
      llnk(359) = -0.5166709698e-02_dp
      llnk(360) = -0.5158394787e-02_dp
      llnk(361) = -0.5150129056e-02_dp
      llnk(362) = -0.5141911977e-02_dp
      llnk(363) = -0.5133743148e-02_dp
      llnk(364) = -0.5125621837e-02_dp
      llnk(365) = -0.5117547698e-02_dp
      llnk(366) = -0.5109520206e-02_dp
      llnk(367) = -0.5101538932e-02_dp
      llnk(368) = -0.5093603358e-02_dp
      llnk(369) = -0.5085713032e-02_dp
      llnk(370) = -0.5077867507e-02_dp
      llnk(371) = -0.5070066320e-02_dp
      llnk(372) = -0.5062309014e-02_dp
      llnk(373) = -0.5054595077e-02_dp
      llnk(374) = -0.5046924184e-02_dp
      llnk(375) = -0.5039295844e-02_dp
      llnk(376) = -0.5031709627e-02_dp
      llnk(377) = -0.5024165063e-02_dp
      llnk(378) = -0.5016661793e-02_dp
      llnk(379) = -0.5009199414e-02_dp
      llnk(380) = -0.5001777482e-02_dp
      llnk(381) = -0.4994395567e-02_dp
      llnk(382) = -0.4987053274e-02_dp
      llnk(383) = -0.4979750283e-02_dp
      llnk(384) = -0.4972486128e-02_dp
      llnk(385) = -0.4965268840e-02_dp
      llnk(386) = -0.4958081936e-02_dp
      llnk(387) = -0.4950932817e-02_dp
      llnk(388) = -0.4943821092e-02_dp
      llnk(389) = -0.4936746301e-02_dp
      llnk(390) = -0.4929708210e-02_dp
      llnk(391) = -0.4922706445e-02_dp
      llnk(392) = -0.4915740611e-02_dp
      llnk(393) = -0.4908810284e-02_dp
      llnk(394) = -0.4901915260e-02_dp
      llnk(395) = -0.4895055124e-02_dp
      llnk(396) = -0.4888229556e-02_dp
      llnk(397) = -0.4881438160e-02_dp
      llnk(398) = -0.4874680679e-02_dp
      llnk(399) = -0.4867956758e-02_dp
      llnk(400) = -0.4861266098e-02_dp
      llnk(401) = -0.4854608317e-02_dp
      llnk(402) = -0.4847983151e-02_dp
      llnk(403) = -0.4841390266e-02_dp
      llnk(404) = -0.4834829391e-02_dp
      llnk(405) = -0.4828300135e-02_dp
      llnk(406) = -0.4821802243e-02_dp
      llnk(407) = -0.4815335427e-02_dp
      llnk(408) = -0.4808899393e-02_dp
      llnk(409) = -0.4802493803e-02_dp
      llnk(410) = -0.4796118410e-02_dp
      llnk(411) = -0.4789772949e-02_dp
      llnk(412) = -0.4783457104e-02_dp
      llnk(413) = -0.4777170573e-02_dp
      llnk(414) = -0.4770913098e-02_dp
      llnk(415) = -0.4764684685e-02_dp
      llnk(416) = -0.4758484990e-02_dp
      llnk(417) = -0.4752313124e-02_dp
      llnk(418) = -0.4746169197e-02_dp
      llnk(419) = -0.4740053059e-02_dp
      llnk(420) = -0.4733964341e-02_dp
      llnk(421) = -0.4727902849e-02_dp
      llnk(422) = -0.4721868283e-02_dp
      llnk(423) = -0.4715860425e-02_dp
      llnk(424) = -0.4709879012e-02_dp
      llnk(425) = -0.4703923846e-02_dp
      llnk(426) = -0.4697994583e-02_dp
      llnk(427) = -0.4692091066e-02_dp
      llnk(428) = -0.4686213045e-02_dp
      llnk(429) = -0.4680360271e-02_dp
      llnk(430) = -0.4674532484e-02_dp
      llnk(431) = -0.4668729492e-02_dp
      llnk(432) = -0.4662951085e-02_dp
      llnk(433) = -0.4657196996e-02_dp
      llnk(434) = -0.4651467004e-02_dp
      llnk(435) = -0.4645760890e-02_dp
      llnk(436) = -0.4640080410e-02_dp
      llnk(437) = -0.4634428369e-02_dp
      llnk(438) = -0.4628793378e-02_dp
      llnk(439) = -0.4623181470e-02_dp
      llnk(440) = -0.4617592432e-02_dp
      llnk(441) = -0.4612026079e-02_dp
      llnk(442) = -0.4606482171e-02_dp
      llnk(443) = -0.4600960531e-02_dp
      llnk(444) = -0.4595460979e-02_dp
      llnk(445) = -0.4589983282e-02_dp
      llnk(446) = -0.4584527233e-02_dp
      llnk(447) = -0.4579092710e-02_dp
      llnk(448) = -0.4573679458e-02_dp
      llnk(449) = -0.4568287340e-02_dp
      llnk(450) = -0.4562916080e-02_dp
      llnk(451) = -0.4557565629e-02_dp
      llnk(452) = -0.4552235678e-02_dp
      llnk(453) = -0.4546926139e-02_dp
      llnk(454) = -0.4541636751e-02_dp
      llnk(455) = -0.4536367408e-02_dp
      llnk(456) = -0.4531117888e-02_dp
      llnk(457) = -0.4525888082e-02_dp
      llnk(458) = -0.4520677720e-02_dp
      llnk(459) = -0.4515486721e-02_dp
      llnk(460) = -0.4510314872e-02_dp
      llnk(461) = -0.4505162010e-02_dp
      llnk(462) = -0.4500027980e-02_dp
      llnk(463) = -0.4494912622e-02_dp
      llnk(464) = -0.4489815761e-02_dp
      llnk(465) = -0.4484737267e-02_dp
      llnk(466) = -0.4479676977e-02_dp
      llnk(467) = -0.4474634684e-02_dp
      llnk(468) = -0.4469610304e-02_dp
      llnk(469) = -0.4464603661e-02_dp
      llnk(470) = -0.4459614543e-02_dp
      llnk(471) = -0.4454642899e-02_dp
      llnk(472) = -0.4449688484e-02_dp
      llnk(473) = -0.4444751266e-02_dp
      llnk(474) = -0.4439830996e-02_dp
      llnk(475) = -0.4434927562e-02_dp
      llnk(476) = -0.4430040859e-02_dp
      llnk(477) = -0.4425170710e-02_dp
      llnk(478) = -0.4420316957e-02_dp
      llnk(479) = -0.4415479491e-02_dp
      llnk(480) = -0.4410658186e-02_dp
      llnk(481) = -0.4405852880e-02_dp
      llnk(482) = -0.4401063430e-02_dp
      llnk(483) = -0.4396289728e-02_dp
      llnk(484) = -0.4391531676e-02_dp
      llnk(485) = -0.4386789086e-02_dp
      llnk(486) = -0.4382061813e-02_dp
      llnk(487) = -0.4377349779e-02_dp
      llnk(488) = -0.4372652829e-02_dp
      llnk(489) = -0.4367973960e-02_dp
      llnk(490) = -0.4363312569e-02_dp
      llnk(491) = -0.4358660920e-02_dp
      llnk(492) = -0.4354023941e-02_dp
      llnk(493) = -0.4349402011e-02_dp
      llnk(494) = -0.4344794018e-02_dp
      llnk(495) = -0.4340200286e-02_dp
      llnk(496) = -0.4335620759e-02_dp
      llnk(497) = -0.4331055332e-02_dp
      llnk(498) = -0.4326503816e-02_dp
      llnk(499) = -0.4321966182e-02_dp
      llnk(500) = -0.4317442332e-02_dp
      llnk(501) = -0.4312932067e-02_dp
      llnk(502) = -0.4308435312e-02_dp
      llnk(503) = -0.4303951987e-02_dp
      llnk(504) = -0.4299481991e-02_dp
      llnk(505) = -0.4295025176e-02_dp
      llnk(506) = -0.4290581476e-02_dp
      llnk(507) = -0.4286150784e-02_dp
      llnk(508) = -0.4281732974e-02_dp
      llnk(509) = -0.4277328005e-02_dp
      llnk(510) = -0.4272935697e-02_dp
      llnk(511) = -0.4268555976e-02_dp
      llnk(512) = -0.4264188766e-02_dp
      llnk(513) = -0.4259833966e-02_dp
      llnk(514) = -0.4255491492e-02_dp
      llnk(515) = -0.4251161154e-02_dp
      llnk(516) = -0.4246842949e-02_dp
      llnk(517) = -0.4242536837e-02_dp
      llnk(518) = -0.4238242629e-02_dp
      llnk(519) = -0.4233960267e-02_dp
      llnk(520) = -0.4229689632e-02_dp
      llnk(521) = -0.4225430669e-02_dp
      llnk(522) = -0.4221183310e-02_dp
      llnk(523) = -0.4216947377e-02_dp
      llnk(524) = -0.4212722872e-02_dp
      llnk(525) = -0.4208509690e-02_dp
      llnk(526) = -0.4204307736e-02_dp
      llnk(527) = -0.4200116898e-02_dp
      llnk(528) = -0.4195937125e-02_dp
      llnk(529) = -0.4191768337e-02_dp
      llnk(530) = -0.4187610437e-02_dp
      llnk(531) = -0.4183463316e-02_dp
      llnk(532) = -0.4179326948e-02_dp
      llnk(533) = -0.4175201222e-02_dp
      llnk(534) = -0.4171086099e-02_dp
      llnk(535) = -0.4166981404e-02_dp
      llnk(536) = -0.4162887134e-02_dp
      llnk(537) = -0.4158803202e-02_dp
      llnk(538) = -0.4154729536e-02_dp
      llnk(539) = -0.4150666091e-02_dp
      llnk(540) = -0.4146612671e-02_dp
      llnk(541) = -0.4142569363e-02_dp
      llnk(542) = -0.4138535935e-02_dp
      llnk(543) = -0.4134515728e-02_dp
      llnk(544) = -0.4130507039e-02_dp
      llnk(545) = -0.4126503797e-02_dp
      llnk(546) = -0.4122510270e-02_dp
      llnk(547) = -0.4118526386e-02_dp
      llnk(548) = -0.4114552039e-02_dp
      llnk(549) = -0.4110587257e-02_dp
      llnk(550) = -0.4106631920e-02_dp
      llnk(551) = -0.4102685926e-02_dp
      llnk(552) = -0.4098749247e-02_dp
      llnk(553) = -0.4094821847e-02_dp
      llnk(554) = -0.4090903630e-02_dp
      llnk(555) = -0.4086994545e-02_dp
      llnk(556) = -0.4083094472e-02_dp
      llnk(557) = -0.4079203396e-02_dp
      llnk(558) = -0.4075321316e-02_dp
      llnk(559) = -0.4071448028e-02_dp
      llnk(560) = -0.4067583587e-02_dp
      llnk(561) = -0.4063727921e-02_dp
      llnk(562) = -0.4059880939e-02_dp
      llnk(563) = -0.4056042583e-02_dp
      llnk(564) = -0.4052212762e-02_dp
      llnk(565) = -0.4048391527e-02_dp
      llnk(566) = -0.4044578730e-02_dp
      llnk(567) = -0.4040774354e-02_dp
      llnk(568) = -0.4036978296e-02_dp
      llnk(569) = -0.4033190534e-02_dp
      llnk(570) = -0.4029410975e-02_dp
      llnk(571) = -0.4025639600e-02_dp
      llnk(572) = -0.4021876379e-02_dp
      llnk(573) = -0.4018121658e-02_dp
      llnk(574) = -0.4014374533e-02_dp
      llnk(575) = -0.4010635431e-02_dp
      llnk(576) = -0.4006904167e-02_dp
      llnk(577) = -0.4003180813e-02_dp
      llnk(578) = -0.3999465257e-02_dp
      llnk(579) = -0.3995757506e-02_dp
      llnk(580) = -0.3992057412e-02_dp
      llnk(581) = -0.3988364979e-02_dp
      llnk(582) = -0.3984680200e-02_dp
      llnk(583) = -0.3981002957e-02_dp
      llnk(584) = -0.3977333187e-02_dp
      llnk(585) = -0.3973670940e-02_dp
      llnk(586) = -0.3970016087e-02_dp
      llnk(587) = -0.3966368605e-02_dp
      llnk(588) = -0.3962728432e-02_dp
      llnk(589) = -0.3959095565e-02_dp
      llnk(590) = -0.3955469896e-02_dp
      llnk(591) = -0.3951851444e-02_dp
      llnk(592) = -0.3948240106e-02_dp
      llnk(593) = -0.3944635840e-02_dp
      llnk(594) = -0.3941038624e-02_dp
      llnk(595) = -0.3937448450e-02_dp
      llnk(596) = -0.3933865180e-02_dp
      llnk(597) = -0.3930288852e-02_dp
      llnk(598) = -0.3926725182e-02_dp
      llnk(599) = -0.3923164225e-02_dp
      llnk(600) = -0.3919609022e-02_dp
      llnk(601) = -0.3916060603e-02_dp
      llnk(602) = -0.3912518941e-02_dp
      llnk(603) = -0.3908984017e-02_dp
      llnk(604) = -0.3905455707e-02_dp
      llnk(605) = -0.3901934071e-02_dp
      llnk(606) = -0.3898419000e-02_dp
      llnk(607) = -0.3894910446e-02_dp
      llnk(608) = -0.3891408443e-02_dp
      llnk(609) = -0.3887912933e-02_dp
      llnk(610) = -0.3884423810e-02_dp
      llnk(611) = -0.3880941150e-02_dp
      llnk(612) = -0.3877464789e-02_dp
      llnk(613) = -0.3873994730e-02_dp
      llnk(614) = -0.3870530985e-02_dp
      llnk(615) = -0.3867073514e-02_dp
      llnk(616) = -0.3863622258e-02_dp
      llnk(617) = -0.3860177173e-02_dp
      llnk(618) = -0.3856738235e-02_dp
      llnk(619) = -0.3853305441e-02_dp
      llnk(620) = -0.3849878697e-02_dp
      llnk(621) = -0.3846458016e-02_dp
      llnk(622) = -0.3843043331e-02_dp
      llnk(623) = -0.3839634656e-02_dp
      llnk(624) = -0.3836231919e-02_dp
      llnk(625) = -0.3832835028e-02_dp
      llnk(626) = -0.3829444026e-02_dp
      llnk(627) = -0.3826058954e-02_dp
      llnk(628) = -0.3822679633e-02_dp
      llnk(629) = -0.3819306152e-02_dp
      llnk(630) = -0.3815938396e-02_dp
      llnk(631) = -0.3812576353e-02_dp
      llnk(632) = -0.3809220046e-02_dp
      llnk(633) = -0.3805869353e-02_dp
      llnk(634) = -0.3802524327e-02_dp
      llnk(635) = -0.3799184885e-02_dp
      llnk(636) = -0.3795851005e-02_dp
      llnk(637) = -0.3792522658e-02_dp
      llnk(638) = -0.3789199831e-02_dp
      llnk(639) = -0.3785882531e-02_dp
      llnk(640) = -0.3782570655e-02_dp
      llnk(641) = -0.3779264191e-02_dp
      llnk(642) = -0.3775963147e-02_dp
      llnk(643) = -0.3772667476e-02_dp
      llnk(644) = -0.3769377113e-02_dp
      llnk(645) = -0.3766092076e-02_dp
      llnk(646) = -0.3762812304e-02_dp
      llnk(647) = -0.3759537833e-02_dp
      llnk(648) = -0.3756268585e-02_dp
      llnk(649) = -0.3753004510e-02_dp
      llnk(650) = -0.3749745613e-02_dp
      llnk(651) = -0.3746491901e-02_dp
      llnk(652) = -0.3743243283e-02_dp
      llnk(653) = -0.3740005626e-02_dp
      llnk(654) = -0.3736767845e-02_dp
      llnk(655) = -0.3733535132e-02_dp
      llnk(656) = -0.3730307327e-02_dp
      llnk(657) = -0.3727084607e-02_dp
      llnk(658) = -0.3723866873e-02_dp
      llnk(659) = -0.3720654107e-02_dp
      llnk(660) = -0.3717446303e-02_dp
      llnk(661) = -0.3714243463e-02_dp
      llnk(662) = -0.3711045523e-02_dp
      llnk(663) = -0.3707852455e-02_dp
      llnk(664) = -0.3704664303e-02_dp
      llnk(665) = -0.3701480968e-02_dp
      llnk(666) = -0.3698302457e-02_dp
      llnk(667) = -0.3695128771e-02_dp
      llnk(668) = -0.3691959874e-02_dp
      llnk(669) = -0.3688795678e-02_dp
      llnk(670) = -0.3685636275e-02_dp
      llnk(671) = -0.3682481602e-02_dp
      llnk(672) = -0.3679331555e-02_dp
      llnk(673) = -0.3676186270e-02_dp
      llnk(674) = -0.3673045561e-02_dp
      llnk(675) = -0.3669909551e-02_dp
      llnk(676) = -0.3666778150e-02_dp
      llnk(677) = -0.3663651302e-02_dp
      llnk(678) = -0.3660529071e-02_dp
      llnk(679) = -0.3657411389e-02_dp
      llnk(680) = -0.3654298215e-02_dp
      llnk(681) = -0.3651189465e-02_dp
      llnk(682) = -0.3648085315e-02_dp
      llnk(683) = -0.3644985621e-02_dp
      llnk(684) = -0.3641890453e-02_dp
      llnk(685) = -0.3638799641e-02_dp
      llnk(686) = -0.3635713278e-02_dp
      llnk(687) = -0.3632631311e-02_dp
      llnk(688) = -0.3629553745e-02_dp
      llnk(689) = -0.3626480539e-02_dp
      llnk(690) = -0.3623411645e-02_dp
      llnk(691) = -0.3620347077e-02_dp
      llnk(692) = -0.3617286859e-02_dp
      llnk(693) = -0.3614230904e-02_dp
      llnk(694) = -0.3611179235e-02_dp
      llnk(695) = -0.3608131812e-02_dp
      llnk(696) = -0.3605088635e-02_dp
      llnk(697) = -0.3602049657e-02_dp
      llnk(698) = -0.3599014895e-02_dp
      llnk(699) = -0.3595984357e-02_dp
      llnk(700) = -0.3592957941e-02_dp
      llnk(701) = -0.3589935718e-02_dp
      llnk(702) = -0.3586917596e-02_dp
      llnk(703) = -0.3583903622e-02_dp
      llnk(704) = -0.3580893698e-02_dp
      llnk(705) = -0.3577887909e-02_dp
      llnk(706) = -0.3574886158e-02_dp
      llnk(707) = -0.3571888466e-02_dp
      llnk(708) = -0.3568899450e-02_dp
      llnk(709) = -0.3565910323e-02_dp
      llnk(710) = -0.3562925154e-02_dp
      llnk(711) = -0.3559943972e-02_dp
      llnk(712) = -0.3556966859e-02_dp
      llnk(713) = -0.3553993656e-02_dp
      llnk(714) = -0.3551024466e-02_dp
      llnk(715) = -0.3548059237e-02_dp
      llnk(716) = -0.3545097968e-02_dp
      llnk(717) = -0.3542140587e-02_dp
      llnk(718) = -0.3539187147e-02_dp
      llnk(719) = -0.3536237614e-02_dp
      llnk(720) = -0.3533291992e-02_dp
      llnk(721) = -0.3530350229e-02_dp
      llnk(722) = -0.3527412335e-02_dp
      llnk(723) = -0.3524478279e-02_dp
      llnk(724) = -0.3521548076e-02_dp
      llnk(725) = -0.3518621713e-02_dp
      llnk(726) = -0.3515699144e-02_dp
      llnk(727) = -0.3512780363e-02_dp
      llnk(728) = -0.3509865386e-02_dp
      llnk(729) = -0.3506954146e-02_dp
      llnk(730) = -0.3504046711e-02_dp
      llnk(731) = -0.3501142980e-02_dp
      llnk(732) = -0.3498243029e-02_dp
      llnk(733) = -0.3495346825e-02_dp
      llnk(734) = -0.3492454277e-02_dp
      llnk(735) = -0.3489565504e-02_dp
      llnk(736) = -0.3486680283e-02_dp
      llnk(737) = -0.3483798748e-02_dp
      llnk(738) = -0.3480921125e-02_dp
      llnk(739) = -0.3478046974e-02_dp
      llnk(740) = -0.3475176495e-02_dp
      llnk(741) = -0.3472309608e-02_dp
      llnk(742) = -0.3469446344e-02_dp
      llnk(743) = -0.3466586679e-02_dp
      llnk(744) = -0.3463730607e-02_dp
      llnk(745) = -0.3460878143e-02_dp
      llnk(746) = -0.3458029208e-02_dp
      llnk(747) = -0.3455183869e-02_dp
      llnk(748) = -0.3452342050e-02_dp
      llnk(749) = -0.3449503792e-02_dp
      llnk(750) = -0.3446669019e-02_dp
      llnk(751) = -0.3443837835e-02_dp
      llnk(752) = -0.3441010114e-02_dp
      llnk(753) = -0.3438185843e-02_dp
      llnk(754) = -0.3435365132e-02_dp
      llnk(755) = -0.3432547840e-02_dp
      llnk(756) = -0.3429734036e-02_dp
      llnk(757) = -0.3426923672e-02_dp
      llnk(758) = -0.3424116757e-02_dp
      llnk(759) = -0.3421313262e-02_dp
      llnk(760) = -0.3418513177e-02_dp
      llnk(761) = -0.3415716524e-02_dp
      llnk(762) = -0.3412923273e-02_dp
      llnk(763) = -0.3410136336e-02_dp
      llnk(764) = -0.3407350916e-02_dp
      llnk(765) = -0.3404568130e-02_dp
      llnk(766) = -0.3401788745e-02_dp
      llnk(767) = -0.3399012718e-02_dp
      llnk(768) = -0.3396240047e-02_dp
      llnk(769) = -0.3393470771e-02_dp
      llnk(770) = -0.3390704781e-02_dp
      llnk(771) = -0.3387942162e-02_dp
      llnk(772) = -0.3385182859e-02_dp
      llnk(773) = -0.3382426879e-02_dp
      llnk(774) = -0.3379674202e-02_dp
      llnk(775) = -0.3376924820e-02_dp
      llnk(776) = -0.3374178738e-02_dp
      llnk(777) = -0.3371435939e-02_dp
      llnk(778) = -0.3368696456e-02_dp
      llnk(779) = -0.3365960182e-02_dp
      llnk(780) = -0.3363227225e-02_dp
      llnk(781) = -0.3360497473e-02_dp
      llnk(782) = -0.3357770991e-02_dp
      llnk(783) = -0.3355047725e-02_dp
      llnk(784) = -0.3352327686e-02_dp
      llnk(785) = -0.3349610914e-02_dp
      llnk(786) = -0.3346897303e-02_dp
      llnk(787) = -0.3344186906e-02_dp
      llnk(788) = -0.3341479724e-02_dp
      llnk(789) = -0.3338775700e-02_dp
      llnk(790) = -0.3336074863e-02_dp
      llnk(791) = -0.3333377225e-02_dp
      llnk(792) = -0.3330682695e-02_dp
      llnk(793) = -0.3327991302e-02_dp
      llnk(794) = -0.3325303130e-02_dp
      llnk(795) = -0.3322618041e-02_dp
      llnk(796) = -0.3319936145e-02_dp
      llnk(797) = -0.3317257379e-02_dp
      llnk(798) = -0.3314581695e-02_dp
      llnk(799) = -0.3311909149e-02_dp
      llnk(800) = -0.3309239696e-02_dp
      llnk(801) = -0.3306573350e-02_dp
      llnk(802) = -0.3303910077e-02_dp
      llnk(803) = -0.3301249895e-02_dp
      llnk(804) = -0.3298592786e-02_dp
      llnk(805) = -0.3295938770e-02_dp
      llnk(806) = -0.3293287792e-02_dp
      llnk(807) = -0.3290639881e-02_dp
      llnk(808) = -0.3287994967e-02_dp
      llnk(809) = -0.3285353159e-02_dp
      llnk(810) = -0.3282714334e-02_dp
      llnk(811) = -0.3280078559e-02_dp
      llnk(812) = -0.3277445795e-02_dp
      llnk(813) = -0.3274816076e-02_dp
      llnk(814) = -0.3272189346e-02_dp
      llnk(815) = -0.3269565604e-02_dp
      llnk(816) = -0.3266944867e-02_dp
      llnk(817) = -0.3264327100e-02_dp
      llnk(818) = -0.3261713686e-02_dp
      llnk(819) = -0.3259103619e-02_dp
      llnk(820) = -0.3256495012e-02_dp
      llnk(821) = -0.3253889400e-02_dp
      llnk(822) = -0.3251286777e-02_dp
      llnk(823) = -0.3248687255e-02_dp
      llnk(824) = -0.3246090539e-02_dp
      llnk(825) = -0.3243496801e-02_dp
      llnk(826) = -0.3240905999e-02_dp
      llnk(827) = -0.3238318138e-02_dp
      llnk(828) = -0.3235733197e-02_dp
      llnk(829) = -0.3233151191e-02_dp
      llnk(830) = -0.3230572128e-02_dp
      llnk(831) = -0.3227995977e-02_dp
      llnk(832) = -0.3225422751e-02_dp
      llnk(833) = -0.3222852394e-02_dp
      llnk(834) = -0.3220284979e-02_dp
      llnk(835) = -0.3217720406e-02_dp
      llnk(836) = -0.3215158781e-02_dp
      llnk(837) = -0.3212600066e-02_dp
      llnk(838) = -0.3210044148e-02_dp
      llnk(839) = -0.3207491153e-02_dp
      llnk(840) = -0.3204940999e-02_dp
      llnk(841) = -0.3202393742e-02_dp
      llnk(842) = -0.3199849338e-02_dp
      llnk(843) = -0.3197307775e-02_dp
      llnk(844) = -0.3194769052e-02_dp
      llnk(845) = -0.3192233205e-02_dp
      llnk(846) = -0.3189700191e-02_dp
      llnk(847) = -0.3187169971e-02_dp
      llnk(848) = -0.3184642524e-02_dp
      llnk(849) = -0.3182117988e-02_dp
      llnk(850) = -0.3179596231e-02_dp
      llnk(851) = -0.3177077300e-02_dp
      llnk(852) = -0.3174561186e-02_dp
      llnk(853) = -0.3172047887e-02_dp
      llnk(854) = -0.3169537359e-02_dp
      llnk(855) = -0.3167029632e-02_dp
      llnk(856) = -0.3164524709e-02_dp
      llnk(857) = -0.3162022553e-02_dp
      llnk(858) = -0.3159523159e-02_dp
      llnk(859) = -0.3157026583e-02_dp
      llnk(860) = -0.3154532729e-02_dp
      llnk(861) = -0.3152041656e-02_dp
      llnk(862) = -0.3149553327e-02_dp
      llnk(863) = -0.3147067778e-02_dp
      llnk(864) = -0.3144584964e-02_dp
      llnk(865) = -0.3142104892e-02_dp
      llnk(866) = -0.3139627563e-02_dp
      llnk(867) = -0.3137152962e-02_dp
      llnk(868) = -0.3134681067e-02_dp
      llnk(869) = -0.3132211915e-02_dp
      llnk(870) = -0.3129745502e-02_dp
      llnk(871) = -0.3127281759e-02_dp
      llnk(872) = -0.3124820761e-02_dp
      llnk(873) = -0.3122362718e-02_dp
      llnk(874) = -0.3119909222e-02_dp
      llnk(875) = -0.3117456503e-02_dp
      llnk(876) = -0.3115006490e-02_dp
      llnk(877) = -0.3112559221e-02_dp
      llnk(878) = -0.3110114595e-02_dp
      llnk(879) = -0.3107672711e-02_dp
      llnk(880) = -0.3105233507e-02_dp
      llnk(881) = -0.3102796921e-02_dp
      llnk(882) = -0.3100363082e-02_dp
      llnk(883) = -0.3097931893e-02_dp
      llnk(884) = -0.3095503371e-02_dp
      llnk(885) = -0.3093077570e-02_dp
      llnk(886) = -0.3090654347e-02_dp
      llnk(887) = -0.3088233816e-02_dp
      llnk(888) = -0.3085815991e-02_dp
      llnk(889) = -0.3083400748e-02_dp
      llnk(890) = -0.3080988196e-02_dp
      llnk(891) = -0.3078578271e-02_dp
      llnk(892) = -0.3076170985e-02_dp
      llnk(893) = -0.3073766340e-02_dp
      llnk(894) = -0.3071364353e-02_dp
      llnk(895) = -0.3068964958e-02_dp
      llnk(896) = -0.3066568203e-02_dp
      llnk(897) = -0.3064174074e-02_dp
      llnk(898) = -0.3061782549e-02_dp
      llnk(899) = -0.3059393659e-02_dp
      llnk(900) = -0.3057007375e-02_dp
      llnk(901) = -0.3054623711e-02_dp
      llnk(902) = -0.3052242585e-02_dp
      llnk(903) = -0.3049864141e-02_dp
      llnk(904) = -0.3047488140e-02_dp
      llnk(905) = -0.3045114846e-02_dp
      llnk(906) = -0.3042744119e-02_dp
      llnk(907) = -0.3040375970e-02_dp
      llnk(908) = -0.3038010418e-02_dp
      llnk(909) = -0.3035647475e-02_dp
      llnk(910) = -0.3033287098e-02_dp
      llnk(911) = -0.3030929263e-02_dp
      llnk(912) = -0.3028574024e-02_dp
      llnk(913) = -0.3026221302e-02_dp
      llnk(914) = -0.3023871162e-02_dp
      llnk(915) = -0.3021523546e-02_dp
      llnk(916) = -0.3019178525e-02_dp
      llnk(917) = -0.3016836027e-02_dp
      llnk(918) = -0.3014496035e-02_dp
      llnk(919) = -0.3012158656e-02_dp
      llnk(920) = -0.3009823741e-02_dp
      llnk(921) = -0.3007491429e-02_dp
      llnk(922) = -0.3005161605e-02_dp
      llnk(923) = -0.3002834314e-02_dp
      llnk(924) = -0.3000509569e-02_dp
      llnk(925) = -0.2998187291e-02_dp
      llnk(926) = -0.2995867561e-02_dp
      llnk(927) = -0.2993550344e-02_dp
      llnk(928) = -0.2991235634e-02_dp
      llnk(929) = -0.2988925225e-02_dp
      llnk(930) = -0.2986615660e-02_dp
      llnk(931) = -0.2984308615e-02_dp
      llnk(932) = -0.2982004089e-02_dp
      llnk(933) = -0.2979702045e-02_dp
      llnk(934) = -0.2977402494e-02_dp
      llnk(935) = -0.2975105470e-02_dp
      llnk(936) = -0.2972810888e-02_dp
      llnk(937) = -0.2970518821e-02_dp
      llnk(938) = -0.2968229240e-02_dp
      llnk(939) = -0.2965942158e-02_dp
      llnk(940) = -0.2963657551e-02_dp
      llnk(941) = -0.2961375403e-02_dp
      llnk(942) = -0.2959095731e-02_dp
      llnk(943) = -0.2956818525e-02_dp
      llnk(944) = -0.2954543818e-02_dp
      llnk(945) = -0.2952271541e-02_dp
      llnk(946) = -0.2950001748e-02_dp
      llnk(947) = -0.2947734412e-02_dp
      llnk(948) = -0.2945469512e-02_dp
      llnk(949) = -0.2943207084e-02_dp
      llnk(950) = -0.2940947077e-02_dp
      llnk(951) = -0.2938689563e-02_dp
      llnk(952) = -0.2936434487e-02_dp
      llnk(953) = -0.2934181799e-02_dp
      llnk(954) = -0.2931931606e-02_dp
      llnk(955) = -0.2929683842e-02_dp
      llnk(956) = -0.2927438481e-02_dp
      llnk(957) = -0.2925195572e-02_dp
      llnk(958) = -0.2922955070e-02_dp
      llnk(959) = -0.2920716941e-02_dp
      llnk(960) = -0.2918481269e-02_dp
      llnk(961) = -0.2916248012e-02_dp
      llnk(962) = -0.2914017179e-02_dp
      llnk(963) = -0.2911788782e-02_dp
      llnk(964) = -0.2909562774e-02_dp
      llnk(965) = -0.2907339192e-02_dp
      llnk(966) = -0.2905117982e-02_dp
      llnk(967) = -0.2902899173e-02_dp
      llnk(968) = -0.2900682799e-02_dp
      llnk(969) = -0.2898468796e-02_dp
      llnk(970) = -0.2896257166e-02_dp
      llnk(971) = -0.2894047930e-02_dp
      llnk(972) = -0.2891841138e-02_dp
      llnk(973) = -0.2889636618e-02_dp
      llnk(974) = -0.2887434580e-02_dp
      llnk(975) = -0.2885234873e-02_dp
      llnk(976) = -0.2883037537e-02_dp
      llnk(977) = -0.2880842566e-02_dp
      llnk(978) = -0.2878649974e-02_dp
      llnk(979) = -0.2876459766e-02_dp
      llnk(980) = -0.2874271894e-02_dp
      llnk(981) = -0.2872086376e-02_dp
      llnk(982) = -0.2869903273e-02_dp
      llnk(983) = -0.2867722443e-02_dp
      llnk(984) = -0.2865545315e-02_dp
      llnk(985) = -0.2863369328e-02_dp
      llnk(986) = -0.2861195689e-02_dp
      llnk(987) = -0.2859024433e-02_dp
      llnk(988) = -0.2856855469e-02_dp
      llnk(989) = -0.2854688909e-02_dp
      llnk(990) = -0.2852524645e-02_dp
      llnk(991) = -0.2850362774e-02_dp
      llnk(992) = -0.2848203196e-02_dp
      llnk(993) = -0.2846045956e-02_dp
      llnk(994) = -0.2843891068e-02_dp
      llnk(995) = -0.2841738481e-02_dp
      llnk(996) = -0.2839588248e-02_dp
      llnk(997) = -0.2837440365e-02_dp
      llnk(998) = -0.2835294724e-02_dp
      llnk(999) = -0.2833151463e-02_dp
      llnk(1000) = -0.2831010498e-02_dp
      llnk(1001) = -0.2828871870e-02_dp
      llnk(1002) = -0.2826735533e-02_dp
      llnk(1003) = -0.2824601509e-02_dp
      llnk(1004) = -0.2822469774e-02_dp
      llnk(1005) = -0.2820340370e-02_dp
      llnk(1006) = -0.2818213253e-02_dp
      llnk(1007) = -0.2816088447e-02_dp
      llnk(1008) = -0.2813965924e-02_dp
      llnk(1009) = -0.2811845711e-02_dp
      llnk(1010) = -0.2809727762e-02_dp
      llnk(1011) = -0.2807612130e-02_dp
      llnk(1012) = -0.2805498780e-02_dp
      llnk(1013) = -0.2803387663e-02_dp
      llnk(1014) = -0.2801278909e-02_dp
      llnk(1015) = -0.2799172307e-02_dp
      llnk(1016) = -0.2797068067e-02_dp
      llnk(1017) = -0.2794966114e-02_dp
      llnk(1018) = -0.2792866401e-02_dp
      llnk(1019) = -0.2790768970e-02_dp
      llnk(1020) = -0.2788673820e-02_dp
      llnk(1021) = -0.2786580911e-02_dp
      llnk(1022) = -0.2784490270e-02_dp
      llnk(1023) = -0.2782401919e-02_dp
      llnk(1024) = -0.2780315803e-02_dp

   end subroutine loadlovenumber

   !
   !
   ! ==========================================================================
   !>
   subroutine tforce(jul0, TIME, xzeta, yzeta, TIDEP, IDIM1, dstart, dstop, eps)

      use messagehandling
      !
      ! ====================================================================
      !
      !     Copyright © 2025, Rijkswaterstaat, All Rights Reserved.
      !
      !     This code is the result of a collaboration between Rijkswaterstaat and Deltares. Contact for the exact licensing:
      !     https://www.rijkswaterstaat.nl/formulieren/contactformulier, software.support@deltares.nl
      !
      ! ********************************************************************
      !
      !     DESCRIPTION
      !
      !     Computes the tidal potential for each active grid point
      !
      ! ********************************************************************
      !
      !     COMMON BLOCKS
      !
      implicit none
      !
      ! **********************************************************************
      !
      !     INPUT / OUTPUT   PARAMETERS
      !
      integer idim1, jul0

      real(kind=dp) :: rmjdat, dstart, dstop, eps, TIME
      real(kind=dp) xzeta(idim1), yzeta(idim1), tidep(idim1)

      real(kind=dp), allocatable, save :: tideuc(:, :, :), tideus(:, :, :) !       (idim1, 0:3,2:3),

      integer, save :: IRC = 0

      character(len=40), dimension(484) :: RECS ! ZAT IN FILE 'HARMONICS'

      !
      !     dstart   i    starting Doodson number
      !     dstop    i    stopping Doodson number
      !     eps      i    tolerance level for tidal force formula
      !     harfil   i    file with tidal harmonics
      !     idim1    i    first dimension of fullbox array (nmax for SIMONA)
      !          i    second dimension of fullbox array (mmax+6 for SIMONA)
      !     irc      i    input parameter for tforce
      !                   irc = 0:  initialisation phase
      !                   irc = 1:  simulation phase
      !     luhar    i    logical unit number to read file with tidal harmonics

      !     name     i    character string containing the name of the
      !                   the calling subroutine.
      !                   Only used for error messages.
      !     rmjdat   i    modified julian day (24-jan-2008 0:00 UTC : 54489.00000)
      !     tidep      o  tidal potential
      !     tideuc   i o  cosine component of tidal potential (array)
      !     tideus   i o  sine component of tidal potential (array)
      !     xzeta    i    latitude (in radians) of grid-points in physiscal plane
      !     yzeta    i    longitude (in radians) of grid-points in physiscal plane
      !
      ! ********************************************************************
      !
      !     LOCAL PARAMETERS
      !
      integer maxdat, maxfld, idebug, i1, i1dbg, i2dbg, N

      real(kind=dp) :: pi, g, rmu, re, d2r, reps
      parameter(idebug=0, i1dbg=0, i2dbg=0)
      parameter(maxdat=500) ! maximal # records in table
      parameter(maxfld=7) ! maximal # fields in table
      parameter(pi=3.14159265358979, re=6378137.0_dp, &
                d2r=pi / 180.0_dp, rmu=3.9860044e14_dp, &
                g=rmu / re / re, reps=1.0e-5_dp)

      integer ntable, nskip
      integer itable(maxdat, maxfld)
      real(kind=dp) :: amps(maxdat), plsmin(6), rklove(3), rhlove(3), &
                       factor(2:3), pol1(0:3, 2:3), cm1(0:3), sm1(0:3)

      integer i, j, nq, mq, IERR
      integer kk(10)
      real(kind=dp) :: fnm, pnm, har, argum, argfct, dtab1, dtab2, &
                       dtab, rlslat, rlslon, rlat, rlong, potent
      real(kind=dp) :: elmnts(6), can(maxdat), san(maxdat)
      real(kind=dp) :: cansum(0:3, 2:3), sansum(0:3, 2:3)
      character(len=80) record
      logical permnt
      real(kind=dp), save :: FACTORIAL(0:6)

      !
      !     amps           table with scaled amplitudes for selected tidal components
      !     argfct         multiplication factor needed to compute argument
      !     argum          argument for time-dependent harmonic components ca,sa
      !     can            table with scaled harmonic components
      !                    cos(argument) * amp(i)
      !     cansum         selected sum of elements of can for fixed mq,nq
      !     cm1            cosine-component of potential
      !     d2r            conversion factor pi/180
      !     dtab           Doodson number: dtab1 + dtab2 / 1000.0_dp
      !     dtab1          first 3 digits of Doodson number
      !     dtab2          second 3 digits of Doodson number
      !     elmnts         array needed for calculation of can, san
      !     factor         table with multiplication factors needed to compute
      !                    tidal potential

      !     fnm            global normalization factor (fnm) found in cartwright 1993
      !                    needed for pol1
      !     g              gravity acceleration
      !     har            amplitude of tidal component
      !     i              loop variable
      !     i1             loop variable

      !     idebug         flag wheter debug output is printed
      !     itable         CTE table read from harmonics.table with the selected CTE
      !                    lines
      !     j              loop variable
      !     kk             array of fields for CTE table
      !     maxdat         maximum number of CTE lines
      !     maxfld         maximum number of fields for each CTE line
      !     mq             order of cm1, sm1 and pol1 (table(i,1))
      !     nq             degree of Legendre polynomial (table(i,7))
      !     nr             number of CTE line
      !     nskip          number of skipped CTE lines in harmonics.table
      !     ntable         number of selected CTE lines
      !     permnt         flag whether tidal line should be skipped
      !     pi             3.14159265358979
      !     plsmin         array with +/- signs for all fields
      !     pnm            output value of legpol1
      !     pol1           Legendre polynomial array
      !     potent         tidal potential
      !     re             radius of the earth
      !     record         character array
      !     reps           real value 1d-5 used for inequalities
      !     rhlove         Love numbers describing the indirect potential effects
      !     rklove         Love numbers describing the geometric-radial effects
      !     rlat           northern latitude in radians
      !     rlong          eastern longitude in radians
      !     rlslat         previous value of rlat
      !     rlslon         previous value of rlong
      !     rmu            gravitational constant (3.9860044e14)
      !     san            table with scaled harmonic components
      !                    sin(argument) * amp(i)
      !     sansum         selected sum of elements of san for fixed mq,nq
      !     sm1            sine-component of potential
      !
      ! ********************************************************************
      !
      !     I / O
      !
      !     harfil = WAQPRO**/harmonics.table  I CTE table (unit luhar)
      !
      ! ********************************************************************
      !
      !     SUBROUTINES CALLED
      !
      !
      !     ASTROL      This copied from richard's subroutine astrol, in goes the
      !                 modified Julian date, out comes an array of six double
      !                 precision variables used for Doodson number computations
      !     LEGPOL1     compute unnormalized associated legendre polynomials up to
      !                 degree 3 and order 3
      !
      ! ********************************************************************
      !
      !     ERROR MESSAGES
      !
      ! ********************************************************************
      !
      !     PSEUDO CODE
      !
      !     save static variables
      !
      !     if (first call) then
      !        initialise
      !        read CTE-table (table, amps, ntable)
      !        if (Dstart <= d(i) <= Dstop and H(i) >= eps) then
      !           read tidal line
      !           amps(i) = har * g * factor
      !        end if
      !
      !        computation and storage of tideuc,tideus at each grid-point:
      !        do (all grid-points)
      !           do nq = 2, 3
      !              do mq = 0, nq
      !                 if (changed latitude) then
      !                    update pol1(m,n) (call legpol1)
      !                 end if
      !                 if (changed longitude) then
      !                    update cm1(m) = cos(), sm1(m) = sin()
      !                 end if
      !                 compute tideuc(nm,mq,nq), tideus(nm,mq,nq)
      !                 store tideuc,tideus
      !              enddo
      !           enddo
      !        enddo
      !     end
      !
      !     update elements-table (call astrol)
      !     compute arrays can, san:
      !     do (all tidal components)
      !        compute argum
      !        can(i) = cos(argum) * amps(i)
      !        san(i) = sin(argum) * amps(i)
      !     enddo
      !     compute arrays cansum, sansum:
      !     do nq = 2, 3
      !        do mq = 0, nq
      !           do (all tidal components)
      !              if ( itable(i,7).eq.nq .and. itable(i,1).eq.mq) then
      !                 cansum(mq, nq) = cansum(mq, nq) + can(i)
      !                 sansum(mq, nq) = sansum(mq, nq) + san(i)
      !              end if
      !           enddo
      !        enddo
      !     enddo
      !
      !     computation of the tidal potential at each grid-point:
      !     do (all grid-points)
      !        do nq = 2, 3
      !           do mq = 0, nq
      !              potent = potent + tideuc(nm, mq, nq) * cansum(mq,nq) +
      !                                tideus(nm, mq, nq) * sansum(mq,nq)
      !           enddo
      !        enddo
      !        tidep(n,m) = potent
      !     enddo
      !
      ! ********************************************************************
      !
      !     DATA STATEMENTS
      !
      data plsmin/+1.0_dp, +1.0_dp, +1.0_dp, +1.0_dp, -1.0_dp, +1.0_dp/
      !
      save itable, ntable, pol1, cm1, sm1, amps
      !
      !c ====================================================================
      !
      !     --- initialisation phase
      !

      ! --- compute modified Julian date (rmjdat)

      ! rmjdat = itdjul - 2400001.0 + time / (24 * 60)

      rmjdat = jul0 - 2400001.0 + time / (24 * 60)

      if (irc == 0) then

         IRC = 1

         FACTORIAL(0) = 1.0_dp
         FACTORIAL(1) = 1.0_dp
         FACTORIAL(2) = 2.0_dp
         FACTORIAL(3) = 6.0_dp
         FACTORIAL(4) = 24.0_dp
         FACTORIAL(5) = 120.0_dp
         FACTORIAL(6) = 720.0_dp

         if (allocated(tideuc)) then
            deallocate (tideuc, tideus)
         end if
         allocate (tideuc(0:3, 2:3, IDIM1), STAT=IERR)
         tideuc = 0.0_dp
         allocate (tideus(0:3, 2:3, IDIM1), STAT=IERR)
         tideus = 0.0_dp

         call iniharmonics(recs)

         if (idebug >= 10) then
            write (6, *) '*** Start reading the harmonics '// &
               'table ***'
            write (6, *)
         end if
         !
         !        --- k and h love numbers for degree 2 and 3
         !
         rklove(1) = 0.0_dp
         rklove(2) = 0.303_dp
         rklove(3) = 0.0937_dp
         rhlove(1) = 0.0_dp
         rhlove(2) = 0.612_dp
         rhlove(3) = 0.293_dp
         !
         do nq = 2, 3
            factor(nq) = (1.0_dp + rklove(nq) - rhlove(nq))
         end do
         !
         ntable = 0
         nskip = 0
         N = 0
10       continue
         N = N + 1
         if (N > 484) then
            goto 20
         end if
         RECORD = RECS(N)
         ! read(luhar,'(a)',end=20) record
         if (idebug >= 10) then
            write (6, *) record
         end if
         if (record(1:1) == '%') then
            go to 10
         end if
         read (record, *) (kk(i), i=1, 7), har
         !
         !            in the CTE tables there is a null line for theoretic
         !        --- reasons, never use this line to compute the tidal
         !            potential.
         !
         permnt = kk(1) == 0 .and. kk(2) == 0 .and. &
                  kk(3) == 0 .and. kk(4) == 0 .and. &
                  kk(5) == 0 .and. kk(6) == 0
         !
         !        --- dtab is the doodson number for a table entry,
         !            select the lines where dstart <= dtab <= dstop
         !
         dtab1 = kk(1) * 100.0_dp + (kk(2) + 5.0_dp) * 10.0_dp + (kk(3) + 5.0_dp)
         dtab2 = (kk(4) + 5.0_dp) * 100.0_dp + (kk(5) + 5.0_dp) * 10.0_dp + (kk(6) + 5.0_dp)
         dtab = dtab1 + dtab2 / 1000.0_dp

         if (.not. permnt .and. abs(har) >= eps .and. dstart <= dtab .and. dtab <= dstop) then

            ntable = ntable + 1
            if (ntable > maxdat) then
               irc = -2
               return
            end if
            do i = 1, 7
               itable(ntable, i) = kk(i)
            end do
            nq = kk(7)
            amps(ntable) = har * g * factor(nq)
         else
            nskip = nskip + 1
         end if

         go to 10

20       continue
         ! rewind(luhar)

         rlslat = -9999.0_dp
         rlslon = -9999.0_dp

         if (idebug >= 10) then
            write (6, *) 'ntable = ', ntable, '   nskip = ', nskip
         end if
         !
         !        --- storage of uc, us
         !
         do i1 = 1, idim1

            rlat = yzeta(i1)
            rlong = xzeta(i1)
            !                     compute legendre polynomials times the global
            !                 --- normalization factor
            !                     (fnm) found in cartwright 1993, also compute the
            !                     astronomical elements
            !
            if (abs(rlat - rlslat) > reps) then
               do nq = 2, 3
                  do mq = 0, nq
                     fnm = 2.0_dp / real(2 * nq + 1, kind=dp) * factorial(nq + mq) / factorial(nq - mq)
                     fnm = sqrt(1.0_dp / (2.0_dp * pi * fnm)) * ((-1.0_dp)**mq)
                     call legpol1(rlat, nq, mq, pnm)
                     pol1(mq, nq) = fnm * pnm
                  end do
               end do
            end if

            if (abs(rlong - rlslon) > reps) then
               do mq = 0, 3
                  cm1(mq) = +cos(real(mq, kind=dp) * rlong)
                  sm1(mq) = +sin(real(mq, kind=dp) * rlong)
               end do
            end if

            !
            !                       --- compute arrays with tideuc, tideus
            !
            do nq = 2, 3
               do mq = 0, nq
                  if (mod(nq + mq, 2) == 0) then
                     tideuc(mq, nq, I1) = +cm1(mq) * pol1(mq, nq)
                     tideus(mq, nq, I1) = -sm1(mq) * pol1(mq, nq)
                  else
                     tideuc(mq, nq, I1) = +sm1(mq) * pol1(mq, nq)
                     tideus(mq, nq, I1) = +cm1(mq) * pol1(mq, nq)
                  end if

                  if (idebug >= 2 .and. i1 == i1dbg) then
                     write (6, 123) 'mq,nq=', mq, nq, &
                        ': rlat=', rlat, ', rlong=', rlong, &
                        ', pol1=', pol1(mq, nq), ', cm1=', cm1(mq), ',', &
                        'sm1=', sm1(mq), &
                        ', tideuc=', tideuc(mq, nq, I1), &
                        ', tideus=', tideus(mq, nq, I1)
123                  format(a, 2i2, 4(a, f8.4), a, /, 12x, 3(a, f8.4))
                  end if
               end do
            end do
            !
            !                 --- and finally save the last known latitude and
            !                     longitude
            !
            rlslat = rlat
            rlslon = rlong

         end do

      end if
      !
      !     --- end of initialisation phase
      !
      !     ------------------------------------------------------------------------
      !
      !     --- update elements-table
      !
      call astrol(rmjdat, elmnts)
      !
      !     --- compute tabels can, san
      !
      do i = 1, ntable
         argum = 0.0_dp
         do j = 1, 6
            argfct = real(itable(i, j), kind=dp)
            argum = argum + argfct * elmnts(j) * plsmin(j)
         end do
         ! argum = mod(argum, 360.0_dp)
         ! if (argum.lt.0.0_dp) argum = argum + 360.0_dp
         argum = argum * d2r
         can(i) = cos(argum) * amps(i)
         san(i) = sin(argum) * amps(i)

         !msgbuf = 'Tide generating component i, argum, amps(i) : '
         !write(msgbuf(50:),'(I8,2F10.4)') i,argum,amps(i)
         !call SetMessage(LEVEL_INFO, msgbuf)

      end do
      !
      !     --- compute tables cansum, sansum
      !
      do nq = 2, 3
         do mq = 0, nq
            cansum(mq, nq) = 0.0_dp
            sansum(mq, nq) = 0.0_dp
            do i = 1, ntable
               if (itable(i, 7) == nq .and. itable(i, 1) == mq) then
                  cansum(mq, nq) = cansum(mq, nq) + can(i)
                  sansum(mq, nq) = sansum(mq, nq) + san(i)
               end if
            end do
         end do
      end do
      !
      !     --- computation of the tidal potential at each grid-point:
      !
      do i1 = 1, idim1

         potent = 0.0_dp
         do nq = 2, 3
            do mq = 0, nq
               potent = potent + tideuc(mq, nq, I1) * cansum(mq, nq)
               potent = potent + tideus(mq, nq, I1) * sansum(mq, nq)
               ! if (idebug.ge.2 .and. i1.eq.i1dbg) then
               !   write(6,'(a,2i2,2(2(a,f10.6)))') 'mq,nq=',mq,nq,    &
               !      ': term uc=',tideuc(i1,mq,nq),'*',               &
               !      cansum(mq,nq),                                   &
               !      ', us=',tideus(i1,mq,nq),'*',                    &
               !      sansum(mq,nq)
               !end if
            end do
         end do
         tidep(i1) = potent
      end do

      ! if (idebug.ge.1 .and. i1dbg.ge.1)    write(6,*) 'tidep=', tidep(i1dbg)

   end subroutine tforce
   !
   !
   ! ==========================================================================
   !>
   subroutine astrol(mjdate, six)
      ! ====================================================================
      !
      !     DESCRIPTION
      !
      !     This copied from richard's subroutine astrol, in goes the
      use precision, only: dp
      !     modified Julian date, out comes an array of six real(kind=dp)
      !     variables used for Doodson number computations
      !
      !     Computes the basic astronomical mean longitudes  s, h, p, N.
      !     Note N is not N', i.e. N is decreasing with time.
      !     These formulae are for the period 1990 - 2010, and were derived
      !     by David Cartwright (personal comm., Nov. 1990).
      !     TIME is UTC in decimal MJD.
      !     All longitudes returned in degrees.
      !
      !     Non-vectorized version.
      !
      ! ********************************************************************
      !
      !     COMMON BLOCKS
      !
      implicit none
      !
      ! ********************************************************************
      !
      !     INPUT / OUTPUT   PARAMETERS
      !
      real(kind=dp) :: six(6), mjdate
      !
      !     mjdate      i    modified julian day (24-jan-2008 0:00 UTC : 54489.00000)
      !     six           o  array of six real(kind=dp) variables used for Doodson
      !                      number computations
      !                      see also Cartwright 1993, summer school lecture notes,
      !                      page 108
      !                      six(1) (tau) mean time angle in lunar days
      !                      six(2) (q)   mean longitude of moon
      !                      six(3) (q')  mean longitude of sun
      !                      six(4) (p)   mean longitude of lunar perigee
      !                      six(5) (N)   mean longitude of ascending lunar node
      !                      six(6) (p')  mean longitude of the Sun at perihelion
      !
      ! ********************************************************************
      !
      !     LOCAL PARAMETERS
      !
      !     --- constant values:
      !
      real(kind=dp) :: circle
      parameter(CIRCLE=360.0_dp)
      !
      !     circle       number of degrees in a circle
      !
      !     --- variables:
      !
      real(kind=dp) :: T, TIME, UT
      integer i
      !
      !     T           translated time: TIME - 51544.4993.0_dp
      !     TIME        input time (mjdate)
      !     UT          fractional part of mjdate: (mjdate - int(mjdate))
      !
      !=======================================================================
      !
      !     --- start of code
      !
      TIME = mjdate
      T = TIME - 51544.4993_dp ! reference to 2000/1/1 1200 o'clock
      !
      !     --- perform translations using translation table of symbols:
      !
      !         nr Cartwright  Doodson  Brown
      !         1  tau         tau      360*t-D+180
      !         2  q           s        L
      !         3  q'          h        L'
      !         4  p           p        \overline(\omega)
      !         5  N           -N'      \Omega
      !         6  p'          p_1      \overline{\omega}'
      !
      six(2) = 218.3164_dp + 13.17639648_dp * T
      six(3) = 280.4661_dp + 0.98564736_dp * T
      six(4) = 83.3535_dp + 0.11140353_dp * T
      six(5) = 125.0445_dp - 0.05295377_dp * T
      six(6) = 282.9384_dp + 0.0000471_dp * T
      !
      !     --- get them in the right quadrant
      !
      do i = 2, 6

         six(i) = mod(six(i), circle)
         if (six(i) < 0.0_dp) then
            six(i) = six(i) + circle
         end if

      end do
      !
      !         argument 1 in the doodson number denotes the mean lunar time.
      !         According to equation 13 and the inline remark after equation 14
      !         it is computed by
      !         alpha_G = 360 * "Universal time in fractions of a day" + q'(T) - 180
      !         tau = alpha_G - q
      !
      UT = (mjdate - int(mjdate))
      six(1) = 360.0_dp * UT + six(3) - 180.0_dp - six(2)
   end subroutine astrol
   !
   !
   ! ==========================================================================
   !>
   subroutine legpol1(theta, n, m, pnm)
      ! ====================================================================
      !
      !     DESCRIPTION
      !
      !     A routine to compute unnormalized associated legendre polynomials
      !     up to degree 3 and order 3.
      !
      ! ********************************************************************
      !
      !     COMMON BLOCKS
      !
      implicit none
      !
      ! ********************************************************************
      !
      !     INPUT / OUTPUT   PARAMETERS
      !
      integer n, m
      real(kind=dp) :: theta, pnm
      !
      !     m       i        degree of Legendre polynomial
      !     n       i        order of Legendre polynomial
      !     pnm       o      value of Legendre polynomial
      !     theta   i        phase
      !
      ! ********************************************************************
      !
      !     LOCAL PARAMETERS
      !
      real(kind=dp) :: cp, sp
      !
      !     cp          cos(theta)
      !     sp          sin(theta)
      !
      !=======================================================================
      !
      pnm = 1e38_dp

      cp = cos(theta)
      sp = sin(theta)
      !
      !     --- I think this comes from (Lambeck,1988), what again are the rules for
      !         obtaining associated Legendre functions?
      !
      if (n == 0) then
         if (m == 0) then
            pnm = 1.0_dp
         end if
      else if (n == 1) then
         if (m == 0) then
            pnm = sp
         end if
         if (m == 1) then
            pnm = cp
         end if
      else if (n == 2) then
         if (m == 0) then
            pnm = 1.5_dp * sp * sp - 0.5_dp
         end if
         if (m == 1) then
            pnm = 3.0_dp * sp * cp
         end if
         if (m == 2) then
            pnm = 3.0_dp * cp * cp
         end if
      else if (n == 3) then
         if (m == 0) then
            pnm = 2.5_dp * sp * sp * sp - 1.5_dp * sp
         end if
         if (m == 1) then
            pnm = cp * (7.5_dp * sp * sp - 1.5_dp)
         end if
         if (m == 2) then
            pnm = 15.0_dp * cp * cp * sp
         end if
         if (m == 3) then
            pnm = 15.0_dp * cp * cp * cp
         end if
      end if
   end subroutine legpol1
   !
   !
   ! ==========================================================================
   !>
   subroutine INIHARMONICS(RECS)
      character(len=40), dimension(484) :: RECS ! ZAT IN FILE 'HARMONICS'

      !%refsys 2000
      !%mj.0_dp    47893.00000000
      !%mjd1    55196.00000000
      !%dmjd         .11410938
      !%ndata   64000
      !%gmearth 3.9860044e14
      !%reearth 6378137
      RECS(1) = ' 0  0  0  0  0  0  2      -.31459'
      RECS(2) = ' 0  0  0  0  1  0  2       .02793'
      RECS(3) = ' 0  0  0  0  2  0  2      -.00028'
      RECS(4) = ' 0  0  0  2  1  0  2       .00004'
      RECS(5) = ' 0  0  1  0 -1 -1  2      -.00004'
      RECS(6) = ' 0  0  1  0  0 -1  2      -.00492'
      RECS(7) = ' 0  0  1  0  0  1  2       .00026'
      RECS(8) = ' 0  0  1  0  1 -1  2       .00004'
      RECS(9) = ' 0  0  2 -2 -1  0  2       .00002'
      RECS(10) = ' 0  0  2 -2  0  0  2      -.00031'
      RECS(11) = ' 0  0  2  0  0  0  2      -.03099'
      RECS(12) = ' 0  0  2  0  0 -2  2      -.00012'
      RECS(13) = ' 0  0  2  0  1  0  2       .00076'
      RECS(14) = ' 0  0  2  0  2  0  2       .00017'
      RECS(15) = ' 0  0  3  0  0 -1  2      -.00181'
      RECS(16) = ' 0  0  3  0  1 -1  2       .00003'
      RECS(17) = ' 0  0  4  0  0 -2  2      -.00007'
      RECS(18) = ' 0  1 -3  1 -1  1  2       .00002'
      RECS(19) = ' 0  1 -3  1  0  1  2      -.00029'
      RECS(20) = ' 0  1 -2 -1 -2  0  2       .00002'
      RECS(21) = ' 0  1 -2 -1 -1  0  2       .00006'
      RECS(22) = ' 0  1 -2  1 -1  0  2       .00048'
      RECS(23) = ' 0  1 -2  1  0  0  2      -.00673'
      RECS(24) = ' 0  1 -2  1  1  0  2       .00044'
      RECS(25) = ' 0  1 -1 -1  0  1  2      -.00021'
      RECS(26) = ' 0  1 -1  0  0  0  2       .00019'
      RECS(27) = ' 0  1 -1  1  0 -1  2       .00005'
      RECS(28) = ' 0  1  0 -1 -2  0  2      -.00003'
      RECS(29) = ' 0  1  0 -1 -1  0  2       .00231'
      RECS(30) = ' 0  1  0 -1  0  0  2      -.03518'
      RECS(31) = ' 0  1  0 -1  1  0  2       .00228'
      RECS(32) = ' 0  1  0  1  0  0  2       .00188'
      RECS(33) = ' 0  1  0  1  1  0  2       .00077'
      RECS(34) = ' 0  1  0  1  2  0  2       .00021'
      RECS(35) = ' 0  1  1 -1  0 -1  2       .00018'
      RECS(36) = ' 0  1  2 -1  0  0  2       .00049'
      RECS(37) = ' 0  1  2 -1  1  0  2       .00024'
      RECS(38) = ' 0  1  2 -1  2  0  2       .00004'
      RECS(39) = ' 0  1  3 -1  0 -1  2       .00002'
      RECS(40) = ' 0  2 -4  2  0  0  2      -.00011'
      RECS(41) = ' 0  2 -3  0  0  1  2      -.00039'
      RECS(42) = ' 0  2 -3  0  1  1  2       .00002'
      RECS(43) = ' 0  2 -2  0 -1  0  2      -.00042'
      RECS(44) = ' 0  2 -2  0  0  0  2      -.00584'
      RECS(45) = ' 0  2 -2  0  1  0  2       .00037'
      RECS(46) = ' 0  2 -2  2  0  0  2       .00004'
      RECS(47) = ' 0  2 -1 -2  0  1  2      -.00004'
      RECS(48) = ' 0  2 -1 -1  0  0  2       .00003'
      RECS(49) = ' 0  2 -1  0  0 -1  2       .00007'
      RECS(50) = ' 0  2 -1  0  0  1  2      -.00020'
      RECS(51) = ' 0  2 -1  0  1  1  2      -.00004'
      RECS(52) = ' 0  2  0 -2 -1  0  2       .00015'
      RECS(53) = ' 0  2  0 -2  0  0  2      -.00288'
      RECS(54) = ' 0  2  0 -2  1  0  2       .00019'
      RECS(55) = ' 0  2  0  0  0  0  2      -.06660'
      RECS(56) = ' 0  2  0  0  1  0  2      -.02761'
      RECS(57) = ' 0  2  0  0  2  0  2      -.00258'
      RECS(58) = ' 0  2  0  0  3  0  2       .00006'
      RECS(59) = ' 0  2  1 -2  0 -1  2       .00003'
      RECS(60) = ' 0  2  1  0  0 -1  2       .00023'
      RECS(61) = ' 0  2  1  0  1 -1  2       .00006'
      RECS(62) = ' 0  2  2 -2  0  0  2       .00020'
      RECS(63) = ' 0  2  2 -2  1  0  2       .00008'
      RECS(64) = ' 0  2  2  0  2  0  2       .00003'
      RECS(65) = ' 0  3 -5  1  0  1  2      -.00002'
      RECS(66) = ' 0  3 -4  1  0  0  2      -.00018'
      RECS(67) = ' 0  3 -3 -1  0  1  2      -.00007'
      RECS(68) = ' 0  3 -3  1  0  1  2      -.00011'
      RECS(69) = ' 0  3 -3  1  1  1  2      -.00005'
      RECS(70) = ' 0  3 -2 -1 -1  0  2      -.00009'
      RECS(71) = ' 0  3 -2 -1  0  0  2      -.00092'
      RECS(72) = ' 0  3 -2 -1  1  0  2       .00006'
      RECS(73) = ' 0  3 -2  1  0  0  2      -.00242'
      RECS(74) = ' 0  3 -2  1  1  0  2      -.00100'
      RECS(75) = ' 0  3 -2  1  2  0  2      -.00009'
      RECS(76) = ' 0  3 -1 -1  0  1  2      -.00013'
      RECS(77) = ' 0  3 -1 -1  1  1  2      -.00004'
      RECS(78) = ' 0  3 -1  0  0  0  2       .00007'
      RECS(79) = ' 0  3 -1  0  1  0  2       .00003'
      RECS(80) = ' 0  3 -1  1  0 -1  2       .00003'
      RECS(81) = ' 0  3  0 -3  0  0  2      -.00023'
      RECS(82) = ' 0  3  0 -3  1 -1  2       .00003'
      RECS(83) = ' 0  3  0 -3  1  1  2       .00003'
      RECS(84) = ' 0  3  0 -1  0  0  2      -.01275'
      RECS(85) = ' 0  3  0 -1  1  0  2      -.00529'
      RECS(86) = ' 0  3  0 -1  2  0  2      -.00050'
      RECS(87) = ' 0  3  0  1  2  0  2       .00005'
      RECS(88) = ' 0  3  0  1  3  0  2       .00002'
      RECS(89) = ' 0  3  1 -1  0 -1  2       .00011'
      RECS(90) = ' 0  3  1 -1  1 -1  2       .00004'
      RECS(91) = ' 0  4 -4  0  0  0  2      -.00008'
      RECS(92) = ' 0  4 -4  2  0  0  2      -.00006'
      RECS(93) = ' 0  4 -4  2  1  0  2      -.00003'
      RECS(94) = ' 0  4 -3  0  0  1  2      -.00014'
      RECS(95) = ' 0  4 -3  0  1  1  2      -.00006'
      RECS(96) = ' 0  4 -2 -2  0  0  2      -.00011'
      RECS(97) = ' 0  4 -2  0  0  0  2      -.00204'
      RECS(98) = ' 0  4 -2  0  1  0  2      -.00084'
      RECS(99) = ' 0  4 -2  0  2  0  2      -.00008'
      RECS(100) = ' 0  4 -1 -2  0  1  2      -.00003'
      RECS(101) = ' 0  4 -1  0  0 -1  2       .00003'
      RECS(102) = ' 0  4  0 -2  0  0  2      -.00169'
      RECS(103) = ' 0  4  0 -2  1  0  2      -.00070'
      RECS(104) = ' 0  4  0 -2  2  0  2      -.00007'
      RECS(105) = ' 1 -4  0  3 -1  0  2       .00014'
      RECS(106) = ' 1 -4  0  3  0  0  2       .00075'
      RECS(107) = ' 1 -4  1  1  0  1  2      -.00003'
      RECS(108) = ' 1 -4  2  1 -1  0  2       .00036'
      RECS(109) = ' 1 -4  2  1  0  0  2       .00194'
      RECS(110) = ' 1 -4  3  1  0 -1  2       .00015'
      RECS(111) = ' 1 -4  4 -1 -1  0  2       .00007'
      RECS(112) = ' 1 -4  4 -1  0  0  2       .00037'
      RECS(113) = ' 1 -4  5 -1  0 -1  2       .00004'
      RECS(114) = ' 1 -3 -1  2  0  1  2      -.00009'
      RECS(115) = ' 1 -3  0  0 -2  0  2      -.00004'
      RECS(116) = ' 1 -3  0  2 -2  0  2      -.00003'
      RECS(117) = ' 1 -3  0  2 -1  0  2       .00125'
      RECS(118) = ' 1 -3  0  2  0  0  2       .00664'
      RECS(119) = ' 1 -3  1  0  0  1  2      -.00011'
      RECS(120) = ' 1 -3  1  1  0  0  2      -.00007'
      RECS(121) = ' 1 -3  1  2  0 -1  2       .00010'
      RECS(122) = ' 1 -3  2  0 -2  0  2      -.00004'
      RECS(123) = ' 1 -3  2  0 -1  0  2       .00151'
      RECS(124) = ' 1 -3  2  0  0  0  2       .00801'
      RECS(125) = ' 1 -3  2  2  0  0  2      -.00007'
      RECS(126) = ' 1 -3  3  0 -1 -1  2       .00010'
      RECS(127) = ' 1 -3  3  0  0 -1  2       .00054'
      RECS(128) = ' 1 -3  4 -2 -1  0  2       .00005'
      RECS(129) = ' 1 -3  4 -2  0  0  2       .00024'
      RECS(130) = ' 1 -3  4  0  0  0  2      -.00008'
      RECS(131) = ' 1 -3  4  0  1  0  2       .00003'
      RECS(132) = ' 1 -2 -2  1 -2  0  2      -.00004'
      RECS(133) = ' 1 -2 -2  3  0  0  2      -.00016'
      RECS(134) = ' 1 -2 -1  1 -1  1  2      -.00007'
      RECS(135) = ' 1 -2 -1  1  0  1  2      -.00042'
      RECS(136) = ' 1 -2  0 -1 -3  0  2      -.00004'
      RECS(137) = ' 1 -2  0 -1 -2  0  2      -.00019'
      RECS(138) = ' 1 -2  0  1 -2  0  2      -.00029'
      RECS(139) = ' 1 -2  0  0  0  1  2       .00004'
      RECS(140) = ' 1 -2  0  1 -1  0  2       .00947'
      RECS(141) = ' 1 -2  0  1  0  0  2       .05019'
      RECS(142) = ' 1 -2  0  3  0  0  2      -.00014'
      RECS(143) = ' 1 -2  1 -1  0  1  2      -.00009'
      RECS(144) = ' 1 -2  1  0 -1  0  2      -.00005'
      RECS(145) = ' 1 -2  1  0  0  0  2      -.00027'
      RECS(146) = ' 1 -2  1  1 -1 -1  2       .00007'
      RECS(147) = ' 1 -2  1  1  0 -1  2       .00046'
      RECS(148) = ' 1 -2  2 -1 -2  0  2      -.00005'
      RECS(149) = ' 1 -2  2 -1 -1  0  2       .00180'
      RECS(150) = ' 1 -2  2 -1  0  0  2       .00953'
      RECS(151) = ' 1 -2  2  1  0  0  2      -.00055'
      RECS(152) = ' 1 -2  2  1  1  0  2       .00017'
      RECS(153) = ' 1 -2  3 -1 -1 -1  2       .00008'
      RECS(154) = ' 1 -2  3 -1  0 -1  2       .00044'
      RECS(155) = ' 1 -2  3  1  0 -1  2      -.00004'
      RECS(156) = ' 1 -2  4 -1  0  0  2      -.00012'
      RECS(157) = ' 1 -1 -2  0 -2  0  2      -.00012'
      RECS(158) = ' 1 -1 -2  2 -1  0  2      -.00014'
      RECS(159) = ' 1 -1 -2  2  0  0  2      -.00079'
      RECS(160) = ' 1 -1 -1  0 -1  1  2      -.00011'
      RECS(161) = ' 1 -1 -1  0  0  1  2      -.00090'
      RECS(162) = ' 1 -1 -1  1  0  0  2       .00004'
      RECS(163) = ' 1 -1  0  0 -2  0  2      -.00152'
      RECS(164) = ' 1 -1  0  0 -1  0  2       .04946'
      RECS(165) = ' 1 -1  0  0  0  0  2       .26216'
      RECS(166) = ' 1 -1  0  2 -1  0  2       .00005'
      RECS(167) = ' 1 -1  0  2  0  0  2      -.00169'
      RECS(168) = ' 1 -1  0  2  1  0  2      -.00028'
      RECS(169) = ' 1 -1  1  0 -1 -1  2       .00008'
      RECS(170) = ' 1 -1  1  0  0 -1  2       .00076'
      RECS(171) = ' 1 -1  2 -2  0  0  2      -.00015'
      RECS(172) = ' 1 -1  2  0 -1  0  2       .00010'
      RECS(173) = ' 1 -1  2  0  0  0  2      -.00343'
      RECS(174) = ' 1 -1  2  0  1  0  2       .00075'
      RECS(175) = ' 1 -1  2  0  2  0  2       .00005'
      RECS(176) = ' 1 -1  3  0  0 -1  2      -.00022'
      RECS(177) = ' 1 -1  4 -2  0  0  2      -.00007'
      RECS(178) = ' 1  0 -3  1  0  1  2      -.00009'
      RECS(179) = ' 1  0 -2  1 -1  0  2      -.00044'
      RECS(180) = ' 1  0 -2  1  0  0  2      -.00193'
      RECS(181) = ' 1  0 -1  0  0  0  2       .00005'
      RECS(182) = ' 1  0 -1  1  0  1  2       .00010'
      RECS(183) = ' 1  0  0 -1 -2  0  2       .00012'
      RECS(184) = ' 1  0  0 -1 -1  0  2      -.00137'
      RECS(185) = ' 1  0  0 -1  0  0  2      -.00741'
      RECS(186) = ' 1  0  0  1 -1  0  2       .00059'
      RECS(187) = ' 1  0  0  1  0  0  2      -.02062'
      RECS(188) = ' 1  0  0  1  1  0  2      -.00414'
      RECS(189) = ' 1  0  0  1  2  0  2       .00012'
      RECS(190) = ' 1  0  1  0  0  0  2       .00012'
      RECS(191) = ' 1  0  1  1  0 -1  2      -.00013'
      RECS(192) = ' 1  0  2 -1 -1  0  2       .00011'
      RECS(193) = ' 1  0  2 -1  0  0  2      -.00394'
      RECS(194) = ' 1  0  2 -1  1  0  2      -.00087'
      RECS(195) = ' 1  0  3 -1  0 -1  2      -.00017'
      RECS(196) = ' 1  0  3 -1  1 -1  2      -.00004'
      RECS(197) = ' 1  1 -4  0  0  2  2       .00029'
      RECS(198) = ' 1  1 -3  0 -1  1  2      -.00006'
      RECS(199) = ' 1  1 -3  0  0  1  2       .00713'
      RECS(200) = ' 1  1 -2  0 -2  0  2       .00010'
      RECS(201) = ' 1  1 -2  0 -1  0  2      -.00137'
      RECS(202) = ' 1  1 -2  0  0  0  2       .12199'
      RECS(203) = ' 1  1 -2  0  0  2  2      -.00007'
      RECS(204) = ' 1  1 -2  2  0  0  2      -.00018'
      RECS(205) = ' 1  1 -2  2  1  0  2      -.00004'
      RECS(206) = ' 1  1 -1  0  0 -1  2      -.00102'
      RECS(207) = ' 1  1 -1  0  0  1  2      -.00288'
      RECS(208) = ' 1  1 -1  0  1  1  2       .00008'
      RECS(209) = ' 1  1  0 -2 -1  0  2      -.00007'
      RECS(210) = ' 1  1  0  0 -2  0  2      -.00005'
      RECS(211) = ' 1  1  0  0 -1  0  2       .00730'
      RECS(212) = ' 1  1  0  0  0  0  2      -.36872'
      RECS(213) = ' 1  1  0  0  1  0  2      -.05002'
      RECS(214) = ' 1  1  0  0  2  0  2       .00108'
      RECS(215) = ' 1  1  1  0  0 -1  2      -.00292'
      RECS(216) = ' 1  1  1  0  1 -1  2      -.00005'
      RECS(217) = ' 1  1  2 -2  0  0  2      -.00018'
      RECS(218) = ' 1  1  2 -2  1  0  2      -.00005'
      RECS(219) = ' 1  1  2  0  0 -2  2      -.00007'
      RECS(220) = ' 1  1  2  0  0  0  2      -.00525'
      RECS(221) = ' 1  1  2  0  1  0  2       .00020'
      RECS(222) = ' 1  1  2  0  2  0  2       .00010'
      RECS(223) = ' 1  1  3  0  0 -1  2      -.00030'
      RECS(224) = ' 1  2 -3  1  0  1  2      -.00017'
      RECS(225) = ' 1  2 -2 -1 -1  0  2      -.00012'
      RECS(226) = ' 1  2 -2  1 -1  0  2       .00012'
      RECS(227) = ' 1  2 -2  1  0  0  2      -.00394'
      RECS(228) = ' 1  2 -2  1  1  0  2      -.00078'
      RECS(229) = ' 1  2 -1 -1  0  1  2      -.00013'
      RECS(230) = ' 1  2 -1  0  0  0  2       .00011'
      RECS(231) = ' 1  2  0 -1 -1  0  2       .00060'
      RECS(232) = ' 1  2  0 -1  0  0  2      -.02062'
      RECS(233) = ' 1  2  0 -1  1  0  2      -.00409'
      RECS(234) = ' 1  2  0 -1  2  0  2       .00008'
      RECS(235) = ' 1  2  0  1  0  0  2       .00032'
      RECS(236) = ' 1  2  0  1  1  0  2       .00020'
      RECS(237) = ' 1  2  0  1  2  0  2       .00012'
      RECS(238) = ' 1  2  1 -1  0 -1  2       .00011'
      RECS(239) = ' 1  2  2 -1  0  0  2       .00008'
      RECS(240) = ' 1  2  2 -1  1  0  2       .00006'
      RECS(241) = ' 1  3 -4  2  0  0  2      -.00006'
      RECS(242) = ' 1  3 -3  0  0  1  2      -.00023'
      RECS(243) = ' 1  3 -3  0  1  1  2      -.00004'
      RECS(244) = ' 1  3 -2  0 -1  0  2      -.00011'
      RECS(245) = ' 1  3 -2  0  0  0  2      -.00342'
      RECS(246) = ' 1  3 -2  0  1  0  2      -.00067'
      RECS(247) = ' 1  3 -1  0  0 -1  2       .00007'
      RECS(248) = ' 1  3  0 -2 -1  0  2       .00004'
      RECS(249) = ' 1  3  0 -2  0  0  2      -.00169'
      RECS(250) = ' 1  3  0 -2  1  0  2      -.00034'
      RECS(251) = ' 1  3  0  0  0  0  2      -.01128'
      RECS(252) = ' 1  3  0  0  1  0  2      -.00723'
      RECS(253) = ' 1  3  0  0  2  0  2      -.00151'
      RECS(254) = ' 1  3  0  0  3  0  2      -.00010'
      RECS(255) = ' 1  3  1  0  0 -1  2       .00004'
      RECS(256) = ' 1  4 -4  1  0  0  2      -.00011'
      RECS(257) = ' 1  4 -3 -1  0  1  2      -.00004'
      RECS(258) = ' 1  4 -2 -1  0  0  2      -.00054'
      RECS(259) = ' 1  4 -2 -1  1  0  2      -.00010'
      RECS(260) = ' 1  4 -2  1  0  0  2      -.00041'
      RECS(261) = ' 1  4 -2  1  1  0  2      -.00026'
      RECS(262) = ' 1  4 -2  1  2  0  2      -.00005'
      RECS(263) = ' 1  4  0 -3  0  0  2      -.00014'
      RECS(264) = ' 1  4  0 -1  0  0  2      -.00216'
      RECS(265) = ' 1  4  0 -1  1  0  2      -.00138'
      RECS(266) = ' 1  4  0 -1  2  0  2      -.00029'
      RECS(267) = ' 2 -4  0  4  0  0  2       .00018'
      RECS(268) = ' 2 -4  2  2  0  0  2       .00077'
      RECS(269) = ' 2 -4  3  2  0 -1  2       .00006'
      RECS(270) = ' 2 -4  4  0  0  0  2       .00048'
      RECS(271) = ' 2 -4  5  0  0 -1  2       .00006'
      RECS(272) = ' 2 -3  0  3 -1  0  2      -.00007'
      RECS(273) = ' 2 -3  0  3  0  0  2       .00180'
      RECS(274) = ' 2 -3  1  1  0  1  2      -.00009'
      RECS(275) = ' 2 -3  1  3  0 -1  2       .00004'
      RECS(276) = ' 2 -3  2  1 -1  0  2      -.00017'
      RECS(277) = ' 2 -3  2  1  0  0  2       .00467'
      RECS(278) = ' 2 -3  3  1  0 -1  2       .00035'
      RECS(279) = ' 2 -3  4 -1 -1  0  2      -.00003'
      RECS(280) = ' 2 -3  4 -1  0  0  2       .00090'
      RECS(281) = ' 2 -3  5 -1  0 -1  2       .00010'
      RECS(282) = ' 2 -2 -2  4  0  0  2      -.00006'
      RECS(283) = ' 2 -2 -1  2  0  1  2      -.00022'
      RECS(284) = ' 2 -2  0  0 -2  0  2      -.00010'
      RECS(285) = ' 2 -2  0  2 -1  0  2      -.00060'
      RECS(286) = ' 2 -2  0  2  0  0  2       .01601'
      RECS(287) = ' 2 -2  1  0  0  1  2      -.00027'
      RECS(288) = ' 2 -2  1  1  0  0  2      -.00017'
      RECS(289) = ' 2 -2  1  2  0 -1  2       .00025'
      RECS(290) = ' 2 -2  2  0 -1  0  2      -.00072'
      RECS(291) = ' 2 -2  2  0  0  0  2       .01932'
      RECS(292) = ' 2 -2  3 -1  0  0  2      -.00004'
      RECS(293) = ' 2 -2  3  0 -1 -1  2      -.00005'
      RECS(294) = ' 2 -2  3  0  0 -1  2       .00130'
      RECS(295) = ' 2 -2  4 -2  0  0  2       .00059'
      RECS(296) = ' 2 -2  4  0  0 -2  2       .00005'
      RECS(297) = ' 2 -2  5 -2  0 -1  2       .00005'
      RECS(298) = ' 2 -1 -2  1 -2  0  2      -.00010'
      RECS(299) = ' 2 -1 -2  3  0  0  2      -.00039'
      RECS(300) = ' 2 -1 -1  1 -1  1  2       .00003'
      RECS(301) = ' 2 -1 -1  1  0  1  2      -.00102'
      RECS(302) = ' 2 -1  0 -1 -2  0  2      -.00046'
      RECS(303) = ' 2 -1  0  1 -2  0  2       .00007'
      RECS(304) = ' 2 -1  0  0  0  1  2       .00009'
      RECS(305) = ' 2 -1  0  1 -1  0  2      -.00451'
      RECS(306) = ' 2 -1  0  1  0  0  2       .12099'
      RECS(307) = ' 2 -1  1 -1  0  1  2      -.00023'
      RECS(308) = ' 2 -1  1  0  0  0  2      -.00065'
      RECS(309) = ' 2 -1  1  1 -1 -1  2      -.00004'
      RECS(310) = ' 2 -1  1  1  0 -1  2       .00113'
      RECS(311) = ' 2 -1  2 -1 -1  0  2      -.00086'
      RECS(312) = ' 2 -1  2 -1  0  0  2       .02298'
      RECS(313) = ' 2 -1  2  1  0  0  2       .00010'
      RECS(314) = ' 2 -1  2  1  1  0  2      -.00008'
      RECS(315) = ' 2 -1  3 -1 -1 -1  2      -.00004'
      RECS(316) = ' 2 -1  3 -1  0 -1  2       .00106'
      RECS(317) = ' 2  0 -3  2  0  1  2      -.00008'
      RECS(318) = ' 2  0 -2  0 -2  0  2      -.00028'
      RECS(319) = ' 2  0 -2  2 -1  0  2       .00007'
      RECS(320) = ' 2  0 -2  2  0  0  2      -.00190'
      RECS(321) = ' 2  0 -1  0 -1  1  2       .00005'
      RECS(322) = ' 2  0 -1  0  0  1  2      -.00217'
      RECS(323) = ' 2  0 -1  1  0  0  2       .00010'
      RECS(324) = ' 2  0  0  0 -2  0  2       .00033'
      RECS(325) = ' 2  0  0  0 -1  0  2      -.02358'
      RECS(326) = ' 2  0  0  0  0  0  2       .63194'
      RECS(327) = ' 2  0  0  2  0  0  2       .00036'
      RECS(328) = ' 2  0  0  2  1  0  2       .00013'
      RECS(329) = ' 2  0  1  0 -1 -1  2      -.00004'
      RECS(330) = ' 2  0  1  0  0 -1  2       .00192'
      RECS(331) = ' 2  0  2 -2  0  0  2      -.00036'
      RECS(332) = ' 2  0  2  0  0  0  2       .00072'
      RECS(333) = ' 2  0  2  0  1  0  2      -.00036'
      RECS(334) = ' 2  0  2  0  2  0  2       .00012'
      RECS(335) = ' 2  0  3  0  0 -1  2       .00005'
      RECS(336) = ' 2  1 -3  1  0  1  2      -.00022'
      RECS(337) = ' 2  1 -2  1 -1  0  2       .00021'
      RECS(338) = ' 2  1 -2  1  0  0  2      -.00466'
      RECS(339) = ' 2  1 -1 -1  0  1  2      -.00007'
      RECS(340) = ' 2  1 -1  0  0  0  2       .00010'
      RECS(341) = ' 2  1  0 -1 -1  0  2       .00065'
      RECS(342) = ' 2  1  0 -1  0  0  2      -.01786'
      RECS(343) = ' 2  1  0  1 -1  0  2      -.00008'
      RECS(344) = ' 2  1  0  1  0  0  2       .00447'
      RECS(345) = ' 2  1  0  1  1  0  2       .00197'
      RECS(346) = ' 2  1  0  1  2  0  2       .00028'
      RECS(347) = ' 2  1  2 -1  0  0  2       .00085'
      RECS(348) = ' 2  1  2 -1  1  0  2       .00041'
      RECS(349) = ' 2  1  2 -1  2  0  2       .00005'
      RECS(350) = ' 2  2 -4  0  0  2  2       .00070'
      RECS(351) = ' 2  2 -3  0  0  1  2       .01719'
      RECS(352) = ' 2  2 -2  0 -1  0  2       .00066'
      RECS(353) = ' 2  2 -2  0  0  0  2       .29401'
      RECS(354) = ' 2  2 -2  2  0  0  2       .00004'
      RECS(355) = ' 2  2 -1  0  0 -1  2      -.00246'
      RECS(356) = ' 2  2 -1  0  0  1  2       .00062'
      RECS(357) = ' 2  2 -1  0  1  1  2      -.00004'
      RECS(358) = ' 2  2  0  0 -1  0  2      -.00103'
      RECS(359) = ' 2  2  0  0  0  0  2       .07992'
      RECS(360) = ' 2  2  0  0  1  0  2       .02382'
      RECS(361) = ' 2  2  0  0  2  0  2       .00259'
      RECS(362) = ' 2  2  1  0  0 -1  2       .00063'
      RECS(363) = ' 2  2  2 -2  0  0  2       .00004'
      RECS(364) = ' 2  2  2  0  0  0  2       .00053'
      RECS(365) = ' 2  3 -3  1  0  1  2       .00003'
      RECS(366) = ' 2  3 -2 -1 -1  0  2       .00006'
      RECS(367) = ' 2  3 -2 -1  0  0  2       .00004'
      RECS(368) = ' 2  3 -2  1  0  0  2       .00085'
      RECS(369) = ' 2  3 -2  1  1  0  2       .00037'
      RECS(370) = ' 2  3 -2  1  2  0  2       .00004'
      RECS(371) = ' 2  3  0 -1 -1  0  2      -.00009'
      RECS(372) = ' 2  3  0 -1  0  0  2       .00447'
      RECS(373) = ' 2  3  0 -1  1  0  2       .00195'
      RECS(374) = ' 2  3  0 -1  2  0  2       .00022'
      RECS(375) = ' 2  3  0  1  0  0  2      -.00003'
      RECS(376) = ' 2  4 -3  0  0  1  2       .00005'
      RECS(377) = ' 2  4 -2  0  0  0  2       .00074'
      RECS(378) = ' 2  4 -2  0  1  0  2       .00032'
      RECS(379) = ' 2  4 -2  0  2  0  2       .00003'
      RECS(380) = ' 2  4  0 -2  0  0  2       .00037'
      RECS(381) = ' 2  4  0 -2  1  0  2       .00016'
      RECS(382) = ' 2  4  0  0  0  0  2       .00117'
      RECS(383) = ' 2  4  0  0  1  0  2       .00101'
      RECS(384) = ' 2  4  0  0  2  0  2       .00033'
      RECS(385) = ' 2  4  0  0  3  0  2       .00005'
      RECS(386) = ' 0  0  0  1  0  0  3      -.00021'
      RECS(387) = ' 0  0  2 -1  0  0  3      -.00004'
      RECS(388) = ' 0  1 -2  0  0  0  3       .00004'
      RECS(389) = ' 0  1  0  0 -1  0  3       .00019'
      RECS(390) = ' 0  1  0  0  0  0  3      -.00375'
      RECS(391) = ' 0  1  0  0  1  0  3      -.00059'
      RECS(392) = ' 0  1  0  0  2  0  3       .00005'
      RECS(393) = ' 0  2 -2  1  0  0  3      -.00012'
      RECS(394) = ' 0  2  0 -1  0  0  3      -.00061'
      RECS(395) = ' 0  2  0 -1  1  0  3      -.00010'
      RECS(396) = ' 0  3 -2  0  0  0  3      -.00010'
      RECS(397) = ' 0  3  0 -2  0  0  3      -.00007'
      RECS(398) = ' 0  3  0  0  0  0  3      -.00031'
      RECS(399) = ' 0  3  0  0  1  0  3      -.00019'
      RECS(400) = ' 0  3  0  0  2  0  3      -.00004'
      RECS(401) = ' 0  4  0 -1  0  0  3      -.00008'
      RECS(402) = ' 0  4  0 -1  1  0  3      -.00005'
      RECS(403) = ' 1 -4  0  2  0  0  3       .00006'
      RECS(404) = ' 1 -4  2  0  0  0  3       .00006'
      RECS(405) = ' 1 -3  0  1 -1  0  3       .00014'
      RECS(406) = ' 1 -3  0  1  0  0  3       .00035'
      RECS(407) = ' 1 -3  2 -1  0  0  3       .00006'
      RECS(408) = ' 1 -2  0  0 -2  0  3       .00004'
      RECS(409) = ' 1 -2  0  0 -1  0  3       .00051'
      RECS(410) = ' 1 -2  0  0  0  0  3       .00128'
      RECS(411) = ' 1 -2  0  2  0  0  3       .00008'
      RECS(412) = ' 1 -2  2  0  0  0  3       .00011'
      RECS(413) = ' 1 -1  0 -1  0  0  3      -.00007'
      RECS(414) = ' 1 -1  0  1 -1  0  3      -.00009'
      RECS(415) = ' 1 -1  0  1  0  0  3       .00065'
      RECS(416) = ' 1 -1  0  1  1  0  3      -.00009'
      RECS(417) = ' 1 -1  2 -1  0  0  3       .00013'
      RECS(418) = ' 1  0  0  0 -1  0  3      -.00059'
      RECS(419) = ' 1  0  0  0  0  0  3       .00399'
      RECS(420) = ' 1  0  0  0  1  0  3      -.00052'
      RECS(421) = ' 1  1 -2  1  0  0  3       .00004'
      RECS(422) = ' 1  1  0 -1 -1  0  3      -.00003'
      RECS(423) = ' 1  1  0 -1  0  0  3       .00022'
      RECS(424) = ' 1  1  0 -1  1  0  3      -.00003'
      RECS(425) = ' 1  1  0  1  0  0  3       .00008'
      RECS(426) = ' 1  1  0  1  1  0  3       .00003'
      RECS(427) = ' 1  2 -2  0  0  0  3       .00005'
      RECS(428) = ' 1  2  0  0 -1  0  3      -.00005'
      RECS(429) = ' 1  2  0  0  0  0  3       .00146'
      RECS(430) = ' 1  2  0  0  1  0  3       .00059'
      RECS(431) = ' 1  2  0  0  2  0  3       .00005'
      RECS(432) = ' 1  3 -2  1  0  0  3       .00005'
      RECS(433) = ' 1  3  0 -1  0  0  3       .00024'
      RECS(434) = ' 1  3  0 -1  1  0  3       .00010'
      RECS(435) = ' 1  4 -2  0  0  0  3       .00004'
      RECS(436) = ' 1  4  0  0  0  0  3       .00005'
      RECS(437) = ' 1  4  0  0  1  0  3       .00005'
      RECS(438) = ' 2 -4  2  1  0  0  3      -.00006'
      RECS(439) = ' 2 -3  0  2  0  0  3      -.00019'
      RECS(440) = ' 2 -3  2  0 -1  0  3      -.00003'
      RECS(441) = ' 2 -3  2  0  0  0  3      -.00019'
      RECS(442) = ' 2 -2  0  1 -1  0  3      -.00018'
      RECS(443) = ' 2 -2  0  1  0  0  3      -.00107'
      RECS(444) = ' 2 -2  2 -1 -1  0  3      -.00003'
      RECS(445) = ' 2 -2  2 -1  0  0  3      -.00020'
      RECS(446) = ' 2 -1  0  0 -2  0  3       .00004'
      RECS(447) = ' 2 -1  0  0 -1  0  3      -.00066'
      RECS(448) = ' 2 -1  0  0  0  0  3      -.00389'
      RECS(449) = ' 2 -1  0  2  0  0  3       .00007'
      RECS(450) = ' 2 -1  2  0  0  0  3       .00010'
      RECS(451) = ' 2  0 -2  1  0  0  3       .00005'
      RECS(452) = ' 2  0  0 -1 -1  0  3       .00004'
      RECS(453) = ' 2  0  0 -1  0  0  3       .00022'
      RECS(454) = ' 2  0  0  1 -1  0  3      -.00003'
      RECS(455) = ' 2  0  0  1  0  0  3       .00059'
      RECS(456) = ' 2  0  0  1  1  0  3       .00011'
      RECS(457) = ' 2  0  2 -1  0  0  3       .00011'
      RECS(458) = ' 2  1  0  0 -1  0  3      -.00021'
      RECS(459) = ' 2  1  0  0  0  0  3       .00359'
      RECS(460) = ' 2  1  0  0  1  0  3       .00067'
      RECS(461) = ' 2  2 -2  1  0  0  3       .00004'
      RECS(462) = ' 2  2  0 -1  0  0  3       .00019'
      RECS(463) = ' 2  2  0 -1  1  0  3       .00004'
      RECS(464) = ' 2  3 -2  0  0  0  3       .00004'
      RECS(465) = ' 2  3  0  0  0  0  3       .00033'
      RECS(466) = ' 2  3  0  0  1  0  3       .00021'
      RECS(467) = ' 2  3  0  0  2  0  3       .00004'
      RECS(468) = ' 2  4  0 -1  0  0  3       .00005'
      RECS(469) = ' 3 -2  0  2  0  0  3      -.00036'
      RECS(470) = ' 3 -2  2  0  0  0  3      -.00036'
      RECS(471) = ' 3 -1  0  1 -1  0  3       .00012'
      RECS(472) = ' 3 -1  0  1  0  0  3      -.00210'
      RECS(473) = ' 3 -1  2 -1  0  0  3      -.00039'
      RECS(474) = ' 3  0 -2  2  0  0  3       .00005'
      RECS(475) = ' 3  0  0  0 -1  0  3       .00043'
      RECS(476) = ' 3  0  0  0  0  0  3      -.00765'
      RECS(477) = ' 3  1 -2  1  0  0  3       .00011'
      RECS(478) = ' 3  1  0 -1  0  0  3       .00043'
      RECS(479) = ' 3  1  0  1  0  0  3      -.00016'
      RECS(480) = ' 3  1  0  1  1  0  3      -.00007'
      RECS(481) = ' 3  2  0  0 -1  0  3       .00004'
      RECS(482) = ' 3  2  0  0  0  0  3      -.00100'
      RECS(483) = ' 3  2  0  0  1  0  3      -.00044'
      RECS(484) = ' 3  2  0  0  2  0  3      -.00005'
   end subroutine INIHARMONICS
end module timespace_data
!
!
!
! ==========================================================================
! ==========================================================================
! ==========================================================================
!>
module M_arcuv ! plotbuitenbeentje
   use precision, only: dp
   implicit none
   private

   real(kind=dp), allocatable, public :: arcuv(:, :, :)
end module M_arcuv
!
!
!
! ==========================================================================
! ==========================================================================
! ==========================================================================
!>
module m_spiderweb ! plot spiderweb
   use precision, only: dp
   implicit none
   private

   real(kind=dp), allocatable, public :: spw(:, :, :)
end module m_spiderweb
!
!
!
! ==========================================================================
! ==========================================================================
! ==========================================================================
!>
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
module timespace_triangle

   use precision
   use timespace_data
   use m_alloc

   implicit none

   integer :: nsold ! nr of samples in previous triangulation
   integer :: numtri
   integer, allocatable, dimension(:, :) :: indx
   real(kind=dp), allocatable, dimension(:) :: xcent
   real(kind=dp), allocatable, dimension(:) :: ycent

   interface triint
      module procedure triint_z1D
      module procedure triint_z2D
      module procedure triint_z3D
   end interface triint

   interface get_extend
      module procedure get_extend1D
      module procedure get_extend2D
   end interface get_extend

   interface find_nearest
      module procedure find_nearest1D
      module procedure find_nearest2D
      module procedure find_nearest1D_missing_value
      module procedure find_nearest2D_missing_value
   end interface find_nearest

contains
   !
   !
   ! ==========================================================================
   !>
   subroutine pinpok(xl, yl, n, x, y, inside)

      implicit none

      real(kind=dp), intent(in) :: xl, yl ! point under consideration
      integer, intent(in) :: n
      real(kind=dp), dimension(n), intent(in) :: x, y ! polygon(n)
      integer, intent(out) :: inside

      integer :: i, i1, i2, np, rechts
      real(kind=dp) :: rl, rm, x1, x2, y1, y2

      if (n <= 2) then
         inside = 1
      else
         np = 0
5        continue
         np = np + 1
         if (np <= n) then
            if (x(np) /= dmiss_default) then
               goto 5
            end if
         end if
         np = np - 1
         inside = 0
         rechts = 0
         i = 0
10       continue
         i1 = mod(i, np) + 1
         i2 = mod(i1, np) + 1
         x1 = x(i1)
         x2 = x(i2)
         y1 = y(i1)
         y2 = y(i2)
         if (xl >= min(x1, x2) .and. xl <= max(x1, x2)) then
            if (xl == x1 .and. yl == y1 .or. & ! tussen of op lijnstuk
                (x1 == x2 .and. & ! op punt 1
                 yl >= min(y1, y2) .and. yl <= max(y1, y2)) .or. & ! op verticale lijn
                (yl == y1 .and. y1 == y2)) then ! op horizontale lijn
               inside = 1
               return
            else if (x1 /= x2) then ! scheve lijn
               rl = (xl - x1) / (x2 - x1)
               rm = (y1 - yl) + rl * (y2 - y1)
               if (rm == 0) then ! op scheve lijn
                  inside = 1
                  return
               else if (rm > 0.0_dp) then ! onder scheve lijn
                  if (xl == x1 .or. xl == x2) then
                     if (x1 > xl .or. x2 > xl) then
                        rechts = rechts + 1
                     end if
                  end if
                  inside = 1 - inside
               end if
            end if
         end if
         i = i + 1
         if (i < np) then
            goto 10
         end if
         if (mod(rechts, 2) /= 0) then
            inside = 1 - inside
         end if
      end if
   end subroutine pinpok
   !
   !
   ! ==========================================================================
   !>
   ! This subroutine interpolates one unstructured dataset xss, yss, zss, kcss, nss to another x, y, z, kcs, nx
   ! It is the only one in this module that is of practical interest to the meteo module.
   ! The rest of the subroutines in this module are assisting this one.
   ! JDLA = 1 (re)triangulates
   subroutine triint_z2D(xss, yss, zss, kcsss, nss, &
                         x, y, z, kcs, kx, mnx, jdla, indxn, wfn)

      implicit none

      ! Global variables
      integer, intent(in) :: nss ! Dimension of samples
      real(kind=dp), dimension(:), intent(in) :: xss ! samples
      real(kind=dp), dimension(:), intent(in) :: yss
      real(kind=dp), dimension(:), intent(in) :: zss ! dimension: nss*kx
      integer, dimension(:), intent(in) :: kcsss ! samples mask

      integer, intent(in) :: mnx ! Dimension of grid
      integer, intent(in) :: kx ! vectormax
      real(kind=dp), dimension(:), intent(in) :: x ! grid
      real(kind=dp), dimension(:), intent(in) :: y
      real(kind=dp), dimension(:, :), intent(out) :: z ! dimension: nx*kx
      integer, dimension(:), intent(in) :: kcs ! grid mask
      integer, intent(in) :: jdla ! refresh delauney yes /no

      integer, optional :: indxn(:, :) ! if present get weightfactors and indices
      real(kind=dp), optional :: wfn(:, :)

      call triint_z1D(xss, yss, zss, kcsss, nss, &
                      x, y, z, kcs, kx, mnx, jdla, indxn, wfn)
   end subroutine triint_z2D
   !
   !
   ! ==========================================================================
   !>
   subroutine triint_z3D(xss, yss, zss, kcsss, nss, &
                         x, y, z, kcs, kx, mnx, jdla, indxn, wfn)

      implicit none

      ! Global variables
      integer, intent(in) :: nss ! Dimension of samples
      real(kind=dp), dimension(:), intent(in) :: xss ! samples
      real(kind=dp), dimension(:), intent(in) :: yss
      real(kind=dp), dimension(:), intent(in) :: zss ! dimension: nss*kx
      integer, dimension(:), intent(in) :: kcsss ! samples mask

      integer, intent(in) :: mnx ! Dimension of grid
      integer, intent(in) :: kx ! vectormax
      real(kind=dp), dimension(:), intent(in) :: x ! grid
      real(kind=dp), dimension(:), intent(in) :: y
      real(kind=dp), dimension(:, :, :), intent(out) :: z ! dimension: nx*kx
      integer, dimension(:), intent(in) :: kcs ! grid mask
      integer, intent(in) :: jdla ! refresh delauney yes /no

      integer, optional :: indxn(:, :) ! if present get weightfactors and indices
      real(kind=dp), optional :: wfn(:, :)

      call triint_z1D(xss, yss, zss, kcsss, nss, &
                      x, y, z, kcs, kx, mnx, jdla, indxn, wfn)

   end subroutine triint_z3D
   !
   !
   ! ==========================================================================
   !>
   subroutine triint_z1D(xss, yss, zss, kcsss, nss, &
                         x, y, z, kcs, kx, mnx, jdla, indxn, wfn)

      use m_ec_basic_interpolation, only: dlaun

      implicit none

      ! Global variables
      integer, intent(in) :: nss ! Dimension of samples
      real(kind=dp), dimension(:), intent(in) :: xss ! samples
      real(kind=dp), dimension(:), intent(in) :: yss
      real(kind=dp), dimension(:), intent(in) :: zss ! dimension: nss*kx
      integer, dimension(:), intent(in) :: kcsss ! samples mask

      integer, intent(in) :: mnx ! Dimension of grid
      integer, intent(in) :: kx ! vectormax
      real(kind=dp), dimension(:), intent(in) :: x ! grid
      real(kind=dp), dimension(:), intent(in) :: y
      real(kind=dp), dimension(kx*mnx), intent(out) :: z ! dimension: mnx*kx
      integer, dimension(:), intent(in) :: kcs ! grid mask
      integer, intent(in) :: jdla ! refresh delauney yes /no

      integer, optional :: indxn(:, :) ! if present get weightfactors and indices
      real(kind=dp), optional :: wfn(:, :)

      ! Local variables

      real(kind=dp), dimension(8) :: x_set
      real(kind=dp), dimension(8) :: y_set
      integer, dimension(8) :: kcs_set = 1
      real(kind=dp), dimension(4) :: x_extr
      real(kind=dp), dimension(4) :: y_extr
      real(kind=dp), dimension(4) :: z_extr
      real(kind=dp), dimension(3) :: zp
      integer, dimension(3) :: indxp

      real(kind=dp), dimension(:), allocatable :: xs
      real(kind=dp), dimension(:), allocatable :: ys
      real(kind=dp), dimension(:), allocatable :: zs
      integer, dimension(:), allocatable :: kcss
      integer :: ns
      integer :: k, n, jgetw, ierr ! , MOUT

      logical :: extra = .false. ! nu even niet

   !! executable statements -------------------------------------------------------
      !
      !     JDLA=1, DO DE LAUNEY
      !     JSLO=1, ALSO SLOPES RD4

      if (nss < 1) then
         return
      end if

      call realloc(xs, nss + 8, 1)
      call realloc(ys, nss + 8, 1)
      call realloc(zs, nss + 8, 1)
      call realloc(kcss, nss + 8, 1)

      ns = 0
      do k = 1, nss
         if (kcsss(k) == 1) then
            ns = ns + 1
            xs(ns) = xss(k)
            ys(ns) = yss(k)
            do n = 1, kx
               zs(kx * (ns - 1) + n) = zss(kx * (k - 1) + n)
            end do
            kcss(ns) = 1
         end if
      end do

      if (extra) then
         call get_extend(mnx, x, y, kcs, x_set(1:4), y_set(1:4))
         call get_extend(ns, xs, ys, kcss, x_set(5:8), y_set(5:8))
         call get_extend(8, x_set, y_set, kcs_set, x_extr, y_extr)

         call extrapolate(ns, xs, ys, zs, kcss, 4, x_extr, y_extr, z_extr)

         xs(ns + 1:ns + 4) = x_extr
         ys(ns + 1:ns + 4) = y_extr
         zs(ns + 1:ns + 4) = z_extr

         ns = ns + 4
      end if

      if (jdla == 1) then
         ! call dlauny(xs, ys, ns)
         call DLAUN(XS, YS, NS, 1, ierr)

      end if

      jgetw = 0 ! niets met gewichten, doe interpolatie
      if (present(indxn) .and. jdla == 1) then
         jgetw = 1 ! haal gewichten       doe interpolatie , gebruik gewichten
      end if
      if (present(indxn) .and. jdla == 0) then
         jgetw = 2 !                      doe interpolatie , gebruik gewichten
      end if

      do n = 1, mnx
         if (kcs(n) == 1) then
            if (jgetw <= 1) then
               call findtri_indices_weights(x(n), y(n), xs, ys, ns, zp, indxp) ! zoeken bij 0 en 1
            end if

            if (jgetw == 1) then ! zetten bij 1
               do k = 1, 3
                  indxn(k, n) = indxp(k)
                  wfn(k, n) = zp(k)
               end do
            else if (jgetw == 2) then ! halen bij 2, je hoeft niet te zoeken
               do k = 1, 3
                  indxp(k) = indxn(k, n)
                  zp(k) = wfn(k, n)
               end do
            end if

            ! en altijd interpoleren

            do k = 1, kx ! over vectormax loop
               if (indxp(1) == 0 .or. indxp(2) == 0 .or. indxp(3) == 0) then
                  !  z(mnx*(k-1) + n) = -999
               else
                  z(mnx * (k - 1) + n) = zp(1) * zs(kx * (indxp(1) - 1) + k) + zp(2) * zs(kx * (indxp(2) - 1) + k) + zp(3) * zs(kx * (indxp(3) - 1) + k)
               end if
            end do

         end if
      end do
   end subroutine triint_z1D
   !
   !
   ! ==========================================================================
   !>
   subroutine findtri_indices_weights(xp, yp, xs, ys, ns, zp, indxp)

      implicit none

      ! Global variables
      real(kind=dp), intent(in) :: xp ! for this point
      real(kind=dp), intent(in) :: yp

      integer, intent(in) :: ns
      real(kind=dp), dimension(ns), intent(in) :: xs ! on this set
      real(kind=dp), dimension(ns), intent(in) :: ys

      integer, dimension(3), intent(out) :: indxp ! find indices to set
      real(kind=dp), dimension(3), intent(out) :: zp ! and corresponding weightfactors

      ! Local variables
      integer :: k
      integer :: k1
      integer :: k2, n3
      integer :: intri
      integer :: nroldfind, nrfind
      real(kind=dp) :: xtmax
      real(kind=dp) :: xtmin
      real(kind=dp) :: ytmax
      real(kind=dp) :: ytmin
      real(kind=dp), dimension(3) :: xt
      real(kind=dp), dimension(3) :: yt
      !
      !
      data nroldfind/0/
      !
   !! executable statements -------------------------------------------------------
      !
      !
      indxp = 0
      n3 = 3

5     continue
      if (nroldfind /= 0) then
         k1 = max(1, nroldfind - 200)
         k2 = min(numtri, nroldfind + 200)
      else
         k1 = 1
         k2 = numtri
      end if
      !
      do k = k1, k2

         xt(1) = xs(indx(1, k))
         xt(2) = xs(indx(2, k))
         xt(3) = xs(indx(3, k))
         yt(1) = ys(indx(1, k))
         yt(2) = ys(indx(2, k))
         yt(3) = ys(indx(3, k))
         xtmax = max(xt(1), max(xt(2), xt(3)))
         ytmax = max(yt(1), max(yt(2), yt(3)))
         xtmin = min(xt(1), min(xt(2), xt(3)))
         ytmin = min(yt(1), min(yt(2), yt(3)))
         if (xp >= xtmin .and. xp <= xtmax .and. yp >= ytmin .and. yp <= ytmax) then
            call pinpok(xp, yp, n3, xt, yt, intri)
            if (intri == 1) then
               nrfind = k
               nroldfind = nrfind
               indxp(1) = indx(1, k)
               indxp(2) = indx(2, k)
               indxp(3) = indx(3, k)
               call linweight(xt, yt, xp, yp, zp)
               ! write(*,*) xp, yp, k, indxp(1), indxp(2), indxp(3)
               return
            end if
         end if
      end do
      if (nroldfind /= 0) then
         nroldfind = 0
         goto 5
      end if
   end subroutine findtri_indices_weights
   !
   !
   ! ==========================================================================
   !>
   subroutine linweight(xt, yt, xp, yp, zp)

      real(kind=dp), intent(in) :: xp ! for this point
      real(kind=dp), intent(in) :: yp

      real(kind=dp), dimension(3) :: xt ! in this triangle
      real(kind=dp), dimension(3) :: yt

      real(kind=dp), dimension(3), intent(out) :: zp ! the weightfactors are...

      real(kind=dp) :: a11, a12, a21, a22, b1, b2, det

      zp = 0
      a11 = xt(2) - xt(1)
      a21 = yt(2) - yt(1)
      a12 = xt(3) - xt(1)
      a22 = yt(3) - yt(1)
      b1 = xp - xt(1)
      b2 = yp - yt(1)

      det = a11 * a22 - a12 * a21
      if (abs(det) < 1e-9) then
         return
      end if
      !
      zp(2) = (a22 * b1 - a12 * b2) / det
      zp(3) = (-a21 * b1 + a11 * b2) / det
      zp(1) = 1.0_dp - zp(2) - zp(3)

   end subroutine linweight
   !
   !
   ! ==========================================================================
   !>
   subroutine linear(x, y, z, xp, yp, &
                   & zp, jslo, slo)
      use precision
      implicit none
      !
      !
      ! COMMON variables
      !
      real(kind=dp) :: dmiss

      data dmiss/-999.0_dp/
      !
      ! Global variables
      !
      integer, intent(in) :: jslo
      real(kind=dp), intent(out) :: slo
      real(kind=dp) :: xp
      real(kind=dp) :: yp
      real(kind=dp) :: zp
      real(kind=dp), dimension(3) :: x
      real(kind=dp), dimension(3) :: y
      real(kind=dp), dimension(3), intent(in) :: z
      !
      !
      ! Local variables
      !

      real(kind=dp) :: a11
      real(kind=dp) :: a12
      real(kind=dp) :: a21
      real(kind=dp) :: a22
      real(kind=dp) :: a31
      real(kind=dp) :: a32
      real(kind=dp) :: b1
      real(kind=dp) :: b2
      real(kind=dp) :: det
      real(kind=dp) :: r3
      real(kind=dp) :: rlam
      real(kind=dp) :: rmhu
      real(kind=dp) :: x3
      real(kind=dp) :: xn
      real(kind=dp) :: xy
      real(kind=dp) :: y3
      real(kind=dp) :: yn
      real(kind=dp) :: z3
      real(kind=dp) :: zn
      !
      !
   !! executable statements -------------------------------------------------------
      !
      !
      !
      !
      !
      zp = dmiss
      a11 = x(2) - x(1)
      a21 = y(2) - y(1)
      a12 = x(3) - x(1)
      a22 = y(3) - y(1)
      b1 = xp - x(1)
      b2 = yp - y(1)
      !
      det = a11 * a22 - a12 * a21
      if (abs(det) < 1e-12) then
         return
      end if
      !
      rlam = (a22 * b1 - a12 * b2) / det
      rmhu = (-a21 * b1 + a11 * b2) / det
      !
      zp = z(1) + rlam * (z(2) - z(1)) + rmhu * (z(3) - z(1))
      if (jslo == 1) then
         a31 = z(2) - z(1)
         a32 = z(3) - z(1)
         x3 = (a21 * a32 - a22 * a31)
         y3 = -(a11 * a32 - a12 * a31)
         z3 = (a11 * a22 - a12 * a21)
         r3 = sqrt(x3 * x3 + y3 * y3 + z3 * z3)
         if (r3 /= 0) then
            xn = x3 / r3
            yn = y3 / r3
            zn = z3 / r3
            xy = sqrt(xn * xn + yn * yn)
            if (zn /= 0) then
               slo = abs(xy / zn)
            else
               slo = dmiss
            end if
         else
            slo = dmiss
         end if
      end if
   end subroutine linear
   !
   !
   ! ==========================================================================
   !>
   subroutine get_extend2D(n, m, x, y, kcs, x_dummy, y_dummy)

      real(kind=dp), dimension(:, :) :: x
      real(kind=dp), dimension(:, :) :: y
      integer, dimension(:, :) :: kcs
      integer :: n
      integer :: m
      real(kind=dp), dimension(:) :: x_dummy
      real(kind=dp), dimension(:) :: y_dummy

      call get_extend1D(n * m, x, y, kcs, x_dummy, y_dummy)

   end subroutine get_extend2D
   !
   !
   ! ==========================================================================
   !>
   subroutine get_extend1D(n, x, y, kcs, x_dummy, y_dummy)

      integer :: n
      real(kind=dp), dimension(n) :: x
      real(kind=dp), dimension(n) :: y
      integer, dimension(n) :: kcs
      real(kind=dp), dimension(4) :: x_dummy
      real(kind=dp), dimension(4) :: y_dummy
      real(kind=dp) :: x_min
      real(kind=dp) :: x_max
      real(kind=dp) :: x_dist
      real(kind=dp) :: y_min
      real(kind=dp) :: y_max
      real(kind=dp) :: y_dist
      integer :: index

      x_min = 1e30
      x_max = -1e30
      y_min = 1e30
      y_max = -1e30

      do index = 1, n
         if (kcs(index) == 1) then
            if (x_min > x(index)) then
               x_min = x(index)
            end if
            if (x_max < x(index)) then
               x_max = x(index)
            end if
            if (y_min > y(index)) then
               y_min = y(index)
            end if
            if (y_max < y(index)) then
               y_max = y(index)
            end if
         end if
      end do

      x_dist = x_max - x_min
      y_dist = y_max - y_min
      x_min = x_min - 0.01_dp * x_dist
      x_max = x_max + 0.01_dp * x_dist
      y_min = y_min - 0.01_dp * y_dist
      y_max = y_max + 0.01_dp * y_dist

      x_dummy(1) = x_min
      y_dummy(1) = y_min
      x_dummy(2) = x_min
      y_dummy(2) = y_max
      x_dummy(3) = x_max
      y_dummy(3) = y_max
      x_dummy(4) = x_max
      y_dummy(4) = y_min

   end subroutine get_extend1D
   !
   !
   ! ==========================================================================
   !>
   subroutine extrapolate(n, x, y, z, kcs, n_extr, x_extr, y_extr, z_extr)

      integer :: n
      real(kind=dp), dimension(n) :: x
      real(kind=dp), dimension(n) :: y
      real(kind=dp), dimension(n) :: z
      integer, dimension(n) :: kcs
      integer :: n_extr
      real(kind=dp), dimension(n_extr), target :: x_extr
      real(kind=dp), dimension(n_extr), target :: y_extr
      real(kind=dp), dimension(n_extr), target :: z_extr
      integer :: i_extr
      integer :: i_min
      real(kind=dp), pointer :: x_a
      real(kind=dp), pointer :: y_a
      real(kind=dp), pointer :: z_a
      real(kind=dp) :: dist_min

      dist_min = 1e30
      i_min = 0

      do i_extr = 1, n_extr
         x_a => x_extr(i_extr)
         y_a => y_extr(i_extr)
         z_a => z_extr(i_extr)
         call find_nearest(n, x, y, z, kcs, x_a, y_a, i_min, dist_min)
         z_a = z(i_min)
      end do

   end subroutine extrapolate
   !
   !
   ! ==========================================================================
   !>
   subroutine find_nearest2D(n, m, x, y, kcs, x_a, y_a, n_min, m_min, dist_min)

      use precision

      integer :: n
      integer :: m
      real(kind=dp), dimension(:, :) :: x
      real(kind=dp), dimension(:, :) :: y
      integer, dimension(:, :) :: kcs
      integer :: n_min
      integer :: m_min
      integer :: i_min
      real(kind=dp) :: x_a
      real(kind=dp) :: y_a
      real(kind=dp) :: dist_min

      call find_nearest1D(n * m, x, y, kcs, x_a, y_a, i_min, dist_min)

      m_min = i_min / n
      n_min = i_min - (m_min * n)
      m_min = m_min + 1

   end subroutine find_nearest2D
   !
   !
   ! ==========================================================================
   !>
   subroutine find_nearest2D_missing_value(n, m, x, y, z, kcs, x_a, y_a, n_min, m_min, dist_min)

      use precision

      integer :: n
      integer :: m
      real(kind=dp), dimension(:, :) :: x
      real(kind=dp), dimension(:, :) :: y
      real(kind=dp), dimension(:, :) :: z
      integer, dimension(:, :) :: kcs
      integer :: n_min
      integer :: m_min
      integer :: i_min
      real(kind=dp) :: x_a
      real(kind=dp) :: y_a
      real(kind=dp) :: dist_min

      call find_nearest1D_missing_value(n * m, x, y, z, kcs, x_a, y_a, i_min, dist_min)

      m_min = i_min / n
      n_min = i_min - (m_min * n)
      m_min = m_min + 1

   end subroutine find_nearest2D_missing_value
   !
   !
   ! ==========================================================================
   !>
   subroutine find_nearest1D(n, x, y, kcs, x_a, y_a, i_min, dist_min)

      use precision

      integer :: n
      real(kind=dp), dimension(n) :: x
      real(kind=dp), dimension(n) :: y
      integer, dimension(n) :: kcs
      integer :: i
      integer :: i_min
      real(kind=dp) :: x_a
      real(kind=dp) :: y_a
      real(kind=dp) :: dist
      real(kind=dp) :: dist_min

      dist_min = 1e30
      i_min = 0

      do i = 1, n
         if (kcs(i) == 1) then
            dist = (x(i) - x_a)**2 + (y(i) - y_a)**2
            if (dist < dist_min) then
               dist_min = dist
               i_min = i
            end if
         end if
      end do

      dist_min = sqrt(dist_min)

   end subroutine find_nearest1D
   !
   !
   ! ==========================================================================
   !>
   subroutine find_nearest1D_missing_value(n, x, y, z, kcs, x_a, y_a, i_min, dist_min)

      use precision

      integer :: n
      real(kind=dp), dimension(n) :: x
      real(kind=dp), dimension(n) :: y
      real(kind=dp), dimension(n) :: z
      integer, dimension(n) :: kcs
      integer :: i
      integer :: i_min
      real(kind=dp) :: x_a
      real(kind=dp) :: y_a
      real(kind=dp) :: dist
      real(kind=dp) :: dist_min

      dist_min = 1e30
      i_min = 0

      do i = 1, n
         if (kcs(i) == 1) then
            dist = (x(i) - x_a)**2 + (y(i) - y_a)**2
            if ((dist < dist_min) .and. (z(i) /= -999.0_dp)) then
               dist_min = dist
               i_min = i
            end if
         end if
      end do

      dist_min = sqrt(dist_min)

   end subroutine find_nearest1D_missing_value
   !
   ! ==========================================================================
   !>
   !subroutine polyindexweight( xe, ye, xs, ys, kcs, ns, xyen, k1, rl)    ! interpolate in a polyline like way
   !
   ! ! Global variables
   ! integer ,                intent(in)     :: ns       ! Dimension of polygon OR LINE BOUNDARY
   ! real(kind=dp), dimension(:),  intent(in) :: xs       ! polygon
   ! real(kind=dp), dimension(:),  intent(in) :: ys
   ! integer, dimension(:),  intent(in)      :: kcs      ! polygon mask
   ! real(kind=dp)                        :: xyen(:)
   ! real(kind=dp)                        :: xe, ye, rl
   !
   !
   ! integer :: ja1, ja2, k, km, k1, k2
   ! real(kind=dp):: x1,x2,y1,y2,dis,xn,yn,dx,dy
   ! real(kind=dp):: dism, dis1, dis2, rl1, rl2, dbdistance
   !
   !
   ! dism = 1e30
   ! do k = 1, ns
   !    dis  = DbdISTANCE( Xe,Ye,XS(K),YS(K) )
   !    if (dis < dism) then
   !       dism = dis
   !       km   = k
   !    end if
   ! enddo
   !
   ! k1 = 0
   !
   ! if (km == 1) then
   !    x1 = xs(km  ); y1 = ys(km  )
   !    x2 = xs(km+1); y2 = ys(km+1)
   !    call LINEDISQ(Xe,Ye,X1,Y1,X2,Y2,JA1,DIS1,XN,YN,RL)
   !    if (ja1 == 1) then
   !       if (dis1 < rdis) k1 = km
   !    end if
   ! else if (km == ns) then
   !    x1 = xs(km-1); y1 = ys(km-1)
   !    x2 = xs(km  ); y2 = ys(km  )
   !    call LINEDISQ(Xe,Ye,X1,Y1,X2,Y2,JA1,DIS1,XN,YN,RL)
   !    if (ja1 == 1) then
   !       if (dis1 < rdis) k1 = km-1
   !    end if
   ! else
   !    x1 = xs(km-1); y1 = ys(km-1)
   !    x2 = xs(km)  ; y2 = ys(km)
   !    call LINEDISQ(Xe,Ye,X1,Y1,X2,Y2,JA1,DIS1,XN,YN,RL1)
   !    x1 = xs(km)  ; y1 = ys(km)
   !    x2 = xs(km+1); y2 = ys(km+1)
   !    call LINEDISQ(Xe,Ye,X1,Y1,X2,Y2,JA2,DIS2,XN,YN,RL2)
   !    if      (ja1 == 1) then ! if on line 1
   !        if (dis1 < rdis) then
   !           k1 = km-1 ; rl = rl1
   !        end if
   !    else if (ja2 == 1) then
   !        if (dis2 < rdis) then
   !           k1 = km ; rl = rl2
   !        end if
   !    else ! niet op een van beiden, maar wel in de buurt, uitwerken. Nu dus alleen convexe randen
   !    end if
   ! end if
   !
   !end subroutine polyindexweight
   !
   !
   ! ==========================================================================
   !>
   !> Selects the index of the polyline segment that intersects with line e--en
   !! with the intersection closest to point e.
   !! The search range is thus from e to en, and not a distance rdis as before.
   !! The normal direction is now
   !! defined by e--en and not normal to the polyline. Also, *all* polyline
   !! segments are checked, not the closest based on dbdistance of pli points.
   subroutine polyindexweight(xe, ye, xen, yen, xs, ys, kcs, ns, kL, wL, kR, wR)

      use m_sferic
      use geometry_module, only: dbdistance, cross
      use m_missing, only: dmiss

      ! Global variables
      integer, intent(in) :: ns !< Dimension of polygon OR LINE BOUNDARY
      real(kind=dp), intent(in) :: xs(:) !< polygon
      real(kind=dp), intent(in) :: ys(:)
      integer, intent(in) :: kcs(:) !< polygon mask
      real(kind=dp), intent(in) :: xe, ye !
      real(kind=dp), intent(in) :: xen, yen !< in input uitstekers, on output SL and CRP
      integer, intent(out) :: kL !< Index of left nearest polyline point (with kcs==1!)
      real(kind=dp), intent(out) :: wL !< Relative weight of left nearest polyline point.
      integer, intent(out) :: kR !< Index of right nearest polyline point (with kcs==1!)
      real(kind=dp), intent(out) :: wR !< Relative weight of right nearest polyline point.

      integer :: k, km, JACROS
      real(kind=dp) :: dis, disM, disL, disR !, rl1, rl2,
      real(kind=dp) :: SL, SM, SMM, SLM, XCR, YCR, CRP, CRPM, DEPS

      DISM = huge(DISM)
      kL = 0 ! Default: No valid point found
      kR = 0 ! idem
      wL = 0.0_dp
      wR = 0.0_dp
      km = 0
      crpm = 0
      disL = 0.0_dp
      disR = 0.0_dp
      DEPS = 1.0e-3_dp

      do k = 1, ns - 1

         call cross(xe, ye, xen, yen, xs(k), ys(k), xs(k + 1), ys(k + 1), JACROS, SL, SM, XCR, YCR, CRP, jsferic, dmiss)

         if (SL >= 0.0_dp .and. SL <= 1.0_dp .and. SM > -DEPS .and. SM < 1.0_dp + DEPS) then ! instead of jacros==1
            DIS = DBDISTANCE(XE, YE, XCR, YCR, jsferic, jasfer3D, dmiss)
            if (DIS < DISM) then ! Found a better intersection point
               DISM = DIS
               km = k
               SMM = SM
               SLM = SL
               CRPM = CRP
            end if
         end if
      end do

      if (km > 0) then
         dis = dbdistance(xs(km), ys(km), xs(km + 1), ys(km + 1), jsferic, jasfer3D, dmiss) ! Length of this segment.

         ! Find nearest valid polyline point left of the intersection (i.e.: kcs(kL) == 1)
         disL = SMM * dis
         do k = km, 1, -1
            if (kcs(k) == 1) then
               kL = k
               exit ! Valid point on the left (distance was already included in disL)
            else if (k > 1) then
               disL = disL + dbdistance(xs(k - 1), ys(k - 1), xs(k), ys(k), jsferic, jasfer3D, dmiss) ! Add entire length of this segment.
            end if
         end do

         ! Find nearest valid polyline point right of the intersection (i.e.: kcs(kR) == 1)
         disR = (1.0_dp - SMM) * dis
         do k = km + 1, ns
            if (kcs(k) == 1) then
               kR = k
               exit ! Valid point on the left (distance was already included in disL)
            else if (k < ns) then
               disR = disR + dbdistance(xs(k), ys(k), xs(k + 1), ys(k + 1), jsferic, jasfer3D, dmiss) ! Add entire length of this segment.
            end if
         end do
      end if

      if (kL /= 0 .and. kR /= 0) then
         wL = disR / (disL + disR)
         wR = 1.0_dp - wL
      else if (kL /= 0) then
         wL = 1.0_dp
      else if (kR /= 0) then
         wR = 1.0_dp
      end if

   end subroutine polyindexweight
   !
end module timespace_triangle ! met leading dimensions 3 of 4
!
!
!
! ==========================================================================
! ==========================================================================
! ==========================================================================
!>
module timespace
!!--description-----------------------------------------------------------------
!
! Read time series in five possible formats:
! uniform       : Delft3D-FLOW format: time, uniform windspeed, direction and pressure
! space varying : Delft3D-FLOW format: time and fields of air_pressure, windx, windy
!                 on Delft3D-FLOW m,n grid
! arcinfo       : time and fields on own equidistant grid
! spiderweb     : time and fields of air_pressure, windspeed, direction op spiderweb grid
! curvi         : time and fields on own curvilinear grid
!
! Main calls from Delft3D-FLOW:
! readmd -> rdmeteo:
!             initmeteo        : allocate meteo structure for this domain
!             adddataprovider  : allocate and initialized an input quantity
!                                with specified format
!             checkmeteo       : check whether input is available for the complete
!                                time interval
! trisol  -> incmeteo:
!             meteoupdate      : prepare meteo data for the current time
!             getmeteoval      : return meteo data for the current time and position
!                                use optional m and n parameters to speed up in case of curvi
!             getspiderval     : same as getmeteoval for spiderweb data
!
! gdp_dealloc:
!             deallocmeteo
!
! Additional calls:
!    getmeteoerror    : returns a string containing an error message
!                       to be used in case success = false for a main call
!    meteogetpaver    : returns the average atmospheric pressure read
!    meteogetpcorr    : returns whether pressure correction is switched on on
!                       the boundaries
!
!!--declarations----------------------------------------------------------------
   use precision

   use timespace_data
   use timespace_triangle
   implicit none

contains
   !
   !
   ! ==========================================================================
   !>
   !> this function selects points (kc = 1) that can receive data from the provider in file =filename
   !! All points have an allowable 'search range', defined by a line from x,y
   !! to xyen(1,) to xyen(2,). Generally, the points in xyen are endpoints of
   !! rrtol times a perpendicular vector to edge links.
   subroutine selectelset(filename, filetype, x, y, xyen, kc, mnx, ki, num, usemask, rrtolrel, pliname)

      use MessageHandling
      use m_inquire_flowgeom
      use geometry_module, only: cross
      use m_missing, only: dmiss
      use m_sferic, only: jsferic
      use m_partitioninfo, only: jampi
      use m_filez, only: oldfil

      implicit none

      ! arguments
      integer, intent(in) :: mnx !< dimension of quantity
      real(kind=dp), intent(in) :: x(:) !< x   of elset of all possible points in model
      real(kind=dp), intent(in) :: y(:) !< y   of elset
      real(kind=dp), intent(in) :: xyen(:, :) !< Points on opposite edges of elementset
      integer, intent(inout) :: kc(:) !< kcs of elset, allowable kandidates have 1, eg. points with less links than edges
      integer, intent(out) :: ki(:) !< Returned indices of allowable points (in x/y) that fall near provided data
      integer :: num !< nr of points served bij this provider

      character(*), intent(in) :: filename ! file name for meteo data file
      integer, intent(in) :: filetype ! spw, arcinfo, uniuvp etc
      logical, intent(in) :: usemask !< Whether to use the mask array kc, or not (allows you to keep kc, but disable it for certain quantities, for example salinitybnd).
      real(kind=dp), intent(in), optional :: rrtolrel !< Optional, a more strict rrtolerance value than the global rrtol. selectelset will succeed if cross SL value <= rrtolrel
      character(len=:), allocatable, optional :: pliname !< Optional, name (identifier) of pli

      ! locals
      real(kind=dp), allocatable :: xs(:) ! temporary array to hold polygon
      real(kind=dp), allocatable :: ys(:) !
      integer, allocatable :: kcs(:) !
      real(kind=dp) :: wL, wR
      integer :: kL, kR, minp, ns, m
      integer :: JACROS
      integer :: ierr
      real(kind=dp) :: SL, SM, XCR, YCR, CRP
      logical :: has_more_pli

      num = 0

      ki = 0

      if (filetype == poly_tim) then

         call oldfil(minp, filename)
         call read1polylin(minp, xs, ys, ns, pliname, has_more_records=has_more_pli)

         if (has_more_pli) then
            call mess(LEVEL_WARN, 'While reading polyline file '''//trim(filename)//''': multiple polylines are not supported in a single file.')
            call mess(LEVEL_WARN, 'Only using first polyline '''//trim(pliname)//''' and ignoring the rest.')
         end if

         if (.not. allocated(kcs)) then
            allocate (kcs(ns))
         else if (ns > size(kcs)) then
            call realloc(kcs, ns, keepExisting=.false.)
         end if

         kcs = 1 ! todo make this safe

         do m = 1, mnx
            if (abs(kc(m)) == 1) then ! point is a possible candidate for a line boundary
               call polyindexweight(x(m), y(m), xyen(1, m), xyen(2, m), xs, ys, kcs, ns, kL, wL, kR, wR)
               if (kL > 0 .or. kR > 0) then
                  if (present(rrtolrel)) then
                     ! x,y -> xyen =approx D + 2*rrtol * D
                     ! This bnd requests a more strict tolerance than the global rrtol, namely: D + 2*rrtolb * D, so:
                     call CROSS(x(m), y(m), xyen(1, m), xyen(2, m), xs(kL), ys(kL), xs(kR), ys(kR), JACROS, SL, SM, XCR, YCR, CRP, jsferic, dmiss)
                     if (SL > rrtolrel) then
                        ! More strict rrtolrel check failed, so do not accept this node.
                        cycle
                     end if
                  end if
                  if (usemask .and. kc(m) == -1) then
                     write (errormessage, '(a,i8.8,a,f12.4,a,f12.4,a)') 'Boundary link ', m, ' already claimed [', (x(m) + xyen(1, m)) / 2., ',', (y(m) + xyen(2, m)) / 2., ']'
                     call mess(LEVEL_WARN, errormessage)
                     cycle
                  else
                     num = num + 1
                     ki(num) = m
                     if (usemask) then ! If we don't use the mask, also don't administer this opened bnd location (e.g. for salinitybnd)
                        kc(m) = -1 ! this tells you this point is already claimed by some bnd
                     end if
                  end if
               end if
            end if
         end do
         write (msgbuf, '(a,a,a,i0,a)') 'boundary: ''', trim(filename), ''' opened ', num, ' cells.'
         call msg_flush()

         deallocate (xs, ys, kcs)

      elseif (filetype == NODE_ID) then

         ierr = findlink_by_nodeid(filename, m)
         if (m <= 0) then
            if (jampi == 0) then
               errormessage = 'Boundary nodeId '''//trim(filename)//''' was not found in the network.'
               call mess(LEVEL_WARN, errormessage)
            end if
            return
         else if (m > size(kc)) then
            errormessage = 'Boundary nodeId '''//trim(filename)//''' exceeds the network size.'
            call mess(LEVEL_WARN, errormessage)
            return
         else
            if (usemask .and. kc(m) == -1) then
               errormessage = 'Boundary with nodeId '''//trim(filename)//''' already claimed; Overlap with other bnds?'
               call mess(LEVEL_WARN, errormessage)
               return
            else if (usemask .and. kc(m) == 0) then
               errormessage = 'Boundary with nodeId '''//trim(filename)//''' is not an allowed location.  AllowBndAtBifurcation=1 might solve this.'
               call mess(LEVEL_WARN, errormessage)
               return
            else
               num = num + 1
               ki(num) = m
               if (usemask) then ! If we don't use the mask, also don't administer this opened bnd location (e.g. for salinitybnd)
                  kc(m) = -1 ! this tells you this point is already claimed by some bnd
               end if
            end if
         end if
      end if
   end subroutine selectelset
   !
   !
   ! ==========================================================================
   !> Selects a subset of flow links that match certain geometrical input.
   !! Possible geometrical inputs are:
   !! * polylines: all flow links intersecting these polylines are selected.
   !! * polygons:  all flow links whose center lies inside these polygons are selected.
   !! * branchid+chainage: the one flow link on this location is selected.
   !! * contactid: the one flow link on this mesh contact is selected.
   !! Only one of these methods is tried, based on loc_spec_type input.
   subroutine selectelset_internal_links(lnx, keg, numg, &
                                         loc_spec_type, loc_file, nump, xpin, ypin, branchindex, chainage, contactId, linktype, &
                                         xps, yps, nps, lftopol, sortLinks)
      use m_inquire_flowgeom
      use m_flowgeom, only: lnx1D, xu, yu, kcu
      use dfm_error
      use messageHandling
      use m_polygon
      use m_reapol
      use m_filez, only: oldfil
      use network_data, only: LINK_1D, LINK_2D, LINK_1D2D_INTERNAL, LINK_1D2D_LONGITUDINAL, LINK_1D2D_STREETINLET, LINK_1D_MAINBRANCH, LINK_1D2D_ROOF, LINK_ALL

      implicit none

      !inputs
      integer, intent(in) :: lnx !< Number of flow links in input. (Currently unused).
      integer, intent(out) :: keg(:) !< Output array containing the flow link numbers that were selected.
      !< Size of array is responsability of call site, and filling starts at index 1 upon each call.
      integer, intent(out) :: numg !< Number of flow links that were selected (i.e., keg(1:numg) will be filled).
      integer, intent(in) :: loc_spec_type !< Type of spatial input for selecting nodes. One of: LOCTP_POLYGON_FILE, LOCTP_POLYLINE_FILE, LOCTP_POLYGON_XY , LOCTP_POLYLINE_XY, LOCTP_BRANCHID_CHAINAGE or LOCTP_CONTACTID.
      character(len=*), optional, intent(in) :: loc_file !< (Optional) File name of a polyline file (when loc_spec_type==LOCTP_POLYGON_FILE).
      integer, optional, intent(in) :: nump !< (Optional) Number of points in polyline coordinate arrays xpin and ypin (when loc_spec_type==LOCTP_POLYGON_XY/LOCTP_POLYLINE_XY).
      real(kind=dp), optional, intent(in) :: xpin(:) !< (Optional) Array with x-coordinates of a polygon/line, used instead of a polygon/line file (when loc_spec_type==LOCTP_POLYGON_XY/LOCTP_POLYLINE_XY).
      real(kind=dp), optional, intent(in) :: ypin(:) !< (Optional) Array with y-coordinates of a polygon/line, used instead of a polygon/line file (when loc_spec_type==LOCTP_POLYGON_XY/LOCTP_POLYLINE_XY).
      integer, optional, intent(in) :: branchindex !< (Optional) Branch index on which flow link is searched for (when loc_spec_type==LOCTP_BRANCHID_CHAINAGE).
      real(kind=dp), optional, intent(in) :: chainage !< (Optional) Offset along specified branch (when loc_spec_type==LOCTP_BRANCHID_CHAINAGE).
      character(len=*), optional, intent(in) :: contactId !< (Optional) Unique contactId for one flow link (when loc_spec_type==LOCTP_CONTACTID) (stored as mesh contact in input grid).
      integer, optional, intent(in) :: linktype !< (Optional) Limit search to specific link types: only 1D flow links (linktype==LINK_1D), 2D (linktype==LINK_2D), or both (linktype==LINK_ALL).
      real(kind=dp), allocatable, optional, intent(inout) :: xps(:), yps(:) !< (Optional) Arrays in which the read in polyline x,y-points can be stored (only relevant when loc_spec_type==LOCTP_POLYGON_FILE/LOCTP_POLYLINE_FILE).
      integer, optional, intent(inout) :: nps !< (Optional) Number of polyline points that have been read in (only relevant when loc_spec_type==LOCTP_POLYGON_FILE/LOCTP_POLYLINE_FILE).
      integer, optional, intent(inout) :: lftopol(:) !< (Optional) Mapping array from flow links to the polyline index that intersected that flow link (only relevant when loc_spec_type==LOCTP_POLYLINE_FILE or LOCTP_POLYLINE_XY).
      integer, optional, intent(in) :: sortLinks !< (Optional) Whether or not to sort the found flow links along the polyline path. (only relevant when loc_spec_type==LOCTP_POLYGON_FILE or LOCTP_POLYGON_XY).

      !locals
      integer :: minp, L, Lstart, Lend, opts, ierr, inp

      integer :: linktype_

      if (present(linktype)) then
         linktype_ = linktype
      else
         linktype_ = LINK_ALL
      end if

      numg = 0
      if (loc_spec_type /= LOCTP_BRANCHID_CHAINAGE .and. loc_spec_type /= LOCTP_CONTACTID) then
         ! This routine uses global xpl, ypl, because of subroutine inwhichpolygon().
         call savepol()
      end if

      if (loc_spec_type == LOCTP_POLYLINE_FILE) then
         ! Single polyline only
         call oldfil(minp, loc_file)
         call read1polylin(minp, xpl, ypl, npl)
      elseif (loc_spec_type == LOCTP_POLYGON_FILE) then
         ! Multiple polygons allowed
         call oldfil(minp, loc_file)
         call reapol(minp, 0)
      else if ((loc_spec_type == LOCTP_POLYGON_XY .or. loc_spec_type == LOCTP_POLYLINE_XY) .and. present(xpin) .and. present(ypin) .and. present(nump)) then
         if (nump > 0) then
            call increasepol(nump, 0)
            xpl(1:nump) = xpin(1:nump)
            ypl(1:nump) = ypin(1:nump)
            npl = nump
         end if
      else if (loc_spec_type == LOCTP_BRANCHID_CHAINAGE .and. present(branchindex) .and. present(chainage)) then
         !
         ! Match by branchid
         !
         if (branchindex > 0) then
            ierr = findlink(branchindex, chainage, L) ! NOTE: L is here assumed to be a net link number
            if (ierr == DFM_NOERR .and. L > 0) then
               keg(1) = L
               numg = 1
            else
               numg = 0
            end if
         end if
      else if (loc_spec_type == LOCTP_CONTACTID .and. present(contactId)) then
         !
         ! Match by contactId
         !
         ierr = findlink_by_contactid(contactId, L) ! NOTE: L is here assumed to be a net link number
         if (ierr == DFM_NOERR .and. L > 0) then
            keg(1) = L
            numg = 1
         else
            numg = 0
         end if
      end if

      if (loc_spec_type == LOCTP_POLYLINE_FILE .or. loc_spec_type == LOCTP_POLYLINE_XY) then
         !
         ! Match by polyline intersection
         !
         opts = 0
         if (present(lftopol)) then
            opts = opts + 1
         end if
         if (present(sortlinks)) then
            opts = opts + 2
         end if

         numg = 0
         select case (opts)
         case (0)
            ierr = findlink(npl, xpl, ypl, keg, numg, linktype=linktype_)
         case (1)
            ierr = findlink(npl, xpl, ypl, keg, numg, lftopol=lftopol, linktype=linktype_)
         case (2)
            ierr = findlink(npl, xpl, ypl, keg, numg, sortlinks=sortlinks, linktype=linktype_)
         case (3)
            ierr = findlink(npl, xpl, ypl, keg, numg, lftopol, sortlinks, linktype=linktype_)
         end select
      else if (loc_spec_type == LOCTP_POLYGON_FILE .or. loc_spec_type == LOCTP_POLYGON_XY) then
         !
         ! Match by inside polygon check
         !

         ! select search range for flow links
         select case (linktype_)
         case (LINK_1D, LINK_1D2D_INTERNAL, LINK_1D2D_LONGITUDINAL, LINK_1D2D_STREETINLET, LINK_1D2D_ROOF)
            Lstart = 1
            Lend = lnx1D
         case (LINK_2D)
            Lstart = lnx1D + 1
            Lend = lnx
         case (LINK_ALL)
            Lstart = 1
            Lend = lnx
         end select

         inp = -1
         ierr = 0
         do L = Lstart, Lend
            if (linktype_ /= LINK_ALL .and. kcu(L) /= linktype_) then
               cycle
            end if

            !if (kc(n) > 0) then ! no kc masking for links (yet?) ! search allowed, (not allowed like closed pipes point etc)
            call inwhichpolygon(xu(L), yu(L), inp)
            !end if

            if (inp > 0) then
               numg = numg + 1
               keg(numg) = L ! Store link number
            end if
         end do
      end if

      if (ierr /= 0) then
         call setmessage(LEVEL_WARN, 'Internal error while reading '//trim(loc_file)//'. The number of found links exceeds the available positions.')
         call setmessage(-LEVEL_WARN, 'The contents of this polygon is ignored.')
         numg = 0
      end if

      if (npl > 0 .and. present(xps)) then
         if (allocated(xps)) then
            deallocate (xps)
         end if
         if (allocated(yps)) then
            deallocate (yps)
         end if
         call realloc(xps, 100000)
         call realloc(yps, 100000)
         xps = xpl ! doubles a bit with xpl for polygon file
         yps = ypl
         nps = npl
      end if

      if (loc_spec_type /= LOCTP_BRANCHID_CHAINAGE) then
         call restorepol()
      end if

   end subroutine selectelset_internal_links

   !> Find and select flow nodes contained inside polygon, or by NodeId.
   !! A mask can be used to limit which flow nodes are a candidate at all.
   !! The output array will be set to value numprov for the flow node numbers
   !! that were selected, such that the call site can know which input file
   !! is affecting which flow nodes.
   subroutine selectelset_internal_nodes(xz, yz, kc, nx, kp, numsel, &
                                       & loc_spec_type, loc_file, numcoord, xpin, ypin, branchid, chainage, nodeId)
      use m_inquire_flowgeom
      use m_flowgeom, only: nd
      use m_polygon
      use m_alloc
      use m_missing
      use dfm_error
      use messagehandling, only: LEVEL_WARN, mess
      use m_delpol
      use m_reapol
      use m_filez, only: oldfil

      implicit none

      real(kind=dp), intent(in) :: xz(:) !< Flow nodes center x-coordinates.
      real(kind=dp), intent(in) :: yz(:) !< Flow nodes center y-coordinates.
      integer, intent(in) :: kc(:) !< Mask for which flow nodes are allowed for selection (1/0 = yes/no).
      integer, intent(in) :: nx !< Number of flow nodes in input.
      integer, intent(out) :: kp(:) !< Output array containing the flow node numbers that were selected.
      !< Size of array is responsability of call site, and filling starts at index 1 upon each call.
      integer, intent(out) :: numsel !< Number of flow nodes that were selected (i.e., kp(1:numsel) will be filled).
      integer, intent(in) :: loc_spec_type !< Type of spatial input for selecting nodes. One of: LOCTP_POLYGON_FILE, LOCTP_POLYGON_XY or LOCTP_BRANCHID_CHAINAGE or LOCTP_NODEID.
      character(len=*), optional, intent(in) :: loc_file !< File name of a polygon file (when loc_spec_type==LOCTP_POLYGON_FILE).
      integer, optional, intent(in) :: numcoord !< Number of coordinates in input arrays (when loc_spec_type==LOCTP_POLYGON_XY).
      real(kind=dp), optional, intent(in) :: xpin(:) !< Polygon x-coordinates (when loc_spec_type==LOCTP_POLYGON_XY).
      real(kind=dp), optional, intent(in) :: ypin(:) !< Polygon y-coordinates (when loc_spec_type==LOCTP_POLYGON_XY).
      character(len=*), optional, intent(in) :: branchId !< Branch id (when loc_spec_type==LOCTP_BRANCHID_CHAINAGE).
      real(kind=dp), optional, intent(in) :: chainage !< Chainage along branch (when loc_spec_type==LOCTP_BRANCHID_CHAINAGE).
      character(len=*), optional, intent(in) :: nodeId !< Node id (network node id) (when loc_spec_type==LOCTP_NODEID).
      !
      ! locals
      integer :: minp, inp, n, nn, ierr
      !
      ! body

      numsel = 0

      select case (loc_spec_type)
      case (LOCTP_POLYGON_FILE)
         call savepol() ! save state
         call delpol() ! clear state
         ! Fill npl, xpl, ypl from file
         call oldfil(minp, loc_file)
         call reapol(minp, 0)
      case (LOCTP_POLYGON_XY)
         ! Fill npl, xpl, ypl from input arrays
         call increasepol(numcoord, 0)
         xpl(1:numcoord) = xpin(1:numcoord)
         ypl(1:numcoord) = ypin(1:numcoord)
         npl = numcoord
      case (LOCTP_BRANCHID_CHAINAGE)
         ierr = findnode(branchId, chainage, n)
         if (ierr /= DFM_NOERR) then
            errormessage = 'While selecting grid points: branchId '''//trim(branchId)//''' was not found in the network.'
            call mess(LEVEL_WARN, errormessage)
            return
         end if

         numsel = 1
         ! TODO: UNST-5013: check for nodenr <= 0 in partitioned models.
         kp(numsel) = n
      case (LOCTP_NODEID)
         ierr = findnode(nodeId, n)
         if (ierr /= DFM_NOERR) then
            errormessage = 'While selecting grid points: nodeId '''//trim(nodeId)//''' was not found in the network.'
            call mess(LEVEL_WARN, errormessage)
            return
         end if

         numsel = 1
         kp(numsel) = n
      case default
         return

      end select

      if (loc_spec_type == LOCTP_POLYGON_FILE .or. loc_spec_type == LOCTP_POLYGON_XY) then
         ! Check which points are inside polygon npl-xpl-ypl
         inp = -1
         do n = 1, nx
            if (kc(n) > 0) then ! search allowed, (not allowed like closed pipes point etc)
               if (npl == 1) then ! 1 point polygon: check whether point lies inside a grid cell
                  nn = size(nd(n)%x)
                  call pinpok(xpl(1), ypl(1), nn, nd(n)%x, nd(n)%y, inp)
               else ! real polygon, check whether grid cell lies inside polygon
                  call inwhichpolygon(xz(n), yz(n), inp)
               end if

               if (inp > 0) then
                  numsel = numsel + 1
                  kp(numsel) = n
               end if
            end if
         end do
      end if
      if (loc_spec_type == LOCTP_POLYGON_FILE) then
         call restorepol() ! restore state
      end if
   end subroutine selectelset_internal_nodes

   !
   !
   ! ==========================================================================
   !> Combine a newly computed (external forcings-)value with an existing one, based on the operand type.
   subroutine operate(a, b, operand)
      use precision
      implicit none
      real(kind=dp), intent(inout) :: a !< Current value, will be updated based on b and operand.
      real(kind=dp), intent(in) :: b !< New value, to be combined with existing value a.
      character(len=1), intent(in) :: operand !< Operand type, valid values: 'O', 'A', '+', '*', 'X', 'N'.

      ! b = factor*b + offset ! todo doorplussen

      if (operand == 'O' .or. operand == 'V') then ! Override, regardless of what was specified before
         a = b
      else if (operand == 'A') then ! Add, means: only if nothing was specified before
         if (a == dmiss_default) then
            a = b
         end if
      else if (a /= dmiss_default) then ! algebra only if not missing
         if (operand == '+') then
            a = a + b
         else if (operand == '*') then
            a = a * b
         else if (operand == 'X') then
            a = max(a, b)
         else if (operand == 'N') then
            a = min(a, b)
         end if
      end if
   end subroutine operate
   !
   !
   ! ==========================================================================
   !>
   function timespaceinitialfield(xu, yu, zu, nx, filename, filetype, method, operand, transformcoef, iprimpos, kcc) result(success) !

      use kdtree2Factory
      use m_samples
      use m_netw
      use m_flowgeom, only: ln2lne, Ln, Lnx, Wu1Duni
      use m_partitioninfo
      use unstruc_netcdf
      use fm_external_forcings_data, only: qid
      use m_ec_interpolationsettings
      use m_flowparameters
      use m_missing
      use m_sferic, only: jsferic, jasfer3D
      use m_polygon, only: NPL, xpl, ypl, zpl
      use m_ec_basic_interpolation, only: triinterp2, averaging2, TerrorInfo
      use geometry_module, only: dbpinpol
      use gridoperations
      use unstruc_model, only: getoutputdir
      use system_utils, only: FILESEP
      use m_arcinfo
      use fm_location_types, only: UNC_LOC_S, UNC_LOC_U, UNC_LOC_CN
      use m_reapol
      use m_delsam
      use m_reasam
      use m_read_samples_from_arcinfo, only: read_samples_from_arcinfo
      use m_read_samples_from_geotiff, only: read_samples_from_geotiff
      use m_filez, only: oldfil, doclose, newfil

      implicit none

      logical :: success

      integer, intent(in) :: nx
      real(kind=dp), intent(in) :: xu(nx)
      real(kind=dp), intent(in) :: yu(nx)
      real(kind=dp), intent(out) :: zu(nx)

      character(*), intent(in) :: filename ! file name for meteo data file
      integer, intent(in) :: filetype ! spw, arcinfo, uniuvp etc
      integer, intent(in) :: method ! time/space interpolation method
      ! 4 : inside polygon
      ! 5 : triangulation
      ! 6 : averaging
      ! 7 : index triangulation
      ! 8 : smoothing
      ! 9 : internal diffusion
      character(1), intent(in) :: operand ! override, add
      real(kind=dp), intent(in) :: transformcoef(:) !< Transformation coefficients
      integer, intent(in) :: iprimpos ! only needed for averaging, position of primitive variables in network
      ! 1 = u point, cellfacemid, 2 = zeta point, cell centre, 3 = netnode
      integer, intent(in), optional :: kcc(nx)

      real(kind=dp), allocatable :: zh(:)
      integer :: ierr
      integer :: minp0, inside, k, jdla, mout
      real(kind=dp), allocatable :: xx(:, :), yy(:, :)
      integer, allocatable :: nnn(:)

      real(kind=dp), allocatable :: xxx(:), yyy(:)
      integer, allocatable :: LnnL(:), Lorg(:)

      real(kind=dp) :: zz

      integer :: n6, L, Lk, n, n1, n2, i
      integer :: ierror, jakc
      integer :: jakdtree = 1

      real(kind=dp) :: rcel_store, percentileminmax_store
      integer :: iav_store, nummin_store

      character(len=5) :: sd

      type(TerrorInfo) :: errorInfo

      success = .false.
      minp0 = 0
      jakc = 0
      if (present(kcc)) then
         jakc = 1
      end if

      if (filename == 'empty') then
         do k = 1, nx
            call operate(zu(k), transformcoef(1), operand)
         end do
      end if

      allocate (zh(nx), stat=ierr)
      call aerr('zh(nx)', ierr, nx)
      zh = dmiss_default

      if (filetype /= ncflow .and. filetype /= arcinfo .and. filetype /= geotiff) then
         call oldfil(minp0, filename)
      end if

      !if (filetype == 1dfield) then
      !   call init1dField(dataFile,filename, quantity)
      !   ! return?
      !end if

      if (method == 4) then ! polyfil

         call savepol()
         call reapol(minp0, 0)

         inside = -1
         do k = 1, nx
            if (jakc == 1) then
               if (kcc(k) == 0) then
                  cycle
               end if
            end if
            call dbpinpol(xu(k), yu(k), inside, dmiss, JINS, NPL, xpl, ypl, zpl)
            if (inside == 1) then
               call operate(zu(k), transformcoef(1), operand)
               zh(k) = zu(k)
            end if
         end do
         call restorepol()

      else if (method == 5 .or. method == 6) then ! triangulation & averaging

         if (filetype == ncflow) then
            call read_flowsamples_from_netcdf(filename, qid, ierr)
         elseif (filetype == ncgrid) then
            ! TODO: support reading initial fields from NetCDF too
            msgbuf = 'timespace::timespaceinitialfield: Error while reading '''//trim(qid)// &
                     ''' from file '''//trim(filename)//'''. File type not supported for initial fields.'
            call warn_flush()
            return
         else if (filetype == arcinfo) then
            call read_samples_from_arcinfo(filename, 0, 0)
         else if (filetype == geotiff) then
            success = read_samples_from_geotiff(filename)
            if (.not. success) then
               return
            end if
         else
            call reasam(minp0, 0)
         end if

         if (method == 5) then
            if (filetype == arcinfo) then
               call bilinarc(xu, yu, zh, nx)
            else
               jdla = 1
               call triinterp2(xu, yu, zh, nx, jdla, XS, YS, ZS, NS, dmiss, jsferic, jins, jasfer3D, &
                               NPL, MXSAM, MYSAM, XPL, YPL, ZPL, transformcoef, kcc)
            end if

         else if (method == 6) then ! and this only applies to flow-link data

!         store settings
            iav_store = iav
            rcel_store = rcel
            percentileminmax_store = percentileminmax
            nummin_store = nummin

            if (transformcoef(4) /= DMISS) then
               iav = int(transformcoef(4))
            end if
            if (transformcoef(5) /= DMISS) then
               rcel = transformcoef(5)
            end if
            if (transformcoef(7) /= DMISS) then
               percentileminmax = transformcoef(7)
            end if
            if (transformcoef(8) /= DMISS) then
               nummin = int(transformcoef(8))
            end if

            if (iprimpos == UNC_LOC_U) then ! primitime position = velocitypoint, cellfacemid
               n6 = 4
               allocate (xx(n6, lnx), yy(n6, lnx), nnn(lnx))
               do L = 1, lnx
                  xx(1, L) = xzw(ln(1, L))
                  yy(1, L) = yzw(ln(1, L))
                  xx(3, L) = xzw(ln(2, L))
                  yy(3, L) = yzw(ln(2, L))
                  Lk = ln2lne(L)
                  xx(2, L) = xk(kn(1, Lk))
                  yy(2, L) = yk(kn(1, Lk))
                  xx(4, L) = xk(kn(2, Lk))
                  yy(4, L) = yk(kn(2, Lk))
               end do
               nnn = 4 ! array nnn
            else if (iprimpos == UNC_LOC_S) then ! primitime position = waterlevelpoint, cell centre
               n6 = maxval(netcell%n)
               if (jsferic == 1) then
                  n6 = n6 + 2 ! safety at poles
               end if

               allocate (xx(n6, nx), yy(n6, nx), nnn(nx))

               allocate (LnnL(n6), Lorg(n6))

               do n = 1, nx
                  call get_cellpolygon(n, n6, nnn(n), rcel, xx(1, n), yy(1, n), LnnL, Lorg, zz)
               end do
               deallocate (LnnL, Lorg)
            else if (iprimpos == UNC_LOC_CN) then ! primitime position = netnode, cell corner

               n6 = 3 * maxval(nmk) ! 2: safe upper bound , 3 : even safer!
               allocate (xx(n6, numk), yy(n6, numk), nnn(numk), xxx(n6), yyy(n6))
               do k = 1, numk
                  if (jakc == 1) then
                     if (kcc(k) /= 1) then
                        cycle
                     end if
                  end if

!                 get the cell list
                  call make_dual_cell(k, n6, rcel, xxx, yyy, nnn(k), Wu1Duni)
                  do i = 1, nnn(k)
                     xx(i, k) = xxx(i)
                     yy(i, k) = yyy(i)
                  end do
               end do

               deallocate (xxx, yyy)
            end if

            if (jakdtree == 1) then
!              initialize kdtree
               call build_kdtree(treeglob, Ns, xs, ys, ierror, jsferic, dmiss)
               if (ierror /= 0) then
!                 disable kdtree
                  call delete_kdtree2(treeglob)
                  jakdtree = 0
               end if
            end if

            call averaging2(1, ns, xs, ys, zs, ipsam, xu, yu, zh, nx, xx, yy, n6, nnn, jakdtree, &
                            dmiss, jsferic, jasfer3D, JINS, NPL, xpl, ypl, zpl, errorInfo, kcc)
            deallocate (xx, yy, nnn)

            if (errorInfo%cntNoSamples > 0) then
               write (msgbuf, '(5a,i0,a)') 'For quantity ', trim(qid), ' in file ', trim(filename), ' no values found for ', errorInfo%cntNoSamples, ' cells/links.'
               call warn_flush()
            end if
            if (allocated(errorInfo%message)) then
               msgbuf = errorInfo%message
               call warn_flush()
            end if
            if (.not. errorInfo%success) then
               return
            end if

!         restore settings
            iav = iav_store
            rcel = rcel_store
            percentileminmax = percentileminmax_store
            nummin = nummin_store

            if (jakdtree == 1) then
               call delete_kdtree2(treeglob)
            end if

         end if

         do k = 1, nx
            if (zh(k) /= dmiss_default) then
               call operate(zu(k), zh(k), operand)
               zh(k) = zu(k)
            end if
         end do

!     SPvdP: sample set can be large, delete it and do not make a copy
         call delsam(-1)
         if (allocated(d)) then
            deallocate (d)
            mca = 0
            nca = 0
         end if

      end if
      success = .true.
      call doclose(minp0)

      if (jawriteDFMinterpretedvalues > 0) then
         n1 = index(trim(filename), FILESEP, .true.)

         !  fix for Linux-prepared input on Windows
         if (n1 == 0) then
            n1 = index(trim(filename), char(47), .true.)
         end if

         sd = ''
         if (jampi == 1) then
            sd = '_'//trim(sdmn)
         end if

         n2 = index(trim(filename), '.', .true.)
         if (n2 == 0) then
            n2 = len_trim(filename)
         else
            n2 = n2 - 1
         end if

         call newfil(mout, trim(getoutputdir())//'DFM_interpreted_values_'//trim(filename(n1 + 1:n2))//trim(sd)//'.xyz')

         do k = 1, nx
            if (zh(k) /= dmiss_default) then
               write (mout, *) xu(k), yu(k), zu(k)
            end if
         end do
         call doclose(mout)
      end if

      if (allocated(zh)) then
         deallocate (zh)
      end if

   end function timespaceinitialfield

   !> Bilinear interpolation for uniform rectangular.
   !! TODO: move to ec_basic_interpolation or bilin5
   subroutine bilinarc(xk, yk, zk, n)
      use m_missing
      integer, intent(in) :: n
      real(kind=dp), intent(in) :: xk(:), yk(:)
      real(kind=dp), intent(out) :: zk(:)

      integer :: k

      do k = 1, n
         if (zk(k) == dmiss) then
            call bilinarcinfo(xk(k), yk(k), zk(k))
         end if
      end do
   end subroutine bilinarc

   !> Bilinear interpolation for uniform rectangular for 1 point
   !! TODO: move to ec_basic_interpolation or bilin5
   subroutine bilinarcinfo(x, y, z)
      use m_arcinfo
      use m_missing
      real(kind=dp), intent(in) :: x, y
      real(kind=dp), intent(out) :: z

      real(kind=dp) :: dm, dn, am, an
      integer :: m, n

      dm = (x - x0) / dxa
      m = int(dm)
      am = dm - m
      m = m + 1
      dn = (y - y0) / dya
      n = int(dn)
      an = dn - n
      n = n + 1
      z = dmiss
      if (m < mca .and. n < nca .and. m >= 1 .and. n >= 1) then
         if (d(m, n) /= dmiss .and. d(m + 1, n) /= dmiss .and. d(m, n + 1) /= dmiss .and. d(m + 1, n + 1) /= dmiss) then
            z = am * an * d(m + 1, n + 1) + &
                (1.0_dp - am) * an * d(m, n + 1) + &
                (1.0_dp - am) * (1.0_dp - an) * d(m, n) + &
                am * (1.0_dp - an) * d(m + 1, n)
         end if
      end if

   end subroutine bilinarcinfo
   !
   !
   ! ==========================================================================
   !>
   function timespaceinitialfield_int(xz, yz, zz, nx, filename, filetype, operand, transformcoef) result(success) ! deze subroutine moet veralgemeniseerd en naar meteo module
      use m_missing
      use m_polygon
      use geometry_module, only: dbpinpol
      use m_reapol
      use m_filez, only: oldfil
      implicit none

      logical :: success

      integer, intent(in) :: nx
      real(kind=dp), intent(in) :: xz(nx)
      real(kind=dp), intent(in) :: yz(nx)
      integer, intent(out) :: zz(nx)
      character(*), intent(in) :: filename ! file name for meteo data file
      integer, intent(in) :: filetype ! spw, arcinfo, uniuvp etc
      character(1), intent(in) :: operand ! file name for meteo data file
      real(kind=dp), intent(in) :: transformcoef(:) !< Transformation coefficients
      integer :: minp0, inside, k

      success = .false.

      call oldfil(minp0, filename)
      if (filetype == inside_polygon) then ! polyfil

         call savepol()
         call reapol(minp0, 0)
         inside = -1
         do k = 1, nx
            call dbpinpol(xz(k), yz(k), inside, &
                          dmiss, JINS, NPL, xpl, ypl, zpl)
            if (inside == 1) then
               if (operand == '+' .and. zz(k) /= imiss) then
                  zz(k) = zz(k) + transformcoef(1)
               else
                  zz(k) = transformcoef(1)
               end if
            end if
         end do
         call restorepol()

      else if (filetype == arcinfo) then ! arcinfo bilinear todo
      else if (filetype == triangulation) then ! triangulation    todo
      end if
      success = .true.
   end function timespaceinitialfield_int

end module timespace

!> Module which constructs and connects the target Items for FM.
!! This is the wrapper between FM and the EC-module.
module m_meteo
   use m_ec_module
   use m_ec_provider
   use MessageHandling
   use m_itdate, only: itdate
   use m_flowtimes, only: tzone
   use m_wind
   use m_nudge
   use m_flow
   use m_transportdata, only: numconst, const_names, ISALT
   use m_waves
   use m_ship
   use fm_external_forcings_data
   use processes_input, only: num_time_functions, funame, funinp, nosfunext, sfunname, sfuninp
   use m_observations_data, only: xyobs
   use string_module
   use m_sediment, only: stm_included, stmpar
   use m_subsidence
   use m_fm_icecover, only: ice_area_fraction, ice_thickness

   implicit none

   type(tEcInstance), pointer, save :: ecInstancePtr !< FM's instance of the EC-module.
   character(maxMessageLen) :: message !< EC's message, to be passed to FM's log.
   !
   integer, dimension(:), allocatable, target :: item_tracerbnd !< dim(numtracers)
   integer, dimension(:), allocatable, target :: item_sedfracbnd !< dim(numfracs)
   integer, dimension(:), allocatable, target :: item_waqfun !< dim(num_time_functions)
   integer, dimension(:), allocatable, target :: item_waqsfun !< dim(nosfunext)

   integer, target :: item_windx !< Unique Item id of the ext-file's 'windx' quantity's x-component.
   integer, target :: item_windy !< Unique Item id of the ext-file's 'windy' quantity's y-component.
   integer, target :: item_windxy_x !< Unique Item id of the ext-file's 'windxy' quantity's x-component.
   integer, target :: item_windxy_y !< Unique Item id of the ext-file's 'windxy' quantity's y-component.

   integer, target :: item_stressx !< Unique Item id of the ext-file's 'stressx' quantity's x-component.
   integer, target :: item_stressy !< Unique Item id of the ext-file's 'stressy' quantity's y-component.
   integer, target :: item_stressxy_x !< Unique Item id of the ext-file's 'stressxy_x' quantity's x-component.
   integer, target :: item_stressxy_y !< Unique Item id of the ext-file's 'stressxy_y' quantity's y-component.

   integer, target :: item_frcu !< Unique Item id of the ext-file's 'frcu' quantity's component.

   integer, target :: item_apwxwy_p !< Unique Item id of the ext-file's 'airpressure_windx_windy' quantity 'p'.
   integer, target :: item_apwxwy_x !< Unique Item id of the ext-file's 'airpressure_windx_windy' quantity 'x'.
   integer, target :: item_apwxwy_y !< Unique Item id of the ext-file's 'airpressure_windx_windy' quantity 'y'.
   integer, target :: item_apwxwy_c !< Unique Item id of the ext-file's 'airpressure_windx_windy_charnock' quantity 'c' (space var Charnock).
   integer, target :: item_charnock !< Unique Item id of the ext-file's 'space var Charnock' quantity 'C'.
   integer, target :: item_waterlevelbnd !< Unique Item id of the ext-file's 'waterlevelbnd' quantity's ...-component.
   integer, target :: item_atmosphericpressure !< Unique Item id of the ext-file's 'atmosphericpressure' quantity
   integer, target :: item_pseudo_air_pressure !< Unique Item id of the ext-file's 'pseudo_air_pressure' quantity
   integer, target :: item_water_level_correction !< Unique Item id of the ext-file's 'water_level_correction' quantity
   integer, target :: item_sea_ice_area_fraction !< Unique Item id of the ext-file's 'sea_ice_area_fraction' quantity
   integer, target :: item_sea_ice_thickness !< Unique Item id of the ext-file's 'sea_ice_thickness' quantity
   integer, target :: item_velocitybnd !< Unique Item id of the ext-file's 'velocitybnd' quantity
   integer, target :: item_dischargebnd !< Unique Item id of the ext-file's 'discharge' quantity
   integer, target :: item_salinitybnd !< Unique Item id of the ext-file's 'salinitybnd' quantity
   integer, target :: item_temperaturebnd !< Unique Item id of the ext-file's 'temperaturebnd' quantity
   integer, target :: item_sedimentbnd !< Unique Item id of the ext-file's 'sedimentbnd' quantity
   integer, target :: item_tangentialvelocitybnd !< Unique Item id of the ext-file's 'tangentialvelocitybnd' quantity
   integer, target :: item_uxuyadvectionvelocitybnd !< Unique Item id of the ext-file's 'uxuyadvectionvelocitybnd'
   integer, target :: item_normalvelocitybnd !< Unique Item id of the ext-file's 'normalvelocitybnd' quantity
   integer, target :: item_rainfall !< Unique Item id of the ext-file's 'rainfall' quantity
   integer, target :: item_rainfall_rate !< Unique Item id of the ext-file's 'rainfall_rate' quantity
   integer, target :: item_air_density !< Unique Item id of the ext-file's 'airdensity' quantity
   integer, target :: item_qhbnd !< Unique Item id of the ext-file's 'qhbnd' quantity
   integer, target :: item_shiptxy !< Unique Item id of the ext-file's 'shiptxy' quantity
   integer, target :: item_movingstationtxy !< Unique Item id of the ext-file's 'movingstationtxy' quantity
   integer, target :: item_pump !< Unique Item id of the ext-file's 'pump' quantityxy' quantity
   integer, target :: item_pump_capacity !< Unique Item id of the structure file's 'pump capacity' quantity
   integer, target :: item_culvert_valveOpeningHeight !< Unique Item id of the structure file's 'culvert valveOpeningHeight' quantity
   integer, target :: item_weir_crestLevel !< Unique Item id of the structure file's 'weir crestLevel' quantity
   integer, target :: item_orifice_crestLevel !< Unique Item id of the structure file's 'orifice crestLevel' quantity
   integer, target :: item_orifice_gateLowerEdgeLevel !< Unique Item id of the structure file's 'orifice gateLowerEdgeLevel' quantity
   integer, target :: item_gate_crestLevel !< Unique Item id of the structure file's 'gate crestLevel' quantity
   integer, target :: item_gate_gateLowerEdgeLevel !< Unique Item id of the structure file's 'gate gateLowerEdgeLevel' quantity
   integer, target :: item_gate_gateOpeningWidth !< Unique Item id of the structure file's 'gate gateOpeningWidth' quantity
   integer, target :: item_general_structure_crestLevel !< Unique Item id of the structure file's 'general structure crestLevel' quantity
   integer, target :: item_general_structure_gateLowerEdgeLevel !< Unique Item id of the structure file's 'general structure gateLowerEdgeLevel' quantity
   integer, target :: item_general_structure_crestWidth !< Unique Item id of the structure file's 'general structure crestWidth' quantity
   integer, target :: item_general_structure_gateOpeningWidth !< Unique Item id of the structure file's 'general structure gateOpeningWidth' quantity
   integer, target :: item_longculvert_valve_relative_opening !< Unique Item id of the structure file's 'longculvert valveRelativeOpening' quantity

   integer, target :: item_frcutim !< Unique Item id of the friction file's 'friction_coefficient_*' quantity
   integer, target :: item_valve1D !< Unique Item id of the ext-file's 'valve1D' quantxy' quantity
   integer, target :: item_damlevel !< Unique Item id of the ext-file's 'damlevel' quantity
   integer, target :: item_gateloweredgelevel !< Unique Item id of the ext-file's 'gateloweredgelevel' quantity
   integer, target :: item_generalstructure !< Unique Item id of the ext-file's 'generalstructure' quantity
   integer, target :: item_lateraldischarge !< Unique Item id of the ext-file's 'generalstructure' quantity

   integer, target :: item_dacs_dew_point_temperature !< Unique Item id of the ext-file's 'dewpoint' quantity
   integer, target :: item_dacs_air_temperature !< Unique Item id of the ext-file's 'airtemperature' quantity
   integer, target :: item_dacs_cloudiness !< Unique Item id of the ext-file's 'cloudiness' quantity
   integer, target :: item_dacs_solar_radiation !< Unique Item id of the ext-file's 'solarradiation' quantity

   integer, target :: item_dac_dew_point_temperature !< Unique Item id of the ext-file's 'dewpoint' quantity
   integer, target :: item_dac_air_temperature !< Unique Item id of the ext-file's 'airtemperature' quantity
   integer, target :: item_dac_cloudiness !< Unique Item id of the ext-file's 'cloudiness' quantity

   integer, target :: item_hacs_relative_humidity !< Unique Item id of the ext-file's 'humidity' quantity
   integer, target :: item_hacs_air_temperature !< Unique Item id of the ext-file's 'airtemperature' quantity
   integer, target :: item_hacs_cloudiness !< Unique Item id of the ext-file's 'cloudiness' quantity
   integer, target :: item_hacs_solar_radiation !< Unique Item id of the ext-file's 'solarradiation' quantity

   integer, target :: item_hac_humidity !< Unique Item id of the ext-file's 'humidity' quantity
   integer, target :: item_hac_air_temperature !< Unique Item id of the ext-file's 'airtemperature' quantity
   integer, target :: item_hac_cloudiness !< Unique Item id of the ext-file's 'cloudiness' quantity

   integer, target :: item_dew_point_temperature !< 'dewpoint' quantity
   integer, target :: item_relative_humidity !< 'humidity' quantity
   integer, target :: item_air_temperature !< 'airtemperature' quantity
   integer, target :: item_cloudiness !< 'cloudiness' quantity
   integer, target :: item_solar_radiation !< 'solarradiation' quantity
   integer, target :: item_long_wave_radiation !< 'longwaveradiation' quantity

   integer, target :: item_discharge_salinity_temperature_sorsin !< Unique Item id of the ext-file's 'discharge_salinity_temperature_sorsin' quantity
   integer, target :: item_sourcesink_discharge !< Unique Item id of the new ext-file's '[SourceSink] discharge' quantity
   integer, allocatable, dimension(:), target :: item_sourcesink_constituent_delta !< Unique Item id of the new ext-file's '[SourceSink] salinityDelta/temperatureDelta/<other constituents>Delta' quantity

   integer, target :: item_hrms !< Unique Item id of the ext-file's 'item_hrms' quantity
   integer, target :: item_tp !< Unique Item id of the ext-file's 'item_tp' quantity
   integer, target :: item_dir !< Unique Item id of the ext-file's 'item_dir' quantity
   integer, target :: item_fx !< Unique Item id of the ext-file's 'item_fx' quantity
   integer, target :: item_fy !< Unique Item id of the ext-file's 'item_fy' quantity
   integer, target :: item_wsbu !< Unique Item id of the ext-file's 'item_wsbu' quantity
   integer, target :: item_wsbv !< Unique Item id of the ext-file's 'item_wsbv' quantity
   integer, target :: item_mx !< Unique Item id of the ext-file's 'item_mx' quantity
   integer, target :: item_my !< Unique Item id of the ext-file's 'item_my' quantity
   integer, target :: item_dissurf !< Unique Item id of the ext-file's 'item_dissurf' quantity
   integer, target :: item_diswcap !< Unique Item id of the ext-file's 'item_diswcap' quantity
   integer, target :: item_distot !< Unique Item id of the ext-file's 'item_distot'  quantity
   integer, target :: item_ubot !< Unique Item id of the ext-file's 'item_ubot' quantity

   integer, target :: item_nudge_temperature !< 3D temperature for nudging
   integer, target :: item_nudge_salinity !< 3D salinity for nudging

   integer, target :: item_subsiduplift
   integer, target :: item_ice_cover !< Unique Item id of the ext-file's 'airpressure_windx_windy' quantity 'p'.

   integer, allocatable, dimension(:) :: countbndpoints(:)
   !
   integer :: n_qhbnd !< Number of already connected qh-boundaries.

   interface ec_gettimespacevalue
      module procedure ec_gettimespacevalue_by_itemID
      module procedure ec_gettimespacevalue_by_name
   end interface ec_gettimespacevalue

   interface ec_gettimeseries
      module procedure ec_gettimeseries_by_itemID
   end interface ec_gettimeseries

   public ec_gettimeseries

   interface
      module logical function ec_addtimespacerelation(name, x, y, mask, vectormax, filename, filetype, method, operand, &
                                                      xyen, z, pzmin, pzmax, pkbot, pktop, targetIndex, forcingfile, srcmaskfile, &
                                                      dtnodal, quiet, varname, varname2, targetMaskSelect, &
                                                      tgt_data1, tgt_data2, tgt_data3, tgt_data4, &
                                                      tgt_item1, tgt_item2, tgt_item3, tgt_item4, &
                                                      multuni1, multuni2, multuni3, multuni4)
         character(len=*), intent(in) :: name !< Name for the target Quantity, possibly compounded with a tracer name.
         real(hp), dimension(:), intent(in) :: x !< Array of x-coordinates for the target ElementSet.
         real(hp), dimension(:), intent(in) :: y !< Array of y-coordinates for the target ElementSet.
         integer, intent(in) :: vectormax !< Vector max (length of data values at each element location).
         integer, dimension(:), intent(in) :: mask !< Array of masking values for the target ElementSet.
         character(len=*), intent(in) :: filename !< File name of meteo data file.
         integer, intent(in) :: filetype !< FM's filetype enumeration.
         integer, intent(in) :: method !< FM's method enumeration.
         character(len=1), intent(in) :: operand !< FM's operand enumeration.
         real(hp), optional, intent(in) :: xyen(:, :) !< FM's distance tolerance / cellsize of ElementSet.
         real(hp), dimension(:), optional, intent(in), target :: z !< FM's array of z/sigma coordinates
         real(hp), dimension(:), optional, pointer :: pzmin !< FM's array of minimal z coordinate
         real(hp), dimension(:), optional, pointer :: pzmax !< FM's array of maximum z coordinate
         integer, dimension(:), intent(in), optional, pointer :: pkbot
         integer, dimension(:), intent(in), optional, pointer :: pktop
         integer, optional, intent(in) :: targetIndex !< target position or rank of (complete!) vector in target array
         character(len=*), optional, intent(in) :: forcingfile !< file containing the forcing data for pli-file 'filename'
         character(len=*), optional, intent(in) :: srcmaskfile !< file containing mask applicable to the arcinfo source data
         real(hp), optional, intent(in) :: dtnodal !< update interval for nodal factors
         logical, optional, intent(in) :: quiet !< When .true., in case of errors, do not write the errors to screen/dia at the end of the routine.
         character(len=*), optional, intent(in) :: varname !< variable name within filename
         character(len=*), optional, intent(in) :: varname2 !< variable name within filename
         character(len=1), optional, intent(in) :: targetMaskSelect !< 'i'nside (default) or 'o'utside mask polygons
         real(hp), dimension(:), optional, pointer :: tgt_data1 !< optional pointer to the storage location for target data 1 field
         real(hp), dimension(:), optional, pointer :: tgt_data2 !< optional pointer to the storage location for target data 2 field
         real(hp), dimension(:), optional, pointer :: tgt_data3 !< optional pointer to the storage location for target data 3 field
         real(hp), dimension(:), optional, pointer :: tgt_data4 !< optional pointer to the storage location for target data 4 field
         integer, optional, intent(inout), target :: tgt_item1 !< optional target item ID 1
         integer, optional, intent(inout), target :: tgt_item2 !< optional target item ID 2
         integer, optional, intent(inout), target :: tgt_item3 !< optional target item ID 3
         integer, optional, intent(inout), target :: tgt_item4 !< optional target item ID 4
         integer, optional, intent(inout), target :: multuni1 !< multiple uni item ID 1
         integer, optional, intent(inout), target :: multuni2 !< multiple uni item ID 2
         integer, optional, intent(inout), target :: multuni3 !< item ID 3
         integer, optional, intent(inout), target :: multuni4 !< item ID 4
      end function ec_addtimespacerelation
   end interface

contains

   !> Initialize the module variables.
   subroutine init_variables()
      ecInstancePtr => null()
      message = ' '
      !
      item_windx = ec_undef_int
      item_windy = ec_undef_int
      item_windxy_x = ec_undef_int
      item_windxy_y = ec_undef_int

      item_stressx = ec_undef_int
      item_stressy = ec_undef_int
      item_stressxy_x = ec_undef_int
      item_stressxy_y = ec_undef_int

      item_frcu = ec_undef_int

      item_apwxwy_p = ec_undef_int
      item_apwxwy_x = ec_undef_int
      item_apwxwy_y = ec_undef_int
      item_apwxwy_c = ec_undef_int
      item_charnock = ec_undef_int
      item_waterlevelbnd = ec_undef_int
      item_atmosphericpressure = ec_undef_int
      item_pseudo_air_pressure = ec_undef_int
      item_water_level_correction = ec_undef_int
      item_sea_ice_area_fraction = ec_undef_int
      item_sea_ice_thickness = ec_undef_int
      item_velocitybnd = ec_undef_int
      item_dischargebnd = ec_undef_int
      item_salinitybnd = ec_undef_int
      item_temperaturebnd = ec_undef_int
      item_sedimentbnd = ec_undef_int
      item_tangentialvelocitybnd = ec_undef_int
      item_uxuyadvectionvelocitybnd = ec_undef_int
      item_normalvelocitybnd = ec_undef_int
      item_rainfall = ec_undef_int
      item_rainfall_rate = ec_undef_int
      item_air_density = ec_undef_int
      item_qhbnd = ec_undef_int
      item_shiptxy = ec_undef_int
      item_movingstationtxy = ec_undef_int
      item_pump = ec_undef_int
      item_pump_capacity = ec_undef_int
      item_culvert_valveOpeningHeight = ec_undef_int
      item_weir_crestLevel = ec_undef_int
      item_orifice_crestLevel = ec_undef_int
      item_orifice_gateLowerEdgeLevel = ec_undef_int
      item_gate_crestLevel = ec_undef_int
      item_gate_gateLowerEdgeLevel = ec_undef_int
      item_gate_gateOpeningWidth = ec_undef_int
      item_general_structure_crestLevel = ec_undef_int
      item_general_structure_gateLowerEdgeLevel = ec_undef_int
      item_general_structure_crestWidth = ec_undef_int
      item_general_structure_gateOpeningWidth = ec_undef_int
      item_longculvert_valve_relative_opening = ec_undef_int
      item_frcutim = ec_undef_int
      item_valve1D = ec_undef_int
      item_lateraldischarge = ec_undef_int
      item_damlevel = ec_undef_int
      item_gateloweredgelevel = ec_undef_int
      item_generalstructure = ec_undef_int
      item_dacs_dew_point_temperature = ec_undef_int
      item_dacs_air_temperature = ec_undef_int
      item_dac_cloudiness = ec_undef_int
      item_dac_dew_point_temperature = ec_undef_int
      item_dac_air_temperature = ec_undef_int
      item_dac_cloudiness = ec_undef_int
      item_dacs_solar_radiation = ec_undef_int
      item_hacs_relative_humidity = ec_undef_int
      item_hacs_air_temperature = ec_undef_int
      item_hacs_cloudiness = ec_undef_int
      item_hacs_solar_radiation = ec_undef_int
      item_dew_point_temperature = ec_undef_int
      item_relative_humidity = ec_undef_int
      item_air_temperature = ec_undef_int
      item_cloudiness = ec_undef_int
      item_solar_radiation = ec_undef_int
      item_long_wave_radiation = ec_undef_int
      item_hac_humidity = ec_undef_int
      item_hac_air_temperature = ec_undef_int
      item_hac_cloudiness = ec_undef_int
      item_nudge_temperature = ec_undef_int
      item_nudge_salinity = ec_undef_int
      item_discharge_salinity_temperature_sorsin = ec_undef_int
      item_sourcesink_discharge = ec_undef_int
      item_hrms = ec_undef_int
      item_tp = ec_undef_int
      item_dir = ec_undef_int
      item_fx = ec_undef_int
      item_fy = ec_undef_int
      item_wsbu = ec_undef_int
      item_wsbv = ec_undef_int
      item_mx = ec_undef_int
      item_my = ec_undef_int
      item_dissurf = ec_undef_int
      item_diswcap = ec_undef_int
      item_distot = ec_undef_int
      item_ubot = ec_undef_int
      item_subsiduplift = ec_undef_int
      !
      n_qhbnd = 0
      !
      ! tracers
      if (allocated(item_tracerbnd)) then
         deallocate (item_tracerbnd)
      end if
      allocate (item_tracerbnd(numtracers))
      item_tracerbnd = ec_undef_int
      !
      if (allocated(item_sedfracbnd)) then
         deallocate (item_sedfracbnd)
      end if
      allocate (item_sedfracbnd(numfracs))
      item_sedfracbnd = ec_undef_int

      if (allocated(item_waqfun)) then
         deallocate (item_waqfun)
      end if
      allocate (item_waqfun(num_time_functions))
      item_waqfun = ec_undef_int

      if (allocated(item_waqsfun)) then
         deallocate (item_waqsfun)
      end if
      allocate (item_waqsfun(nosfunext))
      item_waqsfun = ec_undef_int

      if (allocated(item_sourcesink_constituent_delta)) then
         deallocate (item_sourcesink_constituent_delta)
      end if
      allocate (item_sourcesink_constituent_delta(numconst))
      item_sourcesink_constituent_delta = ec_undef_int

   end subroutine init_variables

   ! ==========================================================================

   !> Translate FM's meteo1 'filetype' enum to EC's 'provFile' enum.
   subroutine filetype_fm_to_ec(filetype, ec_filetype)
      use timespace_parameters
      implicit none
      integer, intent(in) :: filetype
      integer, intent(out) :: ec_filetype
      !
      select case (filetype)
      case (UNIFORM) ! 1
         ec_filetype = provFile_uniform
      case (UNIMAGDIR) ! 2
         ec_filetype = provFile_unimagdir
      case (SVWP) ! 3
         ec_filetype = provFile_svwp
      case (ARCINFO) ! 4
         ec_filetype = provFile_arcinfo
      case (SPIDERWEB) ! 5
         ec_filetype = provFile_spiderweb
      case (CURVI) ! 6
         ec_filetype = provFile_curvi
      case (TRIANGULATION) ! 7
         ec_filetype = provFile_samples
      case (TRIANGULATIONMAGDIR) ! 8
         ec_filetype = provFile_triangulationmagdir
      case (POLY_TIM) ! 9
         ec_filetype = provFile_poly_tim
      case (NCGRID, NCWAVE) ! 11, 14
         ec_filetype = provFile_netcdf
      case (NCFLOW) ! 12
         ec_filetype = provFile_undefined ! only used for timespaceinitialfield, no EC yet.
      case (BCASCII) ! 17
         ec_filetype = provFile_bc
      case (NODE_ID) ! 20
         ec_filetype = provFile_bc
      case (FOURIER) ! 101
         ec_filetype = provFile_fourier
      case default
         ec_filetype = provFile_undefined
      end select
   end subroutine filetype_fm_to_ec

   ! ==========================================================================

   !> Translate FM's meteo1 'method' enum to EC's 'interpolate' enum.
   subroutine method_fm_to_ec(method, ec_method)
      integer, intent(in) :: method
      integer, intent(out) :: ec_method

      integer :: interpMethod, exterpMethod

      interpMethod = mod(method, 100)
      exterpMethod = method / 100
      !
      select case (interpMethod)
      case (0)
         ec_method = interpolate_passthrough
      case (1)
         ec_method = interpolate_timespace
      case (2)
         ec_method = interpolate_spacetime
      case (3)
         if (exterpMethod == 0) then
            ec_method = interpolate_spacetimeSaveWeightFactors
         else
            ec_method = extrapolate_spacetimeSaveWeightFactors
         end if
      case (4) ! TODO: EB: FM's 4 is inside_polygon method, does EC handle this correctly if FM filetype=10?
         ec_method = interpolate_space ! only spatial, inside polygon

         ! TODO: EB: FM does note have an interpolate_time equivalent in its method, only via filetype=uniform
         !case (5)
         !   ec_method = interpolate_time

      case (5)
         ec_method = interpolate_triangle ! only spatial, triangulation
      case (6)
         ec_method = interpolate_unknown ! Not yet supported: only spatial, averaging
         !case (7) ! TODO: EB+AvD: index triangulation (for spatial sedmor fields) may be needed later,
         ! but now overlaps with interpolate_time_extrapolation_ok (for wave coupling) below.
         !   ec_method = interpolate_unknown   ! Not yet supported: only spatial, index triangulation
      case (8)
         ec_method = interpolate_unknown ! Not yet supported: only spatial, smoothing
      case (9)
         ec_method = interpolate_unknown ! Not yet supported: only spatial, internal diffusion
      case (10)
         ec_method = interpolate_unknown ! Not yet supported: only initial vertical profiles
      case (7) ! TODO: EB: FM method 7, where does this come from? ! see hrms method 7
         ec_method = interpolate_time_extrapolation_ok
      case default
         ec_method = interpolate_unknown
      end select
   end subroutine method_fm_to_ec

   ! ==========================================================================

   !> Translate FM's meteo1 'operand' enum to EC's 'operand' enum.
   subroutine operand_fm_to_ec(operand, ec_operand)
      character, intent(in) :: operand
      integer, intent(out) :: ec_operand
      !
      select case (operand)
      case ('O')
         ec_operand = operand_replace
      case ('V')
         ec_operand = operand_replace_if_value
      case ('+')
         ec_operand = operand_add
      case default
         ec_operand = operand_undefined
      end select
   end subroutine operand_fm_to_ec

   !> Convert quantity names as given in user input (ini/ext file)
   !! to a consistent internal representation.
   pure function quantity_name_config_file_to_internal_name(quantity_input_name) result (quantity_internal_name)
      character(len=*), intent(in) :: quantity_input_name !< given by the user in ini/ext file
      character(len=:), allocatable :: quantity_internal_name !< consistent internal name
      
      ! it's not safe to assume that the internal representation is always lower case
      quantity_internal_name = trim(quantity_input_name)
      select case(str_tolower(quantity_internal_name))
      case ('seaiceareafraction')
         quantity_internal_name = 'sea_ice_area_fraction'
      case ('seaicethickness')
         quantity_internal_name = 'sea_ice_thickness'
      case ('bedrocksurfaceelevation')
         quantity_internal_name = 'bedrock_surface_elevation'
      case default
         ! keep other names unchanged
      end select
         
   end function quantity_name_config_file_to_internal_name

   !> Convert quantity names as given in user input (ext file)
   !! to accepted Unstruc names (as used in Fortran code)
   !! Note: for old-style ext quantities, fm_name==input_name, e.g. waterlevelbnd.
   !subroutine bndname_to_fm(input_name, fm_name)
   !   character(len=*), intent(in)  :: input_name !< given by the user
   !   character(len=*), intent(out) :: fm_name    !< known within FM
   !
   !   character(len=256) :: tempname
   !
   !   fm_name  = input_name
   !   tempname = input_name
   !   call str_upper(tempname)
   !   call remove_substr(tempname,'_')
   !   call remove_substr(tempname,'-')
   !   call remove_substr(tempname,' ')
   !
   !   select case (trim(tempname))
   !   case ('WATERLEVEL','VELOCITY','SALINITY','TEMPERATURE','SEDIMENT','TANGENTIALVELOCITY','NORMALVELOCITY','QH','TRACER')
   !      ! These are new-ext-style quantities: FM needs additional 'bnd' behind quantityid
   !      fm_name = trim(tempname)//'bnd'
   !      call str_lower(fm_name)
   !   end select
   !end subroutine bndname_to_fm

   ! ==========================================================================

   !> Translate EC's ext.force-file's item name to the integer EC item handle and to
   !> the data pointer(s), i.e. the array that will contain the values of the target item
   function fm_ext_force_name_to_ec_item(trname, sfname, waqinput, constituent_name, qidname, &
                                         itemPtr1, itemPtr2, itemPtr3, itemPtr4, &
                                         dataPtr1, dataPtr2, dataPtr3, dataPtr4) result(success)
      use m_find_name, only: find_name
      use string_module, only: str_tolower

      logical :: success
      character(len=*), intent(in) :: trname !< Tracer name (if applicatable)
      character(len=*), intent(in) :: sfname !< Sediment fraction name (if applicatable)
      character(len=*), intent(in) :: waqinput !< Water quality input name (if applicatable)
      character(len=*), intent(in) :: constituent_name !< Constituent name (if applicatable)

      character(len=*), intent(in) :: qidname !< Quantity ID (the base quantity if combined with a tracer/sedfrac/constituent name)

      integer, pointer :: itemPtr1, itemPtr2, itemPtr3, itemPtr4
      real(kind=dp), dimension(:), pointer :: dataPtr1, dataPtr2, dataPtr3, dataPtr4

      ! for tracers, sediment fractions, water quality functions and constituents:
      integer :: itrac, isf, ifun, isfun, iconst

      success = .true.

      itemPtr1 => null()
      itemPtr2 => null()
      itemPtr3 => null()
      itemPtr4 => null()
      dataPtr1 => null()
      dataPtr2 => null()
      dataPtr3 => null()
      dataPtr4 => null()
      select case (str_tolower(trim(qidname)))
      case ('windx')
         itemPtr1 => item_windx
         dataPtr1 => wx
      case ('windy')
         itemPtr1 => item_windy
         dataPtr1 => wy
      case ('windxy')
         itemPtr1 => item_windxy_x
         dataPtr1 => wx
         itemPtr2 => item_windxy_y
         dataPtr2 => wy
      case ('sea_ice_area_fraction')
         itemPtr1 => item_sea_ice_area_fraction
         dataPtr1 => ice_area_fraction ! here we require fp == dp
      case ('sea_ice_thickness')
         itemPtr1 => item_sea_ice_thickness
         dataPtr1 => ice_thickness ! here we require fp == dp
      case ('stressx')
         itemPtr1 => item_stressx
         dataPtr1 => wdsu_x
      case ('stressy')
         itemPtr1 => item_stressy
         dataPtr1 => wdsu_y
      case ('stressxy')
         itemPtr1 => item_stressxy_x
         dataPtr1 => wdsu_x
         itemPtr2 => item_stressxy_y
         dataPtr2 => wdsu_y
      case ('friction_coefficient_time_dependent', 'frictioncoefficient')
         itemPtr1 => item_frcu
         dataPtr1 => frcu
      case ('airpressure_windx_windy', 'airpressure_stressx_stressy')
         itemPtr1 => item_apwxwy_p
         dataPtr1 => air_pressure
         itemPtr2 => item_apwxwy_x
         dataPtr2 => ec_pwxwy_x
         itemPtr3 => item_apwxwy_y
         dataPtr3 => ec_pwxwy_y
      case ('airpressure_windx_windy_charnock')
         itemPtr1 => item_apwxwy_p
         dataPtr1 => air_pressure
         itemPtr2 => item_apwxwy_x
         dataPtr2 => ec_pwxwy_x
         itemPtr3 => item_apwxwy_y
         dataPtr3 => ec_pwxwy_y
         itemPtr4 => item_apwxwy_c
         dataPtr4 => ec_pwxwy_c
      case ('charnock')
         itemPtr1 => item_charnock
         dataPtr1 => ec_charnock
      case ('waterlevelbnd', 'neumannbnd', 'riemannbnd', 'outflowbnd')
         itemPtr1 => item_waterlevelbnd
         dataPtr1 => zbndz
      case ('velocitybnd', 'criticaloutflowbnd', 'weiroutflowbnd', 'absgenbnd')
         itemPtr1 => item_velocitybnd
         dataPtr1 => zbndu
      case ('dischargebnd')
         itemPtr1 => item_dischargebnd
         dataPtr1 => zbndq
      case ('salinitybnd')
         itemPtr1 => item_salinitybnd
         dataPtr1 => zbnds
      case ('temperaturebnd')
         itemPtr1 => item_temperaturebnd
         dataPtr1 => zbndTM
      case ('sedimentbnd')
         itemPtr1 => item_sedimentbnd
         dataPtr1 => zbndsd
      case ('tangentialvelocitybnd')
         itemPtr1 => item_tangentialvelocitybnd
         dataPtr1 => zbndt
      case ('uxuyadvectionvelocitybnd')
         itemPtr1 => item_uxuyadvectionvelocitybnd
         dataPtr1 => zbnduxy
      case ('normalvelocitybnd')
         itemPtr1 => item_normalvelocitybnd
         dataPtr1 => zbndn
      case ('airpressure', 'atmosphericpressure')
         itemPtr1 => item_atmosphericpressure
         dataPtr1 => air_pressure
      case ('pseudoairpressure')
         itemPtr1 => item_pseudo_air_pressure
         dataPtr1 => pseudo_air_pressure
      case ('waterlevelcorrection')
         itemPtr1 => item_water_level_correction
         dataPtr1 => water_level_correction
      case ('rainfall')
         itemPtr1 => item_rainfall
         dataPtr1 => rain
      case ('rainfall_rate')
         itemPtr1 => item_rainfall_rate
         dataPtr1 => rain
      case ('airdensity')
         itemPtr1 => item_air_density
         dataPtr1 => air_density
      case ('qhbnd')
         itemPtr1 => item_qhbnd
         dataPtr1 => qhbndz
      case ('shiptxy')
         itemPtr1 => item_shiptxy
         dataPtr1 => xyship
      case ('movingstationtxy')
         itemPtr1 => item_movingstationtxy
         dataPtr1 => xyobs
      case ('pump')
         itemPtr1 => item_pump
         !dataPtr1      => qpump

         ! Hydraulic structure parameters from flow1d: need explicit items here:
      case ('pump_capacity') ! flow1d pump
         itemPtr1 => item_pump_capacity
         dataPtr1 => qpump ! TODO: UNST-2724: needs more thinking, see issue comments.
      case ('culvert_valveopeningheight') ! flow1d culvert
         itemPtr1 => item_culvert_valveOpeningHeight
         !dataPtr1  => null() ! flow1d structure has its own data structure
      case ('weir_crestlevel') ! flow1d weir
         itemPtr1 => item_weir_crestLevel
         !dataPtr1  => null() ! flow1d structure has its own data structure
      case ('orifice_crestlevel') ! flow1d orifice
         itemPtr1 => item_orifice_crestLevel
         !dataPtr1  => null() ! flow1d structure has its own data structure
      case ('orifice_gateloweredgelevel') ! flow1d orifice
         itemPtr1 => item_orifice_gateLowerEdgeLevel
         !dataPtr1  => null() ! flow1d structure has its own data structure
      case ('gate_crestlevel') ! flow1d gate
         itemPtr1 => item_gate_crestLevel
         !dataPtr1  => null() ! flow1d structure has its own data structure
      case ('gate_gateloweredgelevel') ! flow1d gate
         itemPtr1 => item_gate_gateLowerEdgeLevel
         !dataPtr1  => null() ! flow1d structure has its own data structure
      case ('gate_gateopeningwidth') ! flow1d gate
         itemPtr1 => item_gate_gateOpeningWidth
         !dataPtr1  => null() ! flow1d structure has its own data structure
      case ('general_structure_crestlevel') ! flow1d general structure
         itemPtr1 => item_general_structure_crestLevel
         !dataPtr1  => null() ! flow1d structure has its own data structure
      case ('general_structure_gateloweredgelevel') ! flow1d general structure
         itemPtr1 => item_general_structure_gateLowerEdgeLevel
         !dataPtr1  => null() ! flow1d structure has its own data structure
      case ('general_structure_crestwidth') ! flow1d general structure
         itemPtr1 => item_general_structure_crestWidth
         !dataPtr1  => null() ! flow1d structure has its own data structure
      case ('general_structure_gateopeningwidth') ! flow1d general structure
         itemPtr1 => item_general_structure_gateOpeningWidth
         !dataPtr1  => null() ! flow1d structure has its own data structure
      case ('longculvert_valverelativeopening')
         itemPtr1 => item_longculvert_valve_relative_opening
      case ('valve1d')
         itemPtr1 => item_valve1D
      case ('damlevel')
         itemPtr1 => item_damlevel
      case ('dambreaklevelsandwidths')
         ! itemPtr1 and dataPtr1 are provided at a dambreak call
      case ('lateral_discharge')
         itemPtr1 => item_lateraldischarge
         !dataPtr1 => qplat ! Don't set this here, done in adduniformtimerelation_objects().
      case ('gateloweredgelevel')
         itemPtr1 => item_gateloweredgelevel
         dataPtr1 => zgate
      case ('generalstructure')
         itemPtr1 => item_generalstructure
         dataPtr1 => zcgen
      case ('humidity_airtemperature_cloudiness')
         itemPtr1 => item_hac_humidity
         dataPtr1 => relative_humidity
         itemPtr2 => item_hac_air_temperature
         dataPtr2 => air_temperature
         itemPtr3 => item_hac_cloudiness
         dataPtr3 => cloudiness
      case ('humidity_airtemperature_cloudiness_solarradiation')
         itemPtr1 => item_hacs_relative_humidity
         dataPtr1 => relative_humidity
         itemPtr2 => item_hacs_air_temperature
         dataPtr2 => air_temperature
         itemPtr3 => item_hacs_cloudiness
         dataPtr3 => cloudiness
         itemPtr4 => item_hacs_solar_radiation
         dataPtr4 => solar_radiation
      case ('dewpoint_airtemperature_cloudiness')
         itemPtr1 => item_dac_dew_point_temperature
         dataPtr1 => dew_point_temperature
         itemPtr2 => item_dac_air_temperature
         dataPtr2 => air_temperature
         itemPtr3 => item_dac_cloudiness
         dataPtr3 => cloudiness
      case ('dewpoint_airtemperature_cloudiness_solarradiation')
         itemPtr1 => item_dacs_dew_point_temperature
         dataPtr1 => dew_point_temperature
         itemPtr2 => item_dacs_air_temperature
         dataPtr2 => air_temperature
         itemPtr3 => item_dacs_cloudiness
         dataPtr3 => cloudiness
         itemPtr4 => item_dacs_solar_radiation
         dataPtr4 => solar_radiation
      case ('humidity')
         itemPtr1 => item_relative_humidity
         dataPtr1 => relative_humidity
      case ('dewpoint')
         itemPtr1 => item_dew_point_temperature
         dataPtr1 => dew_point_temperature
      case ('airtemperature')
         itemPtr1 => item_air_temperature
         dataPtr1 => air_temperature
      case ('cloudiness')
         itemPtr1 => item_cloudiness
         dataPtr1 => cloudiness
      case ('solarradiation', 'netsolarradiation')
         itemPtr1 => item_solar_radiation
         dataPtr1 => solar_radiation
      case ('longwaveradiation')
         itemPtr1 => item_long_wave_radiation
         dataPtr1 => long_wave_radiation
      case ('nudge_salinity_temperature', 'nudgesalinitytemperature')
         itemPtr2 => item_nudge_salinity
         dataPtr2 => nudge_salinity
         itemPtr1 => item_nudge_temperature
         dataPtr1 => nudge_temperature
      case ('discharge_salinity_temperature_sorsin')
         itemPtr1 => item_discharge_salinity_temperature_sorsin
         ! Do not point to array qstss here.
         ! qstss might be reallocated after initialization (when coupled to Cosumo)
         ! and must be an argument when calling ec_gettimespacevalue.
         nullify (dataPtr1)
      case ('sourcesink_discharge')
         itemPtr1 => item_sourcesink_discharge
         ! Do not point to array qstss here.
         ! qstss might be reallocated after initialization (when coupled to Cosumo)
         ! and must be an argument when calling ec_gettimespacevalue.
         nullify (dataPtr1)
      case ('sourcesink_constituentdelta')
         if (strcmpi(constituent_name, 'salinity')) then
            iconst = ISALT
         else
            iconst = find_name(const_names, constituent_name)
         end if
         itemPtr1 => item_sourcesink_constituent_delta(iconst)
         ! Do not point to array qstss here.
         ! qstss might be reallocated after initialization (when coupled to Cosumo)
         ! and must be an argument when calling ec_gettimespacevalue.
         nullify (dataPtr1)
      case ('hrms', 'wavesignificantheight')
         itemPtr1 => item_hrms
         dataPtr1 => hwavcom
         jamapwav_hwav = 1
      case ('tp', 'tps', 'rtp', 'waveperiod')
         itemPtr1 => item_tp
         dataPtr1 => twavcom
         jamapwav_twav = 1
      case ('dir', 'wavedirection')
         itemPtr1 => item_dir
         dataPtr1 => phiwav
         jamapwav_phiwav = 1
         ! wave height needed as the weighting factor for direction interpolation
         itemPtr2 => item_hrms
         dataPtr2 => hwavcom
      case ('fx', 'xwaveforce')
         itemPtr1 => item_fx
         dataPtr1 => sxwav
      case ('fy', 'ywaveforce')
         itemPtr1 => item_fy
         dataPtr1 => sywav
      case ('wsbu')
         itemPtr1 => item_wsbu
         dataPtr1 => sbxwav
      case ('wsbv')
         itemPtr1 => item_wsbv
         dataPtr1 => sbywav
      case ('mx')
         itemPtr1 => item_mx
         dataPtr1 => mxwav
      case ('my')
         itemPtr1 => item_my
         dataPtr1 => mywav
      case ('dissurf', 'wavebreakerdissipation')
         itemPtr1 => item_dissurf
         dataPtr1 => dsurf
      case ('diswcap', 'whitecappingdissipation')
         itemPtr1 => item_diswcap
         dataPtr1 => dwcap
      case ('totalwaveenergydissipation')
         itemPtr1 => item_distot
         dataPtr1 => distot
      case ('ubot')
         itemPtr1 => item_ubot
         dataPtr1 => uorbwav
      case ('tracerbnd')
         ! get tracer (boundary) number
         itrac = find_name(trnames, trname)
         itemPtr1 => item_tracerbnd(itrac)
         dataPtr1 => bndtr(itrac)%z
      case ('sedfracbnd')
         ! get sediment fraction (boundary) number
         isf = find_name(sfnames, sfname)
         itemPtr1 => item_sedfracbnd(isf)
         dataPtr1 => bndsf(isf)%z
      case ('waqfunction')
         ! get sediment fraction (boundary) number
         ifun = find_name(funame, waqinput)
         itemPtr1 => item_waqfun(ifun)
         dataPtr1 => funinp(ifun, :)
      case ('waqsegmentfunction')
         ! get sediment fraction (boundary) number
         isfun = find_name(sfunname, waqinput)
         itemPtr1 => item_waqsfun(isfun)
         dataPtr1 => sfuninp(isfun, :)
      case ('initialtracer')
         continue
      case ('friction_coefficient_chezy', 'friction_coefficient_manning', 'friction_coefficient_walllawnikuradse', &
            'friction_coefficient_whitecolebrook', 'friction_coefficient_stricklernikuradse', &
            'friction_coefficient_strickler', 'friction_coefficient_debosbijkerk')
         itemPtr1 => item_frcutim ! the same for all types (type is stored elsewhere)
      case ('bedrock_surface_elevation')
         itemPtr1 => item_subsiduplift
         dataPtr1 => subsupl
      case default
         call mess(LEVEL_FATAL, 'm_meteo::fm_ext_force_name_to_ec_item: Unsupported quantity specified in ext-file (construct target field): '//qidname)
         success = .false.
      end select
   end function fm_ext_force_name_to_ec_item

   ! ==========================================================================

   !> Construct and initialize a new Instance of the EC-module.
   subroutine initialize_ec_module()
      use m_sferic
      use unstruc_messages, only: callback_msg
      implicit none
      ! FM re-initialize call: First destroy the EC-module instance.
      if (associated(ecInstancePtr)) then
         if (.not. ecFreeInstance(ecInstancePtr)) then
            message = dumpECMessageStack(LEVEL_WARN, callback_msg)
         end if
      end if
      ! FM initialize call or second phase of re-initialize call.
      if (.not. associated(ecInstancePtr)) then
         call init_variables()
         if (.not. ecCreateInstance(ecInstancePtr)) then
            message = dumpECMessageStack(LEVEL_WARN, callback_msg)
         end if
      end if
      if (jsferic == 1) then
         ecInstancePtr%coordsystem = EC_COORDS_SFERIC
      else
         ecInstancePtr%coordsystem = EC_COORDS_CARTESIAN
      end if

   end subroutine initialize_ec_module

   ! ==========================================================================

   !> Helper function for creating and initializing a target Item.
   function createItem(instancePtr, itemId, quantityId, elementSetId, fieldId) result(success)
      logical :: success !< function status
      type(tEcInstance), pointer :: instancePtr !<
      integer, intent(inout) :: itemId !< Unique Item id.
      integer, intent(inout) :: quantityId !< Unique Quantity id.
      integer, intent(inout) :: elementSetId !< Unique ElementSet id.
      integer, intent(inout) :: fieldId !< Unique Field id.
      !
      success = .true.
      if (itemId == ec_undef_int) then ! if Target Item already exists, do NOT create a new one ...
         itemId = ecCreateItem(ecInstancePtr)
         success = ecSetItemRole(instancePtr, itemId, itemType_target)
         if (success) then
            success = ecSetItemQuantity(instancePtr, itemId, quantityId)
         end if
      end if
      ! ... but we would like to use the newest targetFIELD for this item, since old targetFIELDs can refer to the
      ! wrong data location (Arr1DPtr). This happens in the case that the demand-side arrays are reallocated while
      ! building the targets! Same is done for the elementset, so we are sure to always connect the latest
      ! elementset to this target.
      if (success) then
         success = ecSetItemElementSet(instancePtr, itemId, elementSetId)
      end if
      if (success) then
         success = ecSetItemTargetField(instancePtr, itemId, fieldId)
      end if
   end function createItem

   ! ==========================================================================

   !> Helper function for initializing a Converter.
   function initializeConverter(instancePtr, converterId, convtype, operand, method, srcmask, inputptr) result(success)
      logical :: success !< function status
      type(tEcInstance), pointer :: instancePtr !<
      integer :: converterId !< Id of the converter to be initialized
      integer :: convtype !< Type of conversion
      integer :: operand !< Operand (add/replace)
      integer :: method !< Method of interpolation
      type(tEcMask), optional :: srcmask !< Mask excluding source points
      real(kind=dp), pointer, optional :: inputptr !< pointer to an input arg for the converter (for QHBND)
      !
      success = ecSetConverterType(instancePtr, converterId, convtype)
      if (success) then
         success = ecSetConverterOperand(instancePtr, converterId, operand)
      end if
      if (success) then
         success = ecSetConverterInterpolation(instancePtr, converterId, method)
      end if
      if (present(srcmask)) then
         if (success) then
            success = ecSetConverterMask(instancePtr, converterId, srcmask)
         end if
      end if
      if (present(inputptr)) then
         if (success) then
            success = ecSetConverterInputPointer(instancePtr, converterId, inputptr)
         end if
      end if

   end function initializeConverter

   ! ==========================================================================

   !> Helper function for initializing a Connection.
   function initializeConnection(instancePtr, connectionId, sourceItemId, targetItemId) result(success)
      logical :: success !< function status
      type(tEcInstance), pointer :: instancePtr !<
      integer, intent(inout) :: connectionId !<
      integer, intent(inout) :: sourceItemId !<
      integer, intent(inout) :: targetItemId !<
      !
      success = ecAddConnectionSourceItem(instancePtr, connectionId, sourceItemId)
      if (success) then
         success = ecAddConnectionTargetItem(instancePtr, connectionId, targetItemId)
      end if
      if (success) then
         success = ecAddItemConnection(instancePtr, targetItemId, connectionId)
      end if
   end function initializeConnection

   ! ==========================================================================

   !> Helper function for Connection initialization.
   function checkFileType(actualfiletype, requiredfiletype, name) result(success)
      logical :: success !< function status
      integer, intent(in) :: actualfiletype !< EC-module's filetype enumeration.
      integer, intent(in) :: requiredfiletype !< EC-module's filetype enumeration.
      character(*), intent(in) :: name !< Name for the target Quantity.
      !
      success = .true.
      if (.not. actualfiletype == requiredfiletype) then
         message = 'm_meteo::checkFileType: Unsupported filetype for quantity '//name//'.'
         success = .false.
      end if
   end function checkFileType

   ! ==========================================================================
   function checkVectorMax(ecInstancePtr, sourceItemId, targetItemId) result(success)
      logical :: success !< function result
      type(tEcInstance), pointer :: ecInstancePtr !< the instance pointer
      integer, intent(in) :: sourceItemId !< the source item ID
      integer, intent(in) :: targetItemId !< the target item ID

      type(tEcItem), pointer :: itemPtrSrc !< Item corresponding to sourceItemId
      type(tEcItem), pointer :: itemPtrTgt !< Item corresponding to targetItemId
      integer :: vectorMaxSrc !< vectorMax for source item
      integer :: vectorMaxTgt !< vectorMax for target item
      character(len=1024) :: msg
      success = .true.
      itemPtrSrc => ecSupportFindItem(ecInstancePtr, sourceItemId)
      itemPtrTgt => ecSupportFindItem(ecInstancePtr, targetItemId)
      vectorMaxSrc = itemPtrSrc%quantityPtr%vectorMax
      vectorMaxTgt = itemPtrTgt%quantityPtr%vectorMax
      if (vectorMaxSrc /= vectorMaxTgt) then
         success = .false.
         select case (itemPtrTgt%quantityPtr%name)
         case ('discharge_salinity_temperature_sorsin')
            write (msg, '(a,i0,a,i0,a)') 'Wrong number of data columns in a discharge_salinity_temperature_sorsin time series: ', vectorMaxTgt, ' requested, ', vectorMaxSrc, ' provided.'
            msg = trim(msg)//" With source file '"//trim(itemPtrSrc%elementsetPtr%name)//"'"
            call mess(LEVEL_ERROR, trim(msg))
         case default
            call mess(LEVEL_WARN, "There was a problem with a source of type "//trim(itemPtrSrc%quantityPtr%name) &
                      //" with source file '"//trim(itemPtrSrc%elementsetPtr%name)//"'")
            call mess(LEVEL_ERROR, "Vector max differs for "//trim(itemPtrTgt%quantityPtr%name) &
                      //" values (resp. source, target): ", vectorMaxSrc, vectorMaxTgt)
         end select
      end if
   end function checkVectorMax

   ! ==========================================================================
   function ec_gettimeseries_by_itemID(instancePtr, itemId, t0, t1, dt, target_array) result(success)
      use m_flowtimes
      logical :: success !< function status
      type(tEcInstance), pointer :: instancePtr !< intent(in)
      integer, intent(in) :: itemID !< unique Item id
      real(kind=dp), intent(in) :: t0, t1, dt !< get data corresponding to this number of timesteps since FM's refdate
      real(kind=dp), dimension(:), allocatable, intent(inout) :: target_array !< kernel's data array for the requested values
      real(kind=dp), dimension(:), pointer :: arr1dPtr => null()

      real(kind=dp) :: tt
      integer :: it, nt, blksize
      tt = t0
      it = 0

      nt = ceiling((t1 - t0) / dt) + 1
      if (allocated(target_array)) then
         deallocate (target_array)
      end if
      allocate (target_array(nt * blksize))
      arr1dPtr => ecItemGetArr1DPtr(instancePtr, itemId, 2)
      blksize = size(arr1dPtr)

      call clearECMessage()
      do while (t0 + it * dt < t1)
         if (.not. ec_gettimespacevalue_by_itemID(instancePtr, itemId, irefdate, tzone, tunit, t0 + it * dt, &
                                                  target_array(it * blksize + 1:(it + 1) * blksize))) then
            return ! Message stack was already dumped by gettimespacevalue
         end if
         it = it + 1
      end do
      success = .true.
   end function ec_gettimeseries_by_itemID

   ! ==========================================================================

   !> Convenience wrapper around ec_gettimespacevalue_by_itemID.
   function ec_gettimespacevalue_by_name(instancePtr, group_name, timesteps) result(success)
      use m_flowtimes
      logical :: success !< function status
      type(tEcInstance), pointer :: instancePtr !< intent(in)
      character(len=*), intent(in) :: group_name !< unique group name
      real(kind=dp), intent(in) :: timesteps !< get data corresponding to this number of timesteps since FM's refdate
      !
      success = .false.
      !
      if (trim(group_name) == 'rainfall') then
         if (.not. ec_gettimespacevalue_by_itemID(instancePtr, item_rainfall, irefdate, tzone, tunit, timesteps)) then
            return
         end if
      end if
      if (trim(group_name) == 'rainfall_rate') then
         if (.not. ec_gettimespacevalue_by_itemID(instancePtr, item_rainfall_rate, irefdate, tzone, tunit, timesteps)) then
            return
         end if
      end if
      if (trim(group_name) == 'airdensity') then
         if (.not. ec_gettimespacevalue_by_itemID(instancePtr, item_air_density, irefdate, tzone, tunit, timesteps)) then
            return
         end if
      end if
      if (trim(group_name) == 'humidity_airtemperature_cloudiness') then
         if (.not. ec_gettimespacevalue_by_itemID(instancePtr, item_hac_humidity, irefdate, tzone, tunit, timesteps)) then
            return
         end if
      end if
      if (trim(group_name) == 'humidity_airtemperature_cloudiness_solarradiation') then
         if (.not. ec_gettimespacevalue_by_itemID(instancePtr, item_hacs_relative_humidity, irefdate, tzone, tunit, timesteps)) then
            return
         end if
      end if
      if (trim(group_name) == 'dewpoint_airtemperature_cloudiness') then
         if (.not. ec_gettimespacevalue_by_itemID(instancePtr, item_dac_dew_point_temperature, irefdate, tzone, tunit, timesteps)) then
            return
         end if
      end if
      if (trim(group_name) == 'dewpoint_airtemperature_cloudiness_solarradiation') then
         if (.not. ec_gettimespacevalue_by_itemID(instancePtr, item_dacs_dew_point_temperature, irefdate, tzone, tunit, timesteps)) then
            return
         end if
      end if
      if (trim(group_name) == 'dewpoint') then
         if (.not. ec_gettimespacevalue_by_itemID(instancePtr, item_dew_point_temperature, irefdate, tzone, tunit, timesteps)) then
            return
         end if
      end if
      if (trim(group_name) == 'airtemperature') then
         if (.not. ec_gettimespacevalue_by_itemID(instancePtr, item_air_temperature, irefdate, tzone, tunit, timesteps)) then
            return
         end if
      end if

      if ((trim(group_name) == 'dewpoint_airtemperature_cloudiness' .and. item_dac_dew_point_temperature /= ec_undef_int) &
          .or. &
          (trim(group_name) == 'dewpoint_airtemperature_cloudiness_solarradiation' .and. item_dacs_dew_point_temperature /= ec_undef_int) &
          .or. &
          (trim(group_name) == 'dewpoint' .and. item_dew_point_temperature /= ec_undef_int)) then
         relative_humidity = calculate_relative_humidity(dew_point_temperature, air_temperature)
      end if

      if (index(group_name, 'airpressure_windx_windy') == 1) then
         if (.not. ec_gettimespacevalue_by_itemID(instancePtr, item_apwxwy_p, irefdate, tzone, tunit, timesteps)) then
            return
         end if
      end if
      if (trim(group_name) == 'bedrock_surface_elevation') then
         if (.not. ec_gettimespacevalue_by_itemID(instancePtr, item_subsiduplift, irefdate, tzone, tunit, timesteps)) then
            return
         end if
      end if
      if (index(group_name, 'wavedirection') == 1) then
         if (.not. ec_gettimespacevalue_by_itemID(instancePtr, item_dir, irefdate, tzone, tunit, timesteps)) then
            return
         end if
      end if
      success = .true.
   end function ec_gettimespacevalue_by_name

   !> Computes relative humidity (%) from dew point and air temperature (degC)
   pure elemental function calculate_relative_humidity(td, tm) result(rh)
      real(kind=dp), intent(in) :: td !< dew point temperature temperature (degC)
      real(kind=dp), intent(in) :: tm !< air temperature (degC)
      real(kind=dp) :: rh !< relative humidity (%)

      real(kind=dp), parameter :: B = 17.502_dp
      real(kind=dp), parameter :: C = 240.96_dp

      ! Computation based on Tetens / Magnus formula for water vapour saturation pressure
      ! expressed using temperatures in Celsius scale.
      ! C equals 240.97 in Eq (8) of Buck (1981)
      ! Eq (7.5) of ECMWF (2023) uses temperatures in Kelvin scale:
      ! with a1 * (td - t0) / (td - a4) where a1 = 17.502, t0 = 273.16, a4 = 32.19 (= 273.15 - 240.96)
      
      rh = exp(B * td / (C + td) - B * tm / (C + tm)) * 100.0_dp
   end function calculate_relative_humidity

end module m_meteo
