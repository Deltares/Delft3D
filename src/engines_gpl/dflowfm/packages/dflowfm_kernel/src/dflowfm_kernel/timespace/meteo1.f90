!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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
      integer, intent(out) :: operand !< Operand w.r.t. previous data
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
      operand = OPERAND_OVERRIDE
      call zoekja(minp, rec, keywrd, ja)
      if (ja == 1) then
         block
            character(len=256) :: temp
            l1 = index(rec, '=') + 1
            call checkForSpacesInProvider(rec, l1, l2) ! l2 = l1 + #spaces after the equal-sign
            read (rec(l2:l2), '(a1)', err=990) temp
            operand = convert_legacy_operand_string_to_integer(temp)
         end block
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
      use m_flowparameters, only: jatidep, jaselfal, map_write_settings
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
            if (map_write_settings%tidep > 0 .and. Np > 1) then ! store SAL potential seperately
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
      use m_timespace_data_tables, only: LOAD_LOVE_NUMBERS_H, LOAD_LOVE_NUMBERS_K
      implicit none

      ! Input\Output parameter
      integer, intent(in) :: i1, i2, j1, j2, jaselfal
      real(kind=dp), intent(in) :: avhs(i1:i2, j1:j2)
      real(kind=dp), intent(out) :: self(i1:i2, j1:j2)

      ! Local parameters
      real(kind=dp), parameter :: Me = 5.9726e24_dp, R = 6371e3_dp, g = 9.81_dp, pi = 4.0_dp * atan(1.0_dp), rhow = 1.0240164e3_dp, rhoe = 3.0_dp * Me / (4.0_dp * pi * R * R * R)
      integer :: nlat, nlon, lsave
      integer :: i, j, ierror, isym, nt, l, mdab, ndab, k1
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
            a(1:mdab, l) = 3 * g * rhow * (1 + LOAD_LOVE_NUMBERS_K(l - 1) - LOAD_LOVE_NUMBERS_H(l - 1)) / rhoe / (2 * l - 1) * a(1:mdab, l)
            b(1:mdab, l) = 3 * g * rhow * (1 + LOAD_LOVE_NUMBERS_K(l - 1) - LOAD_LOVE_NUMBERS_H(l - 1)) / rhoe / (2 * l - 1) * b(1:mdab, l)
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

   !
   !
   ! ==========================================================================
   !>
   subroutine tforce(jul0, TIME, xzeta, yzeta, TIDEP, IDIM1, dstart, dstop, eps)
      use messagehandling
      use m_timespace_data_tables, only: SPHERICAL_HARMONICS_DATA
      !
      ! ====================================================================
      !
      !     Copyright © 2026, Rijkswaterstaat, All Rights Reserved.
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
         RECORD = SPHERICAL_HARMONICS_DATA(N)
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

      real(kind=dp), intent(inout) :: a !< Current value, will be updated based on b and operand.
      real(kind=dp), intent(in) :: b !< New value, to be combined with existing value a.
      integer, intent(in) :: operand !< Operand type

      if (operand == OPERAND_OVERRIDE) then ! Override, regardless of what was specified before
         a = b
      else if (operand == OPERAND_OVERRIDE_IF_MISSING) then ! Override, but only if nothing was specified before
         if (a == dmiss_default) then
            a = b
         end if
      else if (a /= dmiss_default) then ! algebra only if not missing
         if (operand == OPERAND_ADD) then
            a = a + b
         else if (operand == OPERAND_MULTIPLY) then
            a = a * b
         else if (operand == OPERAND_MAXIMUM) then
            a = max(a, b)
         else if (operand == OPERAND_MINIMUM) then
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
      integer, intent(in) :: operand
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
      use timespace_parameters, only: OPERAND_ADD
      implicit none

      logical :: success

      integer, intent(in) :: nx
      real(kind=dp), intent(in) :: xz(nx)
      real(kind=dp), intent(in) :: yz(nx)
      integer, intent(out) :: zz(nx)
      character(*), intent(in) :: filename ! file name for meteo data file
      integer, intent(in) :: filetype ! spw, arcinfo, uniuvp etc
      integer, intent(in) :: operand
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
               if (operand == OPERAND_ADD .and. zz(k) /= imiss) then
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
   character(MAXIMUM_EC_MESSAGE_LENGTH) :: message !< EC's message, to be passed to FM's log.
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
   integer, target :: item_secchi_depth !< Unique Item id of the ext-file's 'secchidepth' quantity's component.

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
   integer, target :: item_bubblescreen_discharge !< Unique Item id of the new ext-file's '[BubbleScreen] air discharge' quantity
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
         integer, intent(in) :: operand !< FM's operand enumeration.
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
      item_secchi_depth = ec_undef_int

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
      item_bubblescreen_discharge = ec_undef_int
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
      use timespace_parameters
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
      case (NEAREST_NEIGHBOUR)
         ec_method = interpolate_nearest_neighbour
      case (7) ! TODO: EB: FM method 7, where does this come from? ! see hrms method 7
         ec_method = interpolate_time_extrapolation_ok
      case default
         ec_method = interpolate_unknown
      end select
   end subroutine method_fm_to_ec

   ! ==========================================================================

   !> Translate FM's meteo1 'operand' enum to EC's 'operand' enum.
   subroutine operand_fm_to_ec(operand, ec_operand)
      use timespace_parameters, only: FM_OVERRIDE => OPERAND_OVERRIDE, FM_ADD => OPERAND_ADD
      integer, intent(in) :: operand
      integer, intent(out) :: ec_operand
      
      select case (operand)
      case (FM_OVERRIDE)
         ec_operand = operand_replace
      case (FM_ADD)
         ec_operand = operand_add
      case default
         ec_operand = operand_undefined
      end select
   end subroutine operand_fm_to_ec

   !> Convert quantity names as given in user input (ini/ext file)
   !! to a consistent internal representation.
   pure function quantity_name_config_file_to_internal_name(quantity_input_name) result(quantity_internal_name)
      character(len=*), intent(in) :: quantity_input_name !< given by the user in ini/ext file
      character(len=:), allocatable :: quantity_internal_name !< consistent internal name

      ! it's not safe to assume that the internal representation is always lower case
      quantity_internal_name = trim(quantity_input_name)
      select case (str_tolower(quantity_internal_name))
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
      case ('secchidepth')
         itemPtr1 => item_secchi_depth
         dataPtr1 => spatial_secchi_depth
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
         ! Do not point to array source_sink_all_discharges here.
         ! source_sink_all_discharges might be reallocated after initialization (when coupled to Cosumo)
         ! and must be an argument when calling ec_gettimespacevalue.
         nullify (dataPtr1)
      case ('sourcesink_discharge')
         itemPtr1 => item_sourcesink_discharge
         ! Do not point to array source_sink_all_discharges here.
         ! source_sink_all_discharges might be reallocated after initialization (when coupled to Cosumo)
         ! and must be an argument when calling ec_gettimespacevalue.
         nullify (dataPtr1)
      case ('bubblescreen_discharge')
         itemPtr1 => item_bubblescreen_discharge
         ! Do not point to an array here as it might be reallocated after initialization
         nullify (dataPtr1)
      case ('sourcesink_constituentdelta')
         if (strcmpi(constituent_name, 'salinity')) then
            iconst = ISALT
         else
            iconst = find_name(const_names, constituent_name)
         end if
         itemPtr1 => item_sourcesink_constituent_delta(iconst)
         ! Do not point to array source_sink_all_discharges here.
         ! source_sink_all_discharges might be reallocated after initialization (when coupled to Cosumo)
         ! and must be an argument when calling ec_gettimespacevalue.
         nullify (dataPtr1)
      case ('hrms', 'wavesignificantheight')
         itemPtr1 => item_hrms
         dataPtr1 => hwavcom
         map_write_settings%wav_hwav = 1
      case ('tp', 'tps', 'rtp', 'waveperiod')
         itemPtr1 => item_tp
         dataPtr1 => twavcom
         map_write_settings%wav_twav = 1
      case ('dir', 'wavedirection')
         itemPtr1 => item_dir
         dataPtr1 => phiwav
         map_write_settings%wav_phiwav = 1
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
            message = dump_ec_message_stack(LEVEL_WARN, callback_msg)
         end if
      end if
      ! FM initialize call or second phase of re-initialize call.
      if (.not. associated(ecInstancePtr)) then
         call init_variables()
         if (.not. ecCreateInstance(ecInstancePtr)) then
            message = dump_ec_message_stack(LEVEL_WARN, callback_msg)
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

      call clear_ec_message()
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
