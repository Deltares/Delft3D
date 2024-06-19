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
module m_itdate
   character(len=8) :: refdat
   integer          :: itdate      !< should be user specified for (asc routines)
   integer          :: jul0, imonth0, iday0, iyear0
   double precision :: Tzone ! doubling with "use m_flowtimes, only : tzone"
   end module m_itdate


! ==========================================================================

!> 
module timespace_read
!!--description-----------------------------------------------------------------
!
!!--pseudo code and references--------------------------------------------------
!
! Stef.Hummel@WlDelft.nl
! Herman.Kernkamp@WlDelft.nl
! Adri.Mourits@WlDelft.nl
!
!!--declarations----------------------------------------------------------------
   use precision
   implicit none

  integer,  parameter              :: maxnamelen     = 256
  double precision, parameter      :: dmiss_default  = -999.0_fp       ! Default missing value in meteo arrays
  double precision, parameter      :: xymiss         = -999.0_fp       ! Default missing value in elementset
  character(300), target :: errormessage   = ' '             ! When an error occurs, a message is set in message.
                                                             ! function getmeteoerror returns the message

  double precision                 :: pi                      ! pi
  double precision                 :: d2r                     ! degrees to radials
  double precision                 :: r2d                     ! degrees to radials
  double precision, private, parameter     :: earthrad = 6378137.0_fp ! Mathworld, IUGG

contains
   !
   !
   ! ==========================================================================
   !> 
   !> Parses an UDUnit-conventions datetime unit string.
   !! TODO: replace this by calling C-API from UDUnits(-2).
   function parse_ud_timeunit(timeunitstr, iunit, iyear, imonth, iday, ihour, imin, isec) result(ierr)
      character(len=*), intent(in)  :: timeunitstr !< Time unit by UDUnits conventions, e.g. 'seconds since 2012-01-01 00:00:00.0 +0000'.
      integer,          intent(out) :: iunit       !< Unit in seconds, i.e. 'hours since..' has iunit=3600.
      integer,          intent(out) :: iyear       !< Year in reference datetime.
      integer,          intent(out) :: imonth      !< Month in reference datetime.
      integer,          intent(out) :: iday        !< Day in reference datetime.
      integer,          intent(out) :: ihour       !< Hour in reference datetime.
      integer,          intent(out) :: imin        !< Minute in reference datetime.
      integer,          intent(out) :: isec        !< Seconds in reference datetime.
      integer                       :: ierr        !< Error status, only 0 when successful.
   
      integer :: i, n, ifound, iostat
      character(len=7) :: unitstr
   
      ierr = 0
      unitstr = ' '
   
      n = len_trim(timeunitstr)
      ifound = 0
      do i = 1,n
         if (timeunitstr(i:i) == ' ') then ! First space found
            if (timeunitstr(i+1:min(n, i+5)) == 'since') then
               unitstr = timeunitstr(1:i-1)
               ifound = 1
            else
               ierr = 1
            end if
            exit ! Found or error, look no further.
         end if
      end do
   
      if (ifound == 1) then
         select case(trim(unitstr))
         case('seconds')
            iunit = 1
         case('minutes')
            iunit = 60
         case('hours')
            iunit = 3600
         case('days')
            iunit = 86400
         case('weeks')
            iunit = 604800
         case default
            iunit = -1
         end select
         
         read (timeunitstr(i+7:n), '(I4,1H,I2,1H,I2,1H,I2,1H,I2,1H,I2)', iostat = iostat) iyear, imonth, iday, ihour, imin, isec
      
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
! Stef.Hummel@deltares.nl
! Herman.Kernkamp@deltares.nl
! Adri.Mourits@deltares.nl
! Edwin.Spee@deltares.nl
!
!!--declarations----------------------------------------------------------------
  use precision
  use timespace_read
  use timespace_parameters
  implicit none

  double precision           :: timelast = -1d10  ! time of most recent value requested
                                                  ! if time =< timelast, no updates

  double precision           :: t01ini   = -1d10  ! initial time for dataproviders t0 and t1 fields

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
   subroutine readprovider(minp,qid,filename,filetype,method,operand,transformcoef,ja,varname,smask, maxSearchRadius)
     use m_flowexternalforcings, only: NTRANSFORMCOEF
     use MessageHandling, only : LEVEL_WARN, LEVEL_INFO, mess
     ! globals
     integer,           intent(in)            :: minp             !< File handle to already opened input file.
     integer,           intent(out)           :: filetype         !< File type of current quantity.
     integer,           intent(out)           :: method           !< Time-interpolation method for current quantity.
     character (len=*), intent(out)           :: filename         !< Name of data file for current quantity.
     character (len=*), intent(out)           :: qid              !< Identifier of current quantity (i.e., 'waterlevelbnd')
     character (len=1), intent(out)           :: operand          !< Operand w.r.t. previous data ('O'verride or '+'Append)
     real(kind=hp),     intent(out)           :: transformcoef(:) !< Transformation coefficients
     integer,           intent(out)           :: ja               !< Whether a block was successfully read or not.
     character (len=*), intent(out)           :: varname          !< variable name within filename; only in case of NetCDF
     character (len=*), intent(out), optional :: smask            !< Name of mask-file applied to source arcinfo meteo-data
     real(kind=hp),     intent(out), optional :: maxSearchRadius  !< max search radius for method == 11

     ! locals
     character (len=maxnamelen)       :: rec, keywrd
     integer                          :: l1, l2, jaopt, k, extrapolation
     logical, save                    :: alreadyPrinted = .false.  !< flag to avoid printing the same message many times
     
     integer, parameter :: NUMGENERALKEYWRD_OLD = 26
     character(len=256) :: generalkeywrd_old(NUMGENERALKEYWRD_OLD) = (/ character(len=256) :: &
      'widthleftW1',             & ! ( 1)
      'levelleftZb1',            & ! ( 2)
      'widthleftWsdl',           & ! ( 3)
      'levelleftZbsl',           & ! ( 4)
      'widthcenter',             & ! ( 5)
      'levelcenter',             & ! ( 6)
      'widthrightWsdr',          & ! ( 7)
      'levelrightZbsr',          & ! ( 8)
      'widthrightW2',            & ! ( 9)
      'levelrightZb2',           & ! (10)
      'gateheight',              & ! (11)
      'gateheightintervalcntrl', & ! (12)
      'pos_freegateflowcoeff',   & ! (13)
      'pos_drowngateflowcoeff',  & ! (14)
      'pos_freeweirflowcoeff',   & ! (15)
      'pos_drownweirflowcoeff',  & ! (16)
      'pos_contrcoeffreegate',   & ! (17)
      'neg_freegateflowcoeff',   & ! (18)
      'neg_drowngateflowcoeff',  & ! (19)
      'neg_freeweirflowcoeff',   & ! (20)
      'neg_drownweirflowcoeff',  & ! (21)
      'neg_contrcoeffreegate',   & ! (22)
      'extraresistance',         & ! (23)
      'dynstructext',            & ! (24)
      'gatedoorheight',          & ! (25)
      'door_opening_width'       & ! (26)
     /)

     if (minp == 0) then
        ja = 0
        return
     end if

     keywrd = 'QUANTITY'
     call zoekja(minp,rec,keywrd, ja)
     if (ja .eq. 1) then
        l1 = index(rec,'=') + 1
        call checkForSpacesInProvider(rec, l1, l2)               ! l2 = l1 + #spaces after the equal-sign
        read(rec(l2:),'(a)',err=990) qid
     else
        return
     end if
   
     keywrd = 'FILENAME'
     call zoekja(minp,rec,keywrd, ja)
     if (ja .eq. 1) then
        l1 = index(rec,'=') + 1
        call checkForSpacesInProvider(rec, l1, l2)               ! l2 = l1 + #spaces after the equal-sign
        read(rec(l2:),'(a)',err=990) filename
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

     if (present(smask)) then                                    ! todo: shouldn't this argument be compulsory ? ..... 
        keywrd = 'SOURCEMASK'
        call zoekopt(minp, rec, trim(keywrd), jaopt)
        if (jaopt == 1) then
           read (rec,*) smask
        else 
           smask = '' 
        end if
     endif 
   
     keywrd = 'FILETYPE'
     call zoekja(minp,rec,keywrd, ja)
     if (ja .eq. 1) then
        l1 = index(rec,'=') + 1
        call checkForSpacesInProvider(rec, l1, l2)               ! l2 = l1 + #spaces after the equal-sign
        read(rec(l2:),*,    err=990) filetype
     else
        return
     end if
    
     keywrd = 'METHOD'
     method = spaceandtime  ! default : spaceandtime
     call zoekja(minp,rec,keywrd, ja)
     if (ja .eq. 1)      then
        l1 = index(rec,'=') + 1
        call checkForSpacesInProvider(rec, l1, l2)               ! l2 = l1 + #spaces after the equal-sign
        read(rec(l2:),*,    err=990) method
     else
        return
     end if

     if (method == 11) then
        if (.not. alreadyPrinted) then
           call mess(LEVEL_INFO, 'METHOD=11 is obsolete; use METHOD=3 and EXTRAPOLATION_METHOD=1')
           alreadyPrinted = .true.
        endif
        method = 100 + weightfactors
     else
        keywrd = 'EXTRAPOLATION_METHOD'
        call zoekopt(minp, rec, keywrd, jaopt)
        if (jaopt == 1) then
           read(rec,*,err=990) extrapolation
           method = method + 100 * extrapolation
        endif
     endif

     if (present(maxSearchRadius)) then
        keywrd = 'MAXSEARCHRADIUS'
        call zoekopt(minp, rec, keywrd, jaopt)
        if (jaopt == 1) then
           read(rec,*,err=990) maxSearchRadius
        else
           maxSearchRadius = -1.0_hp
        end if
     end if

     keywrd  = 'OPERAND'
     OPERAND = 'O'  ! hk : default =O
     call zoekja(minp,rec,keywrd, ja)
     if (ja .eq. 1) then
        l1 = index(rec,'=') + 1
        call checkForSpacesInProvider(rec, l1, l2)               ! l2 = l1 + #spaces after the equal-sign
        read(rec(l2:l2),'(a1)',    err=990) operand
     else
        return
     end if

     call readTransformcoefficients(minp, transformcoef)

     if (qid == 'generalstructure') then 
        call mess(LEVEL_WARN, 'Keyword [generalstructure] is not supported in the external forcing file. &
                               Please use a structure file <*.ini> instead.')
        if (NUMGENERALKEYWRD_OLD < NTRANSFORMCOEF) call mess(LEVEL_WARN,'Not all expected keywords are provided.')
        if (NUMGENERALKEYWRD_OLD > NTRANSFORMCOEF) call mess(LEVEL_WARN,'More keywords provided than expected.')
        do k = 1,NUMGENERALKEYWRD_OLD
           call readandchecknextrecord(minp, rec, generalkeywrd_old(k), jaopt)
           if (jaopt == 1) then
               L1 = index(rec,'=') + 1
               read (rec(L1:),*) transformcoef(k)
           else
              call qnerror('Looking for '//trim(generalkeywrd_old(k))//', but getting ', rec, ' ')
           end if
        enddo
     endif   
     
     return
   
     990 call readerror('reading '//trim(keywrd)//' but getting ', rec, minp)
   
   end subroutine readprovider
   !
   subroutine readTransformcoefficients(minp, transformcoef)
     integer,       intent(in) :: minp
     real(kind=hp), intent(out) :: transformcoef(:)

     type tKeyInt
        character(len=32) :: key
        integer           :: value
     end type tKeyInt

     character(len=maxnamelen) :: rec
     integer                   :: jaopt, i, ierr
     type(tKeyInt)             :: pairs(21)

     ! constant keywrd = 'DISCHARGE'/'SALINITY'/'TEMPERATURE' removed, now always via time series, in future also via new ext [discharge]

     transformcoef = -999d0

     pairs(1)%key =  'VALUE'
     pairs(1)%value = 1
     pairs(2)%key =  'FACTOR'
     pairs(2)%value = 2
     pairs(3)%key =  'LAYER'
     pairs(3)%value = 3
     pairs(4)%key =  'IFRCTYP'
     pairs(4)%value = 3
     pairs(5)%key =  'AVERAGINGTYPE'
     pairs(5)%value = 4
     pairs(6)%key = 'TRACERFALLVELOCITY'
     pairs(6)%value = 4
     pairs(7)%key =  'TRACERDECAYTIME'
     pairs(7)%value = 5
     pairs(8)%key =  'RELATIVESEARCHCELLSIZE'
     pairs(8)%value = 5
     pairs(9)%key =  'EXTRAPOLTOL'
     pairs(9)%value = 6
     pairs(10)%key =  'PERCENTILEMINMAX'
     pairs(10)%value = 7
     pairs(11)%key =  'AREA' ! Area for source-sink pipe
     pairs(11)%value = 4
     pairs(12)%key =  'TREF' ! relaxation time for riemann boundary
     pairs(12)%value = 7
     pairs(13)%key =  'NUMMIN' ! minimum number of points in averaging
     pairs(13)%value = 8
     pairs(14)%key =  'startlevelsuctionside'
     pairs(14)%value = 4
     pairs(15)%key =  'stoplevelsuctionside'
     pairs(15)%value = 5
     pairs(16)%key =  'startleveldeliveryside'
     pairs(16)%value = 6
     pairs(17)%key =  'stopleveldeliveryside'
     pairs(17)%value = 7
     pairs(18)%key =  'UNIFORMSALINITYABOVEZ'
     pairs(18)%value = 3
     pairs(19)%key =  'UNIFORMSALINITYBELOWZ'
     pairs(19)%value = 4
     pairs(20)%key =  'UNIFORMVALUEABOVEZ'
     pairs(20)%value = 13
     pairs(21)%key =  'UNIFORMVALUEBELOWZ'
     pairs(21)%value = 14

     do i = 1, size(pairs)
        call zoekopt(minp, rec, trim(pairs(i)%key), jaopt)
        if (jaopt == 1) then
            read (rec,*, iostat=ierr) transformcoef(pairs(i)%value)
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
        character (len=256), intent(in)   :: rec         !< Name of record that includes the keyword and record
        integer,             intent(in)   :: eqsign      !< Location of the equal-sign in the entire record string
        integer,             intent(out)  :: eqsignsp    !< Location of the equal-sign plus first spaces after equal-sign
   
        ! Locals
        integer                           :: i           ! Counter
   
        eqsignsp = eqsign
        do i = 0, 256-eqsign
            if (rec(eqsign+i:eqsign+i) .eq. ' ') then
                eqsignsp = eqsignsp + 1
            else
                exit
            end if
        enddo
   end subroutine checkForSpacesInProvider
   !
   !
   ! ==========================================================================
   !> Reads a single polyline from an open file.
   !! Assumes two-column data with x,y pairs.
   subroutine read1polylin(minp,xs,ys,ns,pliname, has_more_records)
      use m_alloc
      integer,                                 intent(inout) :: minp     !< Unit number of poly file (already opened), will be closed after successful read.
      double precision, allocatable,           intent(  out) :: xs(:)    !< x-coordinates read from file    
      double precision, allocatable,           intent(  out) :: ys(:)    !< y-coordinates read from file
      integer,                                 intent(  out) :: ns       !< Number of pli-points read
      character(len=:), allocatable, optional, intent(  out) :: pliname  !< (Optional) Name (identifier) of the polyline read
      logical,                       optional, intent(  out) :: has_more_records !< (Optional) Whether or not more polyline data exists in the remainder of the file, after reading this one polyline.

      character (len=maxnamelen)   :: rec
      integer                      :: k
   
      ns = 0

      if (present(has_more_records)) then
         has_more_records = .false.
      end if

   10 read(minp,'(a)',end = 999) rec
      if  (rec(1:1) == '*' ) goto 10
      if (present(pliname)) then
         pliname = trim(rec)
      end if  
         
      read(minp,'(a)',end = 999) rec
      read(rec ,*    ,err = 888) ns
      
      if (.not. allocated(xs)) then
        allocate(xs(ns))
      else if (ns > size(xs)) then
        call realloc(xs, ns, keepExisting=.false.)
      end if

      if (.not. allocated(ys)) then
        allocate(ys(ns))
      else if (ns > size(ys)) then
        call realloc(ys, ns, keepExisting=.false.)
      end if

      do k = 1,ns
         read(minp,'(a)',end = 999) rec
         read(rec ,*    ,err = 777) xs(k), ys(k)
      enddo
   
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
      integer, intent(inout) :: minp     !< Unit number of poly file, already open, filepointer can be anywhere in the file.
      logical                :: has_more !< Result, whether or not more polyline data may exist in the remainder of the file.
   
      character (len=maxnamelen)   :: rec
   
      has_more = .false.

      do
         read(minp,'(a)',end = 999) rec
         if (rec(1:1) == '*') then
            cycle
         end if
         
         if (len_trim(rec) == 0) then
            cycle
         else
            ! We encountered a non-comment line, non-whitespace line before EOF
            has_more = .true.
            backspace(minp)
            exit
         end if
      end do

      return
   
   999 continue
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
   character (len=8) :: refda
   integer           :: jul00
   double precision  :: tz, timjan
   
   integer           :: juljan
   
   refdat = refda
   read (refdat,*) itdate
   
   read(refdat(1:4),*) iyear0
   read(refdat(5:6),*) imonth0
   read(refdat(7:8),*) iday0
   
   jul0  = julday(imonth0,iday0,iyear0)
   jul00 = jul0
   
   Tzone = tz
   
   juljan = julday(1,1,iyear0)
   timjan = (jul0 - juljan)*24.d0
   
   end subroutine settimespacerefdat
   !
   !
   ! ==========================================================================
   !> 
   function getmeteoerror( ) result(retval)
      implicit none
      character(300), pointer :: retval
      retval => errormessage
   end function getmeteoerror
   !
   !
   ! ==========================================================================
   !> 
   subroutine meteo_tidepotential(jul0, TIME , dstart, dstop , eps) ! call schrama's routines on reduced set
   use m_sferic
   use m_flowparameters, only: jatidep, jaselfal, jamaptidep
   use m_partitioninfo
   use m_flow
   use m_flowgeom
   integer                                             :: jul0    ! interpolate results in ndx 
   integer                                             :: Np      !< number of potentials in tidep
 
   double precision                                    :: time, dstart, dstop , eps, dxx, dyy
   double precision                                    :: xx(4), yy(4)!, DAREA, DLENGTH, DLENMX
                                                     
   double precision, allocatable, save                 :: xz2(:,:),  yz2(:,:), td2(:,:), self(:,:), avhs(:,:)!, area(:,:)
   double precision                                    :: xmn, xmx, ymn, ymx, di, dj, f11,f21,f12,f22  
                  
   double precision, allocatable, save                 :: td2_x(:,:), td2_y(:,:)   
                                     
   integer                                             :: i,j,n,ierr, m1,m2,n1,n2 , L                                                       
   integer, save                                       :: ndx2
   integer, save                                       :: i1
   integer, save                                       :: i2
   integer, save                                       :: j1
   integer, save                                       :: j2
   integer, save                                       :: INI    = 0
   
   np = size(tidep,1)

   if (INI == 0 ) then 
       INI = 1  
   
      XMN = 1D30 ; YMN = 1D30 ; XMX = -1D30 ; YMX = -1D30
      DO I   = 1,ndx
         xmn = min(xz(i),xmn)
         xmx = max(xz(i),xmx)
         ymn = min(yz(i),ymn)
         ymx = max(yz(i),ymx)
      ENDDO
   
      i1  = floor(xmn); i2 = floor(xmx) + 1
      j1  = floor(ymn); j2 = floor(ymx) + 1
      if (jatidep == 2) then ! gradient intp., one extra
         i1 = i1-1 ; i2 = i2+1
         j1 = j1-1 ; j2 = j2+1
      endif
      
      if ( jaselfal .eq.1 .and. jampi == 1) then
!        globally reduce i1, i2, j1, j2
         i1 = -i1
         j1 = -j1
         call reduce_int4_max(i1,i2,j1,j2)
         i1 = -i1
         j1 = -j1
      end if
      
      
      IF (ALLOCATED (XZ2) ) DEALLOCATE (XZ2,YZ2,TD2)
      allocate ( xz2(i1:i2,j1:j2), stat=ierr)   ! tot aerr 
      allocate ( yz2(i1:i2,j1:j2), stat=ierr)  
      allocate ( td2(i1:i2,j1:j2), stat=ierr)

      if (jatidep > 1) then ! gradient intp.
         if (allocated(td2_x)) deallocate(td2_x, td2_y)  
         allocate ( td2_x(i1:i2,j1:j2), stat=ierr)
         allocate ( td2_y(i1:i2,j1:j2), stat=ierr)
      endif  

      td2 = 0d0
   
      if (jaselfal > 0) then
!         if (allocated(self) ) deallocate ( self, avhs, area ) MVL ask Camille
         if (allocated(self) ) deallocate ( self, avhs )
         allocate ( self(i1:i2,j1:j2), stat=ierr)  
         allocate (avhs(i1:i2,j1:j2), stat=ierr)  
!         allocate ( area(i1:i2,j1:j2), stat=ierr)  
            do i = i1,i2
               do j = j1,j2
                xx(1) = dble(i)-0.5d0 ; yy(1) = dble(j)-0.5d0
                xx(2) = dble(i)+0.5d0 ; yy(2) = dble(j)-0.5d0
                xx(3) = dble(i)+0.5d0 ; yy(3) = dble(j)+0.5d0
                xx(4) = dble(i)-0.5d0 ; yy(4) = dble(j)+0.5d0
            
!                call dAREAN( XX, YY, 4, DAREA, DLENGTH, DLENMX )
!                area(i,j) = darea 
            enddo    
         enddo
      endif
      
      do i = i1,i2
         do j = j1,j2
            xz2(i,j) = i*dg2rd 
            yz2(i,j) = j*dg2rd 
         enddo    
      enddo
           
      ndx2 = (i2-i1+1)*(  j2-j1+1)
   
   end if       
   
   if ( jatidep > 0) then      
      call tforce( jul0, TIME , xz2 , yz2 , Td2, ndx2, dstart, dstop , eps) 
   else
      td2 = 0d0   ! safety
   end if
         
   if (jaselfal > 0) then 
      call aggregatewaterlevels(avhs, i1,i2,j1,j2 )
      
      call selfattraction(avhs, self, i1,i2,j1,j2, jaselfal ) 
   endif      
   
   do n = 1,ndx
      m1  = floor(xz(n))     ; m2 = m1+1 
      n1  = floor(yz(n))     ; n2 = n1+1
      di  = xz(n) - m1   
      dj  = yz(n) - n1
      f11 = (1d0-di)*(1d0-dj)
      f21 = (    di)*(1d0-dj)
      f22 = (    di)*(    dj)
      f12 = (1d0-di)*(    dj)
   
      if (jaselfal > 0) then 
            
         tidep(1,n) = ( td2(m1,n1) + self(m1,n1) )*f11 +    &
                      ( td2(m2,n1) + self(m2,n1) )*f21 +    &
                      ( td2(m2,n2) + self(m2,n2) )*f22 +    &
                      ( td2(m1,n2) + self(m1,n2) )*f12
                    
!        for output only
         if ( jamaptidep.gt.0 .and. Np.gt.1 ) then ! store SAL potential seperately
            tidep(2,n) = ( self(m1,n1) )*f11 +    &
                         ( self(m2,n1) )*f21 +    &
                         ( self(m2,n2) )*f22 +    &
                         ( self(m1,n2) )*f12
         end if
      else
         tidep(1,n) =   td2(m1,n1)                *f11 +    &
                        td2(m2,n1)                *f21 +    &
                        td2(m2,n2)                *f22 +    &
                        td2(m1,n2)                *f12
      endif
   enddo
   
   if ( jatidep > 1) then ! gradient intp., get gradient  

      dyy = 2d0*ra*dg2rd
      do j = j1+1,j2-1 
         dxx  = dyy*cos(yz2(i1,j))
         do i = i1+1,i2-1
            td2_x(i,j) = ( td2(i+1,j) - td2(i-1,j) ) / dxx 
            td2_y(i,j) = ( td2(i,j+1) - td2(i,j-1) ) / dyy 
            if (jaselfal >0) then 
               td2_x(i,j) = td2_x(i,j) + ( self(i+1,j) - self(i-1,j) ) / dxx 
               td2_y(i,j) = td2_y(i,j) + ( self(i,j+1) - self(i,j-1) ) / dyy 
            endif 
         enddo
      enddo

      do L = 1,Lnx
         m1  = floor(xu(L))     ; m2 = m1+1 
         n1  = floor(yu(L))     ; n2 = n1+1
         di  = xu(L) - m1   
         dj  = yu(L) - n1
         f11 = (1d0-di)*(1d0-dj)
         f21 = (    di)*(1d0-dj)
         f22 = (    di)*(    dj)
         f12 = (1d0-di)*(    dj)
            
         tidef(L) = csu(L)*( td2_x(m1,n1)*f11 +    &
                             td2_x(m2,n1)*f21 +    &
                             td2_x(m2,n2)*f22 +    &
                             td2_x(m1,n2)*f12 )    &
                  + snu(L)*( td2_y(m1,n1)*f11 +    &
                             td2_y(m2,n1)*f21 +    &
                             td2_y(m2,n2)*f22 +    &
                             td2_y(m1,n2)*f12 )
      enddo
     
   endif

   end subroutine meteo_tidepotential
   
   subroutine aggregatewaterlevels(avhs, i1,i2,j1,j2 )
   !Compute the water level from the unstructured to the one degree regular grid
   use m_flow
   use m_flowgeom
   use m_GlobalParameters, only: INDTP_2D
   use m_partitioninfo
   use kdtree2Factory
   use unstruc_messages
   implicit none
   integer          :: i1, i2, j1, j2, k, k1, LL, i, j, iL, iR, ierr
   integer, save    :: ini = 0
   double precision :: alf, x, y
   double precision :: avhs(i1:i2,j1:j2), area(i1:i2,j1:j2)
   
   double precision, dimension(:,:),   allocatable       :: xx, yy
   integer,          dimension(:,:),   allocatable       :: kk
   double precision, dimension(:,:,:), allocatable, save :: workin, workout  ! work arrays for parallel communication
   
   integer          :: Ni, Nj
   integer          :: jakdtree = 1
   integer          :: ierror
   
   character(len=1024) :: str
   
   double precision    :: t0, t1
   double precision    :: wo
   double precision    :: Ds
   
   double precision, allocatable, save :: jasea(:,:)
   
     
      
   Ni = i2-i1+1
   Nj = j2-j1+1
   
   if (ini ==0) then 
      call klok(t0)
      
      allocate ( jasea(i1:i2,j1:j2), stat=ierr) 
      
      if ( jakdtree.eq.1 ) then
         call realloc(xx, (/ Ni, Nj /), keepExisting=.false., fill=0d0)
         call realloc(yy, (/ Ni, Nj /), keepExisting=.false., fill=0d0)
         call realloc(kk, (/ Ni, Nj /), keepExisting=.false., fill=0)
         do j=j1,j2
            do i=i1,i2
               xx(i-i1+1,j-j1+1) = dble(i)
               yy(i-i1+1,j-j1+1) = dble(j)
            end do
         end do
         call find_flowcells_kdtree(treeglob,Ni*Nj,xx,yy,kk,jakdtree,INDTP_2D, ierror)
         if ( ierror.ne.0 ) then
            jakdtree = 0
         end if
         
         if ( allocated(xx) ) deallocate(xx)
         if ( allocated(yy) ) deallocate(yy)
      end if
      
      if ( jampi.eq.0 ) then  ! sequential
         do j = j1,j2
             do i = i1,i2
                if( jakdtree.eq.1 ) then
                   k = kk(i-i1+1,j-j1+1)
                else
                   x = dble(i)
                   y = dble(j)
                   call in_flowcell(x, y, K)
                end if
             
                if (k > 0) then
                   jasea(i,j) = 1
                else   
                   jasea(i,j) = 0 
                endif    
             enddo
         enddo
      else
!        allocate work arrays      
         if ( allocated(workin) ) deallocate(workin)
         allocate(workin(2,Ni,Nj))
         workin = 0d0
         if ( allocated(workout) ) deallocate(workout)
         allocate(workout(2,Ni,Nj))
         
         do j = j1,j2
             do i = i1,i2
                if( jakdtree.eq.1 ) then
                   k = kk(i-i1+1,j-j1+1)
                else
                   x = dble(i)
                   y = dble(j)
                   call in_flowcell(x, y, K)
                end if
                
                if ( k.le.Ndxi ) then
                   k1 = k
                else  ! boundary nodes: take connected internal node for domain number (boundary nodes are always in own domain)
                   LL = iabs(nd(k)%ln(1))  !< only one link connected to boundary node
                   k1 = ln(1,LL) + ln(2,LL) - k
                end if
             
                if ( k > 0 ) then
                   if ( idomain(k1).eq.my_rank ) then
!                   jasea(i,j) = 1
                      workin(1,i-i1+1,j-j1+1) = 1d0
                      workin(2,i-i1+1,j-j1+1) = 0d0 ! dummy
                   else
                      workin(1,i-i1+1,j-j1+1) = 0d0
                      workin(2,i-i1+1,j-j1+1) = 0d0 ! dummy
                   end if
                else   
!                   jasea(i,j) = 0
                   workin(1,i-i1+1,j-j1+1) = 0d0
                   workin(2,i-i1+1,j-j1+1) = 0d0 ! dummy
                endif    
             enddo
         enddo
         call reduce_double_sum(2*Ni*Nj, workin, workout)   ! workin too large, but only once
         do j=j1,j2
            do i=i1,i2
               wo = workout(1,i-i1+1,j-j1+1)
               if ( wo.eq.0 ) then
                  jasea(i,j) = 0
               else
                  jasea(i,j) = 1
               end if
            end do
         end do
      end if
      
      call klok(t1)
      write(str,"('SAL/aggregate waterlevels, elapsed time: ', G15.5, 's.')") t1-t0
      call mess(LEVEL_INFO, trim(str))

      ini = 1
   endif
   
   if ( allocated(kk) ) deallocate(kk)
   
   jasea = 1
   
   avhs = 0d0
   area = 0d0
   
   if ( jampi.eq.0 ) then
      do k = 1,ndx
         i = nint(xz(k))
         j = nint(yz(k))
         
         Ds = 0d0
         if ( jaSELFALcorrectWLwithIni.eq.1 ) then
!           water level rise
            Ds = s1init(k)
         end if
         
         if (hs(k)>0.0) then
                     
            avhs(i,j) = avhs(i,j) + (s1(k)-Ds)*ba(k)
            area(i,j) = area(i,j) + ba(k)
         endif
      enddo
   else  ! parallel
      workin = 0d0
      
      do k = 1,Ndx
         i = nint(xz(k))
         j = nint(yz(k))
         
         if ( k.le.Ndxi ) then   ! internal nodes
            k1 = k
         else  ! boundary nodes: take connected internal node for domain number (boundary nodes are always in own domain)
            LL = iabs(nd(k)%ln(1))  !< only one link connected to boundary node
            k1 = ln(1,LL) + ln(2,LL) - k
         end if
         
         Ds = 0d0
         if ( jaSELFALcorrectWLwithIni.eq.1 ) then
!           water level rise
            Ds = s1init(k)
         end if
         
         if (hs(k)>0.0 .and. idomain(k1).eq.my_rank ) then
!                    avhs(i,j) = avhs(i,j) + s1(k)*ba(k)
!                    area(i,j) = area(i,j) + ba(k)
         
            workin(1,i-i1+1,j-j1+1) = workin(1,i-i1+1,j-j1+1) + (s1(k)-Ds)*ba(k)
            workin(2,i-i1+1,j-j1+1) = workin(2,i-i1+1,j-j1+1) + ba(k)
         endif
      enddo
      call reduce_double_sum(2*Ni*Nj, workin, workout)
      
      do j=j1,j2
         do i=i1,i2
            avhs(i,j) = workout(1,i-i1+1,j-j1+1)
            area(i,j) = workout(2,i-i1+1,j-j1+1)
         end do
      end do
   end if
  
  do j = j1,j2
      do i = i1,i2
          if (area(i,j) > 0.0) then
             avhs(i,j) = avhs(i,j) / area(i,j)
          endif
     enddo
  enddo   
  
  do j = j1,j2
      do i = i1,i2
          if (area(i,j) == 0.0 .and. jasea(i,j) == 1) then
             call findleftright(area,i,j,i1,i2,j1,j2,iL,iR,alf) 
             avhs(i,j) = (1d0-alf)*avhs(iL,j) + alf*avhs(iR,j)
          endif
      enddo
  enddo
  
  !Used for testing
  !avhs=1d0 
 
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

   subroutine findleftright(area,ii,ji,i1,i2,j1,j2,iL,iR,alf) 
   implicit none
   integer, intent(in) :: i1,i2, ii, j1, j2, ji
   double precision, intent(in) :: area(i1:i2,j1:j2)
   
   integer, intent(out) :: iL, iR
   double precision, intent(out) :: alf
   integer :: i, dr, dl, findr, findl, disR, disL, stopsearch
   
   stopsearch = 0
   
   findr = 0
   dr = 0
   i = ii
   do while (findr==0)
       if (area(i,ji)/=0) then
           iR = i
           findr = 1
       endif
       if (i==i1) then
           i = i2
           dr = 1
       else
           i = i-1
       endif
       if (i==ii) then
           findr = 1
           iR = ii
           iL = ii
           alf = 0
           stopsearch = 1
       endif    
   enddo
   
   if (stopsearch==0) then
       
        findl = 0
        dl = 0
        i = ii
        do while (findl==0)
           if (area(i,ji)/=0) then
              iL = i
              findl = 1
           endif
           if (i==i2) then
               i = i1
               dl = 1
           else
               i = i+1
           endif
       enddo
   
      if (dl==0) then
          disL = ii - iL
      else
          disL = (ii-i2)-(iL-i1)
      endif
      if (dr==0) then
       disR = ii - iR
      else
          disR = (ii-i1)-(iR-i2)
       endif
   
      alf = dble(disL) / dble(disL-disR)
      
   endif
   end subroutine findleftright
   
   subroutine selfattraction(avhs, self, i1,i2,j1,j2, jaselfal )
   implicit none
   
   ! Input\Output parameter
   integer,          intent(in)  :: i1,i2,j1,j2, jaselfal
   double precision, intent(in)  :: avhs(i1:i2,j1:j2)
   double precision, intent(out) :: self(i1:i2,j1:j2)
   
   ! Local parameters
   double precision, parameter :: Me=5.9726d24, R=6371d3, g=9.81d0, pi=4d0*atan(1.0), rhow=1.0240164d3 ,rhoe=3d0*Me/(4d0*pi*R*R*R)
   integer :: nlat, nlon, n15, lsave, lwork, ldwork, lwk, liwk, lshaec, lshsec
   integer :: i, j, ierror, isym, nt, l, mdab, ndab, k1
!   double precision, dimension(0:1024) :: llnh, llnk
   double precision, dimension(:),   allocatable :: llnh, llnk
   double precision, dimension(:),   allocatable :: work, wk, iwk, wshaec, wshsec
   double precision, dimension(:),   allocatable :: dwork
   double precision, dimension(:,:), allocatable :: a, b
!   double precision, dimension(0:180,0:359) :: avhs1, self1
   double precision, dimension(:,:), allocatable :: avhs1, self1
   
   ! Initialisation
   nlat=181
   nlon=360
   n15=nlon+15
   lsave=nlat*(nlat+1)+3*((nlat-2)*(2*nlat-nlat-1)+n15)
   lshaec=lsave
   lshsec=lsave
   lwork=(nlat+1)*(nlon+3*nlat)+nlat*(2*nlat+1)
   ldwork=nlat+1
   lwk=46*nlat*(nlon+1)
   liwk=14*nlat*(nlon+1)
   mdab=nlat
   ndab=nlat

!  allocate
   allocate(work(1:lwork))
   allocate(dwork(1:ldwork))
   allocate(wk(1:lwk))
   allocate(iwk(1:liwk))
   allocate(wshaec(1:lsave))
   allocate(wshsec(1:lsave))
   allocate (a(1:mdab,1:ndab))
   allocate (b(1:mdab,1:ndab))
   
   allocate(llnh(0:1024))
   allocate(llnk(0:1024))
   allocate(avhs1(0:180,0:359))
   allocate(self1(0:180,0:359))
   
   !Water level need to be defined in an array avhs1, 
   ! where avhs1(i,j) contains the waterlevel on the point with longitude phi(j)=(j-1)*2*pi/nlon
   ! and colatitude theta(i)=(i-1)*pi/(nlat)
   !For a one degree grid, we have nlon=360 and nlat=181
   !If avhs is smaller then 0 is chosen at the location of the missing values
   avhs1 = 0d0
   k1=0
   do i = i1,min0(i2,i1+360-1)
         do j = j1,j2 
             avhs1(j+90,k1)=avhs(i,j)
         enddo   
         k1=k1+1
   enddo
   
   !Load Love numbers
   call loadlovenumber(llnh,llnk)
   
   !Computation
   isym=0
   nt=1
   !Spherical harmonic analysis
   call shaeci(nlat,nlon,wshaec,lshaec,dwork,ldwork,ierror)
   call shaec(nlat,nlon,isym,nt,avhs1,nlat,nlon,a,b,mdab,ndab, &
        wshaec,lshaec,work,lwork,ierror)
   
   !Multiplication in spherical harmonic space (=convolution)
   if (jaselfal == 2) then
        do l=1,ndab
            a(1:mdab,l)=3*g*rhow/rhoe/(2*l-1)*a(1:mdab,l)
            b(1:mdab,l)=3*g*rhow/rhoe/(2*l-1)*b(1:mdab,l)
        end do
   end if
   if (jaselfal == 1) then
        do l=1,ndab
            a(1:mdab,l)=3*g*rhow*(1+llnk(l-1)-llnh(l-1))/rhoe/(2*l-1)*a(1:mdab,l)
            b(1:mdab,l)=3*g*rhow*(1+llnk(l-1)-llnh(l-1))/rhoe/(2*l-1)*b(1:mdab,l)
        end do
   end if
    
   !Spherical harmonic synthesis
   call shseci(nlat,nlon,wshsec,lshsec,dwork,ldwork,ierror)
   call shsec(nlat,nlon,isym,nt,self1,nlat,nlon,a,b,mdab,ndab, &
        wshsec,lshsec,work,lwork,ierror)
   
   !self1 is defined on the same grid than avhs1, we put it back in the same grid than avhs
   self = 0d0
   k1=0
   do i = i1,i2
       if (k1>=360) then
        k1=0
       endif
       do j = j1,j2
           if (j+90>=0 .AND. j-90<=180) then
            self(i,j)=self1(j+90,k1)
           endif
       enddo
       k1=k1+1
   enddo
   
   !Create output file
!   open (newunit=filsal, file='d:\output_SALtide2.txt',status='unknown',position='append')
!   open (newunit=filtide, file='d:\output_tide2.txt',status='unknown',position='append')
!   do i=1,nlat
!            do j=1,nlon
!                write(filsal,fmt=*) self1(i-1,j-1)/g
!                write(filtide,fmt=*) avhs1(i-1,j-1)
!            enddo
!   enddo
!   close(filsal)
!   close(filtide)
 !  open (newunit=filavhs, file='d:\output_avhs.txt',status='unknown',position='append')
 !  do i=i1,i2
 !           do j=j1,j2
 !               write(filavhs,fmt=*) avhs(i,j)
 !           enddo
 !  enddo
 !  close(filavhs)
   
!  deallocate
   if (allocated(work)   ) deallocate(work)
   if (allocated(dwork)  ) deallocate(dwork)
   if (allocated(wk)     ) deallocate(wk)
   if (allocated(iwk)    ) deallocate(iwk)
   if (allocated(wshaec) ) deallocate(wshaec)
   if (allocated(wshsec) ) deallocate(wshsec)
   if (allocated(a)      ) deallocate (a)
   if (allocated(b)      ) deallocate (b)
   
   if (allocated(llnh)   ) deallocate(llnh)
   if (allocated(llnk)   ) deallocate(llnk)
   if (allocated(avhs1)  ) deallocate(avhs1)
   if (allocated(self1)  ) deallocate(self1)
      
   end subroutine selfattraction
   
   subroutine loadlovenumber(llnh,llnk)
   !Define the second load Love number h' and k' up to degree 1024
   implicit none
   
   ! Input\Output parameter
   double precision, dimension(0:1024), intent(out) :: llnh, llnk
   
   !Fill arrays
llnh( 0  )= 0.0000000000D+00
llnh( 1  )= -0.1285877758D+01
llnh( 2  )= -0.9915810331D+00
llnh( 3  )= -0.1050767745D+01
llnh( 4  )= -0.1053393012D+01
llnh( 5  )= -0.1086317605D+01
llnh( 6  )= -0.1143860336D+01
llnh( 7  )= -0.1212408459D+01
llnh( 8  )= -0.1283943275D+01
llnh( 9  )= -0.1354734845D+01
llnh( 10 )= -0.1423282851D+01
llnh( 11 )= -0.1489094554D+01
llnh( 12 )= -0.1552074997D+01
llnh( 13 )= -0.1612273740D+01
llnh( 14 )= -0.1669763369D+01
llnh( 15 )= -0.1724635488D+01
llnh( 16 )= -0.1776963521D+01
llnh( 17 )= -0.1826825601D+01
llnh( 18 )= -0.1874298467D+01
llnh( 19 )= -0.1919461416D+01
llnh( 20 )= -0.1962393632D+01
llnh( 21 )= -0.2003182253D+01
llnh( 22 )= -0.2041915786D+01
llnh( 23 )= -0.2078680486D+01
llnh( 24 )= -0.2113573061D+01
llnh( 25 )= -0.2146680270D+01
llnh( 26 )= -0.2178105661D+01
llnh( 27 )= -0.2207927152D+01
llnh( 28 )= -0.2236242846D+01
llnh( 29 )= -0.2263132641D+01
llnh( 30 )= -0.2288687940D+01
llnh( 31 )= -0.2312991757D+01
llnh( 32 )= -0.2336112443D+01
llnh( 33 )= -0.2358128831D+01
llnh( 34 )= -0.2379107893D+01
llnh( 35 )= -0.2399120761D+01
llnh( 36 )= -0.2418226351D+01
llnh( 37 )= -0.2436482905D+01
llnh( 38 )= -0.2453948379D+01
llnh( 39 )= -0.2470670195D+01
llnh( 40 )= -0.2486697757D+01
llnh( 41 )= -0.2502076334D+01
llnh( 42 )= -0.2516847401D+01
llnh( 43 )= -0.2531050008D+01
llnh( 44 )= -0.2544719530D+01
llnh( 45 )= -0.2557890739D+01
llnh( 46 )= -0.2570594319D+01
llnh( 47 )= -0.2582859779D+01
llnh( 48 )= -0.2594714216D+01
llnh( 49 )= -0.2606182782D+01
llnh( 50 )= -0.2617289738D+01
llnh( 51 )= -0.2628056023D+01
llnh( 52 )= -0.2638502978D+01
llnh( 53 )= -0.2648649164D+01
llnh( 54 )= -0.2658512061D+01
llnh( 55 )= -0.2668109142D+01
llnh( 56 )= -0.2677455130D+01
llnh( 57 )= -0.2686564655D+01
llnh( 58 )= -0.2695451439D+01
llnh( 59 )= -0.2704127764D+01
llnh( 60 )= -0.2712605707D+01
llnh( 61 )= -0.2720896238D+01
llnh( 62 )= -0.2729009765D+01
llnh( 63 )= -0.2736955903D+01
llnh( 64 )= -0.2744743969D+01
llnh( 65 )= -0.2752382423D+01
llnh( 66 )= -0.2759879282D+01
llnh( 67 )= -0.2767242102D+01
llnh( 68 )= -0.2774478021D+01
llnh( 69 )= -0.2781593811D+01
llnh( 70 )= -0.2788595709D+01
llnh( 71 )= -0.2795489680D+01
llnh( 72 )= -0.2802281343D+01
llnh( 73 )= -0.2808976028D+01
llnh( 74 )= -0.2815578704D+01
llnh( 75 )= -0.2822094093D+01
llnh( 76 )= -0.2828526669D+01
llnh( 77 )= -0.2834880683D+01
llnh( 78 )= -0.2841160150D+01
llnh( 79 )= -0.2847368769D+01
llnh( 80 )= -0.2853510163D+01
llnh( 81 )= -0.2859587939D+01
llnh( 82 )= -0.2865604931D+01
llnh( 83 )= -0.2871564378D+01
llnh( 84 )= -0.2877469169D+01
llnh( 85 )= -0.2883322045D+01
llnh( 86 )= -0.2889125648D+01
llnh( 87 )= -0.2894882413D+01
llnh( 88 )= -0.2900594702D+01
llnh( 89 )= -0.2906264743D+01
llnh( 90 )= -0.2911894687D+01
llnh( 91 )= -0.2917486512D+01
llnh( 92 )= -0.2923042145D+01
llnh( 93 )= -0.2928563403D+01
llnh( 94 )= -0.2934052041D+01
llnh( 95 )= -0.2939509674D+01
llnh( 96 )= -0.2944937877D+01
llnh( 97 )= -0.2950338132D+01
llnh( 98 )= -0.2955711880D+01
llnh( 99 )= -0.2961060436D+01
llnh( 100   )= -0.2966385090D+01
llnh( 101   )= -0.2971687056D+01
llnh( 102   )= -0.2976967512D+01
llnh( 103   )= -0.2982227536D+01
llnh( 104   )= -0.2987468183D+01
llnh( 105   )= -0.2992690446D+01
llnh( 106   )= -0.2997895290D+01
llnh( 107   )= -0.3003083596D+01
llnh( 108   )= -0.3008256228D+01
llnh( 109   )= -0.3013413998D+01
llnh( 110   )= -0.3018557697D+01
llnh( 111   )= -0.3023688044D+01
llnh( 112   )= -0.3028805745D+01
llnh( 113   )= -0.3033911465D+01
llnh( 114   )= -0.3039005849D+01
llnh( 115   )= -0.3044089483D+01
llnh( 116   )= -0.3049162947D+01
llnh( 117   )= -0.3054226779D+01
llnh( 118   )= -0.3059281508D+01
llnh( 119   )= -0.3064327612D+01
llnh( 120   )= -0.3069365560D+01
llnh( 121   )= -0.3074395793D+01
llnh( 122   )= -0.3079418737D+01
llnh( 123   )= -0.3084434789D+01
llnh( 124   )= -0.3089444323D+01
llnh( 125   )= -0.3094447698D+01
llnh( 126   )= -0.3099445739D+01
llnh( 127   )= -0.3104437859D+01
llnh( 128   )= -0.3109424785D+01
llnh( 129   )= -0.3114406817D+01
llnh( 130   )= -0.3119384228D+01
llnh( 131   )= -0.3124357299D+01
llnh( 132   )= -0.3129326253D+01
llnh( 133   )= -0.3134291331D+01
llnh( 134   )= -0.3139252753D+01
llnh( 135   )= -0.3144210746D+01
llnh( 136   )= -0.3149165487D+01
llnh( 137   )= -0.3154117170D+01
llnh( 138   )= -0.3159065971D+01
llnh( 139   )= -0.3164012071D+01
llnh( 140   )= -0.3168955611D+01
llnh( 141   )= -0.3173896746D+01
llnh( 142   )= -0.3178835615D+01
llnh( 143   )= -0.3183772364D+01
llnh( 144   )= -0.3188707103D+01
llnh( 145   )= -0.3193639953D+01
llnh( 146   )= -0.3198571025D+01
llnh( 147   )= -0.3203500433D+01
llnh( 148   )= -0.3208428262D+01
llnh( 149   )= -0.3213354609D+01
llnh( 150   )= -0.3218279559D+01
llnh( 151   )= -0.3223203202D+01
llnh( 152   )= -0.3228125600D+01
llnh( 153   )= -0.3233046832D+01
llnh( 154   )= -0.3237966958D+01
llnh( 155   )= -0.3242886050D+01
llnh( 156   )= -0.3247804156D+01
llnh( 157   )= -0.3252721331D+01
llnh( 158   )= -0.3257637624D+01
llnh( 159   )= -0.3262553244D+01
llnh( 160   )= -0.3267467917D+01
llnh( 161   )= -0.3272381835D+01
llnh( 162   )= -0.3277295035D+01
llnh( 163   )= -0.3282207554D+01
llnh( 164   )= -0.3287119415D+01
llnh( 165   )= -0.3292030647D+01
llnh( 166   )= -0.3296941273D+01
llnh( 167   )= -0.3301851314D+01
llnh( 168   )= -0.3306760804D+01
llnh( 169   )= -0.3311669742D+01
llnh( 170   )= -0.3316578148D+01
llnh( 171   )= -0.3321486032D+01
llnh( 172   )= -0.3326393422D+01
llnh( 173   )= -0.3331300520D+01
llnh( 174   )= -0.3336207644D+01
llnh( 175   )= -0.3341113629D+01
llnh( 176   )= -0.3346019149D+01
llnh( 177   )= -0.3350924191D+01
llnh( 178   )= -0.3355828764D+01
llnh( 179   )= -0.3360732862D+01
llnh( 180   )= -0.3365636499D+01
llnh( 181   )= -0.3370539656D+01
llnh( 182   )= -0.3375442336D+01
llnh( 183   )= -0.3380344530D+01
llnh( 184   )= -0.3385246241D+01
llnh( 185   )= -0.3390147452D+01
llnh( 186   )= -0.3395048158D+01
llnh( 187   )= -0.3399948348D+01
llnh( 188   )= -0.3404848021D+01
llnh( 189   )= -0.3409747153D+01
llnh( 190   )= -0.3414645740D+01
llnh( 191   )= -0.3419543764D+01
llnh( 192   )= -0.3424441221D+01
llnh( 193   )= -0.3429338088D+01
llnh( 194   )= -0.3434234352D+01
llnh( 195   )= -0.3439129995D+01
llnh( 196   )= -0.3444025009D+01
llnh( 197   )= -0.3448919371D+01
llnh( 198   )= -0.3453813064D+01
llnh( 199   )= -0.3458706066D+01
llnh( 200   )= -0.3463598369D+01
llnh( 201   )= -0.3468489946D+01
llnh( 202   )= -0.3473380779D+01
llnh( 203   )= -0.3478270847D+01
llnh( 204   )= -0.3483160133D+01
llnh( 205   )= -0.3488048612D+01
llnh( 206   )= -0.3492936263D+01
llnh( 207   )= -0.3497823067D+01
llnh( 208   )= -0.3502708995D+01
llnh( 209   )= -0.3507594040D+01
llnh( 210   )= -0.3512478162D+01
llnh( 211   )= -0.3517361345D+01
llnh( 212   )= -0.3522243562D+01
llnh( 213   )= -0.3527124799D+01
llnh( 214   )= -0.3532005020D+01
llnh( 215   )= -0.3536884206D+01
llnh( 216   )= -0.3541762329D+01
llnh( 217   )= -0.3546639373D+01
llnh( 218   )= -0.3551515301D+01
llnh( 219   )= -0.3556390096D+01
llnh( 220   )= -0.3561263723D+01
llnh( 221   )= -0.3566136170D+01
llnh( 222   )= -0.3571007398D+01
llnh( 223   )= -0.3575877387D+01
llnh( 224   )= -0.3580747513D+01
llnh( 225   )= -0.3585615132D+01
llnh( 226   )= -0.3590481365D+01
llnh( 227   )= -0.3595346263D+01
llnh( 228   )= -0.3600209792D+01
llnh( 229   )= -0.3605071936D+01
llnh( 230   )= -0.3609932657D+01
llnh( 231   )= -0.3614791931D+01
llnh( 232   )= -0.3619649732D+01
llnh( 233   )= -0.3624506035D+01
llnh( 234   )= -0.3629360805D+01
llnh( 235   )= -0.3634214026D+01
llnh( 236   )= -0.3639065656D+01
llnh( 237   )= -0.3643915683D+01
llnh( 238   )= -0.3648764066D+01
llnh( 239   )= -0.3653610782D+01
llnh( 240   )= -0.3658455800D+01
llnh( 241   )= -0.3663299103D+01
llnh( 242   )= -0.3668140651D+01
llnh( 243   )= -0.3672980417D+01
llnh( 244   )= -0.3677818461D+01
llnh( 245   )= -0.3682654761D+01
llnh( 246   )= -0.3687489030D+01
llnh( 247   )= -0.3692321405D+01
llnh( 248   )= -0.3697151858D+01
llnh( 249   )= -0.3701980361D+01
llnh( 250   )= -0.3706806936D+01
llnh( 251   )= -0.3711631455D+01
llnh( 252   )= -0.3716453939D+01
llnh( 253   )= -0.3721274358D+01
llnh( 254   )= -0.3726092689D+01
llnh( 255   )= -0.3730908892D+01
llnh( 256   )= -0.3735722950D+01
llnh( 257   )= -0.3740534821D+01
llnh( 258   )= -0.3745344490D+01
llnh( 259   )= -0.3750151923D+01
llnh( 260   )= -0.3754957088D+01
llnh( 261   )= -0.3759759956D+01
llnh( 262   )= -0.3764560505D+01
llnh( 263   )= -0.3769358699D+01
llnh( 264   )= -0.3774154515D+01
llnh( 265   )= -0.3778947920D+01
llnh( 266   )= -0.3783738888D+01
llnh( 267   )= -0.3788527389D+01
llnh( 268   )= -0.3793313390D+01
llnh( 269   )= -0.3798096866D+01
llnh( 270   )= -0.3802877791D+01
llnh( 271   )= -0.3807656137D+01
llnh( 272   )= -0.3812431870D+01
llnh( 273   )= -0.3817204958D+01
llnh( 274   )= -0.3821975383D+01
llnh( 275   )= -0.3826743110D+01
llnh( 276   )= -0.3831508110D+01
llnh( 277   )= -0.3836271873D+01
llnh( 278   )= -0.3841032150D+01
llnh( 279   )= -0.3845788986D+01
llnh( 280   )= -0.3850542997D+01
llnh( 281   )= -0.3855294157D+01
llnh( 282   )= -0.3860042438D+01
llnh( 283   )= -0.3864787807D+01
llnh( 284   )= -0.3869530239D+01
llnh( 285   )= -0.3874269702D+01
llnh( 286   )= -0.3879006179D+01
llnh( 287   )= -0.3883739635D+01
llnh( 288   )= -0.3888470049D+01
llnh( 289   )= -0.3893197379D+01
llnh( 290   )= -0.3897921616D+01
llnh( 291   )= -0.3902642720D+01
llnh( 292   )= -0.3907360668D+01
llnh( 293   )= -0.3912075438D+01
llnh( 294   )= -0.3916786993D+01
llnh( 295   )= -0.3921495314D+01
llnh( 296   )= -0.3926200363D+01
llnh( 297   )= -0.3930902125D+01
llnh( 298   )= -0.3935600573D+01
llnh( 299   )= -0.3940295676D+01
llnh( 300   )= -0.3944987404D+01
llnh( 301   )= -0.3949675741D+01
llnh( 302   )= -0.3954360653D+01
llnh( 303   )= -0.3959042119D+01
llnh( 304   )= -0.3963720107D+01
llnh( 305   )= -0.3968394592D+01
llnh( 306   )= -0.3973065547D+01
llnh( 307   )= -0.3977732958D+01
llnh( 308   )= -0.3982396782D+01
llnh( 309   )= -0.3987056997D+01
llnh( 310   )= -0.3991713583D+01
llnh( 311   )= -0.3996366510D+01
llnh( 312   )= -0.4001015766D+01
llnh( 313   )= -0.4005661299D+01
llnh( 314   )= -0.4010303104D+01
llnh( 315   )= -0.4014941156D+01
llnh( 316   )= -0.4019575415D+01
llnh( 317   )= -0.4024205867D+01
llnh( 318   )= -0.4028832484D+01
llnh( 319   )= -0.4033455241D+01
llnh( 320   )= -0.4038074113D+01
llnh( 321   )= -0.4042689078D+01
llnh( 322   )= -0.4047300107D+01
llnh( 323   )= -0.4051907177D+01
llnh( 324   )= -0.4056510264D+01
llnh( 325   )= -0.4061109341D+01
llnh( 326   )= -0.4065704388D+01
llnh( 327   )= -0.4070295381D+01
llnh( 328   )= -0.4074882284D+01
llnh( 329   )= -0.4079465087D+01
llnh( 330   )= -0.4084043760D+01
llnh( 331   )= -0.4088618279D+01
llnh( 332   )= -0.4093188619D+01
llnh( 333   )= -0.4097757745D+01
llnh( 334   )= -0.4102319954D+01
llnh( 335   )= -0.4106878131D+01
llnh( 336   )= -0.4111431810D+01
llnh( 337   )= -0.4115981211D+01
llnh( 338   )= -0.4120526303D+01
llnh( 339   )= -0.4125067085D+01
llnh( 340   )= -0.4129603517D+01
llnh( 341   )= -0.4134135580D+01
llnh( 342   )= -0.4138663260D+01
llnh( 343   )= -0.4143186527D+01
llnh( 344   )= -0.4147705363D+01
llnh( 345   )= -0.4152219750D+01
llnh( 346   )= -0.4156729818D+01
llnh( 347   )= -0.4161235270D+01
llnh( 348   )= -0.4165736154D+01
llnh( 349   )= -0.4170232508D+01
llnh( 350   )= -0.4174724295D+01
llnh( 351   )= -0.4179211503D+01
llnh( 352   )= -0.4183694105D+01
llnh( 353   )= -0.4188172090D+01
llnh( 354   )= -0.4192645419D+01
llnh( 355   )= -0.4197114075D+01
llnh( 356   )= -0.4201578059D+01
llnh( 357   )= -0.4206037324D+01
llnh( 358   )= -0.4210491862D+01
llnh( 359   )= -0.4214941658D+01
llnh( 360   )= -0.4219386679D+01
llnh( 361   )= -0.4223826915D+01
llnh( 362   )= -0.4228262339D+01
llnh( 363   )= -0.4232692981D+01
llnh( 364   )= -0.4237118729D+01
llnh( 365   )= -0.4241539614D+01
llnh( 366   )= -0.4245955590D+01
llnh( 367   )= -0.4250366666D+01
llnh( 368   )= -0.4254772819D+01
llnh( 369   )= -0.4259174029D+01
llnh( 370   )= -0.4263570269D+01
llnh( 371   )= -0.4267961523D+01
llnh( 372   )= -0.4272347781D+01
llnh( 373   )= -0.4276728990D+01
llnh( 374   )= -0.4281105174D+01
llnh( 375   )= -0.4285476289D+01
llnh( 376   )= -0.4289842325D+01
llnh( 377   )= -0.4294203252D+01
llnh( 378   )= -0.4298559055D+01
llnh( 379   )= -0.4302909733D+01
llnh( 380   )= -0.4307255251D+01
llnh( 381   )= -0.4311595601D+01
llnh( 382   )= -0.4315930729D+01
llnh( 383   )= -0.4320260674D+01
llnh( 384   )= -0.4324585375D+01
llnh( 385   )= -0.4328909213D+01
llnh( 386   )= -0.4333223786D+01
llnh( 387   )= -0.4337533117D+01
llnh( 388   )= -0.4341837175D+01
llnh( 389   )= -0.4346135928D+01
llnh( 390   )= -0.4350429387D+01
llnh( 391   )= -0.4354717544D+01
llnh( 392   )= -0.4359000351D+01
llnh( 393   )= -0.4363277792D+01
llnh( 394   )= -0.4367549884D+01
llnh( 395   )= -0.4371816587D+01
llnh( 396   )= -0.4376077888D+01
llnh( 397   )= -0.4380333776D+01
llnh( 398   )= -0.4384584234D+01
llnh( 399   )= -0.4388829242D+01
llnh( 400   )= -0.4393068797D+01
llnh( 401   )= -0.4397302881D+01
llnh( 402   )= -0.4401531470D+01
llnh( 403   )= -0.4405754548D+01
llnh( 404   )= -0.4409972120D+01
llnh( 405   )= -0.4414184146D+01
llnh( 406   )= -0.4418390628D+01
llnh( 407   )= -0.4422591551D+01
llnh( 408   )= -0.4426786883D+01
llnh( 409   )= -0.4430976616D+01
llnh( 410   )= -0.4435160749D+01
llnh( 411   )= -0.4439339273D+01
llnh( 412   )= -0.4443512157D+01
llnh( 413   )= -0.4447679385D+01
llnh( 414   )= -0.4451840955D+01
llnh( 415   )= -0.4455997001D+01
llnh( 416   )= -0.4460147471D+01
llnh( 417   )= -0.4464291999D+01
llnh( 418   )= -0.4468430796D+01
llnh( 419   )= -0.4472563895D+01
llnh( 420   )= -0.4476691221D+01
llnh( 421   )= -0.4480812807D+01
llnh( 422   )= -0.4484928622D+01
llnh( 423   )= -0.4489038649D+01
llnh( 424   )= -0.4493142872D+01
llnh( 425   )= -0.4497241313D+01
llnh( 426   )= -0.4501333913D+01
llnh( 427   )= -0.4505420682D+01
llnh( 428   )= -0.4509501609D+01
llnh( 429   )= -0.4513576672D+01
llnh( 430   )= -0.4517645867D+01
llnh( 431   )= -0.4521709169D+01
llnh( 432   )= -0.4525766592D+01
llnh( 433   )= -0.4529818089D+01
llnh( 434   )= -0.4533863683D+01
llnh( 435   )= -0.4537903321D+01
llnh( 436   )= -0.4541938182D+01
llnh( 437   )= -0.4545970060D+01
llnh( 438   )= -0.4549992311D+01
llnh( 439   )= -0.4554008601D+01
llnh( 440   )= -0.4558018930D+01
llnh( 441   )= -0.4562023294D+01
llnh( 442   )= -0.4566021693D+01
llnh( 443   )= -0.4570014092D+01
llnh( 444   )= -0.4574000505D+01
llnh( 445   )= -0.4577980892D+01
llnh( 446   )= -0.4581955270D+01
llnh( 447   )= -0.4585923636D+01
llnh( 448   )= -0.4589885948D+01
llnh( 449   )= -0.4593842227D+01
llnh( 450   )= -0.4597792434D+01
llnh( 451   )= -0.4601736614D+01
llnh( 452   )= -0.4605674684D+01
llnh( 453   )= -0.4609606692D+01
llnh( 454   )= -0.4613532603D+01
llnh( 455   )= -0.4617452421D+01
llnh( 456   )= -0.4621366116D+01
llnh( 457   )= -0.4625273717D+01
llnh( 458   )= -0.4629175176D+01
llnh( 459   )= -0.4633070515D+01
llnh( 460   )= -0.4636959727D+01
llnh( 461   )= -0.4640842768D+01
llnh( 462   )= -0.4644719655D+01
llnh( 463   )= -0.4648590372D+01
llnh( 464   )= -0.4652454903D+01
llnh( 465   )= -0.4656313259D+01
llnh( 466   )= -0.4660165448D+01
llnh( 467   )= -0.4664011409D+01
llnh( 468   )= -0.4667851182D+01
llnh( 469   )= -0.4671684743D+01
llnh( 470   )= -0.4675512056D+01
llnh( 471   )= -0.4679333175D+01
llnh( 472   )= -0.4683148017D+01
llnh( 473   )= -0.4686956657D+01
llnh( 474   )= -0.4690759028D+01
llnh( 475   )= -0.4694555138D+01
llnh( 476   )= -0.4698344998D+01
llnh( 477   )= -0.4702128578D+01
llnh( 478   )= -0.4705905867D+01
llnh( 479   )= -0.4709676879D+01
llnh( 480   )= -0.4713441602D+01
llnh( 481   )= -0.4717200013D+01
llnh( 482   )= -0.4720952106D+01
llnh( 483   )= -0.4724697898D+01
llnh( 484   )= -0.4728437390D+01
llnh( 485   )= -0.4732170536D+01
llnh( 486   )= -0.4735897325D+01
llnh( 487   )= -0.4739617794D+01
llnh( 488   )= -0.4743331898D+01
llnh( 489   )= -0.4747041742D+01
llnh( 490   )= -0.4750747006D+01
llnh( 491   )= -0.4754442562D+01
llnh( 492   )= -0.4758131783D+01
llnh( 493   )= -0.4761815020D+01
llnh( 494   )= -0.4765491574D+01
llnh( 495   )= -0.4769161760D+01
llnh( 496   )= -0.4772825594D+01
llnh( 497   )= -0.4776483086D+01
llnh( 498   )= -0.4780134175D+01
llnh( 499   )= -0.4783778934D+01
llnh( 500   )= -0.4787417346D+01
llnh( 501   )= -0.4791049345D+01
llnh( 502   )= -0.4794674951D+01
llnh( 503   )= -0.4798294201D+01
llnh( 504   )= -0.4801907068D+01
llnh( 505   )= -0.4805513523D+01
llnh( 506   )= -0.4809113591D+01
llnh( 507   )= -0.4812707285D+01
llnh( 508   )= -0.4816294557D+01
llnh( 509   )= -0.4819875456D+01
llnh( 510   )= -0.4823449917D+01
llnh( 511   )= -0.4827017975D+01
llnh( 512   )= -0.4830579617D+01
llnh( 513   )= -0.4834134846D+01
llnh( 514   )= -0.4837683664D+01
llnh( 515   )= -0.4841226070D+01
llnh( 516   )= -0.4844762004D+01
llnh( 517   )= -0.4848291562D+01
llnh( 518   )= -0.4851814662D+01
llnh( 519   )= -0.4855331349D+01
llnh( 520   )= -0.4858841570D+01
llnh( 521   )= -0.4862345359D+01
llnh( 522   )= -0.4865842727D+01
llnh( 523   )= -0.4869333622D+01
llnh( 524   )= -0.4872818085D+01
llnh( 525   )= -0.4876296108D+01
llnh( 526   )= -0.4879767676D+01
llnh( 527   )= -0.4883232788D+01
llnh( 528   )= -0.4886691441D+01
llnh( 529   )= -0.4890143646D+01
llnh( 530   )= -0.4893589383D+01
llnh( 531   )= -0.4897028646D+01
llnh( 532   )= -0.4900461459D+01
llnh( 533   )= -0.4903887800D+01
llnh( 534   )= -0.4907307694D+01
llnh( 535   )= -0.4910721080D+01
llnh( 536   )= -0.4914128000D+01
llnh( 537   )= -0.4917528444D+01
llnh( 538   )= -0.4920922416D+01
llnh( 539   )= -0.4924309950D+01
llnh( 540   )= -0.4927690936D+01
llnh( 541   )= -0.4931065502D+01
llnh( 542   )= -0.4934433516D+01
llnh( 543   )= -0.4937797571D+01
llnh( 544   )= -0.4941156429D+01
llnh( 545   )= -0.4944505566D+01
llnh( 546   )= -0.4947848246D+01
llnh( 547   )= -0.4951184474D+01
llnh( 548   )= -0.4954514217D+01
llnh( 549   )= -0.4957837546D+01
llnh( 550   )= -0.4961154421D+01
llnh( 551   )= -0.4964464816D+01
llnh( 552   )= -0.4967768764D+01
llnh( 553   )= -0.4971066281D+01
llnh( 554   )= -0.4974357343D+01
llnh( 555   )= -0.4977641966D+01
llnh( 556   )= -0.4980920106D+01
llnh( 557   )= -0.4984191792D+01
llnh( 558   )= -0.4987457081D+01
llnh( 559   )= -0.4990715853D+01
llnh( 560   )= -0.4993968216D+01
llnh( 561   )= -0.4997214147D+01
llnh( 562   )= -0.5000453623D+01
llnh( 563   )= -0.5003686648D+01
llnh( 564   )= -0.5006913204D+01
llnh( 565   )= -0.5010133371D+01
llnh( 566   )= -0.5013347076D+01
llnh( 567   )= -0.5016554357D+01
llnh( 568   )= -0.5019755183D+01
llnh( 569   )= -0.5022949569D+01
llnh( 570   )= -0.5026137554D+01
llnh( 571   )= -0.5029319047D+01
llnh( 572   )= -0.5032494140D+01
llnh( 573   )= -0.5035663150D+01
llnh( 574   )= -0.5038825376D+01
llnh( 575   )= -0.5041981220D+01
llnh( 576   )= -0.5045130584D+01
llnh( 577   )= -0.5048273555D+01
llnh( 578   )= -0.5051410090D+01
llnh( 579   )= -0.5054540239D+01
llnh( 580   )= -0.5057663933D+01
llnh( 581   )= -0.5060781203D+01
llnh( 582   )= -0.5063892095D+01
llnh( 583   )= -0.5066996544D+01
llnh( 584   )= -0.5070094560D+01
llnh( 585   )= -0.5073186208D+01
llnh( 586   )= -0.5076271426D+01
llnh( 587   )= -0.5079350236D+01
llnh( 588   )= -0.5082422642D+01
llnh( 589   )= -0.5085488664D+01
llnh( 590   )= -0.5088548256D+01
llnh( 591   )= -0.5091601476D+01
llnh( 592   )= -0.5094648290D+01
llnh( 593   )= -0.5097688683D+01
llnh( 594   )= -0.5100722685D+01
llnh( 595   )= -0.5103750327D+01
llnh( 596   )= -0.5106771540D+01
llnh( 597   )= -0.5109786384D+01
llnh( 598   )= -0.5112799670D+01
llnh( 599   )= -0.5115803150D+01
llnh( 600   )= -0.5118799395D+01
llnh( 601   )= -0.5121789292D+01
llnh( 602   )= -0.5124772858D+01
llnh( 603   )= -0.5127750116D+01
llnh( 604   )= -0.5130721005D+01
llnh( 605   )= -0.5133685604D+01
llnh( 606   )= -0.5136643859D+01
llnh( 607   )= -0.5139595762D+01
llnh( 608   )= -0.5142541391D+01
llnh( 609   )= -0.5145480724D+01
llnh( 610   )= -0.5148413704D+01
llnh( 611   )= -0.5151340435D+01
llnh( 612   )= -0.5154260814D+01
llnh( 613   )= -0.5157174876D+01
llnh( 614   )= -0.5160082667D+01
llnh( 615   )= -0.5162984187D+01
llnh( 616   )= -0.5165879425D+01
llnh( 617   )= -0.5168768373D+01
llnh( 618   )= -0.5171651043D+01
llnh( 619   )= -0.5174527467D+01
llnh( 620   )= -0.5177397602D+01
llnh( 621   )= -0.5180261493D+01
llnh( 622   )= -0.5183119109D+01
llnh( 623   )= -0.5185970497D+01
llnh( 624   )= -0.5188815635D+01
llnh( 625   )= -0.5191654520D+01
llnh( 626   )= -0.5194487149D+01
llnh( 627   )= -0.5197313595D+01
llnh( 628   )= -0.5200133751D+01
llnh( 629   )= -0.5202947738D+01
llnh( 630   )= -0.5205755473D+01
llnh( 631   )= -0.5208556983D+01
llnh( 632   )= -0.5211352324D+01
llnh( 633   )= -0.5214141419D+01
llnh( 634   )= -0.5216924343D+01
llnh( 635   )= -0.5219701054D+01
llnh( 636   )= -0.5222471563D+01
llnh( 637   )= -0.5225235884D+01
llnh( 638   )= -0.5227994025D+01
llnh( 639   )= -0.5230746030D+01
llnh( 640   )= -0.5233491832D+01
llnh( 641   )= -0.5236231461D+01
llnh( 642   )= -0.5238964942D+01
llnh( 643   )= -0.5241692271D+01
llnh( 644   )= -0.5244413412D+01
llnh( 645   )= -0.5247128424D+01
llnh( 646   )= -0.5249837269D+01
llnh( 647   )= -0.5252540015D+01
llnh( 648   )= -0.5255236616D+01
llnh( 649   )= -0.5257927067D+01
llnh( 650   )= -0.5260611388D+01
llnh( 651   )= -0.5263289623D+01
llnh( 652   )= -0.5265961712D+01
llnh( 653   )= -0.5268633068D+01
llnh( 654   )= -0.5271293569D+01
llnh( 655   )= -0.5273947990D+01
llnh( 656   )= -0.5276596206D+01
llnh( 657   )= -0.5279238426D+01
llnh( 658   )= -0.5281874571D+01
llnh( 659   )= -0.5284504655D+01
llnh( 660   )= -0.5287128701D+01
llnh( 661   )= -0.5289746751D+01
llnh( 662   )= -0.5292358757D+01
llnh( 663   )= -0.5294964725D+01
llnh( 664   )= -0.5297564727D+01
llnh( 665   )= -0.5300158699D+01
llnh( 666   )= -0.5302746668D+01
llnh( 667   )= -0.5305328668D+01
llnh( 668   )= -0.5307904687D+01
llnh( 669   )= -0.5310474675D+01
llnh( 670   )= -0.5313038743D+01
llnh( 671   )= -0.5315596857D+01
llnh( 672   )= -0.5318148939D+01
llnh( 673   )= -0.5320695164D+01
llnh( 674   )= -0.5323235360D+01
llnh( 675   )= -0.5325769682D+01
llnh( 676   )= -0.5328298064D+01
llnh( 677   )= -0.5330820487D+01
llnh( 678   )= -0.5333337033D+01
llnh( 679   )= -0.5335847660D+01
llnh( 680   )= -0.5338352356D+01
llnh( 681   )= -0.5340851158D+01
llnh( 682   )= -0.5343344088D+01
llnh( 683   )= -0.5345831113D+01
llnh( 684   )= -0.5348312329D+01
llnh( 685   )= -0.5350787595D+01
llnh( 686   )= -0.5353257028D+01
llnh( 687   )= -0.5355720597D+01
llnh( 688   )= -0.5358178333D+01
llnh( 689   )= -0.5360630227D+01
llnh( 690   )= -0.5363076250D+01
llnh( 691   )= -0.5365516440D+01
llnh( 692   )= -0.5367950847D+01
llnh( 693   )= -0.5370379412D+01
llnh( 694   )= -0.5372802182D+01
llnh( 695   )= -0.5375219136D+01
llnh( 696   )= -0.5377630301D+01
llnh( 697   )= -0.5380035657D+01
llnh( 698   )= -0.5382435244D+01
llnh( 699   )= -0.5384829094D+01
llnh( 700   )= -0.5387217124D+01
llnh( 701   )= -0.5389599434D+01
llnh( 702   )= -0.5391975954D+01
llnh( 703   )= -0.5394346756D+01
llnh( 704   )= -0.5396711759D+01
llnh( 705   )= -0.5399071078D+01
llnh( 706   )= -0.5401424638D+01
llnh( 707   )= -0.5403772479D+01
llnh( 708   )= -0.5406119229D+01
llnh( 709   )= -0.5408456158D+01
llnh( 710   )= -0.5410787342D+01
llnh( 711   )= -0.5413112829D+01
llnh( 712   )= -0.5415432725D+01
llnh( 713   )= -0.5417746886D+01
llnh( 714   )= -0.5420055451D+01
llnh( 715   )= -0.5422358379D+01
llnh( 716   )= -0.5424655695D+01
llnh( 717   )= -0.5426947346D+01
llnh( 718   )= -0.5429233415D+01
llnh( 719   )= -0.5431513881D+01
llnh( 720   )= -0.5433788773D+01
llnh( 721   )= -0.5436058059D+01
llnh( 722   )= -0.5438321777D+01
llnh( 723   )= -0.5440579909D+01
llnh( 724   )= -0.5442832493D+01
llnh( 725   )= -0.5445079538D+01
llnh( 726   )= -0.5447321025D+01
llnh( 727   )= -0.5449556959D+01
llnh( 728   )= -0.5451787383D+01
llnh( 729   )= -0.5454012244D+01
llnh( 730   )= -0.5456231645D+01
llnh( 731   )= -0.5458445490D+01
llnh( 732   )= -0.5460653885D+01
llnh( 733   )= -0.5462856815D+01
llnh( 734   )= -0.5465054208D+01
llnh( 735   )= -0.5467246209D+01
llnh( 736   )= -0.5469432635D+01
llnh( 737   )= -0.5471613649D+01
llnh( 738   )= -0.5473789425D+01
llnh( 739   )= -0.5475959565D+01
llnh( 740   )= -0.5478124308D+01
llnh( 741   )= -0.5480283586D+01
llnh( 742   )= -0.5482437461D+01
llnh( 743   )= -0.5484585919D+01
llnh( 744   )= -0.5486728976D+01
llnh( 745   )= -0.5488866669D+01
llnh( 746   )= -0.5490998937D+01
llnh( 747   )= -0.5493125867D+01
llnh( 748   )= -0.5495247398D+01
llnh( 749   )= -0.5497363593D+01
llnh( 750   )= -0.5499474396D+01
llnh( 751   )= -0.5501579935D+01
llnh( 752   )= -0.5503680091D+01
llnh( 753   )= -0.5505774872D+01
llnh( 754   )= -0.5507864419D+01
llnh( 755   )= -0.5509948597D+01
llnh( 756   )= -0.5512027498D+01
llnh( 757   )= -0.5514101090D+01
llnh( 758   )= -0.5516169407D+01
llnh( 759   )= -0.5518232431D+01
llnh( 760   )= -0.5520290171D+01
llnh( 761   )= -0.5522342671D+01
llnh( 762   )= -0.5524389920D+01
llnh( 763   )= -0.5526435070D+01
llnh( 764   )= -0.5528472978D+01
llnh( 765   )= -0.5530504862D+01
llnh( 766   )= -0.5532531569D+01
llnh( 767   )= -0.5534553070D+01
llnh( 768   )= -0.5536569379D+01
llnh( 769   )= -0.5538580560D+01
llnh( 770   )= -0.5540586509D+01
llnh( 771   )= -0.5542587346D+01
llnh( 772   )= -0.5544583018D+01
llnh( 773   )= -0.5546573556D+01
llnh( 774   )= -0.5548558957D+01
llnh( 775   )= -0.5550539231D+01
llnh( 776   )= -0.5552514398D+01
llnh( 777   )= -0.5554484460D+01
llnh( 778   )= -0.5556449473D+01
llnh( 779   )= -0.5558409336D+01
llnh( 780   )= -0.5560364185D+01
llnh( 781   )= -0.5562313913D+01
llnh( 782   )= -0.5564258612D+01
llnh( 783   )= -0.5566198240D+01
llnh( 784   )= -0.5568132825D+01
llnh( 785   )= -0.5570062431D+01
llnh( 786   )= -0.5571986955D+01
llnh( 787   )= -0.5573906479D+01
llnh( 788   )= -0.5575821017D+01
llnh( 789   )= -0.5577730521D+01
llnh( 790   )= -0.5579635046D+01
llnh( 791   )= -0.5581534623D+01
llnh( 792   )= -0.5583429246D+01
llnh( 793   )= -0.5585318843D+01
llnh( 794   )= -0.5587203534D+01
llnh( 795   )= -0.5589083215D+01
llnh( 796   )= -0.5590958026D+01
llnh( 797   )= -0.5592827913D+01
llnh( 798   )= -0.5594692837D+01
llnh( 799   )= -0.5596552884D+01
llnh( 800   )= -0.5598408013D+01
llnh( 801   )= -0.5600258261D+01
llnh( 802   )= -0.5602103603D+01
llnh( 803   )= -0.5603944084D+01
llnh( 804   )= -0.5605779692D+01
llnh( 805   )= -0.5607610469D+01
llnh( 806   )= -0.5609436368D+01
llnh( 807   )= -0.5611257443D+01
llnh( 808   )= -0.5613073624D+01
llnh( 809   )= -0.5614885059D+01
llnh( 810   )= -0.5616691617D+01
llnh( 811   )= -0.5618493398D+01
llnh( 812   )= -0.5620290369D+01
llnh( 813   )= -0.5622082588D+01
llnh( 814   )= -0.5623870004D+01
llnh( 815   )= -0.5625652635D+01
llnh( 816   )= -0.5627430515D+01
llnh( 817   )= -0.5629203619D+01
llnh( 818   )= -0.5630973564D+01
llnh( 819   )= -0.5632739215D+01
llnh( 820   )= -0.5634498385D+01
llnh( 821   )= -0.5636252874D+01
llnh( 822   )= -0.5638002692D+01
llnh( 823   )= -0.5639747990D+01
llnh( 824   )= -0.5641488433D+01
llnh( 825   )= -0.5643224238D+01
llnh( 826   )= -0.5644955371D+01
llnh( 827   )= -0.5646681860D+01
llnh( 828   )= -0.5648403688D+01
llnh( 829   )= -0.5650120894D+01
llnh( 830   )= -0.5651833499D+01
llnh( 831   )= -0.5653541488D+01
llnh( 832   )= -0.5655244889D+01
llnh( 833   )= -0.5656943648D+01
llnh( 834   )= -0.5658637872D+01
llnh( 835   )= -0.5660327454D+01
llnh( 836   )= -0.5662012541D+01
llnh( 837   )= -0.5663693098D+01
llnh( 838   )= -0.5665369003D+01
llnh( 839   )= -0.5667040432D+01
llnh( 840   )= -0.5668707291D+01
llnh( 841   )= -0.5670369673D+01
llnh( 842   )= -0.5672027532D+01
llnh( 843   )= -0.5673680874D+01
llnh( 844   )= -0.5675329711D+01
llnh( 845   )= -0.5676974102D+01
llnh( 846   )= -0.5678614009D+01
llnh( 847   )= -0.5680249399D+01
llnh( 848   )= -0.5681880401D+01
llnh( 849   )= -0.5683506937D+01
llnh( 850   )= -0.5685128983D+01
llnh( 851   )= -0.5686746614D+01
llnh( 852   )= -0.5688359836D+01
llnh( 853   )= -0.5689968657D+01
llnh( 854   )= -0.5691573040D+01
llnh( 855   )= -0.5693173037D+01
llnh( 856   )= -0.5694768667D+01
llnh( 857   )= -0.5696359900D+01
llnh( 858   )= -0.5697946740D+01
llnh( 859   )= -0.5699529279D+01
llnh( 860   )= -0.5701107407D+01
llnh( 861   )= -0.5702681211D+01
llnh( 862   )= -0.5704250662D+01
llnh( 863   )= -0.5705815820D+01
llnh( 864   )= -0.5707376645D+01
llnh( 865   )= -0.5708933156D+01
llnh( 866   )= -0.5710485372D+01
llnh( 867   )= -0.5712033286D+01
llnh( 868   )= -0.5713576887D+01
llnh( 869   )= -0.5715116238D+01
llnh( 870   )= -0.5716651343D+01
llnh( 871   )= -0.5718182133D+01
llnh( 872   )= -0.5719708718D+01
llnh( 873   )= -0.5721231370D+01
llnh( 874   )= -0.5722752079D+01
llnh( 875   )= -0.5724266184D+01
llnh( 876   )= -0.5725776091D+01
llnh( 877   )= -0.5727281862D+01
llnh( 878   )= -0.5728783383D+01
llnh( 879   )= -0.5730280793D+01
llnh( 880   )= -0.5731774031D+01
llnh( 881   )= -0.5733263026D+01
llnh( 882   )= -0.5734747963D+01
llnh( 883   )= -0.5736228729D+01
llnh( 884   )= -0.5737705360D+01
llnh( 885   )= -0.5739177941D+01
llnh( 886   )= -0.5740646299D+01
llnh( 887   )= -0.5742110595D+01
llnh( 888   )= -0.5743570865D+01
llnh( 889   )= -0.5745026960D+01
llnh( 890   )= -0.5746479033D+01
llnh( 891   )= -0.5747927018D+01
llnh( 892   )= -0.5749370943D+01
llnh( 893   )= -0.5750810827D+01
llnh( 894   )= -0.5752246701D+01
llnh( 895   )= -0.5753678496D+01
llnh( 896   )= -0.5755106289D+01
llnh( 897   )= -0.5756530074D+01
llnh( 898   )= -0.5757949836D+01
llnh( 899   )= -0.5759365629D+01
llnh( 900   )= -0.5760777429D+01
llnh( 901   )= -0.5762185269D+01
llnh( 902   )= -0.5763589050D+01
llnh( 903   )= -0.5764989015D+01
llnh( 904   )= -0.5766384906D+01
llnh( 905   )= -0.5767776906D+01
llnh( 906   )= -0.5769164942D+01
llnh( 907   )= -0.5770549045D+01
llnh( 908   )= -0.5771929251D+01
llnh( 909   )= -0.5773305592D+01
llnh( 910   )= -0.5774678021D+01
llnh( 911   )= -0.5776046522D+01
llnh( 912   )= -0.5777411181D+01
llnh( 913   )= -0.5778771903D+01
llnh( 914   )= -0.5780128791D+01
llnh( 915   )= -0.5781481778D+01
llnh( 916   )= -0.5782830973D+01
llnh( 917   )= -0.5784176296D+01
llnh( 918   )= -0.5785517731D+01
llnh( 919   )= -0.5786855440D+01
llnh( 920   )= -0.5788189236D+01
llnh( 921   )= -0.5789519318D+01
llnh( 922   )= -0.5790845543D+01
llnh( 923   )= -0.5792167984D+01
llnh( 924   )= -0.5793486673D+01
llnh( 925   )= -0.5794801520D+01
llnh( 926   )= -0.5796112641D+01
llnh( 927   )= -0.5797420004D+01
llnh( 928   )= -0.5798723616D+01
llnh( 929   )= -0.5800025860D+01
llnh( 930   )= -0.5801322181D+01
llnh( 931   )= -0.5802614805D+01
llnh( 932   )= -0.5803903742D+01
llnh( 933   )= -0.5805188958D+01
llnh( 934   )= -0.5806470481D+01
llnh( 935   )= -0.5807748369D+01
llnh( 936   )= -0.5809022517D+01
llnh( 937   )= -0.5810293038D+01
llnh( 938   )= -0.5811559908D+01
llnh( 939   )= -0.5812823154D+01
llnh( 940   )= -0.5814082757D+01
llnh( 941   )= -0.5815338711D+01
llnh( 942   )= -0.5816591047D+01
llnh( 943   )= -0.5817839765D+01
llnh( 944   )= -0.5819084925D+01
llnh( 945   )= -0.5820326443D+01
llnh( 946   )= -0.5821564407D+01
llnh( 947   )= -0.5822798792D+01
llnh( 948   )= -0.5824029580D+01
llnh( 949   )= -0.5825256834D+01
llnh( 950   )= -0.5826480495D+01
llnh( 951   )= -0.5827700679D+01
llnh( 952   )= -0.5828917318D+01
llnh( 953   )= -0.5830130356D+01
llnh( 954   )= -0.5831339955D+01
llnh( 955   )= -0.5832546036D+01
llnh( 956   )= -0.5833748572D+01
llnh( 957   )= -0.5834947646D+01
llnh( 958   )= -0.5836143208D+01
llnh( 959   )= -0.5837335326D+01
llnh( 960   )= -0.5838523975D+01
llnh( 961   )= -0.5839709120D+01
llnh( 962   )= -0.5840890822D+01
llnh( 963   )= -0.5842069117D+01
llnh( 964   )= -0.5843243948D+01
llnh( 965   )= -0.5844415380D+01
llnh( 966   )= -0.5845583345D+01
llnh( 967   )= -0.5846747902D+01
llnh( 968   )= -0.5847909107D+01
llnh( 969   )= -0.5849066883D+01
llnh( 970   )= -0.5850221245D+01
llnh( 971   )= -0.5851372233D+01
llnh( 972   )= -0.5852519933D+01
llnh( 973   )= -0.5853664111D+01
llnh( 974   )= -0.5854805079D+01
llnh( 975   )= -0.5855942632D+01
llnh( 976   )= -0.5857076840D+01
llnh( 977   )= -0.5858207705D+01
llnh( 978   )= -0.5859335263D+01
llnh( 979   )= -0.5860459528D+01
llnh( 980   )= -0.5861580446D+01
llnh( 981   )= -0.5862698053D+01
llnh( 982   )= -0.5863812452D+01
llnh( 983   )= -0.5864923445D+01
llnh( 984   )= -0.5866033059D+01
llnh( 985   )= -0.5867137708D+01
llnh( 986   )= -0.5868239098D+01
llnh( 987   )= -0.5869337290D+01
llnh( 988   )= -0.5870432164D+01
llnh( 989   )= -0.5871523895D+01
llnh( 990   )= -0.5872612338D+01
llnh( 991   )= -0.5873697645D+01
llnh( 992   )= -0.5874779683D+01
llnh( 993   )= -0.5875858528D+01
llnh( 994   )= -0.5876934214D+01
llnh( 995   )= -0.5878006676D+01
llnh( 996   )= -0.5879076002D+01
llnh( 997   )= -0.5880142202D+01
llnh( 998   )= -0.5881205128D+01
llnh( 999   )= -0.5882264991D+01
llnh( 1000  )= -0.5883321683D+01
llnh( 1001  )= -0.5884375272D+01
llnh( 1002  )= -0.5885425704D+01
llnh( 1003  )= -0.5886473023D+01
llnh( 1004  )= -0.5887517204D+01
llnh( 1005  )= -0.5888558321D+01
llnh( 1006  )= -0.5889596323D+01
llnh( 1007  )= -0.5890631250D+01
llnh( 1008  )= -0.5891663079D+01
llnh( 1009  )= -0.5892691857D+01
llnh( 1010  )= -0.5893717531D+01
llnh( 1011  )= -0.5894740186D+01
llnh( 1012  )= -0.5895759785D+01
llnh( 1013  )= -0.5896776265D+01
llnh( 1014  )= -0.5897789840D+01
llnh( 1015  )= -0.5898800343D+01
llnh( 1016  )= -0.5899807814D+01
llnh( 1017  )= -0.5900812292D+01
llnh( 1018  )= -0.5901813724D+01
llnh( 1019  )= -0.5902812170D+01
llnh( 1020  )= -0.5903807651D+01
llnh( 1021  )= -0.5904800116D+01
llnh( 1022  )= -0.5905789619D+01
llnh( 1023  )= -0.5906776200D+01
llnh( 1024  )= -0.5907759788D+01

llnk( 0  )= -1.0000000000D+00
llnk( 1  )= -0.1000000000D+01
llnk( 2  )= -0.3054020195D+00
llnk( 3  )= -0.1960294041D+00
llnk( 4  )= -0.1336652689D+00
llnk( 5  )= -0.1047066267D+00
llnk( 6  )= -0.9033564429D-01
llnk( 7  )= -0.8206984804D-01
llnk( 8  )= -0.7655494644D-01
llnk( 9  )= -0.7243844815D-01
llnk( 10 )= -0.6913401466D-01
llnk( 11 )= -0.6635869819D-01
llnk( 12 )= -0.6395689877D-01
llnk( 13 )= -0.6183296641D-01
llnk( 14 )= -0.5992172201D-01
llnk( 15 )= -0.5817772516D-01
llnk( 16 )= -0.5656704205D-01
llnk( 17 )= -0.5506474110D-01
llnk( 18 )= -0.5365205058D-01
llnk( 19 )= -0.5231479422D-01
llnk( 20 )= -0.5104204279D-01
llnk( 21 )= -0.4982562670D-01
llnk( 22 )= -0.4865919131D-01
llnk( 23 )= -0.4753764652D-01
llnk( 24 )= -0.4645728556D-01
llnk( 25 )= -0.4541483359D-01
llnk( 26 )= -0.4440818528D-01
llnk( 27 )= -0.4343487603D-01
llnk( 28 )= -0.4249347823D-01
llnk( 29 )= -0.4158232061D-01
llnk( 30 )= -0.4070034557D-01
llnk( 31 )= -0.3984645832D-01
llnk( 32 )= -0.3901937759D-01
llnk( 33 )= -0.3821827509D-01
llnk( 34 )= -0.3744217649D-01
llnk( 35 )= -0.3669034206D-01
llnk( 36 )= -0.3596186474D-01
llnk( 37 )= -0.3525594412D-01
llnk( 38 )= -0.3457187576D-01
llnk( 39 )= -0.3390882631D-01
llnk( 40 )= -0.3326609909D-01
llnk( 41 )= -0.3264299923D-01
llnk( 42 )= -0.3203883174D-01
llnk( 43 )= -0.3145293280D-01
llnk( 44 )= -0.3088463896D-01
llnk( 45 )= -0.3033334191D-01
llnk( 46 )= -0.2979842108D-01
llnk( 47 )= -0.2927929373D-01
llnk( 48 )= -0.2877539004D-01
llnk( 49 )= -0.2828615868D-01
llnk( 50 )= -0.2781108192D-01
llnk( 51 )= -0.2734963353D-01
llnk( 52 )= -0.2690133637D-01
llnk( 53 )= -0.2646570945D-01
llnk( 54 )= -0.2604229418D-01
llnk( 55 )= -0.2563066630D-01
llnk( 56 )= -0.2523039452D-01
llnk( 57 )= -0.2484107774D-01
llnk( 58 )= -0.2446233127D-01
llnk( 59 )= -0.2409377795D-01
llnk( 60 )= -0.2373506405D-01
llnk( 61 )= -0.2338584530D-01
llnk( 62 )= -0.2304579309D-01
llnk( 63 )= -0.2271459030D-01
llnk( 64 )= -0.2239193633D-01
llnk( 65 )= -0.2207753919D-01
llnk( 66 )= -0.2177111937D-01
llnk( 67 )= -0.2147240908D-01
llnk( 68 )= -0.2118115170D-01
llnk( 69 )= -0.2089710139D-01
llnk( 70 )= -0.2062002059D-01
llnk( 71 )= -0.2034968220D-01
llnk( 72 )= -0.2008586807D-01
llnk( 73 )= -0.1982836895D-01
llnk( 74 )= -0.1957698319D-01
llnk( 75 )= -0.1933151728D-01
llnk( 76 )= -0.1909178544D-01
llnk( 77 )= -0.1885760922D-01
llnk( 78 )= -0.1862881706D-01
llnk( 79 )= -0.1840524275D-01
llnk( 80 )= -0.1818672781D-01
llnk( 81 )= -0.1797312161D-01
llnk( 82 )= -0.1776427277D-01
llnk( 83 )= -0.1756004164D-01
llnk( 84 )= -0.1736029175D-01
llnk( 85 )= -0.1716489163D-01
llnk( 86 )= -0.1697371498D-01
llnk( 87 )= -0.1678663942D-01
llnk( 88 )= -0.1660354744D-01
llnk( 89 )= -0.1642432560D-01
llnk( 90 )= -0.1624886478D-01
llnk( 91 )= -0.1607705911D-01
llnk( 92 )= -0.1590880688D-01
llnk( 93 )= -0.1574400976D-01
llnk( 94 )= -0.1558257306D-01
llnk( 95 )= -0.1542440486D-01
llnk( 96 )= -0.1526941673D-01
llnk( 97 )= -0.1511752309D-01
llnk( 98 )= -0.1496864146D-01
llnk( 99 )= -0.1482269168D-01
llnk( 100   )= -0.1467959656D-01
llnk( 101   )= -0.1453928135D-01
llnk( 102   )= -0.1440167387D-01
llnk( 103   )= -0.1426670401D-01
llnk( 104   )= -0.1413430411D-01
llnk( 105   )= -0.1400440861D-01
llnk( 106   )= -0.1387695416D-01
llnk( 107   )= -0.1375187918D-01
llnk( 108   )= -0.1362912416D-01
llnk( 109   )= -0.1350863142D-01
llnk( 110   )= -0.1339034516D-01
llnk( 111   )= -0.1327421107D-01
llnk( 112   )= -0.1316017669D-01
llnk( 113   )= -0.1304819106D-01
llnk( 114   )= -0.1293820488D-01
llnk( 115   )= -0.1283017014D-01
llnk( 116   )= -0.1272404038D-01
llnk( 117   )= -0.1261977050D-01
llnk( 118   )= -0.1251731678D-01
llnk( 119   )= -0.1241663664D-01
llnk( 120   )= -0.1231768886D-01
llnk( 121   )= -0.1222043336D-01
llnk( 122   )= -0.1212483127D-01
llnk( 123   )= -0.1203084476D-01
llnk( 124   )= -0.1193843710D-01
llnk( 125   )= -0.1184757260D-01
llnk( 126   )= -0.1175821983D-01
llnk( 127   )= -0.1167033892D-01
llnk( 128   )= -0.1158389997D-01
llnk( 129   )= -0.1149887118D-01
llnk( 130   )= -0.1141522155D-01
llnk( 131   )= -0.1133292103D-01
llnk( 132   )= -0.1125194019D-01
llnk( 133   )= -0.1117225053D-01
llnk( 134   )= -0.1109382428D-01
llnk( 135   )= -0.1101663454D-01
llnk( 136   )= -0.1094065488D-01
llnk( 137   )= -0.1086585976D-01
llnk( 138   )= -0.1079222424D-01
llnk( 139   )= -0.1071972412D-01
llnk( 140   )= -0.1064833570D-01
llnk( 141   )= -0.1057803597D-01
llnk( 142   )= -0.1050880250D-01
llnk( 143   )= -0.1044061352D-01
llnk( 144   )= -0.1037344765D-01
llnk( 145   )= -0.1030728418D-01
llnk( 146   )= -0.1024210288D-01
llnk( 147   )= -0.1017788410D-01
llnk( 148   )= -0.1011460854D-01
llnk( 149   )= -0.1005225749D-01
llnk( 150   )= -0.9990812687D-02
llnk( 151   )= -0.9930256346D-02
llnk( 152   )= -0.9870571027D-02
llnk( 153   )= -0.9811739803D-02
llnk( 154   )= -0.9753746115D-02
llnk( 155   )= -0.9696573861D-02
llnk( 156   )= -0.9640207245D-02
llnk( 157   )= -0.9584630905D-02
llnk( 158   )= -0.9529829829D-02
llnk( 159   )= -0.9475790189D-02
llnk( 160   )= -0.9422496073D-02
llnk( 161   )= -0.9369934305D-02
llnk( 162   )= -0.9318091242D-02
llnk( 163   )= -0.9266953580D-02
llnk( 164   )= -0.9216508278D-02
llnk( 165   )= -0.9166742633D-02
llnk( 166   )= -0.9117644217D-02
llnk( 167   )= -0.9069200890D-02
llnk( 168   )= -0.9021400853D-02
llnk( 169   )= -0.8974232435D-02
llnk( 170   )= -0.8927684326D-02
llnk( 171   )= -0.8881745447D-02
llnk( 172   )= -0.8836405029D-02
llnk( 173   )= -0.8791653407D-02
llnk( 174   )= -0.8747481667D-02
llnk( 175   )= -0.8703874181D-02
llnk( 176   )= -0.8660824198D-02
llnk( 177   )= -0.8618321971D-02
llnk( 178   )= -0.8576358051D-02
llnk( 179   )= -0.8534923154D-02
llnk( 180   )= -0.8494008256D-02
llnk( 181   )= -0.8453604422D-02
llnk( 182   )= -0.8413702992D-02
llnk( 183   )= -0.8374295452D-02
llnk( 184   )= -0.8335373508D-02
llnk( 185   )= -0.8296928978D-02
llnk( 186   )= -0.8258953895D-02
llnk( 187   )= -0.8221440446D-02
llnk( 188   )= -0.8184381010D-02
llnk( 189   )= -0.8147768058D-02
llnk( 190   )= -0.8111594272D-02
llnk( 191   )= -0.8075852456D-02
llnk( 192   )= -0.8040535598D-02
llnk( 193   )= -0.8005636771D-02
llnk( 194   )= -0.7971149225D-02
llnk( 195   )= -0.7937066335D-02
llnk( 196   )= -0.7903381636D-02
llnk( 197   )= -0.7870088750D-02
llnk( 198   )= -0.7837181442D-02
llnk( 199   )= -0.7804653597D-02
llnk( 200   )= -0.7772499254D-02
llnk( 201   )= -0.7740712516D-02
llnk( 202   )= -0.7709287634D-02
llnk( 203   )= -0.7678218959D-02
llnk( 204   )= -0.7647500968D-02
llnk( 205   )= -0.7617128214D-02
llnk( 206   )= -0.7587095379D-02
llnk( 207   )= -0.7557397242D-02
llnk( 208   )= -0.7528028665D-02
llnk( 209   )= -0.7498984668D-02
llnk( 210   )= -0.7470260263D-02
llnk( 211   )= -0.7441850636D-02
llnk( 212   )= -0.7413751027D-02
llnk( 213   )= -0.7385956807D-02
llnk( 214   )= -0.7358463369D-02
llnk( 215   )= -0.7331266234D-02
llnk( 216   )= -0.7304360991D-02
llnk( 217   )= -0.7277743343D-02
llnk( 218   )= -0.7251409005D-02
llnk( 219   )= -0.7225353835D-02
llnk( 220   )= -0.7199573711D-02
llnk( 221   )= -0.7174064656D-02
llnk( 222   )= -0.7148822685D-02
llnk( 223   )= -0.7123843938D-02
llnk( 224   )= -0.7099129566D-02
llnk( 225   )= -0.7074666551D-02
llnk( 226   )= -0.7050455306D-02
llnk( 227   )= -0.7026492490D-02
llnk( 228   )= -0.7002774540D-02
llnk( 229   )= -0.6979298005D-02
llnk( 230   )= -0.6956059431D-02
llnk( 231   )= -0.6933055469D-02
llnk( 232   )= -0.6910282814D-02
llnk( 233   )= -0.6887738228D-02
llnk( 234   )= -0.6865418504D-02
llnk( 235   )= -0.6843320529D-02
llnk( 236   )= -0.6821441187D-02
llnk( 237   )= -0.6799777491D-02
llnk( 238   )= -0.6778326425D-02
llnk( 239   )= -0.6757085071D-02
llnk( 240   )= -0.6736050549D-02
llnk( 241   )= -0.6715220049D-02
llnk( 242   )= -0.6694590762D-02
llnk( 243   )= -0.6674159952D-02
llnk( 244   )= -0.6653925199D-02
llnk( 245   )= -0.6633883871D-02
llnk( 246   )= -0.6614032563D-02
llnk( 247   )= -0.6594369232D-02
llnk( 248   )= -0.6574891356D-02
llnk( 249   )= -0.6555596461D-02
llnk( 250   )= -0.6536482218D-02
llnk( 251   )= -0.6517546015D-02
llnk( 252   )= -0.6498785614D-02
llnk( 253   )= -0.6480198683D-02
llnk( 254   )= -0.6461782835D-02
llnk( 255   )= -0.6443535971D-02
llnk( 256   )= -0.6425455846D-02
llnk( 257   )= -0.6407540263D-02
llnk( 258   )= -0.6389786969D-02
llnk( 259   )= -0.6372194033D-02
llnk( 260   )= -0.6354759310D-02
llnk( 261   )= -0.6337480778D-02
llnk( 262   )= -0.6320356275D-02
llnk( 263   )= -0.6303384010D-02
llnk( 264   )= -0.6286561980D-02
llnk( 265   )= -0.6269888285D-02
llnk( 266   )= -0.6253360871D-02
llnk( 267   )= -0.6236978078D-02
llnk( 268   )= -0.6220737994D-02
llnk( 269   )= -0.6204638851D-02
llnk( 270   )= -0.6188678733D-02
llnk( 271   )= -0.6172856062D-02
llnk( 272   )= -0.6157169047D-02
llnk( 273   )= -0.6141616004D-02
llnk( 274   )= -0.6126195168D-02
llnk( 275   )= -0.6110905001D-02
llnk( 276   )= -0.6095743854D-02
llnk( 277   )= -0.6080714363D-02
llnk( 278   )= -0.6065808661D-02
llnk( 279   )= -0.6051025499D-02
llnk( 280   )= -0.6036365150D-02
llnk( 281   )= -0.6021826113D-02
llnk( 282   )= -0.6007406839D-02
llnk( 283   )= -0.5993105878D-02
llnk( 284   )= -0.5978921847D-02
llnk( 285   )= -0.5964853301D-02
llnk( 286   )= -0.5950898825D-02
llnk( 287   )= -0.5937057006D-02
llnk( 288   )= -0.5923326579D-02
llnk( 289   )= -0.5909706151D-02
llnk( 290   )= -0.5896194416D-02
llnk( 291   )= -0.5882789995D-02
llnk( 292   )= -0.5869491718D-02
llnk( 293   )= -0.5856298304D-02
llnk( 294   )= -0.5843208448D-02
llnk( 295   )= -0.5830220904D-02
llnk( 296   )= -0.5817334546D-02
llnk( 297   )= -0.5804548161D-02
llnk( 298   )= -0.5791860577D-02
llnk( 299   )= -0.5779270522D-02
llnk( 300   )= -0.5766776986D-02
llnk( 301   )= -0.5754378816D-02
llnk( 302   )= -0.5742074899D-02
llnk( 303   )= -0.5729864025D-02
llnk( 304   )= -0.5717745257D-02
llnk( 305   )= -0.5705717481D-02
llnk( 306   )= -0.5693779662D-02
llnk( 307   )= -0.5681930652D-02
llnk( 308   )= -0.5670169557D-02
llnk( 309   )= -0.5658495321D-02
llnk( 310   )= -0.5646906981D-02
llnk( 311   )= -0.5635403426D-02
llnk( 312   )= -0.5623983856D-02
llnk( 313   )= -0.5612647214D-02
llnk( 314   )= -0.5601392619D-02
llnk( 315   )= -0.5590219039D-02
llnk( 316   )= -0.5579125640D-02
llnk( 317   )= -0.5568111507D-02
llnk( 318   )= -0.5557175755D-02
llnk( 319   )= -0.5546317422D-02
llnk( 320   )= -0.5535535712D-02
llnk( 321   )= -0.5524829768D-02
llnk( 322   )= -0.5514198739D-02
llnk( 323   )= -0.5503641734D-02
llnk( 324   )= -0.5493157962D-02
llnk( 325   )= -0.5482746632D-02
llnk( 326   )= -0.5472406939D-02
llnk( 327   )= -0.5462138054D-02
llnk( 328   )= -0.5451939170D-02
llnk( 329   )= -0.5441809594D-02
llnk( 330   )= -0.5431748532D-02
llnk( 331   )= -0.5421755212D-02
llnk( 332   )= -0.5411828857D-02
llnk( 333   )= -0.5401975569D-02
llnk( 334   )= -0.5392181719D-02
llnk( 335   )= -0.5382453176D-02
llnk( 336   )= -0.5372788210D-02
llnk( 337   )= -0.5363186748D-02
llnk( 338   )= -0.5353648059D-02
llnk( 339   )= -0.5344171504D-02
llnk( 340   )= -0.5334756310D-02
llnk( 341   )= -0.5325401926D-02
llnk( 342   )= -0.5316107680D-02
llnk( 343   )= -0.5306872920D-02
llnk( 344   )= -0.5297696936D-02
llnk( 345   )= -0.5288579215D-02
llnk( 346   )= -0.5279519432D-02
llnk( 347   )= -0.5270516389D-02
llnk( 348   )= -0.5261569543D-02
llnk( 349   )= -0.5252678535D-02
llnk( 350   )= -0.5243842709D-02
llnk( 351   )= -0.5235061516D-02
llnk( 352   )= -0.5226334273D-02
llnk( 353   )= -0.5217660524D-02
llnk( 354   )= -0.5209039665D-02
llnk( 355   )= -0.5200471169D-02
llnk( 356   )= -0.5191954431D-02
llnk( 357   )= -0.5183488963D-02
llnk( 358   )= -0.5175074221D-02
llnk( 359   )= -0.5166709698D-02
llnk( 360   )= -0.5158394787D-02
llnk( 361   )= -0.5150129056D-02
llnk( 362   )= -0.5141911977D-02
llnk( 363   )= -0.5133743148D-02
llnk( 364   )= -0.5125621837D-02
llnk( 365   )= -0.5117547698D-02
llnk( 366   )= -0.5109520206D-02
llnk( 367   )= -0.5101538932D-02
llnk( 368   )= -0.5093603358D-02
llnk( 369   )= -0.5085713032D-02
llnk( 370   )= -0.5077867507D-02
llnk( 371   )= -0.5070066320D-02
llnk( 372   )= -0.5062309014D-02
llnk( 373   )= -0.5054595077D-02
llnk( 374   )= -0.5046924184D-02
llnk( 375   )= -0.5039295844D-02
llnk( 376   )= -0.5031709627D-02
llnk( 377   )= -0.5024165063D-02
llnk( 378   )= -0.5016661793D-02
llnk( 379   )= -0.5009199414D-02
llnk( 380   )= -0.5001777482D-02
llnk( 381   )= -0.4994395567D-02
llnk( 382   )= -0.4987053274D-02
llnk( 383   )= -0.4979750283D-02
llnk( 384   )= -0.4972486128D-02
llnk( 385   )= -0.4965268840D-02
llnk( 386   )= -0.4958081936D-02
llnk( 387   )= -0.4950932817D-02
llnk( 388   )= -0.4943821092D-02
llnk( 389   )= -0.4936746301D-02
llnk( 390   )= -0.4929708210D-02
llnk( 391   )= -0.4922706445D-02
llnk( 392   )= -0.4915740611D-02
llnk( 393   )= -0.4908810284D-02
llnk( 394   )= -0.4901915260D-02
llnk( 395   )= -0.4895055124D-02
llnk( 396   )= -0.4888229556D-02
llnk( 397   )= -0.4881438160D-02
llnk( 398   )= -0.4874680679D-02
llnk( 399   )= -0.4867956758D-02
llnk( 400   )= -0.4861266098D-02
llnk( 401   )= -0.4854608317D-02
llnk( 402   )= -0.4847983151D-02
llnk( 403   )= -0.4841390266D-02
llnk( 404   )= -0.4834829391D-02
llnk( 405   )= -0.4828300135D-02
llnk( 406   )= -0.4821802243D-02
llnk( 407   )= -0.4815335427D-02
llnk( 408   )= -0.4808899393D-02
llnk( 409   )= -0.4802493803D-02
llnk( 410   )= -0.4796118410D-02
llnk( 411   )= -0.4789772949D-02
llnk( 412   )= -0.4783457104D-02
llnk( 413   )= -0.4777170573D-02
llnk( 414   )= -0.4770913098D-02
llnk( 415   )= -0.4764684685D-02
llnk( 416   )= -0.4758484990D-02
llnk( 417   )= -0.4752313124D-02
llnk( 418   )= -0.4746169197D-02
llnk( 419   )= -0.4740053059D-02
llnk( 420   )= -0.4733964341D-02
llnk( 421   )= -0.4727902849D-02
llnk( 422   )= -0.4721868283D-02
llnk( 423   )= -0.4715860425D-02
llnk( 424   )= -0.4709879012D-02
llnk( 425   )= -0.4703923846D-02
llnk( 426   )= -0.4697994583D-02
llnk( 427   )= -0.4692091066D-02
llnk( 428   )= -0.4686213045D-02
llnk( 429   )= -0.4680360271D-02
llnk( 430   )= -0.4674532484D-02
llnk( 431   )= -0.4668729492D-02
llnk( 432   )= -0.4662951085D-02
llnk( 433   )= -0.4657196996D-02
llnk( 434   )= -0.4651467004D-02
llnk( 435   )= -0.4645760890D-02
llnk( 436   )= -0.4640080410D-02
llnk( 437   )= -0.4634428369D-02
llnk( 438   )= -0.4628793378D-02
llnk( 439   )= -0.4623181470D-02
llnk( 440   )= -0.4617592432D-02
llnk( 441   )= -0.4612026079D-02
llnk( 442   )= -0.4606482171D-02
llnk( 443   )= -0.4600960531D-02
llnk( 444   )= -0.4595460979D-02
llnk( 445   )= -0.4589983282D-02
llnk( 446   )= -0.4584527233D-02
llnk( 447   )= -0.4579092710D-02
llnk( 448   )= -0.4573679458D-02
llnk( 449   )= -0.4568287340D-02
llnk( 450   )= -0.4562916080D-02
llnk( 451   )= -0.4557565629D-02
llnk( 452   )= -0.4552235678D-02
llnk( 453   )= -0.4546926139D-02
llnk( 454   )= -0.4541636751D-02
llnk( 455   )= -0.4536367408D-02
llnk( 456   )= -0.4531117888D-02
llnk( 457   )= -0.4525888082D-02
llnk( 458   )= -0.4520677720D-02
llnk( 459   )= -0.4515486721D-02
llnk( 460   )= -0.4510314872D-02
llnk( 461   )= -0.4505162010D-02
llnk( 462   )= -0.4500027980D-02
llnk( 463   )= -0.4494912622D-02
llnk( 464   )= -0.4489815761D-02
llnk( 465   )= -0.4484737267D-02
llnk( 466   )= -0.4479676977D-02
llnk( 467   )= -0.4474634684D-02
llnk( 468   )= -0.4469610304D-02
llnk( 469   )= -0.4464603661D-02
llnk( 470   )= -0.4459614543D-02
llnk( 471   )= -0.4454642899D-02
llnk( 472   )= -0.4449688484D-02
llnk( 473   )= -0.4444751266D-02
llnk( 474   )= -0.4439830996D-02
llnk( 475   )= -0.4434927562D-02
llnk( 476   )= -0.4430040859D-02
llnk( 477   )= -0.4425170710D-02
llnk( 478   )= -0.4420316957D-02
llnk( 479   )= -0.4415479491D-02
llnk( 480   )= -0.4410658186D-02
llnk( 481   )= -0.4405852880D-02
llnk( 482   )= -0.4401063430D-02
llnk( 483   )= -0.4396289728D-02
llnk( 484   )= -0.4391531676D-02
llnk( 485   )= -0.4386789086D-02
llnk( 486   )= -0.4382061813D-02
llnk( 487   )= -0.4377349779D-02
llnk( 488   )= -0.4372652829D-02
llnk( 489   )= -0.4367973960D-02
llnk( 490   )= -0.4363312569D-02
llnk( 491   )= -0.4358660920D-02
llnk( 492   )= -0.4354023941D-02
llnk( 493   )= -0.4349402011D-02
llnk( 494   )= -0.4344794018D-02
llnk( 495   )= -0.4340200286D-02
llnk( 496   )= -0.4335620759D-02
llnk( 497   )= -0.4331055332D-02
llnk( 498   )= -0.4326503816D-02
llnk( 499   )= -0.4321966182D-02
llnk( 500   )= -0.4317442332D-02
llnk( 501   )= -0.4312932067D-02
llnk( 502   )= -0.4308435312D-02
llnk( 503   )= -0.4303951987D-02
llnk( 504   )= -0.4299481991D-02
llnk( 505   )= -0.4295025176D-02
llnk( 506   )= -0.4290581476D-02
llnk( 507   )= -0.4286150784D-02
llnk( 508   )= -0.4281732974D-02
llnk( 509   )= -0.4277328005D-02
llnk( 510   )= -0.4272935697D-02
llnk( 511   )= -0.4268555976D-02
llnk( 512   )= -0.4264188766D-02
llnk( 513   )= -0.4259833966D-02
llnk( 514   )= -0.4255491492D-02
llnk( 515   )= -0.4251161154D-02
llnk( 516   )= -0.4246842949D-02
llnk( 517   )= -0.4242536837D-02
llnk( 518   )= -0.4238242629D-02
llnk( 519   )= -0.4233960267D-02
llnk( 520   )= -0.4229689632D-02
llnk( 521   )= -0.4225430669D-02
llnk( 522   )= -0.4221183310D-02
llnk( 523   )= -0.4216947377D-02
llnk( 524   )= -0.4212722872D-02
llnk( 525   )= -0.4208509690D-02
llnk( 526   )= -0.4204307736D-02
llnk( 527   )= -0.4200116898D-02
llnk( 528   )= -0.4195937125D-02
llnk( 529   )= -0.4191768337D-02
llnk( 530   )= -0.4187610437D-02
llnk( 531   )= -0.4183463316D-02
llnk( 532   )= -0.4179326948D-02
llnk( 533   )= -0.4175201222D-02
llnk( 534   )= -0.4171086099D-02
llnk( 535   )= -0.4166981404D-02
llnk( 536   )= -0.4162887134D-02
llnk( 537   )= -0.4158803202D-02
llnk( 538   )= -0.4154729536D-02
llnk( 539   )= -0.4150666091D-02
llnk( 540   )= -0.4146612671D-02
llnk( 541   )= -0.4142569363D-02
llnk( 542   )= -0.4138535935D-02
llnk( 543   )= -0.4134515728D-02
llnk( 544   )= -0.4130507039D-02
llnk( 545   )= -0.4126503797D-02
llnk( 546   )= -0.4122510270D-02
llnk( 547   )= -0.4118526386D-02
llnk( 548   )= -0.4114552039D-02
llnk( 549   )= -0.4110587257D-02
llnk( 550   )= -0.4106631920D-02
llnk( 551   )= -0.4102685926D-02
llnk( 552   )= -0.4098749247D-02
llnk( 553   )= -0.4094821847D-02
llnk( 554   )= -0.4090903630D-02
llnk( 555   )= -0.4086994545D-02
llnk( 556   )= -0.4083094472D-02
llnk( 557   )= -0.4079203396D-02
llnk( 558   )= -0.4075321316D-02
llnk( 559   )= -0.4071448028D-02
llnk( 560   )= -0.4067583587D-02
llnk( 561   )= -0.4063727921D-02
llnk( 562   )= -0.4059880939D-02
llnk( 563   )= -0.4056042583D-02
llnk( 564   )= -0.4052212762D-02
llnk( 565   )= -0.4048391527D-02
llnk( 566   )= -0.4044578730D-02
llnk( 567   )= -0.4040774354D-02
llnk( 568   )= -0.4036978296D-02
llnk( 569   )= -0.4033190534D-02
llnk( 570   )= -0.4029410975D-02
llnk( 571   )= -0.4025639600D-02
llnk( 572   )= -0.4021876379D-02
llnk( 573   )= -0.4018121658D-02
llnk( 574   )= -0.4014374533D-02
llnk( 575   )= -0.4010635431D-02
llnk( 576   )= -0.4006904167D-02
llnk( 577   )= -0.4003180813D-02
llnk( 578   )= -0.3999465257D-02
llnk( 579   )= -0.3995757506D-02
llnk( 580   )= -0.3992057412D-02
llnk( 581   )= -0.3988364979D-02
llnk( 582   )= -0.3984680200D-02
llnk( 583   )= -0.3981002957D-02
llnk( 584   )= -0.3977333187D-02
llnk( 585   )= -0.3973670940D-02
llnk( 586   )= -0.3970016087D-02
llnk( 587   )= -0.3966368605D-02
llnk( 588   )= -0.3962728432D-02
llnk( 589   )= -0.3959095565D-02
llnk( 590   )= -0.3955469896D-02
llnk( 591   )= -0.3951851444D-02
llnk( 592   )= -0.3948240106D-02
llnk( 593   )= -0.3944635840D-02
llnk( 594   )= -0.3941038624D-02
llnk( 595   )= -0.3937448450D-02
llnk( 596   )= -0.3933865180D-02
llnk( 597   )= -0.3930288852D-02
llnk( 598   )= -0.3926725182D-02
llnk( 599   )= -0.3923164225D-02
llnk( 600   )= -0.3919609022D-02
llnk( 601   )= -0.3916060603D-02
llnk( 602   )= -0.3912518941D-02
llnk( 603   )= -0.3908984017D-02
llnk( 604   )= -0.3905455707D-02
llnk( 605   )= -0.3901934071D-02
llnk( 606   )= -0.3898419000D-02
llnk( 607   )= -0.3894910446D-02
llnk( 608   )= -0.3891408443D-02
llnk( 609   )= -0.3887912933D-02
llnk( 610   )= -0.3884423810D-02
llnk( 611   )= -0.3880941150D-02
llnk( 612   )= -0.3877464789D-02
llnk( 613   )= -0.3873994730D-02
llnk( 614   )= -0.3870530985D-02
llnk( 615   )= -0.3867073514D-02
llnk( 616   )= -0.3863622258D-02
llnk( 617   )= -0.3860177173D-02
llnk( 618   )= -0.3856738235D-02
llnk( 619   )= -0.3853305441D-02
llnk( 620   )= -0.3849878697D-02
llnk( 621   )= -0.3846458016D-02
llnk( 622   )= -0.3843043331D-02
llnk( 623   )= -0.3839634656D-02
llnk( 624   )= -0.3836231919D-02
llnk( 625   )= -0.3832835028D-02
llnk( 626   )= -0.3829444026D-02
llnk( 627   )= -0.3826058954D-02
llnk( 628   )= -0.3822679633D-02
llnk( 629   )= -0.3819306152D-02
llnk( 630   )= -0.3815938396D-02
llnk( 631   )= -0.3812576353D-02
llnk( 632   )= -0.3809220046D-02
llnk( 633   )= -0.3805869353D-02
llnk( 634   )= -0.3802524327D-02
llnk( 635   )= -0.3799184885D-02
llnk( 636   )= -0.3795851005D-02
llnk( 637   )= -0.3792522658D-02
llnk( 638   )= -0.3789199831D-02
llnk( 639   )= -0.3785882531D-02
llnk( 640   )= -0.3782570655D-02
llnk( 641   )= -0.3779264191D-02
llnk( 642   )= -0.3775963147D-02
llnk( 643   )= -0.3772667476D-02
llnk( 644   )= -0.3769377113D-02
llnk( 645   )= -0.3766092076D-02
llnk( 646   )= -0.3762812304D-02
llnk( 647   )= -0.3759537833D-02
llnk( 648   )= -0.3756268585D-02
llnk( 649   )= -0.3753004510D-02
llnk( 650   )= -0.3749745613D-02
llnk( 651   )= -0.3746491901D-02
llnk( 652   )= -0.3743243283D-02
llnk( 653   )= -0.3740005626D-02
llnk( 654   )= -0.3736767845D-02
llnk( 655   )= -0.3733535132D-02
llnk( 656   )= -0.3730307327D-02
llnk( 657   )= -0.3727084607D-02
llnk( 658   )= -0.3723866873D-02
llnk( 659   )= -0.3720654107D-02
llnk( 660   )= -0.3717446303D-02
llnk( 661   )= -0.3714243463D-02
llnk( 662   )= -0.3711045523D-02
llnk( 663   )= -0.3707852455D-02
llnk( 664   )= -0.3704664303D-02
llnk( 665   )= -0.3701480968D-02
llnk( 666   )= -0.3698302457D-02
llnk( 667   )= -0.3695128771D-02
llnk( 668   )= -0.3691959874D-02
llnk( 669   )= -0.3688795678D-02
llnk( 670   )= -0.3685636275D-02
llnk( 671   )= -0.3682481602D-02
llnk( 672   )= -0.3679331555D-02
llnk( 673   )= -0.3676186270D-02
llnk( 674   )= -0.3673045561D-02
llnk( 675   )= -0.3669909551D-02
llnk( 676   )= -0.3666778150D-02
llnk( 677   )= -0.3663651302D-02
llnk( 678   )= -0.3660529071D-02
llnk( 679   )= -0.3657411389D-02
llnk( 680   )= -0.3654298215D-02
llnk( 681   )= -0.3651189465D-02
llnk( 682   )= -0.3648085315D-02
llnk( 683   )= -0.3644985621D-02
llnk( 684   )= -0.3641890453D-02
llnk( 685   )= -0.3638799641D-02
llnk( 686   )= -0.3635713278D-02
llnk( 687   )= -0.3632631311D-02
llnk( 688   )= -0.3629553745D-02
llnk( 689   )= -0.3626480539D-02
llnk( 690   )= -0.3623411645D-02
llnk( 691   )= -0.3620347077D-02
llnk( 692   )= -0.3617286859D-02
llnk( 693   )= -0.3614230904D-02
llnk( 694   )= -0.3611179235D-02
llnk( 695   )= -0.3608131812D-02
llnk( 696   )= -0.3605088635D-02
llnk( 697   )= -0.3602049657D-02
llnk( 698   )= -0.3599014895D-02
llnk( 699   )= -0.3595984357D-02
llnk( 700   )= -0.3592957941D-02
llnk( 701   )= -0.3589935718D-02
llnk( 702   )= -0.3586917596D-02
llnk( 703   )= -0.3583903622D-02
llnk( 704   )= -0.3580893698D-02
llnk( 705   )= -0.3577887909D-02
llnk( 706   )= -0.3574886158D-02
llnk( 707   )= -0.3571888466D-02
llnk( 708   )= -0.3568899450D-02
llnk( 709   )= -0.3565910323D-02
llnk( 710   )= -0.3562925154D-02
llnk( 711   )= -0.3559943972D-02
llnk( 712   )= -0.3556966859D-02
llnk( 713   )= -0.3553993656D-02
llnk( 714   )= -0.3551024466D-02
llnk( 715   )= -0.3548059237D-02
llnk( 716   )= -0.3545097968D-02
llnk( 717   )= -0.3542140587D-02
llnk( 718   )= -0.3539187147D-02
llnk( 719   )= -0.3536237614D-02
llnk( 720   )= -0.3533291992D-02
llnk( 721   )= -0.3530350229D-02
llnk( 722   )= -0.3527412335D-02
llnk( 723   )= -0.3524478279D-02
llnk( 724   )= -0.3521548076D-02
llnk( 725   )= -0.3518621713D-02
llnk( 726   )= -0.3515699144D-02
llnk( 727   )= -0.3512780363D-02
llnk( 728   )= -0.3509865386D-02
llnk( 729   )= -0.3506954146D-02
llnk( 730   )= -0.3504046711D-02
llnk( 731   )= -0.3501142980D-02
llnk( 732   )= -0.3498243029D-02
llnk( 733   )= -0.3495346825D-02
llnk( 734   )= -0.3492454277D-02
llnk( 735   )= -0.3489565504D-02
llnk( 736   )= -0.3486680283D-02
llnk( 737   )= -0.3483798748D-02
llnk( 738   )= -0.3480921125D-02
llnk( 739   )= -0.3478046974D-02
llnk( 740   )= -0.3475176495D-02
llnk( 741   )= -0.3472309608D-02
llnk( 742   )= -0.3469446344D-02
llnk( 743   )= -0.3466586679D-02
llnk( 744   )= -0.3463730607D-02
llnk( 745   )= -0.3460878143D-02
llnk( 746   )= -0.3458029208D-02
llnk( 747   )= -0.3455183869D-02
llnk( 748   )= -0.3452342050D-02
llnk( 749   )= -0.3449503792D-02
llnk( 750   )= -0.3446669019D-02
llnk( 751   )= -0.3443837835D-02
llnk( 752   )= -0.3441010114D-02
llnk( 753   )= -0.3438185843D-02
llnk( 754   )= -0.3435365132D-02
llnk( 755   )= -0.3432547840D-02
llnk( 756   )= -0.3429734036D-02
llnk( 757   )= -0.3426923672D-02
llnk( 758   )= -0.3424116757D-02
llnk( 759   )= -0.3421313262D-02
llnk( 760   )= -0.3418513177D-02
llnk( 761   )= -0.3415716524D-02
llnk( 762   )= -0.3412923273D-02
llnk( 763   )= -0.3410136336D-02
llnk( 764   )= -0.3407350916D-02
llnk( 765   )= -0.3404568130D-02
llnk( 766   )= -0.3401788745D-02
llnk( 767   )= -0.3399012718D-02
llnk( 768   )= -0.3396240047D-02
llnk( 769   )= -0.3393470771D-02
llnk( 770   )= -0.3390704781D-02
llnk( 771   )= -0.3387942162D-02
llnk( 772   )= -0.3385182859D-02
llnk( 773   )= -0.3382426879D-02
llnk( 774   )= -0.3379674202D-02
llnk( 775   )= -0.3376924820D-02
llnk( 776   )= -0.3374178738D-02
llnk( 777   )= -0.3371435939D-02
llnk( 778   )= -0.3368696456D-02
llnk( 779   )= -0.3365960182D-02
llnk( 780   )= -0.3363227225D-02
llnk( 781   )= -0.3360497473D-02
llnk( 782   )= -0.3357770991D-02
llnk( 783   )= -0.3355047725D-02
llnk( 784   )= -0.3352327686D-02
llnk( 785   )= -0.3349610914D-02
llnk( 786   )= -0.3346897303D-02
llnk( 787   )= -0.3344186906D-02
llnk( 788   )= -0.3341479724D-02
llnk( 789   )= -0.3338775700D-02
llnk( 790   )= -0.3336074863D-02
llnk( 791   )= -0.3333377225D-02
llnk( 792   )= -0.3330682695D-02
llnk( 793   )= -0.3327991302D-02
llnk( 794   )= -0.3325303130D-02
llnk( 795   )= -0.3322618041D-02
llnk( 796   )= -0.3319936145D-02
llnk( 797   )= -0.3317257379D-02
llnk( 798   )= -0.3314581695D-02
llnk( 799   )= -0.3311909149D-02
llnk( 800   )= -0.3309239696D-02
llnk( 801   )= -0.3306573350D-02
llnk( 802   )= -0.3303910077D-02
llnk( 803   )= -0.3301249895D-02
llnk( 804   )= -0.3298592786D-02
llnk( 805   )= -0.3295938770D-02
llnk( 806   )= -0.3293287792D-02
llnk( 807   )= -0.3290639881D-02
llnk( 808   )= -0.3287994967D-02
llnk( 809   )= -0.3285353159D-02
llnk( 810   )= -0.3282714334D-02
llnk( 811   )= -0.3280078559D-02
llnk( 812   )= -0.3277445795D-02
llnk( 813   )= -0.3274816076D-02
llnk( 814   )= -0.3272189346D-02
llnk( 815   )= -0.3269565604D-02
llnk( 816   )= -0.3266944867D-02
llnk( 817   )= -0.3264327100D-02
llnk( 818   )= -0.3261713686D-02
llnk( 819   )= -0.3259103619D-02
llnk( 820   )= -0.3256495012D-02
llnk( 821   )= -0.3253889400D-02
llnk( 822   )= -0.3251286777D-02
llnk( 823   )= -0.3248687255D-02
llnk( 824   )= -0.3246090539D-02
llnk( 825   )= -0.3243496801D-02
llnk( 826   )= -0.3240905999D-02
llnk( 827   )= -0.3238318138D-02
llnk( 828   )= -0.3235733197D-02
llnk( 829   )= -0.3233151191D-02
llnk( 830   )= -0.3230572128D-02
llnk( 831   )= -0.3227995977D-02
llnk( 832   )= -0.3225422751D-02
llnk( 833   )= -0.3222852394D-02
llnk( 834   )= -0.3220284979D-02
llnk( 835   )= -0.3217720406D-02
llnk( 836   )= -0.3215158781D-02
llnk( 837   )= -0.3212600066D-02
llnk( 838   )= -0.3210044148D-02
llnk( 839   )= -0.3207491153D-02
llnk( 840   )= -0.3204940999D-02
llnk( 841   )= -0.3202393742D-02
llnk( 842   )= -0.3199849338D-02
llnk( 843   )= -0.3197307775D-02
llnk( 844   )= -0.3194769052D-02
llnk( 845   )= -0.3192233205D-02
llnk( 846   )= -0.3189700191D-02
llnk( 847   )= -0.3187169971D-02
llnk( 848   )= -0.3184642524D-02
llnk( 849   )= -0.3182117988D-02
llnk( 850   )= -0.3179596231D-02
llnk( 851   )= -0.3177077300D-02
llnk( 852   )= -0.3174561186D-02
llnk( 853   )= -0.3172047887D-02
llnk( 854   )= -0.3169537359D-02
llnk( 855   )= -0.3167029632D-02
llnk(   856 )= -0.3164524709D-02
llnk( 857   )= -0.3162022553D-02
llnk( 858   )= -0.3159523159D-02
llnk( 859   )= -0.3157026583D-02
llnk( 860   )= -0.3154532729D-02
llnk( 861   )= -0.3152041656D-02
llnk( 862   )= -0.3149553327D-02
llnk( 863   )= -0.3147067778D-02
llnk( 864   )= -0.3144584964D-02
llnk( 865   )= -0.3142104892D-02
llnk( 866   )= -0.3139627563D-02
llnk( 867   )= -0.3137152962D-02
llnk( 868   )= -0.3134681067D-02
llnk( 869   )= -0.3132211915D-02
llnk( 870   )= -0.3129745502D-02
llnk( 871   )= -0.3127281759D-02
llnk( 872   )= -0.3124820761D-02
llnk( 873   )= -0.3122362718D-02
llnk( 874   )= -0.3119909222D-02
llnk( 875   )= -0.3117456503D-02
llnk( 876   )= -0.3115006490D-02
llnk( 877   )= -0.3112559221D-02
llnk( 878   )= -0.3110114595D-02
llnk( 879   )= -0.3107672711D-02
llnk( 880   )= -0.3105233507D-02
llnk( 881   )= -0.3102796921D-02
llnk( 882   )= -0.3100363082D-02
llnk( 883   )= -0.3097931893D-02
llnk( 884   )= -0.3095503371D-02
llnk( 885   )= -0.3093077570D-02
llnk( 886   )= -0.3090654347D-02
llnk( 887   )= -0.3088233816D-02
llnk( 888   )= -0.3085815991D-02
llnk( 889   )= -0.3083400748D-02
llnk( 890   )= -0.3080988196D-02
llnk( 891   )= -0.3078578271D-02
llnk( 892   )= -0.3076170985D-02
llnk( 893   )= -0.3073766340D-02
llnk( 894   )= -0.3071364353D-02
llnk( 895   )= -0.3068964958D-02
llnk( 896   )= -0.3066568203D-02
llnk( 897   )= -0.3064174074D-02
llnk( 898   )= -0.3061782549D-02
llnk( 899   )= -0.3059393659D-02
llnk( 900   )= -0.3057007375D-02
llnk( 901   )= -0.3054623711D-02
llnk( 902   )= -0.3052242585D-02
llnk( 903   )= -0.3049864141D-02
llnk( 904   )= -0.3047488140D-02
llnk( 905   )= -0.3045114846D-02
llnk( 906   )= -0.3042744119D-02
llnk( 907   )= -0.3040375970D-02
llnk( 908   )= -0.3038010418D-02
llnk( 909   )= -0.3035647475D-02
llnk( 910   )= -0.3033287098D-02
llnk( 911   )= -0.3030929263D-02
llnk( 912   )= -0.3028574024D-02
llnk( 913   )= -0.3026221302D-02
llnk( 914   )= -0.3023871162D-02
llnk( 915   )= -0.3021523546D-02
llnk( 916   )= -0.3019178525D-02
llnk( 917   )= -0.3016836027D-02
llnk( 918   )= -0.3014496035D-02
llnk( 919   )= -0.3012158656D-02
llnk( 920   )= -0.3009823741D-02
llnk( 921   )= -0.3007491429D-02
llnk( 922   )= -0.3005161605D-02
llnk( 923   )= -0.3002834314D-02
llnk( 924   )= -0.3000509569D-02
llnk( 925   )= -0.2998187291D-02
llnk( 926   )= -0.2995867561D-02
llnk( 927   )= -0.2993550344D-02
llnk( 928   )= -0.2991235634D-02
llnk( 929   )= -0.2988925225D-02
llnk( 930   )= -0.2986615660D-02
llnk( 931   )= -0.2984308615D-02
llnk( 932   )= -0.2982004089D-02
llnk( 933   )= -0.2979702045D-02
llnk( 934   )= -0.2977402494D-02
llnk( 935   )= -0.2975105470D-02
llnk( 936   )= -0.2972810888D-02
llnk( 937   )= -0.2970518821D-02
llnk( 938   )= -0.2968229240D-02
llnk( 939   )= -0.2965942158D-02
llnk( 940   )= -0.2963657551D-02
llnk( 941   )= -0.2961375403D-02
llnk( 942   )= -0.2959095731D-02
llnk( 943   )= -0.2956818525D-02
llnk( 944   )= -0.2954543818D-02
llnk( 945   )= -0.2952271541D-02
llnk( 946   )= -0.2950001748D-02
llnk( 947   )= -0.2947734412D-02
llnk( 948   )= -0.2945469512D-02
llnk( 949   )= -0.2943207084D-02
llnk( 950   )= -0.2940947077D-02
llnk( 951   )= -0.2938689563D-02
llnk( 952   )= -0.2936434487D-02
llnk( 953   )= -0.2934181799D-02
llnk( 954   )= -0.2931931606D-02
llnk( 955   )= -0.2929683842D-02
llnk( 956   )= -0.2927438481D-02
llnk( 957   )= -0.2925195572D-02
llnk( 958   )= -0.2922955070D-02
llnk( 959   )= -0.2920716941D-02
llnk( 960   )= -0.2918481269D-02
llnk( 961   )= -0.2916248012D-02
llnk( 962   )= -0.2914017179D-02
llnk( 963   )= -0.2911788782D-02
llnk( 964   )= -0.2909562774D-02
llnk( 965   )= -0.2907339192D-02
llnk( 966   )= -0.2905117982D-02
llnk( 967   )= -0.2902899173D-02
llnk( 968   )= -0.2900682799D-02
llnk( 969   )= -0.2898468796D-02
llnk( 970   )= -0.2896257166D-02
llnk( 971   )= -0.2894047930D-02
llnk( 972   )= -0.2891841138D-02
llnk( 973   )= -0.2889636618D-02
llnk( 974   )= -0.2887434580D-02
llnk( 975   )= -0.2885234873D-02
llnk( 976   )= -0.2883037537D-02
llnk( 977   )= -0.2880842566D-02
llnk( 978   )= -0.2878649974D-02
llnk( 979   )= -0.2876459766D-02
llnk( 980   )= -0.2874271894D-02
llnk( 981   )= -0.2872086376D-02
llnk( 982   )= -0.2869903273D-02
llnk( 983   )= -0.2867722443D-02
llnk( 984   )= -0.2865545315D-02
llnk( 985   )= -0.2863369328D-02
llnk( 986   )= -0.2861195689D-02
llnk( 987   )= -0.2859024433D-02
llnk( 988   )= -0.2856855469D-02
llnk( 989   )= -0.2854688909D-02
llnk( 990   )= -0.2852524645D-02
llnk( 991   )= -0.2850362774D-02
llnk( 992   )= -0.2848203196D-02
llnk( 993   )= -0.2846045956D-02
llnk( 994   )= -0.2843891068D-02
llnk( 995   )= -0.2841738481D-02
llnk( 996   )= -0.2839588248D-02
llnk( 997   )= -0.2837440365D-02
llnk( 998   )= -0.2835294724D-02
llnk( 999   )= -0.2833151463D-02
llnk( 1000  )= -0.2831010498D-02
llnk( 1001  )= -0.2828871870D-02
llnk( 1002  )= -0.2826735533D-02
llnk( 1003  )= -0.2824601509D-02
llnk( 1004  )= -0.2822469774D-02
llnk( 1005  )= -0.2820340370D-02
llnk( 1006  )= -0.2818213253D-02
llnk( 1007  )= -0.2816088447D-02
llnk( 1008  )= -0.2813965924D-02
llnk( 1009  )= -0.2811845711D-02
llnk( 1010  )= -0.2809727762D-02
llnk( 1011  )= -0.2807612130D-02
llnk( 1012  )= -0.2805498780D-02
llnk( 1013  )= -0.2803387663D-02
llnk( 1014  )= -0.2801278909D-02
llnk( 1015  )= -0.2799172307D-02
llnk( 1016  )= -0.2797068067D-02
llnk( 1017  )= -0.2794966114D-02
llnk( 1018  )= -0.2792866401D-02
llnk( 1019  )= -0.2790768970D-02
llnk( 1020  )= -0.2788673820D-02
llnk( 1021  )= -0.2786580911D-02
llnk( 1022  )= -0.2784490270D-02
llnk( 1023  )= -0.2782401919D-02
llnk( 1024  )= -0.2780315803D-02

   end subroutine loadlovenumber
   
   !
   !
   ! ==========================================================================
   !> 
   subroutine tforce( jul0, TIME , xzeta , yzeta , TIDEP, IDIM1, dstart, dstop , eps)

   use messagehandling 
   !
   ! ====================================================================
   !
   !     Programmer     E.J.O. Schrama
   !
   !     Original URL: https://repos.deltares.nl/repos/simona/bo_omgeving/simona/src/waqua/waqpro/routines/wastfr.f
   !     $Revision: 1850 $, $Date: 2008-04-18 09:19:37 +0200 (Fri, 18 Apr 2008) $
   !
   !     Version 1.1    Date 22-05-2008   c81402: extended for evaluation of
   !                                              tidal forces on grids (AVe,
   !                                              VORtech)
   !     Version 1.0    Date 24-01-2008   initial version
   !
   !     Copyright (c) "E.J.O. Schrama".
   !     Permission to copy or distribute this software or documentation
   !     in hard copy or soft copy granted only by written license
   !     obtained from "Rijkswaterstaat".
   !     All rights reserved. No part of this publication may be
   !     reproduced, stored in a retrieval system (e.g., in memory, disk,
   !     or core) or be transmitted by any means, electronic, mechanical,
   !     photocopy, recording, or otherwise, without written permission
   !     from the publisher.
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
   
         double precision ::  rmjdat, dstart, dstop, eps, TIME
         double precision xzeta(idim1), yzeta(idim1), tidep(idim1)
         
         DOUBLE PRECISION, allocatable,  SAVE :: tideuc(:,:,:), tideus(:,:,:)   !       (idim1, 0:3,2:3), 
   
         INTEGER, SAVE :: IRC = 0
   
         CHARACTER(len=40), dimension(484) :: RECS  ! ZAT IN FILE 'HARMONICS' 
   
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
         
         double precision ::  pi, g, rmu, re, d2r, reps
         parameter (idebug=0, i1dbg=0, i2dbg=0)
         parameter (maxdat=500)            ! maximal # records in table
         parameter (maxfld=7)              ! maximal # fields in table
         parameter (pi  = 3.14159265358979, re = 6378137d0,       &
                    d2r = pi/180d0,         rmu = 3.9860044d14,   &
                    g   = rmu / re / re,    reps = 1d-5 )
   
         integer ntable, nskip
         integer itable(maxdat,maxfld)
         double precision ::  amps(maxdat), plsmin(6), rklove(3), rhlove(3),   &
                 factor(2:3), pol1(0:3,2:3), cm1(0:3), sm1(0:3)
   
         integer i,j,nq,mq, IERR
         integer kk(10 )
         double precision :: fnm, pnm, har, argum, argfct, dtab1, dtab2,       &
                dtab, rlslat, rlslon, rlat, rlong, potent
         double precision :: elmnts(6), can(maxdat), san(maxdat)
         double precision :: cansum(0:3,2:3), sansum(0:3,2:3)
         character*80 record
         logical permnt
         DOUBLE PRECISION, SAVE :: FACTORIAL(0:6)
   
   !
   !     amps           table with scaled amplitudes for selected tidal components
   !     argfct         multiplication factor needed to compute argument
   !     argum          argument for time-dependent harmonic components ca,sa
   !     can            table with scaled harmonic components
   !                    dcos(argument) * amp(i)
   !     cansum         selected sum of elements of can for fixed mq,nq
   !     cm1            cosine-component of potential
   !     d2r            conversion factor pi/180
   !     dtab           Doodson number: dtab1 + dtab2 / 1000d0
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
   !     rmu            gravitational constant (3.9860044d14)
   !     san            table with scaled harmonic components
   !                    dsin(argument) * amp(i)
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
   !        can(i) = dcos(argum) * amps(i)
   !        san(i) = dsin(argum) * amps(i)
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
         data plsmin / +1d0, +1d0, +1d0, +1d0, -1d0, +1d0 /
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
   
   
         if (irc.eq.0 ) then
   
   
            IRC = 1
   
   
            FACTORIAL(0) =   1D0 
            FACTORIAL(1) =   1D0 
            FACTORIAL(2) =   2D0 
            FACTORIAL(3) =   6D0 
            FACTORIAL(4) =  24D0 
            FACTORIAL(5) = 120D0 
            FACTORIAL(6) = 720D0 
   
            if (allocated (tideuc) ) deallocate(tideuc, tideus) 
            ALLOCATE ( tideuc(0:3,2:3,IDIM1), STAT = IERR); tideuc = 0d0 
            ALLOCATE ( tideus(0:3,2:3,IDIM1), STAT = IERR); tideus = 0d0 
   
            call iniharmonics(recs)
   
   
            if (idebug.ge.10) then
               write(6,*) '*** Start reading the harmonics ' //  &
                         'table ***'
               write(6,*)
            end if
   !
   !        --- k and h love numbers for degree 2 and 3
   !
            rklove(1) = 0d0
            rklove(2) = 0.303d0
            rklove(3) = 0.0937d0
            rhlove(1) = 0d0
            rhlove(2) = 0.612d0
            rhlove(3) = 0.293d0
   !
            do nq = 2, 3
               factor(nq) = (1d0 + rklove(nq) - rhlove(nq))
            enddo
   !
            ntable = 0
            nskip  = 0
            N = 0
    10      continue
            N = N + 1
            IF (N > 484) GOTO 20
            RECORD = RECS(N)
            ! read(luhar,'(a)',end=20) record
            if (idebug.ge.10) write(6,*) record
            if (record(1:1).eq.'%') go to 10
            read(record,*) (kk(i),i = 1, 7), har
   !
   !            in the CTE tables there is a null line for theoretic
   !        --- reasons, never use this line to compute the tidal
   !            potential.
   !
            permnt =  kk(1).eq.0 .and. kk(2).eq.0 .and. &
                      kk(3).eq.0 .and. kk(4).eq.0 .and. &
                      kk(5).eq.0 .and. kk(6).eq.0
   !
   !        --- dtab is the doodson number for a table entry,
   !            select the lines where dstart <= dtab <= dstop
   !
            dtab1 = kk(1)*100d0 + (kk(2)+5d0)*10d0 + (kk(3)+5d0)
            dtab2 = (kk(4)+5d0)*100d0 + (kk(5)+5d0)*10d0 + (kk(6)+5d0)
            dtab  = dtab1 + dtab2 / 1000d0
   
            if (.not.permnt .and. abs(har).ge.eps .and. dstart.le.dtab  .and. dtab.le.dstop  ) then
   
               ntable = ntable + 1
               if (ntable.gt.maxdat) then
                  irc = -2
                  return
               end if
               do i = 1, 7
                  itable(ntable,i) = kk(i)
               end do
               nq  = kk(7)
               amps(ntable) = har * g * factor(nq)
            else
               nskip = nskip + 1
            end if
   
            go to 10
   
   
    20      continue
           ! rewind(luhar)
   
            rlslat  = -9999d0
            rlslon  = -9999d0
   
            if (idebug.ge.10) then
               write(6,*) 'ntable = ',ntable,'   nskip = ',nskip
            end if
   !
   !        --- storage of uc, us
   !
               do 90 i1 = 1, idim1
   
                     rlat  = yzeta(i1)
                     rlong = xzeta(i1)
   !                     compute legendre polynomials times the global
   !                 --- normalization factor
   !                     (fnm) found in cartwright 1993, also compute the
   !                     astronomical elements
   !
                     if (abs(rlat-rlslat).gt.reps) then
                        do nq = 2, 3
                           do mq = 0, nq
                              fnm = 2d0 / dble(2*nq+1) * factorial(nq+mq) /  factorial(nq-mq)
                              fnm = sqrt(1d0 / (2d0 * pi * fnm)) *      ((-1d0)**mq)
                              call legpol1( rlat, nq, mq, pnm )
                              pol1(mq, nq) = fnm * pnm
                           enddo
                        enddo
                     end if
   
                     if (abs(rlong-rlslon).gt.reps ) then
                        do mq = 0, 3
                           cm1(mq) = +cos(dble(mq) * rlong)
                           sm1(mq) = +sin(dble(mq) * rlong)
                        enddo
                     end if
   
   !
   !                       --- compute arrays with tideuc, tideus
   !
                     do nq = 2, 3
                        do mq = 0, nq
                           if (mod(nq+mq, 2).eq.0) then
                              tideuc(mq, nq, I1) = +cm1(mq) *                                          pol1(mq, nq)
                              tideus(mq, nq, I1) = -sm1(mq) *                                          pol1(mq, nq)
                           else
                              tideuc(mq, nq, I1) = +sm1(mq) *                                          pol1(mq, nq)
                              tideus(mq, nq, I1) = +cm1(mq) *                                          pol1(mq, nq)
                           end if
   
                           if (idebug.ge.2 .and. i1.eq.i1dbg) then
                              write(6,123) 'mq,nq=',mq,nq,                     &
                                ': rlat=',rlat,', rlong=',rlong,               &
                                ', pol1=',pol1(mq,nq),', cm1=',cm1(mq),',',    &
                                'sm1=',sm1(mq),                                &
                                ', tideuc=',tideuc(mq,nq,I1),                  &
                                ', tideus=',tideus(mq,nq,I1)   
     123                       format(a,2i2,4(a,f8.4),a,/,12x,3(a,f8.4))
                           end if
                        enddo
                     enddo
   !
   !                 --- and finally save the last known latitude and
   !                     longitude
   !
                     rlslat = rlat
                     rlslon = rlong
   
      90       continue
   
         end if
   !
   !     --- end of initialisation phase
   !
   !     ------------------------------------------------------------------------
   !
   !     --- update elements-table
   !
         call astrol( rmjdat, elmnts )
   !
   !     --- compute tabels can, san
   !
         do i = 1, ntable
            argum = 0d0
            do j = 1, 6
               argfct = dble(itable(i, j))
               argum  = argum + argfct * elmnts(j) * plsmin(j)
            end do
            ! argum = mod(argum, 360d0)
            ! if (argum.lt.0d0) argum = argum + 360d0
            argum = argum * d2r
            can(i) = cos(argum) * amps(i)
            san(i) = sin(argum) * amps(i)

            !msgbuf = 'Tide generating component i, argum, amps(i) : '
            !write(msgbuf(50:),'(I8,2F10.4)') i,argum,amps(i)
            !call SetMessage(LEVEL_INFO, msgbuf)

         enddo
   !
   !     --- compute tables cansum, sansum
   !
         do nq = 2, 3
            do mq = 0, nq
               cansum(mq, nq) = 0d0
               sansum(mq, nq) = 0d0
               do i = 1, ntable
                  if ( itable(i,7).eq.nq .and. itable(i,1).eq.mq) then
                     cansum(mq, nq) = cansum(mq, nq) + can(i)
                     sansum(mq, nq) = sansum(mq, nq) + san(i)
                  end if
               enddo
            enddo
         enddo
   !
   !     --- computation of the tidal potential at each grid-point:
   !
            do 190 i1 = 1, idim1
   
                  potent = 0d0
                  do nq = 2, 3
                     do mq = 0, nq
                        potent = potent + tideuc(mq, nq, I1) * cansum(mq,nq)  
                        potent = potent + tideus(mq, nq, I1) * sansum(mq,nq)
                        ! if (idebug.ge.2 .and. i1.eq.i1dbg) then
                        !   write(6,'(a,2i2,2(2(a,f10.6)))') 'mq,nq=',mq,nq,    &
                        !      ': term uc=',tideuc(i1,mq,nq),'*',               &
                        !      cansum(mq,nq),                                   &
                        !      ', us=',tideus(i1,mq,nq),'*',                    &
                        !      sansum(mq,nq)
                        !end if
                     enddo
                  enddo
                  tidep(i1) = potent
     190    continue
   
         ! if (idebug.ge.1 .and. i1dbg.ge.1)    write(6,*) 'tidep=', tidep(i1dbg)

   end subroutine tforce
   !
   !
   ! ==========================================================================
   !> 
   subroutine astrol( mjdate,six )
   ! ====================================================================
   !
   !     Programmer     R. D. Ray
   !
   !     Version 1.0    Date dec. 1990    initial version
   !
   ! ********************************************************************
   !
   !     DESCRIPTION
   !
   !     This copied from richard's subroutine astrol, in goes the
   !     modified Julian date, out comes an array of six double precision
   !     variables used for Doodson number computations
   !
   !     Computes the basic astronomical mean longitudes  s, h, p, N.
   !     Note N is not N', i.e. N is decreasing with time.
   !     These formulae are for the period 1990 - 2010, and were derived
   !     by David Cartwright (personal comm., Nov. 1990).
   !     TIME is UTC in decimal MJD.
   !     All longitudes returned in degrees.
   !     R. D. Ray    Dec. 1990
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
         double precision :: six(6),mjdate
   !
   !     mjdate      i    modified julian day (24-jan-2008 0:00 UTC : 54489.00000)
   !     six           o  array of six double precision variables used for Doodson
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
         double precision :: circle
         parameter  (CIRCLE=360.0D0)
   !
   !     circle       number of degrees in a circle
   !
   !     --- variables:
   !
         double precision :: T,TIME,UT
         integer i
   !
   !     T           translated time: TIME - 51544.4993D0
   !     TIME        input time (mjdate)
   !     UT          fractional part of mjdate: (mjdate - int(mjdate))
   !
   !=======================================================================
   !
   !     --- start of code
   !
         TIME = mjdate
         T    = TIME - 51544.4993D0   ! reference to 2000/1/1 1200 o'clock
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
         six(2) = 218.3164D0 + 13.17639648D0 * T
         six(3) = 280.4661D0 +  0.98564736D0 * T
         six(4) =  83.3535D0 +  0.11140353D0 * T
         six(5) = 125.0445D0 -  0.05295377D0 * T
         six(6) = 282.9384D0 +  0.0000471d0  * T
   !
   !     --- get them in the right quadrant
   !
         do i = 2, 6
   
           six(i) = mod(six(i), circle)
           if (six(i).lt.0d0) six(i) = six(i) + circle
   
         end do
   !
   !         argument 1 in the doodson number denotes the mean lunar time.
   !         According to equation 13 and the inline remark after equation 14
   !         it is computed by
   !         alpha_G = 360 * "Universal time in fractions of a day" + q'(T) - 180
   !         tau = alpha_G - q
   !
         UT     = (mjdate - int(mjdate))
         six(1) = 360d0 * UT + six(3) - 180d0 - six(2)
   end subroutine astrol
   !
   !
   ! ==========================================================================
   !> 
   subroutine legpol1( theta,n,m,pnm )
   ! ====================================================================
   !
   !     Programmer     E. Schrama <e.j.o.schrama@tudelft.nl>
   !
   ! ********************************************************************
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
         integer n,m
         double precision :: theta,pnm
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
         double precision :: cp,sp
   !
   !     cp          cos(theta)
   !     sp          sin(theta)
   !
   !=======================================================================
   !
         pnm = 1d38
   
         cp = cos( theta )
         sp = sin( theta )
   !
   !     --- I think this comes from (Lambeck,1988), what again are the rules for
   !         obtaining associated Legendre functions?
   !
         if (n.eq.0 ) then
           if (m.eq.0 ) pnm = 1d0
         ELSE if (n.eq.1 ) then
            if (m.eq.0 ) pnm = sp
            if (m.eq.1 ) pnm = cp
         ELSE if (n.eq.2 ) then
            if (m.eq.0 ) pnm = 1.5d0*sp*sp - 0.5d0
            if (m.eq.1 ) pnm = 3.0d0*sp*cp
            if (m.eq.2 ) pnm = 3.0d0*cp*cp
         ELSE IF (n.eq.3 ) then
            if (m.eq.0 ) pnm = 2.5d0*sp*sp*sp - 1.5d0*sp
            if (m.eq.1 ) pnm = cp * (7.5d0*sp*sp - 1.5d0)
            if (m.eq.2 ) pnm = 15d0*cp*cp*sp
            if (m.eq.3 ) pnm = 15d0*cp*cp*cp
         end if
   end subroutine legpol1
   !
   !
   ! ==========================================================================
   !> 
   SUBROUTINE INIHARMONICS(RECS)
      CHARACTER(len=40), dimension(484) :: RECS  ! ZAT IN FILE 'HARMONICS' 
   
   !%refsys 2000
   !%mjd0    47893.00000000
   !%mjd1    55196.00000000
   !%dmjd         .11410938
   !%ndata   64000
   !%gmearth 3.9860044d14
   !%reearth 6378137
   RECS(  1) = ' 0  0  0  0  0  0  2      -.31459'
   RECS(  2) = ' 0  0  0  0  1  0  2       .02793'
   RECS(  3) = ' 0  0  0  0  2  0  2      -.00028'
   RECS(  4) = ' 0  0  0  2  1  0  2       .00004'
   RECS(  5) = ' 0  0  1  0 -1 -1  2      -.00004'
   RECS(  6) = ' 0  0  1  0  0 -1  2      -.00492'
   RECS(  7) = ' 0  0  1  0  0  1  2       .00026'
   RECS(  8) = ' 0  0  1  0  1 -1  2       .00004'
   RECS(  9) = ' 0  0  2 -2 -1  0  2       .00002'
   RECS( 10) = ' 0  0  2 -2  0  0  2      -.00031'
   RECS( 11) = ' 0  0  2  0  0  0  2      -.03099'
   RECS( 12) = ' 0  0  2  0  0 -2  2      -.00012'
   RECS( 13) = ' 0  0  2  0  1  0  2       .00076'
   RECS( 14) = ' 0  0  2  0  2  0  2       .00017'
   RECS( 15) = ' 0  0  3  0  0 -1  2      -.00181'
   RECS( 16) = ' 0  0  3  0  1 -1  2       .00003'
   RECS( 17) = ' 0  0  4  0  0 -2  2      -.00007'
   RECS( 18) = ' 0  1 -3  1 -1  1  2       .00002'
   RECS( 19) = ' 0  1 -3  1  0  1  2      -.00029'
   RECS( 20) = ' 0  1 -2 -1 -2  0  2       .00002'
   RECS( 21) = ' 0  1 -2 -1 -1  0  2       .00006'
   RECS( 22) = ' 0  1 -2  1 -1  0  2       .00048'
   RECS( 23) = ' 0  1 -2  1  0  0  2      -.00673'
   RECS( 24) = ' 0  1 -2  1  1  0  2       .00044'
   RECS( 25) = ' 0  1 -1 -1  0  1  2      -.00021'
   RECS( 26) = ' 0  1 -1  0  0  0  2       .00019'
   RECS( 27) = ' 0  1 -1  1  0 -1  2       .00005'
   RECS( 28) = ' 0  1  0 -1 -2  0  2      -.00003'
   RECS( 29) = ' 0  1  0 -1 -1  0  2       .00231'
   RECS( 30) = ' 0  1  0 -1  0  0  2      -.03518'
   RECS( 31) = ' 0  1  0 -1  1  0  2       .00228'
   RECS( 32) = ' 0  1  0  1  0  0  2       .00188'
   RECS( 33) = ' 0  1  0  1  1  0  2       .00077'
   RECS( 34) = ' 0  1  0  1  2  0  2       .00021'
   RECS( 35) = ' 0  1  1 -1  0 -1  2       .00018'
   RECS( 36) = ' 0  1  2 -1  0  0  2       .00049'
   RECS( 37) = ' 0  1  2 -1  1  0  2       .00024'
   RECS( 38) = ' 0  1  2 -1  2  0  2       .00004'
   RECS( 39) = ' 0  1  3 -1  0 -1  2       .00002'
   RECS( 40) = ' 0  2 -4  2  0  0  2      -.00011'
   RECS( 41) = ' 0  2 -3  0  0  1  2      -.00039'
   RECS( 42) = ' 0  2 -3  0  1  1  2       .00002'
   RECS( 43) = ' 0  2 -2  0 -1  0  2      -.00042'
   RECS( 44) = ' 0  2 -2  0  0  0  2      -.00584'
   RECS( 45) = ' 0  2 -2  0  1  0  2       .00037'
   RECS( 46) = ' 0  2 -2  2  0  0  2       .00004'
   RECS( 47) = ' 0  2 -1 -2  0  1  2      -.00004'
   RECS( 48) = ' 0  2 -1 -1  0  0  2       .00003'
   RECS( 49) = ' 0  2 -1  0  0 -1  2       .00007'
   RECS( 50) = ' 0  2 -1  0  0  1  2      -.00020'
   RECS( 51) = ' 0  2 -1  0  1  1  2      -.00004'
   RECS( 52) = ' 0  2  0 -2 -1  0  2       .00015'
   RECS( 53) = ' 0  2  0 -2  0  0  2      -.00288'
   RECS( 54) = ' 0  2  0 -2  1  0  2       .00019'
   RECS( 55) = ' 0  2  0  0  0  0  2      -.06660'
   RECS( 56) = ' 0  2  0  0  1  0  2      -.02761'
   RECS( 57) = ' 0  2  0  0  2  0  2      -.00258'
   RECS( 58) = ' 0  2  0  0  3  0  2       .00006'
   RECS( 59) = ' 0  2  1 -2  0 -1  2       .00003'
   RECS( 60) = ' 0  2  1  0  0 -1  2       .00023'
   RECS( 61) = ' 0  2  1  0  1 -1  2       .00006'
   RECS( 62) = ' 0  2  2 -2  0  0  2       .00020'
   RECS( 63) = ' 0  2  2 -2  1  0  2       .00008'
   RECS( 64) = ' 0  2  2  0  2  0  2       .00003'
   RECS( 65) = ' 0  3 -5  1  0  1  2      -.00002'
   RECS( 66) = ' 0  3 -4  1  0  0  2      -.00018'
   RECS( 67) = ' 0  3 -3 -1  0  1  2      -.00007'
   RECS( 68) = ' 0  3 -3  1  0  1  2      -.00011'
   RECS( 69) = ' 0  3 -3  1  1  1  2      -.00005'
   RECS( 70) = ' 0  3 -2 -1 -1  0  2      -.00009'
   RECS( 71) = ' 0  3 -2 -1  0  0  2      -.00092'
   RECS( 72) = ' 0  3 -2 -1  1  0  2       .00006'
   RECS( 73) = ' 0  3 -2  1  0  0  2      -.00242'
   RECS( 74) = ' 0  3 -2  1  1  0  2      -.00100'
   RECS( 75) = ' 0  3 -2  1  2  0  2      -.00009'
   RECS( 76) = ' 0  3 -1 -1  0  1  2      -.00013'
   RECS( 77) = ' 0  3 -1 -1  1  1  2      -.00004'
   RECS( 78) = ' 0  3 -1  0  0  0  2       .00007'
   RECS( 79) = ' 0  3 -1  0  1  0  2       .00003'
   RECS( 80) = ' 0  3 -1  1  0 -1  2       .00003'
   RECS( 81) = ' 0  3  0 -3  0  0  2      -.00023'
   RECS( 82) = ' 0  3  0 -3  1 -1  2       .00003'
   RECS( 83) = ' 0  3  0 -3  1  1  2       .00003'
   RECS( 84) = ' 0  3  0 -1  0  0  2      -.01275'
   RECS( 85) = ' 0  3  0 -1  1  0  2      -.00529'
   RECS( 86) = ' 0  3  0 -1  2  0  2      -.00050'
   RECS( 87) = ' 0  3  0  1  2  0  2       .00005'
   RECS( 88) = ' 0  3  0  1  3  0  2       .00002'
   RECS( 89) = ' 0  3  1 -1  0 -1  2       .00011'
   RECS( 90) = ' 0  3  1 -1  1 -1  2       .00004'
   RECS( 91) = ' 0  4 -4  0  0  0  2      -.00008'
   RECS( 92) = ' 0  4 -4  2  0  0  2      -.00006'
   RECS( 93) = ' 0  4 -4  2  1  0  2      -.00003'
   RECS( 94) = ' 0  4 -3  0  0  1  2      -.00014'
   RECS( 95) = ' 0  4 -3  0  1  1  2      -.00006'
   RECS( 96) = ' 0  4 -2 -2  0  0  2      -.00011'
   RECS( 97) = ' 0  4 -2  0  0  0  2      -.00204'
   RECS( 98) = ' 0  4 -2  0  1  0  2      -.00084'
   RECS( 99) = ' 0  4 -2  0  2  0  2      -.00008'
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
   END SUBROUTINE INIHARMONICS
end module timespace_data
!
!
!
! ==========================================================================
! ==========================================================================
! ==========================================================================
!> 
Module M_arcuv                       ! plotbuitenbeentje
  implicit none
  double precision, allocatable :: arcuv(:,:,:)
End module M_arcuv
!
!
!
! ==========================================================================
! ==========================================================================
! ==========================================================================
!> 
module m_spiderweb                   ! plot spiderweb
  implicit none
  double precision, allocatable :: spw(:,:,:)
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

    integer                                :: nsold   ! nr of samples in previous triangulation
    integer                                :: numtri
    integer , allocatable, dimension(:, :) :: indx
    double precision, allocatable, dimension(:)    :: xcent
    double precision, allocatable, dimension(:)    :: ycent



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
   
       ! Author: H. Kernkamp
      implicit none
   
      double precision              , intent(in)  :: xl, yl           ! point under consideration 
      integer                       , intent(in)  :: n
      double precision, dimension(n), intent(in)  :: x, y             ! polygon(n)
      integer                      , intent(out)  :: inside
   
      integer  :: i, i1, i2, np, rechts
      double precision :: rl, rm, x1, x2, y1, y2
   
      if (n .le. 2) then
         inside = 1
      else
         np = 0
    5    continue
         np = np + 1
         if (np .le. n) then
            if ( x(np) .ne. dmiss_default) goto 5
         end if
         np = np - 1
         inside = 0
         rechts = 0
         i = 0
   10    continue
         i1 = mod(i,np) + 1
         i2 = mod(i1,np) + 1
         x1 = x(i1)
         x2 = x(i2)
         y1 = y(i1)
         y2 = y(i2)
         if (xl .ge. min(x1,x2) .and. xl .le. max(x1,x2) ) then
            if (xl .eq. x1 .and. yl .eq. y1 .or. &                     ! tussen of op lijnstuk
               (x1 .eq. x2 .and. &                                     ! op punt 1
                yl .ge. min(y1,y2) .and. yl .le. max(y1,y2) ) .or. &   ! op verticale lijn
               (yl .eq. y1 .and. y1 .eq. y2)  ) then                   ! op horizontale lijn    
               inside = 1
               return
            else if (x1 .ne. x2) then                                  ! scheve lijn
               rl = ( xl - x1 )  / ( x2 - x1 )
               rm = ( y1 - yl )  + rl * ( y2 - y1 )
               if (rm .eq. 0) then                                     ! op scheve lijn
                  inside = 1
                  return
               else if (rm .gt. 0d0) then                              ! onder scheve lijn
                  if (xl .eq. x1 .or. xl .eq. x2) then
                     if (x1 .gt. xl .or. x2 .gt. xl) then
                        rechts = rechts + 1
                     end if
                  end if
                  inside = 1 - inside
               end if
            end if
         end if
         i = i + 1
         if (i .lt. np) goto 10
         if (mod(rechts,2) .ne. 0) inside = 1 - inside
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
   subroutine triint_z2D( xss, yss, zss, kcsss, nss,                        &
                          x  , y  , z  , kcs  , kx , mnx, jdla , indxn, wfn )
   
       implicit none
   
   
       ! Global variables
       integer,  intent(in)                            :: nss      ! Dimension of samples
       double precision, dimension(:),   intent(in)    :: xss      ! samples
       double precision, dimension(:),   intent(in)    :: yss
       double precision, dimension(:),   intent(in)    :: zss      ! dimension: nss*kx
       integer , dimension(:),   intent(in)            :: kcsss    ! samples mask
   
   
       integer,  intent(in)                            :: mnx       ! Dimension of grid
       integer,  intent(in)                            :: kx       ! vectormax
       double precision, dimension(:),   intent(in)    :: x        ! grid
       double precision, dimension(:),   intent(in)    :: y
       double precision, dimension(:,:), intent(out)   :: z        ! dimension: nx*kx
       integer , dimension(:),   intent(in)            :: kcs      ! grid mask
       integer,  intent(in)                            :: jdla     ! refresh delauney yes /no
   
       integer , optional                              :: indxn(:,:) ! if present get weightfactors and indices
       double precision, optional                      :: wfn  (:,:)
   
       call triint_z1D( xss, yss, zss, kcsss, nss,                       &
                        x  , y  , z  , kcs  , kx , mnx, jdla, indxn, wfn )
   end subroutine triint_z2D
      !
      !
      ! ==========================================================================
      !> 
   subroutine triint_z3D( xss, yss, zss, kcsss, nss,                       &
                          x  , y  , z  , kcs  , kx , mnx, jdla, indxn, wfn )
   
       implicit none
   
   
       ! Global variables
       integer,  intent(in)                            :: nss      ! Dimension of samples
       double precision, dimension(:),   intent(in)    :: xss      ! samples
       double precision, dimension(:),   intent(in)    :: yss
       double precision, dimension(:),   intent(in)    :: zss      ! dimension: nss*kx
       integer , dimension(:),   intent(in)            :: kcsss    ! samples mask
   
   
       integer,  intent(in)                            :: mnx       ! Dimension of grid
       integer,  intent(in)                            :: kx       ! vectormax
       double precision, dimension(:),   intent(in)    :: x        ! grid
       double precision, dimension(:),   intent(in)    :: y
       double precision, dimension(:,:,:), intent(out) :: z        ! dimension: nx*kx
       integer , dimension(:),   intent(in)            :: kcs      ! grid mask
       integer,  intent(in)                            :: jdla     ! refresh delauney yes /no
   
       integer , optional                              :: indxn(:,:) ! if present get weightfactors and indices
       double precision, optional                      :: wfn  (:,:)
   
       call triint_z1D( xss, yss, zss, kcsss, nss,                       &
                        x  , y  , z  , kcs  , kx , mnx, jdla, indxn, wfn )
   
   end subroutine triint_z3D
   !
   !
   ! ==========================================================================
   !> 
   subroutine triint_z1D( xss, yss, zss, kcsss, nss,                       &
                          x  , y  , z  , kcs  , kx , mnx, jdla, indxn, wfn )
   
       
       use m_ec_basic_interpolation, only: dlaun
   
       implicit none
   
   
       ! Global variables
       integer, intent(in)                             :: nss      ! Dimension of samples
       double precision, dimension(:),  intent(in)     :: xss      ! samples
       double precision, dimension(:),  intent(in)     :: yss
       double precision, dimension(:),  intent(in)     :: zss      ! dimension: nss*kx
       integer , dimension(:),  intent(in)             :: kcsss    ! samples mask
   
       integer,  intent(in)                            :: mnx      ! Dimension of grid
       integer,  intent(in)                            :: kx       ! vectormax
       double precision, dimension(:),     intent(in)  :: x        ! grid
       double precision, dimension(:),     intent(in)  :: y
       double precision, dimension(kx*mnx), intent(out):: z        ! dimension: mnx*kx
       integer , dimension(:),     intent(in)          :: kcs      ! grid mask
       integer,  intent(in)                            :: jdla     ! refresh delauney yes /no
   
       integer , optional                              :: indxn(:,:) ! if present get weightfactors and indices
       double precision, optional                      :: wfn  (:,:)
   
       ! Local variables
   
       double precision, dimension(8)                  :: x_set
       double precision, dimension(8)                  :: y_set
       integer , dimension(8)                          :: kcs_set = 1
       double precision, dimension(4)                  :: x_extr
       double precision, dimension(4)                  :: y_extr
       double precision, dimension(4)                  :: z_extr
       double precision, dimension(3)                  :: zp
       integer , dimension(3)                          :: indxp
   
   
       double precision, dimension(:),   allocatable   :: xs
       double precision, dimension(:),   allocatable   :: ys
       double precision, dimension(:),   allocatable   :: zs
       integer ,         dimension(:),   allocatable   :: kcss
       integer                                         :: ns
       integer                                         :: k, n, jgetw, ierr ! , MOUT
   
       logical :: extra = .false. ! nu even niet
   
   !! executable statements -------------------------------------------------------
       !
       !     JDLA=1, DO DE LAUNEY
       !     JSLO=1, ALSO SLOPES RD4
   
       if (nss<1) then
          return
       end if
   
       call realloc(xs,nss+8,1)
       call realloc(ys,nss+8,1)
       call realloc(zs,nss+8,1)
       call realloc(kcss,nss+8,1)
   
       ns = 0
       do k = 1,nss
          if (kcsss(k) == 1) then
             ns     = ns + 1
             xs(ns) = xss(k)
             ys(ns) = yss(k)
             do n   = 1,kx
                zs(kx*(ns-1)+n) = zss(kx*(k-1)+n)
             enddo
             kcss(ns) = 1
          end if
       enddo
   
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
   
   
       if (jdla==1) then
         ! call dlauny(xs, ys, ns)
          call DLAUN(XS,YS,NS,1,ierr)
          
       end if
   
       jgetw = 0                                            ! niets met gewichten, doe interpolatie
       if ( present(indxn) .and. jdla .eq. 1) jgetw = 1     ! haal gewichten       doe interpolatie , gebruik gewichten
       if ( present(indxn) .and. jdla .eq. 0) jgetw = 2     !                      doe interpolatie , gebruik gewichten
   
       do n = 1,mnx
          if (kcs(n) .eq. 1) then
              if (jgetw .le. 1) then
                  call findtri_indices_weights (x(n),y( n), xs, ys, ns, zp, indxp)     ! zoeken bij 0 en 1
              end if
   
              if (jgetw .eq. 1) then                                                   ! zetten bij 1
                 do k = 1,3
                    indxn(k,n) = indxp(k)
                    wfn(k,n)   = zp(k)
                 enddo
              else if (jgetw .eq. 2) then                                              ! halen bij 2, je hoeft niet te zoeken
                 do k = 1,3
                   indxp(k)    = indxn(k,n)
                   zp(k)       = wfn(k,n)
                 enddo
              end if
   
                                                                                       ! en altijd interpoleren
   
              do k = 1,kx                                                              ! over vectormax loop
                 if (indxp(1)==0 .or. indxp(2)==0 .or. indxp(3)==0 ) then
                    !  z(mnx*(k-1) + n) = -999
                 else
                     z(mnx*(k-1) + n) = zp(1)*zs(kx*(indxp(1)-1)+k) + zp(2)*zs(kx*(indxp(2)-1)+k) + zp(3)*zs(kx*(indxp(3)-1)+k)
                 end if
              enddo
   
            end if
       enddo
   
       deallocate(xs)
       deallocate(ys)
       deallocate(zs)
       deallocate(kcss)
   
   
   end subroutine triint_z1D
   !
   !
   ! ==========================================================================
   !> 
   subroutine findtri_indices_weights(xp, yp, xs, ys, ns, zp, indxp)
   
   
       implicit none
   
   
       ! Global variables
       double precision,                intent(in)   :: xp    ! for this point
       double precision,                intent(in)   :: yp
   
       integer ,                intent(in)   :: ns
       double precision, dimension(ns), intent(in)   :: xs    ! on this set
       double precision, dimension(ns), intent(in)   :: ys
   
   
       integer , dimension(3) , intent(out)  :: indxp ! find indices to set
       double precision, dimension(3) , intent(out)  :: zp    ! and corresponding weightfactors
   
   
       ! Local variables
       integer                               :: k
       integer                               :: k1
       integer                               :: k2, n3
       integer                               :: intri
       integer                               :: nroldfind, nrfind
       double precision                  :: xtmax
       double precision                  :: xtmin
       double precision                  :: ytmax
       double precision                  :: ytmin
       double precision, dimension(3)                :: xt
       double precision, dimension(3)                :: yt
   !
       !
       data nroldfind/0/
   !
   !! executable statements -------------------------------------------------------
   !
       !
       indxp = 0
       n3 = 3
   
       5 continue
       if (nroldfind/=0) then
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
          if (xp>=xtmin .and. xp<=xtmax .and. yp>=ytmin .and. yp<=ytmax) then
             call pinpok(xp ,yp, n3, xt, yt, intri)
             if (intri==1) then
                nrfind = k
                nroldfind = nrfind
                indxp(1)  = indx(1, k)
                indxp(2)  = indx(2, k)
                indxp(3)  = indx(3, k)
                call linweight(xt ,yt ,xp ,yp, zp)
                ! write(*,*) xp, yp, k, indxp(1), indxp(2), indxp(3)
                return
             end if
          end if
       enddo
       if (nroldfind/=0) then
          nroldfind = 0
          goto 5
       end if
   end subroutine findtri_indices_weights
   !
   !
   ! ==========================================================================
   !> 
   subroutine linweight(xt ,yt ,xp ,yp, zp)
   
   
       double precision,                intent(in)   :: xp    ! for this point
       double precision,                intent(in)   :: yp
   
       double precision, dimension(3)                :: xt    ! in this triangle
       double precision, dimension(3)                :: yt
   
       double precision, dimension(3) , intent(out)  :: zp    ! the weightfactors are...
   
   
       double precision   :: a11, a12, a21, a22, b1, b2, det
   
   
       zp  = 0
       a11 = xt(2) - xt(1)
       a21 = yt(2) - yt(1)
       a12 = xt(3) - xt(1)
       a22 = yt(3) - yt(1)
       b1  = xp - xt(1)
       b2  = yp - yt(1)
   
       det = a11*a22 - a12*a21
       if (abs(det)<1E-9) then
          return
       end if
       !
       zp(2) = (  a22*b1 - a12*b2)/det
       zp(3) = ( -a21*b1 + a11*b2)/det
       zp(1) =   1d0 - zp(2) - zp(3)
   
   end subroutine linweight
   !
   !
   ! ==========================================================================
   !> 
   subroutine linear(x         ,y         ,z         ,xp        ,yp        , &
                   & zp        ,jslo      ,slo       )
       use precision
       implicit none
       !
       !
       ! COMMON variables
       !
       double precision :: dmiss
   
       data dmiss /-999d0/
   !
   ! Global variables
   !
       integer, intent(in)            :: jslo
       double precision, intent(out)              :: slo
       double precision :: xp
       double precision :: yp
       double precision :: zp
       double precision, dimension(3) :: x
       double precision, dimension(3) :: y
       double precision, dimension(3), intent(in) :: z
   !
   !
   ! Local variables
   !
   
       double precision :: a11
       double precision :: a12
       double precision :: a21
       double precision :: a22
       double precision :: a31
       double precision :: a32
       double precision :: b1
       double precision :: b2
       double precision :: det
       double precision :: r3
       double precision :: rlam
       double precision :: rmhu
       double precision :: x3
       double precision :: xn
       double precision :: xy
       double precision :: y3
       double precision :: yn
       double precision :: z3
       double precision :: zn
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
       det = a11*a22 - a12*a21
       if (abs(det)<1E-12) then
          return
       end if
       !
       rlam = (a22*b1 - a12*b2)/det
       rmhu = ( - a21*b1 + a11*b2)/det
       !
       zp = z(1) + rlam*(z(2) - z(1)) + rmhu*(z(3) - z(1))
       if (jslo==1) then
          a31 = z(2) - z(1)
          a32 = z(3) - z(1)
          x3 = (a21*a32 - a22*a31)
          y3 = -(a11*a32 - a12*a31)
          z3 = (a11*a22 - a12*a21)
          r3 = sqrt(x3*x3 + y3*y3 + z3*z3)
          if (r3/=0) then
             xn = x3/r3
             yn = y3/r3
             zn = z3/r3
             xy = sqrt(xn*xn + yn*yn)
             if (zn/=0) then
                slo = abs(xy/zn)
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
   subroutine minmax_h(x, n, xmin, xmax )  !   BEPAAL MINIMUM EN MAXIMUM VAN EEN EENDIMENSIONALE ARRAY
       use precision
       implicit none
   
   ! Global variables
   
       integer, intent(in)                :: n
       double precision, dimension(n), intent(in) :: x
       double precision                           :: xmax
       double precision                           :: xmin
   
       integer                            :: i
   
       xmin = 1E30
       xmax = -1E30
   
       do i = 1, n
          xmin = min(xmin, x(i))
          xmax = max(xmax, x(i))
       enddo
   end subroutine minmax_h
   !
   !
   ! ==========================================================================
   !> 
   subroutine get_extend2D(n, m, x, y, kcs, x_ext, y_ext)
   
   
       double precision, dimension(:,:)  :: x
       double precision, dimension(:,:)  :: y
       integer , dimension(:,:)  :: kcs
       integer                 :: n
       integer                 :: m
       double precision, dimension(:)  :: x_ext
       double precision, dimension(:)  :: y_ext
   
       call get_extend1D(n*m, x, y, kcs, x_ext, y_ext)
   
   end subroutine get_extend2D
   !
   !
   ! ==========================================================================
   !> 
   subroutine get_extend1D(n, x, y, kcs, x_ext, y_ext)
   
   
       integer                 :: n
       double precision, dimension(n)  :: x
       double precision, dimension(n)  :: y
       integer , dimension(n)  :: kcs
       double precision, dimension(4)  :: x_ext
       double precision, dimension(4)  :: y_ext
       double precision                :: x_min
       double precision                :: x_max
       double precision                :: x_dist
       double precision                :: y_min
       double precision                :: y_max
       double precision                :: y_dist
       integer                 :: index
   
       x_min =  1E30
       x_max = -1E30
       y_min =  1E30
       y_max = -1E30
   
       do index = 1, n
         if(kcs(index) == 1) then
           if(x_min > x(index)) then
             x_min = x(index)
           end if
           if(x_max < x(index)) then
             x_max = x(index)
           end if
           if(y_min > y(index)) then
             y_min = y(index)
           end if
           if(y_max < y(index)) then
             y_max = y(index)
           end if
         end if
       enddo
   
       x_dist = x_max - x_min
       y_dist = y_max - y_min
       x_min = x_min - 0.01d0*x_dist
       x_max = x_max + 0.01d0*x_dist
       y_min = y_min - 0.01d0*y_dist
       y_max = y_max + 0.01d0*y_dist
   
       x_ext(1) = x_min
       y_ext(1) = y_min
       x_ext(2) = x_min
       y_ext(2) = y_max
       x_ext(3) = x_max
       y_ext(3) = y_max
       x_ext(4) = x_max
       y_ext(4) = y_min
   
   end subroutine get_extend1D
   !
   !
   ! ==========================================================================
   !> 
   subroutine extrapolate(n, x, y, z, kcs, n_extr, x_extr, y_extr, z_extr)
   
   
       integer                            :: n
       double precision, dimension(n)     :: x
       double precision, dimension(n)     :: y
       double precision, dimension(n)     :: z
       integer , dimension(n)  :: kcs
       integer                            :: n_extr
       double precision, dimension(n_extr), target :: x_extr
       double precision, dimension(n_extr), target :: y_extr
       double precision, dimension(n_extr), target :: z_extr
       integer                            :: i_extr
       integer                            :: i_min
       double precision, pointer          :: x_a
       double precision, pointer          :: y_a
       double precision, pointer          :: z_a
       double precision                   :: dist_min
   
       dist_min = 1E30
       i_min = 0
   
       do i_extr = 1, n_extr
           x_a => x_extr(i_extr)
           y_a => y_extr(i_extr)
           z_a => z_extr(i_extr)
           call find_nearest(n, x, y, z, kcs, x_a, y_a, i_min, dist_min)
           z_a = z(i_min)
       enddo
   
   end subroutine extrapolate
   !
   !
   ! ==========================================================================
   !> 
   subroutine find_nearest2D(n, m, x, y, kcs, x_a, y_a, n_min, m_min, dist_min)
   
       use precision
   
       integer                 :: n
       integer                 :: m
       double precision, dimension(:,:)  :: x
       double precision, dimension(:,:)  :: y
       integer , dimension(:,:)  :: kcs
       integer                 :: n_min
       integer                 :: m_min
       integer                 :: i_min
       double precision                :: x_a
       double precision                :: y_a
       double precision                :: dist_min
   
       call find_nearest1D(n*m, x, y, kcs, x_a, y_a, i_min, dist_min)
   
       m_min = i_min/n
       n_min = i_min - (m_min * n)
       m_min = m_min + 1
   
   end subroutine find_nearest2D
   !
   !
   ! ==========================================================================
   !> 
   subroutine find_nearest2D_missing_value(n, m, x, y, z, kcs, x_a, y_a, n_min, m_min, dist_min)
   
       use precision
   
       integer                 :: n
       integer                 :: m
       double precision, dimension(:,:)  :: x
       double precision, dimension(:,:)  :: y
       double precision, dimension(:,:)  :: z
       integer , dimension(:,:)  :: kcs
       integer                 :: n_min
       integer                 :: m_min
       integer                 :: i_min
       double precision                :: x_a
       double precision                :: y_a
       double precision                :: dist_min
   
       call find_nearest1D_missing_value(n*m, x, y, z, kcs, x_a, y_a, i_min, dist_min)
   
       m_min = i_min/n
       n_min = i_min - (m_min * n)
       m_min = m_min + 1
   
   end subroutine find_nearest2D_missing_value
   !
   !
   ! ==========================================================================
   !> 
   subroutine find_nearest1D(n, x, y, kcs, x_a, y_a, i_min, dist_min)
   
       use precision
   
       integer                 :: n
       double precision, dimension(n)  :: x
       double precision, dimension(n)  :: y
       integer , dimension(n)  :: kcs
       integer                 :: i
       integer                 :: i_min
       double precision                :: x_a
       double precision                :: y_a
       double precision                :: dist
       double precision                :: dist_min
   
       dist_min = 1E30
       i_min = 0
   
       do i = 1, n
          if(kcs(i) == 1) then
               dist = (x(i)-x_a)**2 + (y(i)-y_a)**2
               if(dist < dist_min) then
                  dist_min = dist
                  i_min = i
               end if
           end if
       enddo
   
       dist_min = sqrt(dist_min)
   
   end subroutine find_nearest1D
   !
   !
   ! ==========================================================================
   !> 
   subroutine find_nearest1D_missing_value(n, x, y, z, kcs, x_a, y_a, i_min, dist_min)
   
       use precision
   
       integer                 :: n
       double precision, dimension(n)  :: x
       double precision, dimension(n)  :: y
       double precision, dimension(n)  :: z
       integer , dimension(n)  :: kcs
       integer                 :: i
       integer                 :: i_min
       double precision                :: x_a
       double precision                :: y_a
       double precision                :: dist
       double precision                :: dist_min
   
       dist_min = 1E30
       i_min = 0
   
       do i = 1, n
          if(kcs(i) == 1) then
               dist = (x(i)-x_a)**2 + (y(i)-y_a)**2
               if((dist < dist_min).and.(z(i)/=-999d0)) then
                  dist_min = dist
                  i_min = i
               end if
           end if
       enddo
   
       dist_min = sqrt(dist_min)
   
   end subroutine find_nearest1D_missing_value
   !
   !
   ! ==========================================================================
   !> 
   subroutine xxpolyint( xs, ys, zs ,kcs, ns,            &   ! interpolate in a polyline like way
                       x , y  ,z  ,kc , kx , mnx, jintp, xyen, indxn, wfn)
   
       implicit none
   
   
       ! Global variables
       integer, intent(in)                               :: ns       !< Dimension of polygon OR LINE BOUNDARY
       double precision, dimension(:),      intent(in)   :: xs       !< polyline point coordinates
       double precision, dimension(:),      intent(in)   :: ys
       double precision, dimension(:),      intent(in)   :: zs       !< Values at all points. Dimension: ns*kx
       integer, dimension(:),               intent(in)   :: kcs      !< polyline mask
   
       integer,                              intent(in)  :: mnx      !< Dimension of target points
       integer,                              intent(in)  :: kx       !< #values at each point (vectormax)
       double precision, dimension(:),       intent(in)  :: x        !< Grid points (where to interpolate to)
       double precision, dimension(:),       intent(in)  :: y
       double precision, dimension(kx*mnx),  intent(out) :: z        !< Output array for interpolated values. Dimension: mnx*kx
       integer , dimension(:),               intent(in)  :: kc       !< Target (grid) points mask
       integer,                              intent(in)  :: jintp    !< (Re-)interpolate if 1 (otherwise use index weights)
   
       double precision, dimension(:,:),     intent(in)  :: xyen     !< cellsize / tol
       integer,          dimension(:,:),     intent(inout), optional :: indxn   !< pli segment is identified by its first node nr.
       double precision, dimension(:,:),     intent(inout), optional :: wfn     !< If present, get weight index and factor
   
       ! locals
   
       double precision:: wL, wR
       integer :: m, k, kL, kR, jgetw
   
       jgetw = 0                                            ! niets met gewichten, doe interpolatie
       if ( present(indxn) .and. jintp .eq. 1) jgetw = 1    ! haal gewichten       doe interpolatie , gebruik gewichten
       if ( present(indxn) .and. jintp .eq. 0) jgetw = 2    !                      doe interpolatie , gebruik gewichten
   
       do m = 1, mnx
   
           if (jgetw .le. 1) then
               !call polyindexweight( x(m), y(m), xs, ys, kcs, ns, xyen(:,m), k1, rl)    ! interpolate in a polyline like way
               call polyindexweight( x(m), y(m), xyen(1,m), xyen(2,m), xs, ys, kcs, ns, kL, wL, kR, wR)    ! interpolate in a polyline like way
               !call findtri_indices_weights (x(n),y( n), xs, ys, ns, zp, indxp)     ! zoeken bij 0 en 1
               if (jgetw .eq. 1) then                                              ! zetten bij 1
                   indxn(1,m) = kL
                   wfn(1,m)   = wL
                   indxn(2,m) = kR
                   wfn(2,m)   = wR
               end if
           elseif (jgetw .eq. 2) then                                              ! halen bij 2, je hoeft niet te zoeken
               kL = indxn(1,m)
               wL = wfn(1,m)
               kR = indxn(2,m)
               wR = wfn(2,m)
           end if
   
           ! Now do the actual interpolation of data zs -> z
           if (kL > 0) then
               if (kR > 0) then
                   do k = 1,kx
                       z(kx*(m-1)+k) = wL*zs(kx*(kL-1)+k) + wR*zs(kx*(kR-1)+k)
                   end do
               else ! Just left point
                   do k = 1,kx
                       z(kx*(m-1)+k) = wL*zs(kx*(kL-1)+k)
                   end do
               end if
           else if (kR > 0) then
               do k = 1,kx
                   z(kx*(m-1)+k) =                          wR*zs(kx*(kR-1)+k)
               end do
           end if
       enddo
   
   end subroutine xxpolyint
   
   !
   ! ==========================================================================
   !> 
   !subroutine polyindexweight( xe, ye, xs, ys, kcs, ns, xyen, k1, rl)    ! interpolate in a polyline like way
   !
   ! ! Global variables
   ! integer ,                intent(in)     :: ns       ! Dimension of polygon OR LINE BOUNDARY
   ! double precision, dimension(:),  intent(in) :: xs       ! polygon
   ! double precision, dimension(:),  intent(in) :: ys
   ! integer, dimension(:),  intent(in)      :: kcs      ! polygon mask
   ! double precision                        :: xyen(:)
   ! double precision                        :: xe, ye, rl
   !
   !
   ! integer :: ja1, ja2, k, km, k1, k2
   ! double precision:: x1,x2,y1,y2,dis,xn,yn,dx,dy
   ! double precision:: dism, dis1, dis2, rl1, rl2, dbdistance
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
   subroutine polyindexweight( xe, ye, xen, yen, xs, ys, kcs, ns, kL, wL, kR, wR)
   
    use m_sferic 
    use geometry_module, only: dbdistance, cross
    use m_missing, only: dmiss
   
    ! Global variables
    integer         , intent(in)  :: ns       !< Dimension of polygon OR LINE BOUNDARY
    double precision, intent(in)  :: xs(:)    !< polygon
    double precision, intent(in)  :: ys(:)
    integer         , intent(in)  :: kcs(:)   !< polygon mask
    double precision, intent(in)  :: xe, ye   ! 
    double precision, intent(in)  :: xen, yen !< in input uitstekers, on output SL and CRP
    integer         , intent(out) :: kL       !< Index of left nearest polyline point (with kcs==1!)
    double precision, intent(out) :: wL       !< Relative weight of left nearest polyline point.
    integer         , intent(out) :: kR       !< Index of right nearest polyline point (with kcs==1!)
    double precision, intent(out) :: wR       !< Relative weight of right nearest polyline point.
   
   
   integer          :: k, km, JACROS
   double precision :: dis, disM, disL, disR !, rl1, rl2,
   double precision :: SL,SM,SMM,SLM,XCR,YCR,CRP,CRPM,DEPS
   
   DISM = huge(DISM)
   kL   = 0 ! Default: No valid point found
   kR   = 0 ! idem
   wL   = 0d0
   wR   = 0d0
   km   = 0
   crpm = 0
   disL = 0d0
   disR = 0d0
   DEPS = 1d-3
   
   do k = 1, ns-1
      
       call cross(xe, ye, xen, yen, xs(k), ys(k), xs(k+1), ys(k+1), JACROS,SL,SM,XCR,YCR,CRP,jsferic, dmiss)
       
       if (SL >= 0d0 .AND. SL <= 1D0 .AND. SM > -DEPS .AND. SM < 1.0D0+DEPS) then ! instead of jacros==1, solves firmijn's problem  
           DIS = DBDISTANCE(XE,YE, XCR, YCR, jsferic, jasfer3D, dmiss) 
           if (DIS < DISM) then ! Found a better intersection point
               DISM = DIS
               km   = k
               SMM  = SM
               SLM  = SL
               CRPM = CRP
           end if
       end if
   enddo
    
   if (km > 0) then
       dis  = dbdistance(xs(km), ys(km), xs(km+1), ys(km+1), jsferic, jasfer3D, dmiss) ! Length of this segment.
   
       ! Find nearest valid polyline point left of the intersection (i.e.: kcs(kL) == 1)
       disL = SMM*dis
       do k = km,1,-1
           if (kcs(k) == 1) then
               kL = k
               exit ! Valid point on the left (distance was already included in disL)
           else if (k > 1) then
               disL = disL + dbdistance(xs(k-1), ys(k-1), xs(k), ys(k), jsferic, jasfer3D, dmiss) ! Add entire length of this segment.
           end if
       end do
   
       ! Find nearest valid polyline point right of the intersection (i.e.: kcs(kR) == 1)
       disR = (1d0-SMM)*dis
       do k = km+1,ns
           if (kcs(k) == 1) then
               kR = k
               exit ! Valid point on the left (distance was already included in disL)
           else if (k < ns) then
               disR = disR + dbdistance(xs(k), ys(k), xs(k+1), ys(k+1), jsferic, jasfer3D, dmiss) ! Add entire length of this segment.
           end if
       end do
   end if
   
   if (kL /= 0 .and. kR /= 0) then
       wL = disR/(disL+disR)
       wR = 1d0-wL
   else if (kL /= 0) then
       wL = 1d0
   else if (kR /= 0) then
       wR = 1d0
   end if
   
   end subroutine polyindexweight
   !
   !
   ! ==========================================================================
   !> 
!LC: TODO remove
!   SUBROUTINE LINEDISq(X3,Y3,X1,Y1,X2,Y2,JA,DIS,XN,YN,rl) ! = dlinesdis2
!
!   
!   integer          :: ja
!   DOUBLE PRECISION :: X1,Y1,X2,Y2,X3,Y3,DIS,XN,YN
!   DOUBLE PRECISION :: R2,RL,X21,Y21,X31,Y31,dbdistance
!
!   ! korste afstand tot lijnelement tussen eindpunten
!   JA  = 0
!   !X21 = getdx(x1,y1,x2,y2)
!   !Y21 = getdy(x1,y1,x2,y2)
!   call getdxdy(x1,y1,x2,y2,x21,y21)  
!   !X31 = getdx(x1,y1,x3,y3)
!   !Y31 = getdy(x1,y1,x3,y3)
!   call getdxdy(x1,y1,x3,y3,x31,y31)
!   R2  = dbdistance(x2,y2,x1,y1)
!   R2  = R2*R2
!   IF (R2 .NE. 0) THEN
!      RL  = (X31*X21 + Y31*Y21) / R2
!      IF (0d0 .LE. RL .AND. RL .LE. 1d0) then
!         JA = 1
!      end if
!      XN  = X1 + RL*(x2-x1)
!      YN  = Y1 + RL*(y2-y1)
!      DIS = dbdistance(x3,y3,xn,yn)
!   end if
!   RETURN
!   END subroutine LINEDISq
end module timespace_triangle                           ! met leading dimensions 3 of 4
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
! space varying : Delft3D-FLOW format: time and fields of patm, windx, windy
!                 on Delft3D-FLOW m,n grid
! arcinfo       : time and fields on own equidistant grid
! spiderweb     : time and fields of patm, windspeed, direction op spiderweb grid
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
!!--pseudo code and references--------------------------------------------------
!
! Stef.Hummel@WlDelft.nl
! Herman.Kernkamp@WlDelft.nl
! Adri.Mourits@WlDelft.nl
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
   subroutine selectelset( filename, filetype, x, y, xyen, kc, mnx, ki, num, usemask, rrtolrel, pliname)
     
     use MessageHandling
     use m_inquire_flowgeom
     use geometry_module, only: cross
     use m_missing, only: dmiss
     use m_sferic, only: jsferic
     use m_partitioninfo, only : jampi

     implicit none

   
   
     ! arguments
     integer         , intent(in)    :: mnx        !< dimension of quantity
     double precision, intent(in)    :: x(:)       !< x   of elset of all possible points in model
     double precision, intent(in)    :: y(:)       !< y   of elset
     double precision, intent(in)    :: xyen(:,:)  !< Points on opposite edges of elementset
     integer         , intent(inout) :: kc(:)      !< kcs of elset, allowable kandidates have 1, eg. points with less links than edges
     integer         , intent(out)   :: ki(:)      !< Returned indices of allowable points (in x/y) that fall near provided data
     integer                         :: num        !< nr of points served bij this provider
   
     character(*), intent(in)        :: filename   ! file name for meteo data file
     integer     , intent(in)        :: filetype   ! spw, arcinfo, uniuvp etc
     logical,      intent(in)        :: usemask    !< Whether to use the mask array kc, or not (allows you to keep kc, but disable it for certain quantities, for example salinitybnd).
     double precision, intent(in), optional :: rrtolrel !< Optional, a more strict rrtolerance value than the global rrtol. selectelset will succeed if cross SL value <= rrtolrel
     character(len=:),allocatable, optional :: pliname  !< Optional, name (identifier) of pli
     
     ! locals
     double precision, allocatable   :: xs (:)     ! temporary array to hold polygon
     double precision, allocatable   :: ys (:)     !
     integer , allocatable           :: kcs(:)     !
     double precision                :: wL, wR
     integer                         :: kL, kR, minp, ns, m
     integer                         :: JACROS
     integer                         :: ierr
     double precision                :: SL,SM,XCR,YCR,CRP
     logical                         :: has_more_pli
   
     num = 0
   
     ki  = 0
    
     if (filetype == poly_tim) then

        call oldfil(minp, filename)
        call read1polylin(minp,xs,ys,ns,pliname, has_more_records=has_more_pli)
        
        if (has_more_pli) then
           call mess(LEVEL_WARN, 'While reading polyline file '''//trim(filename)//''': multiple polylines are not supported in a single file.')
           call mess(LEVEL_WARN, 'Only using first polyline '''//trim(pliname)//''' and ignoring the rest.')
        end if

        if (.not. allocated(kcs)) then
          allocate(kcs(ns))
        else if (ns > size(kcs)) then
          call realloc(kcs, ns, keepExisting=.false.)
        end if
   
        kcs = 1   ! todo make this safe
        
        do m = 1,mnx
           if (iabs(kc(m)) == 1) then     ! point is a possible candidate for a line boundary
              call polyindexweight( x(m), y(m),  xyen(1,m), xyen(2,m), xs, ys, kcs, ns, kL, wL, kR, wR)
              if (kL > 0 .or. kR > 0) then
                 if (present(rrtolrel)) then
                    ! x,y -> xyen =approx D + 2*rrtol * D
                    ! This bnd requests a more strict tolerance than the global rrtol, namely: D + 2*rrtolb * D, so:
                    call CROSS(x(m), y(m),  xyen(1,m), xyen(2,m), xs(kL), ys(kL), xs(kR), ys(kR), JACROS,SL,SM,XCR,YCR,CRP, jsferic, dmiss)
                    if (SL > rrtolrel) then
                       ! More strict rrtolrel check failed, so do not accept this node.
                       cycle
                    end if
                 end if
                 if (usemask .and. kc(m) .eq. -1 ) then
                    write(errormessage,'(a,i8.8,a,f12.4,a,f12.4,a)') 'Boundary link ',m,' already claimed [',(x(m)+xyen(1,m))/2.,',',(y(m)+xyen(2,m))/2.,']'
                    call mess(LEVEL_WARN, errormessage)
                    cycle
                 else
                    num     =  num + 1
                    ki(num) =  m
                    if (usemask) then ! If we don't use the mask, also don't administer this opened bnd location (e.g. for salinitybnd)
                       kc(m)   = -1                ! this tells you this point is already claimed by some bnd
                    end if
                 end if
              end if
           end if
        enddo
        write(msgbuf,'(a,a,a,i0,a)') 'boundary: ''', trim(filename), ''' opened ', num, ' cells.'
        call msg_flush()

        deallocate(xs, ys, kcs)
        
     elseif (filetype == node_id) then
        
        ierr = findlink_by_nodeid(filename, m)
        if (m <= 0) then
           if (jampi == 0) then
              errormessage = 'Boundary nodeId '''// trim(filename) // ''' was not found in the network.'
              call mess(LEVEL_WARN, errormessage)
           end if
           return
        else if (m > size(kc)) then
           errormessage = 'Boundary nodeId '''// trim(filename) // ''' exceeds the network size.'
           call mess(LEVEL_WARN, errormessage)
           return
        else
           if (usemask .and. kc(m) .eq. -1 ) then
              errormessage = 'Boundary with nodeId '''//trim(filename)//''' already claimed; Overlap with other bnds?'
              call mess(LEVEL_WARN, errormessage)
              return
           else if (usemask .and. kc(m) == 0 ) then
              errormessage = 'Boundary with nodeId '''//trim(filename)//''' is not an allowed location.  AllowBndAtBifurcation=1 might solve this.'
              call mess(LEVEL_WARN, errormessage)
              return
           else
              num     =  num + 1
              ki(num) =  m
              if (usemask) then ! If we don't use the mask, also don't administer this opened bnd location (e.g. for salinitybnd)
                 kc(m)   = -1                ! this tells you this point is already claimed by some bnd
              end if
           end if
         endif
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
   subroutine selectelset_internal_links( xz, yz, nx, ln, lnx, keg, numg, &
                                          loc_spec_type, loc_file, nump, xpin, ypin, branchindex, chainage, contactId, linktype, &
                                          xps, yps, nps, lftopol, sortLinks)
     use m_inquire_flowgeom
     use m_flowgeom, only: lnx1D, xu, yu, kcu
     use dfm_error
     use messageHandling
     use m_polygon
     
     implicit none
     
     !inputs
     double precision,           intent(in   ) :: xz(:)       !< Flow nodes center x-coordinates. (Currently unused).
     double precision,           intent(in   ) :: yz(:)       !< Flow nodes center y-coordinates. (Currently unused).
     integer,                    intent(in   ) :: nx          !< Number of flow nodes in input. (Currently unused).
     integer                   , intent(in   ) :: ln (:,:)    !< Flow link table. (Currently unused).
     integer                   , intent(in   ) :: lnx         !< Number of flow links in input. (Currently unused).
     integer                   , intent(  out) :: keg(:)      !< Output array containing the flow link numbers that were selected.
                                                              !< Size of array is responsability of call site, and filling starts at index 1 upon each call.
     integer                   , intent(  out) :: numg        !< Number of flow links that were selected (i.e., keg(1:numg) will be filled).
     integer,                    intent(in   ) :: loc_spec_type !< Type of spatial input for selecting nodes. One of: LOCTP_POLYGON_FILE, LOCTP_POLYLINE_FILE, LOCTP_POLYGON_XY , LOCTP_POLYLINE_XY, LOCTP_BRANCHID_CHAINAGE or LOCTP_CONTACTID.
     character(len=*), optional, intent(in   ) :: loc_file    !< (Optional) File name of a polyline file (when loc_spec_type==LOCTP_POLYGON_FILE).
     integer         , optional, intent(in   ) :: nump        !< (Optional) Number of points in polyline coordinate arrays xpin and ypin (when loc_spec_type==LOCTP_POLYGON_XY/LOCTP_POLYLINE_XY).
     double precision, optional, intent(in   ) :: xpin(:)     !< (Optional) Array with x-coordinates of a polygon/line, used instead of a polygon/line file (when loc_spec_type==LOCTP_POLYGON_XY/LOCTP_POLYLINE_XY).
     double precision, optional, intent(in   ) :: ypin(:)     !< (Optional) Array with y-coordinates of a polygon/line, used instead of a polygon/line file (when loc_spec_type==LOCTP_POLYGON_XY/LOCTP_POLYLINE_XY).
     integer         , optional, intent(in   ) :: branchindex !< (Optional) Branch index on which flow link is searched for (when loc_spec_type==LOCTP_BRANCHID_CHAINAGE).
     double precision, optional, intent(in   ) :: chainage    !< (Optional) Offset along specified branch (when loc_spec_type==LOCTP_BRANCHID_CHAINAGE).
     character(len=*), optional, intent(in   ) :: contactId   !< (Optional) Unique contactId for one flow link (when loc_spec_type==LOCTP_CONTACTID) (stored as mesh contact in input grid).
     integer,          optional, intent(in   ) :: linktype    !< (Optional) Limit search to specific link types: only 1D flow links (linktype==IFLTP_1D), 2D (linktype==IFLTP_2D), or both (linktype==IFLTP_ALL).
     double precision, allocatable, optional, intent(inout) :: xps(:), yps(:) !< (Optional) Arrays in which the read in polyline x,y-points can be stored (only relevant when loc_spec_type==LOCTP_POLYGON_FILE/LOCTP_POLYLINE_FILE).
     integer,          optional, intent(inout) :: nps         !< (Optional) Number of polyline points that have been read in (only relevant when loc_spec_type==LOCTP_POLYGON_FILE/LOCTP_POLYLINE_FILE).
     integer,          optional, intent(inout) :: lftopol(:)  !< (Optional) Mapping array from flow links to the polyline index that intersected that flow link (only relevant when loc_spec_type==LOCTP_POLYLINE_FILE or LOCTP_POLYLINE_XY).
     integer,          optional, intent(in   ) :: sortLinks   !< (Optional) Whether or not to sort the found flow links along the polyline path. (only relevant when loc_spec_type==LOCTP_POLYGON_FILE or LOCTP_POLYGON_XY).

     !locals
     integer :: minp, L, Lstart, Lend, opts, ierr, inp

     integer :: linktype_

     if (present(linktype)) then
        linktype_ = linktype
     else
        linktype_ = IFLTP_ALL
     end if

     numg = 0 
     if (loc_spec_type /= LOCTP_BRANCHID_CHAINAGE .and. loc_spec_type /= LOCTP_CONTACTID) then
        ! This routine uses global xpl, ypl, because of subroutine inwhichpolygon().
        call savepol()
     end if

     if (loc_spec_type == LOCTP_POLYLINE_FILE) then
        ! Single polyline only
        call oldfil(minp, loc_file)
        call read1polylin(minp,xpl,ypl,npl)
     elseif (loc_spec_type == LOCTP_POLYGON_FILE) then
        ! Multiple polygons allowed
        call oldfil(minp, loc_file)
        call reapol(minp, 0)
     else if ((loc_spec_type==LOCTP_POLYGON_XY .or. loc_spec_type==LOCTP_POLYLINE_XY) .and. present(xpin) .and. present(ypin) .and. present(nump)) then
        if (nump > 0) then
           call increasepol(nump, 0)
           xpl(1:nump) = xpin(1:nump)
           ypl(1:nump) = ypin(1:nump)
           npl = nump
        end if
     else if (loc_spec_type==LOCTP_BRANCHID_CHAINAGE .and. present(branchindex) .and. present(chainage) ) then
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
        endif
     else if (loc_spec_type==LOCTP_CONTACTID .and. present(contactId)) then
        !
        ! Match by contactId
        !
        ierr = findlink_by_contactid(contactId, L) ! NOTE: L is here assumed to be a net link number
        if (ierr==DFM_NOERR .and. L > 0) then
           keg(1) = L
           numg = 1
        else
           numg = 0
        end if
     endif
     
     if (loc_spec_type == LOCTP_POLYLINE_FILE .or. loc_spec_type == LOCTP_POLYLINE_XY) then
        !
        ! Match by polyline intersection
        !
        opts = 0
        if (present(lftopol)) then
           opts = opts+1
        endif
        if (present(sortlinks)) then
           opts = opts+2
        endif
        
        numg = 0
        select case(opts)
        case (0)
           ierr = findlink(npl, xpl, ypl, keg, numg, linktype = linktype_)
        case (1)
           ierr = findlink(npl, xpl, ypl, keg, numg, lftopol = lftopol, linktype = linktype_)
        case (2)
           ierr = findlink(npl, xpl, ypl, keg, numg, sortlinks = sortlinks, linktype = linktype_)
        case (3)
           ierr = findlink(npl, xpl, ypl, keg, numg, lftopol, sortlinks, linktype = linktype_)
        end select
     else if (loc_spec_type == LOCTP_POLYGON_FILE .or. loc_spec_type == LOCTP_POLYGON_XY) then
        !
        ! Match by inside polygon check
        !

        ! select search range for flow links
        select case(linktype_)
        case (IFLTP_1D, IFLTP_1D2D_INT, IFLTP_1D2D_LONG, IFLTP_1D2D_STREET, IFLTP_1D2D_ROOF)
           Lstart = 1
           Lend   = lnx1D
        case (IFLTP_2D)
           Lstart = lnx1D + 1
           Lend   = lnx
        case (IFLTP_ALL)
           Lstart = 1
           Lend   = lnx
        end select

        inp  = -1 
        ierr = 0
        do L  = Lstart,Lend
           if (linktype_ /= IFLTP_ALL .and. kcu(L) /= linktype_) then
              cycle
           end if

           !if (kc(n) > 0) then ! no kc masking for links (yet?) ! search allowed, (not allowed like closed pipes point etc) 
           call inwhichpolygon(xu(L), yu(L), inp)
           !end if
  
           if (inp > 0) then
              numg = numg+1
              keg(numg) = L ! Store link number
           endif   
        enddo   
     endif            
     
     if (ierr /= 0) then
        call setmessage( LEVEL_WARN, 'Internal error while reading '//trim(loc_file)//'. The number of found links exceeds the available positions.')
        call setmessage( -LEVEL_WARN, 'The contents of this polygon is ignored.')
        numg = 0
     endif
     
     if (npl > 0 .and. present(xps)) then
        if(allocated(xps)) deallocate(xps)
        if(allocated(yps)) deallocate(yps)
        call realloc(xps,100000)
        call realloc(yps,100000)
        xps = xpl ! doubles a bit with xpl for polygon file
        yps = ypl
        nps = npl
     endif

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
   use unstruc_messages
   
   implicit none
   
   double precision,           intent(in   ) :: xz(:)      !< Flow nodes center x-coordinates.
   double precision,           intent(in   ) :: yz(:)      !< Flow nodes center y-coordinates.
   integer,                    intent(in   ) :: kc(:)      !< Mask for which flow nodes are allowed for selection (1/0 = yes/no).
   integer,                    intent(in   ) :: nx         !< Number of flow nodes in input.
   integer                   , intent(  out) :: kp(:)      !< Output array containing the flow node numbers that were selected.
                                                           !< Size of array is responsability of call site, and filling starts at index 1 upon each call.
   integer                   , intent(  out) :: numsel     !< Number of flow nodes that were selected (i.e., kp(1:numsel) will be filled).
   integer,                    intent(in   ) :: loc_spec_type !< Type of spatial input for selecting nodes. One of: LOCTP_POLYGON_FILE, LOCTP_POLYGON_XY or LOCTP_BRANCHID_CHAINAGE or LOCTP_NODEID.
   character(len=*), optional, intent(in   ) :: loc_file   !< File name of a polygon file (when loc_spec_type==LOCTP_POLYGON_FILE).
   integer,          optional, intent(in   ) :: numcoord   !< Number of coordinates in input arrays (when loc_spec_type==LOCTP_POLYGON_XY).
   double precision, optional, intent(in   ) :: xpin(:)    !< Polygon x-coordinates (when loc_spec_type==LOCTP_POLYGON_XY).
   double precision, optional, intent(in   ) :: ypin(:)    !< Polygon y-coordinates (when loc_spec_type==LOCTP_POLYGON_XY).
   character(len=*), optional, intent(in   ) :: branchId   !< Branch id (when loc_spec_type==LOCTP_BRANCHID_CHAINAGE).
   double precision, optional, intent(in   ) :: chainage   !< Chainage along branch (when loc_spec_type==LOCTP_BRANCHID_CHAINAGE).
   character(len=*), optional, intent(in   ) :: nodeId     !< Node id (network node id) (when loc_spec_type==LOCTP_NODEID).
   !
   ! locals
   integer   :: minp, inp, n, nn, ierr
   !
   ! body
   
   numsel = 0

   select case(loc_spec_type)
   case (LOCTP_POLYGON_FILE)
      ! Fill npl, xpl, ypl from file
      call oldfil(minp, loc_file)
      call reapol (minp, 0)
   case (LOCTP_POLYGON_XY)
      ! Fill npl, xpl, ypl from input arrays
      call increasepol(numcoord, 0)
      xpl(1:numcoord) = xpin(1:numcoord)
      ypl(1:numcoord) = ypin(1:numcoord)
      npl = numcoord
   case (LOCTP_BRANCHID_CHAINAGE)
      ierr = findnode(branchId, chainage, n)
      if (ierr /= DFM_NOERR) then
         errormessage = 'While selecting grid points: branchId '''// trim(branchId) // ''' was not found in the network.'
         call mess(LEVEL_WARN, errormessage)
         return
      end if

      numsel     = 1
      ! TODO: UNST-5013: check for nodenr <= 0 in partitioned models.
      kp(numsel) = n
   case (LOCTP_NODEID)
      ierr = findnode(nodeId, n)
      if (ierr /= DFM_NOERR) then
         errormessage = 'While selecting grid points: nodeId '''// trim(nodeId) // ''' was not found in the network.'
         call mess(LEVEL_WARN, errormessage)
         return
      end if

      numsel     = 1
      kp(numsel) = n
   case default
      return

   end select

   if (loc_spec_type == LOCTP_POLYGON_FILE .or. loc_spec_type == LOCTP_POLYGON_XY) then
      ! Check which points are inside polygon npl-xpl-ypl
      inp  = -1 
      do n = 1,nx
         if (kc(n) > 0) then ! search allowed, (not allowed like closed pipes point etc) 
            if (npl == 1) then ! 1 point polygon: check whether point lies inside a grid cell
               nn = size(nd(n)%x)
               call pinpok (xpl(1), ypl(1), nn, nd(n)%x, nd(n)%y, inp)
            else               ! real polygon, check whether grid cell lies inside polygon
               call inwhichpolygon(xz(n), yz(n), inp)
            end if

            if (inp > 0) then
               numsel = numsel + 1
               kp(numsel) = n
            endif   
         endif   
      enddo   
   end if
   end subroutine selectelset_internal_nodes

   !
   !
   ! ==========================================================================
   !> Combine a newly computed (external forcings-)value with an existing one, based on the operand type. 
   subroutine operate(a,b,operand)
   use precision
   implicit none
   double precision, intent(inout) :: a !< Current value, will be updated based on b and operand.
   double precision, intent(in)    :: b !< New value, to be combined with existing value a.
   character(len=1), intent(in)    :: operand !< Operand type, valid values: 'O', 'A', '+', '*', 'X', 'N'.
   
   ! b = factor*b + offset ! todo doorplussen
   
   if (operand == 'O' .or. operand == 'V') then              ! Override, regardless of what was specified before 
      a = b
   else if (operand == 'A') then         ! Add, means: only if nothing was specified before   
      if (a == dmiss_default ) then 
          a = b  
      end if
   else if (a .ne. dmiss_default) then   ! algebra only if not missing 
      if (operand == '+') then 
         a = a + b
      else if (operand == '*' ) then 
         a = a * b
      else if (operand == 'X' ) then 
         a = max(a,b)
      else if (operand == 'N' ) then 
         a = min(a,b)
      end if
   end if
   end subroutine operate
   !
   !
   ! ==========================================================================
   !> 
   function timespaceinitialfield(xu, yu, zu, nx, filename, filetype, method, operand, transformcoef, iprimpos, kcc) result(success)  ! 

   use kdtree2Factory
   use m_samples
   use m_netw
   use m_flowgeom, only : ln2lne, Ln, Lnx, Wu1Duni
   use m_partitioninfo
   use unstruc_netcdf
   use m_flowexternalforcings, only: qid
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
   
   implicit none
   
   logical :: success
   
   integer,          intent(in)    :: nx
   double precision, intent(in)    :: xu(nx)
   double precision, intent(in)    :: yu(nx)
   double precision, intent(out)   :: zu(nx)
   integer         , intent(in), optional    :: kcc(nx)

   character(*),     intent(in)    :: filename   ! file name for meteo data file
   integer     ,     intent(in)    :: filetype   ! spw, arcinfo, uniuvp etc
   integer     ,     intent(in)    :: method     ! time/space interpolation method
                                                 ! 4 : inside polygon
                                                 ! 5 : triangulation
                                                 ! 6 : averaging
                                                 ! 7 : index triangulation
                                                 ! 8 : smoothing
                                                 ! 9 : internal diffusion
   character(1),     intent(in)    :: operand    ! override, add
   double precision, intent(in)    :: transformcoef(:) !< Transformation coefficients
   integer     ,     intent(in)    :: iprimpos   ! only needed for averaging, position of primitive variables in network
                                                 ! 1 = u point, cellfacemid, 2 = zeta point, cell centre, 3 = netnode

   double precision, allocatable   :: zh(:) 
   integer                         :: ierr
   integer                         :: minp0, inside,k, jdla, mout 
   double precision, allocatable   :: xx(:,:), yy(:,:)
   integer         , allocatable   :: nnn (:)

   double precision, allocatable   :: xxx(:), yyy(:)
   integer,          allocatable   :: LnnL(:), Lorg(:)
   logical, external               :: read_samples_from_geotiff

   double precision                :: zz

   integer                         :: n6 , L, Lk, n, n1, n2, i
   integer                         :: ierror, jakc
   integer                         :: jakdtree=1

   double precision                :: rcel_store, percentileminmax_store
   integer                         :: iav_store, nummin_store

   character(len=5)                :: sd

   type(TerrorInfo)                :: errorInfo

   success = .false. 
   minp0 = 0
   jakc  = 0
   if (present(kcc)) then
      jakc = 1
   endif

   if (filename == 'empty') then
      do k=1,nx
         call operate(zu(k), transformcoef(1), operand)
      enddo
   endif

   allocate(   zh(nx) , stat=ierr)
   call aerr( 'zh(nx)', ierr, nx)
   zh = dmiss_default

   if (filetype .ne. ncflow .and. filetype .ne. arcinfo .and. filetype /= geotiff) then
       call oldfil(minp0, filename)
   end if

   !if (filetype == 1dfield) then
   !   call init1dField(dataFile,filename, quantity)
   !   ! return?
   !end if

   if (method == 4) then       ! polyfil

      call savepol()
      call reapol(minp0, 0)

      inside = -1
      do k=1,nx
         if (jakc == 1) then  
            if (kcc(k) == 0) cycle
         endif
         call dbpinpol(xu(k), yu(k), inside, dmiss, JINS, NPL, xpl, ypl, zpl)
         if (inside == 1) then
            call operate(zu(k), transformcoef(1), operand)
            zh(k) = zu(k)
         end if
      enddo
      call restorepol()

   else if (method == 5 .or. method == 6) then  ! triangulation & averaging

      if (filetype == ncflow) then
          call read_flowsamples_from_netcdf(filename, qid, ierr)
      elseif (filetype == ncgrid) then
         ! TODO: support reading initial fields from NetCDF too
         msgbuf = 'timespace::timespaceinitialfield: Error while reading ''' // trim(qid) // &
            ''' from file ''' // trim(filename) // '''. File type not supported for initial fields.'
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
             call bilinarc(xu,yu,zh,nx)
          else 
             jdla = 1
             call triinterp2(xu,yu,zh,nx,jdla, XS, YS, ZS, NS, dmiss, jsferic, jins, jasfer3D, & 
                             NPL, MXSAM, MYSAM, XPL, YPL, ZPL, transformcoef, kcc)
          endif

      else if (method == 6) then                ! and this only applies to flow-link data

!         store settings
          iav_store = iav
          rcel_store = rcel
          percentileminmax_store = percentileminmax
          nummin_store = nummin

          if ( transformcoef(4) /= DMISS ) then
              iav  = int(transformcoef(4))
          end if
          if ( transformcoef(5) /= DMISS ) then
              rcel = transformcoef(5)
          end if
          if ( transformcoef(7) /= DMISS ) then
              percentileminmax = transformcoef(7)
          end if
          if ( transformcoef(8) /= DMISS ) then
             nummin = int(transformcoef(8))
          end if

          if (iprimpos == 1) then                ! primitime position = velocitypoint, cellfacemid 
               n6 = 4
               allocate( xx(n6,lnx), yy(n6,lnx), nnn(lnx) )
               do L = 1,lnx
                   xx(1,L) = xzw(ln(1,L))
                   yy(1,L) = yzw(ln(1,L))
                   xx(3,L) = xzw(ln(2,L))
                   yy(3,L) = yzw(ln(2,L))
                   Lk      = ln2lne(L)
                   xx(2,L) = xk(kn(1,Lk))
                   yy(2,L) = yk(kn(1,Lk))
                   xx(4,L) = xk(kn(2,Lk))
                   yy(4,L) = yk(kn(2,Lk))
               enddo
               nnn = 4 ! array nnn
          else if (iprimpos == 2) then           ! primitime position = waterlevelpoint, cell centre
               n6 = maxval(netcell%n)
               if ( jsferic == 1 ) then
                  n6 = n6+2   ! safety at poles
               end if

               allocate( xx(n6,nx), yy(n6,nx), nnn(nx) )

               allocate(LnnL(n6), Lorg(n6))

               do n = 1,nx
                  call get_cellpolygon(n,n6,nnn(n),rcel,xx(1,n),yy(1,n),LnnL,Lorg,zz)
               enddo
               deallocate(LnnL, Lorg)
          else if (iprimpos == 3) then           ! primitime position = netnode, cell corner

               n6 = 3*maxval(nmk)   ! 2: safe upper bound , 3 : even safer!
               allocate( xx(n6,numk), yy(n6,numk), nnn(numk), xxx(n6), yyy(n6) )
               do k = 1,numk
                  if (jakc == 0) then
                     if (kcc(k) /= 1) then
                        cycle
                     end if
                  end if

!                 get the cell list
                  call make_dual_cell(k, n6, rcel, xxx, yyy, nnn(k), Wu1Duni)
                  do i=1,nnn(k)
                     xx(i,k) = xxx(i)
                     yy(i,k) = yyy(i)
                  enddo
               enddo

               deallocate(xxx,yyy)
          end if

          if ( jakdtree == 1 ) then
!              initialize kdtree
               call build_kdtree(treeglob,Ns,xs,ys,ierror, jsferic, dmiss)
               if ( ierror /= 0 ) then
!                 disable kdtree
                  call delete_kdtree2(treeglob)
                  jakdtree = 0
               end if
          end if

          call averaging2(1,ns,xs,ys,zs,ipsam,xu,yu,zh,nx,xx,yy,n6,nnn,jakdtree,&
                             dmiss, jsferic, jasfer3D, JINS, NPL, xpl, ypl, zpl, errorInfo, kcc)
          deallocate(xx,yy,nnn)

          if (errorInfo%cntNoSamples > 0) then
              write (msgbuf,'(5a,i0,a)') 'For quantity ', trim(qid), ' in file ', trim(filename), ' no values found for ', errorInfo%cntNoSamples, ' cells/links.'
              call warn_flush()
          end if
          if (allocated(errorInfo%message)) then
             msgbuf = errorInfo%message
             call warn_flush()
          endif
          if ( .not. errorInfo%success) return

!         restore settings
          iav  = iav_store
          rcel = rcel_store
          percentileminmax = percentileminmax_store
          nummin = nummin_store

          if ( jakdtree == 1 ) then
              call delete_kdtree2(treeglob)
          end if

      end if

      do k=1,nx
         if ( zh(k) /= dmiss_default) then
             call operate(zu(k), zh(k), operand)
             zh(k) = zu(k)
         end if
      end do

!     SPvdP: sample set can be large, delete it and do not make a copy
      call delsam(-1)
      if (allocated (d) ) then 
          deallocate(d) ; mca = 0 ; nca = 0
      endif

   end if
   success = .true.
   call doclose(minp0)

   if (jawriteDFMinterpretedvalues > 0) then
      n1  = index (trim(filename), FILESEP, .true.)

      !  fix for Linux-prepared input on Windows
      if ( n1 == 0 ) then
           n1 = index(trim(filename), char(47), .true.)
      end if

      sd  = ''
      if (jampi == 1) then
          sd = '_'//trim(sdmn) 
      end if

      n2  = index (trim(filename) , '.' , .true.)
      if (n2 == 0) then 
          n2 = len_trim(filename)
      else 
          n2 = n2 -1
      end if

      call newfil(mout, trim(getoutputdir())//'DFM_interpreted_values_'//trim(filename(n1+1:n2))//trim(sd)//'.xyz')

      do k = 1,nx
         if (zh(k) /= dmiss_default) then 
            write(mout,*) xu(k), yu(k), zu(k)
         end if
      enddo
      call doclose(mout)
   endif

   if (allocated (zh) ) deallocate(zh)

   end function timespaceinitialfield

   !> Bilinear interpolation for uniform rectangular.
   !! TODO: move to ec_basic_interpolation or bilin5
   subroutine bilinarc(xk, yk, zk, n)
   use m_missing
   integer      , intent(in)    :: n
   real(kind=hp), intent(in)    :: xk(:), yk(:)
   real(kind=hp), intent(  out) :: zk(:)

   integer          :: k

   do k = 1,n
      if (zk(k) == dmiss) then 
         call bilinarcinfo( xk(k), yk(k), zk(k))
      endif 
   enddo
   end subroutine bilinarc

   !> Bilinear interpolation for uniform rectangular for 1 point
   !! TODO: move to ec_basic_interpolation or bilin5
   subroutine bilinarcinfo( x, y, z)
   use m_arcinfo
   use m_missing
   real(kind=hp), intent(in)    :: x, y
   real(kind=hp), intent(  out) :: z

   real(kind=hp)    :: dm, dn, am, an
   integer          :: m, n

   dm = (x - x0)/dxa ; m = int(dm) ; am = dm - m ; m = m + 1
   dn = (y - y0)/dya ; n = int(dn) ; an = dn - n ; n = n + 1
   z  = dmiss
   if (m < mca .and. n < nca .and. m >= 1 .and. n >= 1) then 
      if (d(m,n) .ne. dmiss .and. d(m+1,n) .ne. dmiss .and. d(m,n+1) .ne. dmiss .and. d(m+1,n+1) .ne. dmiss) then  
         z  =        am  *        an    * d(m+1 , n+1) + &
              (1d0 - am) *        an    * d(m   , n+1) + &
              (1d0 - am) * (1d0 - an)   * d(m   , n  ) + &
                     am  * (1d0 - an)   * d(m+1 , n  )
      endif 
   endif

   end subroutine bilinarcinfo

   subroutine bilinarcinfocheck( x, y, z, landsea)
   use m_arcinfo
   use m_missing
   real(kind=hp), intent(in)  :: x, y
   real(kind=hp), intent(out) :: z
   integer,       intent(out) :: landsea
   real(kind=hp)    :: dm, dn, am, an, zmx, zmn
   integer          :: m, n

   dm = (x - x0)/dxa ; m = int(dm) ; am = dm - m ; m = m + 1
   dn = (y - y0)/dya ; n = int(dn) ; an = dn - n ; n = n + 1
   z  = dmiss
   landsea = 0
   if (m < mca .and. n < nca .and. m >= 1 .and. n >= 1) then 
      z  =        am  *        an    * d(m+1 , n+1) + &
           (1d0 - am) *        an    * d(m   , n+1) + &
           (1d0 - am) * (1d0 - an)   * d(m   , n  ) + &
                  am  * (1d0 - an)   * d(m+1 , n  ) 
      zmx = dble (max( d(m+1 , n+1), d(m   , n+1), d(m   , n  ), d(m+1 , n  ) ) )
      zmn = dble (min( d(m+1 , n+1), d(m   , n+1), d(m   , n  ), d(m+1 , n  ) ) )
      if      ( zmn > 0d0 ) then ! land
         landsea = 3
      else if ( zmx < 0d0 ) then ! sea
         landsea = 2
      else                       ! coastline
         landsea = 1
      endif

   endif

   end subroutine bilinarcinfocheck



   !
   !
   ! ==========================================================================
   !> 
   function timespaceinitialfield_int(xz, yz, zz, nx, filename, filetype, method, operand, transformcoef)  result(success) ! deze subroutine moet veralgemeniseerd en naar meteo module 
      use m_missing
      use m_polygon
      use geometry_module, only: dbpinpol
      implicit none
   
      logical :: success
   
      integer,          intent(in)    :: nx
      double precision, intent(in)    :: xz(nx)
      double precision, intent(in)    :: yz(nx)
      integer         , intent(out)   :: zz(nx)
      character(*),     intent(in)    :: filename   ! file name for meteo data file
      integer     ,     intent(in)    :: filetype   ! spw, arcinfo, uniuvp etc
      integer     ,     intent(in)    :: method     ! time/space interpolation method
      character(1),     intent(in)    :: operand    ! file name for meteo data file
      double precision, intent(in)    :: transformcoef(:) !< Transformation coefficients
      integer                         :: minp0, inside, k

      success = .false.

      call oldfil(minp0, filename)
      if (filetype == inside_polygon) then       ! polyfil

         call savepol()
         call reapol(minp0, 0)
         inside = -1
         do k=1,nx
            call dbpinpol(xz(k), yz(k), inside, &
                          dmiss, JINS, NPL, xpl, ypl, zpl)  
            if (inside == 1) then
               if (operand == '+' .and. zz(k) /= imiss) then
                  zz(k)  = zz(k) + transformcoef(1)
               else
                  zz(k)  = transformcoef(1)
               end if  
            end if
         enddo
         call restorepol()

      else if (filetype == arcinfo) then  ! arcinfo bilinear todo
      else if (filetype == triangulation) then  ! triangulation    todo
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
   use m_flowtimes, only : tzone
   use m_wind
   use m_nudge
   use m_flow
   use m_waves
   use m_ship
   use m_flowexternalforcings
   use processes_input, only: nofun, funame, funinp, nosfunext, sfunname, sfuninp
   use unstruc_messages
   use m_observations
   use string_module
   use m_sediment, only: stm_included, stmpar
   use m_subsidence
   use m_fm_icecover, only: ice_af, ice_h
   
   implicit none
   
   type(tEcInstance), pointer, save :: ecInstancePtr !< FM's instance of the EC-module.
   character(maxMessageLen) :: message !< EC's message, to be passed to FM's log.
   !
   integer, dimension(:), allocatable, target :: item_tracerbnd              !< dim(numtracers)
   integer, dimension(:), allocatable, target :: item_sedfracbnd             !< dim(numfracs)
   integer, dimension(:), allocatable, target :: item_waqfun                 !< dim(nofun)  
   integer, dimension(:), allocatable, target :: item_waqsfun                !< dim(nosfunext)

   integer, target :: item_windx                                             !< Unique Item id of the ext-file's 'windx' quantity's x-component.
   integer, target :: item_windy                                             !< Unique Item id of the ext-file's 'windy' quantity's y-component.
   integer, target :: item_windxy_x                                          !< Unique Item id of the ext-file's 'windxy' quantity's x-component.
   integer, target :: item_windxy_y                                          !< Unique Item id of the ext-file's 'windxy' quantity's y-component.
  
   integer, target :: item_stressx                                           !< Unique Item id of the ext-file's 'stressx' quantity's x-component.
   integer, target :: item_stressy                                           !< Unique Item id of the ext-file's 'stressy' quantity's y-component.
   integer, target :: item_stressxy_x                                        !< Unique Item id of the ext-file's 'stressxy_x' quantity's x-component.
   integer, target :: item_stressxy_y                                        !< Unique Item id of the ext-file's 'stressxy_y' quantity's y-component.
   
   integer, target :: item_frcu                                              !< Unique Item id of the ext-file's 'frcu' quantity's component.
      
   integer, target :: item_apwxwy_p                                          !< Unique Item id of the ext-file's 'airpressure_windx_windy' quantity 'p'.
   integer, target :: item_apwxwy_x                                          !< Unique Item id of the ext-file's 'airpressure_windx_windy' quantity 'x'.
   integer, target :: item_apwxwy_y                                          !< Unique Item id of the ext-file's 'airpressure_windx_windy' quantity 'y'.
   integer, target :: item_apwxwy_c                                          !< Unique Item id of the ext-file's 'airpressure_windx_windy_charnock' quantity 'c' (space var Charnock).
   integer, target :: item_charnock                                          !< Unique Item id of the ext-file's 'space var Charnock' quantity 'C'.
   integer, target :: item_waterlevelbnd                                     !< Unique Item id of the ext-file's 'waterlevelbnd' quantity's ...-component.
   integer, target :: item_atmosphericpressure                               !< Unique Item id of the ext-file's 'atmosphericpressure' quantity
   integer, target :: item_sea_ice_area_fraction                             !< Unique Item id of the ext-file's 'sea_ice_area_fraction' quantity
   integer, target :: item_sea_ice_thickness                                 !< Unique Item id of the ext-file's 'sea_ice_thickness' quantity
   integer, target :: item_velocitybnd                                       !< Unique Item id of the ext-file's 'velocitybnd' quantity
   integer, target :: item_dischargebnd                                      !< Unique Item id of the ext-file's 'discharge' quantity
   integer, target :: item_salinitybnd                                       !< Unique Item id of the ext-file's 'salinitybnd' quantity
   integer, target :: item_temperaturebnd                                    !< Unique Item id of the ext-file's 'temperaturebnd' quantity
   integer, target :: item_sedimentbnd                                       !< Unique Item id of the ext-file's 'sedimentbnd' quantity
   integer, target :: item_tangentialvelocitybnd                             !< Unique Item id of the ext-file's 'tangentialvelocitybnd' quantity
   integer, target :: item_uxuyadvectionvelocitybnd                          !< Unique Item id of the ext-file's 'uxuyadvectionvelocitybnd'
   integer, target :: item_normalvelocitybnd                                 !< Unique Item id of the ext-file's 'normalvelocitybnd' quantity
   integer, target :: item_rainfall                                          !< Unique Item id of the ext-file's 'rainfall' quantity
   integer, target :: item_rainfall_rate                                     !< Unique Item id of the ext-file's 'rainfall_rate' quantity
   integer, target :: item_airdensity                                        !< Unique Item id of the ext-file's 'airdensity' quantity
   integer, target :: item_qhbnd                                             !< Unique Item id of the ext-file's 'qhbnd' quantity
   integer, target :: item_shiptxy                                           !< Unique Item id of the ext-file's 'shiptxy' quantity
   integer, target :: item_movingstationtxy                                  !< Unique Item id of the ext-file's 'movingstationtxy' quantity
   integer, target :: item_pump                                              !< Unique Item id of the ext-file's 'pump' quantityxy' quantity
   integer, target :: item_pump_capacity                                     !< Unique Item id of the structure file's 'pump capacity' quantity
   integer, target :: item_culvert_valveOpeningHeight                        !< Unique Item id of the structure file's 'culvert valveOpeningHeight' quantity
   integer, target :: item_weir_crestLevel                                   !< Unique Item id of the structure file's 'weir crestLevel' quantity
   integer, target :: item_orifice_crestLevel                                !< Unique Item id of the structure file's 'orifice crestLevel' quantity
   integer, target :: item_orifice_gateLowerEdgeLevel                        !< Unique Item id of the structure file's 'orifice gateLowerEdgeLevel' quantity
   integer, target :: item_general_structure_crestLevel                      !< Unique Item id of the structure file's 'general structure crestLevel' quantity
   integer, target :: item_general_structure_gateLowerEdgeLevel              !< Unique Item id of the structure file's 'general structure gateLowerEdgeLevel' quantity
   integer, target :: item_general_structure_crestWidth                      !< Unique Item id of the structure file's 'general structure crestWidth' quantity
   integer, target :: item_general_structure_gateOpeningWidth                !< Unique Item id of the structure file's 'general structure gateOpeningWidth' quantity
   integer, target :: item_longculvert_valve_relative_opening                !< Unique Item id of the structure file's 'longculvert valveRelativeOpening' quantity
   
   integer, target :: item_frcutim                                           !< Unique Item id of the friction file's 'friction_coefficient_*' quantity
   integer, target :: item_valve1D                                           !< Unique Item id of the ext-file's 'valve1D' quantxy' quantity
   integer, target :: item_damlevel                                          !< Unique Item id of the ext-file's 'damlevel' quantity
   integer, target :: item_gateloweredgelevel                                !< Unique Item id of the ext-file's 'gateloweredgelevel' quantity
   integer, target :: item_generalstructure                                  !< Unique Item id of the ext-file's 'generalstructure' quantity
   integer, target :: item_lateraldischarge                                  !< Unique Item id of the ext-file's 'generalstructure' quantity
   
   integer, target :: item_dacs_dewpoint                                     !< Unique Item id of the ext-file's 'dewpoint' quantity
   integer, target :: item_dacs_airtemperature                               !< Unique Item id of the ext-file's 'airtemperature' quantity
   integer, target :: item_dacs_cloudiness                                   !< Unique Item id of the ext-file's 'cloudiness' quantity
   integer, target :: item_dacs_solarradiation                               !< Unique Item id of the ext-file's 'solarradiation' quantity

   integer, target :: item_dac_dewpoint                                      !< Unique Item id of the ext-file's 'dewpoint' quantity
   integer, target :: item_dac_airtemperature                                !< Unique Item id of the ext-file's 'airtemperature' quantity
   integer, target :: item_dac_cloudiness                                    !< Unique Item id of the ext-file's 'cloudiness' quantity

   integer, target :: item_hacs_humidity                                     !< Unique Item id of the ext-file's 'humidity' quantity
   integer, target :: item_hacs_airtemperature                               !< Unique Item id of the ext-file's 'airtemperature' quantity
   integer, target :: item_hacs_cloudiness                                   !< Unique Item id of the ext-file's 'cloudiness' quantity
   integer, target :: item_hacs_solarradiation                               !< Unique Item id of the ext-file's 'solarradiation' quantity

   integer, target :: item_hac_humidity                                      !< Unique Item id of the ext-file's 'humidity' quantity
   integer, target :: item_hac_airtemperature                                !< Unique Item id of the ext-file's 'airtemperature' quantity
   integer, target :: item_hac_cloudiness                                    !< Unique Item id of the ext-file's 'cloudiness' quantity

   integer, target :: item_humidity                                          !< 'humidity' (or 'dewpoint') quantity
   integer, target :: item_airtemperature                                    !< 'airtemperature' quantity
   integer, target :: item_cloudiness                                        !< 'cloudiness' quantity
   integer, target :: item_solarradiation                                    !< 'solarradiation' quantity
   integer, target :: item_longwaveradiation                                 !< 'longwaveradiation' quantity
   
   integer, target :: item_discharge_salinity_temperature_sorsin             !< Unique Item id of the ext-file's 'discharge_salinity_temperature_sorsin' quantity
   integer, target :: item_hrms                                              !< Unique Item id of the ext-file's 'item_hrms' quantity
   integer, target :: item_tp                                                !< Unique Item id of the ext-file's 'item_tp' quantity
   integer, target :: item_dir                                               !< Unique Item id of the ext-file's 'item_dir' quantity
   integer, target :: item_fx                                                !< Unique Item id of the ext-file's 'item_fx' quantity
   integer, target :: item_fy                                                !< Unique Item id of the ext-file's 'item_fy' quantity
   integer, target :: item_wsbu                                              !< Unique Item id of the ext-file's 'item_wsbu' quantity
   integer, target :: item_wsbv                                              !< Unique Item id of the ext-file's 'item_wsbv' quantity
   integer, target :: item_mx                                                !< Unique Item id of the ext-file's 'item_mx' quantity
   integer, target :: item_my                                                !< Unique Item id of the ext-file's 'item_my' quantity
   integer, target :: item_dissurf                                           !< Unique Item id of the ext-file's 'item_dissurf' quantity
   integer, target :: item_diswcap                                           !< Unique Item id of the ext-file's 'item_diswcap' quantity
   integer, target :: item_distot                                            !< Unique Item id of the ext-file's 'item_distot'  quantity
   integer, target :: item_ubot                                              !< Unique Item id of the ext-file's 'item_ubot' quantity

   integer, target :: item_nudge_tem                                         !< 3D temperature for nudging
   integer, target :: item_nudge_sal                                         !< 3D salinity for nudging
   integer, target :: item_dambreakLevelsAndWidthsFromTable                  !< Dambreak heights and widths
   
   integer, target :: item_subsiduplift
   integer, target :: item_ice_cover                                         !< Unique Item id of the ext-file's 'airpressure_windx_windy' quantity 'p'.

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

   contains
   
   !> Initialize the module variables.
   subroutine init_variables()
      ecInstancePtr => null()
      message = ' '
      !
      item_windx                                 = ec_undef_int
      item_windy                                 = ec_undef_int
      item_windxy_x                              = ec_undef_int
      item_windxy_y                              = ec_undef_int
 
      item_stressx                               = ec_undef_int
      item_stressy                               = ec_undef_int
      item_stressxy_x                            = ec_undef_int
      item_stressxy_y                            = ec_undef_int

      item_frcu                                  = ec_undef_int
            
      item_apwxwy_p                              = ec_undef_int
      item_apwxwy_x                              = ec_undef_int
      item_apwxwy_y                              = ec_undef_int
      item_apwxwy_c                              = ec_undef_int
      item_charnock                              = ec_undef_int
      item_waterlevelbnd                         = ec_undef_int
      item_atmosphericpressure                   = ec_undef_int
      item_sea_ice_area_fraction                 = ec_undef_int
      item_sea_ice_thickness                     = ec_undef_int
      item_velocitybnd                           = ec_undef_int
      item_dischargebnd                          = ec_undef_int
      item_salinitybnd                           = ec_undef_int
      item_temperaturebnd                        = ec_undef_int
      item_sedimentbnd                           = ec_undef_int
      item_tangentialvelocitybnd                 = ec_undef_int
      item_uxuyadvectionvelocitybnd              = ec_undef_int
      item_normalvelocitybnd                     = ec_undef_int
      item_rainfall                              = ec_undef_int
      item_rainfall_rate                         = ec_undef_int
      item_airdensity                            = ec_undef_int
      item_qhbnd                                 = ec_undef_int
      item_shiptxy                               = ec_undef_int
      item_movingstationtxy                      = ec_undef_int
      item_pump                                  = ec_undef_int
      item_pump_capacity                         = ec_undef_int
      item_culvert_valveOpeningHeight            = ec_undef_int
      item_weir_crestLevel                       = ec_undef_int
      item_orifice_crestLevel                    = ec_undef_int
      item_orifice_gateLowerEdgeLevel            = ec_undef_int
      item_general_structure_crestLevel          = ec_undef_int
      item_general_structure_gateLowerEdgeLevel  = ec_undef_int
      item_general_structure_crestWidth          = ec_undef_int
      item_general_structure_gateOpeningWidth    = ec_undef_int
      item_longculvert_valve_relative_opening    = ec_undef_int
      item_frcutim                               = ec_undef_int
      item_valve1D                               = ec_undef_int    
      item_lateraldischarge                      = ec_undef_int
      item_damlevel                              = ec_undef_int
      item_gateloweredgelevel                    = ec_undef_int
      item_generalstructure                      = ec_undef_int
      item_dacs_dewpoint                         = ec_undef_int
      item_dacs_airtemperature                   = ec_undef_int
      item_dac_cloudiness                        = ec_undef_int
      item_dac_dewpoint                          = ec_undef_int
      item_dac_airtemperature                    = ec_undef_int
      item_dac_cloudiness                        = ec_undef_int
      item_dacs_solarradiation                   = ec_undef_int
      item_hacs_humidity                         = ec_undef_int
      item_hacs_airtemperature                   = ec_undef_int
      item_hacs_cloudiness                       = ec_undef_int
      item_hacs_solarradiation                   = ec_undef_int
      item_humidity                              = ec_undef_int
      item_airtemperature                        = ec_undef_int
      item_cloudiness                            = ec_undef_int
      item_solarradiation                        = ec_undef_int
      item_longwaveradiation                     = ec_undef_int
      item_hac_humidity                          = ec_undef_int
      item_hac_airtemperature                    = ec_undef_int
      item_hac_cloudiness                        = ec_undef_int
      item_nudge_tem                             = ec_undef_int
      item_nudge_sal                             = ec_undef_int
      item_discharge_salinity_temperature_sorsin = ec_undef_int
      item_hrms                                  = ec_undef_int
      item_tp                                    = ec_undef_int
      item_dir                                   = ec_undef_int
      item_fx                                    = ec_undef_int
      item_fy                                    = ec_undef_int
      item_wsbu                                  = ec_undef_int
      item_wsbv                                  = ec_undef_int
      item_mx                                    = ec_undef_int
      item_my                                    = ec_undef_int
      item_dissurf                               = ec_undef_int
      item_diswcap                               = ec_undef_int
      item_distot                                = ec_undef_int
      item_ubot                                  = ec_undef_int
      item_dambreakLevelsAndWidthsFromTable      = ec_undef_int 
      item_subsiduplift                          = ec_undef_int
      !
      n_qhbnd = 0
      !
      ! tracers
      if ( allocated(item_tracerbnd) ) deallocate(item_tracerbnd)
      allocate(item_tracerbnd(numtracers))
      item_tracerbnd = ec_undef_int
      !
      if ( allocated(item_sedfracbnd) ) deallocate(item_sedfracbnd)
      allocate(item_sedfracbnd(numfracs))
      item_sedfracbnd = ec_undef_int
      ! TO ADD: initial concentration field?
      
      if ( allocated(item_waqfun) ) deallocate(item_waqfun)
      allocate(item_waqfun(nofun))
      item_waqfun = ec_undef_int
      
      if ( allocated(item_waqsfun) ) deallocate(item_waqsfun)
      allocate(item_waqsfun(nosfunext))
      item_waqsfun = ec_undef_int

   end subroutine init_variables

   ! ==========================================================================
   
   !> Translate FM's meteo1 'filetype' enum to EC's 'provFile' enum.
   subroutine filetype_fm_to_ec(filetype, ec_filetype)
   use timespace_parameters
   implicit none
      integer, intent(in)  :: filetype
      integer, intent(out) :: ec_filetype
      !
      select case (filetype)
         case (uniform)             ! 1
            ec_filetype = provFile_uniform
         case (unimagdir)           ! 2
            ec_filetype = provFile_unimagdir
         case (svwp)                ! 3
            ec_filetype = provFile_svwp
         case (arcinfo)             ! 4
            ec_filetype = provFile_arcinfo
         case (spiderweb)           ! 5
            ec_filetype = provFile_spiderweb
         case (curvi)               ! 6
            ec_filetype = provFile_curvi
         case (triangulation)       ! 7
            ec_filetype = provFile_samples
         case (triangulationmagdir) ! 8
            ec_filetype = provFile_triangulationmagdir
         case (poly_tim)            ! 9
            ec_filetype = provFile_poly_tim
         case (ncgrid, ncwave)      ! 11, 14
            ec_filetype = provFile_netcdf
         case (ncflow)              ! 12
            ec_filetype = provFile_undefined ! only used for timespaceinitialfield, no EC yet.
         case (bcascii)             ! 17
            ec_filetype = provFile_bc
         case (node_id)             ! -1
            ec_filetype = provFile_bc
         case (fourier)             ! 101
            ec_filetype = provFile_fourier
         case default
            ec_filetype = provFile_undefined
      end select
   end subroutine filetype_fm_to_ec
   
   ! ==========================================================================
   
   !> Translate FM's meteo1 'method' enum to EC's 'interpolate' enum.
   subroutine method_fm_to_ec(method, ec_method)
      integer, intent(in)  :: method
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
            endif
         case (4) ! TODO: EB: FM's 4 is inside_polygon method, does EC handle this correctly if FM filetype=10?
            ec_method = interpolate_space     ! only spatial, inside polygon

         ! TODO: EB: FM does note have an interpolate_time equivalent in its method, only via filetype=uniform
         !case (5)
         !   ec_method = interpolate_time

         case (5)
            ec_method = interpolate_triangle  ! only spatial, triangulation
         case (6)
            ec_method = interpolate_unknown   ! Not yet supported: only spatial, averaging
         !case (7) ! TODO: EB+AvD: index triangulation (for spatial sedmor fields) may be needed later,
                   ! but now overlaps with interpolate_time_extrapolation_ok (for wave coupling) below.
         !   ec_method = interpolate_unknown   ! Not yet supported: only spatial, index triangulation
         case (8)
            ec_method = interpolate_unknown   ! Not yet supported: only spatial, smoothing
         case (9)
            ec_method = interpolate_unknown   ! Not yet supported: only spatial, internal diffusion
         case (10)
            ec_method = interpolate_unknown   ! Not yet supported: only initial vertical profiles
         case (7) ! TODO: EB: FM method 7, where does this come from? ! see hrms method 7
            ec_method = interpolate_time_extrapolation_ok
         case default
            ec_method = interpolate_unknown
      end select
   end subroutine method_fm_to_ec
   
   ! ==========================================================================
   
   !> Translate FM's meteo1 'operand' enum to EC's 'operand' enum.
   subroutine operand_fm_to_ec(operand, ec_operand)
      character, intent(in)  :: operand
      integer,   intent(out) :: ec_operand
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
   function fm_ext_force_name_to_ec_item(trname, sfname, waqinput, qidname,                        &
                                         itemPtr1, itemPtr2, itemPtr3, itemPtr4, &
                                         dataPtr1, dataPtr2, dataPtr3, dataPtr4  ) result(success)
      logical                               :: success
      character(len=*), intent(in)          :: trname, sfname, waqinput

      character(len=*), intent(in)          :: qidname
     
      integer,                   pointer    :: itemPtr1, itemPtr2, itemPtr3, itemPtr4
      real(hp), dimension(:),    pointer    :: dataPtr1, dataPtr2, dataPtr3, dataPtr4
      
      ! for tracers:      
      integer           :: itrac, isf, ifun, isfun
      integer, external :: findname
      
      success = .true.

      itemPtr1 => null()
      itemPtr2 => null()
      itemPtr3 => null()
      itemPtr4 => null()
      dataPtr1 => null()
      dataPtr2 => null()
      dataPtr3 => null()
      dataPtr4 => null()
      select case (trim(qidname))
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
            dataPtr1 => ice_af
         case ('sea_ice_thickness')
            itemPtr1 => item_sea_ice_thickness
            dataPtr1 => ice_h
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
         case ('friction_coefficient_time_dependent')
            itemPtr1 => item_frcu
            dataPtr1 => frcu
         case ('airpressure_windx_windy', 'airpressure_stressx_stressy')
            itemPtr1 => item_apwxwy_p
            dataPtr1 => patm
            itemPtr2 => item_apwxwy_x
            dataPtr2 => ec_pwxwy_x
            itemPtr3 => item_apwxwy_y
            dataPtr3 => ec_pwxwy_y
         case ('airpressure_windx_windy_charnock')
            itemPtr1 => item_apwxwy_p
            dataPtr1 => patm
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
         case ('velocitybnd', 'criticaloutflowbnd','weiroutflowbnd', 'absgenbnd', 'riemannubnd')
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
         case ('airpressure','atmosphericpressure')
            itemPtr1 => item_atmosphericpressure
            dataPtr1 => patm
         case ('rainfall')
            itemPtr1 => item_rainfall
            dataPtr1 => rain
         case ('rainfall_rate')
            itemPtr1 => item_rainfall_rate
            dataPtr1 => rain
         case ('airdensity')
            itemPtr1 => item_airdensity
            dataPtr1 => airdensity 
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
         case ('pump_capacity')                        ! flow1d pump
            itemPtr1 => item_pump_capacity
            dataPtr1  => qpump ! TODO: UNST-2724: needs more thinking, see issue comments.
         case ('culvert_valveOpeningHeight')           ! flow1d culvert
            itemPtr1 => item_culvert_valveOpeningHeight
            !dataPtr1  => null() ! flow1d structure has its own data structure
         case ('weir_crestLevel')                      ! flow1d weir
            itemPtr1 => item_weir_crestLevel
            !dataPtr1  => null() ! flow1d structure has its own data structure
         case ('orifice_crestLevel')                   ! flow1d orifice
            itemPtr1 => item_orifice_crestLevel
            !dataPtr1  => null() ! flow1d structure has its own data structure
         case ('orifice_gateLowerEdgeLevel')            ! flow1d orifice
            itemPtr1 => item_orifice_gateLowerEdgeLevel
            !dataPtr1  => null() ! flow1d structure has its own data structure
         case ('general_structure_crestLevel')         ! flow1d general structure
            itemPtr1 => item_general_structure_crestLevel
            !dataPtr1  => null() ! flow1d structure has its own data structure
         case ('general_structure_gateLowerEdgeLevel') ! flow1d general structure
            itemPtr1 => item_general_structure_gateLowerEdgeLevel
            !dataPtr1  => null() ! flow1d structure has its own data structure
         case ('general_structure_crestWidth')         ! flow1d general structure
            itemPtr1 => item_general_structure_crestWidth
            !dataPtr1  => null() ! flow1d structure has its own data structure
         case ('general_structure_gateOpeningWidth')   ! flow1d general structure
            itemPtr1 => item_general_structure_gateOpeningWidth
            !dataPtr1  => null() ! flow1d structure has its own data structure
         case ('longCulvert_valveRelativeOpening')
            itemPtr1 => item_longculvert_valve_relative_opening
         case ('valve1D')
            itemPtr1 => item_valve1D
         case ('damlevel')
            itemPtr1 => item_damlevel
         case ('dambreakLevelsAndWidths')      
            itemPtr1 => item_dambreakLevelsAndWidthsFromTable
            dataPtr1 => dambreakLevelsAndWidthsFromTable
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
            dataPtr1 => rhum
            itemPtr2 => item_hac_airtemperature
            dataPtr2 => tair
            itemPtr3 => item_hac_cloudiness
            dataPtr3 => clou
         case ('humidity_airtemperature_cloudiness_solarradiation')
            itemPtr1 => item_hacs_humidity
            dataPtr1 => rhum
            itemPtr2 => item_hacs_airtemperature
            dataPtr2 => tair
            itemPtr3 => item_hacs_cloudiness
            dataPtr3 => clou
            itemPtr4 => item_hacs_solarradiation 
            dataPtr4 => qrad
         case ('dewpoint_airtemperature_cloudiness')
            itemPtr1 => item_dac_dewpoint
            dataPtr1 => rhum                 ! Relative humidity array used to store dewpoints
            itemPtr2 => item_dac_airtemperature
            dataPtr2 => tair
            itemPtr3 => item_dac_cloudiness
            dataPtr3 => clou
         case ('dewpoint_airtemperature_cloudiness_solarradiation')
            itemPtr1 => item_dacs_dewpoint
            dataPtr1 => rhum                 ! Relative humidity array used to store dewpoints
            itemPtr2 => item_dacs_airtemperature
            dataPtr2 => tair
            itemPtr3 => item_dacs_cloudiness
            dataPtr3 => clou
            itemPtr4 => item_dacs_solarradiation 
            dataPtr4 => qrad
         case ('humidity')      
            itemPtr1 => item_humidity
            dataPtr1 => rhum                 ! Relative humidity                
         case ('dewpoint')                   
            itemPtr1 => item_humidity
            dataPtr1 => rhum                 ! Relative humidity array used to store dewpoints              
         case ('airtemperature')
            itemPtr1 => item_airtemperature
            dataPtr1 => tair
         case ('cloudiness')
            itemPtr1 => item_cloudiness
            dataPtr1 => clou
         case ('solarradiation')
            itemPtr1 => item_solarradiation
            dataPtr1 => qrad
         case ('longwaveradiation')
            itemPtr1 => item_longwaveradiation
            dataPtr1 => longwave
         case ('nudge_salinity_temperature')
            itemPtr2 => item_nudge_sal
            dataPtr2 => nudge_sal 
            itemPtr1 => item_nudge_tem
            dataPtr1 => nudge_tem
         case ('discharge_salinity_temperature_sorsin')
            itemPtr1 => item_discharge_salinity_temperature_sorsin
            ! Do not point to array qstss here.
            ! qstss might be reallocated after initialization (when coupled to Cosumo) 
            ! and must be an argument when calling ec_gettimespacevalue.
            nullify(dataPtr1)
         case ('hrms', 'wavesignificantheight')
            itemPtr1 => item_hrms
            dataPtr1 => hwavcom
            jamapwav_hwav = 1
         case ('tp', 'tps', 'rtp', 'waveperiod')
            itemPtr1 => item_tp
            dataPtr1 => twav
            jamapwav_twav = 1
         case ('dir', 'wavedirection')
            itemPtr1 => item_dir
            dataPtr1 => phiwav
            jamapwav_phiwav = 1
         case ('fx','xwaveforce')
            itemPtr1 => item_fx
            dataPtr1 => sxwav
            jamapwav_sxwav = 1
         case ('fy','ywaveforce')
            itemPtr1 => item_fy
            dataPtr1 => sywav
            jamapwav_sywav = 1
        case ('wsbu')
            itemPtr1 => item_wsbu
            dataPtr1 => sbxwav
            jamapwav_sxbwav = 1
         case ('wsbv')
            itemPtr1 => item_wsbv
            dataPtr1 => sbywav
            jamapwav_sybwav = 1
         case ('mx')
            itemPtr1 => item_mx
            dataPtr1 => mxwav
            jamapwav_mxwav = 1
         case ('my')
            itemPtr1 => item_my
            dataPtr1 => mywav
            jamapwav_mywav = 1
         case ('dissurf','freesurfacedissipation')
            itemPtr1 => item_dissurf
            dataPtr1 => dsurf
            jamapwav_dsurf = 1
         case ('diswcap','whitecappingdissipation')
            itemPtr1 => item_diswcap
            dataPtr1 => dwcap
            jamapwav_dwcap = 1
         case ('totalwaveenergydissipation')
            itemPtr1 => item_distot
            dataPtr1 => distot
            jamapwav_distot = 1
         case ('ubot')
            itemPtr1 => item_ubot
            dataPtr1 => uorbwav            
            jamapwav_uorb = 1
         case ('tracerbnd')
            ! get tracer (boundary) number
            itrac = findname(numtracers, trnames, trname)
            itemPtr1 => item_tracerbnd(itrac)
            dataPtr1 => bndtr(itrac)%z
         case ('sedfracbnd') 
            ! get sediment fraction (boundary) number
            isf = findname(numfracs, sfnames, sfname)
            itemPtr1 => item_sedfracbnd(isf)
            dataPtr1 => bndsf(isf)%z
         case ('waqfunction') 
            ! get sediment fraction (boundary) number
            ifun = findname(nofun, funame, waqinput)
            itemPtr1 => item_waqfun(ifun)
            dataPtr1 => funinp(ifun,:)
         case ('waqsegmentfunction') 
            ! get sediment fraction (boundary) number
            isfun = findname(nosfunext, sfunname, waqinput)
            itemPtr1 => item_waqsfun(isfun)
            dataPtr1 => sfuninp(isfun,:)
         case ('initialtracer')
            continue
         case ('friction_coefficient_Chezy', 'friction_coefficient_Manning', 'friction_coefficient_WalLlawNikuradse', &
               'friction_coefficient_WhiteColebrook', 'friction_coefficient_StricklerNikuradse', &
               'friction_coefficient_Strickler', 'friction_coefficient_deDosBijkerk')
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
   implicit none
      ! FM re-initialize call: First destroy the EC-module instance.
      if (associated(ecInstancePtr)) then
         if (.not. ecFreeInstance(ecInstancePtr)) then
            message = dumpECMessageStack(LEVEL_WARN,callback_msg)
         end if
      end if
      ! FM initialize call or second phase of re-initialize call.
      if (.not. associated(ecInstancePtr)) then
         call init_variables()
         if (.not. ecCreateInstance(ecInstancePtr)) then
            message = dumpECMessageStack(LEVEL_WARN,callback_msg)
         end if
      end if
      if (jsferic == 1) then 
         ecInstancePtr%coordsystem = EC_COORDS_SFERIC
      else
         ecInstancePtr%coordsystem = EC_COORDS_CARTESIAN
      endif

   end subroutine initialize_ec_module
   
   ! ==========================================================================
   
   !> Helper function for creating and initializing a target Item.
   function createItem(instancePtr, itemId, quantityId, elementSetId, fieldId) result(success)
      logical                          :: success      !< function status
      type(tEcInstance), pointer       :: instancePtr  !< 
      integer,           intent(inout) :: itemId       !< Unique Item id.
      integer,           intent(inout) :: quantityId   !< Unique Quantity id.
      integer,           intent(inout) :: elementSetId !< Unique ElementSet id.
      integer,           intent(inout) :: fieldId      !< Unique Field id.
      !
      success = .true.
      if (itemId == ec_undef_int) then                ! if Target Item already exists, do NOT create a new one ... 
         itemId = ecCreateItem(ecInstancePtr)
         success              = ecSetItemRole(instancePtr, itemId, itemType_target)
         if (success) success = ecSetItemQuantity(instancePtr, itemId, quantityId)
      end if
      ! ... but we would like to use the newest targetFIELD for this item, since old targetFIELDs can refer to the 
      ! wrong data location (Arr1DPtr). This happens in the case that the demand-side arrays are reallocated while 
      ! building the targets! Same is done for the elementset, so we are sure to always connect the latest 
      ! elementset to this target.
      if (success) success = ecSetItemElementSet(instancePtr, itemId, elementSetId)
      if (success) success = ecSetItemTargetField(instancePtr, itemId, fieldId)    
   end function createItem
   
   ! ==========================================================================
   
   !> Helper function for initializing a Converter.
   function initializeConverter(instancePtr, converterId, convtype, operand, method, srcmask, inputptr) result(success)
      logical                    :: success      !< function status
      type(tEcInstance), pointer :: instancePtr  !< 
      integer                    :: converterId  !< Id of the converter to be initialized
      integer                    :: convtype     !< Type of conversion
      integer                    :: operand      !< Operand (add/replace)
      integer                    :: method       !< Method of interpolation
      type (tEcMask), optional   :: srcmask      !< Mask excluding source points
      real(hp), pointer, optional:: inputptr     !< pointer to an input arg for the converter (for QHBND)
      !
      success              = ecSetConverterType(instancePtr, converterId, convtype)
      if (success) success = ecSetConverterOperand(instancePtr, converterId, operand)
      if (success) success = ecSetConverterInterpolation(instancePtr, converterId, method)
      if (present(srcmask)) then
         if (success) success = ecSetConverterMask(instancePtr, converterId, srcmask)
      end if
      if (present(inputptr)) then
         if (success) success = ecSetConverterInputPointer(instancePtr, converterId, inputptr)
      end if

   end function initializeConverter
   
   ! ==========================================================================
   
   !> Helper function for initializing a Connection.
   function initializeConnection(instancePtr, connectionId, sourceItemId, targetItemId) result(success)
      logical                          :: success      !< function status
      type(tEcInstance), pointer       :: instancePtr  !< 
      integer,           intent(inout) :: connectionId !< 
      integer,           intent(inout) :: sourceItemId !< 
      integer,           intent(inout) :: targetItemId !< 
      !
      success              = ecAddConnectionSourceItem(instancePtr, connectionId, sourceItemId)
      if (success) success = ecAddConnectionTargetItem(instancePtr, connectionId, targetItemId)
      if (success) success = ecAddItemConnection(instancePtr, targetItemId, connectionId)
   end function initializeConnection
   
   ! ==========================================================================
   
   !> Helper function for Connection initialization.
   function checkFileType(actualfiletype, requiredfiletype, name) result(success)
      logical                  :: success          !< function status
      integer,      intent(in) :: actualfiletype   !< EC-module's filetype enumeration.
      integer,      intent(in) :: requiredfiletype !< EC-module's filetype enumeration.
      character(*), intent(in) :: name             !< Name for the target Quantity.
      !
      success = .true.
      if (.not. actualfiletype == requiredfiletype) then
         message = 'm_meteo::checkFileType: Unsupported filetype for quantity '//name//'.'
         success = .false.
      end if
   end function checkFileType
   
   ! ==========================================================================
   !> Replacement function for FM's meteo1 'addtimespacerelation' function.
   logical function ec_addtimespacerelation(name, x, y, mask, vectormax, filename, filetype, method, operand, &
                                            xyen, z, pzmin, pzmax, pkbot, pktop, targetIndex, forcingfile, srcmaskfile, &
                                            dtnodal, quiet, varname, maxSearchRadius, targetMaskSelect, &
                                            tgt_data1, tgt_data2, tgt_data3, tgt_data4,  &
                                            tgt_item1, tgt_item2, tgt_item3, tgt_item4,  &
                                            multuni1,  multuni2,  multuni3,  multuni4)
      use m_ec_module, only: ecFindFileReader, ec_filetype_to_conv_type ! TODO: Refactor this private data access (UNST-703).
      use m_ec_filereader_read, only: ecParseARCinfoMask
      use m_flowparameters, only: jawave
      use m_sferic, only: jsferic
      use m_missing, only: dmiss
      use m_flowtimes, only: refdate_mjd
      use string_module, only: str_upper
      use timespace_parameters
      use timespace

      character(len=*),                 intent(in)            :: name            !< Name for the target Quantity, possibly compounded with a tracer name.
      real(hp), dimension(:),           intent(in)            :: x               !< Array of x-coordinates for the target ElementSet.
      real(hp), dimension(:),           intent(in)            :: y               !< Array of y-coordinates for the target ElementSet.
      integer,                          intent(in)            :: vectormax       !< Vector max (length of data values at each element location).
      integer,  dimension(:),           intent(in)            :: mask            !< Array of masking values for the target ElementSet.
      character(len=*),                 intent(in)            :: filename        !< File name of meteo data file.
      integer,                          intent(in)            :: filetype        !< FM's filetype enumeration.
      integer,                          intent(in)            :: method          !< FM's method enumeration.
      character(len=1),                 intent(in)            :: operand         !< FM's operand enumeration.
      real(hp),               optional, intent(in)            :: xyen(:,:)       !< FM's distance tolerance / cellsize of ElementSet.
      real(hp), dimension(:), optional, intent(in),    target :: z               !< FM's array of z/sigma coordinates
      real(hp), dimension(:), optional, pointer               :: pzmin           !< FM's array of minimal z coordinate
      real(hp), dimension(:), optional, pointer               :: pzmax           !< FM's array of maximum z coordinate
      integer,  dimension(:), optional, pointer               :: pkbot  
      integer,  dimension(:), optional, pointer               :: pktop  
      integer,                optional, intent(in)            :: targetIndex     !< target position or rank of (complete!) vector in target array
      character(len=*),       optional, intent(in)            :: forcingfile     !< file containing the forcing data for pli-file 'filename'
      character(len=*),       optional, intent(in)            :: srcmaskfile     !< file containing mask applicable to the arcinfo source data 
      real(hp),               optional, intent(in)            :: dtnodal         !< update interval for nodal factors
      logical,                optional, intent(in)            :: quiet           !< When .true., in case of errors, do not write the errors to screen/dia at the end of the routine.
      character(len=*),       optional, intent(in)            :: varname         !< variable name within filename
      real(hp),               optional, intent(in)            :: maxSearchRadius !< max search radius in case method==11
      character(len=1),       optional, intent(in)            :: targetMaskSelect !< 'i'nside (default) or 'o'utside mask polygons
      real(hp), dimension(:), optional, pointer               :: tgt_data1       !< optional pointer to the storage location for target data 1 field
      real(hp), dimension(:), optional, pointer               :: tgt_data2       !< optional pointer to the storage location for target data 2 field
      real(hp), dimension(:), optional, pointer               :: tgt_data3       !< optional pointer to the storage location for target data 3 field
      real(hp), dimension(:), optional, pointer               :: tgt_data4       !< optional pointer to the storage location for target data 4 field
      integer,                optional, intent(inout), target :: tgt_item1       !< optional target item ID 1
      integer,                optional, intent(inout), target :: tgt_item2       !< optional target item ID 2
      integer,                optional, intent(inout), target :: tgt_item3       !< optional target item ID 3
      integer,                optional, intent(inout), target :: tgt_item4       !< optional target item ID 4
      integer,                optional, intent(inout), target :: multuni1        !< multiple uni item ID 1
      integer,                optional, intent(inout), target :: multuni2        !< multiple uni item ID 2
      integer,                optional, intent(inout), target :: multuni3        !< item ID 3
      integer,                optional, intent(inout), target :: multuni4        !< item ID 4
      !
      integer :: ec_filetype !< EC-module's enumeration.
      integer :: ec_convtype !< EC-module's convType_ enumeration.
      integer :: ec_method   !< EC-module's interpolate_ enumeration.
      integer :: ec_operand  !< EC-module's operand_ enumeration.
      !
      integer :: fileReaderId   !< Unique FileReader id.
      integer :: quantityId     !< Unique Quantity id.
      integer :: elementSetId   !< Unique ElementSet id.
      integer :: fieldId        !< Unique Field id.
      integer :: fieldId_2      !< Unique Field id.
      integer :: fieldId_3      !< Unique Field id.
      integer :: fieldId_4      !< Unique Field id.
      integer :: converterId    !< Unique Converter id.
      integer :: connectionId   !< Unique Connection id.
      integer :: sourceItemId   !< Unique source item id.
      integer :: sourceItemId_2 !< Unique additional second source item id.
      integer :: sourceItemId_3 !< Unique additional third source item id.
      integer :: sourceItemId_4 !< Unique additional fourth source item id.
      integer :: ndx
      !
      character(len=maxnamelen)       :: sourceItemName           !< name of source item (as created by provider)
      character(len=maxnamelen)       :: target_name              !< Unstruc target name derived from user-specified name 
      character(len=maxnamelen)       :: location                 !< location (name) as specified in the LOCATION field of the new EXT-file
      integer,                pointer :: targetItemPtr1 => null() !< pointer to the target item id
      integer,                pointer :: targetItemPtr2 => null() !< pointer to optional second target item id (e.g. in case of windxy)
      integer,                pointer :: targetItemPtr3 => null() !< pointer to optional third target item id (e.g. in case of spiderweb)
      integer,                pointer :: targetItemPtr4 => null() !< pointer to optional fourth target item id (e.g. in case of hacs)
      real(hp), dimension(:), pointer :: dataPtr1       => null() !< Pointer to FM's 1D data arrays.
      real(hp), dimension(:), pointer :: dataPtr2       => null() !< Pointer to FM's optional extra 1D data array (e.g. in case of windxy)
      real(hp), dimension(:), pointer :: dataPtr3       => null() !< Pointer to FM's optional third 1D data array (e.g. in case of spiderweb)
      real(hp), dimension(:), pointer :: dataPtr4       => null() !< Pointer to FM's optional fourth 1D data array (e.g. in case of hacs)
      type(tEcFileReader)   , pointer :: fileReaderPtr  => null() !< 
      
      logical                       :: success
      logical                       :: quiet_
      character(len=NAMTRACLEN)     :: trname, sfname, qidname
      character (len=20)            :: waqinput
      integer, external             :: findname
      type (tEcMask)                :: srcmask
      integer                       :: itargetMaskSelect    !< 1:targetMaskSelect='i' or absent, 0:targetMaskSelect='o'
      logical                       :: exist, opened, withCharnock, withStress
      
      double precision              :: relrow, relcol
      double precision, allocatable :: transformcoef(:)
      integer                       :: row0, row1, col0, col1, ncols, nrows, issparse, Ndatasize
      character(len=128)            :: txt1, txt2, txt3
      real(hp), pointer             :: inputptr => null()

      call clearECMessage()
      ec_addtimespacerelation = .false.
      if (present(quiet)) then
         quiet_ = quiet
      else
         quiet_ = .false. ! Default: print errors at the end of routine, if no success
      end if
      
      ndx = size(x)
      
      ! ========================================================
      ! Translate FM's enumerations to EC-module's enumerations.
      ! ========================================================
      call filetype_fm_to_ec(filetype, ec_filetype)
      if (ec_filetype == provFile_undefined) then
         write (msgbuf, '(a,i0,a)') 'm_meteo::ec_addtimespacerelation: Unsupported filetype ''', filetype, &
                                    ''' for quantity '''//trim(name)//''' and file '''//trim(filename)//'''.'
         call err_flush()
         return
      end if
      call method_fm_to_ec(method, ec_method)
      if (ec_method == interpolate_unknown) then
         write (msgbuf, '(a,i0,a)') 'm_meteo::ec_addtimespacerelation: Unsupported method ''', method, &
                                    ''' for quantity '''//trim(name)//''' and file '''//trim(filename)//'''.'
         call err_flush()
         return
      end if
      call operand_fm_to_ec(operand, ec_operand)
      if (ec_operand == operand_undefined) then
         write (msgbuf, '(a,a,a)') 'm_meteo::ec_addtimespacerelation: Unsupported operand ''', operand, &
                                    ''' for quantity '''//trim(name)//''' and file '''//trim(filename)//'''.'
         call err_flush()
         return
      end if
      
      ! =================================================
      ! Convert ext file names to accepted Unstruc names.
      ! =================================================
      ! Name conversion: (targetname=qidname==name for all names, except name=tracerbndfoo --> qidname=tracerbnd)
      qidname = name
      call get_tracername(name, trname, qidname)
      call get_sedfracname(name, sfname, qidname)
      call get_waqinputname(name, waqinput, qidname)
      target_name = qidname

      call clearECMessage()
      
      ! ============================================================
      ! If BC-Type file, create filereader and source items here
      ! ============================================================
      location = filename
      if (ec_filetype == provFile_bc) then
         if (.not.ecCreateInitializeBCFileReader(ecInstancePtr, forcingfile, location, qidname, &
                                                 refdate_mjd, tzone, ec_second, fileReaderId)) then
             
            if (.not. quiet_) then
               message = dumpECMessageStack(LEVEL_WARN, callback_msg)
            end if
            message = 'Boundary '''//trim(qidname)//''', location='''//trim(location)//''', file='''//trim(forcingfile)//''' failed!' 
            call mess(LEVEL_ERROR, message)
         end if
      else
               !success = ecSetFileReaderProperties(ecInstancePtr, fileReaderId, ec_filetype, filename, refdate_mjd, tzone, ec_second, name, forcingfile=forcingfile, dtnodal=dtnodal)
               !success = ecSetFileReaderProperties(ecInstancePtr, fileReaderId, ec_filetype, filename, refdate_mjd, tzone, ec_second, name, forcingfile=forcingfile)
      ! ============================================================
      ! For the remaining types, construct the fileReader and source Items here.
      ! ============================================================
         ! first see if the file has already been opened
         inquire(file=trim(fileName), exist = exist, opened = opened)
         if (opened .and. ec_fileType==provFile_spiderweb) then                    ! double file access not allowed when using the Gnu compiler 
            fileReaderPtr => ecFindFileReader(ecInstancePtr, fileName)
            if (.not.associated(fileReaderPtr)) then
               continue
            end if
            fileReaderId = fileReaderPtr%id 
         else
               !success = ecSetFileReaderProperties(ecInstancePtr, fileReaderId, ec_filetype, filename, refdate_mjd, tzone, ec_second, name, dtnodal=dtnodal, varname=varname)
            fileReaderId = ecCreateFileReader(ecInstancePtr)
            
            fileReaderPtr => ecFindFileReader(ecInstancePtr, fileReaderId) ! TODO: Refactor this private data access (UNST-703).
      
            fileReaderPtr%vectormax = vectormax
      
            if (present(forcingfile)) then
               if (present(dtnodal)) then
                  success = ecSetFileReaderProperties(ecInstancePtr, fileReaderId, ec_filetype, filename, refdate_mjd, tzone, ec_second, name, forcingfile=forcingfile, dtnodal=dtnodal/86400.d0)
               else
                  success = ecSetFileReaderProperties(ecInstancePtr, fileReaderId, ec_filetype, filename, refdate_mjd, tzone, ec_second, name, forcingfile=forcingfile)
               end if
               !message = dumpECMessageStack(LEVEL_WARN,callback_msg)
               if (.not. success) then
                  goto 1234
               end if
               if (ecAtLeastOnePointIsCorrection) then       ! TODO: Refactor this shortcut (UNST-180).
                     ecAtLeastOnePointIsCorrection = .false. ! TODO: Refactor this shortcut (UNST-180).
                     ec_addtimespacerelation = .true.
                     return
               end if
            else
               !success = ecSetFileReaderProperties(ecInstancePtr, fileReaderId, ec_filetype, filename, refdate_mjd, tzone, ec_second, name, varname=varname)
               if (name=='qhbnd') then
                   ec_filetype = provFile_qhtable
                   success = ecSetFileReaderProperties(ecInstancePtr, fileReaderId, ec_filetype, filename(1:index(filename,'.'))//'qh', refdate_mjd, tzone, ec_second, name)
               else    
                  if (present(dtnodal)) then
                     success = ecSetFileReaderProperties(ecInstancePtr, fileReaderId, ec_filetype, filename, refdate_mjd, tzone, ec_second, name, dtnodal=dtnodal/86400.d0, varname=varname)
                  else
                     success = ecSetFileReaderProperties(ecInstancePtr, fileReaderId, ec_filetype, filename, refdate_mjd, tzone, ec_second, name, varname=varname)
                  end if
                  if (.not. success) then
                     ! message = ecGetMessage()
                     ! message = dumpECMessageStack(LEVEL_WARN,callback_msg)
                     ! NOTE: do all error dumping (if any) at the end of this routine at label 1234
         
                     ! NOTE: in relation to WAVE: all calling WAVE-related routines now pass quiet=.true. to this addtimespace routine.
                     ! When running online with WAVE and the first WAVE calculation is after the first DFlowFM calculation,
                     ! this message will be generated. This must be a warning: notify the user that DFlowFM is going to do
                     ! a calculation with zero wave values. This message should be written every time step, until proper
                     ! wave data is available. The user has to check whether this behaviour is as expected.
                     goto 1234
                  end if
               end if
            end if
         end if
      end if

      ! ==============================
      ! Construct the target Quantity.
      ! ==============================
      quantityId = ecCreateQuantity(ecInstancePtr)
      if (.not. ecSetQuantity(ecInstancePtr, quantityId, name=target_name, units=' ', vectormax=vectormax)) then
         goto 1234
      end if

      ! ================================
      ! Construct the target ElementSet.
      ! ================================
      elementSetId = ecCreateElementSet(ecInstancePtr)

      if (ec_filetype == provFile_poly_tim) then
            success              = ecSetElementSetType(ecInstancePtr, elementSetId, elmSetType_polytim)
      else
         if (jsferic==0) then
            success              = ecSetElementSetType(ecInstancePtr, elementSetId, elmSetType_cartesian)
         else
            success              = ecSetElementSetType(ecInstancePtr, elementSetId, elmSetType_spheric)
         end if
      end if

      if (success) success = ecSetElementSetXArray(ecInstancePtr, elementSetId, x)
      if (success) success = ecSetElementSetYArray(ecInstancePtr, elementSetId, y)
      if (success) success = ecSetElementSetMaskArray(ecInstancePtr, elementSetId, mask)
      if (success) success = ecSetElementSetNumberOfCoordinates(ecInstancePtr, elementSetId, size(x))
      if (present(xyen)) then
         if (success) success = ecSetElementSetXyen(ecInstancePtr, elementSetId, xyen)
      end if
      
      if (present(z)) then ! 3D
         if (present(pzmin) .and. present(pzmax)) then       ! implicitly means: target elt z-type == SIGMA
            if (success) success = ecSetElementSetZArray(ecInstancePtr, elementSetId, z, pzmin=pzmin, pzmax=pzmax, Lpointer_=.true.)
            if (success) success = ecSetElementSetvptyp(ecInstancePtr, elementSetID, BC_VPTYP_PERCBED) ! sigma layers
         else if (present(pkbot) .and. present(pktop))  then ! implicitly means: target elt z-type == Z WITH sparse kbot/ktop storage
            if (success) success = ecSetElementSetZArray(ecInstancePtr, elementSetId, z, Lpointer_=.true.)
            if (success) success = ecSetElementSetKbotKtop(ecInstancePtr, elementSetId, pkbot, pktop, Lpointer_=.true.)
            if (success) success = ecSetElementSetvptyp(ecInstancePtr, elementSetID, BC_VPTYP_ZDATUM) ! z-layers
         else
            ! ERROR .. TODO: LR
         end if

         ! add 3D settings if needed
         if (ec_filetype == provFile_poly_tim .and. (target_name == 'salinitybnd' .or. target_name == 'temperaturebnd' .or. target_name == 'tracerbnd' .or. target_name == 'sedfracbnd')) then   ! TODO JRE sediment    
            if (success) success = ecSetElementSetMaskArray(ecInstancePtr, elementSetId, mask)
            if (success) success = ecSetElementSetNumberOfCoordinates(ecInstancePtr, elementSetId, size(x))
         end if
      end if
      
      if (.not. success) then
         goto 1234
      end if
      
      ! ==============================================
      ! Construct the target field and the target item
      ! ==============================================
      ! determine which target item (id) will be created, and which FM data array has to be used
      if (.not. fm_ext_force_name_to_ec_item(trname, sfname, waqinput, qidname,                                                &
                                             targetItemPtr1, targetItemPtr2, targetItemPtr3, targetItemPtr4, &
                                             dataPtr1      , dataPtr2      , dataPtr3      , dataPtr4        )) then
         return
      end if
      continue
      
      ! Overrule hard-coded pointers to target data by optional pointers passed in the call     
      if (present(tgt_data1)) dataPtr1 => tgt_data1
      if (present(tgt_data2)) dataPtr2 => tgt_data2
      if (present(tgt_data3)) dataPtr3 => tgt_data3
      if (present(tgt_data4)) dataPtr4 => tgt_data4
                                             
      ! Overrule hard-coded pointers to target items by optional pointers passed in the call     
      if (present(tgt_item1)) targetItemPtr1 => tgt_item1
      if (present(tgt_item2)) targetItemPtr2 => tgt_item2
      if (present(tgt_item3)) targetItemPtr3 => tgt_item3
      if (present(tgt_item4)) targetItemPtr4 => tgt_item4

      ! Create the field and the target item, and if needed additional ones.
      fieldId = ecCreateField(ecInstancePtr)
      success = ecSetField1dArray(ecInstancePtr, fieldId, dataPtr1)
      if (success) success = ecSetFieldMissingValue(ecInstancePtr, fieldId, dmiss)
      if (success) success = createItem(ecInstancePtr, targetItemPtr1, quantityId, elementSetId, fieldId)
      if (present(multuni1)) then                      ! if multiple-uni item(s) specified:
         if (multuni1<0) then
            multuni1=ecInstanceCreateItem(ecInstancePtr)
            if (.not.ecSetItemRole(ecInstancePtr, multuni1, itemType_target)) return
         end if
         connectionId = ecCreateConnection(ecInstancePtr)
         if (.not.ecAddConnectionSourceItem(ecInstancePtr, connectionId, targetItemPtr1)) return        ! connecting source to new converter
         if (.not.ecAddConnectionTargetItem(ecInstancePtr, connectionId, multuni1)) return              ! connecting multuni1 as target item to the new converter
         if (.not.ecCopyItemProperty(ecInstancePtr, multuni1, targetItemPtr1, 'quantityPtr')) return    ! copying the quantity pointer to the multi uni item
         if (.not.ecAddItemConnection(ecInstancePtr, multuni1, connectionId)) return                    ! adding the new converter to multuni1
      end if
      if (associated(targetItemPtr2)) then
         ! second field (e.g. for 'windxy')
         fieldId_2 = ecCreateField(ecInstancePtr)
         if (success) success = ecSetField1dArray(ecInstancePtr, fieldId_2, dataPtr2)
         if (success) success = ecSetFieldMissingValue(ecInstancePtr, fieldId_2, dmiss)
         if (success) success = createItem(ecInstancePtr, targetItemPtr2, quantityId, elementSetId, fieldId_2)
         if (present(multuni2)) then                      ! if multiple-uni item(s) specified:
            if (multuni2<0) then
               multuni2=ecInstanceCreateItem(ecInstancePtr)
               if (.not.ecSetItemRole(ecInstancePtr, multuni2, itemType_target)) return
            end if
            connectionId = ecCreateConnection(ecInstancePtr)
            if (.not.ecAddConnectionSourceItem(ecInstancePtr, connectionId, targetItemPtr2)) return        ! connecting source to new converter
            if (.not.ecAddConnectionTargetItem(ecInstancePtr, connectionId, multuni2)) return              ! connecting multuni1 as target item to the new converter
            if (.not.ecCopyItemProperty(ecInstancePtr, multuni2, targetItemPtr2, 'quantityPtr')) return    ! copying the quantity pointer to the multi uni item
            if (.not.ecAddItemConnection(ecInstancePtr, multuni2, connectionId)) return                    ! adding the new converter to multuni1
         end if
      end if
      if (associated(targetItemPtr3)) then
         ! third field (e.g. for 'airpressure_windx_windy'
         fieldId_3 = ecCreateField(ecInstancePtr)
         if (success) success = ecSetField1dArray(ecInstancePtr, fieldId_3, dataPtr3)
         if (success) success = ecSetFieldMissingValue(ecInstancePtr, fieldId_3, dmiss)
         if (success) success = createItem(ecInstancePtr, targetItemPtr3, quantityId, elementSetId, fieldId_3)
         if (present(multuni3)) then                      ! if multiple-uni item(s) specified:
            if (multuni3<0) then
               multuni3=ecInstanceCreateItem(ecInstancePtr)
               if (.not.ecSetItemRole(ecInstancePtr, multuni3, itemType_target)) return
            end if
            connectionId = ecCreateConnection(ecInstancePtr)
            if (.not.ecAddConnectionSourceItem(ecInstancePtr, connectionId, targetItemPtr3)) return        ! connecting source to new converter
            if (.not.ecAddConnectionTargetItem(ecInstancePtr, connectionId, multuni3)) return              ! connecting multuni1 as target item to the new converter
            if (.not.ecCopyItemProperty(ecInstancePtr, multuni3, targetItemPtr3, 'quantityPtr')) return    ! copying the quantity pointer to the multi uni item
            if (.not.ecAddItemConnection(ecInstancePtr, multuni3, connectionId)) return                    ! adding the new converter to multuni1
         end if
      end if
      if (associated(targetItemPtr4)) then
         ! fourth field (e.g. for 'humidity_airtemperatur_cloudiness_solarradiation'
         fieldId_4 = ecCreateField(ecInstancePtr)
         if (success) success = ecSetField1dArray(ecInstancePtr, fieldId_4, dataPtr4)
         if (success) success = ecSetFieldMissingValue(ecInstancePtr, fieldId_4, dmiss)
         if (success) success = createItem(ecInstancePtr, targetItemPtr4, quantityId, elementSetId, fieldId_4)
         if (present(multuni4)) then                      ! if multiple-uni item(s) specified:
            if (multuni4<0) then
               multuni4=ecInstanceCreateItem(ecInstancePtr)
               if (.not.ecSetItemRole(ecInstancePtr, multuni4, itemType_target)) return
            end if
            connectionId = ecCreateConnection(ecInstancePtr)
            if (.not.ecAddConnectionSourceItem(ecInstancePtr, connectionId, targetItemPtr4)) return        ! connecting source to new converter
            if (.not.ecAddConnectionTargetItem(ecInstancePtr, connectionId, multuni4)) return              ! connecting multuni1 as target item to the new converter
            if (.not.ecCopyItemProperty(ecInstancePtr, multuni4, targetItemPtr4, 'quantityPtr')) return    ! copying the quantity pointer to the multi uni item
            if (.not.ecAddItemConnection(ecInstancePtr, multuni4, connectionId)) return                    ! adding the new converter to multuni1
         end if
      end if
      

      if (.not. success) then
         goto 1234
      end if
      
      ! ==========================
      ! Construct a new Converter.
      ! ==========================
      ec_convtype = ec_filetype_to_conv_type(ec_filetype, name)
      if (ec_convtype == convType_undefined) then
         call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported converter.')
         return
      end if
      
      converterId = ecCreateConverter(ecInstancePtr)
      
      select case(target_name)
      case ('shiptxy', 'movingstationtxy', 'discharge_salinity_temperature_sorsin', 'pump', 'valve1D', 'damlevel', 'gateloweredgelevel', 'generalstructure', 'lateral_discharge','dambreakLevelsAndWidths')
         ! for the FM 'target' arrays, the index is provided by the caller
         if (.not. present(targetIndex)) then
            message = 'Internal program error: missing targetIndex for quantity '''//trim(target_name)
            call mess(LEVEL_ERROR, message)
            return
         end if
         success = initializeConverter(ecInstancePtr, converterId, ec_convtype, operand_replace_element, ec_method)
         if (success) success = ecSetConverterElement(ecInstancePtr, converterId, targetIndex)
      case ('qhbnd')
         ! count qh boundaries
         n_qhbnd = n_qhbnd + 1
         inputptr => atqh_all(n_qhbnd)
         success = initializeConverter(ecInstancePtr, converterId, ec_convtype, operand_replace_element, interpolate_passthrough, inputptr=inputptr)
         if (success) success = ecSetConverterElement(ecInstancePtr, converterId, n_qhbnd)
         ! Each qhbnd polytim file replaces exactly one element in the target data array.
         ! Converter will put qh value in target_array(n_qhbnd)
      case ('windx', 'windy', 'windxy', 'stressxy', 'airpressure', 'atmosphericpressure', 'airpressure_windx_windy','airdensity', &
            'airpressure_windx_windy_charnock', 'charnock', 'airpressure_stressx_stressy','humidity','dewpoint','airtemperature','cloudiness','solarradiation', 'longwaveradiation')
         if (present(srcmaskfile)) then 
            if (ec_filetype == provFile_arcinfo .or. ec_filetype == provFile_curvi) then
               if (.not.ecParseARCinfoMask(srcmaskfile, srcmask, fileReaderPtr)) then
                  write (msgbuf, '(3a)') 'Error while reading mask file ''', trim(srcmaskfile),'''.'
                  call err_flush()
                  return
               endif 
               if (.not.initializeConverter(ecInstancePtr, converterId, ec_convtype, ec_operand, ec_method, srcmask=srcmask)) then 
                  write (msgbuf, '(5a)') 'Error while setting mask to converter (file=''', trim(srcmaskfile), ''', associated with meteo file ''', trim(filename), '''.'
                  call err_flush()
                  return
               end if 
            end if
         else
            if (ec_filetype == provFile_bc .and. target_name=='windxy') then
                ec_convtype = convType_unimagdir
            end if    
            success = initializeConverter(ecInstancePtr, converterId, ec_convtype, ec_operand, ec_method)
         end if
      case ('rainfall')
         if (present(srcmaskfile)) then
            if (allocated(srcmask%msk)) deallocate (srcmask%msk)
            allocate(srcmask%msk(ndx))
            if (allocated(transformcoef)) deallocate (transformcoef)
            allocate(transformcoef(1))
            if (present(targetMaskSelect)) then
               if (targetMaskSelect == 'i') then
                  itargetMaskSelect = 1
               else
                  itargetMaskSelect = 0
               end if
            else
               itargetMaskSelect = 1
            end if
            if (itargetMaskSelect == 1) then
               transformcoef = 1.0d0
               srcmask%msk = 0
            else
               transformcoef = 0.0d0
               srcmask%msk = 1
            end if
            
            success = timespaceinitialfield_int(x, y, srcmask%msk, ndx, srcmaskfile, inside_polygon, ec_method, operand, transformcoef) ! zie meteo module
            if (.not.success) then
               write (msgbuf, '(3a)') 'Error while reading mask file ''', trim(srcmaskfile),'''.'
               call err_flush()
               return
            endif 
            if (.not.initializeConverter(ecInstancePtr, converterId, ec_convtype, ec_operand, ec_method, srcmask=srcmask)) then 
               write (msgbuf, '(5a)') 'Error while setting mask to converter (file=''', trim(srcmaskfile), ''', associated with meteo file ''', trim(filename), '''.'
               call err_flush()
               return
            end if 
            if (allocated(srcmask%msk)) deallocate (srcmask%msk)
            if (allocated(transformcoef)) deallocate (transformcoef)
         else
            success = initializeConverter(ecInstancePtr, converterId, ec_convtype, ec_operand, ec_method)
         end if
      case default
         success = initializeConverter(ecInstancePtr, converterId, ec_convtype, ec_operand, ec_method)
         if (present(targetindex)) then
            success = ecSetConverterElement(ecInstancePtr, converterId, targetindex)
         end if
      end select
      
      if (.not. success) then
         goto 1234
      end if      
         
      ! ================================================================
      ! Construct a new Connection, and connect source and target Items.
      ! ================================================================
      connectionId = ecCreateConnection(ecInstancePtr)
      
      if (.not. ecSetConnectionConverter(ecInstancePtr, connectionId, converterId)) then
         goto 1234
      end if

      ! determine the source item's name
      ! note 1: this can be determined (and be improved) when creating the file reader
      ! note 2: the source item's name is set in the select case switch below. In some cases
      !         of this switch ('special cases') the source-target connections is established
      !         immediatly, and sourceItemName is NOT set.
      !         So the generic 'connect source and target' statements after the switch are
      !         only executed if sourceItemName IS set.
      !
      sourceItemName = ' '

      sourceItemId    = 0
      sourceItemId_2  = 0
      sourceItemId_3  = 0
      sourceItemId_4  = 0

      select case (target_name)
         case ('shiptxy' , 'movingstationtxy', 'discharge_salinity_temperature_sorsin')
            if (.not. checkFileType(ec_filetype, provFile_uniform, target_name)) then
               return
            end if
            ! the file reader will have created an item called 'uniform_item'
            sourceItemName = 'uniform_item'
         case ('pump','generalstructure','damlevel', 'valve1D','gateloweredgelevel','lateral_discharge','dambreakLevelsAndWidths')
            if (checkFileType(ec_filetype, provFile_uniform, target_name)) then
               !
               ! *.tim file
               !
               sourceItemId   = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'uniform_item')
               if (sourceItemId==ec_undef_int) then 
                  ! Add something to the EC message stack about missing source item 
                  return 
               endif 
               if (.not.ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)) return 
            else if (checkFileType(ec_filetype, provFile_bc, target_name)) then
               !
               ! *.bc file
               !
               sourceItemId   = ecFindItemInFileReader(ecInstancePtr, fileReaderId, target_name)
               if (sourceItemId==ec_undef_int) then 
                  ! Add something to the EC message stack about missing source item 
                  return 
               endif 
               if (.not.ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)) return 
            else if (checkFileType(ec_filetype, provFile_fourier, target_name)) then
               sourceItemId   = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'period')
               sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'magnitude')
               sourceItemId_3 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'phase')
               if ((sourceItemId==ec_undef_int) .or. (sourceItemId_2==ec_undef_int) .or. (sourceItemId_3==ec_undef_int)) then 
                  ! Add something to the EC message stack about missing source item 
                  return
               else 
                  if (.not.ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)) return 
                  if (.not.ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_2)) return 
                  if (.not.ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_3)) return 
               endif 
            else if (checkFileType(ec_filetype, provFile_poly_tim, target_name)) then
               sourceItemName = 'polytim_item'
            else 
               ! Add something to the EC message stack about mismatching filetype bla bla 
               return 
            endif 
            if (.not.ecAddConnectionTargetItem(ecInstancePtr, connectionId, targetItemPtr1)) return 
            if (.not.ecAddItemConnection(ecInstancePtr, targetItemPtr1, connectionId)) return 
         case ('qhbnd')
            if ( (.not. checkFileType(ec_filetype, provFile_poly_tim, target_name)) .and.            &  
                 (.not. checkFileType(ec_filetype, provFile_qhtable, target_name))  .and.            &
                 (.not. checkFileType(ec_filetype, provFile_bc, target_name)) ) then
               return
            end if
            if (ec_filetype == provFile_poly_tim) then
               sourceItemName = 'polytim_item'
            else if (ec_filetype == provFile_bc .or. ec_filetype == provFile_qhtable) then
               sourceItemId   = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'discharge')
               sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'waterlevel')
               sourceItemId_3 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'slope')
               sourceItemId_4 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'crossing')
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_2)
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_3)
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_4)
               if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, targetItemPtr1)
               if (success) success = ecAddItemConnection(ecInstancePtr, targetItemPtr1, connectionId)
               if (.not. success) then
                  goto 1234
               end if
            end if
         case ('velocitybnd', 'dischargebnd', 'waterlevelbnd', 'salinitybnd', 'tracerbnd',           &
               'neumannbnd', 'riemannbnd', 'absgenbnd', 'outflowbnd',                      &
               'temperaturebnd', 'sedimentbnd', 'tangentialvelocitybnd', 'uxuyadvectionvelocitybnd', & 
               'normalvelocitybnd', 'criticaloutflowbnd','weiroutflowbnd', 'sedfracbnd','riemannubnd')    
            if ( (.not. checkFileType(ec_filetype, provFile_poly_tim, target_name)) .and.            &  
                 (.not. checkFileType(ec_filetype, provFile_bc, target_name))  ) then
               return
            end if
            if (ec_filetype == provFile_poly_tim) then
               sourceItemName = 'polytim_item'
            else if (ec_filetype == provFile_bc) then
               sourceItemName = name
               call str_upper(sourceItemName)
            end if
         case ('rainfall')
            ! the name of the source item depends on the file reader
            if (ec_filetype == provFile_uniform) then
               sourceItemName = 'uniform_item'
            else if (ec_filetype == provFile_bc) then
               sourceItemName = 'RAINFALL'
            else if (ec_filetype == provFile_netcdf) then
               sourceItemName = 'precipitation_amount'
            else if (ec_filetype == provFile_curvi) then
               sourceItemName = 'curvi_source_item_1'
            else
               call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity rainfall.')
               return
            end if
            if (.not.(ecQuantitySet(ecInstancePtr, quantityId, timeint=timeint_rainfall))) return
         case ('rainfall_rate')
            ! the name of the source item depends on the file reader
            if (ec_filetype == provFile_uniform) then
               sourceItemName = 'uniform_item'
            else if (ec_filetype == provFile_bc) then
               sourceItemName = 'RAINFALL_RATE'
            else if (ec_filetype == provFile_netcdf) then
               sourceItemName = 'rainfall_rate'
            else if (ec_filetype == provFile_curvi) then
               sourceItemName = 'curvi_source_item_1'
            else
               call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity rainfall_rate.')
               return
            end if
         case ('hrms', 'tp', 'tps', 'rtp', 'dir', 'fx', 'fy', 'wsbu', 'wsbv', 'mx', 'my', 'dissurf','diswcap','ubot')
            ! the name of the source item created by the file reader will be the same as the ext.force. quant name
            sourceItemName = target_name
            ! this file contains wave data
            if(jawave==3) then
                ! wave data is read from a com.nc file produced by D-Waves which contains one time field only
                fileReaderPtr%one_time_field = .true.
            endif
            case ('wavesignificantheight', 'waveperiod', 'wavedirection', 'xwaveforce', 'ywaveforce', &
                  'freesurfacedissipation','whitecappingdissipation', 'totalwaveenergydissipation')
             ! the name of the source item created by the file reader will be the same as the ext.force. quant name
            sourceItemName = varname
         case ('airpressure', 'atmosphericpressure')
            if (ec_filetype == provFile_arcinfo) then
               sourceItemName = 'wind_p'
            else if (ec_filetype == provFile_curvi) then
               sourceItemName = 'curvi_source_item_1'
            else if (ec_filetype == provFile_uniform) then
               sourceItemName = 'uniform_item'
            else if (ec_filetype == provFile_spiderweb) then
               sourceItemName = 'p_drop'
            else if (ec_filetype == provFile_netcdf) then
               ! the arc-info file contains 'air_pressure', which is also the standard_name 
               sourceItemName  = 'air_pressure'
            else 
               call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity wind_p.')
               return
            end if
         case ('windx')
            ! the name of the source item depends on the file reader
            if (ec_filetype == provFile_arcinfo) then
               sourceItemName  = 'wind_u'
            else if (ec_filetype == provFile_curvi) then
               sourceItemName = 'curvi_source_item_1'
            else if (ec_filetype == provFile_uniform) then
               sourceItemName = 'uniform_item'
            else if (ec_filetype == provFile_netcdf) then
               sourceItemName = 'eastward_wind'
            else
               call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity windx.')
               return
            end if
         case ('windy')
            ! the name of the source item depends on the file reader
            if (ec_filetype == provFile_arcinfo) then
               sourceItemName  = 'wind_v'
            else if (ec_filetype == provFile_curvi) then
               sourceItemName = 'curvi_source_item_1'
            else if (ec_filetype == provFile_uniform) then
               sourceItemName = 'uniform_item'
            else if (ec_filetype == provFile_netcdf) then
               sourceItemName = 'northward_wind'
            else
               call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity windy.')
               return
            end if
         case ('stressx')
            if (ec_filetype == provFile_netcdf) then
               sourceItemName = 'surface_downward_eastward_stress'
            else
               call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: stressx only implemented for NetCDF.')
               return
            end if
         case ('stressy')
            if (ec_filetype == provFile_netcdf) then
               sourceItemName = 'surface_downward_northward_stress'
            else
               call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: stressy only implemented for NetCDF.')
               return
            end if
         case ('stressxy')
            if (ec_filetype == provFile_netcdf) then
               sourceItemId   = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'surface_downward_eastward_stress')
               sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'surface_downward_northward_stress')
               if (sourceItemId == ec_undef_int .or. sourceItemId_2 == ec_undef_int) then
                  goto 1234
               end if
            else
               call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: stressxy only implemented for NetCDF.')
               return
            end if
            success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
            if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_2)
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_stressxy_x)
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_stressxy_y)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_stressxy_x, connectionId)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_stressxy_y, connectionId)
        case ('charnock')
            if (ec_filetype == provFile_netcdf) then
               sourceItemId   = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'charnock')
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
            else
               call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity '//trim(target_name)//'.')
               return
            end if
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_charnock)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_charnock, connectionId)
         case ('friction_coefficient_time_dependent')
             if (ec_filetype == provFile_netcdf) then
               sourceItemName = 'friction_coefficient' 
            else
               call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: friction_coefficient_time_dependent only implemented for NetCDF.')
               return
            end if
         case ('windxy')
            ! special case: m:n converter, (for now) handle here in case switch
            if (ec_filetype == provFile_unimagdir) then
               sourceItemId  = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'uniform_item')
               success = (sourceItemId /= ec_undef_int)
               if (.not. success) then
                  goto 1234
               end if
            else if (ec_filetype == provFile_uniform) then
               sourceItemId  = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'uniform_item')
               success = (sourceItemId /= ec_undef_int)
               if (.not. success) then
                  goto 1234
               end if
            else if (ec_filetype == provFile_bc) then
               sourceItemId  = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'WINDXY')
               success = (sourceItemId /= ec_undef_int)
               if (.not. success) then
                  goto 1234
               end if
            else if (ec_filetype == provFile_netcdf) then
               sourceItemId   = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'eastward_wind')
               sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'northward_wind')
               success = (sourceItemId /= ec_undef_int .and. sourceItemId_2 /= ec_undef_int)
               if (.not. success) then
                  goto 1234
               end if
            else if (ec_filetype == provFile_spiderweb) then
               sourceItemId   = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'windspeed')
               sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'winddirection')
               success = (sourceItemId /= ec_undef_int .and. sourceItemId_2 /= ec_undef_int)
               if (.not. success) then
                  goto 1234
               end if
            else
               call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity windxy.')
               return
            end if
            if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
            if (sourceItemId_2>0) then
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_2)
            end if
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_windxy_x)
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_windxy_y)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_windxy_x, connectionId)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_windxy_y, connectionId)
         case ('airpressure_windx_windy', 'airpressure_windx_windy_charnock', 'airpressure_stressx_stressy')
            withCharnock = (target_name == 'airpressure_windx_windy_charnock')
            withStress = (target_name == 'airpressure_stressx_stressy')
            ! special case: m:n converter, (for now) handle seperately
            if (ec_filetype == provFile_curvi) then
               sourceItemId   = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'curvi_source_item_1')
               sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'curvi_source_item_2')
               sourceItemId_3 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'curvi_source_item_3')
            else if (ec_filetype == provFile_spiderweb) then
               sourceItemId   = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'windspeed')
               sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'winddirection')
               sourceItemId_3 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'p_drop')
            else if (ec_filetype == provFile_netcdf) then
               sourceItemId   = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'air_pressure')
               if ( .not. withStress) then
                  sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'eastward_wind')
                  sourceItemId_3 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'northward_wind')
               else
                  sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'surface_downward_eastward_stress')
                  sourceItemId_3 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'surface_downward_northward_stress')
               endif
               if (withCharnock) then
                  sourceItemId_4 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'charnock')
                  if (sourceItemId_4 == ec_undef_int) goto 1234
               endif
            else
               call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity ' // trim(target_name) // '.')
               return
            end if
            if (sourceItemId == ec_undef_int .or. sourceItemId_2 == ec_undef_int .or. sourceItemId_3 == ec_undef_int) then
               goto 1234
            end if
            success              = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
            if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_2)
            if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_3)
            if (success .and. withCharnock) then
                         success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_4)
            endif
            if (ec_filetype == provFile_curvi .or. ec_filetype == provFile_netcdf) then
               if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_apwxwy_p)
               if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_apwxwy_x)
               if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_apwxwy_y)
               if (success) success = ecAddItemConnection(ecInstancePtr, item_apwxwy_p, connectionId)
               if (success) success = ecAddItemConnection(ecInstancePtr, item_apwxwy_x, connectionId)
               if (success) success = ecAddItemConnection(ecInstancePtr, item_apwxwy_y, connectionId)
               if (withCharnock) then
                  if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_apwxwy_c)
                  if (success) success = ecAddItemConnection(ecInstancePtr, item_apwxwy_c, connectionId)
               endif
            else if (ec_filetype == provFile_spiderweb) then
               if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_apwxwy_x)
               if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_apwxwy_y)
               if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_apwxwy_p)
               if (success) success = ecAddItemConnection(ecInstancePtr, item_apwxwy_x, connectionId)
               if (success) success = ecAddItemConnection(ecInstancePtr, item_apwxwy_y, connectionId)
               if (success) success = ecAddItemConnection(ecInstancePtr, item_apwxwy_p, connectionId)
            end if
            if (.not. success) then
               goto 1234
            end if
         case ('humidity_airtemperature_cloudiness')
            ! special case: m:n converter, (for now) handle seperately
            if (ec_filetype == provFile_curvi .or. ec_filetype == provFile_uniform .or. ec_filetype == provFile_netcdf) then
               if (ec_filetype == provFile_curvi) then
                  sourceItemId   = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'curvi_source_item_1')
                  sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'curvi_source_item_2')
                  sourceItemId_3 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'curvi_source_item_3')
                  if (sourceItemId == ec_undef_int .or. sourceItemId_2 == ec_undef_int .or. sourceItemId_3 == ec_undef_int) then
                     goto 1234
                  end if
                  success              = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
                  if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_2)
                  if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_3)
               else if (ec_filetype == provFile_uniform) then
                  sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'uniform_item')
                  if (sourceItemId == ec_undef_int) then
                     goto 1234
                  end if
                  success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
               else if (ec_filetype == provFile_netcdf) then
                  sourceItemId   = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'relative_humidity')
                  sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'air_temperature')
                  sourceItemId_3 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'cloud_area_fraction')
                  if (sourceItemId == ec_undef_int .or. sourceItemId_2 == ec_undef_int .or. sourceItemId_3 == ec_undef_int) then
                     goto 1234 
                  end if
                  success              = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
                  if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_2)
                  if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_3)
               end if
               if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_hac_humidity)
               if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_hac_airtemperature)
               if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_hac_cloudiness)
               if (success) success = ecAddItemConnection(ecInstancePtr, item_hac_humidity, connectionId)
               if (success) success = ecAddItemConnection(ecInstancePtr, item_hac_airtemperature, connectionId)
               if (success) success = ecAddItemConnection(ecInstancePtr, item_hac_cloudiness, connectionId)
               if (.not. success) then
                  goto 1234
               end if
            else
               call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity humidity_airtemperature_cloudiness.')
               return
            end if
         case ('humidity_airtemperature_cloudiness_solarradiation')
            ! special case: m:n converter, (for now) handle seperately
            if (ec_filetype == provFile_curvi) then
               sourceItemId   = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'curvi_source_item_1')
               sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'curvi_source_item_2')
               sourceItemId_3 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'curvi_source_item_3')
               sourceItemId_4 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'curvi_source_item_4')
               if (sourceItemId   == ec_undef_int .or. sourceItemId_2 == ec_undef_int .or. &
                   sourceItemId_3 == ec_undef_int .or. sourceItemId_4 == ec_undef_int      ) then
                  goto 1234
               end if
               success              = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_2)
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_3)
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_4)
            else if (ec_filetype == provFile_uniform) then
               sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'uniform_item')
               if (sourceItemId == ec_undef_int) then
                  goto 1234
               end if
               success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
            else if (ec_filetype == provFile_netcdf) then
               sourceItemId   = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'humidity')
               sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'air_temperature')
               sourceItemId_3 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'cloud_area_fraction')
               sourceItemId_4 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'surface_net_downward_shortwave_flux')
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_2)
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_3)
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_4)
            else
               call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity '//trim(target_name)//'.')
               return
            end if

            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_hacs_humidity)
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_hacs_airtemperature)
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_hacs_cloudiness)
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_hacs_solarradiation)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_hacs_humidity, connectionId)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_hacs_airtemperature, connectionId)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_hacs_cloudiness, connectionId)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_hacs_solarradiation, connectionId)
            if (.not. success) then
               goto 1234
            end if
         case ('dewpoint_airtemperature_cloudiness')
            if (ec_filetype == provFile_netcdf) then
               sourceItemId   = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'dew_point_temperature')
               sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'air_temperature')
               sourceItemId_3 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'cloud_area_fraction')
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_2)
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_3)
               if (.not. success) goto 1234
            else if (ec_filetype == provFile_uniform) then
               sourceItemId  = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'uniform_item')
               success = (sourceItemId /= ec_undef_int)
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
            else
               call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity '//trim(target_name)//'.')
               return
            end if
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_dac_dewpoint)
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_dac_airtemperature)
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_dac_cloudiness)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_dac_dewpoint, connectionId)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_dac_airtemperature, connectionId)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_dac_cloudiness, connectionId)
         case ('dewpoint_airtemperature_cloudiness_solarradiation')
            if (ec_filetype == provFile_netcdf) then
               sourceItemId   = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'dew_point_temperature')
               sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'air_temperature')
               sourceItemId_3 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'cloud_area_fraction')
               sourceItemId_4 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'surface_net_downward_shortwave_flux')
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_2)
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_3)
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_4)
            else if (ec_filetype == provFile_uniform) then
               sourceItemId  = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'uniform_item')
               success = (sourceItemId /= ec_undef_int)
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
            else
               call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity '//trim(target_name)//'.')
               return
            end if
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_dacs_dewpoint)
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_dacs_airtemperature)
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_dacs_cloudiness)
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_dacs_solarradiation)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_dacs_dewpoint, connectionId)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_dacs_airtemperature, connectionId)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_dacs_cloudiness, connectionId)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_dacs_solarradiation, connectionId)
         case ('humidity')
            sourceItemName = 'relative_humidity'
         case ('dewpoint')
            sourceItemName = 'dew_point_temperature'
         case ('airtemperature')
            if (ec_filetype == provFile_uniform) then
               sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'uniform_item')
               if (sourceItemId == ec_undef_int) then
                  goto 1234
               end if
               success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
               if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_airtemperature)
               if (success) success = ecAddItemConnection(ecInstancePtr, item_airtemperature, connectionId)
            elseif (ec_filetype == provFile_netcdf) then
               sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'air_temperature')
               success              = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
               if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_airtemperature)
               if (success) success = ecAddItemConnection(ecInstancePtr, item_airtemperature, connectionId)
               if (.not. success) then
                  goto 1234
               end if
            else
               sourceItemName = 'air_temperature'
            end if
         case ('cloudiness')
            if (ec_filetype == provFile_netcdf) then
               sourceItemName = 'cloud_area_fraction'
            else
               sourceItemName = 'cloudiness'
            end if
         case ('airdensity')
            if (ec_filetype == provFile_netcdf) then
               sourceItemId   = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'air_density')
               success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
            else
               call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity '//trim(target_name)//'.')
               return
            end if
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_airdensity)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_airdensity, connectionId)
         case ('solarradiation')
            if (ec_filetype == provFile_netcdf) then
               sourceItemName = 'surface_net_downward_shortwave_flux'
            else
               sourceItemName = 'sw_radiation_flux'
            end if
         case ('longwaveradiation')
            sourceItemName = 'surface_net_downward_longwave_flux'
         case ('nudge_salinity_temperature')
            if (ec_filetype == provFile_netcdf) then
               sourceItemId   = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'sea_water_potential_temperature')
               sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'sea_water_salinity')
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_2)
            else
               call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity '//trim(target_name)//'.')
               return
            end if
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_nudge_tem)
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_nudge_sal)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_nudge_tem, connectionId)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_nudge_sal, connectionId)
         case ('waqfunction') 
            if (.not. checkFileType(ec_filetype, provFile_uniform, target_name)) then
               return
            end if
            ! the file reader will have created an item called 'polytim_item'
            sourceItemName = 'uniform_item'
         case ('waqsegmentfunction')
            ! the name of the source item depends on the file reader
            if (ec_filetype == provFile_netcdf) then
               sourceItemName = name
            else
               call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity '''//trim(name)//'''')
               return
            end if
         case ('initialtracer')
            if (ec_filetype == provFile_netcdf) then
               sourceItemName = name(14:)
            end if
         case ('bedrock_surface_elevation','sea_ice_area_fraction','sea_ice_thickness')
            if (ec_filetype == provFile_arcinfo) then
               sourceItemName = name
            else if (ec_filetype == provFile_curvi) then
               sourceItemName = 'curvi_source_item_1'
            else if (ec_filetype == provFile_netcdf) then
               sourceItemName = name
            else if (ec_filetype == provFile_uniform) then
               sourceItemName = 'uniform_item'
            else 
               call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity '//trim(name)//'.')
               return
            end if   
         case default
            fileReaderPtr => ecFindFileReader(ecInstancePtr, fileReaderId)
            if (fileReaderPtr%nitems>=1) then 
               sourceItemId = fileReaderPtr%items(1)%ptr%id
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
               if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, targetItemPtr1)
               if (success) success = ecAddItemConnection(ecInstancePtr, targetItemPtr1, connectionId)
               if (fileReaderPtr%nitems>=2) then
                  sourceItemId_2 = fileReaderPtr%items(2)%ptr%id
                  if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_2)
                  if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, targetItemPtr2)
                  if (success) success = ecAddItemConnection(ecInstancePtr, targetItemPtr2, connectionId)
                  if (fileReaderPtr%nitems>=3) then
                     sourceItemId_3 = fileReaderPtr%items(3)%ptr%id
                     if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_3)
                     if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, targetItemPtr3)
                     if (success) success = ecAddItemConnection(ecInstancePtr, targetItemPtr3, connectionId)
                     if (fileReaderPtr%nitems>=4) then
                        sourceItemId_4 = fileReaderPtr%items(4)%ptr%id
                        if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_4)
                        if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, targetItemPtr4)
                        if (success) success = ecAddItemConnection(ecInstancePtr, targetItemPtr4, connectionId)
                     endif
                  endif
               endif
               if (success) then
                  ! all statements executed successfully ... this must be good
                  ec_addtimespacerelation = .true.
               else
                  call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Error while default processing of ext-file (connect source and target) for : '//trim(target_name)//'.')
               endif
            else
               call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported quantity specified in ext-file (connect source and target): '//trim(target_name)//'.')
            endif
            return
      end select

      if (sourceItemName /= ' ') then
         ! not a special case, connect source and target
         sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, sourceItemName)
         if (sourceItemId == ec_undef_int) then
            goto 1234
         end if
         if (.not. initializeConnection(ecInstancePtr, connectionId, sourceItemId , targetItemPtr1)) then
            goto 1234
         end if
         if (present(targetIndex)) then
            if (.not. checkVectorMax(ecInstancePtr, sourceItemId , targetItemPtr1)) then
               goto 1234
            endif
         endif
      end if
      
      success = ecSetConnectionIndexWeights(ecInstancePtr, connectionId)

      if ( target_name=='nudge_salinity_temperature' ) then
         call ecConverterGetBbox(ecInstancePtr, SourceItemID, 0, col0, col1, row0, row1, ncols, nrows, issparse, Ndatasize)
         relcol = dble(col1-col0+1)/dble(ncols)
         relrow = dble(row1-row0+1)/dble(nrows)
         write(txt1,"('nudge_salinity_temperature: bounding box')")
         write(txt2,"('col0-col1 X row0-row1 = ', I0, '-', I0, ' X ', I0, '-', I0, ', ncols X nrows = ', I0, ' X ', I0)") col0, col1, row0, row1, ncols, nrows
         write(txt3,"('relcol X relrow = ', F4.2, ' X ', F4.2, ' = ', F4.2)") relcol, relrow, relcol*relrow
         call mess(LEVEL_INFO, trim(txt1) // ' ' // trim(txt2) // ', ' // trim(txt3))
         
         if ( issparse.eq.1 ) then
            write(txt1,"('sparse: data size = ', I0, ', ncols X nrows = ', I0, ' X ', I0, ' = ', I0)") Ndatasize, ncols, nrows, ncols*nrows
            write(txt2,"('factor = ', F4.2)") dble(Ndatasize)/dble(Ncols*Nrows)
            call mess(LEVEL_INFO, trim(txt1) // ' ' // trim(txt2))
         end if
      end if

      ec_addtimespacerelation = .true.
      return
      
      ! Error handling.
1234  continue
      ec_addtimespacerelation = .false.
!     message = ecGetMessage()

      if (.not. quiet_) then
         ! TODO: AvD: I'd rather have a full message stack that will combine EC + meteo + dflowfm, and any caller may print any pending messages.
         ! For now: Print the EC message stack here, and leave the rest to the caller.
         ! TODO: RL: the message below is from m_meteo::message, whereas timespace::getmeteoerror() returns timespace::errormessage. So now this message here is lost/never printed at call site.
         message = dumpECMessageStack(LEVEL_WARN, callback_msg)
         ! Leave this concluding message for the caller to print or not. (via getmeteoerror())
      end if
      message = 'm_meteo::ec_addtimespacerelation: Error while initializing '''//trim(name)//''' from file: '''//trim(filename)//''''
      if (present(forcingfile)) then
         message = trim(message)//' ('''//trim(forcingfile)//''')'
      endif
     
   end function ec_addtimespacerelation
   
   ! ==========================================================================
   function checkVectorMax(ecInstancePtr, sourceItemId , targetItemId) result (success)
      logical                       :: success       !< function result
      type(tEcInstance), pointer    :: ecInstancePtr !< the instance pointer
      integer,           intent(in) :: sourceItemId  !< the source item ID
      integer,           intent(in) :: targetItemId  !< the target item ID

      type(tEcItem),       pointer :: itemPtrSrc     !< Item corresponding to sourceItemId
      type(tEcItem),       pointer :: itemPtrTgt     !< Item corresponding to targetItemId
      integer                      :: vectorMaxSrc   !< vectorMax for source item
      integer                      :: vectorMaxTgt   !< vectorMax for target item
      character(len=1024)          :: msg
      success = .true.
      itemPtrSrc => ecSupportFindItem(ecInstancePtr, sourceItemId)
      itemPtrTgt => ecSupportFindItem(ecInstancePtr, targetItemId)
      vectorMaxSrc = itemPtrSrc%quantityPtr%vectorMax
      vectorMaxTgt = itemPtrTgt%quantityPtr%vectorMax
      if (vectorMaxSrc /= vectorMaxTgt) then
         success = .false.
         select case (itemPtrTgt%quantityPtr%name)
         case ('discharge_salinity_temperature_sorsin')
            write(msg,'(a,i0,a,i0,a)') 'Wrong number of data columns in a discharge_salinity_temperature_sorsin time series: ', vectorMaxTgt, ' requested, ',vectorMaxSrc,' provided.'
            msg = trim(msg) // " With source file '" // trim(itemPtrSrc%elementsetPtr%name) // "'"
            call mess(LEVEL_ERROR,  trim(msg))
         case default
            call mess(LEVEL_WARN, "There was a problem with a source of type " // trim(itemPtrSrc%quantityPtr%name) &
                    // " with source file '" // trim(itemPtrSrc%elementsetPtr%name) // "'")
            call mess(LEVEL_ERROR, "Vector max differs for " // trim(itemPtrTgt%quantityPtr%name) &
                    // " values (resp. source, target): ", vectorMaxSrc, vectorMaxTgt)
         end select
      endif
   end function checkVectorMax
   
   ! ==========================================================================
   function ec_gettimeseries_by_itemID(instancePtr, itemId, t0, t1, dt, target_array) result(success)
      use m_flowtimes
      logical                                                 :: success      !< function status
      type(tEcInstance),                        pointer       :: instancePtr  !< intent(in)
      integer,                                  intent(in)    :: itemID       !< unique Item id
      real(hp),                                 intent(in)    :: t0,t1,dt     !< get data corresponding to this number of timesteps since FM's refdate
      real(hp), dimension(:), allocatable,      intent(inout) :: target_array !< kernel's data array for the requested values
      real(hp), dimension(:), pointer     :: arr1dPtr => null() 


      real(hp) :: tt
      integer  :: it, nt, blksize
      tt = t0
      it = 0
      
      nt = ceiling((t1-t0)/dt)+1
      if (allocated(target_array)) deallocate(target_array)
      allocate(target_array(nt*blksize)) 
      arr1dPtr => ecItemGetArr1DPtr(instancePtr, itemId, 2)
      blksize = size(arr1dPtr)  

      call clearECMessage()
      do while (t0+it*dt<t1)
         if (.not.ec_gettimespacevalue_by_itemID(instancePtr, itemId, irefdate, tzone, tunit, t0+it*dt,  &
                                     target_array(it*blksize+1:(it+1)*blksize))) then
            return         ! Message stack was already dumped by gettimespacevalue
         end if
         it = it + 1
      end do
      success = .true.
   end function ec_gettimeseries_by_itemID

   ! ==========================================================================

   !> Convenience wrapper around ec_gettimespacevalue_by_itemID.
   function ec_gettimespacevalue_by_name(instancePtr, group_name, timesteps) result(success)
      use m_flowtimes
      logical                       :: success     !< function status
      type(tEcInstance), pointer    :: instancePtr !< intent(in)
      character(len=*),  intent(in) :: group_name  !< unique group name
      real(hp),          intent(in) :: timesteps   !< get data corresponding to this number of timesteps since FM's refdate
      double precision, dimension(:), pointer :: ptm, prh, ptd
      !
      success = .false.
      !
      if (trim(group_name) == 'rainfall') then
         if (.not.ec_gettimespacevalue_by_itemID(instancePtr, item_rainfall, irefdate, tzone, tunit, timesteps)) return
         end if
      if (trim(group_name) == 'rainfall_rate') then
         if (.not.ec_gettimespacevalue_by_itemID(instancePtr, item_rainfall_rate, irefdate, tzone, tunit, timesteps)) return
      end if
      if (trim(group_name) == 'airdensity') then
         if (.not.ec_gettimespacevalue_by_itemID(instancePtr, item_airdensity, irefdate, tzone, tunit, timesteps)) return
      end if
      if (trim(group_name) == 'humidity_airtemperature_cloudiness') then
         if (.not.ec_gettimespacevalue_by_itemID(instancePtr, item_hac_humidity, irefdate, tzone, tunit, timesteps)) return
      end if
      if (trim(group_name) == 'humidity_airtemperature_cloudiness_solarradiation') then
         if (.not.ec_gettimespacevalue_by_itemID(instancePtr, item_hacs_humidity, irefdate, tzone, tunit, timesteps)) return
      end if
      if (trim(group_name) == 'dewpoint_airtemperature_cloudiness') then
         if (.not.ec_gettimespacevalue_by_itemID(instancePtr, item_dac_dewpoint, irefdate, tzone, tunit, timesteps)) return
      end if
      if (trim(group_name) == 'dewpoint_airtemperature_cloudiness_solarradiation') then
         if (.not.ec_gettimespacevalue_by_itemID(instancePtr, item_dacs_dewpoint, irefdate, tzone, tunit, timesteps)) return
      end if
      if (trim(group_name) == 'dewpoint') then
         if (.not.ec_gettimespacevalue_by_itemID(instancePtr, item_humidity, irefdate, tzone, tunit, timesteps)) return ! Relative humidity array used to store dewpoints
         if (.not.ec_gettimespacevalue_by_itemID(instancePtr, item_airtemperature, irefdate, tzone, tunit, timesteps)) return ! update tair for conversion of dewpoint to humidity
      end if
      
      if ((trim(group_name) == 'dewpoint_airtemperature_cloudiness' .and. item_dac_dewpoint/=ec_undef_int)    &
          .or.       & 
          (trim(group_name) == 'dewpoint_airtemperature_cloudiness_solarradiation' .and. item_dacs_dewpoint/=ec_undef_int)    &
          .or.       &
          (trim(group_name) == 'dewpoint' .and. item_humidity/=ec_undef_int)) then
          ! Conversion of dewpoint to relative humidity
          ptd => rhum
          prh => rhum
          ptm => tair
          call dewpt2rhum(ptd,ptm,prh)        ! convert dewpoint temperatures to relative humidity (percentage)
      end if
      if (index(group_name, 'airpressure_windx_windy') == 1) then
         if (.not.ec_gettimespacevalue_by_itemID(instancePtr, item_apwxwy_p, irefdate, tzone, tunit, timesteps)) return
      end if
      if (trim(group_name) == 'bedrock_surface_elevation') then
         if (.not.ec_gettimespacevalue_by_itemID(instancePtr, item_subsiduplift, irefdate, tzone, tunit, timesteps)) return
      end if
      success = .true.
   end function ec_gettimespacevalue_by_name

   subroutine dewpt2rhum(td,tm,rh)
   ! in-place conversion of dewpoint temperature to relative humidity, given the air temperature 
   ! $$RH(T,T_d) = \exp\left[\frac{BT}{C+T} - \frac{BT_d}{C+T_d}\right] \times 100$$ 
   use physicalconsts, only : CtoKelvin
   implicit none
   double precision, dimension(:), pointer   :: td    !< dewpoint temperature
   double precision, dimension(:), pointer   :: tm    !< air temperature
   double precision, dimension(:), pointer   :: rh    !< relative humidity

   double precision, parameter               :: B =   17.502 ! exactly as in 
   double precision, parameter               :: C =   -32.19
   integer                                   :: i, n
   td => rh                                  ! Dewpoint temperature was stored in the array where relative humidity will be stored 
   n = size(td)
   do i=1,n
      rh(i) = exp(B*td(i)/(C+td(i)+CtoKelvin) - B*tm(i)/(C+tm(i)+CtoKelvin)) * 100.d0
   end do
   end subroutine dewpt2rhum

end module m_meteo
