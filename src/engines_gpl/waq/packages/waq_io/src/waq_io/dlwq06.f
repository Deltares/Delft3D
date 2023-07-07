!!  Copyright (C)  Stichting Deltares, 2012-2023.
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

      subroutine dlwq06 ( lun    , lchar  , filtype, icmax  , car    ,
     &                    iimax  , iar    , irmax  , rar    , notot  ,
     &                    noseg  , sname  , nowst  , nowtyp , nrftot ,
     &                    nrharm , dtflg1 , dtflg3 , iwidth , vrsion ,
     &                    ioutpt , chkpar , ierr   , iwar            )

!       Deltares Software Centre

!>\file
!>                          Reads all inputs associated with waste loads and withdrawals
!>
!>                          This routine reads:
!>                             - the number of wasteloads and withdrawals
!>                             - the wasteloads ID's and volume numbers (and names and types for modern files)
!>                             - the wasteload concentration/mass values

!     Created            : April    1988 by Marjolein Sileon and Leo Postma

!     Modified           : April    1997 by Rinze Bruinsma  : Tokenized input data file reading added
!                          July     2002 by Leo Postma      : Call to Opt1 changed.
!                          December 2010 by Leo Postma      : Addition diffuse sources
!                                                             Fortran 90 style
!                                                             simpler call to tokenized input

!     Function           : Reads waste loads

!     Subroutines called : opt0     : previous versions input processing (one matrix does all)
!                          gettoken : tokenized data input
!                          srstop   : stop after error with return code
!                          zoek     : search for presence of a string
!                          dlwq5a   : modern context sensitive input data processing
!                          check    : check whether end of data block is encountred correctly

!     Logical units :      lun(27) = unit DELWAQ input file
!                          lun(29) = unit formatted output file
!                          lun( 2) = unit intermediate file (system)
!                          lun( 3) = unit intermediate file (harmonics)
!                          lun( 4) = unit intermediate file (pointers)
!                          lun(15) = unit intermediate file (waste load)

      use m_zoek
      use m_srstop
      use rd_token
      use timers       !   performance timers
      implicit none

!     Parameters    :
!     type     kind  function         name             description

      integer  ( 4), intent(in   ) :: lun    (*)     !< array with unit numbers
      character( *), intent(inout) :: lchar  (*)     !< Filenames for the items
      integer  ( 4), intent(inout) :: filtype(*)     !< type of binary files
      integer  ( 4), intent(in   ) :: icmax          !< size of the character workspace
      character(20), intent(inout) :: car   (icmax)  !< local character workspace
      integer  ( 4), intent(in   ) :: iimax          !< size of the integer   workspace
      integer  ( 4), intent(inout) :: iar   (iimax)  !< local integer   workspace
      integer  ( 4), intent(in   ) :: irmax          !< size of the real      workspace
      real     ( 4), intent(inout) :: rar   (irmax)  !< local real      workspace
      integer  ( 4), intent(in   ) :: notot          !< total number of substances
      integer  ( 4), intent(in   ) :: noseg          !< number of computational volumes
      character(20), intent(in   ) :: sname (notot)  !< IDs of the substances
      integer  ( 4), intent(  out) :: nowst          !< number of waste loads
      integer  ( 4), intent(  out) :: nowtyp         !< number of waste load types
      integer  ( 4), intent(inout) :: nrftot( 11 )   !< number of function items per kind
      integer  ( 4), intent(inout) :: nrharm( 11 )   !< number of harmonic items per kind
      logical      , intent(in   ) :: dtflg1         !< if true then 'date'-format for 2nd time scale
      logical      , intent(in   ) :: dtflg3         !< 'date'-format (F;ddmmhhss,T;yydddhh)
      integer  ( 4), intent(in   ) :: iwidth         !< width of the output file
      real     ( 4), intent(in   ) :: vrsion         !< Input file version number
      integer  ( 4), intent(in   ) :: ioutpt         !< Degree of output in report file
      logical      , intent(out   ) :: chkpar(2)     !< Check for parameters SURF and LENGTH
      integer  ( 4), intent(inout) :: ierr           !< cumulative error count
      integer  ( 4), intent(inout) :: iwar           !< cumulative warning count

!     local

      character( 40)                 chulp       (3) !  Help for reading
      character(255)                 cdummy          !  Help for reading
      character( 20), allocatable :: wstid       (:) !  wasteload id's 20 character
      character( 40), allocatable :: wstname     (:) !  wasteload names
      character( 20), allocatable :: wsttype     (:) !  wasteload types
      character(256), allocatable :: wstid_long  (:) !  array to buffer the non truncated wasteload id's
      character(256), allocatable :: wsttype_long(:) !  array to buffer the non truncated wasteload types
      integer  (  4), allocatable :: iwstseg     (:) !  wasteload segment
      integer  (  4), allocatable :: iwsttype    (:) !  index wasteload type
      integer  (  4), allocatable :: iwstkind    (:) !  kind array: 0 = pres: use present situation
                                                     !              1 = mass: use data as mass/sec      even with flow > 0
                                                     !              2 = conc: use data as concentration even with flow = 0
                                                     !              3 = rain: flow >= 0 use concentration, < 0 use 0.0
                                                     !              4 = well: flow >= 0 use concentration, < 0 use model-C
      real     (  8), allocatable :: drar        (:) !  double precission workspace (very large !lp)
      integer  (  4)                 lunwr           !  binary unit for wasteloads
      integer  (  4)                 itype           !  type of token that is read
      integer  (  4)                 ierr2           !  local error indicator
      integer  (  4)                 ierr_alloc      !  local error indicator for allocation
      integer  (  4)                 i               !  loop counter
      integer  (  4)                 ifound          !  help variable in searches
      integer  (  4)                 ifound2         !  help variable in searches
      logical                        ldummy          !  dummy logical
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "dlwq06", ithndl )

!     Init

      lunwr = lun( 2)
      iposr = 0
      chkpar = .false.
      nowtyp = 0

!        Read number of waste loads

      if ( gettoken( nowst, ierr2 ) .gt. 0 ) goto 20
      if ( nowst .lt. 0 ) then       !   it says that info comes from auxiliary file
         write ( lunut , 2000 ) nowst
         call opt1   ( -1     , lun    , 15     , lchar  , filtype,
     &                 dtflg1 , dtflg3 , 0      , ierr2  , iwar   ,
     &                 .false.)
         if ( ierr2 .gt. 0 ) goto 20
         if ( gettoken( nowst, ierr2 ) .gt. 0 ) goto 20
      endif
      if ( nowst .eq. 0 ) then
         write ( lunut, 2010 )
         goto 20
      endif

!     read waste names, from version 4.9 on names are ID's
!                                           names are 40 characters
!                                           types are 20 characters
      allocate ( wstid     (nowst), wsttype     (nowst), wstname(nowst),
     &           wstid_long(nowst), wsttype_long(nowst), iwstseg(nowst),
     *           iwsttype  (nowst), iwstkind    (nowst), stat = ierr_alloc )
      if ( ierr_alloc .ne. 0 ) then
         write ( lunut , 2160 ) ierr_alloc
         write ( lunut , 2040 ) nowst
         call SRSTOP(1)
      endif
      write ( lunut , 2040 ) nowst
      if ( ioutpt .lt. 3 ) then
         write ( lunut , 2045 )
      else
         if ( iwidth .eq. 5 ) then
            write ( lunut , 2050 )
         else
            write ( lunut , 2060 )
         endif
      endif

!       read wasteload identification

      do 10 i = 1 , nowst

         iwsttype(i)     =  0
         iwstkind(i)     =  0
         if ( gettoken( chulp(1), iwstseg(i), itype, ierr2 ) .gt. 0 ) goto 20
         if ( itype .eq. 1 ) then                  !    character, either SURFACE, BANK or BOTTOM
            iwstseg(i) = 0
            if ( chulp(1) .eq. "SURFACE" ) iwstseg(i) = -1; iwstkind(i) = 2 ! e.g. atmospheric deposition
            if ( chulp(1) .eq. "BANK"    ) iwstseg(i) = -2; iwstkind(i) = 2 ! e.g. bank infiltration, 1D river systems
            if ( chulp(1) .eq. "BED"     ) iwstseg(i) = -3; iwstkind(i) = 2 ! e.g. well and sink
            if ( iwstseg(i) .eq. 0 ) then
               ierr2 = 1
               goto 20
            endif

            if ( iwstseg(i) == -1 .or. iwstseg(i) == -3 ) then
                chkpar(1) = .true.
            endif
            if ( iwstseg(i) == -2 ) then
                chkpar(2) = .true.
            endif
         endif
         if ( gettoken( wstid_long(i), ierr2 ) .gt. 0 ) goto 20
         select case ( wstid_long(i) )
            case ( "MASS" )
               iwstkind(i) = 1
               if ( gettoken( wstid_long(i), ierr2 ) .gt. 0 ) goto 20
            case ( "CONC" )
               iwstkind(i) = 2
               if ( gettoken( wstid_long(i), ierr2 ) .gt. 0 ) goto 20
            case ( "RAIN" )
               iwstkind(i) = 3
               if ( gettoken( wstid_long(i), ierr2 ) .gt. 0 ) goto 20
            case ( "WELL" )
               iwstkind(i) = 4
               if ( gettoken( wstid_long(i), ierr2 ) .gt. 0 ) goto 20
         end select

         if ( vrsion .ge. 4.90 ) then           ! read also name and type
            if ( gettoken( wstname     (i), ierr2 ) .gt. 0 ) goto 20
            if ( gettoken( wsttype_long(i), ierr2 ) .gt. 0 ) goto 20
         else
            wstname(i)      = ' '
            wsttype_long(i) = ' '
         endif

         if ( wstid_long(i)  .eq. ' ' ) write ( wstid_long(i), '(''waste-load id'',i7)' ) i
         if ( wstname(i)     .eq. ' ' ) write ( wstname(i), '(''waste-load name '',i7)' ) i
         if ( wsttype_long(i).eq. ' ' ) wsttype_long(i) = 'waste-load type 1'

         wstid(i)   = wstid_long(i)
         wsttype(i) = wsttype_long(i)

         if ( ioutpt .ge. 3 ) then
            if ( iwidth .eq. 5 ) then
               if ( iwstseg(i) .gt. 0 )
     &            write ( lunut , 2070 ) i, iwstseg(i), iwstkind(i), wstid(i), wstname(i), wsttype(i)
               if ( iwstseg(i) .eq. -1 )
     &            write ( lunut , 2075 ) i, "SURFACE" , iwstkind(i), wstid(i), wstname(i), wsttype(i)
               if ( iwstseg(i) .eq. -2 )
     &            write ( lunut , 2075 ) i, "BANK   " , iwstkind(i), wstid(i), wstname(i), wsttype(i)
               if ( iwstseg(i) .eq. -3 )
     &            write ( lunut , 2075 ) i, "BED    " , iwstkind(i), wstid(i), wstname(i), wsttype(i)
            else
               if ( iwstseg(i) .gt. 0 )
     &            write ( lunut , 2080 ) i, iwstseg(i), iwstkind(i), wstid(i), wstname(i), wsttype(i)
               if ( iwstseg(i) .eq. -1 )
     &            write ( lunut , 2085 ) i, "SURFACE" , iwstkind(i), wstid(i), wstname(i), wsttype(i)
               if ( iwstseg(i) .eq. -2 )
     &            write ( lunut , 2085 ) i, "BANK   " , iwstkind(i), wstid(i), wstname(i), wsttype(i)
               if ( iwstseg(i) .eq. -3 )
     &            write ( lunut , 2085 ) i, "BED    " , iwstkind(i), wstid(i), wstname(i), wsttype(i)
            endif
         endif

!          check for unique ID, error if non-truncated ID is unique otherwise warning

         call ZOEK( wstid(i), i-1, wstid, 20, ifound )
         if ( ifound .gt. 0 ) then
            call ZOEK( wstid_long(i), i-1, wstid_long, 256, ifound2 )
            if ( ifound .eq. ifound2 ) then
               write(lunut,2130) wstid(i)
               iwar = iwar + 1
            else
               write(lunut,2140) wstid(i)
               ierr = ierr + 1
            endif
         endif

!          check if truncated type and non truncated type give the same number

         call ZOEK( wsttype(i)     , nowtyp, wsttype     , 20 , ifound  )
         call ZOEK( wsttype_long(i), nowtyp, wsttype_long, 256, ifound2 )
         if ( ifound .ne. ifound2 ) then
            write(lunut,2150) trim(wsttype_long(i))
            ierr = ierr + 1
         endif

!          if type found set type, otherwise add type

         if ( ifound .gt. 0 ) then
            iwsttype(i) = ifound
         else
            nowtyp = nowtyp + 1
            wsttype(nowtyp)      = wsttype(i)
            wsttype_long(nowtyp) = wsttype_long(i)
            iwsttype(i)          = nowtyp
         endif

!          check segment number

         if ( iwstseg(i) .lt. -3 .or.  iwstseg(i) .gt. noseg .or.
     &        iwstseg(i) .eq.  0                                     ) then
            write ( lunut , 2090 ) iwstseg(i)
            ierr = ierr + 1
         endif

!          write ID and name to system file

         write ( lunwr )  iwstseg(i), iwstkind(i), wstid(i), wstname(i)

   10 continue

!          provide information about the special parameters

      if ( chkpar(1) ) then
         write( lunut, 2210 )
      endif
      if ( chkpar(2) ) then
         write( lunut, 2220 )
      endif


!          give list of all identified wasteload types and write info to system file

      write ( lunut ,   *  )
      write ( lunut , 2110 ) nowtyp
      if ( ioutpt .lt. 2 ) then
         write ( lunut , 2115 )
      else
         write ( lunut , 2112 )
         do i = 1 , nowtyp
            write ( lunut , 2120 ) i, wsttype(i)
         enddo
         write ( lunut ,   *  )
      endif
      write ( lunwr )  ( wsttype(i) , i = 1, nowtyp )
      write ( lunwr )  ( iwsttype(i), i = 1, nowst  )

!          these arrays are not needed further

      deallocate( wstname, wstid_long, wsttype_long, iwstseg, iwsttype )

!          now get the values

      if ( vrsion .ge. 4.90 ) then           ! new input processing
         allocate( drar(irmax) )             ! this array is 100 mb lp
         call dlwq5a ( lun    , lchar  , 15     , iwidth , icmax  ,
     &                 car    , iimax  , iar    , irmax  , rar    ,
     &                 sname  , wstid  , wsttype, nowst  , notot+1,
     &                 nowtyp , drar   , dtflg1 , dtflg3 , vrsion ,
     &                 ioutpt , ierr2  , ierr   , iwar   )
         deallocate( drar )
         if ( ierr2 .eq.  0 ) then
            deallocate( wstid, wsttype )
            goto 30
         endif
      else                                   ! old input processing
         ierr2 = -2
         ldummy = .false.
         call opt0   ( lun    , 15     , 0        , 0        , nowst,
     &                 notot+1, notot+1, nrftot(9), nrharm(9), 1    ,
     &                 dtflg1 , ldummy , ldummy   , iwidth   , lchar,
     &                 filtype, dtflg3 , vrsion   , ioutpt   , ierr2,
     &                 iwar   , .false.)
     &
      endif
      deallocate( wstid, wsttype )
      ierr = ierr + ierr2
      ierr2 = 0

!     error processing

   20 if ( ierr2 .gt. 0 ) ierr = ierr + 1       !   if 2, end of block reached
      if ( ierr2 .eq. 3 ) call SRSTOP(1)        !   end of file reached
      call check  ( cdummy , iwidth , 6      , ierr2  , ierr   )
   30 if ( timon ) call timstop( ithndl )
      return

!       Output formats

 2000 format (  /,' First  selected option   : ',I7 )
 2010 format (  /,' No waste loads ' )
 2020 format ( /,' ERROR the number of wasteloads (',I5,') exceeds',
     *           ' the maximum (',I5,').',
     *          /' The maximum is limited by CHARACTER array space',
     *          /' Consult your system manager to obtain ',I5,' words',
     *           ' of additional storage.' )
 2030 format ( /,' ERROR the number of wasteloads (',I5,') exceeds',
     *           ' the maximum (',I5,').',
     *          /' The maximum is limited by INTEGER array space',
     *          /' Consult your system manager to obtain ',I5,' words',
     *           ' of additional storage.' )
 2040 format (  /,' Number of waste loads: ',I4// )
 2045 format (  /,' Names of waste loads are printed for',
     *            ' output option 3 and higher !' )
 2050 format ( 'Number  segment knd wasteload ID          '/
     *         20X,'wasteload name      ',22X,'wasteload type' )
 2060 format ( 'Number  segment knd wasteload ID          ',
     *             'wasteload name      ',22X,'wasteload type' )
 2070 format (  I6,2X,I7,2X,I1,2X,A20/20X,A40,2X,A20 )
 2075 format (  I6,2X,A7,2X,I1,2X,A20/20X,A40,2X,A20 )
 2080 format (  I6,2X,I7,2X,I1,2X,A20, 2X,A40,2X,A20 )
 2085 format (  I6,2X,A7,2X,I1,2X,A20, 2X,A40,2X,A20 )
 2090 format (    ' ERROR invalid segment number:',I8 )
 2100 format (    ' ERROR: Character array space insufficient' )
 2110 format (    ' Number of different wasteload types: ', I4 )
 2112 format (    ' Type:  Type-string' )
 2115 format (    ' Waste load types are printed for output option',
     *            ' 2 or higher !' )
 2120 format (      I6, 2X, A20  )
 2130 format (    ' WARNING: wasteload ID is not unique:',A)
 2140 format (    ' ERROR: truncated wasteload ID is not unique:',A)
 2150 format (    ' ERROR: truncated wasteload type not unique:',A)
 2160 format (    ' ERROR: allocating wasteload arrays:',I8)
 2210 format (    ' Note: one or more special waste loads - parameter/se
     &gment function SURF required' )
 2220 format (    ' Note: one or more special waste loads - parameter/se
     &gment function LENGTH required' )

      end
