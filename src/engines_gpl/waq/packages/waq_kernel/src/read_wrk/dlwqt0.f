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
      module m_dlwqt0
      use m_dlwqtk
      use m_dlwqta


      implicit none

      contains


      subroutine dlwqt0 ( lun    , itime  , itimel , harmat , array  ,
     &                    iharm  , nrharm , nrftot , idt    , volume ,
     &                    disper , area   , flow   , velo   , aleng  ,
     &                    wastes , bounds , consts , param  , funcs  ,
     &                    sfuncs , ipoint , luntxt , luntx2 , ftype  ,
     &                    intsrt , isflag , ifflag , ivflag , ilflag ,
     &                    update , iktim  , iknmrk , inwspc , anwspc ,
     &                    inwtyp , iwork  , lstrec , lrewin , vollst ,
     &                    rdvolu , gridps , dlwqd  )

!       Deltares Software Centre

!>\file
!>                         Updates time level of all external steering
!>        Besides this routine, there is also the dlwq41 routine that does
!>        same for volumes only. This routine is campletely used once at
!>        the start of simulation. Furthermore it is only used without
!>        reading the new volumes, that is then done by dlwq41.

!     Created: april- 8-1988 by Leo Postma

!     Updated: august 1997 by Leo Postma: Merged with DLWQ45 to one routine
!                   > 2000 by several authors.

!     LOGICAL UNITS       : LUN(3), harmonics file
!                           LUN(4), function pointer file

!     SUBROUTINES CALLED  : DLWQT1, makes one time function
!                           DLWQTA, make values for const,param,func,sfunc
!                           DLWQTK, make values for kenmerk array
!                           open_waq_files, opens files
      use m_dlwqt1
      use m_srstop
      use m_open_waq_files
      use timers
      use delwaq2_data
      use dlwqgrid_mod
      use m_sysn          ! System characteristics
      use m_syst          ! Time function flags

      implicit none

!     Parameters          :

!     type     kind  function         name                         description

      integer  ( 4), intent(inout) :: lun   (*)                  !< Array with unit numbers
      integer  ( 4), intent(in   ) :: itime                      !< Model timer
      integer  ( 4), intent(inout) :: itimel                     !< Model timer one step ago
      real     ( 4), intent(inout) :: harmat(nharms)             !< Matrices harmonic components
      real     ( 4)                :: array (nlines)             !< Set of double file buffers
      integer  ( 4), intent(in   ) :: iharm (niharm)             !< Harmonics time space
      integer  ( 4), intent(inout) :: nrharm(noitem)             !< set of nrs of harmonic records
      integer  ( 4), intent(in   ) :: nrftot(noitem)             !< set of record lengthes
      integer  ( 4), intent(  out) :: idt                        !< Integration time step size
      real     ( 4), intent(  out) :: volume(noseg+nseg2)        !< Array of segment volumes
      real     ( 4), intent(  out) :: disper(nodisp,noq+noq4)    !< Array of dispersions
      real     ( 4), intent(  out) :: area  (noq+noq4)           !< Array of exchange surfaces
      real     ( 4), intent(  out) :: flow  (noq+noq4)           !< Array of flows
      real     ( 4), intent(  out) :: velo  (novelo,noq+noq4)    !< Array of velocities
      real     ( 4), intent(  out) :: aleng (  2   ,noq+noq4)    !< Array of from and to lengthes
      real     ( 4), intent(  out) :: wastes(notot+2,nowst)      !< Array of wasteloads
      real     ( 4), intent(  out) :: bounds(nosys , nobnd)      !< Array of boundary conditions
      real     ( 4), intent(  out) :: consts(nocons)             !< Array of constant values
      real     ( 4), intent(  out) :: param (nopa,noseg+nseg2)   !< Array of parameter values
      real     ( 4), intent(  out) :: funcs (nofun )             !< Array of function values
      real     ( 4), intent(  out) :: sfuncs(noseg+nseg2,nosfun) !< Array of segment functions
      integer  ( 4), intent(in   ) :: ipoint(npoins)             !< Set of pointers to destination
      character*(*), intent(in   ) :: luntxt(*)                  !< text with the unit numbers
      character*200, intent(in   ) :: luntx2(*)                  !< text with the binary files
      integer  ( 4), intent(in   ) :: ftype (*)                  !< type of files to be opened
      integer  ( 4), intent(in   ) :: intsrt                     !< integration option
      integer  ( 4), intent(in   ) :: isflag                     !< = 1 then 'ddhhmmss' format
      integer  ( 4), intent(inout) :: ifflag                     !< = 1 then first invocation
      integer  ( 4), intent(in   ) :: ivflag                     !< = 1 then computed volumes
      integer  ( 4), intent(in   ) :: ilflag                     !< = 0 then constant lengthes
      logical      , intent(inout) :: update                     !< TRUE if update took place
      integer  ( 4), intent(inout) :: iktim (3)                  !< Timers in file
      integer  ( 4), intent(inout) :: iknmrk(noseg+nseg2)        !< Kenmerk array
      integer  ( 4), intent(inout) :: inwspc(newisp)             !< Integer space new time funs
      real     ( 4), intent(inout) :: anwspc(newrsp)             !< Real space new time functions
      integer  ( 4), intent(in   ) :: inwtyp(nobnd+nowst)        !< Types of items
      integer  ( 4)                :: iwork (*)                  !< Integer workspace
      logical      , intent(in   ) :: lstrec                     !< TRUE: last record on rewind wanted
      logical      , intent(  out) :: lrewin                     !< TRUE: rewind took place
      real     ( 4), intent(inout) :: vollst(*)                  !< Last volume record before rewind
      logical      , intent(in   ) :: rdvolu                     !< TRUE: also read volumes
      type(GridPointerColl)        :: GridPs                     !< collection of all grid definitions
      type(delwaq_data)            :: dlwqd                      !< derived type for persistent storage

!     Local declarations

      real         rdummy(1) , adummy(1) , adt   (1)
      logical      lstdum    , lredum    , ldum  (3)
      integer      iph, ipf, ipa, ipi, ipni, ipna           !  incremental pointers
      integer      isnul, isnul2, idummy                    !  dummy variables
      integer      nosss, it, nosubs, is                    !  helpvariables
      integer      ierr                                     !  error flag (not tested)

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqt0", ithandl )

!         open the harmonics and pointer files

      if ( ifflag .eq. 1 ) then
         bndset = .false.
         wstset = .false.
         funset = .false.
         othset = .false.
         call open_waq_files ( lun(3), luntxt(3), 3    , 2     , ierr  )
         call open_waq_files ( lun(4), luntxt(4), 4    , 2     , ierr  )
      endif

!         initialisation

      iph  = 1
      ipf  = 1
      ipa  = 1
      ipi  = 1
      ipni = 1
      ipna = 1
      it   = 1
      isnul  = 0
      isnul2 = 0
      idummy = 0
      lstdum = .false.
      update = .false.

!         integration step size IDT

      if ( nrftot( 1) .gt. 0 ) then
         call dlwqt1 ( lun       , itime      , itimel , iharm(ipf), harmat(iph),
     &                    array(ipa), ipoint(ipi), adt    , 1         , nrharm( 1) ,
     &                    1         , nrftot( 1) , ipa    , iph       , ipf        ,
     &                    ipi       , luntxt     , 5      , isflag    , ifflag     ,
     &                    update    , othset     , 0      , iwork     , lstdum     ,
     &                    lredum    , rdummy     , ftype  , dlwqd     )
         ldum(1) = update
         ldum(2) = othset

         update = ldum(1)
         othset = ldum(2)

         if ( othset ) then
            is = 5
            goto 10
         endif
         idt = adt(1) +   0.5
      endif

!         volumes

!     if read anyway or ( read-requested and there is something to read )
      if ( nrharm( 2) .ge. 0 ) then
         if   ( rdvolu ) then
!           if .not. computed volumes .or. this is the first time
            if ( ivflag     .eq. 0 .or. ifflag .eq. 1 ) then
                  call dlwqt1 ( lun       , itime      , itimel , iharm(ipf), harmat(iph),
     &                          array(ipa), ipoint(ipi), volume , 1         , nrharm( 2) ,
     &                          noseg     , nrftot( 2) , ipa    , iph       , ipf        ,
     &                          ipi       , luntxt     , 7      , isflag    , ifflag     ,
     &                          update    , othset     , 0      , iwork     , lstrec     ,
     &                          lrewin    , vollst     , ftype  , dlwqd     )
                  ldum(1) = update
                  ldum(2) = othset
                  ldum(3) = lrewin

               update = ldum(1)
               othset = ldum(2)
               lrewin = ldum(3)
            endif
         else
            ipa = ipa + nrftot(2)*2
            ipi = ipi + noseg + 3
         endif
         if ( othset ) then
            is = 7
            goto 10
         endif
      endif

!         dispersions

      if ( nrharm( 3) .ge. 0 ) then
         call dlwqt1 ( lun       , itime      , itimel , iharm(ipf), harmat(iph),
     &                    array(ipa), ipoint(ipi), disper , nodisp    , nrharm( 3) ,
     &                    noq       , nrftot( 3) , ipa    , iph       , ipf        ,
     &                    ipi       , luntxt     , 9      , isflag    , ifflag     ,
     &                    update    , othset     , 0      , iwork     , lstdum     ,
     &                    lredum    , rdummy     , ftype  , dlwqd     )
         ldum(1) = update
         ldum(2) = othset

         update = ldum(1)
         othset = ldum(2)

         if ( othset ) then
            is = 9
            goto 10
         endif
      endif

!         area

      if ( nrharm( 4) .ge. 0 ) then
         call dlwqt1 ( lun       , itime      , itimel , iharm(ipf), harmat(iph),
     &                    array(ipa), ipoint(ipi), area   , 1         , nrharm( 4) ,
     &                    noq       , nrftot( 4) , ipa    , iph       , ipf        ,
     &                    ipi       , luntxt     , 10     , isflag    , ifflag     ,
     &                    update    , othset     , 0      , iwork     ,  lstdum    ,
     &                    lredum    , rdummy     , ftype  , dlwqd     )
         ldum(1) = update
         ldum(2) = othset

         update = ldum(1)
         othset = ldum(2)

         if ( othset ) then
            is = 10
            goto 10
         endif
      endif

!         flow

      if ( nrharm( 5) .ge. 0 ) then
            call dlwqt1 ( lun       , itime      , itimel , iharm(ipf) , harmat(iph),
     &                    array(ipa), ipoint(ipi), flow   , 1          , nrharm( 5) ,
     &                    noq       , nrftot( 5) , ipa    , iph        , ipf        ,
     &                    ipi       , luntxt     , 11     , isflag     , ifflag     ,
     &                    update    , othset     , 0      , iwork      , lstdum     ,
     &                    lredum    , rdummy     , ftype  , dlwqd      )
            ldum(1) = update
            ldum(2) = othset

         update = ldum(1)
         othset = ldum(2)

         if ( othset ) then
            is = 11
            goto 10
         endif
      endif

!         velocities

      if ( nrharm( 6) .ge. 0 ) then
         call dlwqt1 ( lun       , itime      , itimel , iharm(ipf) , harmat(iph),
     &                    array(ipa), ipoint(ipi), velo   , novelo     , nrharm( 6) ,
     &                    noq       , nrftot( 6) , ipa    , iph        , ipf        ,
     &                    ipi       , luntxt     , 12     , isflag     , ifflag     ,
     &                    update    , othset     , 0      , iwork      , lstdum     ,
     &                    lredum    , rdummy     , ftype  , dlwqd      )
         ldum(1) = update
         ldum(2) = othset

         update = ldum(1)
         othset = ldum(2)

         if ( othset ) then
            is = 12
            goto 10
         endif
      endif

!         'from'- and 'to'-length

      if ( nrharm( 7) .ge. 0 .and. ilflag .eq. 1 ) then
            call dlwqt1 ( lun       , itime      , itimel , iharm(ipf), harmat(iph),
     &                    array(ipa), ipoint(ipi), aleng  , 2         , nrharm( 7) ,
     &                    noq       , nrftot( 7) , ipa    , iph       , ipf        ,
     &                    ipi       , luntxt     , 13     , isflag    , ifflag     ,
     &                    update    , othset     , 0      , iwork     , lstdum     ,
     &                    lredum    , rdummy     , ftype  , dlwqd     )
         ldum(1) = update
         ldum(2) = othset

         update = ldum(1)
         othset = ldum(2)
         if ( othset ) then
            is = 13
            goto 10
         endif
      endif

!         boundaries

      if ( intsrt .eq. 6 .or. intsrt .eq. 7 ) then
         nosubs = notot
      else
         nosubs = nosys
      endif
      if ( nrharm( 8) .ge. 0 .and. .not. bndset ) then
         call dlwqt1 ( lun       , itime      , itimel , iharm(ipf), harmat(iph),
     &                    array(ipa), ipoint(ipi), bounds , nosubs    , nrharm( 8) ,
     &                    nobnd     , nrftot( 8) , ipa    , iph       , ipf        ,
     &                    ipi       ,  luntxt    , 14     , isflag    , ifflag     ,
     &                    update    , bndset     , 0      , iwork     ,  lstdum    ,
     &                    lredum    , rdummy     , ftype  , dlwqd     )
         ldum(1) = update
         ldum(2) = othset
         ldum(3) = bndset

         update = ldum(1)
         othset = ldum(2)
         bndset = ldum(3)

      endif

      if ( bndset ) then
            call dlwqt1 ( lun    , itime     , itimel , inwspc(ipni), anwspc(ipna),
     &                    adummy , inwtyp(it), bounds , nosubs      , isnul2      ,
     &                    nobnd  , isnul     , ipni   , ipna        , idummy      ,
     &                    ibndmx , luntxt    , 14     , isflag      , ifflag      ,
     &                    update , bndset    , 0      , iwork       , lstdum      ,
     &                    lredum , rdummy    , ftype  , dlwqd       )
         ldum(1) = update
         ldum(2) = othset

         update = ldum(1)
         othset = ldum(2)


         it     = it + nobnd
      endif

!         wastes

      if ( nrharm( 9) .ge. 0 .and. .not. wstset ) then
         call dlwqt1 ( lun       , itime      , itimel , iharm(ipf), harmat(iph),
     &                    array(ipa), ipoint(ipi), wastes , notot+1   , nrharm( 9) ,
     &                    nowst     , nrftot( 9) , ipa    , iph       , ipf        ,
     &                    ipi       , luntxt     , 15     , isflag    , ifflag     ,
     &                    update    , wstset     , 1      , iwork     ,  lstdum    ,
     &                    lredum    , rdummy     , ftype  , dlwqd     )
         ldum(1) = update
         ldum(2) = othset
         ldum(3) = wstset

         update = ldum(1)
         othset = ldum(2)
         wstset = ldum(3)

      endif
      isnul = 0
      isnul2= 0
      if ( wstset ) then
         call dlwqt1 ( lun       , itime     , itimel , inwspc(ipni), anwspc(ipna),
     &                    adummy    , inwtyp(it), wastes , notot+1     , isnul2      ,
     &                    nowst     , isnul     , ipni   , ipna        , idummy      ,
     &                    iwstmx    , luntxt    , 15     , isflag      , ifflag      ,
     &                    update    , wstset    , 1      , iwork       , lstdum      ,
     &                    lredum    , rdummy    , ftype  , dlwqd       )
         ldum(1) = update
         ldum(2) = othset

         update = ldum(1)
         othset = ldum(2)

         it     = it + nowst
      endif

!         functions

      nosss = noseg + nseg2
      if ( nrharm(10) .ge. 0 ) then
         call dlwqta ( lun(16), luntxt(16), lun(19), nosss  , nocons ,
     &                    nopa   , nofun     , nosfun , consts , param  ,
     &                    funcs  , sfuncs    , isflag , ifflag , itime  ,
     &                    gridps , dlwqd     , ierr   )
      endif


!     kenmerk array

      call dlwqtk ( lun    , itime  , iktim  , iknmrk , nosss  ,
     &              40     , luntxt , isflag , ifflag , ifiopk )

!         close the harmonics and pointer files

   10 if ( ifflag .eq. 1 ) then
         close ( lun( 3) )
         close ( lun( 4) )
         if ( othset ) then
            write ( lun(19) , * ) ' error, new time series processing',
     &           ' wanted for an unsupported item: ',luntxt(is)
            call srstop(1)
         endif
      endif

      itimel =  itime
      if ( timon ) call timstop ( ithandl )

      return
      end

      end module m_dlwqt0
