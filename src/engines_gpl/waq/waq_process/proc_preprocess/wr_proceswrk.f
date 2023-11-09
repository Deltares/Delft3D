      module m_wr_proceswrk

      implicit none

      contains

!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2023.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  
!  

      subroutine wr_proceswrk( lurep , procesdef, nodef , defaul, idpnw ,
     +                         ivpnw , dsto     , vsto  , locnam, nopred,
     +                         nocons, nopa     , nofun , nosfun, notot ,
     +                         noloc , nodisp   , novelo, ndspx , nvelx ,
     +                         nlocx , nosys    , nogrid, dename, coname, paname,
     +                         funame, sfname   , syname, intopt, lun   ,
     +                         lchar , noutp    , ioutps, outputs,ndmpar,
     +                         nbufmx, versio   , ndspn , nveln , nrref ,
     +                         proref, nproc    , nflux , novar , nipmsa)

!     Deltares Software Centre

!>/File
!>      write proces work file

!     Created   : Aug   2012 by Jan van Beek

      use m_wrstoc
      use m_wripro
      use m_setvat
      use m_proc_totals
      use m_intoou
      use m_open_waq_files
      use timers         !< performance timers
      use processet      !< use processet definitions
      use output         !< use output definitions
      implicit none

      ! arguments

      integer             , intent(in   ) :: lurep                  !< unit number report file
      type(procespropcoll), intent(in   ) :: procesdef              !< the proces definition
      integer             , intent(in   ) :: nodef                  !< number of default values
      real                , intent(in   ) :: defaul(*)              !< array with default
      integer             , intent(in   ) :: idpnw(*)               !< new dispersion pointers
      integer             , intent(in   ) :: ivpnw(*)               !< new velocity pointers
      real                , intent(in   ) :: dsto(*)                !< dispersion stochi factors
      real                , intent(in   ) :: vsto(*)                !< velocity stochi factors
      character(len=20)   , intent(in   ) :: locnam(*)              !< name of the local variables
      integer             , intent(in   ) :: nopred                 !< number of predfined values
      integer             , intent(in   ) :: nocons                 !< number of constants
      integer             , intent(in   ) :: nopa                   !< number of parameters
      integer             , intent(in   ) :: nofun                  !< number of functions
      integer             , intent(in   ) :: nosfun                 !< number of segment functions
      integer             , intent(in   ) :: notot                  !< number of substances
      integer             , intent(in   ) :: noloc                  !< number of local values
      integer             , intent(in   ) :: nodisp                 !< number of dispersions
      integer             , intent(in   ) :: novelo                 !< number of velocities
      integer             , intent(in   ) :: ndspx                  !< number of dispersions
      integer             , intent(in   ) :: nvelx                  !< number of velocities
      integer             , intent(in   ) :: nlocx                  !< number of local values on exchanges
      integer             , intent(in   ) :: nosys                  !< number of active substances
      integer             , intent(in   ) :: nogrid                 !< number of grids
      character(len=20)   , intent(in   ) :: dename(*)              !< default names
      character(len=20)   , intent(in   ) :: coname(*)              !< constant names
      character(len=20)   , intent(in   ) :: paname(*)              !< parameter names
      character(len=20)   , intent(in   ) :: funame(*)              !< function names
      character(len=20)   , intent(in   ) :: sfname(*)              !< segm.func. names
      character(len=20)   , intent(in   ) :: syname(*)              !< substance names
      integer             , intent(in   ) :: intopt                 !< integration sub options
      integer             , intent(inout) :: lun(*)                 !< unit numbers
      character(len=*)    , intent(in   ) :: lchar(*)               !< filenames
      integer             , intent(in   ) :: noutp                  !< total number of output files
      integer             , intent(in   ) :: ioutps(7,*)            !< (old) output structure
      type(outputcoll)    , intent(in   ) :: outputs                !< output structure
      integer             , intent(in   ) :: ndmpar                 !< number of dump areas
      integer             , intent(in   ) :: nbufmx                 !< maximum buffer length
      real                , intent(in   ) :: versio                 !< version number proces definition file
      integer             , intent(in   ) :: ndspn                  !< number of new dispersions
      integer             , intent(in   ) :: nveln                  !< number of new velocities
      integer             , intent(in   ) :: nrref                  !< maximum nr of references to be resolved
      integer             , intent(in   ) :: proref(nrref,*)        !< input items to be resolved for each process
      integer             , intent(  out) :: nproc                  !< number of processes
      integer             , intent(  out) :: nflux                  !< number of fluxes
      integer             , intent(  out) :: novar                  !< number of variables
      integer             , intent(  out) :: nipmsa                 !< actual length of pmsa array

      ! local

      integer                             :: mxpmsa                 !  maximum length of pmsa array
      integer                             :: nbpr                   !  number of active processes
      integer                             :: ioffx                  !  offset to the exchange items
      integer                   :: no_ins          ! number of output items
      integer                   :: no_ine          ! number of output items
      integer                   :: no_ous          ! number of output items
      integer                   :: no_oue          ! number of output items
      integer                   :: no_flu          ! number of output items
      integer                   :: no_sto          ! number of output items
      integer                   :: no_dis          ! number of output items
      integer                   :: no_vel          ! number of output items
      integer      , pointer :: nsvar(:)
      integer      , pointer :: iflux(:)
      integer      , pointer :: ipmsa(:)
      integer      , pointer :: ipssa(:)
      integer      , pointer :: prvvar(:)
      integer      , pointer :: prvtyp(:)
      integer      , pointer :: progrd(:)
      integer      , pointer :: prondt(:)
      real         , pointer :: stochi(:)
      character*10 , allocatable :: pronam(:)
      character*20,allocatable  :: varnam(:)       ! variable names
      integer     ,allocatable  :: vararr(:)       ! variable array
      integer     ,allocatable  :: varidx(:)       ! variable index
      integer     ,allocatable  :: vartda(:)       ! variable type of dis-aggregation
      integer     ,allocatable  :: vardag(:)       ! variable dis-aggregation variable
      integer     ,allocatable  :: vartag(:)       ! variable type of aggregation
      integer     ,allocatable  :: varagg(:)       ! variable aggregation variable
      integer                             :: iproc                  !  loop counter processes
      integer                             :: ierr2                  !  local error indication
      integer(4)                :: ithndl = 0        ! handle for performance timer
      if (timon) call timstrt( "wr_proceswrk", ithndl )

      ! count active processes (merge nbpr and nproc?)

      nproc = 0
      nflux = 0

      nbpr  = 0
      do iproc = 1, procesdef%cursize
         if ( procesdef%procesprops(iproc)%active ) then
            nbpr = nbpr + 1
         endif
      enddo

      ! calculate new totals

      call proc_totals( lurep , procesdef, no_ins  , no_ine, no_ous,
     +                  no_oue, no_flu   , no_sto  , no_dis, no_vel)

      ! calculate and fill output structure

      nipmsa = 0
      ioffx = nopred+nocons+nopa+nofun+nosfun+notot+noloc+nodef
      mxpmsa = no_ine+no_ins+no_ous+no_oue+no_flu
      allocate (nsvar(nbpr))
      allocate (iflux(nbpr))
      allocate (ipmsa(mxpmsa))
      allocate (ipssa(mxpmsa))
      allocate (prvvar(mxpmsa))
      allocate (prvtyp(mxpmsa))
      allocate (progrd(nbpr))
      allocate (prondt(nbpr))
      allocate (pronam(nbpr))
      call intoou ( procesdef, nproc , nflux , nsvar , pronam,
     +              iflux    , ipmsa , ipssa , nipmsa, ioffx ,
     +              nocons   , nopa  , nofun , nosfun, notot ,
     +              nodisp   , novelo, nodef , noloc , ndspx ,
     +              nvelx    , nlocx , nopred, prvvar, prvtyp,
     +              novar    , progrd, prondt)

      deallocate(ipmsa,ipssa)

      ! set variables attribute's for aggregation dis-aggregation

      allocate(varnam(novar))
      allocate(vararr(novar))
      allocate(varidx(novar))
      allocate(vartda(novar))
      allocate(vardag(novar))
      allocate(vartag(novar))
      allocate(varagg(novar))
      call setvat ( lurep , nocons, nopa  , nofun , nosfun,
     +              nosys , notot , nodisp, novelo, nodef ,
     +              noloc , ndspx , nvelx , nlocx , nflux ,
     +              nopred, novar , vararr, varidx, vartda,
     +              vardag, vartag, varagg, nogrid, coname,
     +              paname, funame, sfname, dename, syname,
     +              locnam, varnam)
      deallocate(varnam)

      ! write stochi file, set stochi array, balance output settings

      if ( btest(intopt,3) .and. .not. btest(intopt,4) ) then
         call open_waq_files ( lun(36) , lchar(36), 36    , 1     , ierr2 )
      endif
      allocate(stochi(notot*no_flu))
      call wrstoc ( procesdef, lun(36), notot  , syname, stochi,
     +              noutp    , ioutps , outputs, ndmpar, nbufmx,
     +              intopt   )
      if ( btest(intopt,3) .and. .not. btest(intopt,4) ) then
         close ( lun(36) )
      endif

      ! write process intermediate file

      call open_waq_files ( lun(24) , lchar(24), 24    , 1     , ierr2 )
      call wripro ( nproc , nsvar  , iflux , nipmsa, prvvar,
     +              prvtyp, noloc  , nodef , defaul, pronam,
     +              nflux , lun(24), versio, stochi, notot ,
     +              nosys , ndspx  , nvelx , nlocx , dsto  ,
     +              vsto  , ndspn  , idpnw , nveln , ivpnw ,
     +              progrd, prondt , novar , vararr, varidx,
     +              vartda, vardag , vartag, varagg, nrref ,
     &              proref)
      close ( lun(24) )
      deallocate(stochi,nsvar,iflux)
      deallocate(prvvar,prvtyp,progrd,prondt)
      deallocate(pronam)
      deallocate(vararr,varidx,vartda,vardag,vartag,varagg)

      if (timon) call timstop( ithndl )
      return
      end

      end module m_wr_proceswrk
