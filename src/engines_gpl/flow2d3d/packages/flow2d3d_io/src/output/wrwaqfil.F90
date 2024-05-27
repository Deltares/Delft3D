      subroutine wrwaqfil ( mmax   , kmax   , nlb    , nub    , mlb    , &
     &                      mub    , nmaxus , nsrc   , kcs    , kcu    , kcv  , kfsmin , &
     &                      kfsmax , nst    , runid  , xcor   , ycor   , &
     &                      xz     , yz     , guv    , gvu    , guu    , &
     &                      gvv    , gsqs   , vol1   , dtsec  , itdate , &
     &                      tstart , tstop  , dt     , thick  , lsal   , &
     &                      ltem   , lsed   , r1     , areau  , areav  , &
     &                      tau    , vdiff  , dps    , dpd    , chezu  , chezv , &
     &                      chez   , mnksrc , namsrc , nto    , nambnd , mnbnd , &
     &                      zmodel , ztop   , zbot   , gdp    )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
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
!!--description-----------------------------------------------------------------
! Routine is called every time step to allow a direct writing of WAQ files
! Routine is now written in fixed format compatible form, 2nd author is not very
!         much in favour of attempts to make code as incompatible as possible.
!         This holds also for the pseudo C-constructs in logical arithmetic
!!--pseudo code and references--------------------------------------------------
!     subroutines called:
!     wrwaqpnt      to write the grid files and pointer files initially
!!--declarations----------------------------------------------------------------
      use precision
      use dfparall
      use io_ugrid, only: t_ug_meta
      use m_write_waqgeom_curvilinear
      use flow2d3d_version_module
!
      use globaldata
      !
      implicit none
      !
      type(globdat),target :: gdp
      !
      ! The following list of pointer parameters is used to point inside the gdp structure
        !
      integer                 , pointer :: lundia
      integer                 , pointer :: aggre
      integer                 , pointer :: itwqff
      integer                 , pointer :: itwqfi
      integer                 , pointer :: itwqfl
      integer                 , pointer :: itim      !  last writen time
      integer                 , pointer :: lunvol    !  file unit number to an output file
      integer                 , pointer :: lunare    !  file unit number to an output file
      integer                 , pointer :: lunflo    !  file unit number to an output file
      integer                 , pointer :: lunsal    !  file unit number to an output file
      integer                 , pointer :: luntem    !  file unit number to an output file
      integer                 , pointer :: lunvdf    !  file unit number to an output file
      integer                 , pointer :: luntau    !  file unit number to an output file
      integer                 , pointer :: lunsrctmp !  file unit number to an output file
      integer                 , pointer :: lunwlk    !  file unit number to an output file
      integer                 , pointer :: lunsrc    !  file unit number to an output file
      integer                 , pointer :: lunkmk    !  file unit number to an output file
      integer                 , pointer :: noseg     !  number of WAQ segments
      integer                 , pointer :: noq       !  total number of WAQ exchanges
      integer                 , pointer :: noq12     !  number of horizontal WAQ exchanges
      integer                 , pointer :: nobrk     !  number of breakpoints in loads file
      integer                 , pointer :: nowalk    !  number of walking discharges
      integer                 , pointer :: cfoutset
      integer                 , pointer :: wqinset
      integer                 , pointer :: wqiinset
      integer                 , pointer :: wqioutset
      integer , dimension(:)  , pointer :: iwlk      ! walkings
      integer , dimension(:)  , pointer :: isaggr    ! segment aggregation pointer
      integer , dimension(:)  , pointer :: ilaggr    ! layer aggregation pointer
      integer , dimension(:)  , pointer :: iqaggr    ! flow aggregation pointer
      integer , dimension(:,:), pointer :: ifsmax    ! maximum active layer z-model
      integer , dimension(:)  , pointer :: ifrmto    ! from-to pointer table
      integer , dimension(:)  , pointer :: kmk       ! WAQ features at start of step
      integer , dimension(:)  , pointer :: ksrwaq    ! stored value of nr of layers source locations
      integer , dimension(:)  , pointer :: lunsed    ! file unit numbers to sediment concentration output files
      integer , dimension(:,:), pointer :: lunsedflx ! file unit numbers to sediment sedimentation and resuspension flux output files
      real(fp), dimension(:,:), pointer :: quwaq     ! Cumulative qxk
      real(fp), dimension(:,:), pointer :: qvwaq     ! Cumulative qyk
      real(fp), dimension(:,:), pointer :: qwwaq     ! Cumulative qzk
      real(fp), dimension(:,:), pointer :: cumsedflx ! Cumulative sedimentation flux
      real(fp), dimension(:,:), pointer :: cumresflx ! Cumulative resuspension flux
      real(fp), dimension(:)  , pointer :: discumwaq ! Cumulated sources m3/s*nstep
      real(sp), dimension(:)  , pointer :: vol       ! WAQ volume at start of step
      real(sp), dimension(:)  , pointer :: vol2      ! WAQ volume at end of step
      real(sp), dimension(:)  , pointer :: sag       ! WAQ segment aggregator
      real(sp), dimension(:)  , pointer :: sag2      ! WAQ segment aggregator2
      real(sp), dimension(:)  , pointer :: qag       ! WAQ flux aggregator
      real(sp), dimension(:)  , pointer :: horsurf   ! horizontal surface of segments
      real(sp), dimension(:)  , pointer :: loads     ! Value of the loads at last step
      logical                 , pointer :: first_cf
      logical                 , pointer :: firsttime
      logical                 , pointer :: waqfil
      logical                 , pointer :: waqol
      character(256)          , pointer :: flaggr
      real(fp)                , pointer :: mtimstep  ! Maximum step size CFL criterion
      real(fp)                          :: dryflc2   ! Half of drying and flooding treshold (m) / kmax
      logical                 , pointer :: sferic
!
!           Global variables
!
      integer mmax                     !!  Dimension of second index in 2d arrays
      integer kmax                     !!  number of layers
      integer nlb                      !!  Lower bound of all n dimensions
      integer nub                      !!  Upper bound of all n dimensions
      integer mlb                      !!  Lower bound of all m dimensions
      integer mub                      !!  Upper bound of all m dimensions
      integer nmaxus                   !!  User nmax, may be odd, nmax is allways even
      integer nsrc                     !!  Number of sources and sinks
      integer kcs   (nlb:nub,mlb:mub)  !!  Fixed property of the computational volumes
      integer kcu   (nlb:nub,mlb:mub)  !!  Fixed property of the computational volumes
      integer kcv   (nlb:nub,mlb:mub)  !!  Fixed property of the computational volumes
      integer kfsmin(nlb:nub,mlb:mub)  !!  Variable lowest active layer (z-model-only)
      integer kfsmax(nlb:nub,mlb:mub)  !!  Variable upper  active layer (z-model-only)
      integer nst                      !!  Time step number
      integer nto                      !!  Number of open boundaries (tidal openings)
      character(*) runid               !!  To make file names
      real(fp) xcor(nlb:nub,mlb:mub)   !!  X-coordinates of depth points
      real(fp) ycor(nlb:nub,mlb:mub)   !!  Y-coordinates of depth points
      real(fp) xz(nlb:nub,mlb:mub)     !!  X-coordinates of zeta points
      real(fp) yz(nlb:nub,mlb:mub)     !!  Y-coordinates of zeta points
      real(fp) guv                     !!  distance between zeta points over v points
      real(fp) gvu                     !!  distance between zeta points over u points
      real(fp) guu                     !!  distance between depth points over u points
      real(fp) gvv                     !!  distance between depth points over v points
      real(fp) gsqs(nlb:nub,mlb:mub)   !!  horizontal surface areas around zeta points
      real(fp) vol1                    !!  volume array
      real(fp) dtsec                   !!  Time step size in seconds
      integer  itdate                  !!  reference time in YYYYMMDD
      real(fp) tstart                  !!  Flow start time in minutes
      real(fp) tstop                   !!  Flow stop time in minutes
      real(fp) dt                      !!  Flow time step in minutes
      real(fp) thick                   !!  Relative layer thickness sigma coords
      integer  lsal                    !!  Substance number salinity
      integer  ltem                    !!  Substance number temperature
      integer  lsed                    !!  Number of suspended sediment fractions
      real(fp) r1                      !!  Substances array
      real(fp) areau                   !!  Area's in the u points
      real(fp) areav                   !!  Area's in the v points
      real(fp) tau                     !!  Tau's at the bottom
      real(fp) vdiff                   !!  vertical diffusivity
      real(hp) dps(nlb:nub,mlb:mub)    !!  depth of zeta points below ref layer
      real(fp) dpd(nlb:nub,mlb:mub)    !!  depth of corner points below ref layer
      real(fp) chezu                   !!  chezy values in u points
      real(fp) chezv                   !!  chezy values in v points
      logical  chez                    !!  if true, there is a chezy value
      integer       mnksrc(7,nsrc)     !!  location of sources and withdrawals
      character(20) namsrc(  nsrc)     !!  names of the wasteloads
      character(20) nambnd(  nto)      !!  names of the open boundaries
      integer mnbnd(7,nto)             !!  indices of the open boundaries
      logical  zmodel                  !!  true if z-model feature is used
      real(fp) zbot                    !!  Maximum depth in the model (relative to the reference level; unit: metres; positive upwards).
                                       !!  It marks the lower boundary of the grid.
      real(fp) ztop                    !!  The ‘imaginary’ maximum water level in the model (relative to the reference level; unit: metres; positive upwards).
                                       !!  This imaginary level is used only to determine the grid distribution. It does not mark the maximum surface level.
!
!           Local variables
!
      integer  (4) nobnd, nolay        !!  number open boundaries, number of WAQ layers
      character(256) filnam            !!  Filename without extension
      integer  (4) itim2               !!  new time stamp in files
      integer  (4) nd, itop            !!  number of domains
      integer  (4) idt                 !!  aggregated time step size
      integer  (4) mode                !!  0 for initialisation, 1 normal, 2 finalisation
      integer  (4) mode2               !!  help variable
      integer  (4) noq1, noq2, noq3    !!  initially needed nr of exchanges in 3 dir.
      integer  (4) idim                !!  dimension work array
      integer  (4) l, i                !!  loop counter substances, sources
      character(5) sf                  !!  character variable for s(ediment concentration)f(iles)
      character(8) ssrff               !!  character variable for s(ediment) s(edimentation and) r(esuspension) f(lux) f(iles)
      integer  (4) istat               !!  allocate return status
      integer, allocatable :: isaggrl(:) !!  segment aggregation pointer (only top/bottom layer, depending on zmodel)
      integer  (4) ipiv                !!  help variable for array shift
      type(t_ug_meta)     :: meta
      character(300) message
!
!! executable statements -------------------------------------------------------
!
      lundia     => gdp%gdinout%lundia
      aggre      => gdp%gdwaqpar%aggre
      itwqff     => gdp%gdwaqpar%itwqff
      itwqfi     => gdp%gdwaqpar%itwqfi
      itwqfl     => gdp%gdwaqpar%itwqfl
      itim       => gdp%gdwaqpar%itim
      lunvol     => gdp%gdwaqpar%lunvol
      lunare     => gdp%gdwaqpar%lunare
      lunflo     => gdp%gdwaqpar%lunflo
      lunsal     => gdp%gdwaqpar%lunsal
      luntem     => gdp%gdwaqpar%luntem
      lunvdf     => gdp%gdwaqpar%lunvdf
      luntau     => gdp%gdwaqpar%luntau
      lunsrctmp  => gdp%gdwaqpar%lunsrctmp
      lunwlk     => gdp%gdwaqpar%lunwlk
      lunsrc     => gdp%gdwaqpar%lunsrc
      lunkmk     => gdp%gdwaqpar%lunkmk
      noseg      => gdp%gdwaqpar%noseg
      noq        => gdp%gdwaqpar%noq
      noq12      => gdp%gdwaqpar%noq12
      nobrk      => gdp%gdwaqpar%nobrk
      nowalk     => gdp%gdwaqpar%nowalk
      cfoutset   => gdp%gdwaqpar%cfoutset
      wqinset    => gdp%gdwaqpar%wqinset
      wqiinset   => gdp%gdwaqpar%wqiinset
      wqioutset  => gdp%gdwaqpar%wqioutset
      iwlk       => gdp%gdwaqpar%iwlk
      isaggr     => gdp%gdwaqpar%isaggr
      ilaggr     => gdp%gdwaqpar%ilaggr
      iqaggr     => gdp%gdwaqpar%iqaggr
      ifsmax     => gdp%gdwaqpar%ifsmax
      ifrmto     => gdp%gdwaqpar%ifrmto
      kmk        => gdp%gdwaqpar%kmk
      ksrwaq     => gdp%gdwaqpar%ksrwaq
      lunsed     => gdp%gdwaqpar%lunsed
      lunsedflx  => gdp%gdwaqpar%lunsedflx
      quwaq      => gdp%gdwaqpar%quwaq
      qvwaq      => gdp%gdwaqpar%qvwaq
      qwwaq      => gdp%gdwaqpar%qwwaq
      cumsedflx  => gdp%gdwaqpar%cumsedflx
      cumresflx  => gdp%gdwaqpar%cumresflx
      discumwaq  => gdp%gdwaqpar%discumwaq
      vol        => gdp%gdwaqpar%vol
      vol2       => gdp%gdwaqpar%vol2
      sag        => gdp%gdwaqpar%sag
      sag2       => gdp%gdwaqpar%sag2
      qag        => gdp%gdwaqpar%qag
      horsurf    => gdp%gdwaqpar%horsurf
      loads      => gdp%gdwaqpar%loads
      first_cf   => gdp%gdwaqpar%first_cf
      firsttime  => gdp%gdwaqpar%firsttime
      waqfil     => gdp%gdwaqpar%waqfil
      waqol      => gdp%gdwaqpar%waqol
      flaggr     => gdp%gdwaqpar%flaggr
      mtimstep   => gdp%gdwaqpar%mtimstep

      dryflc2    =  gdp%gdnumeco%dryflc/2.0/kmax
      
      sferic              => gdp%gdtricom%sferic
    
!
      if (     .not. waqfil ) return
      if ( nst .lt.  itwqff ) return
      if ( nst .gt.  itwqfl ) return
      if ( mod(nst-itwqff,itwqfi) .ne. 0 ) return
!
!           First instance: initialize output, write time-independent files
!
      mode = 1
      if ( nst .eq. itwqfl ) mode = 2
      if ( firsttime ) then
         firsttime = .false.
         mode = 0
         mtimstep = -1.0
         if (parll) then
            write(filnam,'(3a,i3.3,a)') 'com-', trim(runid), '-', inode, '.'
         else
            filnam ='com-'//trim(runid)//'.'
         endif
!           allocate all integer arrays that are needed
!                    ifrmto is the maximally dimensioned from,to,from-1,to+1 table
!                    isaggr: pointer from i,j,k to segment nr (optionally aggregated)
!                    iqaggr: pointer from i,j,k to flow nr    (optionally aggregated)
!                    ilaggr: pointer from k to waq layer nr   (optionally aggregated)

                       allocate ( gdp%gdwaqpar%ifrmto   (12*nmaxus*mmax*kmax) , stat=istat)
         if (istat==0) allocate ( gdp%gdwaqpar%isaggr   (   nmaxus*mmax*kmax) , stat=istat)
         if (istat==0) allocate ( gdp%gdwaqpar%iqaggr   ( 3*nmaxus*mmax*kmax) , stat=istat)
         if (istat==0) allocate ( gdp%gdwaqpar%lunsed   ( max(1,lsed)       ) , stat=istat)
         if (istat==0) allocate ( gdp%gdwaqpar%lunsedflx( lsed     , 2      ) , stat=istat)
         if (istat/=0) then
            write(*,*) '*** ERROR: wrwaqfil: memory allocation error'
            return
         endif
         itop = 1
         if ( zmodel ) then
            itop = kmax
         endif
         if (istat==0) allocate ( gdp%gdwaqpar%kmk (nmaxus*mmax*kmax) , stat=istat)
         if (istat/=0) then
            write(*,*) '*** ERROR: wrwaqfil: memory allocation error'
            return
         endif

!        update local pointers

         ifrmto     => gdp%gdwaqpar%ifrmto
         isaggr     => gdp%gdwaqpar%isaggr
         iqaggr     => gdp%gdwaqpar%iqaggr
         lunsed     => gdp%gdwaqpar%lunsed
         lunsedflx  => gdp%gdwaqpar%lunsedflx
         kmk        => gdp%gdwaqpar%kmk

!           write the .lga .lgo .lgt and the .poi file
!           make the ilaggr and the iqaggr arrays for aggregation

         call wrwaqpnt ( nmaxus , mmax   , kmax   , nlb    , nub    ,    &
     &                   mlb    , mub    , kcs    , kfsmin , isaggr ,    &
     &                   ilaggr , iqaggr , ifrmto , aggre  , flaggr ,    &
     &                   noseg  , noq1   , noq2   , noq3   , noq    ,    &
     &                   nobnd  , kmk    , zmodel , filnam , lundia)

!
!           allocate the real aggregation arrays that are needed:
!                    sag for aggregation on segment basis (dimension 0:noseg)
!                    qag for aggregation on flux    basis (dimension 0:noq  )
!                    and 2 volume arrays
!
         idim = (noseg+1)*max(lsed,1)
                       allocate ( gdp%gdwaqpar%vol    (0:noseg) , stat=istat)
         if (istat==0) allocate ( gdp%gdwaqpar%sag    (  idim ) , stat=istat)
         if (istat==0) allocate ( gdp%gdwaqpar%vol2   (0:noseg) , stat=istat)
         if (istat==0) allocate ( gdp%gdwaqpar%sag2   (0:noseg) , stat=istat)
         if (istat==0) allocate ( gdp%gdwaqpar%qag    (0:noq  ) , stat=istat)
         if (istat==0) allocate ( gdp%gdwaqpar%horsurf(  noseg) , stat=istat)

!           allocate special arrays that are needed for sources

         if ( nsrc .gt. 0 ) then
            if (istat==0) allocate ( gdp%gdwaqpar%loads    (  nsrc) , stat=istat)
            if (istat==0) allocate ( gdp%gdwaqpar%discumwaq(  nsrc) , stat=istat)
            if (istat==0) allocate ( gdp%gdwaqpar%iwlk     (  nsrc) , stat=istat)
            if (istat==0) allocate ( gdp%gdwaqpar%ksrwaq   (2*nsrc) , stat=istat)
            do i = 1, nsrc
               if (mnksrc(7,i) == 4 .or. mnksrc(7,i) == 5 .or. mnksrc(7,i) == 8) then
                  write ( message , '(3A)' ) '*** WARNING: no WAQ communication data for culvert ''', &
                        trim(namsrc(i)), ''' will be written because it is a type d, e or f. Contact support!'
                  write( *      , '(A)' ) trim(message)
                  write( lundia , '(A)' ) trim(message)
               end if
            enddo
         endif
         if (istat/=0) then
            write(*,*) '*** ERROR: wrwaqfil: memory allocation error'
            return
         endif

!        update local pointers

         vol        => gdp%gdwaqpar%vol
         sag        => gdp%gdwaqpar%sag
         vol2       => gdp%gdwaqpar%vol2
         sag2       => gdp%gdwaqpar%sag2
         qag        => gdp%gdwaqpar%qag
         horsurf    => gdp%gdwaqpar%horsurf
         loads      => gdp%gdwaqpar%loads
         discumwaq  => gdp%gdwaqpar%discumwaq
         iwlk       => gdp%gdwaqpar%iwlk
         ksrwaq     => gdp%gdwaqpar%ksrwaq

        !
!           write the .hyd file
        !
        if (parll) then
            write(filnam,'(3a,i3.3,a)') 'com-', trim(runid), '-', inode
        else
            filnam ='com-'//trim(runid)
        endif

         nd = gdp%gdprognm%numdomains
         call wrwaqhyd (filnam , itdate , tstart , tstop  , dt     ,    &
                        itwqff , itwqfl , itwqfi , nmaxus , mmax   ,    &
                        kmax   , thick  , lsal   , ltem   , lsed   ,    &
                        chez   , nsrc   , mnksrc , namsrc , runid  ,    &
                        nowalk , iwlk   , aggre  , flaggr , zmodel ,    &
                        ilaggr , nd     , nlb    , nub    , mlb    ,    &
                        mub    , kfsmin , ksrwaq , noseg  , noq1   ,    &
                        noq2   , noq3   , xz     , yz     , zbot   ,    &
                        ztop   , gdp)
         if (parll) then
            write(filnam,'(3a,i3.3,a)') 'com-', trim(runid), '-', inode, '.'
         else
            filnam ='com-'//trim(runid)//'.'
         endif

!           write the .cco file

         call wrwaqcco ( nmaxus , mmax, ilaggr(kmax), nlb  , nub    ,    &
     &                   mlb    , mub , xcor        , ycor , filnam )

         !
         !
         ! global attributes
         !
         meta%institution = "Deltares"
         meta%source      = "Delft3D-FLOW"   
         meta%references  = "http://www.deltares.nl"    
         call getfullversionstring_flow2d3d(meta%version)
         meta%modelname   = filnam(1:(len(trim(filnam))-1))

         allocate ( isaggrl(nmaxus*mmax) , stat=istat)
         if (istat/=0) then
            write(*,*) '*** ERROR: wrwaqfil: memory allocation error'
            return
         endif
         ipiv = 0
         if ( zmodel ) ipiv = ( kmax - 1 ) * nmaxus * mmax
         isaggrl( 1 : nmaxus * mmax ) = isaggr ( 1 + ipiv : nmaxus * mmax + ipiv )
         
         call wrwaqgeomcl( meta   , lundia, nmaxus , mmax  , kmax  , &
                           nlb    , nub    , mlb   , mub   ,         &
                           xcor   , ycor  , xz     , yz    , dpd   , &
                           kcs    , kcu   , kcv    , sferic, aggre , &
                           isaggrl, nto   , nambnd , mnbnd)
         deallocate ( isaggrl , stat = istat )

!           open all files for time dependent write
!           WARNING: WAQ input files must be written using form=binary
!                    instead of unformatted.
!                    Although it is not standard Fortran

! This part is copied for binary and unformatted
#ifdef HAVE_FC_FORM_BINARY

         open  ( newunit = lunvol , file=trim(filnam)//'vol' , form = 'binary' , SHARED )
         open  ( newunit = lunare , file=trim(filnam)//'are' , form = 'binary' , SHARED )
         open  ( newunit = lunflo , file=trim(filnam)//'flo' , form = 'binary' , SHARED )
         if ( lsal .gt. 0 ) then
            open  ( newunit = lunsal , file=trim(filnam)//'sal' , form = 'binary' , SHARED )
         endif
         if ( ltem .gt. 0 ) then
            open  ( newunit = luntem , file=trim(filnam)//'tem' , form = 'binary' , SHARED )
         endif
         do l = 1, lsed
            sf = "sed00"
            write( sf(4:5), '(i2.2)' ) l
            open  ( newunit = lunsed(l), file=trim(filnam)//sf  , form = 'binary' , SHARED )
            ! sedimentation
            ssrff = "sedflx00"
            write( ssrff(7:8), '(i2.2)' ) l
            open  ( newunit = lunsedflx(l,1), file=trim(filnam)//ssrff  , form = 'binary' , SHARED )
            ! resuspension
            ssrff = "resflx00"
            write( ssrff(7:8), '(i2.2)' ) l
            open  ( newunit = lunsedflx(l,2), file=trim(filnam)//ssrff  , form = 'binary' , SHARED )
         enddo
         if ( ilaggr(kmax) .gt. 1 ) then
            open  ( newunit = lunvdf , file=trim(filnam)//'vdf' , form = 'binary' , SHARED )
         endif
         open  ( newunit = luntau , file=trim(filnam)//'tau' , form = 'binary' , SHARED )
 !       open  ( newunit = lunfmap, file=trim(filnam)//'fmap', form = 'binary' )
         if ( nsrc .gt. 0 ) then
            open  ( newunit = lunsrctmp , file='TMP_'//trim(filnam)//'src' , SHARED )
            if ( nowalk .gt. 0 ) then
               open  ( newunit = lunwlk , file=trim(filnam)//'wlk' )
            endif
            open  ( newunit = lunsrc , file=trim(filnam)//'src' )    ! final file
         endif
#else
         open  ( newunit = lunvol , file=trim(filnam)//'vol' , form = 'unformatted', access='stream')
         open  ( newunit = lunare , file=trim(filnam)//'are' , form = 'unformatted', access='stream')
         open  ( newunit = lunflo , file=trim(filnam)//'flo' , form = 'unformatted', access='stream')
         if ( lsal .gt. 0 ) then
            open  ( newunit = lunsal , file=trim(filnam)//'sal' , form = 'unformatted', access='stream')
         endif
         if ( ltem .gt. 0 ) then
            open  ( newunit = luntem , file=trim(filnam)//'tem' , form = 'unformatted', access='stream')
         endif
         do l = 1, lsed
            sf = "sed00"
            write( sf(4:5), '(i2.2)' ) l
            open  ( newunit = lunsed(l), file=trim(filnam)//sf  , form = 'unformatted', access='stream')
            ! sedimentation
            ssrff = "sedflx00"
            write( ssrff(7:8), '(i2.2)' ) l
            open  ( newunit = lunsedflx(l,1), file=trim(filnam)//ssrff  , form = 'unformatted', access='stream')
            ! resuspension
            ssrff = "resflx00"
            write( ssrff(7:8), '(i2.2)' ) l
            open  ( newunit = lunsedflx(l,2), file=trim(filnam)//ssrff  , form = 'unformatted', access='stream')
         enddo
         if ( ilaggr(kmax) .gt. 1 ) then
            open  ( newunit = lunvdf , file=trim(filnam)//'vdf' , form = 'unformatted', access='stream')
         endif
         open  ( newunit = luntau , file=trim(filnam)//'tau' , form = 'unformatted', access='stream')
 !       open  ( newunit = lunfmap, file=trim(filnam)//'fmap', form = 'unformatted', access='stream')
         if ( nsrc .gt. 0 ) then
            open  ( newunit = lunsrctmp , file='TMP_'//trim(filnam)//'src' )
            if ( nowalk .gt. 0 ) then
               open  ( newunit = lunwlk , file=trim(filnam)//'wlk' )
            endif
            open  ( newunit = lunsrc , file=trim(filnam)//'src' )    ! final file
         endif
#endif

!           write the .srf and .len file (non trivial with aggregation)

         call wrwaqsrf ( nmaxus , mmax   , kmax   , nlb    , nub    ,    &
     &                   mlb    , mub    , gsqs   , guv    , gvu    ,    &
     &                   guu    , gvv    , xcor   , ycor   , xz     ,    &
     &                   yz     , dps    , chezu  , chezv  , chez   ,    &
     &                   noseg  , noq1   , noq2   , noq3   , nobnd  ,    &
     &                   aggre  , isaggr , iqaggr , ilaggr , ifrmto ,    &
     &                   horsurf, itop   , filnam )
         noq12 = noq1 + noq2

!           compute first volume record, and
!           write all other segment related things

         itim = nst*dtsec + 0.5
         vol  = 0.0
         call wrwaqvol ( nmaxus , mmax   , kmax   , nlb    , nub    ,    &
     &                   mlb    , mub    , itim   , vol1   , kcs    ,    &
     &                   kfsmin , kfsmax , gsqs   , lsal   , ltem   ,    &
     &                   lsed   , r1     , tau    , vdiff  , isaggr ,    &
     &                   noseg  , vol    , sag    , sag2   , zmodel ,    &
     &                   ilaggr , mode   , lunvol , lunsal , luntem ,    &
     &                   lunsed , lunvdf , luntau , itdate , cumsedflx,  &
     &                   cumresflx, lunsedflx, itwqfi*2    )

!           write first part of the sources files where appropriate
     if ( nsrc > 0 ) then
            call wrwaqld0 ( nsrc      , nmaxus , mmax   , kmax   , mnksrc , &
        &                   discumwaq , loads  , nobrk  , nowalk , iwlk   , &
        &                   itim      , mode   , isaggr , lunwlk )
     endif
         return
!
!        End of (firsttime), note the return
!
      endif
      nolay = ilaggr(kmax)
!
!
!           Write exchange related items (the area's
!           Compute flows over the last period
!
      call wrwaqflo ( nmaxus , mmax   , kmax   , nlb    , nub    ,       &
     &                mlb    , mub    , itim   , kfsmin , quwaq  ,       &
     &                qvwaq  , qwwaq  , areau  , areav  , noseg  ,       &
     &                horsurf, iqaggr , noq    , noq12  , qag    ,       &
     &                zmodel , itwqfi*2,lunare )
!
!           The step by step online coupling with WAQ
!
      if ( waqol ) then
         !
         ! FLUSH is not supported by Intel compiler on Linux
         !
         !call flush ( lunvol )
         !call flush ( lunare )
         !call flush ( lunflo )
         !call flush ( lunvdf )
         !call flush ( luntau )
         !if ( lsal .gt. 0 ) call flush ( lunsal )
         !if ( ltem .gt. 0 ) call flush ( luntem )
         !if (    zmodel   ) call flush ( lunkmk )
         if ( first_cf ) then
            first_cf = .false.
            call putdio ( 'CFtoWQ' , 'DataCFtoWQ'  , .true. , cfoutset  )
            write(*,*) 'put CFtoWQ, flow initialized 1'
            call getdio ( 'WQtoWQI', 'DataWQtoWQI' , .true. , wqiinset  )
            write(*,*) 'get WQtoWQI, flow initialized 1'
            call putdio ( 'WQItoWQ', 'DataWQItoWQ' , .true. , wqioutset )
            write(*,*) 'put WQItoWQ flow initialize 2'
            call getdio ( 'WQtoCF' , 'DataWQtoCF'  , .true. , wqinset   )
            write(*,*) 'get WQtoCF flow initialize 2'
         else
            call getdio ( 'WQtoCF' , 'DataWQtoCF'  , .false., wqinset   )
            write(*,*) 'get WQtoCF flow from WAQ, using the stream , waiting for WAQ 3'
         endif
         call putdio ( 'CFtoWQ' , 'DataCFtoWQ'  , .false., cfoutset  )
         write(*,*) 'put CFtoWQ, flow put, using the stream 3'
         call getdio ( 'WQtoWQI', 'DataWQtoWQI' , .false., wqiinset   )
         write(*,*) 'get WQtoWQI, waq get 4'
         call putdio ( 'WQItoWQ', 'DataWQItoWQ' , .false., wqioutset )
         write(*,*) 'put WQItoWQ, flow put 4'
      endif
!
!           Write salinity and temperature at end of this time step
!           Compute the new volumes
!
      itim2 = nst*dtsec + 0.5
      call wrwaqvol ( nmaxus , mmax   , kmax   , nlb    , nub    ,    &
     &                mlb    , mub    , itim2  , vol1   , kcs    ,    &
     &                kfsmin , kfsmax , gsqs   , lsal   , ltem   ,    &
     &                lsed   , r1     , tau    , vdiff  , isaggr ,    &
     &                noseg  , vol2   , sag    , sag2   , zmodel ,    &
     &                ilaggr , mode   , lunvol , lunsal , luntem ,    &
     &                lunsed , lunvdf , luntau , itdate , cumsedflx,  &
     &                cumresflx, lunsedflx, itwqfi*2    )
!
!           Check mass balance, write loads and write deficiencies
!
      idt  = itim2 - itim
      mode2 = mode
      if ( itim .eq. int( itwqff*dtsec + 0.5 ) ) mode2 = 0
      call wrwaqbal ( noseg     , noq12  , noq    , vol    , vol2   ,       &
     &                qag       , horsurf, ifrmto , nmaxus , mmax   ,       &
     &                kmax      , isaggr , nolay  , nsrc   , mnksrc ,       &
     &                discumwaq , loads  , nowalk , iwlk   , itim   ,       &
     &                idt       , nobrk  , zmodel , mode2  , itwqfi*2,      &
     &                lunsrctmp , lunwlk , sag    , mtimstep, lundia )

      write ( lunflo ) itim , qag (1:noq)
      itim = itim2
!
!           Last instance: finalize output with a dummy area and flow record
!                          rearrange the wasteload file
!
      if ( mode .eq. 2 ) then
         qag = 0.0
         write ( lunare ) itim2, qag(1:noq)
         write ( lunflo ) itim2, qag(1:noq)
         call wrwaqload ( nsrc   , nmaxus , mmax   , kmax     , mnksrc ,    &
     &                    nolay  , nobrk  , nowalk , iwlk     , isaggr ,    &
     &                    zmodel , itim2  , ksrwaq , lunsrctmp, lunwlk ,    &
     &                    lunsrc )
         close ( lunvol )                      ! volume
         close ( lunare )                      ! area
         close ( lunflo )                      ! flow
         if ( lsal .gt. 0 ) close ( lunsal )   ! salinity
         if ( ltem .gt. 0 ) close ( luntem )   ! temperature
         close ( lunvdf )                      ! vertical diffusion
         close ( luntau )                      ! tau at the bottom
      endif
      end subroutine wrwaqfil
