!!  Copyright (C)  Stichting Deltares, 2012-2024.
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
module m_rdpart
use m_stop_exit
use m_skip_comments
use m_polpart
use m_rdwnd


implicit none

contains


      subroutine rdpart ( lun1   , lun2   , lnam1  )

!       Deltares Software Centre

!>\file
!>                          Reads the delpar input file

!       Created           : July      2011 by Leo Postma

!       Modified          :

!       Subroutines called:

!       Functions called  :

!       Logical units     : lun1 = delpar input file
!                           lun2 = delpar report file

      use rd_token        ! tokenized reading like in DELWAQ
      use m_waq_precision  ! flexible size definition
      use timers          ! performance timers
      use partmem         ! <== this is the data-block that is filled by this routine
      use m_part_modeltypes       ! part model definitions
      use m_part_regular
      use m_part_mesh
      use typos           ! derived types
      use alloc_mod       ! to allocate arrays
      use fileinfo        ! file information for all input/output files
      use openfl_mod      ! explicit interface
      use string_module   ! string conversion tools
      use spec_feat_par   ! special feature parameters

      implicit none

!     Arguments

!     kind            function         name           description

      integer  ( int_wp ), intent(inout) :: lun1          !< unit number input file
      integer  ( int_wp ), intent(inout) :: lun2          !< unit number report file
      character(256), intent(in   ) :: lnam1         !< name of the input file

!     Locals

      integer  ( int_wp )                 lunfil
      integer  ( int_wp )                 ierr            ! cumulative error variable
      integer  ( int_wp )                 ierr2           ! local error variable
      integer  ( 4 )                 itype           ! returned type of gettoken
      integer  ( int_wp )                 ioption         ! option variable for input
      character( 32)                 filvers         ! to read the file version number
      character( 32)                 cwork           ! small character workstring
      character(256)                 cbuffer         ! character buffer
      integer  ( int_wp )                 ibuffer         ! integer buffer
      integer  ( int_wp )                 i, k            ! loop variables
      integer  ( int_wp )                 ios             ! help variable io-status
      integer  ( int_wp )                 nodac           ! help variable nodye + nocont
      integer  ( int_wp )                 ifract          ! help variable oil fractions
      integer  ( int_wp )                 isb, jsub       ! help variables for substances
      integer  ( int_wp )                 ilay            ! help variable layers
      integer  ( int_wp )                 ln              ! help variable for lengthes of strings
      integer  ( int_wp )                 nvsfour         ! number of settling harmonics
      integer  ( int_wp )                 lunin           ! help variable additional files
      integer  ( int_wp )                 id, ih, im, is  ! help variables to read times
      real     ( sp)                 xw1f, xw2f      ! x-coordinates plot window
      real     ( sp)                 yw1f, yw2f      ! y-coordinates plot window
      integer  ( int_wp )                 idummy          ! for not used information
      real     ( sp), allocatable :: ascal(:)        ! array with scale factors
      character(1)                   cchar_save      ! save value from Delwaq
      integer  ( int_wp )                 lunut_save      ! save value from Delwaq
      integer  ( int_wp )                 npos_save       ! save value from Delwaq
      real     ( sp), pointer     :: xpoltmp(:)      ! temp x-coordinates polygon
      real     ( sp), pointer     :: ypoltmp(:)      ! temp y-coordinates polygon
      integer  ( int_wp )                 nrowstmp        ! temp length polygon
      integer  ( int_wp )                 npmargin        ! allocation margin in number of particles
      integer  ( int_wp )              :: ipc             ! numerical integration scheme

      character( 20)                 cplastic        ! plastic name
      real     ( sp)                 rdpldensity     ! read plastic density
      real     ( sp)                 rdplshapefactor ! read plastic shapefactor
      real     ( sp)                 rdplmeansize    ! read plastic meansize
      real     ( sp)                 rdplvarsize     ! read plastic stdevsize
      real     ( sp)                 rdplmusize      ! read plastic meansize
      real     ( sp)                 rdplsigmasize   ! read plastic stdevsize
      real     ( sp)                 rdplfragrate    ! read plastic fragmentation rate
      integer  ( int_wp )                 plmissing

      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /

      if ( timon ) call timstrt( "rdpart", ithndl )

      ierr     = 0
      npmax    = 0
      nrowsmax = 0

!       initialize the tokenized reading facility

      if ( alone ) then                 ! clean initialization
         ilun(1) = lun1
         lch (1) = lnam1
         i = 1
      else                              ! add delpar to the delwaq read-stack
         do i = 1 , lstack              ! at the first free entry
            if ( ilun(i) .ne. 0 .and. lch (i) .eq. lnam1) exit
            if ( ilun(i) .ne. 0 ) cycle
            ilun(i) =  900 + i
            lch (i) =  lnam1
            exit
         enddo
         write  ( lun2, '(/'' including file: '',a )' ) lch(i)
         cchar_save = cchar
         lunut_save = lunut
         npos_save  = npos
      endif
      cchar   = ';'
      lunut   = lun2
      push    = .false.
      npos    = 200
      iposr   =   0
      open  ( newunit=ilun(i), file=lch(i), iostat=ios) ! File might already be open
      if (ios.ne.0 .and. ios.ne.5004) then
         write ( lun2, * ) ' Error opening PART input file'
         call stop_exit(1)
      endif
      rewind (ilun(i))                          ! Be sure to rewind!
      write ( lun2, * )

!       check the file version

      if ( gettoken( filvers, ierr2 ) .ne. 0 ) goto 4001
      if ( filvers(1:19) .eq. 'delpar_version_3.60' ) filvers(1:19) = 'v3.60.00'
      if ( filvers(2:19) .lt. '3.66.00') then
         write(lun2,'(2x,a  )') ' Obsolete input file '
         write(lun2,'(2x,a  )') ' Version found             : ',filvers(1:19)
         write(lun2,'(2x,a  )') ' Lowest version requested  : ','V3.66.00'
         call stop_exit(1)
      endif

!       read titles

      if ( gettoken( title(1), ierr2 ) .ne. 0 ) goto 4002
      if ( gettoken( title(2), ierr2 ) .ne. 0 ) goto 4002
      if ( gettoken( title(3), ierr2 ) .ne. 0 ) goto 4002
      if ( gettoken( title(4), ierr2 ) .ne. 0 ) goto 4002
      write ( lun2, 2000 ) title(1)
      write ( *   , 2000 ) title(1)

!     read the name of the hyd file for ui delft3d (ignored here for now)

      if ( gettoken( cbuffer, ierr2 ) .ne. 0 ) goto 4003
      write ( lun2, 1997 ) cbuffer
      write ( *   , 1997 ) cbuffer

!     read modeltype , and number of tracks, extra number of output substances and option for sed/erosion
!
!     Type of model: 1 - tracer (basic module)
!                    2 - (obsolete option)
!                    3 - (obsolete option)
!                    4 - oil model
!                    5 - (obsolete option)
!                    6 - Probabilistic density driven settling model
!                    7 - ABM model

      if ( gettoken( modtyp , ierr2 ) .ne. 0 ) goto 11
      if ( gettoken( notrak , ierr2 ) .ne. 0 ) goto 11
      if ( gettoken( idummy, ierr2 ) .ne. 0 ) goto 11
      if ( gettoken( ioption, ierr2 ) .ne. 0 ) goto 11
      if ( ioption .eq. 0 ) then
         lsettl = .false.
         write ( lun2, * ) ' Sedimentation-erosion processes are inactive'
         noslay = nolayp
      else
         lsettl = .true.
         write ( lun2, * ) ' Sedimentation-erosion processes are enabled'
         noslay = nolayp + 1
      endif

!     adapt variables for number of substances and layer-thickness

      write ( *, * ) ' Number of layers            : ', nolayp
      if ( notrak .eq. 0 ) then
         write ( lun2, '(/a)' ) '  Particle tracks not written to tracking file'
      elseif ( notrak .eq. 1 ) then
        write ( lun2, '(a)' ) '    Particle tracks written to tracking file'
!         write ( lun2, '(/a)' ) '  Particle tracks written to tracking file every calculation time step'
      elseif ( notrak .gt. 1 ) then
         write ( lun2, '(/a,i0,a)' ) '  Particle tracks written to tracking file at ', notrak, ' times the calculation time step'
      else
         write ( lun2, '(/a)' ) '  Error 1002. Particle tracks interval multiplier is negative!'
         ierr = ierr + 1
      endif
      oil = modtyp .eq. model_oil

!     read numerical parameters

      if ( gettoken( ipc  , ierr2 ) .ne. 0 ) goto 4007
      if ( gettoken( idelt, ierr2 ) .ne. 0 ) goto 4007
      if ( idelt .le. 0 ) idelt = ihdel
      write ( lun2, * ) '   '
      write ( lun2, * ) ' Numerical scheme            : ', ipc
      write ( lun2, * ) '   '
      write ( lun2, * ) ' Time step for hydrodynamics : ', ihdel, ' seconds '
      write ( lun2, * ) ' Time step for part. tracking: ', idelt, ' seconds '
      write ( lun2, * ) '   '
      if ( ihdel .lt. idelt ) then
         write ( lun2, 2023 )
         call stop_exit(1)
      endif
      i = ihdel/idelt
      if ( ihdel .ne. i*idelt ) then
         write ( lun2, 2022 )
         write ( *   , 2022 )
         call stop_exit(1)
      endif
      if ( ipc /= 1 ) then
         write ( lun2, '(/A,I3)')  'Error: Numerical scheme must be 1. &
                        Current value given is: ', ipc
         call stop_exit(1)
      endif

!    read vertical diffusivity parameters

      if ( gettoken( ioptdv , ierr2 ) .ne. 0 ) goto 4008
      if ( gettoken( alpha  , ierr2 ) .ne. 0 ) goto 4008
      if ( gettoken( cdisp  , ierr2 ) .ne. 0 ) goto 4008
      select case ( ioptdv )
         case ( 0 )
            if ( alpha .gt. 0.0 .and. cdisp .gt. 0.0 ) then
               write ( lun2, * ) '   '
               write ( lun2, * ) ' Vertical diff. : ', cdisp,' m2/s'
               write ( lun2, * ) ' Scale factor   : ', alpha
               write ( lun2, * ) '   '
               ldiffz = .true.
            else
               write ( lun2, * ) '   '
               write ( lun2, * ) ' Vertical diffusivity  is zero  '
               write ( lun2, * ) '   '
               ldiffz = .false.
            endif
         case ( 1 )
            if ( alpha .eq. 1.0 ) then
               write ( lun2, * ) '   '
               write ( lun2, * ) ' Vertical diffusion from depth-averaged algebraic model'
               write ( lun2, * ) '   '
               ldiffz = .true.
            elseif ( alpha .gt. 0.0 ) then
               write ( lun2, * ) '   '
               write ( lun2, * ) ' Vertical diffusion from depth-averaged algebraic model'
               write ( lun2, * ) ' Scale factor for diffusion : ',alpha
               write ( lun2, * ) '   '
               ldiffz = .true.
            else
               write ( lun2, * ) '   '
               write ( lun2, * ) ' Vertical diffusivity  is zero  '
               write ( lun2, * ) '   '
               ldiffz = .false.
            endif
         case ( 2 )
            if ( gettoken( dminim , ierr2 ) .ne. 0 ) goto 4008
            if ( alpha .gt. 0.0 .and. dminim .ge. 0.0 ) then
               write ( lun2, * ) '   '
               write ( lun2, * ) ' Vertical diffusion from the hydrodynamic model'
               write ( lun2, * ) ' Scale factor for the vertical diffusivity : ', alpha
               write ( lun2, * ) ' Value added to scaled vertical diffusivity: ', cdisp
               write ( lun2, * ) ' Minimum value to this vertical diffusivity: ', dminim
               write ( lun2, * ) '   '
               ldiffz = .true.
            else
               write ( lun2, * ) '   '
               write ( lun2, * ) ' Vertical diffusivity  is zero  '
               write ( lun2, * ) '   '
               ldiffz = .false.
            endif
         case default
            write ( lun2, * ) ' Error: this vert.diff. option is not valid'
            call stop_exit(1)
      end select
      write ( lun2, * ) '   '

!     layer thickness 3d models

      if ( nolayp .gt. 1 .and. modtyp .ne. model_two_layer_temp ) then
         write ( lun2, 3115 )
         write ( lun2, 3120 ) ( i, tcktot(i), i = 1, nolayp )
      else
         tcktot(1) = 1.0
      endif

!     read substance names

      if ( gettoken( nosubs , ierr2 ) .ne. 0 ) goto 4009
      nfract = 0
      if ( oil ) then
         if ( gettoken( nfract , ierr2 ) .ne. 0 ) goto 4009
      endif
      write ( lun2, '(//a)' ) '  Modelled substances : '
      nosubc = nosubs
      if ( modtyp .eq. model_two_layer_temp ) then
         nolayp = 2
         nosubc = nolayp*nosubs
      endif
      nosub_max = max(nosubc+4,1)
      call alloc ( "substi", substi, nosub_max )
      call alloc ( "mapsub", mapsub, nosub_max )
      do i = 1, nosubs
         if ( gettoken( substi(i), ierr2 ) .ne. 0 ) goto 4009
         write ( lun2, '(12x,a20)' ) substi(i)
      enddo

!     modtyp = 1 (model_tracers) or 2 (model_two_layer_temp) refers to 2d hydrodynamics

      mapsub = 0
      select case ( modtyp )
         case ( 1 )
            write ( *, * ) ' You are using the tracer model '
            pblay  = 0.0
         case ( 2 )
            write ( *, * ) ' You are using the two-layer temperature model'
            mapsub(1) = 1            !.. substance number 1 must be temperature
            mapsub(2) = 2            !.. substance 2 is temperature for bottom layer
         case ( 3 )
            write(lun2,*) ' You are trying to use the red tide model, but the '
            write(lun2,*) ' red tide model is obsolete. Please contact Deltares. '
            call stop_exit(1)
         case ( 4 )
            write(*,'(//)')
            write(*,*) ' You are using the oil model '
            write(*,'(//)')
            write(*,*) ' Number of oil fractions     : ', nfract
            if ( nosubs .lt. 3 ) then
               write ( lun2, * ) ' For oil module at least 3 substances '
               write ( lun2, * ) '(floating, dispersed and sticking oil)'
               ierr = ierr + 1
            endif
            if ( nfract*3 .gt. nosubs ) then
               write ( lun2, * ) ' For oil module at least 3 subst per fraction'
               ierr = ierr + 1
            endif
            do i = 1, nfract*3
               mapsub(i) = i
            enddo
         case ( 5 )
            pblay  = 0.0
            if ( nolayp .eq. 1 ) then
               write ( *, * ) ' You are using the 2d temperature model '
            else
               write ( *, * ) ' You are using the 3d temperature model '
            endif
            mapsub(1) = 1            !.. substance number 1 must be temperature
         case ( 6 )
            write ( *, * ) ' You are using the probabilistic density driven settling model '
         case ( 7 )
            write ( *, * ) ' You are using the general Agent Based Model (ABM)'
            pblay  = 0.0
         case default
            write(lun2 , 2015) modtyp
            nolayp = 1
            ierr = ierr + 1
      end select

!  Detect particle tracks and detect sticking substances

      itrack = 0
      ntrack = 0
      call alloc ( "nplot", nplot, max(notrak,1) )
      nplot  = 0

!  Sticking substances are those substances that stick to land, most often this is oil
!  If substance (i) can stick then mstick(i) > 0 the number gives the source for this
!  sticking substance.
!  I.e. if mstick(6) = 1, then substance 1 is inactive and substance 6 is the origin
!  (eg. floating oil) and is active
!  note: inactive (or stikcing) substances must have mstick(i) < 0

      call alloc ( "mstick", mstick, nosub_max )
      mstick = 0
      nstick = 0
      do isb = 1, nosubs                 !.. substances that are sticking
         if ( isb .gt. (3*nfract) .and. substi(isb)(1:6) .eq. "stick_" ) then
            do jsub = 1, nosubs           !.. substances that are sticking and that are not oil
               if ( substi(jsub)(1:14) .eq. substi(isb)(7:20) ) then
                  mstick(jsub) = isb
                  nstick = nstick + 1
                  mstick(isb ) = -nstick     !.. sticking one itself must ne negative!
                  exit
               endif
               if ( jsub .eq. nosubs ) then
                  write ( lun2, * ) ' Error: sticking substance has no source ', substi(is)
                  call stop_exit(1)
               endif
            enddo
         endif
      enddo

!.. insert substances that are sticking and are oil, each oil fraction is 3 substances

      do ifract = 1, nfract
         isb = (ifract-1)*3 + 1         !.. floating oil (first substance)
         mstick( isb )   = 3*ifract     !   sticks       (third substance)
         isb = (ifract-1)*3 + 2         !.. dispersed oil (second substance)
         mstick( isb )   = 3*ifract     !   sticks       (third substance)
         nstick = nstick + 1
         mstick(3*ifract) = -nstick      !.. sticking substance itself
      enddo
      write ( lun2, 3132 ) nstick
      do isb = 1, nosubs
         write ( lun2, 3133 ) isb, mstick(isb)
      enddo

!.. some not so trivial duplications of substance name arrays
      call alloc ( "subst ", subst , nosub_max*noslay )
      call alloc ( "subst2", subst2, nosub_max )
      i = 0
      do ilay = 1, nolayp
         do isb = 1, nosubs + 1
            i = i + 1
            if (isb .le. nosubs) then
               subst(i)    = substi(isb)
            else
               subst(i)    = 'nr of particles'
            end if
            select case ( nolayp )
               case (   1: 99 )
                  ln = min(len_trim(subst(i))+1,18)
                  write ( subst(i)(ln:),'(a,i2.2)') '_',ilay
               case ( 100:999 )
                  ln = min(len_trim(subst(i))+1,17)
                  write ( subst(i)(ln:),'(a,i3.3)') '_',ilay
            end select
         enddo
      enddo
      substi(nosubs+2) = 'nr of particles'
      substi(nosubs+1) = 'localdepth'                 !     for 3d datasets (required by gpp)
                                                      !     this overwrites something if nolayp > 1
      do ilay = 1, nolayp                             !     tsja
         do isb = 1, nosubs+1
            if ( modtyp .eq. model_two_layer_temp ) then
               i = (isb-1)*nolayp + ilay
               subst2(i   ) = substi(isb)
            elseif ( ilay .eq. 1 ) then
               subst2(isb ) = substi(isb)
            endif
         enddo
      enddo

      select case ( modtyp )
         case ( 1 )
            write ( lun2, 2001) 'Tracer model'
         case ( 2 )
            write ( lun2, 2001) 'Temperature model: stratified 2-layer model'
            do i = 1, nosubs*nolayp
               if ( i .le. nolayp ) then
                  write ( lun2, 2010 ) 'Substance - upper layer:', subst(i)(1:10), subst(i)(11:20)
               else
                  write ( lun2, 2010 ) 'Substance - lower layer:', subst(i)(1:10), subst(i)(11:20)
               endif
            enddo
            write ( lun2, 2011 ) pblay
         case ( 4 )
            write ( lun2, 2002 ) 'Oil model - dispersion and evaporation included'
         case ( 7 )
            write ( lun2, 2001) 'General Agent Based Model (ABM)'
      end select

      subst2( nosubs+2 ) = 'nr of particles'
      subst2( nosubs+1 ) = 'localdepth'         !     added for 3d datasets

!     read the model parameters
!     number of particles
!     note : - npmax must be set to the requested number + 0.5% of that
!              number because due to round-off errors while adding new
!              particles, less then 0.5% of the total number of particles
!              are added surplus; so check with -0.5%

      if ( gettoken( nopart, ierr2 ) .ne. 0 ) goto 4013         !     number of particles
      write ( lun2, 2020 ) nopart
      write ( *   , 2020 ) nopart

      if ( gettoken( rough, ierr2 ) .ne. 0 ) goto 4014          !     roughness
      write ( lun2, 2040 ) rough

      if ( gettoken( drand(1), ierr2 ) .ne. 0 ) goto 4015       !     random walk
      if ( gettoken( drand(2), ierr2 ) .ne. 0 ) goto 4015
      write ( lun2, 2050 ) drand(1), drand(2)
      drand(2) = drand(2)/2.0
      if ( drand(1) .gt. 0.0 ) then
         ldiffh= .true.
         drand(1) = 2.0*sqrt(idelt*drand(1))
      else
         ldiffh= .false.
         drand(1) = 0.0
      endif

      if ( gettoken( drand(3), ierr2 ) .ne. 0 ) goto 4016       !     wind fetch [%]
      write ( lun2, 2060 ) drand(3)

      if ( gettoken( rhow, ierr2 ) .ne. 0 ) goto 40161          !     water density
      write ( lun2, 2065 ) rhow

!     constant wind velocity and direction
      if ( gettoken( cbuffer, nowind, itype, ierr2 ) .ne. 0 ) goto 4017    !     number of wind breakpoints
      if ( itype .eq. 1 ) then                                  ! a character giving a meteo steering file
         write ( lun2, '(//a,a)' ) " Meteo steering file: ", cbuffer(1:len_trim(cbuffer))
         spawnd = .true.
         nowind = 1
         call alloc ( "iwndtm", iwndtm, nowind )
         call alloc ( "wveloa", wveloa, nowind )
         call alloc ( "wdira ", wdira , nowind )
         call rdwnd ( lun2   , cbuffer, nmaxp  , mmaxp  , mnmax2 ,  &
     &                xb     , yb     , lgrid  , ierr2  )
         if ( ierr2 .eq. 0 ) then
            write ( lun2, '(a)' ) " Meteo system succesfully initialized."
         else
            write ( lun2, '(a)' ) " ERROR in the initialisation of the meteo system."
            ierr = ierr+1
         endif
      else
         spawnd = .false.
         nowind = max( nowind, 1 )
         call alloc ( "iwndtm", iwndtm, nowind )
         call alloc ( "wveloa", wveloa, nowind )
         call alloc ( "wdira ", wdira , nowind )
         if ( nowind .lt. 2 ) then
            write(lun2 , 2070) 2, nowind                           !     error, number of wind variations
            ierr = ierr + 1
         endif
         do i = 1, nowind                                          !     wind varies with the time
            if ( gettoken( id  , ierr2 ) .ne. 0 ) goto 4018
            if ( gettoken( ih  , ierr2 ) .ne. 0 ) goto 4018
            if ( gettoken( im  , ierr2 ) .ne. 0 ) goto 4018
            if ( gettoken( is  , ierr2 ) .ne. 0 ) goto 4018
            if ( gettoken( wveloa(i) , ierr2 ) .ne. 0 ) goto 4018
            if ( gettoken( wdira (i) , ierr2 ) .ne. 0 ) goto 4018
            iwndtm(i) = id*86400 + ih*3600 + im*60 + is
         enddo
      endif
      if (fmmodel) then
          call alloc ( "wvelo", wvelo, numcells )
          call alloc ( "wdir" , wdir , numcells )
          write ( lun2, '(//)' )
      else
          call alloc ( "wvelo", wvelo, mnmax2 )
          call alloc ( "wdir" , wdir , mnmax2 )
          write ( lun2, '(//)' )
      endif
!       read number of constants that will follow

      if ( gettoken( noconsp, ierr2 ) .ne. 0 ) goto 4019

      if ( .not. oil ) then
         if ( noconsp .gt. 0 ) call alloc ( "const", const, noconsp )
         do i = 1, noconsp
            if ( gettoken( const(i), ierr2 ) .ne. 0 ) goto 4020
         enddo
      else
         call alloc ( "const", const, noconsp + 2 ) ! extra, minimum thickness oil layer and deflection angle, and two for the Fingas evaporation
         i = 0
         do ifract = 1, nfract
            i = i + 1
            write ( lun2, '(2x,a,a)' ) 'Fraction ', substi( (ifract-1)*3 + 1 )
            if ( gettoken( const(i), ierr2 ) .ne. 0 ) goto 4020      ! evaporation
            if (const(i).lt.-0.5) then
               if (const(i).lt.-1.5) then
                  write(lun2,'(8x,a)')'Effect of water content on evaporation invoked'
               endif
               i = i + 1
               if ( gettoken( const(i), ierr2 ) .ne. 0 ) goto 4020      ! evaporation d180 parameter
               if (const(i)>0) then
                  write ( lun2, '(8x,a,es15.7)' ) '% evaporated at 180 deg. C, ln function          : ', const(i)       !  1
               else if (const(i)<0) then
                  write ( lun2, '(8x,a,es15.7)' ) '% evaporated at 180 deg. C, sq. root function    : ', -const(i)       !  1
               end if
               i = i + 1
               if ( gettoken( const(i), ierr2 ) .ne. 0 ) goto 4020      ! temperature of the evaporation Fingas process
               write ( lun2, '(8x,a,es15.7)' ) 'Temperature in degrees C                         : ', const(i)       !  1
            else
               write ( lun2, '(8x,a,es15.7)' ) 'Evaporation per day                              : ', const(i)       !  1
               write ( lun2, '(8x,a,es15.7)' ) 'Evaporation per time step                        : ', const(i)*idelt/86400.0
            endif
            i = i + 1
            if ( gettoken( const(i), ierr2 ) .ne. 0 ) goto 4020      ! dispersion option
            if ( const(i) .eq. 0 ) then
               i = i + 1
               if ( gettoken( const(i), ierr2 ) .ne. 0 ) goto 4020   ! dispersion rate/day
               write ( lun2, '(8x,a,es15.7)' ) 'Dispersion rate (per day)                        : ', const(i)    !  3
            elseif (const(i).eq.1) then
               i = i + 1
               const(i) = 0.0                                        ! not used
               write ( lun2, '(8x,a,es15.7)' ) 'Dispersion C0 from Delvigne/Sweeney formula'
            elseif (const(i).eq.2) then
               i = i + 1
               if ( gettoken( const(i), ierr2 ) .ne. 0 ) goto 4020   ! dispersion rate/day
               write ( lun2, '(8x,a,es15.7)' ) 'Minimum wind speed dispersion activation        : ', const(i)    !  3
            endif
            i = i + 1
            if ( gettoken( const(i), ierr2 ) .ne. 0 ) goto 4020
            write ( lun2, '(8x,a,f10.3)'  ) 'Stickyness probability [0-1]                     : ', const(i)       !  4
            i = i + 1
            if ( gettoken( const(i), ierr2 ) .ne. 0 ) goto 4020
            write ( lun2, '(8x,a,f10.3)'  ) 'Volatile fraction                                : ', const(i)       !  5
            i = i + 1
            if ( gettoken( const(i), ierr2 ) .ne. 0 ) goto 4020
            write ( lun2, '(8x,a,e10.3)'  ) 'Emulsification parameter                         : ', const(i)       !  6
            i = i + 1
            if ( gettoken( const(i), ierr2 ) .ne. 0 ) goto 4020
            write ( lun2, '(8x,a,f10.3)'  ) 'Maximum water content                            : ', const(i)       !  7
            i = i + 1
            if ( gettoken( const(i), ierr2 ) .ne. 0 ) goto 4020
            write ( lun2, '(8x,a,f10.3)'  ) 'Fraction evaporated to start emulsification      : ', const(i)       !  8
            i = i + 1
            if ( gettoken( const(i), ierr2 ) .ne. 0 ) goto 4020
            write ( lun2, '(8x,a,es15.7)' ) 'Oil density (kg/m3)                              : ', const(i)       !  9
            i = i + 1
            if ( gettoken( const(i), ierr2 ) .ne. 0 ) goto 4020
            write ( lun2, '(8x,a,es15.7)' ) 'Kinematic viscosity (cSt)                        : ', const(i)       ! 10
         enddo
         write ( lun2, '(2x,a)' ) ' Global oil parameters'
         i = i + 1
         if ( gettoken( const(i), ierr2 ) .ne. 0 ) goto 4020    ! minimum thickness oil layer
         write ( lun2, '(8x,a,e10.3)'  ) 'Minimum thickness oil layer                      : ', const(i)       ! 11
         if ( nolayp .gt. 1) then
            i = i + 1
            if ( gettoken( const(i), ierr2 ) .ne. 0 ) goto 4020 ! deflection angle
            write ( lun2, '(8x,a,f10.3)'  ) 'Deflection angle (Coriolis - 3D only)            : ', const(i)       ! 12
         endif
         stickdf = 0
         if ( filvers(2:19) .ge. '3.73.00' ) then
            if ( gettoken( stickdf, ierr2 ) .ne. 0 ) goto 4020       ! sticking at drying flats
         endif
         if ( stickdf .eq. 0 ) then
            write ( lun2, '(8x,a)' ) 'Oil is not sticking at drying flats'
         else
            write ( lun2, '(8x,a)' ) 'Oil is sticking at drying flats'
         endif
      endif

! read special features
! add defaults when special features are not used first
      vertical_bounce = .true.
      apply_wind_drag = .false.
      max_wind_drag_depth = 0.0
      write_restart_file = .false.
      max_restart_age = -1
      pldebug = .false.
      screens = .false.
      nrowsscreens = 0
      partinifile = ' '
      partrelfile = ' '
      abmmodel = .false.
      abmmodelname = ""
      abmstagedev = ""
      chronrev = .false.
      selstage = 0.0


      if ( gettoken( cbuffer, id, itype, ierr2 ) .ne. 0 ) then
         if (ierr2 .eq. 2) then
            write ( lun2, '(a)' ) ' ERROR REMARK: It appears that optional special features include file was not found.'
            write ( lun2, '(a)' ) '               If you do not use any special features, this "ERROR" can be be ignored.'
            if ( gettoken( id  , ierr2 ) .ne. 0 ) goto 4021
         else
         end if
      else
         if (itype .eq. 1) then
            do while (itype .eq. 1)
               call str_lower(cbuffer, len(cbuffer))
               select case (trim(cbuffer))
               case ('no_vertical_bounce')
                  write ( lun2, '(/a)' ) '  Found keyword "no_vertical_bounce": vertical bouncing is switched off.'
                  vertical_bounce = .false.
               case ('max_wind_drag_depth')
                  write ( lun2, '(/a)' ) ' Found keyword "max_wind_drag_depth".'
                  apply_wind_drag = .true.
                  if (gettoken (max_wind_drag_depth, ierr2) .ne. 0 ) goto 9005
                  if (max_wind_drag_depth.lt.0.0) goto 9005
                  write ( lun2, '(/a,f13.4)' ) ' Maximum depth for particles in top layer to be subject to wind drag: ', max_wind_drag_depth
               case ('write_restart_file')
                  write ( lun2, '(/a)' ) '  Found keyword "write_restart_file".'
                  write ( lun2, '(/a,a)' ) '  At the end of a simulation, delpar will write ', &
                                         'a file containing data for all active particles.'
                  write_restart_file = .true.
               case ('max_restart_age')
                  write ( lun2, '(/a)' ) ' Found keyword "max_restart_age".'
                  if (gettoken (max_restart_age, ierr2) .ne. 0 ) goto 9010
                  if (max_restart_age.eq.0) goto 9011
                  write ( lun2, '(/a,i10)' ) ' Maximum age for particles writen into restart file in seconds: ', max_restart_age
               case ('plastics_parameters')
                  if (modtyp /= model_prob_dens_settling) goto 9101
                  write ( *   , 3500 )
                  write ( lun2, 3500 )
                  call alloc ( "plparset", plparset, nosubs )
                  plparset = 0
                  call alloc ( "plparset", pldensity, nosubs )
                  pldensity = 0.0
                  call alloc ( "plparset", plshapefactor, nosubs )
                  plshapefactor = 0.0
                  call alloc ( "plparset", plmeansize, nosubs )
                  plmeansize = 0.0
                  call alloc ( "plparset", plvarsize, nosubs )
                  plvarsize = 0.0
                  call alloc ( "plparset", plmusize, nosubs )
                  plmusize = 0.0
                  call alloc ( "plparset", plsigmasize, nosubs )
                  plsigmasize = 0.0
                  call alloc ( "plparset", plfragrate, nosubs )
                  plfragrate = 0.0
! read the following parameters per plastic substance:
!                 name   density   shapefactor   meansize   varsize   degradationrate
                  if (gettoken( cplastic, ierr2 ) .ne. 0) goto 9103
                  do while (cplastic .ne. 'end')
                     write ( lun2, 3501 ) trim(cplastic)
                     if (gettoken(rdpldensity, ierr2 ) .ne. 0) goto 9104
                     write ( lun2, 3502 ) rdpldensity
                     if (gettoken(rdplshapefactor, ierr2 ) .ne. 0) goto 9104
                     write ( lun2, 3503 ) rdplshapefactor
                     if (gettoken(rdplmeansize, ierr2 ) .ne. 0) goto 9104
                     write ( lun2, 3504 ) rdplmeansize
                     if (gettoken(rdplvarsize, ierr2 ) .ne. 0) goto 9104
                     write ( lun2, 3505 ) rdplvarsize
                     if (gettoken(rdplfragrate, ierr2 ) .ne. 0) goto 9104
                     write ( lun2, 3506 ) rdplfragrate
                     if (rdplmeansize .le. 0.0) goto 9105
                     rdplmusize = log((rdplmeansize**2)/sqrt(rdplvarsize+rdplmeansize**2))
                     rdplsigmasize = sqrt(log(rdplvarsize/(rdplmeansize**2)+1))
                     isb = -1
                     do i = 1, nosubs
                        if(cplastic .eq. substi(i)) then
                           isb = i
                           exit
                        endif
                     enddo
                     if(isb .gt. 0) then
                        if (plparset(isb) .eq. 1) goto 9106
                        write ( lun2, 3507) trim(cplastic)
                        plparset(isb) = 1
                        pldensity(isb) = rdpldensity
                        plshapefactor(isb) = rdplshapefactor
                        plmeansize(isb) = rdplmeansize
                        plvarsize(isb) = rdplvarsize
                        plmusize(isb) = rdplmusize
                        plsigmasize(isb) = rdplsigmasize
                        plfragrate(isb) = rdplfragrate
                     else
                        write ( lun2, 3508) trim(cplastic)
                     endif
                     if (gettoken( cplastic, ierr2 ) .ne. 0) goto 9103
                  end do
                  plmissing = 0
                  do isb = 1, nosubs
                     if (plparset(isb) .eq. 0) then
                        write (lun2, 3509) trim(substi(isb))
                        write (*   , 3509) trim(substi(isb))
                        plmissing = plmissing + 1
                     end if
                  end do
                  if (plmissing .gt. 0) goto 9107
                  write (lun2, 3510)
               case ('pldebug')
                  write ( lun2, '(/a)' ) '  Found keyword "pldebug": will write plastics debug info (e.g. sizes).'
                  pldebug = .true.
               case ('screens')
                  write ( lun2, '(/a)' ) '  Found keyword "screens".'
                  screens = .true.
                  if ( gettoken( permealeft  , ierr2 ) .ne. 0 ) goto 9201   ! leftside permeability of screeens
                  if ( gettoken( permearight , ierr2 ) .ne. 0 ) goto 9202   ! rightside permeability of screeens
                  if ( gettoken( fiscreens   , ierr2 ) .ne. 0 ) goto 9203   ! screens polygon

                  open ( newunit=lunfil, file=fiscreens, status='old', iostat=ierr2 )
                  if ( ierr2 .ne. 0 ) go to 9204
                  call getdim_dis ( lunfil, fiscreens, nrowsscreens, lun2 )
                  close (lunfil)
                  if ( nrowsscreens .gt. 0) then
!     allocate memory for the dispersant polygons, and read them into memory
                     call alloc ( "xpoltmp", xpolscreens, nrowsscreens )
                     call alloc ( "ypoltmp", ypolscreens, nrowsscreens )
                     xpolscreens = 999.999
                     ypolscreens = 999.999
                     call polpart(fiscreens, nrowsscreens, xpolscreens, ypolscreens, nrowstmp, lun2)
                  else
                     write ( lun2, '(/a)' ) '  Screens polygon doesn''t contain any coordinates'
                     screens = .false.
                  endif
               case ('partinifile')
                  write ( lun2, '(/a)' ) '  Found keyword "partinifile".'
                  if ( gettoken( partinifile  , ierr2 ) .ne. 0 ) goto 9301   ! part FM ini file
               case ('partrelfile')
                  write ( lun2, '(/a)' ) '  Found keyword "partrelfile".'
                  if ( gettoken( partrelfile  , ierr2 ) .ne. 0 ) goto 9302   ! part FM ini file
               case ('abmmodel')
                  if (modtyp /= model_abm) goto 9401
                  write ( lun2, '(/a)' ) '  Found keyword "abmmodel".'
                  abmmodel = .true.
                  if ( gettoken( abmmodelname     , ierr2 ) .ne. 0 ) goto 9402   ! ABM model name
                  write ( lun2, '(/a)' ) '  Found ABM model name : ', abmmodelname
                  select case (trim(abmmodelname)) ! Set ABM model
                    case ("test")
                        abmmt = 0        ! model type none
                    case ("european_eel")
                        abmmt = 1        ! model type European eel
                    case ("atlantic_salmon")
                        abmmt = 2        ! model type Atlantic salmon
                    case ("mauve_stinger")
                        abmmt = 3        ! model type Mauve stinger
                    case ("horseshoecrab")
                        abmmt = 4        ! model type Horseshoe crab
                    case ("mangrove_seeds")
                        abmmt = 5        ! model type Mangrove propagules
                    case ("asian_carp_eggs")
                        abmmt = 6        ! model type Asian Carp Eggs
                    case default         !ABM model not filled
                        write(lun2, '(/a)') ' Unrecognised ABM model name : ', trim(abmmodelname)
                        goto 9403
                  end select
                  if ( gettoken( abmstagedev , ierr2 ) .ne. 0 ) goto 9404   ! ABM stage development
                    write ( lun2, '(/a)' ) ' Found ABM stage development name : ', abmstagedev
                  ! Set stage development
                  select case (trim(abmstagedev))
                    case ("dev_fixed")
                        abmsd = 0 !dev_fixed
                    case ("dev_linear")
                        abmsd = 1 !dev_linear
                    case ("dev_interpolate")
                        abmsd = 2 !dev_intpltd
                    case ("dev_asian_carp_eggs")
                        abmsd = 3 !dev_asian_carp_eggs
                    case default        !ABM model not filled
                        write(lun2, '(/a)') ' Unrecognised ABM model stage development : ', trim(abmstagedev)
                        goto 9405
                   end select
               case ('revchron')
                  if (modtyp /= model_abm) goto 9406
                  write ( lun2, '(/a)' ) '  Found keyword "revchron".'
                  chronrev = .true.
                  if ( gettoken( selstage     , ierr2 ) .ne. 0 ) goto 9407   ! Give stage for release
                  write ( lun2, '(/a,f10.3)' ) '  Found stage for reversed release : ', selstage
               case default
                  write ( lun2, '(/a,a)' ) '  Unrecognised keyword: ', trim(cbuffer)
                  goto 9000
               end select
               if ( gettoken( cbuffer, id, itype, ierr2 ) .ne. 0 ) goto 4021
            end do
         end if
      end if

!     read the simulation timers
!     simulation start time (the value of id is read above in the special features section)

      if ( gettoken( ih  , ierr2 ) .ne. 0 ) goto 4021
      if ( gettoken( im  , ierr2 ) .ne. 0 ) goto 4021
      if ( gettoken( is  , ierr2 ) .ne. 0 ) goto 4021
      write ( lun2, 2090 ) id, ih, im, is
      itstrtp = id * 86400 + ih * 3600 + im * 60 + is

!     simulation stop  time

      if ( gettoken( id  , ierr2 ) .ne. 0 ) goto 4022
      if ( gettoken( ih  , ierr2 ) .ne. 0 ) goto 4022
      if ( gettoken( im  , ierr2 ) .ne. 0 ) goto 4022
      if ( gettoken( is  , ierr2 ) .ne. 0 ) goto 4022
      write ( lun2, 2110 ) id,ih,im,is
      itstopp = id * 86400 + ih * 3600 + im * 60 + is
      if (mod(itstopp - itstrtp, idelt) .ne.  0 ) then
        write ( lun2, 2120 ) idelt
        ierr = ierr + 1
      endif

!     delwaq take over time for particles.

      if ( gettoken( id  , ierr2 ) .ne. 0 ) goto 4023
      if ( gettoken( ih  , ierr2 ) .ne. 0 ) goto 4023
      if ( gettoken( im  , ierr2 ) .ne. 0 ) goto 4023
      if ( gettoken( is  , ierr2 ) .ne. 0 ) goto 4023
      iddtim = id*86400 + ih*3600 + im*60 + is
      iddtim = max (0 , iddtim)
      if ( .not. alone ) then
         write ( lun2, 2121 ) iddtim
      else
         write ( lun2, 2122 )
         iddtim = 0
      endif

!     check wind-variations

      if ( spawnd ) then
         write ( lun2, * ) " space-time varying wind through meteomodule"
      else
         write ( lun2, 2071 )
         do i = 1, nowind
           id =     iwndtm(i)         /86400
           ih = mod(iwndtm(i), 86400) / 3600
           im = mod(iwndtm(i),  3600) / 60
           is = mod(iwndtm(i),    60)
           write ( lun2, 2074 ) id, ih, im, is, wveloa(i), wdira(i)
           if ( i .eq. 1 ) then
              if ( iwndtm(i) .gt. itstrtp ) then
                 ierr = ierr + 1
                 write(lun2 , 2072)
              endif
           else
              if ( iwndtm(i) .le. iwndtm(i-1) ) then
                 ierr = ierr + 1
                 write(lun2 , 2073)
              endif
           endif
         enddo
         if ( iwndtm(nowind) .lt. itstopp ) then
            ierr = ierr + 1
            write(lun2 , 2075)
         endif
      endif

!     map file start time

      if ( gettoken( id  , ierr2 ) .ne. 0 ) goto 4024
      if ( gettoken( ih  , ierr2 ) .ne. 0 ) goto 4024
      if ( gettoken( im  , ierr2 ) .ne. 0 ) goto 4024
      if ( gettoken( is  , ierr2 ) .ne. 0 ) goto 4024
      write ( lun2, 2130 ) id, ih, im, is
      icwsta = id * 86400 + ih * 3600 + im * 60 + is
      if ( icwsta .lt. itstrtp ) then
        write(lun2 , 2140)
        ierr = ierr + 1
      endif

!     map file stop  time

      if ( gettoken( id  , ierr2 ) .ne. 0 ) goto 4025
      if ( gettoken( ih  , ierr2 ) .ne. 0 ) goto 4025
      if ( gettoken( im  , ierr2 ) .ne. 0 ) goto 4025
      if ( gettoken( is  , ierr2 ) .ne. 0 ) goto 4025
      write ( lun2, 2150 ) id, ih, im, is
      icwsto = id*86400 + ih*3600 + im*60 + is
      if ( icwsto .lt. icwsta ) then
        write ( lun2, 2160 )
        ierr = ierr + 1
      endif

!     map file step  time

      if ( gettoken( id  , ierr2 ) .ne. 0 ) goto 4026
      if ( gettoken( ih  , ierr2 ) .ne. 0 ) goto 4026
      if ( gettoken( im  , ierr2 ) .ne. 0 ) goto 4026
      if ( gettoken( is  , ierr2 ) .ne. 0 ) goto 4026
      write ( lun2, 2170 ) id, ih, im, is
      icwste = id*86400 + ih*3600 + im*60 + is
      if ( icwste .le.  0 ) then
         write(lun2, *) ' Error: time step mapfile must be positive '
         ierr = ierr + 1
      endif

!.. changes for version 3.00 : time histories like timings map file in input

      if ( gettoken( id  , ierr2 ) .ne. 0 ) goto 4027
      if ( gettoken( ih  , ierr2 ) .ne. 0 ) goto 4027
      if ( gettoken( im  , ierr2 ) .ne. 0 ) goto 4027
      if ( gettoken( is  , ierr2 ) .ne. 0 ) goto 4027
      write ( lun2, 2131 ) id, ih, im, is
      ihstrtp = id * 86400 + ih * 3600 + im * 60 + is
      if ( ihstrtp .lt. itstrtp ) then
         write(lun2 , 2166)
         ierr = ierr + 1
      endif

!     time-history file stop  time

      if ( gettoken( id  , ierr2 ) .ne. 0 ) goto 4028
      if ( gettoken( ih  , ierr2 ) .ne. 0 ) goto 4028
      if ( gettoken( im  , ierr2 ) .ne. 0 ) goto 4028
      if ( gettoken( is  , ierr2 ) .ne. 0 ) goto 4028
      write ( lun2, 2151 ) id, ih, im, is
      ihstopp = id*86400 + ih*3600 + im*60 + is
      if ( ihstopp .lt. ihstrtp ) then
         write( lun2, 2167 )
         ierr = ierr + 1
      endif

!     step time on time-history file

      if ( gettoken( id  , ierr2 ) .ne. 0 ) goto 4029
      if ( gettoken( ih  , ierr2 ) .ne. 0 ) goto 4029
      if ( gettoken( im  , ierr2 ) .ne. 0 ) goto 4029
      if ( gettoken( is  , ierr2 ) .ne. 0 ) goto 4029
      write ( lun2, 2171 ) id, ih, im, is
      ihstepp = id*86400 + ih*3600 + im*60 + is
      if ( ihstepp  .le.  0 ) then
         write(lun2, *) ' Error: time step hisfile must be positive '
         ierr = ierr + 1
      endif

!     offset to real timings

      if ( gettoken( iyear , ierr2 ) .ne. 0 ) goto 4030
      if ( gettoken( imonth, ierr2 ) .ne. 0 ) goto 4030
      if ( gettoken( id  , ierr2 ) .ne. 0 ) goto 4030
      if ( gettoken( ih  , ierr2 ) .ne. 0 ) goto 4030
      if ( gettoken( im  , ierr2 ) .ne. 0 ) goto 4030
      if ( gettoken( is  , ierr2 ) .ne. 0 ) goto 4030
      write ( lun2, 2180 ) iyear, imonth, id, ih, im, is
      iofset = id*86400 + ih*3600 + im*60 + is

!     write t0-string to title string

      write( title(4), '(a4,i4.4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a)' ) &
            'T0: ', iyear, '.', imonth,  '.', id,    ' ', ih, ':', im, ':', is, '  (scu=       1s)'

      if (oil) then

!     special options for oil
!
!     initial condition for oil (from v3.66)
!     oil_opt = xx0 : cold start (no particles ar t=0)
!             = xx1 : warm start oil patches to be read from  ini-file
!
!     dispersant events (from v3.76)
!     oil_opt = x0x : no dispersant events
!             = x1x : dispersant events with with direct chance to disperse per oil type
!
!     boom events (from v3.76)
!     oil_opt = 0xx : no boom introductions
!             = 1xx : boom introductions with direct chance per day to pass the oilboom
!
         ndisapp = 0
         nboomint = 0
         if ( gettoken( oil_opt, ierr2 ) .ne. 0 ) goto 6001
         ini_opt = mod(oil_opt,10)
         tydisp = mod(int(oil_opt/10), 10)
         tyboom = mod(int(oil_opt/100), 10)

         if (ini_opt .eq. 1 ) then
            if ( gettoken( ini_file, ierr2 ) .ne. 0 ) goto 6012
            open ( newunit=lunfil, file=ini_file, status='old', iostat=ierr2 )
            if ( ierr2 .ne. 0 ) go to 1710

!           get maximum no. of initial particles (npmax) and
!           maximum no. of rows for polygones (nrowsmax)
            write ( lun2, * ) ' Reading number of initial particles from polygon file:', trim(ini_file)

            call getdim_ini ( lunfil, ini_file, npmax, npolmax, nrowsmax, lun2 )
            close ( lunfil )
         endif
!        ini_opt = 2 : ascii text file from rasterdata
         if (ini_opt .eq. 2 ) then
            if ( gettoken( ini_file, ierr2 ) .ne. 0 ) goto 6012
            open ( newunit=lunfil, file=ini_file, status='old', iostat=ierr2 )
            if ( ierr2 .ne. 0 ) go to 1710

!           get maximum no. of initial particles (npmax) and
!           maximum no. of rows for coordinates (nrowsmax)
            write ( lun2, * ) ' Reading number of initial particles from ascii file:', trim(ini_file)
            call getdim_asc ( lunfil, ini_file, npmax, nrowsmax, lun2 )
            close ( lunfil )
         endif


!     optional definition of dispersant releases and boom introductions (from v3.76)

         if ( tydisp .gt. 0 ) then
            if (tydisp .ne. 1) goto 6005
!     read number of dispersant events
            if ( gettoken( ndisapp, ierr2 ) .ne. 0 ) goto 6002
            write( *   , 3300 ) ndisapp
            write( lun2, 3300 ) ndisapp
            if ( ndisapp .gt. 0 ) then
!     allocate dispersant data arrays
               call alloc ( "idisset", idisset, ndisapp )
               call alloc ( "efdisp", efdisp, ndisapp, nfract )
               call alloc ( "fidisp", fidisp, ndisapp )
               do i = 1 , ndisapp
!     read dispersant events timings
                  if ( gettoken( id, ierr2 ) .ne. 0 ) goto 6003
                  if ( gettoken( ih, ierr2 ) .ne. 0 ) goto 6003
                  if ( gettoken( im, ierr2 ) .ne. 0 ) goto 6003
                  if ( gettoken( is, ierr2 ) .ne. 0 ) goto 6003
!     read dispersant parameterisation
                  do ifract = 1, nfract
                     if ( gettoken( efdisp(i, ifract), ierr2 ) .ne. 0 ) goto 6006
                     if (tydisp .eq. 1) then
                        if (efdisp(i, ifract) .lt. 0 .or. efdisp(i, ifract) .gt. 1.0) goto 6007
                     end if
                  end do
!     read dispersant polygon file
                  if ( gettoken( fidisp(i), ierr2 ) .ne. 0 ) goto 6010
!     determine maximum no. of rows for polygons (nrowsmax)
                  open ( newunit=lunfil, file=fidisp(i), status='old', iostat=ierr2 )
                  if ( ierr2 .ne. 0 ) go to 1700
                  call getdim_dis ( lunfil, fidisp(i), nrowsmax, lun2 )
                  close (lunfil)

!     check for ascending order of events, and one per timestep
                  idisset (i) = id*86400 + ih*3600 + im*60 + is
                  if ( idisset(i) .lt. itstrtp .or. idisset(i) .gt. itstopp ) then
                     write ( *   , 3320 )
                     write ( lun2, 3320 )
                     ierr = ierr + 1
                  endif
                  if (mod(idisset(i) ,idelt) .ne. 0) then
                     write ( *   , 3321 )
                     write ( lun2, 3321 )
                     ierr = ierr + 1
                  endif
                  if ( i .gt. 1 ) then
                     if ( idisset(i) .le. idisset(i-1)) then
                        write ( *   , 3322 )
                        write ( lun2, 3322 )
                        ierr = ierr + 1
                     endif
                  endif
                  do ifract = 1, nfract
                     write ( lun2, 3310 ) id, ih, im, is, tydisp, substi((ifract-1)*3+1), &
                                          efdisp(i, ifract), trim(fidisp(i))
                  end do
               enddo
            end if
         end if

         if (tyboom .gt. 0 ) then
            if (tyboom .ne. 1) goto 6025
!     read number of boom introductions
            if ( gettoken( nboomint, ierr2 ) .ne. 0 ) goto 6022
            write( *   , 3301 ) nboomint
            write( lun2, 3301 ) nboomint
            if ( nboomint .gt. 0 ) then
!     allocate boom data arrays
               call alloc ( "iboomset", iboomset, nboomint )
               call alloc ( "efboom", efboom, nboomint, nfract )
               call alloc ( "fiboom", fiboom, nboomint )
               do i = 1 , nboomint
!     read boom introduction timings
                  if ( gettoken( id, ierr2 ) .ne. 0 ) goto 6023
                  if ( gettoken( ih, ierr2 ) .ne. 0 ) goto 6023
                  if ( gettoken( im, ierr2 ) .ne. 0 ) goto 6023
                  if ( gettoken( is, ierr2 ) .ne. 0 ) goto 6023
                  do ifract = 1, nfract
                     if ( gettoken( efboom(i, ifract), ierr2 ) .ne. 0 ) goto 6026
                     if (tyboom .eq. 1) then
                        if (efboom(i, ifract) .lt. 0 .or. efboom(i, ifract) .gt. 1.0) goto 6027
                     end if
                  end do
                  if ( gettoken( fiboom(i), ierr2 ) .ne. 0 ) goto 6010
!     determine maximum no. of rows for polygones (nrowsmax)
                  open ( newunit=lunfil, file=fiboom(i), status='old', iostat=ierr2 )
                  if ( ierr2 .ne. 0 ) go to 1701
                  call getdim_dis ( lunfil, fiboom(i), nrowsmax, lun2 )
                  close (lunfil)

!     check for ascending order of events, and one per timestep
                  iboomset (i) = id*86400 + ih*3600 + im*60 + is
                  if ( iboomset(i) .lt. itstrtp .or. iboomset(i) .gt. itstopp ) then
                     write ( *   , 3320 )
                     write ( lun2, 3320 )
                     ierr = ierr + 1
                  endif
                  if (mod(iboomset(i) ,idelt) .ne. 0) then
                     write ( *   , 3321 )
                     write ( lun2, 3321 )
                     ierr = ierr + 1
                  endif
                  if ( i .gt. 1 ) then
                     if ( iboomset(i) .le. iboomset(i-1)) then
                        write ( *   , 3322 )
                        write ( lun2, 3322 )
                        ierr = ierr + 1
                     endif
                  endif
                  do ifract = 1, nfract
                     write ( lun2, 3310 ) id, ih, im, is, tyboom, ifract, efboom(i, ifract), trim(fiboom(i))
                  end do
               enddo
            endif
         endif

         call alloc ( "xpoltmp", xpoltmp, max( nrowsmax, 1 ) )
         call alloc ( "ypoltmp", ypoltmp, max( nrowsmax, 1 ) )

         if ( ndisapp .gt. 0 .and. nrowsmax .gt. 0) then
!     allocate memory for the dispersant polygons, and read them into memory
            call alloc ( "xpoldis", xpoldis, nrowsmax, ndisapp )
            call alloc ( "ypoldis", ypoldis, nrowsmax, ndisapp )
            call alloc ( "nrowsdis", nrowsdis, ndisapp )
            xpoldis = 999.999
            ypoldis = 999.999

            do i = 1 , ndisapp
               call polpart(fidisp(i), nrowsmax, xpoltmp, ypoltmp, nrowstmp, lun2)
               xpoldis(1:nrowstmp, i) = xpoltmp(1:nrowstmp)
               ypoldis(1:nrowstmp, i) = ypoltmp(1:nrowstmp)
               nrowsdis(i) = nrowstmp
            enddo
         endif

         if ( nboomint .gt. 0 ) then
!     allocate memory for the boom polygons, and read them into memory
            call alloc ( "xpolboom", xpolboom, nrowsmax, nboomint )
            call alloc ( "ypolboom", ypolboom, nrowsmax, nboomint )
            call alloc ( "nrowsboom", nrowsboom, nboomint )
            xpolboom = 999.999
            ypolboom = 999.999

            do i = 1 , nboomint
               call polpart(fiboom(i), nrowsmax, xpoltmp, ypoltmp, nrowstmp, lun2)
               xpolboom(1:nrowstmp, i) = xpoltmp(1:nrowstmp)
               ypolboom(1:nrowstmp, i) = ypoltmp(1:nrowstmp)
               nrowsboom(i) = nrowstmp
            enddo
         endif
      endif

!     optionally read the name of a delpar binary initial file
!     and/or read the number of monitoring stations
!     their names and their (x,y) coordinates

      idp_file = " "
      nopart_res = 0
      if ( gettoken( cbuffer, nosta, itype, ierr2 ) .ne. 0 ) goto 4031
      if ( itype .eq. 1) then
         idp_file = cbuffer
         if ( idp_file .ne. ' ' ) then
            write ( lun2, * ) ' Reading number of initial particles from file:', idp_file(1:len_trim(idp_file))
            call openfl ( lunfil, idp_file, 0 )
!           get maximum no. of initial particles (nrespart), don't combine ini_oil with this!
            read ( lunfil ) idummy, nopart_res, idummy
            close ( lunfil )
            npmax = nopart_res
         endif
         if ( gettoken( nosta, ierr2 ) .ne. 0 ) goto 4031
      endif
      if ( ihstepp .le. 0 .and. nosta .gt. 0 ) then
         write ( lun2, 2024 )
         ierr = ierr+1
      endif
      i = max(nosta,1)
      call alloc ( "nmstat", nmstat, i )
      call alloc ( "xstat ", xstat , i )
      call alloc ( "ystat ", ystat , i )
      if ( nosta  >  0 ) write ( lun2, 2026 )
      do i = 1, nosta
         if ( gettoken( nmstat(i), ierr2 ) .ne. 0 ) goto 4032
         if ( gettoken( xstat (i), ierr2 ) .ne. 0 ) goto 4032
         if ( gettoken( ystat (i), ierr2 ) .ne. 0 ) goto 4032
         write ( lun2, 2027 ) nmstat(i),xstat(i),ystat(i)
      enddo

!     read the plot characteristics

      if ( gettoken( iptset, ierr2 ) .ne. 0 ) goto 4033
      if ( iptset .gt. 0 ) then
         call alloc ( "ipset ", ipset , iptset )
         call alloc ( "recovr", recovr, iptset )
         write( lun2, 2190 )
      endif
      do i = 1 , iptset
         if ( gettoken( id, ierr2 ) .ne. 0 ) goto 4034
         if ( gettoken( ih, ierr2 ) .ne. 0 ) goto 4034
         if ( gettoken( im, ierr2 ) .ne. 0 ) goto 4034
         if ( gettoken( is, ierr2 ) .ne. 0 ) goto 4034
         if ( gettoken( recovr(i), ierr2 ) .ne. 0 ) goto 4034
         write ( lun2, 2210 ) id, ih, im, is, recovr(i)
         ipset (i) = id*86400 + ih*3600 + im*60 + is
         if ( ipset(i) .lt. itstrtp .or. ipset(i) .gt. itstopp ) then
            write ( lun2, 2220 )
            ierr = ierr + 1
         endif
         if ( i .gt. 1 ) then
            if ( ipset(i) .le. ipset(i-1) ) then
               write ( lun2, 2222 )
               ierr = ierr + 1
            endif
         endif
      enddo

!     test precise matching of map output

      if ( mod(icwste,idelt) .gt. 0 )  then
         write ( lun2, 2224 )
         ierr = ierr + 1
      endif

!     x-window of the plot

      npgrid = 1
      if ( gettoken( i, xw1f, itype, ierr2 ) .ne. 0 ) goto 4038
      if ( itype .eq. 2 ) then
         npgrid = i
         write ( lun2, '(A,I4)' ) '  Number of plot grids: ',npgrid
      endif
      allocate ( pg(max(1,npgrid)) )
      do i = 1, npgrid
         if ( npgrid .gt. 1 ) write ( lun2, '(/A,i4)' ) 'Information for plotgrid: ', i
         if ( itype .eq. 2 ) then
            if ( gettoken( xw1f, ierr2 ) .ne. 0 ) goto 4038
         endif
         if ( gettoken( xw2f, ierr2 ) .ne. 0 ) goto 4038
         if ( xw1f .gt. xw2f ) then     !       ascending order; swap
            xw1f = xw1f + xw2f
            xw2f = xw1f - xw2f
            xw1f = xw1f - xw2f
         endif
         write ( lun2, 2229 )
         write ( lun2, 2230 ) xw1f , xw2f

!     y-window of the plot

         if ( gettoken( yw1f, ierr2 ) .ne. 0 ) goto 4038
         if ( gettoken( yw2f, ierr2 ) .ne. 0 ) goto 4038
         if ( yw1f .gt. yw2f ) then     !       ascending order; swap
            yw1f = yw1f + yw2f
            yw2f = yw1f - yw2f
            yw1f = yw1f - yw2f
         endif
         write ( lun2, 2240 ) yw1f, yw2f
         pg(i)%xlow  = xw1f
         pg(i)%xhigh = xw2f
         pg(i)%ylow  = yw1f
         pg(i)%yhigh = yw2f

!     resolution of the plot window in the z-direction ?

         if ( gettoken( cbuffer, pg(i)%mmap, itype, ierr2 ) .ne. 0 ) goto 4038
         pg(i)%ztype = 0
         if ( itype .eq. 1 ) then
            if ( gettoken( xw1f, ierr2 ) .ne. 0 ) goto 4038
            if ( gettoken( xw2f, ierr2 ) .ne. 0 ) goto 4038
            if ( xw1f .gt. xw2f ) then     !       ascending order; swap
               xw1f = xw1f + xw2f
               xw2f = xw1f - xw2f
               xw1f = xw1f - xw2f
            endif
            write ( lun2, '(A,A,2f5.2)' ) cbuffer,' : ', xw1f, xw2f
            select case ( cbuffer(1:21) )
               case ( 'Z_values_from_surface' )
                  pg(i)%ztype = 1
               case ( 'Z_values_from_bed    ' )
                  pg(i)%ztype = 2
               case ( 'fraction_from_surface' )
                  pg(i)%ztype = 3
               case default
                  write ( lun2, '(A,A,2f5.2)' ) 'ERROR: token not recognized: ',cbuffer(1:len_trim(cbuffer))
                  call stop_exit(1)
            end select
            if ( gettoken( pg(i)%mmap, ierr2 ) .ne. 0 ) goto 4038
         endif

!     resolution of the plot window

         if ( gettoken( pg(i)%nmap, ierr2 ) .ne. 0 ) goto 4038
         pg(i)%mmap = max( pg(i)%mmap, 3 )
         pg(i)%nmap = max( pg(i)%nmap, 3 )
         write ( lun2, 2241 ) pg(i)%mmap, pg(i)%nmap
      enddo

!     instantaneous release chracteristics
!     number of instantaneous releases

      if ( gettoken( nodye, ierr2 ) .ne. 0 ) goto 4039
      i = max(nodye,1)
      call alloc ( "nmdyer ", nmdyer , i )
      call alloc ( "iwtime ", iwtime , i )
      call alloc ( "kwaste ", kwaste , i )
      call alloc ( "xwaste ", xwaste , i )
      call alloc ( "ywaste ", ywaste , i )
      call alloc ( "zwaste ", zwaste , i )
      call alloc ( "ioptrad", ioptrad, i )
      call alloc ( "radius ", radius , i )
      call alloc ( "fidye  ", fidye  , i )
      call alloc ( "wparm  ", wparm  , i )
      call alloc ( "ndprt  ", ndprt  , i )
      call alloc ( "amassd ", amassd , nosubs, i )
      if ( nodye .gt. 0 ) write ( lun2, 2250 )

      do 10 i = 1 , nodye

!       name of the instantaneous release (version 3.30)

         if ( gettoken( nmdyer(i), ierr2 ) .ne. 0 ) goto 4040
         write ( lun2, 2258 ) nmdyer(i)

!       time of the instantaneous release

         if ( gettoken( id, ierr2 ) .ne. 0 ) goto 4041
         if ( gettoken( ih, ierr2 ) .ne. 0 ) goto 4041
         if ( gettoken( im, ierr2 ) .ne. 0 ) goto 4041
         if ( gettoken( is, ierr2 ) .ne. 0 ) goto 4041
         write ( lun2, 2260 ) id, ih, im, is
         iwtime(i) = id*86400 + ih*3600 + im*60 + is
         if ( iwtime(i) .lt. itstrtp .or. iwtime(i) .gt. itstopp ) then
            write ( lun2, 2270 )
            ierr = ierr + 1
         endif

!       location of the instantaneous release

         if ( .not. oil ) then
            if ( nolayp .eq. 1 ) then
               if ( gettoken( xwaste(i), ierr2 ) .ne. 0 ) goto 4042
               if ( gettoken( ywaste(i), ierr2 ) .ne. 0 ) goto 4042
               if ( gettoken( zwaste(i), ierr2 ) .ne. 0 ) goto 4042
               kwaste(i) = 1
            else
               if ( gettoken( xwaste(i), ierr2 ) .ne. 0 ) goto 4042
               if ( gettoken( ywaste(i), ierr2 ) .ne. 0 ) goto 4042
               if ( gettoken( kwaste(i), ierr2 ) .ne. 0 ) goto 4042
               if (abs(kwaste(i)) .gt. nolayp) goto 4066
               zwaste(i) = 0.0
            endif
         else
            if ( gettoken( xwaste(i), ierr2 ) .ne. 0 ) goto 4042
            if ( gettoken( ywaste(i), ierr2 ) .ne. 0 ) goto 4042
            zwaste(i) = 0.0
            kwaste(i) = 1
         endif

!       option for release radius (and release radius)

         if ( gettoken( ioptrad(i), ierr2 ) .ne. 0 ) goto 4043
         if ( ioptrad(i) .eq. 0 ) then
            ! read all tokens
            if ( gettoken( fidye(i), ibuffer, radius(i), itype, ierr2 ) .ne. 0 ) goto 4043
            if (itype.eq.2) then
               radius(i) = real(ibuffer)
               fidye(i) = ' '
            elseif (itype.eq.3) then
               fidye(i) = ' '
            else
               radius(i) = -999.0
               open ( newunit=lunfil, file=fidye(i), status='old', iostat=ierr2 )
               if ( ierr2 .ne. 0 ) go to 1702
               call getdim_dis ( lunfil, fidye(i), nrowsmax, lun2 )
               close (lunfil)
            endif
         else
            radius(i) = 0
         endif

!       percentage of particles used

         if ( gettoken( wparm(i), ierr2 ) .ne. 0 ) goto 4043
         if ( wparm(i) >= 0.0 ) then
            ndprt(i) = int(wparm(i)*nopart/100.0 + 0.5)
         else
            write ( lun2, 3323 )
            ierr = ierr + 1
         endif
         
         if ( nolayp .eq. 1 ) then
            write ( lun2, 2280 ) xwaste(i), ywaste(i), zwaste(i)
         else
            write ( lun2, 2281 ) xwaste(i), ywaste(i), kwaste(i)
         endif
         if ( radius(i) .ge. 0.0 ) then
            write ( lun2, 2282 ) radius(i), wparm(i)
         else
            write ( lun2, 2283 ) trim(fidye(i)), wparm(i)
         endif

!       mass of the instantaneous release

         write ( lun2, 2289 )
         do k = 1, nosubs
            if ( gettoken( amassd(k,i), ierr2 ) .ne. 0 ) goto 4044
            write( lun2, 2290 ) substi(k), amassd(k,i)
         enddo
   10 continue

!     continuous release chracteristics
!     number of continuous releases

      if ( gettoken( nocont, ierr2 ) .ne. 0 ) goto 4045
      nodac = nodye + nocont
      i = max(nocont,1)
      allocate   ( ascal ( i ) )
      call alloc ( "nmconr", nmconr, i )
      call alloc ( "linear", linear, i )
      call alloc ( "ictmax", ictmax, i )
      call alloc ( "t0cf  ", t0cf  , i )
      call alloc ( "tmassu", tmassu, i )
      call alloc ( "acf   ", acf   , i )
      call alloc ( "ncheck", ncheck, i )
      call alloc ( "rem   ", rem   , i )
      call alloc ( "tmassc", tmassc, i, nosubs )
      call alloc ( "stoch ", stoch , nosubs, i )
      i = max(nodac,1)
      call alloc ( "xwaste", xwaste, i )
      call alloc ( "ywaste", ywaste, i )
      call alloc ( "zwaste", zwaste, i )
      call alloc ( "kwaste", kwaste, i )
      call alloc ( "radius", radius, i )
      call alloc ( "fiwaste",fiwaste, i )
      do k = 1, nodye
         fiwaste(k) = fidye(k)
      enddo
      call alloc ( "wparm ", wparm , i )
      call alloc ( "ndprt ", ndprt , i )
      if ( nocont .gt. 0 ) then
         write(lun2 , 2300) nocont
      else
         allocate   ( ictime( 1, 1 ) )
         allocate   ( ftime ( 1, 1 ) )
         allocate   ( amassc( 1, 1, 1 ) )
      endif

      do 20 i = 1 , nocont

!       name of the continuous release (version 3.30)

         if ( gettoken( nmconr(i), ierr2 ) .ne. 0 ) goto 4046
         write ( lun2, 2259 ) nmconr(i)

!       location of the continuous release

         if ( gettoken( xwaste(i+nodye), ierr2 ) .ne. 0 ) goto 4047
         if ( gettoken( ywaste(i+nodye), ierr2 ) .ne. 0 ) goto 4047
         if ( .not. oil ) then
            if ( nolayp .eq. 1 ) then
               if ( gettoken( zwaste(i+nodye), ierr2 ) .ne. 0 ) goto 4047
               kwaste(i+nodye) = 1
            else
               if ( gettoken( kwaste(i+nodye), ierr2 ) .ne. 0 ) goto 4047
               if (abs(kwaste(i+nodye)) .gt. nolayp) goto 4067
               zwaste(i+nodye) = 0.0
            endif
         else
            zwaste(i+nodye) = 0.0
            kwaste(i+nodye) = 1
         endif

!       radius and scale (% of particles)

         ! read all tokens because it might be the name of a polygon
         if ( gettoken( fiwaste(i+nodye), ibuffer, radius(i+nodye), itype, ierr2 ) .ne. 0 ) goto 4043
         if (itype.eq.2) then
            radius(i+nodye) = real(ibuffer)
            fiwaste(i+nodye) = ' '
         elseif (itype.eq.3) then
            fiwaste(i+nodye) = ' '
         else
            radius(i+nodye) = -999.0
            open ( newunit=lunfil, file=fiwaste(i+nodye), status='old', iostat=ierr2 )
            if ( ierr2 .ne. 0 ) go to 1703
            call getdim_dis ( lunfil, fiwaste(i+nodye), nrowsmax, lun2 )
            close (lunfil)
         endif

         if ( gettoken( wparm (i+nodye), ierr2 ) .ne. 0 ) goto 4043
         if ( wparm(i+nodye) >= 0.0 ) then
            ndprt(i+nodye) = int(wparm(i+nodye)*nopart/100.0 + 0.5)
         else
            write ( lun2, 3323 )
            ierr = ierr + 1
         endif
         if ( nolayp .eq. 1 ) then
            write ( lun2, 2280 ) xwaste(i+nodye), ywaste(i+nodye), zwaste(i+nodye)
         else
            write ( lun2, 2281 ) xwaste(i+nodye), ywaste(i+nodye), kwaste(i+nodye)
         endif
         if ( radius(i+nodye) .ge. 0.0 ) then
            write ( lun2, 2282 ) radius(i+nodye), wparm (i+nodye)
         else
            write ( lun2, 2283 ) trim(fiwaste(i+nodye)), wparm(i+nodye)
         endif

!       scale factors (ascal) for each load

         if ( gettoken( ascal(i), ierr2 ) .ne. 0 ) goto 4049
         write ( lun2, '(/8x,a,f10.2)' ) 'Scale factor : ',ascal(i)

!       interpolation options for continuous releases

         if ( gettoken( linear(i), ierr2 ) .ne. 0 ) goto 5049
         if ( linear(i) .eq. 0 ) then
            write ( lun2, '(/8x,a)' ) 'Block interpolation for load table'
         else
            write ( lun2, '(/8x,a)' ) 'Linear interpolation for load table'
         endif

!       and stochiometry matrix (stoch) for each load

         write ( lun2, 2329 )  nmconr(i)
         do k = 1, nosubs
            if ( gettoken( stoch(k,i), ierr2 ) .ne. 0 ) goto 4050
            write ( lun2, 2290 ) substi(k), stoch(k,i)
         enddo

!       read time points cont's

         if ( gettoken( ictmax(i), ierr2 ) .ne. 0 ) goto 4051
         call alloc ( "ictime", ictime, nocont   , ictmax(i) )
         call alloc ( "ftime ", ftime , nocont   , ictmax(i) )
         call alloc ( "amassc", amassc, nocont   , nosubs, ictmax(i) )
         do k = 1 , ictmax(i)

!         read the breakpoint and rate of mass-release (unity of mass/sec)

            if ( gettoken( id, ierr2 ) .ne. 0 ) goto 4052
            if ( gettoken( ih, ierr2 ) .ne. 0 ) goto 4052
            if ( gettoken( im, ierr2 ) .ne. 0 ) goto 4052
            if ( gettoken( is, ierr2 ) .ne. 0 ) goto 4052
            if ( gettoken( ftime(i,k), ierr2 ) .ne. 0 ) goto 4052
            ictime(i,k) = id*86400 + ih*3600 + im*60 + is
            if ( k .eq. 1 .and. ictime(i,k) .ne. itstrtp ) then
               write ( lun2, 2360 )
               ierr = ierr + 1
            endif
         enddo

!       make up mass by multiplication with scale factors

         do k = 1 , ictmax(i)
            do isb = 1 , nosubs
               amassc( i, isb, k) = ascal(i) * stoch(isb,i) * ftime(i,k)
            enddo
         enddo
         write ( lun2, '(8x,a)'     ) ' Load table (after scaling)'
         write ( lun2, '(/12x,a,a)' ) ' Station ',nmconr(i)
         do isb = 1, nosubs
            write ( lun2, '(15x,10a)' ) substi(isb)
            do k = 1, ictmax(i)
               write ( lun2, 2352 ) ictime(i,k)/86400, mod(ictime(i,k),86400)/3600,   &
                                    mod(ictime(i,k),3600)/60, mod(ictime(i,k),60) ,   &
                                                              amassc( i, isb, k )
            enddo
         enddo

!       check stop-time continuous loads

         if ( ictmax(i) .gt. 0 ) then
            if ( ictime(i,ictmax(i)) .ne. itstopp ) then
               write ( lun2, 2381 )
               ierr = ierr + 1
            endif
         endif

   20 continue
      if ( nocont .gt. 0 ) deallocate(ascal)

! read actual waste polygons
      if (nodac .gt. 0 .and. nrowsmax .gt. 0) then
         call alloc ( "xpoltmp", xpoltmp, nrowsmax )
         call alloc ( "ypoltmp", ypoltmp, nrowsmax )
!        allocate memory for the waste polygons, and read them into memory
         call alloc ( "xpolwaste", xpolwaste, nrowsmax, nodac )
         call alloc ( "ypolwaste", ypolwaste, nrowsmax, nodac )
         call alloc ( "nrowswaste", nrowswaste, nodac )
         xpolwaste = 999.999
         ypolwaste = 999.999
         nrowswaste = 0

         do i = 1 , nodac
            if (radius(i).eq.-999.0) then
               call polpart(fiwaste(i), nrowsmax, xpoltmp, ypoltmp, nrowstmp, lun2)
               xpolwaste(1:nrowstmp, i) = xpoltmp(1:nrowstmp)
               ypolwaste(1:nrowstmp, i) = ypoltmp(1:nrowstmp)
               nrowswaste(i) = nrowstmp
            endif
         enddo
      endif

!       user defined releases

      if ( gettoken( noudef, ierr2 ) .ne. 0 ) goto 4053
      write ( lun2, 2301 ) noudef
      if ( noudef .gt. 0 ) then
         call alloc( "uscal ", uscal , noudef )
         call alloc( "isubud", isubud, noudef )
         call alloc( "iutime", iutime, noudef )
         call alloc( "ifopt ", ifopt , noudef )
         call alloc( "finud ", finud , noudef )
         call alloc( "iftime", iftime, noudef )
         call alloc( "nosud ", nosud , noudef )
         call alloc( "isfud ", isfud , noudef )
         call alloc( "isub  ", isub  , noudef )
         call alloc( "nosyss ", nosyss , noudef )
         call alloc( "aconud", aconud, nosubs, noudef )
         call alloc( "tmasud", tmasud, nosubs, noudef )
         call alloc ( "wparm", wparm , noudef+nodac )
         call alloc ( "ndprt", ndprt , noudef+nodac )
      endif

!     for each release

      do 30 i = 1 , noudef

!       percentage of particles of the release

         if ( gettoken( wparm(i+nodac), ierr2 ) .ne. 0 ) goto 4054
         if ( gettoken( uscal(i), ierr2 ) .ne. 0 ) goto 4054
         if ( wparm(i+nodac) >= 0.0 ) then
            ndprt(i+nodac) = int(wparm(i+nodac)*nopart/100.0 + 0.5)
         else
            write ( lun2, 3323 )
            ierr = ierr + 1
         endif
         
         write ( lun2, 2316) i, ndprt(i+nodac), wparm(i+nodac), uscal(i)

!       read time for delpar release, and substance number

         if ( gettoken( id, ierr2 ) .ne. 0 ) goto 4055
         if ( gettoken( ih, ierr2 ) .ne. 0 ) goto 4055
         if ( gettoken( im, ierr2 ) .ne. 0 ) goto 4055
         if ( gettoken( is, ierr2 ) .ne. 0 ) goto 4055
         if ( gettoken( isubud(i), ierr2 ) .ne. 0 ) goto 4055
         iutime(i) = id*86400 + ih*3600 + im*60 + is
         write ( lun2, 2378 )
         write ( lun2, 2379 ) id,ih,im,is,i,isubud(i)
!
!       read option no. for file type
!       0 = mapfile delwaq
!       1 = restart file delwaq
!       2 = particle coordinates file
!
         if ( gettoken( ifopt(i), ierr2 ) .ne. 0 ) goto 4056

!       read filename delwaq or particle coordinates file

         if ( gettoken( finud(i), ierr2 ) .ne. 0 ) goto 4057
         write ( lun2, 2317 ) finud(i), ifopt(i)
!
         if ( ifopt(i) .ne. 2 ) then
!
!       read time for delwaq, no., no. of subst. and subst. number
!
            if ( gettoken( id, ierr2 ) .ne. 0 ) goto 4058
            if ( gettoken( ih, ierr2 ) .ne. 0 ) goto 4058
            if ( gettoken( im, ierr2 ) .ne. 0 ) goto 4058
            if ( gettoken( is, ierr2 ) .ne. 0 ) goto 4058
            if ( gettoken( nosud(i), ierr2 ) .ne. 0 ) goto 4058
            if ( gettoken( isfud(i), ierr2 ) .ne. 0 ) goto 4058
            write ( lun2, 2376 )
            write ( lun2, 2377 ) id, ih, im, is, ifopt(i), nosud(i), isubud(i), isfud(i)
            iftime(i) = id*86400 + ih*3600 + im*60 + is
         else
            lunin = 51
            call openfl ( lunin, finud(i), 0  )
            read  ( lunin, err=5001 )
            read  ( lunin, err=5002 ) iftime(i)
            write ( lun2 , 2389 ) iftime(i)
            close ( lunin )
         endif

         if ( iftime(i) .lt. itstrtp .or. iftime(i) .gt. itstopp ) write ( lun2, 2176 ) i

   30 continue

!     read time points decays

      call alloc ( "decays", decays, nosubs )
      decays = 0.0
      if ( gettoken( idtset, ierr2 ) .ne. 0 ) goto 4059
      if ( idtset .gt. 0 ) then
         call alloc ( "idtime", idtime, idtset )
         call alloc ( "decay ", decay , nosubs, idtset )
         do 40 i = 1 , idtset
            if ( gettoken( id, ierr2 ) .ne. 0 ) goto 4060
            if ( gettoken( ih, ierr2 ) .ne. 0 ) goto 4060
            if ( gettoken( im, ierr2 ) .ne. 0 ) goto 4060
            if ( gettoken( is, ierr2 ) .ne. 0 ) goto 4060
            do isb = 1, nosubs
               if ( gettoken( decay(isb,i), ierr2 ) .ne. 0 ) goto 4060
            enddo
            idtime(i) = id*86400 + ih*3600 + im*60 + is
            if ( i .eq. 1 .and. idtime(i) .ne. itstrtp ) then
               write ( lun2, 2360 )
               ierr = ierr + 1
            endif
            if ( i .gt. 1 ) then
               if ( idtime(i) .le. idtime(i-1) ) then
                  write ( lun2, 2370 )
                  ierr = ierr + 1
               endif
            endif
   40    continue
         write ( lun2, '(1x,a)' ) ' Decay rates'
         do isb = 1, nosubs
            write ( lun2, '(6x,a)' ) substi(isb)
            do i = 1 , idtset
               write ( lun2, 2374 ) idtime(i)/86400, mod(idtime(i),86400)/3600,   &
                                    mod(idtime(i),3600)/60, mod(idtime(i),60) ,   &
                                                            decay( isb, i )
            enddo
         enddo
         if ( idtime(idtset) .ne. itstopp ) then
            write ( lun2, 2380 )
            ierr = ierr + 1
         endif
      endif

      write ( lun2, 2338 )

!     read power for vz = w * c ** n concentration dependent settling
!     and refinement factor for the plotgrid

      nvsfour = 6
      if ( gettoken( anfac, ierr2 ) .ne. 0 ) goto 4064
      if ( gettoken( irfac, ierr2 ) .ne. 0 ) goto 4065
      irfac = max(1,irfac)
      write ( lun2, 2339 ) anfac,irfac

!     read time points settling velocities

      if ( gettoken( ivtset, ierr2 ) .ne. 0 ) goto 4061
      if ( ivtset .gt. 0 ) then
         call alloc ( "ivtime", ivtime, ivtset )
         call alloc ( "vsfact", vsfact, nvsfour, nosubs )
         call alloc ( "vsfour", vsfour, nvsfour, nosubs, ivtset )

         do 50 isb = 1, nosubs
            write ( lun2, '(4x,a)' ) substi(isb)
            write ( lun2, 2373 )
            do i = 1, ivtset
               if ( gettoken( id, ierr2 ) .ne. 0 ) goto 4062
               if ( gettoken( ih, ierr2 ) .ne. 0 ) goto 4062
               if ( gettoken( im, ierr2 ) .ne. 0 ) goto 4062
               if ( gettoken( is, ierr2 ) .ne. 0 ) goto 4062
               do k = 1, nvsfour
                  if ( gettoken( vsfour(k,isb,i), ierr2 ) .ne. 0 ) goto 4062
               enddo
               write ( lun2, 2375 ) id, ih, im, is, ( vsfour(k,isb,i) , k = 1, nvsfour )
               if ( isb .eq. 1 ) then
                  ivtime(i) = id*86400 + ih*3600 + im*60 + is
                  if ( i .eq. 1 .and. ivtime(i) .ne. itstrtp ) then
                     write ( lun2, 2360 )
                     ierr = ierr + 1
                  endif
                  if ( i .gt. 1 ) then
                     if ( ivtime(i) .le. ivtime(i-1) ) then
                        write ( lun2, 2370 )
                        ierr = ierr + 1
                     endif
                  endif
               endif
            enddo
   50    continue

         if ( ivtime(ivtset) .gt. itstopp ) then
            write ( lun2, 2382 )
            ierr = ierr + 1
         endif
      endif

!       critical shear stress for sedimentation and erosion
!       chezy and the density of water in g/l

      chezy = 50.0
      if ( lsettl ) then
         if ( gettoken( taucs, ierr2 ) .ne. 0 ) goto 4063
         if ( gettoken( tauce, ierr2 ) .ne. 0 ) goto 4063
         if ( gettoken( chezy, ierr2 ) .ne. 0 ) goto 4063
         write ( lun2, 3125 ) taucs, tauce, chezy
      endif

!..  for sedimentation-erosion an extra layer is created
!..  for output routines part12, part13 and parths
!..  for plot routine part13 also extra subsyances names are created

      if ( lsettl ) then
         i = nolayp*nosubs
         write ( lun2, * ) ' Substances defined in bed layer (sed/erosion): '
         do isb = 1, nosubs
            i = i + 1
            subst(i)    = substi(isb)
            ln = min( len_trim(subst(i))+1, 18 )
            write ( subst(i)(ln:), '(a,i2.2)' ) '_', noslay
            write ( lun2, '(6x,a)' ) subst(i)
         enddo
      endif

!     close input file

      if ( alone ) then     
          close ( ilun(1) )
      endif 

!     check on the total number of particles:


!     check on valid zoom window for his, plo and psf file

      do i = 1, npgrid
         xw1f = abs( pg(i)%xhigh - pg(i)%xlow )
         yw1f = abs( pg(i)%yhigh - pg(i)%ylow )
         if ( xw1f .le. 1.0e-4 .or. yw1f .le. 1.0e-4 ) then
            write ( lun2, 2390 )
            ierr = ierr + 1
         endif
      enddo

!     readjustment of the user provided nopart

      do i = 1, nodye + nocont + noudef
         npmax = npmax + ndprt(i)
      enddo

      ! 'npmax' is the estimate (maximum) number of particles used in the calculation based on the wastes.
      ! This is used for the allocation of the arrays instead of the user provided 'nopart' read from the
      ! inp-file, because the sum of particles used for the wastes could be more (or less) than 100%.
      ! 'nopart' is used to scale the total number of particles in the calculation, only relative numbers are
      ! given for the wastes.
      write ( lun2, 3100 ) npmax

      ! Add 1% + 1 uncertainty margin here for the actual allocation because round offs might add some extra particles
      npmargin = npmax / 100
      npmax = npmax + npmargin + 1
      write ( lun2, 3110 ) npmax

!     further allocations

      if (zmodel) then
         call alloc ( "locdepp", locdepp, mnmax2   , noslay )
      endif
      call alloc ( "locdep", locdep, mnmax2   , noslay )
      call alloc ( "adepth", adepth, nosubs   , noslay )
      call alloc ( "apeak ", apeak , nosubs   , noslay )
      call alloc ( "atotal", atotal, noslay   , nosubs )
      call alloc ( "conc  ", concp , nosub_max, mnmax2*noslay )
      call alloc ( "flres ", flres , nosub_max, mnmaxk )
      if ( npgrid .gt. 0 ) then
         call alloc ( "amapsett", amapsett, nosubs   , noslay, pg(1)%nmap, pg(1)%mmap )
         do i = 1, npgrid
            cwork = "nmcell"
            if ( i .gt. 1 ) write( cwork(7:), '(i3.3)' ) i
            nullify ( pg(i)%nmcell )
            call alloc ( cwork, pg(i)%nmcell,                    pg(i)%nmap, pg(i)%mmap )
            cwork = "amap"
            if ( i .gt. 1 ) write( cwork(5:), '(i3.3)' ) i
            nullify ( pg(i)%amap   )
            call alloc ( cwork, pg(i)%amap  , nosub_max, noslay, pg(i)%nmap, pg(i)%mmap )
            cwork = "atrack"
            if ( i .gt. 1 ) write( cwork(7:), '(i3.3)' ) i
            nullify ( pg(i)%atrack )
            call alloc ( cwork, pg(i)%atrack,            noslay, pg(i)%nmap, pg(i)%mmap )
            cwork = "nbin"
            if ( i .gt. 1 ) write( cwork(5:), '(i3.3)' ) i
            nullify ( pg(i)%nbin   )
            call alloc ( cwork, pg(i)%nbin  ,            noslay, pg(i)%nmap, pg(i)%mmap )
            cwork = "imask"
            if ( i .gt. 1 ) write( cwork(6:), '(i3.3)' ) i
            nullify ( pg(i)%imask  )
            call alloc ( cwork, pg(i)%imask ,                    pg(i)%nmap, pg(i)%mmap )
         enddo
      endif
      call alloc ( "dfact ", dfact , nosubs       )
      call alloc ( "fstick", fstick, nosubs       )
      call alloc ( "isfile", isfile, nosub_max    )
      call alloc ( "tmass ", tmass , nosubs       )
      i = max(nodac+noudef,1)
      call alloc ( "nwaste", nwaste, i )
      call alloc ( "mwaste", mwaste, i )
      call alloc ( "aconc ", aconc , i, nosubs )
      call alloc ( "wpart ", wpart , nosubs      , npmax  )
      if (oil) then
         call alloc ( "wpartini ", wpartini , nfract   , npmax  )
      end if
      call alloc ( "spart ", spart , nosubs      , npmax  )
      call alloc ( "rhopart ", rhopart , nosubs      , npmax  )
      call alloc ( "abuoy ", abuoy , npmax        )
      call alloc ( "cbuff ", cbuff , npmax        )
      call alloc ( "ibuff ", ibuff , 3           , npmax  )
      call alloc ( "floil ", floil , npmax        )
      call alloc ( "iptime", iptime, npmax        )
      call alloc ( "imap  ", imap  , npmax       , 3      )
      call alloc ( "kpart ", kpart , npmax        )
      call alloc ( "mpart0", mpart0, npmax        )
      call alloc ( "npart0", npart0, npmax        )
      call alloc ( "npart ", npart , npmax        )
      call alloc ( "rbuff ", rbuff , 3           , npmax  )
      call alloc ( "t0buoy", t0buoy, npmax        )
      call alloc ( "track ", track , 10          , npmax  )
      call alloc ( "mpart ", mpart , npmax        )
      call alloc ( "wsettl", wsettl, npmax        )
      call alloc ( "xa0   ", xa0   , npmax        )
      call alloc ( "xa    ", xa    , npmax        )
      call alloc ( "xyztrk", xyztrk, 3           , npmax  )
      call alloc ( "xpart0", xpart0, npmax        )
      call alloc ( "xpart ", xpart , npmax        )
      call alloc ( "za    ", za    , npmax        )
      call alloc ( "ya0   ", ya0   , npmax        )
      call alloc ( "ya    ", ya    , npmax        )
      call alloc ( "zpart ", zpart , npmax        )
      call alloc ( "ypart0", ypart0, npmax        )
      call alloc ( "ypart ", ypart , npmax        )
      if ( nosta .gt. 0 ) then
         call alloc ( "ihplot", ihplot, nosta )
         call alloc ( "mplsta", mplsta, nosta )
         call alloc ( "mstat ", mstat , nosta )
         call alloc ( "nplsta", nplsta, nosta )
         call alloc ( "nstat ", nstat , nosta )
         call alloc ( "chismp", chismp, nosubs + 1, noslay, nosta )
         call alloc ( "chispl", chispl, nosubs + 1, noslay, nosta )
      endif
      if ( nrowsmax .gt. 0 ) then
         call alloc ( "xpol", xpol, nrowsmax )
         call alloc ( "ypol", ypol, nrowsmax )
      endif
      call alloc ( "elt_names", elt_names,    8+nosub_max*noslay )
      call alloc ( "elt_types", elt_types,    8+nosub_max*noslay )
      call alloc ( "elt_bytes", elt_bytes,    8+nosub_max*noslay )
      call alloc ( "elt_dims ", elt_dims , 6, 8+nosub_max*noslay )
      bufsize = max(noslay*pg(1)%nmap*pg(1)%mmap,noslay*mnmax2,nosta)
      call alloc ( "rbuffr   ", rbuffr   , bufsize )
      call alloc ( "v_swim", v_swim, npmax        )
      call alloc ( "d_swim", d_swim, npmax        )
      call alloc ( "ebb_flow", d_swim, npmax        )

      npmax = npmax - npmargin ! Deduct margin again

!     initialize arrays

      isfile = 0
      npart  = 0
      mpart  = 0
      iptime = 0
      xpart  = 0.0
      ypart  = 0.0
      zpart  = 0.0
      wpart  = 0.0
      if (oil) then
         wpartini = 0.0
      end if
      abuoy  = 0.0
      t0buoy = 0.0
      apeak  = 0.0
      wsettl = 0.0
      v_swim = 0.0
      d_swim = 0.0

!     stop when errors occured during reading

      if ( ierr .ne. 0 ) then
         write ( lun2, '(A,i3)' ) ' Number of errors in processing input file:', ierr
         call stop_exit(1)
      else
         write ( lun2, '(A   )' ) ' '
         write ( lun2, '(A   )' ) '  Input file succesfully read.'
         write ( lun2, '(A   )' ) ' '
         if ( .not. alone ) then        ! reset Delwaq settings
            cchar = cchar_save
            lunut = lunut_save
            npos  = npos_save
         endif
      endif

      if ( timon ) call timstop( ithndl )
      return

!     formats

!     informative formats

 1997 format(//,2x,'Applied hydrodynamics file (hyd-file): ',a/)
 2000 format(   2x,'Simulation: ',/(14x,a40))
 2001 format(/,2x,'Model type: ',/,14x,a,i6                          )
 2002 format(/,2x,'Model type: ',/,14x,a                             )
 2010 format(14x,2a,a10,' - ',a10                                      )
 2011 format(14x,'Relative height of division between layers: ',f10.3  )
 2020 format(/'  Number of particles to be used     :',i11             )
 2026 format(/'  Monitoring stations : ',/,  &
              '        location name               x           y    '  )
 2027 format(8x,a20,2(2x,f11.2)                                        )
 2040 format(/'  Roughness length in meters         :',f11.4           )
 2050 format( '  Horizontal displacement parameters :',2f11.4          )
 2060 format( '  Wind influence coefficient [%]     :',f11.4           )
 2065 format( '  Density of water [kg/m**3]         :',f11.4           )
 2071 format(/'  Table for wind:'/       &
             7x,'Time (dd hh mm ss)',15x,'Wind speed',  &
             13x,'Wind direction' )
 2074 format(8x,i4,'D-',i2.2,'H-',i2.2,'M-',i2.2,'S.',  &
             f20.4,' m/s ',f20.2,'  degr.')
 2090 format(/  &
             '  Start time simulation:',i4,'D-',i2.2,  &
                'H-',i2.2,'M-',i2.2,'S.')
 2110 format('  Stop  time simulation:',i4,'D-',i2.2,  &
                'H-',i2.2,'M-',i2.2,'S.')
 2121 format('  Delwaq take over time for particles: ',i10,' s.')
 2122 format('  No coupling with Delwaq, so no take-over time!' )
 2125 format('  Effective repeatcycle:',i4,'D-',i2.2,  &
               'H-',i2.2,'M-',i2.2,'S.')
 2130 format  &
          (/,'  Start time map file  :',i4,'D-',i2.2,  &
                'H-',i2.2,'M-',i2.2,'S.')
 2131 format  &
          (/,'  Start time time-hist.:',i4,'D-',i2.2,  &
                'H-',i2.2,'M-',i2.2,'S.')
 2170 format('  Time step in map file:',i4,'D-',i2.2,  &
                'H-',i2.2,'M-',i2.2,'S.')
 2171 format('  Time step time-hist. :',i4,'D-',i2.2,  &
               'H-',i2.2,'M-',i2.2,'S.')
 2180 format(/'  Offset to the real world time scale: ',/,  &
      '  Year: ',i4,', Month: ',i2.2,', ',i2.2,      &
                'D-',i2.2,'H-',i2.2,'M-',i2.2,'S.')
 2190 format(/'  Time steps written to plot file (*.plo):')
 2210 format(5x,i4,'D-',i2.2,'H-',i2.2,'M-',i2.2,'S.',  &
                                              '  Recovery rate:',f10.5 )
 2229 format(/'  Zoom window')
 2230 format( '     Xmin        = ',f11.2,' Xmax        = ',f11.2)
 2240 format( '     Ymin        = ',f11.2,' Ymax        = ',f11.2)
 2241 format( '     Cells for x = ',i11  ,' Cells for y = ',i11)
 2250 format(/'  Instantaneous release stations:')
 2258 format(8x,'Station name :',a20  )
 2259 format(8x,'Station name :',a20  )
 2260 format(12x,'Release time            =',   &
                         i4,'D-',i2.2,'H-',i2.2,'M-',i2.2,'S.')
 2280 format(12x,'Coordinates             = (',f13.4,',',f13.4,')'/  &
             12x,'Depth(%) under surface  =  ',f12.1)
 2281 format(12x,'Coordinates             = (',f13.4,',',f13.4,')'/  &
             12x,'Layer                   =  ',i11  )
 2282 format(12x,'Initial radius          =   ',f13.2, ' m.',/,      &
             12x,'Percentage of particles =   ',f12.1, ' %')
 2283 format(12x,'Release in polygon      =   ',A,' (The coordinates above are ignored)'/, &
             12x,'Percentage of particles =   ',f12.1, ' %')
 2289 format(12x,'Released masses : ')
 2290 format(20x,'  Substance : ',a20,e13.4,'  kg/m3')
 2300 format(/'  Number of continuous release stations:', i2  /       )
 2301 format(/'  Number of user defined releases    :', i2  /         )
 2316 format( '  User defined release: ',i3,' no. of particles: ', i9,/,  &
              '  Percentage of particles =   ',f12.1, ' %'           ,/,  &
              '  Scale factor of release =   ',f12.1, '  '             )
 2317 format( '  Filename ud release : ',  a80,                       /,  &
              '  Option for this file: ',i3,                          /)
 2329 format(/8x,'Station name :',a,' released substances')
 2331 format(14x,a,f12.3,5x,a)
 2335 format(//'  Number of breakpoints for continuous loads : ',i5,/  )
 2338 format(//' Settling velocities ')
 2339 format(/4x,'Power for settling velocity (v=w*C**n)  : ',e15.6/  &
              4x,'Refinement factor of the plotgrid for C : ',i5    )
 2352 format( 18x,i4.2,'-',i2.2,'-',i2.2,'-',i2.2,e20.3,' kg/s')
!                              1234567890112345678901....123456789
 2373 format(10x,'time',8x,'base',4x,'amplitude',7x,'period',       &
             8x,'phase',6x,'minimum',6x,'maximum',/,          &
             4x,'dd-hh-mm-ss',7x,'(m/s)',7x,'(m/s)',6x,'(hours)',   &
             6x,'(hours)',8x,'(m/s)',8x,'(m/s)')
!                              1234567890112345678901....123456789
 2374 format(15x,' ' ,i4.2,'-',i2.2,'-',i2.2,'-',i2.2,e11.3,' 1/day')
 2375 format(2x,'  ',i4.2,'-',i2.2,'-',i2.2,'-',i2.2, 6(e11.3,2x)  )
 2376 format(/'     time       option nosyss  delpar subst delwaq subst'/  &
             ,'  dd-hh-mm-ss ',//                                      )
 2377 format( ' ',i4.2,'-',i2.2,'-',i2.2,'-',i2.2,':',  &
                  2(i4,4x),3x,2(i4,6x)    )
 2378 format(/'     time      mud number     delpar subst             '/  &
             ,'  dd-hh-mm-ss '                                         )
 2379 format( ' ',i4.2,'-',i2.2,'-',i2.2,'-',i2.2,':',2(i4,4x)         )
 2389 format(/'   time on file for mud release = ',i9                   )
 3100 format(/,'   The estimated number of particles in the calculation is approximately ',i11, '.')
 3110 format(  '   The number of particles used for memory allocation (ca. 1% extra) is  ',i11, '.')
 3115 format(  '  Relative thickness per layer')
 3120 format(  '        layer ',i4,'; relative thickness = ',f12.5    )
 3125 format(/,'  Critical shear stress sedimentation= ',f12.5,' (Pa)',    &
             /,'  Critical shear stress for erosion  = ',f12.5,' (Pa)',    &
             /,'  Chezy value                        = ',f12.5,'(m^1/2 s-1) ')
 3132 format(/,'  Total number of sticking substances = ',i3          )
 3133 format(  '     Sticking number for substance ',i3,' = ',i3      )
 3300 format(/,'  Number of dispersant applications     :',i3 )
 3301 format(/,'  Number of boom introductions     :',i3 )
 3310 format(5x,i4,'D-',i2.2,'H-',i2.2,'M-',i2.2,'S.', '  Type: ',i2, &
             ' Fraction: ',a20,' Parameter: ',f10.5,'  Polygon file: ', a)

!
!     warning formats
!
 2175 format('  Warning 1001. DELWAQ coupling specified; check map-',  &
               'step!')
 2176 format('  Warning 3001. Time ud release on file ',i3,            &
             '  out of simulation                                     ')
!
!     error formats
!
 2015 format('  Error 1001. Model-type-choice', i5, '; out of range!'  )
 2022 format('  Error 1101. Time step is not a divider of time step ',  &
             '  in hydrodynamic database: interpol. errors will occur' )
 2023 format('  Error 1100. Time step should be less equal than step',  &
              ' in hydrodynamic database '                          )
 2024 format('  Error 1101. History time step should be greater than',  &
              ' zero!'                                                 )
 2025 format('  Error 1102. Number of stations exceeds the ',/,  &
             '         system maximum of',i7,'!'                       )
 2070 format(/' Error 1104. Number of wind variations; min. 2, max.',  &
                i4, '; choosen :', i4, '!'                             )
 2072 format(/' Error 1201. Start time wind series must be equal to',  &
              ' or lower than start time simulation!'                  )
 2073 format(/' Error 1202. Wind time serie must be in ascending',     &
              ' order!'                                                )
 2075 format(/' Error 1203. Stop time wind series must be equal to',   &
              ' or greater than stop time simulation!'                 )
 2100 format(/' Error 1301. Start time flow-file: ',i4,'D-',i2,'H-',i2,&
               'M-',i2,'s; times do not match!'                        )
 2120 format('  Error 1302. Simulation time does not divide by time',  &
               ' step of', i6,'s!'                                     )
 2140 format('  Error 1401. Start time of map-file earlier than start',&
              ' of simulation !'                                       )
 2145 format('  Error 1402. Start time of map-file not equal to start',&
              ' time of simulation for delwaq take-over! '             )
 2150 format('  Stop  time map file  :',i4,'D-',i2.2,'H-',i2.2,'M-',   &
                i2.2,'S.')
 2151 format('  Stop  time-histories :',i4,'D-',i2.2,'H-',i2.2,'M-',   &
                i2.2,'S.')
 2160 format('  Error 1403. Stop time of map-file earlier than start', &
              ' time of map-file !'                                    )
 2165 format('  Error 1404. Stop time of map-file not equal to start', &
              ' time of simulation for delwaq take over !'             )
 2166 format('  Error 1405. Start time of time-histories earlier than', &
              ' start time of simulation !'                            )
 2167 format('  Error 1406. Stop time of time-hist. earlier than start',&
              ' time of time-hist.'                                    )
 2200 format('  Error 1501. Number exceeds the system maximum of',i3,  &
                                                                    '!')
 2220 format('  Error 1502. Plot step out of simulation range ! '      )
 2222 format('  Error 1503. Plot step out of ascending order ! '       )
 2224 format('  Error 1504. Time step map output not matching with ',  &
              ' time step hydrodynamics!'                              )
 2270 format('  Error 1601. Dye release time out of simulation range!' )
!2305 format('  Error 1701. No continuous releases for oil allowed!'   )
 2337 format('  Error 1801. Out of range, minimum = 2, maximum =', i5, &
                                                                    '!')
 2360 format('  Error 1802. Start of time series not equal to start',  &
              ' of simulation!'                                        )
 2370 format('  Error 1803. Time steps not in increasing order!'       )
 2380 format('  Error 1901. Stop decay  time series not equal to stop',&
              ' of simulation!'                                        )
 2381 format('  Error 1902. Stop of continuous time series not equal', &
              ' to stop of simulation!'                                )
 2382 format('  Error 1903. Stop settling vel.series not equal to stop',&
              ' of simulation!'                                        )
 2390 format('  Error: invalid zoom grid',/  &
             12x,'this grid is required for output plo and psf file ')
 3320 format('  Error 2001. Dispersant application time out of', &
                  ' simulation range! '      )
 3321 format('  Error 2002. Dispersant application time not a plural of',&
                  ' model time step! '      )
 3322              format('  Error 2003. Dispersant application time not in ascending',&
                  ' order or at the same time as previous!'       )
 3323 format('  Error 2003. Particle percentaage should be positive')

                  
 3500 format('  Found plastics_parameters keyword '       )
 3501 format(/'  Plastics name                      : ',A)
 3502 format( '  Plastics density            [g/m3] : ',F14.2)
 3503 format( '  Plastics shape factor          [-] : ',F14.4)
 3504 format( '  Plastics mean size             [m] : ',E14.4)
 3505 format( '  Plastics std dev size          [m] : ',E14.4)
 3506 format( '  Plastics fragmentation rate  [1/d] : ',F14.4)
 3507 format(/'  ', A, ' is active in the current model'/)
 3508 format(/'  ', A, ' is NOT active in the current model, settings not used!'/)
 3509 format(/'  No parameters found for plastic named : ',A)
 3510  format(/'  Parameters were found for all plastics'/)
      

11    write(*,*) ' Error when reading the model type '
      write(*,*) ' Is this version 3.50?'
      call stop_exit(1)
4001  write(*,*) 'Error: version string can not be read correctly'
      call stop_exit(1)
4002  write(*,*) 'Error: 4 title strings can not be read correctly'
      call stop_exit(1)
4003  write(*,*) 'Error: name of hyd file can not be read correctly'
      call stop_exit(1)
4007  write(*,*) &
      'Error: numerical scheme or time-step can not be read correctly'
      call stop_exit(1)
4008  write(*,*) &
       'Error: vert. disperson option, vert.disp scale factor', &
       'or vert.disp constant can not be read correctly'
      call stop_exit(1)
4009  write(*,*) 'Error: names of substances can not be read correctly'
      call stop_exit(1)
4011  write(*,*) 'Error: filename of nh4-file can not be read correctly'
      call stop_exit(1)
4012  write(*,*) 'Error: filename of no3-file can not be read correctly'
      call stop_exit(1)
4013  write(*,*) 'Error: number of particles can not be read correctly'
      call stop_exit(1)
4014  write(*,*) 'Error: roughness length can not be read correctly'
      call stop_exit(1)
4015  write(*,*) 'Error: horiz. disp. params can not be read correctly'
      call stop_exit(1)
4016  write(*,*) 'Error: wind drag coefficien can not be read correctly'
      call stop_exit(1)
40161 write(*,*) 'Error: density of water can not be read correctly'
      call stop_exit(1)
4017  write(*,*) 'Error: no. wind breakpoints can not be read correctly'
      call stop_exit(1)
4018  write(*,*) 'Error: breakpoint for wind can not be read correctly'
      call stop_exit(1)
4019  write(*,*) 'Error: no. model specific constants',  &
       ' can not be read correctly'
      call stop_exit(1)
4020  write(*,*) 'Error: value of a model specific constant', &
       ' can not be read correctly'
      call stop_exit(1)
4021  write(*,*) 'Error: start time of simulation',&
       ' can not be read correctly'
      call stop_exit(1)
4022  write(*,*) 'Error: stop time of simulation', &
       ' can not be read correctly'
      call stop_exit(1)
4023  write(*,*) 'Error: waq take over time can not be read correctly'
      call stop_exit(1)
4024  write(*,*) 'Error: start time of map file',  &
       ' can not be read correctly'
      call stop_exit(1)
4025  write(*,*) 'Error: stop time of map file',   &
       ' can not be read correctly'
      call stop_exit(1)
4026  write(*,*) 'Error: time-step in map file',   &
       ' can not be read correctly'
      call stop_exit(1)
4027  write(*,*) 'Error: start time of his file',  &
       ' can not be read correctly'
      call stop_exit(1)
4028  write(*,*) 'Error: stop time of his file',   &
       ' can not be read correctly'
      call stop_exit(1)
4029  write(*,*) 'Error: time-step in map file',   &
       ' can not be read correctly'
      call stop_exit(1)
4030  write(*,*) 'Error: time offset to real time for plotfile ', &
       ' can not be read correctly'
      call stop_exit(1)
4031  write(*,*) 'Error: no. of observation points ', &
       ' can not be read correctly'
      call stop_exit(1)
4032  write(*,*) 'Error: names/coor. stations can not be read correctly'
      call stop_exit(1)
4033  write(*,*) 'Error: no. plot grids can not be read correctly'
      call stop_exit(1)
4034  write(*,*) 'Error: breakpoint plotgrid can not be read correctly'
      call stop_exit(1)
4038  write(*,*) 'Error: (x,y) of zoom window can not be read correctly'
      call stop_exit(1)
4039  write(*,*) &
      'Error: no. instantaneous releases can not be read correctly'
      call stop_exit(1)
4040  write(*,*)  &
      'Error: names instantaneous releases can not be read correctly'
      call stop_exit(1)
4041  write(*,*) &
      'Error: breakpoint instantaneous rel. can not be read correctly'
      call stop_exit(1)
4042  write(*,*) &
      'Error: (x,y,k) instantaneous release can not be read correctly'
      call stop_exit(1)
4043  write(*,*) 'Error: relative z-coordinate or radius (m) or rel-%', &
       ' of the instantaneous release can not be read correctly'
4044  write(*,*) &
       'Error: mass of substances released for instantaneous release ', &
       ' can not be read correctly'
      call stop_exit(1)
4045  write(*,*) 'Error: no. cont.releases can not be read correctly'
      call stop_exit(1)
4046  write(*,*) 'Error: names cont.releases can not be read correctly'
      call stop_exit(1)
4047  write(*,*) 'Error: (x,y,k) cont.release can not be read correctly'
      call stop_exit(1)
4049  write(*,*) 'Error: scale factor of substance ', &
       ' can not be read correctly'
4050  write(*,*) 'Error: stoechiometric coefficient of substance ', &
       ' can not be read correctly'
      call stop_exit(1)
4051  write(*,*) 'Error: no. breakp. cont.rel can not be read correctly'
      call stop_exit(1)
4052  write(*,*) 'Error: breakpoint cont.rel. can not be read correctly'
      call stop_exit(1)
4053  write(*,*) 'Error: no. of user defined releases ', &
       ' can not be read correctly'
      call stop_exit(1)
4054  write(*,*) 'Error: percentages and scale factors of user def.rel', &
       ' can not be read correctly'
      call stop_exit(1)
4055  write(*,*) 'Error: breakpoint and subst-no. of user def.release', &
       ' can not be read correctly'
      call stop_exit(1)
4056  write(*,*) 'Error: option for file type of user def.release', &
       ' can not be read correctly'
      call stop_exit(1)
4057  write(*,*) 'Error: filename of user defined release', &
       ' can not be read correctly'
      call stop_exit(1)
4058  write(*,*) 'Error: breakpoint and subst-no. of user def.release', &
       ' for the map or restart file from waq can not be read correctly'
      call stop_exit(1)
4059  write(*,*) 'Error: no. of breakpoints for decay rate ', &
       ' can not be read correctly'
      call stop_exit(1)
4060  write(*,*) 'Error: breakp. decay rate can not be read correctly'
      call stop_exit(1)
4061  write(*,*) 'Error: no. of breakpoints for settling velocities ', &
       ' can not be read correctly'
      call stop_exit(1)
4062  write(*,*) 'Error: breakp. settl. vel. can not be read correctly'
      call stop_exit(1)
4063  write(*,*) ' Error: sedimentation-erosion params are missing or', &
       ' can not be read correctly'
      call stop_exit(1)
4064  write(*,*) ' Error: power for concentration dependent settling', &
       ' can not be read correctly'
      call stop_exit(1)
4065  write(*,*) ' Error: grid refinement factor', &
       ' can not be read correctly'
      call stop_exit(1)
4066  write(*,*) ' Error: layer number of instantaneous release higher', &
       ' than number of layers'
      call stop_exit(1)
4067  write(*,*) ' Error: layer number of continuous release higher', &
       ' than number of layers'
      call stop_exit(1)

5001  write(*,*) 'Error: 1th record of particles coordinates file', &
       ' can not be read correctly'
      call stop_exit(1)
5002  write(*,*) 'Error: 2nd record of particles coordinates file', &
       ' can not be read correctly'
      call stop_exit(1)
5049  write(*,*) 'Error: interpolation options for cont. releases ', &
       ' can not be read correctly'
      call stop_exit(1)
6001  write(*,*) 'Error: option for initial condition (oil)'
      call stop_exit(1)
6002  write(*,*) 'Error: number of dispersant applications', &
       ' can not be read correctly'
      call stop_exit(1)
6003  write(*,*) 'Error: timing of dispersant application', &
       ' can not be read correctly'
      call stop_exit(1)
6005  write(*,*) 'Error: type of dispersant application parameterisation', &
       ' can currently only be 1 (direct chance to disperse)'
      call stop_exit(1)
6006  write(*,*) 'Error: parameter of dispersant application', &
       ' can not be read correctly'
      call stop_exit(1)
6007  write(*,*) 'Error: parameter of dispersant application', &
       ' of type 1 must be set between 0.0 and 1.0'
      call stop_exit(1)
6010  write(*,*) 'Error: filename of dispersant application', &
       ' can not be read correctly'
      call stop_exit(1)
6011  write(*,*) 'Error: option for initial condition (oil) ', &
       ' can not be read correctly'
      call stop_exit(1)
6012  write(*,*) 'Error: file name for initial condition (ini-file)', &
       ' can not be read correctly (warm start)'
      call stop_exit(1)
6022  write(*,*) 'Error: number of boom introductions', &
       ' can not be read correctly'
      call stop_exit(1)
6023  write(*,*) 'Error: timing of boom introductions', &
       ' can not be read correctly'
      call stop_exit(1)
      call stop_exit(1)
6025  write(*,*) 'Error: type of boom parameterisation', &
       ' can currently only be 1 (chance be holded by the boom)'
      call stop_exit(1)
6026  write(*,*) 'Error: parameter of boom can not be read correctly'
      call stop_exit(1)
6027  write(*,*) 'Error: parameter of boom of type 1 must be set between 0.0 and 1.0'
      call stop_exit(1)
6030  write(*,*) 'Error: filename of boom polygon can not be read correctly'
      call stop_exit(1)
1700  write(*,*) ' Error: could not open dis-file ',fidisp(i)
      call stop_exit(1)
1701  write(*,*) ' Error: could not open boom-file ',fiboom(i)
      call stop_exit(1)
1702  write(*,*) ' Error: could not open instantaneous waste polygon-file ',fidye(i)
      call stop_exit(1)
1703  write(*,*) ' Error: could not open continuous waste polygon-file ',fiwaste(i+nodye)
      call stop_exit(1)
1710  write(*,*) ' Error: could not open ini-file ',ini_file
      call stop_exit(1)

9000  write(lun2,*) ' Error: reading special features '
      call stop_exit(1)
9005  write(lun2,*) ' Error: reading value of max_wind_drag_depth constant'
      call stop_exit(1)
9006  write(lun2,*) ' Error: value of max_wind_drag_depth is less than zero'
      call stop_exit(1)
9010  write(lun2,*) ' Error: value of max_restart_age constant'
      call stop_exit(1)
9011  write(lun2,*) ' Error: max_restart_age is zero. Did you specify a value?'
      call stop_exit(1)

9101  write(lun2,*) ' Error: found plastics_parameters, but this is not a plastics model (modtye /= 6 (model_prob_dens_settling) '
      call stop_exit(1)
9103  write(lun2,*) ' Error: expected substance name of a plastic to be specified '
      call stop_exit(1)
9104  write(lun2,*) ' Error: could not read plastic parameter correctly for ', trim(cplastic)
      call stop_exit(1)
9105  write(lun2,*) ' Error: zero or negative mean size specified for ', trim(cplastic)
      call stop_exit(1)
9106  write(lun2,*) ' Error: plastic "', trim(cplastic), '" was already defined! '
      call stop_exit(1)
9107  write(lun2,'(/A,I3,A)') '  Error: ', plmissing, ' plastic(s) is/are not parametrised! '
      call stop_exit(1)
9201  write(lun2,*) ' Error: expected leftside permeability of screens!'
      call stop_exit(1)
9202  write(lun2,*) ' Error: expected rightside permeability of screens!'
      call stop_exit(1)
9203  write(lun2,*) ' Error: expected screens polygon file name!'
9204  write(lun2,*) ' Error: could not open screens polygon-file: '//trim(fiscreens)
      call stop_exit(1)
9301  write(lun2,*) ' Error: reading part FM ini file name!'
      call stop_exit(1)
9302  write(lun2,*) ' Error: reading part FM elease file name!'
      call stop_exit(1)
9401  write(lun2,*) ' Error: found "ABMmodel" keyword, but this is not a ABM model (modtyp /= 7 (model_abm)) '
      call stop_exit(1)
9402  write(lun2,*) ' Error: expected abm model name!'
      call stop_exit(1)
9403  write(lun2,*) ' Error: no suitable abm model name supplied!'
      call stop_exit(1)
9404  write(lun2,*) ' Error: expected abm stage development method name!'
      call stop_exit(1)
9405  write(lun2,*) ' Error: no suitable stage delelopment method name supplied!'
      call stop_exit(1)
9406  write(lun2,*) ' Error: found "RevChron" keyword, but this is not a ABM model (modtyp /= 7 (model_abm)) '
      call stop_exit(1)
9407  write(lun2,*) ' Error: expected stage of reversed release!'
      call stop_exit(1)

      end

      subroutine getdim_dis ( lun      , dis_file , nrowsmax, lunlog   )
!
!     programmer : michelle jeuken
!     credits    : derived from getdim_ini
!     function   : get dimensions from dispersant-file
!                  (only max. no. of rows per polygone)
!     date       : sep 2013
!
      use get_key_mod
      use openfl_mod      ! explicit interface
      use m_waq_precision       ! flexible size definition
      implicit none           !   force explicit typing
      integer, parameter                  :: max_len_line=200
      integer, parameter                  :: max_len_key=20
      integer                             :: len_file, len_line
      integer                             :: lun, ios
      integer                             :: nrows, nrowsmax
      integer                             :: lunlog
      character(len=256)                  :: dis_file
      character(len=max_len_line)         :: line

      len_file = len_trim(dis_file)

      call skip_comment_lines(lun,ios) ! skip comment lines
      if (ios /= 0) go to 1010

      read (lun,'(a)',iostat=ios) line ! skip block name
      if (ios /= 0) go to 1010

      read (lun,*,iostat=ios)  nrows   ! only read nrows of first polygon
      if (ios /= 0) go to 1010

      nrowsmax = max(nrowsmax, nrows)

      return

!     error handling

 1010 write(*,'(//a,a)')     &
        ' Error: problem while reading dis-file ',dis_file(:len_file)
      backspace lun
      read(lun,'(a)') line ! get last read line
      len_line = len_trim(line)
      len_file = len_trim(dis_file)
      write(lunlog,'(//a,a)')  &
        ' Error: problem while reading dis-file ',dis_file(:len_file)
      write(lunlog,'(a,a)')    &
        '          last line read : ',line(:len_line)
      write(lunlog,'(/a)') ' Please check file !!'
      call stop_exit(1)
   end subroutine

   subroutine getdim_ini ( lun      , ini_file , npart_ini, npolmax  , nrowsmax , &
                              lunlog   )
!
!     programmer : antoon koster
!     function   : get dimensions from ini-file
!                  (no. of initial particles and
!                   max. no. of rows per polygone)
!     date       : may 2004
!
      use get_key_mod
      use openfl_mod      ! explicit interface
      use m_waq_precision       ! flexible size definition
      implicit none           !   force explicit typing
      integer, parameter                  :: max_len_line=200
      integer, parameter                  :: max_len_key=20
      integer                             :: npart_ini,lun, ios
      integer                             :: npolmax, npart_pol
      integer                             :: nrows,irow
      integer (int_wp )                        :: nrowsmax
      integer                             :: lunlog
      integer                             :: len_line,len_file
      character(len=256)                  :: ini_file
      character(len=max_len_line)         :: line
      character(len=max_len_key )         :: key
      logical                             :: key_found

      len_file = len_trim(ini_file)

      npart_ini = 0  ! count no. of initial particles
      npolmax   = 0  ! count no. of polygones
      ios       = 0
      do while ( ios == 0 )

          key = 'particles'
          call get_int_key(lun,key,npart_pol,key_found)
          if (.not. key_found) go to 1000
          npart_ini = npart_ini + npart_pol

          call skip_comment_lines(lun,ios) ! skip comment lines
          if (ios /= 0) go to 1010

          read (lun,'(a)',iostat=ios) line ! skip block name
          if (ios /= 0) go to 1010

          read (lun,*,iostat=ios)  nrows
          if (ios /= 0) go to 1010

          nrowsmax = max(nrowsmax,nrows)
          do irow=1,nrows
             read (lun,'(a)',iostat=ios) line ! skip polygone
             if (ios > 0) go to 1010  ! read error
             if (ios < 0) go to 1020  ! end-of-file
          enddo
          npolmax = npolmax + 1
          if ( .not. more_data(lun) ) go to 100

      enddo
  100 continue
      return

!     error handling

 1000 write(lunlog,'(//a,a)')  &
        ' Error: problem while reading ini-file ',ini_file(:len_file)
      write(lunlog,'(//a,a,a,a)')  &
        ' Error: could not find key ',key
      write(lunlog,'(/a)') ' Please check file !!'
      call stop_exit(1)
 1010 write(*,'(//a,a)')     &
        ' Error: problem while reading ini-file ',ini_file(:len_file)
      backspace lun
      read(lun,'(a)') line ! get last read line
      len_line = len_trim(line)
      len_file = len_trim(ini_file)
      write(lunlog,'(//a,a)')  &
        ' Error: problem while reading ini-file ',ini_file(:len_file)
      write(lunlog,'(a,a)')    &
        '          last line read : ',line(:len_line)
      write(lunlog,'(/a)') ' Please check file !!'
      call stop_exit(1)
 1020write(lunlog,'(//a,a)')  &
        ' Error: problem while reading ini-file ',ini_file(:len_file)
      write(lunlog,'(//a)') ' Error: premature end-of-file found'
      write(lunlog,'(/a )') ' Please check file !!'
      call stop_exit(1)
      end subroutine

subroutine getdim_asc ( lun , asc_file , npart_ini, nrowsmax , &
                              lunlog   )

      use get_key_mod
      use openfl_mod          ! explicit interface
      use m_waq_precision      ! flexible size definition
      implicit none           ! force explicit typing

      integer, parameter                  :: max_len_line=200
      integer, parameter                  :: max_len_key=20
      integer                             :: npart_ini,lun, ios
      integer                             :: npart_asc, npart_fact
      integer (int_wp )                        :: nrowsmax
      integer                             :: lunlog
      integer                             :: len_file
      character(len=256)                  :: asc_file
      character(len=max_len_key )         :: key
      logical                             :: key_found

      len_file = len_trim(asc_file)

      npart_ini = 0  ! count no. of initial particles
      ios       = 0
      do while ( ios == 0 )

          key = 'particles'
          call get_int_key(lun,key,npart_asc,key_found)
          if (.not. key_found) go to 1000

          key = 'factor'
          call get_int_key(lun,key,npart_fact,key_found)
          if (.not. key_found) go to 1000
          npart_ini = npart_ini + npart_asc*npart_fact

          nrowsmax = npart_asc
          if ( .not. more_data(lun) ) go to 100

      enddo
  100 continue
      return

!     error handling

 1000 write(lunlog,'(//a,a)')  &
        ' Error: problem while reading ini-file ',asc_file(:len_file)
      write(lunlog,'(//a,a,a,a)')  &
        ' Error: could not find key ',key
      write(lunlog,'(/a)') ' Please check file !!'
      call stop_exit(1)
      end subroutine


      logical function more_data(lun)
!
!     programmer : antoon koster
!     function   : check after reading polygone, if there
!                  more data (polygones) available
!     date       : may 2004

      implicit none           !   force explicit typing
      integer, parameter           :: max_len_line=200
      integer                      :: nlines, lun, ios, iline
      logical                      :: end_of_file, comment_line
      character(len=max_len_line)  :: line

      nlines      = 0
      end_of_file = .false.
      do while (.not. end_of_file)
         read (lun,'(a)',iostat=ios) line
         end_of_file  = ios < 0
         comment_line = line(1:1) == '*' .or. line(1:1) == ' '
         if (end_of_file .or. .not. comment_line) exit
         nlines = nlines + 1
      enddo
!
!     only rewind file, if there are more data (=polygones)
!     remaining
!
      if (.not. end_of_file) then
!        rewind file
         do iline=1,nlines+1
            backspace lun
         enddo
         more_data = .true.
      else
         more_data = .false.
      endif
      return
      end function more_data


end module m_rdpart
