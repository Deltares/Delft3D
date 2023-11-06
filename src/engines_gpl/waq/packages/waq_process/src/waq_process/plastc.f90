module m_plastc

implicit none

contains

      subroutine PLASTC     ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
      use m_write_error_message
      use m_evaluate_waq_attribute

!XXXDEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'PLASTC' :: PLASTC
!*******************************************************************************
!
      IMPLICIT NONE
!
!     Type    Name         I/O Description
!
      real(4) pmsa(*)     !I/O Process Manager System Array, window of routine to process library
      real(4) fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
      integer ipoint(*)  ! I  Array of pointers in pmsa to get and store the data
      integer increm(*)  ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! I  Number of computational elements in the whole model schematisation
      integer noflux      ! I  Number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
!
!*******************************************************************************
!     D-EM Plastics Model Version 0.1, August 2019 JvG / LB
!          15-08-2019 First version            JvG
!
!     NOTE every receptor is a layer and that the numbering is the same as in the hydrology model adaptor
!     NOTE all time is in seconds
!     NOTE we avoid an artificial delay in compartments without a potential accumulation by using the totflxin matrix (substance, compartment)
!          we fill this going from upstream to downstream, and the outflow from any compartment can therefore include the inflow from upstream (if we want)
!
!     Type    Name         I/O Description                                        Unit
!
!     support variables
      integer,parameter  :: npmsamax = 200
      integer            :: ipnt(npmsamax)    !    Local work array for the pointering
      integer            :: iseg, isegl, iflux, ip, irec, ioq, iatt1, npmsa, ipmsa
      real               :: flux, ro_mmperday, fwashoff, fluxexp
      real*8             :: fluxloss, fluxbur, fluxwash


      ! fixed quantities
      integer,parameter   :: scu = 1
      integer,parameter   :: nsubs = 1
      integer,parameter   :: nrec = 3
      integer,parameter   :: rec_pav = 1
      integer,parameter   :: rec_unp = 2
      integer,parameter   :: rec_sfw = 3

      ! PMSA admin
      integer             :: lins
      integer,parameter   :: line = 0
      integer             :: louts
      integer             :: loute
      integer             :: offset_vel

      ! Flux admin
      integer,parameter   :: iflrel = 1
      integer,parameter   :: ifldec = 2
      integer,parameter   :: iflbur = 3

      ! transport admin CONFUSING, COUPLING ALWAYS TAKES INTERNALS BEFORE BOUNDARIES, DESPITE ORDER IN FLUXES DEFINITION
      integer,parameter   :: pav2sfw = 1
      integer,parameter   :: unp2sfw = 2
      integer,parameter   :: sfw2exp = 3

      ! pointers to concrete items in PROCES.ASC
      integer,parameter   :: ip_nosegl    = 1
      integer,parameter   :: ip_delt      = 2
      integer,parameter   :: ip_totsurf   = 3
      integer,parameter   :: ip_fpaved    = 4
      integer,parameter   :: ip_funpaved  = 5
      integer,parameter   :: ip_fwater    = 6
      integer,parameter   :: ip_ropaved   = 7
      integer,parameter   :: ip_rounpaved = 8
      integer,parameter   :: ip_itime     = 9
      integer,parameter   :: ip_kbur      = 10
      integer,parameter   :: ip_kdecpav   = 11
      integer,parameter   :: ip_kdecunp   = 12
      integer,parameter   :: ip_mpw       = 13
      integer,parameter   :: ip_mpww      = 14
      integer,parameter   :: ip_mpws      = 15
      integer,parameter   :: ip_plastic   = 16
      integer,parameter   :: ip_lotpav    = 17
      integer,parameter   :: ip_hitpav    = 18
      integer,parameter   :: ip_lotunp    = 19
      integer,parameter   :: ip_hitunp    = 20
      integer,parameter   :: lastsingle   = 20

      ! input items
      integer             :: nosegl     ! # of segments per layer (horizontal schematisation elements, SCs + SWBs)
      real*8              :: delt       ! time step
      real                :: totsurf    ! total area
      real                :: fpaved     ! fracrion paved
      real                :: funpaved   ! fraction unpaved
      real                :: fwater     ! fraction water
      real                :: ropaved    ! runoff from paved areas
      real                :: rounpaved  ! unpaved
      integer             :: itime      ! actual time  (not currently used, but still in tables
      real                :: kbur
      real                :: kdecpav
      real                :: kdecunp
      real                :: mpw, mpww, mpws
      real*8              :: plastic
      real                :: lotpav
      real                :: hitpav
      real                :: lotunp
      real                :: hitunp

      ! specific other variables
      real                :: totmpw

      ! work arrays
      real,allocatable    :: frac2rec(:),emisw(:)
      real*8,allocatable    :: totflxin(:,:) ! total losses per receptor and per substance in current SC/SWB

      ! files
      integer,save      :: lu_bin
      integer,save      :: lu_txt
      character*80,parameter :: filbin = 'plastc_em.bin'
      character*80,parameter :: filtxt = 'plastc_em.txt'

      !     other
      logical first
      data first /.true./

      save frac2rec, totflxin, emisw
      save first, npmsa, nosegl, delt, lins, louts, offset_vel



!
!******************************************************************************* INITIAL PROCESSING

      if (first) then

            ! pick up actual dimensions
            nosegl = nint(pmsa(ipoint(ip_nosegl)))

            ! pick up constants
            delt = dble(pmsa(ipoint(ip_delt)))

            lins  = Lastsingle
            louts = nsubs+1
            loute = nsubs
            npmsa = lins+line+louts+loute
            if (npmsa.gt.npmsamax) then
                write (*,*) 'lins = ',lins
                write (*,*) 'line = ',line
                write (*,*) 'louts = ',louts
                write (*,*) 'loute = ',loute
                write (*,*) 'npmsa = ',npmsa
                write (*,*) 'npmsamax = ',npmsamax
                call write_error_message ('PMSA admin array too small')
            endif
            offset_vel = lins+line+louts

            ! allocate work arrays
            allocate(frac2rec(nrec))
            allocate(totflxin(nsubs,nrec))
            allocate(emisw(nosegl))

            ! prepare for output
            open (newunit = lu_bin, file=filbin,access = 'stream')
            open (newunit = lu_txt, file=filtxt)
            write (lu_txt,'(''Emission metadata'')')
            write (lu_txt,'(''Emissions in g/s'')')
            write (lu_txt,'(''Nr of segments:     '',i10)') nosegl
            write (lu_txt,'(''Nr of layers  :              1'')')
            write (lu_txt,'(''Water layer   :              1'')')
            write (lu_txt,'(''Nr of subst   :              1'')')
            write (lu_txt,'(''Plastic       :              1'')')

      endif

!******************************************************************************* PROCESSING in TIME LOOP
      do ipmsa = 1,npmsa
        ipnt(ipmsa) = ipoint(ipmsa)
      enddo

      emisw = 0.0
      do isegl = 1 , nosegl

          totflxin = 0d0

!*******************************************************************************
! Now follows the RELEASE PART
!*******************************************************************************

          ! MPW release -----------------------------------------------------------------------------
          totsurf  = pmsa(ipnt(ip_totsurf))
          fpaved   = max(pmsa(ipnt(ip_fpaved)),1e-10) ! to avoid division by zero for RO paved conversion to mm
          funpaved = max(pmsa(ipnt(ip_funpaved)),1e-10) ! to avoid division by zero for RO unpaved conversion to mm
          fwater   = pmsa(ipnt(ip_fwater))
          mpw      = pmsa(ipnt(ip_mpw))
          mpw      = mpw*(totsurf/10000.)*1000.                    !kg/ha/d to g/d
          mpww     = pmsa(ipnt(ip_mpww))
          mpww     = mpww*(totsurf/10000.)*1000.                    !kg/ha/d to g/d
          mpws     = pmsa(ipnt(ip_mpws))
          mpws     = mpws*(totsurf/10000.)*1000.                    !kg/ha/d to g/d

          ! Releases to paved surfaces
          totmpw = mpw*fpaved + mpws*fpaved/(fpaved+funpaved)
          irec = rec_pav
          iseg = isegl + (irec-1)*nosegl
          iflux = iflrel + (iseg-1)*noflux
          flux = totmpw / 86400. ! /d to /s
          fl(iflux) = flux
          totflxin(nsubs,irec) = totflxin(nsubs,irec) + dble(flux)

          ! Releases to unpaved surfaces
          totmpw = mpw*funpaved + mpws*funpaved/(fpaved+funpaved)
          irec = rec_unp
          iseg = isegl + (irec-1)*nosegl
          iflux = iflrel + (iseg-1)*noflux
          flux = totmpw / 86400. ! /d to /s
          fl(iflux) = flux
          totflxin(nsubs,irec) = totflxin(nsubs,irec) + dble(flux)

          ! Releases to water
          totmpw = mpw*fwater + mpww
          irec = rec_sfw
          iseg = isegl + (irec-1)*nosegl
          iflux = iflrel + (iseg-1)*noflux
          flux = totmpw / 86400. ! /d to /s
          fl(iflux) = flux
          totflxin(nsubs,irec) = totflxin(nsubs,irec) + dble(flux)


!*******************************************************************************
! Now follows the ROUTING PART
!*******************************************************************************

          ! PAVED SYSTEM -------- ----------------------------------------------

          iseg = isegl + (rec_pav-1)*nosegl
          call evaluate_waq_attribute(1,iknmrk(iseg),iatt1) ! pick up first attribute
          if (iatt1.gt.0) then

          ropaved = max(pmsa(ipnt(ip_ropaved)),0.0)
          lotpav = pmsa(ipnt(ip_lotpav))
          hitpav = pmsa(ipnt(ip_hitpav))
          !               m3/s  m2
          ro_mmperday = ropaved / (totsurf*fpaved) * 1000. * 86400.
          ! save this quantity as output
          ip = lins + line + nsubs + 1
          pmsa(ipoint(ip)+increm(ip)*(iseg-1)) = ro_mmperday
          ! calculate fraction removed by runoff
          fwashoff = (ro_mmperday-lotpav)/(hitpav-lotpav)
          fwashoff = max(min(fwashoff,1.0),0.0)
          ! input
          kdecpav = pmsa(ipnt(ip_kdecpav))
          ip = ip_plastic
          plastic = dble(pmsa(ipoint(ip)+(iseg-1)*increm(ip)))
          ! fluxes
          fluxloss = -dble(kdecpav) * plastic / 86400d0
          fluxbur  = 0d0
          fluxwash = (plastic / delt + totflxin(nsubs,rec_pav) - fluxloss - fluxbur)*dble(fwashoff)
          ! output velocities to move the substances
          ioq = (pav2sfw-1)*nosegl + isegl
          pmsa(ipoint(offset_vel+nsubs)+increm(offset_vel+nsubs)*(ioq-1)) = sngl(fluxwash)
          ! increase the inflow balance of the downstream compartments
          totflxin(nsubs,rec_sfw) = totflxin(nsubs,rec_sfw) + fluxwash

          ! now set the fluxes
          iflux = ifldec + (iseg-1)*noflux
          fl(iflux) = sngl(fluxloss)
          iflux = iflbur + (iseg-1)*noflux
          fl(iflux) = sngl(fluxbur)
          endif

         ! UNPAVED SYSTEM ------------------------------------------------------------------------------------

          iseg = isegl + (rec_unp-1)*nosegl
          call evaluate_waq_attribute(1,iknmrk(iseg),iatt1) ! pick up first attribute
          if (iatt1.gt.0) then

          ! rounpaved = max(pmsa(ipnt(ip_rounpaved)),0.0)  THIS IS THE RIGHT STATEMENT AFTER CORRECTION OF BIN FILE
          rounpaved = max(  pmsa(ipoint(ip_rounpaved)+(iseg-1)*increm(ip_rounpaved))   ,0.0) ! TEMP fix
          lotunp = pmsa(ipnt(ip_lotunp))
          hitunp = pmsa(ipnt(ip_hitunp))
          ro_mmperday = rounpaved / (totsurf*funpaved) * 1000. * 86400.
          ! save this quantity as output
          ip = lins + line + nsubs + 1
          pmsa(ipoint(ip)+increm(ip)*(iseg-1)) = ro_mmperday
          ! calculate fraction removed by runoff
          fwashoff = (ro_mmperday-lotunp)/(hitunp-lotunp)
          fwashoff = max(min(fwashoff,1.0),0.0)
          ! input
          kdecunp = pmsa(ipnt(ip_kdecunp))
          kbur = pmsa(ipnt(ip_kbur))
          ip = ip_plastic
          plastic = dble(pmsa(ipoint(ip)+(iseg-1)*increm(ip)))
          ! fluxes
          fluxloss = -dble(kdecunp) * plastic / 86400d0
          fluxbur  = -dble(kbur) * plastic / 86400d0
          fluxwash = (plastic / delt + totflxin(nsubs,rec_unp) - fluxloss - fluxbur)*dble(fwashoff)
          ! output velocities to move the substances
          ioq = (unp2sfw-1)*nosegl + isegl
          pmsa(ipoint(offset_vel+nsubs)+increm(offset_vel+nsubs)*(ioq-1)) = sngl(fluxwash)
          ! increase the inflow balance of the downstream compartments
          totflxin(nsubs,rec_sfw) = totflxin(nsubs,rec_sfw) + fluxwash
          ! now set the fluxes
          iflux = ifldec + (iseg-1)*noflux
          fl(iflux) = sngl(fluxloss)
          iflux = iflbur + (iseg-1)*noflux
          fl(iflux) = sngl(fluxbur)

          endif

          ! ENDPOINT SURFACE WATER

          iseg = isegl + (rec_sfw-1)*nosegl
          call evaluate_waq_attribute(1,iknmrk(iseg),iatt1) ! pick up first attribute
          if (iatt1.gt.0) then

          ! fluxes
          fluxexp = sngl(totflxin(nsubs,rec_sfw))
          ! routing
          ioq = (sfw2exp-1)*nosegl + isegl
          pmsa(ipoint(offset_vel+nsubs)+increm(offset_vel+nsubs)*(ioq-1)) = fluxexp
          ! output
          ip = lins + line + nsubs
          pmsa(ipoint(ip)+increm(ip)*(iseg-1)) = fluxexp
          emisw(isegl) = fluxexp

          endif


          do ipmsa = 1,npmsa
            ipnt(ipmsa) = ipnt(ipmsa) + increm(ipmsa)
          enddo

      enddo

      ! write output
      itime =  nint(pmsa(ipoint(ip_itime)))
      write (lu_txt,'(''Output written for relative time: '',i20)') itime
      write (lu_bin) itime,emisw

      first = .false.


      return
      end

end module m_plastc
