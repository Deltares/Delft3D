module m_espgss
      use m_evaluate_waq_attribute
      use m_write_error_message
implicit none

contains

      subroutine ESPGSS     ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )

      use m_gkwini
      use m_write_error_message
      use m_evaluate_waq_attribute

!!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'ESPGSS' :: ESPGSS
!*******************************************************************************
! Note: name has been changed to avoid conflict with process ESPACE in standard Processes Library
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
!     D-EM Generic Model
!
!     Version 0.9. Jun 2022 JvG   Optionally connect soil to sfw, protect against negative sources to unp/soil
!                                 The connection was made via a soilstore with a prescribed time constant to dampen
!                                 The relocation had to be removed, needs to be organized differently
!                                 Burial was operationalized, it immobilizes the solid part from unp to soi
!     Version 0.8. Sep 2021 JvG   Add ww as an extra receptor (index 0) for all sources
!                                 read the generic allocation of ww to the true receptors
!     Version 0.7. Feb 2021 JvG   Some optimizations:
!                                 expand to 10 type A and 10 type B sources
!                                 detect how many of these are operational
!                                 make the emission factors a space function
!     Version 0.6. Nov 2020 JvG   Generic Single Substance Version
!     Version 0.5. Mar 2020 JvG   Relocate substances due to transport in sewer systems and sludge disposal
!     Version 0.4. Dec 2019 JvG   Update to have more options for spatially variable input
!     Version 0.3. Aug 2019 JvG   Output for (catchment) water quality model added
!     Version 0.2, May 2019 JvG   First version with generic set of compartments and pathways
!     Version 0.1, May 2019 JvG   Starting point: PUB version, Stw layer removed, Sobek related output removed

!     NOTE every receptor is a substance!
!     NOTE all time is in seconds
!
!     Type    Name         I/O Description                                        Unit
!
!     support variables
      integer,parameter  :: npmsamax = 210
      integer            :: ipnt(npmsamax)    !    Local work array for the pointering
      integer            :: iseg, iflux, ip, ipp, isrc, irec, iatt1, npmsa, ipmsa, calls
      real               :: emisvar, losses, flux, ro_mmperday, ra_mmperday, roun_mmperday,&
                            in_mmperday, fwashoff, ferosion, froun, finf, fdisp, kbase
      real               :: fluxloss, conc, fluxbound, fluxunbound, fluxinf, fluxroun, fluxero, fluxwash, fluxleak, &
                            fluxexp,  fluxrem, fluxeff, fluxsld, fluxbas, fluxbur
      real               :: fluxin, limit

      ! fixed quantities
      integer,parameter   :: scu = 1
      integer,parameter   :: nrec = 6
      integer,parameter   :: rec_sew = 1
      integer,parameter   :: rec_pav = 2
      integer,parameter   :: rec_unp = 3
      integer,parameter   :: rec_stw = 4
      integer,parameter   :: rec_sfw = 5
      integer,parameter   :: rec_soi = 6
      integer,parameter   :: nopar_srca = 4 ! now includes fraction to ww as receptor 0
      integer,parameter   :: ipar_srca_ev = 1
      integer,parameter   :: ipar_srca_loc = 2
      integer,parameter   :: ipar_srca_ef = 3
      integer,parameter   :: ipar_srca_ww = 4 ! this one should always be the last one
                                              ! as we use this in looping over receptors with index 0
      integer,parameter   :: nopar_srcb = 3 ! now includes fraction to ww as receptor 0
      integer,parameter   :: ipar_srcb_ev = 1
      integer,parameter   :: ipar_srcb_ef = 2
      integer,parameter   :: ipar_srcb_ww = 3 ! this one should always be the last one

    ! PMSA admin
      integer             :: offset_ww2rec
      integer             :: offset_srca
      integer             :: offset_srcb
      integer             :: offset_conc
      integer             :: lins
      integer,parameter   :: line = 0
      integer             :: louts
      integer             :: loute

      ! Flux admin
      integer             :: fl0_atm
      integer             :: fl0_srca
      integer             :: fl0_srcb
      integer             :: fl0_rest
      integer,parameter   :: pav2sew = 1
      integer,parameter   :: pav2stw = 2
      integer,parameter   :: pav2sfw = 3
      integer,parameter   :: pav2soi = 4
      integer,parameter   :: pav2dec = 5
      integer,parameter   :: unp2sfw = 6
      integer,parameter   :: unp2soi = 7
      integer,parameter   :: unp2dec = 8
      integer,parameter   :: sew2sfw_cso = 9
      integer,parameter   :: sew2stp = 10
      integer,parameter   :: sew2sfw_eff = 11
      integer,parameter   :: sew2soi = 12
      integer,parameter   :: stw2stp = 13
      integer,parameter   :: stw2sfw = 14
      integer,parameter   :: stw2soi = 15
      integer,parameter   :: soi2so2 = 16
      integer,parameter   :: soi2dec = 17
      integer,parameter   :: so22sfw = 18
      integer,parameter   :: sfw2exp = 19
      integer,parameter   :: soi2exp = 20
      integer,parameter   :: sewreloc = 21
      integer,parameter   :: stwreloc = 22
      integer,parameter   :: nofluxrest = 22

      ! pointers to concrete items
      integer,parameter   :: ip_nsrca = 1
      integer,parameter   :: ip_nsrcb = 2
      integer,parameter   :: ip_nrecin = 3
      integer,parameter   :: ip_delt = 4
      integer,parameter   :: ip_itime = 5
      integer,parameter   :: ip_totsurf = 6
      integer,parameter   :: ip_fpaved = 7
      integer,parameter   :: ip_funpaved = 8
      integer,parameter   :: ip_fwater = 9
      integer,parameter   :: ip_rainfall = 10
      integer,parameter   :: ip_ropaved = 11
      integer,parameter   :: ip_rounpaved = 12
      integer,parameter   :: ip_infilt = 13
      integer,parameter   :: ip_totflow = 14
      integer,parameter   :: ip_fcomsew = 15
      integer,parameter   :: ip_leakage = 16
      integer,parameter   :: ip_soilthick = 17
      integer,parameter   :: ip_soilpor = 18
      integer,parameter   :: ip_locsew = 19
      integer,parameter   :: ip_recsew = 20
      integer,parameter   :: ip_locstw = 21
      integer,parameter   :: ip_recstw = 22
      integer,parameter   :: ip_kbur = 23
      integer,parameter   :: ip_kpaved = 24
      integer,parameter   :: ip_kunpaved = 25
      integer,parameter   :: ip_kdunpaved = 26
      integer,parameter   :: ip_ksoil = 27
      integer,parameter   :: ip_drydep = 28
      integer,parameter   :: ip_rainconc = 29
      integer,parameter   :: ip_eff = 30
      integer,parameter   :: ip_sld = 31
      integer,parameter   :: ip_eff_rs = 32
      integer,parameter   :: ip_sld_rs = 33
      integer,parameter   :: ip_fsewered = 34
      integer,parameter   :: ip_soilstore = 35
      integer,parameter   :: ip_ksoilstore = 36
      integer,parameter   :: lastsingle = 36

      ! input items
      integer             :: nsrca     ! # of sources type A
      integer             :: nsrcb     ! # of sources type B
      integer             :: nrecin     ! # of receptors in input
      real                :: delt       ! time step
      integer             :: itime      ! actual time  (not currently used, but still in tables
      real                :: totsurf    ! total area
      real                :: fpaved     ! fracrion paved
      real                :: funpaved   ! fraction unpaved
      real                :: fwater     ! fraction water
      real                :: rainfall   ! rainfall
      real                :: ropaved    ! runoff from paved areas
      real                :: rounpaved  ! unpaved
      real                :: infilt     ! infiltration
!      real                :: totalflow  ! actual flow
      real                :: fcomsew    ! fraction of combined sewers
      real                :: leakage    ! fraction of sewage leaking
      real                :: soilthick
      real                :: soilpor
      !integer             :: locsew     ! fraction of wastewater untreated
      !integer             :: recsew     ! fraction of wastewater treated primary
      !integer             :: locstw     ! fraction of wastewater treated secondary
      !integer             :: recstw     ! fraction of wastewater treated tertiary
      real                :: kbur    ! burial rate
      real                :: kpaved
      real                :: kunpaved
      real                :: kdunpaved
      real                :: ksoil
      real                :: ksoilstore
      real                :: drydep
      real                :: rainconc
      real                :: eff
      real                :: sld
      real                :: eff_rs
      real                :: sld_rs
      real                :: fsewered
      real                :: soilstore

      ! specific other variables

      ! work arrays
      real,allocatable    :: frac2rec(:)
      real,allocatable    :: ww2rec(:,:)
     real,allocatable    :: frac2recA(:,:,:)
      real,allocatable    :: frac2recB(:,:,:)
      real,allocatable    :: sumlocator(:)
      real,allocatable    :: locator(:,:)
      real,allocatable    :: totflxin(:) ! total losses per receptor
      real,allocatable    :: emis(:,:)

      ! constants for substances
      real,allocatable   :: emisfacA(:,:)
!      real,allocatable   :: emisfacB(:,:)
      real :: emisfacB

      ! Prelim SRO model MOVE TO TABLES!!
      real,parameter :: ro_lothr = 2.
      real,parameter :: ro_hithr = 5.
      real,parameter :: ra_lothr = 10.
      real,parameter :: ra_hithr = 20.
      real,parameter :: disp_hithr = 7.

      ! for the mapping of plants
      integer,parameter :: nplantm = 100
!      integer           :: plantcell(nplantm)
!      integer,allocatable :: sewdest(:), stwdest(:)


      ! files
      integer,parameter :: lu_bin = 1961
      integer,parameter :: lu_txt = 1962
      character*80,parameter :: filbin = 'outdata_em.bin'
      character*80,parameter :: filtxt = 'outdata_em.txt'

!     other
!      logical horizontal_relocation
      logical first
      logical ev_defined
      logical ef_defined
      logical :: soil_endpoint
      data first /.true./

      save

!
!******************************************************************************* INITIAL PROCESSING

      if (first) then
            calls = 0

            ! pick up actual dimensions
            nsrca = nint(pmsa(ipoint(ip_nsrca)))
            nsrcb = nint(pmsa(ipoint(ip_nsrcb)))
            nrecin = nint(pmsa(ipoint(ip_nrecin)))
            if (nrecin.ne.nrec) call write_error_message ('Receptors inconsistent')

            ! pick up constants
            delt = nint(pmsa(ipoint(ip_delt)))
            kbur = pmsa(ipoint(ip_kbur))
            !kpaved = pmsa(ipoint(ip_kpaved))
            !kunpaved = pmsa(ipoint(ip_kunpaved))
            kdunpaved = pmsa(ipoint(ip_kdunpaved))
            !ksoil = pmsa(ipoint(ip_ksoil))
            ksoilstore = pmsa(ipoint(ip_ksoilstore))

            ! PMSA admin
            offset_ww2rec = lastsingle
            offset_srca = offset_ww2rec + nrec                   ! data for sources type A
            offset_srcb = offset_srca + nsrca*(nopar_srca+nrec)  ! data for sources type B
            offset_conc = offset_srcb + nsrcb*(nopar_srcb+nrec)  ! Concentration
            lins  = offset_conc + nrec                   ! SUM
            louts = 2
            loute = 0
            npmsa = lins+line+louts+loute
            if (npmsa.gt.npmsamax) then
                write (*,*) 'lins = ',lins
                write (*,*) 'line = ',line
                write (*,*) 'louts = ',louts
                write (*,*) 'loute = ',loute
                write (*,*) 'npmsa = ',npmsa
                write (*,*) 'npmsamax = ',npmsamax
                write (*,*) 'noflux = ',noflux
                call write_error_message ('PMSA admin array too small')
            endif
            if (noflux.ne.(nrec*(1+nsrca+nsrcb)+nofluxrest)) then
                write (*,*) 'nrec  = ',nrec
                write (*,*) 'nsrca = ',nsrca
                write (*,*) 'nsrcb = ',nsrcb
                write (*,*) 'noseg = ',noseg
                write (*,*) 'nofluxrest = ',nofluxrest
                write (*,*) 'noflux = ',noflux
                call write_error_message ('NOFLUX has unexpected value')
            endif

            ! Fluxes Admin
            fl0_atm  = 0
            fl0_srca = fl0_atm + nrec
            fl0_srcb = fl0_srca + nsrca*nrec
            fl0_rest = fl0_srcb + nsrcb*nrec

            ! set non-time variable properties
            ! Emission factors
            allocate(emisfacA(nsrca,noseg))
!            allocate(emisfacB(nsrcb,noseg))
            do isrc = 1,nsrca
                ip = offset_srca + (isrc-1)*(nopar_srca+nrec) + ipar_srca_ef
                ipp = ipoint(ip)
                do iseg = 1,noseg
                    emisfacA(isrc,iseg) = pmsa(ipp)
                    ipp = ipp + increm(ip)
                enddo
            enddo
!            do isrc = 1,nsrcb
!                ip = offset_srcb + (isrc-1)*(nopar_srcb+nrec) + ipar_srcb_ef
!                ipp = ipoint(ip)
!                do iseg = 1,noseg
!                    emisfacB(isrc,iseg) = pmsa(ipp)
!                    ipp = ipp + increm(ip)
!                enddo
!            enddo

            ! collect pre-calculated space dependent allocation of wastewater to receptors
            allocate(ww2rec(nrec,noseg))
            ip = offset_ww2rec
            do irec = 1,nrec
              ip = ip + 1
              ipp = ipoint(ip)
              do iseg = 1 , noseg
                ww2rec(irec,iseg) = pmsa(ipp)
                ipp = ipp + increm(ip)
              enddo
            enddo

            ! distribution over receptors
            ! NOTE receptor 0 is wastewater
            allocate(frac2rec(nrec))
            allocate(frac2recA(0:nrec,nsrca,noseg))
            allocate(frac2recB(0:nrec,nsrcb,noseg))
            do isrc = 1,nsrca
              ip = offset_srca + (isrc-1)*(nopar_srca+nrec) + nopar_srca - 1
              do irec = 0,nrec
                ip = ip + 1
                ipp = ipoint(ip)
                do iseg = 1 , noseg
                  frac2recA(irec,isrc,iseg) = pmsa(ipp)
                  ipp = ipp + increm(ip)
                enddo
              enddo
            enddo
            do isrc = 1,nsrcb
              ip = offset_srcb + (isrc-1)*(nopar_srcb+nrec) + nopar_srcb - 1
              do irec = 0,nrec
                ip = ip + 1
                ipp = ipoint(ip)
                do iseg = 1 , noseg
                  frac2recB(irec,isrc,iseg) = pmsa(ipp)
                  ipp = ipp + increm(ip)
                enddo
              enddo
            enddo

            ! redistribution of wastewater (receptor 0) using ww2rec
            do iseg = 1,noseg
              do isrc = 1,nsrca
                do irec=1,nrec
                    frac2recA(irec,isrc,iseg) = frac2recA(irec,isrc,iseg) + frac2recA(0,isrc,iseg) * ww2rec(irec,iseg)
                enddo
              enddo
              do isrc = 1,nsrcb
                do irec=1,nrec
                    frac2recB(irec,isrc,iseg) = frac2recB(irec,isrc,iseg) + frac2recB(0,isrc,iseg) * ww2rec(irec,iseg)
                enddo
              enddo
            enddo

            ! prepare distribution according to locators of type A sources (CONSTANT IN TIME)
            allocate(totflxin(nrec))
            allocate(emis(2,noseg))

            ! loop over sources types A to store values of and calculate the sum of locators
            allocate (locator(nsrca,noseg))
            allocate (sumlocator(nsrca))
            sumlocator = 0.0
            do isrc = 1,nsrca
                ip = offset_srca + (isrc-1)*(nopar_srca+nrec) + ipar_srca_loc
                ipp = ipoint(ip)
                do iseg = 1,noseg
                    locator(isrc,iseg) = pmsa(ipp)
                    sumlocator(isrc) = sumlocator(isrc) + locator(isrc,iseg)
                    ipp = ipp + increm(ip)
                enddo
            enddo

!           check if soil is endpoint, if not baseflow will be assumed equal to infiltration
            soilthick = pmsa(ipoint(ip_soilthick))
            if (soilthick .gt. 1000.) then
                soil_endpoint = .true.
            else
                soil_endpoint = .false.
            endif


            ! prepare for output
            open (lu_bin,file=filbin,form='binary')
            open (lu_txt,file=filtxt)
            write (lu_txt,'(''Emission metadata'')')
            write (lu_txt,'(''Emissions in g/s'')')
            write (lu_txt,'(''Nr of segments:     '',i10)') noseg
            write (lu_txt,'(''Nr of layers  :              2'')')
            write (lu_txt,'(''Water layer   :              1'')')
            write (lu_txt,'(''Soil  layer   :              2'')')
            write (lu_txt,'(''Nr of subst   :              1'')')
            write (lu_txt,'(''Unknown       :              1'')')
      endif

!******************************************************************************* PROCESSING in TIME LOOP
      calls = calls + 1
      do ipmsa = 1,npmsa
        ipnt(ipmsa) = ipoint(ipmsa)
      enddo

      emis = 0.0
      do iseg = 1 , noseg

          call evaluate_waq_attribute(1,iknmrk(iseg),iatt1) ! pick up first attribute
          if (iatt1.gt.0) then

          totflxin = 0.0

!*******************************************************************************
! Now follows the RELEASE PART
!*******************************************************************************

          ! Type A sources -------------------------------------------------------------
          iflux = fl0_srca
          do isrc = 1,nsrca

              ! pick up (total domain) EV and calculate domain losses per substance
              ip = offset_srca + (isrc-1)*(nopar_srca+nrec) + ipar_srca_ev
              emisvar = pmsa(ipoint(ip))
              losses = emisvar*emisfacA(isrc,iseg)*1000.   ! kg/d to g/d

              ! losses per sc
              if (sumlocator(isrc).gt.0.0) then
                do irec = 1,nrec
                    flux = losses*frac2recA(irec,isrc,iseg)*locator(isrc,iseg)/sumlocator(isrc) / 86400.
                    if (flux.lt.0.0) then ! extraction is prescribed, limit to store plus earlier sources
                        ip = offset_conc + irec
                        conc = pmsa(ipnt(ip))
                        limit = conc / delt + totflxin(irec)  ! pool plus earlier sources
                        flux = max(flux,-limit)
                    endif
                    iflux = iflux + 1
                    fl(iflux+(iseg-1)*noflux) = flux
                    totflxin(irec) = totflxin(irec) + flux
                enddo
              endif
          enddo

          ! Type B sources ------------------------------------------------------------
          iflux = fl0_srcb
          do isrc = 1,nsrcb

              ! losses
              ip = offset_srcb + (isrc-1)*(nopar_srcb+nrec) + ipar_srcb_ev
              emisvar = pmsa(ipnt(ip))
              ip = offset_srcb + (isrc-1)*(nopar_srcb+nrec) + ipar_srcb_ef
              emisfacB = pmsa(ipnt(ip))
              losses = emisvar*emisfacB*1000. ! kg/d to g/d

              ! fluxes
              do irec = 1,nrec
                  iflux = iflux + 1
                  flux = losses*frac2recB(irec,isrc,iseg) / 86400.
                  if (flux.lt.0.0) then ! extraction is prescribed, limit to store plus earlier sources
                        ip = offset_conc + irec
                        conc = pmsa(ipnt(ip))
                        limit = conc / delt + totflxin(irec)  ! pool plus earlier sources
                        flux = max(flux,-limit)
                  endif
                  fl(iflux+(iseg-1)*noflux) = flux
                  totflxin(irec) = totflxin(irec) + flux
              enddo
          enddo

          ! Atmospheric deposition ------------------------------------------------------------------
          totsurf  = max(1.0,pmsa(ipnt(ip_totsurf)))
          fpaved   = max(0.001,pmsa(ipnt(ip_fpaved)))
          funpaved = max(0.001,pmsa(ipnt(ip_funpaved)))
          fwater   = pmsa(ipnt(ip_fwater))
          rainfall = pmsa(ipnt(ip_rainfall)) *86400.    ! m3/s to m3/d
          drydep = pmsa(ipnt(ip_drydep))
          rainconc = pmsa(ipnt(ip_rainconc))
          frac2rec = 0.0  ! default
          frac2rec(rec_pav) = fpaved
          frac2rec(rec_unp) = funpaved
          frac2rec(rec_sfw) = fwater

          ! total dep
          losses = drydep*totsurf + rainfall*rainconc ! g/d

          ! fluxes
          iflux = fl0_atm
          do irec = 1,nrec
              iflux = iflux + 1
              flux = losses*frac2rec(irec) / 86400. ! /d to /s
              fl(iflux+(iseg-1)*noflux) = flux
              totflxin(irec) = totflxin(irec) + flux
          enddo

!*******************************************************************************
! Now follows the ROUTING PART (WITHIN COLUMN ONLY)
!*******************************************************************************

          kpaved = pmsa(ipnt(ip_kpaved))
          kunpaved = pmsa(ipnt(ip_kunpaved))
          ksoil = pmsa(ipnt(ip_ksoil))

            ! PAVED SYSTEM --------
          ! discharge to sew OR stw depending on fraction of combined sewers
          ! TODO move parameters to input tables and pmsa array

          ropaved = pmsa(ipnt(ip_ropaved))
          fcomsew = pmsa(ipnt(ip_fcomsew))
          fsewered = pmsa(ipnt(ip_fsewered))
          !               m3/s  m2
          ro_mmperday = ropaved / (totsurf*fpaved) * 1000. * 86400.
          fwashoff = (ro_mmperday-ro_lothr)/(ro_hithr-ro_lothr)
          fwashoff = max(min(fwashoff,1.0),0.0)

          ! input
          ip = offset_conc + rec_pav
          conc = pmsa(ipnt(ip))
          ! fluxes
          fluxloss = kpaved * conc / 86400.
          fluxwash = (conc / delt + totflxin(rec_pav) - fluxloss)*fwashoff
          !  to mixed sewers
          iflux = fl0_rest+pav2sew + (iseg-1)*noflux
          fl(iflux) = fluxwash*fsewered*fcomsew
          ! and next to separated sewers
          iflux = fl0_rest+pav2stw + (iseg-1)*noflux
          fl(iflux) = fluxwash*fsewered*(1.-fcomsew)
          ! to surface waters
          iflux = fl0_rest+pav2sfw + (iseg-1)*noflux
          fl(iflux) = fluxwash*(1.-fsewered)*fwater
          ! to soils
          iflux = fl0_rest+pav2soi + (iseg-1)*noflux
          fl(iflux) = fluxwash*(1.-fsewered)*(1.-fwater)
          ! now set the decay flux
          iflux = fl0_rest+pav2dec + (iseg-1)*noflux
          fl(iflux) = fluxloss
          ! update admin for downstream segments
          totflxin(rec_sew) = totflxin(rec_sew) + fluxwash*fsewered*fcomsew
          totflxin(rec_stw) = totflxin(rec_stw) + fluxwash*fsewered*(1.-fcomsew)
          totflxin(rec_sfw) = totflxin(rec_sfw) + fluxwash*(1.-fsewered)*fwater
          totflxin(rec_soi) = totflxin(rec_soi) + fluxwash*(1.-fsewered)*(1.-fwater)

         ! UNPAVED SYSTEM ------------------------------------------------------------------------------------
         ! TODO: implement an export to soils by both infiltration and burial, so that there will be no strong accumulation

          rounpaved = pmsa(ipnt(ip_rounpaved))
          roun_mmperday = rounpaved / (totsurf*funpaved) * 1000. * 86400.
          infilt = pmsa(ipnt(ip_infilt))
          in_mmperday = infilt / totsurf * 1000. * 86400.
          ra_mmperday = rainfall / totsurf * 1000.           ! already in m3/d
          ferosion = (ra_mmperday-ra_lothr)/(ra_hithr-ra_lothr)
          ferosion = max(min(ferosion,1.0),0.0)
          fdisp = (roun_mmperday + in_mmperday)/disp_hithr
          fdisp = max(min(fdisp,1.0),0.0)
          froun = roun_mmperday / (roun_mmperday + in_mmperday)
          froun = max(min(froun,1.0),0.0)
          finf = in_mmperday / (roun_mmperday + in_mmperday)
          finf = max(min(finf,1.0),0.0)

          ! input
          ip = offset_conc + rec_unp
          conc = pmsa(ipnt(ip))

          ! fluxes
          fluxloss = kunpaved * conc / 86400.
          fluxbur = kbur * conc / 86400.
          fluxbound = kdunpaved*(conc / delt + totflxin(rec_unp) - fluxloss - fluxbur) ! Unbound substance  flux
          fluxunbound = (1-kdunpaved) * (conc / delt + totflxin(rec_unp) - fluxloss -fluxbur) ! Bound substance  flux
          fluxero = fluxbound * ferosion
          fluxinf = fluxunbound * fdisp * finf
          fluxroun = fluxunbound * fdisp * froun

          iflux = fl0_rest + unp2sfw + (iseg-1)*noflux
          fl(iflux) =  fluxero + fluxroun
          iflux = fl0_rest + unp2soi + (iseg-1)*noflux
          fl(iflux) = fluxinf + fluxbur
          iflux = fl0_rest + unp2dec + (iseg-1)*noflux
          fl(iflux) = fluxloss
          ! to downstream
          totflxin(rec_sfw) = totflxin(rec_sfw) + fluxero + fluxroun
          totflxin(rec_soi) = totflxin(rec_soi) + fluxinf + fluxbur

          ! COMBINED SEWERS ---------------------------------------------------------------------------------
          ! (local leakage) (or CSO): postive is a %, negative a rainfall threshold

          ! input
          eff = pmsa(ipnt(ip_eff))
          sld = pmsa(ipnt(ip_sld))
          leakage = pmsa(ipnt(ip_leakage))
          if (leakage.lt.-0.1) then
              if (ra_mmperday.gt.(-leakage)) then   ! threshold on the mm per day value
                  leakage = 1.0
              else
                  leakage  = 0.0
              endif
          endif

          ! fluxes
          fluxleak =      leakage  * totflxin(rec_sew)
          fluxin   =  (1.-leakage) * totflxin(rec_sew)
          fluxrem  = fluxin * (1.-eff-sld)
          fluxeff  = fluxin *     eff
          fluxsld  = fluxin *         sld

          ! output
          iflux = fl0_rest + sew2sfw_cso + (iseg-1)*noflux
          fl(iflux) = fluxleak
          iflux = fl0_rest + sew2stp + (iseg-1)*noflux
          fl(iflux) = fluxrem
          iflux = fl0_rest + sew2sfw_eff + (iseg-1)*noflux
          fl(iflux) = fluxeff
          iflux = fl0_rest + sew2soi + (iseg-1)*noflux
          fl(iflux) = fluxsld

          ! to downstream
          totflxin(rec_sfw) = totflxin(rec_sfw) + fluxleak + fluxeff
          totflxin(rec_soi) = totflxin(rec_soi) + fluxsld

          ! SEPARATED SEWERS --------------------------------------------------------------------------------

          ! input
          eff_rs = pmsa(ipnt(ip_eff_rs))
          sld_rs = pmsa(ipnt(ip_sld_rs))

          ! fluxes
          fluxin =  totflxin(rec_stw)
          fluxrem  = fluxin * (1.-eff_rs-sld_rs)
          fluxeff  = fluxin *     eff_rs
          fluxsld  = fluxin *            sld_rs

          ! output
          iflux = fl0_rest + stw2stp + (iseg-1)*noflux
          fl(iflux) = fluxrem
          iflux = fl0_rest + stw2sfw + (iseg-1)*noflux
          fl(iflux) = fluxeff
          iflux = fl0_rest + stw2soi + (iseg-1)*noflux
          fl(iflux) = fluxsld

          ! to downstream
          totflxin(rec_sfw) = totflxin(rec_sfw) + fluxeff
          totflxin(rec_soi) = totflxin(rec_soi) + fluxsld

          ! SOILS -------------------------------------------

          if (soil_endpoint) then

              fluxexp = totflxin(rec_soi)
              fluxbas = 0.0
              fluxloss  = 0.0

          else

            ! input
            soilthick = pmsa(ipnt(ip_soilthick))
            soilpor = pmsa(ipnt(ip_soilpor))
            ! in_mmperday was calculated above
            ! kdunpaved was set above
            ! 1/d     mm/d         m                mm/m
            kbase = in_mmperday/(soilthick*soilpor*1000.)*(1.-kdunpaved)  ! only solute fraction
            ip = offset_conc + rec_soi
            conc = pmsa(ipnt(ip))

            ! fluxes
            fluxloss = ksoil * conc / 86400.
            fluxbas  = kbase * conc / 86400.
            fluxexp  = 0.0

          endif

          ! output
          iflux = fl0_rest + soi2dec + (iseg-1)*noflux
          fl(iflux) =  fluxloss
          iflux = fl0_rest + soi2so2 + (iseg-1)*noflux
          fl(iflux) = fluxbas
          iflux = fl0_rest + soi2exp + (iseg-1)*noflux
          fl(iflux) = fluxexp

          ! to downstream in EM
          ! no special action required for base flow as the trick with TOTFLXIN isn't required
          ip = lins+line+2
          pmsa(ipnt(ip)) = fluxexp
          emis(2,iseg) = fluxexp

          ! SOIL STORE --------------------------------------

          if (.not.soil_endpoint) then

            soilstore = pmsa(ipnt(ip_soilstore))

            ! fluxes
            fluxbas  = ksoilstore * soilstore / 86400.

            ! output
            iflux = fl0_rest + so22sfw + (iseg-1)*noflux
            fl(iflux) = fluxbas

            ! to downstream
            totflxin(rec_sfw) = totflxin(rec_sfw) + fluxbas
          endif

          ! ENDPOINT SURFACE WATER --------------------------------------

          fluxexp = totflxin(rec_sfw)
          iflux = fl0_rest + sfw2exp + (iseg-1)*noflux
          fl(iflux) = fluxexp
          ! output
          ip = lins+line+1
          pmsa(ipnt(ip)) = fluxexp
          emis(1,iseg) = fluxexp

          ! END active column
          endif

          do ipmsa = 1,npmsa
            ipnt(ipmsa) = ipnt(ipmsa) + increm(ipmsa)
          enddo
      enddo

      ! write output
      itime =  nint(pmsa(ipoint(ip_itime)))
      write (lu_txt,'(''Output written for relative time: '',i20)') itime
      write (lu_bin) itime,emis

      first = .false.

      return
      end subroutine espgss
end module m_espgss
