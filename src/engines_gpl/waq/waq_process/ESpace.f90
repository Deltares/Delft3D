module m_espace

implicit none

contains

      subroutine ESPACE     ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
      use m_gkwini
      use m_write_error_message
      use m_evaluate_waq_attribute

!!!!!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'ESPACE' :: ESPACE
!
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
!     Final Draft of version for PUB April 2018
!     Changes:
!     double input FrSol and Kd for Unpaved removed
!     consistent use of unit kg/d for sources f all types in input
!     flexible definition of length of PMSA
!     protect against dividion by zero if SUMLOCATOR = 0

!     NOTE every receptor is a layer and that the numbering is the same
!     NOTE all time is in seconds
!
!     June 2018: connection to downstream programmes has been made:
!               one boundary file for active Stormwater segments
!               SWB functionality not further developed
!
!     Type    Name         I/O Description                                        Unit
!
!     support variables
!!    integer,parameter  :: npmsamax = 200
      integer, allocatable, save :: ipnt(:)    !    Local work array for the pointering
      integer            :: iseg, isegl, iflux, ip, isrc, irec, isubs, ioq, iatt1, npmsa, ipmsa, isc
      real               :: emisvar, emisfac, sumlocator, drydep, rainconc, flux, ro_mmperday, ra_mmperday, roun_mmperday,&
                            in_mmperday, fwashoff, ferosion, froun, finf, fdisp, fdeepinf, fgwbflow
      real               :: fluxloss, conc, fluxbound, fluxunbound, fluxinf, fluxroun, fluxero, fluxwash, fluxgwf, fluxdeepinf,&
                            fluxleak, fluxstp, kpaved, kunpaved, ksoil, kdunpaved, fluxexp
      character*20       :: itemname
      character*12       :: ddhhmmss1

      ! fixed quantities
      integer,parameter   :: scu = 1
      integer,parameter   :: nrec = 6
      integer,parameter   :: nsubs = 5
      integer,parameter   :: rec_sew = 1
      integer,parameter   :: rec_pav = 2
      integer,parameter   :: rec_unp = 3
      integer,parameter   :: rec_soi = 4
      integer,parameter   :: rec_stw = 5
      integer,parameter   :: rec_sfw = 6
      integer,parameter   :: nopar_srca = 2
      integer,parameter   :: nopar_srcb = 1
      integer,parameter   :: lu_loc = 1961
      integer,parameter   :: lu_nod = 1962
      integer,parameter   :: lu_ini = 1963
      real   ,parameter   :: qmin = 1e-10

      ! PMSA admin
      integer             :: offset_srca
      integer             :: offset_srcb
      integer             :: offset_ef
      integer             :: offset_ef_srca
      integer             :: offset_ef_srcb
      integer             :: offset_rf_srca
      integer             :: offset_rf_srcb
      integer             :: offset_decp
      integer             :: offset_decup
      integer             :: offset_decso
      integer             :: offset_kdup
      integer             :: offset_conc
      integer             :: lins
      integer,parameter   :: line = 0
      integer             :: louts
      integer             :: loute

      integer             :: offset_vel

      ! Flux admin
      integer             :: fl0_atm
      integer             :: fl0_srca
      integer             :: fl0_srcb
      integer             :: fl0_dec

      ! transport admin
      integer,parameter   :: sew2stw = 1
      integer,parameter   :: sew2stp = 2
      integer,parameter   :: pav2stw = 3
      integer,parameter   :: unp2stw = 4
      integer,parameter   :: unp2soi = 5
      integer,parameter   :: soi2stw = 6
      integer,parameter   :: stw2exp = 7
      integer,parameter   :: sfw2exp = 8
      integer,parameter   :: soi2inf = 9


      ! pointers to concrete items
      integer,parameter   :: ip_nsrca = 1
      integer,parameter   :: ip_nsrcb = 2
      integer,parameter   :: ip_nsubs = 3
      integer,parameter   :: ip_nrecin = 4
      integer,parameter   :: ip_nosegl = 5
      integer,parameter   :: ip_delt = 6
      integer,parameter   :: ip_totsurf = 7
      integer,parameter   :: ip_fpaved = 8
      integer,parameter   :: ip_funpaved = 9
      integer,parameter   :: ip_fwater = 10
      integer,parameter   :: ip_rainfall = 11
      integer,parameter   :: ip_leakage = 12
      integer,parameter   :: ip_ropaved = 13
      integer,parameter   :: ip_rounpaved = 14
      integer,parameter   :: ip_percola = 15
      integer,parameter   :: ip_gwbflow = 16
      integer,parameter   :: ip_deepinf = 17
      integer,parameter   :: ip_totflow = 18
      integer,parameter   :: ip_itime = 19
      integer,parameter   :: lastsingle = 19

      ! input items
      integer             :: nsrca     ! # of sources type A
      integer             :: nsrcb     ! # of sources type B
      integer             :: nsubsin    ! # of substances
      integer             :: nrecin     ! # of receptors in input
      integer             :: nosegl     ! # of segments per layer (horizontal schematisation elements, SCs + SWBs)
      real                :: delt       ! time step
      real                :: totsurf    ! total area
      real                :: fpaved     ! fracrion paved
      real                :: funpaved   ! fraction unpaved
      real                :: fwater     ! fraction water
      real                :: rainfall   ! rainfall
      real                :: leakage    ! fraction of sewage leaking
      real                :: ropaved    ! runoff from paved areas
      real                :: rounpaved  ! unpaved
      real                :: infilt     ! infiltration
      real                :: gwbaseflow ! groundwater flow
      real                :: deepinfilt ! deep infiltration
      integer             :: itime      ! actual time
      real                :: totalflow  ! actual flow

      ! specific other variables
      integer             :: nsc        ! # of SCs per layer
      integer             :: nswb       ! # of SWBs per layer

      ! work arrays
      real,allocatable    :: sc_losses(:,:,:,:) ! Type A static losses per SC
      real,allocatable    :: losses(:)
      real,allocatable    :: frac2rec(:)
      real,allocatable    :: locator(:)
      real,allocatable    :: totflxin(:,:) ! total losses per receptor and per substance in current SC/SWB
      real,allocatable    :: boun(:)

      ! Prelim SRO model
      real,parameter :: ro_lothr = 2.
      real,parameter :: ro_hithr = 5.
      real,parameter :: ra_lothr = 15.
      real,parameter :: ra_hithr = 65.
      real,parameter :: disp_hithr = 7.

      ! file names
      character*255      :: file_out_nodes, file_in_names, file_usefor, file_subs

!     other
      logical first
      data first /.true./

      save sc_losses, losses, frac2rec, totflxin, boun
      save first, npmsa, nsrca, nsrcb, nosegl, delt, &
            offset_srca, offset_srcb, offset_decp, offset_decup, offset_decso, offset_kdup, &
            offset_ef, offset_ef_srca, offset_ef_srcb, offset_rf_srca, offset_rf_srcb, &
            offset_conc, lins, louts, offset_vel, fl0_atm, fl0_srca, fl0_srcb, fl0_dec

!
!******************************************************************************* INITIAL PROCESSING

      if (first) then

            ! pick up actual dimensions
            nsrca = nint(pmsa(ipoint(ip_nsrca)))
            nsrcb = nint(pmsa(ipoint(ip_nsrcb)))
            nsubsin  = nint(pmsa(ipoint(ip_nsubs)))
            if (nsubsin.ne.nsubs) call write_error_message ('Substances inconsistent')
            nrecin = nint(pmsa(ipoint(ip_nrecin)))
            if (nrecin.ne.nrec) call write_error_message ('Receptors inconsistent')
            nosegl = nint(pmsa(ipoint(ip_nosegl)))

            ! pick up constants
            delt = nint(pmsa(ipoint(ip_delt)))

            ! PMSA admin
            offset_srca = lastsingle                      ! EV and locator sources type A
            offset_srcb = offset_srca + nsrca*nopar_srca  ! EV and locator sources type A
            offset_decp = offset_srcb + nsrcb*nopar_srcb  ! decay rate paved
            offset_decup = offset_decp + nsubs            ! decay rate unpaved
            offset_decso = offset_decup + nsubs           ! decay rate soil
            offset_kdup = offset_decso + nsubs            ! Kd rate unpaved
            offset_ef = offset_kdup + nsubs               ! emission factors
            offset_ef_srca = offset_ef + 2*nsubs          ! EF Type A
            offset_ef_srcb = offset_ef_srca + nsrca*nsubs ! Type B
            offset_rf_srca = offset_ef_srcb + nsrcb*nsubs ! release factors type A
            offset_rf_srcb = offset_rf_srca + nsrca*nrec  ! type B
            offset_conc = offset_rf_srcb + nsrcb*nrec     ! Concentration
            lins= offset_conc + nsubs                 ! SUM
            louts = nsubs
            loute = nsubs
            npmsa = lins+line+louts+loute

            allocate( ipnt(npmsa) )

            offset_vel = lins+line+louts

            ! Fluxes Admin
            fl0_atm  = 0
            fl0_srca = fl0_atm + nsubs
            fl0_srcb = fl0_srca + nsrca*nsubs
            fl0_dec  = fl0_srcb + nsrcb*nsubs

            ! prepare distribution according to locators of type A sources (CONSTANT IN TIME)
            allocate(sc_losses(nrec,nsubs,nosegl,nsrca))
            allocate(losses(nsubs))
            allocate(boun(nsubs))
            allocate(frac2rec(nrec))
            allocate(locator(nosegl))
            allocate(totflxin(nsubs,nrec))
            sc_losses = 0.0

            ! loop over sources types
            do isrc = 1,nsrca

                ! calculate total losses
                ip = offset_srca + (isrc-1)*nopar_srca + 1
                emisvar = pmsa(ipoint(ip))
                do isubs = 1,nsubs
                    ip = offset_ef_srca + (isrc-1)*nsubs + isubs
                    emisfac = pmsa(ipoint(ip))
                    losses(isubs) = emisvar*emisfac*1000.   ! kg/d to g/d
                enddo

                ! sum of locator values
                sumlocator = 0.0
                do isegl = 1,nosegl
                    ip = offset_srca + (isrc-1)*nopar_srca + 2
                    locator(isegl) = pmsa(ipoint(ip)+(isegl-1)*increm(ip))
                    sumlocator = sumlocator + locator(isegl)
                enddo

                ! distribution over receptors
                ip = offset_rf_srca + (isrc-1)*nrec
                do irec = 1,nrec
                    ip = ip + 1
                    frac2rec(irec) = pmsa(ipoint(ip))
                enddo

                ! losses per sc
                if (sumlocator.gt.0.0) then
                do isegl = 1,nosegl
                do isubs = 1,nsubs
                do irec = 1,nrec
                    sc_losses(irec,isubs,isegl,isrc) = losses(isubs)*frac2rec(irec)*locator(isegl)/sumlocator
                enddo
                enddo
                enddo
                endif

            enddo

            ! pick up elements from STU file
            open (lu_ini,file='espace.ini')
            call gkwini(lu_ini,'Espace','file_in_names',file_in_names)
            call gkwini(lu_ini,'Espace','file_out_nodes',file_out_nodes)
            call gkwini(lu_ini,'Espace','file_usefor',file_usefor)
            call gkwini(lu_ini,'Espace','file_subs',file_subs)

            ! Headers of output files
            open (lu_loc,file=file_in_names)
            read (lu_loc,*) nsc

            ! one file for all boundaries
            open (lu_nod,file=file_out_nodes)
            write (lu_nod,'(''ITEM ;    '',2i10)') nsc,nsubs
            do isc = 1,nsc
                read (lu_loc,*) itemname
                write (lu_nod,1001) trim(itemname)
            enddo
            write (lu_nod,'(''CONCENTRATIONS'')')
            write (lu_nod,1002) trim(file_usefor)
            write (lu_nod,'(''TIME BLOCK'')')
            write (lu_nod,'(''DATA'')')
            write (lu_nod,1002) trim(file_subs)

            read (lu_loc,*) nswb
            if (nsc+nswb.ne.nosegl) call write_error_message('NSC+NSWB=/NOSEGL')
            if (nswb.gt.0) call write_error_message('NSWB>0 not implemented')

      endif

!******************************************************************************* PROCESSING in TIME LOOP
      do ipmsa = 1,npmsa
        ipnt(ipmsa) = ipoint(ipmsa)
      enddo

      ! pick up time and copy to output
      itime = nint(pmsa(ipoint(ip_itime)))
      call ddhhmmss(itime,scu,ddhhmmss1)
      write (lu_nod,*) ddhhmmss1, ' ; ddhhmmss'

      do isegl = 1 , nosegl

          totflxin = 0.0

!*******************************************************************************
! Now follows the RELEASE PART
!*******************************************************************************

          ! Type A sources -------------------------------------------------------------
          do isrc = 1,nsrca
              do irec = 1,nrec
                  iseg = isegl + (irec-1)*nosegl
                  do isubs = 1,nsubs
                      iflux = fl0_srca + (isrc-1)*nsubs + isubs + (iseg-1)*noflux
                      flux = sc_losses(irec,isubs,isegl,isrc) / 86400.
                      fl(iflux) = flux
                      totflxin(isubs,irec) = totflxin(isubs,irec) + flux
                  enddo
              enddo
          enddo

          ! Type B sources ------------------------------------------------------------
          do isrc = 1,nsrcb

              ! losses
              ip = offset_srcb + isrc
                emisvar = pmsa(ipnt(ip))
              do isubs = 1,nsubs
                  ip = offset_ef_srcb + (isrc-1)*nsubs + isubs
                  emisfac = pmsa(ipoint(ip))
                  losses(isubs) = emisvar*emisfac*1000. ! kg/d to g/d
              enddo

              ! distribution over receptors
              ip = offset_rf_srcb + (isrc-1)*nrec
              do irec = 1,nrec
                  ip = ip + 1
                  frac2rec(irec) = pmsa(ipoint(ip))
              enddo

              ! fluxes
              do irec = 1,nrec
                  iseg = isegl + (irec-1)*nosegl
                  do isubs = 1,nsubs
                      iflux = fl0_srcb  + (isrc-1)*nsubs + isubs + (iseg-1)*noflux
                      flux = losses(isubs)*frac2rec(irec) / 86400.
                      fl(iflux) = flux
                      totflxin(isubs,irec) = totflxin(isubs,irec) + flux
                  enddo
              enddo
          enddo

          ! Atmospheric deposition ------------------------------------------------------------------
          totsurf  = pmsa(ipnt(ip_totsurf))
          fpaved   = pmsa(ipnt(ip_fpaved))
          funpaved = pmsa(ipnt(ip_funpaved))
          fwater   = pmsa(ipnt(ip_fwater))
          rainfall = pmsa(ipnt(ip_rainfall)) *86400.    ! m3/s to m3/d
          totalflow = max(qmin,pmsa(ipnt(ip_totflow)))

          ! total dep
          do isubs = 1,nsubs
              ip = offset_ef + isubs
              drydep = pmsa(ipoint(ip))
              ip = ip + nsubs
              rainconc = pmsa(ipoint(ip))
              losses(isubs) = drydep*totsurf  + rainfall*rainconc ! g/d
          enddo

          ! receptors
          frac2rec(rec_sew) = 0.0
          frac2rec(rec_pav) = fpaved
          frac2rec(rec_unp) = funpaved
          frac2rec(rec_soi) = 0.0
          frac2rec(rec_stw) = 0.0
          frac2rec(rec_sfw) = fwater

          ! fluxes
          do irec = 1,nrec
              iseg = isegl + (irec-1)*nosegl
              do isubs = 1,nsubs
                  iflux = fl0_atm + isubs + (iseg-1)*noflux
                  flux = losses(isubs)*frac2rec(irec) / 86400. ! /d to /s
                  fl(iflux) = flux
                  totflxin(isubs,irec) = totflxin(isubs,irec) + flux
              enddo
          enddo

!*******************************************************************************
! Now follows the ROUTING PART
!*******************************************************************************

          ! SEWER SYSTEM ------------------------------------------------------------------------------------

          iseg = isegl + (rec_sew-1)*nosegl
          call evaluate_waq_attribute(1,iknmrk(iseg),iatt1) ! pick up first attribute
          if (iatt1.gt.0) then

          leakage = pmsa(ipnt(ip_leakage))
          do isubs = 1,nsubs
              ! input
              ! fluxes
              fluxloss = 0.0
              fluxleak =     leakage  * (totflxin(isubs,rec_sew) - fluxloss)
              fluxstp  = (1.-leakage) * (totflxin(isubs,rec_sew) - fluxloss)
              ! output
              ioq = (sew2stw-1)*nosegl + isegl
              pmsa(ipoint(offset_vel+isubs)+increm(offset_vel+isubs)*(ioq-1)) = fluxleak
              ioq = (sew2stp-1)*nosegl + isegl
              pmsa(ipoint(offset_vel+isubs)+increm(offset_vel+isubs)*(ioq-1)) = fluxstp
              ! to downstream
              totflxin(isubs,rec_stw) = totflxin(isubs,rec_stw) + fluxleak
          enddo

          endif

          ! PAVED SYSTEM ------------------------------------------------------------------------------------

          iseg = isegl + (rec_pav-1)*nosegl
          call evaluate_waq_attribute(1,iknmrk(iseg),iatt1) ! pick up first attribute
          if (iatt1.gt.0) then

          ropaved = pmsa(ipnt(ip_ropaved))
          !               m3/s  m2
          ro_mmperday = ropaved / (totsurf*fpaved) * 1000. * 86400.
          fwashoff = (ro_mmperday-ro_lothr)/(ro_hithr-ro_lothr)
          fwashoff = max(min(fwashoff,1.0),0.0)
          do isubs = 1,nsubs
              ! input
              ip = offset_decp + isubs
              kpaved = pmsa(ipoint(ip))
              ip = offset_conc + isubs
              conc = pmsa(ipoint(ip)+(iseg-1)*increm(ip))
              ! fluxes
              fluxloss = kpaved * conc / 86400.
              fluxwash = (conc / delt + totflxin(isubs,rec_pav) - fluxloss)*fwashoff
              ! output
              ioq = (pav2stw-1)*nosegl + isegl
              pmsa(ipoint(offset_vel+isubs)+increm(offset_vel+isubs)*(ioq-1)) = fluxwash
              iflux = fl0_dec + isubs + (iseg-1)*noflux
              fl(iflux) = fluxloss
              ! to downstream
              totflxin(isubs,rec_stw) = totflxin(isubs,rec_stw) + fluxwash
          enddo

          endif

          ! UNPAVED SYSTEM ------------------------------------------------------------------------------------

          iseg = isegl + (rec_unp-1)*nosegl
          call evaluate_waq_attribute(1,iknmrk(iseg),iatt1) ! pick up first attribute
          if (iatt1.gt.0) then

          rounpaved = pmsa(ipnt(ip_rounpaved))
          roun_mmperday = rounpaved / (totsurf*funpaved) * 1000. * 86400.
          infilt = pmsa(ipnt(ip_percola))
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

          do isubs = 1,nsubs
              ! input
              ip = offset_decup + isubs
              kunpaved = pmsa(ipoint(ip))
              ip = offset_kdup + isubs
              kdunpaved = pmsa(ipoint(ip))
              ip = offset_conc + isubs
              conc = pmsa(ipoint(ip)+(iseg-1)*increm(ip))
              ! fluxes
              fluxloss = kunpaved * conc / 86400.
              fluxbound = kdunpaved *(conc / delt + totflxin(isubs,rec_unp) - fluxloss) ! Bound substance  flux
              fluxunbound = (1-kdunpaved) * (conc / delt + totflxin(isubs,rec_unp) - fluxloss) ! Unbound substance  flux
              fluxero = fluxbound * ferosion
              fluxinf = fluxunbound * fdisp * finf
              fluxroun = fluxunbound * fdisp * froun
              ioq = (unp2stw-1)*nosegl + isegl
              pmsa(ipoint(offset_vel+isubs)+increm(offset_vel+isubs)*(ioq-1)) = fluxero + fluxroun
              ioq = (unp2soi-1)*nosegl + isegl
              pmsa(ipoint(offset_vel+isubs)+increm(offset_vel+isubs)*(ioq-1)) = fluxinf
              iflux = fl0_dec + isubs + (iseg-1)*noflux
              fl(iflux) = fluxloss
              ! to downstream
              totflxin(isubs,rec_soi) = totflxin(isubs,rec_soi) + fluxinf
              totflxin(isubs,rec_stw) = totflxin(isubs,rec_stw) + fluxero + fluxroun
          enddo

          endif

          ! SOIL SYSTEM ------------------------------------------------------------------------------------

          iseg = isegl + (rec_soi-1)*nosegl
          call evaluate_waq_attribute(1,iknmrk(iseg),iatt1) ! pick up first attribute
          if (iatt1.gt.0) then

          gwbaseflow = pmsa(ipnt(ip_gwbflow))
          deepinfilt = pmsa(ipnt(ip_deepinf))
          fgwbflow = max(min(gwbaseflow,1.0),0.0)
          fdeepinf = max(min(deepinfilt,1.0),0.0)

          do isubs = 1,nsubs
              ! input
              ip = offset_decso + isubs
              ksoil = pmsa(ipoint(ip))
              ip = offset_conc + isubs
              conc = pmsa(ipoint(ip)+(iseg-1)*increm(ip))
              ! fluxes
              fluxloss = ksoil * conc / 86400.
              fluxgwf = (conc / delt + totflxin(isubs,rec_soi) - fluxloss) * fgwbflow
              fluxdeepinf = (conc / delt + totflxin(isubs,rec_soi) - fluxloss) * fdeepinf
              ioq = (soi2stw-1)*nosegl + isegl
              pmsa(ipoint(offset_vel+isubs)+increm(offset_vel+isubs)*(ioq-1)) = fluxgwf
              ioq = (soi2inf-1)*nosegl + isegl
              pmsa(ipoint(offset_vel+isubs)+increm(offset_vel+isubs)*(ioq-1)) = fluxdeepinf
              iflux = fl0_dec + isubs + (iseg-1)*noflux
              fl(iflux) = fluxloss
              ! to downstream
              totflxin(isubs,rec_stw) = totflxin(isubs,rec_stw) + fluxgwf
          enddo

          endif

          ! ENDPOINT STORM WATER

          iseg = isegl + (rec_stw-1)*nosegl
          call evaluate_waq_attribute(1,iknmrk(iseg),iatt1) ! pick up first attribute
          if (iatt1.gt.0) then

          boun = 0.0
          do isubs = 1,nsubs
              ! fluxes
              fluxexp = totflxin(isubs,rec_stw)
              ! output
              ip = lins + line + isubs
              pmsa(ipoint(ip)+increm(ip)*(iseg-1)) = fluxexp
              ioq = (stw2exp-1)*nosegl + isegl
              pmsa(ipoint(offset_vel+isubs)+increm(offset_vel+isubs)*(ioq-1)) = fluxexp
              boun(isubs) = fluxexp/totalflow
          enddo
          write (lu_nod,1003) boun

          endif

          ! ENDPOINT SURFACE WATER

          iseg = isegl + (rec_sfw-1)*nosegl
          call evaluate_waq_attribute(1,iknmrk(iseg),iatt1) ! pick up first attribute
          if (iatt1.gt.0) then

          do isubs = 1,nsubs
              ! fluxes
              fluxexp = totflxin(isubs,rec_sfw)
              ! output
              ip = lins + line + isubs
              pmsa(ipoint(ip)+increm(ip)*(iseg-1)) = fluxexp
              ioq = (sfw2exp-1)*nosegl + isegl
              pmsa(ipoint(offset_vel+isubs)+increm(offset_vel+isubs)*(ioq-1)) = fluxexp
          enddo

          endif

          do ipmsa = 1,npmsa
            ipnt(ipmsa) = ipnt(ipmsa) + increm(ipmsa)
          enddo

      enddo
      first = .false.

      return
1001  format ('''',a,'''')
1002  format ('INCLUDE ''',a,'''')
1003  format (6e15.6)
      end
      subroutine ddhhmmss(timeinscu,scu,ddhhmmss1)
      integer timeinscu,scu
      character*12 ddhhmmss1
      integer dd,hh,mm,ss,timeinseconds
      timeinseconds = timeinscu*scu
      dd = timeinseconds/86400
      timeinseconds = timeinseconds - dd*86400
      hh = timeinseconds/3600
      timeinseconds = timeinseconds - hh*3600
      mm = timeinseconds/60
      timeinseconds = timeinseconds - mm*60
      ss = timeinseconds
      write(ddhhmmss1,'(i6,3(i2.2))') dd,hh,mm,ss
      return
      end

end module m_espace
