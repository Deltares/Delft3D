module m_plastc
    use m_waq_precision

    implicit none

contains

    subroutine PLASTC     (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_logger_helper
        use m_extract_waq_attribute

        !XXXDEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'PLASTC' :: PLASTC
        !*******************************************************************************
        !
        IMPLICIT NONE
        !
        !     Type    Name         I/O Description
        !
        real(kind = real_wp) :: pmsa(*)     !I/O Process Manager System Array, window of routine to process library
        real(kind = real_wp) :: fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(*)  ! I  Array of pointers in pmsa to get and store the data
        integer(kind = int_wp) :: increm(*)  ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: noseg       ! I  Number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux      ! I  Number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
        integer(kind = int_wp) :: noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
        integer(kind = int_wp) :: noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
        integer(kind = int_wp) :: noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
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
        integer(kind = int_wp), parameter :: npmsamax = 200
        integer(kind = int_wp) :: ipnt(npmsamax)    !    Local work array for the pointering
        integer(kind = int_wp) :: iseg, isegl, iflux, ip, irec, ioq, iatt1, npmsa, ipmsa
        real(kind = real_wp) :: flux, ro_mmperday, fwashoff, fluxexp
        real(kind = dp) :: fluxloss, fluxbur, fluxwash


        ! fixed quantities
        integer(kind = int_wp), parameter :: scu = 1
        integer(kind = int_wp), parameter :: nsubs = 1
        integer(kind = int_wp), parameter :: nrec = 3
        integer(kind = int_wp), parameter :: rec_pav = 1
        integer(kind = int_wp), parameter :: rec_unp = 2
        integer(kind = int_wp), parameter :: rec_sfw = 3

        ! PMSA admin
        integer(kind = int_wp) :: lins
        integer(kind = int_wp), parameter :: line = 0
        integer(kind = int_wp) :: louts
        integer(kind = int_wp) :: loute
        integer(kind = int_wp) :: offset_vel

        ! Flux admin
        integer(kind = int_wp), parameter :: iflrel = 1
        integer(kind = int_wp), parameter :: ifldec = 2
        integer(kind = int_wp), parameter :: iflbur = 3

        ! transport admin CONFUSING, COUPLING ALWAYS TAKES INTERNALS BEFORE BOUNDARIES, DESPITE ORDER IN FLUXES DEFINITION
        integer(kind = int_wp), parameter :: pav2sfw = 1
        integer(kind = int_wp), parameter :: unp2sfw = 2
        integer(kind = int_wp), parameter :: sfw2exp = 3

        ! pointers to concrete items in PROCES.ASC
        integer(kind = int_wp), parameter :: ip_nosegl = 1
        integer(kind = int_wp), parameter :: ip_delt = 2
        integer(kind = int_wp), parameter :: ip_totsurf = 3
        integer(kind = int_wp), parameter :: ip_fpaved = 4
        integer(kind = int_wp), parameter :: ip_funpaved = 5
        integer(kind = int_wp), parameter :: ip_fwater = 6
        integer(kind = int_wp), parameter :: ip_ropaved = 7
        integer(kind = int_wp), parameter :: ip_rounpaved = 8
        integer(kind = int_wp), parameter :: ip_itime = 9
        integer(kind = int_wp), parameter :: ip_kbur = 10
        integer(kind = int_wp), parameter :: ip_kdecpav = 11
        integer(kind = int_wp), parameter :: ip_kdecunp = 12
        integer(kind = int_wp), parameter :: ip_mpw = 13
        integer(kind = int_wp), parameter :: ip_mpww = 14
        integer(kind = int_wp), parameter :: ip_mpws = 15
        integer(kind = int_wp), parameter :: ip_plastic = 16
        integer(kind = int_wp), parameter :: ip_lotpav = 17
        integer(kind = int_wp), parameter :: ip_hitpav = 18
        integer(kind = int_wp), parameter :: ip_lotunp = 19
        integer(kind = int_wp), parameter :: ip_hitunp = 20
        integer(kind = int_wp), parameter :: lastsingle = 20

        ! input items
        integer(kind = int_wp) :: nosegl     ! # of segments per layer (horizontal schematisation elements, SCs + SWBs)
        real(kind = dp) :: delt       ! time step
        real(kind = real_wp) :: totsurf    ! total area
        real(kind = real_wp) :: fpaved     ! fracrion paved
        real(kind = real_wp) :: funpaved   ! fraction unpaved
        real(kind = real_wp) :: fwater     ! fraction water
        real(kind = real_wp) :: ropaved    ! runoff from paved areas
        real(kind = real_wp) :: rounpaved  ! unpaved
        integer(kind = int_wp) :: itime      ! actual time  (not currently used, but still in tables
        real(kind = real_wp) :: kbur
        real(kind = real_wp) :: kdecpav
        real(kind = real_wp) :: kdecunp
        real(kind = real_wp) :: mpw, mpww, mpws
        real(kind = dp) :: plastic
        real(kind = real_wp) :: lotpav
        real(kind = real_wp) :: hitpav
        real(kind = real_wp) :: lotunp
        real(kind = real_wp) :: hitunp

        ! specific other variables
        real(kind = real_wp) :: totmpw

        ! work arrays
        real(kind = real_wp), allocatable :: frac2rec(:), emisw(:)
        real(kind = dp), allocatable :: totflxin(:, :) ! total losses per receptor and per substance in current SC/SWB

        ! files
        integer(kind = int_wp), save :: lu_bin
        integer(kind = int_wp), save :: lu_txt
        character(len=80), parameter :: filbin = 'plastc_em.bin'
        character(len=80), parameter :: filtxt = 'plastc_em.txt'

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

            lins = Lastsingle
            louts = nsubs + 1
            loute = nsubs
            npmsa = lins + line + louts + loute
            if (npmsa>npmsamax) then
                write (*, *) 'lins = ', lins
                write (*, *) 'line = ', line
                write (*, *) 'louts = ', louts
                write (*, *) 'loute = ', loute
                write (*, *) 'npmsa = ', npmsa
                write (*, *) 'npmsamax = ', npmsamax
                call write_error_message ('PMSA admin array too small')
            endif
            offset_vel = lins + line + louts

            ! allocate work arrays
            allocate(frac2rec(nrec))
            allocate(totflxin(nsubs, nrec))
            allocate(emisw(nosegl))

            ! prepare for output
            open (newunit = lu_bin, file = filbin, access = 'stream')
            open (newunit = lu_txt, file = filtxt)
            write (lu_txt, '(''Emission metadata'')')
            write (lu_txt, '(''Emissions in g/s'')')
            write (lu_txt, '(''Nr of segments:     '',i10)') nosegl
            write (lu_txt, '(''Nr of layers  :              1'')')
            write (lu_txt, '(''Water layer   :              1'')')
            write (lu_txt, '(''Nr of subst   :              1'')')
            write (lu_txt, '(''Plastic       :              1'')')

        endif

        !******************************************************************************* PROCESSING in TIME LOOP
        do ipmsa = 1, npmsa
            ipnt(ipmsa) = ipoint(ipmsa)
        enddo

        emisw = 0.0
        do isegl = 1, nosegl

            totflxin = 0d0

            !*******************************************************************************
            ! Now follows the RELEASE PART
            !*******************************************************************************

            ! MPW release -----------------------------------------------------------------------------
            totsurf = pmsa(ipnt(ip_totsurf))
            fpaved = max(pmsa(ipnt(ip_fpaved)), 1e-10) ! to avoid division by zero for RO paved conversion to mm
            funpaved = max(pmsa(ipnt(ip_funpaved)), 1e-10) ! to avoid division by zero for RO unpaved conversion to mm
            fwater = pmsa(ipnt(ip_fwater))
            mpw = pmsa(ipnt(ip_mpw))
            mpw = mpw * (totsurf / 10000.) * 1000.                    !kg/ha/d to g/d
            mpww = pmsa(ipnt(ip_mpww))
            mpww = mpww * (totsurf / 10000.) * 1000.                    !kg/ha/d to g/d
            mpws = pmsa(ipnt(ip_mpws))
            mpws = mpws * (totsurf / 10000.) * 1000.                    !kg/ha/d to g/d

            ! Releases to paved surfaces
            totmpw = mpw * fpaved + mpws * fpaved / (fpaved + funpaved)
            irec = rec_pav
            iseg = isegl + (irec - 1) * nosegl
            iflux = iflrel + (iseg - 1) * noflux
            flux = totmpw / 86400. ! /d to /s
            fl(iflux) = flux
            totflxin(nsubs, irec) = totflxin(nsubs, irec) + dble(flux)

            ! Releases to unpaved surfaces
            totmpw = mpw * funpaved + mpws * funpaved / (fpaved + funpaved)
            irec = rec_unp
            iseg = isegl + (irec - 1) * nosegl
            iflux = iflrel + (iseg - 1) * noflux
            flux = totmpw / 86400. ! /d to /s
            fl(iflux) = flux
            totflxin(nsubs, irec) = totflxin(nsubs, irec) + dble(flux)

            ! Releases to water
            totmpw = mpw * fwater + mpww
            irec = rec_sfw
            iseg = isegl + (irec - 1) * nosegl
            iflux = iflrel + (iseg - 1) * noflux
            flux = totmpw / 86400. ! /d to /s
            fl(iflux) = flux
            totflxin(nsubs, irec) = totflxin(nsubs, irec) + dble(flux)


            !*******************************************************************************
            ! Now follows the ROUTING PART
            !*******************************************************************************

            ! PAVED SYSTEM -------- ----------------------------------------------

            iseg = isegl + (rec_pav - 1) * nosegl
            call extract_waq_attribute(1, iknmrk(iseg), iatt1) ! pick up first attribute
            if (iatt1>0) then

                ropaved = max(pmsa(ipnt(ip_ropaved)), 0.0)
                lotpav = pmsa(ipnt(ip_lotpav))
                hitpav = pmsa(ipnt(ip_hitpav))
                !               m3/s  m2
                ro_mmperday = ropaved / (totsurf * fpaved) * 1000. * 86400.
                ! save this quantity as output
                ip = lins + line + nsubs + 1
                pmsa(ipoint(ip) + increm(ip) * (iseg - 1)) = ro_mmperday
                ! calculate fraction removed by runoff
                fwashoff = (ro_mmperday - lotpav) / (hitpav - lotpav)
                fwashoff = max(min(fwashoff, 1.0), 0.0)
                ! input
                kdecpav = pmsa(ipnt(ip_kdecpav))
                ip = ip_plastic
                plastic = dble(pmsa(ipoint(ip) + (iseg - 1) * increm(ip)))
                ! fluxes
                fluxloss = -dble(kdecpav) * plastic / 86400d0
                fluxbur = 0d0
                fluxwash = (plastic / delt + totflxin(nsubs, rec_pav) - fluxloss - fluxbur) * dble(fwashoff)
                ! output velocities to move the substances
                ioq = (pav2sfw - 1) * nosegl + isegl
                pmsa(ipoint(offset_vel + nsubs) + increm(offset_vel + nsubs) * (ioq - 1)) = real(fluxwash)
                ! increase the inflow balance of the downstream compartments
                totflxin(nsubs, rec_sfw) = totflxin(nsubs, rec_sfw) + fluxwash

                ! now set the fluxes
                iflux = ifldec + (iseg - 1) * noflux
                fl(iflux) = real(fluxloss)
                iflux = iflbur + (iseg - 1) * noflux
                fl(iflux) = real(fluxbur)
            endif

            ! UNPAVED SYSTEM ------------------------------------------------------------------------------------

            iseg = isegl + (rec_unp - 1) * nosegl
            call extract_waq_attribute(1, iknmrk(iseg), iatt1) ! pick up first attribute
            if (iatt1>0) then

                ! rounpaved = max(pmsa(ipnt(ip_rounpaved)),0.0)  THIS IS THE RIGHT STATEMENT AFTER CORRECTION OF BIN FILE
                rounpaved = max(pmsa(ipoint(ip_rounpaved) + (iseg - 1) * increm(ip_rounpaved)), 0.0) ! TEMP fix
                lotunp = pmsa(ipnt(ip_lotunp))
                hitunp = pmsa(ipnt(ip_hitunp))
                ro_mmperday = rounpaved / (totsurf * funpaved) * 1000. * 86400.
                ! save this quantity as output
                ip = lins + line + nsubs + 1
                pmsa(ipoint(ip) + increm(ip) * (iseg - 1)) = ro_mmperday
                ! calculate fraction removed by runoff
                fwashoff = (ro_mmperday - lotunp) / (hitunp - lotunp)
                fwashoff = max(min(fwashoff, 1.0), 0.0)
                ! input
                kdecunp = pmsa(ipnt(ip_kdecunp))
                kbur = pmsa(ipnt(ip_kbur))
                ip = ip_plastic
                plastic = dble(pmsa(ipoint(ip) + (iseg - 1) * increm(ip)))
                ! fluxes
                fluxloss = -dble(kdecunp) * plastic / 86400d0
                fluxbur = -dble(kbur) * plastic / 86400d0
                fluxwash = (plastic / delt + totflxin(nsubs, rec_unp) - fluxloss - fluxbur) * dble(fwashoff)
                ! output velocities to move the substances
                ioq = (unp2sfw - 1) * nosegl + isegl
                pmsa(ipoint(offset_vel + nsubs) + increm(offset_vel + nsubs) * (ioq - 1)) = real(fluxwash)
                ! increase the inflow balance of the downstream compartments
                totflxin(nsubs, rec_sfw) = totflxin(nsubs, rec_sfw) + fluxwash
                ! now set the fluxes
                iflux = ifldec + (iseg - 1) * noflux
                fl(iflux) = real(fluxloss)
                iflux = iflbur + (iseg - 1) * noflux
                fl(iflux) = real(fluxbur)

            endif

            ! ENDPOINT SURFACE WATER

            iseg = isegl + (rec_sfw - 1) * nosegl
            call extract_waq_attribute(1, iknmrk(iseg), iatt1) ! pick up first attribute
            if (iatt1>0) then

                ! fluxes
                fluxexp = real(totflxin(nsubs, rec_sfw))
                ! routing
                ioq = (sfw2exp - 1) * nosegl + isegl
                pmsa(ipoint(offset_vel + nsubs) + increm(offset_vel + nsubs) * (ioq - 1)) = fluxexp
                ! output
                ip = lins + line + nsubs
                pmsa(ipoint(ip) + increm(ip) * (iseg - 1)) = fluxexp
                emisw(isegl) = fluxexp

            endif

            do ipmsa = 1, npmsa
                ipnt(ipmsa) = ipnt(ipmsa) + increm(ipmsa)
            enddo

        enddo

        ! write output
        itime = nint(pmsa(ipoint(ip_itime)))
        write (lu_txt, '(''Output written for relative time: '',i20)') itime
        write (lu_bin) itime, emisw

        first = .false.

        return
    end

end module m_plastc
