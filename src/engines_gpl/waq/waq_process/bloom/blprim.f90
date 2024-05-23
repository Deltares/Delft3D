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
module m_blprim
    use m_waq_precision
    use m_dynrun
    use m_bvect

    implicit none

contains


    !    Store computed biomass in BIOMAS array for use in D40BLO
    !    Compute primary production and associated fluxes

    subroutine blprim (biomas, cnh4, cno3, cpo4, csio, &
            cdetn, cdetp, cco2, ctic, &
            flmora, fldetn, tstepi, extot, exalg, temp, &
            rad, depth, dayl, id, nset, &
            deat4, totnut, chltot, flprpa, fluptn, faclim, &
            uptnit, fracam, fbod5, ratgro, ratmor, algdm, &
            iseg, cgroup, lmixo, lfixn, lcarb, nutcon, &
            flxcon, noutlim, outlim, nunucom, nuecogm, &
            con2out, swblsa, totnin, totpin, totsiin)

        use m_logger_helper, only : stop_with_error
        use bloom_data_dim
        use bloom_data_size
        use bloom_data_caldynam
        use bloom_data_io
        use bloom_data_phyt
        use bloom_data_sumou
        use bloom_data_xvect

        implicit none
        !
        !     Arguments
        !
        !     Name    Type  Length   I/O  Description
        !
        real(kind = real_wp) :: biomas(nuspec)   ! Biomass (gC/m3)
        real(kind = real_wp) :: cnh4             ! Concentration NH4 (gN/m3)
        real(kind = real_wp) :: cno3             ! Concentration NO3 (gN/m3)
        real(kind = real_wp) :: cpo4             ! Concentration PO4 (gP/m3)
        real(kind = real_wp) :: csio             ! Concentration SIO (gSi/m3)
        real(kind = real_wp) :: cdetn            ! Concentration DetN (gN/m3)
        real(kind = real_wp) :: cdetp            ! Concentration DetP (gP/m3)
        real(kind = real_wp) :: cco2             ! Concentration CO2 (g/m3)
        real(kind = real_wp) :: ctic             ! Concentration TIC (gC/m3)
        real(kind = real_wp) :: flmora(nuspec)   ! Mortality fluxes (gC/m3/d)
        real(kind = real_wp) :: fldetn(4)        ! Detritus production (g/m3/d)
        real(kind = real_wp) :: tstepi           ! Time step (d)
        real(kind = real_wp) :: extot            ! Total extinction (1/m)
        real(kind = real_wp) :: exalg            ! Extinction from living algae (1/m)
        real(kind = real_wp) :: temp             ! Temperature (deg.C)
        real(kind = real_wp) :: rad              ! Radiation (J/cm2/week)
        real(kind = real_wp) :: depth            ! Depth (m)
        real(kind = real_wp) :: dayl             ! Length of light period (h)
        integer(kind = int_wp) :: id               ! Week number (1 to 52)
        integer(kind = int_wp) :: nset             ! Counter
        real(kind = real_wp) :: deat4            ! ??
        real(kind = real_wp) :: totnut(4)        ! Total C,N,P,Si in algae (g/m3)
        real(kind = real_wp) :: chltot           ! Total chlorophyl in algae (mgChl/m3)
        real(kind = real_wp) :: flprpa(nuspec)   ! Primary production fluxes (gC/m3/d)
        real(kind = real_wp) :: fluptn(10)       ! Uptake fluxes (g/m3/d)
        real(kind = real_wp) :: faclim(6)        ! Limiting factors (-)
        real(kind = real_wp) :: uptnit           ! Nitrogen uptake per day
        real(kind = real_wp) :: fracam           ! Fraction NH4 of N uptake
        real(kind = real_wp) :: fbod5            ! BOD5/BODinf in algae
        real(kind = real_wp) :: ratgro(nuecog)   ! Effective growth rate per group (1/day)
        real(kind = real_wp) :: ratmor(nuecog)   ! Effective mortality per group (1/day)
        real(kind = real_wp) :: algdm            ! Dry matter in algae (gDM/m3)
        integer(kind = int_wp) :: iseg             ! Segment number
        real(kind = real_wp) :: cgroup(nuecog)   ! Group biomass
        logical    lmixo            ! FLAG mixotrophy
        logical    lfixn            ! FLAG N fixation
        logical    lcarb            ! FLAG C limitation
        integer(kind = int_wp) :: noutlim          ! dimension of OUTLIM
        real(kind = real_wp) :: outlim(noutlim)  ! limiting factors (extended)
        integer(kind = int_wp) :: nunucom          ! max nr of nutrient constraints in DELWAQ output
        integer(kind = int_wp) :: nuecogm          ! max nr of algae groups in DELWAQ in/out
        integer(kind = int_wp) :: con2out(nunucom) ! mapping of actual nutrient constraints to DELWAQ output
        integer(kind = int_wp) :: swblsa           ! switch for BLOOM stand alone option

        integer(kind = int_wp) :: nutcon(nunucom)  ! Nutrients involved in active nutrient constraints
        integer(kind = int_wp) :: flxcon(nunucom)  ! Uptake fluxes involved in active nutrient constraints
        real(kind = real_wp) :: TotNin           ! Total nitrogen for BLOOM stand alone        (g/m3)
        real(kind = real_wp) :: TotPin           ! Total phosphorous for BLOOM stand alone     (g/m3)
        real(kind = real_wp) :: TotSIin          ! Total silicium for BLOOM stand alone        (g/m3)

        !     Local variables
        real(kind = real_wp) :: cmort            ! Additional mortality flux (gDW/m3/d)
        real(kind = dp) :: x(mt)            ! Remaining after mortality (gDW/m3)
        real(kind = dp) :: xj               ! Biomass per group (gDW/m3)
        integer(kind = int_wp) :: i                !
        integer(kind = int_wp) :: i2               !
        integer(kind = int_wp) :: ihulp            !
        integer(kind = int_wp) :: inuco            !
        integer(kind = int_wp) :: j                !
        integer(kind = int_wp) :: k                !
        integer(kind = int_wp) :: ierror           ! Present number of mass errors
        real(kind = dp) :: extot8           ! Real version of input parameter
        real(kind = dp) :: exbac8           ! ..
        real(kind = dp) :: temp8            ! ..
        real(kind = dp) :: rad8             ! ..
        real(kind = dp) :: depth8           ! ..
        real(kind = dp) :: dayl8            ! ..
        real(kind = dp) :: extlim           ! ??
        real(kind = dp) :: deat             ! ??
        real(kind = dp) :: totchl           ! Real version of output parameter
        real(kind = dp) :: totdry           ! Real version of output parameter
        real(kind = dp) :: totcar           ! Real version of output parameter
        real(kind = real_wp) :: uptake           ! Nitrogen uptake (gN/m3/d)
        real(kind = real_wp) :: frmixx           ! Fraction of mixotrophy in production

        real(kind = dp) :: auto(3)
        real(kind = dp) :: autnut
        real(kind = real_wp) :: frmixn
        real(kind = real_wp) :: frmixp
        real(kind = real_wp) :: frmix

        real(kind = dp), parameter :: fixinf = 1.0d+03
        integer(kind = int_wp), parameter :: merror = 100           ! Maximum number of mass errors

        save ierror
        data ierror /0/

        ! Increase BLOOM II indicator for the number of runs.
        nrep = nrep + 1

        !  Transfer actual time step to Bloom (through common variable TSTEP)
        tstep = dble(tstepi)

        !  Compute totals per species group (XINIT), limit to BIOBAS
        do j = 1, nuecog
            xj = 0d0
            do i = it2(j, 1), it2(j, 2)
                xj = xj + dble(biomas(i)) * ctodry(i)
            enddo
            xinit(j) = max(xj, biobas)
        enddo

        !  Compute available nutrients (CONCEN) start with nutrients outside algae pool (dissolved , detritus)
        concen (1) = dble(cno3 + cnh4)
        concen (2) = dble(cpo4)
        concen (3) = dble(csio)
        inuco = 3
        if (lcarb) then
            concen (inuco + 1) = dble(ctic)          ! Decide if this should be tic
            inuco = inuco + 1
        endif
        !     Mixotrophy
        if (lmixo) then
            concen(inuco + 1) = dble(cdetn)
            concen(inuco + 2) = dble(cdetp)
            inuco = inuco + 2
        endif
        !     N-Fixation
        if (lfixn) then
            concen(inuco + 1) = fixinf
            inuco = inuco + 1
        endif
        auto = 0.d0

        ! Check whether biomass minus mortality is reasonably large.
        ! If the value has dropped below TOPLEV, then set the biomass to
        ! zero and correct the detritus fluxes.
        ! Otherwise add nutrients in live phytoplankton to CONCEN.
        !
        ! Loop over algae species
        do j = 1, nuspec
            ! compute remaining biomass after mortality
            if (biomas (j) < 0.0) then
                x (j) = biomas (j)
            else
                x(j) = dble(biomas(j) - tstepi * flmora(j)) * ctodry(j)
                !  Add nutrients in autolyses to concen and auto
                do k = 1, nunuco
                    i = nutcon(k)
                    autnut = dble(tstepi * flmora(j)) * ctodry(j) * (1.d0 - availn(j)) * aa(k, j)
                    if (i<=3) then
                        auto(i) = auto(i) + autnut                   ! only for N,P,Si
                        concen(i) = concen(i) + autnut
                    endif
                enddo

                ! Negative biomass after mortality? Message!
                if (x(j) < -1.0d-2) then
                    ierror = ierror + 1
                    write (outdbg, 1050) ierror, j, biomas(j), iseg, x(j)
                    if (ierror == merror) then
                        write (outdbg, *) 'Fatal ERROR in Bloom module: time step too big'
                        write (*, *) 'Fatal ERROR in Bloom module: time step too big'
                        call stop_with_error()
                    end if
                end if
                1050    format (' Integration error number ', I3, /, &
                        ' Current biomass of type ', I3, ' = ', E15.5, /, &
                        ' in segment', I8, /, &
                        ' Remaining after mortality = ', E15.5, /, &
                        ' Serious error: time step is too big to model', &
                        ' mortality')

                if (x(j) < toplev) then
                    !  Set small biomasses to zero, putting mass into the detritus pools,
                    !  by increasing the mortality and detritus production fluxes
                    cmort = real(x(j)) / tstepi
                    flmora(j) = flmora(j) + cmort / real(ctodry(j))
                    fldetn(1) = fldetn(1) + cmort / real(ctodry(j))
                    do k = 1, nunuco
                        i = nutcon(k)
                        if (i<=3)                          & ! only N,P,Si
                                fldetn(i + 1) = fldetn(i + 1) + cmort * real(aa(k, j))
                    enddo
                    !  Biomass set to zero, for BLOOM to make proper mortality constraint
                    !  and for correct calculation of production fluxes afterwards
                    !  (JvG, June 2006)
                    x(j) = 0.0d0
                endif
                !  End of code for positive biomass
            endif

            !  Add nutrients in live phytoplankton to CONCEN.
            !  Contrary to earlier versions ALL cases pass this
            !  code, because X(J) is used afterwards to calculate
            !  production flux, the same value needs to be given to
            !  BLOOM as available nutrients in CONCEN
            !  (JvG, June 2006)
            do k = 1, nunuco
                i = nutcon(k)
                concen(i) = concen(i) + aa(k, j) * x(j)
            enddo

            !  End of loop over algae species
        enddo

        !   In case of running in stand alone modus (SWBLSA=1) a steady state
        !   is calculated on basis of prescribed total nutrients and
        !   detritus as based on the previous time step (TT dec 2015).
        if (swblsa==1) then
            concen (1) = dble(totnin - cdetn)
            concen (2) = dble(totpin - cdetp)
            concen (3) = dble(totsiin)
        endif

        ! Call BVECT to set the mortality constraints.
        call bvect (x, xdef)

        !  Call DYNRUN. This routine calls the actual BLOOM II module
        extot8 = dble(extot)
        exbac8 = dble(extot - exalg)
        temp8 = dble(temp)
        rad8 = dble(rad)
        depth8 = dble(depth)
        dayl8 = dble(dayl)
        extlim = 0d0
        deat = dble(deat4)

        call dynrun(extot8, exbac8, temp8, rad8, depth8, dayl8, id, iseg, nset, extlim, deat, totchl, totdry, totcar, swblsa)

        ! Store total carbon and chlorophyll concentration
        totnut(1) = real(totcar)
        chltot = real(totchl)
        algdm = real(totdry)

        do i = 2, 4
            totnut(i) = 0.0
        enddo
        do i = 1, 9
            fluptn(i) = 0.0
        enddo
        fbod5 = 0.0

        ! Compute gross production: computed biomass by Bloom is in XDEF(NUROWS+J)
        ! Loop over algae species, compute production per species and total pr.pr.
        ! Added: computation of total nutrients
        ! Added: Calculate uptake fluxes (JvG, June 2006)

        do j = 1, nuspec
            flprpa(j) = real((xdef(j + nurows) - x(j)) / ctodry(j)) / tstepi
            if (.not.lcarb) fluptn(1) = fluptn(1) + flprpa(j)
            fbod5 = fbod5 + real(xdef(j + nurows) / ctodry(j)) * (1. - exp(-5.0 * rmort(j)))
            do k = 1, nunuco
                i = nutcon(k)
                i2 = flxcon(k)
                if (i<=3) then
                    totnut(i + 1) = totnut(i + 1) + real(xdef(j + nurows) * aa(k, j))
                endif
                fluptn(i2) = fluptn(i2) + flprpa(j) * aa(k, j) * ctodry(j)
            enddo

            ! Determine carbon uptake mixotrophy

            if (lmixo) then
                ! Fraction mixotrophy in production
                if (lcarb) then
                    frmixn = aa(5, j) / (aa(1, j) + aa(5, j))
                    frmixp = aa(6, j) / (aa(2, j) + aa(6, j))
                else
                    frmixn = aa(4, j) / (aa(1, j) + aa(4, j))
                    frmixp = aa(5, j) / (aa(2, j) + aa(5, j))
                endif
                frmix = max(frmixn, frmixp)
                fluptn(9) = fluptn(9) + frmix * flprpa(j)
                !           Uptake of DetC should be subtracted from total C-uptake
                fluptn(1) = fluptn(1) - frmix * flprpa(j)
            endif
        enddo

        if (totnut(1) > 1e-30) then
            fbod5 = fbod5 / totnut(1)
        else
            fbod5 = 1.0
        endif

        !  Correct for depletion of NH4 assume that
        !  phytoplankton first depletes ammonia (completely)

        !  If BLOOM stand alone then derive steady state surplus nutrients
        if (swblsa==1) then
            fluptn(2) = (cnh4 + auto(1)) / tstepi
            fluptn(3) = (cno3 - totnin + totnut(2) + cdetn) / tstepi
            fluptn(4) = (cpo4 - totpin + totnut(3) + cdetp) / tstepi
            fluptn(5) = (csio - totsiin + totnut(4)) / tstepi
        else
            if (xdef(1) <= cno3) then
                uptake = fluptn(2)
                fluptn(2) = (cnh4 + auto(1)) / tstepi
                fluptn(3) = uptake - fluptn(2)
            endif
        endif
        uptnit = fluptn(2) + fluptn(3)
        if (uptnit > 1e-30) then
            fracam = fluptn(2) / uptnit
        else
            fracam = 1.0
        endif

        ! Find limiting factors, this algorithm just takes first 3 (inorganic N,P,Si) and last 3 (e,gro,mor)
        ! regardless of additional nutrient constraints. This cnnects to traditional 6 limiting factors output
        ! For extended output (extra nutrient contstraints, grow/mort per group) see below

        i = 0
        do j = 1, nunuco + 3
            read (limit((2 * j - 1):(2 * j)), '(i2)') ihulp
            if ((j<=3).or.(j>nunuco)) then
                i = i + 1
                faclim(i) = real(ihulp)
            endif
        enddo

        !     Extended output for limiting factors

        call bl_isplim(noutlim, outlim, nunucom, nuecogm, con2out)
        !
        ! Loop to compute effective growth and mortality rates
        !
        do j = 1, nuecog
            ratgro(j) = 0.0
            ratmor(j) = 0.0
            cgroup(j) = 0.0
            do i = it2(j, 1), it2(j, 2)
                ratgro(j) = ratgro(j) + real(xdef(i + nurows) - x(i))
                ratmor(j) = ratmor(j) + real(ctodry(i)) * biomas(i) - real(x(i))
                biomas(i) = real(xdef(i + nurows) / ctodry(i))
                cgroup(j) = cgroup(j) + biomas(i)
            enddo
            ratgro(j) = ratgro(j) / real(xinit(j)) / tstepi
            ratmor(j) = ratmor(j) / real(xinit(j)) / tstepi
        enddo

        return

    end
    subroutine bl_isplim(noutlim, outlim, nunucom, nuecogm, con2out)

        use bloom_data_dim
        use bloom_data_matrix
        use bloom_data_phyt

        implicit none

        integer(kind = int_wp) :: noutlim, nunucom, nuecogm
        integer(kind = int_wp) :: con2out(nunucom)
        real(kind = real_wp) :: outlim(noutlim)
        integer(kind = int_wp) :: ii, icon, iconout
        !
        !     ISPLIM  holds a list of actually limiting constraint numbers.
        !             The constraint nrs are dependent of actual NUNUCO (nr of nutrient constraints)
        !             and NUECOG (nr of algae groups). A common example for NUNUCO = 3 and NUECOG = 6:
        !       Legend for limiting factors:
        !       Number   Constraint name
        !         1      NITROGEN
        !         2      PHOSPHOR
        !         3      SILICON
        !         4      KMIN         (light limitation)
        !         5      KMAX         (photo-inhibition)
        !         6                   (to be neglected)
        !         7      Growth-DIATOMS
        !               ......
        !        12      Growth-OSCILAT
        !        13      Mortal-DIATOMS
        !               ......
        !        18      Mortal-OSCILAT
        !             This needs to be converted to the DELWAQ fixed dimension output structure:
        !        1 to NUNUCOM for nutrient limitations
        !        NUNUCOM+1 and +2 for light
        !       NUNUCOM + 3 to NUNUCOM + 2 + NUECOGM for Growth limitation
        !       NUNUCOM + 2 + NUECOGM + 1 to NUNUCOM + 2 + 2*NUECOGM for mortality limitation
        !
        outlim = 0.0
        do ii = 1, mt   ! MT is dimension of ISPLIM according to blmdim.inc
            icon = isplim(ii)
            if (icon>0) then
                if (icon<=nunuco) then
                    iconout = con2out(icon) ! this array determines the mapping of the actual BLOOM constraints to the fixed array of DELWAQ, filled in BLINPU
                elseif (icon<=nunuco + 2) then
                    iconout = nunucom + (icon - nunuco)
                elseif (icon<=nunuco + 3) then
                    !                 this is the constraint NUNUCO + 3 that we can neglect
                elseif (icon<=nunuco + 3 + nuecog) then
                    iconout = nunucom + 2 + icon - (nunuco + 3)
                else
                    iconout = nunucom + 2 + nuecogm + icon - (nunuco + 3 + nuecog)
                endif
                outlim(iconout) = 1.0
            endif
        enddo

        return
    end


end module m_blprim
