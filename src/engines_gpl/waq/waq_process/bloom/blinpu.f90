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
module m_blinpu
    use m_waq_precision

    implicit none

contains


    !    Module to read BLOOM input files
    !
    subroutine blinpu (ntyp_m, ntyp_a, ngro_a, algtyp, lmixo, lfixn, lcarb, nunucom, nutcon, flxcon, con2out, &
            swblsolint, swblobject, bltemlim, blbasmor, swblgrochk, blbiobas, swblmorchk, bltoplev)

        use m_logger_helper, only : stop_with_error
        use m_readfrm
        use m_getidentification
        use bloom_data_dim
        use bloom_data_size
        use bloom_data_arran
        use bloom_data_mass_balance
        use bloom_data_io
        use bloom_data_matrix
        use bloom_data_phyt
        use bloom_data_putin
        use bloom_data_sumou

        implicit none

        integer(kind = int_wp) :: ntyp_m               ! Maximum number of types
        integer(kind = int_wp) :: ntyp_a               ! Actual number of types
        integer(kind = int_wp) :: ngro_a               ! Actual number of groups
        integer(kind = int_wp) :: nunucom              ! Number of constrains
        integer(kind = int_wp) :: j, k, is             ! Indexes
        real(kind = real_wp) :: algtyp(0:20, ntyp_m)  ! Characteristics per algae type
        logical      lmixo                ! Flag mixotrophy
        logical      lfixn                ! Flag N-fixation
        logical      lcarb                ! Flag carbon limitation
        integer(kind = int_wp) :: nutcon(nunucom)      ! Nutrients involved in active nutrient constraints
        integer(kind = int_wp) :: flxcon(nunucom)      ! Uptake fluxes involved in active nutrient constraints
        integer(kind = int_wp) :: con2out(nunucom)     ! Mapping of actual nutrient constraints to DELWAQ output

        !     Former D09 input
        integer(kind = int_wp) :: swblsolint           ! Switch for solar irradiation as total radiation (0) or PAR (1)
        integer(kind = int_wp) :: swblobject           ! Switch for objective growth (1) or biomass (0)
        real(kind = real_wp) :: bltemlim             ! Minimum temperature for growth
        real(kind = real_wp) :: blbasmor             ! Base mortality when temperature is below minimum temperature for growth
        integer(kind = int_wp) :: swblgrochk           ! Switch to use extra constraints on growth rates
        real(kind = real_wp) :: blbiobas             ! Base biomass level per group
        integer(kind = int_wp) :: swblmorchk           ! Switch to use extra mortality constraints
        real(kind = real_wp) :: bltoplev             ! Top level of mortality constraints

        !     Local variables
        character(120) identification_text
        integer(kind = int_wp) :: lparam, i, i1, i2
        real(kind = real_wp) :: autofr
        character(8)   cpmax

        !  From now on BLOOM won't read the D09-file any more. Some settings will be made available through the inp-file later on
        call getidentification(identification_text)
        write(outdbg, '(1x,a/)') trim(identification_text)
        write(outdbg, '(a/)') ' BLOOM debug file'

        write(outdbg, '(a)')  ' Since August 2019, revision 64400, BLOOM will not read the bloominp.d09 file any more.'
        write(outdbg, '(a)')  ' Some of the options that were available trough this file are now accessible'
        write(outdbg, '(a/)') ' through parameters settings. Consult the manual for details.'

        !  Determine nuspec and nuecog
        is = 0
        nuecog = 0
        60   is = is + 1
        if ((algtyp(0, is)>-100.).and.(is<=ntyp_m)) then
            if (is==1) then
                j = 1
                it2(1, 1) = 1
            elseif (is==ntyp_m) then
                it2(j, 2) = ntyp_m
            elseif (nint(algtyp(1, is))/=nint(algtyp(1, is - 1))) then
                it2(j, 2) = is - 1
                j = j + 1
                it2(j, 1) = is
            endif
            it2(j, 2) = is
            nuecog = max(nuecog, nint(algtyp(1, is)))
            goto 60
        endif

        nuecog = j
        nuspec = is - 1
        if ((is==ntyp_m).and.(algtyp(0, ntyp_m)>-100.)) nuspec = ntyp_m

        !  Set the algae characteristics
        lmixo = .false.
        lfixn = .false.
        do j = 1, nuecog
            grname(j)(1:1) = char(ichar('a') + j - 1)
            k = 0
            do i = it2(j, 1), it2(j, 2)
                k = k + 1
                spname(i)(1:1) = char(ichar('a') + j - 1)
                write(spname(i)(3:3), '(i1)') k
                ctodry(i) = algtyp(3, i)
                ekx(i) = algtyp(2, i) * 0.001 / ctodry(i)
                if (algtyp(16, i)>0.0) lmixo = .true.
                if (algtyp(17, i)>0.0) lmixo = .true.
                if (algtyp(18, i)>0.0) lfixn = .true.
                chltoc(i) = 1. / algtyp(7, i)
                chlr(i) = chltoc(i) * ctodry(i)
                pmax1(i) = algtyp(8, i)
                pmax2(i) = algtyp(9, i)
                if (nint(algtyp(10, i))==0) then
                    lpmax(i) = 1
                else
                    lpmax(i) = 0
                endif
                rmort1(i) = algtyp(11, i)
                rmort2(i) = algtyp(12, i)
                rmort3(i) = algtyp(20, i)
                res1(i) = algtyp(13, i)
                res2(i) = algtyp(14, i)
                sdmix(i) = algtyp(19, i)
                autofr = algtyp(15, i)
                availn(i) = dble(1.d0 - autofr)
            end do
        end do

        !     Set admin dependent on NUNUCO
        !     Note that we handle different sets of nutrient constraints
        !      - optional carbon limitation (LCARB)
        !      - mixotrophy (N,P) (LMIXO)
        !      - N-fixation (LFIXN)
        do i = 1, nuspec
            aa(1, i) = algtyp(4, i) / ctodry(i)
            aa(2, i) = algtyp(5, i) / ctodry(i)
            aa(3, i) = algtyp(6, i) / ctodry(i)
            if (lcarb) aa(4, i) = 1. / ctodry(i)
        enddo
        nutcon (1) = 1
        nutcon (2) = 2
        nutcon (3) = 3
        flxcon (1) = 2  ! NH4 uptake
        flxcon (2) = 4  ! PO4 uptake
        flxcon (3) = 5  ! Si uptake
        con2out(1) = 1
        con2out(2) = 2
        con2out(3) = 3
        cstra(1) = 'NITROGEN'
        cstra(2) = 'PHOSPHOR'
        cstra(3) = 'SILICON '
        nunuco = 3
        if (lcarb) then
            nutcon (nunuco + 1) = 4
            flxcon (nunuco + 1) = 1  ! C uptake
            con2out(nunuco + 1) = 4
            cstra(4) = 'CARBON  '
            nunuco = 4
        endif
        if (lmixo) then
            do i = 1, nuspec
                aa(nunuco + 1, i) = max(0.0d0, algtyp(16, i) / ctodry(i))
                aa(nunuco + 2, i) = max(0.0d0, algtyp(17, i) / ctodry(i))
            enddo
            cstra(nunuco + 1) = 'N-Detr'
            limnam(nunuco + 1) = 'N-D'
            cstra(nunuco + 2) = 'P-Detr'
            limnam(nunuco + 2) = 'P-D'
            nutcon (nunuco + 1) = 1
            nutcon (nunuco + 2) = 2
            flxcon (nunuco + 1) = 6  ! DetN uptake
            flxcon (nunuco + 2) = 7  ! DetP uptake
            con2out(nunuco + 1) = 5
            con2out(nunuco + 2) = 6
            nunuco = nunuco + 2
        endif
        if (lfixn) then
            do i = 1, nuspec
                aa(nunuco + 1, i) = max(0.0d0, algtyp(18, i) / ctodry(i))
            enddo
            cstra(nunuco + 1) = 'N-Fix'
            limnam(nunuco + 1) = 'N-F'
            nutcon (nunuco + 1) = 1
            flxcon (nunuco + 1) = 8  ! NFix
            con2out(nunuco + 1) = 7
            nunuco = nunuco + 1
        endif
        cstra(nunuco + 1) = 'KMIN    '
        cstra(nunuco + 2) = 'KMAX    '
        if (nunuco>nunucom) then
            write(outdbg, *) 'ERROR: Number of contraints if greater than the maximum number of constraints in BLOOM'
            write(*, *) 'ERROR: Number of contraints if greater than the maximum number of constraints in BLOOM'
            call stop_with_error()
        end if

        !  Establish various column and row indicators for A-matrix and output
        !  output vector X (or XDEF).
        !  It is assumed there are 2 energy constraints.
        !
        !       NUFILI--number of first light constaint.
        !       NUABCO--number of abiotic constaints (total).
        !       NUEXRO--number of the exclusion row in A-matrix.
        !       NUROWS--number of rows in A-matrix.
        !       NUCOLS--number of elements in X-vector.
        !       NUSPE1--position of first type in X-vector.

        nufili = nunuco + 1
        nuabco = nunuco + 2
        nuexro = nuabco + 1
        nurows = nuexro
        nuspe1 = nurows + 1
        nucols = nurows + nuspec

        ! Switch for solar irr. total rad(0) or PAR(1)
        if(swblsolint==0) then
            solaco = 0.449999988079071d0
        else
            solaco = 1.0d0
        endif

        !  Record names of limiting factors.
        do i = 1, nunuco
            limnam (i) = cstra (i) (1:3)
        end do
        limnam(nunuco + 1) = 'E  '
        limnam(nunuco + 2) = 'Gro'
        limnam(nunuco + 3) = 'Mor'

        !  Call subroutine readfrm to read species/groups names and the integrated efficiency curves
        call readfrm

        !  Abiotic constraints
        cnames = ' '
        do i = 1, nuabco
            cnames (i) = cstra (i)
        end do

        !  Growth and mortality constraints: name + group name.
        i1 = nuexro
        i2 = nuexro + nuecog
        do i = 1, nuecog
            i1 = i1 + 1
            i2 = i2 + 1
            write(cnames (i1), 30) 'Growth', grname(i)
            write(cnames (i2), 30) 'Mortal', grname(i)
        end do
        30 format (a6, '-', a8)

        !  Set various counters used in several routines of BLOOM II.
        !  NREP   = counter for number of calls to all main BLOOM II routines.
        !  NPRODU = counter for BLOOM II production routines (which are NOT
        !           used here).
        !  LPRINT = flag indicating whether normal BLOOM II output routines
        !           are called (LPRINT = 1) or not (LPRINT = 0).
        nrep = 0
        nprodu = 0
        lprint = 1

        ! Print nutrient fraction becoming detritus
        write (outdbg, '(a)') ' Nutrient fraction becoming detritus (non-autolyse fraction):'
        write (outdbg, '(a)') ' Species   (1.0-FrAutAlg)'
        do i = 1, nuspec
            write(outdbg, '(1X,A10,F6.3)') spname(i), availn(i)
        end do

        !  Print present growth characteristics.
        write (outdbg, '(a)') ' Present species growth coefficients:'
        write (outdbg, '(a)') ' Species    PPMax   TCPMx TFPMx     Mort0   TCMrt   MResp   TCRSsp  SDMix'
        do i = 1, nuspec
            if (lpmax(i) == 1) then
                cpmax = 'LINEAR  '
            else
                cpmax = 'EXPONENT'
            endif
            write(outdbg, '(1X,A10,F6.3,1X,F7.3,1X,A8,6(F7.3,1X))') spname(i), pmax1(i), pmax2(i), cpmax, rmort1(i), rmort2(i), &
                    res1(i), res2(i), sdmix(i)
        end do

        write (outdbg, '(a)') ' Present species stochiometry:'
        write (outdbg, '(a)') ' Species      EetVLAlg     NCRAlg    PCRAlg    SCRAlg   ChlAAlg    DMCAlg'
        do i = 1, nuspec
            write(outdbg, '(1X,A10,2X,D10.3,3F10.5,F10.4,4F10.5)') spname(i), ekx(i), (aa(k, i), k = 1, nunuco), chltoc(i), ctodry(i)
        end do

        ! Switch for objective growth (1) or biomass (0)
        if (swblobject == 0) then
            lobfun = 0
            write (outdbg, '(a)') ' Ojective function: maximize total biomass.'
        else
            lobfun = 1
            write (outdbg, '(a)') ' Ojective function: maximize net growth rates.'
        endif

        ! Minimum temperature for growth
        temlim = real(bltemlim, 8)
        basmor = real(blbasmor, 8)
        basmor = 1.000000000000000D-002
        write (outdbg, '(1X,"Minimum Pmax and mortality rate of ",F6.3," for temperatures less or equal to ",F4.1)') basmor, temlim

        ! Switch to use extra constraints on growth rates
        biobas = real(blbiobas, 8)
        if (swblgrochk == 0) then
            lgroch = 0
        else
            lgroch = 1
            write (outdbg, "(' Computation with extra growth rates constraints using a constant base level of ',F8.2,' ug/L'/)") biobas
            !        Change array indices for extra constraints.
            nurows = nurows + nuecog
            nuspe1 = nuspe1 + nuecog
            nucols = nucols + nuecog
        endif

        ! Switch to use extra mortality constraints
        toplev = real(bltoplev, 8)
        if(swblmorchk == 0) then
            lmorch = 0
        else
            lmorch = 1
            write (outdbg, "(' Computation with extra mortality constraints using a top level of ',F8.2,' gDW/m3'/)") toplev
            !        Change array indices for extra constraints.
            nurows = nurows + nuecog
            nuspe1 = nuspe1 + nuecog
            nucols = nucols + nuecog
        endif

        if (nuspec > mt)  then
            write(outdbg, *) 'ERROR: Number of types if greater than the maximum number of types in BLOOM'
            write(*, *) 'ERROR: Number of types if greater than the maximum number of types in BLOOM'
            call stop_with_error()
        end if

        if (nunuco > mn)  then
            write(outdbg, *) 'ERROR: Number of nutrients if greater than the maximum number of nutrients in BLOOM'
            write(*, *) 'ERROR: Number of nutrients if greater than the maximum number of nutrients in BLOOM'
            call stop_with_error()
        end if

        !     Pass actual number of groups and species to main program

        ntyp_a = nuspec
        ngro_a = nuecog

        return
    end

end module m_blinpu
