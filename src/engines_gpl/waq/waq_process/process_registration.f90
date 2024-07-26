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

module process_registration
    use m_waq_precision
    use m_logger_helper, only : stop_with_error, get_log_unit_number
    use m_protistcm
    use m_propsg
    use m_heteroagg
    use m_protistdiat
    use m_protistdiatsedi
    use m_protistgreen
    use m_protistncm
    use m_protistpfd
    use m_protistsummation
    use m_protistzoo
    use m_attout
    use m_prpagg
    use m_sumtyr
    use m_adspo4
    use m_advtra
    use m_agecart
    use m_apatit
    use m_atmdep
    use m_sedimtyr
    use m_waqsediment
    use m_bacmrt
    use m_calsed
    use m_caltem
    use m_calwav
    use m_botmin
    use m_burial
    use m_bodcod
    use m_calchz
    use m_caltau
    use m_d40blo
    use m_consbl
    use m_dayrad
    use m_ddepth
    use m_covmac
    use m_cselac
    use m_clcrad
    use m_dayl
    use m_debgrz
    use m_cascad
    use m_denwat
    use m_depave
    use m_decdet
    use m_dectra
    use m_diggin
    use m_decpc5
    use m_decbod
    use m_degmp
    use m_delwaqg
    use m_densed
    use m_extinc
    use m_dsurf
    use m_effblo
    use m_espace
    use m_grzmac
    use m_flxfrc
    use m_gemmpb
    use m_dissi
    use m_flocsd
    use m_floceq
    use m_emersi
    use m_effave
    use m_dradio
    use m_dmvol
    use m_ebuch4
    use m_dlalg
    use m_extina
    use m_dsptra
    use m_dredge_process
    use m_secchi
    use m_npps12
    use m_staqtl
    use m_somsed
    use m_nralgs
    use m_mac3du
    use m_ptewor
    use m_veg3dx
    use m_rdbalg
    use m_sedox
    use m_oxymin
    use m_harves
    use m_vivian
    use m_vbstat
    use m_sumfrc
    use m_nh3fre
    use m_satoxy
    use m_refl
    use m_radalg
    use m_veg3du
    use m_phcomp
    use m_wkcomp
    use m_sedhm
    use m_hdisp
    use m_pprlim
    use m_ulfix
    use m_vbgro
    use m_sednu2
    use m_nitrif
    use m_mpbnlm
    use m_vbmrt
    use m_stadpt
    use m_sedcom
    use m_hdispv
    use m_priron
    use m_veg2dn
    use m_temper
    use m_stageo
    use m_partmp
    use m_maxmac
    use m_nutupt
    use m_mpbnut
    use m_macnut
    use m_vervlu
    use m_totdep
    use m_sulfox
    use m_stadsc
    use m_specfe
    use m_sedsod
    use m_resant
    use m_sedim
    use m_waqmeteo
    use m_sedcar
    use m_strear
    use m_heatfl
    use m_sulpho
    use m_s12tra
    use m_nutrel
    use m_radmac
    use m_satch4
    use m_vbupt
    use m_ironox
    use m_sulfid
    use m_pripro
    use m_trase2
    use m_macrop
    use m_trcoef
    use m_ironre
    use m_staday
    use m_vbxs12
    use m_macdis
    use m_phcomb
    use m_tfalg
    use m_makpoc
    use m_tempermode
    use m_simph
    use m_resdm
    use m_posoxy
    use m_wlcwoc
    use m_salchl
    use m_hdispa
    use m_vtrans
    use m_watage
    use m_sdppro
    use m_satco2
    use m_rfpart
    use m_swoxy
    use m_resbuf
    use m_rear
    use m_spcarb
    use m_trsoxy
    use m_selfcool
    use m_sedaap
    use m_plastc
    use m_s12tim
    use m_varsal
    use m_respup
    use m_stox3d
    use m_mpbllm
    use m_sulfpr
    use m_restim
    use m_staprc
    use m_mpbtmp
    use m_varoxy
    use m_sedomv
    use m_intpol
    use m_methox
    use m_veloc
    use m_vbxsum
    use m_nlalg
    use m_ssedph
    use m_phcarb
    use m_protist_mortality_salinity, only : protist_mortality_salinity

    implicit none

    private

    public :: pronrs, procal

    integer(kind = int_wp), save :: max_processes ! Exact number of process routines

    type :: process_routine_info
        character(len = 6) :: pronam
        procedure(), pointer, nopass :: procpnt
    end type process_routine_info

    type(process_routine_info), save, allocatable :: process_routine(:)

    integer(kind = int_wp), save, allocatable :: ithand(:)

contains

    subroutine pronrs(pronam, imodul)
        !>\file
        !>       Initialise the process routine information

        !
        !     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
        !
        !     FUNCTION            : Set the pointers to the process routines
        !
        !     SUBROUTINES CALLED  : -
        !
        !     FILES               : -
        !
        !     PARAMETERS          :
        !
        !     NAME
        !     ----
        !     pronam                        Name of the process routine
        !     imodul                        Index of the routine in the registration
        !
        !     Declaration of arguments
        !
        character(len = *), intent(in) :: pronam
        integer(kind = int_wp), intent(out) :: imodul

        integer(kind = int_wp) :: i
        logical :: okay
        logical, save :: first = .true.
        !
        !   Register the process routines
        !
        if (first) then
            first = .false.

            process_routine = [ &
                    process_routine_info('DDEPTH', DDEPTH), &
                            process_routine_info('DSURF', DSURF), &
                            process_routine_info('TOTDEP', TOTDEP), &
                            process_routine_info('EMERSI', EMERSI), &
                            process_routine_info('METEO', METEO), &
                            process_routine_info('HEATFL', HEATFL), &
                            process_routine_info('DAYRAD', DAYRAD), &
                            process_routine_info('TEMPER', TEMPER), &
                            process_routine_info('VARSAL', VARSAL), &
                            process_routine_info('VELOC', VELOC), &
                            process_routine_info('RESTIM', RESTIM), &
                            process_routine_info('STOX3D', STOX3D), &
                            process_routine_info('HDISP', HDISP), &
                            process_routine_info('HDISPV', HDISPV), &
                            process_routine_info('WATAGE', WATAGE), &
                            process_routine_info('INTPOL', INTPOL), &
                            process_routine_info('CALCHZ', CALCHZ), &
                            process_routine_info('CALWAV', CALWAV), &
                            process_routine_info('CALTAU', CALTAU), &
                            process_routine_info('SIMPH', SIMPH), &
                            process_routine_info('SPCARB', SPCARB), &
                            process_routine_info('EXTINA', EXTINA), &
                            process_routine_info('EXTINC', EXTINC), &
                            process_routine_info('CLCRAD', CLCRAD), &
                            process_routine_info('DAYL', DAYL), &
                            process_routine_info('DEPAVE', DEPAVE), &
                            process_routine_info('VTRANS', VTRANS), &
                            process_routine_info('D40BLO', D40BLO), &
                            process_routine_info('PHCOMB', PHCOMB), &
                            process_routine_info('MAKPOC', MAKPOC), &
                            process_routine_info('PHCOMP', PHCOMP), &
                            process_routine_info('SEDCOM', SEDCOM), &
                            process_routine_info('WKCOMP', WKCOMP), &
                            process_routine_info('DMVOL', DMVOL), &
                            process_routine_info('BACMRT', BACMRT), &
                            process_routine_info('SATCO2', SATCO2), &
                            process_routine_info('REAR', REAR), &
                            process_routine_info('ADSPO4', ADSPO4), &
                            process_routine_info('DENSED', DENSED), &
                            process_routine_info('DENWAT', DENWAT), &
                            process_routine_info('NITRIF', NITRIF), &
                            process_routine_info('SATOXY', SATOXY), &
                            process_routine_info('VAROXY', VAROXY), &
                            process_routine_info('BOTMIN', BOTMIN), &
                            process_routine_info('BODCOD', BODCOD), &
                            process_routine_info('DECBOD', DECBOD), &
                            process_routine_info('DECPC5', DECPC5), &
                            process_routine_info('VIVIAN', VIVIAN), &
                            process_routine_info('DISSI', DISSI), &
                            process_routine_info('SEDOX', SEDOX), &
                            process_routine_info('TFALG', TFALG), &
                            process_routine_info('DLALG', DLALG), &
                            process_routine_info('NLALG', NLALG), &
                            process_routine_info('RADALG', RADALG), &
                            process_routine_info('RDBALG', RDBALG), &
                            process_routine_info('PRIPRO', PRIPRO), &
                            process_routine_info('SDPPRO', SDPPRO), &
                            process_routine_info('PPRLIM', PPRLIM), &
                            process_routine_info('NUTUPT', NUTUPT), &
                            process_routine_info('NUTREL', NUTREL), &
                            process_routine_info('NRALGS', NRALGS), &
                            process_routine_info('OXYMIN', OXYMIN), &
                            process_routine_info('CSELAC', CSELAC), &
                            process_routine_info('EBUCH4', EBUCH4), &
                            process_routine_info('SATCH4', SATCH4), &
                            process_routine_info('SULFID', SULFID), &
                            process_routine_info('SULFOX', SULFOX), &
                            process_routine_info('SULFPR', SULFPR), &
                            process_routine_info('METHOX', METHOX), &
                            process_routine_info('SPECFE', SPECFE), &
                            process_routine_info('IRONOX', IRONOX), &
                            process_routine_info('SULPHO', SULPHO), &
                            process_routine_info('IRONRE', IRONRE), &
                            process_routine_info('PRIRON', PRIRON), &
                            process_routine_info('CALSED', CALSED), &
                            process_routine_info('SEDCAR', SEDCAR), &
                            process_routine_info('SEDNU2', SEDNU2), &
                            process_routine_info('SEDSOD', SEDSOD), &
                            process_routine_info('SSEDPH', SSEDPH), &
                            process_routine_info('SOMSED', SOMSED), &
                            process_routine_info('SEDAAP', SEDAAP), &
                            process_routine_info('RESDM', RESDM), &
                            process_routine_info('BURIAL', BURIAL), &
                            process_routine_info('DIGGIN', DIGGIN), &
                            process_routine_info('ADVTRA', ADVTRA), &
                            process_routine_info('DSPTRA', DSPTRA), &
                            process_routine_info('RFPART', RFPART), &
                            process_routine_info('PARTMP', PARTMP), &
                            process_routine_info('TRASE2', TRASE2), &
                            process_routine_info('ULFIX', ULFIX), &
                            process_routine_info('CONSBL', CONSBL), &
                            process_routine_info('SWOXY', SWOXY), &
                            process_routine_info('TRCOEF', TRCOEF), &
                            process_routine_info('VERVLU', VERVLU), &
                            process_routine_info('DEGMP', DEGMP), &
                            process_routine_info('SEDHM', SEDHM), &
                            process_routine_info('SEDOMV', SEDOMV), &
                            process_routine_info('ATMDEP', ATMDEP), &
                            process_routine_info('NH3FRE', NH3FRE), &
                            process_routine_info('POSOXY', POSOXY), &
                            process_routine_info('SECCHI', SECCHI), &
                            process_routine_info('PTEWOR', PTEWOR), &
                            process_routine_info('STREAR', STREAR), &
                            process_routine_info('TRSOXY', TRSOXY), &
                            process_routine_info('APATIT', APATIT), &
                            process_routine_info('HARVES', HARVES), &
                            process_routine_info('VEG2DN', VEG2DN), &
                            process_routine_info('VBSTAT', VBSTAT), &
                            process_routine_info('VBGRO', VBGRO), &
                            process_routine_info('VBMRT', VBMRT), &
                            process_routine_info('VEG3DX', VEG3DX), &
                            process_routine_info('VBUPT', VBUPT), &
                            process_routine_info('VEG3DU', VEG3DU), &
                            process_routine_info('SALCHL', SALCHL), &
                            process_routine_info('DECDET', DECDET), &
                            process_routine_info('S12TRA', S12TRA), &
                            process_routine_info('RESANT', RESANT), &
                            process_routine_info('STADAY', STADAY), &
                            process_routine_info('STADPT', STADPT), &
                            process_routine_info('STADSC', STADSC), &
                            process_routine_info('STAGEO', STAGEO), &
                            process_routine_info('STAPRC', STAPRC), &
                            process_routine_info('STAQTL', STAQTL), &
                            process_routine_info('SUMFRC', SUMFRC), &
                            process_routine_info('FLXFRC', FLXFRC), &
                            process_routine_info('PHCARB', PHCARB), &
                            process_routine_info('HDISPA', HDISPA), &
                            process_routine_info('MAXMAC', MAXMAC), &
                            process_routine_info('COVMAC', COVMAC), &
                            process_routine_info('MACDIS', MACDIS), &
                            process_routine_info('RADMAC', RADMAC), &
                            process_routine_info('MACNUT', MACNUT), &
                            process_routine_info('MACROP', MACROP), &
                            process_routine_info('MAC3DU', MAC3DU), &
                            process_routine_info('GRZMAC', GRZMAC), &
                            process_routine_info('NPPS12', NPPS12), &
                            process_routine_info('DEBGRZ', DEBGRZ), &
                            process_routine_info('FLOCEQ', FLOCEQ), &
                            process_routine_info('DREDGE', dredge_process), &
                            process_routine_info('RESPUP', RESPUP), &
                            process_routine_info('RESBUF', RESBUF), &
                            process_routine_info('SEDIM ', SEDIM), &
                            process_routine_info('S12TIM', S12TIM), &
                            process_routine_info('REFL  ', REFL), &
                            process_routine_info('ATTOUT', ATTOUT), &
                            process_routine_info('CASCAD', CASCAD), &
                            process_routine_info('EFFBLO', EFFBLO), &
                            process_routine_info('EFFAVE', EFFAVE), &
                            process_routine_info('DECTRA', DECTRA), &
                            process_routine_info('ESPACE', ESPACE), &
                            process_routine_info('CALTEM', CALTEM), &
                            process_routine_info('PLASTC', PLASTC), &
                            process_routine_info('WLCWOC', WLCWOC), &
                            process_routine_info('HDISS', HDISS), &
                            process_routine_info('TMODE', TMODE), &
                            process_routine_info('DLWQG2', DLWQG2), &
                            process_routine_info('GEMMPB', GEMMPB), &
                            process_routine_info('MPBNUT', MPBNUT), &
                            process_routine_info('MPBTMP', MPBTMP), &
                            process_routine_info('MPBLLM', MPBLLM), &
                            process_routine_info('MPBNLM', MPBNLM), &
                            process_routine_info('VBXS12', VBXS12), &
                            process_routine_info('VBXSUM', VBXSUM), &
                            process_routine_info('PROPSG', PROPSG), &
                            process_routine_info('PRPAGG', PRPAGG), &
                            process_routine_info('HETAGG', HETAGG), &
                            process_routine_info('SEDTYR', SEDTYR), &
                            process_routine_info('SEDAGG', SEDAGG), &
                            process_routine_info('SUMTYR', SUMTYR), &
                            process_routine_info('PROPFD', PROPFD), &
                            process_routine_info('PRODIA', PRODIA), &
                            process_routine_info('PROGRE', PROGRE), &
                            process_routine_info('PRONCM', PRONCM), &
                            process_routine_info('PROSED', PROSED), &
                            process_routine_info('PROTCM', PROTCM), &
                            process_routine_info('PROZOO', PROZOO), &
                            process_routine_info('DRADIO', DRADIO), &
                            process_routine_info('PHPROT', PHPROT), &
                            process_routine_info('FLOCSD', FLOCSD), &
                            process_routine_info('AGECAR', AGECART), &
                            process_routine_info( 'PRTMRT', protist_mortality_salinity) &
                    ]

            max_processes = size(process_routine)

            allocate(ithand(max_processes))
            ithand = 0
        endif

        !
        !   Determine the index of the routine
        imodul = findloc(process_routine%pronam, pronam, 1)

    end subroutine pronrs

    subroutine procal (process_space_real, imodul, flux, ipoint, increm, &
            num_cells, noflux, iexpnt, iknmrk, num_exchanges_u_dir, &
            num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir, pronam, &
            iproc, dll_opb)
        !>\file
        !>       Calls the process modules

        !     Deltares Software Centre

        use timers

        !     parameters          :

        !     kind           function                 name          description

        real(kind = real_wp), intent(inout) :: process_space_real  (:) ! Process module status array
        integer(kind = int_wp), intent(in) :: imodul      ! Process module number
        real(kind = real_wp), intent(out) :: flux  (:) ! Process fluxes
        integer(kind = int_wp), intent(in) :: ipoint(:) ! Pointer to process data
        integer(kind = int_wp), intent(in) :: increm(:) ! Increment in pointer process data
        integer(kind = int_wp), intent(in) :: num_cells       ! Number of computational volumes
        integer(kind = int_wp), intent(in) :: noflux      ! Number of process fluxes
        integer(kind = int_wp), intent(in) :: iexpnt(:) ! Exchange pointers
        integer(kind = int_wp), intent(in) :: iknmrk(:) ! Tag array
        integer(kind = int_wp), intent(in) :: num_exchanges_u_dir        ! Number of exchanges in first direction
        integer(kind = int_wp), intent(in) :: num_exchanges_v_dir        ! Number of exchanges in second direction
        integer(kind = int_wp), intent(in) :: num_exchanges_z_dir        ! Number of exchanges in third direction
        integer(kind = int_wp), intent(in) :: num_exchanges_bottom_dir        ! Number of exchanges in the water bed
        character(10), intent(in) :: pronam      ! Name of this process
        integer(kind = int_wp), intent(in) :: iproc       ! Process number
        integer(c_intptr_t), intent(in) :: dll_opb     ! open proces library dll handle

        !  local

        integer(kind = int_wp) :: perf_function
        integer(kind = int_wp) :: lunrep
        integer(kind = int_wp) :: ierror

        !
        ! Only monitor the "standard" routines (otherwise we would have to
        ! record the process routines loaded from the open processes library)
        !
        if (timon) then
            if (imodul > 0 .and. imodul <= size(ithand)) call timstrt (pronam, ithand(imodul))
        endif

        if (imodul > 0 .and. imodul <= max_processes) then
            call process_routine(imodul)%procpnt (process_space_real, flux, ipoint, increm, num_cells, &
                    noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                    num_exchanges_z_dir, num_exchanges_bottom_dir)
        else

            !       assumed from dll

            call get_log_unit_number(lunrep)
            if (dll_opb /= 0) then
                ierror = perf_function(dll_opb, pronam, process_space_real, flux, ipoint, increm, num_cells, &
                        noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir)
                if (ierror /= 0) then
                    write(*, *) ' '
                    write(*, *) 'ERROR        : requested module not in open process library dll/so'
                    write(*, *) 'module       : ', pronam
                    write(*, *) 'dll/so handle: ', dll_opb
                    write(lunrep, *) ' '
                    write(lunrep, *) 'ERROR        : requested module not in open process library dll/so'
                    write(lunrep, *) 'module       : ', pronam
                    write(lunrep, *) 'dll/so handle: ', dll_opb
                    call stop_with_error()
                endif
            else
                write(*, *) ' '
                write(*, *) 'ERROR  : requested module not available, no open process library dll/so loaded'
                write(*, *) 'module : ', pronam
                write(lunrep, *) ' '
                write(lunrep, *) 'ERROR  : requested module not available, no open process library dll/so loaded'
                write(lunrep, *) 'module       : ', pronam
                call stop_with_error()
            endif
        endif

        if (timon) then
            if (imodul > 0 .and. imodul <= size(ithand)) call timstop (ithand(imodul))
        endif

    end subroutine procal

end module process_registration
