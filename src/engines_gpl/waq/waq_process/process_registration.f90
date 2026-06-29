!!  Copyright (C)  Stichting Deltares, 2012-2026.
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
    use m_averad
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
    use m_veg3du
    use m_phcomp
    use m_wkcomp
    use m_sedhm
    use m_hdisp
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
    use m_mpbnut
    use m_macnut
    use m_vervlu
    use m_totdep
    use m_sulfox
    use m_stadsc
    use m_stamea
    use m_stadev
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
    use m_radmac
    use m_satch4
    use m_vbupt
    use m_ironox
    use m_sulfid
	use m_dynamo
	use m_dynpro
    use m_trase2
    use m_macrop
    use m_trcoef
    use m_ironre
    use m_staday
    use m_vbxs12
    use m_macdis
    use m_phcomb
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

            ! NOTE: build the registration table element-by-element with explicit
            ! procedure-pointer assignment. nvfortran (26.3) mis-parses a bare
            ! procedure name used as a structure-constructor value for a
            ! procedure-pointer component (NVFORTRAN-S-0084), so process_routine_info
            ! array constructors cannot be used here. This form is compiler-agnostic.
            allocate(process_routine(177))
            process_routine(  1)%pronam = 'DDEPTH' ; process_routine(  1)%procpnt => DDEPTH
            process_routine(  2)%pronam = 'DSURF'  ; process_routine(  2)%procpnt => DSURF
            process_routine(  3)%pronam = 'TOTDEP' ; process_routine(  3)%procpnt => TOTDEP
            process_routine(  4)%pronam = 'EMERSI' ; process_routine(  4)%procpnt => EMERSI
            process_routine(  5)%pronam = 'METEO'  ; process_routine(  5)%procpnt => METEO
            process_routine(  6)%pronam = 'HEATFL' ; process_routine(  6)%procpnt => HEATFL
            process_routine(  7)%pronam = 'AVERAD' ; process_routine(  7)%procpnt => AVERAD
            process_routine(  8)%pronam = 'DAYRAD' ; process_routine(  8)%procpnt => DAYRAD
            process_routine(  9)%pronam = 'TEMPER' ; process_routine(  9)%procpnt => TEMPER
            process_routine( 10)%pronam = 'VELOC'  ; process_routine( 10)%procpnt => VELOC
            process_routine( 11)%pronam = 'RESTIM' ; process_routine( 11)%procpnt => RESTIM
            process_routine( 12)%pronam = 'STOX3D' ; process_routine( 12)%procpnt => STOX3D
            process_routine( 13)%pronam = 'HDISP'  ; process_routine( 13)%procpnt => HDISP
            process_routine( 14)%pronam = 'HDISPV' ; process_routine( 14)%procpnt => HDISPV
            process_routine( 15)%pronam = 'WATAGE' ; process_routine( 15)%procpnt => WATAGE
            process_routine( 16)%pronam = 'INTPOL' ; process_routine( 16)%procpnt => INTPOL
            process_routine( 17)%pronam = 'CALCHZ' ; process_routine( 17)%procpnt => CALCHZ
            process_routine( 18)%pronam = 'CALWAV' ; process_routine( 18)%procpnt => CALWAV
            process_routine( 19)%pronam = 'CALTAU' ; process_routine( 19)%procpnt => CALTAU
            process_routine( 20)%pronam = 'SIMPH'  ; process_routine( 20)%procpnt => SIMPH
            process_routine( 21)%pronam = 'SPCARB' ; process_routine( 21)%procpnt => SPCARB
            process_routine( 22)%pronam = 'EXTINA' ; process_routine( 22)%procpnt => EXTINA
            process_routine( 23)%pronam = 'EXTINC' ; process_routine( 23)%procpnt => EXTINC
            process_routine( 24)%pronam = 'CLCRAD' ; process_routine( 24)%procpnt => CLCRAD
            process_routine( 25)%pronam = 'DAYL'   ; process_routine( 25)%procpnt => DAYL
            process_routine( 26)%pronam = 'DEPAVE' ; process_routine( 26)%procpnt => DEPAVE
            process_routine( 27)%pronam = 'VTRANS' ; process_routine( 27)%procpnt => VTRANS
            process_routine( 28)%pronam = 'D40BLO' ; process_routine( 28)%procpnt => D40BLO
            process_routine( 29)%pronam = 'PHCOMB' ; process_routine( 29)%procpnt => PHCOMB
            process_routine( 30)%pronam = 'MAKPOC' ; process_routine( 30)%procpnt => MAKPOC
            process_routine( 31)%pronam = 'PHCOMP' ; process_routine( 31)%procpnt => PHCOMP
            process_routine( 32)%pronam = 'SEDCOM' ; process_routine( 32)%procpnt => SEDCOM
            process_routine( 33)%pronam = 'WKCOMP' ; process_routine( 33)%procpnt => WKCOMP
            process_routine( 34)%pronam = 'DMVOL'  ; process_routine( 34)%procpnt => DMVOL
            process_routine( 35)%pronam = 'BACMRT' ; process_routine( 35)%procpnt => BACMRT
            process_routine( 36)%pronam = 'SATCO2' ; process_routine( 36)%procpnt => SATCO2
            process_routine( 37)%pronam = 'REAR'   ; process_routine( 37)%procpnt => REAR
            process_routine( 38)%pronam = 'ADSPO4' ; process_routine( 38)%procpnt => ADSPO4
            process_routine( 39)%pronam = 'DENSED' ; process_routine( 39)%procpnt => DENSED
            process_routine( 40)%pronam = 'DENWAT' ; process_routine( 40)%procpnt => DENWAT
            process_routine( 41)%pronam = 'NITRIF' ; process_routine( 41)%procpnt => NITRIF
            process_routine( 42)%pronam = 'SATOXY' ; process_routine( 42)%procpnt => SATOXY
            process_routine( 43)%pronam = 'VAROXY' ; process_routine( 43)%procpnt => VAROXY
            process_routine( 44)%pronam = 'BOTMIN' ; process_routine( 44)%procpnt => BOTMIN
            process_routine( 45)%pronam = 'BODCOD' ; process_routine( 45)%procpnt => BODCOD
            process_routine( 46)%pronam = 'DECBOD' ; process_routine( 46)%procpnt => DECBOD
            process_routine( 47)%pronam = 'DECPC5' ; process_routine( 47)%procpnt => DECPC5
            process_routine( 48)%pronam = 'VIVIAN' ; process_routine( 48)%procpnt => VIVIAN
            process_routine( 49)%pronam = 'DISSI'  ; process_routine( 49)%procpnt => DISSI
            process_routine( 50)%pronam = 'SEDOX'  ; process_routine( 50)%procpnt => SEDOX
            process_routine( 51)%pronam = 'NLALG'  ; process_routine( 51)%procpnt => NLALG
            process_routine( 52)%pronam = 'RDBALG' ; process_routine( 52)%procpnt => RDBALG
            process_routine( 53)%pronam = 'DYNAMO' ; process_routine( 53)%procpnt => DYNAMO
            process_routine( 54)%pronam = 'SDPPRO' ; process_routine( 54)%procpnt => SDPPRO
            process_routine( 55)%pronam = 'DYNPRO' ; process_routine( 55)%procpnt => DYNPRO
            process_routine( 56)%pronam = 'NRALGS' ; process_routine( 56)%procpnt => NRALGS
            process_routine( 57)%pronam = 'OXYMIN' ; process_routine( 57)%procpnt => OXYMIN
            process_routine( 58)%pronam = 'CSELAC' ; process_routine( 58)%procpnt => CSELAC
            process_routine( 59)%pronam = 'EBUCH4' ; process_routine( 59)%procpnt => EBUCH4
            process_routine( 60)%pronam = 'SATCH4' ; process_routine( 60)%procpnt => SATCH4
            process_routine( 61)%pronam = 'SULFID' ; process_routine( 61)%procpnt => SULFID
            process_routine( 62)%pronam = 'SULFOX' ; process_routine( 62)%procpnt => SULFOX
            process_routine( 63)%pronam = 'SULFPR' ; process_routine( 63)%procpnt => SULFPR
            process_routine( 64)%pronam = 'METHOX' ; process_routine( 64)%procpnt => METHOX
            process_routine( 65)%pronam = 'SPECFE' ; process_routine( 65)%procpnt => SPECFE
            process_routine( 66)%pronam = 'IRONOX' ; process_routine( 66)%procpnt => IRONOX
            process_routine( 67)%pronam = 'SULPHO' ; process_routine( 67)%procpnt => SULPHO
            process_routine( 68)%pronam = 'IRONRE' ; process_routine( 68)%procpnt => IRONRE
            process_routine( 69)%pronam = 'PRIRON' ; process_routine( 69)%procpnt => PRIRON
            process_routine( 70)%pronam = 'CALSED' ; process_routine( 70)%procpnt => CALSED
            process_routine( 71)%pronam = 'SEDCAR' ; process_routine( 71)%procpnt => SEDCAR
            process_routine( 72)%pronam = 'SEDNU2' ; process_routine( 72)%procpnt => SEDNU2
            process_routine( 73)%pronam = 'SEDSOD' ; process_routine( 73)%procpnt => SEDSOD
            process_routine( 74)%pronam = 'SSEDPH' ; process_routine( 74)%procpnt => SSEDPH
            process_routine( 75)%pronam = 'SOMSED' ; process_routine( 75)%procpnt => SOMSED
            process_routine( 76)%pronam = 'SEDAAP' ; process_routine( 76)%procpnt => SEDAAP
            process_routine( 77)%pronam = 'RESDM'  ; process_routine( 77)%procpnt => RESDM
            process_routine( 78)%pronam = 'BURIAL' ; process_routine( 78)%procpnt => BURIAL
            process_routine( 79)%pronam = 'DIGGIN' ; process_routine( 79)%procpnt => DIGGIN
            process_routine( 80)%pronam = 'ADVTRA' ; process_routine( 80)%procpnt => ADVTRA
            process_routine( 81)%pronam = 'DSPTRA' ; process_routine( 81)%procpnt => DSPTRA
            process_routine( 82)%pronam = 'RFPART' ; process_routine( 82)%procpnt => RFPART
            process_routine( 83)%pronam = 'PARTMP' ; process_routine( 83)%procpnt => PARTMP
            process_routine( 84)%pronam = 'TRASE2' ; process_routine( 84)%procpnt => TRASE2
            process_routine( 85)%pronam = 'ULFIX'  ; process_routine( 85)%procpnt => ULFIX
            process_routine( 86)%pronam = 'CONSBL' ; process_routine( 86)%procpnt => CONSBL
            process_routine( 87)%pronam = 'SWOXY'  ; process_routine( 87)%procpnt => SWOXY
            process_routine( 88)%pronam = 'TRCOEF' ; process_routine( 88)%procpnt => TRCOEF
            process_routine( 89)%pronam = 'VERVLU' ; process_routine( 89)%procpnt => VERVLU
            process_routine( 90)%pronam = 'DEGMP'  ; process_routine( 90)%procpnt => DEGMP
            process_routine( 91)%pronam = 'SEDHM'  ; process_routine( 91)%procpnt => SEDHM
            process_routine( 92)%pronam = 'SEDOMV' ; process_routine( 92)%procpnt => SEDOMV
            process_routine( 93)%pronam = 'ATMDEP' ; process_routine( 93)%procpnt => ATMDEP
            process_routine( 94)%pronam = 'NH3FRE' ; process_routine( 94)%procpnt => NH3FRE
            process_routine( 95)%pronam = 'POSOXY' ; process_routine( 95)%procpnt => POSOXY
            process_routine( 96)%pronam = 'SECCHI' ; process_routine( 96)%procpnt => SECCHI
            process_routine( 97)%pronam = 'PTEWOR' ; process_routine( 97)%procpnt => PTEWOR
            process_routine( 98)%pronam = 'STREAR' ; process_routine( 98)%procpnt => STREAR
            process_routine( 99)%pronam = 'TRSOXY' ; process_routine( 99)%procpnt => TRSOXY
            process_routine(100)%pronam = 'APATIT' ; process_routine(100)%procpnt => APATIT
            process_routine(101)%pronam = 'HARVES' ; process_routine(101)%procpnt => HARVES
            process_routine(102)%pronam = 'VEG2DN' ; process_routine(102)%procpnt => VEG2DN
            process_routine(103)%pronam = 'VBSTAT' ; process_routine(103)%procpnt => VBSTAT
            process_routine(104)%pronam = 'VBGRO'  ; process_routine(104)%procpnt => VBGRO
            process_routine(105)%pronam = 'VBMRT'  ; process_routine(105)%procpnt => VBMRT
            process_routine(106)%pronam = 'VEG3DX' ; process_routine(106)%procpnt => VEG3DX
            process_routine(107)%pronam = 'VBUPT'  ; process_routine(107)%procpnt => VBUPT
            process_routine(108)%pronam = 'VEG3DU' ; process_routine(108)%procpnt => VEG3DU
            process_routine(109)%pronam = 'SALCHL' ; process_routine(109)%procpnt => SALCHL
            process_routine(110)%pronam = 'DECDET' ; process_routine(110)%procpnt => DECDET
            process_routine(111)%pronam = 'S12TRA' ; process_routine(111)%procpnt => S12TRA
            process_routine(112)%pronam = 'RESANT' ; process_routine(112)%procpnt => RESANT
            process_routine(113)%pronam = 'STADAY' ; process_routine(113)%procpnt => STADAY
            process_routine(114)%pronam = 'STADPT' ; process_routine(114)%procpnt => STADPT
            process_routine(115)%pronam = 'STADSC' ; process_routine(115)%procpnt => STADSC
            process_routine(116)%pronam = 'STAMEA' ; process_routine(116)%procpnt => STAMEA
            process_routine(117)%pronam = 'STADEV' ; process_routine(117)%procpnt => STADEV
            process_routine(118)%pronam = 'STAGEO' ; process_routine(118)%procpnt => STAGEO
            process_routine(119)%pronam = 'STAPRC' ; process_routine(119)%procpnt => STAPRC
            process_routine(120)%pronam = 'STAQTL' ; process_routine(120)%procpnt => STAQTL
            process_routine(121)%pronam = 'SUMFRC' ; process_routine(121)%procpnt => SUMFRC
            process_routine(122)%pronam = 'FLXFRC' ; process_routine(122)%procpnt => FLXFRC
            process_routine(123)%pronam = 'PHCARB' ; process_routine(123)%procpnt => PHCARB
            process_routine(124)%pronam = 'HDISPA' ; process_routine(124)%procpnt => HDISPA
            process_routine(125)%pronam = 'MAXMAC' ; process_routine(125)%procpnt => MAXMAC
            process_routine(126)%pronam = 'COVMAC' ; process_routine(126)%procpnt => COVMAC
            process_routine(127)%pronam = 'MACDIS' ; process_routine(127)%procpnt => MACDIS
            process_routine(128)%pronam = 'RADMAC' ; process_routine(128)%procpnt => RADMAC
            process_routine(129)%pronam = 'MACNUT' ; process_routine(129)%procpnt => MACNUT
            process_routine(130)%pronam = 'MACROP' ; process_routine(130)%procpnt => MACROP
            process_routine(131)%pronam = 'MAC3DU' ; process_routine(131)%procpnt => MAC3DU
            process_routine(132)%pronam = 'GRZMAC' ; process_routine(132)%procpnt => GRZMAC
            process_routine(133)%pronam = 'NPPS12' ; process_routine(133)%procpnt => NPPS12
            process_routine(134)%pronam = 'DEBGRZ' ; process_routine(134)%procpnt => DEBGRZ
            process_routine(135)%pronam = 'FLOCEQ' ; process_routine(135)%procpnt => FLOCEQ
            process_routine(136)%pronam = 'DREDGE' ; process_routine(136)%procpnt => dredge_process
            process_routine(137)%pronam = 'RESBUF' ; process_routine(137)%procpnt => RESBUF
            process_routine(138)%pronam = 'SEDIM ' ; process_routine(138)%procpnt => SEDIM
            process_routine(139)%pronam = 'S12TIM' ; process_routine(139)%procpnt => S12TIM
            process_routine(140)%pronam = 'REFL  ' ; process_routine(140)%procpnt => REFL
            process_routine(141)%pronam = 'ATTOUT' ; process_routine(141)%procpnt => ATTOUT
            process_routine(142)%pronam = 'CASCAD' ; process_routine(142)%procpnt => CASCAD
            process_routine(143)%pronam = 'EFFBLO' ; process_routine(143)%procpnt => EFFBLO
            process_routine(144)%pronam = 'EFFAVE' ; process_routine(144)%procpnt => EFFAVE
            process_routine(145)%pronam = 'DECTRA' ; process_routine(145)%procpnt => DECTRA
            process_routine(146)%pronam = 'ESPACE' ; process_routine(146)%procpnt => ESPACE
            process_routine(147)%pronam = 'CALTEM' ; process_routine(147)%procpnt => CALTEM
            process_routine(148)%pronam = 'PLASTC' ; process_routine(148)%procpnt => PLASTC
            process_routine(149)%pronam = 'WLCWOC' ; process_routine(149)%procpnt => WLCWOC
            process_routine(150)%pronam = 'HDISS'  ; process_routine(150)%procpnt => HDISS
            process_routine(151)%pronam = 'TMODE'  ; process_routine(151)%procpnt => TMODE
            process_routine(152)%pronam = 'DLWQG2' ; process_routine(152)%procpnt => DLWQG2
            process_routine(153)%pronam = 'GEMMPB' ; process_routine(153)%procpnt => GEMMPB
            process_routine(154)%pronam = 'MPBNUT' ; process_routine(154)%procpnt => MPBNUT
            process_routine(155)%pronam = 'MPBTMP' ; process_routine(155)%procpnt => MPBTMP
            process_routine(156)%pronam = 'MPBLLM' ; process_routine(156)%procpnt => MPBLLM
            process_routine(157)%pronam = 'MPBNLM' ; process_routine(157)%procpnt => MPBNLM
            process_routine(158)%pronam = 'VBXS12' ; process_routine(158)%procpnt => VBXS12
            process_routine(159)%pronam = 'VBXSUM' ; process_routine(159)%procpnt => VBXSUM
            process_routine(160)%pronam = 'PROPSG' ; process_routine(160)%procpnt => PROPSG
            process_routine(161)%pronam = 'PRPAGG' ; process_routine(161)%procpnt => PRPAGG
            process_routine(162)%pronam = 'HETAGG' ; process_routine(162)%procpnt => HETAGG
            process_routine(163)%pronam = 'SEDTYR' ; process_routine(163)%procpnt => SEDTYR
            process_routine(164)%pronam = 'SEDAGG' ; process_routine(164)%procpnt => SEDAGG
            process_routine(165)%pronam = 'SUMTYR' ; process_routine(165)%procpnt => SUMTYR
            process_routine(166)%pronam = 'PROPFD' ; process_routine(166)%procpnt => PROPFD
            process_routine(167)%pronam = 'PRODIA' ; process_routine(167)%procpnt => PRODIA
            process_routine(168)%pronam = 'PROGRE' ; process_routine(168)%procpnt => PROGRE
            process_routine(169)%pronam = 'PRONCM' ; process_routine(169)%procpnt => PRONCM
            process_routine(170)%pronam = 'PROSED' ; process_routine(170)%procpnt => PROSED
            process_routine(171)%pronam = 'PROTCM' ; process_routine(171)%procpnt => PROTCM
            process_routine(172)%pronam = 'PROZOO' ; process_routine(172)%procpnt => PROZOO
            process_routine(173)%pronam = 'DRADIO' ; process_routine(173)%procpnt => DRADIO
            process_routine(174)%pronam = 'PHPROT' ; process_routine(174)%procpnt => PHPROT
            process_routine(175)%pronam = 'FLOCSD' ; process_routine(175)%procpnt => FLOCSD
            process_routine(176)%pronam = 'AGECAR' ; process_routine(176)%procpnt => AGECART
            process_routine(177)%pronam = 'PRTMRT' ; process_routine(177)%procpnt => protist_mortality_salinity

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
