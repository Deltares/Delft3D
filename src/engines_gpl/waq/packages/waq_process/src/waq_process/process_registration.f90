!!  Copyright (C)  Stichting Deltares, 2012-2023.
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
use m_srstop
use m_monsys

    implicit none

    private

    public :: pronrs, procal

    integer, parameter :: max_processes = 180  ! Exact number of process routines

    type :: process_routine_info
        character(len=6) :: pronam
        procedure(), pointer, nopass :: procpnt
    end type process_routine_info

    type(process_routine_info), save :: process_routine(max_processes)

contains

subroutine pronrs( pronam, imodul )
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
    character(len=*), intent(in) :: pronam
    integer, intent(out)         :: imodul

    integer :: i
    logical :: okay
    logical, save :: first = .true.

    external :: &
        DDEPTH, DSURF, TOTDEP, EMERSI, METEO, HEATFL, DAYRAD, TEMPER, VARSAL, VELOC, RESTIM, STOX3D, HDISP, HDISPV, WATAGE, INTPOL,      &
        CALCHZ, CALWAV, CALTAU, SIMPH, SPCARB, EXTINA, EXTINC, CLCRAD, DAYL, DEPAVE, VTRANS, D40BLO, PHCOMB, MAKPOC, PHCOMP, SEDCOM,     &
        WKCOMP, DMVOL, BACMRT, SATCO2, REAR, ADSPO4, DENSED, DENWAT, NITRIF, SATOXY, VAROXY, BOTMIN, BODCOD, DECBOD, DECPC5, VIVIAN,     &
        DISSI, SEDOX, TFALG, DLALG, NLALG, RADALG, RDBALG, PRIPRO, SDPPRO, PPRLIM, NUTUPT, NUTREL, NRALGS, OXYMIN, CSELAC, EBUCH4,       &
        SATCH4, SULFID, SULFOX, SULFPR, METHOX, SPECFE, IRONOX, SULPHO, IRONRE, PRIRON, CALSED, SEDCAR, SEDNU2, SEDSOD, SSEDPH,          &
        SOMSED, SEDAAP, RESDM, BURIAL, DIGGIN, ADVTRA, DSPTRA, RFPART, PARTMP, TRASE2, ULFIX, CONSBL, SWOXY, TRCOEF, VERVLU, DEGMP,      &
        SEDHM, SEDOMV, ATMDEP, NH3FRE, POSOXY, SECCHI, PTEWOR, STREAR, TRSOXY, APATIT, HARVES, VEG2DN, VBSTAT, VBGRO, VBMRT, VEG3DX,     &
        VBUPT, VEG3DU, SALCHL, DECDET, S12TRA, RESANT, STADAY, STADPT, STADSC, STAGEO, STAPRC, STAQTL, SUMFRC, FLXFRC, PHCARB, HDISPA,   &
        MAXMAC, COVMAC, MACDIS, RADMAC, MACNUT, MACROP, MAC3DU, GRZMAC, NPPS12, DEBGRZ, FLOCEQ, DREDGE, RESPUP, RESBUF, SEDIM, S12TIM,   &
        REFL, ATTOUT, CASCAD, EFFBLO, EFFAVE, DECTRA, ESPACE, CALTEM, PLASTC, WLCWOC, HDISS, TMODE, DLWQG2, GEMMPB, MPBNUT, MPBTMP,      &
        MPBLLM, MPBNLM, VBXS12, VBXSUM, PROPSG, PRPAGG, HETAGG, SEDTYR, SEDAGG, SUMTYR, PROPFD, PRODIA, PROGRE, PRONCM, PROSED, PROTCM,  &
        PROZOO, DRADIO, PHPROT, FLOCSD, AGECART

!
!   Register the process routines
!
    if ( first ) then
        first = .false.
        if ( size(process_routine) /= max_processes ) then
            write(*,*) 'Programming error im PRONRS - wrong size for array of process routines!'
            stop
        endif

        process_routine(1) = process_routine_info( 'DDEPTH', DDEPTH )
        process_routine(2) = process_routine_info( 'DSURF',  DSURF  )
        process_routine(3) = process_routine_info( 'TOTDEP', TOTDEP )
        process_routine(4) = process_routine_info( 'EMERSI', EMERSI )
        process_routine(5) = process_routine_info( 'METEO',  METEO  )
        process_routine(6) = process_routine_info( 'HEATFL', HEATFL )
        process_routine(7) = process_routine_info( 'DAYRAD', DAYRAD )
        process_routine(8) = process_routine_info( 'TEMPER', TEMPER )
        process_routine(9) = process_routine_info( 'VARSAL', VARSAL )
        process_routine(10) = process_routine_info( 'VELOC',  VELOC  )
        process_routine(11) = process_routine_info( 'RESTIM', RESTIM )
        process_routine(12) = process_routine_info( 'STOX3D', STOX3D )
        process_routine(13) = process_routine_info( 'HDISP',  HDISP  )
        process_routine(14) = process_routine_info( 'HDISPV', HDISPV )
        process_routine(15) = process_routine_info( 'WATAGE', WATAGE )
        process_routine(16) = process_routine_info( 'INTPOL', INTPOL )
        process_routine(17) = process_routine_info( 'CALCHZ', CALCHZ )
        process_routine(18) = process_routine_info( 'CALWAV', CALWAV )
        process_routine(19) = process_routine_info( 'CALTAU', CALTAU )
        process_routine(20) = process_routine_info( 'SIMPH',  SIMPH  )
        process_routine(21) = process_routine_info( 'SPCARB', SPCARB )
        process_routine(22) = process_routine_info( 'EXTINA', EXTINA )
        process_routine(23) = process_routine_info( 'EXTINC', EXTINC )
        process_routine(24) = process_routine_info( 'CLCRAD', CLCRAD )
        process_routine(25) = process_routine_info( 'DAYL',   DAYL   )
        process_routine(26) = process_routine_info( 'DEPAVE', DEPAVE )
        process_routine(27) = process_routine_info( 'VTRANS', VTRANS )
        process_routine(28) = process_routine_info( 'D40BLO', D40BLO )
        process_routine(29) = process_routine_info( 'PHCOMB', PHCOMB )
        process_routine(30) = process_routine_info( 'MAKPOC', MAKPOC )
        process_routine(31) = process_routine_info( 'PHCOMP', PHCOMP )
        process_routine(32) = process_routine_info( 'SEDCOM', SEDCOM )
        process_routine(33) = process_routine_info( 'WKCOMP', WKCOMP )
        process_routine(34) = process_routine_info( 'DMVOL',  DMVOL  )
        process_routine(35) = process_routine_info( 'BACMRT', BACMRT )
        process_routine(36) = process_routine_info( 'SATCO2', SATCO2 )
        process_routine(37) = process_routine_info( 'REAR',   REAR   )
        process_routine(38) = process_routine_info( 'ADSPO4', ADSPO4 )
        process_routine(39) = process_routine_info( 'DENSED', DENSED )
        process_routine(40) = process_routine_info( 'DENWAT', DENWAT )
        process_routine(41) = process_routine_info( 'NITRIF', NITRIF )
        process_routine(42) = process_routine_info( 'SATOXY', SATOXY )
        process_routine(43) = process_routine_info( 'VAROXY', VAROXY )
        process_routine(44) = process_routine_info( 'BOTMIN', BOTMIN )
        process_routine(45) = process_routine_info( 'BODCOD', BODCOD )
        process_routine(46) = process_routine_info( 'DECBOD', DECBOD )
        process_routine(47) = process_routine_info( 'DECPC5', DECPC5 )
        process_routine(48) = process_routine_info( 'VIVIAN', VIVIAN )
        process_routine(49) = process_routine_info( 'DISSI',  DISSI  )
        process_routine(50) = process_routine_info( 'SEDOX',  SEDOX  )
        process_routine(51) = process_routine_info( 'TFALG',  TFALG  )
        process_routine(52) = process_routine_info( 'DLALG',  DLALG  )
        process_routine(53) = process_routine_info( 'NLALG',  NLALG  )
        process_routine(54) = process_routine_info( 'RADALG', RADALG )
        process_routine(55) = process_routine_info( 'RDBALG', RDBALG )
        process_routine(56) = process_routine_info( 'PRIPRO', PRIPRO )
        process_routine(57) = process_routine_info( 'SDPPRO', SDPPRO )
        process_routine(58) = process_routine_info( 'PPRLIM', PPRLIM )
        process_routine(59) = process_routine_info( 'NUTUPT', NUTUPT )
        process_routine(60) = process_routine_info( 'NUTREL', NUTREL )
        process_routine(61) = process_routine_info( 'NRALGS', NRALGS )
        process_routine(62) = process_routine_info( 'OXYMIN', OXYMIN )
        process_routine(63) = process_routine_info( 'CSELAC', CSELAC )
        process_routine(64) = process_routine_info( 'EBUCH4', EBUCH4 )
        process_routine(65) = process_routine_info( 'SATCH4', SATCH4 )
        process_routine(66) = process_routine_info( 'SULFID', SULFID )
        process_routine(67) = process_routine_info( 'SULFOX', SULFOX )
        process_routine(68) = process_routine_info( 'SULFPR', SULFPR )
        process_routine(69) = process_routine_info( 'METHOX', METHOX )
        process_routine(70) = process_routine_info( 'SPECFE', SPECFE )
        process_routine(71) = process_routine_info( 'IRONOX', IRONOX )
        process_routine(72) = process_routine_info( 'SULPHO', SULPHO )
        process_routine(73) = process_routine_info( 'IRONRE', IRONRE )
        process_routine(74) = process_routine_info( 'PRIRON', PRIRON )
        process_routine(75) = process_routine_info( 'CALSED', CALSED )
        process_routine(76) = process_routine_info( 'SEDCAR', SEDCAR )
        process_routine(77) = process_routine_info( 'SEDNU2', SEDNU2 )
        process_routine(78) = process_routine_info( 'SEDSOD', SEDSOD )
        process_routine(79) = process_routine_info( 'SSEDPH', SSEDPH )
        process_routine(80) = process_routine_info( 'SOMSED', SOMSED )
        process_routine(81) = process_routine_info( 'SEDAAP', SEDAAP )
        process_routine(82) = process_routine_info( 'RESDM',  RESDM  )
        process_routine(83) = process_routine_info( 'BURIAL', BURIAL )
        process_routine(84) = process_routine_info( 'DIGGIN', DIGGIN )
        process_routine(85) = process_routine_info( 'ADVTRA', ADVTRA )
        process_routine(86) = process_routine_info( 'DSPTRA', DSPTRA )
        process_routine(87) = process_routine_info( 'RFPART', RFPART )
        process_routine(88) = process_routine_info( 'PARTMP', PARTMP )
        process_routine(89) = process_routine_info( 'TRASE2', TRASE2 )
        process_routine(90) = process_routine_info( 'ULFIX',  ULFIX  )
        process_routine(91) = process_routine_info( 'CONSBL', CONSBL )
        process_routine(92) = process_routine_info( 'SWOXY',  SWOXY  )
        process_routine(93) = process_routine_info( 'TRCOEF', TRCOEF )
        process_routine(94) = process_routine_info( 'VERVLU', VERVLU )
        process_routine(95) = process_routine_info( 'DEGMP',  DEGMP  )
        process_routine(96) = process_routine_info( 'SEDHM',  SEDHM  )
        process_routine(97) = process_routine_info( 'SEDOMV', SEDOMV )
        process_routine(98) = process_routine_info( 'ATMDEP', ATMDEP )
        process_routine(99) = process_routine_info( 'NH3FRE', NH3FRE )
        process_routine(100) = process_routine_info( 'POSOXY', POSOXY )
        process_routine(101) = process_routine_info( 'SECCHI', SECCHI )
        process_routine(102) = process_routine_info( 'PTEWOR', PTEWOR )
        process_routine(103) = process_routine_info( 'STREAR', STREAR )
        process_routine(104) = process_routine_info( 'TRSOXY', TRSOXY )
        process_routine(105) = process_routine_info( 'APATIT', APATIT )
        process_routine(106) = process_routine_info( 'HARVES', HARVES )
        process_routine(107) = process_routine_info( 'VEG2DN', VEG2DN )
        process_routine(108) = process_routine_info( 'VBSTAT', VBSTAT )
        process_routine(109) = process_routine_info( 'VBGRO',  VBGRO  )
        process_routine(110) = process_routine_info( 'VBMRT',  VBMRT  )
        process_routine(111) = process_routine_info( 'VEG3DX', VEG3DX )
        process_routine(112) = process_routine_info( 'VBUPT',  VBUPT  )
        process_routine(113) = process_routine_info( 'VEG3DU', VEG3DU )
        process_routine(114) = process_routine_info( 'SALCHL', SALCHL )
        process_routine(115) = process_routine_info( 'DECDET', DECDET )
        process_routine(116) = process_routine_info( 'S12TRA', S12TRA )
        process_routine(117) = process_routine_info( 'RESANT', RESANT )
        process_routine(118) = process_routine_info( 'STADAY', STADAY )
        process_routine(119) = process_routine_info( 'STADPT', STADPT )
        process_routine(120) = process_routine_info( 'STADSC', STADSC )
        process_routine(121) = process_routine_info( 'STAGEO', STAGEO )
        process_routine(122) = process_routine_info( 'STAPRC', STAPRC )
        process_routine(123) = process_routine_info( 'STAQTL', STAQTL )
        process_routine(124) = process_routine_info( 'SUMFRC', SUMFRC )
        process_routine(125) = process_routine_info( 'FLXFRC', FLXFRC )
        process_routine(126) = process_routine_info( 'PHCARB', PHCARB )
        process_routine(127) = process_routine_info( 'HDISPA', HDISPA )
        process_routine(128) = process_routine_info( 'MAXMAC', MAXMAC )
        process_routine(129) = process_routine_info( 'COVMAC', COVMAC )
        process_routine(130) = process_routine_info( 'MACDIS', MACDIS )
        process_routine(131) = process_routine_info( 'RADMAC', RADMAC )
        process_routine(132) = process_routine_info( 'MACNUT', MACNUT )
        process_routine(133) = process_routine_info( 'MACROP', MACROP )
        process_routine(134) = process_routine_info( 'MAC3DU', MAC3DU )
        process_routine(135) = process_routine_info( 'GRZMAC', GRZMAC )
        process_routine(136) = process_routine_info( 'NPPS12', NPPS12 )
        process_routine(137) = process_routine_info( 'DEBGRZ', DEBGRZ )
        process_routine(138) = process_routine_info( 'FLOCEQ', FLOCEQ )
        process_routine(139) = process_routine_info( 'DREDGE', DREDGE )
        process_routine(140) = process_routine_info( 'RESPUP', RESPUP )
        process_routine(141) = process_routine_info( 'RESBUF', RESBUF )
        process_routine(142) = process_routine_info( 'SEDIM ', SEDIM  )
        process_routine(143) = process_routine_info( 'S12TIM', S12TIM )
        process_routine(144) = process_routine_info( 'REFL  ', REFL   )
        process_routine(145) = process_routine_info( 'ATTOUT', ATTOUT )
        process_routine(146) = process_routine_info( 'CASCAD', CASCAD )
        process_routine(147) = process_routine_info( 'EFFBLO', EFFBLO )
        process_routine(148) = process_routine_info( 'EFFAVE', EFFAVE )
        process_routine(149) = process_routine_info( 'DECTRA', DECTRA )
        process_routine(150) = process_routine_info( 'ESPACE', ESPACE )
        process_routine(151) = process_routine_info( 'CALTEM', CALTEM )
        process_routine(152) = process_routine_info( 'PLASTC', PLASTC )
        process_routine(153) = process_routine_info( 'WLCWOC', WLCWOC )
        process_routine(154) = process_routine_info( 'HDISS' , HDISS  )
        process_routine(155) = process_routine_info( 'TMODE' , TMODE  )
        process_routine(156) = process_routine_info( 'DLWQG2', DLWQG2 )
        process_routine(157) = process_routine_info( 'GEMMPB', GEMMPB )
        process_routine(158) = process_routine_info( 'MPBNUT', MPBNUT )
        process_routine(159) = process_routine_info( 'MPBTMP', MPBTMP )
        process_routine(160) = process_routine_info( 'MPBLLM', MPBLLM )
        process_routine(161) = process_routine_info( 'MPBNLM', MPBNLM )
        process_routine(162) = process_routine_info( 'VBXS12', VBXS12 )
        process_routine(163) = process_routine_info( 'VBXSUM', VBXSUM )
        process_routine(164) = process_routine_info( 'PROPSG', PROPSG )
        process_routine(165) = process_routine_info( 'PRPAGG', PRPAGG )
        process_routine(166) = process_routine_info( 'HETAGG', HETAGG )
        process_routine(167) = process_routine_info( 'SEDTYR', SEDTYR )
        process_routine(168) = process_routine_info( 'SEDAGG', SEDAGG )
        process_routine(169) = process_routine_info( 'SUMTYR', SUMTYR )
        process_routine(170) = process_routine_info( 'PROPFD', PROPFD )
        process_routine(171) = process_routine_info( 'PRODIA', PRODIA )
        process_routine(172) = process_routine_info( 'PROGRE', PROGRE )
        process_routine(173) = process_routine_info( 'PRONCM', PRONCM )
        process_routine(174) = process_routine_info( 'PROSED', PROSED )
        process_routine(175) = process_routine_info( 'PROTCM', PROTCM )
        process_routine(176) = process_routine_info( 'PROZOO', PROZOO )
        process_routine(177) = process_routine_info( 'DRADIO', DRADIO )
        process_routine(178) = process_routine_info( 'PHPROT', PHPROT )
        process_routine(179) = process_routine_info( 'FLOCSD', FLOCSD )
        process_routine(180) = process_routine_info( 'AGECAR', AGECART)

!
!   Sanity check: all pointers in the array should be set
!
        okay = .true.
        do i = 1,size(process_routine)
            if ( process_routine(i)%pronam == '?' ) then
                okay = .false.
                write(*,*) 'Process ',i, ': no proper name'
            endif
            if ( .not. associated(process_routine(i)%procpnt) ) then
                okay = .false.
                write(*,*) 'Process ',i, ': no routine registered'
            endif
        enddo

        if ( .not. okay ) then
            write(*,*) 'One or more errors in the process registration!'
            stop
        endif
    endif

!
!   Determine the index of the routine
!
!
!   Note: This statement did not work with builds from TeamCity.
!         I saw a problem report on the Intel Forum that seems relevant in this context.
!         For now I will do it the pedestrian way.
!         MDK: There is a known bug in intel oneAPI 2021.03 regarding findloc
!
!   imodul = findloc( process_routine%pronam, pronam, 1 )
!
    do i = 1,size(process_routine)
        if ( process_routine(i)%pronam == pronam ) then
            imodul = i
            exit
        endif
    enddo

    !write(*,*) 'PRONRS:', pronam, imodul

end subroutine pronrs

subroutine procal (pmsa   , imodul , flux   , ipoint , increm , &
                   noseg  , noflux , iexpnt , iknmrk , noq1   , &
                   noq2   , noq3   , noq4   , pronam , pronvr , &
                   prvtyp , iproc  , dll_opb)
!>\file
!>       Calls the process modules

!     Deltares Software Centre


    use timers
    use iso_c_binding

!     parameters          :

!     kind           function                 name          description

    real   (4)   , intent(inout)          :: pmsa  ( * ) ! Process module status array
    integer      , intent(in   )          :: imodul      ! Process module number
    real   (4)   , intent(  out)          :: flux  ( * ) ! Process fluxes
    integer      , intent(in   )          :: ipoint( * ) ! Pointer to process data
    integer      , intent(in   )          :: increm( * ) ! Increment in pointer process data
    integer      , intent(in   )          :: noseg       ! Number of computational volumes
    integer      , intent(in   )          :: noflux      ! Number of process fluxes
    integer      , intent(in   )          :: iexpnt(4,*) ! Exchange pointers
    integer      , intent(in   )          :: iknmrk( * ) ! Tag array
    integer      , intent(in   )          :: noq1        ! Number of exchanges in first direction
    integer      , intent(in   )          :: noq2        ! Number of exchanges in second direction
    integer      , intent(in   )          :: noq3        ! Number of exchanges in third direction
    integer      , intent(in   )          :: noq4        ! Number of exchanges in the water bed
    character(10), intent(in   )          :: pronam      ! Name of this process
    integer      , intent(in   )          :: pronvr      ! Not used
    integer      , intent(in   )          :: prvtyp( * ) ! Not used
    integer      , intent(in   )          :: iproc       ! Process number
    integer(c_intptr_t)   , intent(in   ) :: dll_opb     ! open proces library dll handle

!  local

    integer             :: perf_function
    integer             :: lunrep
    integer             :: ierror

    integer, parameter  :: nomax = 500
    integer(4), save    :: ithand(nomax) = 0 !  timer handles

    if ( timon ) then
        if ( imodul .eq. 0 ) then
            write( lunrep, '(2a)' ) 'Unknown process module: ', pronam
            write( lunrep, '(2a)' ) 'Program stopped!'
            error stop
        endif
        if ( imodul .le. nomax ) call timstrt ( pronam, ithand(imodul) )
    endif


    if ( imodul <= max_processes ) then
        call process_routine(imodul)%procpnt ( pmsa   , flux   , ipoint , increm , noseg  ,  &
                                               noflux , iexpnt , iknmrk , noq1   , noq2   ,  &
                                               noq3   , noq4   )
    else

!       assumed from dll

        call getmlu(lunrep)
        if (dll_opb .ne. 0) then
            ierror = perf_function(dll_opb, pronam, pmsa   , flux   , ipoint , increm , noseg  , &
                                   noflux , iexpnt, iknmrk , noq1   , noq2   , noq3   , noq4   )
            if ( ierror .ne. 0 ) then
                write(*,*) ' '
                write(*,*) 'ERROR        : requested module not in open process library dll/so'
                write(*,*) 'module       : ', pronam
                write(*,*) 'dll/so handle: ', dll_opb
                write(lunrep,*) ' '
                write(lunrep,*) 'ERROR        : requested module not in open process library dll/so'
                write(lunrep,*) 'module       : ', pronam
                write(lunrep,*) 'dll/so handle: ', dll_opb
                call srstop(1)
            endif
        else
            write(*,*) ' '
            write(*,*) 'ERROR  : requested module not available, no open process library dll/so loaded'
            write(*,*) 'module : ', pronam
            write(lunrep,*) ' '
            write(lunrep,*) 'ERROR  : requested module not available, no open process library dll/so loaded'
            write(lunrep,*) 'module       : ', pronam
            call srstop(1)
        endif
    endif

    if ( timon ) then
        if ( imodul .le. nomax ) call timstop ( ithand(imodul) )
    endif

end subroutine procal

end module process_registration
