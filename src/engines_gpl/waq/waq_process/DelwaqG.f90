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
!!  along with this prooram. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
module m_delwaqg
use m_waq_precision
use m_inverm


implicit none

contains


     subroutine DLWQG2     ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
     use m_logger, only : write_error_message, get_log_unit_number
     use m_evaluate_waq_attribute

!XXXDEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'DLWQG2' :: DLWQG2
!
!*******************************************************************************
!
      IMPLICIT NONE
!
!     Type    Name         I/O Description
!
      real(kind=real_wp)  ::pmsa(*)     !I/O Process Manager System Array, window of routine to process library
      real(kind=real_wp)  ::fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
      integer(kind=int_wp)  ::ipoint(*)  ! I  Array of pointers in pmsa to get and store the data
      integer(kind=int_wp)  ::increm(*)  ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer(kind=int_wp)  ::noseg       ! I  Number of computational elements in the whole model schematisation
      integer(kind=int_wp)  ::noflux      ! I  Number of fluxes, increment in the fl array
      integer(kind=int_wp)  ::iexpnt(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer(kind=int_wp)  ::iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer(kind=int_wp)  ::noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer(kind=int_wp)  ::noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer(kind=int_wp)  ::noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer(kind=int_wp)  ::noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
!
!*******************************************************************************
!     This process replaces DELWAQ G in stand alone DELWAQ in the context of DFM-WAQ
!
!     PRELIMINARY VERSION for feasibility checks Nov 2019
!     UPDATE              for first practical application April 2021
!                         (1) all S1 variables are now DELWAQ non-transportable states (so DELWAQ can take care of balances and initials)
!                         (2) the arrays ext%%%% have been removed for simplicity
!                             as the pdf and the code are made during developmen, mismatches between the two should/could not occur
!                         (3) provision added for VB related uptake and release

!     Author Jos van Gils
!
!     Type    Name         I/O Description                                        Unit
!
      ! fixed quantities
      real(kind=real_wp), parameter   ::NOX_RATIO = 4.57
      real(kind=real_wp), parameter   ::OXC_RATIO = 2.667
      real(kind=real_wp), parameter   ::NIC_RATIO = 0.933
      real(kind=real_wp), parameter   ::FEC_RATIO = 18.67
      real(kind=real_wp), parameter   ::SUC_RATIO = 1.333
      real(kind=real_wp), parameter   ::CHC_RATIO = 0.5
      real(kind=real_wp), parameter   ::COX_RATIO = 5.33
      real(kind=real_wp), parameter   ::CSU_RATIO = 2.67
      real(kind=real_wp), parameter   ::rhodm = 2.6e6  ! g/m3 solid phase

      integer(kind=int_wp),parameter    ::nototsed = 34
      integer(kind=int_wp),parameter    ::nototseddis = 12
      integer(kind=int_wp),parameter    ::nototsedpart = 22
      integer(kind=int_wp),parameter    ::nofl = 101

      ! pointers to concrete items
      ! constants
      integer(kind=int_wp),parameter  ::ip_ku_dFdcC20 = 1
      integer(kind=int_wp),parameter  ::ip_kl_dFdcC20 = 2
      integer(kind=int_wp),parameter  ::ip_ku_dFdcN20 = 3
      integer(kind=int_wp),parameter  ::ip_kl_dFdcN20 = 4
      integer(kind=int_wp),parameter  ::ip_ku_dFdcP20 = 5
      integer(kind=int_wp),parameter  ::ip_kl_dFdcP20 = 6
      integer(kind=int_wp),parameter  ::ip_ku_dMdcC20 = 7
      integer(kind=int_wp),parameter  ::ip_kl_dMdcC20 = 8
      integer(kind=int_wp),parameter  ::ip_ku_dMdcN20 = 9
      integer(kind=int_wp),parameter  ::ip_kl_dMdcN20 = 10
      integer(kind=int_wp),parameter  ::ip_ku_dMdcP20 = 11
      integer(kind=int_wp),parameter  ::ip_kl_dMdcP20 = 12
      integer(kind=int_wp),parameter  ::ip_ku_dSdcC20 = 13
      integer(kind=int_wp),parameter  ::ip_kl_dSdcC20 = 14
      integer(kind=int_wp),parameter  ::ip_ku_dSdcN20 = 15
      integer(kind=int_wp),parameter  ::ip_kl_dSdcN20 = 16
      integer(kind=int_wp),parameter  ::ip_ku_dSdcP20 = 17
      integer(kind=int_wp),parameter  ::ip_kl_dSdcP20 = 18
      integer(kind=int_wp),parameter  ::ip_k_dprdcC20 = 19
      integer(kind=int_wp),parameter  ::ip_k_DOCdcC20 = 20
      integer(kind=int_wp),parameter  ::ip_al_dNf = 21
      integer(kind=int_wp),parameter  ::ip_al_dPf = 22
      integer(kind=int_wp),parameter  ::ip_au_dNf = 23
      integer(kind=int_wp),parameter  ::ip_au_dPf = 24
      integer(kind=int_wp),parameter  ::ip_al_dNm = 25
      integer(kind=int_wp),parameter  ::ip_al_dPm = 26
      integer(kind=int_wp),parameter  ::ip_au_dNm = 27
      integer(kind=int_wp),parameter  ::ip_au_dPm = 28
      integer(kind=int_wp),parameter  ::ip_al_dNs = 29
      integer(kind=int_wp),parameter  ::ip_al_dPs = 30
      integer(kind=int_wp),parameter  ::ip_au_dNs = 31
      integer(kind=int_wp),parameter  ::ip_au_dPs = 32
      integer(kind=int_wp),parameter  ::ip_a_dNpr = 33
      integer(kind=int_wp),parameter  ::ip_a_dPpr = 34
      integer(kind=int_wp),parameter  ::ip_a_dSpr = 35
      integer(kind=int_wp),parameter  ::ip_b_ni = 36
      integer(kind=int_wp),parameter  ::ip_b_poc1doc = 37
      integer(kind=int_wp),parameter  ::ip_b_poc1poc2 = 38
      integer(kind=int_wp),parameter  ::ip_b_poc2doc = 39
      integer(kind=int_wp),parameter  ::ip_b_poc2poc3 = 40
      integer(kind=int_wp),parameter  ::ip_b_poc3doc = 41
      integer(kind=int_wp),parameter  ::ip_b_poc3poc4 = 42
      integer(kind=int_wp),parameter  ::ip_b_su = 43
      integer(kind=int_wp),parameter  ::ip_kT_dec = 44
      integer(kind=int_wp),parameter  ::ip_SWOMDec = 45
      integer(kind=int_wp),parameter  ::ip_KsOxCon = 46
      integer(kind=int_wp),parameter  ::ip_KsNiDen = 47
      integer(kind=int_wp),parameter  ::ip_KsFeRed = 48
      integer(kind=int_wp),parameter  ::ip_KsSuRed = 49
      integer(kind=int_wp),parameter  ::ip_KsOxDenInh = 50
      integer(kind=int_wp),parameter  ::ip_KsNiIRdInh = 51
      integer(kind=int_wp),parameter  ::ip_KsNiSRdInh = 52
      integer(kind=int_wp),parameter  ::ip_KsSuMetInh = 53
      integer(kind=int_wp),parameter  ::ip_CoxDenInh = 54
      integer(kind=int_wp),parameter  ::ip_CoxIRedInh = 55
      integer(kind=int_wp),parameter  ::ip_CoxSRedInh = 56
      integer(kind=int_wp),parameter  ::ip_CoxMetInh = 57
      integer(kind=int_wp),parameter  ::ip_CniMetInh = 58
      integer(kind=int_wp),parameter  ::ip_RedFacDen = 59
      integer(kind=int_wp),parameter  ::ip_RedFacIRed = 60
      integer(kind=int_wp),parameter  ::ip_RedFacSRed = 61
      integer(kind=int_wp),parameter  ::ip_RedFacMet = 62
      integer(kind=int_wp),parameter  ::ip_CTBactAc = 63
      integer(kind=int_wp),parameter  ::ip_SWOxCon = 64
      integer(kind=int_wp),parameter  ::ip_TcDen = 65
      integer(kind=int_wp),parameter  ::ip_TcIRed = 66
      integer(kind=int_wp),parameter  ::ip_TcMet = 67
      integer(kind=int_wp),parameter  ::ip_TcOxCon = 68
      integer(kind=int_wp),parameter  ::ip_TcSRed = 69
      integer(kind=int_wp),parameter  ::ip_RcNit20 = 70
      integer(kind=int_wp),parameter  ::ip_TcNit = 71
      integer(kind=int_wp),parameter  ::ip_KsAmNit = 72
      integer(kind=int_wp),parameter  ::ip_KsOxNit = 73
      integer(kind=int_wp),parameter  ::ip_ZNit = 74
      integer(kind=int_wp),parameter  ::ip_Rc0NitOx = 75
      integer(kind=int_wp),parameter  ::ip_COXNIT = 76
      integer(kind=int_wp),parameter  ::ip_CTNit = 77
      integer(kind=int_wp),parameter  ::ip_RcSox20 = 78
      integer(kind=int_wp),parameter  ::ip_TcSox = 79
      integer(kind=int_wp),parameter  ::ip_Rc0Sox = 80
      integer(kind=int_wp),parameter  ::ip_CoxSUD = 81
      integer(kind=int_wp),parameter  ::ip_DisSEqFeS = 82
      integer(kind=int_wp),parameter  ::ip_RcDisS20 = 83
      integer(kind=int_wp),parameter  ::ip_TcDisS = 84
      integer(kind=int_wp),parameter  ::ip_RcPrcS20 = 85
      integer(kind=int_wp),parameter  ::ip_lKstH2S = 86
      integer(kind=int_wp),parameter  ::ip_lKstHS = 87
      integer(kind=int_wp),parameter  ::ip_TcKstH2S = 88
      integer(kind=int_wp),parameter  ::ip_TcKstHS = 89
      integer(kind=int_wp),parameter  ::ip_TcPrcS = 90
      integer(kind=int_wp),parameter  ::ip_RcMetOx20 = 91
      integer(kind=int_wp),parameter  ::ip_TcMetOx = 92
      integer(kind=int_wp),parameter  ::ip_Rc0MetOx = 93
      integer(kind=int_wp),parameter  ::ip_RcMetSu20 = 94
      integer(kind=int_wp),parameter  ::ip_TcMetSu = 95
      integer(kind=int_wp),parameter  ::ip_Rc0MetSu = 96
      integer(kind=int_wp),parameter  ::ip_CoxMet = 97
      integer(kind=int_wp),parameter  ::ip_CsuMet = 98
      integer(kind=int_wp),parameter  ::ip_KsMet = 99
      integer(kind=int_wp),parameter  ::ip_KsOxMet = 100
      integer(kind=int_wp),parameter  ::ip_KsSuMet = 101
      integer(kind=int_wp),parameter  ::ip_CTMetOx = 102
      integer(kind=int_wp),parameter  ::ip_fScEbul = 103
      integer(kind=int_wp),parameter  ::ip_FrMetGeCH4 = 104
      integer(kind=int_wp),parameter  ::ip_KadsP_20 = 105
      integer(kind=int_wp),parameter  ::ip_TCKadsP = 106
      integer(kind=int_wp),parameter  ::ip_a_OH_PO4 = 107
      integer(kind=int_wp),parameter  ::ip_fr_FeIM1 = 108
      integer(kind=int_wp),parameter  ::ip_fr_FeIM2 = 109
      integer(kind=int_wp),parameter  ::ip_fr_FeIM3 = 110
      integer(kind=int_wp),parameter  ::ip_fr_Feox = 111
      integer(kind=int_wp),parameter  ::ip_Cc_oxPsor = 112
      integer(kind=int_wp),parameter  ::ip_RcAdPO4AAP = 113
      integer(kind=int_wp),parameter  ::ip_RCprecP20 = 114
      integer(kind=int_wp),parameter  ::ip_TCprecipP = 115
      integer(kind=int_wp),parameter  ::ip_RCdissP20 = 116
      integer(kind=int_wp),parameter  ::ip_TCdissolP = 117
      integer(kind=int_wp),parameter  ::ip_EqVivDisP = 118
      integer(kind=int_wp),parameter  ::ip_RatAPandVP = 119
      integer(kind=int_wp),parameter  ::ip_RCdisAP20 = 120
      integer(kind=int_wp),parameter  ::ip_EqAPATDisP = 121
      integer(kind=int_wp),parameter  ::ip_Cc_oxVivP = 122
      integer(kind=int_wp),parameter  ::ip_KdPO4AAP = 123
      integer(kind=int_wp),parameter  ::ip_MaxPO4AAP = 124
      integer(kind=int_wp),parameter  ::ip_SWAdsP = 125
      integer(kind=int_wp),parameter  ::ip_Ceq_disSi = 126
      integer(kind=int_wp),parameter  ::ip_RCdisSi20 = 127
      integer(kind=int_wp),parameter  ::ip_TCdisSi = 128
      integer(kind=int_wp),parameter  ::ip_SWDisSi = 129
      integer(kind=int_wp),parameter  ::ip_DELT = 130
      integer(kind=int_wp),parameter  ::ip_Poros = 131
      integer(kind=int_wp),parameter  ::ip_Th_DelwaqG = 132
      integer(kind=int_wp),parameter  ::ip_OutInt = 133
      integer(kind=int_wp),parameter  ::ip_ITIME = 134
      integer(kind=int_wp),parameter  ::ip_Exp_Dif = 135
      integer(kind=int_wp),parameter  ::ip_Exp_Tur = 136
      integer(kind=int_wp),parameter  ::linsconstant = 136

      ! variables
      integer(kind=int_wp),parameter  ::ip_pH = 137
      integer(kind=int_wp),parameter  ::ip_Temp = 138
      integer(kind=int_wp),parameter  ::ip_Diflen = 139
      integer(kind=int_wp),parameter  ::ip_TurCoef = 140
      integer(kind=int_wp),parameter  ::ip_DifCoef = 141
      integer(kind=int_wp),parameter  ::ip_Depth = 142

      ! dissolved concentrations in overlying water (dissolved)
      integer(kind=int_wp),parameter  ::OFFSET_cwater = ip_Depth

      ! S1 concentrations (dissolved+particulate)
      integer(kind=int_wp),parameter  ::OFFSET_S1 = ip_Depth + nototseddis

      ! settling fluxes (particulate)
      integer(kind=int_wp),parameter  ::OFFSET_Setl = ip_Depth + nototseddis + nototsed

      ! VB fluxes
      integer(kind=int_wp),parameter  ::OFFSET_VB = ip_Depth + nototseddis + nototsed + nototsedpart
      integer(kind=int_wp),parameter  ::nflvb = 22
      integer(kind=int_wp),parameter  ::ip_fN1VBup = OFFSET_VB + 1
      integer(kind=int_wp),parameter  ::ip_fN2VBup = OFFSET_VB + 2
      integer(kind=int_wp),parameter  ::ip_fP1VBup = OFFSET_VB + 3
      integer(kind=int_wp),parameter  ::ip_fP2VBup = OFFSET_VB + 4
      integer(kind=int_wp),parameter  ::ip_fS1VBup = OFFSET_VB + 5
      integer(kind=int_wp),parameter  ::ip_fS2VBup = OFFSET_VB + 6
      integer(kind=int_wp),parameter  ::ip_fC1VBrel = OFFSET_VB + 7
      integer(kind=int_wp),parameter  ::ip_fC2VBrel = OFFSET_VB + 8
      integer(kind=int_wp),parameter  ::ip_fC3VBrel = OFFSET_VB + 9
      integer(kind=int_wp),parameter  ::ip_fC4VBrel = OFFSET_VB + 10
      integer(kind=int_wp),parameter  ::ip_fN1VBrel = OFFSET_VB + 11
      integer(kind=int_wp),parameter  ::ip_fN2VBrel = OFFSET_VB + 12
      integer(kind=int_wp),parameter  ::ip_fN3VBrel = OFFSET_VB + 13
      integer(kind=int_wp),parameter  ::ip_fN4VBrel = OFFSET_VB + 14
      integer(kind=int_wp),parameter  ::ip_fP1VBrel = OFFSET_VB + 15
      integer(kind=int_wp),parameter  ::ip_fP2VBrel = OFFSET_VB + 16
      integer(kind=int_wp),parameter  ::ip_fP3VBrel = OFFSET_VB + 17
      integer(kind=int_wp),parameter  ::ip_fP4VBrel = OFFSET_VB + 18
      integer(kind=int_wp),parameter  ::ip_fS1VBrel = OFFSET_VB + 19
      integer(kind=int_wp),parameter  ::ip_fS2VBrel = OFFSET_VB + 20
      integer(kind=int_wp),parameter  ::ip_fS3VBrel = OFFSET_VB + 21
      integer(kind=int_wp),parameter  ::ip_fS4VBrel = OFFSET_VB + 22

      ! Initialisation "fluxes"
      integer(kind=int_wp), parameter  ::OFFSET_FL = nototseddis + 101

      integer(kind=int_wp),parameter  ::lins = ip_Depth + nototseddis + nototsed + nototsedpart + nflvb

      ! input constants and variables
      real(kind=real_wp)  ::ku_dFdcC20
      real(kind=real_wp)  ::kl_dFdcC20
      real(kind=real_wp)  ::ku_dFdcN20
      real(kind=real_wp)  ::kl_dFdcN20
      real(kind=real_wp)  ::ku_dFdcP20
      real(kind=real_wp)  ::kl_dFdcP20
      real(kind=real_wp)  ::ku_dMdcC20
      real(kind=real_wp)  ::kl_dMdcC20
      real(kind=real_wp)  ::ku_dMdcN20
      real(kind=real_wp)  ::kl_dMdcN20
      real(kind=real_wp)  ::ku_dMdcP20
      real(kind=real_wp)  ::kl_dMdcP20
      real(kind=real_wp)  ::ku_dSdcC20
      real(kind=real_wp)  ::kl_dSdcC20
      real(kind=real_wp)  ::ku_dSdcN20
      real(kind=real_wp)  ::kl_dSdcN20
      real(kind=real_wp)  ::ku_dSdcP20
      real(kind=real_wp)  ::kl_dSdcP20
      real(kind=real_wp)  ::k_dprdcC20
      real(kind=real_wp)  ::k_DOCdcC20
      real(kind=real_wp)  ::al_dNf
      real(kind=real_wp)  ::al_dPf
      real(kind=real_wp)  ::au_dNf
      real(kind=real_wp)  ::au_dPf
      real(kind=real_wp)  ::al_dNm
      real(kind=real_wp)  ::al_dPm
      real(kind=real_wp)  ::au_dNm
      real(kind=real_wp)  ::au_dPm
      real(kind=real_wp)  ::al_dNs
      real(kind=real_wp)  ::al_dPs
      real(kind=real_wp)  ::au_dNs
      real(kind=real_wp)  ::au_dPs
      real(kind=real_wp)  ::a_dNpr
      real(kind=real_wp)  ::a_dPpr
      real(kind=real_wp)  ::a_dSpr
      real(kind=real_wp)  ::b_ni
      real(kind=real_wp)  ::b_poc1doc
      real(kind=real_wp)  ::b_poc1poc2
      real(kind=real_wp)  ::b_poc2doc
      real(kind=real_wp)  ::b_poc2poc3
      real(kind=real_wp)  ::b_poc3doc
      real(kind=real_wp)  ::b_poc3poc4
      real(kind=real_wp)  ::b_su
      real(kind=real_wp)  ::kT_dec
      real(kind=real_wp)  ::SWOMDec
      real(kind=real_wp)  ::KsOxCon
      real(kind=real_wp)  ::KsNiDen
      real(kind=real_wp)  ::KsFeRed
      real(kind=real_wp)  ::KsSuRed
      real(kind=real_wp)  ::KsOxDenInh
      real(kind=real_wp)  ::KsNiIRdInh
      real(kind=real_wp)  ::KsNiSRdInh
      real(kind=real_wp)  ::KsSuMetInh
      real(kind=real_wp)  ::CoxDenInh
      real(kind=real_wp)  ::CoxIRedInh
      real(kind=real_wp)  ::CoxSRedInh
      real(kind=real_wp)  ::CoxMetInh
      real(kind=real_wp)  ::CniMetInh
      real(kind=real_wp)  ::RedFacDen
      real(kind=real_wp)  ::RedFacIRed
      real(kind=real_wp)  ::RedFacSRed
      real(kind=real_wp)  ::RedFacMet
      real(kind=real_wp)  ::CTBactAc
      real(kind=real_wp)  ::SWOxCon
      real(kind=real_wp)  ::TcDen
      real(kind=real_wp)  ::TcIRed
      real(kind=real_wp)  ::TcMet
      real(kind=real_wp)  ::TcOxCon
      real(kind=real_wp)  ::TcSRed
      real(kind=real_wp)  ::RcNit20
      real(kind=real_wp)  ::TcNit
      real(kind=real_wp)  ::KsAmNit
      real(kind=real_wp)  ::KsOxNit
      real(kind=real_wp)  ::ZNit
      real(kind=real_wp)  ::Rc0NitOx
      real(kind=real_wp)  ::COXNIT
      real(kind=real_wp)  ::CTNit
      real(kind=real_wp)  ::RcSox20
      real(kind=real_wp)  ::TcSox
      real(kind=real_wp)  ::Rc0Sox
      real(kind=real_wp)  ::CoxSUD
      real(kind=real_wp)  ::DisSEqFeS
      real(kind=real_wp)  ::RcDisS20
      real(kind=real_wp)  ::TcDisS
      real(kind=real_wp)  ::RcPrcS20
      real(kind=real_wp)  ::lKstH2S
      real(kind=real_wp)  ::lKstHS
      real(kind=real_wp)  ::TcKstH2S
      real(kind=real_wp)  ::TcKstHS
      real(kind=real_wp)  ::TcPrcS
      real(kind=real_wp)  ::RcMetOx20
      real(kind=real_wp)  ::TcMetOx
      real(kind=real_wp)  ::Rc0MetOx
      real(kind=real_wp)  ::RcMetSu20
      real(kind=real_wp)  ::TcMetSu
      real(kind=real_wp)  ::Rc0MetSu
      real(kind=real_wp)  ::CoxMet
      real(kind=real_wp)  ::CsuMet
      real(kind=real_wp)  ::KsMet
      real(kind=real_wp)  ::KsOxMet
      real(kind=real_wp)  ::KsSuMet
      real(kind=real_wp)  ::CTMetOx
      real(kind=real_wp)  ::fScEbul
      real(kind=real_wp)  ::FrMetGeCH4
      real(kind=real_wp)  ::KadsP_20
      real(kind=real_wp)  ::TCKadsP
      real(kind=real_wp)  ::a_OH_PO4
      real(kind=real_wp)  ::fr_FeIM1
      real(kind=real_wp)  ::fr_FeIM2
      real(kind=real_wp)  ::fr_FeIM3
      real(kind=real_wp)  ::fr_Feox
      real(kind=real_wp)  ::Cc_oxPsor
      real(kind=real_wp)  ::RcAdPO4AAP
      real(kind=real_wp)  ::RCprecP20
      real(kind=real_wp)  ::TCprecipP
      real(kind=real_wp)  ::RCdissP20
      real(kind=real_wp)  ::TCdissolP
      real(kind=real_wp)  ::EqVivDisP
      real(kind=real_wp)  ::RatAPandVP
      real(kind=real_wp)  ::RCdisAP20
      real(kind=real_wp)  ::EqAPATDisP
      real(kind=real_wp)  ::Cc_oxVivP
      real(kind=real_wp)  ::KdPO4AAP
      real(kind=real_wp)  ::MaxPO4AAP
      real(kind=real_wp)  ::SWAdsP
      real(kind=real_wp)  ::Ceq_disSi
      real(kind=real_wp)  ::RCdisSi20
      real(kind=real_wp)  ::TCdisSi
      real(kind=real_wp)  ::SWDisSi
      real(kind=real_wp)  ::DELT
      real(kind=real_wp)  ::Poros
      real(kind=real_wp)  ::Th_DelwaqG
      integer(kind=int_wp)  ::OutInt
      integer(kind=int_wp)  ::Itime
      real(kind=real_wp)  ::Exp_Dif
      real(kind=real_wp)  ::Exp_Tur
      real(kind=real_wp)  ::pH
      real(kind=real_wp)  ::Temp
      real(kind=real_wp)  ::Diflen
      real(kind=real_wp)  ::TurCoef
      real(kind=real_wp)  ::DifCoef
      real(kind=real_wp)  ::Depth

      ! VB fluxes
      real(kind=real_wp)  ::fN1VBup
      real(kind=real_wp)  ::fN2VBup
      real(kind=real_wp)  ::fP1VBup
      real(kind=real_wp)  ::fP2VBup
      real(kind=real_wp)  ::fS1VBup
      real(kind=real_wp)  ::fS2VBup
      real(kind=real_wp)  ::fC1VBrel
      real(kind=real_wp)  ::fC2VBrel
      real(kind=real_wp)  ::fC3VBrel
      real(kind=real_wp)  ::fC4VBrel
      real(kind=real_wp)  ::fN1VBrel
      real(kind=real_wp)  ::fN2VBrel
      real(kind=real_wp)  ::fN3VBrel
      real(kind=real_wp)  ::fN4VBrel
      real(kind=real_wp)  ::fP1VBrel
      real(kind=real_wp)  ::fP2VBrel
      real(kind=real_wp)  ::fP3VBrel
      real(kind=real_wp)  ::fP4VBrel
      real(kind=real_wp)  ::fS1VBrel
      real(kind=real_wp)  ::fS2VBrel
      real(kind=real_wp)  ::fS3VBrel
      real(kind=real_wp)  ::fS4VBrel


      ! states (2 versions for particulates, local without "S1", external with "S1"
      real(kind=real_wp)  ::CH4
      real(kind=real_wp)  ::DOC
      real(kind=real_wp)  ::DON
      real(kind=real_wp)  ::DOP
      real(kind=real_wp)  ::DOS
      real(kind=real_wp)  ::NH4
      real(kind=real_wp)  ::NO3
      real(kind=real_wp)  ::OXY
      real(kind=real_wp)  ::PO4
      real(kind=real_wp)  ::Si
      real(kind=real_wp)  ::SO4
      real(kind=real_wp)  ::SUD

      real(kind=real_wp)  ::AAP
      real(kind=real_wp)  ::APATP
      real(kind=real_wp)  ::FeIIIpa
      real(kind=real_wp)  ::Opal
      real(kind=real_wp)  ::POC1
      real(kind=real_wp)  ::POC2
      real(kind=real_wp)  ::POC3
      real(kind=real_wp)  ::POC4
      real(kind=real_wp)  ::PON1
      real(kind=real_wp)  ::PON2
      real(kind=real_wp)  ::PON3
      real(kind=real_wp)  ::PON4
      real(kind=real_wp)  ::POP1
      real(kind=real_wp)  ::POP2
      real(kind=real_wp)  ::POP3
      real(kind=real_wp)  ::POP4
      real(kind=real_wp)  ::POS1
      real(kind=real_wp)  ::POS2
      real(kind=real_wp)  ::POS3
      real(kind=real_wp)  ::POS4
      real(kind=real_wp)  ::SUP
      real(kind=real_wp)  ::VIVP

      ! local declarations per parent subroutine
      ! adspo4
      real(kind=real_wp)  ::tfe     , cads    , kads    , oh      , fra     , eqaapm  , im1     , im2     , im3     , cadst   , eqaap   , fads
      ! qim1  , qim2  , qim3  , fim1  , fim2  , fim3
      ! nitrif
      real(kind=real_wp)  ::k0nit   , tempc   , amfunc  , oxfunc  , flnit, k1nit
      !Dec
      integer(kind=int_wp)  ::icfrac
      integer(kind=int_wp),parameter  ::ncfrac = 5
      real(kind=real_wp)  ::poc     , pon     , pop     , pos     , rc20upc , rc20loc , rc20upn , rc20lon , rc20upp , rc20lop , &
           aln     , alp     , aun     , aup     , b_dtp   , b_dtd   , swomd   ,                   decflx(12), &
           rc20c   , elfact  , rc20n   , rc20p   , n_fact  , p_fact  , s_fact  , fnut    , rc20s
      !Vivianit & APATITE
      real(kind=real_wp)  ::fprc    , tmpsol  , fsol    , tmpprc
      !CONSELAC
      real(kind=real_wp)  ::rtmin   , froxc   , frnic   , frfec   , frsuc   , frch4c  , fct2    , fct3    , fct4    , fct5    , &
           tempc1  , tempc2  , tempc3  , tempc4  , tempc5  , fox20   , fni20   , ffe20   , fsu20   , fch420  , &
           fox     , fni     , ffe     , fsu     , fch4    , fsum    , frox    , frni    , frfe    , frsu    , &
           frch4   , romax   , rdmax   , rimax   , rsmax   , rden    , roxc    , rired   , rsred   , conflx(6)
      !EBULCH4
      real(kind=real_wp)  ::laythick, cch4s   , dch4    , ebulfl
      !SPECSUD
      real(kind=real_wp)  ::h_ion   , ks1     , ks2     , csdt    , csd1    , csd2    , csd3    , dish2swk, dishswk , disswk  , &
           frh2sdis, frhsdis , frsdis
      !OXIDSUD
      real(kind=real_wp)  ::fluxox  , k0sox   , k1sox
      !PRECSUL
      real(kind=real_wp)  ::tempcp  , tempcd  , fluxpr  , fluxds
      !OXIDCH4
      real(kind=real_wp)  ::chfunc  , sufunc  , lifunc  , flcox   , flcsu   , k0metox , k1metox , k0metsu , k1metsu
      !     INTERFACE TO VB
      real(kind=real_wp)  ::s1_nh4, s1_no3, s1_aap, s1_po4, s1_so4, s1_sud, vbflux
      !     other
      real(kind=real_wp)  ::temp20  , thick

      ! sediment substances definition
      integer(kind=int_wp),parameter  ::is_CH4 = 1
      integer(kind=int_wp),parameter  ::is_DOC = 2
      integer(kind=int_wp),parameter  ::is_DON = 3
      integer(kind=int_wp),parameter  ::is_DOP = 4
      integer(kind=int_wp),parameter  ::is_DOS = 5
      integer(kind=int_wp),parameter  ::is_NH4 = 6
      integer(kind=int_wp),parameter  ::is_NO3 = 7
      integer(kind=int_wp),parameter  ::is_OXY = 8
      integer(kind=int_wp),parameter  ::is_PO4 = 9
      integer(kind=int_wp),parameter  ::is_Si = 10
      integer(kind=int_wp),parameter  ::is_SO4 = 11
      integer(kind=int_wp),parameter  ::is_SUD = 12
      integer(kind=int_wp),parameter  ::is_AAP = 13
      integer(kind=int_wp),parameter  ::is_APATP = 14
      integer(kind=int_wp),parameter  ::is_FeIIIpa = 15
      integer(kind=int_wp),parameter  ::is_Opal = 16
      integer(kind=int_wp),parameter  ::is_POC1 = 17
      integer(kind=int_wp),parameter  ::is_POC2 = 18
      integer(kind=int_wp),parameter  ::is_POC3 = 19
      integer(kind=int_wp),parameter  ::is_POC4 = 20
      integer(kind=int_wp),parameter  ::is_PON1 = 21
      integer(kind=int_wp),parameter  ::is_PON2 = 22
      integer(kind=int_wp),parameter  ::is_PON3 = 23
      integer(kind=int_wp),parameter  ::is_PON4 = 24
      integer(kind=int_wp),parameter  ::is_POP1 = 25
      integer(kind=int_wp),parameter  ::is_POP2 = 26
      integer(kind=int_wp),parameter  ::is_POP3 = 27
      integer(kind=int_wp),parameter  ::is_POP4 = 28
      integer(kind=int_wp),parameter  ::is_POS1 = 29
      integer(kind=int_wp),parameter  ::is_POS2 = 30
      integer(kind=int_wp),parameter  ::is_POS3 = 31
      integer(kind=int_wp),parameter  ::is_POS4 = 32
      integer(kind=int_wp),parameter  ::is_SUP = 33
      integer(kind=int_wp),parameter  ::is_VIVP = 34

      character(len=255) errorstring
      integer(kind=int_wp)  ::item, iflux, iseg, itel, noseg2d, iatt1, iatt2, ilay, isys, iseg2d, ip, ifl
      real(kind=real_wp)                  ::mass3d, sedwatflx, cwater, totmas
      integer(kind=int_wp), allocatable ::bottomsegments(:)
      real(kind=real_wp), allocatable ::sedconc(:,:,:)

      integer(kind=int_wp)                              ::nolay
      real(kind=real_wp), allocatable ::tt(:), td(:)
      real(kind=real_wp), allocatable ::dl(:)  ! layer thickness
      real(kind=real_wp), allocatable ::sd(:)  ! depth of upper surface of layer
      real(kind=real_wp), allocatable ::bd(:)  ! bottomdepth (depth of lower surface of layer)
      real(kind=dp), allocatable :: av(:,:), bv(:), rwork(:)
      real(kind=dp)              :: term
      real(kind=real_wp), allocatable ::kp(:,:), lp(:,:)
      integer(kind=int_wp), allocatable ::iwork(:)

      integer(kind=int_wp)  ::ierror

      logical :: default_profile

      character(len=160) :: moname
      character(len=20)  :: syname(nototsed) = &
                  ['CH4-pore    ', 'DOC-pore    ', 'DON-pore    ', 'DOP-pore    ', 'DOS-pore    ', 'NH4-pore    ', &
                   'NO3-pore    ', 'OXY-pore    ', 'PO4-pore    ', 'Si-pore     ', 'SO4-pore    ', 'SUD-pore    ', &
                   'AAP-bulk    ', 'APATP-bulk  ', 'FeIIIpa-bulk', 'Opal-bulk   ', 'POC1-bulk   ', 'POC2-bulk   ', &
                   'POC3-bulk   ', 'POC4-bulk   ', 'PON1-bulk   ', 'PON2-bulk   ', 'PON3-bulk   ', 'PON4-bulk   ', &
                   'POP1-bulk   ', 'POP2-bulk   ', 'POP3-bulk   ', 'POP4-bulk   ', 'POS1-bulk   ', 'POS2-bulk   ', &
                   'POS3-bulk   ', 'POS4-bulk   ', 'SUP-bulk    ', 'VIVP-bulk   ']

      integer(kind=int_wp), save       ::lumap, lurestart
      integer(kind=int_wp)             ::luinp ! Extra DELWAQG input parameters
      character(len=200) :: mapfile
      character(len=200) :: initfile
      character(len=200) :: restartfile

      logical, save :: first = .true.
      logical       :: only_ox, dissub, sw_vb

      integer(kind=int_wp), save  ::ipnt(1) = -999
      integer(kind=int_wp)        ::i

      save
!
!******************************************************************************* INITIAL PROCESSING


      if (first) then

          call initialise_layers


          ! load constants
          ku_dFdcC20 = PMSA(IPOINT(ip_ku_dFdcC20))
          kl_dFdcC20 = PMSA(IPOINT(ip_kl_dFdcC20))
          ku_dFdcN20 = PMSA(IPOINT(ip_ku_dFdcN20))
          kl_dFdcN20 = PMSA(IPOINT(ip_kl_dFdcN20))
          ku_dFdcP20 = PMSA(IPOINT(ip_ku_dFdcP20))
          kl_dFdcP20 = PMSA(IPOINT(ip_kl_dFdcP20))
          ku_dMdcC20 = PMSA(IPOINT(ip_ku_dMdcC20))
          kl_dMdcC20 = PMSA(IPOINT(ip_kl_dMdcC20))
          ku_dMdcN20 = PMSA(IPOINT(ip_ku_dMdcN20))
          kl_dMdcN20 = PMSA(IPOINT(ip_kl_dMdcN20))
          ku_dMdcP20 = PMSA(IPOINT(ip_ku_dMdcP20))
          kl_dMdcP20 = PMSA(IPOINT(ip_kl_dMdcP20))
          ku_dSdcC20 = PMSA(IPOINT(ip_ku_dSdcC20))
          kl_dSdcC20 = PMSA(IPOINT(ip_kl_dSdcC20))
          ku_dSdcN20 = PMSA(IPOINT(ip_ku_dSdcN20))
          kl_dSdcN20 = PMSA(IPOINT(ip_kl_dSdcN20))
          ku_dSdcP20 = PMSA(IPOINT(ip_ku_dSdcP20))
          kl_dSdcP20 = PMSA(IPOINT(ip_kl_dSdcP20))
          k_dprdcC20 = PMSA(IPOINT(ip_k_dprdcC20))
          k_DOCdcC20 = PMSA(IPOINT(ip_k_DOCdcC20))
          al_dNf = PMSA(IPOINT(ip_al_dNf))
          al_dPf = PMSA(IPOINT(ip_al_dPf))
          au_dNf = PMSA(IPOINT(ip_au_dNf))
          au_dPf = PMSA(IPOINT(ip_au_dPf))
          al_dNm = PMSA(IPOINT(ip_al_dNm))
          al_dPm = PMSA(IPOINT(ip_al_dPm))
          au_dNm = PMSA(IPOINT(ip_au_dNm))
          au_dPm = PMSA(IPOINT(ip_au_dPm))
          al_dNs = PMSA(IPOINT(ip_al_dNs))
          al_dPs = PMSA(IPOINT(ip_al_dPs))
          au_dNs = PMSA(IPOINT(ip_au_dNs))
          au_dPs = PMSA(IPOINT(ip_au_dPs))
          a_dNpr = PMSA(IPOINT(ip_a_dNpr))
          a_dPpr = PMSA(IPOINT(ip_a_dPpr))
          a_dSpr = PMSA(IPOINT(ip_a_dSpr))
          b_ni = PMSA(IPOINT(ip_b_ni))
          b_poc1doc = PMSA(IPOINT(ip_b_poc1doc))
          b_poc1poc2 = PMSA(IPOINT(ip_b_poc1poc2))
          b_poc2doc = PMSA(IPOINT(ip_b_poc2doc))
          b_poc2poc3 = PMSA(IPOINT(ip_b_poc2poc3))
          b_poc3doc = PMSA(IPOINT(ip_b_poc3doc))
          b_poc3poc4 = PMSA(IPOINT(ip_b_poc3poc4))
          b_su = PMSA(IPOINT(ip_b_su))
          kT_dec = PMSA(IPOINT(ip_kT_dec))
          SWOMDec = PMSA(IPOINT(ip_SWOMDec))
          KsOxCon = MAX (1.0E-06 , PMSA(IPOINT(ip_KsOxCon)) )
          KsNiDen = MAX (1.0E-06 , PMSA(IPOINT(ip_KsNiDen)) )
          KsFeRed = MAX (1.0E-06 , PMSA(IPOINT(ip_KsFeRed)) )
          KsSuRed = MAX (1.0E-06 , PMSA(IPOINT(ip_KsSuRed)) )
          KsOxDenInh = MAX (1.0E-06 , PMSA(IPOINT(ip_KsOxDenInh)) )
          KsNiIRdInh = MAX (1.0E-06 , PMSA(IPOINT(ip_KsNiIRdInh)) )
          KsNiSRdInh = MAX (1.0E-06 , PMSA(IPOINT(ip_KsNiSRdInh)) )
          KsSuMetInh = MAX (1.0E-06 , PMSA(IPOINT(ip_KsSuMetInh)) )
          CoxDenInh = PMSA(IPOINT(ip_CoxDenInh))
          CoxIRedInh = PMSA(IPOINT(ip_CoxIRedInh))
          CoxSRedInh = PMSA(IPOINT(ip_CoxSRedInh))
          CoxMetInh = PMSA(IPOINT(ip_CoxMetInh))
          CniMetInh = PMSA(IPOINT(ip_CniMetInh))
          RedFacDen = PMSA(IPOINT(ip_RedFacDen))
          RedFacIRed = PMSA(IPOINT(ip_RedFacIRed))
          RedFacSRed = PMSA(IPOINT(ip_RedFacSRed))
          RedFacMet = PMSA(IPOINT(ip_RedFacMet))
          CTBactAc = PMSA(IPOINT(ip_CTBactAc))
          SWOxCon = PMSA(IPOINT(ip_SWOxCon))
          TcDen = PMSA(IPOINT(ip_TcDen))
          TcIRed = PMSA(IPOINT(ip_TcIRed))
          TcMet = PMSA(IPOINT(ip_TcMet))
          TcOxCon = PMSA(IPOINT(ip_TcOxCon))
          TcSRed = PMSA(IPOINT(ip_TcSRed))
          RcNit20 = PMSA(IPOINT(ip_RcNit20))
          TcNit = PMSA(IPOINT(ip_TcNit))
          KsAmNit = PMSA(IPOINT(ip_KsAmNit))
          KsOxNit = PMSA(IPOINT(ip_KsOxNit))
          ZNit = PMSA(IPOINT(ip_ZNit))
          Rc0NitOx = PMSA(IPOINT(ip_Rc0NitOx))
          COXNIT = PMSA(IPOINT(ip_COXNIT))
          CTNit = PMSA(IPOINT(ip_CTNit))
          RcSox20 = PMSA(IPOINT(ip_RcSox20))
          TcSox = PMSA(IPOINT(ip_TcSox))
          Rc0Sox = PMSA(IPOINT(ip_Rc0Sox))
          CoxSUD = PMSA(IPOINT(ip_CoxSUD))
          DisSEqFeS = PMSA(IPOINT(ip_DisSEqFeS))
          RcDisS20 = PMSA(IPOINT(ip_RcDisS20))
          TcDisS = PMSA(IPOINT(ip_TcDisS))
          RcPrcS20 = PMSA(IPOINT(ip_RcPrcS20))
          lKstH2S = PMSA(IPOINT(ip_lKstH2S))
          lKstHS = PMSA(IPOINT(ip_lKstHS))
          TcKstH2S = PMSA(IPOINT(ip_TcKstH2S))
          TcKstHS = PMSA(IPOINT(ip_TcKstHS))
          TcPrcS = PMSA(IPOINT(ip_TcPrcS))
          RcMetOx20 = PMSA(IPOINT(ip_RcMetOx20))
          TcMetOx = PMSA(IPOINT(ip_TcMetOx))
          Rc0MetOx = PMSA(IPOINT(ip_Rc0MetOx))
          RcMetSu20 = PMSA(IPOINT(ip_RcMetSu20))
          TcMetSu = PMSA(IPOINT(ip_TcMetSu))
          Rc0MetSu = PMSA(IPOINT(ip_Rc0MetSu))
          CoxMet = PMSA(IPOINT(ip_CoxMet))
          CsuMet = PMSA(IPOINT(ip_CsuMet))
          KsMet = PMSA(IPOINT(ip_KsMet))
          KsOxMet = PMSA(IPOINT(ip_KsOxMet))
          KsSuMet = PMSA(IPOINT(ip_KsSuMet))
          CTMetOx = PMSA(IPOINT(ip_CTMetOx))
          fScEbul = PMSA(IPOINT(ip_fScEbul))
          FrMetGeCH4 = PMSA(IPOINT(ip_FrMetGeCH4))
          KadsP_20 = PMSA(IPOINT(ip_KadsP_20))
          TCKadsP = PMSA(IPOINT(ip_TCKadsP))
          a_OH_PO4 = PMSA(IPOINT(ip_a_OH_PO4))
          fr_FeIM1 = PMSA(IPOINT(ip_fr_FeIM1))
          fr_FeIM2 = PMSA(IPOINT(ip_fr_FeIM2))
          fr_FeIM3 = PMSA(IPOINT(ip_fr_FeIM3))
          fr_Feox = PMSA(IPOINT(ip_fr_Feox))
          Cc_oxPsor = PMSA(IPOINT(ip_Cc_oxPsor))
          RcAdPO4AAP = PMSA(IPOINT(ip_RcAdPO4AAP))
          RCprecP20 = PMSA(IPOINT(ip_RCprecP20))
          TCprecipP = PMSA(IPOINT(ip_TCprecipP))
          RCdissP20 = PMSA(IPOINT(ip_RCdissP20))
          TCdissolP = PMSA(IPOINT(ip_TCdissolP))
          EqVivDisP = PMSA(IPOINT(ip_EqVivDisP))
          RatAPandVP = PMSA(IPOINT(ip_RatAPandVP))
          RCdisAP20 = PMSA(IPOINT(ip_RCdisAP20))
          EqAPATDisP = PMSA(IPOINT(ip_EqAPATDisP))
          Cc_oxVivP = PMSA(IPOINT(ip_Cc_oxVivP))
          KdPO4AAP = PMSA(IPOINT(ip_KdPO4AAP))
          MaxPO4AAP = PMSA(IPOINT(ip_MaxPO4AAP))
          SWAdsP = PMSA(IPOINT(ip_SWAdsP))
          Ceq_disSi = PMSA(IPOINT(ip_Ceq_disSi))
          RCdisSi20 = PMSA(IPOINT(ip_RCdisSi20))
          TCdisSi = PMSA(IPOINT(ip_TCdisSi))
          SWDisSi = PMSA(IPOINT(ip_SWDisSi))

          Exp_Tur = PMSA(IPOINT(ip_Exp_Tur))
          Exp_Dif = PMSA(IPOINT(ip_Exp_Dif))

!          ! hardcoded for the moment, needs to be input
!          ! fraction of mixing 0.01 achieved at depth 0.1
!          exp_dif = Log(1./0.01)/0.1
!          exp_tur = exp_dif

          ! Fixed layer thickness and other parameters
          Th_DelwaqG = PMSA(IPOINT(ip_Th_DelwaqG))
          Delt = PMSA(IPOINT(ip_Delt))
          Poros = PMSA(IPOINT(ip_Poros))
          OutInt = nint(PMSA(IPOINT(ip_OutInt)))

          ! check on non-constants (assume numbering is from 1 to linsconstant)
          do item = 1,linsconstant
              if (increm(item)/=0) then
                  errorstring = 'Input item _____ is not supposed to be space dependent'
                  write (errorstring(12:16),'(i5)') item
                  CALL write_error_message (errorstring)
              endif
          enddo

          ! validity checks
          IF (kdpo4aap < 0.0) CALL write_error_message ('kdpo4aap negative')
          IF (a_dNpr < 1E-30) CALL write_error_message ('DECDET: a_dNpr =< 0')
          IF (a_dPpr < 1E-30) CALL write_error_message ('DECDET: a_dPpr =< 0')
          IF (a_dSpr < 1E-30) CALL write_error_message ('DECDET: a_dSpr =< 0')

          IF (al_dNf < 1E-30) CALL write_error_message ('DECDET: al_dN/F/M/S/ =< 0')
          IF (al_dPf < 1E-30) CALL write_error_message ('DECDET: al_dP/F/M/S/ =< 0')
          IF (au_dNf < 1E-30) CALL write_error_message ('DECDET: au_dN/F/M/S/ =< 0')
          IF (au_dPf < 1E-30) CALL write_error_message ('DECDET: au_dP/F/M/S/ =< 0')
          IF (al_dNm < 1E-30) CALL write_error_message ('DECDET: al_dN/F/M/S/ =< 0')
          IF (al_dPm < 1E-30) CALL write_error_message ('DECDET: al_dP/F/M/S/ =< 0')
          IF (au_dNm < 1E-30) CALL write_error_message ('DECDET: au_dN/F/M/S/ =< 0')
          IF (au_dPm < 1E-30) CALL write_error_message ('DECDET: au_dP/F/M/S/ =< 0')
          IF (al_dNs < 1E-30) CALL write_error_message ('DECDET: al_dN/F/M/S/ =< 0')
          IF (al_dPs < 1E-30) CALL write_error_message ('DECDET: al_dP/F/M/S/ =< 0')
          IF (au_dNs < 1E-30) CALL write_error_message ('DECDET: au_dN/F/M/S/ =< 0')
          IF (au_dPs < 1E-30) CALL write_error_message ('DECDET: au_dP/F/M/S/ =< 0')
!
!         Errors if upper limits =< lower limits
!
          IF (au_dNf < al_dNf)  CALL write_error_message ('DECDET: au_dN/F/M/S/ < al_dN/F/M/S/ ')
          IF (au_dPf < al_dPf)  CALL write_error_message ('DECDET: au_dP/F/M/S/ < al_dP/F/M/S/ ')
          IF (au_dNm < al_dNm)  CALL write_error_message ('DECDET: au_dN/F/M/S/ < al_dN/F/M/S/ ')
          IF (au_dPm < al_dPm)  CALL write_error_message ('DECDET: au_dP/F/M/S/ < al_dP/F/M/S/ ')
          IF (au_dNs < al_dNs)  CALL write_error_message ('DECDET: au_dN/F/M/S/ < al_dN/F/M/S/ ')
          IF (au_dPs < al_dPs)  CALL write_error_message ('DECDET: au_dP/F/M/S/ < al_dP/F/M/S/ ')

          IF (ku_dFdcC20 < kl_dFdcC20) CALL write_error_message ('DECDET: ku_d/F/M/S/dec20 < kl_d/F/M/S/dec20 ')
          IF (ku_dFdcN20 < kl_dFdcN20) CALL write_error_message ('DECDET: ku_d/F/M/S/dcN20 < kl_d/F/M/S/dcN20 ')
          IF (ku_dFdcP20 < kl_dFdcP20) CALL write_error_message ('DECDET: ku_d/F/M/S/dcP20 < kl_d/F/M/S/dcP20 ')
          IF (ku_dMdcC20 < kl_dMdcC20) CALL write_error_message ('DECDET: ku_d/F/M/S/dec20 < kl_d/F/M/S/dec20 ')
          IF (ku_dMdcN20 < kl_dMdcN20) CALL write_error_message ('DECDET: ku_d/F/M/S/dcN20 < kl_d/F/M/S/dcN20 ')
          IF (ku_dMdcP20 < kl_dMdcP20) CALL write_error_message ('DECDET: ku_d/F/M/S/dcP20 < kl_d/F/M/S/dcP20 ')
          IF (ku_dSdcC20 < kl_dSdcC20) CALL write_error_message ('DECDET: ku_d/F/M/S/dec20 < kl_d/F/M/S/dec20 ')
          IF (ku_dSdcN20 < kl_dSdcN20) CALL write_error_message ('DECDET: ku_d/F/M/S/dcN20 < kl_d/F/M/S/dcN20 ')
          IF (ku_dSdcP20 < kl_dSdcP20) CALL write_error_message ('DECDET: ku_d/F/M/S/dcP20 < kl_d/F/M/S/dcP20 ')


          ! Switches
          ONLY_OX = .FALSE.
          IF (SWOxCon > 0.5) ONLY_OX = .TRUE.
          SW_VB = .false. ! VB part only if defined
          if (increm(ip_fN1VBup)>0) SW_VB = .true.

          ! Check if sum of array dl equals input fixed layer thickness
          thick = sum(dl)
          IF (abs(thick-Th_DelwaqG)>0.01*Th_DelwaqG) CALL write_error_message ('Inconsistent layer definition (check Th_DelwaqG)')

          ! Determine 2D structure, first find dimension and next fill a mapping array
          noseg2d = 0
          do iseg = 1,noseg
              CALL evaluate_waq_attribute(1,IKNMRK(iseg),iatt1) ! pick up first attribute
              CALL evaluate_waq_attribute(2,IKNMRK(iseg),iatt2) ! pick up second attribute
              if (iatt2==0.or.iatt2==3) then
                  noseg2d = noseg2d+1
              endif
          enddo
          allocate(bottomsegments(noseg2d))
          bottomsegments = 0
          itel = 0
          do iseg = 1,noseg
              CALL evaluate_waq_attribute(1,IKNMRK(iseg),iatt1) ! pick up first attribute
              CALL evaluate_waq_attribute(2,IKNMRK(iseg),iatt2) ! pick up second attribute

              if (iatt2==0.or.iatt2==3) then
                  itel = itel+1
                  bottomsegments(itel) = iseg
              endif
          enddo

          ! create layered structure
          allocate (sedconc(nolay,nototsed,noseg2d))

          ! create initial values for sedconc; zero except parameters taken from external model
          ! substance dependent profile to be added!!
          !
          call initialise_sedconc

          first = .false.

      endif

      ! output
      Itime = nint(PMSA(IPOINT(ip_Itime)))
      if (mod(itime,outint*3600)==0) then
          call write_sedconc
      endif

      !
      ! Calculate correction fluxes - they synchronise the S1 substances with the sedconc array
      ! (required for complicated reasons)
      !
      call sync_S1_sedconc

      ! loop over bottom segments

      do iseg2d = 1,noseg2d
          iseg = bottomsegments(iseg2d) ! this is the 3D segment number (link to PMSA)

          ! Pick up variable environment
          pH      = PMSA(IPOINT(ip_pH     )+(iseg-1)*INCREM(ip_pH     ))
          Temp    = PMSA(IPOINT(ip_Temp   )+(iseg-1)*INCREM(ip_Temp   ))
          Turcoef = PMSA(IPOINT(ip_TurCoef)+(iseg-1)*INCREM(ip_TurCoef))
          DifCoef = PMSA(IPOINT(ip_DifCoef)+(iseg-1)*INCREM(ip_DifCoef))
          Diflen  = PMSA(IPOINT(ip_Diflen )+(iseg-1)*INCREM(ip_Diflen ))
          Depth   = PMSA(IPOINT(ip_Depth  )+(iseg-1)*INCREM(ip_Depth  ))
          if ( default_profile ) then
              do ilay = 1,nolay
                  tt(ilay) = turcoef*exp(-exp_tur*bd(ilay))
                  td(ilay) = difcoef*exp(-exp_dif*sd(ilay))
              enddo
          endif

          if (SW_VB) then
              ip = OFFSET_S1+is_nh4
              s1_nh4 = pmsa(ipoint(ip)+(iseg-1)*increm(ip))
              ip = OFFSET_S1+is_no3
              s1_no3 = pmsa(ipoint(ip)+(iseg-1)*increm(ip))
              ip = OFFSET_S1+is_aap
              s1_aap = pmsa(ipoint(ip)+(iseg-1)*increm(ip))
              ip = OFFSET_S1+is_po4
              s1_po4 = pmsa(ipoint(ip)+(iseg-1)*increm(ip))
              ip = OFFSET_S1+is_so4
              s1_so4 = pmsa(ipoint(ip)+(iseg-1)*increm(ip))
              ip = OFFSET_S1+is_sud
              s1_sud = pmsa(ipoint(ip)+(iseg-1)*increm(ip))

              fN1VBup = PMSA(IPOINT(ip_fN1VBup)+(iseg-1)*INCREM(ip_fN1VBup))
              fN2VBup = PMSA(IPOINT(ip_fN2VBup)+(iseg-1)*INCREM(ip_fN2VBup))
              fP1VBup = PMSA(IPOINT(ip_fP1VBup)+(iseg-1)*INCREM(ip_fP1VBup))
              fP2VBup = PMSA(IPOINT(ip_fP2VBup)+(iseg-1)*INCREM(ip_fP2VBup))
              fS1VBup = PMSA(IPOINT(ip_fS1VBup)+(iseg-1)*INCREM(ip_fS1VBup))
              fS2VBup = PMSA(IPOINT(ip_fS2VBup)+(iseg-1)*INCREM(ip_fS2VBup))

              fC1VBrel = PMSA(IPOINT(ip_fC1VBrel)+(iseg-1)*INCREM(ip_fC1VBrel))
              fC2VBrel = PMSA(IPOINT(ip_fC2VBrel)+(iseg-1)*INCREM(ip_fC2VBrel))
              fC3VBrel = PMSA(IPOINT(ip_fC3VBrel)+(iseg-1)*INCREM(ip_fC3VBrel))
              fC4VBrel = PMSA(IPOINT(ip_fC4VBrel)+(iseg-1)*INCREM(ip_fC4VBrel))
              fN1VBrel = PMSA(IPOINT(ip_fN1VBrel)+(iseg-1)*INCREM(ip_fN1VBrel))
              fN2VBrel = PMSA(IPOINT(ip_fN2VBrel)+(iseg-1)*INCREM(ip_fN2VBrel))
              fN3VBrel = PMSA(IPOINT(ip_fN3VBrel)+(iseg-1)*INCREM(ip_fN3VBrel))
              fN4VBrel = PMSA(IPOINT(ip_fN4VBrel)+(iseg-1)*INCREM(ip_fN4VBrel))
              fP1VBrel = PMSA(IPOINT(ip_fP1VBrel)+(iseg-1)*INCREM(ip_fP1VBrel))
              fP2VBrel = PMSA(IPOINT(ip_fP2VBrel)+(iseg-1)*INCREM(ip_fP2VBrel))
              fP3VBrel = PMSA(IPOINT(ip_fP3VBrel)+(iseg-1)*INCREM(ip_fP3VBrel))
              fP4VBrel = PMSA(IPOINT(ip_fP4VBrel)+(iseg-1)*INCREM(ip_fP4VBrel))
              fS1VBrel = PMSA(IPOINT(ip_fS1VBrel)+(iseg-1)*INCREM(ip_fS1VBrel))
              fS2VBrel = PMSA(IPOINT(ip_fS2VBrel)+(iseg-1)*INCREM(ip_fS2VBrel))
              fS3VBrel = PMSA(IPOINT(ip_fS3VBrel)+(iseg-1)*INCREM(ip_fS3VBrel))
              fS4VBrel = PMSA(IPOINT(ip_fS4VBrel)+(iseg-1)*INCREM(ip_fS4VBrel))
          endif

          ! update sedconc for settling
          do isys = 1,nototsedpart
              ip = offset_setl + isys
              sedwatflx = PMSA(IPOINT(ip)+(iseg-1)*INCREM(ip))  ! g/m2/d
              sedconc(1,nototseddis+isys,iseg2d) = sedconc(1,nototseddis+isys,iseg2d) + sedwatflx/dl(1)*delt ! to g/m3

              !AM: this flux is already handled via the individual sedimentation processes, so do not set it!
              !iflux = nototseddis + nofl + isys
              !fl(iflux+(iseg-1)*noflux) = sedwatflx/depth

          enddo

          kp = 0.0
          lp = 0.0
          do ifl = 1,nofl
              iflux = nototseddis + ifl
              fl(iflux+(iseg-1)*noflux) = 0.0
          enddo

          do ilay = 1,nolay
         ! Derive states from sedconc
          CH4 = max( 0.0, sedconc(ilay,is_CH4,iseg2d) )
          DOC = max( 0.0, sedconc(ilay,is_DOC,iseg2d) )
          DON = max( 0.0, sedconc(ilay,is_DON,iseg2d) )
          DOP = max( 0.0, sedconc(ilay,is_DOP,iseg2d) )
          DOS = max( 0.0, sedconc(ilay,is_DOS,iseg2d) )
          NH4 = max( 0.0, sedconc(ilay,is_NH4,iseg2d) )
          NO3 = max( 0.0, sedconc(ilay,is_NO3,iseg2d) )
          OXY = max( 0.0, sedconc(ilay,is_OXY,iseg2d) )
          PO4 = max( 0.0, sedconc(ilay,is_PO4,iseg2d) )
          Si = max( 0.0, sedconc(ilay,is_Si,iseg2d) )
          SO4 = max( 0.0, sedconc(ilay,is_SO4,iseg2d) )
          SUD = max( 0.0, sedconc(ilay,is_SUD,iseg2d) )
          AAP = max( 0.0, sedconc(ilay,is_AAP,iseg2d) )
          APATP = max( 0.0, sedconc(ilay,is_APATP,iseg2d) )
          FeIIIpa = max( 0.0, sedconc(ilay,is_FeIIIpa,iseg2d) )
          Opal = max( 0.0, sedconc(ilay,is_Opal,iseg2d) )
          POC1 = max( 0.0, sedconc(ilay,is_POC1,iseg2d) )
          POC2 = max( 0.0, sedconc(ilay,is_POC2,iseg2d) )
          POC3 = max( 0.0, sedconc(ilay,is_POC3,iseg2d) )
          POC4 = max( 0.0, sedconc(ilay,is_POC4,iseg2d) )
          PON1 = max( 0.0, sedconc(ilay,is_PON1,iseg2d) )
          PON2 = max( 0.0, sedconc(ilay,is_PON2,iseg2d) )
          PON3 = max( 0.0, sedconc(ilay,is_PON3,iseg2d) )
          PON4 = max( 0.0, sedconc(ilay,is_PON4,iseg2d) )
          POP1 = max( 0.0, sedconc(ilay,is_POP1,iseg2d) )
          POP2 = max( 0.0, sedconc(ilay,is_POP2,iseg2d) )
          POP3 = max( 0.0, sedconc(ilay,is_POP3,iseg2d) )
          POP4 = max( 0.0, sedconc(ilay,is_POP4,iseg2d) )
          POS1 = max( 0.0, sedconc(ilay,is_POS1,iseg2d) )
          POS2 = max( 0.0, sedconc(ilay,is_POS2,iseg2d) )
          POS3 = max( 0.0, sedconc(ilay,is_POS3,iseg2d) )
          POS4 = max( 0.0, sedconc(ilay,is_POS4,iseg2d) )
          SUP = max( 0.0, sedconc(ilay,is_SUP,iseg2d) )
          VIVP = max( 0.0, sedconc(ilay,is_VIVP,iseg2d) )

          TEMP20   = Temp - 20.0
          IM1 = (1.0-poros)*rhodm
          IM2 = 0.0
          IM3 = 0.0

! STARTPROC
          ! +++++++++++++++++++++++++++++++++++++++++++
          ! Existing process AdsPO4AAP
          ! +++++++++++++++++++++++++++++++++++++++++++

          !     Calculation of the total concentration of iron and
          !     the fractions of adsorbed phosphate in the inorganic matter
          !     fractions IM1-3
    !
          TFE  = fr_feim1 * IM1 + fr_feim2 * IM2 + fr_feim3 * IM3

    !     Start the calculation of the sorption flux
    !     Use one of three options
    !
          CADS  = 0.0
          CADST = 0.0
          EQAAP = 0.0
          FADS  = 0.0
          KADS  = 0.0
    !
    !     SWADSP = 0 : Instantaneous equilibrium partitioning
    !
          IF (NINT(SWADSP) == 0) THEN
              FADS  =(((AAP + PO4) / (1.0 + kdpo4aap)) - AAP) / DELT
              EQAAP = -1.0
          ENDIF
    !
    !     SWADSP = 1 : Langmuir sorption
    !
          IF (NINT(SWADSP) == 1) THEN
              IF ( (maxpo4aap >= 1E-10) .and. (kdpo4aap > 1E-10)) THEN
                 CADST = maxpo4aap * TFE
                 EQAAP = (kdpo4aap * CADST * PO4) / (kdpo4aap * PO4 + 1.0)
                 FADS  = rcadpo4aap * (EQAAP - AAP)
              ENDIF
          ENDIF
    !
    !     SWADSP = 2 : pH dependent Langmuir sorption
    !

          IF (NINT(SWADSP) == 2) THEN

    !     Calculate pH dependency (hydroxyl) and factor for redox potential
    !
               OH = 10.0**(PH-14.0)
               IF (OXY >= (Cc_oxPsor*POROS) ) THEN
                  FRA = 1.0
               ELSE
                  FRA = fr_feox
               ENDIF
    !
    !     Calculate total sorption capacity (mol/l)
    !
               CADST = FRA * TFE / (56000.0 * POROS)
    !
    !     Calculate free sorption capacity (mol/l)
    !
               CADS  = MAX( 0.0, CADST - (AAP / (31000.0 * POROS) ) )
    !
    !     Calculate temperature corrected KADS
    !
               KADS  = kadsp_20 * tckadsp**TEMP20
    !
    !     Calculate equilibrium concentration of adsorbed P (gP/m3)
    !
               IF ( ABS(CADS) < 1.E-20 ) THEN
                  EQAAP = 0.0
               ELSE
                  EQAAP = (AAP + PO4)/(1.0 + OH**a_OH_PO4/(KADS*CADS))
               ENDIF
    !
    !     Maximize EQAAP on equivalent CADST
    !
               IF ( CADS < 0.0 ) THEN
                  EQAAPM = 0.9 * CADST * (31000.0 * POROS)
                  EQAAP = EQAAPM
               ENDIF
    !
    !     Calculate the adsorption flux (gP/m3/d)
    !
               FADS  = rcadpo4aap * (EQAAP - AAP)

          ENDIF
    !
    !     Output of original process: EQAAP CADST KADS FIMi QIMi
    !     fluxes in zero order term for relevant substances
          kp(ilay,is_po4) = kp(ilay,is_po4) - fads
          kp(ilay,is_aap) = kp(ilay,is_aap) + fads

    !     store in output
          iflux = nototseddis + 1
          fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + fads*dl(ilay) /depth

          ! +++++++++++++++++++++++++++++++++++++++++++
          ! Existing process Nitrif_NH4 (CODE NEEDED CHANGE as constants were redefined ...
          ! +++++++++++++++++++++++++++++++++++++++++++

!         Set the rates according to CRTEMP and CROXY
!
          k1nit = RcNit20
          IF ( TEMP < CTNit .OR. OXY <= 0.0 ) k1nit = 0.0
!
          K0NIT = 0.0
!
          IF ( TEMP < CTNit .AND. OXY > 0.0 ) THEN
                  K0NIT = znit
          ELSEIF ( TEMP >= CTNit .AND. OXY <= 0.0 ) THEN
                  K0NIT = Rc0NitOx
          ENDIF
!
          IF ( OXY <= (COXNit * POROS) ) K0NIT = 0.0
!
!         Calculate the nitrification flux
!
          TEMPC  = TCNit ** TEMP20
          AMFUNC = NH4 / ( ksamnit * POROS + NH4 )
          OXFUNC = OXY / ( KSOXnit * POROS + OXY )
          FLNIT  = K0NIT + k1nit * TEMPC * AMFUNC * OXFUNC

!         maximise on the availebility of DO and NH4 with safety margin 0.5/0.9

          FLNIT     = MIN(FLNIT,0.5*OXY/NOX_RATIO/DELT)
          FLNIT     = MIN(FLNIT,0.9*NH4/DELT)

    !     Output of original process: OXFUNC
          kp(ilay,is_nh4) = kp(ilay,is_nh4) - flnit
          kp(ilay,is_no3) = kp(ilay,is_no3) + flnit
          kp(ilay,is_oxy) = kp(ilay,is_oxy) - flnit * 4.571
    !     store in output
          iflux = nototseddis + 2
          fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + flnit*dl(ilay) /depth

          ! +++++++++++++++++++++++++++++++++++++++++++
          ! Existing process DecFast DecMedium DecSlow DecRefr DecDOC
          ! +++++++++++++++++++++++++++++++++++++++++++

          rtmin = 0.0
          do icfrac = 1,ncfrac  ! loop over 5 organic carbon fractions
                if (icfrac==1) then
                    ! POC1
                        POC     = MAX(POC1,0.0)
                        PON     = MAX(PON1,0.0)
                        POP     = MAX(POP1,0.0)
                        POS     = MAX(POS1,0.0)
                        RC20UPC = ku_dFdcC20
                        RC20LOC = kl_dFdcC20
                        RC20UPN = ku_dFdcN20
                        RC20LON = kl_dFdcN20
                        RC20UPP = ku_dFdcP20
                        RC20LOP = kl_dFdcP20
                        ALN     = al_dNf
                        ALP     = al_dPf
                        AUN     = au_dNf
                        AUP     = au_dPf
                        B_DTP   = b_poc1poc2
                        B_DTD   = b_poc1doc
                        SWOMD   = swomdec

                elseif (icfrac==2) then
                    ! POC2
                        POC     = MAX(POC2,0.0)
                        PON     = MAX(PON2,0.0)
                        POP     = MAX(POP2,0.0)
                        POS     = MAX(POS2,0.0)
                        RC20UPC = ku_dMdcC20
                        RC20LOC = kl_dMdcC20
                        RC20UPN = ku_dMdcN20
                        RC20LON = kl_dMdcN20
                        RC20UPP = ku_dMdcP20
                        RC20LOP = kl_dMdcP20
                        ALN     = al_dNm
                        ALP     = al_dPm
                        AUN     = au_dNm
                        AUP     = au_dPm
                        B_DTP   = b_poc2poc3
                        B_DTD   = b_poc2doc
                        SWOMD   = swomdec

                elseif (icfrac==3) then
                    ! POC3
                        POC     = MAX(POC3,0.0)
                        PON     = MAX(PON3,0.0)
                        POP     = MAX(POP3,0.0)
                        POS     = MAX(POS3,0.0)
                        RC20UPC = ku_dSdcC20
                        RC20LOC = kl_dSdcC20
                        RC20UPN = ku_dSdcN20
                        RC20LON = kl_dSdcN20
                        RC20UPP = ku_dSdcP20
                        RC20LOP = kl_dSdcP20
                        ALN     = al_dNs
                        ALP     = al_dPs
                        AUN     = au_dNs
                        AUP     = au_dPs
                        B_DTP   = b_poc3poc4
                        B_DTD   = b_poc3doc
                        SWOMD   = swomdec

                elseif (icfrac==4) then
                    ! POC4
                        POC     = MAX(POC4,0.0)
                        PON     = MAX(PON4,0.0)
                        POP     = MAX(POP4,0.0)
                        POS     = MAX(POS4,0.0)
                        RC20UPC = k_dprdcC20
                        RC20LOC = RC20UPC
                        RC20UPN = RC20UPC
                        RC20LON = RC20UPC
                        RC20UPP = RC20UPC
                        RC20LOP = RC20UPC
                        ALN     = 1.E-20
                        ALP     = 1.E-20
                        AUN     = 1.E-20
                        AUP     = 1.E-20
                        B_DTP   = 0.0
                        B_DTD   = 0.0
                        SWOMD   = 1.0

                elseif (icfrac==5) then
                    ! DOC
                        POC     = MAX(DOC,0.0)
                        PON     = MAX(DON,0.0)
                        POP     = MAX(DOP,0.0)
                        POS     = MAX(DOS,0.0)
                        RC20UPC = k_DOCdcC20
                        RC20LOC = RC20UPC
                        RC20UPN = RC20UPC
                        RC20LON = RC20UPC
                        RC20UPP = RC20UPC
                        RC20LOP = RC20UPC
                        ALN     = 1.E-20
                        ALP     = 1.E-20
                        AUN     = 1.E-20
                        AUP     = 1.E-20
                        B_DTP   = 0.0
                        B_DTD   = 0.0
                        SWOMD   = 1.0

                endif
!
!           If  detritus = 0 : set fluxes to zero and skip algorithm
            IF (POC < 1E-10)  THEN
!
              decflx = 0.0

              RC20C  = 0.0
              TEMPC  = 1.0
              ELFACT = 1.0
              RC20N  = 0.0
              RC20P  = 0.0
              N_FACT = 1.0
              P_FACT = 1.0
              S_FACT = 1.0
!
            ELSE
!
!             Calculate degrad. rate at 20oC for current stochiometry
!
              IF ((PON/POC) > AUN .AND. (POP/POC) > AUP) THEN
!
!                -- both stoch's above upper limit
!
                 RC20C = RC20UPC
                 RC20N = RC20UPN
                 RC20P = RC20UPP
!
              ELSE IF ((PON/POC) < ALN .OR. (POP/POC) < ALP)  THEN
!
!                -- one or both stoch's below lower limit
!
                 RC20C = RC20LOC
                 RC20N = RC20LON
                 RC20P = RC20LOP
!
              ELSE
!
!                -- both stoch's between upper and lower limit
!                   or one stoch above ul and one between ul and ll
!
                 IF (AUN == ALN .OR. AUP == ALP) THEN
                    FNUT = 0.5
                 ELSE
                    FNUT = MIN( ((PON/POC)-ALN) / (AUN-ALN) , &
                                ((POP/POC)-ALP) / (AUP-ALP) )
                 ENDIF
!
                 RC20C = RC20LOC + FNUT * (RC20UPC-RC20LOC)
                 RC20N = RC20LON + FNUT * (RC20UPN-RC20LON)
                 RC20P = RC20LOP + FNUT * (RC20UPP-RC20LOP)
!
              ENDIF
!
              RC20S = RC20C
!
!             Calculate correction factors
!             for temperature
!
              TEMPC = kt_dec**TEMP20
!
!             for dominant electron acceptor
!
              IF (OXY > 0.1) THEN
                 ELFACT = 1.0
              ELSE IF (NO3 > 0.1) THEN
                 ELFACT = b_ni
              ELSE
                 ELFACT = b_su
              ENDIF
!
!             for nutrient stripping
!
              IF (NINT(SWOMD) == 0 ) THEN
                 N_FACT = 1.0 + ((PON/POC) - a_dNpr) / a_dNpr
                 P_FACT = 1.0 + ((POP/POC) - a_dPpr) / a_dPpr
                 S_FACT = 1.0 + ((POS/POC) - a_dSpr) / a_dSpr
                 N_FACT = MAX(N_FACT,0.5)
                 P_FACT = MAX(P_FACT,0.5)
                 S_FACT = MAX(S_FACT,0.5)
                 N_FACT = MIN(N_FACT,5.0)
                 P_FACT = MIN(P_FACT,5.0)
                 S_FACT = MIN(S_FACT,5.0)
              ELSE
                 N_FACT = 1.0
                 P_FACT = 1.0
                 S_FACT = 1.0
              ENDIF
!
!             Calculate the fluxes for mineralization and conversion
!

              decflx(9) = RC20C * TEMPC * ELFACT * POC ! DECOC
              decflx(1) = B_DTP * decflx(9) ! CNVPC
              decflx(5) = B_DTD * decflx(9) ! CNVDC
!
              decflx(10) = RC20N * TEMPC * ELFACT * N_FACT * PON
              decflx(2) = (1/N_FACT) * B_DTP * decflx(10)
              decflx(6) = (1/N_FACT) * B_DTD * decflx(10)
!
              decflx(11) = RC20P * TEMPC * ELFACT * P_FACT * POP
              decflx(3) = (1/P_FACT) * B_DTP * decflx(11)
              decflx(7) = (1/P_FACT) * B_DTD * decflx(11)
!
              decflx(12) = RC20S * TEMPC * ELFACT * S_FACT * POS
              decflx(4) = (1/S_FACT) * B_DTP * decflx(12)
              decflx(8) = (1/S_FACT) * B_DTD * decflx(12)

            ENDIF

            rtmin  = rtmin + decflx(9) ! accumulate C mineralisation for later processing in CONSELAC
!
    !       Output of original process: RC20C*TEMPC*ELFACT RC20N*TEMPC*ELFACT RC20P*TEMPC*ELFACT N_FACT P_FACT S_FACT DECOC

            if (icfrac==1) then
                !    ! POC1
                kp(ilay,is_poc1) = kp(ilay,is_poc1) - decflx(1)
                kp(ilay,is_poc2) = kp(ilay,is_poc2) + decflx(1)
                kp(ilay,is_pon1) = kp(ilay,is_pon1) - decflx(2)
                kp(ilay,is_pon2) = kp(ilay,is_pon2) + decflx(2)
                kp(ilay,is_pop1) = kp(ilay,is_pop1) - decflx(3)
                kp(ilay,is_pop2) = kp(ilay,is_pop2) + decflx(3)
                kp(ilay,is_pos1) = kp(ilay,is_pos1) - decflx(4)
                kp(ilay,is_pos2) = kp(ilay,is_pos2) + decflx(4)
                kp(ilay,is_poc1) = kp(ilay,is_poc1) - decflx(5)
                kp(ilay,is_doc ) = kp(ilay,is_doc ) + decflx(5)
                kp(ilay,is_pon1) = kp(ilay,is_pon1) - decflx(6)
                kp(ilay,is_don ) = kp(ilay,is_don ) + decflx(6)
                kp(ilay,is_pop1) = kp(ilay,is_pop1) - decflx(7)
                kp(ilay,is_dop ) = kp(ilay,is_dop ) + decflx(7)
                kp(ilay,is_pos1) = kp(ilay,is_pos1) - decflx(8)
                kp(ilay,is_dos ) = kp(ilay,is_dos ) + decflx(8)
                kp(ilay,is_poc1) = kp(ilay,is_poc1) - decflx(9)
                kp(ilay,is_pon1) = kp(ilay,is_pon1) - decflx(10)
                kp(ilay,is_nh4 ) = kp(ilay,is_nh4 ) + decflx(10)
                kp(ilay,is_pop1) = kp(ilay,is_pop1) - decflx(11)
                kp(ilay,is_po4 ) = kp(ilay,is_po4 ) + decflx(11)
                kp(ilay,is_pos1) = kp(ilay,is_pos1) - decflx(12)
                kp(ilay,is_sud ) = kp(ilay,is_sud ) + decflx(12)

          !     store in output (fluxes 3-14)
                do ifl = 1,12
                  iflux = nototseddis + 2 + ifl
                  fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + decflx(ifl)*dl(ilay) /depth
                enddo

            elseif (icfrac==2) then
                !    ! POC2
                kp(ilay,is_poc2) = kp(ilay,is_poc2) - decflx(1)
                kp(ilay,is_poc3) = kp(ilay,is_poc3) + decflx(1)
                kp(ilay,is_pon2) = kp(ilay,is_pon2) - decflx(2)
                kp(ilay,is_pon3) = kp(ilay,is_pon3) + decflx(2)
                kp(ilay,is_pop2) = kp(ilay,is_pop2) - decflx(3)
                kp(ilay,is_pop3) = kp(ilay,is_pop3) + decflx(3)
                kp(ilay,is_pos2) = kp(ilay,is_pos2) - decflx(4)
                kp(ilay,is_pos3) = kp(ilay,is_pos3) + decflx(4)
                kp(ilay,is_poc2) = kp(ilay,is_poc2) - decflx(5)
                kp(ilay,is_doc ) = kp(ilay,is_doc ) + decflx(5)
                kp(ilay,is_pon2) = kp(ilay,is_pon2) - decflx(6)
                kp(ilay,is_don ) = kp(ilay,is_don ) + decflx(6)
                kp(ilay,is_pop2) = kp(ilay,is_pop2) - decflx(7)
                kp(ilay,is_dop ) = kp(ilay,is_dop ) + decflx(7)
                kp(ilay,is_pos2) = kp(ilay,is_pos2) - decflx(8)
                kp(ilay,is_dos ) = kp(ilay,is_dos ) + decflx(8)
                kp(ilay,is_poc2) = kp(ilay,is_poc2) - decflx(9)
                kp(ilay,is_pon2) = kp(ilay,is_pon2) - decflx(10)
                kp(ilay,is_nh4 ) = kp(ilay,is_nh4 ) + decflx(10)
                kp(ilay,is_pop2) = kp(ilay,is_pop2) - decflx(11)
                kp(ilay,is_po4 ) = kp(ilay,is_po4 ) + decflx(11)
                kp(ilay,is_pos2) = kp(ilay,is_pos2) - decflx(12)
                kp(ilay,is_sud ) = kp(ilay,is_sud ) + decflx(12)

          !     store in output (fluxes 15-26
                do ifl = 1,12
                  iflux = nototseddis + 14 + ifl
                  fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + decflx(ifl)*dl(ilay) /depth
                enddo

            elseif (icfrac==3) then
                !    ! POC3
                kp(ilay,is_poc3) = kp(ilay,is_poc3) - decflx(1)
                kp(ilay,is_poc4) = kp(ilay,is_poc4) + decflx(1)
                kp(ilay,is_pon3) = kp(ilay,is_pon3) - decflx(2)
                kp(ilay,is_pon4) = kp(ilay,is_pon4) + decflx(2)
                kp(ilay,is_pop3) = kp(ilay,is_pop3) - decflx(3)
                kp(ilay,is_pop4) = kp(ilay,is_pop4) + decflx(3)
                kp(ilay,is_pos3) = kp(ilay,is_pos3) - decflx(4)
                kp(ilay,is_pos4) = kp(ilay,is_pos4) + decflx(4)
                kp(ilay,is_poc3) = kp(ilay,is_poc3) - decflx(5)
                kp(ilay,is_doc ) = kp(ilay,is_doc ) + decflx(5)
                kp(ilay,is_pon3) = kp(ilay,is_pon3) - decflx(6)
                kp(ilay,is_don ) = kp(ilay,is_don ) + decflx(6)
                kp(ilay,is_pop3) = kp(ilay,is_pop3) - decflx(7)
                kp(ilay,is_dop ) = kp(ilay,is_dop ) + decflx(7)
                kp(ilay,is_pos3) = kp(ilay,is_pos3) - decflx(8)
                kp(ilay,is_dos ) = kp(ilay,is_dos ) + decflx(8)
                kp(ilay,is_poc3) = kp(ilay,is_poc3) - decflx(9)
                kp(ilay,is_pon3) = kp(ilay,is_pon3) - decflx(10)
                kp(ilay,is_nh4 ) = kp(ilay,is_nh4 ) + decflx(10)
                kp(ilay,is_pop3) = kp(ilay,is_pop3) - decflx(11)
                kp(ilay,is_po4 ) = kp(ilay,is_po4 ) + decflx(11)
                kp(ilay,is_pos3) = kp(ilay,is_pos3) - decflx(12)
                kp(ilay,is_sud ) = kp(ilay,is_sud ) + decflx(12)
                !
          !     store in output (fluxes 27-38
                do ifl = 1,12
                  iflux = nototseddis + 26 + ifl
                  fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + decflx(ifl)*dl(ilay) /depth
                enddo

            elseif (icfrac==4) then
                !    ! POC4
                kp(ilay,is_poc4) = kp(ilay,is_poc4) - decflx(9)
                kp(ilay,is_pon4) = kp(ilay,is_pon4) - decflx(10)
                kp(ilay,is_nh4 ) = kp(ilay,is_nh4 ) + decflx(10)
                kp(ilay,is_pop4) = kp(ilay,is_pop4) - decflx(11)
                kp(ilay,is_po4 ) = kp(ilay,is_po4 ) + decflx(11)
                kp(ilay,is_pos4) = kp(ilay,is_pos4) - decflx(12)
                kp(ilay,is_sud ) = kp(ilay,is_sud ) + decflx(12)

          !     store in output (fluxes 39-50
                do ifl = 1,12
                  iflux = nototseddis + 38 + ifl
                  fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + decflx(ifl)*dl(ilay) /depth
                enddo
                !
            elseif (icfrac==5) then
                !    ! DOC
                kp(ilay,is_doc) = kp(ilay,is_doc) - decflx(9)
                kp(ilay,is_don) = kp(ilay,is_don) - decflx(10)
                kp(ilay,is_nh4) = kp(ilay,is_nh4) + decflx(10)
                kp(ilay,is_dop) = kp(ilay,is_dop) - decflx(11)
                kp(ilay,is_po4) = kp(ilay,is_po4) + decflx(11)
                kp(ilay,is_dos) = kp(ilay,is_dos) - decflx(12)
                kp(ilay,is_sud) = kp(ilay,is_sud) + decflx(12)

          !     store in output (fluxes 51-62
                do ifl = 1,12
                  iflux = nototseddis + 50 + ifl
                  fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + decflx(ifl)*dl(ilay) /depth
                enddo
                !
            endif
          enddo   ! end loop over carbon fractions

          ! +++++++++++++++++++++++++++++++++++++++++++
          ! Existing process Vivianite
          ! +++++++++++++++++++++++++++++++++++++++++++

!         Calculation of the precipitation or dissolution flux
!         dependent on dissolved oxygen
!
          IF ( OXY >= Cc_oxVivP ) THEN
            FPRC   = 0.0
            TMPSOL = TCdissolP**TEMP20
            FSOL   = RCdissP20 * TMPSOL * vivp * OXY / POROS
            IF ( FSOL < 0.0) FSOL = 0.0
            IF ( FSOL*DELT >= vivp) FSOL = 0.5 * vivp / DELT
          ELSE
            FSOL   = 0.0
            TMPPRC = TCprecipP**TEMP20
            FPRC   = RCprecP20 * TMPPRC * ( po4 / POROS - EqVivDisP ) * POROS
            IF ( FPRC < 0.0) FPRC = 0.0
          ENDIF
!
    !     Output of original process: none
          kp(ilay,is_po4 ) = kp(ilay,is_po4 ) - fprc + fsol
          kp(ilay,is_vivp) = kp(ilay,is_vivp) + fprc - fsol
    !     store in output
          iflux = nototseddis + 63
          fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + fprc*dl(ilay) /depth
          iflux = nototseddis + 64
          fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + fsol*dl(ilay) /depth

          ! +++++++++++++++++++++++++++++++++++++++++++
          ! Existing process DisSi
          ! +++++++++++++++++++++++++++++++++++++++++++

!         Calculation of the dissolution flux
!
          FSOL = 0.0
!
          IF (POROS > 0.05) THEN
!
            TEMPC = TCdisSi**TEMP20
!
            IF (NINT(SWDISSi) == 0) THEN
               FSOL  = RCdisSi20 * TEMPC * OPAL * ( Ceq_disSi - Si / POROS )
            ELSE
               FSOL  = RCdisSi20 * TEMPC * OPAL
            ENDIF
!
          ELSE
            FSOL  = 0.0
          ENDIF
!
    !     Output of original process: fsol
          kp(ilay,is_opal) = kp(ilay,is_opal) - fsol
          kp(ilay,is_si  ) = kp(ilay,is_si  ) + fsol
    !     store in output
          iflux = nototseddis + 65
          fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + fsol*dl(ilay) /depth

          ! +++++++++++++++++++++++++++++++++++++++++++
          ! Existing process CONSELAC
          ! +++++++++++++++++++++++++++++++++++++++++++
!
          IF (ONLY_OX) THEN
              FROXC = 1.
              FRNIC = 0.
              FRFEC = 0.
              FRSUC = 0.
              FRCH4C = 0.
          ELSE
!
!           Calculation of the temperature dependency coefficients
!
            IF ( TEMP >= CTBactAc ) THEN
                  FCT2 = 1.0
                  FCT3 = 1.0
                  FCT4 = 1.0
                  FCT5 = 1.0
            ELSE
                  FCT2 = RedFacDen
                  FCT3 = RedFacIRed
                  FCT4 = RedFacSRed
                  FCT5 = RedFacMet
            ENDIF
!
            TEMPC1 = TcOxCon ** TEMP20
            TEMPC2 = FCT2 * TcDen ** TEMP20
            TEMPC3 = FCT3 * TcIRed ** TEMP20
            TEMPC4 = FCT4 * TcSRed ** TEMP20
            TEMPC5 = FCT5 * TcMet ** TEMP20
!
!           Calculation of the unscaled (relative) contributions
!
            FOX20  = oxy / (KsOxCon * POROS + oxy)
!
            FNI20  = ( no3 / (KsNiDen * POROS + no3) ) * &
                     ( 1 - oxy / (KsOxDenInh * POROS + oxy) )
!
            FFE20  = ( feiiipa / (KsFeRed * POROS + feiiipa) ) * &
                     ( 1 - no3 / (KsNiIRdInh * POROS + no3) )
!
            FSU20  = ( so4 / (KsSuRed * POROS + so4) ) * &
                     ( 1 - no3 / (KsNiSRdInh * POROS + no3) )
!
            FCH420 = 1 - so4 / (KsSuMetInh * POROS + so4)
!
!           Adjust for temperature dependency.
!
            FOX    = FOX20  * TEMPC1
            FNI    = FNI20  * TEMPC2
            FFE    = FFE20  * TEMPC3
            FSU    = FSU20  * TEMPC4
            FCH4   = FCH420 * TEMPC5
!
!           Correction of the unscaled contributions for too high
!           dissolved oxygen or nitrate concentrations to allow
!           sulphate reduction or methanogenesis.
!
            IF ( oxy >= (CoxDenInh*POROS) )  FNI = 0.0
            IF ( oxy >= (CoxIRedInh*POROS) )  FFE = 0.0
            IF ( oxy >= (CoxSRedInh*POROS) )  FSU = 0.0
            IF ( oxy >= (CoxMetInh*POROS) .OR.  &
                 no3 >= (CniMetInh*POROS) )  FCH4 = 0.0
!
!           Calculation of the scaled contributions
!
            FSUM  = FOX + FNI + FFE + FSU + FCH4
            FROX  = FOX / FSUM
            FRNI  = FNI / FSUM
            FRFE  = FFE / FSUM
            FRSU  = FSU / FSUM
            FRCH4 = 1 - (FROX + FRNI + FRFE + FRSU)
!
!           Calculate the maximal and proposed consumption fluxes in Carbon equivalents of
!           the electron acceptors oxygen, nitrate and sulphate
!           use a safety margin 0.5 on oxygen 0.9 on the rest
!
            ROMAX = oxy / OXC_RATIO / DELT * 0.5
            RDMAX = no3 / NIC_RATIO / DELT * 0.9
            RIMAX = feiiipa / FEC_RATIO / DELT * 0.9
            RSMAX = so4 / SUC_RATIO / DELT * 0.9

!
!           Correct scaled contributions for availability of electron
!           acceptors nitrate, oxygen, iron and sulphate
!
            RDEN  = FRNI  * RTMIN
            IF ( RDEN > RDMAX .AND. RDEN > 0.0 ) THEN
                   FRNIC = FRNI * RDMAX / RDEN
                   FROX  = FROX + FRNI - FRNIC
            ELSE
                   FRNIC = FRNI
            ENDIF
!
            ROXC  = FROX  * RTMIN
            IF ( ROXC > ROMAX .AND. ROXC > 0.0 ) THEN
                   FROXC = FROX * ROMAX / ROXC
                   FRFE  = FRFE + FROX - FROXC
            ELSE
                   FROXC = FROX
            ENDIF
!
            RIRED  = FRFE  * RTMIN
            IF ( RIRED > RIMAX .AND. RIRED > 0.0 ) THEN
                   FRFEC = FRFE * RIMAX / RIRED
                   FRSU  = FRSU + FRFE - FRFEC
            ELSE
                   FRFEC = FRFE
            ENDIF
!
            RSRED  = FRSU  * RTMIN
            IF ( RSRED > RSMAX .AND. RSRED > 0.0 ) THEN
                   FRSUC = FRSU * RSMAX / RSRED
            ELSE
                   FRSUC = FRSU
            ENDIF
!
            FRCH4C = 1 - (FROXC + FRNIC + FRFEC + FRSUC)

!           endif ONLY_OX
          ENDIF

          conflx(1) = FROXC  * RTMIN
          conflx(2) = FRNIC  * RTMIN
          conflx(3) = FRFEC  * RTMIN
          conflx(4) = FRSUC  * RTMIN
          conflx(5) = FRCH4C * FrMetGeCH4 * RTMIN
          conflx(6) = FRCH4C * (1.-FrMetGeCH4) * RTMIN

          !    Output of original process: FROXC FRNIC FRFEC FRSUC FRCH4C

          kp(ilay,is_oxy    ) = kp(ilay,is_oxy    ) - conflx(1) * 2.667
          kp(ilay,is_no3    ) = kp(ilay,is_no3    ) - conflx(2) * 0.933
          kp(ilay,is_feIIIpa) = kp(ilay,is_feIIIpa) - conflx(3) * 18.6
         ! kp(ilay,is_feIId ) = kp(ilay,is_feIId  ) + conflx(3) * 18.6     TODO
          kp(ilay,is_so4    ) = kp(ilay,is_so4    ) - conflx(4) * 1.333
          kp(ilay,is_sud    ) = kp(ilay,is_sud    ) + conflx(4) * 1.333
          kp(ilay,is_ch4    ) = kp(ilay,is_ch4    ) + conflx(5)

          !     store in output
          do ifl = 1,6
              iflux = nototseddis + 65 + ifl
              fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + conflx(ifl)*dl(ilay) /depth
          enddo

          ! +++++++++++++++++++++++++++++++++++++++++++
          ! Existing process EBULCH4
          ! +++++++++++++++++++++++++++++++++++++++++++

          !     -----Warnings-----
!
          laythick  = dl(ilay)
!
!         Calculate the saturation concentration
!
          CCH4S  = 18.76 * (1 + laythick/10.0) * (1.024**(20 - TEMP))  !! is dit goed?
          DCH4   = CH4 / POROS - CCH4S
!
!         Calculate the ebullition flux
!
          IF ( DCH4 < 0.0 ) THEN
              ebulfl = 0.0
          ELSE
              ebulfl = ( fScEbul * DCH4 ) / DELT
          ENDIF
!
    !     Output of original proces: CCH4S
          kp(ilay,is_ch4) = kp(ilay,is_ch4) - ebulfl
    !     store in output
          iflux = nototseddis + 72
          fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + ebulfl*dl(ilay) /depth

          ! +++++++++++++++++++++++++++++++++++++++++++
          !     Existing process specsud
          ! +++++++++++++++++++++++++++++++++++++++++++

          if ( sud > 1e-20 ) then

            ! speciation

            h_ion      = 10.**(-ph)
            ks1        = 10.**lksth2s * tcksth2s**temp20
            ks2        = 10.**lksths  * tcksths**temp20
            csdt       = sud/(32000.*poros)
            csd1       = csdt/(1.+ks1/h_ion+(ks1*ks2)/(h_ion*h_ion))
            csd2       = ks1*csd1/h_ion
            csd3       = csdt - csd1 - csd2

            dish2swk   = csd1
            dishswk    = csd2
            disswk     = csd3
            frh2sdis   = csd1/csdt
            frhsdis    = csd2/csdt
            frsdis     = 1.0 - frh2sdis - frhsdis
            if ( frsdis < 0.0 ) then
               frsdis = csd3/csdt
            endif

          else

            dish2swk   = 0.0
            dishswk    = 0.0
            disswk     = 0.0
            frh2sdis   = 0.0
            frhsdis    = 0.0
            frsdis     = 0.0

          endif

    !     Output of original proces:  dish2swk dishswk disswk frh2sdis frhsdis frsdis

          ! +++++++++++++++++++++++++++++++++++++++++++
          ! Existing process OXIDSUD code changes to avoid redefinition of constants
          ! +++++++++++++++++++++++++++++++++++++++++++

!         Set the rates according to the DO concentration
!
          k0sox = Rc0Sox
          k1sox = RcSox20
          IF ( oxy <= 0.0 ) THEN
                k1sox  = 0.0
          ENDIF
          IF ( oxy > (CoxSUD * POROS) ) THEN
                k0sox = 0.0
          ENDIF
!
!         Calculate the sulphide oxidation flux
!
          TEMPC  = TcSox ** TEMP20
!
          FLUXOX = k0sox + k1sox * TEMPC * SUD * oxy / POROS
          FLUXOX = MIN(FLUXOX,0.9*SUD/DELT)
          FLUXOX = MIN(FLUXOX,0.5*oxy/2.0/DELT)

    !     Output of original proces:  -
          kp(ilay,is_sud) = kp(ilay,is_sud) - fluxox
          kp(ilay,is_oxy) = kp(ilay,is_oxy) - fluxox * 2.
          kp(ilay,is_so4) = kp(ilay,is_so4) + fluxox
    !     store in output
          iflux = nototseddis + 73
          fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + fluxox*dl(ilay) /depth

          ! +++++++++++++++++++++++++++++++++++++++++++
          ! Existing process PRECSUL
          ! +++++++++++++++++++++++++++++++++++++++++++

!         Calculate the precipitation and dissolution fluxes
!         The constant 32000 concerns conversion mole/l to gS/m3
!
          TEMPCP = TcPrcS ** TEMP20
          TEMPCD = TcDisS ** TEMP20
!
          FLUXPR = 32000.0 * RcPrcS20 * TEMPCP * (DisSWK - DisSEqFeS) * POROS
          FLUXDS = 32000.0 * RcDisS20 * TEMPCD * (DisSEqFeS - DisSWK) * POROS
!
!         Correct fluxes depending on under- or supersaturation
!
          IF ( FLUXPR < 0.0) FLUXPR = 0.0
          IF ( FLUXDS < 0.0) FLUXDS = 0.0
          IF ( FLUXDS*DELT >= SUP) FLUXDS = 0.5 * SUP / DELT
!
    !     Output of original proces:  -
          kp(ilay,is_sud) = kp(ilay,is_sud) - fluxpr + fluxds
          kp(ilay,is_sup) = kp(ilay,is_sup) + fluxpr - fluxds
    !     store in output
          iflux = nototseddis + 74
          fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + fluxpr*dl(ilay) /depth
          iflux = nototseddis + 75
          fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + fluxds*dl(ilay) /depth

          ! +++++++++++++++++++++++++++++++++++++++++++
          ! Existing process OXIDCH4
          ! +++++++++++++++++++++++++++++++++++++++++++
!
!         Set the rates according to CRTEMP, COXC and CSUC
!
          k0metox = Rc0MetOx
          k1metox = RcMetOx20
          IF ( TEMP < CTMetOx .OR. &
               oxy <= 0.0 .OR. oxy <= (CoxMet * POROS) ) THEN
               k1metox  = 0.0
          ENDIF
          IF ( oxy <= 0.0 .OR. oxy > (CoxMet * POROS) ) THEN
               k0metox = 0.0
          ENDIF
!
          k0metsu = Rc0MetSu
          k1metsu = RcMetSu20
          IF ( TEMP < CTMetOx .OR. &
                so4 <= 0.0 .OR. so4 <= (CsuMet * POROS) ) THEN
                k1metsu  = 0.0
          ENDIF
          IF ( so4 <= 0.0 .OR. so4 > (CsuMet * POROS) ) THEN
                k0metsu = 0.0
          ENDIF
!
          IF ( oxy > (CoxMet * POROS) ) THEN
               k1metsu  = 0.0
               k0metsu = 0.0
          ENDIF
!
!         Calculate both methane oxidation fluxes
!
          TEMPC  = TcMetOx ** TEMP20
!
          IF ( (KsMet * POROS + CH4) > 0.0) THEN
                  CHFUNC = CH4 / ( KsMet * POROS + CH4 )
          ELSE
                  CHFUNC = 0.0
          ENDIF
!
          IF ( (KsOxMet * POROS + oxy) > 0.0) THEN
                  OXFUNC = oxy  / ( KsOxMet * POROS  + oxy  )
          ELSE
                  OXFUNC = 0.0
          ENDIF
!

          IF ( (KsSuMet * POROS + so4) > 0.0) THEN
                  SUFUNC = so4  / ( KsSuMet * POROS  + so4  )
          ELSE
                  SUFUNC = 0.0
          ENDIF
!
!         Light inhibition function - removed
          LIFUNC = 0.0
!

          FLCOX = k0metox + k1metox * TEMPC * CHFUNC * OXFUNC * (1-LIFUNC)
          FLCSU = k0metsu + k1metsu * TEMPC * CHFUNC * SUFUNC * (1-LIFUNC) ! I think this was intended to be evaluated with TcMetSu. BUG?
          FLCOX = MIN(FLCOX,0.5*oxy/COX_RATIO/DELT)
          FLCOX = MIN(FLCOX,0.9*CH4/DELT)
          FLCSU = MIN(FLCSU,0.9*so4/CSU_RATIO/DELT)
          FLCSU = MIN(FLCSU,0.9*CH4/DELT)

          !     Output of original proces:  OXFUNC SUFUNC LIFUNC
          kp(ilay,is_ch4) = kp(ilay,is_ch4) - flcox - flcsu
          kp(ilay,is_oxy) = kp(ilay,is_oxy) - flcox * 5.33
          kp(ilay,is_so4) = kp(ilay,is_so4) - flcsu * 2.67
          kp(ilay,is_sud) = kp(ilay,is_sud) + flcsu * 2.67
   !      store in output
          iflux = nototseddis + 76
          fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + flcox*dl(ilay) /depth
          iflux = nototseddis + 77
          fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + flcsu*dl(ilay) /depth

          ! +++++++++++++++++++++++++++++++++++++++++++
          ! Existing process APATITE
          ! +++++++++++++++++++++++++++++++++++++++++++

!         Calculation of the precipitation or dissolution flux
!
          FPRC = 0.0
          FSOL = 0.0
          TMPPRC = TCprecipP**TEMP20
          TMPSOL = TCdissolP**TEMP20
!
          FPRC = RatAPandVP * RCprecP20 * TMPPRC * ( po4 / POROS - EqAPATDisP ) * POROS
          FSOL = RCdisAP20 * TMPSOL * ApatP * ( EqAPATDisP - po4 / POROS )
!
          IF ( FPRC < 0.0) FPRC = 0.0
          IF ( FSOL < 0.0) FSOL = 0.0
          IF ( FSOL*DELT >= ApatP) FSOL = 0.5 * ApatP / DELT

          !     Output of original proces: -
          kp(ilay,is_po4  ) = kp(ilay,is_po4  ) - fprc + fsol
          kp(ilay,is_apatp) = kp(ilay,is_apatp) + fprc - fsol
  !       store in output
          iflux = nototseddis + 78
          fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + fprc*dl(ilay) /depth
          iflux = nototseddis + 79
          fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + fsol*dl(ilay) /depth

          ! +++++++++++++++++++++++++++++++++++++++++++
          ! Interface to VB, programmed as an extra "process" to avoid further logistics
          ! +++++++++++++++++++++++++++++++++++++++++++
          if (sw_vb) then

              ! 6 uptake fluxes, distributed over the layers according to availability
              vbflux = 0.0
              if ( s1_nh4 > 0.0 ) vbflux = fN1VBup * nh4 / s1_nh4 / poros
              kp(ilay,is_nh4  ) = kp(ilay,is_nh4  ) - vbflux
              iflux = nototseddis + 80
              fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + vbflux*dl(ilay) /depth

              vbflux = 0.0
              if ( s1_no3 > 0.0 ) vbflux = fN2VBup * no3 / s1_no3 / poros
              kp(ilay,is_no3  ) = kp(ilay,is_no3  ) - vbflux
              iflux = nototseddis + 81
              fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + vbflux*dl(ilay) /depth


              vbflux = 0.0
              if ( s1_aap > 0.0 ) vbflux = fP1VBup * aap / s1_aap / poros
              kp(ilay,is_aap  ) = kp(ilay,is_aap  ) - vbflux
              iflux = nototseddis + 82
              fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + vbflux*dl(ilay) /depth

              vbflux = 0.0
              if ( s1_po4 > 0.0 ) vbflux = fP2VBup * po4 / s1_po4 / poros
              kp(ilay,is_po4  ) = kp(ilay,is_po4  ) - vbflux
              iflux = nototseddis + 83
              fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + vbflux*dl(ilay) /depth

              vbflux = 0.0
              if ( s1_so4 > 0.0 ) vbflux = fS1VBup * so4 / s1_so4 / poros
              kp(ilay,is_so4  ) = kp(ilay,is_so4  ) - vbflux
              iflux = nototseddis + 84
              fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + vbflux*dl(ilay) /depth

              vbflux = 0.0
              if ( s1_sud > 0.0 ) vbflux = fS2VBup * sud / s1_sud / poros
              kp(ilay,is_sud  ) = kp(ilay,is_sud  ) - vbflux
              iflux = nototseddis + 85
              fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + vbflux*dl(ilay) /depth

              ! 16 release fluxes distributed homogeneously
              vbflux = fC1VBrel / Th_DelwaqG
              kp(ilay,is_poc1 ) = kp(ilay,is_poc1 ) + vbflux
              iflux = nototseddis + 86
              fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + vbflux*dl(ilay) /depth
              vbflux = fC2VBrel / Th_DelwaqG
              kp(ilay,is_poc2 ) = kp(ilay,is_poc2 ) + vbflux
              iflux = nototseddis + 87
              fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + vbflux*dl(ilay) /depth
              vbflux = fC3VBrel / Th_DelwaqG
              kp(ilay,is_poc3 ) = kp(ilay,is_poc3 ) + vbflux
              iflux = nototseddis + 88
              fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + vbflux*dl(ilay) /depth
              vbflux = fC4VBrel / Th_DelwaqG
              kp(ilay,is_poc4 ) = kp(ilay,is_poc4 ) + vbflux
              iflux = nototseddis + 89
              fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + vbflux*dl(ilay) /depth
              vbflux = fN1VBrel / Th_DelwaqG
              kp(ilay,is_pon1 ) = kp(ilay,is_pon1 ) + vbflux
              iflux = nototseddis + 90
              fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + vbflux*dl(ilay) /depth
              vbflux = fN2VBrel / Th_DelwaqG
              kp(ilay,is_pon2 ) = kp(ilay,is_pon2 ) + vbflux
              iflux = nototseddis + 91
              fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + vbflux*dl(ilay) /depth
              vbflux = fN3VBrel / Th_DelwaqG
              kp(ilay,is_pon3 ) = kp(ilay,is_pon3 ) + vbflux
              iflux = nototseddis + 92
              fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + vbflux*dl(ilay) /depth
              vbflux = fN4VBrel / Th_DelwaqG
              kp(ilay,is_pon4 ) = kp(ilay,is_pon4 ) + vbflux
              iflux = nototseddis + 93
              fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + vbflux*dl(ilay) /depth
              vbflux = fP1VBrel / Th_DelwaqG
              kp(ilay,is_pop1 ) = kp(ilay,is_pop1 ) + vbflux
              iflux = nototseddis + 94
              fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + vbflux*dl(ilay) /depth
              vbflux = fP2VBrel / Th_DelwaqG
              kp(ilay,is_pop2 ) = kp(ilay,is_pop2 ) + vbflux
              iflux = nototseddis + 95
              fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + vbflux*dl(ilay) /depth
              vbflux = fP3VBrel / Th_DelwaqG
              kp(ilay,is_pop3 ) = kp(ilay,is_pop3 ) + vbflux
              iflux = nototseddis + 96
              fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + vbflux*dl(ilay) /depth
              vbflux = fP4VBrel / Th_DelwaqG
              kp(ilay,is_pop4 ) = kp(ilay,is_pop4 ) + vbflux
              iflux = nototseddis + 97
              fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + vbflux*dl(ilay) /depth
              vbflux = fS1VBrel / Th_DelwaqG
              kp(ilay,is_pos1 ) = kp(ilay,is_pos1 ) + vbflux
              iflux = nototseddis + 98
              fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + vbflux*dl(ilay) /depth
              vbflux = fS2VBrel / Th_DelwaqG
              kp(ilay,is_pos2 ) = kp(ilay,is_pos2 ) + vbflux
              iflux = nototseddis + 99
              fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + vbflux*dl(ilay) /depth
              vbflux = fS3VBrel / Th_DelwaqG
              kp(ilay,is_pos3 ) = kp(ilay,is_pos3 ) + vbflux
              iflux = nototseddis + 100
              fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + vbflux*dl(ilay) /depth
              vbflux = fS4VBrel / Th_DelwaqG
              kp(ilay,is_pos4 ) = kp(ilay,is_pos4 ) + vbflux
              iflux = nototseddis + 101
              fl(iflux+(iseg-1)*noflux) = fl(iflux+(iseg-1)*noflux) + vbflux*dl(ilay) /depth
          endif

          ! end of loop over layers in present cell
          enddo

! ENDPROC
          do isys = 1,nototsed
              dissub = (isys<=nototseddis)

              ! dl:    layer thicknes
              ! tt:    turcoef
              ! td     difcoef
              ! kp     zero order processes term
              ! lp     first order processes term
              ! av     a(iun,ieq)
              ! bv     b(ieq)
              ! cwater    concentration in overlying water
              ! sedconc(nolay,nototsed,noseg2d)

              if (dissub) then
                ip = offset_cwater + isys
                cwater = pmsa(IPOINT(ip)+(iseg-1)*INCREM(ip))
              endif

              ! build equations
              av = 0d0
              bv = 0d0
              do ilay = 1,nolay

                  ! main diagonal element and rhs for dm/dt term
                  av(ilay,ilay) = av(ilay,ilay) + dble(dl(ilay)/delt)
                  bv(ilay)    = bv(ilay) + dble(sedconc(ilay,isys,iseg2d)*dl(ilay)/delt)

                  ! zero order processes
                  bv(ilay)    = bv(ilay) + dble(kp(ilay,isys)*dl(ilay))

                  ! first order processes
                  !av(ilay,ilay) = av(ilay,ilay) + dble(lp(ilay,isys)*dl(ilay))

                  ! diffusion
                  if (dissub) then
                      ! dissolved upper
                      if (ilay==1) then
                          !
                          ! For the exchange with the overlying water we need the water segment to be active
                          !
                          CALL evaluate_waq_attribute(1,IKNMRK(iseg),iatt1) ! pick up first attribute
                          if ( iatt1 == 1 ) then
                              term = dble(td(ilay)/(diflen/2.+dl(ilay)/2.))
                              bv(ilay)    = bv(ilay) + term*dble(cwater*poros)
                              av(ilay,ilay) = av(ilay,ilay) + term
                          endif
                      else
                          term = dble(td(ilay)/(dl(ilay-1)/2.+dl(ilay)/2.))
                          av(ilay-1,ilay) = av(ilay-1,ilay) - term
                          av(ilay,ilay) = av(ilay,ilay) + term
                      endif
                      ! dissolved lower
                      if (ilay/=nolay) then
                          term = dble(td(ilay+1)/(dl(ilay+1)/2.+dl(ilay)/2.))
                          av(ilay+1,ilay) = av(ilay+1,ilay) - term
                          av(ilay,ilay) = av(ilay,ilay) + term
                      endif
                  else
                      ! solid upper
                      if (ilay/=1) then
                          term = dble(tt(ilay-1)/(dl(ilay-1)/2.+dl(ilay)/2.))
                          av(ilay-1,ilay) = av(ilay-1,ilay) - term
                          av(ilay,ilay) = av(ilay,ilay) + term
                      endif
                      ! solid lower
                      if (ilay/=nolay) then
                          term = dble(tt(ilay)/(dl(ilay+1)/2.+dl(ilay)/2.))
                          av(ilay+1,ilay) = av(ilay+1,ilay) - term
                          av(ilay,ilay) = av(ilay,ilay) + term
                      endif
                  endif

              enddo

              ! solve; solution will come back as bv
              !all printstelsel(av,bv,nolay,'IN',isys)
              call INVERM (av,bv,nolay,1,nolay,iwork,rwork,ierror)
              !all printstelsel(av,bv,nolay,'OUT',isys)
              if (ierror/=0) CALL write_error_message ('Error Solving Local Equations')

              do ilay = 1,nolay
                  sedconc(ilay,isys,iseg2d) = bv(ilay)
              enddo

              ! export solute fluxes
              if (dissub .and. iatt1 == 1 ) then
                  !             g/m3                   / m                   * m2/d  / m  = g/m3/d, positive TOWARDS water column
                  sedwatflx = -(cwater*poros-real(bv(1)))/(diflen/2.+dl(1)/2.) * td(1) / depth
                  iflux = isys
                  fl(iflux+(iseg-1)*noflux) = sedwatflx
              endif
          enddo

      enddo

      contains

      ! Routine to initialise the layer administration either from file or using defaults
      ! Also use this file for the names of the auxiliary files
      !
      subroutine initialise_layers

      integer(kind=int_wp), parameter            ::nolay_default = 7
      real(kind=real_wp), parameter               ::dl_default(nolay_default) = [ 0.001, 0.002, 0.004, 0.008, 0.016, 0.032, 0.037 ]   ! layer thickness

      character(len=40), parameter :: param_file = 'delwaqg.parameters'
      character(len=100)           :: line
      logical                      :: exists, skip_tt_td
      integer(kind=int_wp)                       ::i, lumon, ierr

      call get_log_unit_number( lumon )

      inquire( file = param_file, exist = exists )

      if ( exists ) then
          write( lumon, '(/,3a)' ) 'DELWAQG: use parameters from "', trim(param_file), '" to initialise the sediment layers'

          open( newunit = luinp, file = param_file )
          read( luinp, * ) mapfile
          read( luinp, * ) restartfile
          read( luinp, * ) initfile
          read( luinp, * ) nolay
          default_profile = .false.
      else
          write( lumon, '(/,3a)' ) 'DELWAQG: file "', trim(param_file), '" not found - using default layer parameters instead'
          mapfile     = 'delwaqg.map'
          restartfile = 'delwaqg.restart'
          initfile    = 'none'

          nolay = nolay_default
          default_profile = .true.
      endif

      allocate( tt(nolay), td(nolay), dl(nolay), sd(nolay), bd(nolay), av(nolay,nolay), bv(nolay), rwork(nolay), &
                kp(nolay,nototsed), lp(nolay,nototsed), iwork(nolay) )

      write( lumon, '(2a)' ) 'Map file for detailed sediment concentrations:  ', trim(mapfile)
      write( lumon, '(2a)' ) 'Restart file for subsequent calculations:       ', trim(restartfile)
      if ( initfile /= 'none' ) then
          write( lumon, '(2a)' ) 'Initial conditions for sediment to be used:     ', trim(initfile)
      else
          write( lumon, '(a)' )  'Initial conditions for sediment from "S1" substances'
      endif

      write( lumon, '(a,i0)' ) 'Number of layers: ', nolay
      if ( exists ) then
          read( luinp, '(a)' ) line
          read( line, *, iostat = ierr ) dl(1), td(1), tt(1)
          if ( ierr /= 0 ) then
              default_profile = .true.
              skip_tt_td      = .true.

              read( line, * ) dl(1)
              do i = 2,nolay
                  read( luinp, * ) dl(i)
              enddo
          else
              skip_tt_td      = .false.
              do i = 2,nolay
                  read( luinp, * ) dl(i), td(i), tt(i)
              enddo
          endif
      else
          dl         = dl_default
          skip_tt_td = .false.
      endif
      sd(1) = 0.0
      do i = 1,nolay-1
          sd(i+1) = sd(i) + dl(i)
          bd(i)   = sd(i+1)
      enddo
      bd(nolay) = sd(nolay) + dl(nolay)

      write( lumon, '(a)' ) 'Per layer (in m):'
      if ( skip_tt_td ) then
          write( lumon, '(a)' ) '           Thickness            Top         Bottom'
          do i = 1,nolay
              write( lumon, '(i5,5g15.4)' ) i, dl(i), sd(i), bd(i)
          enddo
      else
          write( lumon, '(a)' ) '           Thickness            Top         Bottom      Diffusion   Bioturbation'
          do i = 1,nolay
              write( lumon, '(i5,5g15.4)' ) i, dl(i), sd(i), bd(i), td(i), tt(i)
          enddo
      endif
      write( lumon, '(a)' ) ' '

      end subroutine initialise_layers


      ! Routine to initialise the sediment concentrations from the initial conditions file or from "S1" substances
      !
      subroutine initialise_sedconc
     
      integer(kind=int_wp)  ::ilay, iseg, iseg2d, ip, isys, iflux
      integer(kind=int_wp)  ::ierr, luinit, lumon
      integer(kind=int_wp)  ::timeini, nosysini, nosegini
      real(kind=real_wp)     ::mass3d, totmas
      real(kind=real_wp)     ::thickness, poros
      character(len=20), allocatable :: synameinit(:)
      character(len=40)              :: title(4)

      thickness = PMSA(IPOINT(ip_Th_DelwaqG))
      poros     = PMSA(IPOINT(ip_poros))

      if ( initfile == 'none' ) then
          sedconc = 0.0
          do iseg2d = 1,noseg2d
              iseg = bottomsegments(iseg2d)
              do isys = 1,nototsed
                  ip = offset_s1 + isys
                  mass3d = PMSA(IPOINT(ip)+(iseg-1)*INCREM(ip)) / Th_DelwaqG ! g/m2 to g/m3
                  do ilay = 1,nolay
                      sedconc(ilay,isys,iseg2d) = mass3d
                  enddo
              enddo
          enddo
      else
          open( newunit = luinit, file = initfile, access = 'stream', iostat = ierr )
          if ( ierr /= 0 ) then
              call get_log_unit_number( lumon )
              write( lumon, '(2a)' ) 'Error reading file: ', trim(initfile)
              write( lumon, '(2a)' ) 'File does not exist - terminating calculation'
              call write_error_message( 'Initial conditions file for sediment not found')
          endif

          read( luinit ) title
          read( luinit ) nosysini, nosegini

          if ( nosysini /= nototsed .or. nosegini /= nolay * noseg2d ) then
              call get_log_unit_number( lumon )
              write( lumon, '(2a)' )     'Error reading file: ', trim(initfile)
              write( lumon, '(a,2i10)' ) 'Wrong size parameters:', nosysini, nosegini
              write( lumon, '(a,2i10)' ) 'Expected parameters:  ', nototsed, nolay * noseg2d
              call write_error_message( 'Initial conditions file for sediment inconsistent')
          endif

          allocate( synameinit(nosysini) )
          read( luinit ) synameinit

          read( luinit ) timeini, (((sedconc(ilay,isys,iseg2d),isys=1,nototsed), iseg2d=1,noseg2d), ilay=1,nolay)

          sedconc(:,1:nototseddis,:) = sedconc(:,1:nototseddis,:) * poros

          close( luinit )
      endif

      call handle_zone_information( thickness, poros )

      end subroutine initialise_sedconc

      ! Set up the fluxes required to bring the S1 substances in line with the detailed sediment
      ! With this, a flux is calculated that compensates the difference in total mass between
      ! detailed sediment concentrations and the S1 substances in one time step
      !
      subroutine sync_S1_sedconc
          integer(kind=int_wp)  ::iseg2d, iseg, isys, ilay, iflux, ip
          real(kind=real_wp)     ::totmas

          do iseg2d = 1,noseg2d
              iseg = bottomsegments(iseg2d)
              do isys = 1,nototsed
                  totmas = 0.0
                  do ilay = 1,nolay
                      totmas = totmas + sedconc(ilay,isys,iseg2d)*dl(ilay)
                  enddo
                  iflux  = OFFSET_FL+isys
                  ip     = OFFSET_S1+isys
                  fl(iflux+(iseg-1)*noflux) = (totmas - pmsa(ipoint(ip)+(iseg-1)*increm(ip)))  / &
                                              pmsa(ipoint(ip_depth)+(iseg-1)*increm(ip_depth)) / delt
              enddo
          enddo
      end subroutine sync_S1_sedconc

      ! Routine to store the sediment concentrations in a map file and a restart file
      !
      subroutine write_sedconc

      logical, save :: first = .true.

      !
      ! Open the files and write the header information
      !
      if ( first ) then
          first = .false.
          open (newunit = lumap,     file = mapfile,     access = 'stream')
          open (newunit = lurestart, file = restartfile, access = 'stream')
          close( lumap,     status = 'delete' )
          close( lurestart, status = 'delete' )

          open (newunit = lumap,     file = mapfile,     access = 'stream')
          open (newunit = lurestart, file = restartfile, access = 'stream')

          moname = ''
          write (lumap) moname
          write (lumap) nototsed,nolay*noseg2d
          write (lumap) syname
      endif

      write (lumap) itime,(((sedconc(ilay,isys,iseg2d)/poros,isys=1,nototseddis), &
                            (sedconc(ilay,isys,iseg2d),isys=nototseddis+1,nototsed),iseg2d=1,noseg2d),ilay=1,nolay)

      !
      ! Rewrite the complete restart file
      !
      rewind( lurestart )

      moname = ''
      write (lurestart) moname
      write (lurestart) nototsed,nolay*noseg2d
      write (lurestart) syname

      write (lurestart) itime,(((sedconc(ilay,isys,iseg2d)/poros,isys=1,nototseddis), &
                                (sedconc(ilay,isys,iseg2d),isys=nototseddis+1,nototsed),iseg2d=1,noseg2d),ilay=1,nolay)

      end subroutine write_sedconc

      subroutine handle_zone_information( thickness, poros )
      use m_logger

      real(kind=real_wp), intent(in) ::thickness, poros

      integer(kind=int_wp)                             ::ierr, luzone, lumon, idx, ic, ip, ip2, ilay
      character(len=200)                 :: zonefile
      integer(kind=int_wp), dimension(:), allocatable ::zone
      integer(kind=int_wp)                             ::nzones
      real(kind=real_wp), dimension(:), allocatable ::initvalue
      character(len=20)                  :: name
      character(len=200)                 :: inputline

      integer(kind=int_wp), parameter                  ::first_s1 = 142

      call get_log_unit_number( lumon )

      read( luinp, *, iostat = ierr ) zonefile

      if ( ierr /= 0 ) then
          close( luinp )
          return
      endif

      !
      ! Read in the zone information - text file with zone numbers
      !
      allocate( zone(noseg2d) )
      open( newunit = luzone, file = zonefile, status = 'old', iostat = ierr )
      if ( ierr /= 0 ) then
          write( lumon, '(2a)' ) 'Error opening zone file - ', trim(zonefile)
          write( lumon, '(a)' )  'Calculation stopped'
          stop
      endif

      read( luzone, *, iostat = ierr ) zone
      if ( ierr /= 0 ) then
          write( lumon, '(2a)' )      'Error reading zone file - ', trim(zonefile)
          write( lumon, '(a,i0,a)' )  'Expecting ', noseg2d, ' zone numbers'
          write( lumon, '(a)' )       'Calculation stopped'
          stop
      endif

      zone   = merge( zone, 1, zone > 0 )
      nzones = maxval( zone )

      write( lumon, '(2a)' )   'Initialisation of the sediment via zones - file: ', trim(zonefile)
      write( lumon, '(a,i0)' ) 'Number of zones: ', nzones

      allocate( initvalue(nzones) )

      !
      ! Now read the lines with initial values per zone:
      ! For the substances these represent the mass per m2 (so the concentration seen by D-Water Quality itself).
      !
      do
          read( luinp, '(a)', iostat = ierr ) inputline
          if ( ierr /= 0 ) then
              exit
          endif

          if ( inputline == ' ' ) then
              cycle
          endif

          read( inputline, *, iostat = ierr ) name, initvalue
          if ( ierr /= 0 ) then
              write( lumon, '(a)' ) 'Error reading the values per zone from line: ', trim(inputline)
              write( lumon, '(a)' ) 'Too few data? Calculation stopped'
              stop
          endif

          idx = getindex(name )
          if ( idx < 1 ) then
              write( lumon, '(a)' ) 'Unknown quantity in values per zone from line: ', trim(inputline)
              write( lumon, '(a)' ) 'Calculation stopped'
              stop
          endif

          if ( idx < first_s1 .and. increm(idx) <= 0 ) then
              write( lumon, '(4a)' ) 'Quantity ', trim(name), ' not defined as a parameter varying per segment'
              write( lumon, '(a)' )  'Input inconsistent. Calculation stopped'
              stop
          endif

          ip  = ipoint(idx)
          ic  = increm(idx)
          ip2 = ip + (noseg2d-1) * ic

          if ( idx < first_s1 ) then
              pmsa(ip:ip2:ic) = initvalue(zone)
          else
              idx = idx - first_s1 + 1
              do ilay = 1,nolay
                  sedconc(ilay,idx,:) = initvalue(zone) / thickness ! sedconc is the concentration in the active
                                                                    ! sediment layer, initvalue is the concentration
                                                                    ! per m2
              enddo
          endif
      enddo
      end subroutine handle_zone_information

      integer function getindex( name )
      character(len=*), intent(in) :: name
      type known
          character(len=20) :: name
          integer(kind=int_wp)            ::index
      end type

      type(known), dimension(40) :: known_name = [ &
          known('PorSed      ', 131), known('Exp_Dif     ', 135), known('Exp_Tur     ', 136), known('pHSed       ', 137), &
          known('TurCoef     ', 140), known('DifCoef     ', 141), known('S1_CH4      ', 142), known('S1_DOC      ', 143), &
          known('S1_DON      ', 144), known('S1_DOP      ', 145), known('S1_DOS      ', 146), known('S1_NH4      ', 147), &
          known('S1_NO3      ', 148), known('S1_OXY      ', 149), known('S1_PO4      ', 150), known('S1_Si       ', 151), &
          known('S1_SO4      ', 152), known('S1_SUD      ', 153), known('S1_AAP      ', 154), known('S1_APATP    ', 155), &
          known('S1_FeIIIpa  ', 156), known('S1_Opal     ', 157), known('S1_PC1      ', 158), known('S1_PC2      ', 159), &
          known('S1_PC3      ', 160), known('S1_PC4      ', 161), known('S1_PN1      ', 162), known('S1_PN2      ', 163), &
          known('S1_PN3      ', 164), known('S1_PN4      ', 165), known('S1_PP1      ', 166), known('S1_PP2      ', 167), &
          known('S1_PP3      ', 168), known('S1_PP4      ', 169), known('S1_PS1      ', 170), known('S1_PS2      ', 171), &
          known('S1_PS3      ', 172), known('S1_PS4      ', 173), known('S1_SUP      ', 174), known('S1_VIVP     ', 175)]

      getindex = -1
      do i = 1,size(known_name)
          if ( name == known_name(i)%name ) then
              getindex = known_name(i)%index
              exit
          endif
      enddo

      end function getindex

      end subroutine dlwqg2

end module m_delwaqg
