function numcmp(cmpinp    )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  
!  
!!--description-----------------------------------------------------------------
!
!    Function: Finds the index in the array
!              with tidal components
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Local parameters
!
    integer, parameter :: mxcmp = 234 !  Description and declaration in tfzeta.igs
!
! Global variables
!
    integer         :: numcmp
                                   !!  Index of the tidal component
    character(8), intent(in)       :: cmpinp
                                   !!  Name of the tidal component
!
!
! Local variables
!
    integer                        :: i                    ! Loop counter 
    character(8)                   :: cmphlp ! Help string containing name of the tidal component or it's equivalence 
    character(8), dimension(0:mxcmp) :: cmparr ! Array with the names of the tidal components 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    !-----Special RWS name "SM      " for component "MS0     "
    !
    cmphlp = cmpinp
    if (cmpinp=='SM      ') cmphlp = 'MS0     '
    !
    !-----Define array contents
    !
    cmparr(0) = 'A0      '
    cmparr(1) = 'SA      '
    cmparr(2) = 'SSA     '
    cmparr(3) = 'MSM     '
    cmparr(4) = 'MM      '
    cmparr(5) = 'MSF     '
    cmparr(6) = 'MS0     '
    cmparr(7) = 'MF      '
    cmparr(8) = 'KO0     '
    cmparr(9) = 'MK0     '
    cmparr(10) = 'SNU     '
    cmparr(11) = 'SN      '
    cmparr(12) = 'MSTM    '
    cmparr(13) = 'MFM     '
    cmparr(14) = '2SM     '
    cmparr(15) = 'MSQM    '
    cmparr(16) = 'MQM     '
    cmparr(17) = '2SMN    '
    cmparr(18) = '2OK1    '
    cmparr(19) = '2Q1     '
    cmparr(20) = 'NJ1     '
    cmparr(21) = 'SIGMA1  '
    cmparr(22) = 'MUK1    '
    cmparr(23) = 'NUJ1    '
    cmparr(24) = 'Q1      '
    cmparr(25) = 'NK1     '
    cmparr(26) = 'RO1     '
    cmparr(27) = 'NUK1    '
    cmparr(28) = 'O1      '
    cmparr(29) = 'TAU1    '
    cmparr(30) = 'MP1     '
    cmparr(31) = 'M1B     '
    cmparr(32) = 'M1C     '
    cmparr(33) = 'M1A     '
    cmparr(34) = 'M1      '
    cmparr(35) = 'NO1     '
    cmparr(36) = 'CHI1    '
    cmparr(37) = 'LP1     '
    cmparr(38) = 'PI1     '
    cmparr(39) = 'TK1     '
    cmparr(40) = 'P1      '
    cmparr(41) = 'SK1     '
    cmparr(42) = 'S1      '
    cmparr(43) = 'K1      '
    cmparr(44) = 'MO1     '
    cmparr(45) = 'SP1     '
    cmparr(46) = 'PSI1    '
    cmparr(47) = 'RP1     '
    cmparr(48) = 'FI1     '
    cmparr(49) = 'KP1     '
    cmparr(50) = 'THETA1  '
    cmparr(51) = 'LABDAO1 '
    cmparr(52) = 'J1      '
    cmparr(53) = 'MQ1     '
    cmparr(54) = '2PO1    '
    cmparr(55) = 'SO1     '
    cmparr(56) = 'OO1     '
    cmparr(57) = '2KO1    '
    cmparr(58) = 'UPSILON1'
    cmparr(59) = 'KQ1     '
    cmparr(60) = '2MN2S2  '
    cmparr(61) = '3MKS2   '
    cmparr(62) = '2NS2    '
    cmparr(63) = '3MS2    '
    cmparr(64) = 'OQ2     '
    cmparr(65) = 'MNK2    '
    cmparr(66) = 'EPSILON2'
    cmparr(67) = 'MNS2    '
    cmparr(68) = '2ML2S2  '
    cmparr(69) = 'MNUS2   '
    cmparr(70) = 'MNK2S2  '
    cmparr(71) = '2MS2K2  '
    cmparr(72) = 'O2      '
    cmparr(73) = 'NLK2    '
    cmparr(74) = '2MK2    '
    cmparr(75) = '2N2     '
    cmparr(76) = 'MU2     '
    cmparr(77) = '2MS2    '
    cmparr(78) = 'SNK2    '
    cmparr(79) = 'NA2     '
    cmparr(80) = 'N2      '
    cmparr(81) = 'KQ2     '
    cmparr(82) = 'NB2     '
    cmparr(83) = 'NU2     '
    cmparr(84) = '3MSN2   '
    cmparr(85) = '2KN2S2  '
    cmparr(86) = 'OP2     '
    cmparr(87) = 'MSK2    '
    cmparr(88) = 'GAMMA2  '
    cmparr(89) = 'ALFA2   '
    cmparr(90) = 'MPS2    '
    cmparr(91) = 'MA2     '
    cmparr(92) = 'M2      '
    cmparr(93) = 'KO2     '
    cmparr(94) = 'MSP2    '
    cmparr(95) = 'MB2     '
    cmparr(96) = 'DELTA2  '
    cmparr(97) = 'MKS2    '
    cmparr(98) = 'M2(KS)2 '
    cmparr(99) = '2SN(MK)2'
    cmparr(100) = 'LABDA2  '
    cmparr(101) = 'SNM2    '
    cmparr(102) = '2MN2    '
    cmparr(103) = 'L2      '
    cmparr(104) = 'L2A     '
    cmparr(105) = 'L2B     '
    cmparr(106) = '2SK2    '
    cmparr(107) = 'T2      '
    cmparr(108) = 'S2      '
    cmparr(109) = 'KP2     '
    cmparr(110) = 'R2      '
    cmparr(111) = 'K2      '
    cmparr(112) = 'MSNU2   '
    cmparr(113) = 'MSN2    '
    cmparr(114) = 'ZETA2   '
    cmparr(115) = 'ETA2    '
    cmparr(116) = 'KJ2     '
    cmparr(117) = 'MKN2    '
    cmparr(118) = '2KM(SN)2'
    cmparr(119) = '2SM2    '
    cmparr(120) = 'SKM2    '
    cmparr(121) = '2MS2N2  '
    cmparr(122) = '2SNU2   '
    cmparr(123) = '2SN2    '
    cmparr(124) = 'SKN2    '
    cmparr(125) = 'MQ3     '
    cmparr(126) = 'NO3     '
    cmparr(127) = 'MO3     '
    cmparr(128) = '2MK3    '
    cmparr(129) = '2MP3    '
    cmparr(130) = 'M3      '
    cmparr(131) = 'NK3     '
    cmparr(132) = 'SO3     '
    cmparr(133) = 'MP3     '
    cmparr(134) = 'MK3     '
    cmparr(135) = 'SP3     '
    cmparr(136) = '2MQ3    '
    cmparr(137) = 'SK3     '
    cmparr(138) = '2SO3    '
    cmparr(139) = 'K3      '
    cmparr(140) = '4MS4    '
    cmparr(141) = '2MNS4   '
    cmparr(142) = '3MK4    '
    cmparr(143) = 'MNLK4   '
    cmparr(144) = '3MS4    '
    cmparr(145) = 'MSNK4   '
    cmparr(146) = 'MN4     '
    cmparr(147) = 'MNU4    '
    cmparr(148) = '2MLS4   '
    cmparr(149) = '2MSK4   '
    cmparr(150) = 'M4      '
    cmparr(151) = '2MKS4   '
    cmparr(152) = 'SN4     '
    cmparr(153) = '3MN4    '
    cmparr(154) = '2SMK4   '
    cmparr(155) = 'MS4     '
    cmparr(156) = 'MK4     '
    cmparr(157) = '2SNM4   '
    cmparr(158) = '2MSN4   '
    cmparr(159) = 'SL4     '
    cmparr(160) = 'S4      '
    cmparr(161) = 'SK4     '
    cmparr(162) = '2SMN4   '
    cmparr(163) = '3SM4    '
    cmparr(164) = '2SKM4   '
    cmparr(165) = 'MNO5    '
    cmparr(166) = '3MK5    '
    cmparr(167) = '3MP5    '
    cmparr(168) = 'M5      '
    cmparr(169) = 'MNK5    '
    cmparr(170) = '2MP5    '
    cmparr(171) = 'MSO5    '
    cmparr(172) = '3MO5    '
    cmparr(173) = 'MSK5    '
    cmparr(174) = '3KM5    '
    cmparr(175) = '2(MN)S6 '
    cmparr(176) = '3MNS6   '
    cmparr(177) = '4MK6    '
    cmparr(178) = '2NM6    '
    cmparr(179) = '4MS6    '
    cmparr(180) = '2MSNK6  '
    cmparr(181) = '2MN6    '
    cmparr(182) = '2MNU6   '
    cmparr(183) = '3MSK6   '
    cmparr(184) = 'M6      '
    cmparr(185) = 'MSN6    '
    cmparr(186) = 'MNK6    '
    cmparr(187) = '4MN6    '
    cmparr(188) = 'MKNU6   '
    cmparr(189) = '2(MS)K6 '
    cmparr(190) = '2MS6    '
    cmparr(191) = '2MK6    '
    cmparr(192) = '2SN6    '
    cmparr(193) = '3MSN6   '
    cmparr(194) = 'MKL6    '
    cmparr(195) = '2SM6    '
    cmparr(196) = 'MSK6    '
    cmparr(197) = 'S6      '
    cmparr(198) = '2MNO7   '
    cmparr(199) = '2NMK7   '
    cmparr(200) = 'M7      '
    cmparr(201) = '2MSO7   '
    cmparr(202) = 'MSKO7   '
    cmparr(203) = '2(MN)8  '
    cmparr(204) = '3MN8    '
    cmparr(205) = '3MNKS8  '
    cmparr(206) = 'M8      '
    cmparr(207) = '2MSN8   '
    cmparr(208) = '2MNK8   '
    cmparr(209) = '3MS8    '
    cmparr(210) = '3MK8    '
    cmparr(211) = '2SNM8   '
    cmparr(212) = 'MSNK8   '
    cmparr(213) = '2(MS)8  '
    cmparr(214) = '2MSK8   '
    cmparr(215) = '3SM8    '
    cmparr(216) = '2SMK8   '
    cmparr(217) = 'S8      '
    cmparr(218) = '2(MN)K9 '
    cmparr(219) = '3MNK9   '
    cmparr(220) = '4MK9    '
    cmparr(221) = '3MSK9   '
    cmparr(222) = '4MN10   '
    cmparr(223) = 'M10     '
    cmparr(224) = '3MSN10  '
    cmparr(225) = '4MS10   '
    cmparr(226) = '2(MS)N10'
    cmparr(227) = '2MNSK10 '
    cmparr(228) = '3M2S10  '
    cmparr(229) = '4MSK11  '
    cmparr(230) = 'M12     '
    cmparr(231) = '4MSN12  '
    cmparr(232) = '5MS12   '
    cmparr(233) = '3MNKS12 '
    cmparr(234) = '4M2S12  '
    !
    !-----Define NUMCMP and re-define if CMPINP is in array
    !
    numcmp = -1
    do i = 0, mxcmp
       if (cmphlp==cmparr(i)) then
          numcmp = i
          exit
       endif
    enddo
end function numcmp
