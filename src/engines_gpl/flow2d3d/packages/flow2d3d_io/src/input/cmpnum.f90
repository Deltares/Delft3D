function cmpnum(num       )
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
!    Function: Finds the name of the tidal component
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
    integer, intent(in)            :: num
                                   !!  Pointer for the tidal components
    character(8)    :: cmpnum
                                   !!  Name of the chosen tidal component
!
!
! Local variables
!
    character(8), dimension(0:mxcmp) :: l ! Array with the names of the tidal components 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    !
    l(0) = 'A0      '
    l(1) = 'SA      '
    l(2) = 'SSA     '
    l(3) = 'MSM     '
    l(4) = 'MM      '
    l(5) = 'MSF     '
    l(6) = 'MS0     '
    l(7) = 'MF      '
    l(8) = 'KO0     '
    l(9) = 'MK0     '
    l(10) = 'SNU     '
    l(11) = 'SN      '
    l(12) = 'MSTM    '
    l(13) = 'MFM     '
    l(14) = '2SM     '
    l(15) = 'MSQM    '
    l(16) = 'MQM     '
    l(17) = '2SMN    '
    l(18) = '2OK1    '
    l(19) = '2Q1     '
    l(20) = 'NJ1     '
    l(21) = 'SIGMA1  '
    l(22) = 'MUK1    '
    l(23) = 'NUJ1    '
    l(24) = 'Q1      '
    l(25) = 'NK1     '
    l(26) = 'RO1     '
    l(27) = 'NUK1    '
    l(28) = 'O1      '
    l(29) = 'TAU1    '
    l(30) = 'MP1     '
    l(31) = 'M1B     '
    l(32) = 'M1C     '
    l(33) = 'M1A     '
    l(34) = 'M1      '
    l(35) = 'NO1     '
    l(36) = 'CHI1    '
    l(37) = 'LP1     '
    l(38) = 'PI1     '
    l(39) = 'TK1     '
    l(40) = 'P1      '
    l(41) = 'SK1     '
    l(42) = 'S1      '
    l(43) = 'K1      '
    l(44) = 'MO1     '
    l(45) = 'SP1     '
    l(46) = 'PSI1    '
    l(47) = 'RP1     '
    l(48) = 'FI1     '
    l(49) = 'KP1     '
    l(50) = 'THETA1  '
    l(51) = 'LABDAO1 '
    l(52) = 'J1      '
    l(53) = 'MQ1     '
    l(54) = '2PO1    '
    l(55) = 'SO1     '
    l(56) = 'OO1     '
    l(57) = '2KO1    '
    l(58) = 'UPSILON1'
    l(59) = 'KQ1     '
    l(60) = '2MN2S2  '
    l(61) = '3MKS2   '
    l(62) = '2NS2    '
    l(63) = '3MS2    '
    l(64) = 'OQ2     '
    l(65) = 'MNK2    '
    l(66) = 'EPSILON2'
    l(67) = 'MNS2    '
    l(68) = '2ML2S2  '
    l(69) = 'MNUS2   '
    l(70) = 'MNK2S2  '
    l(71) = '2MS2K2  '
    l(72) = 'O2      '
    l(73) = 'NLK2    '
    l(74) = '2MK2    '
    l(75) = '2N2     '
    l(76) = 'MU2     '
    l(77) = '2MS2    '
    l(78) = 'SNK2    '
    l(79) = 'NA2     '
    l(80) = 'N2      '
    l(81) = 'KQ2     '
    l(82) = 'NB2     '
    l(83) = 'NU2     '
    l(84) = '3MSN2   '
    l(85) = '2KN2S2  '
    l(86) = 'OP2     '
    l(87) = 'MSK2    '
    l(88) = 'GAMMA2  '
    l(89) = 'ALFA2   '
    l(90) = 'MPS2    '
    l(91) = 'MA2     '
    l(92) = 'M2      '
    l(93) = 'KO2     '
    l(94) = 'MSP2    '
    l(95) = 'MB2     '
    l(96) = 'DELTA2  '
    l(97) = 'MKS2    '
    l(98) = 'M2(KS)2 '
    l(99) = '2SN(MK)2'
    l(100) = 'LABDA2  '
    l(101) = 'SNM2    '
    l(102) = '2MN2    '
    l(103) = 'L2      '
    l(104) = 'L2A     '
    l(105) = 'L2B     '
    l(106) = '2SK2    '
    l(107) = 'T2      '
    l(108) = 'S2      '
    l(109) = 'KP2     '
    l(110) = 'R2      '
    l(111) = 'K2      '
    l(112) = 'MSNU2   '
    l(113) = 'MSN2    '
    l(114) = 'ZETA2   '
    l(115) = 'ETA2    '
    l(116) = 'KJ2     '
    l(117) = 'MKN2    '
    l(118) = '2KM(SN)2'
    l(119) = '2SM2    '
    l(120) = 'SKM2    '
    l(121) = '2MS2N2  '
    l(122) = '2SNU2   '
    l(123) = '2SN2    '
    l(124) = 'SKN2    '
    l(125) = 'MQ3     '
    l(126) = 'NO3     '
    l(127) = 'MO3     '
    l(128) = '2MK3    '
    l(129) = '2MP3    '
    l(130) = 'M3      '
    l(131) = 'NK3     '
    l(132) = 'SO3     '
    l(133) = 'MP3     '
    l(134) = 'MK3     '
    l(135) = 'SP3     '
    l(136) = '2MQ3    '
    l(137) = 'SK3     '
    l(138) = '2SO3    '
    l(139) = 'K3      '
    l(140) = '4MS4    '
    l(141) = '2MNS4   '
    l(142) = '3MK4    '
    l(143) = 'MNLK4   '
    l(144) = '3MS4    '
    l(145) = 'MSNK4   '
    l(146) = 'MN4     '
    l(147) = 'MNU4    '
    l(148) = '2MLS4   '
    l(149) = '2MSK4   '
    l(150) = 'M4      '
    l(151) = '2MKS4   '
    l(152) = 'SN4     '
    l(153) = '3MN4    '
    l(154) = '2SMK4   '
    l(155) = 'MS4     '
    l(156) = 'MK4     '
    l(157) = '2SNM4   '
    l(158) = '2MSN4   '
    l(159) = 'SL4     '
    l(160) = 'S4      '
    l(161) = 'SK4     '
    l(162) = '2SMN4   '
    l(163) = '3SM4    '
    l(164) = '2SKM4   '
    l(165) = 'MNO5    '
    l(166) = '3MK5    '
    l(167) = '3MP5    '
    l(168) = 'M5      '
    l(169) = 'MNK5    '
    l(170) = '2MP5    '
    l(171) = 'MSO5    '
    l(172) = '3MO5    '
    l(173) = 'MSK5    '
    l(174) = '3KM5    '
    l(175) = '2(MN)S6 '
    l(176) = '3MNS6   '
    l(177) = '4MK6    '
    l(178) = '2NM6    '
    l(179) = '4MS6    '
    l(180) = '2MSNK6  '
    l(181) = '2MN6    '
    l(182) = '2MNU6   '
    l(183) = '3MSK6   '
    l(184) = 'M6      '
    l(185) = 'MSN6    '
    l(186) = 'MNK6    '
    l(187) = '4MN6    '
    l(188) = 'MKNU6   '
    l(189) = '2(MS)K6 '
    l(190) = '2MS6    '
    l(191) = '2MK6    '
    l(192) = '2SN6    '
    l(193) = '3MSN6   '
    l(194) = 'MKL6    '
    l(195) = '2SM6    '
    l(196) = 'MSK6    '
    l(197) = 'S6      '
    l(198) = '2MNO7   '
    l(199) = '2NMK7   '
    l(200) = 'M7      '
    l(201) = '2MSO7   '
    l(202) = 'MSKO7   '
    l(203) = '2(MN)8  '
    l(204) = '3MN8    '
    l(205) = '3MNKS8  '
    l(206) = 'M8      '
    l(207) = '2MSN8   '
    l(208) = '2MNK8   '
    l(209) = '3MS8    '
    l(210) = '3MK8    '
    l(211) = '2SNM8   '
    l(212) = 'MSNK8   '
    l(213) = '2(MS)8  '
    l(214) = '2MSK8   '
    l(215) = '3SM8    '
    l(216) = '2SMK8   '
    l(217) = 'S8      '
    l(218) = '2(MN)K9 '
    l(219) = '3MNK9   '
    l(220) = '4MK9    '
    l(221) = '3MSK9   '
    l(222) = '4MN10   '
    l(223) = 'M10     '
    l(224) = '3MSN10  '
    l(225) = '4MS10   '
    l(226) = '2(MS)N10'
    l(227) = '2MNSK10 '
    l(228) = '3M2S10  '
    l(229) = '4MSK11  '
    l(230) = 'M12     '
    l(231) = '4MSN12  '
    l(232) = '5MS12   '
    l(233) = '3MNKS12 '
    l(234) = '4M2S12  '
    !
    cmpnum = 'ERROR   '
    if (num>=0 .and. num<=mxcmp) then
       cmpnum = l(num)
    endif
end function cmpnum
