!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2025.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
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
!  

!> This module contains the functions for astronomical components.

!! @author stef.hummel@deltares.nl
!! @author herman.kernkamp@deltares.nl
!! @author adri.mourits@deltares.nl
!! @author edwin.spee@deltares.nl
module m_ec_astro
   use precision,  only : hp, fp
   use mathconsts, only : pi, degrad

   implicit none

   private

   public :: asc
   public :: asc_map_components

   ! Related to astronomic components
   integer, parameter                      :: kcmp = 1       !<
   integer, parameter                      :: mxkc = 235     !<
   integer,           dimension(16*mxkc)   :: kb_values      !< Help var.
   character(len=8),  dimension(mxkc)      :: kb_keys = ''   !< Array with the names of all components
   logical                                 :: is_initialized = .false.

   contains

      subroutine read_kompbes
         integer                               :: ik, il, i, j
         character(len=80), dimension(mxkc)    :: kombes !< Array with tidal components

         call kompbs(kombes)
         !
         ik = -15
         do i = 1, mxkc
            ik = ik + 16
            il = ik + 15
            read (kombes(i), '(a8,10i3,3(i1,i2))') kb_keys(i), (kb_values(j), j = ik, il)
         enddo
         is_initialized = .true.
      end subroutine read_kompbes


      !> Determination of FR and V0+U.
      !! 'stripped' VERSION OF MAIN (ASCON)
      !! meteo1 : asc
      subroutine asc(omeg, ampl, phas, compnr, idate, itime, ierrs)
         real(kind=hp),    intent(out)   :: omeg      !< period [minute]
         real(kind=hp),    intent(inout) :: ampl      !< amplitude [m]
         real(kind=hp),    intent(inout) :: phas      !< phase [degree]
         integer,          intent(in)    :: compnr    !< component number in the KompBes table
         integer,          intent(in)    :: idate     !< date integer yyyymmdd
         integer,          intent(in)    :: itime     !< time integer hhmmss
         integer,          intent(out)   :: ierrs     !< number of errors
         !
         ! local
         !
         integer, dimension(6)           :: jdatum     !< Date and time

         real(kind=hp), dimension(kcmp)  :: fr  !< Amplitude factors for the referenced components
         real(kind=hp), dimension(kcmp)  :: v0u !< Astronomical arguments of the referenced components [rad]
         real(kind=hp), dimension(kcmp)  :: w   !< Angular velocity of the referenced components [rad/hr]

         integer                         :: jaar   !< Present year
         real(kind=hp)                   :: t      !< Time in hours referred to January 1, 00:00 of the year 'JAAR'
         real(kind=hp), dimension(15)    :: v      !< Help var. to calculate V0U()
         real(kind=hp), dimension(25)    :: f      !< Help var. to calculate FR()

         !
         !! executable statements -------------------------------------------------------
         !

         if (compnr == 0) then         ! A0
            omeg = 0.0_hp
            phas = 0.0_hp
            ierrs = 0
            return
         end if

         jdatum(1) = idate/10000
         jdatum(2) = idate/100 - 100*(idate/10000)
         jdatum(3) = idate - 100*(idate/100)
         jdatum(4) = itime/10000
         jdatum(5) = itime/100 - 100*(itime/10000)
         jdatum(6) = itime - 100*(itime/100)

         jaar = jdatum(1)
         !
         call datumi(jaar, jdatum, t)
         call hulpgr(jaar, t, v, f)
         call bewvuf_by_number(ierrs, (/compnr/), w, v0u, fr, v,f)

         omeg = w(1)/60.0_hp
         ampl = ampl * fr(1)
         phas = phas - v0u(1)
      end subroutine asc

      ! =======================================================================

      !> simulation of external kompbes-file
      !! meteo1 : kompbs
      subroutine kompbs(l)
         character(len=80), dimension(mxkc), intent(out) :: l !< Array with tidal components
         !
         l(1)   = 'SA                 1                            '
         l(2)   = 'SSA                2                            '
         l(3)   = 'MSM          1  1 -2                  1 1       '
         l(4)   = 'MM           1 -1                     1 1       '
         l(5)   = 'MSF          2    -2                  1 1       '
         l(6)   = 'MS0          2    -2    -2  2         1 6       '
         l(7)   = 'MF           2          -2            1 2       '
         l(8)   = 'KO0          2          -2  1 -2-10   1 3119    '
         l(9)   = 'MK0          2          -2  2   -11   120       '
         l(10)  = 'SNU          3  1 -4    -2  2         1 6       '
         l(11)  = 'SN           3 -1 -2    -2  2         1 6       '
         l(12)  = 'MSTM         3  1 -2    -2            1 2       '
         l(13)  = 'MFM          3 -1       -2            1 2       '
         l(14)  = '2SM          4    -4    -4  4         2 6       '
         l(15)  = 'MSQM         4    -2    -2            1 2       '
         l(16)  = 'MQM          4 -2       -2            1 2       '
         l(17)  = '2SMN         5 -1 -4    -4  4         2 6       '
         l(18)  = '2OK1      1 -4     1     4 -2 -1+10   2 3119    '
         l(19)  = '2Q1       1 -4  2  1     2 -1  1      1 3       '
         l(20)  = 'NJ1       1 -4  2  1     2 -1  1      1 41 6    '
         l(21)  = 'SIGMA1    1 -4     3     2 -1  1      1 3       '
         l(22)  = 'MUK1      1 -4     3     2 -2   +10   1 6119    '
         l(23)  = 'NUJ1      1 -4     3     2 -1  1      1 41 6    '
         l(24)  = 'Q1        1 -3  1  1     2 -1  1      1 3       '
         l(25)  = 'NK1       1 -3  1  1     2 -2  1+10   1 6119    '
         l(26)  = 'RO1       1 -3 -1  3     2 -1  1      1 3       '
         l(27)  = 'NUK1      1 -3 -1  3     2 -2 +1+10   1 6119    '
         l(28)  = 'O1        1 -2     1     2 -1  1      1 3       '
         l(29)  = 'TAU1      1 -2     3       -1 -1      1 4       '
         l(30)  = 'MP1       1 -2     3     2 -2 -1      1 6       '
         l(31)  = 'M1B       1 -1 -1  1     2 -1 -1      1 3       '
         l(32)  = 'M1C       1 -1     1     1 -1         112       '
         l(33)  = 'M1A       1 -1  1  1       -1 -1      1 4       '
         l(34)  = 'M1        1 -1  1  1       -1 -1-12   121       '
         l(35)  = 'NO1       1 -1  1  1       -1 -1      1 31 6    '
         l(36)  = 'CHI1      1 -1 -1 +3       -1 -1      1 4       '
         l(37)  = 'LP1       1 -1 -1  3     2 -2  1-13   122       '
         l(38)  = 'PI1       1       -2 +1        1                '
         l(39)  = 'TK1       1       -2  1        1+10   119       '
         l(40)  = 'P1        1       -1           1                '
         l(41)  = 'SK1       1       -1           1+10   119       '
         l(42)  = 'S1        1                                     '
         l(43)  = 'K1        1        1          -1-10   119       '
         l(44)  = 'MO1       1        1       -1 -1      1 31 6    '
         l(45)  = 'SP1       1        1          -1                '
         l(46)  = 'PSI1      1        2 -1       -1                '
         l(47)  = 'RP1       1        2 -1        1                '
         l(48)  = 'FI1       1        3          -1                '
         l(49)  = 'KP1       1        3          -1-11   120       '
         l(50)  = 'THETA1    1  1  1 -1       -1 -1      1 4       '
         l(51)  = 'LABDAO1   1  1  1 -1       -1  1      1 31 6    '
         l(52)  = 'J1        1  1 -1  1       -1 -1      1 4       '
         l(53)  = 'MQ1       1  1 -1  1       -1 -1      1 31 6    '
         l(54)  = '2PO1      1  2    -3    -2  1  1      1 3       '
         l(55)  = 'SO1       1  2    -1    -2  1 -1      1 3       '
         l(56)  = 'OO1       1  2     1    -2 -1 -1      1 5       '
         l(57)  = '2KO1      1  2     1    -2  1  1-10-101 3219    '
         l(58)  = 'UPSILON1  1  3 -1  1    -2 -1  1      1 5       '
         l(59)  = 'KQ1       1  3 -1  1    -2  1 -1-11   1 3120    '
         l(60)  = '2MN2S2    2 -7  1  6     6 -6         3 6       '
         l(61)  = '3MKS2     2 -6     4     6 -6   +11   3 6120    '
         l(62)  = '2NS2      2 -6  2  4     4 -4         2 6       '
         l(63)  = '3MS2      2 -6     6     6 -6         3 6       '
         l(64)  = 'OQ2       2 -5  1  2     4 -2  2      2 3       '
         l(65)  = 'MNK2      2 -5  1  2     4 -4   +11   2 6120    '
         l(66)  = 'EPSILON2  2 -5  1  4     2 -2         1 6       '
         l(67)  = 'MNS2      2 -5  1  4     4 -4         2 6       '
         l(68)  = '2ML2S2    2 -5 -1  6     6 -6  2-13   2 6122    '
         l(69)  = 'MNUS2     2 -5 -1  6     4 -4         2 6       '
         l(70)  = 'MNK2S2    2 -5  1  6     4 -4  0-11   2 6120    '
         l(71)  = '2MS2K2    2 -4           4 -4   +11+112 6220    '
         l(72)  = 'O2        2 -4     2     4 -2  2      2 3       '
         l(73)  = 'NLK2      2 -4     2     4 -4  2+11-131 6120122 '
         l(74)  = '2MK2      2 -4     2     4 -4   +11   1 6120    '
         l(75)  = '2N2       2 -4  2  2     2 -2         1 6       '
         l(76)  = 'MU2       2 -4     4     2 -2         1 6       '
         l(77)  = '2MS2      2 -4     4     4 -4         2 6       '
         l(78)  = 'SNK2      2 -3  1        2 -2   +11   1 6120    '
         l(79)  = 'NA2       2 -3  1  1  1                         '
         l(80)  = 'N2        2 -3  1  2     2 -2         1 6       '
         l(81)  = 'KQ2       2 -3  1  2     2 -1   -10   1 3119    '
         l(82)  = 'NB2       2 -3  1  3 -1                         '
         l(83)  = 'NU2       2 -3 -1  4     2 -2         1 6       '
         l(84)  = '3MSN2     2 -3  1  6     4 -4         4 6       '
         l(85)  = '2KN2S2    2 -3  1  6     2 -2   -11-111 6220    '
         l(86)  = 'OP2       2 -2           2 -1  2      1 3       '
         l(87)  = 'MSK2      2 -2           2 -2   +11   1 6120    '
         l(88)  = 'GAMMA2    2 -2  2        2 -2  2      1 6       '
         l(89)  = 'ALFA2     2 -2     1     2 -2  2      1 6       '
         l(90)  = 'MPS2      2 -2     1     2 -2  1      1 6       '
         l(91)  = 'MA2       2 -2     1                            '
         l(92)  = 'M2        2 -2     2     2 -2         1 6       '
         l(93)  = 'KO2       2 -2     2     2 -1   -10   1 3119    '
         l(94)  = 'MSP2      2 -2     3     2 -2 -1      1 6       '
         l(95)  = 'MB2       2 -2     3                            '
         l(96)  = 'DELTA2    2 -2     4       -2  0      1 7       '
         l(97)  = 'MKS2      2 -2     4     2 -2   -11   1 6120    '
         l(98)  = 'M2(KS)2   2 -2     6     2 -2   -11-111 6220    '
         l(99)  = '2SN(MK)2  2 -1  1 -2            +11   2 6120    '
         l(100) = 'LABDA2    2 -1  1        2 -2  2      1 6       '
         l(101) = 'SNM2      2 -1  1                     2 6       '
         l(102) = '2MN2      2 -1 -1  2     2 -2         3 6       '
         l(103) = 'L2        2 -1 -1  2     2 -2  2-13   122       '
         l(104) = 'L2A       2 -1 -1  2     2 -2  2      1 6       '
         l(105) = 'L2B       2 -1  1  2       -2         1 7       '
         l(106) = '2SK2      2       -2            +11   120       '
         l(107) = 'T2        2       -1  1                         '
         l(108) = 'S2        2                                     '
         l(109) = 'KP2       2                     -10   119       '
         l(110) = 'R2        2        1 -1        2                '
         l(111) = 'K2        2        2            -11   120       '
         l(112) = 'MSNU2     2  1  1 -2                            '
         l(113) = 'MSN2      2  1 -1                     2 6       '
         l(114) = 'ZETA2     2  1  1          -2         1 7       '
         l(115) = 'ETA2      2  1 -1  2       -2         1 7       '
         l(116) = 'KJ2       2  1 -1  2       -1 -2-10   1 4119    '
         l(117) = 'MKN2      2  1 -1  2            -11   2 6120    '
         l(118) = '2KM(SN)2  2  1 -1  4            -11-112 6220    '
         l(119) = '2SM2      2  2    -2    -2  2         1 6       '
         l(120) = 'SKM2      2  2          -2  2   -11   1 6120    '
         l(121) = '2MS2N2    2  2 -2                     2 6       '
         l(122) = '2SNU2     2  3  1 -4    -2  2         1 6       '
         l(123) = '2SN2      2  3 -1 -2    -2  2         1 6       '
         l(124) = 'SKN2      2  3 -1       -2  2   -11   1 6120    '
         l(125) = 'MQ3       3 -5  1  3     4 -3  1      1 31 6    '
         l(126) = 'NO3       3 -5  1  3     4 -3  1      1 31 6    '
         l(127) = 'MO3       3 -4     3     4 -3  1      1 31 6    '
         l(128) = '2MK3      3 -4     3     4 -4  1+10   2 6119    '
         l(129) = '2MP3      3 -4     5     4 -4 -1      2 6       '
         l(130) = 'M3        3 -3     3     3 -3         117       '
         l(131) = 'NK3       3 -3  1  3     2 -2 -1-10   1 6119    '
         l(132) = 'SO3       3 -2     1     2 -1  1      1 3       '
         l(133) = 'MP3       3 -2     1     2 -2  1      1 6119    '
         l(134) = 'MK3       3 -2     3     2 -2 -1-10   1 6119    '
         l(135) = 'SP3       3       -1           1                '
         l(136) = '2MQ3      3 -1 -1  3     2 -3 -1      1 32 6    '
         l(137) = 'SK3       3        1          -1-10   119       '
         l(138) = '2SO3      3  2    -1    -2  1 -1      1 3       '
         l(139) = 'K3        3        3          -1-10-11119120    '
         l(140) = '4MS4      4 -8     8     8 -8         4 6       '
         l(141) = '2MNS4     4 -7  1  6     6 -6         3 6       '
         l(142) = '3MK4      4 -6     4     6 -6   +11   3 6120    '
         l(143) = 'MNLK4     4 -6     4     6 -6  2+11-132 6120122 '
         l(144) = '3MS4      4 -6     6     6 -6         3 6       '
         l(145) = 'MSNK4     4 -5  1  2     4 -4   +11   2 6120    '
         l(146) = 'MN4       4 -5  1  4     4 -4         2 6       '
         l(147) = 'MNU4      4 -5 -1  6     4 -4         2 6       '
         l(148) = '2MLS4     4 -5 -1  6     6 -6  2-13   2 6122    '
         l(149) = '2MSK4     4 -4     2     4 -4   +11   2 6120    '
         l(150) = 'M4        4 -4     4     4 -4         2 6       '
         l(151) = '2MKS4     4 -4     6     4 -4   -11   2 6120    '
         l(152) = 'SN4       4 -3  1  2     2 -2         1 6       '
         l(153) = '3MN4      4 -3 -1  4     4 -4         4 6       '
         l(154) = '2SMK4     4 -2           2 -2   +11   1 6120    '
         l(155) = 'MS4       4 -2     2     2 -2         1 6       '
         l(156) = 'MK4       4 -2     4     2 -2   -11   1 6120    '
         l(157) = '2SNM4     4 -1  1                     2 6       '
         l(158) = '2MSN4     4 -1 -1  2     2 -2         3 6       '
         l(159) = 'SL4       4 -1 -1  2     2 -2  2-13   122       '
         l(160) = 'S4        4                                     '
         l(161) = 'SK4       4        2            -11   120       '
         l(162) = '2SMN4     4  1 -1                     2 6       '
         l(163) = '3SM4      4  2    -2    -2  2         1 6       '
         l(164) = '2SKM4     4  2          -2  2   -11   1 6120    '
         l(165) = 'MNO5      5 -7  1  5     6 -5  1      1 32 6    '
         l(166) = '3MK5      5 -6     5     6 -6  1+10   3 6119    '
         l(167) = '3MP5      5 -6     7     6 -6 -1      3 6       '
         l(168) = 'M5        5 -5  1  5     4 -5 -1-12   2 6121    '
         l(169) = 'MNK5      5 -5  1  5     4 -4 -1-10   2 6119    '
         l(170) = '2MP5      5 -4     3     4 -4  1      2 6       '
         l(171) = 'MSO5      5 -4     3     4 -3         1 31 6    '
         l(172) = '3MO5      5 -4     5     4 -5 -1      1 33 6    '
         l(173) = 'MSK5      5 -2     3     2 -2 -1-10   1 6119    '
         l(174) = '3KM5      5 -2     5     2 -2 -3-14   1 6319    '
         l(175) = '2(MN)S6   6-10  2  8     8 -8         4 6       '
         l(176) = '3MNS6     6 -9  1  8     8 -8         4 6       '
         l(177) = '4MK6      6 -8     6     8 -8   +11   4 6120    '
         l(178) = '2NM6      6 -8  2  6     6 -6         3 6       '
         l(179) = '4MS6      6 -8     8     8 -8         4 6       '
         l(180) = '2MSNK6    6 -7  1  4     6 -6   +11   3 6120    '
         l(181) = '2MN6      6 -7  1  6     6 -6         3 6       '
         l(182) = '2MNU6     6 -7 -1  8     6 -6         3 6       '
         l(183) = '3MSK6     6 -6     4     6 -6   +11   3 6120    '
         l(184) = 'M6        6 -6     6     6 -6         3 6       '
         l(185) = 'MSN6      6 -5  1  4     4 -4         2 6       '
         l(186) = 'MNK6      6 -5  1  6     4 -4   -11   2 6120    '
         l(187) = '4MN6      6 -5 -1  6     6 -6         5 6       '
         l(188) = 'MKNU6     6 -5 -1  8     4 -4   -11   2 6120    '
         l(189) = '2(MS)K6   6 -4     2     4 -4   +11   2 6120    '
         l(190) = '2MS6      6 -4     4     4 -4         2 6       '
         l(191) = '2MK6      6 -4     6     4 -4   -11   2 6120    '
         l(192) = '2SN6      6 -3  1  2     2 -2         1 6       '
         l(193) = '3MSN6     6 -3 -1  4     4 -4         4 6       '
         l(194) = 'MKL6      6 -3 -1  6     4 -4  2-11-131 6120122 '
         l(195) = '2SM6      6 -2     2     2 -2         1 6       '
         l(196) = 'MSK6      6 -2     4     2 -2   -11   1 6120    '
         l(197) = 'S6        6                                     '
         l(198) = '2MNO7     7 -9  1  7     8 -7  1      1 33 6    '
         l(199) = '2NMK7     7 -8  2  7     6 -6 -1-10   3 6119    '
         l(200) = 'M7        7 -7  1  7     6 -7 -1-12   3 6121    '
         l(201) = '2MSO7     7 -6     5     6 -5  1      1 32 6    '
         l(202) = 'MSKO7     7 -4     5     4 -3  1-11   1 31 6120 '
         l(203) = '2(MN)8    8-10  2  8     8 -8         4 6       '
         l(204) = '3MN8      8 -9  1  8     8 -8         4 6       '
         l(205) = '3MNKS8    8 -9  1 10     8 -8   -11   4 6120    '
         l(206) = 'M8        8 -8     8     8 -8         4 6       '
         l(207) = '2MSN8     8 -7  1  6     6 -6         3 6       '
         l(208) = '2MNK8     8 -7  1  8     6 -6   -11   3 6120    '
         l(209) = '3MS8      8 -6     6     6 -6         3 6       '
         l(210) = '3MK8      8 -6     8     6 -6   -11   3 6120    '
         l(211) = '2SNM8     8 -5  1  4     4 -4         2 6       '
         l(212) = 'MSNK8     8 -5  1  6     4 -4   -11   2 6120    '
         l(213) = '2(MS)8    8 -4     4     4 -4         2 6       '
         l(214) = '2MSK8     8 -4     6     4 -4   -11   2 6120    '
         l(215) = '3SM8      8 -2     2     2 -2         1 6       '
         l(216) = '2SMK8     8 -2     4     2 -2   -11   1 6120    '
         l(217) = 'S8        8                                     '
         l(218) = '2(MN)K9   9-10  2  9     8 -8 -1-10   4 6119    '
         l(219) = '3MNK9     9 -9  1  9     8 -8 -1-10   4 6119    '
         l(220) = '4MK9      9 -8     9     8 -8 -1-10   4 6119    '
         l(221) = '3MSK9     9 -6     7     6 -6 -1-10   3 6119    '
         l(222) = '4MN10    10-11  1 10    10-10         5 6       '
         l(223) = 'M10      10-10    10    10-10         5 6       '
         l(224) = '3MSN10   10 -9  1  8     8 -8         4 6       '
         l(225) = '4MS10    10 -8     8     8 -8         4 6       '
         l(226) = '2(MS)N10 10 -7  1  6     6 -6         3 6       '
         l(227) = '2MNSK10  10 -7  1  8     6 -6   -11   3 6120    '
         l(228) = '3M2S10   10 -6     6     6 -6         3 6       '
         l(229) = '4MSK11   11 -8     9     8 -8 -1-10   4 6119    '
         l(230) = 'M12      12-12    12    12-12         6 6       '
         l(231) = '4MSN12   12-11  1 10    10-10         5 6       '
         l(232) = '5MS12    12-10    10    10-10         5 6       '
         l(233) = '3MNKS12  12 -9  1 10     8 -8   -11   4 6120    '
         l(234) = '4M2S12   12 -8     8     8 -8         4 6       '
         l(235) = 'N4        4 -6  2  4     4 -4         2 6       '
      end subroutine kompbs

      ! =======================================================================

      !> Calculates the number of hours referred to January 1, 00:00 of the year 'JAAR' from a given date/time.
      !! meteo1 : datumi
      subroutine datumi(jaar, jdatum, t)
         integer              , intent(in)  :: jaar   !< Year
         integer, dimension(6), intent(in)  :: jdatum !< Date and time
         real(kind=hp),         intent(out) :: t      !< Time in hours referred to January 1, 00:00 of the year 'JAAR'
         !
         integer                      :: i     !< loop counter
         integer                      :: jhulp !< Help var.
         integer                      :: mnd   !< Help var. for the month
         real(kind=hp)                :: rlen  !< Length of a year in hours
         real(kind=hp), dimension(12) :: rmd   !< The number of days of the cumulated counted months
         !
         rmd(1)  =   0.0_hp
         rmd(2)  =  31.0_hp
         rmd(3)  =  59.0_hp
         rmd(4)  =  90.0_hp
         rmd(5)  = 120.0_hp
         rmd(6)  = 151.0_hp
         rmd(7)  = 181.0_hp
         rmd(8)  = 212.0_hp
         rmd(9)  = 243.0_hp
         rmd(10) = 273.0_hp
         rmd(11) = 304.0_hp
         rmd(12) = 334.0_hp
         !
         jhulp = jdatum(1)
         !
         ! Calculate month definitions for leap-years:
         ! year divisible by 4 minus centuries which are not divisible by 4
         if (mod(jhulp, 4) == 0) then
            if (mod(jhulp, 100)/=0 .or. mod(jhulp, 400)==0) then
               do i = 3, 12
                  rmd(i) = rmd(i) + 1d0
               enddo
            endif
         endif
         !
         mnd = jdatum(2)
         t = rmd(mnd)*24.0_hp + real(jdatum(3) - 1, hp)*24.0_hp + real(jdatum(4), hp) &
                              + real(jdatum(5), hp)/60.0_hp     + real(jdatum(6), hp)/3600.0_hp
         !
         ! hypothetical case (jhulp = jdatum(1) and jaar = jdatum(1))
         !
         if (jhulp /= jaar) then
            rlen = 8760.0_hp
            if (jhulp <= jaar) then
               if (mod(jhulp, 4) == 0) rlen = 8784.0_hp
               t = t - rlen
            else
               if (mod(jaar, 4) == 0) rlen = 8784.0_hp
               t = t + rlen
            endif
         endif
      end subroutine datumi

      ! =======================================================================

      !> Calulates helper variables V and F.
      !! meteo1 : hulpgr
      subroutine hulpgr(jaar, tm1, v, f)
         integer                     , intent(in)  :: jaar !< Present year
         real(kind=hp)               , intent(in)  :: tm1  !< Given time in hours referred to January 1, 00:00:00
         real(kind=hp), dimension(15), intent(out) :: v    !< Help var. to calculate V0U()
         real(kind=hp), dimension(25), intent(out) :: f    !< Help var. to calculate FR()
         !
         integer       :: ischrk  !< Number of leap-years since 1900
         integer       :: j
         real(kind=hp) :: ci
         real(kind=hp) :: ci4
         real(kind=hp) :: cri
         real(kind=hp) :: dhalf   !< Value for 0.5 in SIGN function
         real(kind=hp) :: p
         real(kind=hp) :: pix2    !< PI*2.
         real(kind=hp) :: q
         real(kind=hp) :: ri
         real(kind=hp) :: rjaar   !< Real value of JAAR - 1900
         real(kind=hp) :: rk
         real(kind=hp) :: rn1
         real(kind=hp) :: s2ri
         real(kind=hp) :: si
         real(kind=hp) :: si4
         real(kind=hp) :: sri
         real(kind=hp) :: sri3
         real(kind=hp) :: tm3     !< ISCHRK + TM1/24.0, i.e. the number of correction-days since January 1, 1900 00:00 hour, after the length of a year is set to 365 days in the first instance
         real(kind=hp) :: z
         !
         ! Calculate tm3 assuming tm1 plus the number of additional leap-years
         ! since 1900. Centuries which are indivisible by 4 are not leap-years.)
         !
         pix2   = pi * 2.0_hp
         dhalf  = 0.5_hp
         rjaar  = real(jaar - 1900, fp)
         ischrk = int((rjaar - 0.99_hp)/4.0_hp) - int((rjaar - 0.99_hp)/100.0_hp) &
                                                + int((rjaar + 300.0_hp - 0.99_hp)/400.0_hp)
         tm3    = real(ischrk, fp) + real(tm1/24.0_hp, fp)
         !
         v(1) = (180.000_hp + 360.0000000_hp*tm3)*degrad
         v(2) = (277.026_hp + 129.3848200_hp*rjaar + 13.176396800000_hp*tm3)*degrad
         v(3) = (334.384_hp + 40.6624700_hp *rjaar +  0.111404000000_hp*tm3)*degrad
         v(4) = (280.190_hp - 0.2387136_hp  *rjaar +  0.985647360000_hp*tm3)*degrad
         v(5) = (281.221_hp + 0.0171800_hp  *rjaar +  0.000047064943_hp*tm3)*degrad
         v(8) = (259.156_hp + 340.6718100_hp*rjaar -  0.052953945000_hp*tm3)*degrad
         !
         z = 0.009415_hp
         p = atan(z*sin(v(8))/(1.0_hp + z*(1.0_hp - cos(v(8)))))
         z = -0.17794_hp
         q = atan(z*sin(v(8))/(1.0_hp + z*(1.0_hp - cos(v(8)))))
         !
         v(6) = -p - q
         v(7) = p - q
         !
         rk = 0.9137_hp - 0.03569_hp*cos(v(8))
         ri = atan(sqrt(1.0_hp - rk*rk)/rk)
         !
         v(9) = ri
         !
         p   = mod(v(3), pix2) - pix2*(sign(dhalf, v(3)) - dhalf)
         rk  = v(6)
         rn1 = v(7)
         !
         ! Initialization of common arguments
         !
         s2ri = sin(2.0_hp*ri)
         sri  = sin(ri)
         si   = sin(0.5_hp*ri)
         cri  = cos(ri)
         ci   = cos(0.5_hp*ri)
         !
         v(10) = atan(s2ri*sin(rn1)/(s2ri*cos(rn1) + 0.3347_hp))
         v(11) = atan(sri*sri*sin(2.0_hp*rn1)/(sri*sri*cos(2.0_hp*rn1) + 0.0727_hp))
         v(12) = atan(sin(2.0_hp*(p - rk))/(3.0_hp*cri/(ci*ci) + cos(2.0_hp*(p - rk))))
         v(13) = atan(sin(2.0_hp*(p - rk))/(ci*ci/(si*si*6.0_hp) - cos(2.0_hp*(p - rk))))
         v(14) = 3.0_hp*v(10)
         v(15) = 0.0_hp
         !
         ! Reduce all angles to range 0 - 2*pi radials
         !
         do j = 1, 15
            v(j) = mod(v(j), pix2) - pix2*(sign(dhalf, v(j)) - dhalf)
         enddo
         !
         ci4  = ci*ci*ci*ci
         si4  = si*si*si*si
         sri3 = sri*sri*sri
         !
         f(1)  = (2.0_hp/3.0_hp - sri*sri)/0.5021_hp
         f(2)  = sri*sri/0.1578_hp
         f(3)  = sri*ci*ci/0.38_hp
         f(4)  = s2ri/0.7214_hp
         f(5)  = sri*si*si/0.0164_hp
         f(6)  = ci4/0.9154_hp
         f(7)  = sri*sri/0.1565_hp
         f(8)  = si4/0.0017_hp
         f(9)  = (sri - 1.25_hp*sri3)/0.3192_hp
         f(10) = sri3/0.063_hp
         f(11) = sri*sri*ci*ci/0.1518_hp
         f(12) = (1d0 - 10.0_hp*si*si + 15.0_hp*si4)*ci*ci/0.5873_hp
         f(13) = (1d0 - 10.0_hp*ci*ci + 15.0_hp*ci4)*si*si/0.2147_hp
         f(14) = sri*ci4/0.3658_hp
         f(15) = (ci*ci - 2.0_hp/3.0_hp)*sri*ci*ci/0.1114_hp
         f(16) = (ci*ci - 1.0_hp/3.0_hp)*sri*si*si/0.0103_hp
         f(17) = ci4*ci*ci/0.8758_hp
         f(18) = ci4*si*si/0.038_hp
         f(19) = sqrt(0.8965_hp*s2ri*s2ri + 0.6001_hp*s2ri*cos(rn1) + 0.1006_hp)
         f(20) = sqrt(19.0444_hp*sri3*sri + 2.7702_hp*sri*sri*cos(2.0_hp*rn1) + 0.0981_hp)
         f(21) = 6.0_hp*cri*cos(2.0_hp*(p - rk))/(ci*ci) + 9.0_hp*cri*cri/(ci4)
         f(21) = 2.6316_hp*sri*ci*ci*0.5_hp*sqrt(1.0_hp + f(21))
         f(22) = 36.0_hp*si4/(ci4) - 12.0_hp*si*si/(ci*ci)*cos(2.0_hp*(p - rk))
         f(22) = 1.0924_hp*ci4*sqrt(1.0_hp + f(22))
      end subroutine hulpgr

      ! =======================================================================

      function asc_map_components(kcmp, inaam, knum) result (nmissing)
         integer                                         :: nmissing
         integer,                            intent(in)  :: kcmp        !<
         character(len=8), dimension(kcmp),  intent(in)  :: inaam       !< Name of the referenced components
         integer,          dimension(kcmp),  intent(out) :: knum        !< If valid component, Kompbes number, else -1

         integer :: i, j

         nmissing = 0
         knum = -1
         if (.not. is_initialized) then
            call read_kompbes()
         end if
         do i = 1, kcmp                ! loop over given components
            if (inaam(i) == 'A0') then
               knum(i) = 0
            endif
            do j = 1, mxkc             ! loop over the elements of kompbes
               if (inaam(i) == kb_keys(j)) then
                  knum(i) = j          ! maps every used labelled component
               endif
            enddo
            if (knum(i) < 0) then
               nmissing = nmissing + 1
            endif
         enddo
      end function asc_map_components
      ! =======================================================================


      subroutine bewvuf_by_number (ierrs, knum, w, v0u, fr, v, f)
         integer,                           intent(out) :: ierrs !<  Number of error messages
         integer,       dimension(kcmp),    intent(in)  :: knum  !< Kompbes number of the referenced component
         real(kind=hp), dimension(kcmp)                 :: w     !< Angular velocity of the referenced components
         real(kind=hp), dimension(kcmp)                 :: v0u   !< Astronomical arguments of the  referenced components [rad]
         real(kind=hp), dimension(kcmp)                 :: fr    !< Amplitude factors for the referenced components
         real(kind=hp), dimension(15),      intent(in)  :: v     !< Help var. to calculate V0U()
         real(kind=hp), dimension(25),      intent(in)  :: f     !< Help var. to calculate FR()
         !
         integer       :: ia1   !<
         integer       :: ia2   !<
         integer       :: iar   !<
         integer       :: ie1   !<
         integer       :: ie2   !<
         integer       :: iex   !<
         integer       :: ikomp !<
         integer       :: j     !<
         integer       :: kw    !<
         integer       :: kx    !<
         integer       :: mh    !<
         integer       :: mp    !<
         integer       :: mp1   !<
         integer       :: ms    !<
         integer       :: mt    !<
         real(kind=hp) :: dhalf !< Value for 0.5 in SIGN function
         real(kind=hp) :: pix2  !<
         real(kind=hp) :: s1    !<
         real(kind=hp) :: s2    !<
         !
         ierrs = 0         ! status variable to be used later. no longer needed for invalid component labels
         pix2 = pi * 2.0_hp
         dhalf = 0.5_hp
         ! loop over given components
         do ikomp = 1, kcmp
            j = knum(ikomp)
            if (j<0) then              ! Component number<0: component was unknown, set amplitude to zero
               fr(1) = 0d0
               return
            endif
            ! compute angular velocity
            mt = kb_values(16*j - 15)
            ms = kb_values(16*j - 14)
            mp = kb_values(16*j - 13)
            mh = kb_values(16*j - 12)
            mp1 = kb_values(16*j - 11)
            w(ikomp) = mt*15.0_hp + ms*0.54901653_hp + mp*0.0046418333_hp &
                                & + mh*0.04106864_hp + mp1*0.0000019610393_hp
            w(ikomp) = (w(ikomp)*pix2)/360.0_hp
            ! compute v0+u
            v0u(ikomp) = (kb_values(16*j - 8)*pix2)/4.0_hp
            do kw = 1, 7
               kx = 16*j - 16 + kw
               v0u(ikomp) = v0u(ikomp) + v(kw)*kb_values(kx)
            enddo
            ie1 = kb_values(16*j - 7)
            if (ie1 /= 0) then
               ia1 = abs(ie1)
               s1 = real(ie1/ia1, fp)
               v0u(ikomp) = v0u(ikomp) + s1*v(ia1)
               ie2 = kb_values(16*j - 6)
               if (ie2 /= 0) then
                  ia2 = abs(ie2)
                  s2 = real(ie2/ia2, fp)
                  v0u(ikomp) = v0u(ikomp) + s2*v(ia2)
               endif
            endif
            v0u(ikomp) = mod(v0u(ikomp), pix2) - pix2*(sign(dhalf, v0u(ikomp)) - dhalf)
            ! compute f
            fr(ikomp) = 1.0_hp
            iex = kb_values(16*j - 5)
            if (iex /= 0) then
               iar = kb_values(16*j - 4)
               fr(ikomp) = (f(iar))**iex
               iex = kb_values(16*j - 3)
               if (iex /= 0) then
                  iar = kb_values(16*j - 2)
                  fr(ikomp) = fr(ikomp)*(f(iar))**iex
                  iex = kb_values(16*j - 1)
                  if (iex /= 0) then
                     iar = kb_values(16*j)
                     fr(ikomp) = fr(ikomp)*(f(iar))**iex
                  endif
               endif
            endif
         enddo
      end subroutine bewvuf_by_number

      ! =======================================================================

   end module m_ec_astro
