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
module m_partmp
   use m_waq_precision

   implicit none

contains

   subroutine partmp(process_space_real, fl, ipoint, increm, num_cells, &
                     noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                     num_exchanges_z_dir, num_exchanges_bottom_dir)
      use m_logger_helper, only: stop_with_error, write_error_message, get_log_unit_number
      use m_extract_waq_attribute

      !>\file
      !>       Partitioning of micropollutants

      !
      !     Description of the module :
      !
      !        General water quality module for DELWAQ:
      !
      !        PARTITIONING OF MICROPOLLUTANTS: FREE DISSOLVED AND ADSORBED TO
      !        INORGANIC MATTER (THREE FRACTIONS), ORGANIC MATTER (SUM OF DETC
      !        OOC) AND ALGAE (3 GROUPS). NON-EQUILIBRIUM PARTITIONING
      !
      ! Name    T   L I/O   Description                              Units
      ! ----    --- -  -    -------------------                      ----
      ! AIMi    R*4 1 I  mass of inorganic SS                           [g/m3]
      ! CDIS    R*4 1 O  concentration free dissolved MP                [g/m3]
      ! CDOC    R*4 1 O  concentration adsorbed to DOC                  [g/m3]
      ! DELT    R*4 1 I  DELWAQ timestep                                   [d]
      ! DOC     R*4 1 I  dissolved organic Carbon                      [gC/m3]
      ! FACi    R*4 1 L  product of conc. sorbent and Kd                   [-]
      ! FDIS    R*4 1 L  fraction free dissolved in equilibrium            [-]
      ! FDIS2   R*4 1 O  fraction free dissolved                           [-]
      ! FDCOR   R*4 1 L  factor actual/eq. for free dissolved and DOC fr.  [-]
      ! FDOC    R*4 1 O  fraction sorbed to DOC                            [-]
      ! FIMi    R*4 1 O  fraction sorbed to IMi                            [-]
      ! FPARTE  R*4 1 L  fraction particulate in equilibrium               [-]
      ! FPARTO  R*4 1 L  fraction particulate begin of present time step   [-]
      ! FPART   R*4 1 L  fraction particulate end of present time step     [-]
      ! FPCOR   R*4 1 L  factor actual/eq. for POC-PHYT-IMx fractions      [-]
      ! FPHYT   R*4 1 O  fraction sorbed to PHYT                           [-]
      ! FPOC    R*4 1 O  fraction sorbed to POC                            [-]
      ! MP      R*4 1 L  concentration MP waterphase                    [g/m3]
      ! MPDIS   R*4 1 I  concentration MP waterphase [free+DOC]         [g/m3]
      ! MPPAR   R*4 1 I  concentration MP waterphase [IMx+POC+PHYT]     [g/m3]
      ! MPHUM   R*4 1 I  concentration MP waterphase [IMx+DOC+POC+PHYT] [g/m3]
      ! HVTADS  R*4 1 I  half life time kinetic adsorption                 [d]
      ! HVTDES  R*4 1 I  half life time kinetic adsorption                 [d]
      ! KDIMi   R*4 1 I  partition coefficient inorganic matter       [m3/gDM]
      ! KDOC    R*4 1 L  partition coefficient organic carbon          [m3/gC]
      ! KPHYT   R*4 1 I  partition coefficient phytoplankton           [m3/gC]
      ! KPOC    R*4 1 I  partition coefficient organic carbon          [m3/gC]
      ! PHYT    R*4 1 I  phytoplankton                                 [gC/m3]
      ! POC     R*4 1 I  particulate organic Carbon                    [gC/m3]
      ! POR     R*4 1 I  porosity waterphase                           [m3/m3]
      ! RATE    R*4 1 L  actual rate constant kinetic sorption           [1/d]
      ! Qy      R*4 1 O  quality of MP in adsorbens y                    [g/g]
      ! XDOC    R*4 1 I  KdDOC/KdPOC                                       [-]

      !     Logical Units : -

      implicit real(A - H, J - Z)

      real(kind=real_wp) :: process_space_real(*), FL(*)
      integer(kind=int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                              IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

      logical IM1OPT, IM2OPT, IM3OPT, IM4OPT, IM5OPT, IM6OPT, &
         FFFOPT, QQQOPT, WATOPT, HVTOPT, TWOFRC, QUALOPT
      integer(kind=int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10, &
                              IP11, IP12, IP13, IP14, IP15, IP16, IP17, IP18, IP19, IP20, &
                              IP21, IP22, IP23, IP24, IP25, IP26, IP27, IP28, IP29, IP30, &
                              IP31, IP32, IP33, IP34, IP35, IP36, IP37, IP38, IP39, IP40, &
                              IP41, IP42, IP43, IP44, IP45, IP46, IP47, IP48, IP49, IP50
      integer(kind=int_wp) :: IN1, IN2, IN3, IN4, IN5, IN6, IN7, IN8, IN9, IN10, &
                              IN11, IN12, IN13, IN14, IN15, IN16, IN17, IN18, IN19, IN20, &
                              IN21, IN22, IN23, IN24, IN25, IN26, IN27, IN28, IN29, IN30, &
                              IN31, IN32, IN33, IN34, IN35, IN36, IN37, IN38, IN39, IN40, &
                              IN41, IN42, IN43, IN44, IN45, IN46, IN47, IN48, IN49, IN50
      real(kind=real_wp) :: AIM1, KDIM1, FAC1, AIM2, KDIM2, FAC2, &
                            AIM3, KDIM3, FAC3, POC, KPOC, FAC4, &
                            PHYT, KPHYT, FAC5, DOC, XDOC, KDOC, &
                            FAC6, HVTADS, HVTDES, MPPAR, MPDIS, DELT, &
                            MP, POR, FPREC, SUMKD, FDIS, AFACT, &
                            MPHUM, FPARTE, FPARTO, RATE, FPCOR, FDCOR, &
                            FPART, PH, KCROHS, KCROH1, KCROH2, KCROH3, &
                            OH, KSOL, CDIS, CRTOT, CRFREE, MOLWT, &
                            FIM1, FIM2, FIM3, FPOC, FPHYT, FDOC, &
                            QIM1, QIM2, QIM3, QPOC, QPHYT, CDOC, &
                            DISS, DISHS, LKSOL, LKMES, LKMEHS, FSULF, &
                            FDIS2, CDISM, QUAL, KDALL, VOLUME, IAP, &
                            FAC6C
      integer(kind=int_wp) :: LUNREP, IKMRK2
      integer(kind=int_wp) :: IFLUX, ISEG, ISWOX, IGROUP
      logical SEDIME
      !
      IP1 = IPOINT(1)
      IP2 = IPOINT(2)
      IP3 = IPOINT(3)
      IP4 = IPOINT(4)
      IP5 = IPOINT(5)
      IP6 = IPOINT(6)
      IP7 = IPOINT(7)
      IP8 = IPOINT(8)
      IP9 = IPOINT(9)
      IP10 = IPOINT(10)
      IP11 = IPOINT(11)
      IP12 = IPOINT(12)
      IP13 = IPOINT(13)
      IP14 = IPOINT(14)
      IP15 = IPOINT(15)
      IP16 = IPOINT(16)
      IP17 = IPOINT(17)
      IP18 = IPOINT(18)
      IP19 = IPOINT(19)
      IP20 = IPOINT(20)
      IP21 = IPOINT(21)
      IP22 = IPOINT(22)
      IP23 = IPOINT(23)
      IP24 = IPOINT(24)
      IP25 = IPOINT(25)
      IP26 = IPOINT(26)
      IP27 = IPOINT(27)
      IP28 = IPOINT(28)
      IP29 = IPOINT(29)
      IP30 = IPOINT(30)
      IP31 = IPOINT(31)
      IP32 = IPOINT(32)
      IP33 = IPOINT(33)
      IP34 = IPOINT(34)
      IP35 = IPOINT(35)
      IP36 = IPOINT(36)
      IP37 = IPOINT(37)
      IP38 = IPOINT(38)
      IP39 = IPOINT(39)
      IP40 = IPOINT(40)
      IP41 = IPOINT(41)
      IP42 = IPOINT(42)
      IP43 = IPOINT(43)
      IP44 = IPOINT(44)
      IP45 = IPOINT(45)
      IP46 = IPOINT(46)
      IP47 = IPOINT(47)
      IP48 = IPOINT(48)
      IP49 = IPOINT(49)
      IP50 = IPOINT(50)
      !
      IN1 = INCREM(1)
      IN2 = INCREM(2)
      IN3 = INCREM(3)
      IN4 = INCREM(4)
      IN5 = INCREM(5)
      IN6 = INCREM(6)
      IN7 = INCREM(7)
      IN8 = INCREM(8)
      IN9 = INCREM(9)
      IN10 = INCREM(10)
      IN11 = INCREM(11)
      IN12 = INCREM(12)
      IN13 = INCREM(13)
      IN14 = INCREM(14)
      IN15 = INCREM(15)
      IN16 = INCREM(16)
      IN17 = INCREM(17)
      IN18 = INCREM(18)
      IN19 = INCREM(19)
      IN20 = INCREM(20)
      IN21 = INCREM(21)
      IN22 = INCREM(22)
      IN23 = INCREM(23)
      IN24 = INCREM(24)
      IN25 = INCREM(25)
      IN26 = INCREM(26)
      IN27 = INCREM(27)
      IN28 = INCREM(28)
      IN29 = INCREM(29)
      IN30 = INCREM(30)
      IN31 = INCREM(31)
      IN32 = INCREM(32)
      IN33 = INCREM(33)
      IN34 = INCREM(34)
      IN35 = INCREM(35)
      IN36 = INCREM(36)
      IN37 = INCREM(37)
      IN38 = INCREM(38)
      IN39 = INCREM(39)
      IN40 = INCREM(40)
      IN41 = INCREM(41)
      IN42 = INCREM(42)
      IN43 = INCREM(43)
      IN44 = INCREM(44)
      IN45 = INCREM(45)
      IN46 = INCREM(46)
      IN47 = INCREM(47)
      IN48 = INCREM(48)
      IN49 = INCREM(49)
      IN50 = INCREM(50)
      !
      !     Check sediment switch for first segment
      !
      SEDIME = .false.
      if (process_space_real(IP30) > 0.5) SEDIME = .true.
      !
      !     Find metal group for first segment
      !     1 = GENERAL (ZN, CU, CD, PB, HG, NI)
      !     2 = CR
      !     3 = VA, AS
      !     4 = OMP's
      !     5 = viruses (simple partitioning)
      !
      IGROUP = nint(process_space_real(IP31))
      !
      !     OPTIMALISATION PART OUTSIDE SEGMENT LOOP
      !
      !        inorganic matter 1
      !
      if (IN4 == 0 .and. IN11 == 0) then
         AIM1 = process_space_real(IP4)
         KDIM1 = process_space_real(IP11) / 1000.
         FAC1 = AIM1 * KDIM1
         IM1OPT = .true.
      else
         IM1OPT = .false.
      end if
      !        inorganic matter 2
      if (IN5 == 0 .and. IN12 == 0) then
         AIM2 = process_space_real(IP5)
         KDIM2 = process_space_real(IP12) / 1000.
         FAC2 = AIM2 * KDIM2
         IM2OPT = .true.
      else
         IM2OPT = .false.
      end if
      !        inorganic matter 3
      if (IN6 == 0 .and. IN13 == 0) then
         AIM3 = process_space_real(IP6)
         KDIM3 = process_space_real(IP13) / 1000.
         FAC3 = AIM3 * KDIM3
         IM3OPT = .true.
      else
         IM3OPT = .false.
      end if
      !        POC
      if (IN9 == 0 .and. IN14 == 0) then
         POC = process_space_real(IP9)
         if (IGROUP == 4) then
            KPOC = 10**process_space_real(IP14) / 1.e+6
         else
            KPOC = process_space_real(IP14) / 1000.
         end if
         FAC4 = POC * KPOC
         IM4OPT = .true.
      else
         IM4OPT = .false.
      end if
      !        Phytoplankton
      if (IN10 == 0 .and. IN15 == 0) then
         PHYT = process_space_real(IP10)
         if (IGROUP == 4) then
            KPHYT = 10**process_space_real(IP15) / 1.e+6
         else
            KPHYT = process_space_real(IP15) / 1000.
         end if
         FAC5 = PHYT * KPHYT
         IM5OPT = .true.
      else
         IM5OPT = .false.
      end if
      !        DOC and XDOC
      if (IN7 == 0 .and. IN8 == 0 .and. &
          IN14 == 0) then
         DOC = process_space_real(IP7)
         XDOC = process_space_real(IP8)
         if (IGROUP == 4) then
            KDOC = 10**process_space_real(IP14) / 1.e+6
         else
            KDOC = process_space_real(IP14) / 1000.
         end if
         FAC6 = DOC * XDOC * KDOC
         IM6OPT = .true.
      else
         IM6OPT = .false.
      end if
      !
      !        Kinetic sorption
      !
      if (IN20 == 0 .and. IN21 == 0) then
         HVTADS = process_space_real(IP20)
         HVTDES = process_space_real(IP21)
         HVTOPT = .true.
      else
         HVTOPT = .false.
      end if
      !
      !     Check for modelling of total or dis/par
      !
      if (IN2 == 0 .and. IN3 == 0) then
         TWOFRC = .false.
      else
         TWOFRC = .true.
      end if

      if (TWOFRC) then
         if (IN1 /= 0) then
            call get_log_unit_number(LUNREP)
            write (LUNREP, *) &
               'PARTMP: MP with the combination MPDIS/MPPAR is invalid'
            write (*, *) &
               'PARTMP: MP with the combination MPDIS/MPPAR is invalid'
            call stop_with_error()
         end if
         if (IN2 == 0) then
            call get_log_unit_number(LUNREP)
            write (LUNREP, *) &
               'PARTMP: No value for MPDIS in dis/par modelling'
            write (*, *) 'PARTMP: No value for MPDIS in dis/par modelling'
            call stop_with_error()
         end if
         if (IN3 == 0) then
            call get_log_unit_number(LUNREP)
            write (LUNREP, *) &
               'PARTMP: No value for MPPAR in dis/par modelling'
            write (*, *) 'PARTMP: No value for MPPAR in dis/par modelling'
            call stop_with_error()
         end if
      else
         if (IN2 > 0) then
            call get_log_unit_number(LUNREP)
            write (LUNREP, *) &
               'PARTMP: Values for MPDIS and MP!'
            write (*, *) 'PARTMP: Values for MPDIS and MP!'
            call stop_with_error()
         end if
         if (IN3 > 0) then
            call get_log_unit_number(LUNREP)
            write (LUNREP, *) &
               'PARTMP: Values for MPPAR and MP!'
            write (*, *) 'PARTMP: Values for MPPAR and MP!'
            call stop_with_error()
         end if
      end if
      !        Compute F-values or not
      if (IN34 == 0 .and. IN35 == 0 .and. &
          IN36 == 0 .and. IN37 == 0 .and. &
          IN38 == 0 .and. IN39 == 0 .and. &
          IN49 == 0 .and. IN50 == 0) then
         FFFOPT = .true.
      else
         FFFOPT = .false.
      end if
      !        Compute Q-values or not
      if (IN43 == 0 .and. IN44 == 0 .and. &
          IN45 == 0 .and. IN46 == 0 .and. &
          IN47 == 0) then
         QQQOPT = .true.
      else
         QQQOPT = .false.
      end if
      !        Compute Waterphase-values or not
      if (IN33 == 0 .and. IN41 == 0 .and. &
          IN42 == 0 .and. IN49 == 0 .and. &
          IN50 == 0) then
         WATOPT = .true.
      else
         WATOPT = .false.
      end if
      !        Compute qual or not
      if (IN49 == 0 .and. IN50 == 0) then
         QUALOPT = .true.
      else
         QUALOPT = .false.
      end if
      DELT = process_space_real(IP22)
      if (DELT < 1e-20) call write_error_message('DELT in PARTMP zero')
      !
      !----------------------------------------------------------------------C
      !     SEGMENT LOOP
      !----------------------------------------------------------------------C
      !
      IFLUX = 0
      do ISEG = 1, num_cells

         if (btest(IKNMRK(ISEG), 0)) then
            call extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
            if ((IKMRK2 == 0 .or. IKMRK2 == 3) .or. .not. SEDIME) then

               MP = process_space_real(IP1)
               MPDIS = process_space_real(IP2)
               MPPAR = process_space_real(IP3)
               ISWOX = nint(process_space_real(IP23))

               if (TWOFRC) then
                  if (IGROUP == 2) then
                     call get_log_unit_number(LUNREP)
                     write (LUNREP, *) &
                        'PARTMP: Kinetic sorption for Chromium not ', &
                        'implemented!'
                     write (*, *) 'PARTMP: Kinetic sorption for Chromium not ', &
                        'implemented!'
                     call stop_with_error()
                  end if
                  MP = MPDIS + MPPAR
               end if
               process_space_real(IP48) = MP
               POR = process_space_real(IP16)
               THICK = process_space_real(IP17)
               SURF = process_space_real(IP18)
               DMS1 = process_space_real(IP19)
               VOLUME = process_space_real(IP32)

               FL(1 + IFLUX) = 0.0
               !
               !     OXIC (ISWOX = 1) PARTITIONING FOR GENERAL METALS (GROUP 1)
               !     OR CR (GROUP 2) OR VA/AS (GROUP 3) or OMP's (GROUP 4)
               !     OR viruses (group 5)
               !
               if ((ISWOX == 1 .and. IGROUP == 1) .or. &
                   IGROUP == 2 .or. &
                   IGROUP == 3 .or. &
                   IGROUP == 4 .or. &
                   IGROUP == 5) then
                  FPREC = 0.0

                  !          inorganic matter 1
                  if (.not. IM1OPT) then
                     AIM1 = process_space_real(IP4)
                     KDIM1 = process_space_real(IP11) / 1000.
                     FAC1 = AIM1 * KDIM1
                  end if
                  !          inorganic matter 2
                  if (.not. IM2OPT) then
                     AIM2 = process_space_real(IP5)
                     KDIM2 = process_space_real(IP12) / 1000.
                     FAC2 = AIM2 * KDIM2
                  end if
                  !          inorganic matter 3
                  if (.not. IM3OPT) then
                     AIM3 = process_space_real(IP6)
                     KDIM3 = process_space_real(IP13) / 1000.
                     FAC3 = AIM3 * KDIM3
                  end if
                  !          POC
                  if (.not. IM4OPT) then
                     POC = process_space_real(IP9)
                     if (IGROUP == 4) then
                        KPOC = 10**process_space_real(IP14) / 1.e+6
                     else
                        KPOC = process_space_real(IP14) / 1000.
                     end if
                     FAC4 = POC * KPOC
                  end if
                  !          Phytoplankton
                  if (.not. IM5OPT) then
                     PHYT = process_space_real(IP10)
                     if (IGROUP == 4) then
                        KPHYT = 10**process_space_real(IP15) / 1.e+6
                     else
                        KPHYT = process_space_real(IP15) / 1000.
                     end if
                     FAC5 = PHYT * KPHYT
                  end if
                  !          DOC and XDOC
                  if (.not. IM6OPT) then
                     DOC = process_space_real(IP7)
                     XDOC = process_space_real(IP8)
                     if (IGROUP == 4) then
                        KDOC = 10**process_space_real(IP14) / 1.e+6
                     else
                        KDOC = process_space_real(IP14) / 1000.
                     end if
                     FAC6 = DOC * XDOC * KDOC
                  end if
                  !
                  if (.not. SEDIME .and. POR < 1e-20) &
                     call write_error_message('POR in PARTMP zero')
                  !
                  !***********************************************************************
                  !**** Processes connected to the HEAVY METAL PARTITIONING
                  !***********************************************************************
                  !
                  !       Convert DOC (g/m3) to (g/m2) FOR SEDIMENT
                  !
                  FAC6C = FAC6
                  if (SEDIME) FAC6C = FAC6 * POR * THICK
                  !
                  !       Partitioning (free dissolved and adsorbed)
                  !
                  SUMKD = FAC1 + FAC2 + FAC3 + FAC4 + FAC5 + FAC6C

                  if (SUMKD > 1.e-20) then
                     if (SEDIME) then
                        if (THICK > 1.e-20) then
                           FDIS = POR / (POR + SUMKD / THICK)
                        else
                           FDIS = 1.0
                        end if
                     else
                        FDIS = POR / (POR + SUMKD)
                     end if
                     AFACT = (1.0 - FDIS) / SUMKD
                  else
                     FDIS = 1.0
                     AFACT = 0.0
                  end if

                  MPHUM = AFACT * MP
                  !
                  !       Equilibrium fraction PART (excluding DOC!!!)
                  !
                  FPARTE = AFACT * (FAC1 + FAC2 + FAC3 + FAC4 + FAC5)
                  !
                  !       Kinetic sorption
                  !
                  if (.not. HVTOPT) then
                     HVTADS = process_space_real(IP20)
                     HVTDES = process_space_real(IP21)
                  end if
                  if (HVTADS > 1e-20 .or. HVTDES > 1e-20) then
                     if (.not. TWOFRC) then
                        call get_log_unit_number(LUNREP)
                        write (LUNREP, *) &
                           'PARTMP: Kinetic sorption one MP fraction ', &
                           'not possible!'
                        write (LUNREP, *) &
                           'Create MP-Dis & MP-Part for kinetic sorption'
                        write (LUNREP, *) &
                           'or set HLTAdsMP and HLTDesMP to zero'
                        write (*, *) 'PARTMP: Kinetic sorption one MP fraction ', &
                           'not possible!'
                        write (*, *) 'Create MP-Dis & MP-Part for kinetic sorption'
                        write (*, *) 'or set HLTAdsMP and HLTDesMP to zero'
                        call stop_with_error()
                     end if
                     !           Let op: PART is in dit verband POC + IMx + PHYT !!!
                     !           Actual fraction PART
                     if (MP > 1e-20) then
                        FPARTO = MPPAR / MP
                     else
                        FPARTO = 0.0
                     end if
                     RATE = -1.0
                     if (FPARTO > FPARTE .and. HVTDES > 1e-20) &
                        RATE = log(2.) / HVTDES
                     if (FPARTO < FPARTE .and. HVTADS > 1e-20) &
                        RATE = log(2.) / HVTADS
                     !           Fraction part at end of present time step
                     if (RATE > 0.0 .and. FPARTE > 1e-20 &
                         .and. FPARTE < 1.0) then
                        FPART = FPARTE - (FPARTE - FPARTO) * exp(-RATE * DELT)
                        FPCOR = FPART / FPARTE
                        FDCOR = (1.0 - FPART) / (1.0 - FPARTE)
                     else
                        FPART = FPARTE
                        FPCOR = 1.0
                        FDCOR = 1.0
                     end if
                  else
                     FPART = FPARTE
                     FPCOR = 1.0
                     FDCOR = 1.0
                  end if
                  !
                  !       Flux from particulate to dissolved fraction
                  !
                  if (TWOFRC) then
                     if (SEDIME) then
                        FL(1 + IFLUX) = (FPART * MP - MPPAR) / DELT / VOLUME
                     else
                        FL(1 + IFLUX) = (FPART * MP - MPPAR) / DELT
                     end if
                  else
                     FL(1 + IFLUX) = 0.0
                  end if
                  !
                  !       CHROMIUM
                  !
                  if (IGROUP == 2) then
                     PH = process_space_real(IP24)
                     KCROHS = process_space_real(IP25)
                     MOLWT = process_space_real(IP26)
                     KCROH1 = process_space_real(IP27)
                     KCROH2 = process_space_real(IP28)
                     KCROH3 = process_space_real(IP29)

                     OH = 10.0**(-(14 - PH))
                     !
                     !-- 1.4 Determine solubility product (as a dissociation K !!).
                     !
                     KSOL = 10**(-KCROHS)
                     !
                     !-- 2.2 Determine concentration of free Cr3+ ,using fdis as calculated
                     !       under (2.1), and calculate IAP of Cr(OH)3s.
                     !
                     !       CDIS (mg/m3) corresponds with CRTOT (mol/l).
                     !       conversion = ug/l * 1.e-6 g/ug / (M  g/mol)
                     !
                     if (POR <= 1e-10) then
                        CDIS = 0.0
                     else
                        if (SEDIME) then
                           if (THICK > 1.e-20) then
                              CDIS = FDIS * MP / (POR * THICK)
                           else
                              CDIS = 0.0
                           end if
                        else
                           CDIS = FDIS * MP / POR
                        end if
                     end if
                     CRTOT = CDIS * 1.0e-3 / MOLWT
                     !
                     !--   Crtot = Cr3+ + (CrOH 2+) + (Cr(OH)2 +) + (Cr(OH)3 0+)
                     !     Cr3+ = Crtot / (1 + K1*OH + K2*(OH)^2 + K3*(OH)^3)
                     !     (mol/l), K's of the association reaction.
                     !
                     CRFREE = CRTOT / (1 + &
                                       10**(KCROH1) * OH + &
                                       10**(KCROH2) * OH**2 + &
                                       10**(KCROH3) * OH**3)
                     !
                     IAP = CRFREE * OH**3
                     !
                     !-- 2.3 If IAP > Ksol Cr(OH)3s precipitation will take place. In this
                     !       case fdis must be recalculated.
                     !       If IAP < Ksol no precipitation will take place and the under
                     !       (2.1) calculated fdis is correct.
                     !
                     if (IAP >= KSOL) then
                        !
                        !------ 2.3a Calculate Cr3+ and Crtot at present OH- concentration,
                        !            (mol/l) and convert to concentration in mg/m3 (= ug/l).
                        !
                        CRFREE = KSOL / (OH**3)
                        CRTOT = CRFREE * (1 + &
                                          10**(KCROH1) * OH + &
                                          10**(KCROH2) * OH**2 + &
                                          10**(KCROH3) * OH**3)
                        !
                        !------    Conversion = mol/l * M g/mol * 1.0e+6 ug/g
                        !
                        CRTOT = CRTOT * MOLWT * 1.0e+3
                        if (SEDIME) then
                           FPREC = 1.-(CRTOT * POR * THICK / (FDIS * MP))
                        else
                           FPREC = 1.-(CRTOT * POR / (FDIS * MP))
                        end if
                     end if
                  end if
                  !
                  process_space_real(IP40) = FPREC

                  if (.not. FFFOPT) then
                     FIM1 = AFACT * FAC1 * FPCOR * (1.-FPREC)
                     FIM2 = AFACT * FAC2 * FPCOR * (1.-FPREC)
                     FIM3 = AFACT * FAC3 * FPCOR * (1.-FPREC)
                     FPOC = AFACT * FAC4 * FPCOR * (1.-FPREC)
                     FPHYT = AFACT * FAC5 * FPCOR * (1.-FPREC)
                     FDOC = AFACT * FAC6C * FDCOR * (1.-FPREC)
                     process_space_real(IP34) = FDOC
                     process_space_real(IP35) = FIM1
                     process_space_real(IP36) = FIM2
                     process_space_real(IP37) = FIM3
                     process_space_real(IP38) = FPOC
                     process_space_real(IP39) = FPHYT
                  end if
                  !
                  if (.not. QQQOPT) then
                     QIM1 = 0.0
                     QIM2 = 0.0
                     QIM3 = 0.0
                     QPOC = 0.0
                     QPHYT = 0.0
                     if (MPHUM > 1e-20) then
                        if (AIM1 > 1e-20) QIM1 = MPHUM * KDIM1 * FPCOR * (1.-FPREC)
                        if (AIM2 > 1e-20) QIM2 = MPHUM * KDIM2 * FPCOR * (1.-FPREC)
                        if (AIM3 > 1e-20) QIM3 = MPHUM * KDIM3 * FPCOR * (1.-FPREC)
                        if (POC > 1e-20) QPOC = MPHUM * KPOC * FPCOR * (1.-FPREC)
                        if (PHYT > 1e-20) QPHYT = MPHUM * KPHYT * FPCOR * (1.-FPREC)
                     end if
                     process_space_real(IP43) = QIM1
                     process_space_real(IP44) = QIM2
                     process_space_real(IP45) = QIM3
                     process_space_real(IP46) = QPOC
                     process_space_real(IP47) = QPHYT
                  end if

                  if (.not. WATOPT) then
                     !@      Concentration free dissolved MP and DOC waterphase
                     FDIS = FDIS * (1.-FPREC)
                     FDIS2 = FDIS * FDCOR
                     if (POR <= 1.e-10) then
                        CDIS = 0.0
                        CDOC = 0.0
                     else
                        if (SEDIME) then
                           if (THICK > 1.e-20) then
                              CDIS = FDIS * MP * FDCOR / (THICK * POR)
                              CDOC = (1.0 - FPREC) * AFACT * FAC6C * MP * FDCOR / &
                                     (THICK * POR)
                           else
                              CDIS = 0.0
                              CDOC = 0.0
                           end if
                        else
                           CDIS = FDIS * MP * FDCOR / POR
                           CDOC = (1.0 - FPREC) * AFACT * FAC6C * MP * FDCOR / POR
                        end if
                     end if
                     !@      Quality of MP adsorbens waterphase
                     FDIS = FDIS2
                     process_space_real(IP33) = FDIS
                     process_space_real(IP41) = CDIS
                     process_space_real(IP42) = CDOC
                  end if
                  !
                  !     SULFIDIC PARTITIONING (ISWOX = 0) FOR GENERAL METALS
                  !
               elseif (ISWOX == 0 .and. IGROUP == 1) then
                  process_space_real(IP34) = 0.0
                  process_space_real(IP35) = 0.0
                  process_space_real(IP36) = 0.0
                  process_space_real(IP37) = 0.0
                  process_space_real(IP38) = 0.0
                  process_space_real(IP39) = 0.0
                  process_space_real(IP42) = 0.0
                  process_space_real(IP43) = 0.0
                  process_space_real(IP44) = 0.0
                  process_space_real(IP45) = 0.0
                  process_space_real(IP46) = 0.0
                  process_space_real(IP47) = 0.0

                  DISS = process_space_real(IP24)
                  DISHS = process_space_real(IP25)
                  if (DISS <= 1e-20) then
                     write (*, *) 'SwPoreChWK = ', ISWOX, ' 1- oxic, 0 - sulfidic, or'
                     write (*, *) 'DisSWK or DisSSx  = ', DISS, 'should not equal zero'
                     call write_error_message('Fatal error in PARTMP')
                  end if
                  MOLWT = process_space_real(IP26)
                  LKSOL = process_space_real(IP27)
                  LKMES = process_space_real(IP28)
                  LKMEHS = process_space_real(IP29)

                  if (DMS1 < 1e-03) then
                     FDIS = 1.0
                     if (SEDIME) then
                        if (THICK > 1.e-20) then
                           CDIS = MP / (THICK * POR)
                        else
                           CDIS = 0.0
                        end if
                     else
                        CDIS = MP / POR
                     end if
                     FSULF = 0.0
                  elseif (POR < 1e-10) then
                     FSULF = 1.0
                     FDIS = 0.0
                     CDIS = 0.0
                  else
                     CDISM = (1 / (10**LKSOL * DISS)) * &
                             (1 + 10**LKMES * DISS + 10**LKMEHS * DISHS)
                     CDIS = CDISM * MOLWT * 1000.
                     if (SEDIME) then
                        FDIS = CDIS * THICK * POR / MP
                     else
                        FDIS = CDIS * POR / MP
                     end if
                  end if
                  FSULF = 1.-FDIS
                  FDOC = 0.0
                  CDOC = 0.0

                  process_space_real(IP33) = FDIS
                  process_space_real(IP40) = FSULF
                  process_space_real(IP41) = CDIS

               else
                  call get_log_unit_number(LUNREP)

                  write (LUNREP, *) 'Invalid option for partitioning!'
                  write (LUNREP, *) 'SwOXIC= ', ISWOX, ' 1- oxic, 0 - anoxic'
                  write (LUNREP, *) &
                     'Group = ', IGROUP, ' 1- General, 2-Cr, 3-As/Va, 4-OMP, 5-virus'

                  write (*, *) 'Invalid option for partitioning!'
                  write (*, *) 'SwOXIC= ', ISWOX, ' 1- oxic, 0 - anoxic'
                  write (*, *) 'Group = ', IGROUP, ' 1- General, 2-Cr, 3-As/Va, 4-OMP, &
&                                5-virus'
                  call stop_with_error()
               end if
               !
               !     Addition of former process MPQUAL
               !
               QUAL = 0.0
               KDALL = 0.0
               if (.not. QUALOPT) then
                  if (DMS1 >= 1e-20) then
                     !
                     !           Compute in g/g
                     !
                     QUAL = MP * (1.0 - FDIS - FDOC) / DMS1
                     !
                     !           Overall partitioning coefficient
                     !
                     if ((CDIS + CDOC) >= 1e-20) then
                        !                        g/g     g/m3         = m3/g
                        KDALL = QUAL / (CDIS + CDOC)
                     else
                        KDALL = 0.0
                     end if
                     !
                     !           Convert to mg/kg (QUAL) or to l/kg (KDALL)
                     !
                     QUAL = QUAL * 1.e6
                     KDALL = KDALL * 1.e6

                  end if
               end if
               !
               !     Output
               !
               process_space_real(IP49) = QUAL
               process_space_real(IP50) = KDALL

            end if
         end if
         !
         IP1 = IP1 + IN1
         IP2 = IP2 + IN2
         IP3 = IP3 + IN3
         IP4 = IP4 + IN4
         IP5 = IP5 + IN5
         IP6 = IP6 + IN6
         IP7 = IP7 + IN7
         IP8 = IP8 + IN8
         IP9 = IP9 + IN9
         IP10 = IP10 + IN10
         IP11 = IP11 + IN11
         IP12 = IP12 + IN12
         IP13 = IP13 + IN13
         IP14 = IP14 + IN14
         IP15 = IP15 + IN15
         IP16 = IP16 + IN16
         IP17 = IP17 + IN17
         IP18 = IP18 + IN18
         IP19 = IP19 + IN19
         IP20 = IP20 + IN20
         IP21 = IP21 + IN21
         IP22 = IP22 + IN22
         IP23 = IP23 + IN23
         IP24 = IP24 + IN24
         IP25 = IP25 + IN25
         IP26 = IP26 + IN26
         IP27 = IP27 + IN27
         IP28 = IP28 + IN28
         IP29 = IP29 + IN29
         IP30 = IP30 + IN30
         IP31 = IP31 + IN31
         IP32 = IP32 + IN32
         IP33 = IP33 + IN33
         IP34 = IP34 + IN34
         IP35 = IP35 + IN35
         IP36 = IP36 + IN36
         IP37 = IP37 + IN37
         IP38 = IP38 + IN38
         IP39 = IP39 + IN39
         IP40 = IP40 + IN40
         IP41 = IP41 + IN41
         IP42 = IP42 + IN42
         IP43 = IP43 + IN43
         IP44 = IP44 + IN44
         IP45 = IP45 + IN45
         IP46 = IP46 + IN46
         IP47 = IP47 + IN47
         IP48 = IP48 + IN48
         IP49 = IP49 + IN49
         IP50 = IP50 + IN50
         IFLUX = IFLUX + NOFLUX
         !
         !
      end do
      !
      return
   end

end module m_partmp
