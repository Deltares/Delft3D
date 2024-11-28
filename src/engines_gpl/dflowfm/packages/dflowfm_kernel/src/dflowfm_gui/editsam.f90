!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
!

module m_editsam
use m_typevalue
use m_selecteditmode
use m_minmxsam
use m_kcir


implicit none

contains

      subroutine EDITSAM(MODE, KEY)
         use m_choices
         use m_chadep
         use m_samples
         use m_helpnow
         use m_drawthis
         use m_depmax2
         use m_ktext
         use m_putget_un
         use m_okay
         use m_delsam
         use m_draw_nu
         use m_cirr

         integer :: MODE, KEY
         integer :: jonce
         integer :: k, L1, L2
         integer :: newmode
         integer :: nput
         integer :: num
         integer :: numb
         integer :: nwhat
         double precision :: ziso
         double precision :: xp, yp, rd
         integer :: mp, mps
         character TEX * 26

         TEX = ' Edit Samples             '
         WRDKEY = TEX
         NLEVEL = 2
         NUM = 0
         NWHAT = 0
         NPUT = 25
         NUMB = 12
         MP = 0
         MPS = MP
         L1 = 0
         call SAVESAM()

!     user is editing samples: mark samples as unstructured
!      MXSAM = 0
!      MYSAM = 0
!      IPSTAT = IPSTAT_NOTOK

10       continue
         call DRAWNU(KEY)
         call KTEXT(TEX, 1, 2, 15)
         call putget_un(NUM, NWHAT, NPUT, NUMB, XP, YP, KEY)
         if (KEY /= 23) JONCE = 0

         if (NUM /= 0) then
!        ER IS EEN KEUZE
            if (NUM == 4) then
               MODE = NWHAT
               return
            else
               call CHOICES(NUM, NWHAT, KEY)
            end if
         else if (KEY >= 577) then ! Alt+letter switches edit mode.
            call selecteditmode(newmode, key)
            if (newmode > 0 .and. newmode /= mode) then
               mode = newmode
               return
            end if
         else if (KEY == 21) then
!        INS KEY
            MPS = MP
            call SAVESAM()
            if (NPUT == 23 .or. NPUT == 26 .or. NPUT == 27 .or. &
                NPUT == 40 .or. NPUT == 41 .or. NPUT == 49 .or. &
                NPUT == 50 .or. NPUT == 51) then
!           kijken welk punt bij deleten en bij oppakken, changen
               call ISPOI1(XS, YS, NS, XP, YP, MP)
            end if
            if (NPUT == 23 .and. MP /= 0) then
!           punt oppakken
               call CIRR(XP, YP, 0)
               NPUT = 24
            else if (NPUT == 24 .and. MP /= 0) then
!           punt neerzetten
               XS(MP) = XP
               YS(MP) = YP
               call KCIR(XP, YP, ZS(MP))
               NPUT = 23
            else if (NPUT == 25) then
!           punt toevoegen
               call INCREASESAM(NS)
               if (NS >= 1) then
                  RD = ZS(NS)
               else
                  RD = ZISO
               end if
               ! CALL TYPEVALUE(RD,KEY)
               NS = NS + 1
               XS(NS) = XP
               YS(NS) = YP
               ZS(NS) = RD
               call KCIR(XP, YP, ZS(NS))
!           user is editing samples: mark samples as unstructured
               MXSAM = 0
               MYSAM = 0
               IPSTAT = IPSTAT_NOTOK
            else if (NPUT == 26 .and. MP /= 0) then
!           punt deleten
               call CIRR(XP, YP, 0)
               do K = MP, NS
                  XS(K) = XS(K + 1)
                  YS(K) = YS(K + 1)
                  ZS(K) = ZS(K + 1)
               end do
               NS = NS - 1
!           user is editing samples: mark samples as unstructured
               MXSAM = 0
               MYSAM = 0
               IPSTAT = IPSTAT_NOTOK
            else if (NPUT == 27 .and. MP /= 0) then
!           punt in waarde veranderen
               RD = ZS(MP)
               call TYPEVALUE(RD, KEY)
               call KCIR(XP, YP, RD)
               ZS(MP) = RD
            else if (NPUT == 40 .or. NPUT == 41) then
               if (MP /= 0) then
                  if (L1 == 0) then
                     L1 = MP
                     NPUT = 41
                  else
                     L2 = MP
                     NPUT = 40
                     call insertsamples(L1, L2)
                     L1 = 0
                     L2 = 0
                     KEY = 3
                  end if
               end if
            else if (NPUT == 49) then ! Click sample point to set min value for isocol2
               KEY = 3
               if (MP == 0) then ! Miss click: reset iscol2 scaling to auto.
                  jaauto2 = 1
               else
                  vmin2 = ZS(MP)
                  jaauto2 = 0
                  if (vmin2 > vmax2) then
                     key = 0
                  end if
               end if
               call minmxsam()
            else if (NPUT == 50) then ! Click sample point to set max value for isocol2
               KEY = 3
               if (MP == 0) then ! Miss click: reset iscol2 scaling to auto.
                  jaauto2 = 1
               else
                  vmax2 = ZS(MP)
                  jaauto2 = 0
                  if (vmin2 > vmax2) then
                     key = 0
                  end if
               end if
               call minmxsam()
            else if (NPUT == 58) then ! Click start point of sample-based polygon
               call make_samplepath(xp, yp)
            end if
         else if (KEY == 22) then
!        ENTER KEY
         else if (KEY == 23) then
!        ESC
            MP = MPS
            call RESTORESAM()
            KEY = 3
         else if (KEY == 27) then
!        TAB
            call ISPOI1(XS, YS, NS, XP, YP, MP)
            if (MP /= 0) then
               RD = ZS(MP)
               MPS = MP
               call SAVESAM()
               call CHADEP(XP, YP, RD, KEY)
               ZS(MP) = RD
            end if
         else if (KEY == 73 .or. KEY == 73 + 32) then ! I
!        insert mode
            NPUT = 25
         else if (KEY == 8) then ! Backspace KEY
!        delete all samples (within polygon if any) and stay in previous mode.
            call savesam()
            call delsam(1)
            key = 3
!        user is editing samples: mark samples as unstructured
            MXSAM = 0
            MYSAM = 0
            IPSTAT = IPSTAT_NOTOK
         else if (KEY == 68 .or. KEY == 68 + 32) then ! D
!       delete mode
            NPUT = 26
         else if (KEY == 70 .or. KEY == 70 + 32) then ! f
            call SAVESAM()
            ns = 10
            call increasesam(ns)
            xs(1) = xp
            ys(1) = yp
            call TYPEVALUE(RD, KEY)
            zs(1) = rd
            NS = 1
            call flow_initfloodfill()
            call restoresam()
            key = 3
         else if (KEY == 82 .or. KEY == 82 + 32 .and. NPUT /= 24) then ! R
!       replace mode, maar niet bij zetten
            NPUT = 23
         else if (KEY == 76 .or. KEY == 76 + 32) then ! L
!        line mode
            NPUT = 40
         else if (KEY == 67 .or. KEY == 67 + 32) then ! C
!        change mode
            NPUT = 27
         else if (KEY == 77 + 32) then ! m (case sensitive!)
!        click sample to set minimum for isocol2
            NPUT = 49
         else if (KEY == 77) then ! M (case sensitive!)
!        click sample to set maximum for isocol2
            NPUT = 50
         else if (KEY == 72 .or. KEY == 72 + 32) then ! H: hide/show samples
!        click sample to set maximum for isocol2
            ndraw(32) = -ndraw(32)
            key = 3
         else if (KEY == 98) then
!        b RINGS BELL
            call KTEXT('B Rings Bell', 2, 6, 11)
            call OKAY(0)
         else if (KEY == 81 .or. KEY == 81 + 32) then ! Q (for testing only)
            call make_orthocenters(0.5d-2, 1000)
!         call copy_sendlist_to_sam()
            NPUT = 58
         end if

         goto 10

      end subroutine EDITSAM

end module m_editsam
