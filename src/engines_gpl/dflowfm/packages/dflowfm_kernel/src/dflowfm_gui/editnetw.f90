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

module m_editnetw
use m_kcir
use m_highlight_nodesnlinks


implicit none

contains

      subroutine EDITNETW(MODE, KEY)
         use m_dispnodevals
         use m_disnd
         use m_dcirr
         use m_choices
         use m_netw
         use unstruc_colors
         use M_MISSING
         use unstruc_api
         use dfm_error
         use unstruc_messages
         use gridoperations
         use m_mergenodes
         use unstruc_display, only: nhlNetNode
         use m_helpnow
         use m_cconstants
         use m_qnerror
         use m_ktext
         use m_putget_un
         use m_okay
         use m_botlin
         use m_tek_link
         use m_is_link
         use m_draw_nu
         use m_set_col

         integer :: MODE, KEY
         integer :: newmode
         integer :: ja
         integer :: jadd
         integer :: k
         integer :: k1, k2
         integer :: kp
         integer :: kpp
         integer :: LL
         integer :: lnu
         integer :: ncol
         integer :: nl1
         integer :: nl2
         integer :: nput
         integer :: num
         integer :: numb
         integer :: nwhat
         integer :: ierror
         double precision :: xp1
         double precision :: yp1
         double precision :: zp1
         double precision :: xp, yp, zp, ZPP
         character TEX * 26
         integer :: iresult

         TEX = ' Edit Network             '
         WRDKEY = TEX
         NLEVEL = 2
         NUM = 0
         JA = 0
         NWHAT = 0
         NPUT = 0
         NUMB = 10
         JADD = 2
         NCOL = NCOLDN
         K1 = 0
         KPP = 0

         call SAVENET()

         K = 0
         call BOTLIN(0, NUMB, KEY)

10       continue
         call DRAWNU(KEY)
         call KTEXT(TEX, 1, 2, 15)
         if (JADD == 0) then
            call KTEXT(' DELETE NODES     ', 1, 3, 15) ! D
         else if (JADD == 1) then
            call KTEXT(' ADD NODES/ELMS   ', 1, 3, 15) ! I
         else if (JADD == 2) then
            call KTEXT(' REPLACE NODES    ', 1, 3, 15) ! R
         else if (JADD == 3) then
            call KTEXT(' MERGE NODES      ', 1, 3, 15) ! M
         else if (JADD == 4) then
            call KTEXT(' MERGE LINES      ', 1, 3, 15) ! O
         else if (JADD == 5) then
            call KTEXT(' CUT LINES        ', 1, 3, 15) ! C
         else if (JADD == 6) then
            call KTEXT(' DEL ND, LINK L/R ', 1, 3, 15) ! X
         else if (JADD == 7) then
            call KTEXT(' Toggle thin dam (LINKS) ', 1, 3, 15) ! T
         else if (JADD == 8) then
            call KTEXT(' Split LINES      ', 1, 3, 15) ! S
         else if (JADD == 88) then
            call KTEXT(' Insert meshline  ', 1, 3, 15) ! SHIFT-S
         else if (JADD == 9) then
            call KTEXT(' Toggle line attribute ', 1, 3, 15) ! 1
         else if (JADD == 10) then
            call KTEXT(' FIELD MOVE       ', 1, 3, 15) ! V
         else if (JADD == 11) then
            call KTEXT(' FIELD ROTATE     ', 1, 3, 15) ! R
         else if (JADD == 12) then
            call KTEXT(' ZKNODES          ', 1, 3, 15) ! +
         else if (JADD == 13) then
            call KTEXT(' TO LAND BOUNDARY ', 1, 3, 15) ! L
         else if (JADD == 14) then
            call KTEXT(' KILL CELL        ', 1, 3, 15) ! K
         else if (JADD == 15) then
            call KTEXT(' ADD CELL LAYER   ', 1, 3, 15) ! E
         end if
         call putget_un(NUM, NWHAT, NPUT, NUMB, XP, YP, KEY)

         call SETCOL(NCOLDN)
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
!        INS KEY OF LINKERMUIS, kijken welk punt

            call ISNODE(KP, XP, YP, ZP)

            if (NPUT == 65) then ! NODE info mode
               nhlNetNode = KP
               call DISND(KP, 0)
               call highlight_nodesnlinks()
               goto 10 ! Continue with last known operation
            end if

            call SAVENET()
            if (JADD == 1) then ! insert mode
               netstat = NETSTAT_CELLS_DIRTY
               if (KP == 0) then
                  ! CALL GIVENEWNODENUM(KP)
                  call DSETNEWPOINT(XP, YP, KP)
               else
                  call DCIRR(XK(KP), YK(KP), ZK(KP), NCOLDN)
               end if

               if (K1 /= 0) then
                  call CONNECTDBN(K1, KP, LNU)
                  call TEKLINK(LNU, NCOLDN)
                  ! CALL DMOVABS(XK(K1),YK(K1),ZK(K1))
                  ! CALL  DLNABS(XK(KP),YK(KP),ZK(KP))

               end if
               K1 = KP
               NPUT = 39

            else if (JADD == 2) then !replace mode
               netstat = NETSTAT_CELLS_DIRTY
               if (KP /= 0 .and. NPUT == 0) then
                  NPUT = 1
                  KPP = KP
                  ZPP = ZP
                  call TEKNODE(KP, 0)
               else if (KPP /= 0) then
                  NPUT = 0
                  call SAVENET()
                  call SETPOINT(XP, YP, ZPP, KPP)
                  call TEKNODE(KPP, NCOLDN)
                  KPP = 0
               end if
            else if (JADD == 3) then ! MERGE NODES
               netstat = NETSTAT_CELLS_DIRTY
               if (KP /= 0) then !
                  if (K1 == 0) then
!                 punt 1
                     K1 = KP
                     KP = -KP ! FLAG TO ISNODE; DO NOT AGAIN LOOK FOR THIS POINT
                     XP1 = XP
                     YP1 = YP
                     ZP1 = ZP
                     call DCIRR(XK(K1), YK(K1), ZK(K1), NCOLDN)
                     NPUT = 39
                  else
!                 punt 2
                     K2 = KP
                     call DCIRR(XK(K1), YK(K1), ZK(K1), 0)
                     call TEKNODE(K1, 0)
                     call SAVENET()
                     call MERGENODES(K1, K2, JA)
                     call TEKNODE(K2, NCOLDN)
                     K1 = 0
                     K2 = 0
                     NPUT = 38
                  end if
               else ! NO FIND
                  call OKAY(0)
               end if
            else if (JADD == 5 .and. kp /= 0) then ! C - key now free for change ZK value
               zp1 = Zk(kP)
               call TYPEVALUE(zp1, KEY)
               call KCIR(XP, YP, zp1)
               Zk(kP) = zp1
            else if (JADD == 6) then ! DELETE NODE, CONNECT LEFT/RIGHT
               netstat = NETSTAT_CELLS_DIRTY
               if (KP /= 0) then ! CUT LINE
                  if (NMK(KP) == 2) then
!                 punt 1
                     call TEKNODE(KP, 0)

                     NL1 = NOD(KP)%LIN(1)
                     call TEKLINK(NL1, 0)
                     call OTHERNODE(KP, NL1, K1)

                     NL2 = NOD(KP)%LIN(2)
                     call TEKLINK(NL2, 0)
                     call OTHERNODE(KP, NL2, K2)

                     call CONNECTDBN(K1, K2, LNU)
                     call TEKLINK(LNU, NCOLDN)
                     call DELLINK(NL1)
                     call DELLINK(NL2)
                     NPUT = 0
                  end if
               else ! NO FIND
                  call OKAY(0)
               end if
            else if (JADD == 0) then !delete mode
               netstat = NETSTAT_CELLS_DIRTY
               if (KP /= 0) then
                  call TEKNODE(KP, 0)
                  call SAVENET()
                  call DELNODE(KP)
               else
                  call ISLINK(LL, XP, YP, ZP)
                  if (LL /= 0) then
                     call TEKLINK(LL, 0)
                     call DELLINK(LL)
                  end if
               end if
            else if (JADD == 7) then ! thin dam toggle mode
               if (KP == 0) then
                  call ISLINK(LL, XP, YP, ZP)
                  if (LL /= 0) then
                     KN(3, LL) = -KN(3, LL)
                     call TEKLINK(LL, NCOLDN)
                  end if
               end if
            else if (JADD == 8) then ! split line
               if (KP == 0) then
                  call splitlink(xp, yp, 0, 0.9d0, 1, ierror) ! use (xp,yp) and no link specified, use cos parallelogram tolerance and plot
               end if
            else if (JADD == 88) then ! insert meshline
               if (KP == 0) then
                  call insert_netline(xp, yp, 0) ! , 1)
               end if
            else if (JADD == 9) then ! line attribute TOGGLE , 1d OR 2d
               if (KP == 0) then
                  call ISLINK(LL, XP, YP, ZP)
                  if (LL /= 0) then
                     if (kn(3, LL) == 2) then
                        call TEKLINK(LL, 221)
                        kn(3, LL) = 1
                     else if (kn(3, LL) == 1) then
                        call TEKLINK(LL, 3)
                        kn(3, LL) = 2
                     end if
                  end if
               end if
            else if (JADD == 10) then ! Field move
               if (KP /= 0 .and. NPUT == 0) then
                  NPUT = 1
                  KPP = KP
                  ZPP = ZP
                  call TEKNODE(KP, 0)
               else if (KPP /= 0) then
                  call SAVENET()
                  call TEKNODE(KPP, NCOLDN)
                  call netmodfld(xp, yp, kpp)
                  NPUT = 0
                  kpp = 0
                  KEY = 3
               end if
            else if (JADD == 11) then ! Field rotate
               if (KP /= 0 .and. NPUT == 0) then
                  NPUT = 1
                  KPP = KP
                  ZPP = ZP
                  call TEKNODE(KP, 0)
               else if (KPP /= 0) then
                  call SAVENET()
                  call TEKNODE(KPP, NCOLDN)
                  call netrotfld(xp, yp, kpp)
                  NPUT = 0
                  kpp = 0
                  KEY = 3
               end if
            else if (JADD == 12) then ! Field rotate
               if (KP /= 0) then
!              punt in waarde veranderen
                  call TYPEVALUE(ZP, KEY)
                  call KCIR(XP, YP, ZP)
                  ZK(KP) = ZP
               end if
            else if (JADD == 15) then ! Add cell layer
               if (KP /= 0) then
                  call netboundtocurvi(kp)
                  KEY = 3
               end if
            end if
         else if (KEY == 22) then
!        ENTER KEY ENKEL DISPLAY
            call ISNODE(KP, XP, YP, ZP)
            if (KP /= 0) then
               call DISPNODEVALS(KP)
            else
               iresult = FLOW()
               if (iresult == DFM_SIGINT) then
                  call mess(LEVEL_ERROR, 'Final handling of SIGINT signal. Stopping program.')
                  call STOPINT()
               else if (iresult /= DFM_NOERR) then
                  call qnerror('Error occurred while running, please inspect your diagnostic output.', ' ', ' ')
               end if
            end if
         else if (KEY == 23) then
!        ESCAPE KEY
            call RESTORE()
            KEY = 3
         else if (KEY == 27) then
!        TAB
         else if (KEY == 73 .or. KEY == 73 + 32) then ! I-key
            JADD = 1
            K1 = 0
            K2 = 0
            NPUT = 38
         else if (KEY == 8) then ! Backspace KEY
!        delete entire network (within polygon if any) and stay in previous mode.
            call delnet(key, 0, 1)
            K1 = 0
            K2 = 0
            key = 3
         else if (KEY == 68 .or. KEY == 68 + 32) then ! D-key
!        delete mode
            JADD = 0
            K1 = 0
            K2 = 0
            NPUT = -2
         else if (KEY == 82 .or. KEY == 82 + 32) then ! R-key
!        replace mode, maar niet bij zetten
            JADD = 2
            K1 = 0
            K2 = 0
            NPUT = 0
         else if (KEY == 88 .or. KEY == 88 + 32) then ! X-key
!        DELNODE, CONNECT LEFT/RIGHT
            JADD = 6
            K1 = 0
            K2 = 0
            NPUT = -2
         else if (KEY == 77 .or. KEY == 77 + 32) then ! M-key  MERGE
            JADD = 3
            K1 = 0
            K2 = 0
            NPUT = 38
         else if (KEY == 99 .or. KEY == 99 + 32) then ! C-key  Change ZK value
            JADD = 5
            K1 = 0
            NPUT = 60
         else if (KEY == 33 .or. KEY == 49) then ! 1, 1D link
            call ISLINK(LL, XP, YP, ZP)
            if (LL /= 0) then
               kn(3, LL) = 1
               call TEKLINK(LL, 1)
            end if !123
         else if (KEY == 34 .or. KEY == 50) then ! 2, 2D link
            call ISLINK(LL, XP, YP, ZP)
            if (LL /= 0) then
               kn(3, LL) = 2
               call TEKLINK(LL, 1)
            end if
         else if (KEY == 35 .or. KEY == 51) then ! 3, 1d2d internal
            call ISLINK(LL, XP, YP, ZP)
            if (LL /= 0) then
               kn(3, LL) = 3
               call TEKLINK(LL, 1)
            end if
         else if (KEY == 36 .or. KEY == 52) then ! 4, 1d2d lateral
            call ISLINK(LL, XP, YP, ZP)
            if (LL /= 0) then
               kn(3, LL) = 4
               call TEKLINK(LL, 1)
            end if
         else if (KEY == 37 .or. KEY == 53) then ! 5, 1d2d pipe
            call ISLINK(LL, XP, YP, ZP)
            if (LL /= 0) then
               kn(3, LL) = 5
               call TEKLINK(LL, 1)
            end if
         else if (KEY == 38 .or. KEY == 54) then ! 6, 1d branch
            call ISLINK(LL, XP, YP, ZP)
            if (LL /= 0) then
               kn(3, LL) = 6
               call TEKLINK(LL, 1)
            end if
         else if (KEY == 71 .or. KEY == 71 + 32) then ! G-key  netw2curv
            call NETW2CURV(XP, YP)
            KEY = 3
         else if (KEY == 86 .or. KEY == 86 + 32) then ! V-key  fieldmove
            JADD = 10
            K1 = 0
            K2 = 0
            NPUT = 0
         else if (KEY == 66 .or. KEY == 66 + 32) then ! B-key  fieldrotate
            JADD = 11
            K1 = 0
            K2 = 0
            NPUT = 0
         else if (KEY == 76 .or. KEY == 76 + 32) then ! L-key  nettoland
            JADD = 13
            call SAVENET()
            call nettoland()
            KEY = 3
            NPUT = 38
         else if (KEY == 75) then ! K-key  derefine_mesh
            call SAVENET()
            call derefine_mesh(xp, yp, .true.)
         else if (KEY == 75 + 32) then ! k-key  killcell
            call SAVENET()
            call killcell(xp, yp)
         else if (KEY == 70 .or. KEY == 70 + 32) then ! F-key  FIXED POINT
            call ISNODE(KP, XP, YP, ZP)
            if (KP /= 0) then
               call SAVENET()
               if (KC(KP) == -1) then
                  KC(KP) = 1
                  NCOL = 0
               else
                  KC(KP) = -1
                  NCOL = NCOLDN
               end if
               call DCIRR(XK(KP), YK(KP), ZK(KP), NCOL)
               call TEKNODE(KP, NCOLDN)
            end if
         else if (KEY == 76 .or. KEY == 76 + 32) then ! L-key AANRIJGPUNT
            call ISNODE(KP, XP, YP, ZP) ! LINE
            if (KP /= 0) then
               call SAVENET()
               if (KC(KP) == 2) then
                  KC(KP) = 4
                  NCOL = 0
               else
                  KC(KP) = 2
                  NCOL = NCOLRN
               end if
               call DCIRR(XK(KP), YK(KP), ZK(KP), NCOL)
               call TEKNODE(KP, NCOLRN)
            end if
         else if (KEY == 79 .or. KEY == 79 + 32) then ! O-key ONELINE
            call ISNODE(KP, XP, YP, ZP)
            JADD = 4
            if (KP /= 0) then
               call TEKNODE(KP, 0)
               call ONELINE(KP)
            end if
         else if (KEY == 84 .or. KEY == 84 + 32) then ! T-key
!        thin dam mode
            JADD = 7
            K1 = 0
            K2 = 0
            NPUT = 55
         else if (KEY == 83 + 32) then ! S-key
!        split link
            JADD = 8
            K1 = 0
            K2 = 0
            NPUT = 55
         else if (KEY == 83) then ! SHIFT-S-key
!        insert meshline
            JADD = 88
            K1 = 0
            K2 = 0
            NPUT = 55
         else if (KEY == 69 .or. KEY == 69 + 32) then ! E-key
!        add layer of cells
            JADD = 15
            K1 = 0
            K2 = 0
            NPUT = 59
         else if (KEY == 43 .or. KEY == 43 + 32) then ! +-key
!        CHANGE ZK VALUE mode
            JADD = 12
         else if (KEY == 44) then ! ,-key
!        INVERT JINS
            JINS = (1 - JINS)
         else if (KEY == 86 .or. KEY == 86 + 32) then ! V-key
            call VIEWCYCLE(KEY)
         else if (KEY == 32) then
            call flow_spatietimestep()
            key = 3
!      ELSE IF (KEY .EQ. 75 .or. KEY .eq. 75+32) THEN  ! K-KEY
         else if (KEY == 96) then ! `-KEY
            call checknetwork()
            !key = 3
         else if (KEY == 78 .or. KEY == 78 + 32) then ! N-key voor node mode
            ! Display node info
            NPUT = 65
         else if (KEY == 81 .or. KEY == 81 + 32) then ! Q-key
!         call bilin_interp(numk, xk, yk, zk)          ! testing subroutine
!         call net_delete_DMISS()
!         call sam2net_curvi()
            key = 3 ! redraw

            !     call removecell(xp,yp)
            call create_samples_in_triangle()
            !     call fix_global_polygons(1,0)
         end if
!
         goto 10
!
      end subroutine EDITNETW

end module m_editnetw
