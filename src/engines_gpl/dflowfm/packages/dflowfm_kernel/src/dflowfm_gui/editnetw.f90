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

      SUBROUTINE EDITNETW(MODE,KEY)
      use m_netw
      use unstruc_colors
      USE M_MISSING
      use unstruc_api
      use dfm_error
      use unstruc_messages
      use gridoperations
      use m_mergenodes
      use m_save_ugrid_state, only: nodeids
      use unstruc_display, only: nhlNetNode
      implicit none
      integer :: MODE, KEY

      double precision :: ag
      double precision :: cfl
      double precision :: e0
      double precision :: eps
      integer :: newmode
      integer :: ja
      integer :: jadd
      integer :: k
      integer :: k1, k2, k3
      integer :: kp
      integer :: kpp
      integer :: LL
      integer :: lnu
      integer :: ncol
      integer :: nl1
      integer :: nl2
      integer :: nlevel
      integer :: nput
      integer :: num
      integer :: numb
      integer :: nwhat
      integer :: ierror
      double precision :: pi
      double precision :: rho
      double precision :: rhow
      double precision :: xp1
      double precision :: yp1
      double precision :: zp1

      double precision :: xp, yp, zp, ZPP

      COMMON /HELPNOW/ WRDKEY,NLEVEL

      COMMON /CONSTANTS/ E0, RHO, RHOW, CFL, EPS, AG, PI

      CHARACTER TEX*26, WRDKEY*40
      integer :: iresult

      TEX    = ' Edit Network             '
      WRDKEY = TEX
      NLEVEL =  2
      NUM    =  0
      JA     =  0
      NWHAT  =  0
      NPUT   =  0
      NUMB   =  10
      JADD   =  2
      NCOL   =  NCOLDN
      K1     =  0
      KPP    =  0

      CALL SAVENET()

      K      = 0
      CALL BOTLIN(0,NUMB,KEY)

   10 CONTINUE
      CALL DRAWNU(KEY)
      CALL KTEXT(TEX,1,2,15)
      IF (JADD .EQ. 0) THEN
         CALL KTEXT(' DELETE NODES     ',1,3,15)          ! D
      ELSE IF (JADD .EQ. 1) THEN
         CALL KTEXT(' ADD NODES/ELMS   ',1,3,15)          ! I
      ELSE IF (JADD .EQ. 2) THEN
         CALL KTEXT(' REPLACE NODES    ',1,3,15)          ! R
      ELSE IF (JADD .EQ. 3) THEN
         CALL KTEXT(' MERGE NODES      ',1,3,15)          ! M
      ELSE IF (JADD .EQ. 4) THEN
         CALL KTEXT(' MERGE LINES      ',1,3,15)          ! O
      ELSE IF (JADD .EQ. 5) THEN
         CALL KTEXT(' CUT LINES        ',1,3,15)          ! C
      ELSE IF (JADD .EQ. 6) THEN
         CALL KTEXT(' DEL ND, LINK L/R ',1,3,15)          ! X
      ELSE IF (JADD .EQ. 7) THEN
         CALL KTEXT(' Toggle thin dam (LINKS) ',1,3,15)   ! T
      ELSE IF (JADD .EQ. 8) THEN
         CALL KTEXT(' Split LINES      ',1,3,15)          ! S
      ELSE IF (JADD .EQ. 88) THEN
         CALL KTEXT(' Insert meshline  ',1,3,15)          ! SHIFT-S
      ELSE IF (JADD .EQ. 9) THEN
         CALL KTEXT(' Toggle line attribute ',1,3,15)     ! 1
      ELSE IF (JADD .EQ. 10) THEN
         CALL KTEXT(' FIELD MOVE       ',1,3,15)          ! V
      ELSE IF (JADD .EQ. 11) THEN
         CALL KTEXT(' FIELD ROTATE     ',1,3,15)          ! R
      ELSE IF (JADD .EQ. 12) THEN
         CALL KTEXT(' ZKNODES          ',1,3,15)          ! +
      ELSE IF (JADD .EQ. 13) THEN
         CALL KTEXT(' TO LAND BOUNDARY ',1,3,15)          ! L
      ELSE IF (JADD .EQ. 14) THEN
         CALL KTEXT(' KILL CELL        ',1,3,15)          ! K
      ELSE IF (JADD .eq. 15) THEN
         CALL KTEXT(' ADD CELL LAYER   ',1,3,15)          ! E
      ENDIF
      CALL putget_un(NUM,NWHAT,NPUT,NUMB,XP,YP,KEY)

      CALL SETCOL(NCOLDN)
      IF (NUM .NE. 0) THEN
!        ER IS EEN KEUZE
         IF (NUM .EQ. 4) THEN
            MODE = NWHAT
            RETURN
         ELSE
            CALL CHOICES(MODE,NUM,NWHAT,KEY)
         ENDIF
      ELSE IF (KEY >= 577) THEN ! Alt+letter switches edit mode.
        call selecteditmode(newmode, key)
        if (newmode > 0 .and. newmode /= mode) then
            mode = newmode
            return
        end if
      ELSE IF (KEY .EQ. 21) THEN
!        INS KEY OF LINKERMUIS, kijken welk punt

         CALL ISNODE(KP, XP, YP, ZP)

         IF (NPUT .EQ. 65) THEN   ! NODE info mode
            nhlNetNode = KP
            CALL DISND(KP, 0)
            call highlight_nodesnlinks()
            goto 10 ! Continue with last known operation
         end if

         CALL SAVENET()
         IF (JADD .EQ. 1) THEN   ! insert mode
            netstat = NETSTAT_CELLS_DIRTY
            IF (KP .EQ. 0) THEN
               ! CALL GIVENEWNODENUM(KP)
               CALL DSETNEWPOINT(XP,YP,KP)
            ELSE
               CALL DCIRR (XK(KP),YK(KP),ZK(KP),NCOLDN)
            ENDIF



            IF (K1 .NE. 0) THEN
               CALL CONNECTDBN(K1,KP,LNU)
                CALL TEKLINK(LNU,NCOLDN)
              ! CALL DMOVABS(XK(K1),YK(K1),ZK(K1))
              ! CALL  DLNABS(XK(KP),YK(KP),ZK(KP))

            ENDIF
            K1   = KP
            NPUT = 39

         ELSE IF (JADD .EQ. 2) THEN  !replace mode
            netstat = NETSTAT_CELLS_DIRTY
            IF (KP .NE. 0 .AND. NPUT .EQ.0 ) THEN
               NPUT = 1
               KPP  = KP
               ZPP = ZP
               CALL TEKNODE(KP,0)
            ELSE IF (KPP .NE. 0) THEN
               NPUT = 0
               CALL SAVENET()
               CALL SETPOINT(XP,YP,ZPP,KPP)
               CALL TEKNODE(KPP,NCOLDN)
               KPP = 0
            ENDIF
         ELSE IF (JADD .EQ. 3) THEN   ! MERGE NODES
            netstat = NETSTAT_CELLS_DIRTY
            IF (KP .NE. 0) THEN       !
               IF ( K1 .EQ. 0 ) THEN
!                 punt 1
                  K1  = KP
                  KP = -KP ! FLAG TO ISNODE; DO NOT AGAIN LOOK FOR THIS POINT
                  XP1 = XP
                  YP1 = YP
                  ZP1 = ZP
                  CALL DCIRR (XK(K1),YK(K1),ZK(K1),NCOLDN)
                  NPUT = 39
               ELSE
!                 punt 2
                  K2  = KP
                  CALL DCIRR (XK(K1),YK(K1),ZK(K1),0     )
                  CALL TEKNODE(K1,0)
                  CALL SAVENET()
                  CALL MERGENODES(K1,K2,JA)
                  CALL TEKNODE(K2,NCOLDN)
                  K1   = 0
                  K2   = 0
                  NPUT = 38
               ENDIF
            ELSE                    ! NO FIND
               CALL OKAY(0)
            ENDIF
         ELSE IF (JADD .EQ. 5 .and. kp .ne. 0) THEN   ! C - key now free for change ZK value
            zp1 = Zk(kP)
            CALL TYPEVALUE(zp1,KEY)
            CALL KCIR(XP,YP,zp1)
            Zk(kP) = zp1
         ELSE IF (JADD .EQ. 6) THEN   ! DELETE NODE, CONNECT LEFT/RIGHT
            netstat = NETSTAT_CELLS_DIRTY
            IF (KP .NE. 0) THEN       ! CUT LINE
               IF (NMK(KP) .EQ. 2) THEN
!                 punt 1
                  CALL TEKNODE(KP,0)

                  NL1  = NOD(KP)%LIN(1)
                  CALL TEKLINK(NL1,0)
                  CALL OTHERNODE(KP,NL1,K1)

                  NL2  = NOD(KP)%LIN(2)
                  CALL TEKLINK(NL2,0)
                  CALL OTHERNODE(KP,NL2,K2)

                  CALL CONNECTDBN(K1,K2,LNU)
                  CALL TEKLINK (LNU,NCOLDN)
                  CALL DELLINK(NL1)
                  CALL DELLINK(NL2)
                  NPUT = 0
               ENDIF
            ELSE                    ! NO FIND
               CALL OKAY(0)
            ENDIF
         ELSE IF (JADD .EQ. 0) THEN !delete mode
            netstat = NETSTAT_CELLS_DIRTY
            IF (KP .NE. 0) THEN
               CALL TEKNODE(KP,0)
               CALL SAVENET()
               CALL DELNODE(KP)
            ELSE
               CALL ISLINK(LL, XP, YP, ZP)
               IF (LL .NE. 0) THEN
                  CALL TEKLINK(LL,0)
                  CALL DELLINK(LL)
               ENDIF
            ENDIF
         ELSE IF (JADD .EQ. 7) THEN ! thin dam toggle mode
            IF (KP == 0) THEN
               CALL ISLINK(LL, XP, YP, ZP)
               IF (LL /= 0) THEN
                  KN(3,LL) = -KN(3,LL)
                  CALL TEKLINK(LL,NCOLDN)
               ENDIF
            ENDIF
         ELSE IF (JADD .EQ. 8) THEN ! split line
            IF (KP == 0) THEN
               call splitlink(xp, yp, 0, 0.9d0, 1, ierror)   ! use (xp,yp) and no link specified, use cos parallelogram tolerance and plot
            ENDIF
         ELSE IF (JADD .EQ. 88) THEN ! insert meshline
            IF (KP == 0) THEN
               call insert_netline(xp, yp, 0) ! , 1)
            ENDIF
         ELSE IF (JADD .EQ. 9) THEN ! line attribute TOGGLE , 1d OR 2d
            IF (KP == 0) THEN
               CALL ISLINK(LL, XP, YP, ZP)
               IF (LL /= 0) THEN
                  IF ( kn(3,LL) == 2) THEN
                     CALL TEKLINK(LL,221)
                     kn(3,LL) = 1
                  ELSE IF ( kn(3,LL) == 1) THEN
                     CALL TEKLINK(LL,3)
                     kn(3,LL) =  2
                  ENDIF
               ENDIF
            ENDIF
         ELSE IF (JADD .EQ. 10) THEN ! Field move
            IF (KP .NE. 0 .AND. NPUT .EQ.0 ) THEN
               NPUT = 1
               KPP  = KP
               ZPP = ZP
               CALL TEKNODE(KP,0)
            ELSE IF (KPP .NE. 0) THEN
               CALL SAVENET()
               CALL TEKNODE(KPP,NCOLDN)
               call netmodfld(xp,yp,zpp,kpp)
               NPUT  = 0
               kpp   = 0
               KEY   = 3
            ENDIF
         ELSE IF (JADD .EQ. 11) THEN ! Field rotate
            IF (KP .NE. 0 .AND. NPUT .EQ.0 ) THEN
               NPUT = 1
               KPP  = KP
               ZPP = ZP
               CALL TEKNODE(KP,0)
            ELSE IF (KPP .NE. 0) THEN
               CALL SAVENET()
               CALL TEKNODE(KPP,NCOLDN)
               call netrotfld(xp,yp,zpp,kpp)
               NPUT  = 0
               kpp   = 0
               KEY   = 3
            ENDIF
         ELSE IF (JADD .EQ. 12) THEN ! Field rotate
            IF (KP .NE. 0) THEN
!              punt in waarde veranderen
               CALL TYPEVALUE(ZP,KEY)
               CALL KCIR(XP,YP,ZP)
               ZK(KP) = ZP
            ENDIF
         ELSE IF (JADD .EQ. 15 ) THEN  ! Add cell layer
            IF (KP .NE. 0) THEN
               call netboundtocurvi(kp)
               KEY = 3
            ENDIF
         ENDIF
      ELSE IF (KEY .EQ. 22) THEN
!        ENTER KEY ENKEL DISPLAY
         CALL ISNODE(KP, XP, YP, ZP)
         IF (KP .NE. 0) THEN
            CALL DISPNODEVALS(KP)
         ELSE
            iresult = FLOW()
            if (iresult == DFM_SIGINT) then
               call mess(LEVEL_ERROR, 'Final handling of SIGINT signal. Stopping program.')
               call STOPINT()
            else if (iresult /= DFM_NOERR) then
               call qnerror('Error occurred while running, please inspect your diagnostic output.',' ', ' ')
            end if
         ENDIF
      ELSE IF (KEY .EQ. 23) THEN
!        ESCAPE KEY
         CALL RESTORE()
         KEY   = 3
      ELSE IF (KEY .EQ. 27) THEN
!        TAB
      ELSE IF (KEY .EQ. 73 .OR. KEY .EQ. 73+32) THEN    ! I-key
         JADD = 1
         K1   = 0
         K2   = 0
         NPUT = 38
      ELSE IF (KEY .EQ. 8) THEN    ! Backspace KEY
!        delete entire network (within polygon if any) and stay in previous mode.
         call delnet(key,0, 1)
         K1 = 0
         K2 = 0
         key = 3
      ELSE IF (KEY .EQ. 68 .OR. KEY .EQ. 68+32) THEN    ! D-key
!        delete mode
         JADD = 0
         K1   = 0
         K2   = 0
         NPUT = -2
      ELSE IF (KEY .EQ. 82 .OR. KEY .EQ. 82+32) THEN    ! R-key
!        replace mode, maar niet bij zetten
         JADD = 2
         K1   = 0
         K2   = 0
         NPUT = 0
      ELSE IF (KEY .EQ. 88 .OR. KEY .EQ. 88+32) THEN    ! X-key
!        DELNODE, CONNECT LEFT/RIGHT
         JADD = 6
         K1   = 0
         K2   = 0
         NPUT = -2
      ELSE IF (KEY .EQ. 77 .OR. KEY .EQ. 77+32) THEN    ! M-key  MERGE
         JADD =  3
         K1   =  0
         K2   =  0
         NPUT = 38
      ELSE IF (KEY .EQ. 99 .OR. KEY .EQ. 99+32) THEN    ! C-key  Change ZK value
         JADD =  5
         K1   =  0
         NPUT =  60
      ELSE IF (KEY .EQ. 33 .or. KEY .EQ. 49) THEN       ! 1, 1D link
         CALL ISLINK(LL, XP, YP, ZP)
         IF (LL /= 0) THEN
             kn(3,LL) = 1
             CALL TEKLINK(LL,1)
         ENDIF!123
      ELSE IF (KEY .EQ. 34 .or. KEY .EQ. 50) THEN       ! 2, 2D link
         CALL ISLINK(LL, XP, YP, ZP)
         IF (LL /= 0) THEN
             kn(3,LL) = 2
             CALL TEKLINK(LL,1)
         ENDIF
      ELSE IF (KEY .EQ. 35 .or. KEY .EQ. 51) THEN       ! 3, 1d2d internal
         CALL ISLINK(LL, XP, YP, ZP)
         IF (LL /= 0) THEN
             kn(3,LL) = 3
             CALL TEKLINK(LL,1)
         ENDIF
      ELSE IF (KEY .EQ. 36 .or. KEY .EQ. 52) THEN       ! 4, 1d2d lateral
         CALL ISLINK(LL, XP, YP, ZP)
         IF (LL /= 0) THEN
             kn(3,LL) = 4
             CALL TEKLINK(LL,1)
         ENDIF
      ELSE IF (KEY .EQ. 37 .or. KEY .EQ. 53) THEN       ! 5, 1d2d pipe
         CALL ISLINK(LL, XP, YP, ZP)
         IF (LL /= 0) THEN
             kn(3,LL) = 5
             CALL TEKLINK(LL,1)
         ENDIF
      ELSE IF (KEY .EQ. 38 .or. KEY .EQ. 54) THEN       ! 6, 1d branch
         CALL ISLINK(LL, XP, YP, ZP)
         IF (LL /= 0) THEN
             kn(3,LL) = 6
             CALL TEKLINK(LL,1)
         ENDIF
      ELSE IF (KEY .EQ. 71 .OR. KEY .EQ. 71+32) THEN    ! G-key  netw2curv
         CALL NETW2CURV(XP,YP)
         KEY = 3
      ELSE IF (KEY .EQ. 86 .OR. KEY .EQ. 86+32) THEN    ! V-key  fieldmove
         JADD = 10
         K1   = 0
         K2   = 0
         NPUT = 0
      ELSE IF (KEY .EQ. 66 .OR. KEY .EQ. 66+32) THEN    ! B-key  fieldrotate
         JADD = 11
         K1   = 0
         K2   = 0
         NPUT = 0
      ELSE IF (KEY .EQ. 76 .OR. KEY .EQ. 76+32) THEN    ! L-key  nettoland
         JADD = 13
         CALL SAVENET()
         call nettoland()
         KEY  = 3
         NPUT = 38
      ELSE IF (KEY .EQ. 75 ) THEN                       ! K-key  derefine_mesh
         CALL SAVENET()
         call derefine_mesh(xp,yp,.true.)
      ELSE IF ( KEY .EQ. 75+32) THEN                    ! k-key  killcell
         CALL SAVENET()
         call killcell(xp,yp)
      ELSE IF (KEY .EQ. 70 .OR. KEY .EQ. 70+32) THEN    ! F-key  FIXED POINT
         CALL ISNODE(KP, XP, YP, ZP)
         IF (KP .NE. 0) THEN
            CALL SAVENET()
            IF (KC(KP) .EQ. -1) THEN
                KC(KP) = 1
                NCOL   = 0
            ELSE
                KC(KP) = -1
                NCOL   = NCOLDN
            ENDIF
            CALL DCIRR(XK(KP),YK(KP),ZK(KP),NCOL)
            CALL TEKNODE(KP,NCOLDN)
         ENDIF
      ELSE IF (KEY .EQ. 76 .OR. KEY .EQ. 76+32) THEN    ! L-key AANRIJGPUNT
         CALL ISNODE(KP, XP, YP, ZP)                    ! LINE
         IF (KP .NE. 0) THEN
            CALL SAVENET()
            IF (KC(KP) .EQ.  2) THEN
                KC(KP) = 4
                NCOL   = 0
            ELSE
                KC(KP) =  2
                NCOL   = NCOLRN
            ENDIF
            CALL DCIRR(XK(KP),YK(KP),ZK(KP),NCOL)
            CALL TEKNODE(KP,NCOLRN)
         ENDIF
      ELSE IF (KEY .EQ. 79 .OR. KEY .EQ. 79+32) THEN    ! O-key ONELINE
         CALL ISNODE(KP, XP, YP, ZP)
         JADD = 4
         IF (KP .NE. 0) THEN
            CALL TEKNODE(KP,0)
            CALL ONELINE(KP,99999d0)
         ENDIF
      ELSE IF (KEY .EQ. 84 .OR. KEY .EQ. 84+32) THEN    ! T-key
!        thin dam mode
         JADD = 7
         K1   = 0
         K2   = 0
         NPUT = 55
      ELSE IF (KEY .EQ. 83+32) THEN                     ! S-key
!        split link
         JADD = 8
         K1   = 0
         K2   = 0
         NPUT = 55
      ELSE IF (KEY .EQ. 83) THEN                        ! SHIFT-S-key
!        insert meshline
         JADD = 88
         K1   = 0
         K2   = 0
         NPUT = 55
      ELSE IF (KEY .EQ. 69 .OR. KEY .EQ. 69+32) THEN    ! E-key
!        add layer of cells
         JADD = 15
         K1   = 0
         K2   = 0
         NPUT = 59
      ELSE IF (KEY .EQ. 43 .OR. KEY .EQ. 43+32) THEN    ! +-key
!        CHANGE ZK VALUE mode
         JADD = 12
      ELSE IF (KEY .EQ. 44) THEN                        ! ,-key
!        INVERT JINS
         JINS = (1-JINS)
      ELSE IF (KEY .EQ. 86 .OR. KEY .EQ. 86+32) THEN    ! V-key
         CALL VIEWCYCLE(KEY)
      ELSE IF (KEY .EQ. 32) THEN
         call flow_spatietimestep()
         key = 3
!      ELSE IF (KEY .EQ. 75 .or. KEY .eq. 75+32) THEN  ! K-KEY
      ELSE IF (KEY .EQ. 96 ) THEN  ! `-KEY
         call checknetwork()
         !key = 3
      ELSE IF (KEY .EQ. 78 .OR. KEY .EQ. 78+32) THEN    ! N-key voor node mode
         ! Display node info
         NPUT = 65
      ELSE IF (KEY .EQ. 81 .OR. KEY .EQ. 81+32) THEN   ! Q-key
!         call bilin_interp(numk, xk, yk, zk)          ! testing subroutine
!         call net_delete_DMISS()
!         call sam2net_curvi()
         key = 3  ! redraw

    !     call removecell(xp,yp)
         call create_samples_in_triangle()
    !     call fix_global_polygons(1,0)
      ENDIF
!
      GOTO 10
!
      END SUBROUTINE EDITNETW
