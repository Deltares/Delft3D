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

      SUBROUTINE EDITSAM(MODE,KEY)
      use m_samples
      USE M_MISSING
      use unstruc_colors
      use m_partitioninfo
      implicit none
      integer :: MODE, KEY
      double precision :: ddx
      integer :: jalinear
      integer :: jaspline
      integer :: jonce
      integer :: k, L1, L2
      integer :: newmode
      integer :: nlevel
      integer :: nput
      integer :: num
      integer :: numb
      integer :: numinp
      integer :: nwhat
      double precision :: ziso
      double precision :: ziso2
      double precision :: vmax2, vmin2, dv2, val2, ave
      integer :: ncols2, nv2, nis2, nie2, jaauto2, ierror
      COMMON /DEPMAX2/ VMAX2,VMIN2,DV2,VAL2(256),NCOLS2(256),NV2,NIS2,NIE2,JAAUTO2
      integer :: ndraw
      COMMON /DRAWTHIS/ ndraw(50)

      double precision :: xp, yp, rd
      integer :: mp, mps

      COMMON /HELPNOW/ WRDKEY,NLEVEL

      COMMON /ISOPOL/    ZISO,ZISO2,DDX,NUMINP,JASPLINE,JALINEAR

      CHARACTER TEX*26, WRDKEY*40

      TEX    = ' Edit Samples             '
      WRDKEY = TEX
      NLEVEL =  2
      NUM    =  0
      NWHAT  =  0
      NPUT   =  25
      NUMB   =  12
      MP     =  0
      MPS    = MP
      L1     = 0
      CALL SAVESAM()

!     user is editing samples: mark samples as unstructured
!      MXSAM = 0
!      MYSAM = 0
!      IPSTAT = IPSTAT_NOTOK

   10 CONTINUE
      CALL DRAWNU(KEY)
      CALL KTEXT(TEX,1,2,15)
      CALL putget_un(NUM,NWHAT,NPUT,NUMB,XP,YP,KEY)
      IF (KEY .NE. 23) JONCE = 0

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
!        INS KEY
         MPS = MP
         CALL SAVESAM()
         IF (NPUT .EQ. 23 .OR. NPUT .EQ. 26 .OR. NPUT .EQ. 27 .or. &
             NPUT .EQ. 40 .OR. NPUT .EQ. 41 .or. NPUT .EQ. 49 .or. &
             NPUT .EQ. 50 .or. NPUT .EQ. 51 ) THEN
!           kijken welk punt bij deleten en bij oppakken, changen
            CALL ISPOI1(     XS,     YS,     NS,   XP,     YP,     MP)
         ENDIF
         IF ( NPUT .EQ. 23 .AND. MP .NE. 0) THEN
!           punt oppakken
            CALL CIRR(XP,YP,0)
            NPUT = 24
         ELSE IF (NPUT .EQ. 24 .AND. MP .NE. 0) THEN
!           punt neerzetten
            XS(MP) = XP
            YS(MP) = YP
            CALL KCIR(XP,YP,ZS(MP))
            NPUT   = 23
         ELSE IF (NPUT .EQ. 25) THEN
!           punt toevoegen
            CALL INCREASESAM(NS)
            IF (NS .GE. 1) THEN
               RD = ZS(NS)
            ELSE
               RD = ZISO
            ENDIF
            ! CALL TYPEVALUE(RD,KEY)
            NS     = NS + 1
            XS(NS) = XP
            YS(NS) = YP
            ZS(NS) = RD
            CALL KCIR(XP,YP,ZS(NS))
!           user is editing samples: mark samples as unstructured
            MXSAM = 0
            MYSAM = 0
            IPSTAT = IPSTAT_NOTOK
         ELSE IF ( NPUT .EQ. 26 .AND. MP .NE. 0) THEN
!           punt deleten
            CALL CIRR(XP,YP,0)
            DO 30 K = MP,NS
               XS(K) = XS(K+1)
               YS(K) = YS(K+1)
               ZS(K) = ZS(K+1)
   30       CONTINUE
            NS = NS - 1
!           user is editing samples: mark samples as unstructured
            MXSAM = 0
            MYSAM = 0
            IPSTAT = IPSTAT_NOTOK
         ELSE IF ( NPUT .EQ. 27 .AND. MP .NE. 0) THEN
!           punt in waarde veranderen
            RD = ZS(MP)
            CALL TYPEVALUE(RD,KEY)
            CALL KCIR(XP,YP,RD)
            ZS(MP) = RD
         ELSE IF ( NPUT == 40 .OR. NPUT == 41) THEN
            IF (MP .NE. 0) THEN
               IF (L1 == 0) THEN
                   L1   = MP
                   NPUT = 41
               ELSE
                   L2   = MP
                   NPUT = 40
                   CALL insertsamples(L1,L2)
                   L1   = 0
                   L2 = 0
                   KEY  = 3
               ENDIF
            ENDIF
         ELSE IF ( NPUT .EQ. 49 ) THEN ! Click sample point to set min value for isocol2
            KEY  = 3
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
         ELSE IF ( NPUT .EQ. 50 ) THEN ! Click sample point to set max value for isocol2
            KEY  = 3
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
         ELSE IF ( NPUT .EQ. 58 ) THEN ! Click start point of sample-based polygon
            call make_samplepath(xp,yp)
         ENDIF
      ELSE IF (KEY .EQ. 22) THEN
!        ENTER KEY
      ELSE IF (KEY .EQ. 23) THEN
!        ESC
         MP = MPS
         CALL RESTORESAM()
         KEY = 3
      ELSE IF (KEY .EQ. 27) THEN
!        TAB
         CALL ISPOI1(     XS,     YS,     NS,  XP,     YP,     MP)
         IF (MP .NE. 0) THEN
            RD  = ZS(MP)
            MPS = MP
            CALL SAVESAM()
            CALL CHADEP(XP,YP,RD,KEY)
            ZS(MP) = RD
         ENDIF
      ELSE IF (KEY .EQ. 73 .OR. KEY .EQ. 73+32) THEN ! I
!        insert mode
         NPUT = 25
      ELSE IF (KEY .EQ. 8) THEN    ! Backspace KEY
!        delete all samples (within polygon if any) and stay in previous mode.
         call savesam()
         call delsam(1)
         key = 3
!        user is editing samples: mark samples as unstructured
         MXSAM = 0
         MYSAM = 0
         IPSTAT=IPSTAT_NOTOK
      ELSE IF (KEY .EQ. 68 .OR. KEY .EQ. 68+32) THEN ! D
!       delete mode
         NPUT = 26
      ELSE IF (KEY .EQ. 70 .OR. KEY .EQ. 70+32) THEN ! f
         CALL SAVESAM()
         ns = 10
         call increasesam(ns)
         xs(1) = xp
         ys(1) = yp
         CALL TYPEVALUE(RD,KEY)
         zs(1) = rd
         NS = 1
         call flow_initfloodfill()
         call restoresam()
         key = 3
      ELSE IF (KEY .EQ. 82 .OR. KEY .EQ. 82+32 .AND. NPUT .NE. 24) THEN ! R
!       replace mode, maar niet bij zetten
         NPUT = 23
      ELSE IF (KEY .EQ. 76 .OR. KEY .EQ. 76+32) THEN  ! L
!        line mode
         NPUT = 40
      ELSE IF (KEY .EQ. 67 .OR. KEY .EQ. 67+32) THEN  ! C
!        change mode
         NPUT = 27
      ELSE IF (                 KEY .EQ. 77+32) THEN  ! m (case sensitive!)
!        click sample to set minimum for isocol2
         NPUT = 49
      ELSE IF (KEY .EQ. 77                    ) THEN  ! M (case sensitive!)
!        click sample to set maximum for isocol2
         NPUT = 50
      ELSE IF (KEY .EQ. 72 .OR. KEY .EQ. 72+32) THEN  ! H: hide/show samples
!        click sample to set maximum for isocol2
         ndraw(32) = -ndraw(32)
         key = 3
      ELSE IF (KEY .EQ. 98) THEN
!        b RINGS BELL
         CALL KTEXT('B Rings Bell',2,6,11)
         CALL OKAY(0)
      ELSE IF (KEY .EQ. 81 .or. KEY .EQ. 81+32 ) THEN ! Q (for testing only)
         call make_orthocenters(0.5d-2,1000)
!         call copy_sendlist_to_sam()
         NPUT = 58
      ENDIF

      GOTO 10

      END SUBROUTINE EDITSAM
