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

      SUBROUTINE EDITGRID(MODE,NFLD,KEY)
      use unstruc_colors
      use m_grid
      implicit none
      integer :: mode, nfld, key

      integer :: L, NLEVEL, JA, NUM, NWHAT, NPUT, NUMB, MP, NP, MD, ND, &
                 ML, NL, MH, NH, NUMP, NLOC, IN, JN, INSIDE, ndraw, NCOL
      integer :: newmode

      COMMON /HELPNOW/ WRDKEY,NLEVEL
      COMMON /DRAWTHIS/ ndraw(50)

      CHARACTER TEX*20, WRDKEY*40, FIELDOP*40

      double precision :: xp, yp, wf(4)

      TEX    =  ' '//FIELDOP(NFLD)
      L      =  len_trim(TEX)
      WRDKEY =  FIELDOP(NFLD)
      NLEVEL =  3
      JA     =  0
      NUM    =  0
      NWHAT  =  0
      NPUT   =  0
      NUMB   =  17
      NCOL   =  NCOLDG

      MP     = 0
      NP     = 0
      CALL BOTLIN(0,NUMB,KEY)

    10 CONTINUE
      CALL DRAWNU(KEY)
      CALL KTEXT(TEX,1,2,15)
      CALL KTEXT(' Click Grid Points  ',1,3,15)
      CALL putget_un(NUM,NWHAT,NPUT,NUMB,XP,YP,KEY)

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
         IF (NPUT .EQ. 0 .OR. NPUT .EQ. -2) THEN
!           kijken welk punt bij deleten en bij oppakken
            CALL ISPOIN(     xc,     yc,     mmax, nmax, MC,     NC,   zc,    &
                             XP,     YP,     MP,     NP)
         ENDIF
         IF ( NPUT .EQ. 0 .AND. MP .NE. 0) THEN
!           punt oppakken
            CALL TEKGRPT(     xc,     yc,     mmax, nmax, MC,     NC,           &
                              MP,     NP,      0        )
            NPUT = 1
         ELSE IF (NPUT .EQ. 1 .AND. MP .NE. 0) THEN
!           punt neerzetten
            IF (NFLD .EQ. 1) THEN
               CALL SAVEGRD()
               xc(MP,NP) = XP
               yc(MP,NP) = YP
               CALL TEKGRPT(     xc,     yc,     mmax, nmax, MC,     NC,  &
                                 MP,     NP,   NCOL        )
            ELSE IF (NFLD .EQ. 2) THEN
               NUMP = 80
               NLOC = 1
               ML   = MAX(1,MP-NUMP)
               MH   = MIN(MC,MP+NUMP)
               NL   = MAX(1,NP-NUMP)
               NH   = MIN(NC,NP+NUMP)
               CALL TEKGRD(xc,yc,mmax, nmax, ML,NL,MH,NH,0,NDRAW(38),key,mc)
               CALL TEKGRD(xch,ych,mmax, nmax, ML,NL,MH,NH,0,NDRAW(16),key,mch)
               CALL SAVEGRD()
               xc(MP,NP) = XP
               yc(MP,NP) = YP
               CALL MODFLD(     xc,     yc,    xch,    ych,   mmax, nmax, &
                                MC,     NC,     MP,     NP,   &
                              NUMP,   NLOC,      1,      1)
               CALL TEKGRD(xc,yc,mmax, nmax, ML,NL,MH,NH,NCOL,NDRAW(38),key,mc)
               CALL TEKGRD(xch, ych, mmax, nmax, ML, NL, MH, NH, NCOLRG, NDRAW(16),key,mch)
            ENDIF
            NPUT   = 0
         ELSE IF (NPUT .EQ. -1) THEN
!           punt toevoegen
            CALL FINDNM(     XP,     YP,     xc,     yc, mmax, nmax,     &
                             MC,     NC, INSIDE,             &
                             MP,     NP,     IN,     JN, wf)
            IF (INSIDE .EQ. 1) THEN
               CALL SAVEGRD()
               CALL MODGR1(NPUT,             &! xc,  yc, mmax, nmax, MC, NC,
                           MP, NP, IN, JN)!, NCOL)
            ELSE
               CALL OKAY(0)
            ENDIF
         ELSE IF ( NPUT .EQ. -2 .AND. MP .NE. 0) THEN
!           punt deleten
            CALL SAVEGRD()
            CALL TEKGRPT(     xc,     yc,     mmax, nmax, MC,     NC,    &
                              MP,     NP,      0        )
            CALL MODGR1(NPUT,                &!xc, yc, mmax, nmax, MC, NC,
                        MP, NP, IN, JN)!, NCOL)
         ENDIF
      ELSE IF (KEY .EQ. 22) THEN
!        ENTER KEY ENKEL DISPLAY
         CALL ISPOIN(     xc,     yc,     mmax, nmax, MC,     NC,   zc, &
                          XP,     YP,     MD,     ND)
      ELSE IF (KEY .EQ. 23) THEN
!        ESCAPE KEY
         CALL RESTOREGRD()
         KEY   = 3
      ELSE IF (KEY .EQ. 27) THEN
!        TAB
         !CALL SHWXYZ(xc, yc, zc,MC,NC,0,KEY,M,N)
      ELSE IF (KEY .EQ. 73 .OR. KEY .EQ. 73+32) THEN
         IF (NPUT .NE. 1) THEN
!           kijken welk punt dit is t.b.v insert mode
            CALL ISPOIN(     xc,     yc,     mmax, nmax, MC,     NC,   zc,  &
                             XP,     YP,     MP,     NP)
         ENDIF
         NPUT = -1
      ELSE IF (KEY .EQ. 8) THEN    ! Backspace KEY
!        delete entire network (within polygon if any) and stay in previous mode.
         call delgrd(KEY,1,1)
         key = 3
      ELSE IF (KEY .EQ. 68 .OR. KEY .EQ. 68+32) THEN
!        delete mode
         NPUT = -2
      ELSE IF (KEY .EQ. 82 .OR. KEY .EQ. 82+32 .AND. NPUT .NE. 1) THEN
!        replace mode, maar niet bij zetten
         NPUT =  0
      ELSE IF (KEY .EQ. 85 .OR. KEY .EQ. 85+32 ) THEN ! U-KEY, UPDATE PARTITIONING COUNT
         CALL TEKnumnetcells(1)
         KEY = 3
      ELSE IF (KEY .EQ. 98) THEN
!        b RINGS BELL
         CALL KTEXT(' B Rings Bell',2,6,11)
         CALL OKAY(0)
      ELSE IF (KEY .EQ. 76 .OR. KEY .EQ. 76+32) THEN
!        CALL TEKHOOK(XP,YP)
      ENDIF
!
      GOTO 10
!
      END subroutine editgrid
