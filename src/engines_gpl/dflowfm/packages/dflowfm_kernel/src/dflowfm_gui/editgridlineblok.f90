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

      SUBROUTINE EDITGRIDLINEBLOK(MODE,NFLD,KEY)
      use unstruc_colors
      use m_grid
      implicit none

      integer :: mode, nfld, key
      integer :: newmode
      integer :: ndraw, nlevel, bm, nb, mb2, nb2, npt, npt2, nputo, itype, NCOL
      integer :: jonce

      COMMON /DRAWTHIS/ ndraw(50)
      COMMON /HELPNOW/ WRDKEY,NLEVEL
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      CHARACTER TEX*20, WRDKEY*40, FIELDOP*40
      integer :: num, nwhat, numb, nump, mp, np, ipt, ja, mb, m1b, n1b, m2b, n2b, m1, n1, m2, n2, m, n
      integer :: nput
      double precision :: xp, yp

      TEX    = ' '//FIELDOP(NFLD)
      WRDKEY = FIELDOP(NFLD)
      NLEVEL = 3
      NUM    = 0
      NWHAT  = 0
      NUMB   = 6
      NCOL   = NCOLRG
      NUMP   = 80
      MP     = 0
      NP     = 0
      ITYPE  = 1

      NPUT   = 10
      CALL RESETB(NPUT)
      CALL BOTLIN(0,NUMB,KEY)

    10 CONTINUE
      CALL DRAWNU(KEY)
      CALL TEKB(Xc,Yc,MMAX,NMAX,NCOLLN)
      CALL KTEXT(TEX,1,2,15)
      IF (NPT .LE. 1) THEN
         CALL KTEXT(' Indicate a Line    ',1,3,15)
      ELSE
         CALL KTEXT(' Influence or rght M',1,3,15)
      ENDIF

      CALL putget_un(NUM,NWHAT,NPUT,NUMB,XP,YP,KEY)
      IF (KEY .NE. 23) JONCE = 0

      IF (NUM .NE. 0) THEN
!        ER IS EEN KEUZE
         IF (NUM .EQ. 4) THEN
            MODE = NWHAT
            CALL TEKB(Xc,Yc,MMAX,NMAX,0)
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
!        kijken welk punt
         CALL ISPOIN(     Xc,     Yc,     mmax, nmax, MC,     NC,   Zc,  &
                          XP,     YP,     MP,     NP)
         IF (MP .NE. 0) THEN
            IF (NPUT .EQ. 16) THEN
               CALL ONSAMELINE(IPT,MP,NP,JA)
               IF (JA .EQ. 1) THEN
                  MB(IPT) = MP
                  NB(IPT) = NP
                  CALL CIRR(Xc(MP,NP), Yc(MP,NP), NCOLLN)
                  IF (NPT .EQ. 1) NPUT = 11
                  IF (NPT .EQ. 2) NPUT = 14
                  IF (NPT .EQ. 3) NPUT = 15
                  IF (NPT .EQ. 4) NPUT = 19
               ELSE
                  CALL QNERROR('POINT 1 AND 2 SHOULD LIE',     &
                               'ON THE SAME GRIDLINE',' ')
               ENDIF
            ELSE
               CALL NEWBLOCKPOINT(MP,NP,JA,IPT)
               IF (JA .EQ. 1) THEN
!                 voeg punt toe
                  CALL ONSAMELINE(IPT,MP,NP,JA)
                  IF (JA .EQ. 1) THEN
                     CALL SAVEB(NPUT)
                     NPT = NPT + 1
                     MB(NPT) = MP
                     NB(NPT) = NP
                     CALL CIRR(Xc(MB(NPT),NB(NPT)), Yc(MB(NPT),NB(NPT)),NCOLLN)
                     IF (NPT .EQ. 1) NPUT = 11
                     IF (NPT .EQ. 2) NPUT = 14
                     IF (NPT .EQ. 3) NPUT = 15
                     IF (NPT .EQ. 4) NPUT = 19
                  ELSE
                     CALL QNERROR('POINT 1 AND 2 SHOULD LIE','ON THE SAME GRIDLINE',' ')
                  ENDIF
               ELSE IF (JA .EQ. -1) THEN
!                 niet meer toevoegen
                  CALL QNERROR('4 POINTS: CONTINUE = RIGHT MOUSE OR', 'Enter,',' ')
               ELSE IF (JA .EQ. 0) THEN
!                 oud punt geclickt; uitgummen
                  CALL SAVEB(NPUT)
                  CALL CIRR(Xc(MB(IPT),NB(IPT)),Yc(MB(IPT),NB(IPT)),0)
                  IF (IPT .LE. 2) CALL TEKB(Xc,Yc,MMAX,NMAX,0)
                  MB(IPT) = 0
                  NB(IPT) = 0
                  NPUT    = 16
               ENDIF
            ENDIF
         ENDIF
      ELSE IF (KEY .EQ. 22) THEN
         IF (NPT .LE. 1) THEN
           CALL QNERROR('FIRST PRESS MORE POINTS WITH LEFT MOUSE BUTTON',' ',' ')
         ELSE
!           ENTER KEY
            CALL TEKB(Xc,Yc,MMAX,NMAX,0)
            CALL POSITIVEBLOK()
            M1B = MAX(MB(3)-1,1)
            N1B = MAX(NB(3)-1,1)
            M2B = MIN(MB(4)+1,MC)
            N2B = MIN(NB(4)+1,NC)
            IF (NFLD .NE. 4) THEN
               CALL TEKGRD(Xc,Yc,mmax, nmax, M1B,N1B,M2B,N2B,0,NDRAW(38),key,mc)
            ENDIF
!           Begin Operatie
            CALL SAVEGRD()
            IF (NFLD .EQ. 4) THEN
               M1 = MB(1)
               M2 = MB(2)
               N1 = NB(1)
               N2 = NB(2)
               CALL EDITGRIDLINESHIFT(MODE,NFLD,KEY,M1,N1,M2,N2)
               IF (KEY .NE. 23) THEN
                  CALL TEKGRD(      Xc,     Yc,    mmax, nmax, M1B,           &
                                   N1B,    M2B,    N2B,0,NDRAW(38),key,mc)
                  CALL MODGR2(     Xc,     Yc,     Xch,     Ych,   mmax, nmax, &
                                   MC,     NC,   NUMP)
               ENDIF
            ELSE IF (NFLD .EQ. 5) THEN    ! Attraction
               CALL ATTRACTREPULSE(     Xc,     Yc,     Xch,   Ych, &
                                      mmax,   nmax, &
                                        MC,     NC,   NUMP,     -1)
            ELSE IF (NFLD .EQ. 6) THEN    ! Repulsion
               CALL ATTRACTREPULSE(     Xc,     Yc,     Xch,   Ych, &
                                      mmax,   nmax, &
                                        MC,     NC,   NUMP,      1)
            ELSE IF (NFLD .EQ. 7) THEN
               CALL MODGR4( NUMP,1  )
            ELSE IF (NFLD .EQ. 8) THEN
               CALL MODGR4( NUMP,2  )
            ELSE IF (NFLD .EQ. 9) THEN
               CALL DOSMOOTH(NFLD)!Xc,Yc,mmax, nmax, MC,NC,NFLD,IJC,IJYES)
            ELSE IF (NFLD .EQ. 10) THEN
               CALL LINEMIRROR()!Xc,Yc,mmax, nmax, MC,NC,IJC,IJYES)
            ELSE IF (NFLD .EQ. 11) THEN
               M1 = MB(1)
               M2 = MB(2)
               N1 = NB(1)
               N2 = NB(2)
               CALL LOCALREFINE(NUM, m1, n1, m2, n2, 1)
            ELSE IF (NFLD .EQ. 12) THEN
               M1 = MB(1)
               M2 = MB(2)
               N1 = NB(1)
               N2 = NB(2)
               CALL LOCALREFINE(NUM, m1, n1, m2, n2, 2)
            ENDIF
!           Einde Operatie
            CALL TEKGRD(      Xc,     Yc,    mmax,   nmax, M1B,                  &
                             N1B,    M2B,    N2B, NCOLDG, NDRAW(38),key,mc)
            CALL TEKGRD(     Xch,    Ych,    mmax,   nmax, M1B,                  &
                             N1B,    M2B,    N2B, NCOLRG, NDRAW(16),key,mch)
            IF (NPT .LE. 2) KEY = 3
            CALL RESETB(NPUT)
            NPUT = 10
         ENDIF
      ELSE IF (KEY .EQ. 23) THEN
!        ESC
         JONCE = JONCE + 1
         IF (JONCE .EQ. 1) THEN
            CALL RESTOREB(NPUT)
         ELSE IF (JONCE .EQ. 2) THEN
            NPUT = 10
            CALL RESETB(NPUT)
         ELSE IF (JONCE .EQ. 3) THEN
            CALL RESTOREgrd()
         ENDIF
         KEY = 3
      ELSE IF (KEY .EQ. 27) THEN
!        TAB
         CALL SHWXYZ(Xc,Yc,Zc,mmax, nmax, MC,NC,0,KEY,M,N)
      ENDIF
!
      GOTO 10
!
      END subroutine editgridlineblok
