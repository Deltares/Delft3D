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

   SUBROUTINE EDITPOL(MODE,KEY,NETFLOW)
   use m_sferic
   USE M_POLYGON
   use network_data, only: netstat, NETSTAT_CELLS_DIRTY
   USE M_MISSING
   use m_partitioninfo
   use unstruc_colors
   use unstruc_model
   use unstruc_display
   use m_flow, only : kmx, jasal, iturbulencemodel
   use unstruc_api
   use dfm_error
   use unstruc_messages
   implicit none
   double precision :: cdflow
   double precision :: cfric
   double precision :: fbouy
   double precision :: fdyn
   integer :: janet
   integer :: jaquit, jazoomshift, nshift
   integer :: k
   integer :: l1
   integer :: l2
   integer :: l3
   integer :: moments
   integer :: nlevel
   integer :: nput
   integer :: num
   integer :: numb
   integer :: nwhat

   integer :: MODE, KEY, NETFLOW
   integer :: newmode, mout
   double precision :: xp, yp, RD
   integer :: JQN
   integer :: iresult
   integer :: ja4
   logical, external :: ispolystartend

   COMMON /HELPNOW/ WRDKEY,NLEVEL
   COMMON /QNRGF/ JQN
   COMMON /SETTINGS/ FDYN, FBOUY, CDFLOW, CFRIC, MOMENTS, JANET

   CHARACTER TEX*26, WRDKEY*40, fnam*255

   integer :: iii

   if (jampi == 1) then
      write(tex,"(' EDITPOL:', I5)") my_rank
   else
      tex = 'EDITPOL'
   endif

   WRDKEY = TEX
   NLEVEL =  2
   NUM    =  0
   NWHAT  =  0
   NPUT   = -1
   NUMB   =  2
   JAQUIT = 0
   MP     = NPL
   L1     = 0


   CALL SAVEPOL()

   10 CONTINUE
      CALL DRAWNU(KEY)
      CALL KTEXT(TEX,1,2,15)
      CALL putget_un(NUM,NWHAT,NPUT,NUMB,XP,YP,KEY)

      IF (KEY .NE. 81 .AND. KEY .NE. 81+32) JAQUIT = 0

      IF (NUM .NE. 0) THEN
!        ER IS EEN KEUZE
         IF (NUM .EQ. 4) THEN
            MODE = NWHAT
            RETURN
         ELSE
            IF ((JQN .EQ. 1 .AND. NUM .EQ. 5 .AND. NWHAT .EQ. 1) .OR.   &
                (JQN .EQ. 2 .AND. NUM .EQ. 5 .AND. NWHAT .EQ. 8)) THEN
                MP  = 0
            ENDIF
         ENDIF
         CALL CHOICES(MODE,NUM,NWHAT,KEY)
      ELSE IF (KEY >= 577) THEN ! Alt+letter switches edit mode.
        call selecteditmode(newmode, key)
        if (newmode > 0 .and. newmode /= mode) then
            mode = newmode
            return
        end if
      ELSE IF (KEY .EQ. 21) THEN                                          ! Left mouse or ins
!        edit/modify polygon: netcell administration out of date
!        netstat = NETSTAT_CELLS_DIRTY	! unwanted during flow computations

!        INS KEY
         CALL SAVEPOL()
         IF (NPUT .EQ. 0 .OR. NPUT .EQ. -2 .OR. NPUT .EQ. 56 .OR. NPUT .EQ. 61) THEN
!           kijken welk punt bij deleten en bij oppakken
            CALL ISPOI1(XPL, YPL, NPL, XP, YP, MP)
         ENDIF
         IF ( NPUT .EQ. 0 .AND. MP .NE. 0) THEN
!           punt oppakken
            CALL MOVABS(XP,YP)
            CALL HLCIR(RCIR, NCOLTX)
            NPUT = 1
         ELSE IF (NPUT .EQ. 1 .AND. MP .NE. 0) THEN
!           punt neerzetten
            CALL DISP2C(XPL, YPL, NPL, RCIR,      0)
            XPL(MP) = XP
            YPL(MP) = YP
            CALL DISP2C(XPL, YPL, NPL, RCIR, NCOLTX)
            NPUT   = 0
         ELSE IF (NPUT .EQ. -1) THEN
!           punt toevoegen
            call increasepol(npl+1, 1)
            CALL DISP2C(XPL, YPL, NPL, RCIR,      0)
            CALL MODLN2(XPL, YPL, ZPL, MAXPOL, NPL, MP, XP, YP, NPUT)
            CALL DISP2C(XPL, YPL, NPL, RCIR, NCOLTX)
         ELSE IF ( NPUT .EQ. -2 .AND. MP .NE. 0) THEN
!           punt deleten
            CALL SETCOL(0)
            CALL MOVABS(XP,YP)
            IF (MP .EQ. 1) THEN
               CALL CIR(1.4d0*RCIR)
            ELSE
               CALL CIR(RCIR)
            ENDIF
            CALL DISP2C(XPL, YPL, NPL, RCIR,      0)
            CALL MODLN2(XPL, YPL, ZPL, MAXPOL, NPL, MP, XP, YP, NPUT)
            CALL DISP2C(XPL, YPL, NPL, RCIR, NCOLTX)
         ELSE IF ( NPUT == 40 .OR. NPUT == 41) THEN
!           Polyline to land boundary
            CALL ISPOI1(XPL, YPL, NPL, XP, YP, MP)
            IF (MP .NE. 0) THEN
               CALL MOVABS(XP,YP)
               CALL HLCIR(RCIR, NCOLTX)
               IF (L1 == 0) THEN
                   L1    = MP
                   NPUT = 41
               ELSE
                   L2   = MP
                   NPUT = 40
                   CALL POLTOLAND(L1,L2)
                   L1   = 0
                   L2 = 0
                   KEY  = 3
               ENDIF
            ENDIF
         ELSE IF ( NPUT == 42 .OR. NPUT == 43) THEN
!           Polyline to net boundary
            CALL ISPOI1(XPL, YPL, NPL, XP, YP, MP)
            IF (MP .NE. 0) THEN
               CALL MOVABS(XP,YP)
               CALL HLCIR(RCIR, NCOLTX)
               IF (L1 == 0) THEN
                   L1    = MP
                   NPUT = 43
               ELSE
                   L2   = MP
                   NPUT = 42
                   CALL POLTONET(L1,L2)
                   L1   = 0
                   L2 = 0
                   KEY  = 3
               ENDIF
            ENDIF
         ELSE IF ( NPUT == 44 .OR. NPUT == 45) THEN
!           Merge two polylines, click two end points.
            CALL ISPOI1(XPL, YPL, NPL, XP, YP, MP)
            if (mp /= 0 .and. .not. ispolystartend(xpl, ypl, npl, maxpol, mp)) then
                ! Clicked point was not an end point, discard it.
                mp = 0
            end if
            IF (MP .NE. 0) THEN
               CALL MOVABS(XP,YP)
               CALL HLCIR(RCIR, NCOLTX)
               IF (L1 == 0) THEN
                   L1    = MP
                   NPUT = 45
               ELSE
                   L2   = MP
                   NPUT = 44
                   call savepol()
                   CALL mergepoly(xpl, ypl, zpl, maxpol, npl, L1,L2)
                   L1   = 0
                   L2 = 0
                   KEY  = 3
               ENDIF
            ENDIF
         ELSE IF ( NPUT == 46 .OR. NPUT == 47 .OR. NPUT == 466 .OR. NPUT == 477 ) THEN
!           Refine polygon substring (click 2 points)
            CALL ISPOI1(XPL, YPL, NPL, XP, YP, MP)
            IF (MP .NE. 0) THEN
               CALL MOVABS(XP,YP)
               CALL HLCIR(RCIR, NCOLTX)
               IF (L1 == 0) THEN
                   L1    = MP
                   if ( NPUT.eq.46 ) then
                      NPUT = 47
                   else
                      NPUT = 477
                   end if
               ELSE
                   L2   = MP
                   if( NPUT.eq.46 .or. NPUT.eq.47 ) then
                      NPUT = 47
                      CALL refinepolygonpart(L1,L2,0)
                   else
                      NPUT = 477
!                     get uniform spacing
                      RD = dxuni
                      call TYPEVALUE(dxuni,key)
                      CALL refinepolygonpart(L1,L2,1)
                   end if
                   L1   = 0
                   L2 = 0
                   KEY  = 3
               ENDIF
            ENDIF
         ELSE IF ( NPUT .EQ. 56 .AND. MP .NE. 0) THEN
!           punt oppakken
            CALL MOVABS(XP,YP)
            CALL HLCIR(RCIR, NCOLTX)
            NPUT = 57
         ELSE IF (NPUT .EQ. 57 .AND. MP .NE. 0) THEN
!           punt neerzetten
            CALL DISP2C(XPL, YPL, NPL, RCIR,      0)
            call copypol(MP, XP, YP)
            CALL DISP2C(XPL, YPL, NPL, RCIR, NCOLTX)
            NPUT   = 56
         ELSE IF ( NPUT .EQ. 61 .AND. MP .NE. 0) THEN
!           punt in waarde veranderen
            RD = zpl(MP)
            CALL TYPEVALUE(RD,KEY)
            CALL KCIR(XP,YP,RD)
            Zpl(MP) = RD
         ELSE IF ( NPUT.ge.62 .AND. NPUT.le. 67 ) THEN   ! NPUT == 62 .OR. NPUT == 63 .OR. NPUT == 64) THEN
!           Polyline to net boundary
            CALL ISPOI1(XPL, YPL, NPL, XP, YP, MP)
            IF (MP .NE. 0) THEN
               CALL MOVABS(XP,YP)
               CALL HLCIR(RCIR, NCOLTX)
               IF (L1 == 0) THEN
                   L1    = MP
                   NPUT = NPUT +1   ! 63 or 66
               ELSE IF (L2 == 0) THEN
                   L2    = MP
                   NPUT = NPUT +1   ! 64 or 67
               ELSE
                   L3   = MP
                   NPUT = NPUT -2   ! 62 or 65

                   if ( NPUT.eq.62 ) then
                      ja4 = 0
                      call confrm('use fourth side?', ja4)
                      CALL pol2curvi(L1,L2,L3,ja4)
                   else
                      CALL pol2curvi_tri(L1,L2,L3)
                   end if

                   L1 = 0
                   L2 = 0
                   L3 = 0
                   KEY  = 3
               ENDIF
            ENDIF
         ENDIF
      ELSE IF (KEY .EQ. 22) THEN                                          !  ENTER KEY
         IF (NETFLOW == 2) THEN
             iresult = FLOW()
             if (iresult == DFM_SIGINT) then
                call mess(LEVEL_ERROR, 'Final handling of SIGINT signal. Stopping program.')
                call STOPINT()
             else if (iresult /= DFM_NOERR) then
                call qnerror('Error occurred while running, please inspect your diagnostic output.',' ', ' ')
             end if
!         ELSE IF (NPL .EQ. 0) THEN
!             CALL SOLVE(0)
         ELSE IF (NPL .GE. 3 .AND. NPL .LE. 4) THEN
            CALL MAKEPANELXY(1-JANET)
            CALL DELPOL()
         ELSE IF (NPL .GE. 2) THEN
            CALL POLTOLINES()
            CALL DELPOL()
         ENDIF
         KEY = 3
      ELSE IF (KEY .EQ. 23) THEN                                          ! ESC
         CALL RESTOREPOL()
         KEY = 3
         if (nput == 1) then
            NPUT = 0
         end if
      ELSE IF (KEY .EQ. 27) THEN
!        TAB
!        CALL SHWXYZ2(X,Y,RD1,RD2,RD3,MC,NC,0,KEY,M,N)
      ELSE IF (KEY .EQ. 73 .OR. KEY .EQ. 73+32) THEN                      ! i key
         IF (NPUT .NE. 1) THEN
!           kijken welk punt dit is t.b.v insert mode
            CALL ISPOI1( XPL, YPL, NPL, XP, YP, MP)
            IF (MP == 0 .AND. NPL .NE. 0) THEN
               NPL = NPL + 1
               call increasepol(npl, 1)
               XPL(NPL) = dmiss
               YPL(NPL) = dmiss
               ZPL(NPL) = dmiss
            else if (mp /= 0) then
                ! Point was found, now highlight it temporarily on screen.
                CALL MOVABS(XP,YP)
                CALL HLCIR(RCIR, NCOLTX)
            ENDIF
         ENDIF
         NPUT = -1
      ELSE IF (KEY .EQ. 8) THEN                                           ! Backspace KEY
!        delete all polygons and stay in previous mode.
         call savepol()
         call delpol()
         key = 3
      ELSE IF (KEY .EQ. 68 .OR. KEY .EQ. 68+32) THEN                      ! D KEY
!        delete mode
         NPUT = -2
      ELSE IF (KEY .EQ. 81) THEN ! .OR. KEY .EQ. 81+32) THEN                      ! Q-key
!         call split_pol(2,2,100,100)

          NPUT= 62
          L1 = 0
          L2 = 0

      ELSE IF (KEY .EQ. 81+32) THEN
         NPUT = 65
         L1 = 0
         L2 = 0
      ELSE IF (KEY .EQ. 82 .OR. KEY .EQ. 82+32 .AND. NPUT .NE. 1) THEN    ! R KEY
!        replace mode, maar niet bij zetten
         NPUT =  0
      ELSE IF (KEY .EQ. 67) then                                          ! C key
         NPUT =  56                      ! copy polygon orthogonally
      else if (KEY .EQ. 67+32 ) THEN                                      ! c KEY hk's original
         NPUT =  61                      ! change zpl value
      ELSE IF (KEY .EQ. 88 .OR. KEY .EQ. 88+32) THEN                      ! X KEY
!        Lijn openbreken met X
!         CALL SAVEP(MP,MPS,XPL,YPL,NPL,XPH,YPH,NPH,MAXPOL)
         CALL SAVEPOL()
         CALL ISPOI1(XPL, YPL, NPL, XP, YP, MP)
         IF (MP .NE. 0) THEN
            CALL DISP2C(XPL, YPL, NPL, RCIR,      0)
            XPL(MP) = dmiss
            YPL(MP) = dmiss
            ZPL(MP) = dmiss
            CALL DISP2C(XPL, YPL, NPL, RCIR, NCOLTX)
         ENDIF
      ELSE IF (KEY .EQ. 69+32) THEN                                     ! e KEY
!        edit/modify polygon: netcell administration out of date
         netstat = NETSTAT_CELLS_DIRTY

!        Delete deelpolygoon met E
         CALL SAVEPOL()
         CALL ISPOI1(XPL, YPL, NPL, XP, YP, MP)
         IF (MP .NE. 0) THEN
            CALL DISP2C(XPL, YPL, NPL, RCIR, 0)
            CALL MODLN2(XPL, YPL, ZPL, MAXPOL, NPL, MP, XP, YP, -3)
            CALL DISP2C(XPL, YPL, NPL, RCIR, NCOLTX)
         ENDIF
      ELSE IF ( KEY .EQ. 69 ) THEN                                      ! E key
!        edit/modify polygon: netcell administration out of date
         netstat = NETSTAT_CELLS_DIRTY

         CALL SAVEPOL()
         CALL ISPOI1(XPL, YPL, NPL, XP, YP, MP)
         IF (MP .NE. 0) THEN
            CALL DISP2C(XPL, YPL, NPL, RCIR, 0)
            CALL MODLN2(XPL, YPL, ZPL, MAXPOL, NPL, MP, XP, YP, -4)
            CALL DISP2C(XPL, YPL, NPL, RCIR, NCOLTX)
         ENDIF

      ELSE IF (-KEY .EQ. 71 .OR. -KEY .EQ. 71+32) THEN                  ! G KEY
         ! MIRROR LAST POLYGON PART IN Y
         CALL SAVEPOL()
         !CALL SAVEP(MP,MPS,XPL,YPL,NPL,XPH,YPH,NPH,MAXPOL)
         DO K = MP-1,1,-1
            NPL = NPL + 1
            XPL(NPL) = XPL(K)
            YPL(NPL) = 2*YPL(MP) - YPL(K)
            ZPL(NPL) = ZPL(K)
         ENDDO
         CALL DISP2C(XPL, YPL, NPL, RCIR, NCOLTX)
         MP = NPL
      ELSE IF (KEY .EQ. 81 .OR. KEY .EQ. 81+32) THEN
         !  JAQUIT = JAQUIT + 1
         IF (JAQUIT .EQ. 2) CALL STOPINT()
      ELSE IF (KEY .EQ. 86 .OR. KEY .EQ. 86+32) THEN
         CALL VIEWCYCLE(KEY)
      ELSE IF (KEY .EQ. 43 .or. KEY .EQ. 140) THEN                      ! -
         CALL KPLOTPLUSMIN(-1)
         key = 3
      ELSE IF (KEY .EQ. 45 .or. KEY .EQ. 141) THEN                      ! +
         call KPLOTPLUSMIN(1)
         key = 3
      ELSE IF (KEY .EQ. 42) THEN                                        ! *
         CALL nPLOTPLUSMIN(1)
         key = 3
      ELSE IF (KEY .EQ. 47) THEN                                        ! /
         call nPLOTPLUSMIN(-1)
         key = 3
      ELSE IF (KEY .EQ. 55) THEN                                        ! 7
         jsfertek=1-jsfertek
         call WEAREL()
         key = 3
      ELSE IF (                 KEY .EQ. 87+32) THEN                    ! w for water  + 1 (m)

         call DROPWATER(XP,YP,1)
         key = 3

      ELSE IF (KEY .EQ. 87                    ) THEN                    ! W for water  - 1 (m)

         call DROPWATER(XP,YP,-1)
         key = 3

      ELSE IF (                 KEY .EQ. 66+32) THEN                    ! b for bed + 1 (m)

         call DROPland(XP,YP, 1)
         key = 3

      ELSE IF (KEY .EQ. 66                    ) THEN                    ! B for bed - 1 (m)

         call DROPland(XP,YP, -1)
         key = 3

      ELSE IF (jasal > 0 .and.  KEY .EQ. 83+32) THEN                    ! s for salt + 1 (ppt)

         call DROPzout( 1)
         key = 3

      ELSE IF (jasal > 0 .and.     KEY .EQ. 83) THEN                    ! S for salt - 1 (ppt)

         call DROPzout(-1)
         key = 3

      ELSE IF (kmx > 0 .and. iturbulencemodel == 3 .and. KEY .EQ. 75+32) THEN  ! k for kinetic + 0.01

         call DROPk(XP,YP,1)
         key = 3

      ELSE IF (KEY .EQ. 84+32) THEN                                     ! t add (to) tracer
         call droptracer(xp,yp,1d0)
!         call add_particles(1,xp,yp,0)
      ELSE IF (KEY .EQ. 84) THEN                                        ! T t  substract from tracer
         call droptracer(xp,yp,-1d0)

      ELSE IF (KEY .EQ. 32) THEN
         call flow_spatietimestep()
         key = 3
      ELSE IF (KEY .EQ. 76 .OR. KEY .EQ. 76+32) THEN                    ! L KEY

         NPUT = 40  ! TO LAND MODE

      ELSE IF (KEY .EQ. 70+32) THEN                    ! F KEY

         NPUT = 46  ! Refine polygon between two clicked points

      else if ( key.eq.70 ) then                       ! SHIFT-F KEY
         NPUT = 466  ! refine polygon between two points with uniform spacing

      else if ( key.eq. 70 ) then
!        refine polygon with uniform width

         NPUT = 466

      ELSE IF (KEY .EQ. 77 .OR. KEY .EQ. 77+32) THEN                    ! M KEY

         NPUT = 44  ! Merge twee deelpolygonen

      ELSE IF (KEY .EQ. 78 .OR. KEY .EQ. 78+32) THEN                    ! N KEY

         NPUT = 42  ! TO NET  MODE

      ELSE IF (KEY .EQ. 27 .OR. KEY .EQ. 27+32) THEN                    ! ; KEY

         jazoomshift = 0
         nshift = 0
         do while (jazoomshift .ne. 1 .and. nshift < numzoomshift*npl)
            call zoomshift(nshift)
            key = 3
            ndrawpol = 1
            CALL DRAWNU(KEY)
            call halt2(jazoomshift)
         enddo

         ndrawpol = 2
      ELSE IF (KEY .EQ. 46) THEN                                         ! . KEY
         CALL ISPOI1(XPL, YPL, NPL, XP, YP, MP)
         call flippo(MP)
         key = 3

      ELSE IF (KEY .EQ. 79 .OR. KEY == 79 + 32 ) THEN                    ! O - KEY
         L1 = index(md_plifile,'_0')
         IF (L1 == 0) THEN
            md_plifile = 'plif_0001.pli'
            npolf      = 0
         endif
         L1 = index(md_plifile,'_0')
         if (L1 > 0) then
            fnam  = md_plifile(1:L1)//'0001.pli'
            npolf = npolf + 1
            write(fnam(L1+1:L1+4),'(i4.4)') npolf
            call newnewfil(mout, fnam)
            if (mout > 0) then
               call wripol(mout)
            endif
            call plotnu(fnam)
         endif

      ENDIF
!
      GOTO 10
!
      END SUBROUTINE EDITPOL
