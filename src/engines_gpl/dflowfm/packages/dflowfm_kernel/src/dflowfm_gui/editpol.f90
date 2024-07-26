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

   subroutine EDITPOL(MODE, KEY, NETFLOW)
      use m_sferic
      use M_POLYGON
      use network_data, only: netstat, NETSTAT_CELLS_DIRTY
      use M_MISSING
      use m_partitioninfo
      use unstruc_colors
      use unstruc_model
      use unstruc_display
      use m_flow, only: kmx, jasal, iturbulencemodel
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

      common / HELPNOW / WRDKEY, NLEVEL
      common / QNRGF / JQN
      common / SETTINGS / FDYN, FBOUY, CDFLOW, CFRIC, MOMENTS, JANET

      character TEX * 26, WRDKEY * 40, fnam * 255

      if (jampi == 1) then
         write (tex, "(' EDITPOL:', I5)") my_rank
      else
         tex = 'EDITPOL'
      end if

      WRDKEY = TEX
      NLEVEL = 2
      NUM = 0
      NWHAT = 0
      NPUT = -1
      NUMB = 2
      JAQUIT = 0
      MP = NPL
      L1 = 0

      call SAVEPOL()

10    continue
      call DRAWNU(KEY)
      call KTEXT(TEX, 1, 2, 15)
      call putget_un(NUM, NWHAT, NPUT, NUMB, XP, YP, KEY)

      if (KEY /= 81 .and. KEY /= 81 + 32) JAQUIT = 0

      if (NUM /= 0) then
!        ER IS EEN KEUZE
         if (NUM == 4) then
            MODE = NWHAT
            return
         else
            if ((JQN == 1 .and. NUM == 5 .and. NWHAT == 1) .or. &
                (JQN == 2 .and. NUM == 5 .and. NWHAT == 8)) then
               MP = 0
            end if
         end if
         call CHOICES(MODE, NUM, NWHAT, KEY)
      else if (KEY >= 577) then ! Alt+letter switches edit mode.
         call selecteditmode(newmode, key)
         if (newmode > 0 .and. newmode /= mode) then
            mode = newmode
            return
         end if
      else if (KEY == 21) then ! Left mouse or ins
!        edit/modify polygon: netcell administration out of date
!        netstat = NETSTAT_CELLS_DIRTY        ! unwanted during flow computations

!        INS KEY
         call SAVEPOL()
         if (NPUT == 0 .or. NPUT == -2 .or. NPUT == 56 .or. NPUT == 61) then
!           kijken welk punt bij deleten en bij oppakken
            call ISPOI1(XPL, YPL, NPL, XP, YP, MP)
         end if
         if (NPUT == 0 .and. MP /= 0) then
!           punt oppakken
            call MOVABS(XP, YP)
            call HLCIR(RCIR, NCOLTX)
            NPUT = 1
         else if (NPUT == 1 .and. MP /= 0) then
!           punt neerzetten
            call DISP2C(XPL, YPL, NPL, RCIR, 0)
            XPL(MP) = XP
            YPL(MP) = YP
            call DISP2C(XPL, YPL, NPL, RCIR, NCOLTX)
            NPUT = 0
         else if (NPUT == -1) then
!           punt toevoegen
            call increasepol(npl + 1, 1)
            call DISP2C(XPL, YPL, NPL, RCIR, 0)
            call MODLN2(XPL, YPL, ZPL, MAXPOL, NPL, MP, XP, YP, NPUT)
            call DISP2C(XPL, YPL, NPL, RCIR, NCOLTX)
         else if (NPUT == -2 .and. MP /= 0) then
!           punt deleten
            call SETCOL(0)
            call MOVABS(XP, YP)
            if (MP == 1) then
               call CIR(1.4d0 * RCIR)
            else
               call CIR(RCIR)
            end if
            call DISP2C(XPL, YPL, NPL, RCIR, 0)
            call MODLN2(XPL, YPL, ZPL, MAXPOL, NPL, MP, XP, YP, NPUT)
            call DISP2C(XPL, YPL, NPL, RCIR, NCOLTX)
         else if (NPUT == 40 .or. NPUT == 41) then
!           Polyline to land boundary
            call ISPOI1(XPL, YPL, NPL, XP, YP, MP)
            if (MP /= 0) then
               call MOVABS(XP, YP)
               call HLCIR(RCIR, NCOLTX)
               if (L1 == 0) then
                  L1 = MP
                  NPUT = 41
               else
                  L2 = MP
                  NPUT = 40
                  call POLTOLAND(L1, L2)
                  L1 = 0
                  L2 = 0
                  KEY = 3
               end if
            end if
         else if (NPUT == 42 .or. NPUT == 43) then
!           Polyline to net boundary
            call ISPOI1(XPL, YPL, NPL, XP, YP, MP)
            if (MP /= 0) then
               call MOVABS(XP, YP)
               call HLCIR(RCIR, NCOLTX)
               if (L1 == 0) then
                  L1 = MP
                  NPUT = 43
               else
                  L2 = MP
                  NPUT = 42
                  call POLTONET(L1, L2)
                  L1 = 0
                  L2 = 0
                  KEY = 3
               end if
            end if
         else if (NPUT == 44 .or. NPUT == 45) then
!           Merge two polylines, click two end points.
            call ISPOI1(XPL, YPL, NPL, XP, YP, MP)
            if (mp /= 0 .and. .not. ispolystartend(xpl, ypl, npl, maxpol, mp)) then
               ! Clicked point was not an end point, discard it.
               mp = 0
            end if
            if (MP /= 0) then
               call MOVABS(XP, YP)
               call HLCIR(RCIR, NCOLTX)
               if (L1 == 0) then
                  L1 = MP
                  NPUT = 45
               else
                  L2 = MP
                  NPUT = 44
                  call savepol()
                  call mergepoly(xpl, ypl, zpl, maxpol, npl, L1, L2)
                  L1 = 0
                  L2 = 0
                  KEY = 3
               end if
            end if
         else if (NPUT == 46 .or. NPUT == 47 .or. NPUT == 466 .or. NPUT == 477) then
!           Refine polygon substring (click 2 points)
            call ISPOI1(XPL, YPL, NPL, XP, YP, MP)
            if (MP /= 0) then
               call MOVABS(XP, YP)
               call HLCIR(RCIR, NCOLTX)
               if (L1 == 0) then
                  L1 = MP
                  if (NPUT == 46) then
                     NPUT = 47
                  else
                     NPUT = 477
                  end if
               else
                  L2 = MP
                  if (NPUT == 46 .or. NPUT == 47) then
                     NPUT = 47
                     call refinepolygonpart(L1, L2, 0)
                  else
                     NPUT = 477
!                     get uniform spacing
                     RD = dxuni
                     call TYPEVALUE(dxuni, key)
                     call refinepolygonpart(L1, L2, 1)
                  end if
                  L1 = 0
                  L2 = 0
                  KEY = 3
               end if
            end if
         else if (NPUT == 56 .and. MP /= 0) then
!           punt oppakken
            call MOVABS(XP, YP)
            call HLCIR(RCIR, NCOLTX)
            NPUT = 57
         else if (NPUT == 57 .and. MP /= 0) then
!           punt neerzetten
            call DISP2C(XPL, YPL, NPL, RCIR, 0)
            call copypol(MP, XP, YP)
            call DISP2C(XPL, YPL, NPL, RCIR, NCOLTX)
            NPUT = 56
         else if (NPUT == 61 .and. MP /= 0) then
!           punt in waarde veranderen
            RD = zpl(MP)
            call TYPEVALUE(RD, KEY)
            call KCIR(XP, YP, RD)
            Zpl(MP) = RD
         else if (NPUT >= 62 .and. NPUT <= 67) then ! NPUT == 62 .OR. NPUT == 63 .OR. NPUT == 64) THEN
!           Polyline to net boundary
            call ISPOI1(XPL, YPL, NPL, XP, YP, MP)
            if (MP /= 0) then
               call MOVABS(XP, YP)
               call HLCIR(RCIR, NCOLTX)
               if (L1 == 0) then
                  L1 = MP
                  NPUT = NPUT + 1 ! 63 or 66
               else if (L2 == 0) then
                  L2 = MP
                  NPUT = NPUT + 1 ! 64 or 67
               else
                  L3 = MP
                  NPUT = NPUT - 2 ! 62 or 65

                  if (NPUT == 62) then
                     ja4 = 0
                     call confrm('use fourth side?', ja4)
                     call pol2curvi(L1, L2, L3, ja4)
                  else
                     call pol2curvi_tri(L1, L2, L3)
                  end if

                  L1 = 0
                  L2 = 0
                  L3 = 0
                  KEY = 3
               end if
            end if
         end if
      else if (KEY == 22) then !  ENTER KEY
         if (NETFLOW == 2) then
            iresult = FLOW()
            if (iresult == DFM_SIGINT) then
               call mess(LEVEL_ERROR, 'Final handling of SIGINT signal. Stopping program.')
               call STOPINT()
            else if (iresult /= DFM_NOERR) then
               call qnerror('Error occurred while running, please inspect your diagnostic output.', ' ', ' ')
            end if
!         ELSE IF (NPL .EQ. 0) THEN
!             CALL SOLVE(0)
         else if (NPL >= 3 .and. NPL <= 4) then
            call MAKEPANELXY(1 - JANET)
            call DELPOL()
         else if (NPL >= 2) then
            call POLTOLINES()
            call DELPOL()
         end if
         KEY = 3
      else if (KEY == 23) then ! ESC
         call RESTOREPOL()
         KEY = 3
         if (nput == 1) then
            NPUT = 0
         end if
      else if (KEY == 27) then
!        TAB
!        CALL SHWXYZ2(X,Y,RD1,RD2,RD3,MC,NC,0,KEY,M,N)
      else if (KEY == 73 .or. KEY == 73 + 32) then ! i key
         if (NPUT /= 1) then
!           kijken welk punt dit is t.b.v insert mode
            call ISPOI1(XPL, YPL, NPL, XP, YP, MP)
            if (MP == 0 .and. NPL /= 0) then
               NPL = NPL + 1
               call increasepol(npl, 1)
               XPL(NPL) = dmiss
               YPL(NPL) = dmiss
               ZPL(NPL) = dmiss
            else if (mp /= 0) then
               ! Point was found, now highlight it temporarily on screen.
               call MOVABS(XP, YP)
               call HLCIR(RCIR, NCOLTX)
            end if
         end if
         NPUT = -1
      else if (KEY == 8) then ! Backspace KEY
!        delete all polygons and stay in previous mode.
         call savepol()
         call delpol()
         key = 3
      else if (KEY == 68 .or. KEY == 68 + 32) then ! D KEY
!        delete mode
         NPUT = -2
      else if (KEY == 81) then ! .OR. KEY .EQ. 81+32) THEN                      ! Q-key
!         call split_pol(2,2,100,100)

         NPUT = 62
         L1 = 0
         L2 = 0

      else if (KEY == 81 + 32) then
         NPUT = 65
         L1 = 0
         L2 = 0
      else if (KEY == 82 .or. KEY == 82 + 32 .and. NPUT /= 1) then ! R KEY
!        replace mode, maar niet bij zetten
         NPUT = 0
      else if (KEY == 67) then ! C key
         NPUT = 56 ! copy polygon orthogonally
      else if (KEY == 67 + 32) then ! c KEY hk's original
         NPUT = 61 ! change zpl value
      else if (KEY == 88 .or. KEY == 88 + 32) then ! X KEY
!        Lijn openbreken met X
!         CALL SAVEP(MP,MPS,XPL,YPL,NPL,XPH,YPH,NPH,MAXPOL)
         call SAVEPOL()
         call ISPOI1(XPL, YPL, NPL, XP, YP, MP)
         if (MP /= 0) then
            call DISP2C(XPL, YPL, NPL, RCIR, 0)
            XPL(MP) = dmiss
            YPL(MP) = dmiss
            ZPL(MP) = dmiss
            call DISP2C(XPL, YPL, NPL, RCIR, NCOLTX)
         end if
      else if (KEY == 69 + 32) then ! e KEY
!        edit/modify polygon: netcell administration out of date
         netstat = NETSTAT_CELLS_DIRTY

!        Delete deelpolygoon met E
         call SAVEPOL()
         call ISPOI1(XPL, YPL, NPL, XP, YP, MP)
         if (MP /= 0) then
            call DISP2C(XPL, YPL, NPL, RCIR, 0)
            call MODLN2(XPL, YPL, ZPL, MAXPOL, NPL, MP, XP, YP, -3)
            call DISP2C(XPL, YPL, NPL, RCIR, NCOLTX)
         end if
      else if (KEY == 69) then ! E key
!        edit/modify polygon: netcell administration out of date
         netstat = NETSTAT_CELLS_DIRTY

         call SAVEPOL()
         call ISPOI1(XPL, YPL, NPL, XP, YP, MP)
         if (MP /= 0) then
            call DISP2C(XPL, YPL, NPL, RCIR, 0)
            call MODLN2(XPL, YPL, ZPL, MAXPOL, NPL, MP, XP, YP, -4)
            call DISP2C(XPL, YPL, NPL, RCIR, NCOLTX)
         end if

      else if (-KEY == 71 .or. -KEY == 71 + 32) then ! G KEY
         ! MIRROR LAST POLYGON PART IN Y
         call SAVEPOL()
         !CALL SAVEP(MP,MPS,XPL,YPL,NPL,XPH,YPH,NPH,MAXPOL)
         do K = MP - 1, 1, -1
            NPL = NPL + 1
            XPL(NPL) = XPL(K)
            YPL(NPL) = 2 * YPL(MP) - YPL(K)
            ZPL(NPL) = ZPL(K)
         end do
         call DISP2C(XPL, YPL, NPL, RCIR, NCOLTX)
         MP = NPL
      else if (KEY == 81 .or. KEY == 81 + 32) then
         !  JAQUIT = JAQUIT + 1
         if (JAQUIT == 2) call STOPINT()
      else if (KEY == 86 .or. KEY == 86 + 32) then
         call VIEWCYCLE(KEY)
      else if (KEY == 43 .or. KEY == 140) then ! -
         call KPLOTPLUSMIN(-1)
         key = 3
      else if (KEY == 45 .or. KEY == 141) then ! +
         call KPLOTPLUSMIN(1)
         key = 3
      else if (KEY == 42) then ! *
         call nPLOTPLUSMIN(1)
         key = 3
      else if (KEY == 47) then ! /
         call nPLOTPLUSMIN(-1)
         key = 3
      else if (KEY == 55) then ! 7
         jsfertek = 1 - jsfertek
         call WEAREL()
         key = 3
      else if (KEY == 87 + 32) then ! w for water  + 1 (m)

         call DROPWATER(XP, YP, 1)
         key = 3

      else if (KEY == 87) then ! W for water  - 1 (m)

         call DROPWATER(XP, YP, -1)
         key = 3

      else if (KEY == 66 + 32) then ! b for bed + 1 (m)

         call DROPland(XP, YP, 1)
         key = 3

      else if (KEY == 66) then ! B for bed - 1 (m)

         call DROPland(XP, YP, -1)
         key = 3

      else if (jasal > 0 .and. KEY == 83 + 32) then ! s for salt + 1 (ppt)

         call DROPzout(1)
         key = 3

      else if (jasal > 0 .and. KEY == 83) then ! S for salt - 1 (ppt)

         call DROPzout(-1)
         key = 3

      else if (kmx > 0 .and. iturbulencemodel == 3 .and. KEY == 75 + 32) then ! k for kinetic + 0.01

         call DROPk(XP, YP, 1)
         key = 3

      else if (KEY == 84 + 32) then ! t add (to) tracer
         call droptracer(xp, yp, 1d0)
!         call add_particles(1,xp,yp,0)
      else if (KEY == 84) then ! T t  substract from tracer
         call droptracer(xp, yp, -1d0)

      else if (KEY == 32) then
         call flow_spatietimestep()
         key = 3
      else if (KEY == 76 .or. KEY == 76 + 32) then ! L KEY

         NPUT = 40 ! TO LAND MODE

      else if (KEY == 70 + 32) then ! F KEY

         NPUT = 46 ! Refine polygon between two clicked points

      else if (key == 70) then ! SHIFT-F KEY
         NPUT = 466 ! refine polygon between two points with uniform spacing

      else if (key == 70) then
!        refine polygon with uniform width

         NPUT = 466

      else if (KEY == 77 .or. KEY == 77 + 32) then ! M KEY

         NPUT = 44 ! Merge twee deelpolygonen

      else if (KEY == 78 .or. KEY == 78 + 32) then ! N KEY

         NPUT = 42 ! TO NET  MODE

      else if (KEY == 27 .or. KEY == 27 + 32) then ! ; KEY

         jazoomshift = 0
         nshift = 0
         do while (jazoomshift /= 1 .and. nshift < numzoomshift * npl)
            call zoomshift(nshift)
            key = 3
            ndrawpol = 1
            call DRAWNU(KEY)
            call halt2(jazoomshift)
         end do

         ndrawpol = 2
      else if (KEY == 46) then ! . KEY
         call ISPOI1(XPL, YPL, NPL, XP, YP, MP)
         call flippo(MP)
         key = 3

      else if (KEY == 79 .or. KEY == 79 + 32) then ! O - KEY
         L1 = index(md_plifile, '_0')
         if (L1 == 0) then
            md_plifile = 'plif_0001.pli'
            npolf = 0
         end if
         L1 = index(md_plifile, '_0')
         if (L1 > 0) then
            fnam = md_plifile(1:L1)//'0001.pli'
            npolf = npolf + 1
            write (fnam(L1 + 1:L1 + 4), '(i4.4)') npolf
            call newnewfil(mout, fnam)
            if (mout > 0) then
               call wripol(mout)
            end if
            call plotnu(fnam)
         end if

      end if
!
      goto 10
!
   end subroutine EDITPOL
