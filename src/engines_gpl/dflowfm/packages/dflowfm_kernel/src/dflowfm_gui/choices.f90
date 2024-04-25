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
! Copyright notice:
! Several of the graphical user interface routines below make use of the INTERACTER libraries
! (only when run on Windows platforms with display mode on).
! Copyright on the INTERACTER libraries resides with Interactive Software Services Ltd.
! More information: http://www.winteracter.com/iss
!----------------------------------------------------------------------
! subroutines from net.F90
!----------------------------------------------------------------------
   SUBROUTINE CHOICES(MODE,NUM,NWHAT,KEY)
   use m_netw
   use m_samples
   use m_grid
   USE M_MISSING
   use unstruc_display
   use m_polygon
   use m_partitioninfo
   use m_ec_interpolationsettings
   use gridoperations
   use m_oned_functions, only: convert_cross_to_prof
   use unstruc_model, only: md_ident

   implicit none
   integer :: ja, L, n12, ikey, mnx
   integer :: ndraw
   integer :: MODE,NUM,NWHAT,KEY,nwhat2
   integer :: JDEMO
   integer :: irerun          ! orthogonalisenet: rerun

   COMMON /DRAWTHIS/  ndraw(50)
   COMMON /DEMO/ JDEMO
   integer :: maxexp
   integer :: maxopt, ierr
   integer, parameter :: MAXOP = 64
   CHARACTER*40 OPTION(MAXOP),EXP(MAXOP)
   integer, external :: flow_modelinit

   if ( netstat.ne.NETSTAT_OK ) call setnodadm(0)

   IF (NUM .EQ. 1) THEN
   !     load en save files
      CALL NFILES(MODE, NUM,  NWHAT, KEY)
   ELSE IF (NUM .EQ. 2) THEN
   !     operations
!      if ( jins.ne.1 ) then  ! SPvdP: temporarily disabled
!         jins = 1
!         netstat = NETSTAT_CELLS_DIRTY
!      end if
      IF (NWHAT .EQ. 1) THEN
         CALL RESTORE()
      ELSE IF (NWHAT .EQ. 2) THEN
         CALL SAVENET()
         CALL MAKENET(1)
         CALL MINMXNS()
      ELSE IF (NWHAT .EQ. 3) THEN
         CALL curvilinearGRIDfromsplines()
      ELSE IF (NWHAT .EQ. 4) THEN
         CALL curvilinearGRIDinpolygon()
      ELSE IF (NWHAT .EQ. 5) THEN
         call CREATESAMPLESINPOLYGON()
      ELSE IF (NWHAT .EQ. 6) THEN
         CALL SAVENET()
         CALL Triangulatesamplestonetwork(1)
         netstat = NETSTAT_CELLS_DIRTY
      ELSE IF (NWHAT .EQ. 7) THEN
         CALL SAVENET()
         call gridtonet()
         call delgrd(key,0,0) ! no save, no delpol
      ELSE IF (NWHAT .EQ. 8) THEN
         CALL SAVENET()
         irerun = 1
         do while ( irerun.ne.0 )
            call ORTHOGONALISENET(irerun)
         end do
      ELSE IF (NWHAT .EQ. 9) THEN
      ELSE IF (NWHAT .EQ. 10) THEN
         ! call csmfinebnds2unstruc()
         call REFINEPOLYGON ()
      ELSE IF (NWHAT .EQ. 11) THEN
         CALL SAVENET()
         CALL REFINEQUADS()
      ELSE IF (NWHAT .EQ. 12) THEN
         ! CALL quadsTOTRI()
         CALL SAVENET()
         CALL REFINEQUADS_casulli()
      ELSE IF (NWHAT .EQ. 13) THEN
!         CALL RELINK()
!         CALL SAVENET()
!         CALL REFINECELLSANDFACES() !  REFINECELLSONLY()
         CALL SAVENET()
         CALL REFINECELLSANDFACES2() !  REFINECELLSONLY()
      ELSE IF (NWHAT .EQ. 14) THEN
         CALL SAVENET()
         call derefine_mesh(0d0, 0d0, .false.)
      ELSE IF (NWHAT .EQ. 15) THEN
         CALL SAVENET()
         CALL connectcurvilinearquadsddtype()
      ELSE IF (NWHAT .EQ. 16) THEN
         CALL SAVENET()
         CALL TIELDB()
         ! CALL CUTCELLS(1)
      ELSE IF (NWHAT .EQ. 17) THEN
         CALL COPYTRANS()
      ELSE IF (NWHAT .EQ. 18) THEN
         CALL SAVENET()
         call EXTERNALTRIANGLESTOOUTERQUADS()
      ELSE IF (NWHAT .EQ. 19) THEN
      ELSE IF (NWHAT .EQ. 20) THEN
         jareinitialize = 1
         ierr = flow_modelinit()
      ELSE IF (NWHAT .EQ. 21) THEN  ! Refresh net adm. (setnodadm + findcells)
         call findcells(100)        ! include folded cells
         call find1dcells()
!         call findcells(0)          ! do not include folded cells
         call delete_dry_points_and_areas()
         call makenetnodescoding()  ! killcell relies on node codes
      ELSE IF (NWHAT .EQ. 22) THEN
         call interpdivers(2) ! Network zk flow bathy
      ELSE IF (NWHAT .EQ. 23) THEN
         call interpdivers(interpolate_to) ! interpolate to interpolate_to in samples
         if (interpolate_to == 5) then ! plotlin?
            ndraw(36) = 1
         else if (interpolate_to == 1) then
            call setbobs()
         endif
         call setbobs()
      ELSE IF (NWHAT .EQ. 24) THEN
         call make1D2Dconnections()
      ELSE IF (NWHAT .EQ. 25) THEN

         !call flow_initfloodfill()
      ELSE IF (NWHAT .EQ. 26) THEN

      ELSE IF (NWHAT .EQ. 27) THEN
         call flow_spatietimestep()
      ELSE IF (NWHAT .EQ. 28) THEN
         CALL SAVENET()
         CALL MAKECOARSE2FINETRIANGLECONNECTIONCELLS()
      ELSE IF (NWHAT .EQ. 30) THEN
         CALL SAVENET()
         call fliplinks()
      ELSE IF (NWHAT .EQ. 31) THEN
         CALL SAVENET()
         call coarsen_mesh()
      ELSE IF (NWHAT .EQ. 32) THEN
         call savegrd()
!        delete grid
         mc = 0
         nc = 0
         ikey = 3
         call drawnu(ikey)
         call spline2curvi()
      ELSE IF (NWHAT .EQ. 33) THEN
         CALL SAVENET()
         call triangulate_quadsandmore(ja)
      ELSE IF (NWHAT .EQ. 34 ) THEN
         call detect_ridges(1)
      ELSE IF (NWHAT .EQ. 35 ) THEN
!
      ELSE IF (NWHAT .EQ. 36) THEN
!        intentionally left empty
      ELSE IF (NWHAT .EQ. 37 ) THEN
         call partition_to_idomain()
      ELSE IF (NWHAT .EQ. 38 ) THEN
         call make_dual_mesh()
      ELSE IF (NWHAT .EQ. 39) THEN
         call samdif()
      ELSE IF (NWHAT .EQ. 40) THEN
         call smooth_samples_from_GUI()
      ELSE IF (NWHAT .EQ. 41) THEN
         call maketrigrid()
      ENDIF
      KEY = 3
      NUM = 0
      CALL IMOUSECURSORSHAPE(1,'G')
      CALL IMouseCursorShow()
   ELSE IF (NUM .EQ. 3) THEN
   !     display opties
      CALL NDISPLAY(NWHAT,KEY)
      NUM = 0
   ELSE IF (NUM .EQ. 4) THEN
   !     dit zijn de edit nummers
   ELSE IF (NUM .EQ. 5) THEN
   !     addsubdel

      IF (NWHAT .EQ. 1) THEN
         CALL DELPOL()
      !  edit/modify polygon: netcell administration out of date
         netstat = NETSTAT_CELLS_DIRTY
      ELSE IF (NWHAT .EQ. 2) THEN
         CALL DELNET(KEY,0, 1)
      ELSE IF (NWHAT .EQ. 3) THEN
         CALL DELNET(KEY,2, 1)
      ELSE IF (NWHAT .EQ. 4) THEN
         CALL deleteSelectedSplines()
      ELSE IF (NWHAT .EQ. 5) THEN
         call delsam(1)
      ELSE IF (NWHAT .EQ. 6) THEN
         CALL ZEROLAN( KEY)
      ELSE IF (NWHAT .EQ. 7) THEN
         CALL DELgrd(key,1,0)
      ELSE IF (NWHAT .EQ. 8) THEN
         CALL deleteSelectedObservations()
      ELSE IF (NWHAT .EQ. 9) THEN
         CALL REMOVESMALLLINKS()
      ELSE IF (NWHAT .EQ.10) THEN
         CALL MERGENODESINPOLYGON()
      !  netcell administration out of date
         netstat = NETSTAT_CELLS_DIRTY
      ELSE IF (NWHAT .EQ. 12) THEN
         CALL zerowaterdepth()
      ELSE IF (NWHAT .EQ. 13) THEN
         call plusabs_flow(1)
      ELSE IF (NWHAT .EQ. 14) THEN              !****     **
         call plusabs_flow(2)
      ELSE IF (NWHAT .EQ. 15) THEN              !****     **
         ! call plusabs_flow(3)
         mnx  = mmax*nmax
         call PLUSABSD(XC,YC,ZC,mnx,KEY,zc)
      ELSE IF (NWHAT .EQ. 16) THEN              !****     **
         CALL SAVENET()
         CALL PLUSABSD(XK,YK,ZK,NUMK,KEY,XK)
      ELSE IF (NWHAT .EQ.17) THEN              !****     **
         CALL SAVENET()
         CALL PLUSABSD(XK,YK,ZK,NUMK,KEY,YK)
      ELSE IF (NWHAT .EQ.18) THEN              !****     **
         CALL SAVENET()
         CALL PLUSABSD(XK,YK,ZK,NUMK,KEY,ZK)
      ELSE IF (NWHAT .EQ.19) THEN              !****     **
         CALL PLUSABSD(Xs,Ys,Zs,NS,KEY,Zs)
      ELSE IF (NWHAT .EQ.20) THEN              !****     **
         CALL PLUSABSD(Xpl,Ypl,Zpl,NPL,KEY,Zpl)
      ELSE IF (NWHAT .EQ.21) THEN              !****     **
         CALL PLUSABSI(XK,YK,ZK,KN,NUMK,NUML,KEY,kn3typ)
      ELSE IF (NWHAT .EQ.23) THEN
        EXP(1)    = 'MENU                                    '
        EXP(2)    = 'COPY ... TO POLYGON                     '
        OPTION(1) = 'Copy land boundary  to polygon          '
        OPTION(2) = 'Copy net bounds     to polygon          '
        OPTION(3) = 'Copy cross sections to polygon          '
        OPTION(4) = 'Copy thin dams      to polygon          '
        OPTION(5) = 'Copy fixed weirs     to polygon         '
        OPTION(6) = 'Copy splines        to polygon (fine)   '
        OPTION(7) = 'Copy splines        to polygon          '
        OPTION(8) = 'Copy curvigrid bnds to polygon          '
        OPTION(9) = 'Copy 1D netw        to polygon          '
        OPTION(10)= 'Copy whole netw     to polygon          '
        OPTION(11)= 'Copy samples        to polygon          '

        MAXOPT    = 11
        NWHAT2    = 0
        CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
        if (nwhat2 == 1) then
            CALL COPYLDBTOPOL()
        else if (nwhat2 == 2) then
            call copynetboundstopol(0, 1, 0, 1)
        else if (nwhat2 == 3) then
            CALL copycrosssectionstopol()
        else if (nwhat2 == 4) then
            CALL copythindamstopol()
        else if (nwhat2 == 5) then
           CALL copyfixedweirstopol()
        else if (nwhat2 == 6) then
           CALL copysplinestofinepol(11)
        else if (nwhat2 == 7) then
           CALL copysplinestofinepol(1)
        else if (nwhat2 == 8) then
           CALL copycurvigridboundstopol()
        else if (nwhat2 == 9) then
           CALL regrid1D(0) ! 1D netw to pol
        else if (nwhat2 == 10) then
           CALL copynetwtopol()
        else if (nwhat2 == 11) then
           CALL copysamtopol()
        end if
        KEY = 3
      ELSE IF (NWHAT .EQ.24) THEN
        EXP(1)    = 'MENU                                    '
        EXP(2)    = 'COPY POLYGON TO ...                     '
        OPTION(1) = 'Copy polygon to land boundary           '
        OPTION(2) = 'Copy polygon to observation points      '
        OPTION(3) = 'Copy polygon to samples                 '
        OPTION(4) = 'Copy polygon to spline                  '
        OPTION(5) = 'Copy polygon to 1D network              '
        MAXOPT    = 5
        NWHAT2    = 0
        CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
        if (nwhat2 == 1) then
            CALL COPYPOLTOLDB()
        else if (nwhat2 == 2) then
            call copyPolygonToObservations()
        else if (nwhat2 == 3) then
            CALL copyPolygonToSamples()
        else if (nwhat2 == 4) then
            CALL copyPolToSpline()
        else if (nwhat2 == 5) then
            CALL copyPolTo1Dnet()
        end if
        KEY = 3
      ELSE IF (NWHAT .EQ.25) THEN
        EXP(1)    = 'MENU                                    '
        EXP(2)    = 'COPY ... TO SAMPLES                     '
        OPTION(1) = 'Copy polygon              to samples    '
        OPTION(2) = 'Copy values on network nodes to samples '
        OPTION(3) = 'Copy values on network links to samples '
        OPTION(4) = 'Copy values on network cells to samples '
        OPTION(5) = 'Copy values on flow nodes to samples    '
        OPTION(6) = 'Copy values on flow links to samples    '
        OPTION(7) = 'Swap samples and second samples         '
        OPTION(8) = 'Copy curvilinear grid     to samples    '
        OPTION(9) = 'Copy samples              to particles  '
        OPTION(10) = 'Copy dots                 to samples    '
        OPTION(11) = 'Copy samples              to dots       '
        OPTION(12) = 'Copy netnode Zk to samples              '

        MAXOPT    = 12
        NWHAT2    = 0
        CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
        if (nwhat2 == 1) then
         call copypolygontosamples()
        else if (nwhat2 == 2) then
         call copynetnodestosam(1)
        else if (nwhat2 == 3) then
         call copynetlinkstosam()
        else if (nwhat2 == 4) then
        ! call copyflowcellsizetosamples ! copyzktosam()
         call copycellstosam() !subroutine to display the scalar values calculated in the cells
        else if (nwhat2 == 5) then
         CALL copywaterlevelstosamples()
        else if (nwhat2 == 6) then
         CALL copyzlintosamples()
        else if (nwhat2 == 7) then
         call swapsamples()
        else if (nwhat2 == 8) then
         call copygridtosam()
        else if (nwhat2 == 10 ) then
         call copy_dots2sam()
        else if (nwhat2 == 11 ) then
         call copy_sam2dots()
        else if (nwhat2 == 12 ) then
         call copynetnodestosam(0)
        end if
        KEY = 3
      ELSE IF (NWHAT .EQ.26) THEN
         CALL copylandboundaryto1dnetwork()
      ELSE IF (NWHAT .EQ.27) THEN
         CALL copynetwtonetw()
      ELSE IF (NWHAT .EQ.28) THEN
         n12 = 1
         call cutcell_list(n12, '*.POL',5, 0)
      ELSE IF (NWHAT .EQ.29) THEN
         n12 = 3
         call findcells(0)
         call cutcell_list(n12, '*.cut',5, 0)
      ELSE IF (NWHAT .EQ.30) THEN
!        intentionally left empty
      ELSE IF (NWHAT .EQ.31) THEN
         call merge_polylines()
      ELSE IF (NWHAT .EQ.32) THEN
         call delnetzkabovezkuni()
      ELSE iF (NWHAT .EQ.33) THEN
         call del_badortholinks()
      ELSE iF (NWHAT .EQ.34) THEN
         call shift1Dnetnodestoduikers()
      ELSE iF (NWHAT .EQ.35) THEN
         call convert_cross_to_prof(md_ident)
      ELSE iF (NWHAT .EQ.36) THEN
         call connecthangingnodes()
      ELSE iF (NWHAT .EQ.37) THEN
         call removelinksofhangingnodes()
      ELSE iF (NWHAT .EQ.38) THEN
         call makezkbedlevels()
      ENDIF
      NUM  = 0
      KEY  = 3
      if ( jins.ne.1 ) then
         JINS    = 1                           !IMMEADIATELY SET BACK TO NORMAL BEHAVIOUR OR GO BESERK
         netstat = NETSTAT_CELLS_DIRTY
      end if
   ELSE IF (NUM .EQ. 6) THEN
   !     various
      IF (NWHAT .EQ. 1) THEN
         CALL STOPINT()
      ELSE IF (NWHAT .EQ. 2) THEN
         CALL SCHERM()
      ELSE IF (NWHAT .EQ. 3) THEN
         CALL CHANGEnetworkparameters()
      ELSE IF (NWHAT .EQ. 4) THEN
         CALL CHANGEorthoparameters()
      ELSE IF (NWHAT .EQ. 5) THEN
         CALL CHANGEGRIDPARAMETERS() ; KEY = 3
      ELSE IF (NWHAT .EQ. 6) THEN
         CALL CHANGEINTERPOLATIONPARAMETERS()
      ELSE IF (NWHAT .EQ. 7) THEN
         CALL MAPPROJECTIONS(-1,JA) ! -1, INTERACTIEF
         if (ja == 1) then
            call minmxns()
            key = 3
         endif
      ELSE IF  (NWHAT .EQ. 8) THEN
         CALL CHANGETIMEPARAMETERS()
      ELSE IF (NWHAT .EQ. 9) THEN
         Call changegeometryparameters()
      ELSE IF (NWHAT .EQ. 10) THEN
         CALL CHANGEPHYSICALPARAMETERS()
      ELSE IF (NWHAT .EQ. 11) THEN
         CALL CHANGENUMERICALPARAMETERS()
      ELSE IF (NWHAT .EQ. 12) THEN
         CALL CHANGENUMERICALPARAMETERS2()
      ELSE IF (NWHAT .EQ. 13) THEN
         CALL CHANGENUMERICALPARAMETERS3()
      ELSE IF (NWHAT .EQ. 14) THEN
         CALL CHANGENUMERICALPARAMETERS4()
      ELSE IF (NWHAT .EQ. 15) THEN
         CALL CHANGEcolournumbers() ; KEY = 3
      ENDIF
      NUM = 0
   ENDIF

   RETURN
   END SUBROUTINE CHOICES
