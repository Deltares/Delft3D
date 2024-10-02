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
module m_choices
use m_plusabs_flow
use m_plusabsi
use m_plusabsd
use m_nfiles
use m_ndisplay
use m_menuv3
use m_copywaterlevelstosamples
use m_copynetwtonetw
use m_copynetnodestosam
use m_copynetlinkstosam
use m_copygridtosam
use m_copyzlintosamples


implicit none

contains

   subroutine CHOICES(NUM, NWHAT, KEY)
      use m_changetimeparameters
      use m_changephysicalparameters
      use m_changeorthoparameters
      use m_changenumericalparameters4
      use m_changenumericalparameters3
      use m_changenumericalparameters2
      use m_changenumericalparameters
      use m_changenetworkparameters
      use m_changeinterpolationparameters
      use m_changegridparameters
      use m_changegeometryparameters
      use m_changecolournumbers
      use m_netw
      use m_samples
      use m_grid
      use M_MISSING
      use unstruc_display
      use m_polygon
      use m_partitioninfo
      use m_ec_interpolationsettings
      use gridoperations
      use m_oned_functions, only: convert_cross_to_prof
      use unstruc_model, only: md_ident
      use m_drawthis
      use m_delpol
      use m_delsam
      use m_copynetboundstopol
      use m_makenetnodescoding
      use m_set_nod_adm
      use m_draw_nu
      use m_set_bobs
      use m_interpdivers

      implicit none
      integer :: ja, n12, ikey, mnx
      integer :: NUM, NWHAT, KEY, nwhat2
      integer :: irerun ! orthogonalisenet: rerun
      integer :: maxopt, ierr
      integer, parameter :: MAXOP = 64
      character * 40 OPTION(MAXOP), exp(MAXOP)
      integer, external :: flow_modelinit

      if (netstat /= NETSTAT_OK) call setnodadm(0)

      if (NUM == 1) then
         !     load en save files
         call NFILES(NUM, NWHAT, KEY)
      else if (NUM == 2) then
         !     operations
!      if ( jins.ne.1 ) then  ! SPvdP: temporarily disabled
!         jins = 1
!         netstat = NETSTAT_CELLS_DIRTY
!      end if
         if (NWHAT == 1) then
            call RESTORE()
         else if (NWHAT == 2) then
            call SAVENET()
            call MAKENET(1)
            call MINMXNS()
         else if (NWHAT == 3) then
            call curvilinearGRIDfromsplines()
         else if (NWHAT == 4) then
            call curvilinearGRIDinpolygon()
         else if (NWHAT == 5) then
            call CREATESAMPLESINPOLYGON()
         else if (NWHAT == 6) then
            call SAVENET()
            call Triangulatesamplestonetwork(1)
            netstat = NETSTAT_CELLS_DIRTY
         else if (NWHAT == 7) then
            call SAVENET()
            call gridtonet()
            call delgrd(key, 0, 0) ! no save, no delpol
         else if (NWHAT == 8) then
            call SAVENET()
            irerun = 1
            do while (irerun /= 0)
               call ORTHOGONALISENET(irerun)
            end do
         else if (NWHAT == 9) then
         else if (NWHAT == 10) then
            ! call csmfinebnds2unstruc()
            call REFINEPOLYGON()
         else if (NWHAT == 11) then
            call SAVENET()
            call REFINEQUADS()
         else if (NWHAT == 12) then
            ! CALL quadsTOTRI()
            call SAVENET()
            call REFINEQUADS_casulli()
         else if (NWHAT == 13) then
!         CALL RELINK()
!         CALL SAVENET()
!         CALL REFINECELLSANDFACES() !  REFINECELLSONLY()
            call SAVENET()
            call REFINECELLSANDFACES2() !  REFINECELLSONLY()
         else if (NWHAT == 14) then
            call SAVENET()
            call derefine_mesh(0d0, 0d0, .false.)
         else if (NWHAT == 15) then
            call SAVENET()
            call connectcurvilinearquadsddtype()
         else if (NWHAT == 16) then
            call SAVENET()
            call TIELDB()
            ! CALL CUTCELLS(1)
         else if (NWHAT == 17) then
            call COPYTRANS()
         else if (NWHAT == 18) then
            call SAVENET()
            call EXTERNALTRIANGLESTOOUTERQUADS()
         else if (NWHAT == 19) then
         else if (NWHAT == 20) then
            jareinitialize = 1
            ierr = flow_modelinit()
         else if (NWHAT == 21) then ! Refresh net adm. (setnodadm + findcells)
            call findcells(100) ! include folded cells
            call find1dcells()
!         call findcells(0)          ! do not include folded cells
            call delete_dry_points_and_areas()
            call makenetnodescoding() ! killcell relies on node codes
         else if (NWHAT == 22) then
            call interpdivers(2) ! Network zk flow bathy
         else if (NWHAT == 23) then
            call interpdivers(interpolate_to) ! interpolate to interpolate_to in samples
            if (interpolate_to == 5) then ! plotlin?
               ndraw(36) = 1
            else if (interpolate_to == 1) then
               call setbobs()
            end if
            call setbobs()
         else if (NWHAT == 24) then
            call make1D2Dconnections()
         else if (NWHAT == 25) then

            !call flow_initfloodfill()
         else if (NWHAT == 26) then

         else if (NWHAT == 27) then
            call flow_spatietimestep()
         else if (NWHAT == 28) then
            call SAVENET()
            call MAKECOARSE2FINETRIANGLECONNECTIONCELLS()
         else if (NWHAT == 30) then
            call SAVENET()
            call fliplinks()
         else if (NWHAT == 31) then
            call SAVENET()
            call coarsen_mesh()
         else if (NWHAT == 32) then
            call savegrd()
!        delete grid
            mc = 0
            nc = 0
            ikey = 3
            call drawnu(ikey)
            call spline2curvi()
         else if (NWHAT == 33) then
            call SAVENET()
            call triangulate_quadsandmore(ja)
         else if (NWHAT == 34) then
            call detect_ridges(1)
         else if (NWHAT == 35) then
!
         else if (NWHAT == 36) then
!        intentionally left empty
         else if (NWHAT == 37) then
            call partition_to_idomain()
         else if (NWHAT == 38) then
            call make_dual_mesh()
         else if (NWHAT == 39) then
            call samdif()
         else if (NWHAT == 40) then
            call smooth_samples_from_GUI()
         else if (NWHAT == 41) then
            call maketrigrid()
         end if
         KEY = 3
         NUM = 0
         call IMOUSECURSORSHAPE(1, 'G')
         call IMouseCursorShow()
      else if (NUM == 3) then
         !     display opties
         call NDISPLAY(NWHAT, KEY)
         NUM = 0
      else if (NUM == 4) then
         !     dit zijn de edit nummers
      else if (NUM == 5) then
         !     addsubdel

         if (NWHAT == 1) then
            call DELPOL()
            !  edit/modify polygon: netcell administration out of date
            netstat = NETSTAT_CELLS_DIRTY
         else if (NWHAT == 2) then
            call DELNET(KEY, 0, 1)
         else if (NWHAT == 3) then
            call DELNET(KEY, 2, 1)
         else if (NWHAT == 4) then
            call deleteSelectedSplines()
         else if (NWHAT == 5) then
            call delsam(1)
         else if (NWHAT == 6) then
            call ZEROLAN(KEY)
         else if (NWHAT == 7) then
            call DELgrd(key, 1, 0)
         else if (NWHAT == 8) then
            call deleteSelectedObservations()
         else if (NWHAT == 9) then
            call REMOVESMALLLINKS()
         else if (NWHAT == 10) then
            call MERGENODESINPOLYGON()
            !  netcell administration out of date
            netstat = NETSTAT_CELLS_DIRTY
         else if (NWHAT == 12) then
            call zerowaterdepth()
         else if (NWHAT == 13) then
            call plusabs_flow(1)
         else if (NWHAT == 14) then !****     **
            call plusabs_flow(2)
         else if (NWHAT == 15) then !****     **
            ! call plusabs_flow(3)
            mnx = mmax * nmax
            call PLUSABSD(XC, YC, ZC, mnx, KEY, zc)
         else if (NWHAT == 16) then !****     **
            call SAVENET()
            call PLUSABSD(XK, YK, ZK, NUMK, KEY, XK)
         else if (NWHAT == 17) then !****     **
            call SAVENET()
            call PLUSABSD(XK, YK, ZK, NUMK, KEY, YK)
         else if (NWHAT == 18) then !****     **
            call SAVENET()
            call PLUSABSD(XK, YK, ZK, NUMK, KEY, ZK)
         else if (NWHAT == 19) then !****     **
            call PLUSABSD(Xs, Ys, Zs, NS, KEY, Zs)
         else if (NWHAT == 20) then !****     **
            call PLUSABSD(Xpl, Ypl, Zpl, NPL, KEY, Zpl)
         else if (NWHAT == 21) then !****     **
            call PLUSABSI(XK, YK, ZK, KN, NUMK, NUML, KEY, kn3typ)
         else if (NWHAT == 23) then
            exp(1) = 'MENU                                    '
            exp(2) = 'COPY ... TO POLYGON                     '
            OPTION(1) = 'Copy land boundary  to polygon          '
            OPTION(2) = 'Copy net bounds     to polygon          '
            OPTION(3) = 'Copy cross sections to polygon          '
            OPTION(4) = 'Copy thin dams      to polygon          '
            OPTION(5) = 'Copy fixed weirs     to polygon         '
            OPTION(6) = 'Copy splines        to polygon (fine)   '
            OPTION(7) = 'Copy splines        to polygon          '
            OPTION(8) = 'Copy curvigrid bnds to polygon          '
            OPTION(9) = 'Copy 1D netw        to polygon          '
            OPTION(10) = 'Copy whole netw     to polygon          '
            OPTION(11) = 'Copy samples        to polygon          '

            MAXOPT = 11
            NWHAT2 = 0
            call MENUV3(NWHAT2, OPTION, MAXOPT)
            if (nwhat2 == 1) then
               call COPYLDBTOPOL()
            else if (nwhat2 == 2) then
               call copynetboundstopol(0, 1, 0, 1)
            else if (nwhat2 == 3) then
               call copycrosssectionstopol()
            else if (nwhat2 == 4) then
               call copythindamstopol()
            else if (nwhat2 == 5) then
               call copyfixedweirstopol()
            else if (nwhat2 == 6) then
               call copysplinestofinepol(11)
            else if (nwhat2 == 7) then
               call copysplinestofinepol(1)
            else if (nwhat2 == 8) then
               call copycurvigridboundstopol()
            else if (nwhat2 == 9) then
               call regrid1D(0) ! 1D netw to pol
            else if (nwhat2 == 10) then
               call copynetwtopol()
            else if (nwhat2 == 11) then
               call copysamtopol()
            end if
            KEY = 3
         else if (NWHAT == 24) then
            exp(1) = 'MENU                                    '
            exp(2) = 'COPY POLYGON TO ...                     '
            OPTION(1) = 'Copy polygon to land boundary           '
            OPTION(2) = 'Copy polygon to observation points      '
            OPTION(3) = 'Copy polygon to samples                 '
            OPTION(4) = 'Copy polygon to spline                  '
            OPTION(5) = 'Copy polygon to 1D network              '
            MAXOPT = 5
            NWHAT2 = 0
            call MENUV3(NWHAT2, OPTION, MAXOPT)
            if (nwhat2 == 1) then
               call COPYPOLTOLDB()
            else if (nwhat2 == 2) then
               call copyPolygonToObservations()
            else if (nwhat2 == 3) then
               call copyPolygonToSamples()
            else if (nwhat2 == 4) then
               call copyPolToSpline()
            else if (nwhat2 == 5) then
               call copyPolTo1Dnet()
            end if
            KEY = 3
         else if (NWHAT == 25) then
            exp(1) = 'MENU                                    '
            exp(2) = 'COPY ... TO SAMPLES                     '
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

            MAXOPT = 12
            NWHAT2 = 0
            call MENUV3(NWHAT2, OPTION, MAXOPT)
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
               call copywaterlevelstosamples()
            else if (nwhat2 == 6) then
               call copyzlintosamples()
            else if (nwhat2 == 7) then
               call swapsamples()
            else if (nwhat2 == 8) then
               call copygridtosam()
            else if (nwhat2 == 10) then
               call copy_dots2sam()
            else if (nwhat2 == 11) then
               call copy_sam2dots()
            else if (nwhat2 == 12) then
               call copynetnodestosam(0)
            end if
            KEY = 3
         else if (NWHAT == 26) then
            call copylandboundaryto1dnetwork()
         else if (NWHAT == 27) then
            call copynetwtonetw()
         else if (NWHAT == 28) then
            n12 = 1
            call cutcell_list(n12, 0)
         else if (NWHAT == 29) then
            n12 = 3
            call findcells(0)
            call cutcell_list(n12, 0)
         else if (NWHAT == 30) then
!        intentionally left empty
         else if (NWHAT == 31) then
            call merge_polylines()
         else if (NWHAT == 32) then
            call delnetzkabovezkuni()
         else if (NWHAT == 33) then
            call del_badortholinks()
         else if (NWHAT == 34) then
            call shift1Dnetnodestoduikers()
         else if (NWHAT == 35) then
            call convert_cross_to_prof(md_ident)
         else if (NWHAT == 36) then
            call connecthangingnodes()
         else if (NWHAT == 37) then
            call removelinksofhangingnodes()
         else if (NWHAT == 38) then
            call makezkbedlevels()
         end if
         NUM = 0
         KEY = 3
         if (jins /= 1) then
            JINS = 1 !IMMEADIATELY SET BACK TO NORMAL BEHAVIOUR OR GO BESERK
            netstat = NETSTAT_CELLS_DIRTY
         end if
      else if (NUM == 6) then
         !     various
         if (NWHAT == 1) then
            call STOPINT()
         else if (NWHAT == 2) then
            call SCHERM()
         else if (NWHAT == 3) then
            call CHANGEnetworkparameters()
         else if (NWHAT == 4) then
            call CHANGEorthoparameters()
         else if (NWHAT == 5) then
            call CHANGEGRIDPARAMETERS(); KEY = 3
         else if (NWHAT == 6) then
            call CHANGEINTERPOLATIONPARAMETERS()
         else if (NWHAT == 7) then
            call MAPPROJECTIONS(-1, JA) ! -1, INTERACTIEF
            if (ja == 1) then
               call minmxns()
               key = 3
            end if
         else if (NWHAT == 8) then
            call CHANGETIMEPARAMETERS()
         else if (NWHAT == 9) then
            call changegeometryparameters()
         else if (NWHAT == 10) then
            call CHANGEPHYSICALPARAMETERS()
         else if (NWHAT == 11) then
            call CHANGENUMERICALPARAMETERS()
         else if (NWHAT == 12) then
            call CHANGENUMERICALPARAMETERS2()
         else if (NWHAT == 13) then
            call CHANGENUMERICALPARAMETERS3()
         else if (NWHAT == 14) then
            call CHANGENUMERICALPARAMETERS4()
         else if (NWHAT == 15) then
            call CHANGEcolournumbers(); KEY = 3
         end if
         NUM = 0
      end if

      return
   end subroutine CHOICES

end module m_choices
