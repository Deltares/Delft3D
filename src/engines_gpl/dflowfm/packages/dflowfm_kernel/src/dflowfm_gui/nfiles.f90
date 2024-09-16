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

   subroutine NFILES(NUM, NWHAT, KEY)
!  grid lijst
!  NUM = 0, GELUKT, NUM = 1, NIET GELUKT
      use m_netw
      use m_grid
      use m_observations
      use m_monitoring_crosssections
      use m_thindams
      use M_SPLINES, notinusenump => nump
      use unstruc_model
      use m_samples
      use m_flowgeom
      use unstruc_display
      use m_flowparameters
      use unstruc_files, only: defaultFilename, close_all_files
      use unstruc_model
      use unstruc_netcdf
      use unstruc_opengis
      use io_openfoam
      use m_partitioninfo
      use m_sferic
      use m_flowtimes
      use dfm_error
      use gridoperations
      use string_module, only: strcmpi
      use m_setucxcuy_leastsquare, only: reconst2nd
      use m_drawthis
      use m_qnerror
      use m_wrinet
      use m_delpol
      use m_reapol
      use m_delsam
      use m_getint
      use m_wripol
      use m_wrisam
      use m_reasam

      implicit none
      integer :: NUM, NWHAT, KEY
      integer :: ja, ierr
      integer :: mlan
      integer :: midp
      integer :: mtek
      integer :: i, ierror
   integer :: ipli
      logical :: jawel
      logical, external :: read_samples_from_geotiff

      interface
         subroutine realan(mlan, antot)
            integer, intent(inout) :: mlan
            integer, intent(inout), optional :: antot
         end subroutine realan
      end interface

      character FILNAM * 86

      KEY = 0

      if (NWHAT == 1) then
         FILNAM = '*.mdu'
         MLAN = 0
         call FILEMENU(MLAN, FILNAM, ierror)
         if (ierror == -2) then
            call qnerror('file', filnam, 'not found ')
            NUM = 1
         else if (ierror == -1) then ! Cancel
            NUM = 1
         else
            call doclose(mlan) ! TODO: change... [AvD]
            call inidat() ! TODO: call reset_display_settings() +  call dfm_reset_globaldata()
            call resetFullFlowModel()
            call loadModel(filnam)
            call minmxns()
            ! Check for presence of associated display presets
            inquire (file=trim(md_ident)//'.cfg', exist=jawel)
            if (jawel) then
               call load_displaysettings(trim(md_ident)//'.cfg')
            else
               inquire (file='unstruc.cfg', exist=jawel)
               if (jawel) then
                  call load_displaysettings('unstruc.cfg')
               end if
            end if

            NDRAW(2) = 1
            KEY = 3
            NUM = 0
         end if
      else if (NWHAT == 2) then
         FILNAM = '*_net.nc'
         MLAN = 0
         call FILEMENU(MLAN, FILNAM, ierror)
         if (ierror == -2) then
            call qnerror('file', filnam, 'not found ')
            NUM = 1
         else if (ierror == -1) then
            NUM = 1
         else
            call doclose(mlan) ! TODO: change... [AvD]
            call loadNetwork(filnam, JA, 0)
            if (JA == 0) then
               call resetFlow()
               nump = 0 ! Reset cell data
               call MESSAGE('YOU LOADED ', filnam, ' ')
               call MINMXNS()
               NDRAW(2) = 1
               KEY = 3
               NUM = 0
               md_netfile = ' '
               md_netfile = trim(filnam)
            else
               call qnerror('NO NET LOADED', ' ', ' ')
            end if
         end if
      else if (NWHAT == 3) then
         FILNAM = '*_net.nc'
         MLAN = 0
         call FILEMENU(MLAN, FILNAM, ierror)
         if (ierror == -2) then
            call qnerror('file', filnam, 'not found ')
            NUM = 1
         else if (ierror == -1) then
            NUM = 1
         else
            if (index(FILNAM, '.jan') > 0) then
               call REAJANET(Mlan, JA, 1)
            else if (index(FILNAM, '.adc') > 0) then
               call READADCIRCNET(Mlan, JA, 1)
            else
               call doclose(mlan) ! TODO: change... [AvD]
               call loadNetwork(filnam, JA, 1)
            end if

            if (JA == 0) then
               call MESSAGE('YOU LOADED ', filnam, ' ')
               call MINMXNS()
               NDRAW(2) = 1
               KEY = 3
               NUM = 0
               md_netfile = ' '
               md_netfile = trim(filnam)
            else
               call qnerror('NO NET LOADED', ' ', ' ')
            end if
         end if
      else if (NWHAT == 4) then
         FILNAM = '*.grd'
         MLAN = 0
         call FILEMENU(MLAN, FILNAM, ierror)
         if (ierror == -2) then
            call qnerror('file', filnam, 'not found ')
            NUM = 1
         else if (ierror == -1) then
            NUM = 1
         else
            call REAgrid(MLAN, FILNAM, ja) ! DOORLADEN
            if (JA >= 1) then
               call MESSAGE('YOU LOADED ', filnam, ' ')
               call MINMXNS()
               NDRAW(2) = 1
               KEY = 3
               NUM = 0
            else
               call QNERROR('PREMATURE END OF FILE', FILNAM, ' ')
            end if
         end if
      else if (NWHAT == 5) then
         FILNAM = '*.asc'
         MLAN = 0
         call FILEMENU(MLAN, FILNAM, ierror)
         if (ierror == -2) then
            call qnerror('file', filnam, 'not found ')
            NUM = 1
         else if (ierror == -1) then
            NUM = 1
         else
            call readarcinfo(MLAN, ja) ! DOORLADEN
            if (JA >= 1) then
               call MESSAGE('YOU LOADED ', filnam, ' ')
               call MINMXNS()
               NDRAW(2) = 1
               KEY = 3
               NUM = 0
            else
               call QNERROR('PREMATURE END OF FILE', FILNAM, ' ')
            end if
         end if
      else if (NWHAT == 6) then
         FILNAM = '*.pol,*.pli,*.pliz'
         MLAN = 0
         call FILEMENU(MLAN, FILNAM, ierror)
         if (ierror == -2) then
            call qnerror('file', filnam, 'not found ')
            NUM = 1
         else if (ierror == -1) then
            NUM = 1
         else
            call REAPOL(MLAN, 0)
            if (NPL > 0) then
               call MESSAGE('YOU LOADED ', filnam, ' ')
               call MINMXNS()
               KEY = 3
               NUM = 0

               !        read polygon: netcell administration out of date
               netstat = NETSTAT_CELLS_DIRTY
            else
               call qnerror('file', filnam, 'not found ')
            end if
         end if
      else if (NWHAT == 7) then
         FILNAM = '*.spl'
         MLAN = 0
         call FILEMENU(MLAN, FILNAM, ierror)
         if (MLAN /= 0) then
            call readSplines(mlan)
            if (mcs > 0) then
               call MESSAGE('You Opened File ', FILNAM, ' ')
               call MINMXNS()
               NUM = 0
               NDRAW(15) = 1
               KEY = 3
            end if
         end if
      else if (NWHAT == 8) then
         FILNAM = '*.ldb'
         MLAN = 0
         call FILEMENU(MLAN, FILNAM, ierror)
         if (ierror == -2) then
            call qnerror('file', filnam, 'not found ')
            NUM = 1
         else if (ierror == -1) then
            NUM = 1
         else
            i = len_trim(filnam)
            if (i > 3) then
               if (filnam(i - 2:i) == '.nc') then
                  call doclose(mlan)
                  call read_land_boundary_netcdf(filnam)
                  return
               end if
            end if

            call REALAN(MLAN)

            if (MXLAN > 0) then
               call MESSAGE('YOU LOADED ', filnam, ' ')
               call MINMXNS()
               NDRAW(3) = 1
               KEY = 3
               NUM = 0
               md_ldbfile = ' '
               md_ldbfile = filnam
            else
               call qnerror('MXLAN = 0', ' ', ' ')
            end if
         end if
      else if (NWHAT == 9 .or. NWHAT == 10) then
         FILNAM = '*_obs.xyn'
         MLAN = 0
         call FILEMENU(MLAN, FILNAM, ierror)
         if (ierror == -2) then
            call qnerror('file', filnam, 'not found ')
            NUM = 1
         else if (ierror == -1) then
            NUM = 1
         else
            ja = 0
            key = 3
            call doclose(mlan) ! Ugly, but loadObservations reads by filename, not filepointer [AvD]
            if (NWHAT == 10) then
               ja = 1 ! doorladen
            else
               ja = 0
            end if
            call loadObservations(filnam, ja)
            call MESSAGE('YOU LOADED ', filnam, ' ')
            call MINMXNS()
            md_obsfile = ' '
            md_obsfile = filnam
         end if
      else if (NWHAT == 11 .or. NWHAT == 12) then
         FILNAM = '*_crs.pli'
         MLAN = 0
         call FILEMENU(MLAN, FILNAM, ierror)
         if (ierror == -2) then
            call qnerror('file', filnam, 'not found ')
            NUM = 1
         else if (ierror == -1) then
            NUM = 1
         else
            ja = 0
            key = 3
            if (NWHAT == 12) then
               ja = 1 ! doorladen
            else
               ja = 0
            end if
         ipli=0
         CALL reapol_nampli(MLAN, ja,1,ipli) ! Read pol/pli as crs
            call pol_to_crosssections(xpl, ypl, npl, names=nampli)
            if (NPL > 0) call delpol()
            call MESSAGE('YOU LOADED ', filnam, ' ')
            call MINMXNS()
            md_crsfile = ' '
            md_crsfile = filnam
         end if
      else if (NWHAT == 13 .or. NWHAT == 14) then
         FILNAM = '*_thd.pli'
         MLAN = 0
         call FILEMENU(MLAN, FILNAM, ierror)
         if (ierror == -2) then
            call qnerror('file', filnam, 'not found ')
            NUM = 1
         else if (ierror == -1) then
            NUM = 1
         else
            ja = 0
            key = 3
            if (NWHAT == 14) then
               ja = 1 ! doorladen
            else
               ja = 0
            end if
            call REAPOL(MLAN, ja) ! Read pol/pli as thin dam-type crs
            call pol_to_thindams(xpl, ypl, npl)
            call MESSAGE('YOU LOADED ', filnam, ' ')
            call MINMXNS()
            md_thdfile = ' '
            md_thdfile = filnam
         end if
      else if (NWHAT == 15) then
         FILNAM = '*.xyz,*.dem,*.asc,*.tif*'
         MLAN = 0
         call FILEMENU(MLAN, FILNAM, ierror)
         if (ierror == -2) then
            call qnerror('file', filnam, 'not found ')
            NUM = 1
         else if (ierror == -1) then
            NUM = 1
         else
            ja = 0
            key = 3
            i = len_trim(filnam)
            if (i > 3) then
               if (strcmpi(filnam(i - 3:i), '.dem')) then
                  call doclose(mlan)
                  call read_samples_from_dem(trim(filnam), ja)
               else if (strcmpi(filnam(i - 3:i), '.asc')) then
                  call doclose(mlan)

!               delete all samples, regardless of selecting polygon
                  call savepol()
                  call delpol()
                  call savesam()
                  call delsam(0)
                  call restorepol()

                  call read_samples_from_arcinfo(trim(filnam), ja, 1) ! reaasc
               else if (strcmpi(filnam(i - 3:i), '.tif') &
                        .or. strcmpi(filnam(max(1, i - 4):i), '.tiff')) then
                  call doclose(mlan)
                  success = read_samples_from_geotiff(filnam)
               else if (strcmpi(filnam(i - 3:i), '.xyz')) then
                  call reasam(MLAN, ja) ! DOORLADEN
               end if
            else
               call reasam(MLAN, ja) ! DOORLADEN
            end if
            call MESSAGE('YOU LOADED ', filnam, ' ')
            call MINMXNS()
         end if
      else if (NWHAT == 16) then
         if (ndx == 0 .or. lnx == 0) then
            call qnerror('First reinitialise flow model, current dimensions are 0', ' ', ' ')
            return
         end if
         if (ibedlevtyp == 1) then
            FILNAM = '*.xybl'
         else if (ibedlevtyp == 2) then
            FILNAM = '*.xyblu'
         else
            call qnerror('Loading cell bottom levels bl (ibedlevtyp=1) or flow link bottom levels blu (ibedlevtyp=2)', ' ', ' ')
            call qnerror('Change parameter ibedlevtyp in Various, Change Geometry Parameters', ' ', ' ')
            return
         end if
         MLAN = 0
         call FILEMENU(MLAN, FILNAM, ierror)
         if (ierror == -2) then
            call qnerror('file', filnam, 'not found ')
            NUM = 1
         else if (ierror == -1) then
            NUM = 1
         else
            if (ibedlevtyp == 1) then
               call reabl(MLAN)
            else if (ibedlevtyp == 2) then
               call reablu(MLAN)
            end if
            call MESSAGE('YOU LOADED ', filnam, ' ')
            ! CALL MINMXNS()
         end if
      else if (NWHAT == 17) then
         FILNAM = '*_rst.nc'
         MLAN = 0
         call FILEMENU(MLAN, FILNAM, ierror)
         if (ierror == -2) then
            call qnerror('file', filnam, 'not found ')
            NUM = 1
         else if (ierror == -1) then
            NUM = 1
         else
            i = len_trim(filnam)
            if (filnam(i - 6:i) == '_rst.nc' .or. filnam(i - 6:i) == '_RST.NC') then
               call doclose(mlan) ! TODO: change... [AvD]
               call read_restart_from_map(FILNAM, ierr)
               if (ierr /= DFM_NOERR) then
                  call qnerror('Error occurs when reading the restart file.', ' ', ' ')
                  JA = 0
               else
                  JA = 1
               end if
               if (iperot == -1) then
                  call reconst2nd()
               end if
               call setucxucyucxuucyunew() ! reconstruct cell center velocities
            else
               call rearst(MLAN, JA)
            end if
            !else if (filnam(i-6:i) == '_map.nc' .or. filnam(i-6:i) == '_MAP.NC') then
            !   call doclose(MLAN)
            !   call read_restart_from_map(FILNAM,JA)
            !   ! TODO: AvD: No flow_setstarttime here?

            if (JA == 1) then
               call MESSAGE('YOU LOADED ', filnam, ' ')
            else
               call qnerror('NO RESTART LOADED', ' ', ' ')
            end if
            ! CALL MINMXNS()
         end if
      else if (NWHAT == 18) then
         NUM = 0
         FILNAM = '*.bmp'
         MIDP = 0
         call FILEMENU(MIDP, FILNAM, ierror)
         if (ierror /= 0) then
            NDRAW(26) = 0
         else if (MIDP /= 0) then
            call DOCLOSE(MIDP)
            call LOADBITMAP(FILNAM)
            call MESSAGE('YOU LOADED ', filnam, ' ')
            call MINMXNS()
         end if
         KEY = 3
      else if (NWHAT == 20) then
         FILNAM = '*.mdu'
         MTEK = 1
         call FILEMENU(MTEK, FILNAM, ierror)
         if (ierror /= 0) then
            NUM = 1
         else
            call doclose(mtek)
            call writeMDUFile(filnam, ja)
            call MESSAGE('YOU SAVED ', filnam, ' ')
            NUM = 0
         end if
      else if (NWHAT == 21 .or. NWHAT == 22 .or. NWHAT == 24) then
         if (NUMK == 0) then
            call QNERROR('NO NET TO SAVE', ' ', ' ')
            NUM = 0
         else
            if (nwhat == 21 .or. nwhat == 22) then
               FILNAM = '*_net.nc'
            else if (nwhat == 24) then
               FILNAM = '*_net.plt'
            end if

            MTEK = 1
            call FILEMENU(MTEK, FILNAM, ierror)
            if (ierror /= 0) then
               NUM = 1
            else
               call doclose(mtek)
               if (nwhat == 21) then
                  if (index(filnam, '.net') > 0) then
                     call NEWFIL(MTEK, filnam); call WRINET(MTEK)
                  else
                     call unc_write_net(filnam, janetcell=0, janetbnd=0)
                  end if
               else if (nwhat == 22) then ! _net.nc with extra cell info (for example necessary for Baseline/Bas2FM input)
                  !origial call unc_write_net(filnam, janetcell = 1, janetbnd = 0)
                  call unc_write_net('UG'//filnam, janetcell=1, janetbnd=0, iconventions=UNC_CONV_UGRID)
                  call unc_write_net(filnam, janetcell=1, janetbnd=1) ! wrinet
               else if (nwhat == 24) then
                  call ini_tecplot()
                  call wrinet_tecplot(filnam)
               end if
               call MESSAGE('YOU SAVED ', filnam, ' ')
               md_netfile = ' '
               md_netfile = filnam

               NUM = 0
            end if
         end if
      else if (NWHAT == 23) then
         if (NUMK == 0) then
            call QNERROR('NO NET TO SAVE', ' ', ' ')
            NUM = 0
         else
            !call foam_write_polymesh('testfoam')
            FILNAM = '*.kml'
            MTEK = 1
            ja = 1
            if (jsferic /= 1) then
               call confrm('Model is not in spherical coordinates. Proceed? (not recommended)', ja)
            end if
            if (ja == 1) then
               call change_kml_parameters(ja)
            else
               ja = 1 ! Hereafter, 1 means 'no/cancelled'
            end if
            if (ja == 0) then ! 0: NOT cancelled
               call FILEMENU(MTEK, FILNAM, ierror)
               if (ierror /= 0) then
                  NUM = 1
               else
                  call doclose(mtek)
                  call kml_write_net(filnam)
                  call MESSAGE('YOU SAVED ', filnam, ' ')
                  NUM = 0
               end if
            end if
         end if
      else if (NWHAT == 24) then
      else if (NWHAT == 25) then
         if (MC == 0 .or. NC == 0) then
            call QNERROR('NO GRID TO SAVE', ' ', ' ')
            NUM = 0
         else
            FILNAM = '*.grd'
            MTEK = 1
            call FILEMENU(MTEK, FILNAM, ierror)
            if (ierror /= 0) then
               NUM = 1
            else
               call wrirgf(mtek, filnam)
               call MESSAGE('YOU SAVED ', filnam, ' ')
               NUM = 0
            end if
         end if
      else if (NWHAT == 26) then
         if (NPL == 0) then
            call QNERROR('THERE IS NO POLYGON TO SAVE', ' ', ' ')
            NUM = 0
         else
            FILNAM = '*.pol,*.pli,*.pliz'
            MIDP = 1
            call FILEMENU(MIDP, FILNAM, ierror)
            if (ierror /= 0) then
               NUM = 1
            else
               call WRIPOL(MIDP)
               if (index(Filnam, 'crs') == 0 .and. index(Filnam, 'CRS') == 0 .and. index(Filnam, 'vlay') == 0 .and. index(Filnam, 'VLAY') == 0) then
                  call wricmps(filnam)
               end if
               call MESSAGE('YOU SAVED ', filnam, ' ')
               NUM = 0
               md_plifile = ' '; md_plifile = filnam
            end if
         end if
      else if (NWHAT == 27) then
         if (mcs == 0) then
            call QNERROR('There Are No Splines to SAVE', ' ', ' ')
         else
            FILNAM = '*.spl'
            MLAN = 1
            call FILEMENU(MLAN, FILNAM, ierror)
            if (ierror == 0) then
               call writeSplines(MLAN)
               call MESSAGE('You Saved File ', FILNAM, ' ')
            end if
         end if
      else if (NWHAT == 28) then
         if (MXLAN == 0) then
            call QNERROR('THERE IS NO LANDBOUNDARY TO SAVE', ' ', ' ')
            NUM = 0
         else
            FILNAM = '*.ldb'
            MIDP = 1
            call FILEMENU(MIDP, FILNAM, ierror)
            if (ierror /= 0) then
               NUM = 1
            else
               call WRILAN(MIDP)
               call MESSAGE('YOU SAVED ', filnam, ' ')
               NUM = 0
               md_ldbfile = ' '
               md_ldbfile = filnam
            end if
         end if
      else if (NWHAT == 29) then
         if (numobs == 0) then
            call QNERROR('THERE are NO observation points TO SAVE', ' ', ' ')
            NUM = 0
         else
            FILNAM = defaultFilename('obs')
            MIDP = 1
            call FILEMENU(MIDP, FILNAM, ierror)
            if (ierror /= 0) then
               NUM = 1
            else
               call doclose(midp)
               call saveObservations(filnam)
               call MESSAGE('YOU SAVED ', filnam, ' ')
               NUM = 0
               md_obsfile = ' '
               md_obsfile = filnam
            end if
         end if
      else if (NWHAT == 30) then
         if (ncrs == 0) then
            call QNERROR('THERE are NO cross sections TO SAVE', ' ', ' ')
            NUM = 0
         else
            FILNAM = '*_crs.pli'
            MIDP = 1
            call FILEMENU(MIDP, FILNAM, ierror)
            if (ierror /= 0) then
               NUM = 1
            else
               call WRICRS(MIDP)
               call MESSAGE('YOU SAVED ', filnam, ' ')
               NUM = 0
               md_crsfile = ' '
               md_crsfile = filnam
            end if
         end if
      else if (NWHAT == 31) then
         if (Ns == 0) then
            call QNERROR('THERE are NO samples TO SAVE', ' ', ' ')
            NUM = 0
         else
            FILNAM = '*.xyz'
            MIDP = 1
            call FILEMENU(MIDP, FILNAM, ierror)
            if (ierror /= 0) then
               NUM = 1
            else
               call WRIsam(MIDP)
               call MESSAGE('YOU SAVED ', filnam, ' ')
               NUM = 0
            end if
         end if
      else if (NWHAT == 32) then
         if (ndx == 0 .or. lnx == 0) then
            call qnerror('First reinitialise flow model, current dimensions are 0', ' ', ' ')

            return
         else
            if (ibedlevtyp == 1) then
               FILNAM = '*.xybl'
            else if (ibedlevtyp == 2) then
               FILNAM = '*.xyblu'
            else
               call qnerror('Just saving the network is sufficient for (preferred option) ibedlevtyp = 3 ', ' ', ' ')
               call qnerror('See Various, Change Geometry Parameters ', ' ', ' ')
               return
            end if
            MIDP = 1
            call FILEMENU(MIDP, FILNAM, ierror)
            if (ierror /= 0) then
               NUM = 1
            else
               if (ibedlevtyp == 1) then
                  call WRIbl(MIDP)
               else if (ibedlevtyp == 2) then
                  call WRIblu(MIDP)
               end if
               call MESSAGE('YOU SAVED ', filnam, ' ')
               NUM = 0
            end if
         end if
      else if (NWHAT == 33) then
         if (NDX == 0) then
            call QNERROR('THERE IS NO FLOW TO SAVE', ' ', ' ')
            NUM = 0
         else
            FILNAM = '*_rst.nc'
            MIDP = 1
            call FILEMENU(MIDP, FILNAM, ierror)
            if (ierror /= 0) then
               NUM = 1
            else
               call doclose(midp)
               call unc_write_rst(filnam)
               call wrirstfileold(time1)
               call MESSAGE('YOU SAVED ', filnam, ' ')
               NUM = 0
            end if
         end if
      else if (NWHAT == 34) then
         if (NDX == 0) then
            call QNERROR('THERE IS NO FLOW TO SAVE', ' ', ' ')
            NUM = 0
         else
            FILNAM = '*_map.nc'
            MIDP = 1
            call FILEMENU(MIDP, FILNAM, ierror)
            if (ierror /= 0) then
               NUM = 1
            else
               call doclose(midp)
               call unc_write_map(filnam)
               call MESSAGE('YOU SAVED ', filnam, ' ')
               NUM = 0
            end if
         end if
      else if (NWHAT == 35) then
         FILNAM = '*'
         MIDP = 0
         call FILEMENU(MIDP, FILNAM, ierror)
         if (ierror /= 0) then
            NUM = 1
         else
            call doclose(midp)
            call parsekerst(filnam)
            NUM = 0
            KEY = 3
         end if
!

!
!   ELSE IF (NWHAT .EQ. 20) THEN
!      FILNAM = '*.unt'
!      MLAN   = 0
!      CALL FILEMENU(MLAN,FILNAM,ierror)
!      IF (ierror .EQ. -2) THEN
!         CALL qnerror('file' , filnam, 'not found ')
!         NUM = 1
!      ELSE IF (ierror /= 0) THEN
!         NUM = 1
!      ELSE
!         CALL reajanet(MLAN,JA,1) !1=DOORLADEN
!         CALL MESSAGE('YOU LOADED ' , filnam, ' ')
!         CALL MINMXNS()
!         KEY = 3
!
!      ENDIF

      else if (NWHAT == 36) then

         if (numk == 0) then
            call QNERROR('THERE is no network to save ', ' ', ' ')
            NUM = 0
         else
            FILNAM = '*.node'
            MIDP = 1
            call FILEMENU(MIDP, FILNAM, ierror)
            if (ierror /= 0) then
               NUM = 1
            else
               call WRIswan(MIDP, filnam)
               call MESSAGE('YOU SAVED ', filnam, ' ')
               NUM = 0
            end if
         end if
      else if (NWHAT == 37) then ! partition files
         if (ndomains < 1) then
            call qnerror('no partitions found', ' ', ' ')
         else
!         FILNAM = '*_net.nc'
            filnam = md_netfile
            MTEK = 1
            call FILEMENU(MTEK, FILNAM, ierror)
            if (ierror /= 0) then
               NUM = 1
            else
               call doclose(mtek)
               md_partugrid = 1
               call getint('NetCDF ugrid? (0:UGRID-0.9, 1:UGRID-1.0, needed for 1D)', md_partugrid)
               call partition_write_domains(filnam, 6, 1, 1, md_partugrid) ! make subdomains for default solver
               call MESSAGE('YOU SAVED ', filnam, ' partitions')
               md_netfile = ' '
               md_netfile = filnam

               NUM = 0
            end if
         end if

      else if (NWHAT == 38) then
         call STOPINT()
         NUM = 0
      end if
      ! Nader uitwerken, of helemaal overboord ermee
      NUM = 0
      return
   end subroutine NFILES
