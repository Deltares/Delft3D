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

   SUBROUTINE NFILES(MODE, NUM,  NWHAT,  KEY)
!  grid lijst
!  NUM = 0, GELUKT, NUM = 1, NIET GELUKT
   use m_netw
   use m_grid
   use m_observations
   use m_monitoring_crosssections
   use m_thindams
   USE M_SPLINES, notinusenump => nump
   use unstruc_model
   use m_samples
   use m_flowgeom
   use unstruc_display
   use m_flowparameters
   use unstruc_files, only:defaultFilename, close_all_files
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

   implicit none
   integer :: MODE, NUM,  NWHAT,  KEY
   integer :: ja, ierr
   integer :: mlan
   integer :: midp
   integer :: mtek
   integer :: ndraw
   integer :: i, k, ierror
   logical :: jawel
   logical, external :: read_samples_from_geotiff

   interface
      subroutine realan(mlan, antot)
         integer, intent(inout)                ::  mlan
         integer, intent(inout), optional      ::  antot
      end subroutine realan
   end interface

   COMMON /DRAWTHIS/ ndraw(50)
   COMMON /BACKGROUND/ SCREENFILE
   CHARACTER FILNAM*86, SCREENFILE*86

   KEY    = 0

   IF (NWHAT .EQ. 1) THEN
      FILNAM = '*.mdu'
      MLAN   = 0
      CALL FILEMENU(MLAN,FILNAM,ierror)
      IF (ierror .EQ. -2) THEN
         CALL qnerror('file' , filnam, 'not found ')
         NUM = 1
      ELSE IF (ierror .EQ. -1) THEN   ! Cancel
         NUM = 1
      ELSE
        call doclose(mlan) ! TODO: change... [AvD]
        call inidat() ! TODO: call reset_display_settings() +  call dfm_reset_globaldata()
        call resetFullFlowModel()
        CALL loadModel(filnam)
        call minmxns()
        ! Check for presence of associated display presets
        inquire (file = trim(md_ident)//'.cfg', exist = jawel)
        if (jawel) then
            call load_displaysettings(trim(md_ident)//'.cfg')
        else
            inquire (file = 'unstruc.cfg', exist = jawel)
            if (jawel) then
               call load_displaysettings('unstruc.cfg')
            endif
        end if

        NDRAW(2) = 1
        KEY = 3
        NUM = 0
      ENDIF
   ELSE IF (NWHAT .EQ. 2) THEN
      FILNAM = '*_net.nc'
      MLAN   = 0
      CALL FILEMENU(MLAN,FILNAM,ierror)
      IF (ierror .EQ. -2) THEN
         CALL qnerror('file' , filnam, 'not found ')
         NUM = 1
      ELSE IF (ierror .EQ. -1) THEN
         NUM = 1
      ELSE
        call doclose(mlan) ! TODO: change... [AvD]
         CALL loadNetwork(filnam, JA, 0)
         IF (JA == 0) THEN
            CALL resetFlow()
            nump = 0 ! Reset cell data
            CALL MESSAGE('YOU LOADED ' , filnam, ' ')
            CALL MINMXNS()
            NDRAW(2) = 1
            KEY = 3
            NUM = 0
            md_netfile = ' '
            md_netfile = trim(filnam)
         ELSE
            CALL qnerror('NO NET LOADED', ' ', ' ')
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 3) THEN
      FILNAM = '*_net.nc'
      MLAN   = 0
      CALL FILEMENU(MLAN,FILNAM,ierror)
      IF (ierror .EQ. -2) THEN
         CALL qnerror('file' , filnam, 'not found ')
         NUM = 1
      ELSE IF (ierror .EQ. -1) THEN
         NUM = 1
      ELSE
         IF (INDEX(FILNAM, '.jan') > 0) then
            call REAJANET(Mlan,JA,1)
         ELSE IF (INDEX(FILNAM, '.adc') > 0) then
            call READADCIRCNET(Mlan,JA,1)
         else
            call doclose(mlan) ! TODO: change... [AvD]
            call loadNetwork(filnam, JA, 1)
         endif

         IF (JA == 0) THEN
            CALL MESSAGE('YOU LOADED ' , filnam, ' ')
            CALL MINMXNS()
            NDRAW(2) = 1
            KEY = 3
            NUM = 0
            md_netfile = ' '
            md_netfile = trim(filnam)
         ELSE
            CALL qnerror('NO NET LOADED', ' ', ' ')
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 4) THEN
      FILNAM = '*.grd'
      MLAN   = 0
      CALL FILEMENU(MLAN,FILNAM,ierror)
      IF (ierror .EQ. -2) THEN
         CALL qnerror('file' , filnam, 'not found ')
         NUM = 1
      ELSE IF (ierror .EQ. -1) THEN
         NUM = 1
      ELSE
         CALL REAgrid(MLAN,FILNAM,ja)  ! DOORLADEN
         IF (JA .GE. 1) THEN
            CALL MESSAGE('YOU LOADED ' , filnam, ' ')
            CALL MINMXNS()
            NDRAW(2) = 1
            KEY = 3
            NUM = 0
         ELSE
            CALL QNERROR('PREMATURE END OF FILE', FILNAM, ' ')
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 5) THEN
      FILNAM = '*.asc'
      MLAN   = 0
      CALL FILEMENU(MLAN,FILNAM,ierror)
      IF (ierror .EQ. -2) THEN
         CALL qnerror('file' , filnam, 'not found ')
         NUM = 1
      ELSE IF (ierror .EQ. -1) THEN
         NUM = 1
      ELSE
         CALL readarcinfo(MLAN,ja)  ! DOORLADEN
         IF (JA .GE. 1) THEN
            CALL MESSAGE('YOU LOADED ' , filnam, ' ')
            CALL MINMXNS()
            NDRAW(2) = 1
            KEY = 3
            NUM = 0
         ELSE
            CALL QNERROR('PREMATURE END OF FILE', FILNAM, ' ')
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 6) THEN
      FILNAM = '*.pol,*.pli,*.pliz'
      MLAN   = 0
      CALL FILEMENU(MLAN,FILNAM,ierror)
      IF (ierror .EQ. -2) THEN
         CALL qnerror('file' , filnam, 'not found ')
         NUM = 1
      ELSE IF (ierror .EQ. -1) THEN
         NUM = 1
      ELSE
         CALL REAPOL(MLAN, 0)
         IF (NPL .GT. 0) THEN
            CALL MESSAGE('YOU LOADED ' , filnam, ' ')
            CALL MINMXNS( )
            KEY = 3
            NUM = 0

   !        read polygon: netcell administration out of date
            netstat = NETSTAT_CELLS_DIRTY
         ELSE
            CALL qnerror('file' , filnam, 'not found ')
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 7) THEN
      FILNAM = '*.spl'
      MLAN   = 0
      CALL FILEMENU(MLAN,FILNAM,ierror)
      IF (MLAN .NE. 0) THEN
         CALL readSplines(mlan)
         IF (mcs .GT. 0) THEN
            CALL MESSAGE('You Opened File ', FILNAM, ' ')
            CALL MINMXNS()
            NUM  = 0
            NDRAW(15) = 1
            KEY  = 3
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 8) THEN
      FILNAM = '*.ldb'
      MLAN   = 0
      CALL FILEMENU(MLAN,FILNAM,ierror)
      IF (ierror .EQ. -2) THEN
         CALL qnerror('file' , filnam, 'not found ')
         NUM = 1
      ELSE IF (ierror .EQ. -1) THEN
         NUM = 1
      ELSE
        i = len_trim(filnam)
        if (i > 3) then
            if (filnam(i-2:i) == '.nc') then
                call doclose(mlan)
                call read_land_boundary_netcdf(filnam)
                return
            end if
        end if

         CALL REALAN(MLAN)

         IF (MXLAN .GT. 0) THEN
            CALL MESSAGE('YOU LOADED ' , filnam, ' ')
            CALL MINMXNS()
            NDRAW(3) = 1
            KEY = 3
            NUM = 0
            md_ldbfile = ' '
            md_ldbfile = filnam
         ELSE
            CALL qnerror('MXLAN = 0',' ',' ')
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 9 .or. NWHAT .EQ. 10 ) THEN
      FILNAM = '*_obs.xyn'
      MLAN   = 0
      CALL FILEMENU(MLAN,FILNAM,ierror)
      IF (ierror .EQ. -2) THEN
         CALL qnerror('file' , filnam, 'not found ')
         NUM = 1
      ELSE IF (ierror .EQ. -1) THEN
         NUM = 1
      ELSE
         ja = 0
         key = 3
         call doclose(mlan) ! Ugly, but loadObservations reads by filename, not filepointer [AvD]
         if (NWHAT == 10) then
            ja = 1 ! doorladen
         else
            ja = 0
         end if
         call loadObservations(filnam, ja)
         CALL MESSAGE('YOU LOADED ' , filnam, ' ')
         CALL MINMXNS()
         md_obsfile = ' '
         md_obsfile = filnam
      ENDIF
   ELSE IF (NWHAT .EQ. 11 .or. NWHAT .EQ. 12 ) THEN
      FILNAM = '*_crs.pli'
      MLAN   = 0
      CALL FILEMENU(MLAN,FILNAM,ierror)
      IF (ierror .EQ. -2) THEN
         CALL qnerror('file' , filnam, 'not found ')
         NUM = 1
      ELSE IF (ierror .EQ. -1) THEN
         NUM = 1
      ELSE
         ja = 0
         key = 3
         if (NWHAT == 12) then
            ja = 1 ! doorladen
         else
            ja = 0
         end if
         CALL REAPOL(MLAN, ja) ! Read pol/pli as crs
         call pol_to_crosssections(xpl, ypl, npl, names=nampli)
         if ( NPL.gt.0 ) call delpol()
         CALL MESSAGE('YOU LOADED ' , filnam, ' ')
         CALL MINMXNS()
         md_crsfile = ' '
         md_crsfile = filnam
      ENDIF
   ELSE IF (NWHAT .EQ. 13 .or. NWHAT .EQ. 14 ) THEN
      FILNAM = '*_thd.pli'
      MLAN   = 0
      CALL FILEMENU(MLAN,FILNAM,ierror)
      IF (ierror .EQ. -2) THEN
         CALL qnerror('file' , filnam, 'not found ')
         NUM = 1
      ELSE IF (ierror .EQ. -1) THEN
         NUM = 1
      ELSE
         ja = 0
         key = 3
         if (NWHAT == 14) then
            ja = 1 ! doorladen
         else
            ja = 0
         end if
         CALL REAPOL(MLAN, ja) ! Read pol/pli as thin dam-type crs
         call pol_to_thindams(xpl, ypl, npl)
         CALL MESSAGE('YOU LOADED ' , filnam, ' ')
         CALL MINMXNS()
         md_thdfile = ' '
         md_thdfile = filnam
      ENDIF
   ELSE IF (NWHAT .EQ. 15) THEN
      FILNAM = '*.xyz,*.dem,*.asc,*.tif*'
      MLAN   = 0
      CALL FILEMENU(MLAN,FILNAM,ierror)
      IF (ierror .EQ. -2) THEN
         CALL qnerror('file' , filnam, 'not found ')
         NUM = 1
      ELSE IF (ierror .EQ. -1) THEN
         NUM = 1
      ELSE
         ja = 0
         key = 3
         i = len_trim(filnam)
         if (i > 3) then
            if (strcmpi(filnam(i-3:i), '.dem')) then
                call doclose(mlan)
                call read_samples_from_dem(trim(filnam), ja)
            else if (strcmpi(filnam(i-3:i), '.asc')) then
                call doclose(mlan)

!               delete all samples, regardless of selecting polygon
                call savepol()
                call delpol()
                call savesam()
                call delsam(0)
                call restorepol()

                call read_samples_from_arcinfo(trim(filnam), ja, 1)  ! reaasc
            else if (strcmpi(filnam(      i-3 :i), '.tif') &
                .or. strcmpi(filnam(max(1,i-4):i), '.tiff')) then
               call doclose(mlan)
               success = read_samples_from_geotiff(filnam)
            else if (strcmpi(filnam(i-3:i), '.xyz')) then
                CALL reasam(MLAN,ja)  ! DOORLADEN
            end if
         else
            CALL reasam(MLAN,ja)  ! DOORLADEN
         end if
         CALL MESSAGE('YOU LOADED ' , filnam, ' ')
         CALL MINMXNS()
      ENDIF
   ELSE IF (NWHAT .EQ. 16) THEN
      if (ndx == 0 .or. lnx == 0) then
         call qnerror('First reinitialise flow model, current dimensions are 0',' ',' ')
         return
      endif
      if (ibedlevtyp == 1) then
         FILNAM = '*.xybl'
      else if (ibedlevtyp == 2) then
         FILNAM = '*.xyblu'
      else
         CALL qnerror('Loading cell bottom levels bl (ibedlevtyp=1) or flow link bottom levels blu (ibedlevtyp=2)',' ',' ')
         CALL qnerror('Change parameter ibedlevtyp in Various, Change Geometry Parameters',' ',' ')
         return
      endif
      MLAN   = 0
      CALL FILEMENU(MLAN,FILNAM,ierror)
      IF (ierror .EQ. -2) THEN
         CALL qnerror('file' , filnam, 'not found ')
         NUM = 1
      ELSE IF (ierror .EQ. -1) THEN
         NUM = 1
      ELSE
         if (ibedlevtyp == 1) then
            CALL reabl(MLAN)
         else if (ibedlevtyp == 2) then
            CALL reablu(MLAN)
         endif
         CALL MESSAGE('YOU LOADED ' , filnam, ' ')
         ! CALL MINMXNS()
      ENDIF
   ELSE IF (NWHAT .EQ. 17) THEN
      FILNAM = '*_rst.nc'
      MLAN   = 0
      CALL FILEMENU(MLAN,FILNAM,ierror)
      IF (ierror .EQ. -2) THEN
         CALL qnerror('file' , filnam, 'not found ')
         NUM = 1
      ELSE IF (ierror .EQ. -1) THEN
         NUM = 1
      ELSE
         i = len_trim(filnam)
         if (filnam(i-6:i) == '_rst.nc' .or. filnam(i-6:i) == '_RST.NC') then
            call doclose(mlan) ! TODO: change... [AvD]
            call read_restart_from_map(FILNAM, ierr)
            if (ierr /= DFM_NOERR) then
               call qnerror('Error occurs when reading the restart file.',' ', ' ')
               JA = 0
            else
               JA = 1
            end if
            if (iperot == -1) then
               call reconst2nd ()
            endif
            call setucxucyucxuucyunew() ! reconstruct cell center velocities
         else
            call rearst(MLAN,JA)
         endif
         !else if (filnam(i-6:i) == '_map.nc' .or. filnam(i-6:i) == '_MAP.NC') then
         !   call doclose(MLAN)
         !   call read_restart_from_map(FILNAM,JA)
         !   ! TODO: AvD: No flow_setstarttime here?

         if (JA == 1) then
            call MESSAGE('YOU LOADED ' , filnam, ' ')
         else
            call qnerror('NO RESTART LOADED', ' ', ' ')
         endif
         ! CALL MINMXNS()
      ENDIF
   ELSE IF (NWHAT .EQ. 18) THEN
         NUM    = 0
         FILNAM = '*.bmp'
         MIDP   = 0
         CALL FILEMENU(MIDP,FILNAM,ierror)
         IF (ierror /= 0) THEN
            NDRAW(26) = 0
         ELSE IF (MIDP /= 0) THEN
            CALL DOCLOSE(MIDP)
            CALL LOADBITMAP(FILNAM)
            CALL MESSAGE('YOU LOADED ' , filnam, ' ')
            CALL MINMXNS( )
         ENDIF
         KEY = 3
   ELSE IF (NWHAT .EQ. 20) THEN
      FILNAM = '*.mdu'
      MTEK   = 1
      CALL FILEMENU(MTEK,FILNAM,ierror)
      IF (ierror /= 0) THEN
         NUM = 1
      ELSE
         call doclose(mtek)
         call writeMDUFile(filnam, ja)
         CALL MESSAGE('YOU SAVED ' , filnam, ' ')
         NUM = 0
      ENDIF
   ELSE IF (NWHAT .EQ. 21 .or. NWHAT .EQ. 22 .or. NWHAT .EQ. 24) THEN
      IF (NUMK .EQ. 0) THEN
         CALL QNERROR('NO NET TO SAVE',' ',' ')
         NUM = 0
      ELSE
         if ( nwhat.eq.21 .or. nwhat .eq. 22) then
         FILNAM = '*_net.nc'
         else if ( nwhat.eq.24 ) then
            FILNAM = '*_net.plt'
         end if

         MTEK   = 1
         CALL FILEMENU(MTEK,FILNAM,ierror)
         IF (ierror /= 0) THEN
            NUM = 1
         ELSE
            call doclose(mtek)
            if (nwhat.eq.21) then
               if (index(filnam, '.net') > 0) then
                   CALL NEWFIL(MTEK, filnam) ; CALL WRINET(MTEK)
               else
               call unc_write_net(filnam, janetcell = 0, janetbnd = 0)
               endif
            else if ( nwhat .eq. 22) then ! _net.nc with extra cell info (for example necessary for Baseline/Bas2FM input)
               if ( netstat.ne.NETSTAT_OK ) then
                  call findcells(0)
                  call find1dcells()
               end if
               call unc_write_net(filnam, janetcell = 1, janetbnd = 1) ! wrinet
!               !call unc_write_net_ugrid2(filnam, janetcell = 0, janetbnd = 0)

               !origial call unc_write_net(filnam, janetcell = 1, janetbnd = 0)
               call unc_write_net('UG'//filnam, janetcell = 1, janetbnd = 0, iconventions = UNC_CONV_UGRID)
            else if ( nwhat.eq.24 ) then
               call ini_tecplot()
               call wrinet_tecplot(filnam)
            end if
            CALL MESSAGE('YOU SAVED ' , filnam, ' ')
            md_netfile = ' '
            md_netfile = filnam


            NUM = 0
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 23) THEN
      IF (NUMK .EQ. 0) THEN
         CALL QNERROR('NO NET TO SAVE',' ',' ')
         NUM = 0
      ELSE
          !call foam_write_polymesh('testfoam')
         FILNAM = '*.kml'
         MTEK   = 1
         ja = 1
         if (jsferic /= 1) then
             call confrm('Model is not in spherical coordinates. Proceed? (not recommended)', ja)
         end if
         if (ja == 1) then
            call change_kml_parameters(ja)
         else
             ja = 1 ! Hereafter, 1 means 'no/cancelled'
         end if
         if (ja==0) then ! 0: NOT cancelled
            CALL FILEMENU(MTEK,FILNAM,ierror)
            IF (ierror /= 0) THEN
                NUM = 1
            ELSE
                call doclose(mtek)
                call kml_write_net(filnam)
                CALL MESSAGE('YOU SAVED ' , filnam, ' ')
                NUM = 0
            end if
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 24) THEN
   ELSE IF (NWHAT .EQ. 25) THEN
      IF (MC == 0 .or. NC == 0) THEN
         CALL QNERROR('NO GRID TO SAVE',' ',' ')
         NUM = 0
      ELSE
         FILNAM = '*.grd'
         MTEK   = 1
         CALL FILEMENU(MTEK,FILNAM,ierror)
         IF (ierror /= 0) THEN
            NUM = 1
         ELSE
            call wrirgf(mtek, filnam)
            CALL MESSAGE('YOU SAVED ' , filnam, ' ')
            NUM = 0
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 26) THEN
      IF (NPL .EQ. 0) THEN
         CALL QNERROR('THERE IS NO POLYGON TO SAVE',' ',' ')
         NUM = 0
      ELSE
         FILNAM = '*.pol,*.pli,*.pliz'
         MIDP   = 1
         CALL FILEMENU(MIDP,FILNAM,ierror)
         IF (ierror /= 0) THEN
            NUM = 1
         ELSE
            CALL WRIPOL(MIDP)
            if ( index(Filnam,'crs') == 0 .and. index(Filnam,'CRS') == 0 .and. index(Filnam,'vlay') == 0 .and. index(Filnam,'VLAY') == 0) then
                call wricmps(filnam)
            endif
            CALL MESSAGE('YOU SAVED ' , filnam, ' ')
            NUM = 0
            md_plifile = ' ' ; md_plifile = filnam
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 27) THEN
      IF (mcs .EQ. 0) THEN
         CALL QNERROR('There Are No Splines to SAVE',' ',' ')
      ELSE
         FILNAM = '*.spl'
         MLAN   = 1
         CALL FILEMENU(MLAN,FILNAM,ierror)
         IF (ierror == 0) THEN
            CALL writeSplines(MLAN)
            CALL MESSAGE('You Saved File ', FILNAM, ' ')
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 28) THEN
      IF (MXLAN .EQ. 0) THEN
         CALL QNERROR('THERE IS NO LANDBOUNDARY TO SAVE',' ',' ')
         NUM = 0
      ELSE
         FILNAM = '*.ldb'
         MIDP   = 1
         CALL FILEMENU(MIDP,FILNAM,ierror)
         IF (ierror /= 0) THEN
            NUM = 1
         ELSE
            CALL WRILAN(MIDP)
            CALL MESSAGE('YOU SAVED ' , filnam, ' ')
            NUM = 0
            md_ldbfile = ' '
            md_ldbfile = filnam
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 29) THEN
      IF (numobs .EQ. 0) THEN
         CALL QNERROR('THERE are NO observation points TO SAVE',' ',' ')
         NUM = 0
      ELSE
         FILNAM = defaultFilename('obs')
         MIDP   = 1
         CALL FILEMENU(MIDP,FILNAM,ierror)
         IF (ierror /= 0) THEN
            NUM = 1
         ELSE
            call doclose(midp)
            CALL saveObservations(filnam)
            CALL MESSAGE('YOU SAVED ' , filnam, ' ')
            NUM = 0
            md_obsfile = ' '
            md_obsfile = filnam
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 30) THEN
      IF (ncrs .EQ. 0) THEN
         CALL QNERROR('THERE are NO cross sections TO SAVE',' ',' ')
         NUM = 0
      ELSE
         FILNAM = '*_crs.pli'
         MIDP   = 1
         CALL FILEMENU(MIDP,FILNAM,ierror)
         IF (ierror /= 0) THEN
            NUM = 1
         ELSE
            CALL WRICRS(MIDP)
            CALL MESSAGE('YOU SAVED ' , filnam, ' ')
            NUM = 0
            md_crsfile = ' '
            md_crsfile = filnam
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 31) THEN
      IF (Ns .EQ. 0) THEN
         CALL QNERROR('THERE are NO samples TO SAVE',' ',' ')
         NUM = 0
      ELSE
         FILNAM = '*.xyz'
         MIDP   = 1
         CALL FILEMENU(MIDP,FILNAM,ierror)
         IF (ierror /= 0) THEN
            NUM = 1
         ELSE
            CALL WRIsam(MIDP)
            CALL MESSAGE('YOU SAVED ' , filnam, ' ')
            NUM = 0
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 32) THEN
      if (ndx == 0 .or. lnx == 0) then
         call qnerror('First reinitialise flow model, current dimensions are 0',' ',' ')

         return
      else
         if (ibedlevtyp == 1) then
            FILNAM = '*.xybl'
         else if (ibedlevtyp == 2) then
            FILNAM = '*.xyblu'
         else
            CALL qnerror('Just saving the network is sufficient for (preferred option) ibedlevtyp = 3 ',' ',' ')
            CALL qnerror('See Various, Change Geometry Parameters ',' ',' ')
            return
         endif
         MIDP   = 1
         CALL FILEMENU(MIDP,FILNAM,ierror)
         IF (ierror /= 0) THEN
            NUM = 1
         ELSE
            if (ibedlevtyp == 1) then
               CALL WRIbl(MIDP)
            else if (ibedlevtyp == 2) then
               CALL WRIblu(MIDP)
            endif
            CALL MESSAGE('YOU SAVED ' , filnam, ' ')
            NUM = 0
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 33) THEN
      IF (NDX .EQ. 0) THEN
         CALL QNERROR('THERE IS NO FLOW TO SAVE',' ',' ')
         NUM = 0
      ELSE
         FILNAM = '*_rst.nc'
         MIDP   = 1
         CALL FILEMENU(MIDP,FILNAM,ierror)
         IF (ierror /= 0) THEN
            NUM = 1
         ELSE
            call doclose(midp)
            CALL unc_write_rst(filnam)
            call wrirstfileold(time1)
            CALL MESSAGE('YOU SAVED ' , filnam, ' ')
            NUM = 0
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 34) THEN
      IF (NDX .EQ. 0) THEN
         CALL QNERROR('THERE IS NO FLOW TO SAVE',' ',' ')
         NUM = 0
      ELSE
         FILNAM = '*_map.nc'
         MIDP   = 1
         CALL FILEMENU(MIDP,FILNAM,ierror)
         IF (ierror /= 0) THEN
            NUM = 1
         ELSE
            call doclose(midp)
            CALL unc_write_map(filnam)
            CALL MESSAGE('YOU SAVED ' , filnam, ' ')
            NUM = 0
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 35) THEN
     FILNAM = '*'
     MIDP   = 0
     CALL FILEMENU(MIDP,FILNAM,ierror)
     IF (ierror /= 0) THEN
        NUM = 1
     ELSE
        call doclose(midp)
        call parsekerst(filnam)
        NUM = 0
        KEY = 3
     ENDIF
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

   ELSE IF (NWHAT .EQ. 36) THEN

      IF (numk .EQ. 0) THEN
         CALL QNERROR('THERE is no network to save ',' ',' ')
         NUM = 0
      ELSE
         FILNAM = '*.node'
         MIDP   = 1
         CALL FILEMENU(MIDP,FILNAM,ierror)
         IF (ierror /= 0) THEN
            NUM = 1
         ELSE
            CALL WRIswan(MIDP,filnam)
            CALL MESSAGE('YOU SAVED ' , filnam, ' ')
            NUM = 0
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 37 ) THEN    ! partition files
      if ( ndomains.lt.1 ) then
         call qnerror('no partitions found', ' ', ' ')
      else
!         FILNAM = '*_net.nc'
         filnam = md_netfile
         MTEK   = 1
         CALL FILEMENU(MTEK,FILNAM,ierror)
         IF (ierror /= 0) THEN
            NUM = 1
         ELSE
            call doclose(mtek)
            md_partugrid = 1
            call getint('NetCDF ugrid? (0:UGRID-0.9, 1:UGRID-1.0, needed for 1D)', md_partugrid)
            call partition_write_domains(filnam, 6, 1, 1, md_partugrid) ! make subdomains for default solver
            CALL MESSAGE('YOU SAVED ' , filnam, ' partitions')
            md_netfile = ' '
            md_netfile = filnam

            NUM = 0
         ENDIF
      end if

   ELSE IF (NWHAT .EQ. 38) THEN
      CALL STOPINT()
      NUM = 0
   ENDIF
   ! Nader uitwerken, of helemaal overboord ermee
   NUM = 0
   RETURN
   END SUBROUTINE NFILES
