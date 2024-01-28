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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
   SUBROUTINE INIDAT()

   USE m_missing
   use m_netw
   USE M_BOAT
   USE M_AFMETING
   USE M_SEASTATE
   use unstruc_model
   use unstruc_display
   use unstruc_messages
   use M_splines, only: increasespl, maxspl, maxsplen, readsplines
   USE M_SAMPLES
   use m_commandline_option
   use dfm_signals
   use gridoperations
   use m_monitoring_crosssections, only: increaseCrossSections
   implicit none

   double precision :: ag, cdflow, cfl, cfric, deltx, delty, deltz, dscr, dx, e0, eps, epsgs, fbouy, fdyn, gx, gy, gz
   integer :: itgs
   integer :: janet, jav
   integer :: jqn
   integer :: jview
   integer :: k
   integer :: maxitgs
   integer :: moments
   integer :: ndraw
   integer :: nlevel
   double precision :: pi
   double precision :: rho
   double precision :: rhow
   double precision :: rk
   double precision :: rmiss
   double precision :: splfac
   double precision :: splfac2
   double precision :: wpqr
   double precision :: xyz
   double precision :: zfac
   double precision :: zupw
   integer, save :: jaSkipCmdLineArgs = 0 !< Later set to 1, to read cmdline args just once.

   COMMON /DRAWTHIS/ ndraw(50)
   COMMON /HELPNOW/  WRDKEY,NLEVEL
   COMMON /SPLINEFAC/ SPLFAC, SPLFAC2
   COMMON /CONSTANTS/ E0, RHO, RHOW, CFL, EPS, AG, PI
   COMMON /SETTINGS/ FDYN, FBOUY, CDFLOW, CFRIC, MOMENTS, JANET
   COMMON /QNRGF/ JQN

   COMMON /PERSPX/ WPQR,DELTX,DELTY,DELTZ,ZFAC,DSCR,ZUPW
   COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4
   COMMON /GRAVITY/ GX,GY,GZ
   COMMON /SOLVER/ EPSGS, MAXITGS, ITGS
   COMMON /PLOTFIL/   PLOTJE
   CHARACTER PLOTJE*255
   CHARACTER WRDKEY*40


   !  1=CLS
   !  2=GRID/NET    1=RECHT   2=SPLINE
   !  3=LAND    1=NORMAAL 2=PUNTJES
   !  4=SAM     1=FCIR    2=CIJFER
   !  5=DEP     1=DOT  2=FIL 3=LIN 4=NOPOINT 5=CIJFER
   !  6=POLYGON
   !  7=TEKENENEN ELEMENTEN UIT ELEMENT ADMINISTRATIE
   !  8=DRAW INTERPOL YES/NO                           NODEVALS
   !  9=MET     1=VLAK 2=PER 3=SUNPER 4=DRYFL 5=PROJ
   !  10=HCOPY  1=JA
   !  11=COUR   1=M,2=N,3=MN                           STRESSES
   !  12=ISOSCALE 1=NODES, 2=LINKS, 3=BOTH, 4=OFF
   !  13=?
   !  14=ELEMENT STRESSES
   !  15=SPLINES
   !  16=PREVIOUS STATE GRID
   !  17=SECOND DEPTH
   !  18=BOAT
   !  19=DDBOUNDARIES, INTERNAL BOUNDARIES

   call dfm_add_signalwatchers() ! Register ourselves to Ctrl-C and Ctrl-Z presses, etc. for emergency map files.

   CALL ININUMBERS()

   call maketekaltimes()

   JQN       = 2
   NDRAW(1)  = 1  ! clear screen yes/no
   NDRAW(2)  = 1  ! display network
   NDRAW(3)  = 1  ! display landboundary
   NDRAW(4)  = 0  ! void
   NDRAW(5)  = 2  ! void
   NDRAW(6)  = 0  ! void
   NDRAW(7)  = 1  ! WHICH netLINk VALue, 1 = NO
   NDRAW(8)  = 1  ! WHICH netNODe VALue, 1 = NO
   NDRAW(9)  = 1  ! NORMAL OR PERSPECTIVE
   NDRAW(10) = 0  ! also plot to file yes/no
   NDRAW(11) = 4  ! HOW HISPLAY LINVAL, ,Linkhow: 1=no, 2=numbers, 3=isofil smooth, 4 = isofil, 5=dots
   NDRAW(12) = 3  ! 1=nodes, 2=links, 3=nodes+links, 4 = no isoscale
   NDRAW(13) = 2  !1 !0  ! SHOW UV VECTORS AND FLOW FORCES
   NDRAW(14) = 2  ! reserved for disval
   NDRAW(15) = 1  ! display splines
   NDRAW(16) = 0  ! display PREVIOUS NETWORK
   NDRAW(17) = 0  ! void
   NDRAW(18) = 2  ! Sideview, 1 = no, 2=small, 3=larger, 4=largest
   NDRAW(19) = 4  ! HOW DISPLAY NODEVAL, Nodehow: 1=no, 2=numbers, 3=isofil smooth, 4 = isofil, 5=dots
   NDRAW(20) = 0  ! void
   NDRAW(21) = 0  ! void
   NDRAW(22) = 0  ! SHOW netcell types tri, quads penta, hexa
   NDRAW(26) = 0  ! 1 = BITMAP
   NDRAW(27) = 1  ! 1 = nothing, 2 = both surf and bot, 3 just bot, 4 just surf
   NDRAW(28) = 2  ! values at Flow Nodes 1=no, 2=s1, 3=bl, 4=ba, 5=v1    , nodewhat
   NDRAW(29) = 1  ! values at Flow Links 1=no, 2=u1, 3=q1, 4=au,         , linkwhat
   ndraw(30) = 1  ! do not show all flow links
   ndraw(31) = 1  ! do not show show values at cell corners, 2=ucnx, 3=ucny
   ndraw(32) = 1  ! show samples coloured dot
   ndraw(33) = 1  ! show values at net cells, 2=number, 3=aspect ratio, 4=orientation vectors, 5=aspect ratio and orientation vectors, 6=cell area, 7=coarsening information
   ndraw(34) = 0  ! Banf, 0=no, 1 =seqtr, 3=(seqtr-s), tekbanf
   ndraw(35) = 1  ! yes draw reference profiles (only for 3D)
   ndraw(36) = 0  ! values on flow nodes minus plotlin default 0
   ndraw(37) = 2  ! 0 = no, 1 = probe, no boundaryvalues, 2=probe+boundaryvalues
   ndraw(38) = 1  ! display curvilinear grid: 0 = no, 1 = lines, 2 = count netw
   ndraw(39) = 0  ! 1=predraw bathy in link colours, 0 = not do so
   ndraw(40) = 0  ! waterbal
   ndraw(41) = 1  ! sorsin


   plotje    = ' '
   jview     = len_trim (plotje)

   JVIEW     = 1
   JAV       = 3
   ITGS      = 0

   NUML      = 0
   NUMK      = 0
   NUML0     = 0
   NUMK0     = 0
   NUMP      = 0
   MXLAN     = 0
   NPL       = 0
   NSMAX     = 0
   NS        = 0
   NS2       = 0
   MC        = 0
   NC        = 0

   MXBOAT    = 0
   NCLBOAT   = 170


   CALL PARAMTEXT('Waterlevel                          (m )', 1)

   if (.not. allocated(xk)) then
      allocate( xk (1), yk (1), zk (1) , NOD (1) , KC (1) , NMK (1) , KN(3,1), LC(1), RNOD(1), RLIN(1))
      allocate(nod(1)%lin(1))
      NMK = 0
   endif
   if (.not. allocated(xk0)) then
      allocate( xk0(1), yk0(1), zk0(1) , NOD0(1) , KC0(1) , NMK0(1), KN0(3,1), LC0(1)  )
      allocate(nod0(1)%lin(1))
      nmk0 = 0
   endif

   KMAX = 1
   LMAX = 1
   CALL INCREASENETW(KMAX, LMAX)

   CALL INCREASEPOL(MAXPOL, 0)
!write (*,*) 'increased pols'
   CALL INCREASEGRID(2,2)
!write (*,*) 'increased grid'

   call increasespl(maxspl, maxsplen)
!write (*,*) 'increased spl'

   !call increaseCrossSections(maxcrs)
!write (*,*) 'increased crs'

   CALL INCREASESAM(2)
!write (*,*) 'increased sam'

   CALL INCREASELAN(MAXLAN)
!write (*,*) 'increased lan'

   CALL ZERONET()
!write (*,*) 'zeronet'

   XK0  = 0 ; YK0  = 0 ; ZK0  = 0
   !XK1  = 0 ; YK1  = 0 ; ZK1  = 0
   RK    = 0

   RNOD = dmiss ; RLIN = dmiss

   XLAN = xymis ; YLAN = xymis ; ZLAN = 0 ; NCLAN = 0
   XPL  = 0 ; YPL  = 0

   KN   = 0 ; KN0  = 0

   NMK  = 0 ; NMK0 = 0
   KC   = 0 ; KC0  = 0
   LC   = 0 ; LC0  = 0



   DX       = 1.0d20
   RMISS    = -999
   ZUPW     = 1d0
   AG       = 9.81d0
   PI       = ACOS(-1.)
   RHOW     = 1000
   JVAST    = 0
   RLENGTH  = 1
   RWIDTH   = 0.01d0
   RTHICK   = 0.01d0
   LFAC     = 2
   MOMENTS  = 1

   XYZ      = 0

   TWOPI    = 2*ACOS(-1d0)
   WAVLEN   = WAVCEL*WAVPER
   WAVKX    = TWOPI/WAVLEN
   WAVOM    = TWOPI/WAVPER

   CALL INISFERIC()

   if ( jaSkipCmdLineArgs.eq.1 ) then
      iarg_autostart = -1
   else
      do k=1,numfiles

         call loadfile(inputfiles(k))
 
      end do
   end if

   jaSkipCmdLineArgs = 1 !< Do not process cmdline again (for example when reading another mdu via files menu)

   ! Merge cmd line switches with mdu file settings
   if (iarg_autostart /= -1) then
       md_jaAutoStart = iarg_autostart
   end if

   if ( jaGUI.eq.1 ) then
      CALL MINMXNS()
      !CALL WEAREL()
   end if

   WRDKEY = 'PROGRAM PURPOSE'
   NLEVEL = 1

   RETURN
   END SUBROUTINE INIDAT

   
   subroutine loadfile(inarg)

   USE m_missing
   use m_netw
   USE M_BOAT
   USE M_AFMETING
   USE M_SEASTATE
   use unstruc_model
   use unstruc_display
   use unstruc_messages
   use M_splines, only: increasespl, readsplines
   USE M_SAMPLES
   use m_commandline_option
   use dfm_signals
   use gridoperations
   use m_monitoring_crosssections, only: increaseCrossSections


   implicit none
   CHARACTER inarg*(*), EXT*4
   LOGICAL JAWEL
   integer :: minp, n1, n2, istat, ja

   interface
      subroutine realan(mlan, antot)
         integer, intent(inout)                ::  mlan
         integer, intent(inout), optional      ::  antot
      end subroutine realan
   end interface

   INQUIRE(FILE = trim(inarg),EXIST = JAWEL)
   if (JAWEL) then
      ! Find file extention based on first full stop symbol '.' at the back of the string.
      N1  = INDEX (inarg,'.', .true.)
      N2  = len_trim(inarg)
      EXT = ' '
      EXT = inarg(N1:N2)
      IF (EXT .EQ. '.ldb' .OR. EXT .EQ. '.LDB' ) THEN
         CALL OLDFIL (MINP, inarg)
         CALL REALAN (MINP)
      ELSE IF (EXT .EQ. '.net' .OR. (EXT .EQ. '.nc' .and. inarg(max(1,N1-4):max(1,N1-1)) == '_net') ) THEN
         !CALL OLDFIL (MINP, inarg)
         CALL loadNetwork(trim(inarg),istat,0)
         if (istat == 0) then
            md_netfile = ' ' ; md_netfile = trim(inarg)
         endif
      ELSE IF (EXT .EQ. '.bmp' .OR. EXT .EQ. '.BMP' ) THEN
         CALL LOADBITMAP(inarg)
      ELSE IF (EXT .EQ. '.mdu' .OR. EXT .EQ. '.MDU' ) THEN
         CALL LOADMODEL(inarg)
      ELSE IF (EXT .EQ. '.xyz' .OR. EXT .EQ. '.XYZ' ) THEN
         CALL OLDFIL(MINP, inarg)
         CALL REASAM(MINP, 1)
      ELSE IF (EXT .EQ. '.asc' .OR. EXT .EQ. '.ASC' ) THEN
         CALL OLDFIL(MINP, inarg)
         call doclose(MINP)
         call read_samples_from_arcinfo(trim(inarg), 1, 0) ! indeed do not prompt anymore, so japrompt = 0
      ELSE IF (EXT .EQ. '.pol' .OR. EXT .EQ. '.POL' ) THEN
         CALL OLDFIL(MINP, inarg)
         CALL REAPOL(MINP, 0)
      ELSE IF (EXT .EQ. '.pli' .OR. EXT .EQ. '.PLI' ) THEN
         CALL OLDFIL(MINP, inarg)
         CALL REAPOL(MINP, 0)
      ELSE IF (EXT .EQ. '.spl' .OR. EXT .EQ. '.SPL' ) THEN
         CALL OLDFIL (MINP, inarg)
         CALL READSPLINES(MINP)
      ELSE IF (EXT .EQ. '.grd' .OR. EXT .EQ. '.GRD' ) THEN
         CALL OLDFIL(MINP, inarg)
         CALL REAgrid(MINP, inarg,ja)
      ELSE IF (EXT .EQ. '.rst' .OR. EXT .EQ. '.RST' ) THEN
         md_restartfile = trim(inarg)
      ELSE IF (EXT .EQ. '.cfg' .OR. EXT .EQ. '.CFG' ) THEN
         md_cfgfile = inarg
      ENDIF
   else
       call mess(LEVEL_INFO, 'File not found: '''//trim(inarg)//'''. Ignoring this commandline argument.')
   end if
   end subroutine loadfile

   subroutine savefile(inarg)

   USE m_missing
   use m_netw
   USE M_BOAT
   USE M_AFMETING
   USE M_SEASTATE
   use unstruc_model
   use unstruc_display
   use unstruc_messages
   use M_splines, only: increasespl, readsplines, writesplines
   USE M_SAMPLES
   use m_commandline_option
   use dfm_signals
   use gridoperations
   use m_monitoring_crosssections, only: increaseCrossSections
   use unstruc_netcdf, only : unc_write_net

   implicit none
   CHARACTER inarg*(*), EXT*4
   integer :: minp, n1, n2, istat  ! in this subroutine minp should be: mout

       ! Find file extention based on first full stop symbol '.' at the back of the string.
      N1  = INDEX (inarg,'.', .true.)
      N2  = len_trim(inarg)
      EXT = ' '
      EXT = inarg(N1:N2)
      IF (EXT .EQ. '.ldb' .OR. EXT .EQ. '.LDB' ) THEN
         CALL newFIL (MINP, inarg)
         CALL wriLAN (MINP)
      ELSE IF (EXT .EQ. '.net' .OR. (EXT .EQ. '.nc' .and. inarg(max(1,N1-4):max(1,N1-1)) == '_net') ) THEN
         call unc_write_net(inarg)
       ELSE IF (EXT .EQ. '.bmp' .OR. EXT .EQ. '.BMP' ) THEN
         ! CALL LOADBITMAP(inarg)
      ELSE IF (EXT .EQ. '.mdu' .OR. EXT .EQ. '.MDU' ) THEN
         call WriteMDUFile(inarg, istat)   
      ELSE IF (EXT .EQ. '.xyz' .OR. EXT .EQ. '.XYZ' ) THEN
         CALL newFIL(MINP, inarg)
         CALL wriSAM(MINP)
      ELSE IF (EXT .EQ. '.asc' .OR. EXT .EQ. '.ASC' ) THEN
         CALL newFIL(MINP, inarg)
         call wriSAM(MINP)
      ELSE IF (EXT .EQ. '.pol' .OR. EXT .EQ. '.POL' .or. EXT .EQ. '.pli' .OR. EXT .EQ. '.PLI' ) THEN
         CALL newFIL(MINP, inarg)
         CALL wripol(MINP)
      ELSE IF (EXT .EQ. '.spl' .OR. EXT .EQ. '.SPL' ) THEN
         CALL newFIL (MINP, inarg)
         CALL writeSPLINES(MINP)
      ELSE IF (EXT .EQ. '.grd' .OR. EXT .EQ. '.GRD' ) THEN
         CALL wrirgf(MINP, inarg)
      ELSE IF (EXT .EQ. '.rst' .OR. EXT .EQ. '.RST' ) THEN
         md_restartfile = trim(inarg)
      ELSE IF (EXT .EQ. '.cfg' .OR. EXT .EQ. '.CFG' ) THEN
         call save_displaysettings(inarg)
      ENDIF
     end subroutine savefile

