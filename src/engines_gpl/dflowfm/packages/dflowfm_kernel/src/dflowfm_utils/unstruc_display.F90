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

! m_WEARELT movet to gridgeom

MODULE M_DEVICES
  INTEGER           :: NPX,NPY,NCOLR,NDEV,NOPSYS,IWS,IHS
END MODULE M_DEVICES

module m_textlines
  double precision  :: txsize
  double precision  :: txxpos
  double precision  :: txypos
  character (len=60):: txlin(3)
end module m_textlines

module unstruc_colors

   USE m_WEARELT
   USE M_DEVICES
   use m_textlines
!! Centralizes color definitions for unstruc.
!! Color specifications are based on Interactor.

implicit none

    integer :: klvec=4, klaxs=30, klscl=221, kltex=3, klfra=31, klobs=227, klsam=33, klzm=31, klank=31, klprof=222, KLSRC=233 

    ! Color numbers for standard colors.
    integer :: ncolgray       = 255
    integer :: ncolred        = 252
    integer :: ncolyellow     = 251
    integer :: ncolgreen      = 250
    integer :: ncolcyan       = 249
    integer :: ncolblue       = 248
    integer :: ncolmagenta    = 247
    integer :: ncolmaroon     = 246
    integer :: ncoldarkgreen  = 245
    integer :: ncolteal       = 244
    integer :: ncolpink       = 243
    integer :: ncolorange     = 242
    integer :: ncollavender   = 241
    integer :: ncolbrown      = 240
    
    integer :: ncoldn    = 3   !< Design net
    integer :: ncolrn    = 211 !< Previous state net
    integer :: ncolnn    = 89  ! 203 !< Net node dots
    integer :: ncoldg    = 31  !< Design grid
    integer :: ncolrg    = 212 !< Previous state grid
    integer :: ncolln    = 120 !< Land boundary
    integer :: ncolsp    = 204 !< Splines
    integer :: ncoltx    = 210 !< Some textlines 
    integer :: ncolpl    = 221 !< Polygons
    integer :: ncolcrs   = 230 !< Cross sections
    integer :: ncolthd   = 231 !< Thin dams
    integer :: ncolfxw   = 232 !< Fixed weirs
    integer :: ncolmh    = 191 !< Fixed weirs
    integer :: ncolwarn1 = 191 ! warning1
    integer :: ncolwarn2 =  31 ! warning2
    integer :: ncolwarn3 =  22 ! warning3
    integer :: ncolhl    =  31 ! Highlight nodes/links
    integer :: ncolANA   =  63 ! 180! ANALYTIC SOLOUTIONS

    integer :: ncolblack = 254
    integer :: ncolwhite = 253
    
    ! colors in text screens
    ! 0 : Black       4 : Cyan
    ! 1 : Red         5 : Blue
    ! 2 : Yellow      6 : Magenta
    ! 3 : Green       7 : White

    INTEGER :: STDFOR = 0, STDBCK = 5,    &   !   std
               MNUFOR = 0, MNUBCK = 4,    &   !   choice menu's
               INPFOR = 0, INPBCK = 4,    &   !   input menu's
               ERRFOR = 1, ERRBCK = 7,    &   !   error messages
               LBLFOR = 7, LBLBCK = 5,    &   !   menu names
               LINFOR = 0, LINBCK = 4,    &   !   lines
               TOPFOR = 1, TOPBCK = 7,    &   !   top line
               HLPFOR = 7, HLPBCK = 5,    &   !   help window
               BOTFOR = 7, BOTBCK = 5,    &   !   page indication
               KEYFOR = 1, KEYBCK = 4,    &   !   key indication
               WNDFOR = 0, WNDBCK = 4,    &   !   menu indication, POPUP WINDOW HELP
               SHAFOR = 7, SHABCK = 0         !   menu indication, shadow behind input forms

    integer :: nbluep
    integer :: nblues
    integer :: ngreenp
    integer :: ngreens
    integer :: nredp
    integer :: nreds

    character(len=255) :: coltabfile  = ' '
    character(len=255) :: coltabfile2 = ' '


end module unstruc_colors

module unstruc_display
!! Handles all display settings and screen plotting for Unstruc
!! (Not yet, a lot is still in REST.F90 [AvD])

! 

use unstruc_colors
implicit none
#ifndef HAVE_DISPLAY
#define HAVE_DISPLAY 1
#endif
#if HAVE_DISPLAY==1
    integer :: jaGUI      = 1          !< GUI (1) or not (0)
#else
    integer :: jaGUI      = 0          !< GUI (1) or not (0)
#endif

    integer :: ntek       = 0
    integer :: plottofile = 0
    integer :: jadatetime = 0
    integer :: jareinitialize = 0



    ! Highlight certain net/flow node/link numbers
    integer :: nhlNetNode  = 0          !< Number of netnode  to be highlighted
    integer :: nhlNetLink  = 0          !< Number of netlink  to be highlighted
    integer :: nhlFlowNode = 0          !< Number of flownode to be highlighted
    integer :: nhlFlowLink = 0          !< Number of flowlink to be highlighted
    INTEGER :: NPOS(4)                  !< Size + position of HELP text screen
    integer :: jaHighlight =  0         !< Completely enable/disable highlighting.
 
    integer :: ndrawPol           = 2   !< Polygon, 1=No, 2=Regular, 3=plus numbers ZPL, 4=plus isocolour ZPL
    integer :: ndrawObs           = 2   !< Observationstation : 1='NO, 2=Cross, 3=Cross + name4=Polyfil,5='Polyfil + name,6=Cross+waterlevel,7=Cross+velocity magnitudes
    integer :: ndrawCrossSections = 5   !< how draw cross sections
    integer :: ndrawThinDams      = 2   !< show thin dams  0=no, 1=polylines, 2=net links
    integer :: ndrawFixedWeirs     = 1   !< show fixed weirs 0=no, 1=polylines, 2=flow links
    integer :: ndrawPart          = 2   !< Particles, 1=No, 2=Yes
    integer :: ndrawDots          = 2   !< dots, 1=No, 2=Yes
    integer :: ndrawStructures    = 1   !< structures, 1=No, 2=Yes (only symbols), 3=Yes (symbols and IDs)
    integer :: idisLink           = 0   !< Index of flowlink which is to be displayed with more information
  
    integer :: grwhydopt = 1            !< Groundwater & Hydrology display menu item

    integer :: numzoomshift       = 250 !< nr of steps in zoomshift
    double precision :: wetplot   = 0.001 !< only show wet waterlevel points if (hs>wetplot)
    double precision :: yfac      = 0.0   !< cheap perspective
    integer :: jafullbottomline   = 0     !<larger bottomline with more complete description in screen 
    double precision :: profmax(20) = -999d0 !< minmax axes of tekprofiles 
    double precision :: profmin(20) = -999d0 
    double precision :: ymn, zmn             ! for tekrailines  
    
    public dis_info_1d_link
    public plotStructures
    
    interface Write2Scr
      module procedure Write2ScrInt
      module procedure Write2ScrDouble
      module procedure Write2ScrChar
    end interface Write2Scr
    
contains

 
subroutine load_displaysettings(filename)
    use properties
    use unstruc_messages
    use dflowfm_version_module
    use m_missing
    use M_RAAITEK
    use M_isoscaleunit
    use m_transport, only: iconst_cur
    USE M_FLOW, only: kplot, nplot, kplotfrombedorsurface, kplotordepthaveraged
    use m_observations, only : jafahrenheit
    use m_sferic
!   use unstruc_opengl    ! circular dependency
 

    character(len=*), intent(in) :: filename
    
    type(tree_data), pointer :: dis_ptr
    character(len=2) :: nrstring
    integer :: istat, numdraw, i
    logical :: success, jawel
    integer :: jaeps, jaland
    
    integer          :: ndraw
    COMMON /DRAWTHIS/   ndraw(50)
   
    integer          :: ncols, nv,nis,nie,jaAUTO, KRGB(4)
    double precision :: VMAX,VMIN,DV,VAL
    COMMON /DEPMAX/     VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO
  
    integer          :: ncols2, nv2,nis2,nie2,jaAUTO2
    double precision :: VMAX2,VMIN2,DV2,VAL2
    COMMON /DEPMAX2/    VMAX2,VMIN2,DV2,VAL2(256),NCOLS2(256),NV2,NIS2,NIE2,JAAUTO2
   
    integer          :: nvec 
    double precision :: vfac, vfacforce
    COMMON /VFAC/       VFAC,VFACFORCE,NVEC
   
    integer          :: jaxis 
    double precision :: xleft, ybot
    COMMON /SCREENAREA/ XLEFT, YBOT,JAXIS
   
    integer          :: ndec 
    double precision :: xsc, ysc, scalesize
    COMMON /SCALEPOS/   XSC ,YSC, SCALESIZE,NDEC
   
    integer          :: NHCDEV, NUMHCOPTS, IHCOPTS
    COMMON /HARDCOPY/   NHCDEV,NUMHCOPTS,IHCOPTS(2,20)
   
    double precision :: tsize
    COMMON /TEXTSIZE/   TSIZE
     
    integer          :: jaopengl_loc
    double precision :: x, y, dy, asp


    ! Put .dis file into a property tree
    call tree_create(trim(filename), dis_ptr)
    call prop_file('ini',trim(filename), dis_ptr,istat)
    if (istat /= 0) then
        call mess(LEVEL_ERROR, 'Display settings file '''//trim(filename)//''' not found. Code: ', istat)
        return
    else
        call mess(LEVEL_DEBUG, 'Opened display settings file : ', trim(filename) )
    endif

    numdraw = 41
    do i=1,numdraw
       write (nrstring, '(I2)') i
       call prop_get_integer(dis_ptr, '*', 'ndraw('//trim(adjustl(nrstring))//')', ndraw(i), success)
    end do

    call prop_get_integer(dis_ptr, '*', 'grwhydopt', grwhydopt, success)
    
!   load active constituent number
    call prop_get_integer(dis_ptr, '*', 'ICONST', iconst_cur, success)

    call prop_Get_integer(dis_ptr, '*', 'ndrawpol           '  , ndrawpol        , success)
    call prop_Get_integer(dis_ptr, '*', 'ndrawobs           '  , ndrawobs        , success)
    call prop_Get_integer(dis_ptr, '*', 'ndrawcrosssections '  , ndrawcrosssections , success)


    call prop_Get_integer(dis_ptr, '*', 'NHCDEV       '  , NHCDEV          , success)
    call prop_Get_integer(dis_ptr, '*', 'JAEPS        '  , JAEPS           , success)
    call prop_Get_integer(dis_ptr, '*', 'JALAND       '  , JALAND          , success)
    call prop_Get_DOUBLE (dis_ptr, '*', 'CR           '  , CR              , success)
    call prop_Get_DOUBLE (dis_ptr, '*', 'TSIZE        '  , TSIZE           , success)
    call prop_Get_DOUBLE (dis_ptr, '*', 'dmiss        '  , dmiss           , success)
    call prop_Get_DOUBLE (dis_ptr, '*', 'XLEFT        '  , XLEFT           , success)
    call prop_Get_DOUBLE (dis_ptr, '*', 'YBOT         '  , YBOT            , success)
    call prop_Get_integer(dis_ptr, '*', 'JAXIS        '  , JAXIS           , success)
    call prop_Get_DOUBLE (dis_ptr, '*', 'VFAC         '  , VFAC            , success)
    call prop_Get_integer(dis_ptr, '*', 'nvec         '  , nvec            , success)
    call prop_Get_integer(dis_ptr, '*', 'NTEK         '  , NTEK            , success)
    call prop_Get_integer(dis_ptr, '*', 'PLOTTOFILE   '  , PLOTTOFILE      , success)
    call prop_Get_DOUBLE (dis_ptr, '*', 'ZMINrai      '  , ZMINrai         , success)
    call prop_Get_DOUBLE (dis_ptr, '*', 'ZMAXrai      '  , ZMAXrai         , success)
    call prop_Get_integer(dis_ptr, '*', 'jtextflow    '  , jtextflow       , success)
    call prop_Get_integer(dis_ptr, '*', 'numzoomshift '  , numzoomshift    , success)
    call prop_Get_integer(dis_ptr, '*', 'jaHighlight  '  , jaHighlight     , success)
    call prop_Get_integer(dis_ptr, '*', 'nhlNetNode   '  , nhlNetNode      , success)
    call prop_Get_integer(dis_ptr, '*', 'nhlNetLink   '  , nhlNetLink      , success)
    call prop_Get_integer(dis_ptr, '*', 'nhlFlowNode  '  , nhlFlowNode     , success)
    call prop_Get_integer(dis_ptr, '*', 'nhlFlowLink  '  , nhlFlowLink     , success)
    call prop_Get_DOUBLE (dis_ptr, '*', 'wetplot      '  , wetplot         , success)
    call prop_Get_DOUBLE (dis_ptr, '*', 'yfac         '  , yfac            , success)

    call prop_Get_integer(dis_ptr, '*', 'JAAUTO       '  , JAAUTO          , success)
    call prop_Get_integer(dis_ptr, '*', 'NV           '  , NV              , success)
    call prop_Get_DOUBLE (dis_ptr, '*', 'VMIN,        '  , VMIN            , success) ! remove muuuchch later
    call prop_Get_DOUBLE (dis_ptr, '*', 'VMIN         '  , VMIN            , success)
    call prop_Get_DOUBLE (dis_ptr, '*', 'VMAX         '  , VMAX            , success)
    call prop_Get_DOUBLE (dis_ptr, '*', 'DV           '  , DV              , success)
    DO I = 1,NV
       VAL(I) = VMIN + (I-1)*DV/(NV-1)
    ENDDO

    call prop_Get_integer(dis_ptr, '*', 'JAAUTO2      '  , JAAUTO2         , success)
    call prop_Get_integer(dis_ptr, '*', 'NV2          '  , NV2             , success)
    call prop_Get_DOUBLE (dis_ptr, '*', 'VMIN2        '  , VMIN2           , success)
    call prop_Get_DOUBLE (dis_ptr, '*', 'VMAX2        '  , VMAX2           , success)
    call prop_Get_DOUBLE (dis_ptr, '*', 'DV2          '  , DV2             , success)
    DO I = 1,NV2
       VAL2(I) = VMIN2 + (I-1)*DV2/(NV2-1)
    ENDDO
    
    call prop_Get_DOUBLE (dis_ptr, '*', 'XSC          '  , XSC             , success) 
    call prop_Get_DOUBLE (dis_ptr, '*', 'YSC          '  , YSC             , success)
    call prop_Get_DOUBLE (dis_ptr, '*', 'SCALESIZE    '  , SCALESIZE       , success)
    call prop_Get_INTEGER(dis_ptr, '*', 'NDEC         '  , NDEC            , success)
 
    call prop_Get_string (dis_ptr, '*', 'UNIT(1)      '  , UNIT(1)         , success)
    call prop_Get_string (dis_ptr, '*', 'PARAMTEX(1)  '  , PARAMTEX(1)     , success)
    call prop_Get_string (dis_ptr, '*', 'UNIT(2)      '  , UNIT(2)         , success)
    call prop_Get_string (dis_ptr, '*', 'PARAMTEX(2)  '  , PARAMTEX(2)     , success)

    call prop_Get_DOUBLE (dis_ptr, '*', 'X1           '  , X1              , success)
    call prop_Get_DOUBLE (dis_ptr, '*', 'Y1           '  , Y1              , success)
    call prop_Get_DOUBLE (dis_ptr, '*', 'X2           '  , X2              , success)

    call prop_Get_DOUBLE (dis_ptr, '*', 'X0           '  , X               , success) ! should override previous set
    call prop_Get_DOUBLE (dis_ptr, '*', 'Y0           '  , Y               , success)
    call prop_Get_DOUBLE (dis_ptr, '*', 'DYH          '  , DY              , success)
    if (.not. success) then ! to also use old cfg files
       x = 0.5d0*(x1+x2)
       call inqasp(asp)
       dy = (x2-x1)*asp
       y  = y1 + 0.5d0*dy
    endif

    call prop_Get_integer(dis_ptr, '*', 'SFERTEK      '  , JSFERTEK        , success)
    call setwynew(x,y,dy)

    ! Color scheme isolines
    call prop_get_string(dis_ptr, 'isocol', 'COLTABFILE', coltabfile)
    inquire(file = trim(coltabfile), exist = jawel)
    if (jawel) then
        call SETCOLTABFILE(coltabfile ,0)
    end if

    call prop_get_string(dis_ptr, 'isocol2', 'COLTABFILE2', coltabfile2)
    inquire(file = trim(coltabfile2), exist = jawel)
    if (jawel) then
        call SETCOLTABFILE(coltabfile2 ,1)
    end if

    jaopengl_loc = -1
    call prop_Get_integer(dis_ptr, '*', 'jaopengl'  , jaopengl_loc    , success)
    if (jaopengl_loc .ne. -1 ) then
       jaopengl_loc = 1 
       call iset_jaopengl(jaopengl_loc)
    end if
    
    call prop_get_integers(dis_ptr, '*', 'NCOLDG ', KRGB, 4 , success) ; if (success) NCOLDG   = KRGB(1) ; if (success) CALL SETINTRGB(KRGB) 
    call prop_get_integers(dis_ptr, '*', 'NCOLRG ', KRGB, 4 , success) ; if (success) NCOLRG   = KRGB(1) ; if (success) CALL SETINTRGB(KRGB) 
    call prop_get_integers(dis_ptr, '*', 'NCOLDN ', KRGB, 4 , success) ; if (success) NCOLDN   = KRGB(1) ; if (success) CALL SETINTRGB(KRGB) 
    call prop_get_integers(dis_ptr, '*', 'NCOLRN ', KRGB, 4 , success) ; if (success) NCOLRN   = KRGB(1) ; if (success) CALL SETINTRGB(KRGB) 
    call prop_get_integers(dis_ptr, '*', 'NCOLNN ', KRGB, 4 , success) ; if (success) NCOLNN   = KRGB(1) ; if (success) CALL SETINTRGB(KRGB) 
    call prop_get_integers(dis_ptr, '*', 'NCOLSP ', KRGB, 4 , success) ; if (success) NCOLSP   = KRGB(1) ; if (success) CALL SETINTRGB(KRGB) 
    call prop_get_integers(dis_ptr, '*', 'NCOLLN ', KRGB, 4 , success) ; if (success) NCOLLN   = KRGB(1) ; if (success) CALL SETINTRGB(KRGB) 
    call prop_get_integers(dis_ptr, '*', 'NCOLTX ', KRGB, 4 , success) ; if (success) NCOLTX   = KRGB(1) ; if (success) CALL SETINTRGB(KRGB) 
    call prop_get_integers(dis_ptr, '*', 'NCOLPL ', KRGB, 4 , success) ; if (success) NCOLPL   = KRGB(1) ; if (success) CALL SETINTRGB(KRGB) 
    call prop_get_integers(dis_ptr, '*', 'NCOLCRS', KRGB, 4 , success) ; if (success) NCOLCRS  = KRGB(1) ; if (success) CALL SETINTRGB(KRGB) 
    call prop_get_integers(dis_ptr, '*', 'NCOLTHD', KRGB, 4 , success) ; if (success) NCOLTHD  = KRGB(1) ; if (success) CALL SETINTRGB(KRGB) 
    call prop_get_integers(dis_ptr, '*', 'NCOLFXW', KRGB, 4 , success) ; if (success) NCOLFXW  = KRGB(1) ; if (success) CALL SETINTRGB(KRGB) 
    call prop_get_integers(dis_ptr, '*', 'NCOLHL ', KRGB, 4 , success) ; if (success) NCOLHL   = KRGB(1) ; if (success) CALL SETINTRGB(KRGB) 
    call prop_get_integers(dis_ptr, '*', 'KLVEC  ', KRGB, 4 , success) ; if (success) KLVEC    = KRGB(1) ; if (success) CALL SETINTRGB(KRGB) 
    call prop_get_integers(dis_ptr, '*', 'KLAXS  ', KRGB, 4 , success) ; if (success) KLAXS    = KRGB(1) ; if (success) CALL SETINTRGB(KRGB) 
    call prop_get_integers(dis_ptr, '*', 'KLSCL  ', KRGB, 4 , success) ; if (success) KLSCL    = KRGB(1) ; if (success) CALL SETINTRGB(KRGB) 
    call prop_get_integers(dis_ptr, '*', 'KLTEX  ', KRGB, 4 , success) ; if (success) KLTEX    = KRGB(1) ; if (success) CALL SETINTRGB(KRGB) 
    call prop_get_integers(dis_ptr, '*', 'KLOBS  ', KRGB, 4 , success) ; if (success) KLOBS    = KRGB(1) ; if (success) CALL SETINTRGB(KRGB) 
    call prop_get_integers(dis_ptr, '*', 'KLPROF ', KRGB, 4 , success) ; if (success) KLPROF   = KRGB(1) ; if (success) CALL SETINTRGB(KRGB) 
    call prop_get_integers(dis_ptr, '*', 'KLSRC  ', KRGB, 4 , success) ; if (success) KLSRC    = KRGB(1) ; if (success) CALL SETINTRGB(KRGB) 
    
    call prop_get_integer (dis_ptr, '*', 'NREDS  ', NREDS   , success) 
    call prop_get_integer (dis_ptr, '*', 'NGREENS', NGREENS , success) 
    call prop_get_integer (dis_ptr, '*', 'NBLUES ', NBLUES  , success) 
    call prop_get_integer (dis_ptr, '*', 'NREDP  ', NREDP   , success) 
    call prop_get_integer (dis_ptr, '*', 'NGREENP', NGREENP , success) 
    call prop_get_integer (dis_ptr, '*', 'NBLUEP ', NBLUEP  , success) 

    call prop_get_integer (dis_ptr, '*', 'kplotbedsur', kplotfrombedorsurface, success)
    call prop_get_integer (dis_ptr, '*', 'kplotordepthaveraged', kplotordepthaveraged, success)
    call prop_get_integer (dis_ptr, '*', 'kplot', kplot, success)
    call prop_get_integer (dis_ptr, '*', 'nplot', nplot, success) 
    
    call prop_get_integer (dis_ptr, '*', 'jaFahrenheit', jaFahrenheit, success)

    call prop_get_double  (dis_ptr, '*', 'profmax(1)', profmax(1), success)
    call prop_get_double  (dis_ptr, '*', 'profmin(1)', profmin(1), success)
    call prop_get_double  (dis_ptr, '*', 'profmax(2)', profmax(2), success)
    call prop_get_double  (dis_ptr, '*', 'profmin(2)', profmin(2), success)
    
    
    RCIR  = CR*(X2 - X1)
    VFAC  = MAX( 0d0, VFAC)
    VFACFORCE  = MAX( 0d0, VFACFORCE)
    XLEFT = MAX( 0d0,(MIN(XLEFT,0.25d0) ) )
    YBOT  = MAX( 0d0,(MIN(YBOT ,0.25d0) ) )
    JAXIS = MIN( 1  ,(MAX(JAXIS,0) ) )
    IF (JAXIS .EQ. 1) THEN
       IF (XLEFT .EQ. 0) XLEFT = .15
       IF (YBOT  .EQ. 0) YBOT  = .10
    ENDIF
 
    DO I = 1,NUMHCOPTS
       IF (IHCOPTS(1,I) .EQ. 22) IHCOPTS(2,I) = JAEPS
       IF (IHCOPTS(1,I) .EQ. 5)  IHCOPTS(2,I) = JALAND
    ENDDO

    CALL SETTEXTSIZE()
    if (plottofile == 1) then
        ndraw(10) = 1
    end if

    
end subroutine load_displaysettings


subroutine save_displaysettings(filename)
    use properties
    use unstruc_messages
    use dflowfm_version_module
    use m_missing
    use M_RAAITEK
    use m_sferzoom
    use M_isoscaleunit
    use m_transport, only: iconst_cur
    use m_flow
    use m_observations
    use m_sferic
!   use unstruc_opengl    ! circular dependency


    character(len=*), intent(in) :: filename

    type(tree_data), pointer     :: dis_ptr
    character(len=20)            :: rundat
    integer                      :: mfil, istat, i, KRGB(4)
    
    
    integer          :: ndraw
    COMMON /DRAWTHIS/   ndraw(50)
   
    integer          :: ncols, nv,nis,nie,jaAUTO
    double precision :: VMAX,VMIN,DV,VAL
    COMMON /DEPMAX/     VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO
  
    integer          :: ncols2, nv2,nis2,nie2,jaAUTO2
    double precision :: VMAX2,VMIN2,DV2,VAL2
    COMMON /DEPMAX2/    VMAX2,VMIN2,DV2,VAL2(256),NCOLS2(256),NV2,NIS2,NIE2,JAAUTO2
   
    integer          :: nvec 
    double precision :: vfac, vfacforce
    COMMON /VFAC/       VFAC,VFACFORCE,NVEC
   
    integer          :: jaxis 
    double precision :: xleft, ybot
    COMMON /SCREENAREA/ XLEFT, YBOT,JAXIS
   
    integer          :: ndec 
    double precision :: xsc, ysc, scalesize
    COMMON /SCALEPOS/   XSC ,YSC, SCALESIZE,NDEC
   
    integer          :: NHCDEV, NUMHCOPTS, IHCOPTS
    COMMON /HARDCOPY/   NHCDEV,NUMHCOPTS,IHCOPTS(2,20)
   
    double precision :: tsize
    COMMON /TEXTSIZE/   TSIZE
      
    integer :: jaeps, jaland
   
    integer, external :: iget_jaopengl

    call newfil(mfil, filename)

    ! Put .dis file into a property tree
    call tree_create('displaysettings', dis_ptr)

    call prop_set(dis_ptr, '*', 'ndraw(1) ' , ndraw(1) , ' ! clear screen yes/no                                                                                              ')
    call prop_set(dis_ptr, '*', 'ndraw(2) ' , ndraw(2) , ' ! display network                                                                                                  ')
    call prop_set(dis_ptr, '*', 'ndraw(3) ' , ndraw(3) , ' ! display landboundary                                                                                             ')
    call prop_set(dis_ptr, '*', 'ndraw(4) ' , ndraw(4) , ' ! void                                                                                                             ')
    call prop_set(dis_ptr, '*', 'ndraw(5) ' , ndraw(5) , ' ! void , this comment is copied from inidat, should be a copy                                                      ')
    call prop_set(dis_ptr, '*', 'ndraw(6) ' , ndraw(6) , ' ! void                                                                                                             ')
    call prop_set(dis_ptr, '*', 'ndraw(7) ' , ndraw(7) , ' ! WHICH netLINk VALue, 1 = NO  2=link nr, 3=node nrs based on links                                                ')
    call prop_set(dis_ptr, '*', 'ndraw(8) ' , ndraw(8) , ' ! WHICH netNODe VALue, 1 = NO, 2=node nr, 3=nr of links, 4=link nrs based on nodes                                 ')
    call prop_set(dis_ptr, '*', 'ndraw(9) ' , ndraw(9) , ' ! NORMAL OR PERSPECTIVE                                                                                            ')
    call prop_set(dis_ptr, '*', 'ndraw(10)' , ndraw(10), ' ! also plot to file yes/no                                                                                         ')
    call prop_set(dis_ptr, '*', 'ndraw(11)' , ndraw(11), ' ! HOW HISPLAY LINVAL, ,Linkhow: 1=no, 2=numbers, 3=isofil smooth, 4 = isofil, 5=dots                               ')
    call prop_set(dis_ptr, '*', 'ndraw(12)' , ndraw(12), ' ! 1=nodes, 2=links, 3=nodes+links, 4 = no isoscale                                                                 ')
    call prop_set(dis_ptr, '*', 'ndraw(13)' , ndraw(13), ' ! 1 !0  ! SHOW UV VECTORS AND FLOW FORCES                                                                          ')
    call prop_set(dis_ptr, '*', 'ndraw(14)' , ndraw(14), ' ! reserved for disval                                                                                              ')
    call prop_set(dis_ptr, '*', 'ndraw(15)' , ndraw(15), ' ! display splines                                                                                                  ')
    call prop_set(dis_ptr, '*', 'ndraw(16)' , ndraw(16), ' ! display PREVIOUS NETWORK                                                                                         ')
    call prop_set(dis_ptr, '*', 'ndraw(17)' , ndraw(17), ' ! void                                                                                                             ')
    call prop_set(dis_ptr, '*', 'ndraw(18)' , ndraw(18), ' ! Sideview, 1 = no, 2=small, 3=larger, 4=largest                                                                                                  ')
    call prop_set(dis_ptr, '*', 'ndraw(19)' , ndraw(19), ' ! HOW DISPLAY NODEVAL, Nodehow: 1=no, 2=numbers, 3=isofil smooth, 4 = isofil, 5=dots                               ')
    call prop_set(dis_ptr, '*', 'ndraw(20)' , ndraw(20), ' ! void                                                                                                             ')
    call prop_set(dis_ptr, '*', 'ndraw(21)' , ndraw(21), ' ! void                                                                                                             ')
    call prop_set(dis_ptr, '*', 'ndraw(22)' , ndraw(22), ' ! SHOW netcell types tri, quads penta, hexa                                                                        ')
    call prop_set(dis_ptr, '*', 'ndraw(26)' , ndraw(26), ' ! 1 = BITMAP                                                                                                       ')
    call prop_set(dis_ptr, '*', 'ndraw(27)' , ndraw(27), ' ! 1 = nothing, 2 = both surf and bot, 3 just bot, 4 just surf                                                      ')
    call prop_set(dis_ptr, '*', 'ndraw(28)' , ndraw(28), ' ! values at Flow Nodes 1=no, 2=s1, 3=bl, 4=ba, 5=v1    , nodewhat                                                  ')
    call prop_set(dis_ptr, '*', 'ndraw(29)' , ndraw(29), ' ! values at Flow Links 1=no, 2=u1, 3=q1, 4=au,         , linkwhat                                                  ')
    call prop_set(dis_ptr, '*', 'ndraw(30)' , ndraw(30), ' ! do not show all flow links                                                                                       ')
    call prop_set(dis_ptr, '*', 'ndraw(31)' , ndraw(31), ' ! do not show show values at cell corners, 2=ucnx, 3=ucny                                                          ')
    call prop_set(dis_ptr, '*', 'ndraw(32)' , ndraw(32), ' ! show samples coloured dot                                                                                        ')
    call prop_set(dis_ptr, '*', 'ndraw(33)' , ndraw(33), ' ! show values at net cells, 2=number, 3=aspect ratio, 4=orientation vectors, 5=aspect ratio and orientation vectors')
    call prop_set(dis_ptr, '*', 'ndraw(34)' , ndraw(34), ' ! Banf, 0=no, 1 =seqtr, 3=(seqtr-s), tekbanf                                                                       ')
    call prop_set(dis_ptr, '*', 'ndraw(35)' , ndraw(35), ' ! yes draw reference profiles (only for 3D)                                                                        ')
    call prop_set(dis_ptr, '*', 'ndraw(36)' , ndraw(36), ' ! values on flow nodes minus plotlin default 0                                                                     ')
    call prop_set(dis_ptr, '*', 'ndraw(37)' , ndraw(37), ' ! 0 = no, 1 = probe, no boundaryvalues, 2=probe+boundaryvalues                                                     ')
    call prop_set(dis_ptr, '*', 'ndraw(38)' , ndraw(38), ' ! display curvilinear grid: 0 = no, 1 = lines, 2 = count netw                                                     ')
    call prop_set(dis_ptr, '*', 'ndraw(39)' , ndraw(39), ' ! show bedlevels (0:no, 1:yes)                                                         ')
    call prop_set(dis_ptr, '*', 'ndraw(40)' , ndraw(40), ' ! show waterbalance (0:no, 1:yes)                                                         ')
    call prop_set(dis_ptr, '*', 'ndraw(41)' , ndraw(41), ' ! show sorsin (0:no, 1:yes source=white, sink=black, 2=1+names)                                                         ')

    call prop_set(dis_ptr, '*', 'grwhydopt' , grwhydopt, ' ! suboption display flow nodes > grw&hydrology parameters')
    
    call prop_set(dis_ptr, '*', 'ndrawpol'  , ndrawpol , ' ! Polygon, 1=No, 2=Regular, 3=plus numbers ZPL, 4=plus isocolour ZPL                                               ')
    call prop_set(dis_ptr, '*', 'ndrawobs'  , ndrawobs , ' ! 1=NO, 2=Cross, 3=Cross + name4=Polyfil,5=Polyfil + name,6=Cross+waterlevel,7=Cross+velocity magnitudes           ')
    call prop_set(dis_ptr, '*', 'ndrawcrosssections'  , ndrawcrosssections , ' ! 1=NO, etc                                                                                      ')
    
!   save active constituent number
    call prop_set_integer(dis_ptr, '*', 'ICONST', iconst_cur, ' ! active constituent number')

    DO I = 1,NUMHCOPTs
      IF (IHCOPTS(1,I) .EQ. 22) JAEPS  = IHCOPTS(2,I)
      IF (IHCOPTS(1,I) .EQ. 5)  JALAND = IHCOPTS(2,I)
    ENDDO

    call prop_set(dis_ptr, '*', 'NHCDEV       '  , NHCDEV          )
    call prop_set(dis_ptr, '*', 'JAEPS        '  , JAEPS           )
    call prop_set(dis_ptr, '*', 'JALAND       '  , JALAND          )
    call prop_set(dis_ptr, '*', 'CR           '  , CR              )
    call prop_set(dis_ptr, '*', 'TSIZE        '  , TSIZE           )
    call prop_set(dis_ptr, '*', 'dmiss        '  , dmiss           )
    call prop_set(dis_ptr, '*', 'XLEFT        '  , XLEFT           )
    call prop_set(dis_ptr, '*', 'YBOT         '  , YBOT            )
    call prop_set(dis_ptr, '*', 'JAXIS        '  , JAXIS           )
    call prop_set(dis_ptr, '*', 'VFAC         '  , VFAC            )
    call prop_set(dis_ptr, '*', 'nvec         '  , nvec            )
    call prop_set(dis_ptr, '*', 'NTEK         '  , NTEK            )
    call prop_set(dis_ptr, '*', 'PLOTTOFILE   '  , PLOTTOFILE      )
    call prop_set(dis_ptr, '*', 'ZMINrai      '  , ZMINrai         )
    call prop_set(dis_ptr, '*', 'ZMAXrai      '  , ZMAXrai         )
    call prop_set(dis_ptr, '*', 'jtextflow    '  , jtextflow       )
    call prop_set(dis_ptr, '*', 'numzoomshift '  , numzoomshift    )
    call prop_set(dis_ptr, '*', 'jaHighlight  '  , jaHighlight     )
    call prop_set(dis_ptr, '*', 'nhlNetNode   '  , nhlNetNode      )
    call prop_set(dis_ptr, '*', 'nhlNetLink   '  , nhlNetLink      )
    call prop_set(dis_ptr, '*', 'nhlFlowNode  '  , nhlFlowNode     )
    call prop_set(dis_ptr, '*', 'nhlFlowLink  '  , nhlFlowLink     )
    call prop_set(dis_ptr, '*', 'wetplot      '  , wetplot         )
    call prop_set(dis_ptr, '*', 'yfac         '  , yfac            )

    call prop_set(dis_ptr, '*', 'JAAUTO       '  , JAAUTO          )
    call prop_set(dis_ptr, '*', 'NV           '  , NV              )
    call prop_set(dis_ptr, '*', 'VMIN,        '  , VMIN            )
    call prop_set(dis_ptr, '*', 'VMAX         '  , VMAX            )
    call prop_set(dis_ptr, '*', 'DV           '  , DV              )

    call prop_set(dis_ptr, '*', 'JAAUTO2      '  , JAAUTO2         )
    call prop_set(dis_ptr, '*', 'NV2          '  , NV2             )
    call prop_set(dis_ptr, '*', 'VMIN2        '  , VMIN2           )
    call prop_set(dis_ptr, '*', 'VMAX2        '  , VMAX2           )
    call prop_set(dis_ptr, '*', 'DV2          '  , DV2             )

    call prop_set(dis_ptr, '*', 'XSC          '  , XSC             )
    call prop_set(dis_ptr, '*', 'YSC          '  , YSC             )
    call prop_set(dis_ptr, '*', 'SCALESIZE    '  , SCALESIZE       )
    call prop_set(dis_ptr, '*', 'NDEC         '  , NDEC            )
 
    call prop_set(dis_ptr, '*', 'UNIT(1)      '  , UNIT(1)         )
    call prop_set(dis_ptr, '*', 'PARAMTEX(1)  '  , PARAMTEX(1)     )
    call prop_set(dis_ptr, '*', 'UNIT(2)      '  , UNIT(2)         )
    call prop_set(dis_ptr, '*', 'PARAMTEX(2)  '  , PARAMTEX(2)     )

   ! call prop_set(dis_ptr, '*', 'X1           '  , X1              )
   ! call prop_set(dis_ptr, '*', 'Y1           '  , Y1              )
   ! call prop_set(dis_ptr, '*', 'X2           '  , X2              )

    call prop_set(dis_ptr, '*', 'X0           '  , X0              ) 
    call prop_set(dis_ptr, '*', 'Y0           '  , Y0              )
    call prop_set(dis_ptr, '*', 'DYH          '  , DYH             )
    call prop_set(dis_ptr, '*', 'SFERTEK      '  , JSFERTEK        )

    call prop_set_string(dis_ptr, '*', 'COLTABFILE',  coltabfile)
    call prop_set_string(dis_ptr, '*', 'COLTABFILE2', coltabfile2)

    call prop_set(dis_ptr, '*', 'jaopengl', iget_jaopengl())
   
    KRGB(1) = NCOLDG  ; CALL GETINTRGB(KRGB) ; call prop_set(dis_ptr, '*', 'NCOLDG ', KRGB )
    KRGB(1) = NCOLRG  ; CALL GETINTRGB(KRGB) ; call prop_set(dis_ptr, '*', 'NCOLRG ', KRGB )
    KRGB(1) = NCOLDN  ; CALL GETINTRGB(KRGB) ; call prop_set(dis_ptr, '*', 'NCOLDN ', KRGB )
    KRGB(1) = NCOLRN  ; CALL GETINTRGB(KRGB) ; call prop_set(dis_ptr, '*', 'NCOLRN ', KRGB )
    KRGB(1) = NCOLNN  ; CALL GETINTRGB(KRGB) ; call prop_set(dis_ptr, '*', 'NCOLNN ', KRGB )
    KRGB(1) = NCOLSP  ; CALL GETINTRGB(KRGB) ; call prop_set(dis_ptr, '*', 'NCOLSP ', KRGB )
    KRGB(1) = NCOLLN  ; CALL GETINTRGB(KRGB) ; call prop_set(dis_ptr, '*', 'NCOLLN ', KRGB )
    KRGB(1) = NCOLTX  ; CALL GETINTRGB(KRGB) ; call prop_set(dis_ptr, '*', 'NCOLTX ', KRGB )
    KRGB(1) = NCOLPL  ; CALL GETINTRGB(KRGB) ; call prop_set(dis_ptr, '*', 'NCOLPL ', KRGB )
    KRGB(1) = NCOLCRS ; CALL GETINTRGB(KRGB) ; call prop_set(dis_ptr, '*', 'NCOLCRS', KRGB )
    KRGB(1) = NCOLTHD ; CALL GETINTRGB(KRGB) ; call prop_set(dis_ptr, '*', 'NCOLTHD', KRGB )
    KRGB(1) = NCOLFXW ; CALL GETINTRGB(KRGB) ; call prop_set(dis_ptr, '*', 'NCOLFXW', KRGB )
    KRGB(1) = NCOLHL  ; CALL GETINTRGB(KRGB) ; call prop_set(dis_ptr, '*', 'NCOLHL ', KRGB )
    KRGB(1) = KLVEC   ; CALL GETINTRGB(KRGB) ; call prop_set(dis_ptr, '*', 'KLVEC  ', KRGB )
    KRGB(1) = KLAXS   ; CALL GETINTRGB(KRGB) ; call prop_set(dis_ptr, '*', 'KLAXS  ', KRGB )
    KRGB(1) = KLSCL   ; CALL GETINTRGB(KRGB) ; call prop_set(dis_ptr, '*', 'KLSCL  ', KRGB )
    KRGB(1) = KLTEX   ; CALL GETINTRGB(KRGB) ; call prop_set(dis_ptr, '*', 'KLTEX  ', KRGB )
    KRGB(1) = KLOBS   ; CALL GETINTRGB(KRGB) ; call prop_set(dis_ptr, '*', 'KLOBS  ', KRGB )
    KRGB(1) = KLPROF  ; CALL GETINTRGB(KRGB) ; call prop_set(dis_ptr, '*', 'KLPROF ', KRGB )
    KRGB(1) = KLSRC   ; CALL GETINTRGB(KRGB) ; call prop_set(dis_ptr, '*', 'KLSRC  ', KRGB )
    
    call prop_set(dis_ptr, '*', 'NREDS  ', NREDS    )
    call prop_set(dis_ptr, '*', 'NGREENS', NGREENS  )
    call prop_set(dis_ptr, '*', 'NBLUES ', NBLUES   )
    call prop_set(dis_ptr, '*', 'NREDP  ', NREDP    )
    call prop_set(dis_ptr, '*', 'NGREENP', NGREENP  )
    call prop_set(dis_ptr, '*', 'NBLUEP ', NBLUEP   )

    call prop_set(dis_ptr, '*', 'kplotbedsur', kplotfrombedorsurface)
    call prop_set(dis_ptr, '*', 'kplotordepthaveraged', kplotordepthaveraged)
    call prop_set(dis_ptr, '*', 'kplot', kplot)
    call prop_set(dis_ptr, '*', 'nplot', nplot)
    
    call prop_set(dis_ptr, '*', 'jaFahrenheit', jaFahrenheit)
    
    call prop_set(dis_ptr, '*', 'profmax(1)', profmax(1))
    call prop_set(dis_ptr, '*', 'profmin(1)', profmin(1))
    call prop_set(dis_ptr, '*', 'profmax(2)', profmax(2))
    call prop_set(dis_ptr, '*', 'profmin(2)', profmin(2))
    
    call datum(rundat)
    write(mfil, '(a,a)') '# Generated on ', trim(rundat)
    write(mfil, '(a,a)') '# ', trim(version_full)
    call prop_write_inifile(mfil, dis_ptr, istat)


   call doclose(mfil)
end subroutine save_displaysettings

!> Plots all observation points in the current viewport
subroutine plotObservations() ! TEKOBS
    
    use m_observations
    use M_FLOWGEOM
    use m_flow
    use m_transport, only: NUMCONST, itemp, ITRA1, ITRAN, constituents

    integer      :: n, NN, K, kb, kt
    character*40 :: tex
    double precision  :: znod, temb, temt
    logical inview

    if (ndrawobs == 1 ) return

    call IGrCharJustify('L')
    call settextsizefac(1.0d0)

    do n  = 1, numobs+nummovobs
       if (.not. inview(xobs(n), yobs(n))) cycle

       call setcol(klobs)

       if (ndrawobs .ne. 4 .and. ndrawobs .ne. 5 ) then
            if (n > numobs) then
                ! It is a moving obs:
                call plotDiamond(xobs(n), yobs(n))
            else
                call plotCross(xobs(n), yobs(n))
            end if
       endif
       if (ndrawobs == 3) then
           call settextsizefac(1.5d0)
           call igrcharfont(7)
           call gtext(' '//trim(namobs(n)), xobs(n), yobs(n), klobs)
           call igrcharfont(1)
       endif

       K  = KOBS(N)
       IF (K > 0) THEN
          if (ndrawobs == 4 .or. ndrawobs == 5 ) then
             nn = size( nd(K)%x )
             call PFILLER(nd(k)%x, nd(k)%y, nn,klobs,klobs)
             if (ndrawobs == 5) then
                call gtext(' '//trim(namobs(n)), xobs(n), yobs(n), 221)
             endif
          else if (ndrawobs == 6) then
             tex = '           (m)' 
             write (tex,'(f10.4)') s1(k)
             call gtext(tex(1:14), xobs(n), yobs(n), ncolblack)
          else if (ndrawobs == 7) then
             tex = '           (m)' 
             write (tex,'(f10.4)') s1(k) - bl(k)
             call gtext(tex(1:14), xobs(n), yobs(n), ncolblack)   
          else if (ndrawobs == 8) then
             write (tex,'(f10.4)') sqrt(ucx(k)*ucx(k) + ucy(k)*ucy(k) )
             call gtext(tex(1:14), xobs(n), yobs(n), ncolblack)
          else if (ndrawobs == 9) then
             write (tex,'(f10.4)') znod(k) 
             call gtext(tex(1:14), xobs(n), yobs(n), ncolblack)   
          else if (ndrawobs == 10) then
             if (kmx > 0) then 
                call getkbotktop(k,kb,kt)
                if (jaFahrenheit == 0) then 
                   temt = constituents(itemp,kt) 
                   temb = constituents(itemp,kb)
                else
                   temt = 32d0 + (9d0/5d0)*constituents(itemp,kt)
                   temb = 32d0 + (9d0/5d0)*constituents(itemp,kb)
                endif     
                write (tex,'(2f6.1)') temt, temb
                call gtext(tex(1:14), xobs(n), yobs(n), ncolblack)
             else 
                write (tex,'(2f6.1)') constituents(itemp,k)
                call gtext(tex(1:14), xobs(n), yobs(n), ncolblack)
             endif   
          else if (ndrawobs == 11) then
             write (tex,'(I4.0)') k 
             call gtext(tex(1:4), xobs(n), yobs(n), ncolblack)     
          endif
       ENDIF
    end do

end subroutine plotObservations


subroutine plotSplines(m1, m2, ncol)
    USE M_SPLINES
    use m_alloc

    implicit none

    integer, intent(in), optional :: m1
    integer, intent(in), optional :: m2
    integer, intent(in), optional :: ncol

    integer :: m1_, m2_, ncol_
    integer :: m, n2, numpi, numnew

    double precision, allocatable, dimension(:) :: xlist, ylist

!   allocate
    allocate(xlist(1), ylist(1))

    if (present(m1)) then
        m1_ = m1
    else
        m1_ = 1
    endif
    if (present(m2)) then
        m2_ = m2
    else
        m2_ = mcs
    endif
    if (present(ncol)) then
        ncol_ = ncol
    else
        ncol_ = ncolsp
    endif


    n2 = 0
    if (m1_ > 0) then
        do m = m1_,m2_
            CALL NUMP(m,NUMPI)

!           reallocate if necessary
            if ( numpi.gt.ubound(xlist,1) ) then
               numnew = int(1.2d0*dble(numpi))+1
               call realloc(xlist,numnew)
               call realloc(ylist,numnew)
            end if
            xlist(1:numpi) = xsp(m,1:numpi)
            ylist(1:numpi) = ysp(m,1:numpi)

            call plotSpline(xlist, ylist, numpi, ncol_)
        enddo
    endif

!   deallocate
    deallocate(xlist, ylist)

    return
end subroutine plotSplines


subroutine plotSpline(xh, yh, numpi, ncol)
    use m_wearelt
    double precision, dimension(numpi), intent(in) :: xh, yh
    integer, intent(in) :: numpi
    integer, intent(in) :: ncol

    !integer :: imax = 500 ! TODO: uit DIMENS [AvD]
!    double precision :: XH2(1000), YH2(1000)
    double precision, allocatable, dimension(:) :: xh2, yh2
    double precision :: xk, yk, tn


    integer :: ndraw
    COMMON /DRAWTHIS/ ndraw(50)
    integer :: i, met, k, numk
    MET = NDRAW(15)

    if (met == 0) return

!   allocate
    allocate(xh2(numpi), yh2(numpi))

    NUMK  = 20
    CALL SETCOL(NCOL)

    IF (NUMPI .EQ. 1) THEN
        CALL MOVABS(XH(1),YH(1))
        IF (MET .LE. 2) CALL CIR(1.4*RCIR)
    ELSE IF (NUMPI .GT. 1) THEN
        CALL MOVABS(XH(1),YH(1))
        IF (MET .LE. 2) CALL CIR(1.4*RCIR)
        CALL SPLINE(XH,NUMPI,XH2)
        CALL SPLINE(YH,NUMPI,YH2)

         do I = 1,NUMPI-1
            do k = 1,NUMK
               TN = (I - 1) + dble(K) / dble(NUMK)
               CALL SPLINT(XH,XH2,NUMPI,TN,XK)
               CALL SPLINT(YH,YH2,NUMPI,TN,YK)
               CALL LNABS(XK,YK)
            end do
            IF (MET .LE. 2) CALL CIR(RCIR)
    end do
    ENDIF

!   allocate
    deallocate(xh2, yh2)

    RETURN
    ! TODO: M,N numbers (tekadministratie) [AvD]
end subroutine plotSpline


subroutine plotCrossSections() ! tekcrs
    use m_monitoring_crosssections

    integer :: i, met, jaArrow
    character :: tex*40

    met = ndrawCrosssections
    if (met == 1) return

    if (met >= 3) then
        jaArrow = 1
    else
        jaArrow = 0
    end if

    call thicklinetexcol(ncolcrs) 

    do i=1,ncrs   ! ToDo: writing labels only after first time step
        tex = ' '
        if (met == 4) then
            tex = trim(crs(i)%name)
        else if (met == 5) then
            tex = '1234567890 m3/s'
            call write_num_label(10, 3, crs(i)%sumvalcur(IPNT_Q1C)) ! discharge
        else if (met == 6) then
            tex = '1234567890 m2'
            call write_num_label(10, 3, crs(i)%sumvalcur(IPNT_AUC)) ! area
        else if (met == 7) then
            tex = '1234567890 m/s'
            call write_num_label(10, 3, crs(i)%sumvalcur(IPNT_U1A)) ! ave velocity
        else if (met == 8) then
            tex = '1234567890 m'
            call write_num_label(10, 4, crs(i)%sumvalcur(IPNT_S1A)) ! ave. waterlevel
        else if (met == 9) then
            tex = '1234567890 m'
            call write_num_label(10, 4, crs(i)%sumvalcur(IPNT_HUA)) ! ave. waterdepth 
        else if (met == 10) then
            tex = '1234567890 c*m3/s'
            call write_num_label(10, 4, crs(i)%sumvalcur(IPNT_HUA+1)) !  transport 
        else if (met == 11) then
            tex = '1234567890 c*m3/s'
            call write_num_label(10, 4, crs(i)%sumvalcur(IPNT_HUA+2)) !  transport 
        endif

        call plotCrossSectionPath(crs(i)%path, 2, ncolcrs, jaArrow, tex)
    end do
    
    call resetlinesizesetc()
    
contains
    !> Plot on TEX label, with certain nr of digits. UNLESS too big, then as integer.
    !! Make sure to keep TEX string length in line with numw variable (10?).
    subroutine write_num_label(numw, numd, val)
        implicit none
        integer, intent(in) :: numw !< Available chars (10?)
        integer, intent(in) :: numd !< Num digits preferred (3?)
        double precision, intent(in) :: val !< Value to be printed

        character(len=7) :: fmt
        fmt = '(f10.3)'
        if (val > -1d0 * 10**(numw-numd-2) .and. val < 10**(numw-numd-1)) then
            write(fmt(3:4), '(i2)') numw
            write(fmt(6:6), '(i1)') numd
            write(tex(1:numw), fmt) val
        else
            if (numw < 10) then
                fmt = '(i1)   '
                write(fmt(3:3), '(i1)') numw
            else
                fmt = '(i10)  '
                write(fmt(3:4), '(i2)') numw
            end if
            write(tex(1:numw),trim(fmt)) int(val,selected_int_kind(15)) ! discharge
        end if
    end subroutine write_num_label
end subroutine plotCrossSections


subroutine plotThinDams()
    use m_thindams

    integer :: i

    if (ndrawThinDams == 0 .or. nthd == 0) return

    call thicklinetexcol(ncolthd) 
    
    do i=1,nthd
        call plotCrossSectionPath(thd(i), ndrawThinDams, ncolthd, 0, ' ')
    end do
        
    call resetlinesizesetc()
end subroutine plotThinDams



subroutine plotFixedWeirs()
   use m_fixedweirs 
   use m_flowgeom, only : lnx, lncn, bob
   use m_flow,     only : hu, isimplefixedweirs  
   use m_netw,     only : xk, yk 

   integer             :: i, L, k3, k4, ncol
   double precision    :: xu, yu

   if (ndrawFixedWeirs == 0 .or. nfxw == 0 ) return

   call thicklinetexcol(ncolfxw) 


   if (isimplefixedweirs == 0) then 
       

      do i=1,nfxw
          call plotCrossSectionPath(fxw(i), ndrawFixedWeirs, ncolfxw, 0, ' ')
      end do

      
   else

  
       if (Lnx == 0) then 
          return
       endif

       do i = 1,nfxw
          L = lnfxw(i) 
          
          if (L > 0) then  
        
             call setcol(ncolfxw)

             k3=lncn(1,L) ; k4=lncn(2,L)  
         
             if (ndrawfixedweirs == 3 .or. ndrawfixedweirs == 4) then 
                call isocol(bob(1,L),ncol) 
             endif
             if (ndrawfixedweirs == 5) then 
                if (hu(L) >  0 ) then 
                   cycle
                endif 
             endif  
             call movabs( xk(k3), yk(k3) )
             call lnabs(  xk(k4), yk(k4) )
          endif      
     
       enddo


       if (ndrawFixedWeirs == 2 .or. ndrawFixedWeirs == 4) then 
          call setcol(ncolblack)
          do i  = 1,nfxw
             L  = lnfxw(i) ; k3=lncn(1,L) ; k4=lncn(2,L)  
             xu =  0.5d0*( xk(k3) +  xk(k4) ) 
             yu =  0.5d0*( yk(k3) +  yk(k4) ) 
             if ( ndrawFixedWeirs == 4) then 
                call isocol(bob(1,L),ncol) 
             endif          
             call htext(bob(1,L) , xu, yu) 
          enddo
       endif

   endif
    
   call resetlinesizesetc()    

end subroutine plotFixedWeirs


!> Plots a cross section path on the screen.
!! Prior to a 'geominit' the original polyline path is shown, for an
!! initialized model the crossed flow links are highlighted.
subroutine plotCrossSectionPath(path, met, ncol, jaArrow, label)
    use m_crspath
    use m_wearelt
    use geometry_module, only: normalout
    use m_missing, only : dmiss, dxymis
    use m_sferic, only: jsferic, jasfer3D
    use gridoperations
    
    type(tcrspath),   intent(in) :: path    !< Path definition
    integer,          intent(in) :: met     !< Method: 1=plot polyline, 2=plot crossed net/flow links (as stored in path%xk)
    integer,          intent(in) :: ncol    !< Drawing color
    character(len=*), intent(in) :: label   !< Text label to be displayed.
    integer,          intent(in) :: jaArrow !< Whether or not (1/0) to draw an outgoing arrow.
    logical inview
 
    integer :: j, jj, jmin, jmax
    double precision :: xt, yt, rn, rt, xx1, yy1, xx2, yy2, xx, yy


    call setcol(ncol)

    ! If crs is not yet placed on flow links, just plot the coarse polyline:
    ! (or flow links are already known, but plotmethod==1)
    if (path%np > 0 .and. (path%lnx <= 0 .or. met==1)) then
        call movabs(path%xp(1), path%yp(1))
        !call cir(.4d0*rcir)
        jmax = 1 ! jmax is the last visible point in coarse polyline.
                 ! Only #1 is not checked (so user should zoom out when even that one is not visible)
        if (path%np > 1) then
           do j=2,path%np
               call lnabs(path%xp(j), path%yp(j))
           
               if (inview(path%xp(j), path%yp(j))) then  ! find first and last j in viewing area
                   jmax = j
               end if
               !call cir(.4d0*rcir)
           end do
        else if (path%np == 1) then
           call cir(rcir)
        end if
        xx2 = path%xp(jmax)
        yy2 = path%yp(jmax)
    ! Else, default: plot all crossed flow links in crs.
    else if (path%lnx > 0 .and. met==2)  then
        jmin  = 0
        jmax  = 0

        do j=1,path%lnx
            call movabs(path%xk(1,j), path%yk(1,j))
            ! call cir(.2d0*rcir)
            xx = path%xk(2,j) ; yy = path%yk(2,j)
            call lnabs(xx,yy)
            ! call cir(.2d0*rcir)

            if (inview(xx,yy)) then  ! find first and last j in viewing area
                jmax = j
                if (jmin == 0) jmin = j
            end if
        end do
        if (jmax == 0 .and. jmin == 0) then
           call LINEWIDTH(1)
           return
        else
           if ( path%xk(1,jmin) > path%xk(1,jmax) ) then
              jj = jmin ; jmin = jmax ; jmax = jj
           endif
           xx1 = path%xk(1,jmin); yy1 = path%yk(1,jmin)
           xx2 = path%xk(1,jmax); yy2 = path%yk(1,jmax)
        endif

        ! For a monitoring cross section, plot the positive direction
        ! as an arrow in view area, and show discharge or other quant.
        if (jaArrow == 1) then
            xt = .5d0*(path%xk(1,jmin) + path%xk(2,jmin))
            yt = .5d0*(path%yk(1,jmin) + path%yk(2,jmin))
            call normalout(path%xk(1,jmin), path%yk(1,jmin), path%xk(2,jmin), path%yk(2,jmin), rn, rt, jsferic, jasfer3D, dmiss, dxymis)
            call arrowsxy(xt,yt,rn,rt,4d0*rcir)
        endif

    end if ! path%lnx > 0
    if (len_trim(label) > 0) then
        call igrcharfont(7)
        xt = xx2 ; yt = yy2
        if (xt > x2 - dsix) xt = x2-dsix
        call gtext(trim(label), xt, yt, kltex)
    endif



end subroutine plotCrossSectionPath

subroutine thicklinetexcol(ncol)
integer :: ncol
call settextsizefac(2.0d0)
call LINEWIDTH(2)
call setcol(ncol)
end subroutine thicklinetexcol


subroutine resetlinesizesetc()
call settextsize()
CALL LINEWIDTH(1)
call igrcharfont(1)
end subroutine resetlinesizesetc

SUBROUTINE MINMXNS()
      USE M_BITMAP
      use network_data
      USE M_SAMPLES
      use m_arcinfo
      USE M_grid
      USE M_SPLINES
      implicit none
      double precision :: aspect
      double precision :: dx
      double precision :: dy
      integer :: n
      integer :: ndraw
      double precision :: xcmax, xcmin, xlmax, xlmin, xplmax, xplmin, xsmax, xsmin, xspmax, xspmin
      double precision :: ycmax, ycmin, ylmax, ylmin, yplmax, yplmin, ysmax, ysmin, yspmax, yspmin
      double precision :: xm, ym
      double precision ::  XH(10), YH(10)
      COMMON /DRAWTHIS/ ndraw(50)
      CALL DMINMAX(   XLAN,  MXLAN,  XLMIN,   XLMAX, MAXLAN)
      CALL DMINMAX(   YLAN,  MXLAN,  YLMIN,   YLMAX, MAXLAN)

      CALL DMINMAX(   XK  ,  NUMK ,  XKMIN,   XKMAX, KMAX  )
      CALL DMINMAX(   YK  ,  NUMK ,  YKMIN,   YKMAX, KMAX  )

      CALL DMINMAX(   Xc  ,  mc*nc,  XCMIN,   XCMAX, mc*nc )
      CALL DMINMAX(   Yc  ,  mc*nc,  YCMIN,   YCMAX, mc*nc )

      CALL DMINMAX(  XSP  ,   mcS*MAXSPLEN,  XSPMIN, XSPMAX,  mcS*MAXSPLEN) ! SPLINES
      CALL DMINMAX(  YSP  ,   mcS*MAXSPLEN,  YSPMIN, YSPMAX,  mcS*MAXSPLEN)

      CALL  DMINMAX(  XPL ,  NPL  ,  XPLMIN,   XPLMAX, MAXPOL)
      CALL  DMINMAX(  YPL ,  NPL  ,  YPLMIN,   YPLMAX, MAXPOL)

      if ( NS.gt.0 ) then
         CALL  DMINMAX(  XS  ,  NS   ,  XSMIN,   XSMAX, NS    )
         CALL  DMINMAX(  YS  ,  NS   ,  YSMIN,   YSMAX, NS    )
      else if (mca > 0) then
         xsmin = x0 
         xsmax = x0 + dxa*(mca-1)
         ysmin = y0
         ysmax = y0 + dya*(nca-1)
      else 
         xsmin =  0d0
         xsmax =  0d0
         ysmin =  0d0
         ysmax =  0d0
      end if

      IF (NDRAW(26) .EQ. 1) THEN
         XMIN = MIN(XMIN,XP(1))
         XMAX = MAX(XMAX,XP(2))
         YMIN = MIN(YMIN,YP(1))
         YMAX = MAX(YMAX,YP(4))
      ENDIF

      N = 0
      IF (XKMAX .NE. XKMIN .OR. YKMAX .NE. YKMIN) THEN
         N     = N+1
         XH(N) = XKMAX
         YH(N) = YKMAX
         N     = N+1
         XH(N) = XKMIN
         YH(N) = YKMIN
      ENDIF

      IF (XLMAX .NE. XLMIN .OR. YLMAX .NE. YLMIN) THEN
         N     = N+1
         XH(N) = XLMAX
         YH(N) = YLMAX
         N     = N+1
         XH(N) = XLMIN
         YH(N) = YLMIN
      ENDIF

      IF (XPLMAX .NE. XPLMIN .OR. YPLMAX .NE. YPLMIN) THEN
         N     = N+1
         XH(N) = XPLMAX
         YH(N) = YPLMAX
         N     = N+1
         XH(N) = XPLMIN
         YH(N) = YPLMIN
      ENDIF

      IF (XSMAX .NE. XSMIN .OR. YSMAX .NE. YSMIN) THEN
         N     = N+1
         XH(N) = XSMAX
         YH(N) = YSMAX
         N     = N+1
         XH(N) = XSMIN
         YH(N) = YSMIN
      ENDIF

      IF (XCMAX .NE. XCMIN .OR. YCMAX .NE. YCMIN) THEN
         N     = N+1
         XH(N) = XCMAX
         YH(N) = YCMAX
         N     = N+1
         XH(N) = XCMIN
         YH(N) = YCMIN
      ENDIF

      IF (XSPMAX .NE. XSPMIN .OR. YSPMAX .NE. YSPMIN) THEN
         N     = N+1
         XH(N) = XSPMAX
         YH(N) = YSPMAX
         N     = N+1
         XH(N) = XSPMIN
         YH(N) = YSPMIN
      ENDIF

      CALL DMINMAX(     XH,      N,   XMIN,   XMAX, 10)
      CALL DMINMAX(     YH,      N,   YMIN,   YMAX, 10)

 
      IF (XMAX .EQ. XMIN .and. YMAX .EQ. YMIN) THEN
          XMIN = 0d0     ; YMIN = 0d0
          CALL INQASP(ASPECT)
          XMAX = 1000d0  ; Ymax = aspect*1000d0
      ENDIF

      CALL MINMAXWORLD(XMIN,YMIN,XMAX,YMAX)

      RETURN
END subroutine minmxns

!> Plot all structures in the current viewport
subroutine plotStructures()
use m_GlobalParameters
use unstruc_colors
use unstruc_channel_flow
use m_flowgeom, only: xu, yu, lnx
use gridoperations
use m_flowparameters, only: epshu
use m_flow, only: hu
use m_wearelt, only: rcir
implicit none

integer              :: is,link
double precision     :: icon_rw_size !< Size of plotted icons in real-world coordinates.
double precision     :: x, y
character(len=Idlen) :: text
logical              :: active, inview

if (ndrawStructures <= 1) then
   return
end if

! Determine icon_rw_size. 
icon_rw_size = 2*rcir

call IGrCharJustify('L')

! Draw structures at the velocity points where they are located
if (network%loaded) then
   do is = 1,network%sts%Count
   
      ! Get structure x,y coordinates.
      if (network%sts%struct(is)%numlinks > 0) then
         link = abs(network%sts%struct(is)%linknumbers(1))
      else
         link = 0
      end if

      if (link > 0 .and. link <= lnx) then ! for safety
         x = xu(link)
         y = yu(link)
         if (.not. inView(x, y)) cycle
         
         ! Draw structure.
         active = hu(link) > epshu         
         call movabs(x, y)
         ! Uses same symbols and colors as for Sobek 2.
         select case(network%sts%struct(is)%type)
         case (ST_PUMP)
            active = network%sts%struct(is)%pump%is_active
            call drawTriangle(x, y, icon_rw_size, ncolorange, ncolblack, active)
         case (ST_GENERAL_ST)
            call drawTriangle(x, y, icon_rw_size, ncolpink, ncolblack, active)
         case (ST_WEIR)
            call drawTriangle(x, y, icon_rw_size, ncolgreen, ncolblack, active)
         case (ST_ORIFICE)
            call drawTriangle(x, y, icon_rw_size, ncoldarkgreen, ncolblack, active)
         case (ST_CULVERT)
            call drawTriangle(x, y, icon_rw_size, ncolmaroon, ncolblack, active)      
         case (ST_UNI_WEIR)
            call drawTriangle(x, y, icon_rw_size, ncolgray, ncolblack, active)
         case (ST_BRIDGE)
            call drawTriangle(x, y, icon_rw_size, ncollavender, ncolblack, active)
         case (ST_DAMBREAK)
            call drawStar(x, y, 1.5*icon_rw_size, ncolred, ncolblack)
         case default
         end select
         
         if (ndrawStructures <= 2) then
            cycle
         end if
         
         ! Draw label with structure id.
         call igrcharfont(7)
         call gtext(trim(network%sts%struct(is)%id), x + 0.5*icon_rw_size, y - 1.3*icon_rw_size, ncolwhite)
      end if
   end do
end if

call resetlinesizesetc()

end subroutine plotStructures

!> Draws a filled (or empty) triangle at current position.
!! Filled means: one colour for inside, one colour for edge.
subroutine drawTriangle(x, y, size, icolfill, icoledge, filled)
implicit none

double precision, intent(in) :: x        !< x coordinate of center of triangle.
double precision, intent(in) :: y        !< y coordinate of center of triangle.
double precision, intent(in) :: size     !< size of triangle in world coordinates.
integer,          intent(in) :: icolfill !< Colour number for inner fill
integer,          intent(in) :: icoledge !< Colour number for edge
logical,          intent(in) :: filled   !< Filled or empty  

if (filled) then 
   ! Fill
   call IGrFillPattern(4,0,0)
   call setcol(icolfill)
   call IGrTriangle(real(x - size/2), real(y - size/2), real(x + size/2), real(y - size/2), real(x), real(y + size/2))
   call IGrFillPattern(0,0,0)
   call setcol(icoledge)
   call IGrTriangle(real(x - size/2), real(y - size/2), real(x + size/2), real(y - size/2), real(x), real(y + size/2))
else 
   ! Edge
   call IGrFillPattern(0,0,0)  
   call IGrLineWidth(3,1)
   call setcol(icoledge)
   call IGrTriangle(real(x - size/2), real(y - size/2), real(x + size/2), real(y - size/2), real(x), real(y + size/2))
   call IGrLineWidth(1,1)
   call setcol(icolfill)
   call IGrTriangle(real(x - size/2), real(y - size/2), real(x + size/2), real(y - size/2), real(x), real(y + size/2))
endif 
! Edge
call IGrFillPattern(4,0,0)    ! Reset fill pattern

end subroutine drawTriangle


!> Draws a filled four-pointed star at current position.
!! Filled means: one colour for inside, one colour for edge.
subroutine drawStar(x, y, size, icolfill, icoledge)
implicit none

double precision,   intent(in) :: x        !< x coordinate of center of star.
double precision,   intent(in) :: y        !< y coordinate of center of star.
double precision,   intent(in) :: size     !< size of start in world coordinates.
integer,            intent(in) :: icolfill !< Colour number for inner fill
integer,            intent(in) :: icoledge !< Colour number for edge

double precision, dimension(8) :: xs
double precision, dimension(8) :: ys

xs = (/ x - size/2, x - size/8, x, x + size/8, x + size/2, x + size/8, x, x - size/8 /)
ys = (/ y, y - size/8, y - size/2, y - size/8, y, y + size/8, y + size/2, y + size/8 /)

! Fill
call IGrFillPattern(4,0,0)
call setcol(icolfill)
call IGrPolygonSimple(real(xs), real(ys), 8)

! Edge
call IGrFillPattern(0,0,0)
call setcol(icoledge)
call IGrPolygonSimple(real(xs), real(ys), 8)

! Reset IGrFillPattern.
call IGrFillPattern(4,0,0)

end subroutine drawStar


!> Display information for a (1D) flow link and its connected nodes.
!! If it has a structure on it, then also display relevant fields
!! of this structure.
subroutine dis_info_1d_link(LL)
use m_flowgeom
use network_data
use m_flow
use unstruc_channel_flow
use m_1d_structures
use m_Pump
use m_Culvert
implicit none

integer, intent(in) :: LL !< flow link number

character TEX*48, str_type*21, tex_empty*48
integer :: linec ! line counter
integer :: colc  ! colume counter
integer :: k1, k2! node number
integer :: L     ! net link number
integer :: line_max ! maximal line number
integer :: branchindex, ilocallin, nstruc, istrtype, i
double precision, external :: zlin , znod
type(t_pump), pointer      :: ppump
type(t_culvert), pointer   :: pculvert


linec = 7
colc  = 1
line_max = 48

! write an empty line
tex_empty = ''
call IOUTSTRINGXY(colc, linec, tex_empty)

linec = linec + 1
tex = ' Info for current link+nodes, press q to exit.'
call IOUTSTRINGXY(colc, linec, tex)

linec = linec + 1
call IOUTSTRINGXY(colc, linec, tex_empty)

! block for node 1
k1 = ln(1,LL)
if (k1 > 0) then
   call Write2Scr(linec, 'Node 1 number', k1, '-')
   call Write2Scr(linec, 'Kfs', kfs(k1), '-')
   call Write2Scr(linec, 'Water level  (s1)', s1(k1), 'm')
   call Write2Scr(linec, 'Water depth  (hs)', hs(k1), 'm')
   call Write2Scr(linec, 'Bottom level (bl)', bl(k1), 'm')
   call Write2Scr(linec, 'Volume     (vol1)', vol1(k1), 'm3')
   call Write2Scr(linec, 'Volume   (vol1_f)', vol1_f(k1), 'm3')
   if (znod(k1) .ne. dmiss) then 
   call Write2Scr(linec, 'znod(k1)         ', znod(k1), 'znod')
   endif

end if

! write an empty line
linec = linec + 1
call IOUTSTRINGXY(colc, linec, tex_empty)

! block for node 2
k2 = ln(2,LL)
if (k2 > 0) then
   call Write2Scr(linec, 'Node 2 number', k2, '-')
   call Write2Scr(linec, 'Kfs', kfs(k2), '-')
   call Write2Scr(linec, 'Water level  (s1)', s1(k2), 'm')
   call Write2Scr(linec, 'Water depth  (hs)', hs(k2), 'm')
   call Write2Scr(linec, 'Bottom level (bl)', bl(k2), 'm')
   call Write2Scr(linec, 'Volume     (vol1)', vol1(k2), 'm3')
   call Write2Scr(linec, 'Volume   (vol1_f)', vol1_f(k2), 'm3')
   if (znod(k2) .ne. dmiss) then 
   call Write2Scr(linec, 'znod(k2)         ', znod(k2), 'znod')
   endif


end if

! write an empty line
linec = linec + 1
call IOUTSTRINGXY(colc, linec, tex_empty)

! block for flow link
call Write2Scr(linec, 'Flow link number', LL, '-')
call Write2Scr(linec, 'Flow link type (kcu)', kcu(LL), '-')
L = abs(ln2lne(LL))
call Write2Scr(linec, 'Net link number', L, '-')
call Write2Scr(linec, 'Net link type  (kn3)', kn(3,L), '-')

if (network%loaded .and. kcu(LL) == 1) then
   branchindex = network%adm%lin2ibr(LL)
   if (branchindex >= 1 .and. branchindex <= network%brs%Count) then
      call Write2Scr(linec, 'Branch id', network%brs%branch(branchindex)%id(1:21))

      ilocallin = network%adm%lin2local(LL)
      if (ilocallin >= 1 .and. ilocallin <= network%brs%branch(branchindex)%uPointsCount) then
         call Write2Scr(linec, 'Chainage', network%brs%branch(branchindex)%uPointsChainages(ilocallin), 'm')
      else
         call Write2Scr(linec, 'Chainage', 'N/A')
      end if
   else
      call Write2Scr(linec, 'Branch id', 'N/A')
      call Write2Scr(linec, 'Chainage',  'N/A')
   end if

end if

call Write2Scr(linec, 'Bob(1,L)', bob(1,LL), 'm')
call Write2Scr(linec, 'Bob(2,L)', bob(2,LL), 'm')
call Write2Scr(linec, 'Bob0(1,L)', bob0(1,LL), 'm')
call Write2Scr(linec, 'Bob0(2,L)', bob0(2,LL), 'm')

call Write2Scr(linec, 'Flow area     (au)', au(LL), 'm2')
call Write2Scr(linec, 'Flow width    (wu)', wu(LL), 'm')
call Write2Scr(linec, 'Water depth   (hu)', hu(LL), 'm')
call Write2Scr(linec, 'Velocity      (u1)', u1(LL), 'm/s')
call Write2Scr(linec, 'Discharge     (q1)', q1(LL), 'm3/s')
call Write2Scr(linec, 'g/CCH      (cfuhi)', cfuhi(LL), '1/m')
if (zlin(LL) .ne. -999) then 
call Write2Scr(linec, 'zlin              ', zlin(LL), 'zlin')
endif

! If this flowlink has a stucture on it, then also display related info.
if (network%loaded .and. kcu(LL) == 1) then
   nstruc = network%adm%lin2str(LL) ! Assume only 1 structure on the flowlink
else
   nstruc = 0
end if

if (nstruc > 0) then
   call Write2Scr(linec, 'Structure id', network%sts%struct(nstruc)%id(1:21))
   
   istrtype = network%sts%struct(nstruc)%type
   call GetStrucType_from_int(istrtype, str_type)
   call Write2Scr(linec, 'Structure type', str_type)   
   
   select case (istrtype)
   case (ST_PUMP)
      ppump=>network%sts%struct(nstruc)%PUMP
      call Write2Scr(linec, 'Direction', ppump%direction, 'm')
      call Write2Scr(linec, 'Head', ppump%pump_head, 'm')
      call Write2Scr(linec, 'Actual stage', ppump%actual_stage, '-')
      if (ppump%is_active) then
         call Write2Scr(linec, 'Is active?', 1, '-')
      else
         call Write2Scr(linec, 'Is active?', 0, '-')
      end if
      call Write2Scr(linec, 'Current capacity', ppump%current_capacity, 'm3/s')
      call Write2Scr(linec, 'Reduction factor', ppump%reduction_factor, '-')
   case (ST_CULVERT)
      pculvert=>network%sts%struct(nstruc)%culvert
      call write2scr(linec, 'Left level', pculvert%leftlevel, 'm')
      call write2scr(linec, 'Right level', pculvert%rightlevel, 'm')
      call write2scr(linec, 'Allowed flow dir.', pculvert%allowedflowdir, '-')
      call write2scr(linec, 'Length', pculvert%length, 'm')
      
      if (pculvert%has_valve) then
         call write2scr(linec, 'Valve opening', pculvert%valveOpening, 'm')
      end if

      call write2scr(linec, 'Inlet loss coef.', pculvert%inletlosscoeff, '-')
      call write2scr(linec, 'Outlet loss coef.', pculvert%outletlosscoeff, '-')
   case default
      linec = linec + 1
      call IOUTSTRINGXY(1, linec, ' Display for this structure type is not supported.')     
   end select
end if

! write empty lines to erase lines with structure data from the previous clicked flow link, if any
do i = linec+1, line_max
   call IOUTSTRINGXY(colc, i, tex_empty)
end do

return
end subroutine dis_info_1d_link

!> Writes a line with integer data to the screen.
   subroutine Write2ScrInt(ipos, desc, val, unit)
      implicit none
      integer, intent(inout)        :: ipos
      character(len=*), intent(in)  :: desc
      integer, intent(in)           :: val
      character(len=*), intent(in)  :: unit

      character :: tex*48, help*27
      ipos = ipos+1
      help = ' '//desc
      write(tex, '(a23,'' = '', i13, '' ('',a,'')'')') help, val, trim(unit)
      call IOUTSTRINGXY(1,ipos,tex)
   end subroutine Write2ScrInt

   !> Writes a line with double precision data to the interacter screen.
   subroutine Write2ScrDouble(ipos, desc, val, unit)
      implicit none
      integer, intent(inout)        :: ipos
      character(len=*), intent(in)  :: desc
      double precision, intent(in)  :: val
      character(len=*), intent(in)  :: unit

      character :: tex*48, help*27
      ipos = ipos+1
      help = ' '//desc
      write(tex, '(a23,'' = '', g13.4, '' ('',a,'')'')') help, val, trim(unit)
      call IOUTSTRINGXY(1,ipos,tex)
   end subroutine Write2ScrDouble

   !> Writes a line with character data to the screen.
   subroutine Write2ScrChar(ipos, desc, val)
      implicit none

      integer, intent(inout)        :: ipos
      character(len=*), intent(in)     :: desc
      character(len=*), intent(in)     :: val

      character :: text*48, help*24, help2*21

      ipos = ipos + 1
      help = ' '//desc
      help2 = trim(adjustl(val))
      write(text, '(A23,'' = '',A21)') help, help2
      call IOUTSTRINGXY(1, ipos, text)
   end subroutine Write2ScrChar

end module unstruc_display

 subroutine zoomshift(nshift) ! based on polygon
 use unstruc_display
 use m_flowtimes
 use m_polygon
 implicit none
 integer :: nshift, ndraw, i1
 double precision :: dr, x00, y00, dxw, dyw, rshift

 COMMON /DRAWTHIS/ ndraw(50)

 nshift = nshift + 1
 rshift = dble(nshift)/dble(numzoomshift)
 i1     = int(rshift) + 1
 i1     = min(i1,npl-1)
 dr     = rshift - i1 + 1
 x00    = (1d0 - dr)*xpl(i1) + dr*xpl(i1+1)
 y00    = (1d0 - dr)*ypl(i1) + dr*ypl(i1+1)
 dxw    = 0.5d0*(x2-x1)
 dyw    = 0.5d0*(y2-y1)
 x1     = x00 - dxw
 x2     = x00 + dxw
 y1     = y00 - dyw
 y2     = y00 + dyw
 call setwor(x1,y1,x2,y2)
 ndraw(10) = 1 ! wel plotten
 end subroutine zoomshift


subroutine tekship()
 use m_ship
 implicit none
 double precision :: sx2,sy2,css,sns, rr, cr, sr, snum
 integer :: n
 if (iniship == 0) return

 call setcol(4)

 do n = 1,nshiptxy
    css = cos(shi(n))     ; sns = sin(shi(n))

    call smovabs(n,  1.0d0 ,  0.0d0)
    call slnabs (n,  0.9d0 , -1.0d0)
    call slnabs (n, -1.0d0 , -1.0d0)
    call slnabs (n, -1.0d0 ,  1.0d0)
    call slnabs (n,  0.9d0 ,  1.0d0)
    call slnabs (n,  1.0d0 ,  0.0d0)

    snum =  css*fx2(n) + sns*fy2(n)               ! pressure force in shipL dir
    call shtext(n, snum, -1.3d0, 0d0)

    snum = -sns*fx2(n) + css*fy2(n)               ! pressure force in shipB dir
    call shtext(n, snum, -1.3d0, 1d0)

    snum = fm2(n)/shL(n)                          ! pressure mom vertical ax
    call shtext(n, snum, -1.3d0,-1d0)

    snum =  css*fricx(n) + sns*fricy(n)           ! fric force in shipL dir
    call shtext(n, snum,  1.3d0, 0d0)

    snum = -sns*fricx(n) + css*fricy(n)           ! fric force in shipB dir
    call shtext(n, snum,  1.3d0, 1d0)

    snum = fricm(n)/shL(n)                        ! fric mom vertical ax
    call shtext(n, snum,  1.3d0,-1d0)

    snum =  css*stuwx(n) + sns*stuwy(n)           ! stuwforce in shipL dir
    call shtext(n, snum, -0.8d0, 0d0)

    snum = -sns*stuwx(n) + css*stuwy(n)           ! stuwforce in shipB dir
    call shtext(n, snum, -0.8d0, 1d0)

    snum = stuwm(n)/shL(n)                        ! stuwmom vertical ax normalised by half length
    call shtext(n, snum, -0.8d0,-1d0)

    snum =  css*shu(n) + sns*shv(n)               ! snelheid in shipL dir
    call shtext(n, snum, -0.d0, 0d0)

    snum = -sns*shu(n) + css*shv(n)               ! snelheid in shipB dir
    call shtext(n, snum, -0.0d0, 1.1d0)

    snum = sho(n)                                 ! ronjes/minuut vertical ax
    call shtext(n, snum*60d0/6.28d0, -0.d0,-1.1d0)

    sx2 = shx(n) - shL(n)*css                     ! rudder
    sy2 = shy(n) - shL(n)*sns
    call movabs(sx2, sy2)
    rr  = 0.4d0*shb(n) ; cr = cos(shi(n) + roer(n)) ; sr = sin(shi(n) + roer(n))
    call lnabs(sx2 - rr*cr, sy2 - rr*sr)

 enddo
 end subroutine tekship

 
subroutine tekwindvector()
 USE m_wind
 use m_wearelt
 use unstruc_display
 use m_heatfluxes
 use m_flow 
 use m_transport
 use m_flowgeom
 use m_wind
 use m_xbeach_data, only: csx, snx, itheta_view
 use m_flowparameters, only: jawave
 use m_missing
 use m_statistics
 use messagehandling

 implicit none
 COMMON /DRAWTHIS/   ndraw(50)
 integer :: ndraw 
 double precision :: xp, yp, vfw, ws, dyp, upot,ukin,ueaa
 character tex*60 
 integer :: ncol, k, kk, vlatin, vlatout, i, mout
 
 
 if (ndraw(40) == 0 .and. npdf == 0) return
 
 call thicklinetexcol(ncolln) 

 yp  = 0.15*y1 + 0.85*y2
 dyp = 0.025*(y2-y1)

 if (npdf > 0) then
    xp = 0.97*x1 + 0.03*x2 
  
    call newfil(mout, 'cumulative.tek')
    write(mout,'(a)') '* Column 1 : cos ()'
    write(mout,'(a)') '* Column 2 : fraction ()'

    msgbuf = 'BL01'
    call msg_flush(); write(mout,'(a)') msgbuf
    write (msgbuf,'(i4,A)') npdf, ' 2 ' 
    call msg_flush() ; write(mout,'(a)') msgbuf
     
    do i = npdf-1,1,-1
       yp  = yp - dyp
       tex = '                                 '
       write(tex(1:), '(2F10.6)') ypdf(i), xpdf(i)     
       call GTEXT(tex, xp, yp, ncolln)
       write (msgbuf,'(a)') tex
       call msg_flush(); write(mout,'(a)') msgbuf
    enddo
    call doclose(mout)
 endif
 
 if (jawind > 0 ) then 
    xp  = 0.90*x1 + 0.10*x2
    vfw = 0.1d0*(x2-x1)/10d0   ! 10 m/s is 0.1*screen
    call arrowsxy( xp, yp, windxav, windyav, vfw)
    ws = sqrt(windxav*windxav + windyav*windyav)   
    xp = 0.97*x1 + 0.03*x2
    yp = 0.25*y1 + 0.75*y2
    tex = 'Wind   :             (m/s)'
    write(tex(10:17), '(F8.3)') ws 
    call GTEXT(tex, xp, yp, ncolln)
 endif
  
 if (a1ini == 0d0) then
    call resetlinesizesetc()
    return
 endif

 yp = 0.25*y1 + 0.75*y2
 xp = 0.97*x1 + 0.03*x2    
 if (vinraincum > 0) then   
    xp = 0.97*x1 + 0.03*x2    
    yp = yp - 2*dyp 
    tex = 'Rain   :           (mm/hr)'
    write(tex(10:17), '(F8.4)') 3600*1000*qinrain/a1ini
    call GTEXT(tex, xp, yp, ncolln)
    yp  = yp - dyp
    tex = 'Hrain  :               (m)' 
    write(tex(10:20), '(F11.4)') vinraincum/a1ini   
    call GTEXT(tex, xp, yp, ncolln)
 endif
 
 if (voutevacum > 0) then   
    yp = yp - dyp 
    tex = 'Evap   :           (mm/hr)'
    write(tex(10:17), '(F8.4)') 3600*1000*qouteva/a1ini
    call GTEXT(tex, xp, yp, ncolln)
    yp  = yp - dyp
    tex = 'Hevap  :               (m)' 
    write(tex(10:20), '(F11.4)') voutevacum/a1ini   
    call GTEXT(tex, xp, yp, ncolln)
 endif
 
 if (ndraw(40) == 1) then 
 
    ncol = ncoltx
    call thicklinetexcol(ncol)  
    
    if (vinbndcum > 0 .or. voutbndcum > 0) then 
       yp  = yp - dyp                 
       tex = 'HinBnd :               (m)'
       write(tex(10:20), '(F11.4)')  vinbndcum/a1ini   
       call GTEXT(tex, xp, yp, ncol)
   
       yp  = yp - dyp                 
       tex = 'HoutBnd:               (m)'
       write(tex(10:20), '(F11.4)') -voutbndcum/a1ini   
       call GTEXT(tex, xp, yp, ncol)
    endif

    vlatin  = sum(vinlatcum(1:2))
    vlatout = sum(voutlatcum(1:2))
    if (vlatin > 0 .or. vlatout > 0) then 
       yp  = yp - dyp                 
       tex = 'HinLat :               (m)'
       write(tex(10:20), '(F11.4)')  vlatin/a1ini   
       call GTEXT(tex, xp, yp, ncol)
   
       yp  = yp - dyp                 
       tex = 'HoutLat:               (m)'
       write(tex(10:20), '(F11.4)') -vlatout/a1ini   
       call GTEXT(tex, xp, yp, ncol)
    endif

    
    if (vingrwcum > 0 .or. voutgrwcum > 0) then 
       yp  = yp - dyp                 
       tex = 'Hingrw :               (m)'
       write(tex(10:20), '(F11.4)')  vingrwcum/a1ini   
       call GTEXT(tex, xp, yp, ncol)
       
       yp  = yp - dyp                 
       tex = 'Houtgrw:               (m)'
       write(tex(10:20), '(F11.4)') -voutgrwcum/a1ini   
       call GTEXT(tex, xp, yp, ncol)
    endif
       
    yp  = yp - dyp                 
    tex = 'Htot   :               (m)'
    write(tex(10:20), '(F11.4)') (vol1tot-vol1ini)/a1ini   
    call GTEXT(tex, xp, yp, ncol)
    
    if ( jagrw > 0 ) then 
       yp  = yp - dyp 
       tex = 'Hgrw   :               (m)'
       write(tex(10:20), '(F11.4)') (volgrw-volgrwini)/a1ini   
       call GTEXT(tex, xp, yp, ncol)
    endif 
       
       yp  = yp - dyp                 
       tex = 'Hini   :               (m)'
       write(tex(10:20), '(F11.4)') vol1ini/a1ini
       call GTEXT(tex, xp, yp, ncol)
    
    if (jagrw > 0) then   
       yp  = yp - dyp                 
       tex = 'Hgrwini:               (m)'
       write(tex(10:20), '(F11.4)') volgrwini/a1ini
       call GTEXT(tex, xp, yp, ncol)
    endif
    
       yp  = yp - dyp                 
       tex = 'Areatot:               (m)'
       write(tex(10:20), '(E11.5)') a1ini   
       call GTEXT(tex, xp, yp, ncol)
    
    if (jatem == 5) then 
       yp  = yp - 2*dyp
       tex = 'QSUNav :             (W/m2)'
       write(tex(10:20), '(E11.5)') Qsunav   
       call GTEXT(tex, xp, yp, ncol)
    
       yp  = yp - dyp
       tex = 'QEVAav :             (W/m2)'
       write(tex(10:20), '(E11.5)') Qevaav   
       call GTEXT(tex, xp, yp, ncol)
    
       yp  = yp - dyp
       tex = 'QCONav :             (W/m2)'
       write(tex(10:20), '(E11.5)') QCONav   
       call GTEXT(tex, xp, yp, ncol)
    
       yp  = yp - dyp
       tex = 'QLongav:             (W/m2)'
       write(tex(10:20), '(E11.5)') QLongav    
       call GTEXT(tex, xp, yp, ncol)
    
       yp  = yp - dyp
       tex = 'Qfreeav:             (W/m2)'
       write(tex(10:20), '(E11.5)') Qfreeav    
       call GTEXT(tex, xp, yp, ncol)
       
    endif   
    
 else if (ndraw(40) == 2) then 
    
    call upotukinueaa(upot,ukin,ueaa)     

    ncol = ncoltx


    yp  = yp - 2.5*dyp
    tex = 'Ut0:                 (kg/(m.s2))'
    if (upot0+ukin0 > 1000) then 
       write(tex(8:20), '(F11.2)') ukin0+upot0
    else
       write(tex(8:20), '(F11.7)') ukin0+upot0
    endif 
    call GTEXT(tex, xp, yp, ncol)

    yp  = yp - dyp
    tex = 'Upot :               (kg/(m.s2))'
    if (upot > 1000) then 
       write(tex(8:20), '(F11.2)') upot   
    else
       write(tex(8:20), '(F11.7)') upot 
    endif 
    call GTEXT(tex, xp, yp, ncol)
  
    yp  = yp - dyp
    tex = 'Ukin :               (kg/(m.s2))'
    if (ukin > 1000) then 
       write(tex(8:20), '(F11.2)') ukin   
    else
       write(tex(8:20), '(F11.7)') ukin 
    endif 
    call GTEXT(tex, xp, yp, ncol)
  
    yp  = yp - dyp
    tex = 'Utot :               (kg/(m.s2))'
    if (upot + ukin > 1000) then 
       write(tex(8:20), '(F11.2)') upot+ukin   
    else
       write(tex(8:20), '(F11.7)') upot+ukin
    endif 
    call GTEXT(tex, xp, yp, ncol)

    yp  = yp - dyp
    tex = 'Upot/Ut0:                   ( )'
    write(tex(8:20), '(F11.7)') upot/max(ukin0 + upot0,eps4)
    call GTEXT(tex, xp, yp, ncol)

    yp  = yp - dyp
    tex = 'Ukin/Ut0:                   ( )'
    write(tex(8:20), '(F11.7)') ukin/max(ukin0 + upot0,eps4)
    call GTEXT(tex, xp, yp, ncol)

    yp  = yp - dyp
    tex = 'Utot/Ut0:                   ( )'
    write(tex(8:20), '(F11.7)') (ukin+upot)/(ukin0 + upot0)
    call GTEXT(tex, xp, yp, ncol)

    if (jasal > 0 .or. jatem > 0) then
    yp  = yp - dyp
    tex = 'Ueaa :               (kg/(m.s2))'
    write(tex(8:20), '(F11.2)') ueaa    
    call GTEXT(tex, xp, yp, ncol)
    endif

 else if (ndraw(40) == 3) then 

    ncol = ncoltx
    do i = 1,numconst
       ueaa = 0d0
       do kk = 1,ndxi
          do k = kbot(kk), ktop(kk)
             ueaa = ueaa + vol1(k)*constituents(i,k)
          enddo   
       enddo  
     
       yp  = yp - 2.5*dyp
       tex = 'Mass :                 (c*m3)'
       if (ueaa > 1000) then 
          write(tex(8:20), '(F11.2)') ueaa
       else
          write(tex(8:20), '(F11.7)') ueaa
       endif 
       call GTEXT(tex, xp, yp, ncol)

    enddo      
     
 endif   
 
 if ( jawave.eq.4 ) then
    xp = 0.90*x1 + 0.10*x2
    yp = 0.85*y1 + 0.15*y2

    call thicklinetexcol(ncolln)  
    call arrowsxy( xp, yp, csx(itheta_view), snx(itheta_view), 0.1d0*(x2-x1))
 end if

 call resetlinesizesetc()
  
end subroutine tekwindvector


subroutine upotukinueaa(upot,ukin,ueaa)
use m_flow
use m_flowgeom
use m_missing
implicit none
double precision :: upot, ukin, ueaa
double precision :: vtot, roav, hh, zz, dzz, rhok, bmin
integer k, kk
       
upot = 0d0 ; ukin = 0d0 ; ueaa = 0d0 ; vtot = 0d0 ; roav = 0d0; bmin = 1d9

do kk = 1,ndx
   bmin = min(bmin, bl(kk))
   if ( hs(kk) == 0 ) cycle
   do k = kbot(kk), ktop(kk) 
      vtot = vtot + vol1(k)                                                 ! m3
      if (jasal > 0) then 
         roav = roav + vol1(k)*rho(k)                                       ! kg
       else
         roav = roav + vol1(k)*rhomean                                      ! kg
      endif
   enddo  
enddo 
if (vtot == 0d0) then
   return
end if

roav = roav / vtot                                                          ! kg/m3

do kk = 1,ndx
   if ( hs(kk) == 0 ) cycle
   do k = kbot(kk), ktop(kk) 
      if (kmx > 0) then 
         zz   = (zws(k) + zws(k-1))*0.5d0  - bmin                                 ! m
      else 
         zz   = s1(k) - bmin
      endif
      if (jasal > 0) then 
         rhok = rho(k)
      else 
         rhok = rhomean 
      endif
      ueaa = ueaa + vol1(k)*zz*(rho(k) - roav)                              ! kg.m
      upot = upot + vol1(k)*zz* rho(k)                                      ! kg.m
      ukin = ukin + vol1(k)*    rho(k)*(ucx(k)*ucx(k) + ucy(k)*ucy(k))*0.5d0! kg.m2/s2
   enddo  
enddo   

ueaa  = ueaa*ag/vtot                                                        ! kg/(m.s2)
upot  = upot*ag/vtot  
ukin  = ukin*0.5/vtot        

if (upot0 == dmiss) upot0 = upot
if (ukin0 == dmiss) ukin0 = ukin

! upot = upot - upot0 
                                                                  ! 
end subroutine upotukinueaa

   SUBROUTINE GETINTRGB(KRGB) ! GET interacter RGB FOR NCOL
    implicit none
    integer           :: KRGB(4)       
    integer           :: rgb
    INTEGER, external :: InfoGrPalette !access interacter palette info
        
    ! grab the rgb value of the color nr
    rgb = InfoGrPalette(KRGB(1)) 
    
    ! split into separate r, g, b channel values (0.0 - 1.0)
    KRGB(2) = IAND(rgb,z'ff')
    KRGB(3) = IAND(ISHFT(rgb,-8),z'ff')
    KRGB(4) = IAND(ISHFT(rgb,-16),z'ff')
    
 END SUBROUTINE
  
 SUBROUTINE SETINTRGB(KRGB) ! SAME, SET
 implicit none
 integer           :: KRGB(4)       
 CALL IGRPALETTERGB(KRGB(1), KRGB(2), KRGB(3), KRGB(4))
 END SUBROUTINE
