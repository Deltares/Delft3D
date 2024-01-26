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

   SUBROUTINE CHANGEDISPLAYPARAMETERS()
   USE M_RAAITEK
   USE M_MISSING
   use unstruc_display
   use m_sediment
   use m_flow, only : kplotfrombedorsurface, kplotordepthaveraged

   use dflowfm_version_module, only : company, product_name
   use unstruc_opengl,         only : jaOpenGL

   implicit none
   double precision :: dv
   double precision :: dx
   double precision :: dxshow
   double precision :: dy
   integer :: i
   integer :: ifexit
   integer :: ifinit
   integer :: ih
   integer :: ihcopts
   integer :: il
   integer :: imp
   integer :: inp
   integer :: ir
   integer :: iw
   integer :: ixp
   integer :: iyp
   integer :: jaauto
   integer :: jaeps
   integer :: jaland
   integer :: jaxis
   integer :: key
   integer :: nbut
   integer :: ncols
   integer :: ndec
   integer :: ndraw
   integer :: nhcdev
   integer :: nie
   integer :: nis
   integer :: nlevel
   integer :: numfldactual
   integer :: numhcopts
   integer :: numparactual
   integer :: nv
   integer :: nvec
   double precision :: rmiss, val, vfac, vfacforce, vmax, vmin
   double precision :: x0
   double precision :: xd
   double precision :: xleft
   double precision :: xsc
   double precision :: y0
   double precision :: ybot
   double precision :: ysc
   double precision :: scalesize
   double precision :: tsize
   integer :: JQN

   integer, parameter :: NUMPAR = 33, NUMFLD = 2*NUMPAR
   INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
   CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
   COMMON /HELPNOW/ WRDKEY,NLEVEL
   COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO
   COMMON /TEXTSIZE/ TSIZE
   COMMON /SCREENAREA/ XLEFT,YBOT,JAXIS

   COMMON /HARDCOPY/ NHCDEV,NUMHCOPTS,IHCOPTS(2,20)
   COMMON /SCALEPOS/ XSC,YSC,SCALESIZE,NDEC
   COMMON /VFAC/ VFAC,VFACFORCE,NVEC
   COMMON /DRAWTHIS/  ndraw(50)
   COMMON /QNRGF/ JQN
   double precision :: VS(4,4)
   integer, external :: infoinput
   external :: highlight_form_line
!
   NLEVEL    = 3
   OPTION(1) = 'HARDCOPY DRIVER NUMBER                  ' ; IT(2*1)  = 2
   OPTION(2) = 'ENCAPSULATED POSTSCRIPT                 ' ; IT(2*2)  = 2
   OPTION(3) = 'LANDSCAPE                               ' ; IT(2*3)  = 2
   OPTION(4) = 'SIZE OF DOTS                            ' ; IT(2*4)  = 6
   OPTION(5) = 'SIZE OF NUMBERS                         ' ; IT(2*5)  = 6
   OPTION(6) = 'DEFAULT VALUE                           ' ; IT(2*6)  = 6
   OPTION(7) = 'LEFT SCREEN MARGIN                      ' ; IT(2*7)  = 6
   OPTION(8) = 'BOTTOM SCREEN MARGIN                    ' ; IT(2*8)  = 6
   OPTION(9) = 'PLOTTING AXIS YES/NO                    ' ; IT(2*9)  = 2
   OPTION(10)= 'SCALEFACTOR FOR VECTORS                 ' ; IT(2*10) = 6
   OPTION(11)= 'VECTOR   INTERVAL                       ' ; IT(2*11) = 2
   OPTION(12)= 'PLOTTING INTERVAL NTEK                  ' ; IT(2*12) = 2
   OPTION(13)= 'PLOT TO FILE YES/NO                     ' ; IT(2*13) = 2
   OPTION(14)= 'MINIMUM ZLEVEL RAAITEK                  ' ; IT(2*14) = 6
   OPTION(15)= 'MAXIMUM ZLEVEL RAAITEK                  ' ; IT(2*15) = 6
   OPTION(16)= 'PLOT TOP ROWS INFORMATION TEXT          ' ; IT(2*16) = 2
   OPTION(17)= 'Number of zoomshift intervals, press ;  ' ; IT(2*17) = 2
   OPTION(18)= 'Enable/disable minmax highlighting      ' ; IT(2*18) = 2
   OPTION(19)= 'Highlight specific net node number      ' ; IT(2*19) = 2
   OPTION(20)= 'Highlight specific net link number      ' ; IT(2*20) = 2
   OPTION(21)= 'Highlight specific flow node number     ' ; IT(2*21) = 2
   OPTION(22)= 'Highlight specific flow link number     ' ; IT(2*22) = 2
   OPTION(23)= 'Node waterdepth plotting threshold      ' ; IT(2*23) = 6
   OPTION(24)= 'Plot sideview in cheap perspective 1/0  ' ; IT(2*24) = 6
   OPTION(25)= 'Grain size fraction nr to plot          ' ; IT(2*25) = 2
   OPTION(26)= 'Show vertical reference profiles 1/0    ' ; IT(2*26) = 2
   OPTION(27)= 'display flownodes minus plotlin: 1      ' ; IT(2*27) = 2
   OPTION(28)= '                                        ' ; IT(2*28) = 2
   OPTION(29)= 'use OpenGL (0:no, 1:yes)                ' ; IT(2*29) = 2
   OPTION(30)= 'show bedlevels (0:no, 1:yes)            ' ; IT(2*30) = 2
   OPTION(31)= 'show waterbal. on screen (0:no, 1:yes)  ' ; IT(2*31) = 2
   OPTION(32)= 'kplotfrombedorsurface (1:bed, 2:surf)   ' ; IT(2*32) = 2
   OPTION(33)= 'kplotordepthaveraged  (1:kplot, 2:averg)' ; IT(2*33) = 2

!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6
   HELPM (1)  = '1:hgl 2:ps 4:rgh 6:bmp 7:pcx 8:dxf 9:cgm 10:wpm 11:wmf 12gl2'
   HELPM (2)  = '1 = ENCAPSULATED POSTSCRIPT (eps), 0 = POSTSCRIPT (ps)      '
   HELPM (3)  = '1 = LANDSCAPE, 0 = PORTRAIT                                 '
   HELPM (4 ) = 'REAL VALUE, SIZE OF DOTS RELATIVE TO SCREENSIZE             '
   HELPM (5 ) = 'REAL VALUE, SIZE OF NUMBERS, STANDARD VALUE 0.5             '
   HELPM (6 ) = 'REAL VALUE, FOR MISSING POINTS (PARAMETER dmiss)            '
   HELPM (7 ) = 'REAL VALUE, LEFT MARGIN AS FRACTION SCREEN SIZE (0.0-0.25)  '
   HELPM (8 ) = 'REAL VALUE, BOTTOM MARGIN AS FRACTION OF SCREEN (0.0-0.25)  '
   HELPM (9 ) = 'PLOT AXIS, 1 = YES, 0 = NO                                  '
   HELPM (10) = 'REAL VALUE, 2 MEANS 1 CM ON SCREEN IS APPROXIMATELY 2 M/S   '
   HELPM (11) = '1= plot every vector, 2=plot every second vector etc.       '
   HELPM (12) = 'INTEGER PLOTTING INTERVAL                                   '
   HELPM (13) = 'PLOT TO FILE AFTER EACH NTEK, 1 = YES, 0 = NO               '
   HELPM (14) = 'IF -999, AUTO ADJUST TO BOTTOM                              '
   HELPM (15) = 'IF -999, AUTO ADJUST TO ZNOD                                '
   HELPM (16) = '1 = YES, 0 = NO                                             '
   HELPM (17) = 'press ;key in editpol with polygon present after zooming in '
   HELPM (18) = 'Enable/disable highlighting of nodes/links/nodmin. (1/0)    '
   HELPM (19) = 'Number of the net node to be highlighted.                   '
   HELPM (20) = 'Number of the net link to be highlighted.                   '
   HELPM (21) = 'Number of the flow node to be highlighted.                  '
   HELPM (22) = 'Number of the flow link to be highlighted.                  '
   HELPM (23) = 'Only plot node if waterdepth hs > wetplot                   '
   HELPM (24) = 'Plot raai with cheap perspective 1/0                        '
   HELPM (25) = 'Integer, <= mxgr                                            '
   HELPM (26) = '1=yes , 0=no , only for 3D                                  '
   HELPM (27) = '1=yes , 0=no                                                '
   HELPM (28) = 'Intentionally left blank                                    '
   HELPM (29) = '1=yes , 0=no                                                '
   HELPM (30) = '1=yes , 0=no                                                '
   HELPM (31) = '1=yes , 0=no                                                '
   HELPM (32) = '1=bed ,-1=surf                                              '
   HELPM (33) = '1=kplot , 2=depth averaged                                  '


   NUMPARACTUAL = NUMPAR
   NUMFLDACTUAL = 2*NUMPARACTUAL
   IR = 0
   DO I = 1,NUMPAR
      IL = IR + 1
      IR = IL + 1
      IX(IL) = 13
!      IX(IR) = 53
      IX(IR) = 95
      IY(IL) = I ! Many menu lines, use dense view to fit on screen.
      IY(IR) = I
!      IS(IL) = 40
      IS(IL) = 82
      IS(IR) = 10
      IT(IL) = 1001
    ENDDO


!  Initialise
   CALL IWinWordWrap('OFF')
   CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
   CALL INHIGHLIGHT('WHITE','RED')
   IW     = NPOS(3)
   IXP    = NPOS(1) + (IWS-IW)/2
   IYP    = NPOS(2)
   IH     = IHS - 9

!  Header of filewindow
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP,IW,1)
   CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
   CALL IWinOutCentre(1,trim(company)//'-'//trim(product_name)//' DISPLAY PARAMETER FORM')
   CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
!
!  Explain keyfunctions in bottom window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IHS-1,IW,2)
   CALL IWinOutStringXY(1,1,'move = , Tab, confirm = Enter, no change = Esc, help = F3')
   CALL IWinOutStringXY(1,2,'right mouse = Enter, click outside window = Esc')

!  Filewindow is middelste window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP+3,IW,IH)

   CALL InControlKey(29,129)
   CALL InControlKey(30,128)

!  NUMWNH = InfoWindow(1)
!  CALL IWinSelect(NUMWNH)

!  Define a new form by supplying arrays containing
!  field positions, sizes and types
   CALL IFORMDEFINE('W',NUMFLDACTUAL,IX,IY,IS,IT)

!  Define a help field and define help strings
!  for 2 of the 4 input fields
   CALL IFORMHELP(13,IH,60)

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      CALL IFORMPUTSTRING (IL,OPTION(I))
      CALL IFORMPUTHELP   (IR,HELPM(I))
      CALL IFORMATTRIBUTEN(IR,0,0,7)
   ENDDO

   DO I = 1,NUMHCOPTs
      IF (IHCOPTS(1,I) .EQ. 22) JAEPS  = IHCOPTS(2,I)
      IF (IHCOPTS(1,I) .EQ. 5)  JALAND = IHCOPTS(2,I)
   ENDDO

   CALL IFORMPUTINTEGER(2*1 , NHCDEV)
   CALL IFORMPUTINTEGER(2*2 , JAEPS)
   CALL IFORMPUTINTEGER(2*3 , JALAND)
   CALL IFormPutDouble (2*4 , CR    ,'(F10.3)')
   CALL IFormPutDouble (2*5 , TSIZE ,'(F10.3)')
   CALL IFormPutDouble (2*6 , dmiss,'(F10.3)')
   CALL IFormPutDouble (2*7 , XLEFT,'(F10.3)')
   CALL IFormPutDouble (2*8 , YBOT ,'(F10.3)')
   CALL IFORMPUTINTEGER(2*9 , JAXIS)
   CALL IFormPutDouble (2*10, VFAC ,'(F10.3)')
   CALL IFormPutinteger(2*11, nvec)
   CALL IFORMPUTINTEGER(2*12, NTEK)
   CALL IFORMPUTINTEGER(2*13, PLOTTOFILE)
   CALL IFormPutDouble (2*14, ZMINrai ,'(F10.3)')
   CALL IFormPutDouble (2*15, ZMAXrai ,'(F10.3)')
   CALL IFORMPUTINTEGER(2*16, JTEXTFLOW)
   CALL IFORMPUTINTEGER(2*17, numzoomshift)
   CALL IFORMPUTINTEGER(2*18, jaHighlight)
   CALL IFORMPUTINTEGER(2*19, nhlNetNode)
   CALL IFORMPUTINTEGER(2*20, nhlNetLink)
   CALL IFORMPUTINTEGER(2*21, nhlFlowNode)
   CALL IFORMPUTINTEGER(2*22, nhlFlowLink)
   CALL IFormPutDouble (2*23, wetplot,'(F10.5)')
   CALL IFormPutDouble (2*24, YFAC,'(F10.5)')
   CALL IFORMPUTINTEGER(2*25, jgrtek)    ! grain size fraction to plot
   CALL IFORMPUTINTEGER(2*26, ndraw(35)) ! 1/0
   CALL IFORMPUTINTEGER(2*27, ndraw(36)) ! 1/0
   CALL IFORMPUTINTEGER(2*29, jaOpenGL) ! 1/0
   CALL IFORMPUTINTEGER(2*30, ndraw(39)) ! 1/0
   CALL IFORMPUTINTEGER(2*31, ndraw(40)) ! show waterbal
   CALL IFORMPUTINTEGER(2*32, kplotfrombedorsurface) ! kplotupordown
   CALL IFORMPUTINTEGER(2*33, kplotordepthaveraged)  !

   !  Display the form with numeric fields left justified
   !  and set the initial field to number 2
   CALL IOUTJUSTIFYNUM('L')
   IFEXIT = 2
   call IFormAttribute(IFEXIT-1, 'BU', ' ', ' ')
   CALL IFORMSHOW()

30 CONTINUE
   IFINIT = IFEXIT
   CALL IFormEditUser(IFINIT, IFEXIT, highlight_form_line)
!  check for Help, Confirm, Quit
   KEY = INFOINPUT(55)
   IF (KEY .EQ. -2) THEN
       NBUT = INFOINPUT(61)
       IF (NBUT .GE. 1) THEN
          IMP = INFOINPUT(62) + 1
          INP = INFOINPUT(63) + 1
          IF (IMP .GE. IXP .AND. IMP .LT. IXP+IW .AND.   &
              INP .GE. IYP+3   .AND. INP .LT. IYP+IH+3+2  ) THEN
             IF (NBUT .EQ. 1) THEN
                KEY = 21
             ELSE
                KEY = 22
             ENDIF
          ELSE
             KEY = 23
          ENDIF
       ENDIF
   ELSE IF (KEY .EQ. -1) THEN
      KEY = INFOINPUT(57)
   ENDIF
   IF (KEY .EQ. 26) THEN
       WRDKEY = OPTION(IFEXIT/2)
       CALL HELP(WRDKEY,NLEVEL)
   ELSE IF (KEY .EQ. 22 .OR. KEY .EQ. 23) THEN
       IF (KEY .EQ. 22) THEN
           CALL IFORMGETINTEGER(2*1 , NHCDEV)
           CALL IFORMGETINTEGER(2*2 , JAEPS)
           CALL IFORMGETINTEGER(2*3 , JALAND)
           CALL IFormGetDouble (2*4 , CR)
           CALL IFormGetDouble (2*5 , TSIZE)
           CALL IFormGetDouble (2*6 , dmiss)
           CALL IFormGetDouble (2*7 , XLEFT)
           CALL IFormGetDouble (2*8 , YBOT )
           CALL IFORMGETINTEGER(2*9 , JAXIS)
           CALL IFormGetDouble (2*10, VFAC )
           CALL IFormGetinteger(2*11, nvec )
           CALL IFORMGETINTEGER(2*12, NTEK)
           CALL IFORMGETINTEGER(2*13, PLOTTOFILE)
           CALL IFormGetDouble (2*14, ZMINrai)
           CALL IFormGetDouble (2*15, ZMAXrai)
           CALL IFORMGETINTEGER(2*16, jtextflow)
           CALL IFORMGETINTEGER(2*17, numzoomshift)
           CALL IFORMGETINTEGER(2*18, jaHighlight)
           CALL IFORMGETINTEGER(2*19, nhlNetNode)
           CALL IFORMGETINTEGER(2*20, nhlNetLink)
           CALL IFORMGETINTEGER(2*21, nhlFlowNode)
           CALL IFORMGETINTEGER(2*22, nhlFlowLink)
           CALL IFormGetDouble (2*23, wetplot)
           CALL IFormGetDouble (2*24, yfac)
           CALL IFORMGETINTEGER(2*25, jgrtek )
           jgrtek = max(1,min(jgrtek,mxgr))
           CALL IFORMGETINTEGER(2*26, ndraw(35) )
           CALL IFORMGETINTEGER(2*27, ndraw(36) )
           CALL IFORMGETINTEGER(2*29, jaOpenGL  )
           CALL IFORMGETINTEGER(2*30, ndraw(39) )
           CALL IFORMGETINTEGER(2*31, ndraw(40) )
           CALL IFORMGETINTEGER(2*32, kplotfrombedorsurface )
           CALL IFORMGETINTEGER(2*33, kplotordepthaveraged  )

           VFAC  = MAX( 0d0, VFAC)
           VFACFORCE  = MAX( 0d0, VFACFORCE)
           XLEFT = MAX( 0d0,(MIN(XLEFT,0.25d0) ) )
           YBOT  = MAX( 0d0,(MIN(YBOT ,0.25d0) ) )
           JAXIS = MIN( 1  ,(MAX(JAXIS,0) ) )
           IF (JAXIS .EQ. 1) THEN
              IF (XLEFT .EQ. 0) XLEFT = .15
              IF (YBOT  .EQ. 0) YBOT  = .10
           ENDIF
           ! call WEAREL()
           DO I = 1,NUMHCOPTS
              IF (IHCOPTS(1,I) .EQ. 22) IHCOPTS(2,I) = JAEPS
              IF (IHCOPTS(1,I) .EQ. 5)  IHCOPTS(2,I) = JALAND
           ENDDO
           CALL SETTEXTSIZE()
           if (plottofile == 1) then
               ndraw(10) = 1
           end if
       ENDIF
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL IWinClose(1)
       RETURN
   ELSE IF (KEY .EQ. 21) THEN
      IF (IFEXIT .EQ. 1 .OR. IFEXIT .EQ. 3) THEN
          WRDKEY = HELPM(IFEXIT)
          CALL HELP(WRDKEY,NLEVEL)
      ENDIF
   ENDIF
   GOTO 30

   END SUBROUTINE CHANGEDISPLAYPARAMETERS
