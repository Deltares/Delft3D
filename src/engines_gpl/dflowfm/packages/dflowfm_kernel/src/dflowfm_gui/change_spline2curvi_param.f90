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

!> spline2curvi parameter menu
subroutine change_spline2curvi_param(jacancelled)
   use M_GRIDSETTINGS
   use unstruc_display
   use dflowfm_version_module, only : company, product_name
   use m_spline2curvi

   implicit none
   integer, intent(out) :: jacancelled !< Whether or not (1/0) user has pressed 'Esc' in parameter screen.

   integer :: i
   integer :: ifexit
   integer :: ifinit
   integer :: ih
   integer :: il
   integer :: imp
   integer :: inp
   integer :: ir
   integer :: iw
   integer :: ixp
   integer :: iyp
   integer :: key
   integer :: nbut
   integer :: nlevel
   integer :: numfldactual
   integer :: numparactual
   integer :: idum=0

   integer, parameter :: NUMPAR = 14, NUMFLD = 2*NUMPAR
   INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
   CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
   COMMON /HELPNOW/ WRDKEY,NLEVEL
   integer, external :: infoinput
   external :: highlight_form_line

   jacancelled = 0
   NLEVEL    = 4

   OPTION(1) = 'MAXIMUM NUMBER OF GRIDCELLS ALONG SPLINE' ; IT(1*2)  = 2
   OPTION(2) = 'MAXIMUM NUMBER OF GRIDCELLS PERP. SPLINE' ; IT(2*2)  = 2
   OPTION(3) = 'ASPECT RATIO OF FIRST GRID LAYER        ' ; IT(3*2)  = 6
   OPTION(4) = 'GRID LAYER HEIGHT GROWTH FACTOR         ' ; IT(4*2)  = 6
   OPTION(5) = 'MAXIMUM GRID LENGTH ALONG CENTER SPLINE ' ; IT(5*2)  = 6
   OPTION(6) = 'CURVATURE-ADAPTED GRID SPACING     (0,1)' ; IT(6*2)  = 2
   OPTION(7) = 'GROW GRID OUTSIDE FIRST PART       (0,1)' ; IT(7*2)  = 2
   OPTION(8) = 'MAX. NUM. OF GRIDCELL PERP. IN UNI. PART' ; IT(8*2)  = 2
   OPTION(9) = '                                        ' ; IT(9*2)  = 2
   OPTION(10) = 'GRIDPTS. ON TOP OF EACH OTHER TOLERANCE ' ; IT(10*2)  = 6
   OPTION(11) = 'MINIMUM ABS. SINE OF CROSSING ANGLES   ' ; IT(11*2) = 6
   OPTION(12) = 'PREVENT COLL.S W/OTHER GRIDPARTS  (0,1) ' ; IT(12*2) = 2
   OPTION(13) = '                                        ' ; IT(13*2) = 2
   OPTION(14) = 'UNIFORM GRIDSIZE (NETBND2GRID ONLY) (m) ' ; IT(14*2) = 6
!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6
   HELPM (1) = 'INTEGER VALUE <                                             '
   HELPM (2) = 'INTEGER VALUE <                                             '
   HELPM (3) = 'REAL    VALUE <                                             '
   HELPM (4) = 'REAL    VALUE <                                             '
   HELPM (5) = 'REAL    VALUE <                                             '
   HELPM (6) = 'INTEGER VALUE <                                             '
   HELPM (7) = 'INTEGER VALUE <                                             '
   HELPM (8) = 'INTEGER VALUE <                                             '
   HELPM (9) = '                                                            '
   HELPM (10) = 'REAL    VALUE <                                             '
   HELPM (11) = 'REAL    VALUE <                                             '
   HELPM (12) = 'INTEGER VALUE <                                             '
   HELPM (13) = '                                                            '
   HELPM (14) = 'REAL    VALUE <                                             '

   CALL SAVEKEYS()
   NUMPARACTUAL = NUMPAR
   NUMFLDACTUAL = 2*NUMPARACTUAL

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL     = IR + 1  ; IR     = IL + 1
      IS(IL) = 82      ; IS(IR) = 10
      IX(IL) = 10      ; IX(IR) = 100
      IY(IL) = 2*I     ; IY(IR) = 2*I
      IT(IL) = 1001    ! ir staat hierboven
   ENDDO

   ! Initialise
   CALL IWinWordWrap('OFF')
   CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
   CALL INHIGHLIGHT('WHITE','RED')
   IW   = NPOS(3)
   IXP = NPOS(1) + (IWS-IW)/2
   IYP  = NPOS(2)
   IH  = IHS - 9

   ! Header of filewindow
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP,IW,1)
   CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
   CALL IWinOutCentre(1,trim(company)//'-'//trim(product_name)// ' PARAMETER FORM')
   CALL ITEXTCOLOURN(HLPFOR,HLPBCK)

   ! Explain keyfunctions in bottom window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IHS-1,IW,2)
   CALL IWinOutStringXY(1,1,'move = , Tab, confirm = Enter, no change = Esc, help = F3')
   CALL IWinOutStringXY(1,2,'right mouse = Enter, click outside window = Esc')

   ! Filewindow is middelste window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP+3,IW,IH)
   CALL InControlKey(29,129)
   CALL InControlKey(30,128)

   ! Define a new form by supplying arrays containing Field positions, sizes and types
   CALL IFORMDEFINE('W',NUMFLDACTUAL,IX,IY,IS,IT)

   ! Define a help field and define help strings for 2 of the 4 input fields
   CALL IFORMHELP(13,IH,60)

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      CALL IFORMPUTSTRING (IL,OPTION(I))
      CALL IFORMPUTHELP   (IR,HELPM(I))
      CALL IFORMATTRIBUTEN(IR,0,0,7)
   ENDDO

   CALL IFORMPUTINTEGER (2*1, MFAC               )
   CALL IFORMPUTINTEGER (2*2, NFAC               )
   CALL IFORMPUTDOUBLE  (2*3, daspect,   '(F7.3)')
!   CALL IFORMPUTDOUBLE  (2*4, maxaspect, '(F7.3)')
   CALL IFORMPUTDOUBLE  (2*4, dgrow,     '(F7.3)')
   CALL IFORMPUTDOUBLE  (2*5, dwidth,    '(F11.3)')
   CALL IFORMPUTINTEGER (2*6, jacurv             )
   CALL IFORMPUTINTEGER (2*7, jaoutside          )
   CALL IFORMPUTINTEGER (2*8, NFACUNIMAX         )
!   CALL IFORMPUTINTEGER (2*9, idum               )
   CALL IFORMPUTDOUBLE  (2*10, dtolLR,    '(F11.5)')
   CALL IFORMPUTDOUBLE  (2*11, dtolcos,   '(F11.3)')
   CALL IFORMPUTINTEGER (2*12, jaCheckFrontCollision)
   CALL IFORMPUTDOUBLE  (2*14, dunigridsize,   '(F11.3)')

   NFAC = max(NFAC,NFACUNIMAX)

   ! Display the form with numeric fields left justified and set the initial field to number 2
   CALL IOUTJUSTIFYNUM('L')
   IFEXIT = 2
   call IFormAttribute(IFEXIT-1, 'BU', ' ', ' ')
   CALL IFORMSHOW()

30 CONTINUE
   IFINIT = IFEXIT
   CALL IFormEditUser(IFINIT, IFEXIT, highlight_form_line)
   ! check for Help, Confirm, Quit
   KEY = INFOINPUT(55)
   IF (KEY .EQ. -2) THEN
       NBUT = INFOINPUT(61)
       IF (NBUT .GE. 1) THEN
          IMP = INFOINPUT(62) + 1
          INP = INFOINPUT(63) + 1
          IF (IMP .GE. IXP .AND. IMP .LT. IXP+IW .AND.  &
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
           CALL IFORMGETINTEGER (2*1 , MFAC     )
           CALL IFORMGETINTEGER (2*2 , NFAC     )
           CALL IFORMGETDOUBLE  (2*3 , daspect  )
!           CALL IFORMGETDOUBLE  (2*4 , maxaspect)
           CALL IFORMGETDOUBLE  (2*4 , dgrow    )
           CALL IFORMGETDOUBLE  (2*5 , dwidth   )
           CALL IFORMGETINTEGER (2*6 , jacurv   )
           CALL IFORMGETINTEGER (2*7 , jaoutside)
           CALL IFORMGETINTEGER (2*8 , NFACUNIMAX)
!           CALL IFORMGETINTEGER (2*9 , idum)
           CALL IFORMGETDOUBLE  (2*10, dtolLR)
           CALL IFORMGETDOUBLE  (2*11, dtolcos)
           CALL IFORMGETINTEGER (2*12, jaCheckFrontCollision)
           CALL IFORMGETDOUBLE  (2*14, dunigridsize)
       ELSEIF (KEY .EQ. 23) THEN
          jacancelled = 1
       ENDIF
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL RESTOREKEYS()
       RETURN
   ELSE IF (KEY .EQ. 21) THEN
      IF (IFEXIT .EQ. 1 .OR. IFEXIT .EQ. 3) THEN
          WRDKEY = HELPM(IFEXIT)
          CALL HELP(WRDKEY,NLEVEL)
      ENDIF
   ENDIF
   GOTO 30

end subroutine change_spline2curvi_param
