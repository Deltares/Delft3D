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

   SUBROUTINE MAKENETPARAMETERS()
   USE M_MAKENET
   use unstruc_display
   use dflowfm_version_module, only : company, product_name
   implicit none
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

   integer, parameter :: NUMPAR = 15, NUMFLD = 2*NUMPAR
   INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
   CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
   COMMON /HELPNOW/ WRDKEY,NLEVEL
   integer, external :: infoinput
   external :: highlight_form_line

   NLEVEL    = 4
   OPTION(1) = 'MAZE TYPE: SQUARE, WIEBER, HEX, TRI  ( )' ; IT(1*2)  = 2
   OPTION(2) = 'NR OF MAZES X                        ( )' ; IT(2*2)  = 2
   OPTION(3) = 'NR OF MAZES Y                        ( )' ; IT(3*2)  = 2
   OPTION(4) = 'MAZE ANGLE     1-90                (deg)' ; IT(4*2)  = 6
   OPTION(5) = 'MAZE SIZE                            (m)' ; IT(5*2)  = 6
   OPTION(6) = 'LINE THICKNESS                      (mm)' ; IT(6*2)  = 6
   OPTION(7) = 'ORIGIN X                             (m)' ; IT(7*2)  = 6
   OPTION(8) = 'ORIGIN Y                             (m)' ; IT(8*2)  = 6
   OPTION(9) = 'ORIGIN Z                             (m)' ; IT(9*2)  = 6
   OPTION(10)= 'DX (FOR TYPE 0 ONLY)                 (m)' ; IT(10*2) = 6
   OPTION(11)= 'DY (FOR TYPE 0 ONLY)                 (m)' ; IT(11*2) = 6
   OPTION(12)= '                                        ' ; IT(12*2) = 1001
   OPTION(13)= 'MAZE SIZE HORIZONTAL PART HEXAGON   (cm)' ; IT(13*2) = 6
   OPTION(14)= 'Hanging nodes when dx <              (m)' ; IT(14*2) = 6
   OPTION(15)= 'Radius                               (m)' ; IT(15*2) = 6


!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6
   HELPM (1) = &
   'SQUARE=0, WIEBER=1, HEX1=2, HEX2=3, TRIA=4, GLOBALNET=6     '
   HELPM (2) = &
   '                                                            '
   HELPM (3) = &
   '                                                            '
   HELPM (4) = &
   '                                                            '
   HELPM (5) = &
   '                                                            '
   HELPM (6) = &
   '                                                            '
   HELPM (6) = &
   '                                                            '
   HELPM (7) = &
   '                                                            '
   HELPM (8) = &
   '                                                            '
   HELPM (9) = &
   '                                                            '
   HELPM (10)= &
   '                                                            '
   HELPM (11)= &
   '                                                            '
   HELPM (12)= &
   '                                                            '
   HELPM (13)= &
   'ONLY VALID FOR MAZE TYPE = 2, HEXAGON                       '
   HELPM (13)= &
   'ONLY VALID FOR MAZE TYPE = 6, GLOBALNET                     '


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

   CALL IFORMPUTINTEGER (2*1, NTYP           )
   CALL IFORMPUTINTEGER (2*2, NRX            )
   CALL IFORMPUTINTEGER (2*3, NRY            )
   CALL IFormPutDouble  (2*4, ANGLE, '(F7.3)')
   CALL IFormPutDouble  (2*5, SIZE,  '(F7.3)')
   CALL IFormPutDouble  (2*6, THICK, '(F7.3)')
   CALL IFormPutDouble  (2*7, X0   , '(F7.3)')
   CALL IFormPutDouble  (2*8, Y0   , '(F7.3)')
   CALL IFormPutDouble  (2*9, Z0   , '(F7.3)')
   CALL IFormPutDouble  (2*10,DX0  , '(F7.3)')
   CALL IFormPutDouble  (2*11,DY0  , '(F7.3)')
   CALL IFormPutDouble  (2*13,HSIZE, '(F7.3)')
   CALL IFormPutDouble  (2*14,DXdouble, '(F7.0)')
   CALL IFormPutDouble  (2*15,Radius,   '(F7.0)')


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
           CALL IFORMGETINTEGER (2*1 , NTYP )
           CALL IFORMGETINTEGER (2*2 , NRX  )
           CALL IFORMGETINTEGER (2*3 , NRY  )
           CALL IFormGetDouble  (2*4 , ANGLE)
           CALL IFormGetDouble  (2*5 , SIZE )
           CALL IFormGetDouble  (2*6 , THICK)
           CALL IFormGetDouble  (2*7 , X0   )
           CALL IFormGetDouble  (2*8 , Y0   )
           CALL IFormGetDouble  (2*9 , Z0   )
           CALL IFormGetDouble  (2*10, DX0  )
           CALL IFormGetDouble  (2*11, DY0  )
           CALL IFormGetDouble  (2*13, HSIZE)
           CALL IFormGetDouble  (2*14, DXdouble)
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

   END SUBROUTINE MAKENETPARAMETERS
