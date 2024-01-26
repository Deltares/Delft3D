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

      SUBROUTINE TEXTPARAMETERS()
      use unstruc_display
      use dflowfm_version_module, only : company, product_name

      implicit none
      integer :: i
      integer :: ifexit
      integer :: ifinit
      integer :: ih
      Integer :: il
      integer :: imp
      integer :: inp
      integer :: ir
      integer :: iw
      integer :: ixp
      integer :: iyp
      integer :: key
      integer :: nbut
      integer :: nlevel
      integer :: numfld
      integer :: numpar
      double precision :: txtimsize
      double precision :: txtimx
      double precision :: txtimy
      integer, external :: infoinput
      external :: highlight_form_line

      PARAMETER  (NUMPAR = 9, NUMFLD = 2*NUMPAR)
      INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
      CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
      CHARACTER TXTIM*60

      COMMON /HELPNOW/ WRDKEY,NLEVEL
      COMMON /TEXTIM/  TXTIMSIZE, TXTIMX, TXTIMY, TXTIM
!
      NLEVEL    = 3
      OPTION(1) = 'LINE 1:'
      OPTION(2) = 'LINE 2:'
      OPTION(3) = 'LINE 3:'
      OPTION(4) = 'FNTSIZ:'
      OPTION(5) = 'XPOS  :'
      OPTION(6) = 'YPOS  :'
      OPTION(7) = 'SIZE TM'
      OPTION(8) = 'XPOS TM'
      OPTION(9) = 'YPOS TM'

!      123456789012345678901234567890123456789012345678901234567890
!               1         2         3         4         5         6
      HELPM (1) =  'FIRST TEXTLINE                                              '
      HELPM (2) =  'SECOND TEXTLINE                                             '
      HELPM (3) =  'THIRD TEXTLINE                                              '
      HELPM (4) =  'FONTSIZE, ONLY FOR TEXTLINES, DEFAULT FONTSIZE = 0.5        '
      HELPM (5) =  'RELATIVE SCREEN X POSITION, 0 = LEFT, 1 = RIGHT             '
      HELPM (6) =  'RELATIVE SCREEN Y POSITION, 0 = BOTTOM, 1 = TOP             '
      HELPM (7) =  'FONTSIZE, FOR TIME/DATE IN ANIMATE INCREMENTAL              '
      HELPM (8) =  'SCREEN X POSITION, FOR TIME/DATE IN ANIMATE INCREMENTAL     '
      HELPM (9) =  'SCREEN Y POSITION, FOR TIME/DATE IN ANIMATE INCREMENTAL     '

      IR = 0
      DO 10 I = 1,NUMPAR
         IL = IR + 1
         IR = IL + 1
         IX(IL) = 2
!         IX(IR) = 14
         IX(IR) = 56
         IY(IL) = 2*I
         IY(IR) = 2*I
!         IS(IL) = 40
         IS(IL) = 82
         IS(IR) = 60
         IT(IL) = 1001
         IT(IR) = 1
         IF (I .GE. 4) THEN
            IS(IR) = 5
            IT(IR) = 6
         ENDIF
   10 CONTINUE

      CALL SAVEKEYS()
!     Initialise
      CALL IWinWordWrap('OFF')
      CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
      CALL INHIGHLIGHT('WHITE','RED')
      IW     = NPOS(3)
      IXP    = NPOS(1) + (IWS-IW)/2
      IYP    = NPOS(2)
      IH     = IHS - 9

!     Header of filewindow
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IYP,IW,1)
      CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
      CALL IWinOutCentre(1,trim(company)//'-'//trim(product_name) // ' PARAMETER FORM')
      CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
!
!     Explain keyfunctions in bottom window
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IHS-1,IW,2)
      CALL IWinOutStringXY(1,1,'move = , Tab, confirm = Enter, no change = Esc, help = F3')
      CALL IWinOutStringXY(1,2,'right mouse = Enter, click outside window = Esc')

!     Filewindow is middelste window
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IYP+3,IW,IH)

      CALL InControlKey(29,129)
      CALL InControlKey(30,128)

!     NUMWNH = InfoWindow(1)
!     CALL IWinSelect(NUMWNH)

!     Define a new form by supplying arrays containing
!     field positions, sizes and types
      CALL IFORMDEFINE('W',NUMFLD,IX,IY,IS,IT)

!     Define a help field and define help strings
!     for 2 of the 4 input fields
      CALL IFORMHELP(13,IH,60)

      IR = 0
      DO 20 I = 1,NUMPAR
         IL = IR + 1
         IR = IL + 1
         CALL IFORMPUTSTRING (IL,OPTION(I))
         CALL IFORMPUTHELP   (IR,HELPM(I))
         CALL IFORMATTRIBUTEN(IR,0,0,7)
   20 CONTINUE

      CALL IFORMPUTSTRING (2*1,TXLIN(1))
      CALL IFORMPUTSTRING (2*2,TXLIN(2))
      CALL IFORMPUTSTRING (2*3,TXLIN(3))
      CALL IFormPutDouble (2*4,TXSIZE,'(F5.2)')
      CALL IFormPutDouble (2*5,TXXpos   ,'(F5.2)')
      CALL IFormPutDouble (2*6,TXYpos   ,'(F5.2)')
      CALL IFormPutDouble (2*7,TXTIMSIZE,'(F5.2)')
      CALL IFormPutDouble (2*8,TXTIMX   ,'(F5.2)')
      CALL IFormPutDouble (2*9,TXTIMY   ,'(F5.2)')

!  Display the form with numeric fields left justified
!  and set the initial field to number 2
   CALL IOUTJUSTIFYNUM('L')
   IFEXIT = 2
   call IFormAttribute(IFEXIT-1, 'BU', ' ', ' ')
   CALL IFORMSHOW()

   30 CONTINUE
      IFINIT = IFEXIT
      CALL IFormEditUser(IFINIT, IFEXIT, highlight_form_line)
!     check for Help, Confirm, Quit
      KEY = INFOINPUT(55)
      IF (KEY .EQ. -2) THEN
          NBUT = INFOINPUT(61)
          IF (NBUT .GE. 1) THEN
             IMP = INFOINPUT(62) + 1
             INP = INFOINPUT(63) + 1
             IF (IMP .GE. IXP .AND. IMP .LT. IXP+IW .AND.    &
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
             CALL IFORMGETSTRING (2*1,TXLIN(1))
             CALL IFORMGETSTRING (2*2,TXLIN(2))
             CALL IFORMGETSTRING (2*3,TXLIN(3))
             CALL IFormGetDouble (2*4,TXSIZE  )
             CALL IFormGetDouble (2*5,TXXpos     )
             CALL IFormGetDouble (2*6,TXYpos     )
             CALL IFormGetDouble (2*7,TXTIMSIZE  )
             CALL IFormGetDouble (2*8,TXTIMX     )
             CALL IFormGetDouble (2*9,TXTIMY     )
             TXSIZE = MAX(0d0,MIN(TXSIZE,10d0))
             TXXpos = MAX(0d0,MIN(TXXpos,1d0))
             TXYpos = MAX(0d0,MIN(TXYpos,1d0))
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

      END
