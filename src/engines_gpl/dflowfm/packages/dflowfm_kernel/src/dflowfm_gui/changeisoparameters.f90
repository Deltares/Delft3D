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

      SUBROUTINE CHANGEISOPARAMETERS()
      use unstruc_display
      use dflowfm_version_module, only : company, product_name

      implicit none
      double precision :: dv, dv2
      double precision :: dvi, dvi2
      double precision :: dvnu
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
      integer :: jaauto, jaauto2
      integer :: key
      integer :: nbut
      integer :: ncols, ncols2
      integer :: ndec
      Integer :: nie, nie2
      integer :: nien
      integer :: nis, nis2
      integer :: nisn
      integer :: nlevel
      integer :: numfld
      integer :: numpar
      integer :: nv, nv2
      integer :: nvn
      double precision :: scalesize
      double precision :: val, val2
      double precision :: vmax, vmax2
      double precision :: vmaxn
      double precision :: vmin, vmin2
      double precision :: vminn
      double precision :: xsc
      double precision :: ysc
      PARAMETER  (NUMPAR = 19, NUMFLD = 2*NUMPAR)
      INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
      CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
      COMMON /HELPNOW/ WRDKEY,NLEVEL
      COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO
      COMMON /DEPMAX2/ VMAX2,VMIN2,DV2,VAL2(256),NCOLS2(256),NV2,NIS2,NIE2,JAAUTO2
      COMMON /SCALEPOS/ XSC,YSC,SCALESIZE,NDEC
      integer, external :: infoinput
      external :: highlight_form_line
!
      NLEVEL    = 3
      OPTION(1) = 'AUTOSCALE ON OR OFF                     '
      OPTION(2) = 'NUMBER OF ISOCOLOURS                    '
      OPTION(3) = 'MINIMUM ISOLINE VALUE                   '
      OPTION(4) = 'MAXIMUM ISOLINE VALUE                   '
      OPTION(5) = 'ISOLINE INTERVAL                        '
      OPTION(6) = 'COLOUR NUMBER OF FIRST COLOUR           '
      OPTION(7) = 'COLOUR NUMBER OF LAST  COLOUR           '
      OPTION(8) = 'X COOR LOWER LEFT CORNER OF LEGEND (0-1)'
      OPTION(9) = 'Y COOR LOWER LEFT CORNER OF LEGEND (0-1)'
      OPTION(10)= 'NUMBER OF DECIMALS COLOURSCALE LEGEND   '
      OPTION(11)= 'FONTSIZE COLOURSCALE LEGEND (0.5-1.5)   '
      OPTION(12)= 'Settings for secondary legend:          '
      OPTION(13)= '  AUTOSCALE ON OR OFF                   '
      OPTION(14)= '  NUMBER OF ISOCOLOURS                  '
      OPTION(15)= '  MINIMUM ISOLINE VALUE                 '
      OPTION(16)= '  MAXIMUM ISOLINE VALUE                 '
      OPTION(17)= '  ISOLINE INTERVAL                      '
      OPTION(18)= '  COLOUR NUMBER OF FIRST COLOUR         '
      OPTION(19)= '  COLOUR NUMBER OF LAST  COLOUR         '
!      123456789012345678901234567890123456789012345678901234567890
!               1         2         3         4         5         6
      HELPM (1) = 'INTEGER VALUE , AUTOSCALE OFF = 0, ON = 1                   '
      HELPM (2) = 'INTEGER VALUE =< 30                                         '
      HELPM (3) = 'REAL VALUE, IF CHANGED, AUTOSCALE IS TURNED OFF             '
      HELPM (4) = 'REAL VALUE, IF CHANGED, AUTOSCALE IS TURNED OFF             '
      HELPM (5) = 'REAL VALUE, IF CHANGED, AUTOSCALE IS TURNED OFF             '
      HELPM (6) = 'INTEGER VALUE, STARTINDEX OF ISOCOLOURS (0-255) DEFAULT 46  '
      HELPM (7) = 'INTEGER VALUE, ENDINDEX OF ISOCOLOURS (0-255)   DEFAULT 224 '
      HELPM (8) = 'REAL VALUE (0-1), X COORDINATE LOWER LEFT CORNER LEGEND     '
      HELPM (9) = 'REAL VALUE (0-1), Y COORDINATE LOWER LEFT CORNER LEGEND     '
      HELPM (10)= 'INTEGER, NR OF DECIMALS IN COLOURSCALE LEGEND               '
      HELPM (11)= 'REAL VALUE, FONTSIZE OF COLOURSCALE LEGEND TEXT, DEFAULT 0.5'
      HELPM (12)= '                                                            '
      HELPM (13)= 'INTEGER VALUE , AUTOSCALE OFF = 0, ON = 1                   '
      HELPM (14)= 'INTEGER VALUE =< 30                                         '
      HELPM (15)= 'REAL VALUE, IF CHANGED, AUTOSCALE IS TURNED OFF             '
      HELPM (16)= 'REAL VALUE, IF CHANGED, AUTOSCALE IS TURNED OFF             '
      HELPM (17)= 'REAL VALUE, IF CHANGED, AUTOSCALE IS TURNED OFF             '
      HELPM (18)= 'INTEGER VALUE, STARTINDEX OF ISOCOLOURS (0-255) DEFAULT 46  '
      HELPM (19)= 'INTEGER VALUE, ENDINDEX OF ISOCOLOURS (0-255)   DEFAULT 224 '

      IR = 0
      DO 10 I = 1,NUMPAR
         IL = IR + 1
         IR = IL + 1
         IX(IL) = 13
         IX(IR) = 95
         IY(IL) = 2*I
         IY(IR) = 2*I
         IS(IL) = 82
         IS(IR) = 10
         IT(IL) = 1001
         IF (I .GE. 3 .AND. I .LE. 5 .or. I .GE. 15 .AND. I .LE. 17 ) THEN
            ! Real values:
            IT(IR) = 6
         ELSE
            ! Integer values:
            IT(IR) = 2
         ENDIF
   10 CONTINUE
      IT(2*8)  = 6
      IT(2*9)  = 6
      IT(2*11) = 6

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
      CALL IWinOutCentre(1,trim(company)//'-'//trim(product_name) // ' ISOPARAMETER FORM')
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

      CALL IFORMPUTINTEGER( 2,JAAUTO)
      CALL IFORMPUTINTEGER( 4,NV)
      CALL IFormPutDouble ( 6,VMIN ,'(F10.3)')
      CALL IFormPutDouble ( 8,VMAX ,'(F10.3)')
      DVI = DV/(NV-1)
      CALL IFormPutDouble (10,DVI  ,'(F10.3)')
      CALL IFORMPUTINTEGER(12,NIS)
      CALL IFORMPUTINTEGER(14,NIE)
      CALL IFormPutDouble (16,XSC  ,'(F10.3)')
      CALL IFormPutDouble (18,YSC  ,'(F10.3)')
      CALL IFORMPUTINTEGER(20,NDEC)
      CALL IFormPutDouble (22,SCALESIZE ,'(F10.3)')
      ! 2nd isocolour legend:
      CALL IFORMPUTINTEGER(26,JAAUTO2)
      CALL IFORMPUTINTEGER(28,NV2)
      CALL IFormPutDouble (30,VMIN2 ,'(F10.3)')
      CALL IFormPutDouble (32,VMAX2 ,'(F10.3)')
      DVI2 = DV2/(NV2-1)
      CALL IFormPutDouble (34,DVI2  ,'(F10.3)')
      CALL IFORMPUTINTEGER(36,NIS2)
      CALL IFORMPUTINTEGER(38,NIE2)


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
              CALL IFORMGETINTEGER( 2,JAAUTO)
              JAAUTO = MAX(0,MIN(JAAUTO,2) )
              CALL IFORMGETINTEGER( 4,NVN)
              CALL IFormGetDouble ( 6,VMINN)
              CALL IFormGetDouble ( 8,VMAXN)
              CALL IFormGetDouble (10,DVNU)
              CALL IFORMGETINTEGER(12,NISN)
              CALL IFORMGETINTEGER(14,NIEN)
              IF (NV .NE. NVN .OR. NIS .NE. NISN .OR. NIE .NE. NIEN)THEN
                 NV  = MAX(2,NVN)
                 NIS = MAX(1,MIN(NISN,250))
                 NIE = MAX(NIS+NV+1,MIN(NIEN,254))
              ENDIF

              CALL IFormGetDouble (16,XSC)
              CALL IFormGetDouble (18,YSC)
              XSC = MAX(0d0,MIN(XSC,1d0))
              YSC = MAX(0d0,MIN(YSC,1d0))
              CALL IFORMGETINTEGER(20,NDEC)
              IF (NDEC .GT. 7) NDEC = 7
              CALL IFormGetDouble(22,SCALESIZE)
              SCALESIZE = MAX(0d0,MIN(SCALESIZE,1d0))

              IF (DVNU .NE. DVI .OR. VMAXN .NE. VMAX .OR. VMINN .NE. VMIN  ) JAAUTO = 0

              IF (JAAUTO .EQ. 0) THEN
                 DV = (NV-1)*DVNU
                 IF (VMIN .NE. VMINN .AND. VMAX .NE. VMAXN) THEN
                    VMIN = VMINN
                    VMAX = VMAXN
                    DV   = VMAX - VMIN
                 ELSE IF (VMAX .NE. VMAXN) THEN
                    VMAX = VMAXN
                    VMIN = VMAX - DV
                 ELSE
                    VMIN = VMINN
                    VMAX = VMIN + DV
                 ENDIF
                 DO I = 1,NV
                    VAL(I) = VMIN + (I-1)*DV/(NV-1)
                 ENDDO
              ENDIF

              ! Secondary isocolour legend
              CALL IFORMGETINTEGER(26,JAAUTO2)
              JAAUTO2 = MAX(0,MIN(JAAUTO2,1) )
              CALL IFORMGETINTEGER(28,NVN)
              CALL IFormGetDouble (30,VMINN)
              CALL IFormGetDouble (32,VMAXN)
              CALL IFormGetDouble (34,DVNU)
              CALL IFORMGETINTEGER(36,NISN)
              CALL IFORMGETINTEGER(38,NIEN)
              IF (NV2 .NE. NVN .OR. NIS2 .NE. NISN .OR. NIE2 .NE. NIEN)THEN
                 NV2  = MAX(2,NVN)
                 NIS2 = MAX(1,MIN(NISN,250))
                 NIE2 = MAX(NIS2+NV2+1,MIN(NIEN,254))
              ENDIF

              IF (DVNU .NE. DVI2 .OR. VMAXN .NE. VMAX2 .OR. &
                  VMINN .NE. VMIN2  ) JAAUTO2 = 0

              IF (JAAUTO2 .EQ. 0) THEN
                 DV2 = (NV2-1)*DVNU
                 IF (VMIN2 .NE. VMINN .AND. VMAX2 .NE. VMAXN) THEN
                    VMIN2 = VMINN
                    VMAX2 = VMAXN
                    DV2   = VMAX2 - VMIN2
                 ELSE IF (VMAX2 .NE. VMAXN) THEN
                    VMAX2 = VMAXN
                    VMIN2 = VMAX2 - DV2
                 ELSE
                    VMIN2 = VMINN
                    VMAX2 = VMIN2 + DV2
                 ENDIF
                 DO I = 1,NV2
                    VAL2(I) = VMIN2 + (I-1)*DV2/(NV2-1)
                 ENDDO
              ENDIF
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

      END
