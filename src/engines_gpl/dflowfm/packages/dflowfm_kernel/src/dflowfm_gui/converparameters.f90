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

!----------------------------------------------------------------------
! subroutines from rgfstuff.f90
!----------------------------------------------------------------------
      SUBROUTINE CONVERPARAMETERS(JA)
      USE M_MAPPROPARAMETERS
      use unstruc_display
      use m_sferic
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
      integer :: ja
      integer :: key
      integer :: l
      integer :: nbut
      integer :: nlevel
      integer :: numfld
      integer :: numpar
      PARAMETER  (NUMPAR = 10, NUMFLD = 2*NUMPAR)
      INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
      CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60, TEX*132
      COMMON /HELPNOW/ WRDKEY,NLEVEL
      integer, external :: infoinput
      external :: highlight_form_line

!
      JA        = 0
      NLEVEL    = 3
      OPTION(1) = 'Type of Map Projection (0,1,2,3,4,-1)   '
      OPTION(2) = 'UTM Zone Nr (1-60)                      '
      OPTION(3) = 'Northern (1) or southern (0) hemisphere '
      OPTION(4) = 'Offset X-Direction                      '
      OPTION(5) = 'Offset Y-Direction                      '
      OPTION(6) = 'Rotation Left (deg)                     '
      OPTION(7) = 'X Scalefactor                           '
      OPTION(8) = 'Y Scalefactor                           '
      OPTION(9) = 'X centrepoint (deg) for stereographic   '
      OPTION(10)= 'Y centrepoint (deg) for stereographic   '


!      123456789012345678901234567890123456789012345678901234567890
!               1         2         3         4         5         6
      HELPM (1) = '0=Trans/Rot,1=UTM,2=Amer,3=RD(Parijs),4=MERC,-1=AFFINE.XYX  '
      HELPM (2) = 'Usually 0, Except When Type = 1 (UTM) and Cartesian         '
      HELPM (3) = 'Only used for UTM->latlon conversion                        '
      HELPM (4) = 'X = X + Offset X-Direction, Real Value (m) (Only for Type=0)'
      HELPM (5) = 'Y = Y + Offset Y-Direction, Real Value (m) (Only for Type=0)'
      HELPM (6) = 'Rotationcenter = Center of Grid            (Only for Type=0)'
      HELPM (7) = 'Dimensionsless ()                          (Only for Type=0)'
      HELPM (8) = 'Dimensionsless ()                          (Only for Type=0)'
      HELPM (9) = 'Degrees                                    (Only for Type=5)'
      HELPM (10)= 'Degrees                                    (Only for Type=5)'


      CALL SAVEKEYS()

      IR = 0
      DO 10 I = 1,NUMPAR
         IL = IR + 1
         IR = IL + 1
         IX(IL) = 13
!         IX(IR) = 53
         IX(IR) = 95
         IY(IL) = 2*I
         IY(IR) = 2*I
         IS(IL) = 82
         IS(IR) = 10
         IS(IR) = 10
         IT(IL) = 1001
         IF (I .LE. 3) THEN
            IT(IR) = 2
         ELSE
            IT(IR) = 6
         ENDIF
   10 CONTINUE

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

      IF (JSFERIC .EQ. 1) THEN
         TEX = 'Conversion from Spherical (deg) to Cartesian (m) Coordinates'
      ELSE
         TEX = 'Conversion from Cartesian (m) to Spherical (deg) Coordinates'
      ENDIF
      L = len_trim(TEX)
      CALL IWinOutCentre(1,TEX(1:L))

      CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
!
!     Explain keyfunctions in bottom window
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IHS-1,IW,2)
      CALL IWinOutStringXY(1,1,'move = ., Tab, confirm = Enter, no change = Esc, help = F3')
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

      CALL IFORMPUTINTEGER( 1*2 ,ITYPE)
      CALL IFORMPUTINTEGER( 2*2 ,IZONE)
      CALL IFORMPUTINTEGER( 3*2 ,IHEM)
      CALL IFormPutDouble ( 4*2,DELTX,'(F10.3)')
      CALL IFormPutDouble ( 5*2,DELTY,'(F10.3)')
      CALL IFormPutDouble ( 6*2,FI,'(F10.3)')
      CALL IFormPutDouble ( 7*2,XF,'(F10.3)')
      CALL IFormPutDouble ( 8*2,YF,'(F10.3)')
      CALL IFormPutDouble ( 9*2,xcstereo,'(F10.3)')
      CALL IFormPutDouble (10*2,ycstereo,'(F10.3)')


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
              CALL IFORMGETINTEGER( 1*2,ITYPE)
              CALL IFORMGETINTEGER( 2*2,IZONE)
              CALL IFORMGETINTEGER( 3*2,IHEM)
              CALL IFormGetDouble ( 4*2,DELTX)
              CALL IFormGetDouble ( 5*2,DELTY)
              CALL IFormGetDouble ( 6*2,FI)
              CALL IFormGetDouble ( 7*2,XF)
              CALL IFormGetDouble ( 8*2,YF)
              CALL IFormGetDouble ( 9*2,Xcstereo)
              CALL IFormGetDouble (10*2,ycstereo)
              CSE = COS(DG2RD*FI)
              SNE = SIN(DG2RD*FI)
              JA  = 1
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

      END SUBROUTINE CONVERPARAMETERS
