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

      SUBROUTINE CHANGEGRIDPARAMETERS()
      USE M_GRID
      USE M_GRIDSETTINGS
      use m_sferic
      use unstruc_display
      use m_polygon
      use dflowfm_version_module, only : company, product_name
      implicit none

      integer :: numpar, numfld, numparactual, numfldactual
      PARAMETER  (NUMPAR = 15, NUMFLD = 2*NUMPAR)
      INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
      integer :: nlevel
      CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
      COMMON /HELPNOW/ WRDKEY,NLEVEL
      integer, external :: infoinput
      external :: highlight_form_line
!
      integer :: ip,ir, il, iw, ixp, iyp, ih, i, iuvfieldorg, ifexit, ifinit, key
      integer :: nbut, imp, inp, k
      double precision :: phi

      NLEVEL    = 3
      OPTION(1) = 'M-REFINEMENT FACTOR                     '
      OPTION(2) = 'N-REFINEMENT FACTOR                     '
      OPTION(3) = 'NR SMOOTHING ITERATIONS                 '
      OPTION(4) = 'SMOOTHING PARAMETER                     '
      OPTION(5) = 'ATTRACTION/REPULSION PARAMETER          '
      OPTION(6) = 'PASSIVE GRID OR GRID FIXED IN PASTE     '
      OPTION(7) = 'GO BACK TO STARTUP DIRECTORY YES/NO     '
      OPTION(8) = 'LINE OR SPLINE REPRESENTATION  (0.0-1.0)'
      OPTION(9) = 'EQUIDISTANT OR SMOOTH INTERPOL (0.0-1.0)'
      OPTION(10)= 'INCREASE FACTOR IN LINE MIRROR  (0.1-10)'
      OPTION(11)= 'Spherical or Cartesian coordinates (1 0)'
      OPTION(12)= 'DRAW STEREOGRAPHIC OR NO PROJECTION(1 0)'
!     pillar grid
      option(13)= 'PILLAR RADIUS (m)                       '
      option(14)= 'PILLAR X-COORDINATE                     '
      option(15)= 'PILLAR Y-COORDINATE                     '
!
!      123456789012345678901234567890123456789012345678901234567890
!               1         2         3         4         5         6
      HELPM (1) = 'INTEGER VALUE <                                             '
      HELPM (2) = 'INTEGER VALUE <                                             '
      HELPM (3) = 'SMOOTHING, EDIT  : (0.0 - 100)  DEFAULT = 20,  INTERMEDIATE '
      HELPM (4) = 'SMOOTHING  EDIT  : (0.0 - 1.0)  DEFAULT = 0.2, INTERMEDIATE '
      HELPM (5) = 'ATTRACT./REPULS. : (0.0 - 0.5)  DEFAULT = 0.1, INTERMEDIATE '
      HELPM (6) = 'GRID PASTE       : (0.0 - 1.0)  0.0: GRID FIXED, 1.0:PASSIVE'
      HELPM (7) = 'ALWAYS BACK TO STARTUP DIRECTORY (1) OR KEEP NEW DIR. (0)   '
      HELPM (8) = 'STRAIGHT LINES REPRESENTATION = 0, CURVED LINES = 1         '
      HELPM (9) = 'SPLINES TO GRID  : (0.0 - 1.0) DEFAULT = 1.0, SMOOTH INTERP.'
      HELPM (10)= 'GRID SIZE INCREASE IN LINE MIRROR, 1.0 = EQUAL SIZE         '
      HELPM (11)= '1 = Spherical, 0 = Cartesian                                '
      HELPM (12)= '1 = STEREOGRAPHIC PROJECTION , 0 = NO PROJECTION            '
      HELPM (13)= 'SET RADIUS TO 0 FOR NO PILLAR                               '
      HELPM (14)= '                                                            '
      HELPM (14)= '                                                            '


      CALL SAVEKEYS()
      IP = 20
      WRITE(HELPM(1)(IP:IP+4),'(I5)') MIN(MMAX-1, 1 + (MMAX-1)/MAX(1,(MC-1)) )
      WRITE(HELPM(2)(IP:IP+4),'(I5)') MIN(NMAX-1, 1 + (NMAX-1)/MAX(1,(NC-1)) )

      IF (JDEMO .EQ. 1) THEN
         NUMPARACTUAL = 6
      ELSE
         NUMPARACTUAL = NUMPAR
      ENDIF
      NUMFLDACTUAL = 2*NUMPARACTUAL

      IR = 0
      DO 10 I = 1,NUMPARACTUAL
         IL = IR + 1
         IR = IL + 1
         IX(IL) = 13
         IX(IR) = 95
         IY(IL) = 2*I
         IY(IR) = 2*I
         IS(IL) = 82
         IS(IR) = 10
         IT(IL) = 1001
         IF (I .LE. 3 .OR. I == 7 .OR. I == 10 .OR. I == 11 .OR. I==12) THEN
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
      CALL IWinOutCentre(1,trim(company)//'-'//trim(product_name)// ' PARAMETER FORM')
      CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
!
!     Explain keyfunctions in bottom window
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IHS-1,IW,2)
      CALL IWinOutStringXY (1,1,'move = ., Tab, confirm = Enter, no change = Esc, help = F3')
      CALL IWinOutStringXY (1,2,'right mouse = Enter, click outside window = Esc')

!     Filewindow is middelste window
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IYP+3,IW,IH)

      CALL InControlKey(29,129)
      CALL InControlKey(30,128)

!     NUMWNH = InfoWindow(1)
!     CALL IWinSelect(NUMWNH)

!     Define a new form by supplying arrays containing
!     field positions, sizes and types
      CALL IFORMDEFINE('W',NUMFLDACTUAL,IX,IY,IS,IT)

!     Define a help field and define help strings
!     for 2 of the 4 input fields
      CALL IFORMHELP(13,IH,60)

      IR = 0
      DO 20 I = 1,NUMPARACTUAL
         IL = IR + 1
         IR = IL + 1
         CALL IFORMPUTSTRING (IL,OPTION(I))
         CALL IFORMPUTHELP   (IR,HELPM(I))
         CALL IFORMATTRIBUTEN(IR,0,0,7)
    20 CONTINUE

      CALL IFORMPUTINTEGER(2*1,MFAC)
      CALL IFORMPUTINTEGER(2*2,NFAC)
      CALL IFORMPUTINTEGER(2*3,ITSMO)
      CALL IFormPutDouble (2*4,CSMO,'(F5.3)')
      CALL IFormPutDouble (2*5,RFAC,'(F5.3)')
      CALL IFormPutDouble (2*6,BAAS2,'(F5.3)')
      CALL IFORMPUTINTEGER(2*7,KEEPSTARTDIR)
      CALL IFormPutDouble (2*8,SPLFAC,'(F5.3)')
      CALL IFormPutDouble (2*9,SPLFAC2,'(F5.3)')
      CALL IFormPutDouble (2*10,FACMIR,'(F5.3)')
      CALL IFORMPUTINTEGER(2*11,jsferic)
      CALL IFORMPUTINTEGER(2*12,jsferTEK)
      CALL IFormPutDouble (2*13,pil_rad,'(F7.3)')
      CALL IFormPutDouble (2*14,pil_x,  '(F7.3)')
      CALL IFormPutDouble (2*15,pil_y,  '(F7.3)')


!   Diplay the form with numeric fields left justified
!   an set the initial field to number 2
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
              CALL IFORMGETINTEGER(2*1,MFAC)
              CALL IFORMGETINTEGER(2*2,NFAC)
              CALL IFORMGETINTEGER(2*3,ITSMO)
              CALL IFormGetDouble (2*4,CSMO)
              CALL IFormGetDouble (2*5,RFAC)
              CALL IFormGetDouble (2*6,BAAS2)
              CALL IFORMGETINTEGER(2*7,KEEPSTARTDIR)
              CALL IFormGetDouble (2*8,SPLFAC)
              CALL IFormGetDouble (2*9,SPLFAC2)
              CALL IFormGetDouble (2*10,FACMIR)
              CALL IFORMGETINTEGER(2*11,jsferic)
              CALL IFORMGETINTEGER(2*12,jsferTEK)
              CALL IFormGetDouble (2*13,pil_rad)
              CALL IFormGetDouble (2*14,pil_x)
              CALL IFormGetDouble (2*15,pil_y)

              KEEPSTARTDIR = MAX(0,KEEPSTARTDIR)
              KEEPSTARTDIR = MIN(1,KEEPSTARTDIR)
              !MFAC = MAX(1,MFAC)
              !NFAC = MAX(1,NFAC)
              CSMO = MAX(0d0,CSMO)
              RFAC = MAX(0d0,RFAC)
              BAAS2 = MAX(0d0, MIN(BAAS2,1d0) )
              SPLFAC= MAX(0d0, MIN(SPLFAC,1d0) )
              SPLFAC2=MAX(0d0, MIN(SPLFAC2,1d0) )
              FACMIR=MAX(0.1d0, MIN(FACMIR,10d0) )
              jsferic = max(0,min(jsferic,1))

              if ( pil_rad < 0d0 ) then ! cre
                 if (maxpol < mfac+1) then
                     call increasepol(mfac+1, 0)
                 endif
                 pil_rad = abs(pil_rad)
                 do k = 1,mfac+1
                    phi = twopi* ( dble(k-1) / dble(mfac) )
                    xpl(k) = pil_x + pil_rad*cos(phi)
                    ypl(k) = pil_y + pil_rad*sin(phi)
                 enddo
                 npl = mfac+1
             endif
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

      END SUBROUTINE CHANGEGRIDPARAMETERS
