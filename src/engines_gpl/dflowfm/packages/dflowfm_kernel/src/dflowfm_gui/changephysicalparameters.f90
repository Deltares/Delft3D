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

   SUBROUTINE CHANGEPHYSICALPARAMETERS()
   use m_netw
   USE M_FLOW
   use m_flowgeom
   USE M_FLOWTIMES
   USE m_sferic
   use m_wind
   use unstruc_display
   use dflowfm_version_module, only : company, product_name
   implicit none

   integer :: numpar, numfld, numparactual, numfldactual
   PARAMETER  (NUMPAR = 15, NUMFLD = 2*NUMPAR)
   INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
   CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
   integer :: nlevel
   COMMON /HELPNOW/ WRDKEY,NLEVEL
   integer, external :: infoinput
   external :: highlight_form_line
!
   integer :: ir, il, iw, ixp, iyp, ih, i, iuvfieldorg, ifexit, ifinit, key, ierr
   integer :: nbut, imp, inp
   double precision :: frcuniorg

   NLEVEL     = 4
   OPTION( 1) = 'frcuni                                  ' ; it(2* 1) = 6
   OPTION( 2) = 'ifrctypuni Friction formulation         ' ; it(2* 2) = 2
   OPTION( 3) = 'Windspeed     (m/s)                     ' ; it(2* 3) = 6
   OPTION( 4) = 'Winddirection ( ) 90= to East 0=to North' ; it(2* 4) = 6
   OPTION( 5) = 'vicouv                           (m2/s) ' ; it(2* 5) = 6
   OPTION( 6) = 'Vicoww                           (m2/s) ' ; it(2* 6) = 6
   OPTION( 7) = 'Dicouv                           ( )    ' ; it(2* 7) = 6
   OPTION( 8) = 'Dicoww                           ( )    ' ; it(2* 8) = 6
   OPTION( 9) = 'Verticall Wall Nikuradse         (m)    ' ; it(2* 9) = 6
   OPTION(10) = 'Smagorinsky                      ( )    ' ; it(2*10) = 6
   OPTION(11) = 'Elder                            ( )    ' ; it(2*11) = 6
   OPTION(12) = 'uniform friction coefficient 1D         ' ; it(2*12) = 6
   OPTION(13) = 'uniform friction coefficient 1D2D intern' ; it(2*13) = 6
   OPTION(14) = 'uniform friction coefficient 1D groundly' ; it(2*14) = 6
   OPTION(15) = 'uniform rainfall              (mm/hr)   ' ; it(2*15) = 6


!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6

   HELPM ( 1) = 'uniform friction coefficient                                '
   HELPM ( 2) = ' 0=Chz, 1=Mann, 2=White-Col, 3=White-Col-Waqua, 10=Glass    '
   HELPM ( 3) = '                                                            '
   HELPM ( 4) = '                                                            '
   HELPM ( 5) = 'background horizontal viscosity                             '
   HELPM ( 6) = 'background vertical   viscosity (0: no vert. visc. at all)  '
   HELPM ( 7) = 'background horizontal diffusivity                           '
   HELPM ( 8) = 'background vertical   diffusivity (0: no vert. diff. at all)'
   HELPM ( 9) = 'VERTICAL WALL NIKURADSE ROUGHNESS, (wall_z0 = KS/30)     (M)'
   HELPM (10) = 'vicuv = vicuv + ( (Smagorinsky*dx)**2)*Strainrate_S, eg 0.1 '
   HELPM (11) = 'vicuv = vicuv +    Elder*0.009*H*U                   eg 1.0 '
   HELPM (12) = 'uniform friction coefficient 1D                             '
   HELPM (13) = 'uniform friction coefficient 1D2D internal Link             '
   HELPM (14) = 'uniform friction coefficient 1D groundlayer                 '
   HELPM (15) = '(if non-zero overrides ext forcings)                        '

   CALL SAVEKEYS()

   NUMPARACTUAL = NUMPAR
   NUMFLDACTUAL = 2*NUMPARACTUAL

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      IX(IL) = 13
      IX(IR) = 95
      IY(IL) = 2*I
      IY(IR) = 2*I
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
   CALL IWinOutCentre(1,trim(company)//'-'//trim(product_name)// ' PARAMETER FORM')
   CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
!
!  Explain keyfunctions in bottom window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IHS-1,IW,2)
   CALL IWinOutStringXY(1,1,'move = ., Tab, confirm = Enter, no change = Esc, help = F3')
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

   frcuniorg = frcuni
   CALL IFormPutDouble  (2* 1 , frcuni, '(F8.3)')
   CALL IFORMPUTinteger (2* 2 , ifrctypuni      )
   CALL IFormPutDouble  (2* 3 , windsp ,'(F8.3)')
   CALL IFormPutDouble  (2* 4 , winddir,'(F8.3)')
   CALL IFormPutDouble  (2* 5 , vicouv ,'(e9.2)')
   CALL IFormPutDouble  (2* 6 , vicoww ,'(e8.3)')
   CALL IFORMPUTdouble  (2* 7 , dicouv, '(e8.3)')
   CALL IFORMPUTdouble  (2* 8 , dicoww, '(e8.3)')
   CALL IFormPutDouble  (2* 9 , wall_ks,'(F8.3)')
   CALL IFormPutDouble  (2*10 , Smagorinsky,'(F8.3)')
   CALL IFormPutDouble  (2*11 , Elder,      '(F8.3)')
   CALL IFormPutDouble  (2*12 , frcuni1D,   '(F8.3)')
   CALL IFormPutDouble  (2*13 , frcuni1D2D, '(F8.3)')
   CALL IFormPutDouble  (2*14 , frcuni1Dgrounlay, '(F8.3)')
   CALL IFormPutDouble  (2*15 , rainuni   , '(F8.3)')

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

           CALL IFormGetDouble  (2* 1 , frcuni )
           CALL IFORMGETinteger (2* 2 , ifrctypuni)
           CALL IFormGetDouble  (2* 3 , windsp )
           CALL IFormGetDouble  (2* 4 , winddir)
           CALL IFormGetDouble  (2* 5 , vicouv )
           CALL IFormGetDouble  (2* 6 , vicoww )
           CALL IFORMGetdouble  (2* 7 , dicouv        )
           CALL IFORMGetdouble  (2* 8 , dicoww        )
           CALL IFormGetDouble  (2* 9 , wall_ks)
           CALL IFormGetDouble  (2*10 , Smagorinsky)
           CALL IFormGetDouble  (2*11 , Elder)
           CALL IFormGetDouble  (2*12 , frcuni1D)
           CALL IFormGetDouble  (2*13 , frcuni1D2D)
           CALL IFormGetDouble  (2*14 , frcuni1Dgrounlay)
           CALL IFormGetDouble  (2*15 , rainuni)

           if (allocated (frcu) .and. frcuniorg .ne. frcuni) then
               frcu = frcuni
           endif

           if (rainuni > 0d0) then
              if (.not. allocated(rain) ) then
                 allocate ( rain(ndx) , stat=ierr) ; rain = 0d0
                 call aerr('rain(ndx)', ierr, ndx)
              endif
              jarain = 1 ; jaqin = 1
           endif

           wall_z0 = wall_ks / 30d0
           if (windsp .ne. 0d0) then
              call setuniformwind()
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

   END SUBROUTINE CHANGEPHYSICALPARAMETERS
