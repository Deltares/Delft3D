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

 SUBROUTINE CHANGEcolournumbers()
   use m_netw
   USE M_FLOW
   use m_flowgeom
   USE m_sferic
   use m_wind
   use unstruc_display
   use m_reduce
   use dflowfm_version_module, only : company, product_name
   use unstruc_messages
   use m_fixedweirs
   use m_observations
   implicit none

   integer :: numpar, numfld, numparactual, numfldactual
   PARAMETER  (NUMPAR = 34 , NUMFLD = 2*NUMPAR)
   INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
   CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
   integer :: nlevel
   COMMON /HELPNOW/ WRDKEY,NLEVEL
   integer, external :: infoinput
   external :: highlight_form_line
!
   integer :: ir, il, iw, ixp, iyp, ih, i, iuvfieldorg, ifexit, ifinit, key, ja, niadvec
   integer :: nbut, imp, inp
   INTEGER :: KR , KG  , KB, KL

   KR = 0 ; KG = 0 ; KB = 0 ; KL = -1

   NLEVEL     = 4
   OPTION( 1) = 'NCOLDG=31          DESIGN GRID          ' ; it(2* 1) = 2
   OPTION( 2) = 'NCOLRG=212         PREVIOUS STATE GRID  ' ; it(2* 2) = 2
   OPTION( 3) = 'NCOLDN=3           DESIGN NET           ' ; it(2* 3) = 2
   OPTION( 4) = 'NCOLRN=211         PREVIOUS STATE NET   ' ; it(2* 4) = 2
   OPTION( 5) = 'NCOLNN=205         NETNODES             ' ; it(2* 5) = 2
   OPTION( 6) = 'NCOLSP=204         SPLINES              ' ; it(2* 6) = 2
   OPTION( 7) = 'NCOLLN=120         LAND BOUNDARY        ' ; it(2* 7) = 2
   OPTION( 8) = 'NCOLTX=210         TEXTLINES            ' ; it(2* 8) = 2
   OPTION( 9) = 'NCOLPL=221         POLYGON              ' ; it(2* 9) = 2
   OPTION(10) = 'NCOLCRS=230        CROSS SECTIONS       ' ; it(2*10) = 2
   OPTION(11) = 'NCOLTHD=231        THIN DAMS            ' ; it(2*11) = 2
   OPTION(12) = 'NCOLFXW=232        FIXED WEIRS          ' ; it(2*12) = 2
   OPTION(13) = 'NCOLHL=31          HIGHLIGHT NODES/LINKS' ; it(2*13) = 2
   OPTION(14) = 'KLVEC=4            VECTORS 110          ' ; it(2*14) = 2
   OPTION(15) = 'KLPROF=222         PROFILES             ' ; it(2*15) = 2
   OPTION(16) = 'KLSCL=221          ISOSCALE LEGEND      ' ; it(2*16) = 2
   OPTION(17) = 'KLTEX=3            NUMBERS              ' ; it(2*17) = 2
   OPTION(18) = 'KLOBS=221          OBSERVATION POINTS   ' ; it(2*18) = 2
   OPTION(19) = '                                        ' ; it(2*19) = 2
   OPTION(20) = 'Change RGB of colour Nr                 ' ; it(2*20) = 2
   OPTION(21) = 'R                                       ' ; it(2*21) = 2
   OPTION(22) = 'G                                       ' ; it(2*22) = 2
   OPTION(23) = 'B                                       ' ; it(2*23) = 2
   OPTION(24) = '                                        ' ; it(2*24) = 2
   OPTION(25) = 'R SCREEN                                ' ; it(2*25) = 2
   OPTION(26) = 'G SCREEN                                ' ; it(2*26) = 2
   OPTION(27) = 'B SCREEN                                ' ; it(2*27) = 2
   OPTION(28) = '                                        ' ; it(2*28) = 2
   OPTION(29) = 'R PLOT                                  ' ; it(2*29) = 2
   OPTION(30) = 'G PLOT                                  ' ; it(2*30) = 2
   OPTION(31) = 'B PLOT                                  ' ; it(2*31) = 2
   OPTION(32) = '                                        ' ; it(2*32) = 2
   OPTION(33) = 'JaFahrenheit                            ' ; it(2*33) = 2
   OPTION(34) = 'KLSRC=233         SORSIN                ' ; it(2*34) = 2


!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6

   HELPM ( 1) = '0<= ncol <=255                                              '
   HELPM ( 2) = '0<= ncol <=255                                              '
   HELPM ( 3) = '0<= ncol <=255                                              '
   HELPM ( 4) = '0<= ncol <=255                                              '
   HELPM ( 5) = '0<= ncol <=255                                              '
   HELPM ( 6) = '0<= ncol <=255                                              '
   HELPM ( 7) = '0<= ncol <=255                                              '
   HELPM ( 8) = '0<= ncol <=255                                              '
   HELPM ( 9) = '0<= ncol <=255                                              '
   HELPM (10) = '0<= ncol <=255                                              '
   HELPM (11) = '0<= ncol <=255                                              '
   HELPM (12) = '0<= ncol <=255                                              '
   HELPM (13) = '0<= ncol <=255                                              '
   HELPM (14) = '0<= ncol <=255                                              '
   HELPM (15) = '0<= ncol <=255                                              '
   HELPM (16) = '0<= ncol <=255                                              '
   HELPM (17) = '0<= ncol <=255                                              '
   HELPM (18) =    '0<= ncol <=255                                              '
   HELPM (1:31) =  '0<= ncol <=255                                              '
   HELPM (32:33) = '0/1                                                         '
   HELPM (34) = '0<= ncol <=255                                              '

   CALL SAVEKEYS()
   NUMPARACTUAL = NUMPAR
   NUMFLDACTUAL = 2*NUMPARACTUAL

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      IX(IL) = 13
      IX(IR) = 95
      IY(IL) = I
      IY(IR) = I
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
   CALL IWinOutCentre(1,trim(company)//'-'//trim(product_name) // ' PARAMETER FORM')
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

   CALL IFormPutINTEGER (2* 1 ,NCOLDG     )
   CALL IFORMPUTINTEGER (2* 2 ,NCOLRG     )
   CALL IFORMPUTINTEGER (2* 3 ,NCOLDN     )
   CALL IFORMPUTINTEGER (2* 4 ,NCOLRN     )
   CALL IFORMPUTINTEGER (2* 5 ,NCOLNN     )
   CALL IFORMPUTINTEGER (2* 6 ,NCOLSP     )
   CALL IFORMPUTINTEGER (2* 7 ,NCOLLN     )
   CALL IFORMPUTINTEGER (2* 8 ,NCOLTX     )
   CALL IFORMPUTINTEGER (2* 9 ,NCOLPL     )
   CALL IFORMPUTINTEGER (2*10 ,NCOLCRS    )
   CALL IFORMPUTINTEGER (2*11 ,NCOLTHD    )
   CALL IFORMPUTINTEGER (2*12 ,NCOLFXW    )
   CALL IFORMPUTINTEGER (2*13 ,NCOLHL     )
   CALL IFORMputINTEGER (2*14 ,KLVEC      )
   CALL IFORMputINTEGER (2*15 ,KLPROF     )
   CALL IFORMPUTINTEGER (2*16 ,KLSCL      )
   CALL IFORMputINTEGER (2*17 ,KLTEX      )
   CALL IFORMputINTEGER (2*18 ,KLOBS      )

   CALL IFORMputINTEGER (2*20 ,KL         )
   CALL IFORMputINTEGER (2*21 ,KR         )
   CALL IFORMputINTEGER (2*22 ,KG         )
   CALL IFORMputINTEGER (2*23 ,KB         )

   CALL IFORMPUTINTEGER (2*25 ,NREDS      )
   CALL IFORMPUTINTEGER (2*26 ,NGREENS    )
   CALL IFORMPUTINTEGER (2*27 ,NBLUES     )

   CALL IFORMPUTINTEGER (2*29 ,NREDP      )
   CALL IFORMPUTINTEGER (2*30 ,NGREENP    )
   CALL IFORMPUTINTEGER (2*31 ,NBLUEP     )

   CALL IFORMPUTINTEGER (2*33 ,Jafahrenheit)
   CALL IFORMPUTINTEGER (2*34 ,KLSRC)

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

          CALL IFORMGETINTEGER (2* 1 ,NCOLDG     )
          CALL IFORMGETINTEGER (2* 2 ,NCOLRG     )
          CALL IFORMGETINTEGER (2* 3 ,NCOLDN     )
          CALL IFORMGETINTEGER (2* 4 ,NCOLRN     )
          CALL IFORMGETINTEGER (2* 5 ,NCOLNN     )
          CALL IFORMGETINTEGER (2* 6 ,NCOLSP     )
          CALL IFORMGETINTEGER (2* 7 ,NCOLLN     )
          CALL IFORMGETINTEGER (2* 8 ,NCOLTX     )
          CALL IFORMGETINTEGER (2* 9 ,NCOLPL     )
          CALL IFORMGETINTEGER (2*10 ,NCOLCRS    )
          CALL IFORMGETINTEGER (2*11 ,NCOLTHD    )
          CALL IFORMGETINTEGER (2*12 ,NCOLFXW    )
          CALL IFORMGETINTEGER (2*13 ,NCOLHL     )
          CALL IFORMGEtINTEGER (2*14 ,KLVEC      )
          CALL IFORMGEtINTEGER (2*15 ,KLPROF     )
          CALL IFORMGETINTEGER (2*16 ,KLSCL      )
          CALL IFORMGEtINTEGER (2*17 ,KLTEX      )
          CALL IFORMGEtINTEGER (2*18 ,KLOBS      )

          CALL IFORMGETINTEGER (2*20 ,KL         )  ; KL   = MIN(255,       KL  )
          CALL IFORMGETINTEGER (2*21 ,KR         )  ; KR   = MIN(255, MAX(0,KR  ))
          CALL IFORMGETINTEGER (2*22 ,KG         )  ; KG   = MIN(255, MAX(0,KG  ))
          CALL IFORMGETINTEGER (2*23 ,KB         )  ; KB   = MIN(255, MAX(0,KB  ))

          CALL IFORMGETINTEGER (2*25 ,NREDS      )  ; NREDS   = MIN(255, MAX(0,NREDS  ))
          CALL IFORMGETINTEGER (2*26 ,NGREENS    )  ; NGREENS = MIN(255, MAX(0,NGREENS))
          CALL IFORMGETINTEGER (2*27 ,NBLUES     )  ; NBLUES  = MIN(255, MAX(0,NBLUES ))

          CALL IFORMGETINTEGER (2*29 ,NREDP      )  ; NREDP   = MIN(255, MAX(0,NREDP  ))
          CALL IFORMGETINTEGER (2*30 ,NGREENP    )  ; NGREENP = MIN(255, MAX(0,NGREENP))
          CALL IFORMGETINTEGER (2*31 ,NBLUEP     )  ; NBLUEP  = MIN(255, MAX(0,NBLUEP ))

          CALL IFORMGEtINTEGER (2*33 ,JaFahrenheit  )
          CALL IFORMGEtINTEGER (2*34 ,KLSRC         )

          IF (KL > 0) THEN
              CALL IGRPALETTERGB(KL,KR,KG,KB)
          ENDIF

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

   END SUBROUTINE CHANGEcolournumbers
