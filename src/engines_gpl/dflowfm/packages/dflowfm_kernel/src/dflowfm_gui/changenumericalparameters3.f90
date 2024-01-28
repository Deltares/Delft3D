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

   SUBROUTINE CHANGENUMERICALPARAMETERS3()
   use m_netw
   USE M_FLOW
   use m_flowgeom
   USE m_sferic
   use m_wind
   use m_sediment
   use unstruc_display
   use m_reduce
   use dflowfm_version_module, only : company, product_name
   use unstruc_messages
   use m_fixedweirs
   use m_waves
   implicit none

   integer :: numpar, numfld, numparactual, numfldactual
   PARAMETER  (NUMPAR = 22, NUMFLD = 2*NUMPAR)
   INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
   CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
   integer :: nlevel
   COMMON /HELPNOW/ WRDKEY,NLEVEL
   integer, external :: infoinput
   external :: highlight_form_line
!
   integer :: ir, il, iw, ixp, iyp, ih, i, iuvfieldorg, ifexit, ifinit, key, ja, niadvec
   integer :: nbut, imp, inp

   NLEVEL     = 4
   OPTION( 1) = 'Clveg                                ( )' ; it(2* 1) = 6
   OPTION( 2) = 'Cdveg                                ( )' ; it(2* 2) = 6
   OPTION( 3) = 'Rhoveg                           (kg/m3)' ; it(2* 3) = 6
   OPTION( 4) = 'Cbveg                         (kg.m2/s2)' ; it(2* 4) = 6
   OPTION( 5) = 'Stemheightstd                        ( )' ; it(2* 5) = 6
   OPTION( 6) = 'Hwavuni                              (m)' ; it(2* 6) = 6
   OPTION( 7) = 'Twavuni                              (s)' ; it(2* 7) = 6
   OPTION( 8) = 'Phiwavuni                            ( )' ; it(2* 8) = 6
   OPTION( 9) = 'Wave model nr modind                 ( )' ; it(2* 9) = 2
   OPTION(10) = 'Slotw1D                              (m)' ; it(2*10) = 6
   OPTION(11) = 'Slotw2D                              (m)' ; it(2*11) = 6
   OPTION(12) = 'Epsmaxlev                            (m)' ; it(2*12) = 6
   OPTION(13) = 'Epsmaxlevm                           (m)' ; it(2*13) = 6
   OPTION(14) = 'jawavestreaming terms in D3Dwavemodel( )' ; it(2*14) = 2
   OPTION(15) = 'jawaveStokes 0,1,2,3                 ( )' ; it(2*15) = 2
   OPTION(16) = 'jawavelogprof                        ( )' ; it(2*16) = 2
   OPTION(17) = 'Maxitforestersal                     ( )' ; it(2*17) = 2
   OPTION(18) = 'Maxitforestertem                     ( )' ; it(2*18) = 2
   OPTION(19) = 'Noderivedtypes (Noderivedtypes in mdu)     ( )' ; it(2*19) = 2
   OPTION(20) = 'Maxdegree                            ( )' ; it(2*20) = 2
   OPTION(21) = 'Jaevap                               ( )' ; it(2*21) = 2
   OPTION(22) = 'Jaseddenscoupling                    ( )' ; it(2*22) = 2


!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6

   HELPM ( 1) = 'Distance coeff (0.8)                                    ( ) '
   HELPM ( 2) = 'Drag coefficient (0.8)                                  ( ) '
   HELPM ( 3) = 'Vegetation specific density, if > 0, include bending        '
   HELPM ( 4) = 'Bending stiffness coefficient                               '
   HELPM ( 5) = 'Standard deviation of stem height                           '
   HELPM ( 6) = '                                                            '
   HELPM ( 7) = '                                                            '
   HELPM ( 8) = '                                                            '
   HELPM ( 9) = 'wave model nr 1-9                                           '
   HELPM (10) = 'Slotwidth in 1D , default 1d-3                          (m) '
   HELPM (11) = 'Slotwidth in 2D , default 0d-3                          (m) '
   HELPM (12) = 'Max level diff in Newton iterations,      default 1d-8  (m) '
   HELPM (13) = 'Max level diff in outer loop of Nested Newton def 1d-8  (m) '
   HELPM (14) = '>=1 streaming, >= 2 streaming + turb                        '
   HELPM (15) = '0=no, 1 = uniform, 2 = non-uniform, 3=2+vertical visc Stokes'
   HELPM (16) = '0=depth-av, 1 = log profile                                 '
   HELPM (17) = 'Max nr of iterations                                        '
   HELPM (18) = 'Max nr of iterations                                        '
   HELPM (19) = '0=use der. types, 1 = less, 2 = lesser, 5 = also deallo der.'
   HELPM (20) = '6 = default, 666 = number of the devil                      '
   HELPM (21) = '1 = evaporation computed bij heatfluxmodel , 0= no evap     '
   HELPM (22) = '0=no, 1 = yes                                               '


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

   CALL IFORMputdouble  (2* 1 ,Clveg            , '(F7.3)' )
   CALL IFORMputdouble  (2* 2 ,Cdveg            , '(F7.3)' )
   CALL IFORMputdouble  (2* 3 ,Rhoveg           , '(F7.3)' )
   CALL IFORMputdouble  (2* 4 ,Cbveg            , '(F7.3)' )
   CALL IFORMputdouble  (2* 5 ,Stemheightstd    , '(F7.3)' )
   CALL IFORMputdouble  (2* 6 ,hwavuni          , '(F7.3)' )
   CALL IFORMputdouble  (2* 7 ,twavuni          , '(F7.3)' )
   CALL IFORMputdouble  (2* 8 ,phiwavuni        , '(F7.3)' )
   CALL IFORMputinteger (2* 9 ,modind           )
   CALL IFORMputdouble  (2*10 ,Slotw1D          , '(E8.2)' )
   CALL IFORMputdouble  (2*11 ,Slotw2D          , '(E8.2)' )
   CALL IFORMputdouble  (2*12 ,Epsmaxlev        , '(E8.2)' )
   CALL IFORMputdouble  (2*13 ,Epsmaxlevm       , '(E8.2)' )
   CALL IFORMputinteger (2*14 ,jawavestreaming             )
   CALL IFORMputinteger (2*15 ,jawaveStokes                )
   CALL IFORMputinteger (2*16 ,Maxitverticalforestersal    )
   CALL IFORMputinteger (2*17 ,Maxitverticalforestertem    )
   CALL IFORMputinteger (2*18 ,Noderivedtypes                    )
   CALL IFORMputinteger (2*19 ,maxdge                      )
   CALL IFORMputinteger (2*20 ,Jaevap                      )
   CALL IFORMputinteger (2*21 ,Jaseddenscoupling           )


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

          CALL IFORMGETdouble  (2* 1 ,Clveg            )
          CALL IFORMGETdouble  (2* 2 ,Cdveg            )
          CALL IFORMGETdouble  (2* 3 ,Rhoveg           )
          CALL IFORMGETdouble  (2* 4 ,Cbveg            )
          CALL IFORMGETdouble  (2* 5 ,stemheightstd    )
          CALL IFORMGETdouble  (2* 6 ,  hwavuni        )   ;  if (  hwavuni > 0d0)   hwav =   hwavuni
          CALL IFORMGETdouble  (2* 7 ,  twavuni        )   ;  if (  twavuni > 0d0)   twav =   twavuni
          CALL IFORMGETdouble  (2* 8 ,phiwavuni        )   ;  if (phiwavuni > 0d0) phiwav = phiwavuni
          CALL IFORMGETinteger (2* 9 ,modind           )
          CALL IFORMGETdouble  (2*10 ,Slotw1D          )
          CALL IFORMGETdouble  (2*11 ,Slotw2D          )
          CALL IFORMGETdouble  (2*12 ,Epsmaxlev        )
          CALL IFORMGETdouble  (2*13 ,Epsmaxlevm       )
          CALL IFORMGETinteger (2*14 ,jawavestreaming         )
          CALL IFORMGETinteger (2*15 ,jawaveStokes            )
          CALL IFORMGETinteger (2*16 ,Maxitverticalforestersal)
          CALL IFORMGETinteger (2*17 ,Maxitverticalforestertem)
          CALL IFORMGETinteger (2*18 ,Noderivedtypes                )
          CALL IFORMGETinteger (2*19 ,Maxdge                  )
          CALL IFORMGETinteger (2*20 ,Jaevap                  )
          CALL IFORMgetinteger (2*21 ,Jaseddenscoupling            )
          if (jaevap > 0) then
             if (.not. allocated (evap) ) then
                allocate (evap(ndx))
             endif
             jaqin = 1
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

   END SUBROUTINE CHANGENUMERICALPARAMETERS3
