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

   SUBROUTINE CHANGEgeometryPARAMETERS()
   use m_netw
   USE M_FLOW
   use m_flowgeom
   USE M_FLOWTIMES
   USE m_sferic
   use m_wind
   use unstruc_display
   use m_fixedweirs
   use dflowfm_version_module, only : company, product_name
   implicit none

   integer :: numpar, numfld, numparactual, numfldactual
   PARAMETER  (NUMPAR = 28, NUMFLD = 2*NUMPAR)
   INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
   CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
   integer :: nlevel
   COMMON /HELPNOW/ WRDKEY,NLEVEL
   integer, external :: infoinput
   external :: highlight_form_line
!
   integer :: ir, il, iw, ixp, iyp, ih, i, iuvfieldorg, ifexit, ifinit, key
   integer :: nbut, imp, inp

   NLEVEL    = 4

   OPTION( 1)= 'sini                                (m) ' ; it(2* 1) = 6
   OPTION( 2)= 'zkuni                               (m) ' ; it(2* 2) = 6
   OPTION( 3)= 'bedslope                            ( ) ' ; it(2* 3) = 6
   OPTION( 4)= 'anglat                            (deg) ' ; it(2* 4) = 6
   OPTION( 5)= 'ibedlevtyp                          ( ) ' ; it(2* 5) = 2
   OPTION( 6)= 'Kmx, nr of Vertical sigma layers    ( ) ' ; it(2* 6) = 2
   OPTION( 7)= 'Jazlayercenterbedvel                ( ) ' ; it(2* 7) = 2
   OPTION( 8)= 'Jasfer3D                            ( ) ' ; it(2* 8) = 2
   OPTION( 9)= 'Jalimnor                            ( ) ' ; it(2* 9) = 2
   OPTION(10)= 'minimum 1D link length,             (m) ' ; it(2*10) = 6
   OPTION(11)= 'Uniform 1D link width               (m) ' ; it(2*11) = 6
   OPTION(12)= '1D profile type                     ( ) ' ; it(2*12) = 2
   OPTION(13)= '2D conveyance                       ( ) ' ; it(2*13) = 2
   OPTION(14)= 'non linear continuity 2D            ( ) ' ; it(2*14) = 2
   OPTION(15)= 'non linear continuity 1D            ( ) ' ; it(2*15) = 2
   OPTION(16)= 'sdropstep  when dropping water      (m) ' ; it(2*16) = 6
   OPTION(17)= 'zkdropstep when dropping land       (m) ' ; it(2*17) = 6
   OPTION(18)= 'Ifixedweirscheme                    ( ) ' ; it(2*18) = 2
   OPTION(19)= 'Layertype                           ( ) ' ; it(2*19) = 2
   OPTION(20)= 'Sigmagrowthfactor                   ( ) ' ; it(2*20) = 6
   OPTION(21)= 'Sillheightmin                       (m) ' ; it(2*21) = 6
   OPTION(22)= 'Mxlayz nr of vertical z-layers      ( ) ' ; it(2*22) = 2
   OPTION(23)= 'ihuzcsig, L,R sig at u central part ( ) ' ; it(2*23) = 2
   OPTION(24)= 'Keepzlayering at bed                ( ) ' ; it(2*24) = 2
   OPTION(25)= 'Numtopsig (only for z-layers)       ( ) ' ; it(2*25) = 2
   OPTION(26)= 'Numtopsiguniform                    ( ) ' ; it(2*26) = 2
   OPTION(27)= 'ihuz, only for keepzlayeringatbed>=3( ) ' ; it(2*27) = 2
   OPTION(28)= 'jazlayeratubybob                    ( ) ' ; it(2*28) = 2
 

!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6

   HELPM (1) = 'initial waterlevel                                          '
   HELPM (2) = 'uniform bottom level                                        '
   HELPM (3) = 'bedslope                                                    '
   HELPM (4) = 'angle of latitude, Delft = 52.0                             '
   HELPM (5) = '1=cell tiledep bl, 2=u-point blu, 3=netw,mean-u 4=netw, maxu'
   HELPM (6) = '0=2D ORIGINAL CODE, 1=2D IN 3D CODE, >1= 3D CODE            '
   HELPM (7) = '1=orig, 2=sigma-like                                        '
  !HELPM (7) = '0=D3D, 0.5dx outside, 1=on net bnd, 2=on polylin (not yet)  '
   HELPM (8) = '0=org, 1=jasfer3D                                           '
   HELPM (9) = 'Jalimnor                                                    '
   HELPM (10)= 'dxmin1D (except for duikers)                                '
   HELPM (11)= 'wu1DUNI                                                     '
   HELPM (12)= '1=circle, 2=rectan, 3=rectan (peri=wid), 4=3,nonlin         '
   HELPM (13)= '0:R=H, 1:R=A/P, 2:K=analytic-1D conv, 3:K=analytic-2D conv  '
   HELPM (14)= 'only for ibedlevtyp==3 and 2D conveyance >=1                '
   HELPM (15)= 'nonlin = max (nonlin1D, nonlin2D)                           '
   HELPM (16)= 'Specify absolute value for dropping water.                  '
   HELPM (17)= 'Specify absolute value for dropping land.                   '
   HELPM (18)= '0=only setbobs, 1=small stencil, 2=full subgrid weir        '
   HELPM (19)= '1=all sigma, 2=all z, 3=left sigma, 4=left z                '
   HELPM (20)= '1d0=uniform, 1.1d0 = increase factor from bottom up         '
   HELPM (21)= 'Only Fixedweirs if both left and right sillheight > Sillmin '
   HELPM (22)= 'max nr of z-layers                                          '
   HELPM (23)= '1=mean, 2=max, 3=min, 4= uniform                            '
   HELPM (24)= '0=no, 1=yes, 2=kb/kb+1 50/50                                '
   HELPM (25)= 'only for zlayers: numer of top layers behaving sigma like   '
   HELPM (26)= 'only for zlayers: keep numtopsig constant 1=yes, 0=no       '
   HELPM (27)= '(1-4): 1,2 lower 3,4 all, central 1,3 max 2,4               '
   HELPM (28)= '0, 1                                                        '


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

   CALL IFormPutDouble  (2*1 ,sini,     '(F8.3)')
   CALL IFormPutDouble  (2*2 ,zkuni,    '(F8.3)')
   CALL IFormPutDouble  (2*3 ,bedslope, '(F8.3)')
   CALL IFormPutDouble  (2*4 ,anglat,   '(F8.3)')
   CALL IFORMPUTINTEGER (2*5 ,ibedlevtyp        )
   CALL IFORMPUTINTEGER (2*6 ,kmx               )
   CALL IFORMPUTINTEGER (2*7 ,Jazlayercenterbedvel)
   CALL IFORMPUTINTEGER (2*8 ,jasfer3D          )
   CALL IFORMPUTinteger (2*9 ,jalimnor          )
   CALL IFORMPUTdouble  (2*10,dxmin1D,   '(F8.3)' )
   CALL IFORMPUTdouble  (2*11,wu1DUNI ,   '(F8.3)' )
   CALL IFORMPUTINTEGER (2*12,iproftypuni          )
   CALL IFORMPUTINTEGER (2*13,jaconveyance2D    )
   CALL IFORMPUTINTEGER (2*14,nonlin2D          )
   CALL IFORMPUTINTEGER (2*15,nonlin1D          )
   CALL IFormPutDouble  (2*16,sdropstep,  '(F8.3)')
   CALL IFormPutDouble  (2*17,zkdropstep, '(F8.3)')
   CALL IFORMPUTINTEGER (2*18,ifixedweirscheme     )
   CALL IFORMPUTINTEGER (2*19,Layertype           )
   CALL IFormPutDouble  (2*20,Sigmagrowthfactor, '(F8.3)')
   CALL IFormPutDouble  (2*21,Sillheightmin    , '(F8.3)')
   CALL IFORMPUTINTEGER (2*22,Mxlayz              )
   CALL IFORMPUTINTEGER (2*23,ihuzcsig            )
   CALL IFORMPUTINTEGER (2*24,keepzlayeringatbed  )
   CALL IFORMPUTINTEGER (2*25,numtopsig           )
   CALL IFORMPUTINTEGER (2*26,janumtopsiguniform  )
   CALL IFORMPUTINTEGER (2*27,ihuz                )
   CALL IFORMPUTINTEGER (2*28,jaZlayeratubybob    )


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
           CALL IFormGetDouble  (2*1 ,sini  )
           CALL IFormGetDouble  (2*2 ,zkuni )
           CALL IFormGetDouble  (2*3 ,bedslope    )
           CALL IFormGetDouble  (2*4 ,anglat      )
           CALL IFORMgeTINTEGER (2*5 ,ibedlevtyp  )
           CALL IFORMgeTINTEGER (2*6 ,kmx         )
           CALL IFORMgeTINTEGER (2*7 ,Jazlayercenterbedvel  )
           CALL IFORMgeTINTEGER (2*8 ,jasfer3D    )
           CALL IFORMgetinteger (2*9 ,jalimnor    )
           CALL IFORMgetdouble  (2*10,dxmin1D        )
           CALL IFORMgetdouble  (2*11,wu1DUNI        )
           CALL IFORMgeTINTEGER (2*12,iproftypuni    )
           CALL IFORMgeTINTEGER (2*13,jaconveyance2D )
           CALL IFORMgeTINTEGER (2*14,nonlin2D       )
           CALL IFORMgeTINTEGER (2*15,nonlin1D       )
           CALL IFormGetDouble  (2*16,sdropstep)
           CALL IFormGetDouble  (2*17,zkdropstep)
           CALL IFORMgeTINTEGER (2*18,ifixedweirscheme  )
           CALL IFORMgeTINTEGER (2*19,Layertype         )
           CALL IFormGetDouble  (2*20,Sigmagrowthfactor )
           CALL IFormGetDouble  (2*21,Sillheightmin     )
           CALL IFORMgetINTEGER (2*22,Mxlayz            )
           CALL IFORMgeTINTEGER (2*23,ihuzcsig          )
           CALL IFORMgeTINTEGER (2*24,keepzlayeringatbed)
           CALL IFORMgeTINTEGER (2*25,numtopsig         )
           CALL IFORMgeTINTEGER (2*26,janumtopsiguniform)
           CALL IFORMgeTINTEGER (2*27,ihuz              )
           CALL IFORMgeTINTEGER (2*28,jaZlayeratubybob  )
    
           if (kmx > 0 .or. mxlayz > 0) then
              if (layertype > 1) then
                 kmx = max(kmx,mxlayz) ; iadvec = 33
              endif
           endif

           if (ibedlevtyp .ne. 3) then
              jaconveyance2D = -1
              nonlin2D       = 0
           else if (nonlin2d > 0 .and. jaconveyance2D == 0 ) then
              jaconveyance2D = -1
           endif
           nonlin = max(nonlin1D, nonlin2D)
           call inisferic()
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

   END SUBROUTINE CHANGEgeometryPARAMETERS
