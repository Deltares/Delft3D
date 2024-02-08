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

   SUBROUTINE CHANGENUMERICALPARAMETERS()
   use m_netw
   USE M_FLOW
   use m_flowgeom
   USE m_sferic
   use m_wind
   use unstruc_display
   use m_reduce
   use m_sediment, only: dmorfac
   use dflowfm_version_module, only : company, product_name
   use unstruc_messages
   use m_fixedweirs
   implicit none

   integer :: numpar, numfld, numparactual, numfldactual
   PARAMETER  (NUMPAR = 24, NUMFLD = 2*NUMPAR)
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
   OPTION( 1) = 'COURANT NR                           ( )' ; it(2* 1) = 6
   OPTION( 2) = 'IADVEC                                  ' ; it(2* 2) = 2
   OPTION( 3) = 'IADVEC1D                                ' ; it(2* 3) = 2
   OPTION( 4) = 'Limtyp scalar   transport               ' ; it(2* 4) = 2
   OPTION( 5) = 'Limtyp hu                               ' ; it(2* 5) = 2
   OPTION( 6) = 'Limtyp momentum transport               ' ; it(2* 6) = 2
   OPTION( 7) = 'itstep                                  ' ; it(2* 7) = 2
   OPTION( 8) = 'teta                                ( ) ' ; it(2* 8) = 6
   OPTION( 9) = 'icgsolver                           ( ) ' ; it(2* 9) = 2
   OPTION(10) = 'Transport Method                    ( ) ' ; it(2*10) = 2
   OPTION(11) = 'Salinity included 0/1               ( ) ' ; it(2*11) = 2
   OPTION(12) = 'Temperature model nr, 0=no, 5=heatflx() ' ; it(2*12) = 2
   OPTION(13) = 'Anti creep                          ( ) ' ; it(2*13) = 2
   OPTION(14) = '                                    ( ) ' ; it(2*14) = 6
   OPTION(15) = 'irov 0,1,2,3                        ( ) ' ; it(2*15) = 2
   OPTION(16) = 'icorio, 0, 5=org def., even=2D weigh( ) ' ; it(2*16) = 2
   OPTION(17) = 'jatidep tidal potential forcing 0/1 ( ) ' ; it(2*17) = 2
   OPTION(18) = 'EpsCG, CG solver stop criterion     ( ) ' ; it(2*18) = 6
   OPTION(19) = 'Epshu, flooding criterion           (m) ' ; it(2*19) = 6
   OPTION(20) = 'JaExplicitsinks                     ( ) ' ; it(2*20) = 2
   OPTION(21) = 'Corioadamsbashfordfac               ( ) ' ; it(2*21) = 6
   OPTION(22) = 'Newcorio                            ( ) ' ; it(2*22) = 2
   OPTION(23) = 'Barocterm                           ( ) ' ; it(2*23) = 2
   OPTION(24) = 'Barocadamsbashfordfac               ( ) ' ; it(2*24) = 6


!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6

   HELPM ( 1) = 'Total    COURANT                                            '
   HELPM ( 2) = '0=N0, 33=Full Perot, 1=wenn tot, 2=wenn inoutdif, 3 =       '
   HELPM ( 3) = 'see iadvec                                                  '
   HELPM ( 4) = '0=No, 1=Minmod, 2=VanLeer, 3=Koren, 4=Monotonized Central  '
   HELPM ( 5) = '0=No, 1=Minmod, 2=VanLeer, 3=Koren, 4=Monotonized Central, 21=central'
   HELPM ( 6) = '0=No, 1=Minmod, 2=VanLeer, 3=Koren, 4=Monotonized Central  '
   HELPM ( 7) = '2=implicit pressure, 1=no pressure, 0 = only transport      '
   HELPM ( 8) = '0.5 < teta =< 1.0                                           '
   HELPM ( 9) = '1 = GS_OMP, 2 = GS_OMPthreadsafe, 3 = GS, 4 = SaadILUD      '
   HELPM (10) = '0=Herman transport, 1=transport module (default), 2=no      '
   HELPM (11) = '0=no salinity, 1=yes salinity                               '
   HELPM (12) = 'Temperature model nr, 0=no temp, 5=heat flux 3=excess       '
   HELPM (13) = '0=No, 1=Yes anticreep  only in sigma layers                 '
   HELPM (14) = 'default 1d-8                                                '
   HELPM (15) = '0=free slip, 1 =partial slip, 2=no slip, 3 =hydraul. smooth '
   HELPM (16) = '0=no 5=default, 3,4 no weights, 5-10 Olga, 25-30 Ham        '
   HELPM (17) = '0=no tidal potential, 1=yes tidal potential                 '
   HELPM (18) = 'Guus, if max(abs(r/rk) < epscg , or Saad L2norm < epscg     '
   HELPM (19) = 'hu > epshu: link flows                                      '
   HELPM (20) = '1=expl, 0 = impl                                            '
   HELPM (21) = '>0 = Adams Bashford, standard= 0.5, only for Newcorio=1     '
   HELPM (22) = '0=prior to 27-11-2019, 1=no normal forcing on open bnds, 12#'
   HELPM (23) = '3=default, 4=new                                            '
   HELPM (24) = '>0 = Adams Bashford, standard= 0.5, only for Baroctimeint=4 '


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

   NIADVEC = IADVEC
   CALL IFormPutDouble  (2* 1 ,CFLmx,  '(F8.3)' )
   CALL IFORMPUTINTEGER (2* 2 ,NIADVEC          )
   CALL IFORMPUTINTEGER (2* 3 ,IADVEC1D         )
   CALL IFORMPUTINTEGER (2* 4 ,Limtypsa         )
   CALL IFORMPUTINTEGER (2* 5 ,Limtyphu         )
   CALL IFORMPUTINTEGER (2* 6 ,Limtypmom        )
   CALL IFORMPUTINTEGER (2* 7 ,itstep           )
   CALL IFormPutDouble  (2* 8 ,teta0  ,'(F10.3)')
   CALL IFORMPUTinteger (2* 9 ,icgsolver        )
   CALL IFORMPUTinteger (2*11 ,jasal            )
   CALL IFORMPUTinteger (2*12 ,jatem            )
   CALL IFORMPUTinteger (2*13 ,jacreep          )
   CALL IFORMPUTdouble  (2*14 ,epsmaxlev,'(e10.5)' )
   CALL IFORMPUTinteger (2*15 ,irov             )
   CALL IFORMPUTinteger (2*16 ,icorio           )
   CALL IFORMPUTinteger (2*17 ,jatidep          )
   CALL IFormPutDouble  (2*18 ,epscg, '(e10.5)' )
   CALL IFormPutDouble  (2*19 ,epshu, '(e10.5)' )
   CALL IFORMPUTinteger (2*20 ,jaexplicitsinks  )
   CALL IFormputDouble  (2*21 ,Corioadamsbashfordfac,'(e10.5)')
   CALL IFormputinteger (2*22 ,Newcorio)
   CALL IFormputinteger (2*23 ,Jabarocterm)
   CALL IFormputDouble  (2*24 ,Barocadamsbashfordfac,'(e10.5)')

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

          CALL IFormgetDouble  (2* 1 ,CFLmx           )
          CALL IFORMgeTINTEGER (2* 2 ,NIADVEC         )
          CALL IFORMgeTINTEGER (2* 3 ,IADVEC1D        )
          CALL IFORMgeTINTEGER (2* 4 ,Limtypsa        )  ;  limtypsa  = max(0, min (limtypsa ,30))
          CALL IFORMgeTINTEGER (2* 5 ,Limtyphu        )  ;  limtyphu  = max(0, min (limtyphu ,30))
          CALL IFORMgeTINTEGER (2* 6 ,Limtypmom       )  ;  limtypmom = max(0, min (limtypmom,30))
          CALL IFORMgeTINTEGER (2* 7 ,itstep          )
          CALL IFormgetDouble  (2* 8 ,teta0           )
          CALL IFORMgeTinteger (2* 9 ,icgsolver       )
          CALL IFORMgeTinteger (2*11 ,jasal           )
          CALL IFORMgeTinteger (2*12 ,jatem           )
          CALL IFORMgeTinteger (2*13 ,jacreep         )
          CALL IFORMgeTdouble  (2*14 ,epsmaxlev       )
          CALL IFORMgeTinteger (2*15 ,irov            )
          CALL IFORMgeTinteger (2*16 ,icorio          )
          CALL IFORMgeTinteger (2*17 ,jatidep         )
          CALL IFormgetDouble  (2*18 ,epscg           )
          CALL IFormgetDouble  (2*19 ,epshu           )
          CALL IFORMgeTinteger (2*20 ,jaexplicitsinks )
          CALL IFormgetDouble  (2*21 ,Corioadamsbashfordfac)
          CALL IFormgetinteger (2*22 ,Newcorio)
          CALL IFormgetinteger (2*23 ,Jabarocterm)
          CALL IFormgetDouble  (2*24 ,Barocadamsbashfordfac)

          epshs    = 0.2d0*epshu  ! minimum waterdepth for setting cfu
          if (niadvec .ne. iadvec) then
             if (nfxw > 0) then
                call confrm('If Fixedweirs present, please reinitialise the model', ja)
             endif
             iadvec = niadvec
             call iadvecini()
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

   END SUBROUTINE CHANGENUMERICALPARAMETERS
