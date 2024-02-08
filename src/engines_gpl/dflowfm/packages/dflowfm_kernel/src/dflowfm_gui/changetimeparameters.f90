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

  SUBROUTINE CHANGETIMEPARAMETERS()
   USE M_FLOWTIMES
   use unstruc_display
   use dflowfm_version_module, only : company, product_name
   use unstruc_messages
   use m_flow
   implicit none
   integer :: numpar, numfld, numparactual, numfldactual
   PARAMETER  (NUMPAR = 14, NUMFLD = 2*NUMPAR)
   INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
   CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
   integer :: nlevel
   COMMON /HELPNOW/ WRDKEY,NLEVEL
   integer, external :: infoinput
   external :: highlight_form_line
!
   integer :: ir, il, iw, ixp, iyp, ih, i, iuvfieldorg, ifexit, ifinit, key
   integer :: nbut, imp, inp, ierr

   NLEVEL     = 4
   i = 1
   OPTION( i) = 'Dt_user                             (s) ' ; it(2*i) = 6 ; i=i+1
   OPTION( i) = 'Dt_max                              (s) ' ; it(2*i) = 6 ; i=i+1
   OPTION( i) = 'Use automatic time step             ( ) ' ; it(2*i) = 2 ; i=i+1
   OPTION( i) = 'Tstart_user                         (s) ' ; it(2*i) = 6 ; i=i+1
   OPTION( i) = 'Tstop_user                          (s) ' ; it(2*i) = 6 ; i=i+1
   OPTION( i) = 'HisInterval                         (s) ' ; it(2*i) = 6 ; i=i+1
   OPTION( i) = 'MapInterval                         (s) ' ; it(2*i) = 6 ; i=i+1
   OPTION( i) = 'RstInterval                         (s) ' ; it(2*i) = 6 ; i=i+1
   OPTION( i) = 'WaqInterval                         (s) ' ; it(2*i) = 6 ; i=i+1
   OPTION( i) = 'Initial timestep                    (s) ' ; it(2*i) = 6 ; i=i+1
   OPTION( i) = 'Current time                        (s) ' ; it(2*i) = 6 ; i=i+1
   OPTION( i) = 'Smoothing time boundaries Tlfsmo    (s) ' ; it(2*i) = 6 ; i=i+1
   OPTION( i) = 'Dtfacmax                            ( ) ' ; it(2*i) = 6 ; i=i+1
   OPTION( i) = 'Tspinupturblogprof                  ( ) ' ; it(2*i) = 6 ; i=i+1


!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6

   i = 1
   HELPM ( i) = 'User timestep (rythm of external forcing updates)           ' ; i=i+1
   HELPM ( i) = 'Max timestep                                                ' ; i=i+1
   HELPM ( i) = ' 1=2D V/Qouth, 3=3D Vk/Qouthk, 5=3D Vk/Qouhvk, 8=5, kt-1    ' ; i=i+1
   HELPM ( i) = '                                                            ' ; i=i+1
   HELPM ( i) = '                                                            ' ; i=i+1
   HELPM ( i) = '                                                            ' ; i=i+1
   HELPM ( i) = '                                                            ' ; i=i+1
   HELPM ( i) = '                                                            ' ; i=i+1
   HELPM ( i) = '                                                            ' ; i=i+1
   HELPM ( i) = '                                                            ' ; i=i+1
   HELPM ( i) = '                                                            ' ; i=i+1
   HELPM ( i) = '                                                            ' ; i=i+1
   HELPM ( i) = 'dt = min(dtnew, dtfacmax*dtold)                             ' ; i=i+1
   HELPM ( i) = '                                                            ' ; i=i+1


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

   i = 1
   CALL IFormPutDouble  (2* i ,dt_user           ,'(F10.3)') ; i=i+1
   CALL IFormPutDouble  (2* i ,dt_max            ,'(F10.3)') ; i=i+1
   CALL IFORMPUTINTEGER (2* i ,ja_timestep_auto            ) ; i=i+1
   CALL IFormPutDouble  (2* i ,tstart_user       ,'(F10.0)') ; i=i+1
   CALL IFormPutDouble  (2* i ,tstop_user        ,'(F10.0)') ; i=i+1
   CALL IFormPutDouble  (2* i ,ti_his            ,'(F10.3)') ; i=i+1
   CALL IFormPutDouble  (2* i ,ti_map            ,'(F10.3)') ; i=i+1
   CALL IFormPutDouble  (2* i ,ti_rst            ,'(F10.3)') ; i=i+1 
   CALL IFormPutDouble  (2* i ,ti_waq            ,'(F10.3)') ; i=i+1
   CALL IFormPutDouble  (2* i ,dt_init           ,'(F10.3)') ; i=i+1
   CALL IFormPutDouble  (2* i ,time1             ,'(F10.3)') ; i=i+1
   CALL IFormPutDouble  (2* i ,Tlfsmo            ,'(F10.3)') ; i=i+1
   CALL IFormPutDouble  (2* i ,Dtfacmax          ,'(F10.3)') ; i=i+1
   CALL IFormPutDouble  (2* i ,Tspinupturblogprof,'(F10.3)') ; i=i+1

   ! Display the form with numeric fields left justified
   ! and set the initial field to number 2
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

           i = 1
           CALL IFormGetDouble  (2* i ,dt_user           ) ; i=i+1
           CALL IFormGetDouble  (2* i ,dt_max            ) ; i=i+1
           CALL IFORMgeTINTEGER (2* i ,ja_timestep_auto  ) ; i=i+1
           CALL IFormGetDouble  (2* i ,tstart_user       ) ; i=i+1
           CALL IFormGetDouble  (2* i ,tstop_user        ) ; i=i+1 
           CALL IFormGetDouble  (2* i ,ti_his            ) ; i=i+1
           CALL IFormGetDouble  (2* i ,ti_map            ) ; i=i+1 
           CALL IFormGetDouble  (2* i ,ti_rst            ) ; i=i+1
           CALL IFormGetDouble  (2* i ,ti_waq            ) ; i=i+1
           CALL IFormGetDouble  (2* i ,dt_init           ) ; i=i+1
           CALL IFormGetDouble  (2* i ,Time1             ) ; i=i+1
           CALL IFormGetDouble  (2* i ,Tlfsmo            ) ; i=i+1
           CALL IFormGetDouble  (2* i ,Dtfacmax          ) ; i=i+1
           CALL IFormGetDouble  (2* i ,Tspinupturblogprof) ; i=i+1

           if (dt_max > dt_user) then
               dt_max = dt_user
               write(msgbuf, '(a,f9.6,a)') 'DtMax should be <= DtUser. It has been reset to: ', dt_max
               call msg_flush()
           end if

           if (ja_timestep_auto == 3 .or. ja_timestep_auto == 4) then
              if (.not. allocated(Squ2D) ) allocate ( squ2D(ndkx) , stat=ierr )
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

   END SUBROUTINE CHANGETIMEPARAMETERS
