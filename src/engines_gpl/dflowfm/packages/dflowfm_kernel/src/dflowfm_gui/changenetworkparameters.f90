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

   SUBROUTINE changenetworkPARAMETERS()
   use m_sferic
   use network_data
   use unstruc_display
   use m_ec_triangle
   use m_missing
   use dflowfm_version_module, only : company, product_name
   use unstruc_model, only: md_dryptsfile

   implicit none
   integer :: i
   integer  :: ierror
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
   integer :: key
   integer :: nbut
   integer :: nlevel
   integer :: numfldactual
   integer :: numparactual
   integer :: jins_old  ! netcell administration out of date if jins changes
   integer :: iselect, minp
   CHARACTER*128 select(3)

   integer, parameter :: NUMPAR = 22, NUMFLD = 2*NUMPAR
   INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
   CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
   COMMON /HELPNOW/ WRDKEY,NLEVEL
   integer, external :: infoinput
   external :: highlight_form_line

   jins_old = jins

   NLEVEL    = 4
   OPTION(1) = 'SELECT INSIDE POLYGON (1/0), 1 = INSIDE ' ; IT( 1*2)  = 2
   !OPTION(2) = 'TRIANGLEMINANGLE                       ' ; IT( 2*2)  = 6
   OPTION(2) = 'jadelnetlinktyp                         ' ; IT( 2*2)  = 2
   OPTION(3) = 'TRIANGLEMAXANGLE                        ' ; IT( 3*2)  = 6
   OPTION(4) = 'TRIANGLESIZEFACTOR, MAX.INSIDE/ AV.EDGE ' ; IT( 4*2)  = 6
   OPTION(5) = 'limit center; 1.0:in cell <-> 0.0:on c/g' ; IT( 5*2)  = 6
   OPTION(6 )= 'cosphiutrsh in geominit (good orhto)    ' ; IT( 6*2)  = 6
   OPTION(7 )= 'remove small links       0.0->          ' ; IT( 7*2)  = 6
   OPTION(8 )= 'TIME CONSUMING NETWORK CHECKS YES/NO 1/0' ; IT( 8*2)  = 2
   OPTION(9 )= 'NR OF SMOOTH. ITER. IN COURANT NETWORK  ' ; IT( 9*2)  = 2
   OPTION(10)= 'SMALLEST CELLSIZE IN COURANT NETWORK    ' ; IT(10*2)  = 6
   OPTION(11)= 'REMOVE SMALL TRIANGLES, TRIAREAREMFRAC  ' ; IT(11*2)  = 6
   OPTION(12)= 'REFINE NETWORK (QUADS) DIRECTION: 0,-1,1' ; IT(12*2)  = 2
   OPTION(13)= 'Merge nodes closer than tooclose (m)    ' ; IT(13*2)  = 6
   OPTION(14)= 'Connect 1D end nodes to branch if closer' ; IT(14*2)  = 6
   OPTION(15)= 'Uniform DX in copy landb to 1D netw     ' ; IT(15*2)  = 6
   OPTION(16)= 'snap-to-landbdy tolerance, netboundary  ' ; IT(16*2)  = 6
   OPTION(17)= 'snap-to-landbdy tolerance, inner network' ; IT(17*2)  = 6
   OPTION(18)= 'max nr of faces allowed in removesmallfl' ; IT(18*2)  = 2
!   OPTION(19)= 'dry/illegal/cutcells file (*.pol, *.lst)' ; IT(19*2)  = 4
   if ( len_trim(md_dryptsfile).eq.0 ) then
      OPTION(19) = 'DRY CELL FILE (none)'
   else
      OPTION(19) = 'DRY CELL FILE (' // trim(md_dryptsfile(1:min(len_trim(md_dryptsfile),25))) // ')'
   end if
   IT(19*2) = 4
   OPTION(20)= '1D2D link generation algorithm          ' ; IT(20*2)  = 2
   OPTION(21)= 'Lateral algorithm search radius         ' ; IT(21*2)  = 6
   OPTION(22)= 'Use middle latitude (1/0)               ' ; IT(22*2)  = 2

!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6
   HELPM (1) = &
   '1=inside, 0 = outside polygon (TO BE RESET AFTER USE)       '
   HELPM (2) = &
   '0 = delete nodes, > 0 = linktypkn3 to delete                '
   HELPM (3) = &
   '                                                            '
   HELPM (4) = &
   'MAX. INSIDE TRIANGLE SIZE / AVERAGE SIZE ON POLYGON         '
   HELPM (5 )= &
   '                                                            '
   ! 'in geominit, 1.0=inside, on edge ,  0.9=inside close to edge'

   HELPM (6) = &
   'No flow model created if cosphiu > cosphiutrsh              '

   HELPM (7 )= &
   '0.0 = remove no links, 0.1=remove links < 0.1 sqrt(baL+baR) '
   HELPM (8 )= &
   '                                                            '
   HELPM (9 )= &
   'NR OF SMOOTH. ITERATIONS IN COURANT NETWORK, SAMPLES RQUIRED'
   HELPM (10)= &
   'SMALLEST CELLSIZE IN COURANT NETWORK, SAMPLES REQUIRED      '
   HELPM (11)= &
   'SMALL TRIANGLE REMOVED IF TRIAREA < AV. ADJACENT AREAS      '
   HELPM (12)= &
   '0=BOTH DIRECTIONS, -1 = ONLY THIS, 1 = ONLY THAT            '
   HELPM (13)= &
   'Used in merge nodes on top of each other                    '
   HELPM (14)= &
   'than xx (m) to branch node, used in mergenodesontop         '
   HELPM (15)= &
   'used in copylandboundaryto1Dnetwork                         '
   HELPM (16)= &
   'tolerance in snap-to-landbdy, netboundary only (meshwidths) '
   HELPM (17)= &
   'tolerance in snap-to-landbdy, inner network    (meshwidths) '
   HELPM (18)= &
   'max nr of faces allowed in removesmallflowlinks             '
   HELPM (19)= &
   'choose                                                      '
   WRITE(HELPM (20), '(I0,A,I0,A,I0,A)') &
   I1D2DTP_1TO1, ': default (1-to-1), ', I1D2DTP_1TON_EMB, ': embedded 1-to-n, ', I1D2DTP_1TON_LAT, ': lateral 1-to-n.'
    HELPM (22)= &
   '1 = yes, 0 = no                                             '


   CALL SAVEKEYS()
   NUMPARACTUAL = NUMPAR
   NUMFLDACTUAL = 2*NUMPARACTUAL

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL     = IR + 1  ; IR     = IL + 1
      IS(IL) = 82      ; IS(IR) = 10
      IX(IL) = 10      ; IX(IR) = 92
      IY(IL) = 2*I     ; IY(IR) = 2*I
      IT(IL) = 1001    ! ir staat hierboven
   ENDDO

   ! Initialise
   CALL IWinWordWrap('OFF')
   CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
   CALL INHIGHLIGHT('WHITE','RED')
   IW   = NPOS(3)
   IXP = NPOS(1) + (IWS-IW)/2
   IYP  = NPOS(2)
   IH  = IHS - 9

   ! Header of filewindow
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP,IW,1)
   CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
   CALL IWinOutCentre(1,trim(company)//'-'//trim(product_name)// ' PARAMETER FORM')
   CALL ITEXTCOLOURN(HLPFOR,HLPBCK)

   ! Explain keyfunctions in bottom window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IHS-1,IW,2)
   CALL IWinOutStringXY(1,1,'move = , Tab, confirm = Enter, no change = Esc, help = F3')
   CALL IWinOutStringXY(1,2,'right mouse = Enter, click outside window = Esc')

   ! Filewindow is middelste window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP+3,IW,IH)
   CALL InControlKey(29,129)
   CALL InControlKey(30,128)

   ! Define a new form by supplying arrays containing Field positions, sizes and types
   CALL IFORMDEFINE('W',NUMFLDACTUAL,IX,IY,IS,IT)

   ! Define a help field and define help strings for 2 of the 4 input fields
   CALL IFORMHELP(13,IH,60)

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      CALL IFORMPUTSTRING (IL,OPTION(I))
      CALL IFORMPUTHELP   (IR,HELPM(I))
      CALL IFORMATTRIBUTEN(IR,0,0,7)
   ENDDO

   CALL IFORMpuTINTEGER (2*1 , jins    )
   !CALL IFormputDouble (2*2 , TRIANGLEMINANGLE  ,   '(F7.3)')
   CALL IFormputinteger (2*2 , jadelnetlinktyp   )
   CALL IFormputDouble  (2*3 , TRIANGLEMAXANGLE  ,   '(F7.3)')
   CALL IFormputDouble  (2*4 , TRIANGLESIZEFAC   ,   '(F7.3)')
   CALL IFormputDouble  (2*5 , dcenterinside,        '(F7.3)')
   CALL IFormputDouble  (2*6 , cosphiutrsh       ,   '(F7.3)')
   CALL IFormputDouble  (2*7 , removesmalllinkstrsh, '(F7.3)')
   CALL IFORMpuTINTEGER (2*8 , JOCHECKNET)
   CALL IFORMpuTINTEGER (2*9 , NUMITCOURANT)
   CALL IFormputDouble  (2*10, SMALLESTSIZEINCOURANT,'(F7.0)')
   CALL IFormputDouble  (2*11, TRIAREAREMFRAC       ,'(F7.3)')
   CALL IFORMpuTINTEGER (2*12, M13QUAD)
   CALL IFormputDouble  (2*13, Tooclose             ,'(F7.3)')
   CALL IFormputDouble  (2*14, CONNECT1DEND        ,'(F7.3)')
   CALL IFormputDouble  (2*15, Unidx1D              ,'(F7.3)')
   CALL IFormputDouble  (2*16, DCLOSE_bound         ,'(F7.3)')
   CALL IFormputDouble  (2*17, DCLOSE_whole         ,'(F7.3)')
   CALL IFormputinteger (2*18, maxfaceallow)

   CALL IFORMPUTSTRING  (2*19, md_dryptsfile)
   iselect=1
   select(1) = 'use'
   select(2) = 'new'
   select(3) = 'none'
   CALL IFORMPUTMENU(2*19, select,3,iselect)

   CALL IFormputinteger (2*20, imake1d2dtype)
   call IFormputDouble  (2*21, searchRadius1D2DLateral,'(F7.3)')
   CALL IFormputinteger (2*22, jamidlat)


   ! Display the form with numeric fields left justified and set the initial field to number 2
   CALL IOUTJUSTIFYNUM('L')
   IFEXIT = 2
   call IFormAttribute(IFEXIT-1, 'BU', ' ', ' ')
   CALL IFORMSHOW()

30 CONTINUE
   IFINIT = IFEXIT
   CALL IFormEditUser(IFINIT, IFEXIT, highlight_form_line)
   ! check for Help, Confirm, Quit
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
           ! netcell administration out of date if jins changes
           CALL IFORMGETINTEGER (2*1 , jins    )
           if ( jins.ne.jins_old ) netstat = NETSTAT_CELLS_DIRTY
           jins_old = jins

           !CALL IFormGetDouble  (2*2 , TRIANGLEMINANGLE)
           CALL IFormGetinteger  (2*2 , jadelnetlinktyp)
           CALL IFormGetDouble  (2*3 , TRIANGLEMAXANGLE)
           CALL IFormGetDouble  (2*4 , TRIANGLESIZEFAC)
           CALL IFormGetDouble  (2*5 , dcenterinside)
           CALL IFormgetDouble  (2*6 , cosphiutrsh    )
           CALL IFormGetDouble  (2*7 , removesmalllinkstrsh)
           CALL IFORMGETINTEGER (2*8 , JOCHECKNET)
           CALL IFORMGETINTEGER (2*9 , NUMITCOURANT)
           CALL IFormGetDouble  (2*10, SMALLESTSIZEINCOURANT)
           CALL IFormGetDouble  (2*11, TRIAREAREMFRAC)
           CALL IFORMGETINTEGER (2*12, M13QUAD)
           CALL IFormGetDouble  (2*13, Tooclose)
           CALL IFormGetDouble  (2*14, CONNECT1DEND)
           CALL IFormGetDouble  (2*15, Unidx1D)
           CALL IFormGetDouble  (2*16, DCLOSE_BOUND)
           CALL IFormGetDouble  (2*17, DCLOSE_WHOLE)
           CALL IFormGetinteger (2*18, maxfaceallow)

           CALL IFORMGETSTRING(2*19, md_dryptsfile)
           CALL IFORMGETMENU(2*19, iselect)
           if ( iselect.eq.2 ) then
              minp = 2  ! select file only
              call filemenu(minp,md_dryptsfile,ierror)
           else if ( iselect.eq.3 ) then
              md_dryptsfile = ''
           end if
           iselect = 1

           CALL IFormGetinteger (2*20, imake1d2dtype)
           call IFormGetDouble  (2*21, searchRadius1D2DLateral)
           CALL IFormGetinteger (2*22, jamidlat)

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

   END SUBROUTINE changenetworkPARAMETERS
