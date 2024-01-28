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

      SUBROUTINE START_PROGRAM()
      use M_dimens
      USE M_DEVICES
      use unstruc_files
      use unstruc_startup
      use dflowfm_version_module, only : base_name
      use unstruc_display, only : jaGUI
      use unstruc_messages

      implicit none

      integer :: infofile
      integer :: infohardware
      integer :: infoopsystem
      integer :: ja
      integer :: jmouse
      integer :: jscreen
      integer :: key
      integer :: nlevel
      integer :: num
      integer :: numclargs
      integer :: nwhat
      COMMON /HELPNOW/   WRDKEY, NLEVEL
      COMMON /KERN3D/    INFOFILE,NAMEGRID,NAMEFIELDI,NAMEFIELDO,GRIDAT
      COMMON /MESSAGETOSCREEN/ JSCREEN
      CHARACTER NAMEGRID*80,NAMEFIELDI*80,NAMEFIELDO*80,GRIDAT*1
      CHARACTER WRDKEY*40
      CHARACTER(len=8192) :: cmd
      integer :: cmdlen

!
      WRDKEY  = 'PROGRAM PURPOSE'
      NLEVEL  = 1
      JSCREEN = 0
      INFOFILE = 0

      CALL INIDIA(base_name)

      CALL FIRSTLIN(MDIA)
      CALL FIRSTLIN(6)

      CALL get_command(cmd, cmdlen)
      write (msgbuf, '(a,a)') 'Command: ', cmd(1:cmdlen); call msg_flush()

      if ( jaGUI.ne.1 ) return

!     initialisatiefiles
      CALL initProgram()

      if ( jaGUI.ne.1 ) return

! SPvdP: disabled mouse-check for mouseless buildserver
!      JMOUSE = INFOHARDWARE(13)
!      IF (JMOUSE .EQ. 1) THEN
!         CALL QNERROR ('NO MOUSE FOUND',' ',' ')
!         NLEVEL = 2
!         WRDKEY = 'MOUSE INSTALLATION'
!         CALL HELP(WRDKEY,NLEVEL)
!         CALL STOPINT()
!      ENDIF

      WRITE(msgbuf,*) 'MAXIMUM NUMBER OF LINKS         : ', LMAX      ; call msg_flush()
      WRITE(msgbuf,*) 'MAXIMUM NUMBER OF NODES         : ', KMAX      ; call msg_flush()
      WRITE(msgbuf,*) 'RESOLUTION GRAPHICS SCREEN      : ', NPX,  NPY ; call msg_flush()
      WRITE(msgbuf,*) 'RESOLUTION TEXT     SCREEN      : ', IWS,  IHS ; call msg_flush()
      WRITE(msgbuf,*) 'NUMBER OF COLOURS AVAILABLE     : ', NCOLR     ; call msg_flush()

   15 CONTINUE
      NUMCLARGS = INFOOPSYSTEM(2)
      IF (NUMCLARGS .GT. 0 .OR. INFOFILE .EQ. 1) RETURN
      KEY  = 0
      JA   = 2

      CALL MENUH(JA,NUM,NWHAT)
      CALL BOTLIN(JA,0,KEY)

      IF (KEY .GE. 24 .AND. KEY .LE. 26) THEN
         CALL FKEYS(KEY)
         GOTO 15
      ENDIF

      RETURN
      END SUBROUTINE START_PROGRAM
