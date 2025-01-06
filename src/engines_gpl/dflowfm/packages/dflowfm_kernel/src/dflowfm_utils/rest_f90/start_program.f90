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

module m_start_program

implicit none

private

public :: start_program

contains

      subroutine START_PROGRAM()
         use M_dimens
         use M_DEVICES
         use unstruc_files
         use unstruc_startup
         use dflowfm_version_module, only: base_name
         use m_gui
         use unstruc_messages
         use m_helpnow
         use m_fkeys
         use m_menuh
         use m_botlin
         use m_firstlin

         integer :: infofile
         integer :: infoopsystem
         integer :: ja
         integer :: jscreen
         integer :: key
         integer :: num
         integer :: numclargs
         integer :: nwhat
         character(len=8192) :: cmd
         integer :: cmdlen

!
         WRDKEY = 'PROGRAM PURPOSE'
         NLEVEL = 1
         JSCREEN = 0
         INFOFILE = 0

         call INIDIA(base_name)

         call FIRSTLIN(MDIA)
         call FIRSTLIN(6)

         call get_command(cmd, cmdlen)
         write (msgbuf, '(a,a)') 'Command: ', cmd(1:cmdlen); call msg_flush()

         if (jaGUI /= 1) return

!     initialisatiefiles
         call initProgram()

         if (jaGUI /= 1) return

! SPvdP: disabled mouse-check for mouseless buildserver
!      JMOUSE = INFOHARDWARE(13)
!      IF (JMOUSE .EQ. 1) THEN
!         CALL QNERROR ('NO MOUSE FOUND',' ',' ')
!         NLEVEL = 2
!         WRDKEY = 'MOUSE INSTALLATION'
!         CALL HELP(WRDKEY,NLEVEL)
!         CALL STOPINT()
!      ENDIF

         write (msgbuf, *) 'MAXIMUM NUMBER OF LINKS         : ', LMAX; call msg_flush()
         write (msgbuf, *) 'MAXIMUM NUMBER OF NODES         : ', KMAX; call msg_flush()
         write (msgbuf, *) 'RESOLUTION GRAPHICS SCREEN      : ', NPX, NPY; call msg_flush()
         write (msgbuf, *) 'RESOLUTION TEXT     SCREEN      : ', IWS, IHS; call msg_flush()
         write (msgbuf, *) 'NUMBER OF COLOURS AVAILABLE     : ', NCOLR; call msg_flush()

15       continue
         NUMCLARGS = INFOOPSYSTEM(2)
         if (NUMCLARGS > 0 .or. INFOFILE == 1) return
         KEY = 0
         JA = 2

         call MENUH(JA, NUM, NWHAT)
         call BOTLIN(JA, 0, KEY)

         if (KEY >= 24 .and. KEY <= 26) then
            call FKEYS(KEY)
            goto 15
         end if

         return
      end subroutine START_PROGRAM

end module m_start_program
