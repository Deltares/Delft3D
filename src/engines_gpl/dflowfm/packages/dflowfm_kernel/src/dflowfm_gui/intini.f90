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

module m_intini
use m_settextsize
use m_setgrafmod
use m_setcolortable
use m_minmaxworld


implicit none

contains

      subroutine INTINI()
         use m_inikeys
         use m_sferic
         use dflowfm_version_module, only: company, product_name, version_full
         use m_wearelt
         use m_devices
         use m_locatora
         use m_screenarea
         use m_colnow
         use m_initscreen
         use m_view_port
         
         integer :: icrhf
         integer :: infoopsystem

         JSFERIC = 0
         ! CALL ISCREENMODEOPTIONS(1,NTXCOLS)
         ! CALL ISCREENMODEOPTIONS(2,NTXROWS)
         call ISCREENMODEOPTIONS(6, 1)
         call ISCREENMODEOPTIONS(9, 1)
         NOPSYS = INFOOPSYSTEM(1)
         NCOLR = 256

         if (NOPSYS == 1 .and. JVGA == 1) then
            NXPIX = 640
            NYPIX = 480
            NCOLR = 16
!        CALL VGA@()
         end if
         call ISCREENOPEN(' ', 'GR', NXPIX, NYPIX, NCOLR)

         call ISCREENTITLE('G', trim(company)//'-'//trim(product_name)//' '//trim(version_full))

         !CALL ISCREENTITLE('G', PROGNM)

         call SETGRAFMOD()
         call SETCOLORTABLE()

         call INIKEYS()
!      CALL INSERTOVER('OVER')

!     set size crosshair cursor
         ICRHF = 1d0 / CROSHRSZ

         call IGRINPUTOPTIONS(5, ICRHF)
!
         call InEventSelect(0, 0)
         if (NOPSYS == 1) then
!        Mouse button down, up, move is an event
            call InEventSelect(0, 1 + 2 + 8)
         else
!        Mouse button down, up, resize an event
            call InEventSelect(0, 1 + 2 + 32)
            call InEventSelect(0, 1 + 2 + 8 + 32)
!        Enable processing of expose/resize events
            call InControlKey(50, 259)
         end if
!
         call ICURSOR(' ')

!     exit on mouse click outside input area
         call INMOUSEOPTIONS(2, 1)

!     only BUTTON DOWN
         call INMOUSEOPTIONS(3, 0)

         call IFRAMEOPTIONS(6, 15)
         call IFRAMEOPTIONS(7, 0)

!     CALL IFRAMETYPE(9)
!     CALL IFORMDEFAULTS(3)

         call SETTEXTSIZE()
         call IGRFILLPATTERN(4, 0, 0)

         YBOT = 0d0
         XLEFT = 0d0
         JAXIS = 0
         call viewport(0.0, 0.0, 1.0, 1.0)
!      CALL IPGAREA(0.0,0.0,1.0,1.0)

         XMIN = 0d0
         XMAX = 1d0
         YMIN = 0d0
         YMAX = 1d0
         X1 = XMIN
         X2 = XMAX
         Y1 = YMIN
         Y2 = YMAX
         NCOLNOW = 31
         XLC = 0
         YLC = 0

         call MINMAXWORLD(XMIN, YMIN, XMAX, YMAX)

         return
      end

end module m_intini
