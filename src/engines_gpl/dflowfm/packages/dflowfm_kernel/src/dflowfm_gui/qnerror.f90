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

      !>   write an error-message to the log-file and GUI
      subroutine QNERROR(W1, W2, W3)
         use unstruc_messages
         use m_devices
         use unstruc_model, only: MD_AUTOSTARTSTOP, md_jaAutoStart
         use unstruc_display, only: jaGUI
         implicit none

         character(len=*), intent(in) :: W1, W2, W3

         integer :: infoattribute
         integer :: key
         integer :: nbck
         integer :: nfor
         integer :: nLEVEL
         character(len=600) :: REC, rec2

         common / HELPNOW / WRDKEY, NLEVEL
         character WRDKEY * 40

         REC = trim(W1)//' '//trim(W2)//' '//trim(W3)

         if (len_trim(W2) == 0) then
            rec2 = msgbuf
         else
            rec2 = ' '
         end if
         msgbuf = REC

         ! No user dialog in batchmode runs:
         if (jaGUI == 1 .and. md_jaAutoStart /= MD_AUTOSTARTSTOP) then
            call warn_flush()
            !     inquire current colors
            NFOR = InfoAttribute(13)
            NBCK = InfoAttribute(14)
            call IWinAction('FCP')
            !     set error color
            call ITEXTCOLOUR('BWHITE', 'RED')
            call IWinOpen(1, IHS - 2, IWS, 3)
            call IWINOUTSTRINGXY(IWS - 15, 3, 'press any key')
            call OKAY(0)
            call ITEXTCOLOUR('BLUE', 'BWHITE')
            call IWINOutCentre(2, trim(REC))

            if (len_trim(rec2) > 0) then
               call IWINOutCentre(3, trim(rec2))
            end if

10          continue
            !     CALL INFLUSH()
            call INKEYEVENT(KEY)
            if (KEY == 50 .or. (KEY >= 254 .and. KEY <= 259)) then
               goto 10
            else
               call GETKEY2(KEY)
               if (KEY >= 24 .and. KEY <= 26) then
                  WRDKEY = REC
                  NLEVEL = 4
                  call FKEYS(KEY)
                  goto 10
               end if
            end if

            call IWinClose(1)
            !                            reset colors
            call ITEXTCOLOURN(NFOR, NBCK)

         else

            call mess(LEVEL_ERROR, trim(msgbuf))

         end if

         return
      end
