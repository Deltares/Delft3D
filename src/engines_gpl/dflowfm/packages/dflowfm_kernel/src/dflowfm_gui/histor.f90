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
submodule(m_histor) m_histor_

   implicit none

contains

   module subroutine HISTOR()
      use unstruc_files, only: mdia
      use unstruc_colors
      use unstruc_display_data, only: npos
      use m_devices, only: ihs, iws
      use dflowfm_version_module, only: company, product_name
      use m_helpnow
      use m_help

      integer :: ih
      integer :: infoinput
      integer :: infowindow
      integer :: ipos
      integer :: iw
      integer :: ixp
      integer :: iyp
      integer :: j
      integer :: jatab
      integer :: jofnd
      integer :: k
      integer :: key
      integer :: kstart
      integer :: maxtxt
      integer :: numchc
      integer :: numtop
      integer :: numtxt
      integer :: numwnh

      parameter(MAXTXT=400)
      character DIATXT(MAXTXT) * 70
!
      rewind (MDIA)
      K = 0
10    continue
      read (MDIA, '(A)', end=888)
      K = K + 1
      goto 10
888   continue
      KSTART = K - MAXTXT + 2
      rewind (MDIA)
!
      K = 0
      J = 1
20    continue
      K = K + 1
      if (K >= KSTART) then
         read (MDIA, '(A)', end=999) DIATXT(J)
         J = J + 1
      else
         read (MDIA, '(A)', end=999)
      end if
      goto 20
999   continue
!
      backspace (MDIA)
      NUMTXT = J - 1
      JATAB = 0
      JOFND = 0
      NUMTOP = NUMTXT
      NUMCHC = NUMTXT
      NLEVEL = 1

!     Initialise
      call IWinWordWrap('OFF')
      call ITEXTCOLOURN(HLPFOR, HLPBCK)
      call INHIGHLIGHT('WHITE', 'RED')
      IW = NPOS(3)
      IXP = NPOS(1) + (IWS - IW) / 2
      IYP = NPOS(2)
      IH = IHS - 9

!     Header of filewindow
      call IWinAction('FPC')
      call IWinOpen(IXP, IYP, IW, 1)
      call ITEXTCOLOURN(LBLFOR, LBLBCK)
      call IWinOutCentre(1, trim(company)//'-'//trim(product_name)//' HISTORY')
      call ITEXTCOLOURN(HLPFOR, HLPBCK)
!
!     Explain keyfunctions in bottom window
      call IWinAction('FPC')
      call IWinOpen(IXP, IHS - 1, IW, 2)
      call IWinOutStringXY(1, 1, 'move = ,Pgup, Pgdwn, home; quit = Esc')
!
!     Filewindow is middelste window
      call IWinAction('FPC')
      call IWinOpen(IXP, IYP + 3, IW, IH)
!
      NUMWNH = InfoWindow(1)
      call IWinSelect(NUMWNH)

50    continue
      IPOS = max(1, NUMTXT - 10)
      call IWINBROWSETEXT(DIATXT, NUMTXT, 10, IPOS, ' ')
      KEY = INFOINPUT(55)
      if (KEY == 24) then
         call HELP(WRDKEY, NLEVEL)
         goto 50
      end if

      call IWinClose(1)
      call IWinClose(1)
      call IWinClose(1)

   end subroutine HISTOR

end submodule m_histor_
