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
module m_help
   use m_search
   use m_searc2
   use m_scrolh
   use m_scrlpg

   implicit none
contains
   subroutine HELP(WRDKEY, NLEVEL)
      use unstruc_colors
      use unstruc_display_data, only: npos
      use m_devices, only: ihs, iws
      use dflowfm_version_module, only: company, product_name
      use m_helpc

      integer :: i
      integer :: ih
      integer :: infowindow
      integer :: iw
      integer :: ixp
      integer :: ixs
      integer :: iyp
      integer :: iys
      integer :: japop
      integer :: jatab
      integer :: jofnd
      integer :: len
      integer :: line
      integer :: maxkwd
      integer :: nahead
      integer :: nback
      integer :: nforg
      integer :: nlevel
      integer :: numchc
      integer :: numkey
      integer :: numpag
      integer :: numpgk
      integer :: numtop
      integer :: numwnb
      integer :: numwnh
      integer :: numwnk
      integer :: numwnt
!     Gives helptext starting from wrdkey in screen with dimensions npos
      parameter(MAXKWD=400)
      integer NHTONK(MAXHLP), NKTONH(MAXKWD)
      character WRDKEY * 40, KEYWRD(MAXKWD) * 40, LOOKUP * 20, TEXLIN * 80
!
!     Initialise
      call IWinWordWrap('OFF')
      call ITEXTCOLOURN(HLPFOR, HLPBCK)
      call INHIGHLIGHT('WHITE', 'RED')
!     IXP    = 1
!     IYP    = 1
!     IW     = IXP + IWS-IW
!     IH     = INFOSCREEN(3) - 9
      IW = NPOS(3)
      IH = IHS - 9
      IXP = NPOS(1) + (IWS - IW) / 2
      IYP = NPOS(2) - 1
      NAHEAD = 1
      JATAB = 0
      JAPOP = 0
      NUMTOP = NUMTXT + 1
      NUMCHC = 1
      NUMKEY = 0
      NUMPAG = 1 + (NUMTXT - IH + 1) / IH
      LOOKUP = WRDKEY
!
!     Count the number of keywords in text and make cross references
      do I = 1, NUMTXT
         if (HLPTXT(I) (1:3) /= '   ') then
            NUMKEY = NUMKEY + 1
            KEYWRD(NUMKEY) = HLPTXT(I)
            NKTONH(NUMKEY) = I
         end if
         NHTONK(I) = NUMKEY
      end do
      NUMPGK = 1 + (NUMKEY - IH + 1) / IH
!
!     Header of helpwindow
      call IWinAction('FPC')
      call IWinOpen(IXP, IYP, IW, 1)
      NUMWNT = InfoWindow(1)
      call ITEXTCOLOURN(LBLFOR, LBLBCK)
      TEXLIN = '               '//trim(company)//'-'//trim(product_name)//' HELPWINDOW'
      call IWinOutSTRINGXY(1, 1, TEXLIN)
      call IWinOutStringXY(IW - 16, 1, 'page =    of   ')
      call IWinOutIntegerXY(IW - 3, 1, NUMPAG, 2)

!     TEXLIN = '                     '//PROGNM//  ' HELPWINDOW
!    *        page =    of   '
!     CALL IWinOutStringXY(1,1,TEXLIN)
!     CALL IWinOutIntegerXY(IW-10,1,NUMPAG,2)

!     Explain keyfunctions in bottom window
      call IWinAction('FPC')
      call IWinOpen(IXP, IYP + 6 + IH, IW, 2)
      NUMWNB = InfoWindow(1)
      call ITEXTCOLOURN(HLPFOR, HLPBCK)
      call IWinOutStringXY(1, 1, 'pages = PgUp/PgDn; scroll =   ; toggle keyword menu = Tab')
      call IWinOutStringXY(1, 2, 'top or bottom = Home/End; exit = Esc; search = F7')
!
!     Helpwindow is middelste window
      call IWinAction('FPC')
      call IWinOpen(IXP, IYP + 4, IW, IH)
      NUMWNH = InfoWindow(1)
!
!     Start with keyword WRDKEY
      call SEARCH(NAHEAD, NLEVEL, HLPTXT, NUMTXT, WRDKEY, NUMCHC, JOFND)
!
20    continue
!
!
!     Display one page of help
      if (JATAB == 0) then
         call IWinSelect(NUMWNH)
         call SCRLPG(HLPTXT, NUMTXT, NUMTOP, NUMCHC, IH)
      else
         call IWinSelect(NUMWNK)
         call SCRLPG(KEYWRD, NUMKEY, NUMTOP, NUMCHC, IH)
      end if
!
!     Display pagenumber in top window
      call IWinSelect(NUMWNT)
      call IWinOutIntegerXY(IW - 9, 1, 1 + NUMTOP / IH, 2)
!
!     Indicate present keyword level with cursor position
      call ITextAttribute('BRU')
      if (JATAB == 0) then
         call IWinSelect(NUMWNH)
         call IWinOutStringXY(NLEVEL, NUMCHC - NUMTOP + 1, HLPTXT(NUMCHC) (NLEVEL:NLEVEL))
      else
         call IWinSelect(NUMWNK)
         call IWinOutStringXY(NLEVEL, NUMCHC - NUMTOP + 1, KEYWRD(NUMCHC) (NLEVEL:NLEVEL))
      end if
      call ITextAttribute(' ')
!
!     Get instructions
      if (JATAB == 0) then
         call SCROLH(NUMCHC, HLPTXT, NUMTXT, NLEVEL, IH, JOFND, JATAB)
      else
         call SCROLH(NUMCHC, KEYWRD, NUMKEY, NLEVEL, IH, JOFND, JATAB)
      end if
!
      if (JOFND == -1) then
!        Search for keyword
         IXS = NPOS(1) + 46
         IYS = NPOS(2) + IH + 6
         call InStringXYDef(IXS, IYS, ' => ', 0, LOOKUP, LEN)
         if (JATAB == 0) then
            call SEARC2(NAHEAD, HLPTXT, NUMTXT, LOOKUP, NUMCHC, JOFND)
         else
            call SEARC2(NAHEAD, KEYWRD, NUMKEY, LOOKUP, NUMCHC, JOFND)
         end if
         call IWinSelect(NUMWNB)
         call ITEXTCOLOURN(HLPFOR, HLPBCK)
         call IWinOutStringXY(1, 2, 'top or bottom = Home/End; exit = Esc; search : F7)                               . ')
         if (JATAB == 1) call ITEXTCOLOURN(WNDFOR, WNDBCK)
      else if (JATAB == 1) then
!        met tab wordt popup keyword window geopend of gesloten
         if (JAPOP == 0) then
            call IWinSelect(NUMWNT)
            call ITEXTCOLOURN(LBLFOR, LBLBCK)
            TEXLIN = '               '//trim(company)//'-'//trim(product_name)//' KEYWORDWINDOW'
            call IWinOutSTRINGXY(1, 1, TEXLIN)
            call IWinOutStringXY(IW - 16, 1, 'page =    of   ')
            call IWinOutIntegerXY(IW - 3, 1, NUMPGK, 2)
            call ITEXTCOLOURN(WNDFOR, WNDBCK)
            call IWinAction('PC')
            call IWinOpen(IXP + 40, IYP + 4, IW - 40, IH)
            NUMWNK = InfoWindow(1)
            JAPOP = 1
            LINE = NUMCHC - NUMTOP
            NUMCHC = NHTONK(NUMCHC)
            NUMTOP = max(1, min(NUMCHC - LINE, NUMKEY - IH + 1))
         end if
      else
         if (JAPOP == 1) then
            call IWinSelect(NUMWNK)
            call ITEXTCOLOURN(HLPFOR, HLPBCK)
            call IWinClose(1)
            JAPOP = 0
            call IWinSelect(NUMWNT)
            call ITEXTCOLOURN(LBLFOR, LBLBCK)
            TEXLIN = '               '//trim(company)//'-'//trim(product_name)//' HELPWINDOW'
            call IWinOutSTRINGXY(1, 1, TEXLIN)
            call IWinOutStringXY(IW - 16, 1, 'page =    of   ')
            call IWinOutIntegerXY(IW - 3, 1, NUMPAG, 2)
            LINE = NUMCHC - NUMTOP
            NUMCHC = NKTONH(NUMCHC)
            NUMTOP = NUMCHC - LINE
            call ITEXTCOLOURN(HLPFOR, HLPBCK)
         end if
      end if

      if (NUMCHC /= 0) goto 20

      if (JAPOP == 1) then
         call IWinClose(1)
      end if
      call IWinClose(1)
      call IWinClose(1)
      call IWinClose(1)
      call ITEXTCOLOURN(NFORG, NBACK)
      return
   end
end module m_help
