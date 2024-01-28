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

      SUBROUTINE HELP(WRDKEY,NLEVEL)
      use unstruc_display
      use dflowfm_version_module, only : company, product_name
      implicit none
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
      integer :: maxhlp
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
      integer :: numtxt
      integer :: numwnb
      integer :: numwnh
      integer :: numwnk
      integer :: numwnt
!     Gives helptext starting from wrdkey in screen with dimensions npos
      PARAMETER (MAXHLP = 2000, MAXKWD = 400)
      INTEGER NHTONK(MAXHLP), NKTONH(MAXKWD)
      CHARACTER HLPTXT(MAXHLP)*80,WRDKEY*40,KEYWRD(MAXKWD)*40,LOOKUP*20,TEXLIN*80
      COMMON /HELPC/   HLPTXT,NUMTXT
!
!     Initialise
      CALL IWinWordWrap('OFF')
      CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
      CALL INHIGHLIGHT('WHITE','RED')
!     IXP    = 1
!     IYP    = 1
!     IW     = IXP + IWS-IW
!     IH     = INFOSCREEN(3) - 9
      IW     = NPOS(3)
      IH     = IHS - 9
      IXP    = NPOS(1) + (IWS-IW)/2
      IYP    = NPOS(2) - 1
      NAHEAD = 1
      JATAB  = 0
      JAPOP  = 0
      NUMTOP = NUMTXT + 1
      NUMCHC = 1
      NUMKEY = 0
      NUMPAG = 1 + (NUMTXT-IH+1) / IH
      LOOKUP = WRDKEY
!
!     Count the number of keywords in text and make cross references
      DO 10 I = 1,NUMTXT
         IF (HLPTXT(I)(1:3) .NE. '   ') THEN
            NUMKEY         = NUMKEY + 1
            KEYWRD(NUMKEY) = HLPTXT(I)
            NKTONH(NUMKEY) = I
         ENDIF
         NHTONK(I)      = NUMKEY
   10 CONTINUE
      NUMPGK = 1 + (NUMKEY-IH+1) / IH
!
!     Header of helpwindow
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IYP,IW,1)
      NUMWNT = InfoWindow(1)
      CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
      TEXLIN = '               '//trim(company)//'-'//trim(product_name)//' HELPWINDOW'
      CALL IWinOutSTRINGXY(1,1,TEXLIN)
      CALL IWinOutStringXY (IW-16,1,'page =    of   ')
      CALL IWinOutIntegerXY(IW-3,1,NUMPAG,2)

!     TEXLIN = '                     '//PROGNM//  ' HELPWINDOW
!    *        page =    of   '
!     CALL IWinOutStringXY(1,1,TEXLIN)
!     CALL IWinOutIntegerXY(IW-10,1,NUMPAG,2)

!     Explain keyfunctions in bottom window
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IYP+6+IH,IW,2)
      NUMWNB = InfoWindow(1)
      CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
      CALL IWinOutStringXY (1,1,'pages = PgUp/PgDn; scroll =   ; toggle keyword menu = Tab')
      CALL IWinOutStringXY (1,2,'top or bottom = Home/End; exit = Esc; search = F7')
!
!     Helpwindow is middelste window
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IYP+4,IW,IH)
      NUMWNH = InfoWindow(1)
!
!     Start with keyword WRDKEY
      CALL SEARCH(NAHEAD,NLEVEL,HLPTXT,NUMTXT,WRDKEY,NUMCHC,JOFND)
!
   20 CONTINUE
!
!
!     Display one page of help
      IF (JATAB .EQ. 0) THEN
         CALL IWinSelect(NUMWNH)
         CALL SCRLPG(HLPTXT,NUMTXT,NUMTOP,NUMCHC,IH)
      ELSE
         CALL IWinSelect(NUMWNK)
         CALL SCRLPG(KEYWRD,NUMKEY,NUMTOP,NUMCHC,IH)
      ENDIF
!
!     Display pagenumber in top window
      CALL IWinSelect(NUMWNT)
      CALL IWinOutIntegerXY(IW-9,1,1+NUMTOP/IH,2)
!
!     Indicate present keyword level with cursor position
      CALL ITextAttribute('BRU')
      IF (JATAB .EQ. 0) THEN
      CALL IWinSelect(NUMWNH)
      CALL IWinOutStringXY (NLEVEL,NUMCHC-NUMTOP+1,HLPTXT(NUMCHC)(NLEVEL:NLEVEL))
      ELSE
      CALL IWinSelect(NUMWNK)
      CALL IWinOutStringXY (NLEVEL,NUMCHC-NUMTOP+1,KEYWRD(NUMCHC)(NLEVEL:NLEVEL))
      ENDIF
      CALL ITextAttribute(' ')
!
!     Get instructions
      IF (JATAB .EQ. 0) THEN
         CALL SCROLH(NUMCHC,HLPTXT,NUMTXT,NLEVEL,IH,JOFND,JATAB)
      ELSE
         CALL SCROLH(NUMCHC,KEYWRD,NUMKEY,NLEVEL,IH,JOFND,JATAB)
      ENDIF
!
      IF (JOFND .EQ. -1) THEN
!        Search for keyword
         IXS    = NPOS(1)+46
         IYS    = NPOS(2)+IH+6
         CALL InStringXYDef(IXS,IYS,' => ',0,LOOKUP,LEN)
         IF (JATAB .EQ. 0) THEN
            CALL SEARC2(NAHEAD,NLEVEL,HLPTXT,NUMTXT,LOOKUP,NUMCHC,JOFND)
         ELSE
            CALL SEARC2(NAHEAD,NLEVEL,KEYWRD,NUMKEY,LOOKUP,NUMCHC,JOFND)
         ENDIF
         CALL IWinSelect(NUMWNB)
         CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
         CALL IWinOutStringXY (1,2,'top or bottom = Home/End; exit = Esc; search : F7)                               . ')
         IF (JATAB .EQ. 1) CALL ITEXTCOLOURN(WNDFOR,WNDBCK)
      ELSE IF (JATAB .EQ. 1) THEN
!        met tab wordt popup keyword window geopend of gesloten
         IF (JAPOP .EQ. 0) THEN
            CALL IWinSelect(NUMWNT)
            CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
            TEXLIN = '               '//trim(company)//'-'//trim(product_name)//' KEYWORDWINDOW'
            CALL IWinOutSTRINGXY(1,1,TEXLIN)
            CALL IWinOutStringXY (IW-16,1,'page =    of   ')
            CALL IWinOutIntegerXY(IW-3,1,NUMPGK,2)
            CALL ITEXTCOLOURN(WNDFOR,WNDBCK)
            CALL IWinAction('PC')
            CALL IWinOpen(IXP+40,IYP+4,IW-40,IH)
            NUMWNK = InfoWindow(1)
            JAPOP  = 1
            LINE   = NUMCHC - NUMTOP
            NUMCHC = NHTONK(NUMCHC)
            NUMTOP = MAX( 1,MIN( NUMCHC - LINE,NUMKEY - IH + 1) )
         ENDIF
      ELSE
         IF (JAPOP .EQ. 1) THEN
            CALL IWinSelect(NUMWNK)
            CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
            CALL IWinClose(1)
            JAPOP  = 0
            CALL IWinSelect(NUMWNT)
            CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
            TEXLIN = '               '//trim(company)//'-'//trim(product_name)//' HELPWINDOW'
            CALL IWinOutSTRINGXY(1,1,TEXLIN)
            CALL IWinOutStringXY (IW-16,1,'page =    of   ')
            CALL IWinOutIntegerXY(IW-3,1,NUMPAG,2)
            LINE   = NUMCHC - NUMTOP
            NUMCHC = NKTONH(NUMCHC)
            NUMTOP = NUMCHC - LINE
            CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
         ENDIF
      ENDIF

      IF (NUMCHC .NE. 0) GOTO 20

      IF (JAPOP .EQ. 1) THEN
         CALL IWinClose(1)
      ENDIF
      CALL IWinClose(1)
      CALL IWinClose(1)
      CALL IWinClose(1)
      CALL ITEXTCOLOURN(NFORG,NBACK)
      RETURN
      END
