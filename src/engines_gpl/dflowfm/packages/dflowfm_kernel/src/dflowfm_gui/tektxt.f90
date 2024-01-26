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

      subroutine TEKTXT ()
      use m_wearelt
      implicit none
      integer :: ia
      integer, save :: ini = 0
      integer :: k
      integer :: maxtxt
      integer :: ntxt
!      ------------------------------------------------------------------
!     tekenen van de strings die in een file staan en ingelezen zijn met
!     REATXT
!     ------------------------------------------------------------------
      common /XYTEXT/    xtxt,ytxt,coltxt,symtxt,heitxt,ntxt
      common /TEXTSS/    xytexts
      parameter (maxtxt = 2000)
      double precision :: xtxt(maxtxt), ytxt(maxtxt),heitxt(maxtxt)
      integer    symtxt(maxtxt), coltxt(maxtxt)
      character  xytexts(maxtxt)*120

      if (ini .eq. 0) then
         ntxt = 0
         ini  = 1
      endif
      IF (NTXT .LE. 0) RETURN

!     call IGrSymbSet('calctek.smb')
!     call IGrCharSet('symbols.chr')
!     call IGrCharSize(3.0,3.0)
!     call IGrCharJustify ('C')

      do 10 k = 1,ntxt
         call SETCOL (coltxt(k))

!        call IGRMOVETO    ( xtxt(k),ytxt(k) )
!        call IGRCIRCLEREL ( rcir            )

         call IGrCharJustify ('C')
         call IGrCharSize (real(heitxt(k)),real(heitxt(k)))

         if (symtxt(k) .ne. 0) then
            call IGrSymbOut (real(xtxt(k)),real(ytxt(k)),symtxt(k))
         endif

         call IGrMoveTo    ( real(xtxt(k)+1.1*rcir),real(ytxt(k)) )
         ia = len_trim(xytexts(k))
         call IGrCharJustify ('L')
         call DRAWTEXT   ( real(xtxt(k)+1.1*rcir),real(ytxt(k)),xytexts(k)(1:ia))
   10 continue

      call IGrCharSize (0.5,0.5)

      return
      end
