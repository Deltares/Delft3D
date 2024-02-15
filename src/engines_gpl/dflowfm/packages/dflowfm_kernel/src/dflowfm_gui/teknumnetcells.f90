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

      subroutine TEKnumnetcells(jatel)
      use m_grid
      use m_netw
      use m_polygon
      use m_missing
      use unstruc_display
      use geometry_module, only: dbpinpol
      use gridoperations

      implicit none
      integer          :: i,j,n,ncol,jatel,in,k,im,jm,mxnum
      double precision :: xx(4),yy(4),x,y,z

      double precision :: vmax, vmin, dv, val
      integer          :: ncols, nv, nis, nie, jaauto
      COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO
      logical inview


      call savepol()

      im = 0
      jm = 0
      mxnum = 0
      if (jatel ==1) then
          if (nump == 0) call findcells(0)
          ijyes = 0
      else
          vmax = -9d9
          vmin = -vmax
          do j = 1,nc-1
             do i = 1,mc-1
                if (ijyes(i,j) > 0) then
                   x  = 0.25d0* (xc(i,j) + xc(i+1,j) + xc(i+1,j+1) + xc(i,j+1) )
                   y  = 0.25d0* (yc(i,j) + yc(i+1,j) + yc(i+1,j+1) + yc(i,j+1) )
                   if (inview(x,y) ) then
                      if (ijyes(i,j) > mxnum) then
                         im = i
                         jm = j
                         mxnum = ijyes(i,j)
                      endif
                      z = ijyes(i,j)
                      vmax = max(z,vmax)
                      vmin = min(z,vmin)
                   endif
                endif
             enddo
          enddo
          DV   = VMAX - VMIN
          DO I = 1,NV
             VAL(I) = VMIN + (I-1)*DV/(NV-1)
          ENDDO
      endif


      do j = 1,nc-1
         do i = 1,mc-1
            n = 0
            if (xc(i,j)   .ne. dmiss .and. xc(i+1,j)   .ne. dmiss .and. &
                xc(i,j+1) .ne. dmiss .and. xc(i+1,j+1) .ne. dmiss ) then
                n = n + 1
                xpl(n) = xc(i,j  )
                ypl(n) = yc(i,j)
                n = n + 1
                xpl(n) = xc(i+1,j)
                ypl(n) = yc(i+1,j)
                n = n + 1
                xpl(n) = xc(i+1,j+1)
                ypl(n) = yc(i+1,j+1)
                n = n + 1
                xpl(n) = xc(i,j+1)
                ypl(n) = yc(i,j+1)
                npl = 4
                if (jatel == 1) then
                   in = -1
                   do k = 1,nump
                      call dbpinpol( xzw(k), yzw(k), in,  dmiss, JINS, NPL, xpl, ypl, zpl)
                      ijyes(i,j) = ijyes(i,j) + in
                   enddo
                else
                   z = ijyes(i,j)
                   x = (xpl(1)+xpl(2)+xpl(3)+xpl(4))/4
                   y = (ypl(1)+ypl(2)+ypl(3)+ypl(4))/4
                   call kcir(x,y,z)
                endif
            endif
         enddo
      enddo

      call restorepol()

      if (im > 0 ) then
         i  = im
         j = jm
         z  = ijyes(i,j)
         x  = 0.25d0* (xc(i,j) + xc(i+1,j) + xc(i+1,j+1) + xc(i,j+1) )
         y  = 0.25d0* (yc(i,j) + yc(i+1,j) + yc(i+1,j+1) + yc(i,j+1) )
         CALL SETTEXTSIZEfac(2d0)
         call htext(z, x, y)
         CALL SETTEXTSIZE()
      endif

      end subroutine TEKnumnetcells
