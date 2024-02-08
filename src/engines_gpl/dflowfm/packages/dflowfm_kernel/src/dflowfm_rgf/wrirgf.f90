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

      !> Write a curvilinear grid to (ascii) grd-file.
      !! NOTE: 'new' format (RGFGrid 4.x)
      !!
      !! Format:
      !! Start with at least one comment line, prefixed by '*', with optional keyword Spherical (old RGFGRID 3.x style)
      !! Next, zero or more key=value lines (all optional):
      !!   Coordinate System=Spherical
      !!   Missing Value=...
      !! First subsequent line without a '=' should be '0 0 0' (backwards compatibility)
      !! Next line should be mmax, nmax
      !! That ends the header, start reading coordinates in the usual fashion.
      SUBROUTINE WRIRGF(MRGF,FILNAM)
      use m_sferic
      use m_grid
      use m_missing
      use m_arcinfo
      use m_polygon

      implicit none
      double precision :: half
      integer :: ipnt, n, i,j, nfirst
      integer :: mrgf, mdep



      CHARACTER NAME2*76, FILNAM*(*)

      IPNT  = INDEX(FILNAM,'.')
      NAME2 = FILNAM
      WRITE(NAME2(IPNT+1:),'(A)') 'enc'

      CALL FIRSTLIN(MRGF)
      IF (JSFERIC .EQ. 1) THEN
         WRITE(MRGF,'(A)') 'Coordinate System=Spherical'
      ENDIF
      WRITE(MRGF,'(A,F14.3)') 'Missing Value=',XYMIS

      WRITE(MRGF,'(2I8)') MC,NC
      WRITE(MRGF,'(3I8)') 0, 0, 0 ! Backwards compatibility
     ! CALL CORRGF(Xc,Yc,MMAX,NMAX)
     ! CALL ISITU()
     ! CALL WRIENC(NAME2, Xc, MC, NC, IJC, IJYES,mmax,nmax)
      HALF = 0
      CALL READYY('Writing Grid File',HALF)
      CALL ECRTAB(Xc,MC,NC,MRGF,HALF,mmax,nmax)
      HALF = 0.5d0
      CALL ECRTAB(Yc,MC,NC,MRGF,HALF,mmax,nmax)

      CALL READYY(' ',-1d0)
      CALL DOCLOSE (MRGF)

      WRITE(NAME2(IPNT+1:),'(A)') 'asc'
      call newfil(MDEP,NAME2)
      call wriarc(MDEP,ZC,MMAX,NMAX,MC,NC,X0,Y0,DXA,DYA,DMISS)
      call doclose(MDEP)

      WRITE(NAME2(IPNT+1:),'(A)') 'dep'
      call newfil(MDEP,NAME2)
      call  WRIDEP(MDEP,Zc,1,1,mmax,nmax,mmax,nmax)
      call doclose(MDEP)

      if (mc*nc < -1000) then ! save grd to polygon for partitioning
         call savepol()
         n = 0 ; nfirst = 0
         do i = 1, mc-1
            do j = 1,nc-1
               if (xc(i,j)   .ne. dmiss .and. xc(i+1,j)   .ne. dmiss .and. &
                   xc(i,j+1) .ne. dmiss .and. xc(i+1,j+1) .ne. dmiss ) then
                   if (nfirst .ne. 0) then
                      n = n + 1 ; xpl(n) = xc(i,j  )   ; ypl(n) = yc(i,j)
                      n = n + 1 ; xpl(n) = xc(i+1,j)   ; ypl(n) = yc(i+1,j)
                      n = n + 1 ; xpl(n) = xc(i+1,j+1) ; ypl(n) = yc(i+1,j+1)
                      n = n + 1 ; xpl(n) = xc(i,j+1)   ; ypl(n) = yc(i,j+1)
                      n = n + 1 ; xpl(n) = dmiss       ; ypl(n) = dmiss
                   else
                      nfirst = 1
                   endif
                endif
            enddo
         enddo
         npl = n
         WRITE(NAME2(IPNT:),'(A)') '_part.pol'
         call newfil(MDEP,NAME2)
         call wripol(mdep)
         call restorepol()
      endif

      END SUBROUTINE WRIRGF
