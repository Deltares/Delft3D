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

   SUBROUTINE TEKSAM(MET)

      use unstruc_colors
      use m_missing, only: DMISS
      use unstruc_opengl, only: jaopengl
      use m_samples
      use unstruc_display
      use m_arcinfo

      implicit none
      double precision :: deltx, RC
      double precision :: delty
      double precision :: deltz
      double precision :: dscr
      double precision :: hrc
      integer :: i, KMOD
      integer :: jastart
      integer :: key
      integer :: mcs
      integer :: ncol
      integer :: ncs
      integer :: ndraw
      integer :: ns1, m, n
      double precision :: wpqr
      double precision :: x
      double precision :: xold
      double precision :: y
      double precision :: yold
      double precision :: z
      double precision :: zfac
      double precision :: zupw
      integer :: MET
!     TEKEN SAMPLES
      COMMON /PERSPX/ WPQR,DELTX,DELTY,DELTZ,ZFAC,DSCR,ZUPW
      COMMON /SAMPLESADM/  MCS,NCS,NS1
      double precision :: VS(4,4)
      logical inview

      IF (MET .EQ. 0) RETURN
    
      IF (MET .EQ. 4 .OR. MET .EQ. 5) CALL SETTEXTSIZE()
      RC      = 1.7d0*RCIR
      HRC     = RCIR/2
      KMOD    = MAX(1,NS/100)
      key     = 0
 
!     Fix for OpenGL rendering
      if ( jaopengl.eq.1 .and. MET.eq.1 ) then
         MET = 7
      end if
   
      if (met == 5) then
          CALL SETCOL(KLSAM)
      else
          call minmxsam()
      endif

      DO I = 1, NS 

         IF (MOD(I,KMOD) .EQ. 0) THEN
            CALL HALT2(KEY)
            IF (KEY .EQ. 1) RETURN
         ENDIF

         X = XS(I)
         Y = YS(I)
         Z = ZS(I)
  
         if ( Z.EQ.DMISS ) cycle ! SPvdP: structured sample data may comprise missing values

         call tek1sample (x,y,z,met,rc,hrc,i,i)
  
      ENDDO 

      CALL IGRFILLPATTERN(4,0,0)
      CALL IGRCHARDIRECTION('H')
      RETURN
      END SUBROUTINE TEKSAM

      SUBROUTINE TEKarc (MET)
      use m_arcinfo 
      use unstruc_display
      use m_missing, only: DMISS
      
      implicit none
      double precision :: hrc, rc, x, y, z
      integer          :: met, m, n, key
  
      IF (MET .EQ. 4 .OR. MET .EQ. 5) CALL SETTEXTSIZE()
      RC      = 1.7d0*RCIR
      HRC     = RCIR/2

      if (met == 5) then
          CALL SETCOL(KLSAM)
      else
          call minmxarc()
      endif

      do n= 1,nca

         CALL HALT2(KEY)
         IF (KEY .EQ. 1) RETURN
         do m = 1,mca
         
            z = d(m,n)
            if ( z == dmiss) cycle 
            x = x0 + dxa*(m-1)
            y = y0 + dya*(n-1)
            call tek1sample (x,y,z,met,rc,hrc,m,n)

         enddo
     enddo
     end SUBROUTINE TEKarc

  
     subroutine tek1sample(x,y,z,met,rc,hrc,m,n)
     use unstruc_colors
     
     use unstruc_opengl, only: jaopengl
     use unstruc_display
     use m_arcinfo

     double precision :: x,y,z,rc,hrc
     integer          :: met,m,n

     COMMON /DRAWTHIS/ ndraw(50)

     IF (INVIEW (X,Y) ) THEN
         IF (NDRAW(9) .EQ. 2) THEN
!            CALL VIEW(XS(I),YS(I),ZS(I),X0S,Y0S,VS,X,Y,ZC)
         ENDIF
         IF (MET .ne. 5) THEN
            CALL ISOCOL2(Z,NCOL)
         ENDIF
         IF (MET .EQ. 1 .OR. MET .EQ. 2) THEN
            IF (NDRAW(9) .EQ. 1) THEN
!
!               CALL MOVABS(X,Y)
!               CALL CIR(RCIR)
!!              CALL HTEXT(ZS(I),X,Y)

               call box(x-0.5d0*rcir,y-0.5d0*rcir,x+0.5d0*rcir,y+0.5d0*rcir)

               IF (MET .EQ. 2) THEN
                  CALL MOVABS(X,Y)
                  CALL IGRFILLPATTERN(0,0,0)
                  CALL SETCOL(1)
                  CALL CIR(RCIR)
                  CALL IGRFILLPATTERN(4,0,0)
               ENDIF

           ENDIF
         ELSE IF (MET .EQ. 3) THEN
            CALL PTABS(X,Y)
         ELSE IF (MET .EQ. 4 .OR. MET .EQ. 5) THEN
            CALL HTEXT(Z,X,Y)
         ELSE IF (MET .EQ. 6) THEN
            CALL MOVABS(X,Y)
            CALL CIR(RCIR)
            CALL HTEXT(Z,X+rcir,Y)
         ELSE IF (MET .EQ. 7) THEN
            CALL KREC5(X,Y,HRC,HRC)
         ELSE IF (MET .EQ. 8) THEN
            CALL HITEXT(m,X,Y)
         ENDIF
     ENDIF

     end subroutine tek1sample


