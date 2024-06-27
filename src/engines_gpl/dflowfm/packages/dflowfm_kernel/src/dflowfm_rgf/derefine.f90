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

!***************7***  INTERPOLATION ************************************
      SUBROUTINE DEREFINE(M1, N1, M2, N2,NUM)
      use m_grid
      use m_gridsettings
      use unstruc_messages
      use m_missing
      implicit none

      integer :: m1, n1, m2, n2, num

      integer :: I, J, IR,INOW,JR,JNOW,MFA,NFA,MFAA,NFAA,MD,ND
      double precision, allocatable :: XR(:,:), YR(:,:)
      allocate(xr(mmax, nmax), yr(mmax, nmax))

      IF (MFAC .GE. MC) THEN
         CALL QNERROR('M-refinement factor larger than grid M-dimension' ,' ',' ')
         NUM = 0
         RETURN
      ENDIF
      IF (NFAC .GE. NC) THEN
         CALL QNERROR('N-refinement factor larger than grid M-dimension' ,' ',' ')
         NUM = 0
         RETURN
      ENDIF
      CALL SAVEgrd()

      call mess(LEVEL_DEBUG, 'DEREFINE BY: ', MFAC, NFAC)
      CALL READYY('DEREFINE',0d0)

      XR = dmiss
      YR = dmiss

      MD   = M2 - M1
      ND   = N2 - N1
      MFAA = MFAC
      NFAA = NFAC
      IF (MD .EQ. 0) MFAA = 1
      IF (ND .EQ. 0) NFAA = 1

      IR   = 1
      INOW = 1
      do I = 1,MC
         IF (INOW .GE. M1 .AND. INOW .LT. M2) THEN
            MFA = MFAA
         ELSE
            MFA = 1
         ENDIF
         JR   = 1
         JNOW = 1
         IF (INOW .LE. MC) THEN
            do j = 1,NC
               IF (JNOW .GE. N1 .AND. JNOW .LT. N2) THEN
                  NFA = NFAA
               ELSE
                  NFA = 1
               ENDIF
               IF (JNOW .LE. NC) THEN
                  XR(IR,JR) = Xc(INOW,JNOW)
                  YR(IR,JR) = Yc(INOW,JNOW)
                  JR   = JR + 1
                  JNOW = JNOW + NFA
               ENDIF
            end do
            IR   = IR + 1
            INOW = INOW + MFA
         ENDIF
end do

      CALL PUTARR(XR,Xc,MMAX,NMAX)
      CALL PUTARR(YR,Yc,MMAX,NMAX)
      CALL NUMS(Xc,mmax, nmax, MC,NC)
!     MC = INOW
!     NC = JNOW

      CALL READYY('DEREFINE',1d0)
      CALL READYY('DEREFINE',-1d0)
      deallocate(XR, YR)
      RETURN
      END subroutine derefine
