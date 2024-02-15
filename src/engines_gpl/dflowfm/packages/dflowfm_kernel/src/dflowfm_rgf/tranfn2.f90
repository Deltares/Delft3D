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

      SUBROUTINE TRANFN2(    X1,     X2,     X3,     X4,             &  ! WAS B
                             Y1,     Y2,     Y3,     Y4,             &
                            IMX, MX, NX,    XRH,  YRH)

      USE M_GRIDSETTINGS
      use m_orthosettings, only: ITIN
      implicit none
      double precision :: db
      double precision :: dl
      double precision :: don
      double precision :: dr
      double precision :: ds
      double precision :: dx
      double precision :: dx1
      double precision :: dx2
      double precision :: dx3
      double precision :: dx4
      double precision :: dy
      double precision :: dy1
      double precision :: dy2
      double precision :: dy3
      double precision :: dy4
      double precision :: f1
      double precision :: f2
      double precision :: f3
      double precision :: f4
      integer :: i
      integer :: ierr
      integer :: imx
      integer :: j
      integer :: k
      integer :: key
      integer :: mixelliptic
      integer :: mm
      integer :: mnx
      integer :: mx
      integer :: nn
      integer :: nx
      double precision :: rb
      double precision :: rl
      double precision :: ro
      double precision :: rr
      double precision :: w
      double precision :: w12
      double precision :: w34
      double precision :: wa
      double precision :: xh
      double precision :: xy
      double precision :: yh

      double precision , DIMENSION(:,:), ALLOCATABLE  :: X1V,Y1V,X2V,Y2V,     &
                                                          X3V,Y3V,X4V,Y4V,     &
                                                          SI ,SJ, W1, W2, W3, W4
      double precision , DIMENSION (:), ALLOCATABLE   :: D1 ,D2, D3, D4, TI, TJ

      double precision :: XRH(MX,NX),YRH(MX,NX),                    &
                          X1(IMX), X2(IMX), X3(IMX), X4(IMX),       &
                          Y1(IMX), Y2(IMX), Y3(IMX), Y4(IMX)
      DOUBLE PRECISION :: DDX, DDY, RI, RJ, T1, T2, T3, T4
!      double precision , DIMENSION(:,:), ALLOCATABLE  :: X1V,Y1V,X2V,Y2V,     &
!                                                          X3V,Y3V,X4V,Y4V,     &
!                                                          SI ,SJ, W1, W2, W3, W4
!      double precision , DIMENSION (:), ALLOCATABLE   :: D1 ,D2, D3, D4, TI, TJ
!
!      double precision :: XRH(MX,NX),YRH(MX,NX),                    &
!                          X1(IMX), X2(IMX), X3(IMX), X4(IMX),       &
!                          Y1(IMX), Y2(IMX), Y3(IMX), Y4(IMX)
!      DOUBLE PRECISION :: DDX, DDY, RI, RJ, T1, T2, T3, T4


       CHARACTER TEX*4
!     1,2    VERTICALEN
!     3,4    HORIZONTALEN
!     D1234  REL. LIJN COORDINAAT 0-1
!     SI,SJ  REL. VELD COORDINAAT 0-1
!     TI,TJ  SCHATTING KOORDELENGTES
!     W1234  VELD WEEGFACTOR
!     X1V    SCHATTING VELDCOORDINAAT VANUIT VERTICAAL 1

       NN  = NFAC + 1
       MM  = MFAC + 1
       MNX  = MAX(MM, NN)
       ALLOCATE(X1V(MM,NN),STAT=IERR)
       ALLOCATE(Y1V(MM,NN),STAT=IERR)
       ALLOCATE(X2V(MM,NN),STAT=IERR)
       ALLOCATE(Y2V(MM,NN),STAT=IERR)
       ALLOCATE(X3V(MM,NN),STAT=IERR)
       ALLOCATE(Y3V(MM,NN),STAT=IERR)
       ALLOCATE(X4V(MM,NN),STAT=IERR)
       ALLOCATE(Y4V(MM,NN),STAT=IERR)
       ALLOCATE(si (MM,NN),STAT=IERR)
       ALLOCATE(sj (MM,NN),STAT=IERR)
       ALLOCATE(W1 (MM,NN),STAT=IERR)
       ALLOCATE(W2 (MM,NN),STAT=IERR)
       ALLOCATE(W3 (MM,NN),STAT=IERR)
       ALLOCATE(W4 (MM,NN),STAT=IERR)

       ALLOCATE(D1 (MNX),STAT=IERR)
       ALLOCATE(D2 (MNX),STAT=IERR)
       ALLOCATE(D3 (MNX),STAT=IERR)
       ALLOCATE(D4 (MNX),STAT=IERR)
       ALLOCATE(TI (MNX),STAT=IERR)
       ALLOCATE(TJ (MNX),STAT=IERR)



       CALL ABREL2(X1,Y1,D1,NN,T1)
       CALL ABREL2(X2,Y2,D2,NN,T2)
       CALL ABREL2(X3,Y3,D3,MM,T3)
       CALL ABREL2(X4,Y4,D4,MM,T4)

       DO I = 1,MM
          DO J = 1,NN
             RI   = DBLE(I-1)/DBLE(MFAC)                       ! INDEXWEGING I
             RJ   = DBLE(J-1)/DBLE(NFAC)                       ! INDEXWEGING J

             SI(I,J) = (1D0-RJ)*D3(I) + RJ*D4(I)               ! AFSTANDSWEGING I
             SJ(I,J) = (1D0-RI)*D1(J) + RI*D2(J)               ! AFSTANDSWEGING J

          ENDDO
       ENDDO



       DO I = 1,MM
          DO J  = 1,NN
             W1(I,J) = (1D0-SJ(I,J))*T3 + SJ(I,J)*T4          ! AFSTANDSGEWOGEN TOTALE KOORDELENGTE I-RICHTING
             W2(I,J) = (1D0-SI(I,J))*T1 + SI(I,J)*T2          ! AFSTANDSGEWOGEN TOTALE KOORDELENGTE J-RICHTING
             W3(I,J) = W2(I,J) / W1(I,J)                      ! ATPF
             W4(I,J) = W1(I,J) / W2(I,J)
             WA      = 1D0 / ( W3(I,J) + W4(I,J) )
             W1(I,J) = WA*W3(I,J)
             W2(I,J) = WA*W4(I,J)
          ENDDO
       ENDDO

       DO I = 1,MM                                             ! randen
         XRH(I,1)  = X3(I)
         XRH(I,NN) = X4(I)
         YRH(I,1)  = Y3(I)
         YRH(I,NN) = Y4(I)
       ENDDO

       DO J = 1,NN
         XRH(1,J)  = X1(J)
         XRH(MM,J) = X2(J)
         YRH(1,J)  = Y1(J)
         YRH(MM,J) = Y2(J)
       ENDDO

       DO I = 2,MM-1                                           ! BINNENGEBIED 1E SCHATTING MET RANDPUNTEN
          DO J = 2,NN-1

             XRH(I,J) = ( (1D0-SI(I,J))*X1(J) + SI(I,J)*X2(J) ) * W1(I,J) +   &
                        ( (1D0-SJ(I,J))*X3(I) + SJ(I,J)*X4(I) ) * W2(I,J)
             YRH(I,J) = ( (1D0-SI(I,J))*Y1(J) + SI(I,J)*Y2(J) ) * W1(I,J) +   &
                        ( (1D0-SJ(I,J))*Y3(I) + SJ(I,J)*Y4(I) ) * W2(I,J)
          ENDDO
       ENDDO

       ! CALL TEKGRD(XRH,YRH,MM,NN,1,1,MM,NN,31,2,KEY,MM)
       ! CALL WAITESC()


       DO I = 1,MM                                             ! EVEN TERUGGEZET
          DO J  = 1,NN
             W1(I,J) = (1D0-SJ(I,J))*D3(I)*T3 + SJ(I,J)*D4(I)*T4     ! AFSTANDSGEWOGEN KOORDELENGTE I-RICHTING
             W2(I,J) = (1D0-SI(I,J))*D1(J)*T1 + SI(I,J)*D2(J)*T2     ! AFSTANDSGEWOGEN KOORDELENGTE J-RICHTING
          ENDDO
       ENDDO

       DO I = 2,MM                                             ! DI TUSSEN NETNODES
          DO J  = 1,NN
             W3(I,J) = W1(I,J) - W1(I-1,J)
          ENDDO
       ENDDO

       DO I = 1,MM                                             ! DJ TUSSEN NETNODES
          DO J  = 2,NN
             W4(I,J) = W2(I,J) - W2(I,J-1)
          ENDDO
       ENDDO

       DO I = 2,MM                                             ! atpI over cellen
          DO J  = 2,NN-1
             W1(I,J) = 0.25D0*( W4(I,J) + W4(I,J+1) + W4(I-1,J) + W4(I-1,J+1) ) / W3(I,J)
          ENDDO
       ENDDO


       DO I = 2,MM-1                                           ! atpJ over cellen
          DO J  = 2,NN
             W2(I,J) = 0.25D0*( W3(I,J) + W3(I,J-1) + W3(I+1,J) + W3(I+1,J-1) ) / W4(I,J)
          ENDDO
       ENDDO





!        W1 = W3; W2 = W4

       DO K  = 1, ITIN

          W3 = XRH
          W4 = YRH
          DO I = 2,MM-1                                        ! BINNEN
             DO J = 2,NN-1

                WA       = 1D0 / ( W1(I,J) + W1(I+1,J) + W2(I,J) +  W2(I  ,J+1) )

                XRH(I,J) = WA*   ( W3(I-1,J  )*W1(I,J) +  W3(I+1,J  )*W1(I+1,J  ) +  &
                                   W3(I  ,J-1)*W2(I,J) +  W3(I  ,J+1)*W2(I  ,J+1) )

                YRH(I,J) = WA*   ( W4(I-1,J  )*W1(I,J) +  W4(I+1,J  )*W1(I+1,J  ) +  &
                                   W4(I  ,J-1)*W2(I,J) +  W4(I  ,J+1)*W2(I  ,J+1) )

             ENDDO
          ENDDO

          ! hier nog een bnd lus zeker weten goed!

          ! CALL TEKGRD(XRH,YRH,MM,NN,1,1,MM,NN,31,2,KEY,MM)
          ! CALL WAITESC()

       ENDDO


       DEALLOCATE(X1V,Y1V,X2V,Y2V,X3V,Y3V,X4V,Y4V,si,sj,W1,W2,W3,W4)

       DEALLOCATE ( D1, D2, D3, D4, TI, TJ )

       RETURN
       END subroutine tranfn2
