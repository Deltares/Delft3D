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

   SUBROUTINE COPYPOLYGONTOSAMPLES()
   USE M_SAMPLES
   USE M_POLYGON
   USE m_missing
   USE M_NETW, ONLY : UNIDX1D
   USE m_fixedweirs, ONLY : SILLHEIGHTMIN
   use geometry_module, only: dbdistance, normalout
   use m_sferic, only: jsferic, jasfer3D

   implicit none
   integer                    :: k, n, KD, KU, KUU, KKN, KK
   DOUBLE PRECISION           :: RX1, RY1, RX2, RY2, V, R, DDX, A, B, DL, DR, WIDL, WIDR

!  interpolate missing zpl values in polylines, if possible
   call interpolate_zpl_in_polylines()

   N = NS
   CALL INCREASESAM(NS+NPL)
   DO K  = 1, NPL-1
      KU = K + 1 ; KUU = MIN(NPL, K+2)
      IF (XPL(K) .NE. DMISS .AND. XPL(KU) .NE. DMISS) THEN

         if (jakol45 == 0) then
            N     = N + 1
            IF (N > NSMAX) THEN
                CALL INCREASESAM(2*N)
            ENDIF
            XS(N) = XPL(K)
            YS(N) = YPL(K)
            ZS(N) = ZPL(K)  ; IF (ZS(N) == DMISS) ZS(N) = 1D0
         endif

         IF (JAKOL45 > 0 .AND. ZPL(K) .NE. DMISS) THEN
             IF (.NOT. (XPL(K) == XPL(KU) .AND. YPL(K) == YPL(KU) ) ) THEN
                 call normalout(XPL(K), YPL(K), XPL(KU), YPL(KU), rx1, ry1, jsferic, jasfer3D, dmiss, dxymis)
                 RX2 = RX1 ; RY2 = RY1
                 IF (K > 1) THEN
                    IF (XPL(K-1) .NE. DMISS ) THEN
                       call normalout(XPL(K-1), YPL(K-1), XPL(K), YPL(K), rx2, ry2, jsferic, jasfer3D, dmiss, dxymis)
                       RX2 = 0.5D0*(RX1 + RX2)
                       RY2 = 0.5D0*(RY1 + RY2)
                    ENDIF
                 ENDIF

                 N     = N + 1
                 IF (N > NSMAX) THEN
                    CALL INCREASESAM(2*N)
                 ENDIF

                 WIDL = 0.1D0
                 WIDR = 0.1D0
                 IF (DZR(K) > Sillheightmin .AND. DZL(K) > Sillheightmin) THEN
                     WIDL = 2D0*DZL(K)
                     WIDR = 2D0*DZR(K)
                 ENDIF

                 XS(N) = XPL(K) - RX2*WIDL
                 YS(N) = YPL(K) - RY2*WIDL
                 ZS(N) = ZPL(K) - DZL(K)
                 N     = N + 1
                 XS(N) = XPL(K) + RX2*WIDR
                 YS(N) = YPL(K) + RY2*WIDR
                 ZS(N) = ZPL(K) - DZR(K)
             ENDIF
         ENDIF

         V = DBDISTANCE( XPL(K), YPL(K), XPL(KU), YPL(KU), jsferic, jasfer3D, dmiss)
         IF (V > 0D0 .AND. UNIDX1D > 0) THEN
             R = V/UNIDX1D
             IF ( R > 1D0) THEN
                 KKN = R + 1
                 DO KK = 1,KKN-1
                    A  = DBLE(KK)/DBLE(KKN) ; B = 1D0 - A

                    if (jakol45 == 0) then
                       N  = N+1
                       IF (N > NSMAX) THEN
                          CALL INCREASESAM(2*N)
                       ENDIF
                       XS(N) = B*XPL(K) + A*XPL(KU)
                       YS(N) = B*YPL(K) + A*YPL(KU)
                       ZS(N) = B*ZPL(K) + A*ZPL(KU)
                       IF (ZPL(K) == DMISS .OR. ZPL(KU) == DMISS) THEN
                           ZS(N) = 1D0
                       ENDIF
                    endif

                    IF (JAKOL45 > 0 .AND. ZPL(K) .NE. DMISS .AND. ZPL(KU) .NE. DMISS) THEN

                       WIDL   = 0.1D0
                       WIDR   = 0.1D0
                       DL     = B*DZL(K) + A*DZL(KU)
                       DR     = B*DZR(K) + A*DZR(KU)
                       IF (DL > Sillheightmin .AND. DR > Sillheightmin) THEN ! slope assumed
                          WIDL = 2D0*DL
                          WIDR = 2D0*DR
                       ENDIF

                       N     = N + 1
                       IF (N > NSMAX) THEN
                          CALL INCREASESAM(2*N)
                       ENDIF

                       XS(N) = B*XPL(K) + A*XPL(KU) - RX1*WIDL
                       YS(N) = B*YPL(K) + A*YPL(KU) - RY1*WIDL
                       ZS(N) = B*ZPL(K) + A*ZPL(KU)
                       ZS(N) = ZS(N) - DL

                       N     = N + 1
                       IF (N > NSMAX) THEN
                          CALL INCREASESAM(2*N)
                       ENDIF

                       XS(N) = B*XPL(K) + A*XPL(KU) + RX1*WIDR
                       YS(N) = B*YPL(K) + A*YPL(KU) + RY1*WIDR
                       ZS(N) = B*ZPL(K) + A*ZPL(KU)
                       ZS(N) = ZS(N) - DR
                    ENDIF

                 ENDDO
             ENDIF
         ENDIF
         IF (XPL(KUU) == DMISS .OR. KU == NPL) THEN

             if (jakol45 == 0) then
                N     = N + 1
                IF (N > NSMAX) THEN
                    CALL INCREASESAM(2*N)
                ENDIF
                XS(N) = XPL(KU)
                YS(N) = YPL(KU)
                ZS(N) = ZPL(KU) ; IF (ZS(N) == DMISS) ZS(N) = 1D0
             endif

             IF (JAKOL45 > 0 .AND. ZPL(KU) .NE. DMISS) THEN

                 WIDL   = 0.1D0
                 WIDR   = 0.1D0
                 DL     = DZL(KU)
                 DR     = DZR(KU)
                 IF (DL > Sillheightmin .AND. DR > Sillheightmin) THEN
                    WIDL = 2D0*DL
                    WIDR = 2D0*DR
                 ENDIF

                 N     = N + 1
                 XS(N) = XPL(KU) - RX1*WIDL
                 YS(N) = YPL(KU) - RY1*WIDL
                 ZS(N) = ZPL(KU) - DZL(KU)
                 N     = N + 1
                 XS(N) = XPL(KU) + RX1*WIDR
                 YS(N) = YPL(KU) + RY1*WIDR
                 ZS(N) = ZPL(KU) - DZR(KU)
             ENDIF

         ENDIF

      ENDIF
   ENDDO

   NS = N
   call delpol()
   END SUBROUTINE COPYPOLYGONTOSAMPLES
