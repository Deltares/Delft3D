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

   !> Make a coding of all net nodes for later use in net orthogonalisation,
   !! net coupling and 'poltoland' functionality.
   !! network_data::NB values: 1=INTERN, 2=RAND, 3=HOEK, 0/-1=DOET NIET MEE OF 1D
   SUBROUTINE MAKENETNODESCODING()

   use m_netw
   use m_sferic, only: jsferic, jasfer3D
   use m_missing, only : dxymis
   use geometry_module, only: dcosphi
   use gridoperations

   implicit none

   integer :: k
   integer :: k1
   integer :: k2
   integer :: L, LL


   IF (ALLOCATED (NB)) DEALLOCATE (NB)
   ALLOCATE (NB(NUMK)) ;  NB = 0


   DO L  = 1,NUML                         ! NODE BOUNDARY ADMINISTRATION
      K1 = KN(1,L) ; K2 = KN(2,L)
      if ( k1.lt.1 .or. k2.lt.1 ) cycle   ! SPvdP: safety
      IF (KN(3,L) == 2 .or. KN(3,L) == 0 ) THEN
         IF (NB(K1) .NE. -1 .AND. NB(K2) .NE. -1) THEN
            IF (LNN(L) == 0) THEN            ! LINK ZONDER BUURCELLEN
                NB(K1) = -1 ; NB(K2) = -1
            ELSE IF (LNN(L) == 1) THEN       ! LINK MET 1 BUURCEL
                NB(K1) = NB(K1) + 1
                NB(K2) = NB(K2) + 1
            ENDIF
         ENDIF
      else ! (kn(3,l) == 1 .or. kn(3,l) == 3 .or. kn(3,L) == 4) then ! 1D-links sowieso niet meenemen.
         nb(k1) = -1
         nb(k2) = -1
      ENDIF
   ENDDO

   ! INTERNE PUNTEN BLIJVEN OP NUL STAAN
   DO K = 1, NUMK
      IF (KC(K) == 1) THEN
         IF (NB(K) == 1 .OR. NB(K) == 2) THEN
            IF (NMK(K) == 2) THEN
               NB(K) =  3                ! HOEKPUNT 'bolle hoek'
            ELSE
               ! NMK(K) > 2: find the two edge links (and connected neighbour nodes K1 and K2)
               K1 = 0 ; K2 = 0
               do L=1,NMK(K)
                  LL = nod(K)%lin(L)
                  if (LNN(LL) == 1) then
                     if (K1 == 0) then
                        call othernode(K, LL, K1)
                     else
                        call othernode(K, LL, K2)
                     end if
                  end if
               end do
               if (K1 /= 0 .and. K2 /= 0) then
                  if (dcosphi(xk(k),yk(k),xk(k1),yk(k1), &
                              xk(k),yk(k),xk(k2),yk(k2), jsferic, jasfer3D, dxymis) > -CORNERCOS) then ! cos(90+15) = -.25
                     NB(K) =  3             ! HOEKPUNT 'holle hoek'
                  else
                     NB(K) =  2             ! RANDPUNT
                  end if
               else
                  NB(K) =  2                ! RANDPUNT
               end if
            ENDIF
         ELSE IF (NB(K) .gt. 2) THEN
               NB(K) =  3                ! 'HOEKPUNT' tussen disjuncte cellen.
         ELSE IF (NB(K) .NE. -1) THEN
             NB(K) = 1                   ! INTERN PUNT
         ENDIF
      ELSE
         NB(K) = 0                       ! DOET NIET MEE
      ENDIF
   ENDDO
   do k = 1,numk
!      if (kc(k) == 0) nb(k) = 0
      if ( nmk(k) .lt. 2 ) nb(k) = -1    ! hanging node
   enddo
   END SUBROUTINE MAKENETNODESCODING
