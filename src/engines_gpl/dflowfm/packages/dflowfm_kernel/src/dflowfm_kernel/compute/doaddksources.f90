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

 subroutine doaddksources() ! add k sources
 use m_flow
 use m_flowtimes
 implicit none

 integer          :: n, k, kk, kk2
 double precision :: qsrck, dvoli, dtol = 1d-4

 do n  = 1,numsrc
    if (ksrc(2,n) == 0 .and. ksrc(5,n) == 0) cycle  ! due to initialisation

    if (arsrc(n) == 0) cycle
    kk    = ksrc(1,n)                   ! 2D pressure cell nr FROM
    kk2   = ksrc(4,n)                   ! 2D pressure cell nr TO
    qsrck = qsrc(n)

    if (kk > 0) then                    ! FROM Point
       k = ksrc(2,n) ; dvoli = 1d0/max(vol1(k),dtol)
       if (qsrck > 0) then              ! FROM k to k2
          turkinepsws(1,k) = turkinepsws(1,k) - dts*qsrck*dvoli*turkinepsws(1,k)
       else if  (qsrck  < 0) then       ! FROM k2 to k
          turkinepsws(1,k) = turkinepsws(1,k) - dts*qsrck*dvoli*0.5D0*(qsrck/arsrc(n))**2
       endif
    endif

    if (kk2 > 0) then                   ! TO Point
       k = ksrc(5,n) ; dvoli = 1d0/max(vol1(k),dtol)
       if (qsrck > 0) then
          turkinepsws(1,k) = turkinepsws(1,k) + dts*qsrck*dvoli*0.5D0*(qsrck/arsrc(n))**2
       else if  (qsrck  < 0) then
          turkinepsws(1,k) = turkinepsws(1,k) + dts*qsrck*dvoli*turkinepsws(1,k)
       endif
    endif

 enddo
 end subroutine doaddksources
