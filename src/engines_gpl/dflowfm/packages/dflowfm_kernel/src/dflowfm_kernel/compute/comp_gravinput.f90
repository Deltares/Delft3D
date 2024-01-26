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

   !> compute Graviational Input from tidal forces and SAL
   subroutine comp_GravInput()
      use m_flowgeom
      use m_flow
      use unstruc_messages
      use m_partitioninfo
      implicit none

      double precision :: force, dfac

      integer          :: L, k, k1, k2
      logical          :: Ldoit1, Ldoit2

!     initialize
      GravInput = 0d0
      SALInput  = 0d0
      force     = 0d0

      SALinput2 = 0d0

!     reconstruct tidel force at cell centers and compute power at once
      do L=1,Lnx
         if ( tidef(L).ne.0d0 ) then
            k1 = ln(1,L)
            k2 = ln(2,L)

            if ( jaselfal.gt.0 ) then
   !           compute total unlimited tide force
               force = ( tidep(1,k2) - tidep(1,k1) )*dxi(L)

   !           compute limitation factor (see setextforcechkadvec)
               dfac = 1d0
               if ( abs(force).gt.0d0 ) then
                  dfac = tidef(L)/force
               end if

   !           compute limited SAL force
               force  = dfac * ( tidep(2,k2) - tidep(2,k1) )*dxi(L)
            end if

!           contribution from left cell
            Ldoit1 = .true.
            Ldoit2 = .true.
            if ( jampi.eq.1 ) then
               if ( idomain(k1).ne.my_rank ) Ldoit1 = .false.
               if ( idomain(k2).ne.my_rank ) Ldoit2 = .false.
            end if

            if ( Ldoit1 .and. k1.le.Ndxi ) then  ! boundary cells excluded
               dfac = rho(k1)*hs(k1)*ba(k1) * (ucx(k1)*wcx1(L) + ucy(k1)*wcy1(L))
               GravInput = GravInput + dfac * tidef(L)
               if ( jaselfal.gt.0 ) then
                  SALInput  = SALInput  + dfac * force
               end if
            end if

!           contribution from right cell
            if ( Ldoit2 .and. k2.le.Ndxi ) then
               dfac = rho(k2)*hs(k2)*ba(k2) * (ucx(k2)*wcx2(L) + ucy(k2)*wcy2(L))
               GravInput = GravInput + dfac * tidef(L)
               if ( jaselfal.gt.0 ) then
                  SALInput  = SALInput  + dfac * force
               end if
            end if
         end if
      end do


      if ( jaselfal.gt.0 ) then
         if ( jampi.eq.0 ) then
            do k=1,Ndxi
               SALinput2 = SALinput2 + tidep(2,k) * rho(k) * sq(k)
            end do
         else
            do k=1,Ndxi
               if ( idomain(k).ne.my_rank ) then
                  SALinput2 = SALinput2 + tidep(2,k) * rho(k) * sq(k)
               end if
            end do
         end if
      end if

      return
   end subroutine comp_GravInput
