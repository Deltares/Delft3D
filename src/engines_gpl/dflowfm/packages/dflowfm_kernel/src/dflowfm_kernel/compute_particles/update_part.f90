!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
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

!> update particles or add to summed fluxes
subroutine update_part()
   use m_particles
   use m_flowtimes
   use m_flowgeom, only: Lnx, wu, bl
   use m_flow
   use m_transport, only: numconst, constituents
   use m_missing
   implicit none

   integer                                     :: LL, Lb, Lt

   logical                     :: Lsurface

   double precision, parameter :: huni=1d0

   if ( japart.ne.1 ) return

   Lsurface = ( threeDtype.eq.1 )

   if ( Lsurface ) then
      do LL=1,Lnx
         call getLbotLtop(LL,Lb,Lt)
         qfreesurf(LL) = u1(Lt)*huni*wu(LL)
      end do
   end if

   if ( time0.ge.starttime ) then

      call add_particles_from_release_file(time0)
      if ( timestep.le.0d0 ) then   ! update particles every computational time step
         if ( .not.Lsurface ) then
            call update_particles(q1,s0,s1,dts)
         else
            call update_particles(qfreesurf,bl+huni,bl+huni,dts)
         end if
         timepart = time1

         if ( jatracer.eq.1 ) then
!           udpate particle concentration
            call comp_concentration(s1,numconst,part_iconst,constituents)
         end if
      else

!        check if timestep has been started
         if ( timelast.eq.DMISS ) then
!           start particle timestep
            timelast = time0
            timenext = time0+timestep
            sbegin = s0
            qpart = 0d0
         end if

!        sum fluxes of this computational time step
         if ( .not.Lsurface ) then
            call part_sumfluxes(q1,Dts)
         else
            call part_sumfluxes(qfreesurf,Dts)
         end if

         if ( time1.ge.timenext ) then
!           finish particle timestep
            qpart = qpart/(time1-timelast)
            if ( .not.Lsurface ) then
               call update_particles(qpart, sbegin, s1, time1-timelast)
            else
               call update_particles(qfreesurf, bl+huni, bl+huni, time1-timelast)
            end if
            timepart = time1

!           start new particle timestep
            timelast = time1
            timenext = time1 + timestep
            sbegin = s1
            qpart = 0d0

            if ( jatracer.eq.1 ) then
!              udpate particle concentration
               call comp_concentration(s1,numconst,part_iconst,constituents)
            end if

         end if
      end if
   end if

   return
end subroutine update_part
