module m_fm_update

implicit none

contains

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

!> update particles or add to summed fluxes
subroutine update_part(itime)
   use m_fm_update_particles
   use m_fm_particles_in_grid
   use partmem
   use m_part_regular, only: npart
   use m_particles, laypart => kpart
   use m_part_times
   use m_part_geom, only: Lnx, bl
   use m_part_flow
   use m_part_transport, only: numconst, constituents
   use m_missing
   use timers
   use fileinfo  , lun=> lunit    ! logical unit numbers for files

   use part09fm_mod
   use part14fm_mod

   implicit none

   integer, intent(in)         :: itime

   integer                     :: LL, Lb, Lt

   logical                     :: Lsurface

   real, dimension(:), allocatable :: xpartr
   real, dimension(:), allocatable :: ypartr
   real, dimension(:), allocatable :: zpartr
   integer                         :: nopartorg

   double precision, parameter :: huni=1d0

   integer(4) ithndl              ! handle to time this subroutine
   data ithndl / 0 /
   if ( timon ) call timstrt( "update_part", ithndl )

   Lsurface = .false. !( threeDtype.eq.1 )

   if ( Lsurface ) then
      do LL=1,Lnx
         qfreesurf(LL) = huni ! u1(Lt)*huni*wu(LL) -> q from top layer!
      end do
   end if

   if ( time0.ge.starttime ) then

!     add releases defined via an xyz file

      call add_particles_from_release_file(time0)

!     add dye release (from input file directly)

      if ( nodye .gt. 0 ) then
         !
         ! AM: this needs to be improved
         !
         nopartorg = nopart
         call part09fm ( lun(2)   , itime    , nodye    , nwaste   , mwaste   ,    &
                         xwaste   , ywaste   , iwtime   , amassd   , aconc    ,    &
                         npart    , mpart    , xpart    , ypart    , zpart    ,    &
                         wpart    , laypart,  hpart     , iptime   , nopart   ,    &
                         radius   , nrowswaste,                                    &
                         xpolwaste           , ypolwaste           ,               &
                         ndprt    , nosubs   ,                           &
                         layt     , tcktot   , zmodel   , laytop   , laybot   ,    &
                         nplay    , kwaste   , nolayp   ,                          &
                         modtyp   , zwaste   , track    , nmdyer   , substi   ,    &
                         rhopart)
      endif

!     add continuous release (from input file directly)

      if ( nocont .gt. 0 ) then
         call part14fm ( itime    , idelt    , nodye    , nocont   , ictime   ,    &
                         ictmax   , nwaste   , mwaste   , xwaste   , ywaste   ,    &
                         zwaste   , aconc    , rem      , npart    , ndprt    ,    &
                         mpart    , xpart    , ypart    , zpart    , wpart    ,    &
                         laypart  , hpart    ,                                     &
                         iptime   , nopart   , pblay    , radius   , nrowswaste,   &
                         xpolwaste           , ypolwaste           ,               &
                         ftime    , tmassu   , nosubs   ,                          &
                         ncheck   , t0buoy   , modtyp   , abuoy    , t0cf     ,    &
                         acf      , lun(2)   , layt     , tcktot   ,    &
                         zmodel   , laytop   , laybot   , nplay    , kwaste   ,    &  !< kwaste -> laywaste
                         nolayp   , linear   , track    ,                          &
                         nmconr   , spart    , rhopart  , noconsp  , const)
      endif

      if ( timestep.le.0d0 ) then   ! update particles every computational time step
         if ( .not.Lsurface ) then
            call update_particles(q1,h0,h1,dts)
         else
            call update_particles(qfreesurf,bl+huni,bl+huni,dts)
         end if
         timepart = time1
      else

         !        check if timestep has been started
         if ( timelast.eq.DMISS ) then
            !           start particle timestep
            timelast = time0
            timenext = time0+timestep
            hbegin = h0
            qpart = 0d0
         end if


         if ( time1.ge.timenext ) then
            !           finish particle timestep
            qpart = qpart/(time1-timelast)
            if ( .not.Lsurface ) then
               call update_particles(q0, hbegin, h1, time1-timelast)
            else
               call update_particles(q0, bl+huni, bl+huni, time1-timelast)

            end if

            timepart = time1

            !           start new particle timestep
            timelast = time1
            timenext = time1 + timestep
            hbegin = h1
            qpart = 0d0
         end if
      end if
   end if

   if ( timon ) call timstop ( ithndl )
end subroutine update_part



end module m_fm_update
