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

!> compute vertical fluxes
subroutine comp_fluxver(NUMCONST, limtyp, thetavert, Ndkx, kmx, zws, qw, kbot, ktop, sed, nsubsteps, jaupdate, ndeltasteps, flux, wsf)
   use m_flowgeom, only: Ndx, ba, kfs  ! static mesh information
   use m_flowtimes, only: dts
   use m_flowparameters, only: cflmx
   use m_flow, only : hs, s1, epshsdif, cffacver, a1, jaimplicitfallvelocity  ! do not use m_flow, please put this in the argument list
   use m_transport, only : ISED1, ISEDN   ! preferably in argument list
   use m_sediment,  only: mtd
   use unstruc_messages
   use m_sediment, only : jased, ws, sedtra, stmpar, stm_included
   use sediment_basics_module
   use timers

   implicit none

   integer,                                    intent(in)    :: NUMCONST     !< number of transported quantities
   integer,                                    intent(in)    :: limtyp       !< limiter type
   double precision, dimension(NUMCONST),      intent(in)    :: thetavert    !< compute fluxes (<1) or not (1)
   integer,                                    intent(in)    :: Ndkx         !< total number of flownodes (dynamically changing)
   integer,                                    intent(in)    :: kmx          !< maximum number of layers (dynamically changing)
   double precision, dimension(Ndkx),          intent(in)    :: zws          !< vertical coordinate of layers at interface/center locations
   double precision, dimension(Ndkx),          intent(in)    :: qw           !< flow-field vertical discharges
   integer,          dimension(Ndx),           intent(in)    :: kbot         !< flow-node based layer administration
   integer,          dimension(Ndx),           intent(in)    :: ktop         !< flow-node based layer administration
   double precision, dimension(NUMCONST,Ndkx), intent(in)    :: sed          !< transported quantities
   integer,                                    intent(in)    :: nsubsteps    !< number of substeps
   integer,          dimension(Ndx),           intent(in)    :: jaupdate     !< update cell (1) or not (0)
   integer,          dimension(Ndx),           intent(in)    :: ndeltasteps  !< number of substeps between updates
   double precision, dimension(NUMCONST,Ndkx), intent(inout) :: flux         !< adds vertical advection fluxes
   double precision, dimension(NUMCONST),      intent(in   ) :: wsf          !< vertical fall velocities


   double precision, dimension(2049)                         :: dz

   double precision                                          :: sedL, sedR, ds1L, ds2L, ds1R, ds2R, sl3L, sl3R, cf, dum
   double precision                                          :: dt_loc
   double precision                                          :: qw_loc

   integer                                                   :: kk, k, kb, kt, kL, kR, kLL, kRR
   integer                                                   :: j, ll

   double precision                                          :: dlimiter

   double precision, parameter                               :: DTOL = 1d-8

   integer(4) ithndl /0/
   if (timon) call timstrt ( "comp_fluxver", ithndl )

   if ( sum(1d0-thetavert(1:NUMCONST)).lt.DTOL ) goto 1234 ! nothing to do

   !if ( limtyp.eq.6 ) then
   !   call message(LEVEL_ERROR, 'transport/comp_fluxver: limtyp==6 not supported')
   !end if

   dt_loc = dts

   !$xOMP PARALLEL DO                                                                       &
   !$xOMP PRIVATE(kk,kb,kt,dz,k,cf,kL,kR,j,sedL,sedR,kLL,kRR,sl3L,sl3R,ds1L,ds1R,ds2L,ds2R,qw_loc) &
   !$xOMP FIRSTPRIVATE(dt_loc)
   do kk=1,Ndx
      if (kfs(kk) <= 0) cycle

      if ( nsubsteps.gt.1 ) then
         if (jaupdate(kk).eq.0 ) then
            cycle
         else
            dt_loc = dts * ndeltasteps(kk)
         end if
      else
         dt_loc = dts
      end if

      kb = kbot(kk)
      kt = ktop(kk)
      dz(1)         = max(dtol, zws(kb)-zws(kb-1) )
      ! dz(2:kt-kb+1) = max(dtol, 0.5d0*(zws(kb+1:kt)-zws(kb-1:kt-1))  ) ! thickness between cell centers org
      dz(2:kt-kb+1) = max(dtol, 0.5d0*(zws(kb+1:kt)-zws(kb-1:kt-2))  ) ! thickness between cell centers

      dz(kt-kb+2)   = max(dtol, zws(kt)-zws(kt-1) )

      ! dz = max(dz,dtol)  !     fix for zero-thickness layer

       do k=kb,kt-1
         ! cf = dt_loc*qw(k)/(ba(kk)*dz(k-kb+2))

         kL = k   ! max(k,kb)
         kR = k+1 ! min(k+1,kt)

         do j=1,NUMCONST
            qw_loc = qw(k)
            if (jaimplicitfallvelocity == 0) then  ! explicit
               if (jased < 4) then
                  qw_loc = qw(k) - wsf(j)*ba(kk)
               elseif ( stm_included .and. j.ge.ISED1 .and. j.le.ISEDN ) then
                  ll = j-ISED1+1
                  if (k<sedtra%kmxsed(kk,ll)) then
                     qw_loc = qw(k)     ! settling flux zero below kmxsed layer
                  else
                     qw_loc = qw(k) - mtd%ws(k,ll)*ba(kk)
                  endif
               else
                  qw_loc = qw(k) - wsf(j)*ba(kk)   ! enable tracers with settling vel icw morphology
               endif
            endif
                         

            if (cffacver > 0d0) then
               cf = cffacver*dt_loc*abs(qw_loc)/(ba(kk)*dz(k-kb+2)) ! courant nr
               cf = max(0d0,1d0-cf)                                 ! use high order only for small courant
            else
               cf = 1d0                                             ! or always use it, is MUSCL = default
            endif

            if ( thetavert(j).eq.1d0 ) cycle

            sedL = sed(j,kL)
            sedR = sed(j,kR)

            if ( thetavert(j).gt.0d0 ) then ! semi-explicit, use central scheme
               flux(j,k) = flux(j,k) + qw_loc*0.5d0*(sedL+sedR)
            else     ! fully explicit
             !  if (limtyp.ne.0  ) then
                  if ( k.gt.kb-1 .and. qw_loc.gt.0d0 ) then
                     kLL  = max(k-1,kb)
                     sL3L = dz(k-kb+2)/dz(k-kb+1)

                     ds2L =  sedR-sedL
                     ds1L = (sedL-sed(j,kLL))*sl3L
                     sedL = sedL + 0.5d0 * cf * dlimiter(ds1L,ds2L,limtyp) * ds2L
                  end if

                  if ( k.lt.kt .and. qw_loc.lt.0d0 .and. s1(kk)-zws(kb-1) > epshsdif ) then
                     kRR = min(k+2,kt)
                     sL3R = dz(k-kb+2)/dz(k-kb+3)

                     ds2R =  sedL-sedR
                     ds1R = (sedR-sed(j,kRR))*sl3R
                     sedR = sedR + 0.5d0 * cf * dlimiter(ds1R,ds2R,limtyp) * ds2R
                  end if
              !  end if

               flux(j,k) = flux(j,k) + max(qw_loc,0d0)*sedL + min(qw_loc,0d0)*sedR
            end if
         end do
      end do
   end do

   !$xOMP END PARALLEL DO

1234 continue

   if (timon) call timstop( ithndl )
   return
end subroutine comp_fluxver
