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

module m_flgsfurufm

   implicit none

contains

   subroutine flgsfurufm(formno, m, teken, husb, hdsb, velhght, zs, ds, dg, dc, wstr,&
                     & cwfa, cwd, mugfa, cgfa, cgda, strdamf, lambda)
      use precision, only: dp
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------

      ! use m_GlobalParameters
      ! use cpluv

      use m_flowgeom, only: dx
      use m_flow
      use m_flowtimes
      use m_iterfurufm

      implicit none
!
! Local parameters
!
      real(kind=dp), parameter :: relax = 0.0d0, alfa = 0.9d0
!
! Global variables
!
      integer, intent(in) :: formno
      integer, intent(in) :: m
      real(kind=dp), intent(in) :: cgda
      real(kind=dp), intent(in) :: cgfa
      real(kind=dp), intent(in) :: cwd
      real(kind=dp), intent(in) :: cwfa
      real(kind=dp), intent(in) :: dc
      real(kind=dp), intent(in) :: dg
      real(kind=dp), intent(in) :: ds
      real(kind=dp), intent(in) :: hdsb
      real(kind=dp), intent(in) :: husb
      real(kind=dp), intent(in) :: mugfa
      real(kind=dp), intent(in) :: strdamf, lambda
      real(kind=dp), intent(in) :: teken
      real(kind=dp), intent(in) :: velhght
      real(kind=dp), intent(in) :: wstr
      real(kind=dp), intent(in) :: zs
!
!
! Local variables
!
      integer :: itgenstr
      logical :: again
      real(kind=dp) :: cu
      real(kind=dp) :: dh
      real(kind=dp) :: dxdt
      real(kind=dp) :: hs1
      real(kind=dp) :: mu
      real(kind=dp) :: rhsc
      real(kind=dp) :: ustru
      real(kind=dp) :: su
      real(kind=dp) :: sd

!
!
!! executable statements -------------------------------------------------------
!
      !
      !=======================================================================
      !                      Deltares
      !                One-Two Dimensional Modelling System
      !                           S O B E K
      !
      ! Subsystem:          Flow Module
      !
      ! Programmer:         J.Kuipers
      !
      ! Module:             FLGSFURU (FLow General Structure
      !                               calculate FU and RU)
      !
      ! Module description: The linearization coefficients FU and RU are
      !                     calculated for the general structure
      !
      !                     The stage of the flow was already determined.
      !
      !
      ! Parameters:
      ! NR NAME              IO DESCRIPTION
      ! 16 cgda              I  Contraction coefficient for drowned gate flow
      !                         (adapted)
      ! 15 cgfa              I  Contraction coefficient for gate flow
      !                         (adapted)
      ! 13 cwd               I  Contraction coefficient for drowned weir flow.
      ! 12 cwfa              I  Contraction coefficient for free weir flow.
      !                         (adapted)
      ! 10 dc                I  Critical water level (free gate flow)
      !  9 dg                I  Gate opening height.
      !  8 ds                I  Water level immediately downstream the gate.
      !  1 formno            I  Flow condition of general structure:
      !                         0 : closed or dry
      !                         1 : free weir flow
      !                         2 : drowned weir flow
      !                         3 : free gate flow
      !                         4 : drowned gate flow
      !  5 hdsb              I  Downstream water level.
      !  4 husb              I  Upstream water level.
      !  2 m                 I  Grid index of structure
      ! 14 mugfa             I  Vertical contraction coefficient for free gate
      !                         flow (adapted)
      !  3 teken             I  Flow direction (+1/-1).
      !  6 velhght           I  Velocity height
      ! 11 wstr              I  Width at centre of structure.
      !  7 zs                I  Bed level at centre of structure.
      !=======================================================================
      !     Include Pluvius data space
      !     Declaration of parameters:
      !     Declaration of local variables:
      !
      if (formno == 0) then
         ! closed or dry
         ! hu(m) = 0d0
         au(m) = 0d0; fu(m) = 0d0; ru(m) = 0d0
      else
         again = .true.
      end if
      !
      !     Calculate upstream energy level w.r.t sill
      !
      hs1 = husb + velhght - zs

      !
      itgenstr = 0
      dxdt = strdamf * dx(m) / dts
      do while (again)
         itgenstr = itgenstr + 1
         if (formno == 1) then
            !           free weir flow
            cu = cwfa**2 * ag / 1.5d0
            !TEM        WRITE (11,*) cu,cwfa
            au(m) = wstr * hs1 * 2.0d0 / 3.0d0
            ustru = cwfa * sqrt(ag * 2.0d0 / 3.0d0 * hs1)
            rhsc = cu * (hdsb + velhght - zs) * teken
         elseif (formno == 2) then
            !           drowned weir flow
            cu = cwd**2 * 2.0d0 * ag
            au(m) = wstr * ds
            dh = max(hs1 - ds, 0.d0)
            ustru = cwd * sqrt(ag * 2.0d0 * dh)
            rhsc = cu * (hdsb + velhght - (ds + zs)) * teken
         elseif (formno == 3) then
            !           free gate flow
            mu = mugfa * cgfa
            cu = mu**2 * 2.0d0 * ag
            au(m) = wstr * dg
            dh = max(hs1 - dc, 0.d0)
            ustru = mu * sqrt(ag * 2.0d0 * dh)
            rhsc = cu * (hdsb + velhght - (dc + zs)) * teken
         elseif (formno == 4) then
            !           drowned gate flow
            mu = mugfa * cgda
            cu = mu**2 * 2.0d0 * ag
            au(m) = wstr * dg
            dh = max(hs1 - ds, 0.d0)
            ustru = mu * sqrt(ag * 2.0d0 * dh)
            rhsc = cu * (hdsb + velhght - (ds + zs)) * teken
         else
         end if

         ! dads(m) = wstr

         if (teken > 0) then
            su = husb
            sd = hdsb
         else
            sd = husb
            su = hdsb
         end if
         again = iterfurufm(m, su, sd, ustru, cu, rhsc, dxdt, lambda) .and. itgenstr < 100
      end do
      q1(m) = au(m) * u1(m) !  this may be done without
   end subroutine flgsfurufm

end module m_flgsfurufm
