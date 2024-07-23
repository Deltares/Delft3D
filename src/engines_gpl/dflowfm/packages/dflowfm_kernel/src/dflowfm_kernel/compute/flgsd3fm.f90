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

subroutine flgsd3fm(wsd, wstr, zs, w2, zb2, ds1, ds2, elu, hd, rhoast, cwd,   &
                & ds, lambda)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   implicit none
!
! Local parameters
!
   double precision, parameter :: c23 = 2.0d0 / 3.0d0, c13 = 1.0d0 / 3.0d0
!
! Global variables
!
   double precision, intent(in) :: cwd
   double precision, intent(out) :: ds
   double precision, intent(in) :: ds1
   double precision, intent(in) :: ds2
   double precision, intent(in) :: elu
   double precision, intent(in) :: hd
   double precision, intent(in) :: lambda
   double precision, intent(in) :: rhoast
   double precision, intent(in) :: w2
   double precision, intent(in) :: wsd
   double precision, intent(in) :: wstr
   double precision, intent(in) :: zb2
   double precision, intent(in) :: zs
!
!
! Local variables
!
   double precision :: aw
   double precision :: bw
   double precision :: cw
   double precision :: d2
   double precision :: fac
   double precision :: h2a
   double precision :: h2b
   double precision :: h2c
   double precision :: hsl
   double precision :: hulp
   double precision :: hulp1
   double precision :: p
   double precision :: phi
   double precision :: q
   double precision :: r60
   double precision :: term
   double precision :: u
   double precision :: v
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
   ! Programmer:         J.Brouwer/J.Kuipers
   !
   ! Module:             FLGSD3 (FLow Gen. Struct. Depth sill 3rd ord. eq.)
   !
   ! Module description: Compute water depth ds at the sill by solving a
   !                     third order algebraic equation.
   !
   !                     In case of drowned weir flow the water level at
   !                     the sill is required. The water depth is calcu-
   !                     lated in this routine.
   !
   !
   ! Parameters:
   ! NR NAME              IO DESCRIPTION
   ! 11 cwd               I  Correction coefficient for drowned weir flow.
   !  6 ds1               I  Delta s1 general structure.
   !  7 ds2               I  Delta s2 general structure.
   ! 12 ds                IO Water level immediately downstream the gate.
   !  8 elu               I  Upstream energy level.
   !  9 hd                I  Downstream water level.
   ! 13 lambda            I  Extra resistance in general structure.
   ! 10 rhoast            I  Downstream water density divided by upstream
   !                         water density.
   !  4 w2                I  Width at right side of structure.
   !  1 wsd               I  Width structure right or left side.
   !  2 wstr              I  Width at centre of structure.
   !  5 zb2               I  Bed level at right side of structure.
   !  3 zs                I  Bed level at centre of structure.
   !=======================================================================
   !
   !     Declaration of parameters:
   !
   !
   !     Declaration of local variables:
   !
   !
   !JK   LOGICAL uitput
   !JK   COMMON /UITPUT/uitput
   !
   !     Calculate Dw (=term), Aw, Bw and Cw according to appendix C of
   !     the design document River Rural integratietraject deel 3.
   !
   !JK   WRITE (11,*) 'FLGSD3'
   !JK   WRITE (11,*)      'wsd,wstr,zs ,w2 ,zb2,ds1 ,ds2 ,elu ,hd'    ,
   !JK  +                   wsd,wstr,zs ,w2 ,zb2,ds1 ,ds2 ,elu ,hd
   d2 = hd - zb2
   hsl = elu - zs
   !JK   WRITE (11,*)  'hsl',hsl
   term = ((4.0d0 * cwd * cwd * rhoast * wstr * wstr) / (w2 * d2)) * (1.0d0 + lambda / d2)
   !
   aw = (-term * hsl - 4.0d0 * cwd * wstr + (1.0d0 - rhoast)                       &
      & * (w2 / 12.0d0 + wsd / 4.0d0) + 0.5d0 * (rhoast + 1.0d0) * (c13 * w2 + c23 * wsd))  &
      & / term
   !
   bw = (4.0d0 * cwd * wstr * hsl + (1.0d0 - rhoast)                                 &
        & * ((d2 + ds1) * (w2 + wsd) / 6.d0 + ds1 * wsd * c13) + 0.5d0 * (rhoast + 1.0d0)   &
        & * ((ds1 + ds2 - d2) * (c13 * w2 + c23 * wsd) + (c23 * d2 + c13 * ds1)             &
            & * w2 + (c13 * d2 + c23 * ds1) * wsd)) / term
   !
   cw = ((1.0d0 - rhoast) * ((d2 + ds1)**2 * (w2 + wsd) / 12.d0 + ds1**2 * wsd / 6.0d0)  &
      & + 0.5d0 * (rhoast + 1.0d0) * (ds1 + ds2 - d2)                              &
      & * ((c23 * d2 + c13 * ds1) * w2 + (c13 * d2 + c23 * ds1) * wsd)) / term
   !
   !     Solve the equation ds**3 + aw*ds**2 + bw*ds +cw to get the water
   !     level at the sill
   !
   p = bw / 3.0d0 - aw * aw / 9.0d0
   q = aw * aw * aw / 27.0d0 - aw * bw / 6.0d0 + cw / 2.0d0
   hulp = q * q + p * p * p
   !
   if (hulp < 0.0d0) then
      p = abs(p)
      phi = acos(abs(q) / p / sqrt(p)) / 3.0d0
      r60 = acos(0.5d0)
      fac = sign(2.d0, q) * sqrt(p)
      h2a = -fac * cos(phi)
      h2b = fac * cos(r60 - phi)
      h2c = fac * cos(r60 + phi)
      ds = max(h2a, h2b, h2c) - aw / 3.0d0
   else
      hulp = sqrt(hulp)
      hulp1 = -q + hulp
      if (abs(hulp1) < 1e-6) then
         u = 0; v = 0
      else ! hk: ook fix for Erwin, ARS 15132
         u = abs(hulp1)**c13 * sign(1.0d0, hulp1)
         hulp1 = -q - hulp
         v = abs(hulp1)**c13 * sign(1.0d0, hulp1)
      end if
      ds = u + v - aw / 3.0d0
   end if
end subroutine flgsd3fm
