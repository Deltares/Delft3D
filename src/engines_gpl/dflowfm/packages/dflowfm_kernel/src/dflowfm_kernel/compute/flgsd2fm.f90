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

module m_flgsd2fm

   implicit none

contains

   subroutine flgsd2fm(wsd, wstr, zs, w2, zb2, dg, ds1, ds2, elu, hd, rhoast,    &
                   & cgd, imag, ds, lambda)
      use precision, only: dp
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
      implicit none
!
! Local parameters
!
      real(kind=dp), parameter :: c23 = 2.0d0 / 3.0d0, c13 = 1.0d0 / 3.0d0
!
! Global variables
!
      logical, intent(out) :: imag
      real(kind=dp), intent(in) :: cgd
      real(kind=dp), intent(in) :: dg
      real(kind=dp), intent(out) :: ds
      real(kind=dp), intent(in) :: ds1
      real(kind=dp), intent(in) :: ds2
      real(kind=dp), intent(in) :: elu
      real(kind=dp), intent(in) :: hd
      real(kind=dp), intent(in) :: lambda
      real(kind=dp), intent(in) :: rhoast
      real(kind=dp), intent(in) :: w2
      real(kind=dp), intent(in) :: wsd
      real(kind=dp), intent(in) :: wstr
      real(kind=dp), intent(in) :: zb2
      real(kind=dp), intent(in) :: zs
!
!
! Local variables
!
      real(kind=dp) :: ag
      real(kind=dp) :: bg
      real(kind=dp) :: cg
      real(kind=dp) :: d2
      real(kind=dp) :: det
      real(kind=dp) :: hsl
      real(kind=dp) :: terma
      real(kind=dp) :: termb
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
      ! Module:             FLGSD2 (FLow Gen. Struct. Depth sill 2nd ord. eq.)
      !
      ! Module description: Compute water depth ds at the sill by a second
      !                     order algebraic equation.
      !
      !                     In case of drowned gate flow the water level at
      !                     the sill is required. The water depth is calcu-
      !                     lated in this routine.
      !
      !
      ! Parameters:
      ! NR NAME              IO DESCRIPTION
      ! 12 cgd               I  Correction coefficient for drowned gate flow.
      !  6 dg                I  Gate opening height.
      !  7 ds1               I  Delta s1 general structure.
      !  8 ds2               I  Delta s2 general structure.
      ! 14 ds                IO Water level immediately downstream the gate.
      !  9 elu               I  Upstream energy level.
      ! 10 hd                I  Downstream water level.
      ! 13 imag              O  Logical indicator, = TRUE when determinant of
      !                         second order algebraic equation less than
      !                         zero.
      ! 15 lambda            I  Extra resistance in general structure.
      ! 11 rhoast            I  Downstream water density divided by upstream
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
      !     Calculate Ag, Bg and Cg according to appendix C of
      !     the design document River Rural integratietraject deel 3.
      !JK   WRITE  (11,*)  'IN FLGSD2 ----'
      !
      ag = (1.0d0 - rhoast) * (w2 / 12.0d0 + wsd / 4.0d0) + 0.5d0 * (rhoast + 1.0d0)      &
          & * (c13 * w2 + c23 * wsd)
      d2 = hd - zb2
      !
      terma = (4.0d0 * rhoast * cgd * cgd * dg * dg * wstr * wstr) / (w2 * d2) * (1.0d0 + lambda / d2)
      termb = 4.0d0 * cgd * dg * wstr
      !
      bg = (1.0d0 - rhoast) * ((d2 + ds1) * (w2 + wsd) / 6.d0 + ds1 * wsd * c13)            &
         & + 0.5d0 * (rhoast + 1.0d0)                                               &
         & * ((ds1 + ds2 - d2) * (c13 * w2 + c23 * wsd) + (c23 * d2 + c13 * ds1)             &
         & * w2 + (c13 * d2 + c23 * ds1) * wsd) + terma - termb
      !
      hsl = elu - zs
      !
      cg = (1.0d0 - rhoast) * ((d2 + ds1)**2 * (w2 + wsd) / 12.d0 + ds1**2 * wsd / 6.0d0)   &
         & + 0.5d0 * (rhoast + 1.0d0) * (ds1 + ds2 - d2)                              &
         & * ((c23 * d2 + c13 * ds1) * w2 + (c13 * d2 + c23 * ds1) * wsd) - terma * hsl +        &
         & termb * hsl
      !
      det = bg * bg - 4.0d0 * ag * cg
      if (det < 0.0d0) then
         imag = .true.
         !JK      WRITE (11,*) 'Det=',det
      else
         imag = .false.
         ds = (-bg + sqrt(det)) / (2.0d0 * ag)
      end if
   end subroutine flgsd2fm

end module m_flgsd2fm
