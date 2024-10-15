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

subroutine flccgsfm(dg, dsc, cgd, cgf, cw, mugf, cgda, cgfa, mugfa)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   use m_dpsequfm
   implicit none
!
! Global variables
!
   double precision, intent(in) :: cgd
   double precision, intent(out) :: cgda
   double precision, intent(in) :: cgf
   double precision, intent(out) :: cgfa
   double precision, intent(in) :: cw
   double precision :: dg
   double precision :: dsc
   double precision, intent(in) :: mugf
   double precision, intent(out) :: mugfa
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
   ! Module:             FLCCGS (FLow Corr. Coefficients for General Structure)
   !
   ! Module description: Correct coefficients for gate flow
   !
   !                     In the formulas for the gate and weir several
   !                     coefficients are applied. To avoid discontinuities
   !                     in the transition from weir to gate flow, the
   !                     correction coefficient cgd should be corrected.
   !
   !
   ! Parameters:
   ! NR NAME              IO DESCRIPTION
   !  3 cgd               I  Correction coefficient for drowned gate flow.
   !  7 cgda              O  Adapted correction coefficient for drowned
   !                         gate flow.
   !  4 cgf               I  Correction coefficient for free gate flow.
   !  8 cgfa              O  Adapted correction coefficient for free gate
   !                         flow.
   !  5 cw                I  Correction coefficient for weir flow.
   !  1 dg                I  Gate opening height.
   !  2 dsc               I  Depth at sill or critical depth.
   !  6 mugf              I  Contraction coefficient for free gate flow.
   !  9 mugfa             O  Adapted contraction coefficient for free gate
   !                         flow.
   !=======================================================================
   !
   !     Declaration of parameters:
   !
   !
   !     Logical function
   !
   !
   !     dsc contains ds or dc
   !
   if (.not. dpsequfm(dsc, 0.0d0, 1.d-20)) then
      !
      if (dg / dsc > mugf) then
         mugfa = dg / dsc
      else
         mugfa = mugf
      end if
      !
      if (cgd > cw) then
         if (dpsequfm(dg, 0.0d0, 1.0d-20)) then
            cgda = cgd
         else
            cgda = min(dsc / dg * cw, cgd)
         end if
      else
         cgda = max(dg / dsc * cw, cgd)
      end if
      !
      if (cgf > cw) then
         if (dpsequfm(dg, 0.0d0, 1.0d-20)) then
            cgfa = cgf
         else
            cgfa = min(dsc / dg * cw, cgf)
         end if
      else
         cgfa = max(dg / dsc * cw, cgf)
      end if
      !
   else
      mugfa = mugf
      cgda = cgd
      cgfa = cgf
   end if
end subroutine flccgsfm
