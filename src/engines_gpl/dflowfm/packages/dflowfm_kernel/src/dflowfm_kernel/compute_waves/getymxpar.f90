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

subroutine getymxpar(modind, tauwav, taucur, fw, cdrag, abscos, ypar, ymxpar)
   implicit none
   integer, intent(in) :: modind
   double precision, intent(in) :: tauwav, taucur, fw, cdrag, abscos
   double precision, intent(out) :: ypar, ymxpar
   double precision :: xpar ! Variable x in in expression for parametrized models
   real, dimension(8) :: coeffi ! Coefficient i in expression for parametrized models
   real, dimension(8) :: coeffj ! Coefficient j in expression for parametrized models
   real, dimension(8, 4) :: aa ! Coefficient a(i) in expression for parameter a
   real, dimension(8, 4) :: bb ! Coefficient b(i) in expression for parameter b
   real, dimension(8, 4) :: mm ! Coefficient m(i) in expression for parameter n
   real, dimension(8, 4) :: nn ! Coefficient n(i) in expression for parameter n
   real, dimension(8, 4) :: pp ! Coefficient p(i) in expression for parameter p
   real, dimension(8, 4) :: qq ! Coefficient q(i) in expression for parameter q
   double precision :: lfc, cj, coeffb, coeffp, coeffq, ci, coeffa, coeffm, coeffn

   data bb/0.29, 0.65, 0.27, 0.73, 0.22, 0.32, 0.47, -0.06, &
      0.55, 0.29, 0.51, 0.40, 0.73, 0.55, 0.29, 0.26, &
      -0.10, -0.30, -0.10, -0.23, -0.05, 0.00, -0.09, 0.08, &
      -0.14, -0.21, -0.24, -0.24, -0.35, 0.00, -0.12, -0.03/

   data pp/-0.77, -0.60, -0.75, -0.68, -0.86, -0.63, -0.70, -1.00, &
      0.10, 0.10, 0.13, 0.13, 0.26, 0.05, 0.13, 0.31, &
      0.27, 0.27, 0.12, 0.24, 0.34, 0.00, 0.28, 0.25, &
      0.14, -0.06, 0.02, -0.07, -0.07, 0.00, -0.04, -0.26/

   data qq/0.91, 1.19, 0.89, 1.04, -0.89, 1.14, 1.65, 0.38, &
      0.25, -0.68, 0.40, -0.56, 2.33, 0.18, -1.19, 1.19, &
      0.50, 0.22, 0.50, 0.34, 2.60, 0.00, -0.42, 0.25, &
      0.45, -0.21, -0.28, -0.27, -2.50, 0.00, 0.49, -0.66/

   data coeffj/3.00, 0.50, 2.70, 0.50, 2.70, 3.00, 0.60, 1.50/

   !-----for Tau_max
   data aa/-0.06, -0.01, -0.07, 0.11, 0.05, 0.00, -0.01, -0.45, &
      1.70, 1.84, 1.87, 1.95, 1.62, 2.00, 1.58, 2.24, &
      -0.29, -0.58, -0.34, -0.49, -0.38, 0.00, -0.52, 0.16, &
      0.29, -0.22, -0.12, -0.28, 0.25, 0.00, 0.09, -0.09/

   data mm/0.67, 0.63, 0.72, 0.65, 1.05, 0.00, 0.65, 0.71, &
      -0.29, -0.09, -0.33, -0.22, -0.75, 0.50, -0.17, 0.27, &
      0.09, 0.23, 0.08, 0.15, -0.08, 0.00, 0.18, -0.15, &
      0.42, -0.02, 0.34, 0.06, 0.59, 0.00, 0.05, 0.03/

   data nn/0.75, 0.82, 0.78, 0.71, 0.66, 0.00, 0.47, 1.19, &
      -0.27, -0.30, -0.23, -0.19, -0.25, 0.50, -0.03, -0.66, &
      0.11, 0.19, 0.12, 0.17, 0.19, 0.00, 0.59, -0.13, &
      -0.02, -0.21, -0.12, -0.15, -0.03, 0.00, -0.50, 0.12/

   data coeffi/0.80, 0.67, 0.82, 0.67, 0.82, 1.00, 0.64, 0.77/

   if (tauwav < 1.0d-8) then
      xpar = 1d0
      ypar = 1d0
      ymxpar = 1d0
   else
      xpar = taucur / (taucur + tauwav)
      if (xpar < 1.0d-8 .or. modind > 8) then
         ypar = 0d0
         ymxpar = 1d0
      else
         lfc = log10(fw / cdrag)
         cj = abscos**coeffj(modind)
         coeffb = (bb(modind, 1) + bb(modind, 2) * cj) + (bb(modind, 3) + bb(modind, 4) * cj) * lfc
         coeffp = (pp(modind, 1) + pp(modind, 2) * cj) + (pp(modind, 3) + pp(modind, 4) * cj) * lfc
         coeffq = (qq(modind, 1) + qq(modind, 2) * cj) + (qq(modind, 3) + qq(modind, 4) * cj) * lfc
         ypar = xpar * (1d0 + coeffb * (xpar**coeffp) * ((1d0 - xpar)**coeffq))
         ci = abscos**coeffi(modind)
         coeffa = (aa(modind, 1) + aa(modind, 2) * ci) + (aa(modind, 3) + aa(modind, 4) * ci) * lfc
         coeffm = (mm(modind, 1) + mm(modind, 2) * ci) + (mm(modind, 3) + mm(modind, 4) * ci) * lfc
         coeffn = (nn(modind, 1) + nn(modind, 2) * ci) + (nn(modind, 3) + nn(modind, 4) * ci) * lfc
         ymxpar = 1d0 + coeffa * (xpar**coeffm) * ((1d0 - xpar)**coeffn)
      end if
   end if
end subroutine getymxpar
