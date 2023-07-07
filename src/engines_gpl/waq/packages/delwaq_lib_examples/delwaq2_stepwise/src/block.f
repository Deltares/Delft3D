!!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

      BLOCK DATA
      IMPLICIT INTEGER (A-Z)
      COMMON /ZDLMTZ/DLM(256), IGN(256), IDLM, IIGN, PUSH(256), PTR
      DATA PTR/0/, IDLM, IIGN/2, 2/
!     DATA DLM/64*0, 1, 42*0, 1, 148*0/
!     DATA IGN/64*0, 1, 42*0, 1, 148*0/
! *** Replaced previous two line by following two lines on ASCI computers
      DATA DLM/32*0, 1, 11*0, 1, 211*0/
      DATA IGN/32*0, 1, 11*0, 1, 211*0/
      END
