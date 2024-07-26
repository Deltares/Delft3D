!!  Copyright (C)  Stichting Deltares, 2012-2024.
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


! Module for exchanging sediment parameters between various processes
! - used by DLWQG2 and by VEG3DU.
!
module layered_sediment
    implicit none

    integer, allocatable :: bottomsegments(:)
    real, allocatable :: sedconc(:, :, :)

    integer, parameter :: num_layers = 7
    real :: dl(num_layers) = [0.001, 0.002, 0.004, 0.008, 0.016, 0.032, 0.037]   ! layer thickness
    real :: bd(num_layers) = [0.001, 0.003, 0.007, 0.015, 0.031, 0.063, 0.100]   ! bottomdepth (depth of lower surface of layer)
    real :: sd(num_layers) = [0.000, 0.001, 0.003, 0.007, 0.015, 0.031, 0.063]   ! depth of upper surface of layer
    real :: tt(num_layers)
    real :: td(num_layers)

    ! sediment substances definition - indices into sedconc
    integer, parameter :: nototsed = 34
    integer, parameter :: nototseddis = 12
    integer, parameter :: nototsedpart = nototsed - nototseddis
    integer, parameter :: is_CH4 = 1
    integer, parameter :: is_DOC = 2
    integer, parameter :: is_DON = 3
    integer, parameter :: is_DOP = 4
    integer, parameter :: is_DOS = 5
    integer, parameter :: is_NH4 = 6
    integer, parameter :: is_NO3 = 7
    integer, parameter :: is_OXY = 8
    integer, parameter :: is_PO4 = 9
    integer, parameter :: is_Si = 10
    integer, parameter :: is_SO4 = 11
    integer, parameter :: is_SUD = 12
    integer, parameter :: is_AAP = 13
    integer, parameter :: is_APATP = 14
    integer, parameter :: is_FeIIIpa = 15
    integer, parameter :: is_Opal = 16
    integer, parameter :: is_POC1 = 17
    integer, parameter :: is_POC2 = 18
    integer, parameter :: is_POC3 = 19
    integer, parameter :: is_POC4 = 20
    integer, parameter :: is_PON1 = 21
    integer, parameter :: is_PON2 = 22
    integer, parameter :: is_PON3 = 23
    integer, parameter :: is_PON4 = 24
    integer, parameter :: is_POP1 = 25
    integer, parameter :: is_POP2 = 26
    integer, parameter :: is_POP3 = 27
    integer, parameter :: is_POP4 = 28
    integer, parameter :: is_POS1 = 29
    integer, parameter :: is_POS2 = 30
    integer, parameter :: is_POS3 = 31
    integer, parameter :: is_POS4 = 32
    integer, parameter :: is_SUP = 33
    integer, parameter :: is_VIVP = 34

end module layered_sediment
