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

module m_debgrz_output
    use m_waq_precision

    implicit none

    private
    public :: debgrz_output

    type :: debgrz_output
        real(kind=real_wp) :: totbiomass    !< Total biomass             [gC]
        real(kind=real_wp) :: biomass       !< Total biomass conc.    [gC/m3]
        real(kind=real_wp) :: totafdw       !< Total ash-free dry weight
                                            !<                        [gAFDW]
        real(kind=real_wp) :: afdw          !< Total ash-free dry weight
                                            !< concentration       [gAFDW/m3]
        real(kind=real_wp) :: totww         !< Total wet weight         [gWW]
        real(kind=real_wp) :: ww            !< Total wet weight conc.[gWW/m3]
        real(kind=real_wp) :: ww_ind        !< Wet weight of individual
                                            !<                      [gWW/ind]
        real(kind=real_wp) :: v             !< Individual volume    [cm3/ind]
        real(kind=real_wp) :: e             !< Individual energy      [J/ind]
        real(kind=real_wp) :: r             !< Individual gonads      [J/ind]
        real(kind=real_wp) :: length        !< Individual Length         [cm]
        real(kind=real_wp) :: gsi           !< Gonadosomatic Index        [-]
        real(kind=real_wp) :: e_scaled      !< Scaled energy density      [-]
        real(kind=real_wp) :: harvest       !< Total harvested biomass[gWW/d]
        real(kind=real_wp) :: spawn         !< Total spawned biomass  [gWW/d]
        real(kind=real_wp) :: grossgr       !< Gross growth         [gC/m2/d]
        real(kind=real_wp) :: nettgr        !< Net growth           [gC/m2/d]
        real(kind=real_wp) :: dens_out      !< Nr of individuals     [#/m2/d]
        real(kind=real_wp) :: c_balance     !< Balance of carbon      [gC/m3]
        real(kind=real_wp) :: n_balance     !< Balance of nitorgen    [gN/m3]
        real(kind=real_wp) :: p_balance     !< Balance of phosphorus  [gP/m3]

        contains
            procedure :: update_pmsa
    end type debgrz_output

    contains

    subroutine update_pmsa(this, pmsa, iparray, input_count)
        !< Assign output variables to pmsa array
        class(debgrz_output) :: this !< This instance of output_variables

        real(kind=real_wp),   intent(inout)   :: pmsa(*)

        integer(kind=int_wp), intent(in)    :: iparray(*)
        integer(kind=int_wp) :: input_count

        pmsa(iparray(input_count+1))  = this%totbiomass
        pmsa(iparray(input_count+2))  = this%biomass
        pmsa(iparray(input_count+3))  = this%totafdw
        pmsa(iparray(input_count+4))  = this%afdw
        pmsa(iparray(input_count+5))  = this%totww
        pmsa(iparray(input_count+6))  = this%ww
        pmsa(iparray(input_count+7))  = this%ww_ind
        pmsa(iparray(input_count+8))  = this%v
        pmsa(iparray(input_count+9))  = this%e
        pmsa(iparray(input_count+10)) = this%r
        pmsa(iparray(input_count+11)) = this%length
        pmsa(iparray(input_count+12)) = this%gsi
        pmsa(iparray(input_count+13)) = this%e_scaled
        pmsa(iparray(input_count+14)) = this%harvest
        pmsa(iparray(input_count+15)) = this%spawn
        pmsa(iparray(input_count+16)) = this%grossgr
        pmsa(iparray(input_count+17)) = this%nettgr
        pmsa(iparray(input_count+18)) = this%dens_out
        pmsa(iparray(input_count+19)) = this%c_balance
        pmsa(iparray(input_count+20)) = this%n_balance
        pmsa(iparray(input_count+21)) = this%p_balance
    end subroutine update_pmsa

end module m_debgrz_output
