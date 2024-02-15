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
module waq_attribute_utils
    use m_waq_precision

    implicit none
    private
    public :: set_feature, evaluate_dimension_match

contains


    subroutine set_feature (feature_index, feature_value, update_value)
        !! Sets a specific feature in a multi-feature integer based on the provided index
        !!
        !! The feature is an integer with at most 9 10-base features.
        !! This routine sets a feature. Routine evaluate_waq_attribute is the mirror
        !! routine that reads the feature.

        integer(kind = int_wp), intent(in) :: feature_index       !! Index of the feature to be set
        integer(kind = int_wp), intent(inout) :: feature_value    !! Feature value to be modified
        integer(kind = int_wp), intent(in) :: update_value        !! Value to update the feature with

        integer(kind = int_wp) :: helper_value           ! to store lower order part
        integer(kind = int_wp) :: power_ten          ! to store higher powers of 10

        if(feature_index == 1) then
            feature_value = (feature_value / 10) * 10 + update_value
        elseif (feature_index == 2) then
            helper_value = mod(feature_value, 10)
            feature_value = (feature_value / 100) * 100 + update_value * 10 + helper_value
        elseif (feature_index == 3) then
            helper_value = mod(feature_value, 100)
            feature_value = (feature_value / 1000) * 1000 + update_value * 100 + helper_value

        elseif (feature_index <= 0 .or. feature_index > 9) then
            feature_value = -999
        else
            power_ten = 10**feature_index + 0.5
            helper_value = mod(feature_value, power_ten / 10)
            feature_value = (feature_value / power_ten) * power_ten + update_value * power_ten / 10 + helper_value
        endif

        return
    end subroutine set_feature

    subroutine evaluate_dimension_match (target_dimension, num_exchanges_3d, is_match)

        ! utility that evaluates the TRswitch for the target model dimension
        ! Evaluates if the target model dimension matches the number of 3D exchanges

        integer(kind = int_wp), intent(in) :: target_dimension      !! Target dimension indicator
        integer(kind = int_wp), intent(in) :: num_exchanges_3d      !! Number of exchanges in 3 direction.
        logical, intent(out) :: is_match

        is_match = .TRUE.
        if (target_dimension == 3 .AND. num_exchanges_3d   == 0) then
            is_match = .FALSE.
        endif

        if (target_dimension == 12 .AND. num_exchanges_3d   > 0) then
            is_match = .FALSE.
        end if

    end subroutine evaluate_dimension_match
end module waq_attribute_utils
