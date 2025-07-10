!!  Copyright (C)  Stichting Deltares, 2012-2025.
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

!!  *********************************************************************
!!  *    Modules containing all the functions for PROTIST                *
!!  *********************************************************************

!!   contains the following functions:
!!    - protist_uptake_functions
!!              contains all the functions needed for uptake of nutrients
!!


module protist_uptake_functions

    use protist_math_functions
    IMPLICIT NONE
contains

    ! function to calculate the influence of p status on uptake of NH4 and NO3
    ! NutC4Pval varies bettwen PinteractionValue and NutCvaluedepending on the status of PC
    ! Units: gN gC-1
    real function interactionP(PinteractionValue, PCstatus, NCstatus, NutCvalue) result(NutC4Pval)
        real, intent(in) :: PinteractionValue   ! either optimum or maximum PC value for either opt or max NC value
        real, intent(in) :: PCstatus, NCstatus  ! nutrient status of the cell
        real, intent(in) :: NutCvalue           ! either optimum or maximum NC value

        ! only if P status worse then N status then NutCvalue changed
        if (PCstatus < NCstatus) then
            NutC4Pval = PinteractionValue + PCstatus * (NutCvalue - PinteractionValue)
        else
            NutC4Pval = NutCvalue
        end if
    end function interactionP

    ! uptake phosphate
    ! Units: gP gC-1 d-1
    real function uptakeP(NutC, NutCmin, NutCopt, NutCmax, maxUmT, resource, halfSat) result(up)
        real, intent(in) :: NutC, NutCmin, NutCopt, NutCmax  ! cellular quotas (current, minimum, optimum, maximum)
        real, intent(in) :: maxUmT                  ! species specific maximum growth rate
        real, intent(in) :: resource                ! nutrient
        real, intent(in) :: halfSat                 ! species specific monod half saturation constant
        ! curve forming parameters for increase sigmoid
        real, parameter :: L_in = 1.0                  ! upper asymptote
        real, parameter :: k_in = -16.0                ! growth rate of the curve
        real, parameter :: b_in = 0.7                  ! displacement along the x-axis
        real, parameter :: yFactor = 10.0              ! multiplies the plateau of the curve
        ! curve forming parameters for decrease sigmoid
        real, parameter :: L_de = 1.0                  ! upper asymptote
        real, parameter :: k_de = -40.0                ! growth rate of the curve
        real, parameter :: b_de = 0.9                  ! displacement along the x-axis
        ! function auxiliaries
        real nNutC_below, nNutC_above     ! normalized NutC ratio between minVal and optVal/max
        real APincrease, APdecrease      ! acquistion potential as a sigmoidal curve [0,1]
        real optUptake                    ! uptake at optimal nutrient quota
        real resource_check               ! check that resource is not negative

        ! increase of acquisition potential when NutC < NutCopt
        nNutC_below = normalize(NutC, NutCmin, NutCopt)
        APincrease = sigmoidLogistic(L_in, k_in, b_in, nNutC_below)

        ! decrease of acquisition potential when NutC > NutCopt
        nNutC_above = normalize(NutC, NutCmin, NutCmax)
        APdecrease = sigmoidLogistic(L_de, k_de, b_de, nNutC_above)

        ! check that resource if not negative
        resource_check = max(0.0, resource)

        ! species specific uptake at optimum nutrient ratio
        optUptake = monod(resource_check, halfSat) * maxUmT * NutCopt
        ! uptake at current nutrient ratio
        up = optUptake * APincrease * yFactor + optUptake * APdecrease
    end function uptakeP

    ! uptake NH4
    ! Units: gN gC-1 d-1
    real function uptakeNH4(minPC4optNC, minPC4maxNC, PCstatus, NCstatus, NutC, NutCmin, NutCopt, NutCmax, maxUmT, relGrowthRate, resource, halfSat) result(up)
        real, intent(in) :: NutC, NutCmin, NutCopt, NutCmax  ! cellular quotas (current, minimum, optimum, maximum)
        real, intent(in) :: maxUmT                           ! species specific maximum growth rate
        real, intent(in) :: resource                         ! nutrient
        real, intent(in) :: halfSat                          ! species specific monod half saturation constant
        real, intent(in) :: PCstatus, NCstatus               ! cellular nutrient status
        real, intent(in) :: minPC4optNC, minPC4maxNC         ! minimum PC status for optimal/maximum NC status
        real, intent(in) :: relGrowthRate                    ! relative max growth rate on that nutrient
        ! curve forming parameters for increase sigmoid
        real, parameter :: L_in = 1.0                 ! upper asymptote
        real, parameter :: k_in = -24.0               ! growth rate of the curve
        real, parameter :: b_in = 0.85                ! displacement along the x-axis
        real, parameter :: yFactor = 3.0           ! multiplies the plateau of the curve
        ! curve forming parameters for decrease sigmoid
        real, parameter :: L_de = 1.0                 ! upper asymptote
        real, parameter :: k_de = -40.0               ! growth rate of the curve
        real, parameter :: b_de = 0.85                ! displacement along the x-axis
        real NutCPopt, NutCPmax       ! N interaction with P status
        real nNutCopt, nNutCmax       ! normailzed quotas with P interaction
        real APincrease, APdecrease   ! acquistion potential as a sigmoidal curve [0,1]
        real optUptake                ! uptake at optimal nutrient quota
        real resource_check           ! check that resource is not negative

        ! increase of acquisition potential when NutC < NutCopt
        NutCPopt = interactionP(minPC4optNC, PCstatus, NCstatus, NutCopt)
        nNutCopt = normalize(NutC, NutCmin, NutCPopt)
        APincrease = sigmoidLogistic(L_in, k_in, b_in, nNutCopt)

        ! decrease of acquisition potential when NutC > NutCopt
        NutCPmax = interactionP(minPC4maxNC, PCstatus, NCstatus, NutCmax)
        nNutCmax = normalize(NutC, NutCmin, NutCPmax)
        APdecrease = sigmoidLogistic(L_de, k_de, b_de, nNutCmax)

        ! check that resource is not negative
        resource_check = max(0.0, resource)

        ! species specific uptake at optimum nutrient ratio
        optUptake = monod(resource_check, halfSat) * maxUmT * NutCopt * relGrowthRate
        ! uptake at current nutrient ratio
        up = optUptake * APincrease * yFactor + optUptake * APdecrease
    end function uptakeNH4

    ! uptake NO3
    ! Units: gN gC-1 d-1
    real function uptakeNO3(minPC4maxNC, PCstatus, NCstatus, NutC, NutCmin, NutCopt, NutCmax, maxUmT, relGrowthRate, resource, halfSat) result(up)
        real, intent(in) :: NutC, NutCmin, NutCopt, NutCmax  ! cellular quotas (current, minimum, optimum, maximum)
        real, intent(in) :: maxUmT                           ! species specific maximum growth rate
        real, intent(in) :: resource                         ! nutrient
        real, intent(in) :: halfSat                          ! species specific monod half saturation constant
        real, intent(in) :: PCstatus, NCstatus               ! cellular nutrient status
        real, intent(in) :: minPC4maxNC                      ! minimum PC status for optimal/maximum NC status
        real, intent(in) :: relGrowthRate                    ! relative max growth rate on that nutrient
        ! curve forming parameters for decrease sigmoid
        real, parameter :: L_de = 1.0                 ! upper asymptote
        real, parameter :: k_de = -55.0               ! growth rate to form the curve
        real, parameter :: b_de = 0.9                 ! displacement along the x-axis
        real :: NutCPmax       ! N interaction with P status
        real :: nNutCmax       ! normailzed quotas with P interaction
        real :: APdecrease     ! acquistion potential as a sigmoidal curve [0,1]
        real :: optUptake      ! uptake at optimal nutrient quota
        real :: resource_check               ! check that resource is not negative

        ! decrease of acquisition potential when NutC = NutCopt
        NutCPmax = interactionP(minPC4maxNC, PCstatus, NCstatus, NutCmax)
        nNutCmax = normalize(NutC, NutCmin, NutCPmax)
        APdecrease = sigmoidLogistic(L_de, k_de, b_de, nNutCmax)

        ! check that resource if not negative
        resource_check = max(0.0, resource)

        ! species specific uptake at optimum nutrient ratio
        optUptake = monod(resource_check, halfSat) * maxUmT * NutCopt * relGrowthRate
        ! uptake at current nutrient ratio
        up = optUptake * APdecrease
    end function uptakeNO3

    ! uptake silica
    ! Units: gSi gC-1 d-1
    real function uptakeSi(NutC, NutCmin, NutCopt, NutCmax, maxUmT, resource, halfSat) result(up)
        real, intent(in) :: NutC, NutCmin, NutCopt, NutCmax  ! cellular quotas (current, minimum, optimum, maximum)
        real, intent(in) :: maxUmT                           ! species specific maximum growth rate
        real, intent(in) :: resource                         ! nutrient
        real, intent(in) :: halfSat                          ! species specific monod half saturation constant
        ! curve forming parameters for decrease sigmoid
        real, parameter :: L_de = 1.0               ! upper asymptote
        real, parameter :: k_de = -80.0             ! growth rate to form the curve
        real, parameter :: b_de = 0.95              ! displacement along the x-axis
        real :: nNutC              ! normalized NutC ratio between minVal and optVal
        real :: APdecrease         ! acquistion potential as a sigmoidal curve [0,1]
        real :: optUptake          ! uptake at quota below the optimal nutrient quota
        real :: resource_check               ! check that resource is not negative

        ! decrease of acquisition potential when NutC = NutCopt
        nNutC = normalize(NutC, NutCmin, NutCmax)
        APdecrease = sigmoidLogistic(L_de, k_de, b_de, nNutC)

        ! check that resource if not negative
        resource_check = max(0.0, resource)

        ! species specific uptake at optimum nutrient ratio
        optUptake = monod(resource_check, halfSat) * maxUmT * NutCopt
        ! uptake at current nutrient ratio
        up = optUptake * APdecrease
    end function uptakeSi

end module protist_uptake_functions
