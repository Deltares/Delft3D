!----- AGPL --------------------------------------------------------------------
!                                                                         
!  Copyright (C)  Stichting Deltares, 2017-2025.                          
!                                                                          
!  The program is free software: you can redistribute it and/or modify             
!  it under the terms of the GNU Affero General Public License as 
!  published by the Free Software Foundation version 3.          
!                                                                
!  The program  is distributed in the hope that it will be useful,        
!  but WITHOUT ANY WARRANTY; without even the implied warranty of    
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      
!  GNU Affero General Public License for more details.               
!                                                       
!  You should have received a copy of the GNU Affero General Public License
!  along with the program.  If not, see <http://www.gnu.org/licenses/>.    
!                                                                                     
!  Stichting Deltares                                                 
!  P.O. Box 177                                               
!  2600 MH Delft, The Netherlands
!                                               
!-------------------------------------------------------------------------------

module snow
    implicit none

    contains

    subroutine snowpack_hbv(precipitation, temperature, tti, tt, ttm, cfmax, whc, &
    snow, snowwater, snowmelt, rainfall, snowfall, n)

    integer, intent(in) :: n
    double precision, intent(in) :: precipitation(n), temperature(n), tti(n), tt(n)
    double precision, intent(in) :: ttm(n), cfmax(n), whc(n)
    double precision, intent(inout) :: snow(n), snowwater(n)
    ! this would normally be out, but we use inout here to keep the right shape
    ! in python, otherwise we get a 1D array back when we want a 2D array
    double precision, intent(inout) :: snowmelt(n), rainfall(n), snowfall(n)

    double precision :: rfcf, cfr, sfcf
    double precision :: rainfrac(n), potsnowmelt(n), potrefreezing(n), refreezing(n)
    double precision :: snowfrac(n), precipcorr(n), maxsnowwater(n)

    rfcf = 1.0  ! correction factor for rainfall
    cfr = 0.05  ! refreeing efficiency constant in refreezing of freewater in snow
    sfcf = 1.0  ! correction factor for snowfall

    ! fraction of precipitation which falls as rain
    where (tti == 0.0)
        where (temperature > tt)
            rainfrac = 1.0
        elsewhere
            rainfrac = 0.0
        end where
    elsewhere
        rainfrac = min((temperature - (tt - tti / 2.0)) / tti, 1.0d0)
    end where
    rainfrac = max(rainfrac, 0.0d0)

    ! fraction of precipitation which falls as snow
    snowfrac = 1.0 - rainfrac
    ! different correction for rainfall and snowfall
    precipcorr = sfcf * snowfrac * precipitation + rfcf * rainfrac * precipitation

    snowfall = snowfrac * precipcorr  ! snowfall depth
    rainfall = rainfrac * precipcorr  ! rainfall depth
    ! potential snow melt, based on temperature
    where (temperature > ttm)
        potsnowmelt = cfmax * (temperature - ttm)
    elsewhere
        potsnowmelt = 0.0
    end where
    ! potential refreezing, based on temperature
    where (temperature < ttm)
        potrefreezing = cfmax * cfr * (ttm - temperature)
    elsewhere
        potrefreezing = 0.0
    end where
    ! actual refreezing
    where (temperature < ttm)
        refreezing = min(potrefreezing, snowwater)
    elsewhere
        refreezing = 0.0
    end where

    ! no landuse correction here
    snowmelt = min(potsnowmelt, snow)  !actual snow melt
    snow = snow + snowfall + refreezing - snowmelt  !dry snow content
    snowwater = snowwater - refreezing  !free water content in snow
    maxsnowwater = snow * whc  ! max water in the snow
    snowwater = snowwater + snowmelt + rainfall  ! add all water and potentially supersaturate the snowpack
    rainfall = max(snowwater - maxsnowwater, 0.0d0)  ! rain + surpluss snowwater
    snowwater = snowwater - rainfall

end subroutine snowpack_hbv
end module snow
