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

      subroutine blmort (biomas, temp  , faut  , fdet  , flautn, fldetn,
     j                   flooxn, flmora, deat4 , tstepi, lmixo , lfixn ,
     j                   lcarb , nutcon, flxcon)

      use bloom_data_dim
      use bloom_data_size 
      use bloom_data_phyt    

      implicit none

      real(4)    biomas(*)  ! Biomass (gC/m3)
      real(4)    temp       ! Temperature (deg.C)
      real(4)    faut(*)    ! Fraction autolysis (-)
      real(4)    fdet(*)    ! Fraction detritus (-)
      real(4)    flautn(*)  ! Nutrient autolysis fluxes (g/m3/d)
      real(4)    fldetn(*)  ! Detritus production fluxes (g/m3/d)
      real(4)    flooxn(*)  ! OOX production fluxes (g/m3/d)
      real(4)    flmora(*)  ! Algae mortality fluxes (gC/m3/d)
      real(4)    deat4      ! ??$Check necessity to transfer$
      real(4)    tstepi     ! Time step (d)
      logical    lmixo      ! Flag mixotrophy
      logical    lfixn      ! Flag N-fixation
      logical    lcarb      ! Flag carbon limitation
      integer    nutcon(*)  ! Nutrients involved in active nutrient constraints
      integer    flxcon(*)  ! Uptake fluxes involved in active nutrient constraints

!     Local variables

      real(8)    temp8      ! Temperature (deg.C)
      real(8)    deat       ! ??
      real(8)    foox       ! Fraction other organic
      real(4)    cphyt      ! Biomass (gC/m3)
      real(4)    cmort      ! Mortality flux (gC/m3/d)
      real(4)    cmorta     ! Autolysis flux (gC/m3/d)
      real(4)    cmortd     ! Detritus prod. (gC/m3/d)
      real(4)    cmorto     ! OOx production (gC/m3/d)
      integer    i, j, k    ! Counters

!  Zero fluxes
      do j = 1,4
         flautn(j) = 0.0
         fldetn(j) = 0.0
         flooxn(j) = 0.0
      enddo

!  Call subroutine natmor: calculate natural mortality rate constants.
      deat  = 0d0
      temp8 = dble(temp)
      call natmor(deat, temp8)
      deat4 = sngl(deat)

!  Mortality module.
!
!  Objective: obtain nutrient fluxes to detritus, OOx and dissolved
!  nutrient pools due to mortality.
!
!  Again note that nutrient fluxes are computed from BLOOM's
!  stochiometry matrix and hence follow from biomasses in units dry
!  weight. The biomass mortality flux for DLWQWQ, however, is in units
!  of carbon.
!
!  Loop over algae species
      do j=1,nuspec
         cphyt = max(biomas(j) , 0.0)

!  Compute total mortality for this species and store the flux
!  avoid undershoots leading to negative biomass
         cmort = min(cphyt * sngl(rmort(j)) , cphyt / tstepi)
         flmora(j) = cmort

! Partition the mortality flux over detritus(D)/OOx(O)/autolysis(A)
         foox   = (1. - faut(j) - fdet(j))
         cmorta = cmort * faut(j)
         cmortd = cmort * fdet(j)
         cmorto = cmort * foox

! Detritus production for C, N, P, Si (for C including part autolysis)
! Autolysis for C, N, P, Si (NOT for carbon)
! OOx production for C, N, P, Si (for C including part autolysis)
         fldetn(1) = fldetn(1) + cmortd + cmorta * fdet(j) / (fdet(j) + foox)
         flooxn(1) = flooxn(1) + cmorto + cmorta * foox / (fdet(j) + foox)
         do k=1, nunuco
            i = nutcon(k)
            if (i.le.3) then
            fldetn(i+1) = fldetn(i+1) + cmortd * sngl(ctodry(j) * aa(k,j))
            flautn(i+1) = flautn(i+1) + cmorta * sngl(ctodry(j) * aa(k,j))
            flooxn(i+1) = flooxn(i+1) + cmorto * sngl(ctodry(j) * aa(k,j))
            endif
         enddo
      enddo

      return
      end