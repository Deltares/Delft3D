!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2025.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!> Module for flocculation formulations
module flocculation
    use precision, only: fp, INT32
    use flocculation_manning
    use flocculation_chassagne
    implicit none
    private
    
    public macro_floc_settling_manning
    public micro_floc_settling_manning
    public macro_floc_frac_manning
    public floc_manning
    
    public macro_floc_settling_chassagne
    public micro_floc_settling_chassagne
    public macro_floc_frac_chassagne
    public floc_chassagne
    
    public flocculate
    public get_tshear_tdiss

    public FLOC_NONE, FLOC_MANNING_DYER, FLOC_CHASSAGNE_SAFAR, FLOC_VERNEY_ETAL

    integer(kind=INT32), parameter :: FLOC_NONE                 = 0      !< no flocculation
    integer(kind=INT32), parameter :: FLOC_MANNING_DYER         = 1      !< flocculation based on Manning and Dyer (2007)
    integer(kind=INT32), parameter :: FLOC_CHASSAGNE_SAFAR      = 2      !< flocculation based on Chassagne and Safar (2020)
    integer(kind=INT32), parameter :: FLOC_VERNEY_ETAL          = 3      !< flocculation based on Verney et al (2010)
    
    real(fp), parameter            :: PARAM_SOULSBY             = 3.0_fp !< Coefficient of proportionality according to Soulsby (see Manning and Dyer)

contains

!> Update the mass distribution of clay over the various floc sizes.
subroutine flocculate(cfloc, flocdt, breakdt, flocmod)
!
! Global variables
!
    real(fp), dimension(:,:), intent(inout)  :: cfloc   !< Concentration split per clay fraction and floc size [g/m3]
    real(fp),                 intent(in)     :: flocdt  !< Relaxation factor towards equilibrium with more macro flocs [-]
    real(fp),                 intent(in)     :: breakdt !< Relaxation factor towards equilibrium with less macro flocs [-]
    integer,                  intent(in)     :: flocmod !< Flocculation model being used [-]
    
!
! Local variables 
!
    integer  :: i              !< Clay population index
    integer  :: j              !< Floc size index
    integer  :: nflocpop       !< Number of clay populations
    integer  :: nflocsizes     !< Number of floc size classes
    real(fp) :: adt            !< Relaxation factor towards equilibtium [-]
    real(fp) :: eq_cfloc_micro !< Equilibrium concentration of micro flocs within specific clay population [g/m3]
    real(fp) :: eq_cfloc_macro !< Equilibrium concentration of macro flocs within specific clay population [g/m3]
    real(fp) :: macro_frac     !< Fraction of macro flocs mass of total spm mass [-]
    real(fp) :: tcclay         !< Total clay concentration [g/m3]
    real(fp) :: tcpop          !< Total concentration of specific clay population [g/m3]
    !
    nflocpop = size(cfloc,1)
    nflocsizes = size(cfloc,2)
    
    tcclay = 0.0_fp
    do j = 1, nflocsizes
       do i = 1, nflocpop
           tcclay = tcclay + cfloc(i,j)
       enddo
    enddo

    ! only change the composition when nflocsizes >1, 
    ! otherwise it is seen as a mixture of microflocs and macroflocs.
    if (nflocsizes > 1) then 
       select case (flocmod)
       case (FLOC_MANNING_DYER, FLOC_CHASSAGNE_SAFAR)
          if (flocmod == FLOC_MANNING_DYER) then
             call macro_floc_frac_manning( tcclay, macro_frac )
          else
             call macro_floc_frac_chassagne( tcclay, macro_frac )
          endif
          
          do i = 1, nflocpop
             tcpop = cfloc(i,1) + cfloc(i,2)
             !
             eq_cfloc_macro = macro_frac * tcpop
             eq_cfloc_micro = tcpop - eq_cfloc_macro
             !
             if (eq_cfloc_macro > cfloc(i,2)) then ! towards more macro flocs, use flocculation time scale
                adt = flocdt
             else ! towards less macro flocs, use break-up time scale
                adt = breakdt
             endif
             cfloc(i,1) = cfloc(i,1) + adt * (eq_cfloc_micro - cfloc(i,1))
             cfloc(i,2) = cfloc(i,2) + adt * (eq_cfloc_macro - cfloc(i,2))
          enddo
       
       case (FLOC_VERNEY_ETAL)
          !call floc_verney
       
       end select
    endif
   
end subroutine flocculate


!> Calculate the turbulent shear and dissipation for different flow models
subroutine get_tshear_tdiss( tshear, tdiss, rho_water, tke, tlength, timtur, taub, waterdepth, localdepth, vonkar )
!
! Global variables
!
    real(fp), intent(out)           :: tshear     !< Turbulent shear stress [N/m2]
    real(fp), intent(inout)         :: tdiss      !< Turbulent dissipation epsilon [m2/s3]
    real(fp), intent(in)            :: rho_water  !< Water density [kg/m3]
    real(fp), optional, intent(in)  :: tke        !< Turbulent kinetic energy k [m2/s2]
    real(fp), optional, intent(in)  :: tlength    !< Turbulent length scale L [m]
    real(fp), optional, intent(in)  :: timtur     !< Turbulent time scale tau [1/s]
    real(fp), optional, intent(in)  :: taub       !< Bed shear stress [N/m2]
    real(fp), optional, intent(in)  :: waterdepth !< Total water depth [m]
    real(fp), optional, intent(in)  :: localdepth !< Depth below water surface [m]
    real(fp), optional, intent(in)  :: vonkar     !< Von Karman constant [-]

!
! Local variables 
!
    real(fp), parameter :: cd = 0.09_fp ** 0.75_fp !< turbulence constant [-] cmu^(3/4)
    
    real(fp) :: ustar         !< shear velocity [m/s]
    real(fp) :: z             !< height above the bed [m]
    real(fp) :: xi            !< relative depth [-]

    if (present(tke)) then
       if (present(timtur)) then ! k-tau
          tshear = PARAM_SOULSBY * rho_water * tke
          tdiss = tke * timtur
       elseif (present(tlength)) then ! k-L
          tshear = PARAM_SOULSBY * rho_water * tke
          tdiss = cd * tke ** 1.5_fp / tlength
       else ! k-eps
          tshear = PARAM_SOULSBY * rho_water * tke
          ! tdiss already set
       endif
    elseif (present(waterdepth) .and. present(taub) .and. present(vonkar)) then
       if (present(localdepth)) then ! algebraic
          z = waterdepth - localdepth
          xi = localdepth/waterdepth
       else ! 2D
          z  = 0.5_fp * waterdepth
          xi = 0.5_fp
       endif
       ustar = sqrt(taub / rho_water)
       tshear = xi * taub
       tdiss = (xi * ustar ** 3) / (vonkar * z)
    endif
end subroutine get_tshear_tdiss

end module flocculation
