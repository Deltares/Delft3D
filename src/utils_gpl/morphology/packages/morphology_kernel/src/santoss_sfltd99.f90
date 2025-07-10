subroutine santoss_sfltd99(d50, sc, st, swc, swt, udeltanet, sfltc, sfltt)
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
!  
!  
!!--description-----------------------------------------------------------------
!
!   The SANTOSS practical sand transport model, version 2.08  
!   Computations of the orbital motions in the nearshore morphodynamical model
!   using the formula of Abreu et al. (2010)
!
!   Computation of the sheet-flow layer thickness using the formula of
!   Dohmen-Janssen (1999)
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! arguments
!
    real(fp)  , intent(in)  :: d50
    real(fp)  , intent(in)  :: sc
    real(fp)  , intent(in)  :: st
    real(fp)  , intent(in)  :: swc
    real(fp)  , intent(in)  :: swt
    real(fp)  , intent(in)  :: udeltanet
!
    real(fp)  , intent(out) :: sfltc
    real(fp)  , intent(out) :: sfltt
!
! local variables
!
    real(fp)                :: fs_const
!
!! executable statements -------------------------------------------------------
!
    fs_const=25.08_fp         ! fine sand constant

    if (comparereal(udeltanet,0.0_fp) == 0) then
        ! using the wave-alone bed shear stresses
        if (d50 <= 0.00015_fp) then
            sfltc=d50*fs_const*swc
            sfltt=d50*fs_const*swt
        elseif (d50 >= 0.00020_fp) then
            sfltc=d50*13.0_fp*swc
            sfltt=d50*13.0_fp*swt
        else
            ! linear interpolation between values for D50=0.15 mm and D50=0.20 mm
            sfltc=d50*swc*(fs_const+(d50-0.00015_fp)* &
                        & (13.0_fp-fs_const)/(0.00020_fp-0.00015_fp))
            sfltt=d50*swt*(fs_const+(d50-0.00015_fp)* &
                        & (13.0_fp-fs_const)/(0.00020_fp-0.00015_fp))
        endif
    else
        ! using the total bed shear stresses
        if (d50 <= 0.00015_fp) then
            sfltc=d50*fs_const*sc
            sfltt=d50*fs_const*st
        elseif (d50 >= 0.00020_fp) then
            sfltc=d50*13.0_fp*sc
            sfltt=d50*13.0_fp*st
        else
            ! linear interpolation between values for d50=0.15 mm and d50=0.20mm
            sfltc=d50*sc*(fs_const+(d50-0.00015_fp)* &
                       & (13.0_fp-fs_const)/(0.00020_fp-0.00015_fp))
            sfltt=d50*st*(fs_const+(d50-0.00015_fp)* &
                       & (13.0_fp-fs_const)/(0.00020_fp-0.00015_fp))
        endif
    endif
end subroutine santoss_sfltd99
