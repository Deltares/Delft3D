module m_calrou
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
!    module holding roughness related functions/subroutines
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
use precision
implicit none
private
public :: calrou, white_coolebrook
contains
subroutine calrou(kn_sum    ,fracto    ,fracbu      ,depth     ,ch_lin_ser, &
                & rough     ,defrou    ,rouflo      ,iarea_avg ,ch_sum_par, &
                & ch_sum_ser,ch_pnt_ser,alf_area_ser)
!!--description-----------------------------------------------------------------
!
!    Function: Finalize rougness calculation.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!
! Global variables
!
    integer     , intent(in) :: iarea_avg
    real(fp)    , intent(in) :: alf_area_ser
    real(fp)    , intent(in) :: defrou
    real(fp)    , intent(in) :: depth
    real(fp)                 :: fracbu
    real(fp)                 :: fracto
    real(fp)    , intent(in) :: ch_lin_ser
    real(fp)    , intent(in) :: ch_pnt_ser
    real(fp)                 :: ch_sum_par
    real(fp)                 :: ch_sum_ser
    real(fp)                 :: kn_sum
    real(fp)    , intent(out):: rough
    character(4), intent(in) :: rouflo
!
! Local variables
!
    real(fp) :: ch_sum
    real(fp) :: ch_sum_inv2
    real(fp) :: fracbc
    real(fp) :: rcbuno
    real(fp) :: rcbuys
    real(fp) :: rchedt
    real(fp) :: rkdef
    real(fp) :: chdef
!
!! executable statements -------------------------------------------------------
!
    !
    ! iarea_avg == 1 corresponds to the WAQUA Nikuradse implementation. In that
    ! implementation the area of the water free area was ignored when computing
    ! the residual area. We reproduce that approach here for consistency. For
    ! all other purposes fracto should be the area with roughness assigned plus
    ! the water free area.
    !
    if (iarea_avg /= 1) then
       fracto = fracto + fracbu
    endif
    !
    ! Fill the residual grid cell area with the default roughness.
    !
    if (fracto<0.99999) then
       !
       ! Transform default if not White-Colebrook
       !
       
       if ( defrou > 0d0 ) then
       
          if (rouflo=='WHIT') then
             rkdef = defrou
             chdef = white_coolebrook(depth, rkdef, iarea_avg)
          elseif (rouflo=='CHEZ') then
             chdef = defrou
             rkdef = (12.0_fp*depth) / (10.0_fp**(chdef/18.0_fp))
          elseif (rouflo=='MANN') then
             chdef = depth**(1.0_fp/6.0_fp) / defrou
             rkdef = (12.0_fp*depth) / (10.0_fp**(chdef/18.0_fp))
          elseif (rouflo=='Z   ') then
             rkdef = defrou * 30.0_fp
             chdef = white_coolebrook(depth, rkdef, iarea_avg)
          else
          endif
          kn_sum     = kn_sum     + (1.0_fp - fracto)*rkdef
          ch_sum_par = ch_sum_par + (1.0_fp - fracto)*chdef
          ch_sum_ser = ch_sum_ser + (1.0_fp - fracto)/(chdef*chdef)
       else ! default roughness<=0 -> set roughness to 0 for method 2 later (only relevant for method iarea_avg==2)
       end if
       !
       !
       ! Calculate correction factor
       !
       fracbu = min(fracbu, 0.99999_fp)
       fracbc = 1.0_fp / (1.0_fp - fracbu)
       !
       kn_sum     = fracbc*kn_sum
       ch_sum_par = fracbc*ch_sum_par
       ch_sum_ser = fracbc*ch_sum_ser
    endif
    !
    if (iarea_avg==1) then
       !
       ! Calculate roughness for hedges
       !
       if (ch_lin_ser>0.001) then
          rchedt = sqrt(1.0_fp/ch_lin_ser)
          kn_sum = kn_sum + (12.0_fp*depth)/(10.0_fp**(rchedt/18.0_fp))
       endif
       !
       ! Calculate roughness for trees
       !
       if (ch_pnt_ser>0.001) then
          rchedt = sqrt(1.0_fp/ch_pnt_ser)
          kn_sum = kn_sum + (12.0_fp*depth)/(10.0_fp**(rchedt/18.0_fp))
       endif
       !
       ! Calculate the c-value for the grid cell part without buildings
       ! the k-value is known(kn_sum).
       ! Calculate the c-value for the whole grid cell and then
       ! calculate the roughness for the whole grid cell
       !
       if (fracbu>0.001 .and. kn_sum>0.001) then
          fracbu = max( min(0.843_fp, fracbu) , 0.014_fp )
          rcbuno = 18.0_fp * log10(12.0_fp*depth/kn_sum)
          rcbuys = rcbuno * (1.12 - 0.25*fracbu - 0.99*sqrt(fracbu))
          kn_sum = (12.0_fp*depth) / (10.0_fp**(rcbuys/18.0_fp))
       endif
       !
       ! Store calculated roughness and transform the calculated
       ! Nikuradse roughness if not White-Colebrook.
       !
       kn_sum = max(kn_sum, 0.001_fp)
       if (rouflo=='WHIT') then
          rough = kn_sum
       elseif (rouflo=='CHEZ') then
          rough = 18.0_fp * log10(max((12.0_fp*depth)/kn_sum, 1.0129_fp))
       elseif (rouflo=='MANN') then
          rough = depth**(1.0_fp/6.0_fp) / (18.0_fp*log10(max((12.0_fp*depth)/kn_sum, 1.0129_fp)))
       elseif (rouflo=='Z   ') then
          rough = kn_sum / 30.0_fp
       else
       endif
    elseif (iarea_avg==2) then
    
       if ( defrou > 0d0 ) then  ! default roughness > 0
          !
          ! Combine serial and parallel roughnesses
          !
          ch_sum = alf_area_ser/sqrt(ch_sum_ser) &
                 & + (1.0_fp - alf_area_ser)*ch_sum_par
          !
          ! Calculate the c-value for the grid cell part without buildings
          ! the k-value is known(kn_sum).
          ! Calculate the c-value for the whole grid cell and then
          ! calculate the roughness for the whole grid cell
          !
          if (fracbu>0.00001) then
             fracbu = max( min(0.843_fp, fracbu) , 0.014_fp )
             ch_sum = ch_sum * (1.12_fp - 0.25_fp*fracbu - 0.99_fp*sqrt(fracbu))
          endif
          !
          ! Combine area, line and tree roughnesses
          !
          ch_sum_inv2 = 1.0_fp / (ch_sum * ch_sum)
          ch_sum      = 1.0_fp / sqrt(ch_sum_inv2 + ch_lin_ser + ch_pnt_ser)
          !
          ! Store calculated roughness and transform the calculated
          ! Chezy roughness if not Chezy.
          !
          if (rouflo=='WHIT') then
             rough = (12.0_fp*depth) / (10.0_fp**(ch_sum/18.0_fp))
          elseif (rouflo=='CHEZ') then
             rough = ch_sum
          elseif (rouflo=='MANN') then
             rough = depth**(1.0_fp/6.0_fp) / ch_sum
          elseif (rouflo=='Z   ') then
             rough = (12.0_fp*depth) / (10.0_fp**(ch_sum/18.0_fp))
             rough = rough / 30.0_fp
          else
          endif
       
       else
          rough = 0d0
       end if
    endif
end subroutine calrou

!> White-Coolebrook formulation for roughness.
!! If area averaging is type 2 (Chezy C based, parallel / serial), log10 must be save for ratios < 1.0
function white_coolebrook(radius, roughness_length, iarea_avg)
   real(kind=fp), intent(in) :: radius             !< hydraulic radius
   real(kind=fp), intent(in) :: roughness_length   !< Nikuradse roughness length
   integer,       intent(in) :: iarea_avg          !< area averaging method
   real(kind=fp)             :: white_coolebrook   !< function result

   real(kind=fp), parameter  :: cfix = 1.0010_fp   !< parameter to avoid negative roughness

   if (iarea_avg == 1) then
      white_coolebrook = 18.0_fp * log10(12.0_fp * radius / roughness_length)
   else
      white_coolebrook = 18.0_fp * log10(max(cfix, 12.0_fp * radius / roughness_length))
   endif
end function white_coolebrook

end module m_calrou
