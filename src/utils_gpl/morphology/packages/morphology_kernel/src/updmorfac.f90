subroutine updmorfac(morpar, timhr, refjulday)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
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
! update morfac in case of varyingmorfac
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use morphology_data_module, only: morpar_type
    use table_handles
    !
    implicit none
!
! Arguments
!
    type(morpar_type)                    , intent(inout) :: morpar
    real(fp)                             , intent(in)    :: timhr
    integer                              , intent(in)    :: refjulday
!
! Local variables
!
    real(fp), dimension(1)              :: value
    character(256)                      :: errmsg
!
!! executable statements -------------------------------------------------------
!
    !
    ! Obtain new value of time-varying morfac
    !
    call gettabledata(morpar%morfacfile, &
           & morpar%morfactable, &
           & morpar%morfacpar, 1, &
           & morpar%morfacrec, value, &
           & timhr, refjulday, errmsg)
    morpar%morfac = value(1)
end subroutine updmorfac
