module m_d3d_chktrt
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2026.                                
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
!-------------------------------------------------------------------------------

implicit none 

private
public :: d3d4_chktrt

contains

subroutine d3d4_chktrt(lundia, error, kcu, kcv, gdp)
    use globaldata
    use trachytopes_data_module, only: trachy_type
    use grid_dimens_module, only: griddimtype
    use m_trtrou, only: chktrt

    ! Arguments
    type(globdat), target :: gdp
    integer, intent(in) :: lundia
    logical, intent(out) :: error
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: kcu
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: kcv
    
    ! Local variables
    integer :: m, n, nm

    do m = gdp%d%mlb, gdp%d%mub
        do n = gdp%d%nlb, gdp%d%nub
            call n_and_m_to_nm(n, m, nm, gdp)
            gdp%gdtrachy%dir(1)%kcu_trt(nm) = kcu(n, m)
            gdp%gdtrachy%dir(2)%kcu_trt(nm) = kcv(n, m)
        end do
    end do

    call chktrt(lundia, error, gdp%griddim, &
                gdp%gdtrachy, gdp%gdbedformpar%flnmD50, gdp%gdbedformpar%flnmD90, &
                gdp%gdbedformpar%lfbedfrmrou, gdp%gdprocs%sedim, gdp%d%ddbound)

end subroutine d3d4_chktrt

end module m_d3d_chktrt