subroutine initbcdat(gdp)
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp 
!
!! executable statements -------------------------------------------------------
!
gdp%gdbcdat%timtap   = -999.0_fp
gdp%gdbcdat%depbnd   = -999.0_fp
gdp%gdbcdat%nmskf    = -999
gdp%gdbcdat%nmskl    = -999
gdp%gdbcdat%mmskf    = -999
gdp%gdbcdat%mmskl    = -999
gdp%gdbcdat%ncomp    = -999
gdp%gdbcdat%nsplit   = -999
gdp%gdbcdat%gntoftoq = -999
nullify(gdp%gdbcdat%pindex)
nullify(gdp%gdbcdat%bct_order)
nullify(gdp%gdbcdat%mnbnd_global)
nullify(gdp%gdbcdat%ext_bnd)
gdp%gdbcdat%ascon  = ' '
nullify(gdp%gdbcdat%compnames)
nullify(gdp%gdbcdat%dist_pivot_part)
nullify(gdp%gdbcdat%hydrbcf)
!
end subroutine initbcdat
