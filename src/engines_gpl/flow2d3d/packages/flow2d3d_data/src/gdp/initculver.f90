 subroutine initculver(gdp       )
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
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                        , pointer :: ifirst
    integer                        , pointer :: nculv
    character(256)                 , pointer :: culverfile
!
!! executable statements -------------------------------------------------------
!
    ifirst           => gdp%gdculver%ifirst
    nculv            => gdp%gdculver%nculv
    culverfile       => gdp%gdculver%culverfile
    !
    ifirst = 1
    nculv = 0
    nullify(gdp%gdculver%arcul)
    nullify(gdp%gdculver%calfa)
    nullify(gdp%gdculver%clcul)
    nullify(gdp%gdculver%cleng)
    nullify(gdp%gdculver%closs1)
    nullify(gdp%gdculver%closs2)
    nullify(gdp%gdculver%closs3)
    nullify(gdp%gdculver%cmann)
    nullify(gdp%gdculver%htcul)
    nullify(gdp%gdculver%numrel1)
    nullify(gdp%gdculver%numrel2)
    nullify(gdp%gdculver%numrel3)
    nullify(gdp%gdculver%poscul)
    nullify(gdp%gdculver%wetar1)
    nullify(gdp%gdculver%wetar2)
    nullify(gdp%gdculver%wetar3)
    nullify(gdp%gdculver%wtcul)
    nullify(gdp%gdculver%dll_name)
    nullify(gdp%gdculver%dll_function)
    nullify(gdp%gdculver%dll_handle)
    nullify(gdp%gdculver%dll_usrfil)
    nullify(gdp%gdculver%dll_integers)
    nullify(gdp%gdculver%dll_reals)
    nullify(gdp%gdculver%dll_strings)
    culverfile = ' '
end subroutine initculver
