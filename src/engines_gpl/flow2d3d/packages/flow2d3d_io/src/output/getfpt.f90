subroutine getfpt(nmax    ,mmax    ,kmax    ,nofou    ,ifou   ,gdp   )
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
!    Function: determines pointer in the real array
!              for variable FOUNAM(IFOU) and off set for
!              variable FOUNAM(IFOU) layer FLAYNO(IFOU) ,
!              and constituent FCONNO(IFOU)
!
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
    integer          , dimension(:), pointer :: fconno
    integer          , dimension(:), pointer :: flayno
    integer(pntrsize), dimension(:), pointer :: ifoupt
    integer          , dimension(:), pointer :: iofset
    character(16)    , dimension(:), pointer :: founam
!
! Global variables
!
    integer, intent(in)  :: ifou
    integer, intent(in)  :: kmax    !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)  :: mmax    !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)  :: nmax    !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)  :: nofou   !  Description and declaration in dimens.igs
!
! Local variables
!
    integer(pntrsize), external :: getpointer
!
!! executable statements -------------------------------------------------------
!
    fconno   => gdp%gdfourier%fconno
    flayno   => gdp%gdfourier%flayno
    ifoupt   => gdp%gdfourier%ifoupt
    iofset   => gdp%gdfourier%iofset
    founam   => gdp%gdfourier%founam
    !
    ! get pointer and define off set layer and constituent
    !
    ifoupt(ifou) = getpointer(founam(ifou), gdp)
    iofset(ifou) =   (fconno(ifou)-1)*kmax*(nmax+2*gdp%d%ddbound)*(mmax+4+2*gdp%d%ddbound) &
                 & + (flayno(ifou)-1)     *(nmax+2*gdp%d%ddbound)*(mmax+4+2*gdp%d%ddbound)
end subroutine getfpt
