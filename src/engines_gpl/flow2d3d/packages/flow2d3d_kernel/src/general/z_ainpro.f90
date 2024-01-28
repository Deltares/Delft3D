function z_ainpro(vect1     ,vect2     ,kmax      ,kfs      ,kfsz0     , &
                & icx       ,icy       ,gdp       )
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
! Computes inproduct between two vectors.
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
    integer , pointer :: m1_nhy
    integer , pointer :: m2_nhy
    integer , pointer :: n1_nhy
    integer , pointer :: n2_nhy
!
! Global variables
!
    integer                                         , intent(in) :: icx
    integer                                         , intent(in) :: icy
    integer                                         , intent(in) :: kmax     !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfs      !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: kfsz0    !  Description and declaration in esm_alloc_int.f90
    real(fp)                                                     :: z_ainpro
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: vect1
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: vect2
!
! Local variables
!
    integer :: ddb
    integer :: icxy
    integer :: k
    integer :: m
    integer :: ndelta
    integer :: nm
    integer :: nmst
    integer :: nmstart
    real(fp):: sum
!
!! executable statements -------------------------------------------------------
!
    m1_nhy  => gdp%gdnonhyd%m1_nhy
    m2_nhy  => gdp%gdnonhyd%m2_nhy
    n1_nhy  => gdp%gdnonhyd%n1_nhy
    n2_nhy  => gdp%gdnonhyd%n2_nhy
    !
    ddb = gdp%d%ddbound
    icxy = max(icx, icy)
    !
    ndelta = n2_nhy - n1_nhy
    nmstart = (n1_nhy + ddb) + (m1_nhy - 1 + ddb)*icxy
    !
    sum = 0.0
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
          if (kfs(nm) == 1) then
             do k = 1, kmax
                if (kfsz0(nm, k)/=0) then
                   sum = sum + vect1(nm, k)*vect2(nm, k)
                endif
             enddo
          endif
       enddo
    enddo
    z_ainpro = sum
end function z_ainpro
