subroutine z_lu(bbka      ,bbk       ,bbkc      ,kmax      ,icx       , &
              & icy       ,nmmax     ,kfsz0     ,pbbk      ,pbbkc     , &
              & kfs       ,kfsmin    ,kfsmx0    ,gdp       )
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
! Computes LU decomposition
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
    integer                                          , intent(in)  :: icx
    integer                                          , intent(in)  :: icy
    integer                                          , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                        :: nmmax  !  Description and declaration in dimens.igs
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                     :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(in)  :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(in)  :: kfsmx0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax) , intent(in)  :: kfsz0  !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax) , intent(in)  :: bbk
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax) , intent(in)  :: bbka
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax) , intent(in)  :: bbkc
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax) , intent(out) :: pbbk
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: pbbkc
!
! Local variables
!
    integer :: ddb
    integer :: icxy
    integer :: k
    integer :: mink
    integer :: m
    integer :: ndelta
    integer :: nm
    integer :: nmst
    integer :: nmstart
    real(fp):: bi
!
!! executable statements -------------------------------------------------------
!
    m1_nhy  => gdp%gdnonhyd%m1_nhy
    m2_nhy  => gdp%gdnonhyd%m2_nhy
    n1_nhy  => gdp%gdnonhyd%n1_nhy
    n2_nhy  => gdp%gdnonhyd%n2_nhy
    !
    ddb  = gdp%d%ddbound
    icxy = max(icx, icy)
    !
    ndelta  = n2_nhy - n1_nhy
    nmstart = (n1_nhy + ddb) + (m1_nhy - 1 + ddb)*icxy
    !
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
          if (kfs(nm) == 1) then
             mink = kfsmin(nm)
             if (kfsz0(nm, mink) /= 0) then
                bi              = 1.0_fp / bbk(nm, mink)
                pbbk (nm, mink) = bi
                pbbkc(nm, mink) = bbkc(nm, mink) * bi
             endif
             !
             do k = mink+1, kfsmx0(nm)
                if (kfsz0(nm, k) /= 0) then
                   bi           = 1.0_fp / (bbk(nm, k) - bbka(nm, k)*pbbkc(nm, k - 1))
                   pbbk (nm, k) = bi
                   pbbkc(nm, k) = bbkc(nm, k) * bi
                endif
             enddo
          endif
       enddo
    enddo
end subroutine z_lu
