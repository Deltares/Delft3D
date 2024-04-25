subroutine z_kfmnmx(j         ,kmax      ,nm        ,nmref     , &
                  & dep       ,dzmin     ,s1v       ,kfmin     ,kfmax     , &
                  & kf        ,zk        ,dz1       ,gdp       )
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
!
! Global variables
!
    integer                                                      :: j
    integer                                         , intent(in) :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in) :: nm
    integer                                         , intent(in) :: nmref
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                   :: kf
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                   :: kfmax
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                   :: kfmin
    real(fp)                                        , intent(in) :: dep
    real(fp)                                        , intent(in) :: dzmin
    real(fp)                                        , intent(in) :: s1v
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: dz1
    real(fp), dimension(0:kmax)                     , intent(in) :: zk
!
! Local variables
!
    integer :: k
    logical :: found
    logical :: found1
!
!! executable statements -------------------------------------------------------
!
    !
    found = .false.
    !
    do k = 1, kmax
       dz1(nm, k) = dz1(nmref, k)
       if ((zk(k)-dzmin>-dep .or. k==kmax) .and. .not. found) then
          kfmin(nm)  = k
          dz1(nm, k) = dep + zk(k)
          found      = .true.
       else
          if (.not. found) dz1(nm, k) = 0.0_fp
       endif
    enddo
    !
    if (s1v > -dep) then
       !
       !   Wet computational point
       !
       found1 = .false.
       !
       if (found) then
          kfmax(nm) = kfmin(nm)
          do k = kfmin(nm), kmax
             if ((zk(k) + dzmin >= s1v) .or. k == kmax) then
                if (.not. found1) then
                   kfmax(nm) = k
                   found1    = .true.
                endif
                if (k /= kfmin(nm)) then
                   dz1(nm, k) = max(0.0_fp, s1v - max( - dep, zk(k - 1)))
                else
                   dz1(nm, k) = max(0.0_fp, s1v + dep)
                endif
             endif
          enddo
       else
          kfmax(nm) = kfmin(nm)
          !
          ! kfmin(nm) not found but cell contains water: something went wrong
          !
       endif
    else
       !
       ! Dry point
       !
       kfmax(nm) = max(kfmin(nm),1)
       do k = max(kfmin(nm),1), kmax
          dz1(nm, k) = 0.0_fp
       enddo
    endif
    ! 
end subroutine z_kfmnmx
