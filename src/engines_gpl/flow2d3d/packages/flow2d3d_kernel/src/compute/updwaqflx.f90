subroutine updwaqflx(nst       ,zmodel    ,nmmax     ,kmax      ,kcs       , &
                   & kcu       ,kcv       ,qxk       ,qyk       ,qzk       , &
                   & nsrc      ,disch     ,gdp       )
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
!    Function: Update cumulative fluxes for WAQ
!
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
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                 , pointer :: itwqff
    integer                 , pointer :: itwqfl
    real(fp), dimension(:,:), pointer :: quwaq     ! Cumulative qxk
    real(fp), dimension(:,:), pointer :: qvwaq     ! Cumulative qyk
    real(fp), dimension(:,:), pointer :: qwwaq     ! Cumulative qzk
    real(fp), dimension(:)  , pointer :: discumwaq ! Cumulated sources m3/s*nstep 
    logical                 , pointer :: waqfil
!
! Global variables
!
    integer                                                  :: nsrc   !  Description and declaration in esm_alloc_int.f90
    integer                                                  :: nst    !!  Time step number
    integer                                                  :: nmmax  !  Description and declaration in esm_alloc_int.f90
    integer                                                  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)               :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)               :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)               :: kcv    !  Description and declaration in esm_alloc_int.f90
    logical                                                  :: zmodel !  Description and declaration in procs.igs
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)       :: qzk    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: qxk    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: qyk    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                                :: disch  !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer :: k
    integer :: n
    integer :: nm
!
!! executable statements -------------------------------------------------------
!
    itwqff     => gdp%gdwaqpar%itwqff
    itwqfl     => gdp%gdwaqpar%itwqfl
    quwaq      => gdp%gdwaqpar%quwaq
    qvwaq      => gdp%gdwaqpar%qvwaq
    qwwaq      => gdp%gdwaqpar%qwwaq
    discumwaq  => gdp%gdwaqpar%discumwaq 
    waqfil     => gdp%gdwaqpar%waqfil
    !
    if (.not.waqfil) return
    if (nst<itwqff .or. nst>=itwqfl) return
    !
    ! calculate cumulative discharge
    !
    do nm = 1, nmmax
       if (kcu(nm) /= 0) then
          do k = 1, kmax
             quwaq(nm, k) = quwaq(nm, k) + qxk(nm, k)
          enddo
       endif
       if (kcv(nm) /= 0) then
          do k = 1, kmax
             qvwaq(nm, k) = qvwaq(nm, k) + qyk(nm, k)
          enddo
       endif
       if (kcs(nm) /= 0 .and. kmax>1) then
          do k = 0, kmax
             qwwaq(nm, k) = qwwaq(nm, k) + qzk(nm, k)
          enddo
       endif
    enddo
    !
    ! calculate cum. discharge in discharge points
    !
    do n = 1, nsrc
       discumwaq(n) = discumwaq(n) + disch(n)
    enddo
end subroutine updwaqflx
