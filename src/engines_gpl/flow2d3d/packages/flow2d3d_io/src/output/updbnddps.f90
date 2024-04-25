subroutine updbnddps(flupd     ,dstep     ,j         ,nmmaxj    ,kmax      , &
               & nsrc      ,qu        ,qv        ,discum    ,gdp       )
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
!    Function: Update cummelative values for discharges QU, QV
!              and DISCUM
!              DSTEP in 1 / (twice the time step interval in
!              equivalents of DT) or DSTEP = 0. to initialize
! Method used:
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
!
! Global variables
!
    integer                            , intent(in)  :: j      !!  Begin pointer for arrays which have been transformed into 1D arrays. Due to the shift in the 2nd (M-) index, JSTART = -2*NMAX + 1
    integer                            , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                            , intent(in)  :: nmmaxj !  Description and declaration in dimens.igs
    integer                            , intent(in)  :: nsrc   !  Description and declaration in esm_alloc_int.f90
    logical                            , intent(in)  :: flupd  !!  Flag to update (true) or initialize (false) the discharge arrays
    real(fp)                           , intent(in)  :: dstep  !!  1. / total number of timesteps (interval to write comm. file)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax) :: qu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax) :: qv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                        :: discum !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer :: k  ! Loop counter 1,KMAX 
    integer :: n  ! Loop counter 1,NSRC 
    integer :: nm ! Loop counter J,NMMAXJ 
!
!! executable statements -------------------------------------------------------
!
    ! For timestep NST = ITCOMC before writing to communication file
    ! because QU, QV and DISCUM are calculated every half DT.
    ! devide QU, QV and DISCUM by (2 * ITCOMI). here DSTEP=1/(2*ITCOMI)
    !
    if (flupd) then
       do nm = j, nmmaxj
          do k = 1, kmax
             qu(nm, k) = qu(nm,k) * dstep
             qv(nm, k) = qv(nm,k) * dstep
          enddo
       enddo
       do n = 1, nsrc
          discum(n) = discum(n) * dstep
       enddo
    else
       !
       ! At timestep NST = ITCOMF - ITCOMI or after writing to commu-
       ! cation file there will be a reset of QU, QV and DISCUM to 0.
       !
       do nm = j, nmmaxj
          do k = 1, kmax
             qu(nm, k) = 0.0_fp
             qv(nm, k) = 0.0_fp
          enddo
       enddo
       do n = 1, nsrc
          discum(n) = 0.0_fp
       enddo
    endif
end subroutine updbnddps
