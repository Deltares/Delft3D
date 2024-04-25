subroutine coupled(nlb     ,nub     ,mlb     ,mub   ,add   , &
                 & r0      ,kmax    ,lstsci  ,lcon  ,thick , &
                 & m_intake,n_intake,k_intake,s0    ,dps   , &
                 & dzs0    ,kfsmn0  ,kfsmx0  ,zmodel,gdp   )
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
!  $Id: coupled.f90 5939 2016-03-11 18:00:21Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160128_34357_NearField_Coupling/src/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/coupled.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Coupled intakes/outlets for cortime to flow coupling
! Method used:
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
! Global variables
!
    integer                                                    , intent(in)  :: nlb
    integer                                                    , intent(in)  :: nub
    integer                                                    , intent(in)  :: mlb
    integer                                                    , intent(in)  :: mub
    integer                                                    , intent(in)  :: kmax
    integer                                                    , intent(in)  :: lstsci
    integer                                                    , intent(in)  :: lcon
    integer                                                    , intent(in)  :: m_intake
    integer                                                    , intent(in)  :: n_intake
    integer                                                    , intent(in)  :: k_intake
    integer    , dimension(nlb:nub,mlb:mub)                    , intent(in)  :: kfsmx0     ! Description and declaration in esm_alloc_int.f90
    integer    , dimension(nlb:nub,mlb:mub)                    , intent(in)  :: kfsmn0     ! Description and declaration in esm_alloc_int.f90
    real(fp)                                                   , intent(out) :: add
    real(fp)   , dimension(nlb:nub,mlb:mub)                    , intent(in)  :: s0
    real(prec) , dimension(nlb:nub,mlb:mub)                    , intent(in)  :: dps
    real(fp)   , dimension(nlb:nub,mlb:mub, kmax)              , intent(in)  :: dzs0       ! Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(nlb:nub,mlb:mub, kmax,lstsci)       , intent(in)  :: r0         ! Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(kmax)                               , intent(in)  :: thick      ! Description and declaration in esm_alloc_real.f90
    logical                                                    , intent(in)  :: zmodel
!
! Local variables
!
    integer :: k
!
!! executable statements -------------------------------------------------------
!
    add = 0.0_fp
    if (m_intake > 0) then
       if (k_intake == 0) then
          if (.not. zmodel) then
             do k = 1, kmax
                add = add + thick(k)*r0(n_intake, m_intake,k,lcon)
             enddo
          else
             do k = kfsmn0(n_intake, m_intake), kfsmx0(n_intake, m_intake)
                add = add + dzs0(n_intake, m_intake,k)*r0(n_intake, m_intake,k,lcon)/max(s0(n_intake, m_intake)+real(dps(n_intake, m_intake),fp),0.01_fp)
             enddo
          endif
       else
          add = r0(n_intake, m_intake,k_intake,lcon)
       endif
    endif
end subroutine coupled
