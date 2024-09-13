subroutine desa(x_jet   ,y_jet    ,z_jet   ,s_jet   ,nrow    , &
              & kcs     ,xz       ,yz      ,dps     ,s1      , &
              & nmmax   ,thick    ,kmax    ,lstsci  ,lsal    , &
              & ltem    ,disch_nf ,sour_nf ,gdp     )
!              & ltem    ,disch_nf ,sour_nf ,b_jet   ,v_jet   , gdp     )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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
!  $Id: desa.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/desa.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Converts Jet3D output to delft3d sources following the DESA
!              methodology of Joseph Lee
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)          , pointer :: q_diff
    real(fp)          , pointer :: t0_diff
    real(fp)          , pointer :: s0_diff
!
! Global variables
!
    integer                                                    , intent(in)  :: kmax     !  Description and declaration in tricom.igs
    integer                                                    , intent(in)  :: lstsci   !  Description and declaration in tricom.igs
    integer                                                    , intent(in)  :: lsal     !  Description and declaration in tricom.igs
    integer                                                    , intent(in)  :: ltem     !  Description and declaration in tricom.igs
    integer                                                    , intent(in)  :: nmmax    !  Description and declaration in tricom.igs
    integer                                                    , intent(in)  :: nrow     !  Description and declaration in tricom.igs
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcs      !  Description and declaration in esm_alloc_real.f90 gs
    real(fp)   , dimension(nrow)                               , intent(in)  :: x_jet    !  Description and declaration in
    real(fp)   , dimension(nrow)                               , intent(in)  :: y_jet    !  Description and declaration in
    real(fp)   , dimension(nrow)                               , intent(in)  :: z_jet    !  Description and declaration in
    real(fp)   , dimension(nrow)                               , intent(in)  :: s_jet    !  Description and declaration in
!    real(fp)   , dimension(nrow)                               , intent(in)  :: b_jet    !  Description and declaration in
!    real(fp)   , dimension(nrow)                               , intent(in)  :: v_jet    !  Description and declaration in
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s1       !  Description and declaration in esm_alloc_real.f90 gs
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: xz       !  Description and declaration in esm_alloc_real.f90 gs
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: yz       !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(out) :: disch_nf !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax,lstsci) , intent(out) :: sour_nf  !  Description and declaration in
    real(fp)   , dimension(kmax)                               , intent(in)  :: thick    !  Description and declaration in esm_alloc_real.f90 gs
    real(prec) , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: dps      !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer       :: irow
    integer       :: k_end
    integer       :: k_irow
    integer       :: k_last
    integer       :: k_start
    integer       :: lcon
    integer       :: nm_end
    integer       :: nm_irow
    integer       :: nm_last
    integer       :: nm_start
    real(fp)      :: dis_dil
    real(fp)      :: dis_tot
    real(fp)      :: q1
    real(fp)      :: q2
!
!! executable statements -------------------------------------------------------
!
    q_diff         => gdp%gdnfl%q_diff
    t0_diff        => gdp%gdnfl%t0_diff
    s0_diff        => gdp%gdnfl%s0_diff
    !
    dis_dil = 0.0_fp
    dis_tot = 0.0_fp
    !
    ! Get characteristics starting point
    !
    call findnmk(xz      , yz      , dps     , s1       , kcs      ,nmmax   , &
               & thick   , kmax    , x_jet(1), y_jet(1) , z_jet(1) ,nm_start, &
               & k_start , gdp    )
    !
    nm_last = nm_start
    k_last  = k_start
    !
    ! Get characteristics end      point
    !
    call findnmk(xz      , yz      , dps         , s1          , kcs         ,nmmax   , &
                 thick   , kmax    , x_jet(nrow) , y_jet(nrow) , z_jet(nrow) ,nm_end  , &
                 k_end   ,gdp    )
    !
    ! Cycle over points in jet3d output
    !
    disch_nf(1:nmmax, 1:kmax)           = 0.0_fp
    sour_nf (1:nmmax, 1:kmax, 1:lstsci) = 0.0_fp
    !
    do irow = 2, nrow
       !
       ! Get position of point
       !
       call findnmk (xz      , yz      , dps         , s1          , kcs         ,nmmax   , &
                     thick   , kmax    , x_jet(irow) , y_jet(irow) , z_jet(irow) ,nm_irow , &
                     k_irow  ,gdp    )
       !
       if (nm_irow == 0 .or. k_irow == 0) then
          nm_irow = nm_last
          k_irow  = k_last
       endif
       nm_last  = nm_irow
       k_last   = k_irow
       !
       ! Fill disch_nf array: Desa Method, subtract the amount of water corresponding with the dilution
       !                      Keep track of total amounts of water, salt in orer to discahrge the correct
       !                      amounts at the end of the near field
       !
       if (nm_last /= nm_end .or. k_last /= k_end) then
          dis_dil                  = (s_jet(irow) - s_jet(irow-1))*q_diff
!         call det_qjet(q1    ,v_jet(irow-1),b_jet(irow-1)/2.)
!         call det_qjet(q2    ,v_jet(irow)  ,b_jet(irow)/2.  )
!         dis_dil = q2 - q1
!         dis_dil = 0.35814_fp*0.125_fp*(v_jet(irow) + v_jet(irow-1))*(b_jet(irow) + b_jet(irow-1))* &
!                                     & (s_jet(irow) - s_jet(irow-1))
          dis_tot                  = dis_tot + dis_dil
          disch_nf(nm_last,k_last) = disch_nf(nm_last,k_last) - dis_dil
       endif
    enddo
    !
    ! End of near field discharge total amount of diluted plume
    !
    disch_nf (nm_last,k_last)    = disch_nf(nm_last,k_last) + q_diff + dis_tot
    if (lsal /= 0) then
       sour_nf(nm_last,k_last,lsal) = q_diff * s0_diff
    endif
    if (ltem /= 0) then
       sour_nf(nm_last,k_last,ltem) = q_diff * t0_diff
    endif
    do lcon = ltem + 1, lstsci
       sour_nf(nm_last, k_last, lcon) = q_diff * 1.0_fp
    enddo
    !
end subroutine desa
