subroutine jet3d2flow(thick  ,kmax   ,dps    ,s1     ,disch_nf ,sour_nf , &
                    & lstsci ,lsal   ,ltem   ,xz     ,yz       ,nmmax   , &
                    & kcs    ,flwang ,sign   ,gdp    )
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
!  $Id: jet3d2flow.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/jet3d2flow.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Converts Jet3D output to delft3d sources following the DESA
!              methodology of Joseph Lee
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!
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
    integer           , pointer :: m_diff
    integer           , pointer :: n_diff
!
! Global variables
!
    integer                                                    , intent(in)  :: kmax     !  Description and declaration in tricom.igs
    integer                                                    , intent(in)  :: lstsci   !  Description and declaration in tricom.igs
    integer                                                    , intent(in)  :: lsal     !  Description and declaration in tricom.igs
    integer                                                    , intent(in)  :: ltem     !  Description and declaration in tricom.igs
    integer                                                    , intent(in)  :: nmmax    !  Description and declaration in tricom.igs
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcs      !  Description and declaration in
    real(fp)                                                                 :: sign     !  Description and declaration in tricom.igs
    real(fp)   ,                                                 intent(in)  :: flwang   !  Description and declaration in esm_alloc_real.f90 gs
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
    integer                                  :: irow
    integer                   , external     :: newlun
    integer                                  :: nm_diff
    integer                                  :: nrow
    integer                                  :: luntmp
    real(fp)                                 :: rdum
    real(fp)                                 :: xxx
    real(fp)                                 :: yyy
    real(fp) , dimension(:)   , allocatable  :: x_jet
    real(fp) , dimension(:)   , allocatable  :: y_jet
    real(fp) , dimension(:)   , allocatable  :: z_jet
    real(fp) , dimension(:)   , allocatable  :: b_jet
    real(fp) , dimension(:)   , allocatable  :: s_jet
    real(fp) , dimension(:)   , allocatable  :: v_jet
!
!! executable statements -------------------------------------------------------
!
    m_diff         => gdp%gdnfl%m_diff
    n_diff         => gdp%gdnfl%n_diff
    !
    call n_and_m_to_nm(n_diff, m_diff, nm_diff, gdp)
    !
    ! Open Jet3d output file and read jet characteristics end of near field
    !
    luntmp = newlun (gdp)
    open (luntmp,file='str3dtek.xxx',status='old')
    !
    call skipstarlines (luntmp)
    read (luntmp,'( )',end = 999)
    !
    read (luntmp,*) nrow
    !
    allocate (x_jet(nrow))
    allocate (y_jet(nrow))
    allocate (z_jet(nrow))
    allocate (b_jet(nrow))
    allocate (s_jet(nrow))
    allocate (v_jet(nrow))
    !
    do irow = 1, nrow
       read (luntmp,*)       rdum       , x_jet(irow), y_jet(irow), z_jet(irow), &
                           & s_jet(irow), b_jet(irow), v_jet(irow)
    enddo
    !
    ! Rising or sinking plume: determine terminal level
    !
    irow = 2
    if (z_jet(2) >= z_jet(1)) then
       do while (z_jet(irow) >= z_jet(irow-1).and. irow < nrow)
          irow = irow + 1
       enddo
    elseif (z_jet(2) <= z_jet(1)) then
       do while (z_jet(irow) <= z_jet(irow-1) .and. irow < nrow)
          irow = irow + 1
       enddo
    endif
    !
    nrow = irow
    !
    ! Jet3d relative to main flow direction, convert coordinates back to orignal coordinate system
    !
    do irow = 1, nrow
       xxx = x_jet(irow)*cos(flwang*degrad) - sign*y_jet(irow)*sin(flwang*degrad)
       yyy = x_jet(irow)*sin(flwang*degrad) + sign*y_jet(irow)*cos(flwang*degrad)
       x_jet(irow) = xz(nm_diff) + xxx
       y_jet(irow) = yz(nm_diff) + yyy
       z_jet(irow) = real(dps(nm_diff),fp) - z_jet(irow)
    enddo
    !
    ! Fill sources and sinks following the Desa Method of Prof. Lee
    !
    call desa(x_jet   ,y_jet    ,z_jet   ,s_jet   ,nrow    , &
            & kcs     ,xz       ,yz      ,dps     ,s1      , &
            & nmmax   ,thick    ,kmax    ,lstsci  ,lsal    , &
            & ltem    ,disch_nf ,sour_nf ,gdp     )
    !        & ltem    ,disch_nf ,sour_nf ,b_jet   ,v_jet   ,gdp     )
    !
    ! Deallocate temporary arrays
    !
    deallocate (x_jet)
    deallocate (y_jet)
    deallocate (z_jet)
    deallocate (b_jet)
    deallocate (s_jet)
    deallocate (v_jet)
    !
999 continue
    !
    close (luntmp)
    !
end subroutine jet3d2flow
