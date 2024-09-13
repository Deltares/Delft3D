subroutine near_field(u1    ,v1      ,rho    ,thick ,kmax  ,alfas ,dps   ,&
                    & s1    ,disch_nf,sournf ,lstsci,lsal  ,ltem  ,xz    ,&
                    & yz    ,nmmax   ,kcs    ,kcs_nf,r1    ,gdp   )
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
!  $Id: near_field.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/near_field.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Converts flow results to cormix input
!              Left to do:
!              1) interpolate to equidistant layer distribution jet3d
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
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer           , pointer :: m_diff
    integer           , pointer :: n_diff
    integer           , pointer :: m_amb
    integer           , pointer :: n_amb
    integer           , pointer :: idensform
    real(fp)          , pointer :: q_diff
    real(fp)          , pointer :: t0_diff
    real(fp)          , pointer :: s0_diff
    real(fp)          , pointer :: rho0_diff
    real(fp)          , pointer :: d0
    real(fp)          , pointer :: h0
    real(fp)          , pointer :: sigma0
    real(fp)          , pointer :: theta0
    character(256)    , pointer :: nflmod
!
! Global variables
!
    integer                                                    , intent(in)  :: kmax     !  Description and declaration in
    integer                                                    , intent(in)  :: lstsci
    integer                                                    , intent(in)  :: lsal
    integer                                                    , intent(in)  :: ltem
    integer                                                    , intent(in)  :: nmmax
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcs      !
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kcs_nf   !
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: alfas    !
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s1       !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: xz       !  Description and declaration in
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: yz       !  Description and declaration in
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(out) :: disch_nf !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: rho      !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: u1       !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: v1       !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax,lstsci) , intent(in)  :: r1       !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax,lstsci) , intent(out) :: sournf   !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(kmax)                               , intent(in)  :: thick    !  Description and declaration in esm_alloc_real.f90
    real(prec) , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: dps      !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    real(fp)           :: flwang
    real(fp)           :: sign
    logical            :: corend
    logical            :: first_time
!
!! executable statements -------------------------------------------------------
!
    data    first_time /.true./
    save    first_time
    !
    idensform      => gdp%gdphysco%idensform
    m_diff         => gdp%gdnfl%m_diff
    n_diff         => gdp%gdnfl%n_diff
    m_amb          => gdp%gdnfl%m_amb
    n_amb          => gdp%gdnfl%n_amb
    q_diff         => gdp%gdnfl%q_diff
    t0_diff        => gdp%gdnfl%t0_diff
    s0_diff        => gdp%gdnfl%s0_diff
    rho0_diff      => gdp%gdnfl%rho0_diff
    d0             => gdp%gdnfl%d0
    h0             => gdp%gdnfl%h0
    sigma0         => gdp%gdnfl%sigma0
    theta0         => gdp%gdnfl%theta0
    nflmod         => gdp%gdnfl%nflmod
    !
    ! Convert flow results (velocities and densities) at (mdiff,ndiff) to nearfield input
    ! and write cormix/nrfield input file
    !
    select case (nflmod)
       case('cormix')
          if (first_time) then
             first_time = .false.
             call corinp_gen(idensform,gdp)
          endif
          !
          ! Convert flow results to input for cormix and write to input file
          !
          call wri_cormix(u1    ,v1    ,rho   ,thick ,kmax  ,dps   , &
                        & s1    ,alfas ,gdp                        )
          !
          ! Do the Cormix simulation
          !
          corend = .false.
          call util_system('cormix.bat')
          do while (.not. corend)
             inquire (file='cormixrun.end',exist=corend)
          enddo
          !
          ! Finally convert cormix results to flow input
          !
          call cormix2flow(thick  ,kmax   ,dps   ,s1   ,disch_nf ,sournf , &
                         & lstsci ,lsal   ,ltem  ,xz   ,yz       ,nmmax  , &
                         & kcs    ,gdp   )
       case ('jet3d')
          !
          ! Convert flow results to input for jet3d and write to input file
          !
          call wri_jet3d(u1    ,v1    ,rho    ,thick ,kmax      ,dps   , &
                       & s1    ,alfas ,flwang ,sign  ,idensform ,gdp   )
          !
          ! Do the Jet3d simulation
          !
          corend = .false.
          call util_system('jet3d.bat')
          do while (.not. corend)
             inquire (file='jet3drun.end',exist=corend)
          enddo
          !
          ! Finally convert Jet3D results to flow input
          ! The appoach followed is the DESA approach as suggested by Prof. Lee
          ! Clean up temporary files afterwards
          !
          call jet3d2flow(thick  ,kmax   ,dps  ,s1   ,disch_nf ,sournf, &
                        & lstsci ,lsal   ,ltem ,xz   ,yz       ,nmmax  ,&
                        & kcs    ,flwang ,sign ,gdp  )
          call util_system('del str3dinp.xxx')
          call util_system('del str3dprt.xxx')
          call util_system('del str3dtek.xxx')
          call util_system('del jet3drun.end')
       case ('nrfield')
       !
       ! Nothing (yet)
       !
    end select
    !
end subroutine near_field
