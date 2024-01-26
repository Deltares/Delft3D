subroutine init_nfl  (kmax, lstsci, gdp   )
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
!  $Id: init_nfl.f90 6513 2016-08-31 18:15:52Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160128_34357_NearField_Coupling/src/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/init_nfl.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Converts flow results to cormix input
!              nog wel doen:
!              1) interpoleren naar equidistante laagverdeling jet3d
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
    ! They replace the  include igd / include igp lines
    !
    integer                         , pointer :: idensform
    character(256)                  , pointer :: nflmod
    integer                         , pointer :: no_dis
    integer                         , pointer :: no_amb_max
    integer                         , pointer :: itnflri
!
! Global variables
!
    integer                                   :: kmax
    integer                                   :: lstsci
!
! Local variables
!
    integer :: istat
!
!! executable statements -------------------------------------------------------
!
    idensform      => gdp%gdphysco%idensform
    nflmod         => gdp%gdnfl%nflmod
    no_dis         => gdp%gdnfl%no_dis
    no_amb_max     => gdp%gdnfl%no_amb_max
    itnflri        => gdp%gdinttim%itnflri
    !
    ! Initialisation
    !
    select case (nflmod)
       case ('corjet','cortime','generic')
          call det_num_dis(no_dis, no_amb_max, gdp) !FIXME if generic actually is using xml input
       case ('jet3d')
          no_dis = 1
    end select
                  allocate (gdp%gdnfl%m_diff        (no_dis)                                    , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%n_diff        (no_dis)                                    , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%no_amb        (no_dis)                                    , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%m_amb         (no_dis,no_amb_max)                         , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%n_amb         (no_dis,no_amb_max)                         , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%m_intake      (no_dis)                                    , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%n_intake      (no_dis)                                    , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%k_intake      (no_dis)                                    , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%x_diff        (no_dis)                                    , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%y_diff        (no_dis)                                    , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%x_amb         (no_dis,no_amb_max)                         , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%y_amb         (no_dis,no_amb_max)                         , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%x_intake      (no_dis)                                    , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%y_intake      (no_dis)                                    , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%z_intake      (no_dis)                                    , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%q_diff        (no_dis)                                    , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%const_operator(no_dis)                                    , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%const_diff    (no_dis,lstsci)                             , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%rho0_diff     (no_dis)                                    , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%d0            (no_dis)                                    , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%h0            (no_dis)                                    , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%sigma0        (no_dis)                                    , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%theta0        (no_dis)                                    , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%basecase      (no_dis,2)                                  , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%base_path     (no_dis)                                    , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%disnf         (gdp%d%nmlb:gdp%d%nmub, kmax,no_dis)        , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%disnf_intake  (gdp%d%nmlb:gdp%d%nmub, kmax,no_dis)        , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%disnf_entr    (gdp%d%nmlb:gdp%d%nmub, kmax,no_dis)        , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%sournf        (gdp%d%nmlb:gdp%d%nmub, kmax,lstsci,no_dis) , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%nf_src_momu   (gdp%d%nmlb:gdp%d%nmub,kmax,no_dis)         , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%nf_src_momv   (gdp%d%nmlb:gdp%d%nmub,kmax,no_dis)         , stat = istat)
    if(istat==0)  allocate (gdp%gdnfl%nf_const      (lstsci)                                    , stat = istat)
    !
    ! Read corjet/cortime input data from file corinp.dat
    !
    !   select case (nflmod)
    !      case ('corjet','cortime')
    !         call corinp_gen(idensform,gdp)
    !   end select
    !
    gdp%gdnfl%m_diff       = 0
    gdp%gdnfl%n_diff       = 0
    gdp%gdnfl%m_amb        = 0
    gdp%gdnfl%n_amb        = 0
    gdp%gdnfl%m_intake     = 0
    gdp%gdnfl%n_intake     = 0
    gdp%gdnfl%k_intake     = 0
    gdp%gdnfl%x_diff       = 0.0_fp
    gdp%gdnfl%y_diff       = 0.0_fp
    gdp%gdnfl%x_amb        = 0.0_fp
    gdp%gdnfl%y_amb        = 0.0_fp
    gdp%gdnfl%x_intake     = 0.0_fp
    gdp%gdnfl%y_intake     = 0.0_fp
    gdp%gdnfl%z_intake     = 0.0_fp
    gdp%gdnfl%q_diff       = 0.0_fp
    gdp%gdnfl%const_diff   = 0.0_fp
    gdp%gdnfl%rho0_diff    = 0.0_fp
    gdp%gdnfl%d0           = 0.0_fp
    gdp%gdnfl%h0           = 0.0_fp
    gdp%gdnfl%sigma0       = 0.0_fp
    gdp%gdnfl%theta0       = 0.0_fp
    gdp%gdnfl%basecase     = ' '
    gdp%gdnfl%disnf        = 0.0_fp
    gdp%gdnfl%disnf_intake = 0.0_fp
    gdp%gdnfl%disnf_entr   = 0.0_fp
    gdp%gdnfl%sournf       = 0.0_fp
    gdp%gdnfl%nf_src_momu  = 0.0_fp
    gdp%gdnfl%nf_src_momv  = 0.0_fp
    if (itnflri > 0) then
       allocate (gdp%gdnfl%waitfilesold(no_dis), stat = istat)
       gdp%gdnfl%waitfilesold = ' '
    else
       nullify(gdp%gdnfl%waitfilesold)
    endif
    gdp%gdnfl%nf_q_source       = -999.0_fp
    gdp%gdnfl%nf_q_intake       = -999.0_fp
    gdp%gdnfl%nf_const_operator = -999
    gdp%gdnfl%nf_const          = -999.0_fp
    gdp%gdnfl%nf_src_mom        = .false.
    nullify(gdp%gdnfl%nf_intake)
    nullify(gdp%gdnfl%nf_sink)
    nullify(gdp%gdnfl%nf_sour)
    !
    ! TO DO: deallocate all these arrays somewhere
    !
end subroutine init_nfl
