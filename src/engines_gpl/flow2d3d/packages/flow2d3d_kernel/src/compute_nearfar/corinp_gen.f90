subroutine corinp_gen(idensform, gdp)
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
!  $Id: corinp_gen.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/corinp_gen.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Reads the Cormix input file
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!
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
    integer                :: idensform
!
! Local variables
!
    integer                :: luntmp
    integer, external      :: newlun
    real(fp)               :: dummy
!
!! executable statements -------------------------------------------------------
!
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
    luntmp = newlun(gdp)
    open (luntmp,file='corinp.dat')
    !
    ! Read position diffusor
    !
    call skipstarlines (luntmp)
    read (luntmp,*) m_diff
    call skipstarlines (luntmp)
    read (luntmp,*) n_diff
    !
    ! Read position ambient conditions
    !
    call skipstarlines (luntmp)
    read (luntmp,*) m_amb
    call skipstarlines (luntmp)
    read (luntmp,*) n_amb
    !
    ! Read discharge characteristics
    !
    call skipstarlines (luntmp)
    read (luntmp,*) q_diff
    call skipstarlines (luntmp)
    read (luntmp,*) t0_diff
    call skipstarlines (luntmp)
    read (luntmp,*) s0_diff
    select case (idensform)
       case( dens_Eckart )
          call dens_eck    (t0_diff, s0_diff,rho0_diff, dummy, dummy)
       case( dens_Unesco)
          call dens_unes   (t0_diff, s0_diff,rho0_diff, dummy, dummy)
    end select
    !
    ! Read remainder of cormix general input
    !
    call skipstarlines (luntmp)
    read (luntmp,*) d0
    call skipstarlines (luntmp)
    read (luntmp,*) h0
    call skipstarlines (luntmp)
    read (luntmp,*) theta0
    call skipstarlines (luntmp)
    read (luntmp,*) sigma0
    !
    ! Close the general cormix input file
    !
    close (luntmp)
    !
end subroutine corinp_gen
