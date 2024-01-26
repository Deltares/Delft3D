subroutine corinp_gen(idensform, gdp)
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
!    Function: Reads input needed for the coupling of Corjet/Cortime/Cormix
!              with Delft3d-Flow
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
    ! They replace the  include igd / include igp lines
    !
    integer                      , pointer :: no_dis
    integer       ,dimension(:)  , pointer :: m_diff
    integer       ,dimension(:)  , pointer :: n_diff
    integer       ,dimension(:)  , pointer :: no_amb
    integer       ,dimension(:,:), pointer :: m_amb
    integer       ,dimension(:,:), pointer :: n_amb
    integer       ,dimension(:)  , pointer :: m_intake
    integer       ,dimension(:)  , pointer :: n_intake
    integer       ,dimension(:)  , pointer :: k_intake
    real(fp)      ,dimension(:)  , pointer :: q_diff
    real(fp)      ,dimension(:,:), pointer :: const_diff
    real(fp)      ,dimension(:)  , pointer :: d0
    real(fp)      ,dimension(:)  , pointer :: h0
    real(fp)      ,dimension(:)  , pointer :: sigma0
    real(fp)      ,dimension(:)  , pointer :: theta0
    character(256),dimension(:,:), pointer :: basecase
!
! Global variables
!
    integer                :: idensform
!
! Local variables
!
    integer                :: luntmp
    integer, external      :: newlun
    integer                :: idis
    real(fp)               :: dummy
    character              :: cdummy
!
!! executable statements -------------------------------------------------------
!
    no_dis         => gdp%gdnfl%no_dis
    m_diff         => gdp%gdnfl%m_diff
    n_diff         => gdp%gdnfl%n_diff
    no_amb         => gdp%gdnfl%no_amb
    m_amb          => gdp%gdnfl%m_amb
    n_amb          => gdp%gdnfl%n_amb
    m_intake       => gdp%gdnfl%m_intake
    n_intake       => gdp%gdnfl%n_intake
    k_intake       => gdp%gdnfl%k_intake
    q_diff         => gdp%gdnfl%q_diff
    const_diff     => gdp%gdnfl%const_diff
    d0             => gdp%gdnfl%d0
    h0             => gdp%gdnfl%h0
    sigma0         => gdp%gdnfl%sigma0
    theta0         => gdp%gdnfl%theta0
    basecase       => gdp%gdnfl%basecase
    !
    open (newunit=luntmp,file='corinp.dat')


    !
    ! Read dummy line
    !

    call skipstarlines (luntmp)
    read (luntmp,*) cdummy
    call skipstarlines (luntmp)
    read (luntmp,*) dummy

    !
    ! For each diffuser
    !
    do idis = 1, no_dis
    !
    ! Read position diffusor
    !
    call skipstarlines (luntmp)
       read (luntmp,*) m_diff(idis)
    call skipstarlines (luntmp)
       read (luntmp,*) n_diff(idis)
    !
    ! Read position ambient conditions
    !
    call skipstarlines (luntmp)
       read (luntmp,*) m_amb(idis,1)
    call skipstarlines (luntmp)
       read (luntmp,*) n_amb(idis,1)
       !
       ! Read intake location
       !
       call skipstarlines (luntmp)
       read (luntmp,*) m_intake(idis)
       call skipstarlines (luntmp)
       read (luntmp,*) n_intake(idis)
       call skipstarlines (luntmp)
       read (luntmp,*) k_intake(idis)
    !
    ! Read discharge characteristics
    !
    call skipstarlines (luntmp)
       read (luntmp,*) q_diff(idis)
    call skipstarlines (luntmp)
       read (luntmp,*) const_diff(idis,1)
    call skipstarlines (luntmp)
       read (luntmp,*) const_diff(idis,2)
    !
    ! Read remainder of cormix general input
    !
    call skipstarlines (luntmp)
       read (luntmp,*) d0(idis)
       call skipstarlines (luntmp)
       read (luntmp,*) h0(idis)
    call skipstarlines (luntmp)
       read (luntmp,*) theta0(idis)
    call skipstarlines (luntmp)
       read (luntmp,*) sigma0(idis)
    call skipstarlines (luntmp)
       read (luntmp,*) basecase(idis,1)
       read (luntmp,*) basecase(idis,2)

    enddo

    !
    ! Close the general cormix input file
    !
    close (luntmp)
    !
end subroutine corinp_gen
