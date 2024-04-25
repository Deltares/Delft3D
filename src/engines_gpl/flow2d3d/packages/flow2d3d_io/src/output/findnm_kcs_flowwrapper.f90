subroutine findnm_kcs_flowwrapper(xp    , yp    , mp    , np    , &
                                & rmp   , rnp   , inside,spheric, gdp)
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
!    Function: - Locate a point in the Delft3D curvilinear mesh
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
    include 'fsm.i'
    include 'tri-dyn.igd'
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer(pntrsize) , pointer :: xcor
    integer(pntrsize) , pointer :: ycor
    integer(pntrsize) , pointer :: kcs
    integer , pointer :: mlb
    integer , pointer :: mmax
    integer , pointer :: mub
    integer , pointer :: nlb
    integer , pointer :: nmaxus
    integer , pointer :: nub
    real(hp), pointer :: dearthrad
!
! Global variables
!
    integer  , intent(inout) :: mp      ! M index of point (initially last value)
    integer  , intent(inout) :: np      ! N index of point (initially last value)
    real(fp) , intent(in)    :: xp      ! X coordinate of point
    real(fp) , intent(in)    :: yp      ! Y coordinate of point
    real(fp) , intent(out)   :: rmp     ! Fractional M index of point
    real(fp) , intent(out)   :: rnp     ! Fractional N index of point
    logical  , intent(inout) :: inside  ! True if point lies inside grid (or mp,np valid)
    logical  , intent(in)    :: spheric ! Spherical coordinates
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    xcor      => gdp%gdr_i_ch%xcor
    ycor      => gdp%gdr_i_ch%ycor
    kcs       => gdp%gdr_i_ch%kcs
    mlb       => gdp%d%mlb
    mmax      => gdp%d%mmax
    mub       => gdp%d%mub
    nlb       => gdp%d%nlb
    nmaxus    => gdp%d%nmaxus
    nub       => gdp%d%nub
    dearthrad => gdp%gdconstd%dearthrad
    !
    call findnm_kcs (xp    , yp    ,r(xcor),r(ycor), mlb   , mub   , &
                   & nlb   , nub   , mmax  , nmaxus, mp    , np    , &
                   & rmp   , rnp   ,i(kcs) , inside, spheric,dearthrad)
end subroutine findnm_kcs_flowwrapper
