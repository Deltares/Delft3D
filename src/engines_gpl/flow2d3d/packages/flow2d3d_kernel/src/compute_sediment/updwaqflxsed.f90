subroutine updwaqflxsed(nst       ,nm        ,l         ,trndiv    ,sedflx    , &
                      & eroflx    ,gdp       )
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
!    Function: Update cumulative sedimentation and resuspension fluxes for WAQ
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
    real(fp), dimension(:,:), pointer :: cumsedflx ! Cumulative sedimentation flux
    real(fp), dimension(:,:), pointer :: cumresflx ! Cumulative resuspension flux
    logical                 , pointer :: waqfil
!
! Global variables
!
    integer                                                  :: nst    !!  Time step number
    integer                                                  :: nm     !!  Grid point
    integer                                                  :: l      !!  Sediment fraction
    real(fp)                                                 :: trndiv !!  Deposition flux due to divergence of bed/total load
    real(fp)                                                 :: sedflx !!  Sedimentation flux
    real(fp)                                                 :: eroflx !!  Erosion flux
!
! Local variables
!
!   NONE
!
!! executable statements -------------------------------------------------------
!
    itwqff     => gdp%gdwaqpar%itwqff
    itwqfl     => gdp%gdwaqpar%itwqfl
    cumsedflx  => gdp%gdwaqpar%cumsedflx
    cumresflx  => gdp%gdwaqpar%cumresflx
    waqfil     => gdp%gdwaqpar%waqfil
    !
    if (.not.waqfil) return
    if (nst<itwqff .or. nst>=itwqfl) return
    !
    ! calculate cumulative sedimentation and resuspension fluxes
    !
    cumsedflx(nm, l) = cumsedflx(nm, l) + sedflx
    cumresflx(nm, l) = cumresflx(nm, l) + eroflx
    !
    if (trndiv>0) then
       cumsedflx(nm, l) = cumsedflx(nm, l) + trndiv
    else
       cumresflx(nm, l) = cumresflx(nm, l) - trndiv
    endif
end subroutine updwaqflxsed
