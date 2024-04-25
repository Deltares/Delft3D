subroutine D3S_setup(componentID  , runid  , lundia      , &
                     confFile     , &
                     mlb          , mub    , &
                     nlb          , nub    , &
                     xcor         , ycor   , &
                     nto          , nambnd , mnbnd       , &
                     nostat       , namst  , mnstat      , mask, &
                     Tstart_Julian, delta_T, numTimeSteps, &
                     errstring    ,success      ) 
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use D3D_Sobek 
    use precision 
    !
    implicit none
!
! arguments
!
    integer                                        , intent(in) :: lundia         ! Unit number of diagnosis file
    integer                                        , intent(in) :: mlb
    integer                                        , intent(in) :: nlb
    integer                                        , intent(in) :: mub
    integer                                        , intent(in) :: nub
    integer                                        , intent(in) :: nto
    integer           , dimension(7, nto)          , intent(in) :: mnbnd
    integer                                        , intent(in) :: nostat
    integer           , dimension(2, nostat)       , intent(in) :: mnstat
    integer                                        , intent(in) :: numTimeSteps
    integer           , dimension(nto)                          :: mask           ! to identify boundaries that are coupled to sobek: mask=1
    real(fp)          , dimension(nlb:nub,mlb:mub) , intent(in) :: xcor
    real(fp)          , dimension(nlb:nub,mlb:mub) , intent(in) :: ycor
    real(hp)                                       , intent(in) :: delta_T
    real(hp)                                       , intent(in) :: Tstart_Julian   
    character(*)                                   , intent(in) :: runid
    character(*)                                   , intent(in) :: componentID
    character(*)                                   , intent(in) :: confFile      ! configuration file name
    character(Len=20) , dimension(nto)             , intent(in) :: nambnd
    character(Len=20) , dimension(nostat)          , intent(in) :: namst
    character(256)                                              :: errstring 
    logical                                                     :: success
!
!locals 
!
    integer                                       :: i
    integer                                       :: j
    real(sp)       , dimension(:,:) , allocatable :: xcor_sp
    real(sp)       , dimension(:,:) , allocatable :: ycor_sp

!
!! executable statements -------------------------------------------------------
!
    write(*,'(a)') 'Initializing communication with SOBEKSIM ...' 
    allocate(xcor_sp(nlb:nub,mlb:mub))
    allocate(ycor_sp(nlb:nub,mlb:mub))
    do j=nlb,nub
       do i=mlb,mub
          xcor_sp(j,i) = real(xcor(j,i),sp)
          ycor_sp(j,i) = real(ycor(j,i),sp)
       enddo
    enddo
    success = D3S_Init( componentID  , runid  , &
                        confFile     , &
                        mlb          , mub    , &
                        nlb          , nub    , &
                        xcor_sp      , ycor_sp, &
                        nto          , nambnd , mnbnd       , &
                        nostat       , namst  , mnstat      , mask, &
                        Tstart_Julian, delta_T, numTimeSteps) 
    deallocate(xcor_sp)
    deallocate(ycor_sp)
    call D3S_LastError(errstring)
    write(lundia,'(5x,a)') 'The following boundaries are coupled to SobekSIM. Values from input files are not used:'
    do i=1,nto
       if (mask(i) == 1) then
          write(lundia,'(5x,a)') nambnd(i)
       endif
    enddo
end subroutine D3S_setup
