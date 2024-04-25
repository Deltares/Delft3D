subroutine compvel(wavetime ,layer_model ,flowVelocityType ,kfu       ,kfv     , &
                 & u1       ,mmax        ,nmax             ,kmax      ,filnam  , &
                 & dps      ,s1          ,thick            ,dzu1      ,rbuff   )
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use wave_data
    !
    implicit none
!
! Global parameters
!
    integer                             , intent(in)  :: mmax
    integer                             , intent(in)  :: nmax
    integer                             , intent(in)  :: kmax
    integer, dimension(mmax, nmax)      , intent(in)  :: kfu
    integer, dimension(mmax, nmax)      , intent(in)  :: kfv
    real   , dimension(kmax)            , intent(in)  :: thick
    real   , dimension(mmax, nmax)      , intent(in)  :: dps
    real   , dimension(mmax, nmax)      , intent(in)  :: s1
    real   , dimension(mmax, nmax)      , intent(out) :: u1
    real   , dimension(mmax, nmax, kmax), intent(in)  :: dzu1
    real   , dimension(nmax, mmax, kmax), intent(in)  :: rbuff
    type(wave_time_type)                              :: wavetime
    integer                                           :: flowVelocityType
    character(*)                                      :: filnam
    character(*)                                      :: layer_model
!
! Local parameters
!
    integer                                 :: k
    integer                                 :: m
    integer                                 :: n
    real                                    :: dep
    !
    if (layer_model(1:11) == 'SIGMA-MODEL') then
        !
        ! FLOW model used Sigma-layering in the vertical
        !
        if ( flowVelocityType == FVT_SURFACE_LAYER ) then
           do m = 1, mmax
              do n = 1, nmax
                 u1(m, n) = rbuff(n, m, 1)
              enddo
           enddo
        elseif ( flowVelocityType == FVT_DEPTH_AVERAGED ) then
           do m = 1, mmax
              do n = 1, nmax
                 u1(m, n) = 0.0
                 do k = 1,kmax
                    u1(m, n) = u1(m, n) + thick(k) * rbuff(n, m, k)
                 enddo
              enddo
           enddo
        elseif ( flowVelocityType == FVT_WAVE_DEPENDENT ) then
           call wavcur2d(wavetime  ,layer_model ,kfu       ,kfv       , &
                       & u1        ,mmax        ,nmax      ,kmax      , &
                       & filnam    ,dps         ,s1        ,thick     , &
                       & dzu1      ,rbuff       )
        else
           call wavestop(1, 'compvel: flowVelocityType value unrecognized')
        endif
    elseif (layer_model(1:7) == 'Z-MODEL') then
        !
        ! FLOW model used Z-layering in the vertical
        !
        if ( flowVelocityType == FVT_SURFACE_LAYER ) then
           do m = 1, mmax
              do n = 1, nmax
                 do k = kmax, 1, -1
                    if (dzu1(m, n, k) > 0.0) then
                       u1(m, n) = rbuff(n, m, k)
                       exit
                    endif
                 enddo
              enddo
           enddo
        elseif ( flowVelocityType == FVT_DEPTH_AVERAGED ) then
           do m = 1, mmax
              do n = 1, nmax
                 u1(m, n) = 0.0
                 dep      = 0.0 
                 do k = 1, kmax
                    u1(m, n) = u1(m, n) + dzu1(m, n, k) * rbuff(n, m, k)
                    dep      = dep + dzu1(m, n, k)
                 enddo
                 dep     = max(dep, 0.01)
                 u1(m,n) = u1(m,n)/dep
              enddo
           enddo
        elseif ( flowVelocityType == FVT_WAVE_DEPENDENT ) then
           call wavcur2d(wavetime  ,layer_model ,kfu       ,kfv       , &
                       & u1        ,mmax        ,nmax      ,kmax      , &
                       & filnam    ,dps         ,s1        ,thick     , &
                       & dzu1      ,rbuff       )
        else
           call wavestop(1, 'compvel: flowVelocityType value unrecognized')
        endif
    else
       !
       ! Erroneous vertical layering definition found on COM-FILE
       !
       write(*, '(2a)') 'compvel: Erroneous vertical layering definition found on COM-FILE: LAYER_MODEL = ', trim(layer_model)
       call wavestop(1, 'compvel: Erroneous vertical layering definition found on COM-FILE: LAYER_MODEL = '//trim(layer_model))
    endif
end subroutine compvel
