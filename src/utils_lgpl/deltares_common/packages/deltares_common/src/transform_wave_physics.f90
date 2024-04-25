!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

subroutine transform_wave_physics(    hs        ,dir       ,period    ,depth     , &
                                    & fx        ,fy        ,mx        ,my        , &
                                    & distot    ,dissurf   ,diswcap              , &
                                    & m         ,n         ,hrms      ,tp        , &
                                    & grav      ,swflux    ,swdis                , &
                                    & gamma0    ,wsbodyu   ,wsbodyv   ,ierr          )

    !!--description-----------------------------------------------------------------
    ! NONE
    !!--pseudo code and references--------------------------------------------------
    ! NONE
    !!--declarations----------------------------------------------------------------
    use mathconsts, only: pi_sp, sqrt2_sp, degrad_sp
    use precision
    implicit none
    !
    ! Global variables
    !
    integer                , intent(in)  :: m
    integer                , intent(in)  :: n
    integer                              :: swdis
    real   , dimension(m*n)              :: depth
    real   , dimension(m*n)              :: dir
    real   , dimension(m*n)              :: distot
    real   , dimension(m*n)              :: dissurf
    real   , dimension(m*n)              :: diswcap
    real   , dimension(m*n)              :: fx
    real   , dimension(m*n)              :: fy
    real                   , intent(in)  :: gamma0 ! JONSWAP peak enhancement factor
    real                                 :: grav
    real   , dimension(m*n), intent(out) :: hrms
    real   , dimension(m*n), intent(in)  :: hs
    real   , dimension(m*n), intent(out) :: mx
    real   , dimension(m*n), intent(out) :: my
    real   , dimension(m*n), intent(in)  :: period
    real   , dimension(m*n), intent(out) :: tp
    real   , dimension(m*n)              :: wsbodyu
    real   , dimension(m*n)              :: wsbodyv
    logical                              :: swflux
    integer                              :: ierr
    !
    ! Local variables
    !
    integer                        :: lcount
    integer                        :: npnt
    logical                        :: water_is_too_shallow_or_waves_are_too_small
    real                           :: deph
    real                           :: dirh
    real                           :: dish
    real                           :: diss
    real                           :: fxhis
    real                           :: fxx
    real                           :: fyhis
    real                           :: fyy
    real                           :: hrm
    real                           :: perfac
    real                           :: tpp
    real                           :: wavel
    real                           :: wsbodyuu
    real                           :: wsbodyvv
    !
    !! executable statements -------------------------------------------------------
    !
    perfac = 1.
    call jonswap_mean2peak_period_factor(gamma0, perfac, ierr)
    if (ierr < 0) then
        write(*,'(a,f10.5)') 'ERROR: gamma0 = ',gamma0,' lies outside allowed range [1,20]'
        goto 999
    endif
    !
    ! Start loop
    !
    npnt  = m*n
    do lcount = 1,npnt
        hrm   = hs(lcount)/sqrt2_sp
        dirh  = dir(lcount)
        deph  = depth(lcount)
        tpp   = period(lcount)*perfac
        fxhis = fx(lcount)
        fyhis = fy(lcount)
        dish  = distot(lcount)
        diss  = dissurf(lcount) + diswcap(lcount)
        !
        call wave_length(  hrm, deph, tpp, wavel, water_is_too_shallow_or_waves_are_too_small, grav  )
        !
        ! If .not. swdis use fx, fy from SWAN
        ! else compute forces based on dissipation and celerity
        !
        wsbodyuu = 0.0
        wsbodyvv = 0.0
        call wave_forces(dirh      ,tpp       ,fxhis                , &
                       & fyhis     ,dish      ,diss      ,wavel     , &
                       & water_is_too_shallow_or_waves_are_too_small, &
                       & fxx       ,fyy                             , &
                       & swdis     ,grav      ,wsbodyuu  , wsbodyvv  )
        hrms(lcount)    = hrm
        tp(lcount)      = tpp
        fx(lcount)      = fxx
        fy(lcount)      = fyy
        wsbodyu(lcount) = wsbodyuu
        wsbodyv(lcount) = wsbodyvv
        
        if (.not.water_is_too_shallow_or_waves_are_too_small) then
            if (wavel>1.0E-6 .and. swflux) then
                mx(lcount) = .125*grav*hrm*hrm*tpp/wavel*cos(dirh*degrad_sp)
                my(lcount) = .125*grav*hrm*hrm*tpp/wavel*sin(dirh*degrad_sp)
            else
                mx(lcount) = 0.
                my(lcount) = 0.
            endif
        else
            mx(lcount) = 0.
            my(lcount) = 0.
        endif
        !
        ! End loop
        !
    enddo
999 continue
    end subroutine transform_wave_physics
