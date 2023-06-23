!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
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

subroutine transform_swan_physics(hs        ,dir       ,period    ,depth     , &
                                    & fx        ,fy        ,mx        ,my        , &
                                    & distot    ,dissurf   ,diswcap   , &
                                    & m         ,n         ,hrms      ,tp        , &
                                    & grav      ,swflux    ,swdis     , &
                                    & gamma0    ,wsbodyu   ,wsbodyv   ,ierr          )

    !!--description-----------------------------------------------------------------
    ! NONE
    !!--pseudo code and references--------------------------------------------------
    ! NONE
    !!--declarations----------------------------------------------------------------
    use mathconsts, only: pi_sp, sqrt2_sp, degrad_sp
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
    integer                        :: l
    integer                        :: npnt
    logical                        :: corht
    logical                        :: ldep
    real                           :: deph
    real                           :: dirh
    real                           :: dish
    real                           :: diss
    real                           :: dismax
    real                           :: fxhis
    real                           :: fxx
    real                           :: fyhis
    real                           :: fyy
    real                           :: hrm
    real                           :: perfac
    real                           :: qbsli
    real                           :: tpp
    real                           :: wavek
    real                           :: wavel
    real                           :: wsbodyuu
    real                           :: wsbodyvv
    !
    !! executable statements -------------------------------------------------------
    !
    corht  = .false.
    perfac = 1.
    call factorise_waveperiod(gamma0, perfac, ierr)
    if (ierr < 0) then
        write(*,'(a,f10.5)') 'ERROR: gamma0 = ',gamma0,' lies outside allowed range [1,20]'
        goto 999
    endif
    !
    ! Start loop
    !
    npnt  = m*n
    do l = 1,npnt
        hrm   = hs(l)/sqrt2_sp
        dirh  = dir(l)
        deph  = depth(l)
        tpp   = period(l)*perfac
        fxhis = fx(l)
        fyhis = fy(l)
        dish  = distot(l)
        diss  = dissurf(l) + diswcap(l)
        !
        call wave_length(  hrm, deph, tpp, wavel, wavek, ldep, grav  )
        !
        ! If .not. swdis use fx, fy from SWAN
        ! else compute forces based on dissipation and celerity
        !
        wsbodyuu = 0.0
        wsbodyvv = 0.0
        call wave_fluxes(dirh      ,deph      ,tpp       ,fxhis     , &
                       & fyhis     ,dish      ,diss      ,wavel     , &
                       & ldep      ,fxx       ,fyy       ,dismax    , &
                       & corht     ,swdis     ,grav      ,wsbodyuu  , wsbodyvv  )
        hrms(l)    = hrm
        dir(l)     = dirh
        depth(l)   = deph
        tp(l)      = tpp
        fx(l)      = fxx
        fy(l)      = fyy
        wsbodyu(l) = wsbodyuu
        wsbodyv(l) = wsbodyvv
        distot(l)  = dish
        if (.not.ldep) then
            if (wavel>1.0E-6 .and. swflux) then
                mx(l) = .125*grav*hrm*hrm*tpp/wavel*cos(dirh*degrad_sp)
                my(l) = .125*grav*hrm*hrm*tpp/wavel*sin(dirh*degrad_sp)
            else
                mx(l) = 0.
                my(l) = 0.
            endif
        else
            mx(l) = 0.
            my(l) = 0.
        endif
        !
        ! End loop
        !
    enddo
999 continue
    end subroutine transform_swan_physics