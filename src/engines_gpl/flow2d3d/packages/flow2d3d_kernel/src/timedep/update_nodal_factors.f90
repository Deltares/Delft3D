subroutine update_nodal_factors(timnow, kc, ntof, nto, kcd, hydrbc, omega, gdp)
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
! Update the nodel factors with current date/time i_date_time
! i_date_time is set in setcurrentdatetime
! Both nodal factors for harmonic boundaries and tidal forces are updated
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    use globaldata
    !
    implicit none
!
! The following list of pointer parameters is used to point inside the gdp structure
!
    integer, dimension(:)           , pointer       :: i_date_time
    integer                         , pointer       :: lundia
    integer                         , pointer       :: nrcmp
    character(1)                    , pointer       :: ascon
    real(fp)                        , pointer       :: time_nodal_update_bnd
    real(fp)                        , pointer       :: time_nodal_update_tgf
    real(fp)                        , pointer       :: ti_nodal ! update interval in unit of mdf-file
    real(fp)                        , pointer       :: dt       ! time step in unit of mdf-file
    real(hp), dimension(:)          , pointer       :: tgffr
    real(hp), dimension(:)          , pointer       :: tgfv0u
    real(hp), dimension(:)          , pointer       :: tgfw
    character(8), dimension(:)      , pointer       :: compnames
    real(fp)    , dimension(:,:,:)  , pointer       :: hydrbcf
    character(8), dimension(:)      , pointer       :: tgfnam
    character(24)                   , pointer       :: date_time
!
! Global variables
!
    integer                         , intent(in)    :: kc     ! actual number of components
    integer                         , intent(in)    :: ntof   ! nr. of fourier openings
    integer                         , intent(in)    :: nto    ! total nr of openings
    integer                         , intent(in)    :: kcd    ! max(1, kmax, kc)
    real(fp), dimension(4, nto, kcd), intent(inout) :: hydrbc
    real(fp), dimension(kc)         , intent(out)   :: omega
    real(fp)                        , intent(in)    :: timnow
    type(globdat),target            , intent(in)    :: gdp
!
! Local variables
!
    integer                                         :: ierrs
    integer                                         :: i
    integer                                         :: k
    integer                                         :: n
    logical                                         :: is_first
    logical                                         :: need_update
    real(hp), dimension(:), allocatable             :: fr
    real(hp), dimension(:), allocatable             :: v0u
    real(hp), dimension(:), allocatable             :: w
    character(256)                                  :: message
!
!! executable statements -------------------------------------------------------
!
    compnames             => gdp%gdbcdat%compnames
    hydrbcf               => gdp%gdbcdat%hydrbcf
    ascon                 => gdp%gdbcdat%ascon
    nrcmp                 => gdp%gdtfzeta%nrcmp
    tgffr                 => gdp%gdtfzeta%tgffr
    tgfv0u                => gdp%gdtfzeta%tgfv0u
    tgfw                  => gdp%gdtfzeta%tgfw
    tgfnam                => gdp%gdtfzeta%tgfnam
    dt                    => gdp%gdexttim%dt
    lundia                => gdp%gdinout%lundia
    i_date_time           => gdp%gdinttim%i_date_time
    time_nodal_update_bnd => gdp%gdinttim%time_nodal_update_bnd
    time_nodal_update_tgf => gdp%gdinttim%time_nodal_update_tgf
    ti_nodal              => gdp%gdinttim%ti_nodal
    date_time             => gdp%gdinttim%date_time
    !
    ! update for harmonic boundaries
    !
    is_first = (time_nodal_update_bnd == 0.0_fp)
    need_update = ((timnow - time_nodal_update_bnd) * dt >= ti_nodal)
    if ((is_first .or. need_update) .and. ascon == 'Y' .and. kc > 0 .and. ntof > 0) then
       write (message, '(2a)') 'Nodal factors updated at ', date_time
       call prterr(lundia, 'G051', trim(message))
       !
       ! initialisations for A0:
       !
       if (is_first) then
          omega(1) = 0.0_fp
          do n = 1, ntof
             hydrbc(1, n, 1) = hydrbcf(1, n, 1)
             hydrbc(2, n, 1) = hydrbcf(2, n, 1)
             hydrbc(3, n, 1) = 0.0_fp
             hydrbc(4, n, 1) = 0.0_fp
          enddo
       endif
       !
       ! update for harmonic boundaries
       ! (skip if only A0 is given)
       !
       if (kc > 1) then
          time_nodal_update_bnd = timnow
          allocate(v0u(kc-1), fr(kc-1), w(kc-1), stat=ierrs)
          if (ierrs /= 0) then
             call prterr(lundia, 'U021', 'Update nodal factors: memory alloc error')
             call d3stop(1, gdp)
          endif
          !
          call asc(w, v0u, fr, kc-1, compnames, i_date_time, ierrs, gdp)
          if (ierrs > 0) then
             write (*,*) 'ierrs after call asc for boundaries = ', ierrs
          endif
          !
          do k = 2, kc
             omega(k) = real(w(k-1)*raddeg_hp, fp)
             do n = 1, ntof
                hydrbc(1, n, k) = hydrbcf(1, n, k) * real(fr(k-1),fp)
                hydrbc(2, n, k) = hydrbcf(2, n, k) * real(fr(k-1),fp)
                hydrbc(3, n, k) = hydrbcf(3, n, k) - real(v0u(k-1) * raddeg_hp,fp)
                hydrbc(4, n, k) = hydrbcf(4, n, k) - real(v0u(k-1) * raddeg_hp,fp)
                !
                ! correct for PHASE jump at both ends of tidal opening
                ! (originally in triasc)
                !
                hydrbc(3,n,k) = modulo(hydrbc(3,n,k), 360.0_fp)
                hydrbc(4,n,k) = modulo(hydrbc(4,n,k), 360.0_fp)
                !
                if ((hydrbc(3, n, k) - hydrbc(4, n, k)) > 180.0_fp) then
                   hydrbc(4, n, k) = hydrbc(4, n, k) + 360.0_fp
                elseif ((hydrbc(3, n, k) - hydrbc(4, n, k)) < -180.0_fp) then
                   hydrbc(3, n, k) = hydrbc(3, n, k) + 360.0_fp
                endif
             enddo
          enddo
          !
          deallocate(v0u, fr, w)
       endif
    endif
    !
    ! update for tidal forces
    !
    is_first = (time_nodal_update_tgf == 0.0_fp)
    need_update = ((timnow - time_nodal_update_tgf) * dt >= ti_nodal)
    if ((is_first .or. need_update) .and. nrcmp > 0) then
       time_nodal_update_tgf = timnow
       ierrs = 0
       call asc(tgfw, tgfv0u, tgffr, nrcmp, tgfnam, i_date_time, ierrs, gdp)
       if (ierrs > 0) then
          write (*,*) 'ierrs after call asc for tgf = ', ierrs
       endif
       !
       do k = 1, nrcmp
          tgfv0u(k) = tgfv0u(k) * raddeg_hp
          tgfw(k)   = tgfw(k)   * raddeg_hp
       enddo
    endif
   !
end subroutine update_nodal_factors
