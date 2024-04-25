subroutine chkadv(lundia    ,nmmax     ,kmax      ,icx       , &
                & icy       ,kfu       ,kfv       ,nst       , &
                & guu       ,gvu       ,u0        ,v0        , &
                & kcs       ,gdp       )
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
!    Function: Checks stability (Courant number) for horizontal velocity and
!              for Sigma-layer model
!              (following definitions G.S. Stelling, 1984)
!              Subroutine is called four times from TRISOL.
!              Every half time step:
!                 - For velocities in U-points
!                 - For velocities in V-points
! Method used:
!     Comment: In case the locally the Courant number is larger than 1
!              a warning is generated and a suggestion for the hdt.
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
    type(globdat), target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    ! They replace the  include igd / include igp lines
    !
    integer      , pointer :: cntcflmsg
    integer      , pointer :: maxcflmsg
    logical      , pointer :: cflmsg
    real(fp)     , pointer :: hdt
    character(6) , pointer :: momsol
!
! Global variables
!
    integer                                         , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                         , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                         , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                       :: lundia !  Description and declaration in inout.igs
    integer                                         , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer                                                       :: nst    !!  Current time step counter
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfv    !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax  )            :: u0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax  )            :: v0     !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer        :: k
    integer        :: k_cflumax
    integer        :: k_cflvmax
    integer        :: kenm
    integer        :: m
    integer        :: n
    integer        :: ndm
    integer        :: ndmu
    integer        :: nm
    integer        :: nmu
    integer        :: nm_cflumax
    integer        :: nm_cflvmax
    real(fp)       :: cflumax
    real(fp)       :: cflvmax
    real(fp)       :: dtadv
    real(fp)       :: vvv
    real(fp)       :: u_cflumax
    real(fp)       :: v_cflvmax
    character(256) :: errmsg
    character(1)   :: vel
    character(1)   :: point
!
!! executable statements -------------------------------------------------------
!
    cntcflmsg  => gdp%gdflwpar%flwoutput%cntcflmsg
    maxcflmsg  => gdp%gdflwpar%flwoutput%maxcflmsg
    cflmsg     => gdp%gdflwpar%flwoutput%cflmsg
    hdt        => gdp%gdnumeco%hdt
    momsol     => gdp%gdnumeco%momsol
    !
    ! In the Sigma-layer model, for momsol = Flood and Van Leer-2, the advection terms are discretised in an explicit way
    ! based on a (directional) upwind spatial discretisation.
    !
    ! Check velocity field
    ! (maximum value based on CFL stability criterion for advection:
    !  hdt*max (|u0(nm,k)|/gvu(nm), |v0(nm,k)|/guu(nm)) <1)
    !
    ! If the upperbound of 1 is exceeded:
    ! A warning is generated and an advice for the timestep is given.
    ! The simulation is never stopped.
    !
    if (cntcflmsg >= maxcflmsg .and. .not. cflmsg) then
       !
       ! Already written maximum number of warnings 
       ! and user did not request output for whole simulation period
       !
       return
    endif
    !
    ! Initialise local parameters
    !
    nm_cflumax = 0
    nm_cflvmax = 0
    k_cflumax  = 0
    k_cflvmax  = 0
    cflumax    = 0.0_fp
    cflvmax    = 0.0_fp
    u_cflumax  = 0.0_fp
    v_cflvmax  = 0.0_fp
    !
    ndm  = -icy
    nmu  =  icx
    ndmu = -icy + icx  
    do nm = 1, nmmax
       ndm  = ndm  + 1
       nmu  = nmu  + 1
       ndmu = ndmu + 1
       if (kfu(nm)==1 .and. kcs(nm)/=3 .and. kcs(nmu)/=3 .and. kcs(nm)/=-1 .and. kcs(nmu)/=-1) then
          do k = 1, kmax
             !
             ! Flow from cell nm to nmu
             !
             if ( hdt*abs(u0(nm,k))/gvu(nm) > cflumax ) then
                cflumax     = hdt * abs(u0(nm,k))/gvu(nm)
                nm_cflumax  = nm
                k_cflumax   = k
                u_cflumax   = abs(u0(nm,k))
             endif
             kenm    = max(1, kfv(nm) + kfv(ndm) + kfv(ndmu) + kfv(nmu))
             vvv     = v0(nm  , k)*kfv(nm  ) + v0(ndm, k)*kfv(ndm)      &
                   & + v0(ndmu, k)*kfv(ndmu) + v0(nmu, k)*kfv(nmu)
             vvv     = vvv / real(kenm,fp)
             if ( hdt*abs(vvv/guu(nm)) > cflvmax ) then
                cflvmax    = hdt * abs(vvv/guu(nm))
                nm_cflvmax = nm
                k_cflvmax  = k
                v_cflvmax  = abs(vvv)
             endif
          enddo
       endif
    enddo
    !
    ! U-direction
    !
    if (icy == 1) then
       point = 'U'
       vel   = 'U'
    else 
       point = 'V'
       vel   = 'V'
    endif
    if (cflumax > 1.0_fp) then
       !
       cntcflmsg = cntcflmsg + 1
       !
       ! Determine advised time step
       ! Multiplication by 2 to get a suggestion for the whole time step
       !
       dtadv = 2.0_fp * gvu(nm_cflumax) / u_cflumax
       !
       call nm_to_n_and_m(nm_cflumax, n, m, gdp)
       errmsg = ''
       write (errmsg, '(5a,f8.2,a,i0,a,i0,a,i0,a,i0,a,e10.3,a)') &
            & 'Courant number for ', vel, '-velocity in ', point, '-point equals ', cflumax, & 
            & ' for (m,n,k) = (', m, ',', n, ',', k_cflumax, '),  at nst = ', nst ,'. Advised time step: ', dtadv/60.0_fp, ' minutes.'
       call prterr (lundia, 'G051', trim(errmsg))
    endif
    !
    ! V-direction
    !
    if (icy == 1) then
       point = 'U'
       vel   = 'V'
    else 
       point = 'V'
       vel   = 'U'
    endif
    if (cflvmax > 1.0_fp) then
       !
       cntcflmsg = cntcflmsg + 1
       !
       ! Determine advised time step
       ! Multiplication by 2 to get a suggestion for the whole time step
       !
       dtadv = 2.0_fp * guu(nm_cflvmax) / v_cflvmax
       !
       call nm_to_n_and_m(nm_cflvmax, n, m, gdp)
       errmsg = ''
       write (errmsg, '(5a,f8.2,a,i0,a,i0,a,i0,a,i0,a,e10.3,a)') &
            & 'Courant number for ', vel, '-velocity in ', point, '-point equals ', cflvmax, & 
            & ' for (m,n,k) = (', m, ',', n, ',', k_cflvmax, '),  at nst = ', nst ,'. Advised time step: ', dtadv/60.0_fp, ' minutes.'
       call prterr (lundia, 'G051', trim(errmsg))
    endif
    !
    ! Write message to tri-diag file if more than the maximum number of warnings has been written
    !
    if (cntcflmsg >= maxcflmsg .and. .not. cflmsg) then
       write (errmsg, '(a,i0,a)') 'Maximum number of Courant-warnings for advection: ', maxcflmsg, ' was written. Check is no longer performed. &
                                 & Checking for the complete simulation period can be switched on using keyword: CflMsg = #Y#'
       call prterr (lundia, 'G051', trim(errmsg))
    endif
end subroutine chkadv
