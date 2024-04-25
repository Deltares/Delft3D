subroutine z_chkadv(lundia    ,nmmax     ,kmax      ,icx       , &
                  & icy       ,kfu       ,kfuz0     ,kfvz0     , &
                  & guu       ,gvu       ,u0        ,v0        , &
                  & kfumx0    ,kfumin    ,dzu0      ,dzs0      , &
                  & nst       ,kcs       ,gdp       )
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
!  $Id: z_chkadv.f90$
!  : https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20111115_13532_z-model_improvements_oss-merge/engines_gpl/flow2d3d/packages/kernel/src/compute/z_chkadv.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Checks stability (Courant number) for horizontal velocity and
!              for Z-layer model
!              (following definitions G.S. Stelling, 1984)
!              Subroutine is called four times from Z-TRISOL.
!              Every half time step:
!                 - For U- and V-velocities in U-points
!                 - For U- and V-velocities in V-points
! Method used:
!     Comment: In case the locally the Courant number is larger than 1
!              a warning is generated and a suggestion for the timestep.
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
    integer      , pointer :: nh_level
    real(fp)     , pointer :: hdt
    character(6) , pointer :: momsol
    logical      , pointer :: zmodel
    logical      , pointer :: nonhyd
!
! Global variables
!
    integer                                         , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir.
                                                                            !!  If icx=1 then computation proceeds in the Y-dir.
    integer                                         , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                         , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                       :: lundia !  Description and declaration in inout.igs
    integer                                         , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer                                                       :: nst    !!  Current time step counter
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfumx0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfumin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub,kmax) , intent(in)  :: kfuz0  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub,kmax) , intent(in)  :: kfvz0  !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub,kmax) , intent(in)  :: dzs0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub,kmax) , intent(in)  :: dzu0   !  Description and declaration in esm_alloc_real.f90
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
    real(fp)       :: frac_dz
    real(fp)       :: frac_dz_u
    real(fp)       :: frac_dz_v
    real(fp)       :: timestep
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
    nh_level   => gdp%gdnonhyd%nh_level
    hdt        => gdp%gdnumeco%hdt
    momsol     => gdp%gdnumeco%momsol
    zmodel     => gdp%gdprocs%zmodel
    nonhyd     => gdp%gdprocs%nonhyd
    !
    ! In the Z-layer model - for momsol = mdue - the advection terms are discretised in an explicit way
    ! based on a directional upwind spatial discretisation.
    !
    ! Check velocity field
    ! (maximum value based on CFL stability criterion for advection:
    !  hdt*max (|u0(nm,k)|/gvu(nm), |v0(nm,k)|/guu(nm)) <1)
    !
    ! If the upperbound of 1 is exceeded a warning is generated and 
    ! the Courant number is written to the tri-diag file.
    ! The simulation is not stopped.
    ! A maximum number of MAXCFLMSG(=100) warnings is written to the tri-diag file
    ! Using the keyword CflMsg (=#Y#) the used can switch on the writing of these warnings
    ! for the whole simulation period (no restriction on the number of warnings)
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
    cflumax    = 1.0_fp
    cflvmax    = 1.0_fp
    u_cflumax  = 0.0_fp
    v_cflvmax  = 0.0_fp
    frac_dz    = 0.0_fp
    frac_dz_u  = 0.0_fp
    frac_dz_v  = 0.0_fp
    !
    if (momsol == 'mdue  ' .or. (nonhyd .and. nh_level == nh_full)) then
       !
       ! Using the whole time step
       !
       timestep = 2.0_fp*hdt
       !
    elseif (momsol == 'iupw  ' .or. momsol == 'mdui  ' .or. momsol == 'flood ' .or. momsol == 'finvol') then
       !
       ! Advection determined implicitly using first order upwind method in z_uzd
       ! ADI method
       !
       timestep = hdt
       !
    endif
    ndm  = -icy
    nmu  =  icx
    ndmu = -icy + icx  
    do nm = 1, nmmax
       ndm  = ndm  + 1
       nmu  = nmu  + 1
       ndmu = ndmu + 1
       if (kfu(nm)==1 .and. kcs(nm)/=3 .and. kcs(nmu)/=3 .and. kcs(nm)/=-1 .and. kcs(nmu)/=-1) then
          do k = kfumin(nm), kfumx0(nm)
             if (kfuz0(nm,k) > 0) then
                if (u0(nm,k) > 0.0_fp) then
                   !
                   ! Flow from cell nm to nmu, layer fraction based upon dzu0(nm,k) and dzs0(nm,k)
                   !
                   !frac_dz = dzu0(nm,k) / max(dzs0(nm,k), 0.01_fp)
                   if ( timestep*u0(nm,k)/gvu(nm) > cflumax ) then
                      cflumax     = timestep * u0(nm,k)/gvu(nm)
                      nm_cflumax  = nm
                      k_cflumax   = k
                      u_cflumax   = abs(u0(nm,k))
                      !frac_dz_u   = frac_dz
                   endif
                   !kenm    = max(1, kfvz0(nm, k) + kfvz0(ndm, k) + kfvz0(ndmu, k) + kfvz0(nmu, k))
                   !vvv     = v0(nm  , k)*kfvz0(nm  , k) + v0(ndm, k)*kfvz0(ndm, k)      &
                   !      & + v0(ndmu, k)*kfvz0(ndmu, k) + v0(nmu, k)*kfvz0(nmu, k)
                   !vvv     = vvv / real(kenm,fp)
                   !if ( frac_dz*timestep*abs(vvv/guu(nm)) > cflvmax ) then
                   !   cflvmax    = frac_dz * timestep * abs(vvv/guu(nm))
                   !   nm_cflvmax = nm
                   !   k_cflvmax  = k
                   !   v_cflvmax  = vvv
                   !   frac_dz_v  = frac_dz
                   !endif
                elseif (u0(nm,k) < 0.0_fp) then
                   !
                   ! Flow from cell nmu to nm, layer fraction based upon dzu0(nm,k) and dzs0(nmu,k)
                   !
                   !frac_dz = dzu0(nm,k) / max(dzs0(nmu,k), 0.01_fp)
                   if ( -timestep*u0(nm,k)/gvu(nm) > cflumax ) then
                      cflumax    = -timestep * u0(nm,k)/gvu(nm)
                      nm_cflumax = nm
                      k_cflumax  = k
                      u_cflumax  = abs(u0(nm,k))
                      !frac_dz_u  = frac_dz
                   endif
                   !kenm    = max(1, kfvz0(nm, k) + kfvz0(ndm, k) + kfvz0(ndmu, k) + kfvz0(nmu, k))
                   !vvv     = v0(nm  , k)*kfvz0(nm  , k) + v0(ndm, k)*kfvz0(ndm, k)      &
                   !      & + v0(ndmu, k)*kfvz0(ndmu, k) + v0(nmu, k)*kfvz0(nmu, k)
                   !vvv     = vvv / real(kenm,fp)
                   !if ( frac_dz*timestep*abs(vvv/guu(nm)) > cflvmax ) then
                   !   cflvmax    = frac_dz * timestep * abs(vvv/guu(nm))
                   !   nm_cflvmax = nm
                   !   k_cflvmax  = k
                   !   v_cflvmax  = vvv
                   !   frac_dz_v  = frac_dz
                   !endif
                endif
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
       !
       dtadv = gvu(nm_cflumax) / abs(u_cflumax)
       if (momsol == 'iupw  ' .or. momsol == 'mdui  ' .or. momsol == 'flood ' .or. momsol == 'finvol') then
          dtadv = 2.0_fp * dtadv
       endif
       !
       call nm_to_n_and_m(nm_cflumax, n, m, gdp)
       errmsg = ''
       !if (frac_dz_u > 2.0_fp) then
       !   write (errmsg, '(5a,f8.2,a,i0,a,i0,a,i0,a,i0,a,e10.3,a,e10.3,a)') &
       !        & 'Courant number for ', vel, '-velocity in ', point, '-point equals ', cflumax, & 
       !        & ' for (m,n,k) = (', m, ',', n, ',', k_cflumax, '),  at nst = ', nst ,', due to large gradient in layer thickness: ', frac_dz_u, &
       !        & '. Advised time step: ', dtadv/60.0_fp, ' minutes.'
       !   call prterr (lundia, 'G051', trim(errmsg))
       !else
          write (errmsg, '(5a,f8.2,a,i0,a,i0,a,i0,a,i0,a,e10.3,a)') &
               & 'Courant number for ', vel, '-velocity in ', point, '-point equals ', cflumax, & 
               & ' for (m,n,k) = (', m, ',', n, ',', k_cflumax, '),  at nst = ', nst ,'. Advised time step: ', dtadv/60.0_fp, ' minutes.'
          call prterr (lundia, 'G051', trim(errmsg))
       !endif
    endif
    !!
    !! V-direction
    !!
    !if (icy == 1) then
    !   point = 'U'
    !   vel   = 'V'
    !else 
    !   point = 'V'
    !   vel   = 'U'
    !endif
    !if (cflvmax > 1.0_fp) then
    !   !
    !   cntcflmsg = cntcflmsg + 1
    !   !
    !   ! Determine advised time step
    !   !
    !   dtadv = guu(nm_cflvmax) / (abs(v_cflvmax)*frac_dz_v)
    !   if (momsol == 'iupw  ' .or. momsol == 'mdui  ' .or. momsol == 'flood ' .or. momsol == 'finvol') then
    !      dtadv = 2.0_fp * dtadv
    !   endif
    !   !
    !   call nm_to_n_and_m(nm_cflvmax, n, m, gdp)
    !   errmsg = ''
    !   if (frac_dz_v > 2.0_fp) then
    !      write (errmsg, '(5a,f8.2,a,i0,a,i0,a,i0,a,i0,a,e10.3,a,e10.3,a)') &
    !           & 'Courant number for ', vel, '-velocity in ', point, '-point equals ', cflvmax, & 
    !           & ' for (m,n,k) = (', m, ',', n, ',', k_cflvmax, '),  at nst = ', nst ,', due to large gradient in layer thickness: ', frac_dz_v, &
    !           & '. Advised time step: ', dtadv/60.0_fp, ' minutes.'
    !      call prterr (lundia, 'G051', trim(errmsg))
    !   else
    !      write (errmsg, '(5a,f8.2,a,i0,a,i0,a,i0,a,i0,a,e10.3,a)') &
    !           & 'Courant number for ', vel, '-velocity in ', point, '-point equals ', cflvmax, & 
    !           & ' for (m,n,k) = (', m, ',', n, ',', k_cflvmax, '),  at nst = ', nst ,'. Advised time step: ', dtadv/60.0_fp, ' minutes.'
    !      call prterr (lundia, 'G051', trim(errmsg))
    !   endif
    !endif
    !
    ! Write message to tri-diag file if more than the maximum number of warnings has been written
    !
    if (cntcflmsg >= maxcflmsg .and. .not. cflmsg) then
       write (errmsg, '(a,i0,a)') 'Maximum number of Courant-warnings for advection: ', maxcflmsg, ' was written. Check is no longer performed. &
                                 & Checking for the complete simulation period can be switched on using keyword: CflMsg = #Y#'
       call prterr (lundia, 'G051', trim(errmsg))
    endif
end subroutine z_chkadv
