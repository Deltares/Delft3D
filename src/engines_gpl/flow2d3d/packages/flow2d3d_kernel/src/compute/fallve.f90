subroutine fallve(kmax      ,nmmax     ,lsal      ,ltem      ,lsed      , &
                & kcs       ,kfs       ,u0        ,v0        , &
                & wphy      ,r0        ,rtur0     ,ltur      ,thick     , &
                & saleqs    ,temeqs    ,rhowat    ,ws        , &
                & icx       ,icy       ,lundia    ,dps       ,s0        , &
                & umean     ,vmean     ,z0urou    ,z0vrou    ,kfu       , &
                & kfv       ,zmodel    ,kfsmx0    ,kfsmn0    ,dzs0      , &
                & taubmx    ,lstsci    ,rich      ,gdp       )
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
!    Function: Relation between sediment concentration
!              and vertical fall velocity. Model for
!              hindered settling.
!              Fall velocity at layer interfaces.
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts, only: ee
    use sediment_basics_module, only: SEDTYP_CLAY
    use morphology_data_module
    use flocculation, only: get_tshear_tdiss
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)                           , pointer :: ag
    real(fp)                           , pointer :: vonkar
    !
    real(fp)                           , pointer :: vicmol
    real(fp)                           , pointer :: csoil
    real(fp)           , dimension(:)  , pointer :: rhosol
    real(fp)         , dimension(:,:)  , pointer :: dss
    real(fp)           , dimension(:)  , pointer :: sedd50
    real(fp)           , dimension(:)  , pointer :: sedd50fld
    integer            , dimension(:)  , pointer :: sedtyp
    !
    real(fp)                           , pointer :: timsec
    !
    character(256)     , dimension(:)  , pointer :: dll_usrfil
    character(256)     , dimension(:)  , pointer :: dll_function
    integer(pntrsize)  , dimension(:)  , pointer :: dll_handle
    integer            , dimension(:)  , pointer :: iform_settle
    real(fp)           , dimension(:,:), pointer :: par_settle
    !
    integer                            , pointer :: max_integers
    integer                            , pointer :: max_reals
    integer                            , pointer :: max_strings
    integer            , dimension(:)  , pointer :: dll_integers
    real(hp)           , dimension(:)  , pointer :: dll_reals
    character(256)     , dimension(:)  , pointer :: dll_strings
!
! Global variables
!
    integer                                                 , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                                 , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                 , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in)  :: lsal   !  Description and declaration in dimens.igs
    integer                                                 , intent(in)  :: lsed   !  Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in)  :: ltem   !  Description and declaration in dimens.igs
    integer                                                 , intent(in)  :: ltur   !  Description and declaration in dimens.igs
    integer                                                 , intent(in)  :: lstsci !  Description and declaration in dimens.igs
    integer                                                               :: lundia !  Description and declaration in inout.igs
    integer                                                 , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)  :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)  :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)  :: kfsmn0 !  Description and declaration in iidim.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)  :: kfsmx0 !  Description and declaration in iidim.f90
    logical                                                 , intent(in)  :: zmodel !  Description and declaration in procs.igs
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      , intent(in)  :: dzs0   !  Description and declaration in rjdim.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, lsed)            :: ws     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      , intent(in)  :: rhowat !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      , intent(in)  :: u0     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      , intent(in)  :: v0     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      , intent(in)  :: wphy   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci) , intent(in):: r0    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                  :: rich   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, ltur), intent(in):: rtur0  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)  :: s0     !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)  :: taubmx !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)  :: umean  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)  :: vmean  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)  :: z0urou !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)  :: z0vrou !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(kmax)                             , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp)                                                , intent(in)  :: saleqs    
    real(fp)                                                , intent(in)  :: temeqs    
!
! Local variables
!
    integer                     :: i
    integer                     :: istat
    integer                     :: k
    integer                     :: kab
    integer                     :: kbe
    integer                     :: kend
    integer                     :: kstart
    integer                     :: l
    integer                     :: ll
    integer                     :: lst
    integer                     :: n
    integer                     :: ndm
    integer                     :: nm
    integer                     :: nmd
    integer                     :: m
    logical                     :: error
    real(fp)                    :: cclay
    real(fp)                    :: chezy
    real(fp)                    :: ctot
    real(fp)                    :: fl
    real(fp)                    :: h0
    real(fp)                    :: kn
    real(fp)                    :: ldepth
    real(fp)                    :: rhoint
    real(fp)                    :: sag
    real(fp)                    :: salint
    real(fp)                    :: temint
    real(fp)                    :: tka
    real(fp)                    :: tkb
    real(fp)                    :: tkt
    real(fp)                    :: tshear
    real(fp)                    :: tur_eps
    real(fp)                    :: tur_k
    real(fp)                    :: tur_l
    real(fp)                    :: u
    real(fp)                    :: um
    real(fp)                    :: v
    real(fp)                    :: vm
    real(fp)                    :: w
    real(fp)                    :: wsloc
    real(fp)                    :: z0rou
    real(fp), dimension(:), pointer :: localpar
    character(256)              :: errmsg
!
!! executable statements -------------------------------------------------------
!
    ag                  => gdp%gdphysco%ag
    vonkar              => gdp%gdphysco%vonkar
    !
    vicmol              => gdp%gdphysco%vicmol
    csoil               => gdp%gdsedpar%csoil
    rhosol              => gdp%gdsedpar%rhosol
    dss                 => gdp%gdsedpar%dss
    sedd50              => gdp%gdsedpar%sedd50
    sedd50fld           => gdp%gdsedpar%sedd50fld
    sedtyp              => gdp%gdsedpar%sedtyp
    !
    timsec              => gdp%gdinttim%timsec
    !
    dll_usrfil          => gdp%gdtrapar%dll_usrfil_settle
    dll_function        => gdp%gdtrapar%dll_function_settle
    dll_handle          => gdp%gdtrapar%dll_handle_settle
    iform_settle        => gdp%gdtrapar%iform_settle
    par_settle          => gdp%gdtrapar%par_settle
    !
    max_integers        => gdp%gdtrapar%max_integers_settle
    max_reals           => gdp%gdtrapar%max_reals_settle
    max_strings         => gdp%gdtrapar%max_strings_settle
    dll_integers        => gdp%gdtrapar%dll_integers_settle
    dll_reals           => gdp%gdtrapar%dll_reals_settle
    dll_strings         => gdp%gdtrapar%dll_strings_settle
    !
    allocate (localpar (gdp%gdtrapar%npar), stat = istat)
    !
    error = .false.
    lst   = max(lsal, ltem)
    !
    sag = sqrt(ag)
    !
    do nm = 1, nmmax
       if (kfs(nm)==0 .or. kcs(nm)>2) cycle
       !
       nmd  = nm - icx
       ndm  = nm - icy
       !
       h0 = s0(nm) + real(dps(nm),fp)
       um = (umean(nm) + umean(nmd))/2.0_fp
       vm = (vmean(nm) + vmean(ndm))/2.0_fp
       !
       ! Calculate total (possibly wave enhanced) roughness
       !
       kn    = max(1, kfu(nm) + kfu(nmd) + kfv(nm) + kfv(ndm))
       z0rou = (  kfu(nmd)*z0urou(nmd) + kfu(nm)*z0urou(nm) &
             &  + kfv(ndm)*z0vrou(ndm) + kfv(nm)*z0vrou(nm)  )/kn
       chezy = sag * log( 1.0_fp + h0/max(1.0e-8_fp,ee*z0rou) ) / vonkar
       !
       ! loop over the interfaces in the vertical
       !
       if (zmodel) then
           kstart = kfsmn0(nm)-1
           kend   = kfsmx0(nm)-1
           ldepth = h0 ! from bottom to top, so start with total water depth
       else
           kstart = 1
           kend   = kmax
           ldepth = 0.0_fp ! from top to bottom, so start at surface
       endif
       do k = kstart, kend
          !
          ! define indices kab and kbe pointing to the layer physically ABove and BElow the interface
          ! The variables ku/kd for up and down have been avoided because we use those for pos/neg k-index
          ! directions elsewhere in the Delft3D code.
          !
          if (zmodel) then
             kab    = k + 1
             kbe    = max(k, kfsmn0(nm))
             tka    = dzs0(nm,kab)
             tkb    = dzs0(nm,kbe)
             ldepth = ldepth - tkb
          else
             kab    = k
             kbe    = min(k + 1, kmax)
             tka    = thick(kab)
             tkb    = thick(kbe)
             ldepth = ldepth + tka*h0 ! tka/tkb relative for sigma model
          endif
          tkt = tka + tkb
          !
          ! Input parameters are passed via dll_reals/integers/strings-arrays
          !
          if (lsal > 0) then
             salint = max(0.0_fp, (tka*r0(nm, kbe, lsal) + tkb*r0(nm, kab, lsal)  ) / tkt )
          else
             salint = saleqs
          endif
          !
          if (ltem > 0) then
             temint = (  tka*r0(nm, kbe, ltem) + tkb*r0(nm, kab, ltem)  ) / tkt
          else
             temint = temeqs
          endif
          !
          rhoint = (tka*rhowat(nm,kbe) + tkb*rhowat(nm,kab)) / tkt
          !
          u = (tka*u0(nm ,kbe) + tkb*u0(nm ,kab) + &
             & tka*u0(nmd,kbe) + tkb*u0(nmd,kab)) / 2.0_fp / tkt
          v = (tka*v0(nm ,kbe) + tkb*v0(nm ,kab) + &
             & tka*v0(ndm,kbe) + tkb*v0(ndm,kab)) / 2.0_fp / tkt
          w = (tka*wphy(nm,kbe) + tkb*wphy(nm,kab)) / tkt
          !
          if (ltur == 2) then ! k-eps
             tur_k   = rtur0(nm,k,1)
             tur_eps = rtur0(nm,k,2)
          elseif (ltur == 1) then ! k-L
             tur_k   = rtur0(nm,k,1)
             tur_eps = -999.0_fp
          else ! algebraic or constant
             tur_k   = -999.0_fp
             tur_eps = -999.0_fp
          endif
          !
          if (kmax == 0) then ! 2D
             call get_tshear_tdiss( tshear, tur_eps, rhoint, taub = taubmx(nm), waterdepth = h0, vonkar = vonkar )
          elseif (ltur == 0) then ! algebraic or constant
             call get_tshear_tdiss( tshear, tur_eps, rhoint, taub = taubmx(nm), waterdepth = h0, localdepth = ldepth)
          elseif (ltur == 1) then ! k-L
             ! compute mixing length analogous to rl in turclo
             if (rich(nm, k)>=0.0) then
                fl = exp(-2.3_fp * min(rich(nm, k), 30.0_fp))
             else
                fl = (1.0_fp - 14.0_fp*rich(nm, k))**0.25_fp
             endif
             tur_l = vonkar * (h0 - ldepth) * sqrt(ldepth/h0) * fl
             call get_tshear_tdiss( tshear, tur_eps, rhoint, tke = tur_k, tlength = tur_l, vonkar = vonkar)
          else ! k-eps
             call get_tshear_tdiss( tshear, tur_eps, rhoint, tke = tur_k )
          endif
          !
          ctot = 0.0_fp
          cclay = 0.0_fp
          do l = 1, lsed
             ll = lst + l
             ctot = ctot + r0(nm, k, ll)
             if (sedtyp(l) == SEDTYP_CLAY) cclay = cclay + r0(nm, k, ll)
          enddo
          !
          do l = 1, lsed
             ll = lst + l
             !
             do i = 1,gdp%gdtrapar%npar
                localpar(i) = par_settle(i,l)
             enddo
             !
             if (max_reals < WS_MAX_RP) then
                write(errmsg,'(a,a,a)') 'Insufficient space to pass real values to settling routine.'
                call write_error(errmsg, unit=lundia)
                error = .true.
                return
             endif
             dll_reals(WS_RP_TIME ) = real(timsec ,hp)
             dll_reals(WS_RP_ULOC ) = real(u      ,hp)
             dll_reals(WS_RP_VLOC ) = real(v      ,hp)
             dll_reals(WS_RP_WLOC ) = real(w      ,hp)
             dll_reals(WS_RP_SALIN) = real(salint ,hp)
             dll_reals(WS_RP_TEMP ) = real(temint ,hp)
             dll_reals(WS_RP_RHOWT) = real(rhoint ,hp)
             dll_reals(WS_RP_CFRCB) = real(r0(nm,kbe,ll),hp)
             dll_reals(WS_RP_CTOT ) = real(ctot   ,hp)
             dll_reals(WS_RP_KTUR ) = real(tur_k  ,hp)
             dll_reals(WS_RP_EPTUR) = real(tur_eps,hp)
             if (sedd50(l)<0.0_fp) then
                dll_reals(WS_RP_D50) = real(sedd50fld(nm),hp)
             else
                dll_reals(WS_RP_D50) = real(sedd50(l),hp)
             endif
             dll_reals(WS_RP_DSS  ) = real(dss(nm,l) ,hp)
             dll_reals(WS_RP_RHOSL) = real(rhosol(l) ,hp)
             dll_reals(WS_RP_CSOIL) = real(csoil     ,hp)
             dll_reals(WS_RP_GRAV ) = real(ag        ,hp)
             dll_reals(WS_RP_VICML) = real(vicmol    ,hp)
             dll_reals(WS_RP_WDEPT) = real(h0        ,hp)
             dll_reals(WS_RP_UMEAN) = real(um        ,hp)
             dll_reals(WS_RP_VMEAN) = real(vm        ,hp)
             dll_reals(WS_RP_CHEZY) = real(chezy     ,hp)
             dll_reals(WS_RP_SHTUR) = real(tshear    ,hp)
             dll_reals(WS_RP_CCLAY) = real(cclay     ,hp)
             !
             if (max_integers < WS_MAX_IP) then
                write(errmsg,'(a,a,a)') 'Insufficient space to pass integer values to settling routine.'
                call write_error(errmsg, unit=lundia)
                error = .true.
                return
             endif
             call nm_to_n_and_m(nm, n, m, gdp)
             dll_integers(WS_IP_NM  ) = nm
             dll_integers(WS_IP_N   ) = n
             dll_integers(WS_IP_M   ) = m
             dll_integers(WS_IP_K   ) = k
             dll_integers(WS_IP_ISED) = l
             !
             if (max_strings < WS_MAX_SP) then
                write(errmsg,'(a,a,a)') 'Insufficient space to pass strings to settling routine.'
                call write_error(errmsg, unit=lundia)
                error = .true.
                return
             endif
             dll_strings(WS_SP_RUNID) = gdp%runid
             dll_strings(WS_SP_USRFL) = dll_usrfil(l)
             !
             call eqsettle(dll_function, dll_handle, max_integers, max_reals, max_strings, &
                         & dll_integers, dll_reals, dll_strings, lundia, iform_settle(l),  &
                         & localpar, gdp%gdtrapar%npar, wsloc, error)
             if (error) call d3stop(1, gdp)
             !
             ws(nm, k, l) = wsloc
          enddo     ! l
       enddo        ! k
    enddo           ! nm
    deallocate (localpar, stat = istat)
end subroutine fallve          
