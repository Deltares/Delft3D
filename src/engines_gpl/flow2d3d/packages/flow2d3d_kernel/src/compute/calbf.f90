subroutine calbf(stage     ,nmmax     ,nmaxddb   ,dps       ,s1        , &
               & u         ,v         ,kfs       ,kadu      ,kadv      , &
               & kfu       ,kfv       ,kmax      ,lsedtot   , &
               & icx       ,icy       ,hdt       ,lundia    ,nst       , &
               & norow     ,nocol     ,irocol    , &
               & kcs       ,kcu       ,kcv       , &
               & gsqs      ,e0        ,e1        , &
               & sour      ,sink      ,bbk       ,bdddx     ,bddx      , &
               & bdx       ,bux       ,buux      ,buuux     ,uvdwk     , &
               & vvdwk     ,bbkl      ,ddkl      ,cvalu0    ,cvalv0    , & 
               & umean     ,vmean     ,guu       ,gvv       , &
               & sbuu      ,sbvv      ,gdp       )       
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
! Calculate bedform height and length using bedform.f90
! And solve advection equation using difbf.f90 
! called for U/V-direction respectively.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    !
    use precision
    use flow2d3d_timers
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! Global variables
    !
    integer                                              , intent(in)  :: nmmax   !  Description and declaration in dimens.igs
    integer                                              , intent(in)  :: nmaxddb
    integer                                              , intent(in)  :: icx
    integer                                              , intent(in)  :: icy
    integer                                              , intent(in)  :: kmax    !  Description and declaration in dimens.igs
    integer                                              , intent(in)  :: lsedtot !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in)  :: kadu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in)  :: kadv    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kfs     !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kfu     !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kfv     !  Description and declaration in esm_alloc_int.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: dps     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: s1      !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in)  :: u       !!  EULARIAN U-velocities at old time level
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in)  :: v       !!  EULARIAN V-velocities at old time level
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: cvalu0  ! Roughness in U-direction        
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: cvalv0  ! Roughness in V-direction        
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: umean   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: vmean   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: guu     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: gvv     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot), intent(in)  :: sbuu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot), intent(in)  :: sbvv    !  Description and declaration in esm_alloc_real.f90
    integer                                                            :: lundia  !  Description and declaration in inout.igs
    integer                                              , intent(in)  :: norow   !  Description and declaration in esm_alloc_int.f90
    integer                                              , intent(in)  :: nocol   !  
    integer                                              , intent(in)  :: nst
    integer   , dimension(5, norow+nocol)                , intent(in)  :: irocol  !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kcs     !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kcu     !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kcv     !  Description and declaration in esm_alloc_int.f90
    real(fp)                                             , intent(in)  :: hdt
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: bbk
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: bbkl
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: bdddx
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: bddx
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: bdx
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: buuux
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: buux
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: bux
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: ddkl
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: gsqs    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: e0      !  Array containing value at old time step 
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: e1      !  Result at new time step
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: sink    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: sour    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: uvdwk
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: vvdwk
    character(8)                                         , intent(in)  :: stage   !!  First or second half time step
    !
    !Local parameters
    ! 
    integer                                   :: ic
    integer                                   :: nm
    integer                                   :: lsed
    integer                                   :: tcnt
    real(fp)                                  :: hdtb
    real(fp)                                  :: hpow
    real(fp)                                  :: depth
    real(fp)                                  :: depthu
    real(fp)                                  :: depthv
    real(fp)                                  :: cflcheck   
    real(fp)                                  :: T_relax
    real(fp)                                  :: phi
    real(fp)                                  :: gamma
    real(fp)                                  :: fr_loc2
    real(fp)                                  :: sbu
    real(fp)                                  :: sbv
    real(fp)                                  :: utot2
    real(fp)                                  :: umean_at_v
    real(fp)                                  :: vmean_at_u
    integer                         , pointer :: bdfrlxtype
    integer                         , pointer :: itmor
    logical                         , pointer :: lfbdfmor
    logical                         , pointer :: lfbedfrm
    logical                         , pointer :: lfbedfrmCFL
    logical                         , pointer :: lfbedfrmADV
    real(fp)                        , pointer :: ag
    real(fp)                        , pointer :: bdfC_Hn
    real(fp)                        , pointer :: bdfC_Hp
    real(fp)                        , pointer :: bdfGmin
    real(fp)                        , pointer :: bdfHmax
    real(fp)                        , pointer :: bdfL_Hc
    real(fp)                        , pointer :: bdfL_Hp
    real(fp)                        , pointer :: bdfPmax
    real(fp)                        , pointer :: bedformL_H
    real(fp)                        , pointer :: bedformT_H
    real(fp)                        , pointer :: dryflc
    real(fp)                        , pointer :: morfac
    real(fp)      , dimension(:)    , pointer :: cdpar
    real(fp)      , dimension(:)    , pointer :: duneheight
    real(fp)      , dimension(:)    , pointer :: duneheightequi
    real(fp)      , dimension(:)    , pointer :: dunelength
    real(fp)      , dimension(:)    , pointer :: qbedformx
    real(fp)      , dimension(:)    , pointer :: qbedformy
    real(fp)      , dimension(:)    , pointer :: ubedform
    integer       , dimension(:,:)  , pointer :: ibtyp
    real(fp)      , dimension(:,:)  , pointer :: bval
!
!! executable statements -------------------------------------------------------
!    
    ag                      => gdp%gdphysco%ag
    bdfC_Hn                 => gdp%gdbedformpar%bdfC_Hn
    bdfC_Hp                 => gdp%gdbedformpar%bdfC_Hp
    bdfGmin                 => gdp%gdbedformpar%bdfGmin
    bdfHmax                 => gdp%gdbedformpar%bdfHmax
    bdfL_Hc                 => gdp%gdbedformpar%bdfL_Hc
    bdfL_Hp                 => gdp%gdbedformpar%bdfL_Hp
    bdfPmax                 => gdp%gdbedformpar%bdfPmax
    bdfrlxtype              => gdp%gdbedformpar%bdfrlxtype
    cdpar                   => gdp%gdbedformpar%cdpar
    bedformL_H              => gdp%gdbedformpar%bedformL_H
    bedformT_H              => gdp%gdbedformpar%bedformT_H 
    duneheight              => gdp%gdbedformpar%duneheight
    duneheightequi          => gdp%gdbedformpar%duneheightequi
    dunelength              => gdp%gdbedformpar%dunelength
    lfbdfmor                => gdp%gdbedformpar%lfbdfmor
    lfbedfrm                => gdp%gdbedformpar%lfbedfrm
    lfbedfrmCFL             => gdp%gdbedformpar%lfbedfrmCFL
    lfbedfrmADV             => gdp%gdbedformpar%lfbedfrmADV
    qbedformx               => gdp%gdbedformpar%qbedformx
    qbedformy               => gdp%gdbedformpar%qbedformy
    ubedform                => gdp%gdbedformpar%ubedform
    itmor                   => gdp%gdmorpar%itmor
    morfac                  => gdp%gdmorpar%morfac
    dryflc                  => gdp%gdnumeco%dryflc
    ibtyp                   => gdp%gdadv2d%ibtyp
    bval                    => gdp%gdadv2d%bval
    !
    ! The time step used for the bedform adaptation depends on the
    ! interpretation of the morphological factor. In a tidal environment
    ! the hydrodynamic time step should be used, but in a riverine
    ! environment the morphological time scale should be used.
    !
    if (lfbdfmor) then 
        hdtb = hdt*morfac
        if ( (comparereal(morfac,0.0_fp) == 0) .or. (nst <= itmor)) then
            ! no update of bedforms necessary
            return
        endif
    else
       hdtb = hdt
    endif  
    !
    ! Calculate EQUILIBRIUM bedform heights/lengths
    !
    call bedform(nmmax     ,dps       ,s1        ,u         ,v         , &
               & kfs       ,kfu       ,kfv       ,kmax      , &
               & icx       ,icy       ,cvalu0    ,cvalv0    ,gdp       )
    !
    ! In case of equilibrium formulation, copy equilibrium values to bedform
    ! height array.
    !
    if (bdfrlxtype == 0) then
       duneheight = duneheightequi
       return
    endif
    !
    ! Calculate staggered bedform celerity components and bedform celerity (ubedform)
    !
    select case (bdfrlxtype)
       case(1:3)
          !
          ! The bedform celerity is needed for advection and relaxation types
          ! 2 and 3.
          !
          if (bdfrlxtype /= 1 .or. lfbedfrmADV) then
             ! CH = aC*(U^bC)/H
             hpow = (cdpar(2)-1.0_fp)/2.0_fp
             do nm = 1, nmmax
                if (kfs(nm)>0) then
                   !
                   ! Compute bedform celerity
                   !
                   depth = s1(nm) + real(dps(nm),fp)
                   !
                   if (kfu(nm)>0) then
                      depthu = max(0.5_fp*(s1(nm+icx) + real(dps(nm+icx),fp) + depth),dryflc)
                      vmean_at_u = 0.25_fp*(vmean(nm) + vmean(nm-icy) + vmean(nm+icx) + vmean(nm+icx-icy))
                      utot2 = umean(nm)**2+vmean_at_u**2
                      !
                      qbedformx(nm) = cdpar(1)*(utot2**hpow)*umean(nm)/depthu
                   else
                      qbedformx(nm) = 0.0_fp
                   endif
                   !
                   if (kfu(nm)>0) then
                      depthv = max(0.5_fp*(s1(nm+icy) + real(dps(nm+icy),fp) + depth),dryflc)
                      umean_at_v = 0.25_fp*(umean(nm) + umean(nm-icx) + umean(nm+icy) + umean(nm-icx+icy))
                      utot2 = umean_at_v**2+vmean(nm)**2
                      !
                      qbedformy(nm) = cdpar(1)*(utot2**hpow)*vmean(nm)/depthv
                   else
                      qbedformy(nm) = 0.0_fp
                   endif
                   !
                   ubedform(nm)  = 0.5_fp*sqrt((qbedformx(nm-icx)+qbedformx(nm))**2+(qbedformy(nm-icy)+qbedformy(nm))**2)
                else
                   qbedformx(nm) = 0.0_fp
                   qbedformy(nm) = 0.0_fp
                   ubedform(nm)  = 0.0_fp
                endif
             enddo
          endif
       case(4)
          ! CH = max[(H/H_max)^CHp,G_min] * CHn * S / [H * (1-Fr^2)]
          do nm = 1, nmmax
             if (kfs(nm)>0) then
                !
                ! Compute bedform celerity
                !
                depth = s1(nm) + real(dps(nm),fp)
                !
                if (kfu(nm)>0) then
                   depthu = max(0.5_fp*(s1(nm+icx) + real(dps(nm+icx),fp) + depth),dryflc)
                   gamma = min(max((depthu/bdfHmax)**bdfC_Hp,bdfGmin),1.0_fp)
                   vmean_at_u = 0.25_fp*(vmean(nm) + vmean(nm-icy) + vmean(nm+icx) + vmean(nm+icx-icy))
                   fr_loc2 = (umean(nm)**2+vmean_at_u**2)/ag/depthu
                   sbu = 0.0_fp
                   do lsed = 1, lsedtot
                      sbu = sbu + sbuu(nm,lsed)
                   enddo
                   qbedformx(nm) = gamma*bdfC_Hn*sbu/depthu/max(1.0_fp-fr_loc2,0.1_fp)
                else
                   qbedformx(nm) = 0.0_fp
                endif
                !
                if (kfv(nm)>0) then
                   depthv = max(0.5_fp*(s1(nm+icy) + real(dps(nm+icy),fp) + depth),dryflc)
                   gamma = max((depthv/bdfHmax)**bdfC_Hp,bdfGmin)
                   umean_at_v = 0.25_fp*(umean(nm) + umean(nm-icx) + umean(nm+icy) + umean(nm-icx+icy))
                   fr_loc2 = (umean_at_v**2+vmean(nm)**2)/ag/depthv
                   sbv = 0.0_fp
                   do lsed = 1, lsedtot
                      sbv = sbv + sbvv(nm,lsed)
                   enddo
                   qbedformy(nm) = gamma*bdfC_Hn*sbv/depthv/max(1.0_fp-fr_loc2,0.1_fp)
                else
                   qbedformy(nm) = 0.0_fp
                endif
                !
                ubedform(nm)  = 0.5_fp*sqrt((qbedformx(nm-icx)+qbedformx(nm))**2+(qbedformy(nm-icy)+qbedformy(nm))**2)
             else
                qbedformx(nm) = 0.0_fp
                qbedformy(nm) = 0.0_fp
                ubedform(nm)  = 0.0_fp
             endif
          enddo
    end select
    !
    ! Calculate bedform fluxes (qbedformx and qbedformy)
    !
    if (lfbedfrmADV) then
       do nm = 1, nmmax
          !
          ! Compute bedform fluxes
          !
          qbedformx(nm) = qbedformx(nm)*guu(nm)
          qbedformy(nm) = qbedformy(nm)*gvv(nm)
       enddo
    else
       qbedformx = 0.0_fp
       qbedformy = 0.0_fp
    endif
    !
    ! Calculate the growth rate of the dune
    !     
    select case (bdfrlxtype)
       case(1)
          ! TH = given
          T_relax          = max(bedformT_H,hdtb)
          do nm = 1, nmmax
             if (kfs(nm)>0) then
                sour(nm) = duneheightequi(nm)/T_relax
                sink(nm) = 1.0_fp/T_relax
             endif
          enddo
       case(2)
          ! LH = given
          ! TH = LH/CH
          do nm = 1, nmmax
             if (kfs(nm)>0) then
                if ( abs(ubedform(nm)) > 1e-20_fp ) then
                   T_relax  = max(bedformL_H/ubedform(nm),hdtb)
                   sour(nm) = duneheightequi(nm)/T_relax
                   sink(nm) = 1.0_fp/T_relax
                endif
             endif
          enddo
       case(3)
          ! LH = LHc * dunelength
          ! TH = LH/CH
          do nm = 1, nmmax
             if (kfs(nm)>0) then
                bedformL_H = bdfL_Hc*dunelength(nm)
                if ( abs(ubedform(nm)) > 1e-20_fp ) then
                   T_relax  = max(bedformL_H/ubedform(nm),hdtb)
                   sour(nm) = duneheightequi(nm)/T_relax
                   sink(nm) = 1.0_fp/T_relax
                endif
             endif
          enddo
       case(4)
          ! LH = min[(H_max/H)^LHp,P_max] * dunelength
          ! TH = LH/CH
          do nm = 1, nmmax
             if (kfs(nm)>0) then
                depth = max(s1(nm) + real(dps(nm),fp),dryflc)
                phi   = max(min((bdfHmax/depth)**bdfL_Hp,bdfPmax),1.0_fp)
                bedformL_H = phi*dunelength(nm)
                if ( abs(ubedform(nm)) > 1e-20_fp ) then
                   T_relax  = max(bedformL_H/ubedform(nm),hdtb)
                   sour(nm) = duneheightequi(nm)/T_relax
                   sink(nm) = 1.0_fp/T_relax
                endif
             endif
          enddo
    end select    
    !
    ! Perform optional check on CFL condition for bedform advection.
    !
    if (lfbedfrmADV .and. lfbedfrmCFL) then
       do nm = 1, nmmax
          cflcheck = 2.0_fp*max(abs(qbedformx(nm)),abs(qbedformy(nm)))*hdtb/(gvv(nm)*guu(nm))
          if (cflcheck > 1.0_fp) then
             call prterr(lundia, 'U190', 'CALBF: Possible violation of CFL-condition [Keywords: BdfaC, BdfbC, MorFac]' )
             lfbedfrmCFL = .false.
             exit
          endif
       enddo
    endif
    !
    ! Set the boundary conditions for the advection solver
    !
    bval = 0.0_fp
    do ic = 1, norow+nocol
       !
       ! Neumann boundary (dH/dn=0) at all open boundaries
       ! Dirichlet boundary (H=0) at all closed bounaries
       !
       if (irocol(4, ic) /= 1) then
          ibtyp(1,ic) = 2
       else
          ibtyp(1,ic) = 1
       endif
       if (irocol(5, ic) /= 1) then
          ibtyp(2,ic) = 2
       else
          ibtyp(2,ic) = 1
       endif
    enddo
    !
    ! Calculate the advection and relaxation of the dunes
    !
    if (stage == 'stage1') then
       call adv2d(hdtb      ,lundia    ,nst       ,icx       ,icy       , &
                & nmmax     ,norow     ,irocol    ,kadu      ,kadv      , &
                & kcs       ,kcu       ,kfs       ,kfu       ,kfv       , &
                & kmax      ,ibtyp     ,bval      ,kmax      , &
                & qbedformx ,qbedformy ,gsqs      ,duneheight,e1        , &
                & sour      ,sink      ,bbk       ,bdddx     ,bddx      , &
                & bdx       ,bux       ,buux      ,buuux     ,uvdwk     , &
                & vvdwk     ,bbkl      ,ddkl      ,0         ,gdp       )
    else
       call adv2d(hdtb      ,lundia    ,nst       ,icy       ,icx       , &
                & nmmax     ,nocol     ,irocol(1, norow+1)   ,kadv      ,kadu      , &
                & kcs       ,kcv       ,kfs       ,kfv       ,kfu       , &
                & kmax      ,ibtyp     ,bval      ,kmax      , &
                & qbedformy ,qbedformx ,gsqs      ,duneheight,e1        , &
                & sour      ,sink      ,bbk       ,bdddx     ,bddx      , &
                & bdx       ,bux       ,buux      ,buuux     ,uvdwk     , &
                & vvdwk     ,bbkl      ,ddkl      ,0         ,gdp       )
    endif
    !
    ! Copy solution back to duneheight
    !
    duneheight = e1
end subroutine calbf
