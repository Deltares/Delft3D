subroutine z_drychk(idry      ,j         ,nmmaxj    ,nmmax     ,kmax      , &
                  & nfltyp    ,icx       ,icy       ,kfu       ,kfv       , &
                  & kfs       ,kcs       ,kfuz0     ,kfvz0     ,kfsz1     , &
                  & kfsmin    ,kfsmn0    ,kfsmax    ,kfsmx0    ,s1        , &
                  & r1        ,dps       ,qxk       ,qyk       ,w1        , &
                  & lstsci    ,dzs1      ,zk        ,nst       ,gdp       )
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
!    Function: This subroutine checks for drying in water level
!              points. In case the point is dry, all surrounding
!              mask arrays (KFU and KFV) are set to zero and sub-
!              sequently SUD computation will be repeated
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer  , pointer :: lundia
    real(fp) , pointer :: drycrt
    real(fp) , pointer :: dzmin
!
! Global variables
!
    integer                                             , intent(in)  :: icx    !  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                             , intent(in)  :: icy    !  Increment in the Y-dir. (see ICX)
    integer                                             , intent(out) :: idry   !  Flag set to 1 if a dry point is detected in routine DRYCHK after SUD is completed
    integer                                                           :: j      !  Begin pointer for arrays which have been transformed into 1D arrays. Due to the shift in the 2nd (M-) index, J = -2*NMAX + 1
    integer                                             , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                           :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                           :: nfltyp !  Description and declaration in esm_alloc_int.f90
    integer                                             , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer                                                           :: nmmaxj !  Description and declaration in dimens.igs
    integer                                                           :: nst    !  Time step number
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfsmax !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfsmx0 !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfsmn0 !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(out) :: kfsz1  !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(out) :: kfuz0  !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(out) :: kfvz0  !  Description and declaration in esm_alloc_int.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)        :: r1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: w1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(out) :: dzs1   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(out) :: qxk    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(out) :: qyk    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(0:kmax), intent(in) :: zk
!
! Local variables
!
    integer                :: k
    integer                :: l
    integer                :: ndm
    integer                :: nm
    integer                :: nmd
    integer                :: nm_pos    ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
    integer, dimension(1)  :: nm_s1max1
    integer, dimension(1)  :: nm_s1max2
    logical                :: flood
    logical                :: zmodel
    real(fp)               :: drytrsh
    real(fp)               :: s1max1
    real(fp)               :: s1max2
    real(fp)               :: zdiff
    character(300)         :: errmsg
!
!! executable statements -------------------------------------------------------
!
    lundia  => gdp%gdinout%lundia
    drycrt  => gdp%gdnumeco%drycrt
    dzmin   => gdp%gdzmodel%dzmin
    !
    idry   = 0
    nm_pos = 1
    !
    ! A drying treshold to avoid very thin layers in active cells
    ! 0.01 * dryflc (0.02*drycrt), but limited between 10^(-9) and 10^(-3)
    ! Such thin layers cause inaccuracies in the solution of the transport equation (in conservative formulation)
    ! This choice of limits was chosen avoid too thin layers, but to simultaneously allow thin layers when 
    ! demanded by the user through a small dryflc, e.g. for dry dambreak problems.
    !
    drytrsh = max(1.0e-9_fp, min(0.02_fp*drycrt, 1.0e-3_fp))
    do nm = 1, nmmax
       nmd = nm - icx
       ndm = nm - icy
       if ( (kcs(nm)==1 .or. kcs(nm)==2) ) then
          !
          ! Check on kfs(nm) == 1 is necessary, because when the last active cell edge of a cell
          ! was set dry in Z_SUD, all KFU/KFV are zero and this check would not be performed
          ! 
          if (kfu(nm)==1 .or. kfu(nmd)==1 .or. kfv(nm)==1 .or. kfv(ndm)==1 .or. kfs(nm)==1) then
             if ( s1(nm) <= -real(dps(nm),fp)+drytrsh ) then
                kfu(nm ) = 0
                kfu(nmd) = 0
                kfv(nm ) = 0
                kfv(ndm) = 0
                do k = 1, kmax  ! evt. kfsmin, kmax
                   kfuz0(nm , k) = 0
                   kfuz0(nmd, k) = 0
                   kfvz0(nm , k) = 0
                   kfvz0(ndm, k) = 0
                   qxk  (nm , k) = 0.0_fp
                   qxk  (nmd, k) = 0.0_fp
                   qyk  (nm , k) = 0.0_fp
                   qyk  (ndm, k) = 0.0_fp
                   dzs1 (nm , k) = 0.0_fp
                enddo
                !
                ! At least one cell that was active has become dry:
                ! Redo Z_SUD (from Z_ADI) with these cells isolated
                !
                idry = 1
             endif
          endif
       endif
    enddo
    !
    ! Determine global maximum of 'idry' over all nodes
    ! Note: this enables to synchronize the repeating computation of SUD
    !
    call dfreduce_gdp( idry, 1, dfint, dfmax, gdp )
    !
    ! CHECK FOR FOUR DRY VELOCITY POINTS
    !
    s1max1 = -999.0_fp
    s1max2 = -999.0_fp
    do nm = 1, nmmax
       !
       ! determination number of layers at old time level in case of flooding.
       !
       if (kcs(nm) > 0) then
          nmd = nm - icx
          ndm = nm - icy
          kfs(nm) = max(kfu(nm), kfu(nmd), kfv(nm), kfv(ndm))
       endif
       !
       if (kcs(nm) > 0) then
          !
          ! 15-3-2007 change to allow S1 > ZK(KMAX), needed for NH-models
          !
          if ( s1(nm) >= zk(kmax) ) then
             kfsmax(nm) = kmax
          else
             do k = kfsmin(nm), kmax
                if ( zk(k)+dzmin >= s1(nm) ) then
                   kfsmax(nm) = k
                   exit
                endif
             enddo
          endif
          !
          ! Find the (new) location of the bed layer
          !
          do k = 1, kmax
             if ( zk(k)-dzmin >= -real(dps(nm),fp) .or. k == kmax) then
                kfsmin(nm) = k
                exit
             endif
          enddo
          !
          ! determination number of layers at new time level
          !
          kfsz1(nm,:) = 0
          do k = kfsmin(nm), kfsmax(nm)
             kfsz1(nm, k) = kfs(nm)
          enddo
       endif
    enddo
    !
    ! Delft3D-16494: NOT NECESSARY?
    !
    ! Exchange mask array kfs with neighbours for parallel runs
    !
    call dfexchg ( kfs, 1,    1, dfint, nm_pos, gdp )
    !
    ! issue warning if maximum water level is above zk(kmax) (ZTOP)
    !
    s1max1    = maxval(s1)
    s1max2    = maxval(s1(1:nmmax))
    nm_s1max1 = maxloc(s1)
    nm_s1max2 = maxloc(s1(1:nmmax))
    kfsmax(nm_s1max1(1)) = max(kfsmax(nm_s1max1(1)),1)
    kfsmax(nm_s1max2(1)) = max(kfsmax(nm_s1max2(1)),1)
    if (s1max1 > zk(kmax)+0.5*(dzs1(nm_s1max1(1),kfsmax(nm_s1max1(1)))).and. kfs(nm_s1max1(1)) == 1) then
       write (errmsg, '(a,g10.3,2a,i0,a,i0,a,i0,2a)') '1: Maximum water level ', s1max1, &
                    & ', (m). Top layer is too thick. ',                                 &
                    & ', for nm = ', nm_s1max1, ', icx = ', icx,                         &
                    & ' and nst = ', nst,                                                &
                    & '; Changing ZTOP is strongly advised'
       call prterr(lundia, 'U190', trim(errmsg))
    endif
    !
    if (s1max2 > zk(kmax)+0.5*(dzs1(nm_s1max2(1),kfsmax(nm_s1max2(1)))).and. kfs(nm_s1max2(1)) == 1) then
       write (errmsg, '(a,g10.3,2a,i0,a,i0,a,i0,2a)') '2: Maximum water level ', s1max2, &
                    & ', (m). Top layer is too thick.',                                  &
                    & ', for nm = ', nm_s1max2, ', icx = ', icx,                         &
                    & ' and nst = ', nst,                                                &
                    & '; Changing ZTOP is strongly advised'
       call prterr(lundia, 'U190', trim(errmsg))
    endif
    !
    ! Recalculate DZS1
    ! Reset all DZS1 to for all inactive points above KFSMAX
    !
    do nm = 1, nmmax
       if (kcs(nm) /= 0) then
          do k = kfsmin(nm), kfsmax(nm)
             if (kfsmin(nm) == kfsmax(nm)) then
                dzs1(nm, k) = real(dps(nm),fp) + s1(nm)
             elseif (k == kfsmin(nm)) then
                dzs1(nm, k) = zk(k) + real(dps(nm),fp)
             elseif (k == kfsmax(nm)) then
                dzs1(nm, k) = s1(nm) - zk(k - 1)
             else
                dzs1(nm, k) = zk(k) - zk(k - 1)
             endif
          enddo
          do k = kfsmax(nm) + 1, kmax
             dzs1(nm, k) = 0.0_fp
          enddo
       endif
    enddo
    !
    ! A "trick" to ensure that "wet" points that were dry
    ! obtain a velocity (see also Z_CHECKU and Z_DIFU)
    !
    do nm = 1, nmmax
       if (kfs(nm) == 1) then
          if (kfsmin(nm) < kfsmn0(nm)) then
             do k = kfsmin(nm), kfsmn0(nm)-1
                w1(nm, k) = w1(nm, kfsmn0(nm))
             enddo
          elseif (kfsmin(nm) > kfsmn0(nm)) then
             do k = kfsmn0(nm), kfsmin(nm)-1
                w1(nm, k) = 0.0_fp
             enddo
          endif
          if (kfsmax(nm) > kfsmx0(nm)) then
             do k = kfsmx0(nm), kfsmax(nm)
                w1(nm, k) = w1(nm, kfsmx0(nm))
             enddo
          endif
       endif
    enddo
    do nm = 1, nmmax
       !
       ! Copy concentration to new layer. NB. KFS might be equal to zero!
       !
       if ((kcs(nm)==1 .or. kcs(nm)==-1) .and. kfsmax(nm) > kfsmx0(nm)) then
          do k = kfsmx0(nm)+1, kfsmax(nm)
             do l = 1, lstsci
                r1(nm, k, l) = r1(nm, kfsmx0(nm), l)
             enddo
          enddo
       endif
       !
       ! Copy concentration to bottom layer. NB. KFS might be equal to zero!
       !
       if (kcs(nm).eq.1 .and. kfsmin(nm) < kfsmn0(nm)) then
          do k = kfsmin(nm), kfsmn0(nm)-1
             do l = 1, lstsci
                r1(nm, k, l) = r1(nm, kfsmn0(nm), l)
             enddo
          enddo
       endif
    enddo
end subroutine z_drychk
