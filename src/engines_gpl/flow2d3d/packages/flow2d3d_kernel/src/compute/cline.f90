subroutine cline(hdt       ,j         ,nmmaxj    ,kmax      ,icx       , &
               & icy       ,icxy      ,mf        ,nf        ,dxf       , &
               & dyf       ,xf        ,yf        ,u1        ,v1        , &
               & xcor      ,ycor      ,guu       ,gvv       ,guv       , &
               & gvu       ,kcu       ,kcv       ,kcs       ,kfu       , &
               & kfv       ,windxt    ,windyt    ,windft    , &
               & s1        ,dpu       ,dpv       ,thick     ,dpdro     , &
               & kfumin    ,kfumax    ,kfvmin    ,kfvmax    , &
               & dzu1      ,dzv1      ,zk        ,gdp       )
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
!    Function: - calculates position of one drogue if release time < NST
!                if stop time > NST and drogue on active point
! Method used:
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
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                  , pointer :: ifirst
    integer                  , pointer :: ndim
    integer                  , pointer :: md
    integer                  , pointer :: loop
    integer,  dimension(:)   , pointer :: inc
    real(fp), dimension(:,:) , pointer :: ud
    real(fp), dimension(:,:) , pointer :: xd
    real(fp), dimension(:)   , pointer :: rdep
    integer                  , pointer :: lundia
    logical                  , pointer :: wind
    logical                  , pointer :: temp
    logical                  , pointer :: drogue
    logical                  , pointer :: zmodel
!
! Global variables
!
    integer                                         , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX
                                                                            !!  then computation proceeds in the X-
                                                                            !!  dir. If icx=1 then computation pro-
                                                                            !!  ceeds in the Y-dir.
    integer                                         , intent(in)  :: icxy   !!  Max (ICX  ,ICY )
    integer                                         , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                       :: j      !!  Begin pointer for arrays which have
                                                                            !!  been transformed into 1D arrays.
                                                                            !!  Due to the shift in the 2nd (M-)
                                                                            !!  index, J = -2*NMAX + 1
    integer                                         , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                       :: mf     !!  M-coordinate at start of calculation
                                                                            !!  and after calculation (-><- Cline)
    integer                                                       :: nf     !!  N coordinate at start of calculation
                                                                            !!  and after calculation (-><- Cline)
    integer                                                       :: nmmaxj !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)   :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)   :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)   :: kcv    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)   :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)   :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)   :: kfumax !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)   :: kfumin !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)   :: kfvmax !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)   :: kfvmin !  Description and declaration in esm_alloc_int.f90
    real(fp)                                                      :: dxf    !!  Delta X at start of calculation
                                                                            !!  and after calculation (-><- Cline)
    real(fp)                                                      :: dyf    !!  Delta Y at start of calculation
                                                                            !!  and after calculation (-><- Cline)
    real(fp)                                        , intent(in)  :: hdt    !  Description and declaration in esm_alloc_real.f90
    real(fp)                                        , intent(in)  :: windft !  Description and declaration in trisol.igs
    real(fp)                                        , intent(in)  :: windxt !  Description and declaration in trisol.igs
    real(fp)                                        , intent(in)  :: windyt !  Description and declaration in trisol.igs
    real(fp)                                        , intent(out) :: xf     !!  X coordinate at start of calculation
                                                                            !!  and after calculation (-><- Cline)
    real(fp)                                        , intent(out) :: yf     !!  Y coordinet at start of calculation
                                                                            !!  and after calculation (-><- Cline)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: xcor   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: ycor   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: dpu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: dpv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dzu1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dzv1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                       , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax)                     , intent(in)  :: zk
    real(fp)                                        , intent(in)  :: dpdro  ! drodep for one drogue
!
! Local variables
!
    integer                              :: ddb
    integer                              :: i       ! Hulp Var.
    integer                              :: istat
    integer                              :: m       ! M-coordinate starts at MF 
    integer                              :: n       ! N coordinate starts at NF 
    integer                              :: ndm     ! N-1,M array index 
    integer                              :: ndmd    ! N-1,M-1 array index 
    integer                              :: num     ! N+1,M array index
    integer                              :: nm      ! N,M array index 
    integer                              :: nmd     ! N,M-1 array index 
    integer                              :: nmu     ! N,M+1 array index
    integer                              :: kc
    integer                              :: kfmin
    integer                              :: kfmax
    integer                              :: kfstep
    real(fp)                             :: dtn     ! Maximum time of drogues in 1 cell 
    real(fp)                             :: dxdeta  ! X distance between (n,m) and (n-1,m) 
    real(fp)                             :: dxdksi  ! X distance between (n,m) and (n,m-1) 
    real(fp)                             :: dydeta  ! Y distance between (n,m) and (n-1,m) 
    real(fp)                             :: dydksi  ! Y distance between (n,m) and (n,m-1) 
    real(fp)                             :: guvndm
    real(fp)                             :: guvnm
    real(fp)                             :: gvunm
    real(fp)                             :: gvunmd
    real(fp)                             :: te      ! Time to pass cell 
    real(fp)                             :: wcu     ! Windcorrection on u-velocity 
    real(fp)                             :: wcv     ! Windcorrection on v-velocity 
    real(fp)                             :: xr      ! Hulp var. for XD(1,1) 
    real(fp)                             :: yr      ! Hulp var. for XD(2,1) 
    real(fp)                             :: wlev
    real(fp)                             :: uu1
    real(fp)                             :: uu2
    real(fp)                             :: vv1
    real(fp)                             :: vv2
    real(fp)                             :: interp
!
!! executable statements -------------------------------------------------------
!
    ifirst     => gdp%gdcline%ifirst
    ndim       => gdp%gdcline%ndim
    md         => gdp%gdcline%md
    loop       => gdp%gdcline%loop
    inc        => gdp%gdcline%inc
    ud         => gdp%gdcline%ud
    xd         => gdp%gdcline%xd
    rdep       => gdp%gdcline%rdep
    lundia     => gdp%gdinout%lundia
    wind       => gdp%gdprocs%wind
    temp       => gdp%gdprocs%temp
    drogue     => gdp%gdprocs%drogue
    zmodel     => gdp%gdprocs%zmodel
    !
    if (ifirst == 1) then
       ifirst = 0
                     allocate (gdp%gdcline%inc (ndim)    , stat = istat)
       if (istat==0) allocate (gdp%gdcline%ud  (ndim, 2 ), stat = istat)
       if (istat==0) allocate (gdp%gdcline%xd  (ndim, md), stat = istat)
       if (istat==0) allocate (gdp%gdcline%rdep(0:kmax+1), stat = istat)
       if (istat/=0) then
          call prterr(lundia, 'U021', 'Cline: memory alloc error')
          call d3stop(1, gdp)
       endif
       !
       ! update the local pointers
       !
       inc        => gdp%gdcline%inc
       ud         => gdp%gdcline%ud
       xd         => gdp%gdcline%xd
       rdep       => gdp%gdcline%rdep
    endif
    !
    ! initialition of m,n xd and inc
    !
    ddb       = gdp%d%ddbound
    m         = mf
    n         = nf
    xd(1, md) = dxf
    xd(2, md) = dyf
    inc(1)    = 0
    inc(2)    = 0
    uu1       = 0.0_fp
    uu2       = 0.0_fp
    vv1       = 0.0_fp
    vv2       = 0.0_fp
    !
    ! time in seconds (velocities in m/s)
    !
    dtn = hdt
    !
    ! walk through model with drogue as long as DTN > 0 and
    ! inc(1) / inc(2) not equal 0 (not first time i=1)
    !
    do i = 1, loop
       !
       if (dtn < 0.0) then
          exit
       endif
       if (i>1 .and. inc(1)==0 .and. inc(2)==0) then
          exit
       endif
       !
       ! redefine m,n if drogue hit cell boundary during time step
       ! (see sline)
       !
       m = m + inc(1)
       n = n + inc(2)
       !
       ! initialize nm, nmd, ndm and ndmd
       ! the first time kcs (nm) = 1 (see per3), so n,m will be inside
       ! 2 <= m <= mmax-1 and 2 <= n <= nmax-1 the first time.
       ! Substraction or addition by 1, can never cause an array out of
       ! bound
       !
       nm   = (n + ddb)*icy + (m + ddb)*icx - icxy
       nmd  = nm - icx
       nmu  = nm + icx
       ndm  = nm - icy
       num  = nm + icy
       ndmd = ndm - icx
       !
       ! test for active point (kcs (nm) = 1)
       ! if kcs (nm) = 0 => permanent dry point
       ! if kcs (nm) = 2 => open boundary point (for left and lower
       !                       boundaries xcor (ndmd) might not exist)
       !
       if (kcs(nm) /= 1) then
          exit
       endif
       !
       ! test for existing gvu and guv by testing if
       ! mask arrays kcu and kcv in these points are not equal 0
       !
       if (kcu(nm)==0 .and. kcu(nmd)==0 .and. kcv(nm)==0 .and. kcv(ndm)==0) then
          exit
       endif
       !
       ! reset gvu and guv depending on kcu and kcv values.
       ! if kcu = 0 then per definition kfu = 0
       !
       gvunm  = real(kcu(nm) ,fp)*gvu(nm)  + real(1 - kcu(nm) ,fp)
       gvunmd = real(kcu(nmd),fp)*gvu(nmd) + real(1 - kcu(nmd),fp)
       guvnm  = real(kcv(nm) ,fp)*guv(nm)  + real(1 - kcv(nm) ,fp)
       guvndm = real(kcv(ndm),fp)*guv(ndm) + real(1 - kcv(ndm),fp)
       !
       ! jump out of 'loop' if head bites tail
       !
       if (i>1 .and. m==mf .and. n==nf) then
          exit
       endif
       !
       ! Compute velocities at the four cell boundaries
       ! at a certain depth to be used by the drogue
       ! transport routines
       !
       if (zmodel) then
          kfstep = -1
       else
          kfmin  = 1
          kfmax  = kmax
          kfstep = 1
       endif
       !
       kc = min(1,kcs(nmd))
       wlev = (kcs(nm)*s1(nm) + kc*s1(nmd))/(kcs(nm) + kc)
       call layerdep(rdep  , thick, kmax, nmd   , dpu   , wlev, &
                   & zmodel, zk   , dzu1, kfumin, kfumax, gdp )
       if (zmodel) then
           kfmin  = kfumax(nmd)
           kfmax  = kfumin(nmd)
       endif
       if (kfu(nmd)/=0) then
           uu1 = interp (rdep, u1(nmd,:), kfmin, kfmax, kfstep, kmax, dpdro)
       endif
       !
       kc = min(1,kcs(nmu))
       wlev = (kcs(nm)*s1(nm) + kc*s1(nmu))/(kcs(nm) + kc)
       call layerdep(rdep  , thick, kmax, nm    , dpu   , wlev, &
                   & zmodel, zk   , dzu1, kfumin, kfumax, gdp )
       if (zmodel) then
          kfmin  = kfumax(nm)
          kfmax  = kfumin(nm)
       endif
       if (kfu(nm)/=0) then
           uu2 = interp (rdep, u1(nm ,:), kfmin, kfmax, kfstep, kmax, dpdro)
       endif
       !
       kc = min(1,kcs(ndm))
       wlev = (kcs(nm)*s1(nm) + kc*s1(ndm))/(kcs(nm) + kc)
       call layerdep(rdep  , thick, kmax, ndm   , dpv   , wlev, &
                   & zmodel, zk   , dzv1, kfvmin, kfvmax, gdp )
       if (zmodel) then
          kfmin  = kfvmax(ndm)
          kfmax  = kfvmin(ndm)
       endif
       if (kfv(ndm)/=0) then
           vv1 = interp (rdep, v1(ndm,:), kfmin, kfmax, kfstep, kmax, dpdro)
       endif 
       !
       kc = min(1,kcs(num))
       wlev = (kcs(nm)*s1(nm) + kc*s1(num))/(kcs(nm) + kc)
       call layerdep(rdep  , thick, kmax, nm    , dpv   , wlev, &
                   & zmodel, zk   , dzv1, kfvmin, kfvmax, gdp )
       if (zmodel) then
          kfmin  = kfvmax(nm)
          kfmax  = kfvmin(nm)
       endif
       if (kfv(nm)/=0) then
           vv2 = interp (rdep, v1(nm ,:), kfmin, kfmax, kfstep, kmax, dpdro)
       endif
       !
       !                                         ud(2,2)
       !                                         --
       ! initialisation velocities      ud(1,1) |  | ud(1,2)
       ! between 2 cell boundaries:              --
       !   scaled with cell-length               ud(2,1)
       !
       ! take in account temporary drypoints (mask kfu, kfv)
       !
       if (abs(windft)<1.E-6) then
          ud(1, 1) = kfu(nmd)*uu1/gvunmd
          ud(1, 2) = kfu(nm) *uu2/gvunm
          ud(2, 1) = kfv(ndm)*vv1/guvndm
          ud(2, 2) = kfv(nm) *vv2/guvnm
       else
          dxdeta   = xcor(nm) - xcor(ndm)
          dydeta   = ycor(nm) - ycor(ndm)
          dxdksi   = xcor(nm) - xcor(nmd)
          dydksi   = ycor(nm) - ycor(nmd)
          wcu      = (windxt*dydeta - windyt*dxdeta)/guu(nm)
          wcv      = (windyt*dxdksi - windxt*dydksi)/gvv(nm)
          ud(1, 1) = kfu(nmd)*(uu1 + wcu)/gvunmd
          ud(1, 2) = kfu(nm) *(uu2 + wcu)/gvunm
          ud(2, 1) = kfv(ndm)*(vv1 + wcv)/guvndm
          ud(2, 2) = kfv(nm) *(vv2 + wcv)/guvnm
       endif
       !
       ! initialisation coordinates
       !
       if (inc(1) == 0) then
          xd(1, 1) = xd(1, md)
       elseif (inc(1) == 1) then
          xd(1, 1) = 0.0
       else
          xd(1, 1) = 1.0
       endif
       !
       if (inc(2) == 0) then
          xd(2, 1) = xd(2, md)
       elseif (inc(2) == 1) then
          xd(2, 1) = 0.0
       else
          xd(2, 1) = 1.0
       endif
       !
       ! calculate coordinates drogue movement for one cell
       ! update time spend in one cell
       !
       call sline(dtn       ,md        ,ndim      ,te        ,xd        , &
                & ud        ,inc       )
       dtn = dtn - te
       !
       ! calculate and write streamline coordinates
       !
       xr = xd(1, md)
       yr = xd(2, md)
       xf = (1.0 - xr)*(1.0 - yr)*xcor(ndmd) + xr*(1.0 - yr)*xcor(ndm) + (1.0 - xr) &
          & *yr*xcor(nmd) + xr*yr*xcor(nm)
       yf = (1.0 - xr)*(1.0 - yr)*ycor(ndmd) + xr*(1.0 - yr)*ycor(ndm) + (1.0 - xr) &
          & *yr*ycor(nmd) + xr*yr*ycor(nm)
    enddo
    !
    ! exit 'loop'
    !
    !
    ! redefine mf,nf and dxf,dyf
    !
    mf  = m
    nf  = n
    dxf = xd(1, md)
    dyf = xd(2, md)
end subroutine cline
