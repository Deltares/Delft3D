subroutine incbcc(lundia    ,timnow    ,zmodel    ,nmax      ,mmax      , &
                & kmax      ,nto       ,nrob      ,lstsc     ,noroco    , &
                & tprofc    ,itbcc     ,mnbnd     ,nob       ,kstp      , &
                & kfsmin    ,kfsmax    ,rob       ,rbnd      ,guu       , &
                & gvv       ,dps       ,s0        ,sig       ,procbc    , &
                & zstep     ,dzs1      ,zk        ,gdp       )
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
!    Function: Carry out interpolation in space, determine time
!              increments and updates the time dependent const.
!              BC (if NTO > 0 and LSTSC > 0)
! Method used: - At each time step the increment values are added
!                to update the values at the vertices of the
!                boundaries
!              - Hereafter space interpolation is applied to cal-
!                culate the boundary values at each point.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
    use globaldata
    use m_openda_exchange_items, only : get_openda_buffer
    !
    implicit none
    !
    ! Enumeration
    !
    integer, parameter :: start_pivot = 1
    integer, parameter ::   end_pivot = 2
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                 , pointer :: ltem
    integer                 , pointer :: lsal
    real(fp)                , pointer :: tstop
    integer                 , pointer :: itfinish
    integer                 , pointer :: lunbcc
    logical                 , pointer :: newstp
    real(fp), dimension(:,:), pointer :: dist_pivot_part
!
! Global variables
!
    integer                                                                                :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                :: lstsc  !  Description and declaration in dimens.igs
    integer                                                                                :: lundia !  Description and declaration in inout.igs
    integer                                                                                :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                  , intent(in)  :: noroco !  Description and declaration in esm_alloc_int.f90
    integer                                                                  , intent(in)  :: nrob   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                :: nto    !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(7, nto)                                         , intent(in)  :: mnbnd  !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(5, nto, lstsc)                                                :: itbcc  !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(8, nrob)                                        , intent(in)  :: nob    !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     :: kfsmax !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(nrob, lstsc)                                                  :: kstp   !  Description and declaration in esm_alloc_int.f90
    logical                                                                  , intent(in)  :: zmodel !  Description and declaration in procs.igs
    real(fp)                                                                               :: timnow !!  Current timestep (multiples of dt)
    real(fp)     , dimension(2, nto, lstsc)                                                :: zstep  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(4, nto, kmax, lstsc)                                          :: procbc !  Description and declaration in esm_alloc_real.f90
    real(prec)   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax) , intent(in)  :: dzs1   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(kmax)                                           , intent(in)  :: sig    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(0:kmax)                                         , intent(in)  :: zk
    real(fp)     , dimension(kmax, max(lstsc, 1), 2, noroco)                               :: rbnd   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(kmax, nrob)                                                   :: rob    !  Description and declaration in esm_alloc_real.f90
    character(10), dimension(nto, lstsc)                                                   :: tprofc !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer                                      :: i
    integer                                      :: incx    ! Nr. of grid points-1 (in the x-dir.) between the begin and the end point of an opening section 
    integer                                      :: incy    ! Nr. of grid points-1 (in the y-dir.) between the begin and the end point of an opening section 
    integer                                      :: istsc   ! Index number of constituent 
    integer                                      :: ito     ! Index number of open boundary loc. 
    integer                                      :: j       ! Loop variable 
    integer                                      :: k       ! Loop counter over KMAX 
    integer                                      :: kp      ! First array index of array CIRC pointing to the nr. of row/column in array IROCOL
    integer                                      :: kpc     ! First array index of array CIRC pointing to the column number in arra IROCOL 
    integer                                      :: kpp     ! Hulp varible 
    integer                                      :: kpr     ! First array index of array CIRC pointing to the row number in array IROCOL 
    integer                                      :: kq      ! Second array index of array CIRC pointing to the nr. of row/column in array IROCOL 
    integer                                      :: kqc     ! Second array index of array CIRC pointing to the column number in arra IROCOL 
    integer                                      :: kqq     ! Hulp varible 
    integer                                      :: kqr     ! Second array index of array CIRC pointing to the row number in array IROCOL 
    integer                                      :: l       ! Loop variable for constituents 
    integer                                      :: maxinc  ! Max. of (INCX,INCY,1) 
    integer                                      :: mend    ! End coord. (in the x-dir.) of an open bound. section 
    integer                                      :: mp
    integer                                      :: msta    ! Starting coord. (in the x-dir.) of an open bound. section 
    integer                                      :: n       ! Loop variable 
    integer                                      :: n1      ! Pointer var. relating NOB to MNBND 
    integer                                      :: nend    ! End coord. (in the y-dir.) of an open bound. section 
    integer                                      :: np
    integer                                      :: nsta    ! Starting coord. (in the y-dir.) of an open bound. section
    integer                                      :: lb      ! lowerboundary of loopcounter
    integer                                      :: ub      ! upperboundary of loopcounter
    integer                                      :: mgg     ! M-coord. of the actual open boundary point, which may differ from the ori- ginal position due to grid staggering
    integer                                      :: ngg     ! N-coord. of the actual open boundary point, which may differ from the ori- ginal position due to grid staggering
    integer                                      :: posrel  ! code denoting the position of the open boundary, related to the complete grid
    logical                                      :: first   ! Flag = TRUE in case a time-dependent file is read for the 1-st time 
    real(fp)                                     :: dist    ! Real distance between an open bounda- ry point to the begin point of the related opening section 
    real(fp)                                     :: distx   ! Incremental distance (in the x-dir.) between two consecutive open boundary points belonging to the same section 
    real(fp)                                     :: disty   ! Incremental distance (in the x-dir.) between two consecutive open boundary points belonging to the same section 
    real(fp)                                     :: frac    ! Fraction between DIST and the total length of an opening section
    real(fp)                                     :: guuz1
    real(fp)                                     :: guuz2
    real(fp)                                     :: gvvz1
    real(fp)                                     :: gvvz2
    real(fp)                                     :: h0
    real(fp)                                     :: r1
    real(fp)                                     :: r2
    real(fp)                                     :: reldep
    real(fp)                                     :: sigjmp
    real(fp)                                     :: timscl  ! Multiple factor to create minutes from read times 
    real(fp)                                     :: totl    ! Actual length of an openbnd. section 
    real(fp)                                     :: zcord
    real(fp)                                     :: zjmp
    real(fp), dimension(:, :, :, :), allocatable :: procbc_old
!
!! executable statements -------------------------------------------------------
!
    ltem            => gdp%d%ltem
    lsal            => gdp%d%lsal
    newstp          => gdp%gdincbcc%newstp
    lunbcc          => gdp%gdluntmp%lunbcc
    itfinish        => gdp%gdinttim%itfinish
    tstop           => gdp%gdexttim%tstop
    dist_pivot_part => gdp%gdbcdat%dist_pivot_part
    if (lsal .ne. 0 .or. ltem .ne. 0) then
       allocate(procbc_old(2, nto, kmax, max(lsal,ltem)))
    endif
    !
    ! Initialization local parameters
    ! TIMSCL will not been used in UPDBCC
    !
    first  = .false.
    timscl = 1.0
    !
    ! Update values if necessary
    !
    do ito = 1, nto
       do istsc = 1, lstsc
          !
          ! When using several instances, it is necessary to do a partial rewind of the file since
          ! a newly selected instance has an older time stamp. 
          ! Luckily, this file (see updbcc) can be read by record.
          ! So we reset the record pointer to the start record if timnow < itbcc(1,..)
          !
          if (timnow < real(itbcc(1,ito,istsc),fp)) then
              itbcc(5, ito, istsc) = itbcc(3, ito, istsc) - 1

             first = .true.
             call updbcc(lunbcc    ,lundia    ,first     ,itbcc     ,ito       , &
                       & istsc     ,timnow    ,itfinish  ,timscl    , &
                       & nto       ,kmax      ,lstsc     ,procbc    ,tprofc    , &
                       & zstep     ,gdp       )
             !          
             ! Now, we have to update procbc(1,:) . Its current value is still the value in the
             ! file at itbcc(1,:)
             !
             do k = 1, kmax
                procbc(1,ito,k,istsc) = procbc(1,ito,k,istsc) +                                                 &
                                      & 2.0_fp*(timnow-0.5_fp - real(itbcc(1,ito,istsc)))*procbc(3,ito,k,istsc) 
                procbc(2,ito,k,istsc) = procbc(2,ito,k,istsc) +                                                 &
                                      & 2.0_fp*(timnow-0.5_fp - real(itbcc(1,ito,istsc)))*procbc(4,ito,k,istsc)               
             enddo 
          endif
          !
          if (timnow > real(itbcc(2, ito, istsc),fp)) then
             zstep(1, ito, istsc) = zstep(2, ito, istsc)
             itbcc(1, ito, istsc) = itbcc(2, ito, istsc)
             !
             ! Read new time step and concentrations data
             !
             call updbcc(lunbcc    ,lundia    ,first     ,itbcc     ,ito       , &
                       & istsc     ,timnow    ,itfinish  ,timscl    , &
                       & nto       ,kmax      ,lstsc     ,procbc    ,tprofc    , &
                       & zstep     ,gdp       )
             newstp = .true.
          endif
          !
          ! interpolation in time
          !
          do k = 1, kmax
             procbc(1, ito, k, istsc) = procbc(1, ito, k, istsc)                &
                                      & + procbc(3, ito, k, istsc)
             procbc(2, ito, k, istsc) = procbc(2, ito, k, istsc)                &
                                      & + procbc(4, ito, k, istsc)
          enddo
       enddo
       !
       ! Overrule procbc(1:2,:) with OpenDA input, if present
       ! this is done for salinity and temperature, if present
       ! note: store this value and reset procbc at the end of this routine.
       ! This is to prevent noise-on-noise since procbc(1:2) is never read from file again
       !
       if (lsal .ne. 0) then
          procbc_old(1:2,ito,1:kmax,lsal) = procbc(1:2,ito,1:kmax,lsal)
          call get_openda_buffer('bound_salt', ito, 2, kmax, procbc(1:2,ito,1:kmax,lsal))
       endif
       if (ltem .ne. 0) then
          procbc_old(1:2,ito,1:kmax,ltem) = procbc(1:2,ito,1:kmax,ltem)
          call get_openda_buffer('bound_temp', ito, 2, kmax, procbc(1:2,ito,1:kmax,ltem))    
       endif
    enddo
    !
    ! for all "conservative" constituents
    !
    do l = 1, lstsc
       !
       ! calculate space fraction of opening for function values in hob
       !
       do n = 1, nrob
          n1     = nob(8, n)
          msta   = mnbnd(1, n1)
          nsta   = mnbnd(2, n1)
          mend   = mnbnd(3, n1)
          nend   = mnbnd(4, n1)
          posrel = mnbnd(7, n1)
          incx   = mend - msta
          incy   = nend - nsta
          maxinc = max(1, abs(incx), abs(incy))
          incx   = incx/maxinc
          incy   = incy/maxinc
          !
          ! use coord. at zeta points to calculate the interpolated
          ! values from the two utmost points of the opening
          !
          if (parll) then
             !
             ! If the start point/pivot is outside this partition,
             ! add the distance from that point/pivot to the first point inside this partition
             ! to totl and distl
             !
             totl = dist_pivot_part(start_pivot,n1)
          else
             totl = 0.0_fp
          endif
          dist = totl
          frac = 0.0_fp
          do j = 1, maxinc
             msta  = msta + incx
             nsta  = nsta + incy
             !
             ! In case of a diagonal water level boundary (e.g. south-east):
             ! Pythagoras is used to calculate the distance from xz,yz(m,n) to xz,yz(m+1,n+1):
             !       d_y((m,n),(m+1,n+1)) = 0.5*(guuz(m,n) + guuz(m+1,n+1))
             !       d_x((m,n),(m+1,n+1)) = 0.5*(gvvz(m,n) + gvvz(m+1,n+1))
             !       dist = sqrt(d_x*d_x + d_y*d_y)
             !       Where guuz/gvvz is guu/gvv, extrapolated to the boundary zeta-point (outside the domain),
             !       using the first two guu/gvv inside the domain:
             !       guuz(m,n) = ( 3*guu(m+offm1,n+offn1) - guu(m+offm2,n+offn2) ) / 2
             !       Where the indices offset values offm/n1/2 can have the values 0, 1, 2, 3, depending on:
             !       - the orientation of the open boundary related to the domain
             !         north      boundary: nob(4)=0, nob(6)=2, incy=0
             !         north-east boundary: nob(4)=2, nob(6)=2, incx=-incy
             !         east       boundary: nob(4)=2, nob(6)=0, incx=0
             !         south-east boundary: nob(4)=2, nob(6)=1, incx= incy
             !         south      boundary: nob(4)=0, nob(6)=1, incy=0
             !         south-west boundary: nob(4)=1, nob(6)=1, incx=-incy
             !         west       boundary: nob(4)=1, nob(6)=0, incx=0
             !         north-west boundary: nob(4)=1, nob(6)=2, incx= incy
             !       - The value of incx/incy on diagonal boundaries (+1 or -1)
             ! Assumption: - the grid is more or less cartesian locally
             ! Note:       - incx and incy are -1, 0 or 1
             !             - gvvz is based on 2 gvv values with constant m-index and n-indices difference 1
             !             - guuz is based on 2 guu values with constant n-index and m-indices difference 1
             !             - vertical/horizontal boundaries (north, east, south, west):
             !               - flagged by incx=0 or incy=0
             !               - d_y=0 or d_x=0, so instead of Pythagoras: dist = dist + d_x + d_y
             !                 (guu/gvv are distances and always >0)
             !               - extrapolation of guu/gvv to guuz/gvvz is still needed for the non-zero d_x/d_y
             !
             !
             ! Compute distance in xi-direction
             !
             if (incx == 0) then
                !
                ! east or west boundary
                !
                distx = 0.0_fp
             else
                ngg = nsta
                select case(nob(4,n))
                case (0)
                   if (nob(6,n) == 1) then
                      ! south boundary, gvv(ngg,..) and gvv(ngg+1,..) are inside domain
                      gvvz1 = (3.0_fp*gvv(ngg,msta)      - gvv(ngg+1,msta)     ) / 2.0_fp
                      gvvz2 = (3.0_fp*gvv(ngg,msta-incx) - gvv(ngg+1,msta-incx)) / 2.0_fp
                   elseif (nob(6,n) == 2) then
                      ! north boundary, gvv(ngg-1,..) and gvv(ngg-2,..) are inside domain
                      gvvz1 = (3.0_fp*gvv(ngg-1,msta)      - gvv(ngg-2,msta)     ) / 2.0_fp
                      gvvz2 = (3.0_fp*gvv(ngg-1,msta-incx) - gvv(ngg-2,msta-incx)) / 2.0_fp
                   else
                      ! nob(6) is always 1 or 2 for open boundaries that are not east or west boundaries
                   endif
                case (1)
                   if (nob(6,n) == 1) then
                      ! south-west boundary
                      if (incy > 0) then
                         ! incx<0, msta     : gvv(ngg,..)   and gvv(ngg+1,..) are inside domain
                         !         msta-incx: gvv(ngg-1,..) and gvv(ngg,..)   are inside domain
                         gvvz1 = (3.0_fp*gvv(ngg  ,msta)      - gvv(ngg+1,msta)     ) / 2.0_fp
                         gvvz2 = (3.0_fp*gvv(ngg-1,msta-incx) - gvv(ngg  ,msta-incx)) / 2.0_fp
                      else
                         ! incy<0, incx>0, msta     : gvv(ngg,..)   and gvv(ngg+1,..) are inside domain
                         !                 msta-incx: gvv(ngg+1,..) and gvv(ngg+2,..) are inside domain
                         gvvz1 = (3.0_fp*gvv(ngg  ,msta)      - gvv(ngg+1,msta)     ) / 2.0_fp
                         gvvz2 = (3.0_fp*gvv(ngg+1,msta-incx) - gvv(ngg+2,msta-incx)) / 2.0_fp
                      endif
                   elseif (nob(6,n) == 2) then
                      ! north-west boundary
                      if (incy > 0) then
                         ! incx>0, msta     : gvv(ngg-1,..) and gvv(ngg-2,..) are inside domain
                         !         msta-incx: gvv(ngg-2,..) and gvv(ngg-3,..) are inside domain
                         gvvz1 = (3.0_fp*gvv(ngg-1,msta)      - gvv(ngg-2,msta)     ) / 2.0_fp
                         gvvz2 = (3.0_fp*gvv(ngg-2,msta-incx) - gvv(ngg-3,msta-incx)) / 2.0_fp
                      else
                         ! incy<0, incx<0, msta     : gvv(ngg-1,..) and gvv(ngg-2,..) are inside domain
                         !                 msta-incx: gvv(ngg,..)   and gvv(ngg-1,..) are inside domain
                         gvvz1 = (3.0_fp*gvv(ngg-1,msta)      - gvv(ngg-2,msta)     ) / 2.0_fp
                         gvvz2 = (3.0_fp*gvv(ngg  ,msta-incx) - gvv(ngg-1,msta-incx)) / 2.0_fp
                      endif
                   else
                      ! nob(6) is always 1 or 2 for open boundaries that are not east or west boundaries
                   endif
                case (2)
                   if (nob(6,n) == 1) then
                      ! south-east boundary
                      if (incy > 0) then
                         ! incx>0, msta     : gvv(ngg,..)   and gvv(ngg+1,..) are inside domain
                         !         msta-incx: gvv(ngg-1,..) and gvv(ngg,..)   are inside domain
                         gvvz1 = (3.0_fp*gvv(ngg  ,msta)      - gvv(ngg+1,msta)     ) / 2.0_fp
                         gvvz2 = (3.0_fp*gvv(ngg-1,msta-incx) - gvv(ngg  ,msta-incx)) / 2.0_fp
                      else
                         ! incy<0, incx<0, msta     : gvv(ngg,..)   and gvv(ngg+1,..) are inside domain
                         !                 msta-incx: gvv(ngg+1,..) and gvv(ngg+2,..) are inside domain
                         gvvz1 = (3.0_fp*gvv(ngg  ,msta)      - gvv(ngg+1,msta)     ) / 2.0_fp
                         gvvz2 = (3.0_fp*gvv(ngg+1,msta-incx) - gvv(ngg+2,msta-incx)) / 2.0_fp
                      endif
                   elseif (nob(6,n) == 2) then
                      ! north-east boundary
                      if (incy > 0) then
                         ! incx<0, msta     : gvv(ngg-1,..) and gvv(ngg-2,..) are inside domain
                         !         msta-incx: gvv(ngg-2,..) and gvv(ngg-3,..) are inside domain
                         gvvz1 = (3.0_fp*gvv(ngg-1,msta)      - gvv(ngg-2,msta)     ) / 2.0_fp
                         gvvz2 = (3.0_fp*gvv(ngg-2,msta-incx) - gvv(ngg-3,msta-incx)) / 2.0_fp
                      else
                         ! incy<0, incx>0, msta     : gvv(ngg-1,..) and gvv(ngg-2,..) are inside domain
                         !                 msta-incx: gvv(ngg,..)   and gvv(ngg-1,..) are inside domain
                         gvvz1 = (3.0_fp*gvv(ngg-1,msta)      - gvv(ngg-2,msta)     ) / 2.0_fp
                         gvvz2 = (3.0_fp*gvv(ngg  ,msta-incx) - gvv(ngg-1,msta-incx)) / 2.0_fp
                      endif
                   else
                      ! nob(6) is always 1 or 2 for open boundaries that are not east or west boundaries
                   endif
                case default
                   ! nob(4) is always 0, 1 or 2
                endselect
                distx = 0.5_fp * (gvvz1 + gvvz2)
             endif
             !
             ! Compute distance in eta-direction
             !
             if (incy == 0) then
                !
                ! north or south boundary
                !
                disty = 0.0_fp
             else
                mgg = msta
                select case(nob(6,n))
                case (0)
                   if (nob(4,n) == 1) then
                      ! west boundary, guu(..,mgg) and guu(..,mgg+1) are inside domain
                      guuz1 = (3.0_fp*guu(nsta     ,mgg) - guu(nsta     ,mgg+1)) / 2.0_fp
                      guuz2 = (3.0_fp*guu(nsta-incy,mgg) - guu(nsta-incy,mgg+1)) / 2.0_fp
                   elseif (nob(4,n) == 2) then
                      ! east boundary, guu(..,mgg-1) and guu(..,mgg-2) are inside domain
                      guuz1 = (3.0_fp*guu(nsta     ,mgg-1) - guu(nsta     ,mgg-2)) / 2.0_fp
                      guuz2 = (3.0_fp*guu(nsta-incy,mgg-1) - guu(nsta-incy,mgg-2)) / 2.0_fp
                   else
                      ! nob(4) is always 1 or 2 for open boundaries that are not north or south boundaries
                   endif
                case (1)
                   if (nob(4,n) == 1) then
                      ! south-west boundary
                      if (incx > 0) then
                         ! incy<0, nsta     : guu(..,mgg)   and guu(..,mgg+1) are inside domain
                         !         nsta-incy: guu(..,mgg-1) and guu(..,mgg)   are inside domain
                         guuz1 = (3.0_fp*guu(nsta     ,mgg)   - guu(nsta     ,mgg+1)) / 2.0_fp
                         guuz2 = (3.0_fp*guu(nsta-incy,mgg-1) - guu(nsta-incy,mgg)  ) / 2.0_fp
                      else
                         ! incx<0, incy>0, nsta     : guu(..,mgg)   and guu(..,mgg+1) are inside domain
                         !                 nsta-incy: guu(..,mgg+1) and guu(..,mgg+2) are inside domain
                         guuz1 = (3.0_fp*guu(nsta     ,mgg)   - guu(nsta     ,mgg+1)) / 2.0_fp
                         guuz2 = (3.0_fp*guu(nsta-incy,mgg+1) - guu(nsta-incy,mgg+2)) / 2.0_fp
                      endif
                   elseif (nob(4,n) == 2) then
                      ! south-east boundary
                      if (incx > 0) then
                         ! incy>0, nsta     : guu(..,mgg-1) and guu(..,mgg-2) are inside domain
                         !         nsta-incy: guu(..,mgg-2) and guu(..,mgg-3) are inside domain
                         guuz1 = (3.0_fp*guu(nsta     ,mgg-1) - guu(nsta     ,mgg-2)) / 2.0_fp
                         guuz2 = (3.0_fp*guu(nsta-incy,mgg-2) - guu(nsta-incy,mgg-3)) / 2.0_fp
                      else
                         ! incx<0, incy<0, nsta     : guu(..,mgg-1) and guu(..,mgg-2) are inside domain
                         !                 nsta-incy: guu(..,mgg)   and guu(..,mgg-1) are inside domain
                         guuz1 = (3.0_fp*guu(nsta     ,mgg-1) - guu(nsta     ,mgg-2)) / 2.0_fp
                         guuz2 = (3.0_fp*guu(nsta-incy,mgg)   - guu(nsta-incy,mgg-1)) / 2.0_fp
                      endif
                   else
                      ! nob(4) is always 1 or 2 for open boundaries that are not north or south boundaries
                   endif
                case (2)
                   if (nob(4,n) == 1) then
                      ! north-west boundary
                      if (incx > 0) then
                         ! incy>0, nsta     : guu(..,mgg)   and guu(..,mgg+1) are inside domain
                         !         nsta-incy: guu(..,mgg-1) and guu(..,mgg)   are inside domain
                         guuz1 = (3.0_fp*guu(nsta     ,mgg)   - guu(nsta     ,mgg+1)) / 2.0_fp
                         guuz2 = (3.0_fp*guu(nsta-incy,mgg-1) - guu(nsta-incy,mgg)  ) / 2.0_fp
                      else
                         ! incx<0, incy<0, nsta     : guu(..,mgg)   and guu(..,mgg+1) are inside domain
                         !                 nsta-incy: guu(..,mgg+1) and guu(..,mgg+2) are inside domain
                         guuz1 = (3.0_fp*guu(nsta     ,mgg)   - guu(nsta     ,mgg+1)) / 2.0_fp
                         guuz2 = (3.0_fp*guu(nsta-incy,mgg+1) - guu(nsta-incy,mgg+2)) / 2.0_fp
                      endif
                   elseif (nob(4,n) == 2) then
                      ! north-east boundary
                      if (incx > 0) then
                         ! incy<0, nsta     : guu(..,mgg-1) and guu(..,mgg-2) are inside domain
                         !         nsta-incy: guu(..,mgg-2) and guu(..,mgg-3) are inside domain
                         guuz1 = (3.0_fp*guu(nsta     ,mgg-1) - guu(nsta     ,mgg-2)) / 2.0_fp
                         guuz2 = (3.0_fp*guu(nsta-incy,mgg-2) - guu(nsta-incy,mgg-3)) / 2.0_fp
                      else
                         ! incx<0, incy>0, nsta     : guu(..,mgg-1) and guu(..,mgg-2) are inside domain
                         !                 nsta-incy: guu(..,mgg)   and guu(..,mgg-1) are inside domain
                         guuz1 = (3.0_fp*guu(nsta     ,mgg-1) - guu(nsta     ,mgg-2)) / 2.0_fp
                         guuz2 = (3.0_fp*guu(nsta-incy,mgg)   - guu(nsta-incy,mgg-1)) / 2.0_fp
                      endif
                   else
                      ! nob(4) is always 1 or 2 for open boundaries that are not north or south boundaries
                   endif
                case default
                   ! nob(6) is always 0, 1 or 2
                endselect
                disty = 0.5_fp * (guuz1 + guuz2)
             endif
             if (incx/=0 .and. incy/=0) then
                distx = distx * distx
                disty = disty * disty
                totl  = totl + sqrt(distx + disty)
             else
                ! distx==0 or disty==0
                totl  = totl + distx + disty
             endif
             if (msta==nob(1, n) .and. nsta==nob(2, n)) then
                dist = totl
             endif
          enddo
          if (parll) then
             !
             ! If the end point/pivot is outside this partition,
             ! add the distance from that point/pivot to the last point inside this partition
             ! to totl
             !
             totl = totl + dist_pivot_part(end_pivot,n1)
          endif
          if (totl > 0.) then
             frac = dist/totl
          endif
          !
          ! Interpolate in space constituents along surface and bottom
          !
          do k = 1, kmax
             ! rob   (k,n) = VALPNT(n1,k,l,frac)
             rob(k, n) = procbc(1, n1, k, l)                                    &
                       & + frac*(procbc(2, n1, k, l) - procbc(1, n1, k, l))
          enddo
          !
          ! calculate rbnd array
          ! where nob (4,n) := type of opening and
          !       nob (5,n) := row (conform irocol table)
          !       nob (6,n) := type of opening and
          !       nob (7,n) := column (conform irocol table)
          !
          kpr = nob(4, n)
          kqr = nob(5, n)
          kpc = nob(6, n)
          kqc = nob(7, n)
          kp  = kpr
          kq  = kqr
          kpp = kpc
          kqq = kqc
          !
          ! If row boundary not available then use col boundary
          !
          if (kp*kq == 0) then
             kp  = kpc
             kq  = kqc
             kpp = kpr
             kqq = kqr
          endif
          !
          ! Interpolation in vertical direction
          !
          if (tprofc(n1, l)(1:7) == 'uniform') then
             !
             ! uniform
             ! For all KMAX layers ROB is equal (stored in ROB(1,N))
             !
             do k = 1, kmax
                rbnd(k, l, kp, kq) = rob(1, n)
             enddo
          elseif (tprofc(n1, l)(1:6) == 'linear') then
             !
             ! Linear
             ! Values of bottom and surface will be used to interpolated
             ! for all KMAX layers
             !
             if (zmodel) then
                zcord = 0.
                do k = kfsmin(nsta, msta), kmax
                   if (k==kfsmin(nsta, msta)) then
                      zcord = zcord + .5*dzs1(nsta, msta, k)
                   else
                      zcord = zcord +                                           &
                            & .5*(dzs1(nsta, msta, k - 1) + dzs1(nsta, msta, k))
                   endif
                   reldep = zcord/(real(dps(nsta, msta),fp) + s0(nsta, msta))
                   rbnd(k, l, kp, kq) = rob(kmax, n)                            &
                                      & + reldep*(rob(1, n) - rob(kmax, n))
                enddo
             else
                do k = 1, kmax
                   rbnd(k, l, kp, kq) = rob(kmax, n) + (1. + sig(k))            &
                                      & *(rob(1, n) - rob(kmax, n))
                enddo
             endif
          elseif (tprofc(n1, l)(1:4) == 'step') then
             !
             ! step function
             ! The two values of the Step function are written in
             ! ROB(1,N) and ROB(KMAX,N)
             !
             if (newstp) then
                mp     = nob(1, n)
                np     = nob(2, n)
                h0     = real(dps(np, mp),fp) + s0(np, mp)
                zjmp   = s0(np, mp) - zstep(1, n1, l)
                sigjmp = (zjmp-s0(np, mp))/max(h0, 0.01_fp)
                do k = 1, kmax
                   if (zmodel) then
                      !
                      ! In the z-model we start from the bottom layer upwards, 
                      ! with z running from the bed level to the free surface level
                      !
                      if (zk(k) > zjmp) then
                         kstp(n, l) = k
                         goto 610
                      endif
                      !
                      ! In the sigma-model we start from the top layer downwards, 
                      ! with sig running from 0 to -1
                      !
                   elseif (sigjmp > sig(k)) then
                      kstp(n, l) = max(k - 1, 1)
                      goto 610
                   else
                   endif
                enddo
                kstp(n, l) = kmax
             endif
610          continue
             !
             ! Since rob(1,n) contains the bc-value near the free-surface and 
             ! rob(kmax,n) the value near the bed, we need to switch the order
             ! for sigma- and z-layer models
             !
             if (zmodel) then
                 r2 = rob(1, n)
                 r1 = rob(kmax, n)
             else
                 r1 = rob(1, n)
                 r2 = rob(kmax, n)
             endif
             do k = 1, kmax
                if (k <= kstp(n, l)) then
                   rbnd(k, l, kp, kq) = r1
                else
                   rbnd(k, l, kp, kq) = r2
                endif
             enddo
          elseif (tprofc(n1, l) == '3d-profile') then
             !
             ! 3D-profile function
             ! The user defined input is stored in ROB for all KMAX
             ! layers
             !
             do k = 1, kmax
                rbnd(k, l, kp, kq) = rob(k, n)
             enddo
          else
          endif
          !
          ! If oblique boundary is defined then fill the
          ! computed RBND also in other direction
          !
          do k = 1, kmax
             if (kpp*kqq /= 0) then
                rbnd(k, l, kpp, kqq) = rbnd(k, l, kp, kq)
             endif
          enddo
       enddo
    enddo
    newstp = .false.
    !
    ! Now reset procbc(1:2)
    !
    if (lsal .ne. 0) then
       procbc(1:2,1:nto,1:kmax,lsal) = procbc_old(1:2,1:nto,1:kmax,lsal)
    endif
    if (ltem .ne. 0) then
       procbc(1:2,1:nto,1:kmax,ltem) = procbc_old(1:2,1:nto,1:kmax,ltem)
    endif    
    if (lsal .ne. 0 .or. ltem .ne. 0) then
       deallocate(procbc_old)
    endif
end subroutine incbcc
