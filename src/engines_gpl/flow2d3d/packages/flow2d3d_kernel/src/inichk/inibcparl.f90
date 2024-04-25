subroutine inibcparl(nto       ,nrob      ,mnbnd     ,nob       ,typbnd    , &
                   & guu       ,gvv       ,gdp       )
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
!    Function: When running parallel,
!              the start/end pivot points of an open boundary may lay outside
!              the current partition.
!              Store the distance from the start pivot to the first point inside
!              this partition in dist_pivot_part(start_pivot,n), zero when inside
!              Store the distance from the end   pivot to the last point inside
!              this partition in dist_pivot_part(  end_pivot,n), zero when inside
!     Method used: 
!     - Collect needed gu/vu/v of the full global model in the master node
!     - Broadcast it to all partitions
!     - Each partition calculates the needed distances
!     - Deallocate the temporary full global model arrays
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use dfparall
    use dffunctionals
    !
    use globaldata
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
    integer                 , pointer :: kmax           !  Description and declaration in esm_alloc_int.f90
    integer                 , pointer :: mmax           !  Description and declaration in esm_alloc_int.f90
    integer                 , pointer :: nmaxus         !  Description and declaration in esm_alloc_int.f90
    integer                 , pointer :: mfg
    integer                 , pointer :: mlg
    integer                 , pointer :: nfg
    integer                 , pointer :: nlg
    integer                 , pointer :: mmaxgl
    integer                 , pointer :: nmaxgl
    integer , dimension(:,:), pointer :: iarrc
    integer , dimension(:)  , pointer :: mf
    integer , dimension(:)  , pointer :: ml
    integer , dimension(:)  , pointer :: nf
    integer , dimension(:)  , pointer :: nl
    integer , dimension(:)  , pointer :: bct_order       !  Description and declaration in bcdat.igs
    integer , dimension(:,:), pointer :: mnbnd_global    !  Description and declaration in bcdat.igs
    integer                 , pointer :: lundia
    integer                 , pointer :: ntof            !  Description and declaration in dimens.igs
    integer                 , pointer :: ntoq            !  Description and declaration in dimens.igs
    real(fp), dimension(:,:), pointer :: dist_pivot_part !  Description and declaration in bcdat.igs
!
! Global variables
!
    integer                                                           , intent(in)  :: nto    !!  Max. number of open boundaries
    integer                                                           , intent(in)  :: nrob   !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(7, nto)                                  , intent(in)  :: mnbnd  !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(8, nrob)                                 , intent(in)  :: nob    !  Description and declaration in esm_alloc_int.f90
    character(1) , dimension(nto)                                     , intent(in)  :: typbnd !  Description and declaration in esm_alloc_char.f90
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: gvv    !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer                                :: i
    integer                                :: incx
    integer                                :: incy
    integer                                :: istat
    integer                                :: j
    integer                                :: maxinc
    integer                                :: msta
    integer                                :: mend
    integer                                :: nsta
    integer                                :: nend
    integer                                :: n
    integer                                :: mgg          ! M-coord. of the actual open boundary point, which may differ from the ori- ginal position due to grid staggering
    integer                                :: ngg          ! N-coord. of the actual open boundary point, which may differ from the ori- ginal position due to grid staggering
    integer                                :: np
    integer                                :: lb           ! lowerboundary of loopcounter
    integer                                :: ub           ! upperboundary of loopcounter
    integer                                :: pivot        ! loops over start_pivot and end_pivot
    logical                                :: horiz
    real(fp)                               :: distx
    real(fp)                               :: disty
    real(fp)                               :: guuz1
    real(fp)                               :: guuz2
    real(fp)                               :: gvvz1
    real(fp)                               :: gvvz2
    real(fp), dimension(:,:), allocatable  :: guu_global   ! temporary array storing guu of the full global domain
    real(fp), dimension(:,:), allocatable  :: gvv_global   ! temporary array storing gvv of the full global domain
!
!! executable statements -------------------------------------------------------
!
    if (.not. parll) return
    !
    mmax         => gdp%d%mmax
    nmaxus       => gdp%d%nmaxus
    kmax         => gdp%d%kmax
    ntof         => gdp%d%ntof
    ntoq         => gdp%d%ntoq
    mfg          => gdp%gdparall%mfg
    mlg          => gdp%gdparall%mlg
    nfg          => gdp%gdparall%nfg
    nlg          => gdp%gdparall%nlg
    nmaxgl       => gdp%gdparall%nmaxgl
    mmaxgl       => gdp%gdparall%mmaxgl
    iarrc        => gdp%gdparall%iarrc
    mf           => gdp%gdparall%mf
    ml           => gdp%gdparall%ml
    nf           => gdp%gdparall%nf
    nl           => gdp%gdparall%nl
    bct_order    => gdp%gdbcdat%bct_order
    mnbnd_global => gdp%gdbcdat%mnbnd_global
    lundia       => gdp%gdinout%lundia
    !
    allocate(gdp%gdbcdat%dist_pivot_part(2, nto), stat=istat)
    dist_pivot_part => gdp%gdbcdat%dist_pivot_part
    if (istat /= 0) then
       call prterr(lundia, 'P004', 'memory alloc error in inibcparl(dist_pivor_part)')
       call d3stop(1, gdp)
    endif
    dist_pivot_part = 0.0_fp
    !
                  allocate(guu_global(1:nmaxgl,1:mmaxgl), stat=istat)
    if (istat==0) allocate(gvv_global(1:nmaxgl,1:mmaxgl), stat=istat)
    if (istat /= 0) then
       call prterr(lundia, 'P004', 'memory alloc error in inibcparl(guu_global/gvv_global)')
       call d3stop(1, gdp)
    endif
    !
    ! Collect all guu values from all partitions in the array guu_global in the master partition
    ! NOTE: the dfgather call will reallocate guu_global on the master
    !
    call dfgather(guu, guu_global, nf, nl, mf, ml, iarrc, gdp)
    !
    ! The master partition broadcasts this guu array to all partitions
    !
    call dfbroadc_gdp (guu_global, (nmaxgl)*(mmaxgl), dfloat, gdp )
    !
    ! Collect all gvv values from all partitions in the array gvv_global in the master partition
    ! NOTE: the dfgather call will reallocate gvv_global on the master
    !
    call dfgather(gvv, gvv_global, nf, nl, mf, ml, iarrc, gdp)
    !
    ! The master partition broadcasts this gvv array to all partitions
    !
    call dfbroadc_gdp (gvv_global, (nmaxgl)*(mmaxgl), dfloat, gdp )
    !
    ! loop over all boundaries that are (partly) inside this partition
    !
    do n=1,nto
       !
       ! Find an index np for which nob(:,np) refers to a point belonging to this open boundary: nob(8,np)=n
       ! np is needed to use nob(4,np) and nob(6,np) to detect the location of this open boundary related to the domain:
       !   north      boundary: nob(4)=0, nob(6)=2, incy=0
       !   north-east boundary: nob(4)=2, nob(6)=2, incx=-incy
       !   east       boundary: nob(4)=2, nob(6)=0, incx=0
       !   south-east boundary: nob(4)=2, nob(6)=1, incx= incy
       !   south      boundary: nob(4)=0, nob(6)=1, incy=0
       !   south-west boundary: nob(4)=1, nob(6)=1, incx=-incy
       !   west       boundary: nob(4)=1, nob(6)=0, incx=0
       !   north-west boundary: nob(4)=1, nob(6)=2, incx= incy
       do np=1,nrob
          if (nob(8,np) == n) then
             exit
          endif
       enddo
       !
       ! loop over start_pivot (1) and end_pivot (2)
       do pivot=start_pivot,end_pivot
          if (pivot == start_pivot) then
             !
             ! (msta,nsta): boundary pivot point outside partition
             ! (mend,nend): first point along boundary inside partition
             !
             msta   = mnbnd_global(1,bct_order(n))
             nsta   = mnbnd_global(2,bct_order(n))
             mend   = mnbnd(1,n) + mfg - 1
             nend   = mnbnd(2,n) + nfg - 1
          else ! end_pivot
             !
             ! (msta,nsta): first point along boundary inside partition
             ! (mend,nend): boundary pivot point outside partition
             !
             msta   = mnbnd(3,n) + mfg - 1
             nsta   = mnbnd(4,n) + nfg - 1
             mend   = mnbnd_global(3,bct_order(n))
             nend   = mnbnd_global(4,bct_order(n))
          endif
          incx   = mend - msta
          incy   = nend - nsta
          maxinc = max(abs(incx), abs(incy))
          incx   = incx / max(1,maxinc)
          incy   = incy / max(1,maxinc)
          select case (typbnd(n))
             case ('Z')
                ! Waterlevel boundary, ityp=2
                do while ((msta/=mend) .or. (nsta/=nend))
                   !
                   ! This loop is NOT entered when the boundary is completely inside this partition
                   ! This loop is entered for horizontal (nsta=nend), vertical (msta=mend) and diagonal (msta!=mend,nsta!=nend) boundaries
                   !
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
                      select case(nob(4,np))
                      case (0)
                         if (nob(6,np) == 1) then
                            ! south boundary, gvv(ngg,..) and gvv(ngg+1,..) are inside domain
                            gvvz1 = (3.0_fp*gvv_global(ngg,msta)      - gvv_global(ngg+1,msta)     ) / 2.0_fp
                            gvvz2 = (3.0_fp*gvv_global(ngg,msta-incx) - gvv_global(ngg+1,msta-incx)) / 2.0_fp
                         elseif (nob(6,np) == 2) then
                            ! north boundary, gvv(ngg-1,..) and gvv(ngg-2,..) are inside domain
                            gvvz1 = (3.0_fp*gvv_global(ngg-1,msta)      - gvv_global(ngg-2,msta)     ) / 2.0_fp
                            gvvz2 = (3.0_fp*gvv_global(ngg-1,msta-incx) - gvv_global(ngg-2,msta-incx)) / 2.0_fp
                         else
                            ! nob(6) is always 1 or 2 for open boundaries that are not east or west boundaries
                         endif
                      case (1)
                         if (nob(6,np) == 1) then
                            ! south-west boundary
                            if (incy > 0) then
                               ! incx<0, msta     : gvv(ngg,..)   and gvv(ngg+1,..) are inside domain
                               !         msta-incx: gvv(ngg-1,..) and gvv(ngg,..)   are inside domain
                               gvvz1 = (3.0_fp*gvv_global(ngg  ,msta)      - gvv_global(ngg+1,msta)     ) / 2.0_fp
                               gvvz2 = (3.0_fp*gvv_global(ngg-1,msta-incx) - gvv_global(ngg  ,msta-incx)) / 2.0_fp
                            else
                               ! incy<0, incx>0, msta     : gvv(ngg,..)   and gvv(ngg+1,..) are inside domain
                               !                 msta-incx: gvv(ngg+1,..) and gvv(ngg+2,..) are inside domain
                               gvvz1 = (3.0_fp*gvv_global(ngg  ,msta)      - gvv_global(ngg+1,msta)     ) / 2.0_fp
                               gvvz2 = (3.0_fp*gvv_global(ngg+1,msta-incx) - gvv_global(ngg+2,msta-incx)) / 2.0_fp
                            endif
                         elseif (nob(6,np) == 2) then
                            ! north-west boundary
                            if (incy > 0) then
                               ! incx>0, msta     : gvv(ngg-1,..) and gvv(ngg-2,..) are inside domain
                               !         msta-incx: gvv(ngg-2,..) and gvv(ngg-3,..) are inside domain
                               gvvz1 = (3.0_fp*gvv_global(ngg-1,msta)      - gvv_global(ngg-2,msta)     ) / 2.0_fp
                               gvvz2 = (3.0_fp*gvv_global(ngg-2,msta-incx) - gvv_global(ngg-3,msta-incx)) / 2.0_fp
                            else
                               ! incy<0, incx<0, msta     : gvv(ngg-1,..) and gvv(ngg-2,..) are inside domain
                               !                 msta-incx: gvv(ngg,..)   and gvv(ngg-1,..) are inside domain
                               gvvz1 = (3.0_fp*gvv_global(ngg-1,msta)      - gvv_global(ngg-2,msta)     ) / 2.0_fp
                               gvvz2 = (3.0_fp*gvv_global(ngg  ,msta-incx) - gvv_global(ngg-1,msta-incx)) / 2.0_fp
                            endif
                         else
                            ! nob(6) is always 1 or 2 for open boundaries that are not east or west boundaries
                         endif
                      case (2)
                         if (nob(6,np) == 1) then
                            ! south-east boundary
                            if (incy > 0) then
                               ! incx>0, msta     : gvv(ngg,..)   and gvv(ngg+1,..) are inside domain
                               !         msta-incx: gvv(ngg-1,..) and gvv(ngg,..)   are inside domain
                               gvvz1 = (3.0_fp*gvv_global(ngg  ,msta)      - gvv_global(ngg+1,msta)     ) / 2.0_fp
                               gvvz2 = (3.0_fp*gvv_global(ngg-1,msta-incx) - gvv_global(ngg  ,msta-incx)) / 2.0_fp
                            else
                               ! incy<0, incx<0, msta     : gvv(ngg,..)   and gvv(ngg+1,..) are inside domain
                               !                 msta-incx: gvv(ngg+1,..) and gvv(ngg+2,..) are inside domain
                               gvvz1 = (3.0_fp*gvv_global(ngg  ,msta)      - gvv_global(ngg+1,msta)     ) / 2.0_fp
                               gvvz2 = (3.0_fp*gvv_global(ngg+1,msta-incx) - gvv_global(ngg+2,msta-incx)) / 2.0_fp
                            endif
                         elseif (nob(6,np) == 2) then
                            ! north-east boundary
                            if (incy > 0) then
                               ! incx<0, msta     : gvv(ngg-1,..) and gvv(ngg-2,..) are inside domain
                               !         msta-incx: gvv(ngg-2,..) and gvv(ngg-3,..) are inside domain
                               gvvz1 = (3.0_fp*gvv_global(ngg-1,msta)      - gvv_global(ngg-2,msta)     ) / 2.0_fp
                               gvvz2 = (3.0_fp*gvv_global(ngg-2,msta-incx) - gvv_global(ngg-3,msta-incx)) / 2.0_fp
                            else
                               ! incy<0, incx>0, msta     : gvv(ngg-1,..) and gvv(ngg-2,..) are inside domain
                               !                 msta-incx: gvv(ngg,..)   and gvv(ngg-1,..) are inside domain
                               gvvz1 = (3.0_fp*gvv_global(ngg-1,msta)      - gvv_global(ngg-2,msta)     ) / 2.0_fp
                               gvvz2 = (3.0_fp*gvv_global(ngg  ,msta-incx) - gvv_global(ngg-1,msta-incx)) / 2.0_fp
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
                      select case(nob(6,np))
                      case (0)
                         if (nob(4,np) == 1) then
                            ! west boundary, guu(..,mgg) and guu(..,mgg+1) are inside domain
                            guuz1 = (3.0_fp*guu_global(nsta     ,mgg) - guu_global(nsta     ,mgg+1)) / 2.0_fp
                            guuz2 = (3.0_fp*guu_global(nsta-incy,mgg) - guu_global(nsta-incy,mgg+1)) / 2.0_fp
                         elseif (nob(4,np) == 2) then
                            ! east boundary, guu(..,mgg-1) and guu(..,mgg-2) are inside domain
                            guuz1 = (3.0_fp*guu_global(nsta     ,mgg-1) - guu_global(nsta     ,mgg-2)) / 2.0_fp
                            guuz2 = (3.0_fp*guu_global(nsta-incy,mgg-1) - guu_global(nsta-incy,mgg-2)) / 2.0_fp
                         else
                            ! nob(4) is always 1 or 2 for open boundaries that are not north or south boundaries
                         endif
                      case (1)
                         if (nob(4,np) == 1) then
                            ! south-west boundary
                            if (incx > 0) then
                               ! incy<0, nsta     : guu(..,mgg)   and guu(..,mgg+1) are inside domain
                               !         nsta-incy: guu(..,mgg-1) and guu(..,mgg)   are inside domain
                               guuz1 = (3.0_fp*guu_global(nsta     ,mgg)   - guu_global(nsta     ,mgg+1)) / 2.0_fp
                               guuz2 = (3.0_fp*guu_global(nsta-incy,mgg-1) - guu_global(nsta-incy,mgg)  ) / 2.0_fp
                            else
                               ! incx<0, incy>0, nsta     : guu(..,mgg)   and guu(..,mgg+1) are inside domain
                               !                 nsta-incy: guu(..,mgg+1) and guu(..,mgg+2) are inside domain
                               guuz1 = (3.0_fp*guu_global(nsta     ,mgg)   - guu_global(nsta     ,mgg+1)) / 2.0_fp
                               guuz2 = (3.0_fp*guu_global(nsta-incy,mgg+1) - guu_global(nsta-incy,mgg+2)) / 2.0_fp
                            endif
                         elseif (nob(4,np) == 2) then
                            ! south-east boundary
                            if (incx > 0) then
                               ! incy>0, nsta     : guu(..,mgg-1) and guu(..,mgg-2) are inside domain
                               !         nsta-incy: guu(..,mgg-2) and guu(..,mgg-3) are inside domain
                               guuz1 = (3.0_fp*guu_global(nsta     ,mgg-1) - guu_global(nsta     ,mgg-2)) / 2.0_fp
                               guuz2 = (3.0_fp*guu_global(nsta-incy,mgg-2) - guu_global(nsta-incy,mgg-3)) / 2.0_fp
                            else
                               ! incx<0, incy<0, nsta     : guu(..,mgg-1) and guu(..,mgg-2) are inside domain
                               !                 nsta-incy: guu(..,mgg)   and guu(..,mgg-1) are inside domain
                               guuz1 = (3.0_fp*guu_global(nsta     ,mgg-1) - guu_global(nsta     ,mgg-2)) / 2.0_fp
                               guuz2 = (3.0_fp*guu_global(nsta-incy,mgg)   - guu_global(nsta-incy,mgg-1)) / 2.0_fp
                            endif
                         else
                            ! nob(4) is always 1 or 2 for open boundaries that are not north or south boundaries
                         endif
                      case (2)
                         if (nob(4,np) == 1) then
                            ! north-west boundary
                            if (incx > 0) then
                               ! incy>0, nsta     : guu(..,mgg)   and guu(..,mgg+1) are inside domain
                               !         nsta-incy: guu(..,mgg-1) and guu(..,mgg)   are inside domain
                               guuz1 = (3.0_fp*guu_global(nsta     ,mgg)   - guu_global(nsta     ,mgg+1)) / 2.0_fp
                               guuz2 = (3.0_fp*guu_global(nsta-incy,mgg-1) - guu_global(nsta-incy,mgg)  ) / 2.0_fp
                            else
                               ! incx<0, incy<0, nsta     : guu(..,mgg)   and guu(..,mgg+1) are inside domain
                               !                 nsta-incy: guu(..,mgg+1) and guu(..,mgg+2) are inside domain
                               guuz1 = (3.0_fp*guu_global(nsta     ,mgg)   - guu_global(nsta     ,mgg+1)) / 2.0_fp
                               guuz2 = (3.0_fp*guu_global(nsta-incy,mgg+1) - guu_global(nsta-incy,mgg+2)) / 2.0_fp
                            endif
                         elseif (nob(4,np) == 2) then
                            ! north-east boundary
                            if (incx > 0) then
                               ! incy<0, nsta     : guu(..,mgg-1) and guu(..,mgg-2) are inside domain
                               !         nsta-incy: guu(..,mgg-2) and guu(..,mgg-3) are inside domain
                               guuz1 = (3.0_fp*guu_global(nsta     ,mgg-1) - guu_global(nsta     ,mgg-2)) / 2.0_fp
                               guuz2 = (3.0_fp*guu_global(nsta-incy,mgg-2) - guu_global(nsta-incy,mgg-3)) / 2.0_fp
                            else
                               ! incx<0, incy>0, nsta     : guu(..,mgg-1) and guu(..,mgg-2) are inside domain
                               !                 nsta-incy: guu(..,mgg)   and guu(..,mgg-1) are inside domain
                               guuz1 = (3.0_fp*guu_global(nsta     ,mgg-1) - guu_global(nsta     ,mgg-2)) / 2.0_fp
                               guuz2 = (3.0_fp*guu_global(nsta-incy,mgg)   - guu_global(nsta-incy,mgg-1)) / 2.0_fp
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
                      distx                    = distx * distx
                      disty                    = disty * disty
                      dist_pivot_part(pivot,n) = dist_pivot_part(pivot,n) + sqrt(distx + disty)
                   else
                      ! distx==0 or disty==0
                      dist_pivot_part(pivot,n) = dist_pivot_part(pivot,n) + distx + disty
                   endif
                enddo
             case ('T')
                !
                ! Distances are not used at Total discharge boundaries
                !
             case default ! 'C', 'Q', 'R'
                mgg   = msta
                ngg   = nsta
                if (msta == mend) then
                   !
                   ! Opening in the vertical direction
                   ! When on the right side of the domain, the correct guu is one index down (staggered grid)
                   !
                   horiz = .false.
                   if (mnbnd(7,n) == 3) mgg = mgg - 1
                else
                   !
                   ! Opening in the horizontal direction
                   ! When on the top side of the domain, the correct gvv is one index down (staggered grid)
                   !
                   horiz = .true.
                   if (mnbnd(7,n) == 4) ngg = ngg - 1
                endif
                !
                ! Distance between points calculated
                ! When MSTA/NSTA are updated first use lower GVV/GUU
                !
                do while ((msta/=mend) .or. (nsta/=nend))
                   !
                   ! This loop is NOT entered when the boundary is completely inside this partition
                   ! This loop is ONLY entered for horizontal (nsta=nend,incx=1) and vertical (msta=mend,incy=1) boundaries,
                   ! not for diagonal boundaries (compare with case ('Z'))
                   !
                   msta = msta + incx
                   nsta = nsta + incy
                   if (horiz) then
                      !
                      ! The distance between xz,yz(ngg,msta) and xz,yz(ngg,msta-1) is:
                      ! 0.5*guu(ngg,msta) + 0.5*guu(ngg,msta-1)
                      !
                      dist_pivot_part(pivot,n) = dist_pivot_part(pivot,n) &
                                               &  + 0.5_fp * (gvv_global(ngg,msta) + gvv_global(ngg,msta-incx))
                   else
                      !
                      ! The distance between xz,yz(nsta,mgg) and xz,yz(nsta-1,mgg) is:
                      ! 0.5*guu(nsta,mgg) + 0.5*guu(nsta-1,mgg)
                      !
                      dist_pivot_part(pivot,n) = dist_pivot_part(pivot,n) &
                                               & + 0.5_fp * (guu_global(nsta,mgg) + guu_global(nsta-incy,mgg))
                   endif
                enddo
          end select
       enddo ! pivot
    enddo ! boundary n
    !
    ! mnbnd_global was stored for usage in this subroutine and can be deallocated now
    !
    deallocate (gdp%gdbcdat%mnbnd_global, stat=istat)
    deallocate (guu_global, stat=istat)
    deallocate (gvv_global, stat=istat)
end subroutine inibcparl
