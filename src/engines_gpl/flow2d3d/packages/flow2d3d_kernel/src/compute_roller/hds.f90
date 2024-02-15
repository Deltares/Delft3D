subroutine hds(kfs       ,dps       ,s1        ,xcor      ,ycor      , &
             & nmax      ,mmax      ,theta     ,rlabda    ,grmasu    , &
             & grmasv    ,grfacu    ,grfacv    ,f_lam     ,lundia    , &
             & gdp      )
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
! Adapted from Reniers' Code for Breaker Delay
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
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
integer                                                        , intent(in)  :: mmax
integer                                                        , intent(in)  :: nmax
integer                                                                      :: lundia  !  Description and declaration in inout.igs
integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: kfs     !  Description and declaration in esm_alloc_int.f90 
real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: grmasu  !  Description and declaration in esm_alloc_real.f90
real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: grmasv  !  Description and declaration in esm_alloc_real.f90
real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: theta
real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: rlabda  !  Description and declaration in esm_alloc_real.f90 
real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: xcor    !  Description and declaration in esm_alloc_real.f90
real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: ycor    !  Description and declaration in esm_alloc_real.f90 
real(prec), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: dps     !  Description and declaration in esm_alloc_real.f90
real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: s1      !  Description and declaration in esm_alloc_real.f90 
real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: grfacu  !  Description and declaration in esm_alloc_real.f90
real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: grfacv  !  Description and declaration in esm_alloc_real.f90
real(fp)                                                       , intent(in)  :: f_lam
!
! Local variables
!
    integer                             :: j
    integer                             :: m
    integer                             :: pos
    integer                             :: mx
    integer                             :: n
    integer                             :: istat
    real(fp)                            :: fic
    real(fp)                            :: fis
    real(fp)                            :: grt
    real(fp)                            :: grut
    real(fp)                            :: gruvt
    real(fp)                            :: grvt
    real(fp)                            :: hdps
    real(fp)                            :: mrun
    real(fp)                            :: mrup
    real(fp)                            :: mrvn
    real(fp)                            :: mrvp
    real(fp)                            :: sdx
    real(fp), dimension(:), allocatable :: dx
    real(fp), dimension(:), allocatable :: dy
!
! executable statements -------------------------------------------------------
!

                 allocate (dx(mmax), stat = istat)
   if (istat==0) allocate (dy(nmax), stat = istat)
   if (istat /= 0) then
      call prterr(lundia, 'P004', 'memory alloc error in hds, roller module')
      call d3stop(1, gdp)
   endif
   dx(1) = 0.0
   dy(1) = 0.0
   do m = 1, mmax
      do n = 1, nmax
         if (kfs(n, m)==1) then
             mx = 1
             sdx = 0.0
             mrup = 0.0
             mrun = 0.0
             mrvp = 0.0
             mrvn = 0.0
             !
             ! delay in negative x-direction
             !
             do pos = m-1,1,-1
                dx(mx + 1) = sqrt((xcor(n, m) - xcor(n, pos))**2 + (ycor(n, m) - ycor(n, pos))**2)
                if (kfs(n, pos)==1 .and. dx(mx + 1)<f_lam*rlabda(n,m)) then
                   mx  = mx + 1
                   sdx = sdx + dx(mx)
                else
                   exit
                endif
             enddo
             if (sdx>0.0) then
                do j = 1, mx
                   hdps = max(real(dps(n, m - j + 1),fp)+s1(n, m - j + 1),0.01_fp)
                   grt  = sqrt((grmasu(n, m - j + 1)/hdps)**2 + &
                             & (grmasv(n, m - j + 1)/hdps)**2)
                   mrun = mrun + grt*(dx(mx) - dx(j))/sdx
                enddo
             else
                hdps = max(real(dps(n, m),fp)+s1(n, m),0.01_fp)
                mrun = sqrt((grmasu(n, m)/hdps)**2 + &
                          & (grmasv(n, m)/hdps)**2)
             endif
             !
             ! delay in positive x-direction
             !
             mx = 1
             sdx = 0.0
             do pos = m+1,mmax,1
                dx(mx + 1) = sqrt((xcor(n, m) - xcor(n, pos))**2 + (ycor(n, m) - ycor(n, pos))**2)
                if (kfs(n, pos)==1 .and. dx(mx + 1)<f_lam*rlabda(n,m)) then
                   mx  = mx + 1
                   sdx = sdx + dx(mx)
                else
                   exit
                endif
             enddo
             if (sdx>0.0) then
                do j = 1, mx
                   hdps = max(real(dps(n, m + j - 1),fp)+s1(n, m + j - 1),0.01_fp)
                   grt  = sqrt((grmasu(n, m + j - 1)/hdps)**2 + &
                            &  (grmasv(n, m + j - 1)/hdps)**2)
                   mrup = mrup + grt*(dx(mx) - dx(j))/sdx
                enddo
             else
                hdps = max(real(dps(n, m),fp)+s1(n, m),0.01_fp)
                mrup = sqrt((grmasu(n, m)/hdps)**2 + &
                          & (grmasv(n, m)/hdps)**2)
             endif
             !
             ! delay in positive y-direction
             !
             mx  = 1
             sdx = 0.0
             do pos = n+1,nmax,1
                dy(mx + 1) = sqrt((xcor(n, m) - xcor(pos, m))**2 + (ycor(n, m) - ycor(pos, m))**2)
                if (kfs(pos, m)==1 .and. dy(mx + 1)<f_lam*rlabda(n,m)) then
                   mx  = mx + 1
                   sdx = sdx + dy(mx)
                else
                   exit
                endif
             enddo
             if (sdx>0.0) then
                do j = 1, mx
                   hdps = max(real(dps(n + j - 1, m),fp)+s1(n + j - 1, m),0.01_fp)
                   grt  = sqrt((grmasu(n + j - 1, m)/hdps)**2 + &
                             & (grmasv(n + j - 1, m)/hdps)**2   )
                   mrvp = mrvp + grt*(dy(mx) - dy(j))/sdx
                enddo
             else
                hdps = max(real(dps(n, m),fp)+s1(n, m),0.01_fp)
                mrvp = sqrt((grmasu(n, m)/hdps)**2 + &
                          & (grmasv(n, m)/hdps)**2)
             endif
             !
             ! delay in negative y-direction
             !
             mx  = 1
             sdx = 0.0
             !
             do pos = n-1,1,-1
                dy(mx + 1) = sqrt((xcor(n, m) - xcor(pos, m))**2 + (ycor(n, m) - ycor(pos, m))**2)
                if (kfs(pos, m)==1 .and. dy(mx + 1)<f_lam*rlabda(n,m)) then
                   mx  = mx + 1
                   sdx = sdx + dy(mx)
                else
                   exit
                endif
             enddo
             if (sdx>0.0) then
                do j = 1, mx
                   hdps = max(real(dps(n - j + 1, m),fp)+s1(n - j + 1, m),0.01_fp)
                   grt  = sqrt((grmasu(n - j + 1, m)/hdps)**2 + &
                             & (grmasv(n - j + 1, m)/hdps)**2   )
                   mrvn = mrvn + grt*(dy(mx) - dy(j))/sdx
                enddo
             else
                hdps = max(real(dps(n, m),fp)+s1(n, m),0.01_fp)
                mrvn = sqrt((grmasu(n, m)/hdps)**2 + &
                          & (grmasv(n, m)/hdps)**2)
             endif
             !
             ! combine the contributions to obtain delay in wave direction
             !
             fis = sin(theta(n, m)*degrad)/max(abs(sin(theta(n, m)*degrad)),0.01_fp)
             fic = cos(theta(n, m)*degrad)/max(abs(cos(theta(n, m)*degrad)),0.01_fp)
             !
             grvt = sin(theta(n, m)*degrad)*((1.0 + fis)*mrvp + (1.0 - fis)*mrvn)/2.0
             grut = cos(theta(n, m)*degrad)*((1.0 + fic)*mrun + (1.0 - fic)*mrup)/2.0
             !
             !  total delayed flux in m.n point
             !
             gruvt = sqrt(grvt**2 + grut**2)
             !
             ! separate into x-and y directions
             !
             hdps = max(real(dps(n, m),fp)+s1(n, m) , 0.01_fp)
             !
             ! Breaker Delay Adjustment (Adjustment applied to mass flux in Euler and taubot)
             !
             grfacu(n, m) = gruvt*hdps*cos(theta(n, m)*degrad)-grmasu(n,m)
             grfacv(n, m) = gruvt*hdps*sin(theta(n, m)*degrad)-grmasv(n,m)
         else
         endif
      enddo
   enddo
   if (allocated(dx)) deallocate(dx, stat=istat)
   if (allocated(dy)) deallocate(dy, stat=istat)
end subroutine hds
