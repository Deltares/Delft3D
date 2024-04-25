subroutine inigeo_dd(lundia    ,mmax      ,nmax      ,nmaxus    ,guu       , &
                   & gvv       ,guv       ,gvu       ,gsqs      ,gsqd      , &
                   & guz       ,gvz       ,gud       ,gvd       ,gsqiu     , &
                   & gsqiv     ,kfu       ,kfv       ,kfs       ,kcu       , &
                   & kcv       ,kcs       ,gdp       )
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
! Sets the geometry parameters of the model running in
! coupling points.
! INIGEO_DD should be used in combination with INIGEO
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
!
! Global variables
!
    integer                                                                    :: lundia !  Description and declaration in inout.igs
    integer                                                      , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                      , intent(in)  :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                      , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: kcv    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: kfv    !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: gsqd   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: gsqiu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: gsqiv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: gsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: gud    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: guz    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: gvd    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: gvz    !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer        :: ddb
    integer        :: iter
    integer        :: isum
    integer        :: kcsx
    integer        :: kcsy
    integer        :: kenm
    integer        :: kenmu
    integer        :: kenmv
    integer        :: m               ! Current M-index of the active point in the current computational ROW 
    integer        :: md              ! Current M-index minus 1 (see M) ROW 
    integer        :: mu              ! Current M-index plus  1 (see M) 
    integer        :: n               ! Current N-index of the active point in the current computational COLUMN 
    integer        :: nd              ! Current N-index minus 1 (see N) 
    integer        :: nu              ! Current N-index plus  1 (see N)
    logical        :: success
    logical        :: new_area_method ! .True.  when gsqs, gsqd, gsqiu and gsqiv are determined with the new approach
                                      ! .False. when gsqs, gsqd, gsqiu and gsqiv are determined with the old approach
    real(fp)       :: aver1
    real(fp)       :: aver2
    real(fp)       :: dx              ! Mesh sizes in both directions
    real(fp)       :: dy              ! Mesh sizes in both directions
    character(20)  :: errmsg          ! Character var. containing the error message to be written to file. The message depend on the error. 
    character(300) :: message
!
!! executable statements -------------------------------------------------------
!
    success = .true.
    ! Due to the a-symmetric domain decomposition coupling
    ! for velocity points (at left side additional column for U,
    ! and at bottom-side additional column for V) some of the lower
    ! bounds have been reduced by "DDB"
    ! Noted that this implementation  only works for DDB=1
    !
    errmsg = ' '
    !
    ! Temporary solution: the new method for determining GSQS, GSQD, GSQIU and GSQIV is switched off
    ! leads to difference in test bench results
    !
    new_area_method = .false.
    !
    ddb = gdp%d%ddbound
    !
    ! reset KCS for "inner corner" ad hoc solution (see mapper_uvz.cpp)
    ! N.B. a flexible stripsize is the preferred solution 
    !
    ! Row M=0 and column N=0 always corresponds to an additional KCS=3
    ! point (see inner corners in mapper.uvz.cpp) and can be removed
    !
    if (ddb > 0 ) then
       do m = 1, mmax
          kcs(0,m) = 0
       enddo
       do n = 1, nmaxus
          kcs(n,0) = 0
       enddo
    endif
    do n = 1-ddb, nmaxus
       do m = 1-ddb, mmax
          mu = min(m+1,mmax)
          nu = min(n+1,nmaxus)
          md = max(m-1,1-ddb)
          nd = max(n-1,1-ddb)
          if (kcs(n, m )==3) then
             iter = 0
             if (kcs(n , mu)==3) iter = iter + 1
             if (kcs(n , md)==3) iter = iter + 1
             if (kcs(nd, m )==3) iter = iter + 1
             if (kcs(nu, m )==3) iter = iter + 1
             isum = kcs(n , mu)+kcs(n , md)+kcs(nd, m )+kcs(nu, m )
             if (iter==1 .and. isum==3) then
                 kcs(n,m) = 0
             endif
             if (iter==2 .and. isum==6) then
                 kcs(n,m) = 0
             endif
             if (iter==3 .and. isum==9) then
                 kcs(n,m) = 0
             endif
             !  reset binnenhoekjes
             kcsx = kcs(n , mu) + kcs(n , md)
             kcsy = kcs(nu, m ) + kcs(nd, m )
             if ( kcsx ==4 .and. kcsy==4 ) then
                if (guu(n ,m )==0.0) then
                   guu(n ,m ) = min(guu(n  ,m-1) , guu(n  ,m+1))
                endif
                if (guu(n ,md)==0.0) then
                   guu(n ,md) = min(guu(n  ,m-2) , guu(n  ,m  ))
                endif
                if (gvv(n ,m )==0.0) then
                   gvv(n ,m ) = min(gvv(n-1,m  ) , gvv(n+1,m  ))
                endif
                if (gvv(nd,m )==0.0) then
                   gvv(nd,m ) = min(gvv(n-2,m  ) , gvv(n  ,m  ))
                endif
             endif
          endif
       enddo
    enddo
    !
    ! Compute KFU
    !
    do n = 1, nmaxus - 1
       do m = 1 - ddb, mmax
          mu = min(m+1,mmax)
          md = max(m-1,1-ddb)
          !
          ! at left side of subdomain interface:
          !
          if (kcs(n, m)==3 .and. kcs(n, mu)==1) then
             kfu(n, md) = 1
             kfu(n, m ) = 1
          endif
          !
          ! at right side of subdomain interface:
          !
          if (kcs(n, md)==1 .and. kcs(n, m)==3) then
             kfu(n, md) = 1
             kfu(n, m ) = 1
          endif
       enddo
    enddo
    !
    ! Compute KFV
    !
    do m = 1, mmax - 1
       do n = 1 - ddb, nmaxus
          nu = min(n+1,nmaxus)
          nd = max(n-1,1-ddb)
          !
          ! at bottom side of subdomain interface:
          !
          if (kcs(n, m)==3 .and. kcs(nu, m)==1) then
             kfv(nd, m) = 1
             kfv(n , m) = 1
          endif
          !
          ! at top side of subdomain interface:
          !
          if (kcs(nd, m)==1 .and. kcs(n, m)==3) then
             kfv(nd, m) = 1
             kfv(n , m) = 1
          endif
       enddo
    enddo
    !
    ! Compute KFS
    !
    do m = 1, mmax
       do n = 1, nmaxus
          if (kcs(n, m)==3) kfs(n, m) = 1
       enddo
    enddo
    !
    ! Compute GUU
    ! For coupling boundaries of one cell wide the "tangential"
    ! mesh size GUU equals zero and the mesh size of one cell further is applied
    !
    do n = 1, nmaxus - 1
       nd = max(n-1,1-ddb)
       do m = 1 - ddb, mmax-1
          mu = min(m+1,mmax)
          md = max(m-1,1-ddb)
          !
          ! at left side of subdomain interface:
          !
          if (kcs(n,m)==3 .and. kcs(n,mu)==1) then
             if (kcs(n,md) == 0 .and. guu(n,md)==0.0) then
                guu(n,md) = guu(n,m)
             endif
             if (gvv(nd,m) == 0.0) then
                gvv(nd,m) = gvv(n,m)
             endif
             if (gvv(n,m) == 0.0) then
                gvv(n,m) = gvv(nd,m)
             endif
             if (gvv(nd,m)==0.0 .and. gvv(n,m)==0.0) then
                gvv(n ,m) = gvv(n ,mu)
                gvv(nd,m) = gvv(nd,mu)
             endif
          endif
          !
          ! at right side of subdomain interface:
          !
          if (kcs(n,mu)==3 .and. kcs(n,m)==1) then
             if (guu(n,mu)==0.0) then
                guu(n,mu) = guu(n,m)
             endif
             if (gvv(nd,mu) == 0.0) then
                gvv(nd,mu) = gvv(n,mu)
             endif
             if (gvv(n,mu) == 0.0)then
                gvv(n,mu) = gvv(nd,mu)
             endif
             if (gvv(nd,mu)==0.0 .and. gvv(n,mu)==0.0)then
                gvv(n ,mu) = gvv(n ,m)
                gvv(nd,mu) = gvv(nd,m)
             endif
          endif
       enddo
    enddo
    !
    ! Compute GVV
    ! For coupling boundaries of one cell wide the "tangential"
    ! mesh size GVV equals zero and the mesh size of one cell further is applied
    !
    do m = 1, mmax - 1
       md = max(m-1,1-ddb)
       do n = 1 - ddb, nmaxus-1
          nu  = min(nmax, n + 1)
          nd  = max(1-ddb,n - 1)
          !
          ! at bottom side of subdomain interface:
          !
          if (kcs(n,m)==3 .and. kcs(nu,m)==1) then
             if (kcs(nd,m) == 0 .and. gvv(nd,m) == 0.0) then
                gvv(nd,m) = gvv(n,m)
             endif
             if (guu(n,md) == 0.0) then
                guu(n,md) = guu(n,m)
             endif
             if (guu(n,m) == 0.0) then
                guu(n,m) = guu(n,md)
             endif
             if (guu(n,md)==0.0 .and. guu(n,m) == 0.0) then
                guu(n,m ) = guu(nu,m )
                guu(n,md) = guu(nu,md)
             endif
          endif
          !
          ! at top side of subdomain interface:
          !
          if (kcs(nu,m)==3 .and. kcs(n,m)==1) then
             if (gvv(nu,m) == 0.0) then
                gvv(nu,m) = gvv(n,m)
             endif
             if (guu(nu,md) == 0.0) then
                guu(nu,md) = guu(nu,m)
             endif
             if (guu(nu,m) == 0.0) then
                guu(nu,m) = guu(nu,md)
             endif
             if (guu(nu,md)==0.0 .and. guu(nu,m)==0.0) then
                guu(nu,md) = guu(n,md)
                guu(nu,m ) = guu(n,m )
             endif
          endif
       enddo
    enddo
    !
    ! Compute GUV only if one of the GUU values <> 0. (KCS's = 3)
    ! (GUV is GUU at V-velocity point)
    !
    do m = 1, mmax
       md = max(m-1,1-ddb)
       do n = 1 - ddb, nmaxus
          nu = min(n+1, nmaxus)
          if (kcs(n,m)==3 .or. kcs(nu,m)==3) then
             kenm = 0
             if (guu(n ,m ) /= 0.0) kenm = kenm + 1
             if (guu(n ,md) /= 0.0) kenm = kenm + 1
             if (guu(nu,m ) /= 0.0) kenm = kenm + 1
             if (guu(nu,md) /= 0.0) kenm = kenm + 1
             if ( kenm > 0 ) then
                guv(n,m) = (guu(n,m)+guu(n,md)+guu(nu,m)+guu(nu,md)) / kenm
             endif
             if ( kcs(n,m)+kcs(nu,m)>=4 ) then
                aver1    = min(guu(n ,md) , guu(n ,m))
                aver2    = min(guu(nu,md) , guu(nu,m))
                guv(n,m) = 0.5_fp * (aver1+aver2)
             endif
          endif
       enddo
    enddo
    !
    ! Compute GVU only if one of the GVV values <> 0. (KCS's = 3)
    ! (GVU is GVV at U-velocity point)
    !
    do n = 1, nmaxus
       nd = max(n-1,1-ddb)
       do m = 1 - ddb, mmax
          mu = min(m+1,mmax)
          if (kcs(n, m)==3 .or. kcs(n, mu)==3) then
             kenm = 0
             if (gvv(n, m)/=0.) kenm = kenm + 1
             if (gvv(nd, m)/=0.) kenm = kenm + 1
             if (gvv(n, mu)/=0.) kenm = kenm + 1
             if (gvv(nd, mu)/=0.) kenm = kenm + 1
             if ( kenm > 0 ) then
                gvu(n, m) = (gvv(n, m) + gvv(nd, m) + gvv(n, mu) + gvv(nd, mu)) / kenm
             endif
             if ( kcs(n, m)+kcs(n, mu)>=4 ) then
                aver1    = min(gvv(nd,m ),gvv(n,m ))
                aver2    = min(gvv(nd,mu),gvv(n,mu))
                gvu(n,m) = 0.5_fp * (aver1+aver2)
             endif
          endif
       enddo
    enddo
    !
    ! Recompute GSQS at DD-boundaries
    ! copy GSQS from inner cell at DD-boundary
    !
    if (new_area_method) then
       !
       ! new way of determining gsqs and gsqs
       ! old way was wrong in case of strongly non-orthogonal cells
       !
       do n = 1, nmaxus
          nd = max(n-1, 1-ddb)
          nu = min(n+1, nmaxus)
          do m = 1, mmax
             md = max(m-1, 1-ddb)
             mu = min(m+1, mmax)
             if (kcs(n, m) == 3) then
                if (kcs(n,md) == 1) then
                   gsqs(n,m) = gsqs(n,md)
                elseif (kcs(n,mu) == 1) then
                   gsqs(n,m) = gsqs(n,mu)
                elseif(kcs(nd,m) == 1) then
                   gsqs(n,m) = gsqs(nd,m)
                elseif(kcs(nu,m) == 1) then
                   gsqs(n,m) = gsqs(nu,m)
                endif
             endif
          enddo
       enddo
       !
       ! Recompute GSQS at DD-boundaries
       ! copy GSQD from inner cell at DD-boundary
       !
       do n = 1, nmaxus
          nd = max(n-1, 1-ddb)
          nu = min(n+1, nmaxus)
          do m = 1, mmax
             md = max(m-1, 1-ddb)
             mu = min(m+1, mmax)
             if (kcs(n,m) == 3) then
                if (kcs(n,md) == 1) then
                   gsqd(n,m) = gsqd(n,md)
                elseif (kcs(n,mu) == 1) then
                   gsqd(n,m) = gsqd(n,mu)
                elseif(kcs(nd,m) == 1) then
                   gsqd(n,m) = gsqd(nd,m)
                elseif(kcs(nu,m) == 1) then
                   gsqd(n,m) = gsqd(nu,m)
                endif
             endif
          enddo
       enddo
    else
       !
       ! old way of determining gsqs and gsqs
       ! was wrong in case of strongly non-orthogonal cells
       !
       do n = 1, nmaxus
          nu = min(n+1,nmaxus)
          nd = max(n-1,1-ddb)
          do m = 1, mmax
             if (kcs(n, m)==3) then
                mu = min(m+1,mmax)
                md = max(m-1,1-ddb)
                kenmv = 0
                if (gvv(n , m)/=0.) kenmv = kenmv + 1
                if (gvv(nd, m)/=0.) kenmv = kenmv + 1
                kenmv = max(1, kenmv)
                dx = (gvv(n, m) + gvv(nd, m ))/kenmv
                kenmu = 0
                if (guu(n, m )/=0.) kenmu = kenmu + 1
                if (guu(n, md)/=0.) kenmu = kenmu + 1
                kenmu = max(1, kenmu)
                dy = (guu(n, m) + guu(n , md))/kenmu
                !
                ! For coupling boundaries of one cell wide the "tangential"
                ! mesh size is not defined. As a work around the grid size
                ! perpendicular to the coupling boundary is taken.
                !
                if ( kcu(n, m ) + kcu(n, md) .eq. 0 ) then
                   dy = dx
                endif
                if ( kcv(n, m ) + kcv(nd, m) .eq. 0 ) then
                   dx = dy
                endif
                gsqs(n, m) =   dx * dy
                !
                kenmv = 0
                if (gvv(n, m )/=0.) kenmv = kenmv + 1
                if (gvv(n, mu)/=0.) kenmv = kenmv + 1
                kenmv = max(1, kenmv)
                kenmu = 0
                if (guu(n , m)/=0.) kenmu = kenmu + 1
                if (guu(nu, m)/=0.) kenmu = kenmu + 1
                kenmu = max(1, kenmu)
                gsqd(n, m) =  ((gvv(n, m) + gvv(n , mu))/kenmv)                      &
                           & *((guu(n, m) + guu(nu, m ))/kenmu)
             endif
          enddo
       enddo
    endif    
    !
    ! Check computed GUU, GVU, GVV, GUV and GSQS
    !
    do n = 1 - ddb, nmaxus
       do m = 1 - ddb, mmax
          if (kcu(n, m)/=0) then
             if (guu(n, m)<=0.) then
                write (errmsg(10:), '(2i5)') m, n
                errmsg(:9) = 'GUU   at '
                call prterr(lundia    ,'V016'    ,errmsg    )
                success = .false.
             endif
             if (gvu(n, m)<=0.) then
                write (errmsg(10:), '(2i5)') m, n
                errmsg(:9) = 'GVU   at '
                call prterr(lundia    ,'V016'    ,errmsg    )
                success = .false.
             endif
          endif
          if (kcv(n, m)/=0) then
             if (gvv(n, m)<=0.) then
                write (errmsg(10:), '(2i5)') m, n
                errmsg(:9) = 'GVV   at '
                call prterr(lundia    ,'V016'    ,errmsg    )
                success = .false.
             endif
             if (guv(n, m)<=0.) then
                write (errmsg(10:), '(2i5)') m, n
                errmsg(:9) = 'GUV   at '
                call prterr(lundia    ,'V016'    ,errmsg    )
                success = .false.
             endif
          endif
          if (kcs(n, m)/=0) then
             if (gsqs(n, m)<=0.) then
                success = .false.
                nd = max(n - 1, 1)
                nu = min(n + 1, nmaxus)
                md = max(m - 1, 1)
                mu = min(m + 1, mmax)
                if (      (nd==n .or. kcs(nd,m )/=1) &
                    .and. (nu==n .or. kcs(nu,m )/=1) &
                    .and. (md==m .or. kcs(n ,md)/=1) &
                    .and. (mu==m .or. kcs(n ,mu)/=1)) then
                   write (message, '(a,i0,a,i0,a)') 'Boundary point (m,n) = (', m, ',', n, ') is not connected to an active cell'
                   call prterr(lundia, 'P004', trim(message))
                else
                   write (errmsg(10:), '(2i5)') m, n
                   errmsg(:9) = 'GSQS  at '
                   call prterr(lundia    ,'V016'    ,errmsg    )
                endif
             endif
          endif
       enddo
    enddo
    !
    ! Compute GUZ, GVZ, GUD, GVD, GSQIU and GSQIV at coupling points
    ! Also in column/row zero
    !
    ! All points are recomputed,because coupling points and neighbours of
    ! points are involved
    !
    do n = 1 - ddb, nmaxus
       nu = min(n+1,nmaxus)
       nd = max(n-1,1-ddb)
       do m = 1 - ddb, mmax
          mu = min(m+1,mmax)
          md = max(m-1,1-ddb)
          guz(n, m) = 0.5*(guu(n, m) + guu(n, md))
          gvz(n, m) = 0.5*(gvv(n, m) + gvv(nd, m))
          gud(n, m) = 0.5*(guu(n, m) + guu(nu, m))
          gvd(n, m) = 0.5*(gvv(n, m) + gvv(n, mu))
          !
          if (kcu(n, m)/=0) then
             if (new_area_method) then
                gsqiu(n, m) = 2.0_fp / (gsqs(n, m)+gsqs(n,mu))
             else
                gsqiu(n, m) = 1./(gvu(n, m)*guu(n, m))
             endif
          else
             gsqiu(n, m) = 0.0_fp
          endif
          if (kcv(n, m)/=0) then
             if (new_area_method) then
                gsqiv(n, m) = 2.0_fp / (gsqs(n, m)+gsqs(nu,m))
             else
                gsqiv(n, m) = 1./(guv(n, m)*gvv(n, m))
             endif
          else
             gsqiv(n, m) = 0.0_fp
          endif
       enddo
    enddo
    if (.not. success) then
       call d3stop (3,gdp)
    endif
end subroutine inigeo_dd
