subroutine fouana(mmax      ,nmaxus    ,nofou     ,ifou      ,kcs      , &
                & kfs       ,kfu       ,kfv       ,nst       ,rarray    , &
                & umean     ,vmean     ,dps       ,gdp       )
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
!    Function: - performs fourier analysis i.e. computes suma
!                and sumb
!              - calculates MAX or MIN value
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)                             , pointer :: ag
    integer        , dimension(:)        , pointer :: ftmsto
    integer        , dimension(:)        , pointer :: ftmstr
    integer        , dimension(:)        , pointer :: foumask
    real(fp)       , dimension(:)        , pointer :: foufas
    real(fp)       , dimension(:,:,:)    , pointer :: fousma
    real(fp)       , dimension(:,:,:)    , pointer :: fousmb
    character(1)   , dimension(:)        , pointer :: fouelp
    character(16)  , dimension(:)        , pointer :: founam
    integer        , dimension(:)        , pointer :: kfst0
!
! Global variables
!
    integer                                                                , intent(in)  :: ifou   !!  Counter
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in)  :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in)  :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer                                                                , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer                                                                , intent(in)  :: nofou  !  Description and declaration in dimens.igs
    integer                                                                , intent(in)  :: nst    !!  Time step number
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in)  :: rarray !!  Array for fourier analysis
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in)  :: umean  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in)  :: vmean  !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in)  :: dps
!
! Local variables
!
    integer         :: kkfu
    integer         :: kkfv
    integer         :: m       ! Loop counter over MMAX 
    integer         :: md
    integer         :: n       ! Loop counter over NMAXUS 
    integer         :: nd
    integer         :: nm      ! Loop counter over NMMAX 
    real(fp)        :: angl
    real(fp)        :: uuu     ! umean in zeta point
    real(fp)        :: vvv     ! vmean in zeta point
    real(fp)        :: utot2   ! |U|**2 = uuu**2 + vvv**2
!
!! executable statements -------------------------------------------------------
!
    ag        => gdp%gdphysco%ag
    ftmsto    => gdp%gdfourier%ftmsto
    ftmstr    => gdp%gdfourier%ftmstr
    foumask   => gdp%gdfourier%foumask
    foufas    => gdp%gdfourier%foufas
    fousma    => gdp%gdfourier%fousma
    fousmb    => gdp%gdfourier%fousmb
    fouelp    => gdp%gdfourier%fouelp
    founam    => gdp%gdfourier%founam
    kfst0     => gdp%gdpostpr%kfst0
    !
    ! Initialize for MAX = -1.0e+30 / MIN = 1.0e+30
    !
    if (nst==ftmstr(ifou)) then
       if (fouelp(ifou)=='x' .or. fouelp(ifou)=='e') then
          fousma(:, :, ifou) = -1.0e+30_fp
          fousmb(:, :, ifou) = -1.0e+30_fp
       elseif (fouelp(ifou)=='i') then
          fousma(:, :, ifou) =  1.0e+30_fp
       else
          fousma(:, :, ifou) =  0.0_fp
          fousmb(:, :, ifou) =  0.0_fp
       endif
    endif
    !
    ! Perform fourier analysis, every timestep as long as NST value
    ! lies in requested time interval FTMSTR and FTMSTO
    !
    if (nst>=ftmstr(ifou) .and. nst<ftmsto(ifou)) then
       if (fouelp(ifou) == 'x') then
          !
          ! Calculate MAX value
          !
          if (founam(ifou) == 's1') then
             if (foumask(ifou) == 0) then
                do n = 1, nmaxus
                   do m = 1, mmax
                      if (kfs(n,m) == 1) then
                         !
                         ! Waterlevel (fousma) and waterdepth (fousmb),
                         ! only for wet points
                         !
                         fousma(n,m,ifou) = max(fousma(n,m,ifou), rarray(n,m))
                         fousmb(n,m,ifou) = max(fousmb(n,m,ifou), rarray(n,m) + real(dps(n,m),fp))
                      endif
                   enddo
                enddo
             elseif (foumask(ifou) == 1) then
                do n = 1, nmaxus
                   do m = 1, mmax
                      call n_and_m_to_nm(n, m, nm, gdp)
                      if (kfs(n,m)==1 .and. kfst0(nm)==0) then
                         !
                         ! Waterlevel (fousma) and waterdepth (fousmb),
                         ! only for wet points, only for initially dry points
                         !
                         fousma(n,m,ifou) = max(fousma(n,m,ifou), rarray(n,m))
                         fousmb(n,m,ifou) = max(fousmb(n,m,ifou), rarray(n,m) + real(dps(n,m),fp))
                      endif
                   enddo
                enddo
             endif
          else
             do n = 1, nmaxus
                do m = 1, mmax
                   if (kcs(n,m) == 1) then
                      fousma(n, m, ifou) = max(fousma(n,m,ifou), rarray(n,m))
                   endif
                enddo
             enddo
          endif
       elseif (fouelp(ifou) == 'e') then
          !
          ! Calculate MAX Energy head value
          !
             if (foumask(ifou) == 0) then
                do n = 1, nmaxus
                   nd = max(n-1 , 1)
                   do m = 1, mmax
                      if (kfs(n,m) == 1) then
                         !
                         ! Energy head, only for wet points
                         !
                         md    = max(m-1 , 1)
                         kkfu  = max( 1 , kfu(n,m)+kfu(n ,md) )
                         kkfv  = max( 1 , kfv(n,m)+kfv(nd,m ) )
                         uuu   = (umean(n,md)*kfu(n,md) + umean(n,m)*kfu(n,m)) / kkfu
                         vvv   = (vmean(nd,m)*kfv(nd,m) + vmean(n,m)*kfv(n,m)) / kkfv
                         utot2 = uuu*uuu + vvv*vvv
                         fousma(n,m,ifou) = max(fousma(n,m,ifou), 0.5_hp*utot2/ag + rarray(n,m))
                      endif
                   enddo
                enddo
             elseif (foumask(ifou) == 1) then
                do n = 1, nmaxus
                   nd = max(n-1 , 1)
                   do m = 1, mmax
                      call n_and_m_to_nm(n, m, nm, gdp)
                      if (kfs(n,m)==1 .and. kfst0(nm)==0) then
                         !
                         ! Energy head, only for wet points, only for initially dry points
                         !
                         md    = max(m-1 , 1)
                         kkfu  = max( 1 , kfu(n,m)+kfu(n ,md) )
                         kkfv  = max( 1 , kfv(n,m)+kfv(nd,m ) )
                         uuu   = (umean(n,md)*kfu(n,md) + umean(n,m)*kfu(n,m)) / kkfu
                         vvv   = (vmean(nd,m)*kfv(nd,m) + vmean(n,m)*kfv(n,m)) / kkfv
                         utot2 = uuu*uuu + vvv*vvv
                         fousma(n,m,ifou) = max(fousma(n,m,ifou), 0.5_hp*utot2/ag + rarray(n,m))
                      endif
                   enddo
                enddo
             endif
       elseif (fouelp(ifou) == 'i') then
          !
          ! Calculate MIN value
          !
          do n = 1, nmaxus
             do m = 1, mmax
                if (kcs(n,m) == 1) then
                   fousma(n,m,ifou) = min(fousma(n,m,ifou), rarray(n,m))
                endif   
             enddo
          enddo
       !
       ! Calculate total for fourier analyse
       !
       else
          angl = real(nst - ftmstr(ifou),fp)*foufas(ifou)
          do n = 1, nmaxus
             do m = 1, mmax
                if (kcs(n,m) == 1) then
                   fousma(n,m,ifou) = fousma(n,m,ifou) + rarray(n,m)*cos(angl)
                   fousmb(n,m,ifou) = fousmb(n,m,ifou) + rarray(n,m)*sin(angl)
                endif   
             enddo
          enddo
       endif
    endif
end subroutine fouana
