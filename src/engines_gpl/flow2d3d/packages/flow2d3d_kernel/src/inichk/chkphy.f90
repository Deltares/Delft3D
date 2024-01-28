subroutine chkphy(lundia     ,error     ,salin     ,temp      ,wind      , &
                & nmax       ,mmax      ,kmax      ,lmax      ,ktemp     , &
                & temeqs     ,saleqs    ,fclou     ,sarea     ,wstcof    , &
                & rhow       ,rhoa      ,kcu       ,kcv       ,kcs       , &
                & cfurou     ,cfvrou    ,vicuv     ,dicuv     ,anglon    , &
                & solrad_read,swrf_file ,sferic    ,gdp       )
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
!    Function: - Checks all physical coefficients
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
    real(fp)                , pointer :: tzone
!
! Global variables
!
    integer                                                                , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                , intent(in)  :: ktemp  !  Description and declaration in tricom.igs
    integer                                                                , intent(in)  :: lmax   !  Description and declaration in dimens.igs
    integer                                                                              :: lundia !  Description and declaration in inout.igs
    integer                                                                , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                , intent(in)  :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)           , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)           , intent(in)  :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)           , intent(in)  :: kcv    !  Description and declaration in esm_alloc_int.f90
    logical                                                                              :: error  !!  Flag=TRUE if an error is encountered
    logical                                                                , intent(in)  :: salin  !  Description and declaration in procs.igs
    logical                                                                , intent(in)  :: temp   !  Description and declaration in procs.igs
    logical                                                                , intent(in)  :: wind   !  Description and declaration in procs.igs
    logical                                                                , intent(in)  :: solrad_read !  Description and declaration in procs.igs
    logical                                                                , intent(in)  :: swrf_file   !  Description and declaration in procs.igs
    logical                                                                , intent(in)  :: sferic !  Description and declaration in procs.igs
    real(fp)                                                               , intent(in)  :: anglon !  Description and declaration in heat.igs
    real(fp)                                                                             :: fclou  !  Description and declaration in heat.igs
    real(fp)                                                               , intent(in)  :: rhoa   !  Description and declaration in physco.igs
    real(fp)                                                               , intent(in)  :: rhow   !  Description and declaration in esm_alloc_real.f90
    real(fp)                                                               , intent(in)  :: saleqs !  Description and declaration in tricom.igs
    real(fp)                                                               , intent(in)  :: sarea  !  Description and declaration in heat.igs
    real(fp)                                                               , intent(in)  :: temeqs !  Description and declaration in tricom.igs
    real(fp), dimension(6)                                                               :: wstcof !  Description and declaration in physco.igs
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub,3)                      :: cfurou !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub,3)                      :: cfvrou !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax + 2), intent(in)  :: dicuv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax + 2), intent(in)  :: vicuv  !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer           :: k
    integer           :: m
    integer           :: n
    logical           :: error1
!
!! executable statements -------------------------------------------------------
!
    tzone       => gdp%gdexttim%tzone
    !
    ! initialize local parameters
    !
    error  = .false.
    error1 = .false.
    !
    ! check value of TEMEQS, only if SALIN=.true. and TEMP <> .true.
    !
    if (salin .and. .not.temp .and. temeqs<=0.0) then
       call prterr(lundia    ,'V061'    ,'Temperature for Eq. of state'  )
       error = .true.
    endif
    !
    ! check value of SALEQS, only if TEMP=.true. and SALIN <> .true.
    !
    if (temp .and. .not.salin .and. saleqs<0.0) then
       call prterr(lundia    ,'V061'    ,'Salinity for Eq. of state'     )
       error = .true.
    endif
    !
    ! check FCLOU only if KTEMP=1 or 4
    !
    if (ktemp == 1) then
       fclou = fclou/100.0
       if (comparereal(fclou, 0.0_fp) < 0) then
          call prterr(lundia    ,'V061'    ,'Cloud par. for Excess T-model' )
          error = .true.
       endif
    endif
    if (ktemp == 4) then
       fclou = fclou/100.0
       if (comparereal(fclou, 0.0_fp) < 0) then
          call prterr(lundia    ,'V061'    ,'Cloud par. for Murakami T-model'          )
          error = .true.
       endif
    endif
    !
    ! check SAREA, only if KTEMP = 1,2 or 3
    !
    if (ktemp==1 .and. sarea<=0.0) then
       call prterr(lundia    ,'V061'    ,'Absolute T-model (Lee)'        )
       error = .true.
    elseif (ktemp==2 .and. sarea<=0.0) then
       call prterr(lundia    ,'V061'    ,'Absolute T-model'   )
       error = .true.
    elseif (ktemp==3 .and. sarea<=0.0) then
       call prterr(lundia    ,'V061'    ,'Area par. for Excess T-model'  )
       error = .true.
    else
    endif
    !
    ! Check ANGLON, only if KTEMP = 5, and solar radiation is NOT input
    !
    if (.not.sferic) then
        if (ktemp==5 .and. .not.solrad_read .and. .not.swrf_file .and. &
           & comparereal(tzone , 0.0_fp) == 0) then
           if (comparereal(anglon , 0.0_fp) == 0) then
              call prterr(lundia, 'U184', ' ')
           endif
        endif
    endif
    !
    ! check WSTCOF(1), (3) and RHOA for negative value,
    ! and WSTCOF(2) = WSTCOF(4), only if wind is true
    !
    if (wind) then
       if (wstcof(1) < 0.0_fp) then
          call prterr(lundia    ,'P004'    ,'Negative wind stress coefficient'       )
          error = .true.
       endif
       if (wstcof(3) < 0.0_fp) then
          call prterr(lundia    ,'P004'    ,'Negative wind stress coefficient'       )
          error = .true.
       endif
       if (wstcof(5) < 0.0_fp) then
          call prterr(lundia    ,'P004'    ,'Negative wind stress coefficient'       )
          error = .true.
       endif
       ! 
       ! Checks below YET to be expanded for 6 WSTCOF values
       !
       if (comparereal(wstcof(2), wstcof(4)) == 0) then
          if (comparereal(wstcof(1), wstcof(3)) == 0) then
             wstcof(4) = wstcof(2) + 1.0_fp
          else
             call prterr(lundia    ,'V062'    ,' '       )
             error = .true.
          endif
       endif
       if (rhoa <= 0.0) then
          call prterr(lundia    ,'V061'    ,'Density of air'     )
          error = .true.
       endif
    endif
    !
    ! check RHOW
    !
    if (rhow <= 0.0) then
       call prterr(lundia    ,'V061'    ,'Density of water'   )
       error = .true.
    endif
    !
    ! check VICUV
    ! NOTE: reset error1 to false at the end so that it can be used again
    !
    do k = 1, kmax
       do m = 1, mmax
          do n = 1, nmax
             if (kcs(n, m) > 0 .and. vicuv(n, m, k)<0.0) then 
                error1 = .true.
             endif
          enddo
       enddo
    enddo
    if (error1) call prterr(lundia, 'V061', 'Eddy viscosity')
    error = error .or. error1
    error1 = .false.
    !
    ! check DICUV
    ! NOTE: reset error1 to false at the end so that it can be used again
    !
    if (lmax > 0) then
       do k = 1, kmax
          do m = 1, mmax
             do n = 1, nmax
                if (kcs(n, m) > 0 .and. dicuv(n, m, k)<0.0) then 
                   error1 = .true.
                endif
             enddo
          enddo
       enddo
       if (error1) call prterr(lundia, 'V061', 'Eddy diffusivity')
       error = error .or. error1
       error1 = .false.
    endif
    !
    ! check BEDSTRESS coeff.
    !
    do m = 1, mmax
       do n = 1, nmax
          if (kcu(n, m)==1) then
             if (cfurou(n, m, 1)<=0.0) then
                error1 = .true.
             endif
          elseif (kcu(n, m)==0) then
             cfurou(n, m, 1) = -999.0_fp
          endif
          if (kcv(n, m)==1 ) then
             if (cfvrou(n, m, 1)<=0.0) then
                error1 = .true.
             endif
          elseif (kcv(n, m)==0) then
             cfvrou(n, m, 1) = -999.0_fp
          endif
       enddo
    enddo
    if (error1) call prterr(lundia, 'V061', 'Bed stress coefficient')
    error = error .or. error1
end subroutine chkphy
