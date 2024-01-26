subroutine chkvic(lundia    ,j         ,nmmaxj    ,nmmax     ,kmax      , &
                & icx       ,icy       ,timnow    ,kfs       ,kfu       , &
                & kfv       ,kcs       ,lstsci    ,guv       ,gvu       , &
                & vicuv     ,dicuv     ,itype     ,kfsmin    ,kfsmax    , &
                & gdp       )
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
!    Function: Checkes stability criterion for horizontal viscosity and
!              diffusivity according to "first2d.doc"
!              (R.E. Uittenbogaard 24-12-99)
!              NOTE: changes to incorporate HLES in DD
! Method used:
!     Comment: The HLES contribution to vicuv/dicuv is stored in array element
!              kmax+2, the background contribution in kmax+1
!              
!              Four different checks are done:  
!              
!   itype = 1             : check on input value for horizontal viscosity
!   itype = 1 and lstsci>0: check on input value for horizontal diffusivity
!   itype = 2             : check on HLES value for horizontal viscosity
!   itype = 2 and lstsci>0: check on HLES value for horizontal diffusivity
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
    real(fp)     , pointer :: hdt
    logical      , pointer :: zmodel
!
! Global variables
!
    integer                                         , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX
                                                                            !!  then computation proceeds in the X-
                                                                            !!  dir. If icx=1 then computation pro-
                                                                            !!  ceeds in the Y-dir.
    integer                                         , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                       :: j      !!  Begin pointer for arrays which have
                                                                            !!  been transformed into 1D arrays.
                                                                            !!  Due to the shift in the 2nd (M-)
                                                                            !!  index, J = -2*NMAX + 1
    integer                                         , intent(in)  :: itype
    integer                                         , intent(in)  :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                       :: lundia !  Description and declaration in inout.igs
    integer                                         , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer                                                       :: nmmaxj !  Description and declaration in dimens.igs
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfsmax !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfv    !  Description and declaration in esm_alloc_int.f90
    real(fp)                                        , intent(in)  :: timnow
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax + 2)          :: dicuv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax + 2)          :: vicuv  !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer        :: icount
    integer        :: iexit
    integer        :: itotal
    integer        :: kbg
    integer        :: k
    integer        :: kstart
    integer        :: kend
    integer        :: m
    integer        :: n
    integer        :: nm
    logical        :: error
    real(fp)       :: chezy  ! Total chezy value in zeta point 
    real(fp)       :: depth  ! Total depth 
    real(fp)       :: dicmax
    real(fp)       :: vicmax
    character(256) :: errmsg
!
!! executable statements -------------------------------------------------------
!
    hdt        => gdp%gdnumeco%hdt
    zmodel     => gdp%gdprocs%zmodel
    !
    ! Initialization
    !
    kbg   = kmax + 1
    !
    ! The HLES contribution to vicuv/dicuv is stored in array element kmax+2
    ! The background contribution is stored in array element kmax+1
    !
    if (itype == 1) then
       !
       ! Check input value VICUV(KBG) and set upper bound for VICUV
       ! (maximum value based on CFL stability criterion)
       !
       ! The first ten points for which vicuv is set to vicmax
       ! are printed in the diagnostic-file
       !
       icount = 0
       itotal = 0
       do nm = 1, nmmax
          if (kfs(nm) == 1) then
             vicmax = 1.0/(guv(nm)*guv(nm)) + 1.0/(gvu(nm)*gvu(nm))
             vicmax = 0.9 / (4.0*hdt*vicmax)
             if (vicuv(nm, kbg) >=  vicmax) then
                vicuv(nm, kbg) = vicmax
                icount         = icount + 1
                if (icount <= 10) then
                   call nm_to_n_and_m(nm, n, m, gdp)
                   write (errmsg,'(a,i5,a,i5,a)') &
                       & 'Background horizontal Eddy Viscosity reset for (m,n) = (', &
                       & m, ',', n, ')'
                   call prterr (lundia, 'U190', trim(errmsg))
                endif
             endif
             itotal = itotal + 1
          endif
       enddo
       !
       ! Print error messages (if any)
       !
       if (icount > 10) then
           write (errmsg,'(a,i6,a)') &
               & 'Background horizontal Eddy Viscosity reset for ', icount, ' points.'
          call prterr(lundia, 'U190', trim(errmsg))
       endif
       !
       ! Stop Delft3D-FLOW if criterion is exceeded in 10% of the points
       !
       if (real(icount,fp) > real(itotal,fp)/10.0_fp) then
          write (errmsg, '(a)') &
               & 'The courant number is violated in the viscosity term in more than 10% of the points. Decrease the time step to solve this problem.'
          call prterr(lundia, 'P004', trim(errmsg))
          !
          ! stop routine for DELFT3D
          !
          iexit = 4
          call d3stop(iexit, gdp)
       endif
       !
       ! Check input value DICUV(KBG) and set upper bound for DICUV
       ! (maximum value based on CFL stability criterion)
       !
       ! The first ten points for which vicuv is set to dicmax
       ! are printed in the diagnostic-file
       !
       if (lstsci > 0 ) then
          icount = 0
          itotal = 0
          do nm = 1, nmmax
             if (kfs(nm) == 1) then
                vicmax = 1.0/(guv(nm)*guv(nm)) + 1.0/(gvu(nm)*gvu(nm))
                vicmax = 0.9 / (4.0*hdt*vicmax)
                !
                ! Criterion adapted because of Prandtl-Schmidt number
                !
                dicmax = 0.7 * vicmax
                if (dicuv(nm, kbg) >= dicmax) then
                   dicuv(nm, kbg) = dicmax
                   icount         = icount + 1
                   if (icount <= 10) then
                      call nm_to_n_and_m(nm, n, m, gdp)
                      write (errmsg,'(a,i5,a,i5,a)') &
                          & 'Background horizontal Eddy Diffusivity reset for (m,n) = (', &
                          & m, ',', n, ')'
                      call prterr (lundia, 'U190', trim(errmsg))
                   endif
                endif
                itotal = itotal + 1
             endif
          enddo
          !
          ! Print error messages (if any)
          !
          if (icount > 10) then
              write (errmsg,'(a,i6,a)') &
                  & 'Background horizontal Eddy Diffusivity reset for ',icount,' points.'
              call prterr(lundia, 'U190', trim(errmsg))
          endif
          !
          ! Stop Delft3D-FLOW if criterion is exceeded in 10% of the points
          !
          if (real(icount,fp) > real(itotal,fp)/10.0_fp) then
             write (errmsg, '(a)') &
                  & 'The courant number is violated in the viscosity term in more than 10% of the points. Decrease the time step to solve this problem.'
             call prterr(lundia, 'P004', trim(errmsg))
             !
             ! stop routine for DELFT3D
             !
             iexit = 4
             call d3stop(iexit, gdp)
          endif
       endif    ! lstsci > 0
    elseif (itype == 2) then
       !
       ! Check VICUV(K) value, which have been computed by the HLES model
       ! and set upper bound for VICUV
       ! (maximum value based on CFL stability criterion)
       !
       ! Exclude DD coupling points: those values will be overwritten by
       ! the other domain; resetting them is useless
       !
       ! The first ten points for which vicuv is set to vicmax
       ! are printed in the diagnostic-file
       !
       error  = .false.
       icount = 0
       do nm = 1, nmmax
          if (kfs(nm)==1 .and. kcs(nm)/=3) then
             vicmax = 1.0/(guv(nm)*guv(nm)) + 1.0/(gvu(nm)*gvu(nm))
             vicmax = 0.9 / (4.0*hdt*vicmax)
             if (zmodel) then
                kstart = kfsmin(nm)
                kend   = kfsmax(nm)
             else
                kstart = 1
                kend   = kmax
             endif
             do k = kstart, kend
                if (vicuv(nm,k) > vicmax) then
                   vicuv(nm,k) = vicmax
                   error       = .true.
                   icount      = icount + 1
                endif
             enddo
             if (error .and. icount<=10) then
                call nm_to_n_and_m(nm, n, m, gdp)
                write (errmsg,'(a,g15.5,a,i5,a,i5,a)') &
                    & 'Horizontal Eddy Viscosity reset at time ', &
                    & timnow, ' for (m,n) = (', m, ',', n, ')'
                call prterr (lundia, 'U190', trim(errmsg))
                error = .false.
             endif
          endif
       enddo
       !
       ! Print error messages (if any)
       !
       if (error .and. icount > 10) then
           write (errmsg,'(a,g15.5,a,i6,a)') &
               & 'Horizontal Eddy Viscosity reset at time ', timnow, ' for ', icount, ' points.'
          call prterr(lundia, 'U190', trim(errmsg))
       endif
       if (lstsci > 0) then
          !
          ! Check DICUV(K) value, which has been computed by the HLES model
          ! and set upper bound for DICUV
          ! (maximum value based on CFL stability criterion)
          !
          ! Exclude DD coupling points: those values will be overwritten by
          ! the other domain; resetting them is useless
          !
          ! The first ten points for which dicuv is set to vicmax
          ! are printed in the diagnostic-file
          !
          error  = .false.
          icount = 0
          do nm = 1, nmmax
             if (kfs(nm)==1 .and. kcs(nm)/=3) then
                vicmax = 1.0/(guv(nm)*guv(nm)) + 1.0/(gvu(nm)*gvu(nm))
                vicmax = 0.9 / (4.0*hdt*vicmax)
                !
                ! Criterion adapted because of Prandtl-Schmidt number
                !
                dicmax = 0.7 * vicmax
                if (zmodel) then
                   kstart = kfsmin(nm)
                   kend   = kfsmax(nm)
                else
                   kstart = 1
                   kend   = kmax
                endif
                do k = kstart, kend
                   if (dicuv(nm,k) > dicmax) then
                      dicuv(nm,k) = dicmax
                      error       = .true.
                      icount      = icount + 1
                   endif
                enddo
                if (error .and. icount<=10) then
                   call nm_to_n_and_m(nm, n, m, gdp)
                   write (errmsg,'(a,g15.5,a,i5,a,i5,a)') &
                       & 'Horizontal Eddy Diffusivity reset at time ', &
                       & timnow, ' for (m,n) = (', m, ',', n, ')'
                   call prterr (lundia, 'U190', trim(errmsg))
                   error = .false.
                endif
             endif
          enddo
          !
          ! Print error messages (if any)
          !
          if (error .and. icount > 10) then
              write (errmsg,'(a,g15.5,a,i6,a)') &
                  & 'Horizontal Eddy Diffusivity reset at time ', timnow, ' for ', icount, ' points.'
             call prterr(lundia, 'U190', trim(errmsg))
          endif
       endif    ! lstsci > 0
    endif       ! itype == 2
end subroutine chkvic
