subroutine chkic(lundia    ,error     ,mmax      ,nmax      ,kmax      , &
               & nmaxus    ,lstsci    ,ltur      ,lturi     ,lsecfl    , &
               & r1        ,kfs       ,rtur1     ,gdp       )
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
!    Function: Checks for negative values of array r1 and of
!              array rtur1 only if rtur1 is read from restart
!              file
! Method used:
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
!
! Global variables
!
    integer                                                                    , intent(in) :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                    , intent(in) :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                                    , intent(in) :: lsecfl !  Description and declaration in esm_alloc_int.f90
    integer                                                                    , intent(in) :: ltur   !  Description and declaration in esm_alloc_int.f90
    integer                                                                    , intent(in) :: lturi  !  Description and declaration in tricom.igs
    integer                                                                                 :: lundia !  Description and declaration in inout.igs
    integer                                                                    , intent(in) :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                 :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                    , intent(in) :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in) :: kfs
    logical                                                                                 :: error  !!  Flag=TRUE if an error is encountered
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax, ltur), intent(in) :: rtur1  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, lstsci), intent(in) :: r1     !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer :: k
    integer :: l
    integer :: m
    integer :: n
    logical :: lerror ! Flag=TRUE if a warning is encountered 
!
!! executable statements -------------------------------------------------------
!
    ! Negative values for constituents 'not allowed'
    ! constituent values from a restart file may contain small negative
    ! values. therefore 'not allowed' is changed in writing a warning
    !
    if (lstsci > 0) then
       lerror = .false.
       do l = 1, lstsci
          !
          ! Spiral flow intensity can be both negative and positive,
          ! so, no check for l == lsecfl
          !
          if (l /= lsecfl) then
             do k = 1, kmax
                do m = 1, mmax
                   do n = 1, nmaxus
                      if (r1(n, m, k, l)<0.0 .and. kfs(n,m) == 1) then
                         lerror = .true.
                      endif
                   enddo
                enddo
             enddo
          endif
       enddo
       !
       ! errormessage is a warning
       !
       if (lerror) call prterr(lundia, 'V060', 'Initial condition')
    endif
    !
    ! Negative values for turbulence 'not allowed' if read (restart)
    !
    if (lturi <= 0) then
       do l = 1, ltur
          do k = 0, kmax
             do m = 1, mmax
                do n = 1, nmaxus
                   if (rtur1(n, m, k, l) < 0.0 .and. kfs(n,m) == 1) then
                      error = .true.
                   endif
                enddo
             enddo
          enddo
       enddo
       !
       ! errormessage
       !
       if (error) then
          call prterr(lundia    ,'V063'    ,'Turbulence Restart Values'     )
       endif
    endif
end subroutine chkic
