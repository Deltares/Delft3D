subroutine chkdis(lundia    ,error     ,nsrc      ,zmodel    ,nmax      , &
                & mmax      ,nmaxus    ,kmax      ,namsrc    ,mnksrc    , &
                & kcs       ,xyzsrc    ,sig       ,zk        ,dps       , &
                & s1        ,xz        ,yz        ,gdp       )
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
!    Function: - Checks the location of the following points (if
!                specified) : Discharges
!                These points should/must lie on the active pnts.
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use dfparall
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
    integer                                                           , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                         :: lundia !  Description and declaration in inout.igs
    integer                                                           , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                         :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                           , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer                                                           , intent(in)  :: nsrc   !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(7, nsrc)                                  , intent(in)  :: mnksrc !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    logical                                                                         :: error  !!  Flag=TRUE if an error is encountered
    logical                                                           , intent(in)  :: zmodel !  Description and declaration in procs.igs
    real(fp)     , dimension(3, nsrc)                                 , intent(out) :: xyzsrc !  Description and declaration in esm_alloc_real.f90
    real(prec)   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: xz     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: yz     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(kmax)                                    , intent(in)  :: sig    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(0:kmax)                                  , intent(in)  :: zk     !!  Initial layer thickness
    character(20), dimension(nsrc)                                                  :: namsrc !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer            :: ipow
    integer            :: k       ! Help var. 
    integer            :: locpow
    integer            :: m       ! Current M-index of the active point in the current computational ROW 
    integer            :: n       ! Current N-index of the active point in the current computational COLUMN 
    integer            :: nr      ! Sequence number of open boundary points 
    real(fp)           :: h0      ! Water depth (DPS+S1) 
    character(40)      :: errmsg  ! Character var. containing the error message to be written to file. The message depend on the error. 
!
!! executable statements -------------------------------------------------------
!
    ! initialize local parameters
    !
    errmsg = ' '
    !
    ! Check position discharges
    !     when outside boundaries, then error
    !     check array places 4,5,6, in case of powerstation check also
    !     array places 1, 2, 3
    !
    do nr = 1, nsrc
       m = mnksrc(4, nr)
       n = mnksrc(5, nr)
       k = mnksrc(6, nr)
       !
       ! skip this point when it is outside this partition
       !
       if (k == -1) cycle
       locpow = 1
       if (mnksrc(7, nr)==2) locpow = 2
       do ipow = 1, locpow
          if (ipow==2) then
             m = mnksrc(1, nr)
             n = mnksrc(2, nr)
             k = mnksrc(3, nr)
             if (mnksrc(1, nr)==mnksrc(4, nr) .and. mnksrc(2, nr)==mnksrc(5, nr)&
               & .and. mnksrc(3, nr)==mnksrc(6, nr)) then
                write (lundia,'(''*** WARNING In and outlet of power '',''station '', a, '' are the same'')') namsrc(nr)
                write (lundia, '(20x,'' (m,n,k) = '',3i4)') m, n, k
             endif
          endif
          !
          ! Test values with model dimensions
          !
          if (m<1 .or. n<1) then
             errmsg = 'Discharge location ' // namsrc(nr)
             call prterr(lundia    ,'U007'    ,errmsg    )
             write (lundia, '(20x,'' (m,n,k) = '',3i4)') m, n, k
             error = .true.
             exit
          endif
          if (m>mmax .or. n>nmaxus) then
             errmsg = 'Discharge location ' // namsrc(nr)
             call prterr(lundia    ,'U140'    ,errmsg    )
             write (lundia, '(20x,'' (m,n,k) = '',3i4)') m, n, k
             error = .true.
             exit
          endif
          if (k<0 .or. k>kmax) then
             call prterr(lundia    ,'V052'    ,namsrc(nr))
             write (lundia, '(20x,'' (m,n,k) = '',3i4)') m, n, k
             error = .true.
          endif
          !
          ! Test discharge is inner active point (KCS = 1)
          !
          if (kcs(n, m)/=1 .and. kcs(n, m)/=-1) then
             call prterr(lundia    ,'V051'    ,namsrc(nr))
             write (lundia, '(20x,'' (m,n,k) = '',3i4)') m, n, k
             error = .true.
          endif
       enddo
    enddo
    if (error) goto 9999
    !
    ! Compute exact position discharges
    !     NOTE: the Z - posistion is "nonsense" for MNKSRC(3,NR) = 0
    !     xyzsrc contains the position of the outlet in case of
    !     power station or culvert
    !     NOTE: IMPORTANT
    !     In case of z-model different way of determining the z-coordinate
    !     (using relative layer thickness; complementary value is used due
    !      to inverted definition of layer indices in the z-model)
    !
    do nr = 1, nsrc
       m = mnksrc(4, nr)
       n = mnksrc(5, nr)
       k = mnksrc(6, nr)
       !
       ! skip this point when it is outside this partition
       !
       if (k == -1) cycle
       xyzsrc(1, nr) = xz(n, m)
       xyzsrc(2, nr) = yz(n, m)
       !
       h0 = real(dps(n, m),fp) + s1(n, m)
       !
       if (k==0) then
          xyzsrc(3, nr) = 0.5*h0
       elseif (zmodel) then
          xyzsrc(3, nr) = zk(k)*h0
       else
          xyzsrc(3, nr) = (1.0 + sig(k))*h0
       endif
    enddo
 9999 continue
end subroutine chkdis
