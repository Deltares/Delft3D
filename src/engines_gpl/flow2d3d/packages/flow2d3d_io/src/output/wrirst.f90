subroutine wrirst(lundia    ,runid     ,itrstc    ,nmaxus    ,mmax      , &
                & nmax      ,kmax      ,lstsci    ,ltur      ,s1        , &
                & u1        ,v1        ,r1        ,rtur1     ,umnldf    , &
                & vmnldf    ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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
!  $Id: wrirst.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/io/src/output/wrirst.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: This routine writes the relevant output arrays to
!              the (single precision) restart file
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use string_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)              , pointer :: dt
    real(fp)              , pointer :: tunit
    real(fp)              , pointer :: timsec
    integer               , pointer :: julday
    integer               , pointer :: ifirst
!
! Global variables
!
    integer                                                                    , intent(in)  :: itrstc !!  Current time counter for the restart file. Start writing after first interval is passed. Last time will always be written to file for ITRSTI > 0
    integer                                                                    , intent(in)  :: kmax !  Description and declaration in esm_alloc_int.f90
    integer                                                                    , intent(in)  :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                                    , intent(in)  :: ltur !  Description and declaration in esm_alloc_int.f90
    integer                                                                                  :: lundia !  Description and declaration in inout.igs
    integer                                                                    , intent(in)  :: mmax !  Description and declaration in esm_alloc_int.f90
    integer                                                                                  :: nmax !  Description and declaration in esm_alloc_int.f90
    integer                                                                    , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: s1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: umnldf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: vmnldf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax, ltur), intent(in)  :: rtur1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in)  :: u1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in)  :: v1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, lstsci), intent(in)  :: r1 !  Description and declaration in esm_alloc_real.f90
    character(*)                                                                             :: runid
!
! Local variables
!
    integer        :: idate    ! Absolute date related to ITDATE and TIMSEC 
    integer        :: itime    ! Absolute time related to ITDATE and TIMSEC 
    integer        :: k        ! Help var. 
    integer        :: l        ! Help var. 
    integer        :: lrid     ! Actual length of var. RUNID 
    integer        :: lunrst   ! Unit number for the restart file, used only whne simulation sets the initial condition from this file 
    integer        :: m        ! Help var. 
    integer        :: n        ! Help var. 
    integer        :: newlun
    logical        :: ex       ! Help logical var. to determine whether the file currently beeing checked exist 
    character(256) :: filrst   ! Char. var. containing the restart file name 
!
!! executable statements -------------------------------------------------------
!
    ifirst      => gdp%gdwrirst%ifirst
    timsec      => gdp%gdinttim%timsec
    julday      => gdp%gdinttim%julday
    dt          => gdp%gdexttim%dt
    tunit       => gdp%gdexttim%tunit
    !-----statics
    !
    !
    !-----initialisation local parameters
    !
    filrst = ' '
    timsec = real(itrstc,fp)*dt*tunit
    !
    !-----Get absolute date and time
    !
    call timdat(julday    ,timsec    ,idate    ,itime     )
    !
    !-----get file name and test file existence
    !
    call remove_leading_spaces(runid     ,lrid      )
    !
    filrst(:8 + lrid) = 'tri-rst.' // runid(:lrid)
    write (filrst(8 + lrid + 1:8 + lrid + 16), '(a1,i8.8,a1,i6.6)') &
        & '.', idate, '.', itime
    !
    !-----Define new unit number
    !
    lunrst = newlun(gdp)
    !
    !-----Test existence of restart file
    !
    inquire (file = filrst(1:8 + lrid + 16), exist = ex)
    if (ex) then
       !
       !--------First entry write warning
       !
       if (ifirst==1) then
          call prterr(lundia    ,'S014'    ,filrst(:8 + lrid + 16)          )
       !
       endif
       open (lunrst, file = filrst(:8 + lrid + 16))
       close (lunrst, status = 'delete')
    endif
    !
    !-----New file => open file
    !
    open (lunrst, file = filrst(:8 + lrid + 16), form = 'unformatted',          &
         & status = 'new')
    !
    !-----write restart values S1, U1, V1, R1 and RTUR1
    !
    write (lunrst) ((real(s1(n, m) ,sp), m = 1, mmax), n = 1, nmaxus)
    !
    do k = 1, kmax
       write (lunrst) ((real(u1(n, m, k) ,sp), m = 1, mmax), n = 1, nmaxus)
    enddo
    !
    do k = 1, kmax
       write (lunrst) ((real(v1(n, m, k) ,sp), m = 1, mmax), n = 1, nmaxus)
    enddo
    !
    do l = 1, lstsci
       do k = 1, kmax
          write (lunrst) ((real(r1(n, m, k, l) ,sp), m = 1, mmax), n = 1, nmaxus)
       enddo
    enddo
    !
    do l = 1, ltur
       do k = 0, kmax
          write (lunrst) ((real(rtur1(n, m, k, l) ,sp), m = 1, mmax), n = 1, nmaxus)
       enddo
    enddo
    !
    !-----write filtered velocities to restart file to allow for
    !     restarts using subgrid viscosity model
    !
    write (lunrst) ((real(umnldf(n, m) ,sp), m = 1, mmax), n = 1, nmaxus)
    write (lunrst) ((real(vmnldf(n, m) ,sp), m = 1, mmax), n = 1, nmaxus)
    !
    !-----Close unit
    !
    close (lunrst)
    !
    !-----Redefine entry number
    !
    ifirst = 0
end subroutine wrirst
