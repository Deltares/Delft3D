subroutine secbou(j         ,nmmaxj    ,kmax      ,icx       ,icy       , &
                & lstsci    ,lsecfl    ,kfu       ,irocol    ,norow     , &
                & s0        ,s1        ,dps       ,r1        ,sour      , &
                & sink      ,gdp       )
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
!    Function: Horizontal boundary conditions for secondary
!              flow (spiral motion intensity) is stored in R1
!              The horizontal and vertical boundary conditions
!              assume I_BE + I_CE
!              Secondary flow only in 2D situation
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
    real(fp)     , pointer :: eps
    real(fp)     , pointer :: dryflc
!
! Global variables
!
    integer, intent(in)            :: icx
                                   !!  Increment in the x-dir., if icx= nmax
                                   !!  then computation proceeds in the x-
                                   !!  dir. if icx=1 then computation pro-
                                   !!  ceeds in the y-dir.
    integer, intent(in)            :: icy
                                   !!  Increment in the y-dir. (see icx)
    integer         :: j
                                   !!  Begin pointer for arrays which have
                                   !!  been transformed into 1d arrays.
                                   !!  due to the shift in the 2nd (m-)
                                   !!  index, j = -2*nmax + 1
    integer, intent(in)            :: kmax !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)            :: lsecfl !  Description and declaration in dimens.igs
    integer, intent(in)            :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer         :: nmmaxj !  Description and declaration in dimens.igs
    integer, intent(in)            :: norow !  Description and declaration in esm_alloc_int.f90
    integer, dimension(5, norow), intent(in) :: irocol !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfu !  Description and declaration in esm_alloc_int.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: dps !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: s0 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: s1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), intent(out) :: r1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), intent(in) :: sink !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), intent(in) :: sour !  Description and declaration in esm_alloc_real.f90
!
!
! Local variables
!
    integer                        :: ddb
    integer                        :: ic
    integer                        :: icxy
    integer                        :: k                    ! 2dH application 
    integer                        :: mf                   ! IROCOL(2,IC)-1 
    integer                        :: ml                   ! IROCOL(3,IC) 
    integer                        :: n                    ! IROCOL(1,IC) 
    integer                        :: nmf
    integer                        :: nmfu                 ! NMF+ICX 
    integer                        :: nml
    integer                        :: nmlu                 ! NML+ICX 
    real(fp)                       :: epsh
    real(fp)                       :: equili               ! Sour/sink 
    real(fp)                       :: h0new
    real(fp)                       :: h0old
    real(fp)                       :: sinkhn
!
!
!! executable statements -------------------------------------------------------
!
    eps       => gdp%gdconst%eps
    dryflc    => gdp%gdnumeco%dryflc
    !
    epsh = 0.5*eps*dryflc
    !
    ddb = gdp%d%ddbound
    icxy = max(icx, icy)
    k = 1
    !
    ! loop over computational grid
    !
    do ic = 1, norow
       !
       n = irocol(1, ic)
       mf = irocol(2, ic) - 1
       ml = irocol(3, ic)
       nmf = (n + ddb)*icy + (mf + ddb)*icx - icxy
       nml = (n + ddb)*icy + (ml + ddb)*icx - icxy
       nmlu = nml + icx
       nmfu = nmf + icx
       !
       !***open boundary at begin of row
       !
       if (kfu(nmf)==1) then
          h0new = s1(nmfu) + real(dps(nmfu),fp)
          sinkhn = sink(nmfu, k, lsecfl)*h0new
          if (abs(sinkhn)>epsh) then
             h0old = s0(nmfu) + real(dps(nmfu),fp)
             equili = sour(nmfu, k, lsecfl)*h0old/sinkhn
             r1(nmf, k, lsecfl) = equili
          else
             r1(nmf, k, lsecfl) = 0.0
          endif
       endif
       !
       ! open boundary at end of row
       !
       if (kfu(nml)==1) then
          h0new = s1(nml) + real(dps(nml),fp)
          sinkhn = sink(nml, k, lsecfl)*h0new
          if (abs(sinkhn)>epsh) then
             h0old = s0(nml) + real(dps(nml),fp)
             equili = sour(nml, k, lsecfl)*h0old/sinkhn
             r1(nmlu, k, lsecfl) = equili
          else
             r1(nmlu, k, lsecfl) = 0.0
          endif
       endif
    enddo
end subroutine secbou
