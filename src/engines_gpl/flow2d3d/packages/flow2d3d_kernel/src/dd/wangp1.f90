subroutine wangp1(s1        ,kcs       ,irocol    ,norow     ,icx       , &
                & icy       ,j         ,nmmaxj    ,a         ,b         , &
                & c         ,d         ,gdp       )
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
!    Function: WANGP1 performs step 1 of the so-called
!              "Method of Wang", which solves tridiagonal systems
!              (pre-elimination step)
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
    integer                                   , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX
                                                                      !!  then computation proceeds in the X-
                                                                      !!  dir. If icx=1 then computation pro-
                                                                      !!  ceeds in the Y-dir.
    integer                                   , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                 :: j      !!  Begin pointer for arrays which have
                                                                      !!  been transformed into 1D arrays.
                                                                      !!  Due to the shift in the 2nd (M-)
                                                                      !!  index, J = -2*NMAX + 1
                                                                      !!  Begin pointer for arrays which have
                                                                      !!  been transformed into 1D arrays.
                                                                      !!  Due to the shift in the 2nd (M-)
                                                                      !!  index, J = -2*NMAX + 1
    integer                                                 :: nmmaxj !  Description and declaration in dimens.igs
    integer                                   , intent(in)  :: norow  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(5, norow)             , intent(in)  :: irocol !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              :: a
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              :: b
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              :: c      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              :: d
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              :: s1     !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer  :: ddb
    integer  :: ic
    integer  :: icxy
    integer  :: iend
    integer  :: istart
    integer  :: mf
    integer  :: ml
    integer  :: n
    integer  :: nm
    integer  :: nmd
    integer  :: nmf
    integer  :: nmfu
    integer  :: nmfuu
    integer  :: nml
    integer  :: nmld
    integer  :: nmldd
    integer  :: nmlu
    integer  :: nmu
    real(fp) :: fac
!
!! executable statements -------------------------------------------------------
!
    !
    icxy = max(icx, icy)
    ddb  = gdp%d%ddbound
    !
    do ic = 1, norow
       n     = irocol(1, ic)
       mf    = irocol(2, ic) - 1
       ml    = irocol(3, ic)
       nmf   = (n + ddb)*icy + (mf + ddb)*icx - icxy
       nmfu  = nmf + icx
       nmfuu = nmf + 2*icx
       nml   = (n + ddb)*icy + (ml + ddb)*icx - icxy
       nmlu  = nml + icx
       nmld  = nml - icx
       nmldd = nml - 2*icx
       !
       !  FORWARD SWEEP
       !
       if (kcs(nmf)==0) istart = nmfuu
       if (kcs(nmf)==2) istart = nmfu
       if (kcs(nmf)==3) istart = nmfuu
       iend = nml
       if (kcs(nmlu)==2) iend = nmlu
       do nm = istart, iend, icx
          if (kcs(nm)>0) then
             nmd   = nm - icx
             fac   = a(nm)/b(nmd)
             a(nm) = -fac*a(nmd)
             b(nm) = b(nm) - fac*c(nmd)
             d(nm) = d(nm) - fac*d(nmd)
          endif
       enddo
       !
       !  BACKWARD SWEEP
       !
       if (kcs(nmlu)==0) iend = nmld
       if (kcs(nmlu)==2) iend = nml
       if (kcs(nmlu)==3) iend = nmldd
       istart = nmfu
       if (kcs(nmf)==2) istart = nmf
       do nm = iend, istart, -icx
          if (kcs(nm)>0) then
             fac   = c(nm)/b(nm + icx)
             nmu   = nm + icx
             a(nm) = a(nm) - fac*a(nmu)
             c(nm) = -fac*c(nmu)
             d(nm) = d(nm) - fac*d(nmu)
          endif
       enddo
    enddo
end subroutine wangp1
