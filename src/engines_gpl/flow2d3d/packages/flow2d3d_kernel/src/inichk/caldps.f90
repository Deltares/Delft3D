subroutine caldps(nmmax     ,nfltyp    ,icx       , &
                & icy       ,kcs       ,dpd       ,dps       ,gdp       )
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
!    Function: - Initiates the depth values at water level points
!                which depends on the drying and flooding
!                procedure (In UI only NO and MAX procedure
!                allowed)
!              - Initializes nfltyp (used by MOR-transport)
! Method used: DPSOPT = MEAN-> .25 * (  dpd(nm  ) + dpd(nmd )
!                                     + dpd(ndmd) + dpd(ndm ) )
!                              nfltyp = 1
!
!              DPSOPT = MAX -> MAX   ( dpd(nm  ) , dpd(nmd ),
!                                      dpd(ndmd) , dpd(ndm ) )
!                              nfltyp = 2
!
!              DPSOPT = MIN -> .5  * (  MIN (dpd(nm  ), dpd(ndmd))
!                                     + MIN (dpd(nmd ), dpd(ndm )) )
!                              nfltyp = 3
!
!              DPSOPT = DP  -> dpd(nm)
!                              nfltyp = 4
!                              dpd = .25 * (  dps(numu) + dps(num)
!                                           + dps(nmu ) + dps(nm ) )
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
    character(8)   , pointer :: dpsopt
    logical        , pointer :: rst_dp
!
! Global variables
!
    integer                                     , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                     , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                     , intent(out) :: nfltyp !  Description and declaration in esm_alloc_int.f90
    integer                                     , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              :: kcs    !  Description and declaration in esm_alloc_int.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              :: dpd    !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub), intent(out) :: dps    !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer           :: kcddd
    integer           :: kcddu
    integer           :: kcdud
    integer           :: kcduu
    integer           :: ndm    ! Help var. NM-ICY 
    integer           :: ndmd   ! Help var. NDM-ICX 
    integer           :: nm     ! Help var. loops 1,NMMAX and J,NMMAXJ 
    integer           :: nmd
    integer           :: nmu    ! Help var. NM+ICX 
    integer           :: num    ! Help var. NM+ICY 
    integer           :: numu
    real(fp)          :: fac1
    real(fp)          :: fac2
    real(fp)          :: fac3
    real(fp)          :: fac4
    real(fp)          :: dep
    real(fp)          :: fact
    real(fp)          :: rmissval
    integer           :: nm_pos ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
!
!! executable statements -------------------------------------------------------
!
    dpsopt             => gdp%gdnumeco%dpsopt
    rst_dp             => gdp%gdrestart%rst_dp
    !
    rmissval = -999.0
    nm_pos   = 1
    !
    if (dpsopt=='MEAN') then
       nfltyp = 1
       !
       ! calculate DPS depth in zeta point as the mean of 4 surrounding
       ! depth points
       ! -ICX := -1 in M-direction, -ICY := -1 in N-direction
       ! there will always at least one combination of neighbours 1
       !
       do nm = 1, nmmax
          nmd  = nm - icx
          ndm  = nm - icy
          ndmd = ndm - icx
          if (kcs(nm)==1) then
             dps(nm) = real(0.25*(dpd(nm) + dpd(nmd) + dpd(ndmd) + dpd(ndm)),prec)
          elseif (kcs(nm)==2) then
             nmu = nm + icx
             num = nm + icy
             kcduu = 0
             if (kcs(num)==1 .or. kcs(nmu)==1) kcduu = 1
             kcdud = 0
             if (kcs(num)==1 .or. kcs(nmd)==1) kcdud = 1
             kcddd = 0
             if (kcs(ndm)==1 .or. kcs(nmd)==1) kcddd = 1
             kcddu = 0
             if (kcs(ndm)==1 .or. kcs(nmu)==1) kcddu = 1
             fact = 1.0_fp/real(kcduu + kcdud + kcddd + kcddu,fp)
             dps(nm) = real(fact*(kcduu*dpd(nm) + kcdud*dpd(nmd) + kcddd*dpd(ndmd)      &
                     & + kcddu*dpd(ndm)),prec)
          else
          endif
       enddo
    elseif (dpsopt=='MAX') then
       nfltyp = 2
       !
       ! calculate DPS depth in zeta point as maximum of 4 depth points
       ! NOTE: that the contents of KFS are here identical to KCS
       !       (except for boundary points)
       ! -ICX := -1 in M-direction, -ICY := -1 in N-direction
       !
       do nm = 1, nmmax
          nmd  = nm - icx
          ndm  = nm - icy
          ndmd = ndm - icx
          if (kcs(nm)==1) then
             dps(nm) = real(max(dpd(nm), dpd(nmd), dpd(ndmd), dpd(ndm)),prec)
          elseif (kcs(nm)==2) then
             nmu = nm + icx
             num = nm + icy
             dep = -1.0e+32
             if (kcs(nmu)==1) dep = max(dep, dpd(ndm), dpd(nm))
             if (kcs(num)==1) dep = max(dep, dpd(nmd), dpd(nm))
             if (kcs(nmd)==1) dep = max(dep, dpd(nmd), dpd(ndmd))
             if (kcs(ndm)==1) dep = max(dep, dpd(ndm), dpd(ndmd))
             dps(nm) = real(dep,prec)
          else
          endif
       enddo
    elseif (dpsopt=='MIN') then
       nfltyp = 3
       !
       ! calculate DPS depth in zeta point as mean of 2 minima of 2
       ! surrounding depth points
       ! NOTE: that the contents of KFS are here identical to KCS
       !       (except for boundary points)
       ! -ICX := -1 in M-direction, -ICY := -1 in N-direction
       !
       do nm = 1, nmmax
          nmd  = nm - icx
          ndm  = nm - icy
          ndmd = ndm - icx
          if (kcs(nm)==1) then
             dps(nm) = real(0.5*(min(dpd(nm), dpd(ndmd)) + min(dpd(nmd), dpd(ndm))),prec)
          elseif (kcs(nm)==2) then
             nmu = nm + icx
             num = nm + icy
             dep = 1.0e+32
             if (kcs(nmu)==1) dep = min(dep, dpd(ndm), dpd(nm))
             if (kcs(num)==1) dep = min(dep, dpd(nmd), dpd(nm))
             if (kcs(nmd)==1) dep = min(dep, dpd(nmd), dpd(ndmd))
             if (kcs(ndm)==1) dep = min(dep, dpd(ndm), dpd(ndmd))
             dps(nm) = real(dep,prec)
          else
          endif
       enddo
    elseif (dpsopt=='DP') then
       !
       ! nfltyp may not be zero
       ! if it is zero, the check on negative water depths is disabled
       !
       nfltyp = 4
       !
       ! Set DPS depth in zeta point = DPD (INDIA)
       ! NOTE: that the contents of KFS are here identical to KCS
       !       (except for boundary points)
       ! -ICX := -1 in M-direction, -ICY := -1 in N-direction
       !
       do nm = 1, nmmax
          nmd  = nm - icx
          ndm  = nm - icy
          ndmd = ndm - icx
          if (kcs(nm)==1 .or. rst_dp .or. comparereal(real(dpd(nm),fp),rmissval)/=0) then
             dps(nm) = real(dpd(nm),prec)
          elseif (kcs(nm)==2) then
             nmu = nm + icx
             num = nm + icy
             if (kcs(nmu)==1) dep = dpd(nmu)
             if (kcs(num)==1) dep = dpd(num)
             if (kcs(nmd)==1) dep = dpd(nmd)
             if (kcs(ndm)==1) dep = dpd(ndm)
             dps(nm) = real(dep,prec)
          else
          endif
       enddo
       !
       ! Compute DPD from DPS
       !
       do nm = 1, nmmax
          nmu  = nm  + icx
          num  = nm  + icy
          numu = nmu + icy
          fac1=0.0_fp
          fac2=0.0_fp
          fac3=0.0_fp
          fac4=0.0_fp
          if (kcs(numu)==1 .and. comparereal(real(dps(numu),fp),rmissval)/=0) fac1=1.0_fp
          if (kcs(nmu) ==1 .and. comparereal(real(dps(nmu ),fp),rmissval)/=0) fac2=1.0_fp
          if (kcs(nm)  ==1 .and. comparereal(real(dps(nm  ),fp),rmissval)/=0) fac3=1.0_fp
          if (kcs(num) ==1 .and. comparereal(real(dps(num ),fp),rmissval)/=0) fac4=1.0_fp
          fact = fac1 + fac2 + fac3 + fac4
          if (comparereal(real(fact,fp),0.0_fp) == 1) then
             dpd(nm) = (  fac1*real(dps(numu),fp) &
                        + fac2*real(dps(nmu ),fp) &
                        + fac3*real(dps(nm  ),fp) &
                        + fac4*real(dps(num ),fp)  ) / fact
          endif
       enddo
    else
    endif
    !
    ! exchange depths with neighbours for parallel runs
    !
    call dfexchg ( dpd, 1, 1, dfloat, nm_pos, gdp )
    call dfexchg ( dps, 1, 1, dfprec, nm_pos, gdp )
    !
    ! Adapt kcs: if dps = missval, kcs = 0
    ! Note: do not adapt in halo area in case of parallel runs, i.e. kcs = -1
    !
    do nm = 1, nmmax
       if ( kcs(nm) /= -1 .and. comparereal(real(dps(nm),fp),rmissval) == 0 ) then
          kcs(nm) = 0
       endif
    enddo
end subroutine caldps
