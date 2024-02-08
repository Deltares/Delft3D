subroutine calseddf2004(ustarc    ,ws        ,tp        ,hrms      ,h1        , &
                      & seddif    ,kmax      ,sig       ,thick     ,dicww     , &
                      & tauwav    ,tauc      ,ltur      ,delw      ,rhowat    , &
                      & uwbih     ,aks       ,caks      ,caks_ss3d ,deltas    , &
                      & aks_ss3d  ,d50       ,salinity  ,ws0       ,psi       , &
                      & epspar    ,eps       ,vonkar    ,salmax    ,wave      , &
                      & epsmax    ,epsmxc    )
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
! Compute sediment diffusion coefficient
! Van Rijn (2004)
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use sediment_basics_module
    !
    implicit none
!
! Arguments
!
    integer                    , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                    , intent(in)  :: ltur   !  Description and declaration in esm_alloc_int.f90
    real(fp)                   , intent(in)  :: psi
    real(fp)                   , intent(in)  :: aks
    real(fp)                   , intent(out) :: aks_ss3d
    real(fp)                   , intent(in)  :: caks
    real(fp)                   , intent(out) :: caks_ss3d
    real(fp)                   , intent(in)  :: d50
    real(fp)                   , intent(in)  :: delw
    real(fp)                   , intent(out) :: deltas
    real(fp)                   , intent(in)  :: h1
    real(fp)                   , intent(in)  :: hrms   !  Description and declaration in esm_alloc_real.f90
    real(fp)                   , intent(in)  :: rhowat !  Description and declaration in esm_alloc_real.f90
    real(fp)                   , intent(in)  :: salinity
    real(fp)                   , intent(in)  :: salmax
    real(fp)                   , intent(in)  :: tauc
    real(fp)                   , intent(in)  :: tauwav
    real(fp)                   , intent(in)  :: tp     !  Description and declaration in esm_alloc_real.f90
    real(fp)                   , intent(in)  :: ustarc
    real(fp)                   , intent(in)  :: uwbih
    real(fp)                   , intent(in)  :: ws0
    real(fp), dimension(0:kmax), intent(in)  :: dicww  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax), intent(out) :: seddif !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax), intent(in)  :: ws     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)  , intent(in)  :: sig    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)  , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp)                   , intent(in)  :: eps
    real(fp)                   , intent(in)  :: vonkar
    logical                    , intent(in)  :: epspar
    logical                    , intent(in)  :: wave
    real(fp)                   , intent(out) :: epsmax
    real(fp)                   , intent(out) :: epsmxc
!
! Local variables
!
    integer                     :: i
    real(fp)                    :: betaw
    real(fp)                    :: c
    real(fp)                    :: cnew
    real(fp)                    :: cmax
    real(fp)                    :: cmaxs
    real(fp)                    :: efloc
    real(fp)                    :: epsbed
    real(fp)                    :: epscur
    real(fp)                    :: epstot
    real(fp)                    :: epswav
    real(fp)                    :: fch2
    real(fp)                    :: fdamp
    real(fp)                    :: ffloc
    real(fp)                    :: hinset
    real(fp)                    :: fhulp
    real(fp)                    :: gambr
    real(fp)                    :: ff
    real(fp)                    :: fi
    real(fp)                    :: hs
    real(fp)                    :: ustw
    real(fp)                    :: z
    real(fp)                    :: zkmax
    real(fp)                    :: znew
    logical                     :: difvr
!
!! executable statements -------------------------------------------------------
!
    if (tp>0.0_fp .and. wave) then
       hs = hrms * sqrt(2.0_fp)
       if (hs/h1 > 0.4_fp) then
          gambr = 1.0_fp + sqrt((hs/h1)-0.4_fp)
       else
          gambr = 1.0_fp
       endif
       deltas = 2.0_fp*gambr*delw
       deltas = min(max(0.05_fp, deltas), 0.2_fp)
       ustw   = sqrt(tauwav/rhowat)
       betaw  = min(1.0_fp+2.0_fp*(ws(1)/max(1.0e-5_fp , ustw))**2, 1.5_fp)
       !
       ! epsbed is now based on isobe-horikawa i.s.o. uwb
       ! including a damping factor as a function of the mobility parameter
       !
       fdamp  = min(max(0.1_fp , sqrt(250.0_fp/psi)), 1.0_fp)
       epsbed = 0.018_fp * fdamp * betaw * gambr * deltas * uwbih
       epsmax = 0.035_fp * gambr * h1 * hs / tp
       epsmax = min(max(epsbed , epsmax), 0.05_fp)
    else
       deltas = 0.05_fp
       epsbed = 0.0_fp
       epsmax = 0.0_fp
    endif
    !
    ! calculate vertical sediment diffusion coefficient
    !
    difvr = epspar .and. wave
    call calseddf1993(ustarc    ,ws        ,h1        ,kmax      ,sig       , &
                    & thick     ,dicww     ,tauwav    ,tauc      ,ltur      , &
                    & eps       ,vonkar    ,difvr     ,deltas    ,epsbed    , &
                    & epsmax    ,epsmxc    ,seddif    )
    !
    ! Determine height cell centre kmax
    !
    zkmax = (1.0_fp+sig(kmax)) * h1
    if (zkmax>aks .and. caks>1.0e-5_fp) then
       !
       ! If aks is lower than cell centre of kmax layer, compute new reference
       ! concentration in kmax centre by working upward from aks. This new
       ! reference concentration will be used in the source and sink terms
       ! but it will not be used in determining the suspended load correction
       ! in bott3d; for the latter the original reference height is used.
       !
       cmaxs = 0.65_fp
       cmax  = min(max(0.05_fp, (d50/dsand)*cmaxs) , cmaxs)
       ffloc = 1.0_fp
       fch2  = min(max(0.3_fp, d50/(1.5_fp*dsand)) , 1.0_fp)
       !
       c = caks
       z = aks
       do i = 1, 20
          !
          ! epstot: diffusion term
          ! note: z will always be less than or equal to h1/2
          !
          if (z > deltas) then
             epswav = epsbed + (epsmax-epsbed) * ((z-deltas)/(0.5_fp*h1-deltas))
          else
             epswav = epsbed
          endif
          epscur = epsmxc * (1.0_fp-(1.0_fp-2.0_fp*z/h1)**2)
          epstot = sqrt(epswav**2 + epscur**2)
          !
          ! fi : damping term
          !
          fi  = max(0.01_fp, fch2*(1.0_fp+((c/cmaxs)**0.8_fp)-2.0_fp*((c/cmaxs)**0.4_fp)))
          !
          ! hinset : hindered settling term
          ! note: the effect of other sediment fractions is missing, compare FALLVE
          !
          hinset = max(0.01_fp, sqrt(1.0_fp-0.65_fp*c/cmax))
          !
          ! ffloc : flocculation term 
          ! note: the effect of other sediment fractions is missing, compare FALLVE
          !
          if (d50 < dsand .and. salinity>0.01_fp .and. salmax>0.0_fp) then
             fhulp = max(1.0_fp, 4.0_fp+log10(2.0_fp*c/cmax))
             efloc = min(max(0.0_fp, dsand/d50-1.0_fp) , 3.0_fp)
             ffloc = min(max(1.0_fp, fhulp**efloc) , 10.0_fp)
             ffloc = (ffloc-1.0_fp) * min(1.0_fp , salinity/salmax) + 1.0_fp
          endif
          ff   = -ws0 * hinset * ffloc / (epstot * fi)
          !
          znew = min(aks*(zkmax/aks)**(real(i,fp)/20.0_fp), zkmax)
          cnew = c * exp((znew-z)*ff)
          !
          z    = znew
          c    = max(1.0e-6_fp, cnew)
       enddo      
       aks_ss3d  = zkmax
       caks_ss3d = c
    endif
end subroutine calseddf2004
