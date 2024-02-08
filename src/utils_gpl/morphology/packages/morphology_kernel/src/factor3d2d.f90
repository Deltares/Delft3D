subroutine factor3d2d(kmax      ,aks       ,kmaxsd    ,sig       ,thick     , &
                    & seddif    ,ws        ,bakdif    ,z0rou     ,h1        , &
                    & factor    )
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
!   Determine conversion factor between reference concentration caks at aks and
!   effective depth averaged concentration conc2d.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
!
! Arguments
!
    integer                         , intent(in)   :: kmax     ! number of layers
    integer                         , intent(out)  :: kmaxsd   ! layer above reference height
    real(fp)                        , intent(inout):: aks      ! reference height
    real(fp), dimension(kmax)       , intent(in)   :: sig      ! sigma coordinate of layer centre
    real(fp), dimension(kmax)       , intent(in)   :: thick    ! layer thickness
    real(fp), dimension(0:kmax)     , intent(in)   :: seddif   ! diffusion coefficient at layer interfaces
    real(fp), dimension(0:kmax)     , intent(in)   :: ws       ! settling velocity at layer interfaces
    real(fp)                        , intent(in)   :: bakdif   ! background diffusivity
    real(fp)                        , intent(in)   :: z0rou    ! wave enhanced bed roughness
    real(fp)                        , intent(in)   :: h1       ! water depth
    real(fp)                        , intent(out)  :: factor   ! conversion factor 3D to 2D
!
! Local variables
!
    integer           :: k
    real(fp)          :: caks   ! concentration at aks
    real(fp)          :: conc   ! concentration in layer k
    real(fp)          :: conck  ! concentration in layer kmaxsd
    real(fp)          :: diff   ! diffusion at cell interface
    real(fp)          :: dz     ! distance between layer k and previous concentration
    real(fp)          :: fact1  ! ratio of concentrations
    real(fp)          :: sumu   ! integral of u
    real(fp)          :: sumcu  ! integral of conc*u
    real(fp)          :: u      ! velocity in layer k
    real(fp)          :: z      ! mean z level of layer k
    real(fp)          :: zk     ! mean z level of layer kmaxsd
!
!! executable statements -------------------------------------------------------
!
    !
    ! Determine first center cell above aks (at most kmax-1)
    !
    kmaxsd = 1
    do k = kmax-1, 1, -1
       !
       ! Calculate level of lower cell interface
       !
       z = (1.0_fp + sig(k) - thick(k)/2.0_fp) * h1
       if (z >= aks) then
          kmaxsd = k
          exit
       endif
    enddo
    !
    ! Compute the conversion factor from reference concentration to depth
    ! averaged concentration. Use reference concentration of 1.0 as basis.
    !
    caks = 1.0_fp
    !
    ! The stationary settling/diffusion equation in the vertical.
    ! c * w_s + eps * dc/dz = 0
    ! is used to derive the concentrations from the reference value.
    ! This equation is discretized using a simple upwind approximation for
    ! concentration and fall velocity, and central difference for concentration
    ! gradient. This gives
    ! c = c_known / (1 + w_s*dz/eps)
    ! where dz = z - z_known with a limit in case eps goes to zero.
    !
    ! Determine concentration in kmaxsd cell.
    !
    k = kmaxsd
    !
    z      = h1*(1.0_fp + sig(kmaxsd))
    dz     = z-aks
    diff   = seddif(kmaxsd) + bakdif
    fact1  = 1.0_fp + min(dz * ws(kmaxsd) / diff, 10.0_fp)
    conc   = caks / fact1
    !
    zk     = z
    conck  = conc
    !
    u      = log(1.0_fp + z/z0rou)
    sumu   = u*thick(k)
    sumcu  = u*conc*thick(k)
    !
    ! Now work upward
    !
    do k = kmaxsd - 1, 1, -1
       !
       ! In higher layers, the sediment concentration gradually reduces.
       !
       z      = h1 * (1.0_fp + sig(k))
       diff   = max(seddif(k) + bakdif, 0.1_fp*dz*ws(k)) ! \_ lines should be switched
       dz     = h1 * (sig(k)-sig(k+1))                   ! /
       fact1  = 1.0_fp + dz * ws(k) / diff
       conc   = conc / fact1
       !
       u      = log(1.0_fp + z/z0rou)
       sumu   = sumu  + u*thick(k)
       sumcu  = sumcu + u*conc*thick(k)
    enddo
    !
    ! And then work down
    !
    conc = conck
    do k = kmaxsd + 1, kmax
       !
       ! In the near-bed layers, the sediment concentration slowly increases.
       ! Since seddif will be set to approximately 10*dz*ws in EROSED, we
       ! should use fact1 = 1+ws*dz/diff = 1.1 here. However, we will use 1.0
       ! for the time being since this is in line with the old code.
       !
       z      = h1 * (1.0_fp + sig(k))
       fact1  = 1.0_fp ! 1.1_fp seems to be more consistent
       conc   = conc * fact1
       !
       u      = log(1.0_fp + z/z0rou)
       sumu   = sumu  + u*thick(k)
       sumcu  = sumcu + u*conc*thick(k)
    enddo
    !
    factor = sumcu/max(sumu, 1.0e-6_fp)
end subroutine factor3d2d
