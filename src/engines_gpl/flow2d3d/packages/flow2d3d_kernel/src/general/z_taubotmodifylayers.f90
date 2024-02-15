subroutine z_taubotmodifylayers(nmmax  ,kmax     ,lstsci       ,icx     ,icy          , & 
                              & kfs    ,kfsmin   ,kfsmax       ,dps     ,dzs1         , &
                              & kfu    ,kfumin   ,kfumax       ,dpu     ,dzu1         , &
                              & kfv    ,kfvmin   ,kfvmax       ,dpv     ,dzv1         , &
                              & r0     ,s0       ,s1           ,zk      ,modify_dzsuv , &
                              & dtsec  ,gsqs     ,kfsmx0       ,qzk     ,umean        , &
                              & vmean  ,dzs0     ,ztbml_upd_r1 ,gdp      )
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
!  $Id: z_taubotmodifylayers.f90 1246 2012-02-09 11:25:49Z platzek $
!  : https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20111115_13532_z-model_improvements_oss-merge/engines_gpl/flow2d3d/packages/kernel/src/general/z_taubotmodifylayers.f90 $
!!--description-----------------------------------------------------------------
!
! Computes inproduct between two vectors.
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
    integer                                         , intent(in) :: icx          !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in) :: icy          !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in) :: kmax         !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in) :: nmmax
    integer                                         , intent(in) :: lstsci
    integer , dimension(3)                                       :: modify_dzsuv
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfs          !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfu          !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfv          !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfsmin       !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfumin       !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfvmin       !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfsmx0       !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfsmax       !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfumax       !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfvmax       !  Description and declaration in esm_alloc_int.f90
    real(fp)                                        , intent(in) :: dtsec        
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: gsqs         !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)    , intent(in) :: dps          !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                   :: dpu          !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                   :: dpv          !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                   :: s0           !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                   :: s1           !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                   :: umean        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                   :: vmean        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax)                                  :: zk           !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: dzs0         !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: dzs1         !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: dzu1         !  Description and declaration in esm_alloc_real.f90 
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: dzv1         !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)     :: r0           !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)           :: qzk          !  Description and declaration in esm_alloc_real.f90
    logical                                                      :: ztbml_upd_r1 ! Determines whether the concentrations in the 2 bottom layers
                                                                                 ! need to be updated or not
!
! Local variables
!
    integer  :: k
    integer  :: l
    integer  :: nm
    real(fp) :: dzh
!
!! executable statements -------------------------------------------------------
!
    !
    ! Modification of near bed layer thicknesses to obtain 
    ! a smoother approximation of the bed shear stress,
    !
    ! Modification of layer thickness in cell centres
    !     (including mass conserving approach for constituents)
    ! Modification of layer thickness in velocity points
    !     (is additionally done in subroutine z_checku in case of flooding)
    !
    ! First ensure conservation of constituents for the modified layering
    !
    if (modify_dzsuv(1)==1) then
       !
       ! First ensure conservation of constituents for the modified layering
       !
       do nm = 1, nmmax
          !
          ! Only in case of more than 1 layer
          ! 
          if (kfs(nm)==1 .and. kfsmax(nm)>kfsmin(nm)) then
             k = kfsmin(nm)
             if (ztbml_upd_r1 .or. dzs1(nm,k)+dzs1(nm,k+1)/=dzs0(nm,k)+dzs0(nm,k+1)) then
                if (dzs1(nm,k) < dzs1(nm,k+1)) then
                   do l = 1, lstsci
                      r0(nm,k,l) = (        r0(nm,k+1,l)*(dzs1(nm,k+1)-dzs1(nm,k)) +     &
                                 &   2.0_fp*r0(nm,k  ,l)* dzs1(nm,k)                 ) / &
                                 & (dzs1(nm,k+1) + dzs1(nm,k))
                   enddo
                elseif (dzs1(nm,k) > dzs1(nm,k+1)) then
                   do l = 1, lstsci
                      r0(nm,k+1,l) = (        r0(nm,k  ,l)*(dzs1(nm,k)-dzs1(nm,k+1)) +     &
                                   &   2.0_fp*r0(nm,k+1,l)* dzs1(nm,k+1)                 ) / &
                                   & (dzs1(nm,k+1) + dzs1(nm,k))
                   enddo
                endif
             endif
             !
             ! Take the movement of the free surface into account if it is or was located 
             ! in the second layer from the bottom.
             ! Specifically if the water level crosses the interface between layers 
             ! kfsmin(nm)+1 and kfsmin(nm)+2, the modification is adapted.
             ! These effects are taken into account via a balancing flux qzk(nm,kfsmin(nm))
             ! across the interface between layers kfsmin(nm) and kfsmin(nm)+1
             !
             if (ztbml_upd_r1 .or. dzs1(nm,k)+dzs1(nm,k+1)/=dzs0(nm,k)+dzs0(nm,k+1)) then
                if (kfsmax(nm) == k+1) then
                   if (kfsmx0(nm) > k+1) then
                      qzk(nm,k) = qzk(nm,k) - 0.5_fp*gsqs(nm)*(s1(nm)-zk(k+1))/dtsec
                   else
                      qzk(nm,k) = qzk(nm,k) - 0.5_fp*gsqs(nm)*(s1(nm)-s0(nm))/dtsec
                   endif
                elseif (kfsmx0(nm) == k+1) then
                   if (kfsmax(nm) > k+1) then
                      qzk(nm,k) = qzk(nm,k) + 0.5_fp*gsqs(nm)*(s0(nm)-zk(k+1))/dtsec
                   else
                      qzk(nm,k) = qzk(nm,k) - 0.5_fp*gsqs(nm)*(s1(nm)-s0(nm))/dtsec
                   endif 
                endif
             endif
             !
             ! Now the actual layer remapping to an equidistant distribution of the 
             ! two near-bed layers
             !
             dzh           = 0.5_fp*(dzs1(nm, k) + dzs1(nm, k+1))
             dzs1(nm, k  ) = dzh
             dzs1(nm, k+1) = dzs1(nm,k)
          endif
       enddo
    endif
    !
    ! Modification of layer thickness in U-velocity points
    !
    if (modify_dzsuv(2)==1) then
       do nm = 1, nmmax
          if (kfu(nm)==1 .and. kfumax(nm)>kfumin(nm)) then
             k       = kfumin(nm)
             dzu1(nm, k  ) = 0.5_fp*(dzu1(nm,k)+dzu1(nm,k+1))
             dzu1(nm, k+1) = dzu1(nm,k)
          endif
       enddo
    endif
    !
    ! Modification of layer thickness in V-velocity points
    !
    if (modify_dzsuv(3)==1) then
       do nm = 1, nmmax
          if (kfv(nm)==1 .and. kfvmax(nm)>kfvmin(nm)) then
             k       = kfvmin(nm)
             dzv1(nm, k  ) = 0.5_fp*(dzv1(nm,k)+dzv1(nm,k+1))
             dzv1(nm, k+1) = dzv1(nm,k)
          endif
       enddo
    endif
end subroutine z_taubotmodifylayers
