subroutine dens(j         ,nmmaxj    ,nmmax     ,kmax       ,lstsci    , &
              & lsal      ,ltem      ,lsed      ,kcs        ,saleqs    ,temeqs    , &
              & densin    ,zmodel    ,thick     ,r1         ,rho       , &
              & sumrho    ,rhowat    ,rhosol    ,ifirst_dens,gdp       )
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
!    Function: Computes water density from temperature and
!              salinity using equation of state (rhowat).
!              Computes mud-water density given sediment
!              mass concentrations (rho).
! Method used: Equation of state following Eckart, (C. Eckart,
!              The equation of state of water and sea water at
!              low temperatures and pressures, American Journal
!              of Science, april 1958) or following UNESCO
!              (UNESCO, Algorithms for computation of fundamental
!              properties of seawater, UNESCO technical papers
!              in marine science, 1983)
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
    real(fp)               , pointer :: rhow
    integer                , pointer :: idensform
!
! Global variables
!
    integer                                                              :: j      !!  Begin pointer for arrays which have been transformed into 1D arrays. Due to the shift in the 2nd (M-)index, J = -2*NMAX + 1
    integer, intent(in)                                                  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)                                                  :: lsal   !  Description and declaration in dimens.igs
    integer, intent(in)                                                  :: lsed   !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)                                                  :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)                                                  :: ltem   !  Description and declaration in dimens.igs
    integer, intent(in)                                                  :: nmmax  !  Description and declaration in dimens.igs
    integer                                                              :: nmmaxj !  Description and declaration in dimens.igs
    integer, intent(in)                                                  :: ifirst_dens 
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: kcs    !  Description and declaration in esm_alloc_int.f90
    logical, intent(in)                                                  :: densin !  Description and declaration in morpar.igs
    logical, intent(in)                                                  :: zmodel !  Description and declaration in procs.igs
    real(fp), intent(in)                                                 :: saleqs !  Description and declaration in tricom.igs
    real(fp), intent(in)                                                 :: temeqs !  Description and declaration in tricom.igs
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                     :: rho    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                     :: rhowat !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                     :: sumrho !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), intent(in) :: r1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                               , intent(in) :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(lsed)                               , intent(in) :: rhosol !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer  :: k
    integer  :: l
    integer  :: ll
    integer  :: lst
    integer  :: nm
    integer  :: nm_pos ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
    real(fp) :: dummy
    real(fp) :: sal
    real(fp) :: temp

!
!! executable statements -------------------------------------------------------
!
    ! Notice:
    ! Avoid a select case statement inside a double do-loop
    !
    rhow      => gdp%gdphysco%rhow
    idensform => gdp%gdphysco%idensform
    nm_pos    =  1
    !
    if (ifirst_dens == 1) then
       !
       ! Initialization of the density arrays for all nm points
       ! The computations in this subroutine are only performed for kcs > 0
       !
       do nm = 1, nmmax
          do k = 1, kmax
             rhowat(nm,k) = rhow
             rho(nm,k) =  rhowat(nm,k)
          enddo
          sumrho(nm,1) = 0.5_fp*thick(1)*rho(nm,1)
          do k = 2, kmax
             sumrho(nm,k) = sumrho(nm,k-1) + 0.5_fp*(thick(k)*rho(nm, k) + thick(k-1)*rho(nm,k-1))
          enddo
       enddo
    endif
    !
    !
    ! COMPUTE DENSITIES AFTER SALINITY OR TEMPERATURE-COMPUTATION
    !
    do k = 1, kmax
       do nm = 1, nmmax
          if (lsal/=0 .or. ltem/=0)then
             !
             ! Either salinity or temperature is varying
             !
             temp = temeqs
             sal  = saleqs
             if (ltem/=0) temp = r1(nm,k,ltem)
             if (lsal/=0) sal  = r1(nm,k,lsal)

             select case (idensform)
             case( dens_Eckart )
                call dens_eck (temp, sal, rhowat(nm,k), dummy, dummy )
             case( dens_UNESCO )
                call dens_unes(temp, sal, rhowat(nm,k), dummy, dummy )
             case( dens_NaClSol)
                call dens_nacl(temp, sal, rhowat(nm,k), dummy, dummy )
             end select
          else
             !
             ! CONSTANT DENSITY
             !
             rhowat(nm, k) = rhow
          endif
       enddo
    enddo
    !
    ! COPY RHOWAT TO RHO
    !
    do nm = 1, nmmax
       if (kcs(nm) <= 0) cycle
       do k = 1, kmax
          rho(nm,k) = rhowat(nm,k)
       enddo
    enddo
    !
    ! SEDIMENT LOAD
    !
    if (densin) then
       do nm = 1, nmmax
          if (kcs(nm) <= 0) cycle
          lst = max(lsal, ltem)
          do l = 1, lsed
             ll = lst + l
             do k = 1, kmax
                rho(nm,k) = rho(nm,k) + r1(nm,k,ll) - rhowat(nm,k)*r1(nm,k,ll)/rhosol(l)
             enddo
          enddo
       enddo
    endif
    !
    ! DETERMINE FLUID DENSITY OVER WATER COLUMN; ONLY FOR SIGMA
    !
    if (.not.zmodel) then
       do nm = 1, nmmax
          if (kcs(nm) <= 0) cycle
          sumrho(nm,1) = 0.5_fp*thick(1)*rho(nm,1)
          do k = 2, kmax
             sumrho(nm,k) = sumrho(nm,k-1) + 0.5_fp*(thick(k)*rho(nm, k) + thick(k-1)*rho(nm,k-1))
          enddo
       enddo
    endif
    !
    ! exchange with neighbours for parallel runs
    !
    call dfexchg(rho   , 1, kmax, dfloat, nm_pos, gdp)
    call dfexchg(rhowat, 1, kmax, dfloat, nm_pos, gdp)
    call dfexchg(sumrho, 1, kmax, dfloat, nm_pos, gdp)
end subroutine dens
