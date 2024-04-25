subroutine usrbrl(icx       ,icy       ,nmmax     ,kmax      ,kfu       , &
                & kspu      ,gvu       ,u0        ,v         ,bbk       , &
                & ubrlsu    ,diapl     ,rnpl      ,gdp       )
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
! The routine adds additional energy losses due to 3D hydraulic structures.
! The energy loss is modelled as linear or quadratic friction term which is 
! integrated implicitly. This implies adding this term in the main diagonal matrix
! element BBK of the momentum equation.  
! The following hydr. structures are implemented:
!    Weir        - KSPU(NM,0)=3,
!    Rigid sheet - KSPU(NM,0)=5 (linear),
!    Porous plate- KSPU(NM,0)=6, 
!    Bridge      - KSPU(NM,0)=7
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
    logical , pointer :: veg3d
!
! Global variables
!
    integer                                          , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX
                                                                             !!  then computation proceeds in the X-
                                                                             !!  dir. If icx=1 then computation pro-
                                                                             !!  ceeds in the Y-dir.
    integer                                          , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                          , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                          , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in)  :: kspu   !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(in)  :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: bbk    !!  Internal work array, coefficient la-
                                                                             !!  yer velocity in (N,M,K) implicit part
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax) , intent(in)  :: diapl  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax) , intent(in)  :: rnpl   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax) , intent(in)  :: u0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax) , intent(in)  :: ubrlsu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax) , intent(in)  :: v      !!  V-velocities at new/old time level
                                                                             !!  depending on the ADI-stage (calling
                                                                             !!  routines)
!
! Local variables
!
    integer :: k    ! Loop counter over KMAX 
    integer :: ksp  ! Value for structure point KFU(NM)*ABS (KSPU(NM,0))*KSPU(NM,K) 
    integer :: ndm  ! NM-ICY 
    integer :: ndmu ! NMU-ICY 
    integer :: nm   ! Loop counter over NMMAX 
    integer :: nmu  ! NM+ICX 
    real(fp):: dia
    real(fp):: fplant
    real(fp):: rn
    real(fp):: uuu  ! Total velocity in U-point NM 
    real(fp):: vvv  ! Mean value of 4 surrounding v-vel. in U-point of NM 
!
!! executable statements -------------------------------------------------------
!
    veg3d     => gdp%gdprocs%veg3d
    !
    ! either: general local weir (3D); quadratic friction (KSPU=3)
    ! or    : rigid sheet, linear friction (KSPU=5), or CDW (quadratic as well)
    !
    do k = 1, kmax
       ndm  = -icy
       nmu  = icx
       ndmu = -icy + icx
       do nm = 1, nmmax
          ndm  = ndm + 1
          ndmu = ndmu + 1
          nmu  = nmu + 1
          ksp  = kfu(nm)*abs(kspu(nm, 0))*kspu(nm, k)
          if (ksp==5) then
             bbk(nm, k) = bbk(nm, k) + ubrlsu(nm, k)/gvu(nm)
          elseif ((ksp==3) .or. (ksp==6) .or. (ksp==7)) then
             vvv = .25*(v(ndm, k) + v(ndmu, k) + v(nm, k) + v(nmu, k))
             uuu = sqrt(u0(nm, k)**2 + vvv**2)
             bbk(nm, k) = bbk(nm, k) + uuu*ubrlsu(nm, k)/gvu(nm)
          else
          endif
          if ((kfu(nm)*kspu(nm,0) == 10 .or. kfu(nm)*kspu(nm,0) == 4) &
            & .and. kspu(nm, k) == 0                                   ) then
             vvv = .25*(v(ndm, k) + v(ndmu, k) + v(nm, k) + v(nmu, k))
             uuu = sqrt(u0(nm, k)**2 + vvv**2)
             bbk(nm, k) = bbk(nm, k) + uuu*ubrlsu(nm, k)/gvu(nm)
          endif
       enddo
    enddo
    !
    ! (Rigid) 3D Vegetation Model
    !
    if (veg3d) then
       do k = 1, kmax
          ndm  = -icy
          nmu  = icx
          ndmu = -icy + icx
          do nm = 1, nmmax
             ndm  = ndm + 1
             ndmu = ndmu + 1
             nmu  = nmu + 1
             if (diapl(nm, k)/=0.0 .or. diapl(nmu, k)/=0.0) then
                vvv    = 0.25*(v(ndm, k) + v(ndmu, k) + v(nm, k) + v(nmu, k))
                uuu    = u0(nm, k)
                dia    = 0.5*(diapl(nm, k) + diapl(nmu, k))
                rn     = 0.5*(rnpl(nm, k) + rnpl(nmu, k))
                fplant = 0.5*dia*rn*sqrt(uuu*uuu + vvv*vvv)
                bbk(nm, k) = bbk(nm, k) + fplant
             endif
          enddo
       enddo
    endif
end subroutine usrbrl
