subroutine inicdw(lundia    ,nmax      ,mmax      ,nmaxus    ,kmax      , &
                & kspu      ,kspv      ,dpu       ,dpv       , &
                & porosu    ,porosv    ,cdwztu    ,cdwzbu    ,cdwztv    , &
                & cdwzbv    ,gdp       )
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
! Function:
! Computes z-coordinates of the fixed gates referenced from the bottom using
! DPU and DPV
!
! Method used:
! Convert the reference level and definition of the gate co-ordinates 
! from: cartesian + MSL to depth referenced from the water surface
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
    logical , pointer :: struct
    logical , pointer :: cdwstruct
!
! Global variables
!
    integer                                                             , intent(in)  :: kmax
    integer                                                             , intent(in)  :: lundia
    integer                                                             , intent(in)  :: mmax
    integer                                                             , intent(in)  :: nmax
    integer                                                             , intent(in)  :: nmaxus
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax), intent(in)  :: kspu
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax), intent(in)  :: kspv
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)                :: porosu
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)                :: porosv
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                      :: dpu
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                      :: dpv
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                      :: cdwztu
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                      :: cdwzbu
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                      :: cdwztv
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                      :: cdwzbv
!
! Local variables
!
    integer :: ddb
    integer :: k
    integer :: m   ! Help var. do loop MMAX
    integer :: n   ! Help var. do loop NMAX
!
!! executable statements -------------------------------------------------------
!
    struct     => gdp%gdprocs%struct
    cdwstruct  => gdp%gdprocs%cdwstruct
!
ddb = gdp%d%ddbound
do n = 1 - ddb, nmax
   do m = 1 - ddb, mmax
      do k=1,kmax
         porosu(n,m,k) = 1.0
         porosv(n,m,k) = 1.0
      enddo
   enddo
enddo
!
if (cdwstruct) then
   do n = 1 - ddb, nmax
      do m = 1 - ddb, mmax
         if (abs(kspu(n,m,0))==10) then
            cdwztu(n, m) = cdwztu(n, m) + dpu(n, m)
            cdwzbu(n, m) = cdwzbu(n, m) + dpu(n, m)
         endif
         if (abs(kspv(n,m,0))==10) then
            cdwztv(n, m) = cdwztv(n, m) + dpv(n, m)
            cdwzbv(n, m) = cdwzbv(n, m) + dpv(n, m)
         endif
      enddo
   enddo
endif
end subroutine inicdw
