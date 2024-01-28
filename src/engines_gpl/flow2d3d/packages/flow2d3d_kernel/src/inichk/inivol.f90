subroutine inivol(j         ,nmmaxj    ,nmmax     ,kmax      ,zmodel    , &
                & kcs       ,kcu       ,kcv       ,kfsmin    ,kfsmax    , &
                & kfumin    ,kfumax    ,kfvmin    ,kfvmax    ,thick     , &
                & s1        ,dps       ,gsqs      ,guu       ,gvv       , &
                & hu        ,hv        ,dzs1      ,dzu1      ,dzv1      , &
                & volum1    ,porosu    ,porosv    ,areau     ,areav     ,gdp       )
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
! Computes volumes and areas from depth values at water level
! points, water levels, transformation coefficients
! GUU and GVV; adapted for use in ZMODEL as well
!
! Needed to maintain compatibility with Delft3D-WAQ
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
!
! Global variables
!
    integer                                                   :: j
    integer                                     , intent(in)  :: kmax
    integer                                     , intent(in)  :: nmmax
    integer                                                   :: nmmaxj
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)   , intent(in)  :: kcs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)   , intent(in)  :: kcu
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)   , intent(in)  :: kcv
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)   , intent(in)  :: kfsmax
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)   , intent(in)  :: kfsmin
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)   , intent(in)  :: kfumax
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)   , intent(in)  :: kfumin
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)   , intent(in)  :: kfvmax
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)   , intent(in)  :: kfvmin
    logical                                     , intent(in)  :: zmodel
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: dps
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: gsqs
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: guu
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: gvv
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: hu
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: hv
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: s1
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dzs1
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dzu1
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dzv1
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: areau
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: areav
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: porosu
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: porosv
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(out) :: volum1
    real(fp), dimension(kmax)                       , intent(in)  :: thick
!
! Local variables
!
    integer :: k
    integer :: nm
!
!! executable statements -------------------------------------------------------
!
if (.not.zmodel) then
   do k = 1, kmax
      do nm = 1, nmmax
         if (kcs(nm)/=0) then
            volum1(nm, k) = thick(k)*(s1(nm) + real(dps(nm),fp))*gsqs(nm)
         endif
         if (kcu(nm)/=0) then
            areau(nm, k) = thick(k)*hu(nm)*guu(nm)*porosu(nm, k)
         endif
         if (kcv(nm)/=0) then
            areav(nm, k) = thick(k)*hv(nm)*gvv(nm)*porosv(nm, k)
         endif
      enddo
   enddo
else
   do nm = 1, nmmax
      if (kcs(nm)/=0) then
         do k = 1, kmax
            volum1(nm, k) = dzs1(nm, k)*gsqs(nm)
         enddo
      endif
      if (kcu(nm)/=0) then
         do k = 1, kmax
            areau(nm, k) = dzu1(nm, k)*guu(nm)*porosu(nm, k)
         enddo
      endif
      if (kcv(nm)/=0) then
         do k = 1, kmax
            areav(nm, k) = dzv1(nm, k)*gvv(nm)*porosv(nm, k)
         enddo
      endif
   enddo
endif
end subroutine inivol
