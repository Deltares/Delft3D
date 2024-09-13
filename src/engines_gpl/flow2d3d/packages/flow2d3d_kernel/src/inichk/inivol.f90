subroutine inivol(j         ,nmmaxj    ,nmmax     ,kmax      ,zmodel    , &
                & kcs       ,kcu       ,kcv       ,kfsmin    ,kfsmax    , &
                & kfumin    ,kfumax    ,kfvmin    ,kfvmax    ,thick     , &
                & s1        ,dps       ,gsqs      ,guu       ,gvv       , &
                & hu        ,hv        ,dzs1      ,dzu1      ,dzv1      , &
                & volum1    ,porosu    ,porosv    ,areau     ,areav     , &
                & aguu      ,agvv      ,agsqs     ,kfs       ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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
!  $Id: inivol.f90 6033 2016-04-19 08:23:40Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/inichk/inivol.f90 $
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
    integer                , pointer :: dim_nmlist
    integer, dimension(:,:), pointer :: NMlistMERGED_d
    integer, dimension(:)  , pointer :: Nmerged_d
    logical                , pointer :: virtualLINK
    real(fp)               , pointer :: thresMERGE_d
    real(fp), dimension(:) , pointer :: agsqs_link
    integer                , pointer :: cutcell
!
! Global variables
!
    integer                                                   :: j
    integer                                     , intent(in)  :: kmax
    integer                                     , intent(in)  :: nmmax
    integer                                                   :: nmmaxj
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)   , intent(in)  :: kfs
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
!   cutcells:
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: aguu      !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: agvv      !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: agsqs
!
! Local variables
!
    integer :: k
    integer :: nm
    integer :: idummy
!
!! executable statements -------------------------------------------------------
!
    dim_nmlist     => gdp%gdimbound%dim_nmlist
    NMlistMERGED_d => gdp%gdimbound%NMlistMERGED_d
    Nmerged_d      => gdp%gdimbound%Nmerged_d
    virtualLINK    => gdp%gdimbound%virtualLINK
    thresMERGE_d   => gdp%gdimbound%thresMERGE_d
    agsqs_link     => gdp%gdimbound%agsqs_link
    cutcell        => gdp%gdimbound%cutcell
agsqs_link(gdp%d%nmlb:gdp%d%nmub) = agsqs(gdp%d%nmlb:gdp%d%nmub) 
if (cutcell==2.and.virtualLINK) then
   call VIRTUALlinkAREAS(kfs,agsqs_link,agsqs,gsqs,NMlistMERGED_d,Nmerged_d,thresMERGE_d,idummy,idummy,gdp%d%nmmax,gdp%d%nmlb,gdp%d%nmub,idummy, dim_nmlist, gdp)
endif

if (.not.zmodel) then
   do k = 1, kmax
      do nm = 1, nmmax
         if (kcs(nm)/=0) then
            volum1(nm, k) = thick(k)*(s1(nm) + real(dps(nm),fp))*gsqs(nm)*agsqs_link(nm)
           ! write(898900,'(2i10,15f25.15)') kmax-kmax,nm,s1(nm),real(dps(nm),fp),agsqs(nm)
         endif
         if (kcu(nm)/=0) then
            areau(nm, k) = thick(k)*hu(nm)*aguu(nm)*guu(nm)*porosu(nm, k)
         endif
         if (kcv(nm)/=0) then
            areav(nm, k) = thick(k)*hv(nm)*agvv(nm)*gvv(nm)*porosv(nm, k)
         endif
      enddo
   enddo
else
   do nm = 1, nmmax
      if (kcs(nm)/=0) then
         do k = 1, kmax
            volum1(nm, k) = dzs1(nm, k)*gsqs(nm)*agsqs(nm)
         enddo
      endif
      if (kcu(nm)/=0) then
         do k = 1, kmax
            areau(nm, k) = dzu1(nm, k)*aguu(nm)*guu(nm)*porosu(nm, k)
         enddo
      endif
      if (kcv(nm)/=0) then
         do k = 1, kmax
            areav(nm, k) = dzv1(nm, k)*agvv(nm)*gvv(nm)*porosv(nm, k)
         enddo
      endif
   enddo
endif
end subroutine inivol
