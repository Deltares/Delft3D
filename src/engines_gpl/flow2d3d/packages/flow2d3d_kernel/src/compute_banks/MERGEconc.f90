subroutine MERGEconc(r1        ,gsqsR     ,dps        ,s1         ,thick      ,&
                     volum1    ,kfs       ,lundia     ,ddbound    ,nst        ,&
                     lstsci    ,nmlb      ,nmub       ,kmax       ,nmmax      ,&
                     lsedtot   ,nmaxddb   ,gdp)
                     
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2013.                                
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
!  $Id: MERGEconc.f90 7020 2017-02-16 21:50:45Z platzek $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute_banks/MERGEconc.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Computes the local curvature of streakline (2dh)
!              using an approximation of the chord and the angle between velocity vectors
! Method used:
!
! Author: Alberto Canestrelli
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                 , pointer :: cutcell
    integer                 , pointer :: dim_nmlist
    integer, dimension(:)   , pointer :: Nmerged_bed
    integer, dimension(:,:) , pointer :: NMlistMERGED_bed
    logical                 , pointer :: virtualMERGEupdCONC
    logical                 , pointer :: virtualLINK
    real(fp), dimension(:,:), pointer :: vol1Tk
    real(fp), dimension(:,:), pointer :: vol
    real(fp), dimension(:,:), pointer :: dh
!
! Global variables
!
    real(fp), dimension(nmlb:nmub, kmax, lstsci),intent(inout)   :: r1  
    real(fp), dimension(nmlb:nmub, kmax),intent(inout)           :: volum1
    real(fp), dimension(nmlb:nmub),intent(in)                    :: gsqsR
    real(prec), dimension(nmlb:nmub),intent(in)                  :: dps
    real(fp), dimension(nmlb:nmub),intent(in)                    :: s1
    real(fp), dimension(kmax),intent(in)                         :: thick
    integer, dimension(nmlb:nmub),intent(in)                     :: kfs
    integer, intent(in)                                          :: kmax !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)                                          :: nmmax !  Description and declaration in dimens.igs
    integer, intent(in)                                          :: nmaxddb
    integer, intent(in)                                          :: lstsci
    integer, intent(in)                                          :: lundia
    integer, intent(in)                                          :: ddbound
    integer, intent(in)                                          :: nst
    integer, intent(in)                                          :: nmlb
    integer, intent(in)                                          :: nmub
    integer, intent(in)                                          :: lsedtot
 !
!
! Local variables
!
    integer                        :: L
    integer                        :: k
    integer                        :: icx
    integer                        :: icy
    integer                        :: nm
    logical                        :: Ldummy
    integer                        :: Idummy,Idummyy,Idummyyy
    real(fp)                       :: Rdummy(kmax)
!
!! executable statements -------------------------------------------------------
!
    cutcell             => gdp%gdimbound%cutcell
    dim_nmlist          => gdp%gdimbound%dim_nmlist
    Nmerged_bed         => gdp%gdimbound%Nmerged_bed
    NMlistMERGED_bed    => gdp%gdimbound%NMlistMERGED_bed
    virtualMERGEupdCONC => gdp%gdimbound%virtualMERGEupdCONC
    virtualLINK         => gdp%gdimbound%virtualLINK
    vol1Tk              => gdp%gdimbound%Dwrkak1_T
    vol                 => gdp%gdimbound%Dwrkak1
    dh                  => gdp%gdimbound%Dwrkak2
    !
    if (cutcell.gt.0.and.virtualMERGEupdCONC.and..not.(virtualLINK)) THEN
!             
       icx = nmaxddb
       icy = 1
!
       do nm=1,nmmax
          if (kfs(nm)==1) then
             dh(nm,1:kmax) = volum1(nm,1:kmax)/gsqsR(nm)
          else
             dh(nm,1:kmax) = 1._fp !avoid division by zero
          endif
       enddo
       do l = 1, lstsci
          do nm = 1, nmmax
             !vol(nm,1:kmax) =  r1(nm,1:kmax,l)*dh(nm,1:kmax)
              vol1Tk(1:kmax,nm) =  r1(nm,1:kmax,l)*dh(nm,1:kmax)
          enddo
          !vol1Tk(1:kmax,:) = transpose(vol(:,1:kmax))
          CALL virtMERG(vol1Tk(:,:),gsqsR,s1,dps,Rdummy,icx,icy,nmmax,nmlb,nmub,nst,1,kmax,1,kmax,lundia,Ldummy,&   
                        Idummy,Idummyy,Idummyyy,0,nmaxddb,ddbound,& !1 check large bed variations
                        NMlistMERGED_bed,Nmerged_bed, dim_nmlist)
          !vol(:,1:kmax) = transpose(vol1Tk(1:kmax,:))
          do nm = 1, nmmax
             vol(nm,1:kmax)   =  vol1Tk(1:kmax,nm)
             r1 (nm,1:kmax,l) =  vol(nm,1:kmax)/dh(nm,1:kmax)
          enddo
         ! do nm=1,nmmax
         !    if (comparereal(gsqsR(nm),0._fp)>0) then
         !       CTk(1:kmax,1:nmmax) = vol1Tk(:,1:nmmax)/dh(1:nmmax,1:kmax)
         !    else
         !       dh(nm,1:kmax) = 0._fp
         !    endif
         ! enddo
         ! vol1Tk(1:kmax,:)*gsqsR(:)
          !r1(1:nmmax,1:kmax,l) =  vol(1:nmmax,1:kmax)/dh(1:nmmax,1:kmax)
          !do nm=1,nmmax
          !write(567890,'(2i9,25f25.5)') nst,nm,r1(nm,1:kmax,1)
          !enddo
       enddo
    endif
end 

