subroutine drychk_cc(kfs_cc,   poros    ,agsqs   ,s1     ,dps    ,dpL   ,dpH    ,nmmax   ,nmlb, nmub ,zmodel)
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
!  $Id: drychk_cc.f90 5868 2016-02-19 12:33:51Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute_banks/drychk_cc.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: This subroutine checks for drying in water level
!              at FLOODED cut cells
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
    !
    implicit none
!
! Global variables
!
    integer                                           , intent(in)       :: nmmax
    integer                                           , intent(in)       :: nmlb
    integer                                           , intent(in)       :: nmub
    real(prec), dimension(nmlb:nmub)                  , intent(inout)    :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp),   dimension(nmlb:nmub)                  , intent(inout)    :: s1     !  Description and declaration in esm_alloc_real.f90!
    real(fp),   dimension(nmlb:nmub)                  , intent(in)       :: dpL     !  Description and declaration in esm_alloc_real.f90!
    real(fp),   dimension(nmlb:nmub)                  , intent(in)       :: dpH     !  Description and declaration in esm_alloc_real.f90!
    real(fp),   dimension(nmlb:nmub)                  , intent(in)       :: agsqs     !  Description and declaration in esm_alloc_real.f90!
    real(fp),   dimension(nmlb:nmub)                  , intent(in)       :: poros     !  Description and declaration in esm_alloc_real.f90!
    integer ,   dimension(nmlb:nmub)                  , intent(in)       :: kfs_cc
    logical                                           , intent(in)       :: zmodel
! Local variables
!
    integer       :: lungrd
    integer       :: nm
!! executable statements -------------------------------------------------------
!
    !
    do nm = 1, nmmax

       if ( kfs_cc(nm)==1) then
          if (Zmodel) write(*,*) 'fix dryck_cc!!!'
          if ( comparereal(s1(nm),dpH(nm))>0) then
             !continue 
          elseif ( comparereal(s1(nm),dpH(nm))<=0.and.comparereal(s1(nm),dpL(nm))>=0) then
             dps(nm) = dpL(nm)
             !if (poros(nm).gt.1.e-14_fp) then
             if (comparereal(poros(nm),0._fp).eq.0) then
                write(*,*) 'Porosity should not be zero'
                !pause
                stop
             endif
             s1(nm) = -(1._fp-poros(nm))*(dpH(nm)-s1(nm))/poros(nm) + s1(nm)
             if (comparereal(s1(nm),dpL(nm))<=0) then
                ! Might never be used if sud is repeated but is it maybe better to write it?
                ! kfs_cc(nm) = -1
                ! idry = 1 not needed, drychk will do it.
             endif
          else !I dry the entire cells
             !
             !  NOTE: 
             !
             dps(nm) = dpL(nm)
             ! Might never be used if sud is repeated but is it maybe better to write it?
             !kfs_cc(nm) = -1
             !idry = 1 not needed, drychk will do it.
             
          endif
       endif
       !
       ! determine global maximum of 'idry' over all nodes
       ! Note: this enables to synchronize the repeating computation of SUD
       !
    enddo
    !   
end subroutine drychk_cc
