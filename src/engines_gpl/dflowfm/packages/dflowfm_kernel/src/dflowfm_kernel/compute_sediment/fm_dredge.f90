module m_fm_dredge
!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------
!  
!  
!-------------------------------------------------------------------------------

   private
   public fm_rddredge
   public fm_dredge

contains

   subroutine fm_rddredge(dredgepar, md_dredgefile, error)
       use m_rddredge, only:rddredge
       use precision, only: fp
       use properties, only: tree_data, tree_create
       use m_depfil_stm, only:  depfil_stm
       use m_flowgeom, only: ba_mor, griddim, ndx
       use m_flowtimes, only: julrefdat
       use unstruc_files, only: mdia
       use m_sediment, only: stmpar
       use m_bedform, only: bfmpar
       use dredge_data_module, only: dredge_type
       !
       implicit none
       !
       type(dredge_type), target, intent(inout) :: dredgepar
       character(255)           , intent(in)    :: md_dredgefile
       logical                  , intent(out)   :: error
   !
   ! Local variables
   !
       type(tree_data), pointer                :: dad_ptr
   !
   !! executable statements -------------------------------------------------------
   !
       dredgepar%dredgefile = md_dredgefile
       call tree_create  ( "Dredging input", dad_ptr )
       call rddredge(dredgepar, dad_ptr, stmpar%sedpar, bfmpar%lfbedfrm, &
                   & stmpar%morpar, mdia, julrefdat, &
                   & ba_mor, griddim, '', 1, ndx, error)
   end subroutine fm_rddredge

   subroutine fm_dredgecommunicate(a, n, error, msgstr)
       use precision, only: fp
       use m_partitioninfo, only: reduce_sum
       !
       implicit none
       !
       integer               , intent(in)    :: n      ! length of real array
       real(fp), dimension(n), intent(inout) :: a      ! real array to be accumulated
       logical               , intent(out)   :: error  ! error flag
       character(*)          , intent(out)   :: msgstr ! string to pass message
       !
       error = .false. 
       msgstr = ''
       call reduce_sum(n, a)
   end subroutine fm_dredgecommunicate

   subroutine fm_dredge(error)
       use precision, only: fp, hp
       use m_sediment, only: mtd, stmpar, sedtra
       use m_flow, only: s1
       use m_flowtimes, only: time1, dts, julrefdat, tstart_user, tfac
       use m_flowgeom, only: bl_ave, ndx
       use m_dad, only: dadpar, model_has_dredge_links_across_partitions
       use unstruc_files, only: mdia
       use m_bedform, only: bfmpar
       use m_partitioninfo, only: my_rank, numranks
       use m_dredge_initialize, only: dredge_initialize
       use m_dredge, only: dredge
       use message_module, only: writemessages
       use bedcomposition_module, only: updmorlyr
       use m_fm_morstatistics, only: morstats
       !
       implicit none
       !
       logical                        , intent(out) :: error
       !
       ! The following list of pointer parameters is used to point inside the data structures
       !
       real(hp)                       , pointer :: morft
       real(fp)      , dimension(:,:) , pointer :: dbodsd
       integer       , dimension(:)   , pointer :: kfsed
       integer                        , pointer :: lsedtot
       real(fp)      , dimension(:)   , pointer :: cdryb
       logical                        , pointer :: firstdredge
   !
   ! Local variables
   !
       real(fp)                            :: morhr
       integer                             :: nst
       logical                             :: spinup
       real(fp), dimension(:), allocatable :: dz_dummy
       integer                             :: istat
   !
   !! executable statements -------------------------------------------------------
   !
       firstdredge         => dadpar%firstdredge
       kfsed               => sedtra%kfsed
       dbodsd              => sedtra%dbodsd
       lsedtot             => stmpar%lsedtot
       cdryb               => stmpar%sedpar%cdryb
       morft               => stmpar%morpar%morft
       !
       nst = 1
       error = .false.
       morhr = real(morft*24.0_hp,fp)
       if (firstdredge) then
           call dredge_initialize(dadpar, my_rank + 1, numranks, mdia, error, fm_dredgecommunicate)
       endif
       !
       if (.not.error) then
           spinup = time1 < tstart_user + stmpar%morpar%tmor*tfac
           call dredge(ndx, lsedtot, spinup, cdryb, bl_ave, 1.0_fp, &
                       & dbodsd, kfsed, s1, time1/3600_hp, morhr, dadpar, error, &
                       & fm_dredgecommunicate, bfmpar%duneheight, stmpar%morpar, dts, numranks, mdia, &
                       & julrefdat, 1, ndx, sedtra, stmpar%morlyr, mtd%messages)
       endif
       
       if (firstdredge) then
          do nst = 1, size(dadpar%dredge_prop)
             if (.not. dadpar%dredge_prop(nst)%in1domain) then
               model_has_dredge_links_across_partitions = .true.
               exit
             end if
          end do
       end if
       !
       ! Update sediment administration for dumping only
       ! dbodsd is filled (kg/m^2 sediment added to a cell)
       !
       if (.not.error .and. stmpar%morpar%cmpupd) then 
          allocate(dz_dummy(1:ndx), stat=istat)   ! no actual bed update, unlike updmorlyr in fm_erosed.f90
          if (stmpar%morpar%moroutput%morstats) then
             call morstats(dbodsd)
          endif   
          if (updmorlyr(stmpar%morlyr, dbodsd, dz_dummy, mtd%messages) /= 0) then
             call writemessages(mtd%messages, mdia)
             error = .true.
             return
          else
             call writemessages(mtd%messages, mdia)
          endif
          deallocate(dz_dummy, stat=istat)
       endif
    end subroutine fm_dredge
   
end module m_fm_dredge
