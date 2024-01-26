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

!> Finds the flow nodes/cell numbers for each given x,y point (e.g., an observation station)
subroutine find_flownode(N, xobs, yobs, namobs, kobs, jakdtree, jaoutside, iLocTp)
   use unstruc_messages
   use m_partitioninfo
   use m_flowgeom
   use m_GlobalParameters, only: INDTP_1D, INDTP_2D, INDTP_ALL
   use kdtree2Factory
   use geometry_module, only: dbdistance
   use m_missing, only: dmiss
   use m_sferic, only: jsferic, jasfer3D

   implicit none

   integer,                            intent(in)     :: N           !< number of points
   double precision,     dimension(N), intent(in)     :: xobs, yobs  !< points coordinates
   character(len=IdLen), dimension(N), intent(in)     :: namobs      !< names of points
   integer,              dimension(N), intent(inout)  :: kobs        !< associated flow nodes, if found.
   integer,                            intent(inout)  :: jakdtree    !< use kdtree (1) or not (other)
   integer,                            intent(in)     :: jaoutside   !< allow outside cells (for 1D) (1) or not (0)
   integer,                            intent(in)     :: iLocTp      !< Node type, one of INDTP_1D/2D/ALL.
   integer                                         :: ierror      !  error (1) or not (0)
   integer                                         :: i, k, k1b
   integer,           dimension(1)                 :: idum
   double precision                                :: d1, d2

   ierror = 1

   if ( jakdtree.eq.1 ) then
      call find_flowcells_kdtree(treeglob,N,xobs,yobs,kobs,jaoutside,iLocTp, ierror)

      if ( jampi.eq.1 ) then
!        globally reduce ierror
         idum(1) = ierror
         call reduce_int_max(1, idum)
         ierror = idum(1)
      end if

      if ( ierror.ne.0 ) then
         jakdtree = 0   ! retry without kdtree
      end if

!     disable observation stations without attached flowlinks
      do i=1,N
         k=kobs(i)
         if ( k.gt.0 ) then
            if ( nd(k)%lnx.lt.1 ) then
               kobs(i) = 0
            end if
         end if
      end do
   end if

   if ( jakdtree.ne.1 ) then
      do i=1,N
         call inflowcell(xobs(i),yobs(i),k,jaoutside, iLocTp)
         if ( jaoutside.eq.1 .and. (iLocTp == INDTP_1D .or. iLocTp == INDTP_ALL)) then
            call CLOSETO1DORBND(xobs(i),yobs(i),k1B)
            IF (K .ne. 0 .and. k1b .ne. 0) THEN
                D1 = DBDISTANCE(XZ(K1B), YZ(K1B), XOBS(I), YOBS(I), jsferic, jasfer3D, dmiss)
                D2 = DBDISTANCE(XZ(K  ), YZ(K  ), XOBS(I), YOBS(I), jsferic, jasfer3D, dmiss)
                IF ( D1 < D2 ) THEN
                   K = K1B
                ENDIF
            ELSE IF (K1B .NE. 0) THEN
                K = K1B
            ENDIF
         end if
         kobs(i) = 0
         if ( k.ne.0 ) then
            if ( nd(k)%lnx.gt.0 ) then
               kobs(i) = k
            end if
         end if
      end do
   end if

   if ( jampi.eq.1 .and. N.gt.0 ) then
!     check if this subdomain owns the observation station
      call reduce_kobs(N,kobs,xobs,yobs,jaoutside)
   end if

   do i=1,N
      if ( kobs(i).eq.0 ) then
          write(msgbuf, '(a,i0,a,a,a)') 'Could not find flowcell for point #', i, ' (', trim(namobs(i)), '). Discarding.'
          call msg_flush()
      endif
   end do

   ierror = 0
1234 continue

   return
   end subroutine find_flownode
