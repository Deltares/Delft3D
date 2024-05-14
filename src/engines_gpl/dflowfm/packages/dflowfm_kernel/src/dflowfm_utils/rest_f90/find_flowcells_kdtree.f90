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

!> find flow cells with kdtree2
  subroutine find_flowcells_kdtree(treeinst,Ns,xs,ys,inod,jaoutside,iLocTp, ierror)

     use m_missing
     use m_flowgeom
     use m_GlobalParameters, only: INDTP_1D, INDTP_2D, INDTP_ALL
     use kdtree2Factory
     use m_sferic
     use unstruc_messages
     use gridoperations
     use geometry_module, only: dbdistance, pinpok

     implicit none

     type(kdtree_instance),           intent(inout) :: treeinst
     integer,                         intent(in)    :: Ns      !< number of samples
     double precision, dimension(Ns), intent(in)    :: xs, ys  !< observation coordinates
     double precision, dimension(:),  allocatable   :: xx, yy  !< unique station coordinates
     integer,          dimension(:),  allocatable   :: iperm   !< permutation array
     integer,          dimension(:),  allocatable   :: invperm !< inverse array
     integer,          dimension(Ns), intent(out)   :: inod    !< flow nodes
     integer,                         intent(in)    :: jaoutside  !< allow outside cells (for 1D) (1) or not (0)
     integer,                         intent(in)    :: iLocTp !< (0) not for obs, or obs with locationtype==0, (1) for obs with locationtype==1, (2) for obs with locationtype==2
     integer,                         intent(out)   :: ierror  !< error (>0), or not (0)

     character(len=128)                           :: mesg, FNAM

     integer,          parameter                  :: Msize=10

     double precision, dimension(Msize)           :: xloc, yloc
     integer,          dimension(Msize)           :: Lorg
     integer,          dimension(Msize)           :: LnnL

     double precision                             :: dmaxsize, R2search, t0, t1, zz

     integer                                      :: i, ip1, isam, in, k, N, NN
     integer                                      :: inum, num, jj
     integer                                      :: in3D, j, fid
     integer                                      :: nstart, nend
     logical                                      :: jadouble
     double precision                             :: dist_old, dist_new

     ierror = 1

     inod = 0

     call klok(t0)

!    build kdtree
     call build_kdtree(treeinst, Ns, xs, ys, ierror, jsferic, dmiss)

     if ( ierror.ne.0 ) then
        goto 1234
     end if

     ! define the searching range, this is especially for the purpose of snapping obs to 1D, 2D or 1D+2D flownodes.
     ! For other purpose it should stay as before
     select case(iLocTp)
     case (INDTP_ALL)
        nstart = 1
        nend   = ndx
     case(INDTP_1D) ! 1d flownodes coordinates
        nstart = ndx2D+1
        nend   = ndx
     case(INDTP_2D) ! 2d flownodes coordinates
        nstart = 1
        nend   = ndx2D
     end select

     call mess(LEVEL_INFO, 'Finding flow nodes...')

!    loop over flownodes
     do k = nstart, nend
!       fill query vector
        call make_queryvector_kdtree(treeinst,xz(k),yz(k), jsferic)

!       compute maximum flowcell dimension
        dmaxsize = 0d0
        N = size(nd(k)%x)
        do i=1,N
           ip1=i+1; if ( ip1.gt.N ) ip1=ip1-N
           dmaxsize = max(dmaxsize, dbdistance(nd(k)%x(i),nd(k)%y(i),nd(k)%x(ip1),nd(k)%y(ip1), jsferic, jasfer3D, dmiss))
        end do

!       determine square search radius
        R2search = 1.1d0*dmaxsize**2  ! 1.1d0: safety

!       get the cell polygon that is safe for periodic, spherical coordinates, inluding poles
        call get_cellpolygon(k,Msize,N,1d0,xloc,yloc,LnnL,Lorg,zz)

        if ( N.lt.1 ) then
           if ( k.le.Ndxi ) then
              continue
           end if
           cycle
        end if

!       count number of points in search area
        NN = kdtree2_r_count(treeinst%tree,treeinst%qv,R2search)

        if ( NN.eq.0 ) cycle ! no links found

!       reallocate if necessary
        call realloc_results_kdtree(treeinst,NN)

!       find nearest NN samples
        call kdtree2_n_nearest(treeinst%tree,treeinst%qv,NN,treeinst%results)

!       check if samples are in cell
        do i=1,NN
           isam = treeinst%results(i)%idx

           if ( k>ndx2D .and. k<ndxi+1 .and. jaoutside.eq.1 ) then  ! For 1D nodes, skip point-in-cell check
              in = 1                           ! These are always accepted if closest.
           else
              call pinpok(xs(isam), ys(isam), N, xloc, yloc, in, jins, dmiss)

!!             BEGIN DEBUG
!              if ( jsferic.eq.1 .and. jasfer3D.eq.1 ) then
!                 call pinpok3D(xs(isam),ys(isam),N,xloc,yloc,in3D)
!                 if ( in3D.ne.in ) then
!                    write(FNAM, "('obs_', I0, '_cell_', I0, '.m')") isam, k
!                    call newfil(fid, trim(FNAM))
!                    write(fid,"('x=[', $)")
!                    do j = 1,N
!                       if ( j.gt.1 ) write(6,*)
!                       write(fid,"(E15.5, $)") xloc(j)
!                    end do
!                    write(fid,"('];')")
!
!                    write(fid,"('y=[', $)")
!                    do j = 1,N
!                       if ( j.gt.1 ) write(6,*)
!                       write(fid,"(E15.5, $)") yloc(j)
!                    end do
!                    write(fid,"('];')")
!
!                    write(fid,"('xp=', E15.5, ';')") xs(isam)
!                    write(fid,"('yp=', E15.5, ';')") ys(isam)
!                    call doclose(fid)
!
!                    continue
!                 end if
!              end if
!!             END DEBUG

           endif
           if ( in.eq.1 ) then
              if ( inod(isam).ne.0 ) then            ! should not happen, but it can: for example in case of overlapping 1D branches
                 write(mesg, "('find_flowcells_kdtree: sample/point ', I0, ' in cells ', I0, ' and ', I0)") isam, inod(isam), k
                 call mess(LEVEL_INFO, mesg  )
!                goto 1234
                 if ( k>ndx2D .and. k<ndxi+1 .and. jaoutside.eq.1 ) then  ! ONLY in case of a 1D node, consider replacing, if the 1D node is closer
                    dist_old = dbdistance(xs(isam),ys(isam),xz(inod(isam)), yz(inod(isam)),jsferic, jasfer3D, dmiss)
                    dist_new = dbdistance(xs(isam),ys(isam),xz(k), yz(k),jsferic, jasfer3D, dmiss)
                    if (dist_new<dist_old) then            ! if the new candidate is nearer to the observation station  ...
                       inod(isam) = k                      ! ... adopt the new candidate as primary candidate
                    endif
                    write(mesg, "('   selected : ',I0,' based on distance comparison.')")  inod(isam)
                    call mess(LEVEL_INFO, mesg  )
                 end if
              else
                 inod(isam) = k
              end if
           end if
        end do
     end do

     call klok(t1)

     write(mesg, "('done in ', F12.5, ' sec.')") t1-t0
     call mess(LEVEL_INFO, trim(mesg))

     ierror = 0
1234 continue

!    deallocate
     if ( treeinst%itreestat.ne.ITREE_EMPTY ) call delete_kdtree2(treeinst)

     return
  end subroutine find_flowcells_kdtree
