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

 SUBROUTINE MERGENODESINPOLYGON()

  use m_netw
  use kdtree2Factory
  use unstruc_messages
  use m_sferic
  use m_missing
  use m_polygon, only: NPL, xpl, ypl, zpl
  use geometry_module, only: dbpinpol, dbdistance
  use gridoperations
	use m_mergenodes

  implicit none

  INTEGER           :: K, KK, KM, K1, K2, KK1, KK2, KA, KB, kn3, L, LL, JA, JACROS
  INTEGER           :: IBR, KP, N, JADUM
  DOUBLE PRECISION  :: DIST, DISMIN
  DOUBLE PRECISION  :: SL, SM, XCR, YCR, CRP, XB, YB
  integer :: kint, Lint, in

  double precision                            :: R2search             ! squared search radius

  integer                                     :: NN
  integer                                     :: numk_inpoly          ! number of nodes in polygon
  integer, parameter                          :: NUMKDTREEMIN = 100   ! minimum number of nodes required for kdtree
  integer, parameter                          :: jakdtree = 1         ! use kdtree (1) or not (0)

  integer                                     :: itp, i, kkother, kother, nummerged, jadone, ierror, nrl1d

  double precision, dimension(:), allocatable :: xx, yy  ! coordinates of nodes in polygon

  integer,          dimension(:), allocatable :: iperm   ! permutation array

  double precision                            :: xboundmin, xboundmax, d

  logical                                     :: Lmerge

  CALL SAVENET()

  call setnodadm(0)
  KC = 0
  in = -1
  node:DO K = 1,NUMK
     CALL DBPINPOL( XK(K), YK(K), in, dmiss, JINS, NPL, xpl, ypl, zpl)
     if ( in.gt.0 ) then
        kc(k) = 0  ! Initialize for link loop below
        DO kk=1,nmk(K)
           LL = abs(nod(k)%lin(kk))

           ! KC(1D NODES) = 1 , KC(2D NODES) = 2

           if (kn(3,LL) == 1 .or. kn(3,LL) == 6) then
              itp = 1 ! "1D" netnode type
           else if (kn(3,LL) == 3 .or. kn(3,LL) == 4 .or. kn(3,LL) == 5 .or. kn(3,LL) == 7) then
              itp = kn(3,LL) ! 1d2d connections
           else if (kn(3,LL) == 2) then
              itp = 2 ! "2D" netnode type
           else
              itp = 0
           end if

           kc(k) = max(kc(k), itp)

        end do
     end if
  ENDDO node

  if ( jsferic.eq.1 ) then
     call get_meshbounds(xboundmin, xboundmax)
  end if

  CALL READYY(' ', 0.5d0 )

  kint = max(numk/100,1)
  if (tooclose > 0) then

     CALL READYY('Merging nodes',0d0)

     jadone = 0

     if ( jakdtree.eq.1 .and. numk.gt.NUMKDTREEMIN ) then

        call mess(LEVEL_INFO, 'Merging nodes on top of each other...')

!       get coordinates of nodes in polygon
        allocate(xx(numk))
        xx = 0d0
        allocate(yy(numk))
        yy = 0d0
        allocate(iperm(numk))
        iperm = 0

        nummerged = 0

        numk_inpoly = 0
        do k=1,numk
           if ( kc(k) >= 1 .and. xk(k).ne.DMISS .and. yk(k).ne.DMISS ) then
              numk_inpoly=numk_inpoly+1
              xx(numk_inpoly) = xk(k)
              yy(numk_inpoly) = yk(k)
              iperm(numk_inpoly) = k
           end if
        end do

!       compute squared search radius
        R2search = tooclose**2

!       initialize kdtree
        call build_kdtree(treeglob,numk_inpoly,xx,yy, ierror, jsferic, dmiss)

!       deallocate arrays with node coordinates
        deallocate(xx)
        deallocate(yy)

        if ( ierror.eq.0 ) then
           jadone = 1

!          find and merge nodes on top of each other
           do kk=1,numk_inpoly
              k = iperm(kk)

              if ( k.eq.0 ) cycle   ! already merged

              IF (MOD(K,kint) .EQ. 0) THEN
                 CALL READYY(' ',MIN( 1d0,dble(k)/kint ) )
              ENDIF

!              fill query vector
               call make_queryvector_kdtree(treeglob,xk(k),yk(k), jsferic)

!              count number of points in search area
               NN = kdtree2_r_count(treeglob%tree,treeglob%qv,R2search)

               if ( NN.gt.1 ) then ! at least two nodes need to be merged
!                 resize results array if necessary
                  call realloc_results_kdtree(treeglob,NN)

!                 find other nodes
                  call kdtree2_n_nearest(treeglob%tree,treeglob%qv,NN,treeglob%results)

!                 merge with other nodes
                  do i=1,NN
                     kkother = treeglob%results(i)%idx
                     kother  = iperm(kkother)
!                    exclude own node and nodes already merged
                     if ( kother.ne.k .and. kother.gt.0 ) then

                        Lmerge = .false.
                        if (kc(k) == 1 .and. (kc(kother) == 1 .or. kc(kother) >= 3)) then
                           Lmerge = .true.
                        else if (kc(k) == 2 .and. kc(kother) == 2) then
                           Lmerge = .true.
                        else if (kc(k) >= 3 .and. nmk(k) > 1) then ! Only 1d2d links if they are not endpoints that should connect inside a 2D cell.
                           Lmerge = .true.
                        end if

                        if ( Lmerge ) then
                           kc(k) = max(kc(k), kc(kother)) ! merged node gets maximum of the two node types
                           call mergenodes(kother,k,ja)
                           if ( ja.eq.1 ) then
                              iperm(kkother) = 0
                              nummerged = nummerged+1
                           endif
                        else
                           continue
                        end if
                     end if
                  end do
               end if
           end do

           call mess(LEVEL_INFO, 'done.')
           call mess(LEVEL_INFO, 'number of merges: ', nummerged)
        end if

!       deallocate permutation array
        if ( allocated(iperm) ) deallocate(iperm)

!       deallocate kdtree
        if ( treeglob%itreestat.ne.ITREE_EMPTY ) call delete_kdtree2(treeglob)
     end if

     if ( jadone.ne.1 ) then
!       non-kdtree
        DO K = 1,NUMK
           IF (MOD(K,kint) .EQ. 0) THEN
               CALL READYY(' ',MIN( 1d0,dble(k)/kint ) )
           ENDIF

           IF (KC(K) > 0) THEN
              DO KK = K+1,NUMK
                 IF (KC(KK) > 0) THEN
                    IF (dbdistance( XK(K), yk(k), XK(KK), yk(kk), jsferic, jasfer3D, dmiss ) < TOOCLOSE ) THEN
                       CALL MERGENODES(K,KK,JA)
                       IF (JA .EQ. 1) THEN
                          KC(K)  = -ABS(KC(K))
                       ENDIF
                    ENDIF
                 ENDIF
              ENDDO
           ENDIF
        ENDDO

     end if

     if ( jsferic.eq.1 ) then
        call rearrange_worldmesh(xboundmin, xboundmax)
     end if

     CALL READYY(' ',-1d0)
  endif

  if (CONNECT1DEND > 0) then

     CALL READYY('Connecting 1D nodes',0d0)

     DO K = 1,NUMK                             ! MERGE 1d ENDPOINTS TO 1d ENDPOINTS THAT ARE REALLY CLOSE
         IF (MOD(K,kint) .EQ. 0) THEN
             CALL READYY(' ',.5d0*MIN( 1d0,dble(k)/kint ) )
         ENDIF
         IF (KC(K) == 1 .AND. NMK(K) == 1) THEN
           DO KK = K+1,NUMK
              IF (KC(KK) == 1  .AND. NMK(KK) == 1) THEN
                 IF (dbdistance( XK(K), yk(k), XK(KK), yk(kk), jsferic, jasfer3D, dmiss) < 0.2*CONNECT1DEND  ) THEN
                    CALL MERGENODES(K,KK,JA)
                    IF (JA .EQ. 1) THEN
                       KC(K)  = -1
                       KC(KK) = -1
                    ENDIF
                 ENDIF
              ENDIF
           ENDDO
        ENDIF
     ENDDO

     CALL SETBRANCH_LC(nrl1d)
     if (nrl1d == 0) then
         CALL READYY(' ',-1d0) ; netstat = NETSTAT_OK
         return
     endif

     KC    = 1
     DO L  = 1,NUML
       IF (KN(3,L) == 2) THEN               ! KC(1D NODES) = 1 , KC(2D NODES) = 2
          KC( KN(1,L) ) = 2
          KC( KN(2,L) ) = 2
       ENDIF
     ENDDO
     Lint = max(NUML/100,1)
     DO L = 1,NUML
         IF (MOD(L,Lint) .EQ. 0) THEN
             CALL READYY(' ',.5d0+.5d0*MIN( 1d0,dble(L)/Lint ) )
         ENDIF
        IF (KN(3,L) == 1 .or. KN(3,L) == 4) THEN
           kn3 = kn(3,L)
           K1 = KN(1,L) ; K2 = KN(2,L)
           IF (KC(K1) > 0 .and. KC(K2) > 0) THEN
              KA = 0
              IF (NMK(K1) == 1 .AND. NMK(K2)== 2) THEN
                 KA = K1 ; KB = K2
              ELSE IF (NMK(K2) == 1 .AND. NMK(K1)== 2) THEN
                 KA = K2 ; KB = K1
              ENDIF

              IF (KA .NE. 0) THEN
                 DISMIN = 1D9 ; KM = 0
                 DO K = 1,NUMK
                    IF (KA .NE. K .AND. KC(K) == 1) THEN
                       JADUM = 1
                       IF (LC(L) == LC(NOD(K)%LIN(1)) ) THEN
                          ! Known bug: do not only check %lin(1), but all links.
                          JADUM = 0
                          CYCLE  !  SKIP OWN BRANCH
                       ENDIF

                       IF (dbdistance( XK(K), yk(k), XK(Ka), yk(ka), jsferic, jasfer3D, dmiss ) < CONNECT1DEND  ) THEN
                           DIST = dbdistance( XK(KA),YK(KA),XK(K),YK(K), jsferic, jasfer3D, dmiss)
                           IF ( Dist < DISMIN ) THEN
                              dismin = dist ; KM = K
                           ENDIF
                       ENDIF
                    ENDIF
                 ENDDO


                 IF (KM .NE. 0) THEN

                    IF (DISMIN < 0.5*UNIDX1D) THEN
                       CALL MERGENODES(KA,KM,JA)
                    ELSE
                       NUML = NUML + 1
                       KN(1,NUML) = KA
                       KN(2,NUML) = KM
                       KN(3,NUML) = kn3 ! 1 or 4
                       LC(  NUML) = LC(L)
                    ENDIF
                    KC(KA) = 0
                    KC(KM) = 0
                 ENDIF

              ENDIF

           ENDIF
        ENDIF

     ENDDO
     CALL READYY(' ',-1d0)
     call setnodadm(0)
     netstat = NETSTAT_OK
  ENDIF

  END SUBROUTINE MERGENODESINPOLYGON
