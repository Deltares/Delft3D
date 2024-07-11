   !LC TO DO: PASS CALL-BACK  FUNCTION FOR INTERACTER MESSAGES
   module gridoperations

   use MessageHandling, only: msgbox, mess, LEVEL_ERROR, LEVEL_WARN
   use m_ug_meshgeom
   implicit none

   !new functions
   public :: make1D2Dinternalnetlinks 
   public :: make1D2Droofgutterpipes
   public :: make1D2Dstreetinletpipes
   public :: ggeo_make1D2DRiverLinks
   public :: ggeo_make1D2Dembeddedlinks
   public :: ggeo_convert
   public :: ggeo_convert_1d_arrays
   public :: ggeo_get_links_count
   public :: ggeo_get_links
   public :: ggeo_count_or_create_edge_nodes
   public :: ggeo_deallocate
   public :: ggeo_initialize
   public :: ggeo_construct_netcelllin_from_netcellnod


   !from net.f90
   public :: RESTORE
   public :: SAVENET
   public :: INCREASENETW
   public :: increasenetcells
   public :: alreadycell
   public :: setnewpoint
   public :: dsetnewpoint
   public :: CROSSED2d_BNDCELL
   public :: OTHERNODE
   public :: OTHERNODECHK
   public :: getlink_between_nodes
   public :: SETNODADM_GRD_OP
   public :: INIALLOCnetcell
   public :: update_cell_circumcenters
   public :: FINDCELLS
   public :: FINDTRIS
   public :: FINDQUADS
   public :: FINDPENTAS
   public :: FINDHEXAS
   public :: iscounterclockwise
   public :: rechtsaf
   public :: CONNECTDBN
   public :: CONNECTDB
   public :: ADDLINKTONODES
   public :: SETNODLIN
   public :: CHKLINSIZTONODE
   public :: GIVENEWNODENUM
   public :: INCELLS
   public :: sort_links_ccw
   public :: get_cellpolygon
   public :: make_dual_cell
   public :: dlinkangle
 
   
   ! unstruct.F90
   public :: getcellsurface
   public :: getcellweightedcenter
   public :: getcellcircumcenter

   public :: network_dimensions_message

   private

   ! for mapping 1d internal nodes to the client, which can be different
   integer, allocatable :: mesh1dInternalToClientMapping(:)    
   ! for mapping 2d faces to the client ordering, which can be different
   type(t_ug_meshgeom)  :: meshgeom2d
   ! store the indexses of the links
   integer, allocatable :: connectionIndexes(:,:)   
   integer :: numValidLinks

   contains

   !> Initializes the gridoperations module.
   !! Only initializes debug timers for now.
   subroutine init_gridoperations()
   use Timers
   
      call timini()
      timon = .false. ! Set to .true. for performance timers

   end subroutine init_gridoperations

   !-----------------------------------------------------------------!
   ! net.f90
   !-----------------------------------------------------------------!

   !> TODO: Document me


   SUBROUTINE GIVENEWNODENUM(KNU)
   !LC use m_netw
   use network_data
   implicit none
   integer :: KNU

   integer :: kx
   integer :: lx

   IF ( NUMK == SIZE(KC) ) THEN
      KX = 1.2*NUMK ; LX = 1.2*NUML
      CALL INCREASENETW(KX, LX)
   ENDIF
   NUMK = NUMK + 1
   KNU  = NUMK
   RETURN
   END SUBROUTINE GIVENEWNODENUM

  
   !> Restore variables with backup data
   SUBROUTINE RESTORE()
   use network_data
   implicit none
   integer :: k, KX, LS, LS0, LX, NODSIZ, IERR

   !IF ( NUMK0.EQ.0 ) RETURN
   if (.not. allocated(xk0) .or. .not. allocated(kn0) .or. .not. allocated(nod0)) return

   KX = size(XK0) ! restore everything present (in case numk/numk0 has not yet been increased)

   XK (1:KX)  = XK0 (1:KX)
   YK (1:KX)  = YK0 (1:KX)
   ZK (1:KX)  = ZK0 (1:KX)

   NMK(1:KX)  = NMK0(1:KX)
   KC (1:KX)  = KC0 (1:KX)

   LX = size(LC0) ! restore everything present (in case numl/numl0 has not yet been increased)

   KN(:,1:LX) = KN0(:,1:LX)
   LC(  1:LX) = LC0(  1:LX)

   ! Only restore optional dxe when it is there already
   if (allocated(dxe)) then
      dxe(1:LX) = dxe0(1:LX)
   end if

   NODSIZ = SIZE(NOD)

   DO K = 1,KX
      IF (ALLOCATED(NOD0(K)%LIN) ) THEN
         LS0 = SIZE(NOD0(K)%LIN )  ! LS0 = NMK0(K)
      ELSE
         LS0 = 0
      ENDIF

      IF (LS0 .GE. 1) THEN
         ! IF (.NOT. ASSOCIATED(NOD(K)%LIN) ) THEN
         IF (ALLOCATED(NOD(K)%LIN) ) THEN
            LS = SIZE(NOD(K)%LIN )
         ELSE
            LS = 0
         ENDIF
         IF (LS .LT. LS0) THEN
            IF (LS .GE. 1) DEALLOCATE (NOD(K)%LIN )
            ALLOCATE   (NOD(K)%LIN(LS0), STAT = IERR )
            NOD(K)%LIN = 0
         ENDIF
         NOD(K)%LIN(1:LS0) = NOD0(K)%LIN(1:LS0)
      ENDIF
   ENDDO

   NUMK = NUMK0
   NUML = NUML0

   !  need findcells
   netstat = NETSTAT_CELLS_DIRTY
   RETURN
   END SUBROUTINE RESTORE

   SUBROUTINE SAVENET()
   use network_data
   implicit none
   integer :: ierr
   integer :: k, KX, LS, LS0, LX, NN

   if (.not. allocated(xk) .or. .not. allocated(kn) .or. .not. allocated(nod)) return

   KX = KMAX ! backup everything present (in case numk has not yet been increased) ! KX = NUMK
   IF (ALLOCATED(nod0)) THEN
      DO K= 1, SIZE(NOD0)
         if ( allocated(nod0(k)%lin) ) DEALLOCATE(NOD0(K)%LIN)
      ENDDO
      DEALLOCATE(NOD0)
   ENDIF
   ALLOCATE ( NOD0(KX) , stat = ierr )
   !CALL AERR('NOD0(KX)', IERR, KX)

   if (allocated(xk0)) deallocate(xk0,yk0,zk0)
   allocate ( XK0(KX), YK0(KX), ZK0(KX) , STAT=IERR)
   !call aerr('XK0(KX), YK0(KX), ZK0(KX)', IERR, 3*kx)

   if (allocated (KC0) ) deallocate ( KC0 )
   ALLOCATE( KC0(KX), STAT=IERR)

   if (allocated (nmk0) ) deallocate ( NMK0 )
   ALLOCATE( NMK0(KX), STAT=IERR)

   XK0 (1:KX) = XK (1:kx)
   YK0 (1:KX) = YK (1:kx)
   ZK0 (1:KX) = ZK (1:kx)
   KC0( 1:KX) = KC (1:kx)
   NMK0(1:KX) = NMK(1:kx)

   IF (ALLOCATED(LC0)) DEALLOCATE(KN0 ,LC0)
   LX = LMAX ! backup everything present (in case numl has not yet been increased) ! LX = NUML
   ALLOCATE (KN0(3,LX), LC0(LX), STAT=IERR)

   KN0(:,1:LX) = KN(:,1:LX)
   LC0(  1:LX) = LC(  1:LX)

   ! Only save optional dxe when it is there already
   if (allocated(dxe)) then
      if (allocated(dxe0)) deallocate(dxe0)
      allocate(dxe0(LX), STAT=IERR)
      dxe0(1:LX) = dxe(1:LX)
   end if


   DO K   = 1,KX
      LS  = NMK(K) ! SIZE(NOD (K)%LIN )
      IF (LS .GE. 1) THEN
         !       IF (.NOT. ASSOCIATED(NOD0(K)%LIN) ) THEN
         IF (.NOT. ALLOCATED(NOD0(K)%LIN) ) THEN
            LS0 = 0
         ELSE
            LS0 = SIZE(NOD0(K)%LIN )
         ENDIF
         IF (LS0 .LT. LS) THEN
            IF (LS0 .GE. 1 .and. allocated(NOD0(K)%LIN)) DEALLOCATE (NOD0(K)%LIN )
            ALLOCATE   (NOD0(K)%LIN(LS) ) ; NOD0(K)%LIN = 0
         ENDIF
         NOD0(K)%LIN(1:LS) = NOD(K)%LIN(1:LS)
      ENDIF
   ENDDO

   NUMK0 = NUMK
   NUML0 = NUML
   RETURN
   END SUBROUTINE SAVENET

   !> Increase the number of net links
   SUBROUTINE INCREASENETW(K0,L0, also_dxe)
   !LC removed use m_netw
   use network_data
   use m_alloc
   use m_missing, only : xymis, dmiss

   implicit none
   integer,           intent(in) :: K0       !< New number of net nodes.
   integer,           intent(in) :: L0       !< New number of net links
   logical, optional, intent(in) :: also_dxe !< Also allocate the optional dxe array for edge lengths. Default: .false.

   integer :: ierr
   integer :: k
   integer :: knxx
   logical :: also_dxe_

   if (present(also_dxe)) then
      also_dxe_ = also_dxe
   else
      also_dxe_ = .false.
   end if

   if (also_dxe_) then
      ! Directly ensure dxe allocation, since it may not have been allocated before (as it is optional).
      call realloc(dxe, LMAX, keepExisting = .true., fill = dmiss)
   end if

   if (K0 < KMAX .and. L0 < LMAX) RETURN

   CALL SAVENET()

   IF (KMAX <= K0) THEN
      KMAX = K0 + 100000   ! 2 KAN WEG
      IF (allocated(nod)) then
         do k = 1,size(nod)
            if ( allocated(nod(k)%lin) ) deallocate (nod(k)%lin)
         enddo
         deallocate(nod, xk, yk, zk, kc, nmk, rnod)
      end if
      ALLOCATE ( NOD(KMAX) , STAT = IERR)
      CALL AERR('NOD(KMAX)', IERR, KMAX )
      ALLOCATE ( XK (KMAX), YK (KMAX), ZK (KMAX), KC (KMAX), NMK (KMAX), RNOD(KMAX) , STAT=IERR   )
      CALL AERR('XK (KMAX), YK (KMAX), ZK (KMAX), KC (KMAX), NMK (KMAX), RNOD(KMAX)', IERR, 7*KMAX)

      DO K = 1,KMAX
         IF ((allocated(NMK0)).and.(K .LE. SIZE(NMK0))) THEN
            KNXX = MAX(NMK0(K),KNX)
         ELSE
            KNXX = KNX
         ENDIF
         ALLOCATE(NOD(K)%LIN(KNXX) , STAT=IERR) ;
         NOD(K)%LIN = 0
      ENDDO

      NMK = 0 ; KC = 1 ; XK = XYMIS ; YK = XYMIS ; ZK = dmiss
   ENDIF

   IF (LMAX <= L0) THEN
      LMAX = L0 + 3*100000
      IF (SIZE(LC) > 0 .and. allocated(kn)) THEN
         DEALLOCATE(KN ,LC , RLIN)
      ENDIF
      ALLOCATE (KN (3,LMAX), LC (LMAX), STAT=IERR) ; KN = 0 ; LC = 0 ! TODO: AvD: catch memory error
      ALLOCATE (RLIN (LMAX), STAT=IERR)
      
      if (also_dxe_) then
         if (allocated(dxe)) deallocate(dxe)
         allocate(dxe(LMAX))
         dxe = dmiss
      end if
   ENDIF

   CALL RESTORE()

   END SUBROUTINE INCREASENETW

   !> Increases the global netcell array to a new size.
   !! Will not shrink the array. Specify a growfac > 1.0 to grow in bigger chunks.
   !!
   !! Example:
   !! do
   !!     call increasenetcells(NUMP+1, 1.2, .true.)
   subroutine increasenetcells(numpx, growfac, keepExisting)
   use network_data
   use m_alloc
   implicit none
   integer,          intent(in) :: numpx        !< New maximum size for netcell array.
   real,             intent(in) :: growfac      !< When growing, resize by additional factor growfac*numpx (e.g. 1.2)
   logical,          intent(in) :: keepExisting !< Restore existing data after reallocate, otherwise leave undefined.

   integer :: p, ierr, n0
   integer :: numpx0 !< Current size of netcell
   integer :: numpx1 !< Actual size of to-be-increased netcell
   if (allocated(netcell)) then
      numpx0 = size(netcell)
   else
      numpx0 = 0
   end if

   if (numpx0 >= numpx) return ! Array is still large enough

   numpx1 = max(numpx, ceiling(numpx*growfac)) ! Grow a bigger chunk at once

   ! 1: SAVE
   if (nump > 0 .and. keepExisting) then ! NOTE: Only create backup if nump > 0 (not numpx0 > 0)

      if(allocated(netcell_sav)) then
         !          deallocate netcell_sav
         do p=1,ubound(netcell_sav,1)
            if ( allocated(netcell_sav(p)%nod) ) deallocate(netcell_sav(p)%nod)
            if ( allocated(netcell_sav(p)%lin) ) deallocate(netcell_sav(p)%lin)
         end do
         deallocate(netcell_sav)
      end if

      allocate(netcell_sav(nump), stat = ierr)
      CALL AERR('netcell_sav(nump)', ierr, nump)

      do p=1,nump
         n0 = netcell(p)%n
         if (n0 <= 0) then
            cycle
         end if

         allocate(netcell_sav(p)%nod(n0), netcell_sav(p)%lin(n0), stat = ierr)
         !CALL AERR('netcell_sav(p)%nod(n0), netcell_sav(p)%lin(n0)', ierr, 2*n0)

         netcell_sav(p)%n   = netcell(p)%n
         netcell_sav(p)%nod = netcell(p)%nod
         netcell_sav(p)%lin = netcell(p)%lin

         deallocate(netcell(p)%nod, netcell(p)%lin)
      end do
   end if

   if (allocated(netcell)) then
      deallocate(netcell)
      CALL AERR('netcell', 0, -numpx0)
   end if

   allocate(netcell(numpx1), stat = ierr)
   CALL AERR('netcell(numpx1)', ierr, numpx1)

   ! 2: RESTORE
   if (nump > 0 .and. keepExisting) then ! NOTE: Only restore backup if nump > 0 (not numpx0 > 0)
      do p=1,nump
         n0 = netcell_sav(p)%n
         if (n0 <= 0) then
            cycle
         end if

         allocate(netcell(p)%nod(n0), netcell(p)%lin(n0), stat = ierr)
         !CALL AERR('netcell(p)%nod(n0), netcell(p)%lin(n0)', ierr, 2*n0)

         netcell(p)%n   = netcell_sav(p)%n
         netcell(p)%nod = netcell_sav(p)%nod
         netcell(p)%lin = netcell_sav(p)%lin

         deallocate(netcell_sav(p)%nod, netcell_sav(p)%lin)
      end do

      deallocate(netcell_sav)
      CALL AERR('netcell_sav', 0, -nump)
   end if

   end subroutine increasenetcells

   !> check and see if the links already form a cell
   logical function alreadycell(N, K, L)
   !use m_netw
   use network_data
   implicit none

   integer,               intent(in) :: N  !< number of links and nodes
   integer, dimension(N), intent(in) :: K  !< node set
   integer, dimension(N), intent(in) :: L  !< link set

   integer                           :: i, j
   integer                           :: kL, kR, LL

   integer                           :: num

   integer                           :: knod, kcom

   integer, dimension(N)             :: icell

   integer, dimension(N)             :: dum

   !   search for a link that:
   !     -is a member of the link set,
   !     -bounds two cells which are adjacent to the links in the link set, and
   !     -connects two nodes that are members of the node set
   !
   !   this link will be an internal link in the polygon formed by the links in the link set, hence these links do not form a new cell

   alreadycell = .false.

   do i=1,N
      if ( lnn(L(i)).eq.2 ) return
   end do

   icell = lne(1, L)

   do i=1,N
      if ( lnn(L(i)).lt.1 ) cycle

      do j = 1,netcell(icell(i))%N
         LL = netcell(icell(i))%lin(j)

         if ( lnn(LL).ne.2 ) cycle  ! this also excludes all members of the link set

         kL = lne(1,LL)
         kR = lne(2,LL)

         !        check if both kL and kR are members of the cell set
         dum = 0
         where( icell.eq.kL ) dum = 1
         if ( sum(dum).eq.0 ) cycle   ! kL not a member
         dum = 0
         where( icell.eq.kR ) dum = 1
         if ( sum(dum).eq.0 ) cycle   ! kR not a member

         !        check if the link connects nodes in the node set
         dum = 0
         where( K.eq.kn(1,LL) ) dum =1
         if ( sum(dum).eq.0 ) cycle   ! first node of link not a member
         dum = 0
         where( K.eq.kn(2,LL) ) dum =1
         if ( sum(dum).eq.0 ) cycle   ! second node of link not a member

         alreadycell = .true.
         return
      end do
   end do

   !   check for nodes inside the polygon - to be done

   !   see if a cell contains only triangles that share a node
   kcom = 0
   !  if ( N.eq.4 ) then ! quads only
   do i=1,N
      LL = L(i)

      if ( lnn(LL).lt.1 ) then
         kcom = 0
         exit
      end if

      if ( netcell(icell(i))%N.ne.3 ) then
         kcom = 0
         exit ! triangles only
      end if

      !       find the node of the triangle not on the link
      knod = sum(netcell(icell(i))%nod(1:3)) - kn(1,LL) - kn(2,LL)

      if ( kcom.eq.0 ) then  ! set common node
         kcom = knod
      else if ( knod.ne.kcom ) then  ! check with common node
         kcom = 0
         exit
      end if
   end do
   !  end if

   ! common node found
   if ( kcom.ne.0 ) then
      alreadycell = .true.
      return
   end if


   end function alreadycell

   !> TODO: Document me
   SUBROUTINE SETNEWPOINT(XP,YP,ZP,K1)

   use network_data
   use m_missing, only : dmiss, xymis

   implicit none
   integer :: jav
   integer :: jview
   double precision :: xyz
   double precision :: XP, YP, ZP
   integer :: K1

   COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4
   CALL GIVENEWNODENUM(K1)
   xk(k1) = xp
   yk(k1) = yp
   zk(k1) = zp
   IF (KC(K1) .EQ. 0) KC(K1) = 1
   RETURN
   END SUBROUTINE SETNEWPOINT

  SUBROUTINE DSETNEWPOINT(XP,YP,K)
  use network_data
  use m_missing
 
  implicit none
  DOUBLE PRECISION :: XP, YP
  INTEGER          :: K
  CALL GIVENEWNODENUM(K)
  XK(K) = XP; YK(K) = YP ; ZK(K) = dmiss; KC(K) = K
  RETURN
  END SUBROUTINE DSETNEWPOINT

   SUBROUTINE CROSSED2d_BNDCELL(NML, XP1, YP1, XP2, YP2 , NC1, Lfound)
   !use m_netw
   use network_data
   use m_cell_geometry, only: xz, yz
   use m_missing, only : dmiss
   use geometry_module, only : crossinbox, cross
   use m_sferic, only: jsferic, jasfer3D

   implicit none
   INTEGER          :: NC1, NML
   DOUBLE PRECISION :: XP1, YP1, XP2, YP2

   INTEGER          :: L, JACROS, K1, K2, LL, Lfound
   DOUBLE PRECISION :: SL, SM, XCR, YCR, CRP, slm

   NC1 = 0
   slm = 1d9
   DO L  = 1,NML
      K1 = KN(1,L) ; K2 = KN(2,L)
      if ( k1.lt.1 .or. k2.lt.1 ) cycle   ! SPvdP: safety

       IF (LNN(L) == 1) THEN       ! LINK MET 1 BUURCEL
         IF (KN(3,L) == 2) THEN
            CALL CROSSinbox (XP1, YP1, XP2, YP2, XK(K1), YK(K1), XK(K2), YK(K2), jacros, SL, SM, XCR, YCR, CRP, jsferic, dmiss)
            if (jacros == 1) then
               if (sl < slm) then
                  NC1 = LNE(1,L)
                  slm = sl
                  Lfound = L
               endif
            end if
         ENDIF
      ENDIF
   ENDDO
 
   END SUBROUTINE CROSSED2d_BNDCELL

   SUBROUTINE CROSSEDanother1Dlink(NML, XP1, YP1, NC1, Lfound)
   !use m_netw
   use network_data
   use m_cell_geometry, only: xz, yz
   use m_missing, only : dmiss
   use geometry_module, only : crossinbox, cross
   use m_sferic, only: jsferic, jasfer3D

   implicit none
   INTEGER          :: NC1, NML
   DOUBLE PRECISION :: XP1, YP1, XP2, YP2

   INTEGER          :: L, JACROS, K1, K2, LL, Lfound
   DOUBLE PRECISION :: SL, SM, XCR, YCR, CRP, slm
  
   if (nc1 > 0) then 
      xp2 = xz(nc1) ; yp2 = yz(nc1)

      do LL = 1,nml
         if ( LL == Lfound) cycle
         IF ( kn(3,LL) == 1 .or. kn(3,LL) == 3 .or. kn(3,LL) == 6 ) THEN   ! crossing any another 1D type 
             K1 = KN(1,LL) ; K2 = KN(2,LL)
             if ( k1.lt.1 .or. k2.lt.1 ) cycle   ! SPvdP: safety
             CALL CROSS(XP1, YP1, XP2, YP2, XK(K1), YK(K1), XK(K2), YK(K2), jacros, SL, SM, XCR, YCR, CRP, jsferic, dmiss)
             if (jacros == 1) then
                 if (sl > 0d0 .and. sl < 1d0 .and. sm > 0d0 .and. sm < 1d0) then
                    nc1 = 0
                    return
                 endif   
             endif   
         endif   
      enddo
   endif 
   END SUBROUTINE CROSSEDanother1Dlink

   
   SUBROUTINE OTHERNODE(K1,L1,K2)

   use network_data
   implicit none
   integer :: K1, L1, K2

   integer :: ka

   KA = KN(1,L1)
   K2 = KN(2,L1)
   IF (KA .EQ. K1) RETURN
   K2 = KA
   RETURN
   END SUBROUTINE OTHERNODE

   SUBROUTINE OTHERNODECHK(K1,L1,K2)
   use network_data
   implicit none

   integer :: K1, L1, K2

   K2 = 0

   IF (KN(3,L1) .NE. 2 .and. KN(3,L1) .NE. 0) RETURN


   IF (K1 == KN(1,L1)) THEN
      IF (LC(L1) == 1) RETURN
      K2 =  KN(2,L1)
      RETURN
   ENDIF

   IF (LC(L1) == -1) RETURN
   K2 = KN(1,L1)

   RETURN
   END SUBROUTINE OTHERNODECHK


   !> Find the netlink that connects two netnodes (in either direction).
   !! When not found, negative value is returned.
   !!
   !! Note: setnodadm_grd_op() must have been run already.
   subroutine getlink_between_nodes(k1, k2, L)
      use network_data
      implicit none
      integer, intent(in   ) :: k1 !< One end node of the link
      integer, intent(in   ) :: k2 !< The other end node of the link
      integer, intent(  out) :: L  !< Resulting link number, -1 when no such link exists between k1 and k2.

      integer ::  iL, kother

      L = -1

      if (k1 <= 0 .or. k1 > numk) then
         return ! Invalid node
      end if
      
      if (.not. allocated(nod)) then
         return ! setnodadm has not been called yet
      end if

      do iL=1,nmk(k1)
         call othernode(k1, nod(k1)%lin(iL), kother)
         if (kother == k2) then
            L = nod(k1)%lin(iL)
         end if
      end do

   end subroutine getlink_between_nodes


   SUBROUTINE SETNODADM_GRD_OP(JACROSSCHECK_)

   use network_data

   use mathconsts, only: degrad_hp
   use geometry_module, only: getdx, getdy, dcosphi, cross
   use m_missing, only : dmiss, dxymis
   use m_sferic, only: pi, jsferic, jasfer3D
   use MessageHandling
   use m_alloc

   implicit none
   INTEGER               :: JACROSSCHECK_ !< remove crossed 2D links (1), or not (0), output permutation array (+10)

   double precision :: crp, e, e1
   integer          :: jacros, mout
   integer          :: k, k1, k12, k2, k22, k3, KI, ka, kb, kk, L, L1, L2, LL, LLL, LI, LTOT, ls, JA
   INTEGER          :: jDupLinks, jOverlapLinks, jSmallAng, maxlin
   double precision :: sl, sm, xcr, ycr

   INTEGER, ALLOCATABLE          ::  KC2(:), KN2(:,:), KCK(:)
   double precision              :: phi, dx, dy, dmaxcosp, dcosp, costriangleminangle, phi0

   double precision :: X(4), Y(4)

   integer, dimension(:), allocatable :: Lperm_new

   integer :: jacrosscheck ! remove 2D crossing netlinks (1) or not (0)
   integer :: japermout    ! output permutation array (1) or not (0)
   integer :: janodperm    ! output node permutation array (1) or not (0)

   
   logical :: need_to_allocate_kc

   jacrosscheck = jacrosscheck_
   japermout = 0
   if ( jacrosscheck.ge.10 ) then
      japermout    = 1
      jacrosscheck = jacrosscheck - 10
   end if

   IF (NUML == 0) RETURN

   E = 1E-6 ; E1 = 1-E

   if ( japermout.eq.1 ) then
      ! allocate permutation array
      call realloc(Lperm, numL, keepExisting=.false., fill=0)
      call realloc(Lperminv, numL, keepExisting=.false., fill=0)
      do L=1,numL
         Lperm(L) = L
      end do
      allocate(Lperm_new(numL))
      ! allocate permutation array for nodes
      call realloc(nodePermutation, numk, keepExisting=.false., fill=0)
   end if

   IF (JACROSSCHECK == 1) THEN
      LL = 0
      DO L=NUML,1,-1
         K1 = KN(1,L)  ; K2 = KN(2,L); K3 = KN(3, L)
         if (k3 .NE. 2) then
            cycle ! 1D links mogen blijven
         endif

         !IF (DINVIEW(XK(K1),YK(K1),ZK(K1)) .OR. DINVIEW(XK(K2),YK(K2),ZK(K2)) ) THEN
            DO LLL = MAX(1,L-1), 1 ,-1
               KA = KN(1,LLL) ; KB = KN(2,LLL)
               ! If interfaces share same node, no further action:
               if (k1 == ka .or. k1 == kb .or. k2 == ka .or. k2 == kb ) cycle
               X(1) = XK(K1)
               Y(1) = YK(K1)
               X(2) = XK(K2)
               Y(2) = YK(K2)
               X(3) = XK(KA)
               Y(3) = YK(KA)
               X(4) = XK(KB)
               Y(4) = YK(KB)
               CALL CROSS(XK(K1), YK(K1), XK(K2), YK(K2), XK(KA), YK(KA), XK(KB), YK(KB), JACROS,SL,SM,XCR,YCR,CRP,jsferic, dmiss)
               IF (SL > E .AND. SL < E1 .AND. SM > E .AND. SM < E1 ) THEN
                  KN(1,L) = 0; KN(2,L) = 0 ; KN(3, L) = -1; EXIT
               ENDIF
            ENDDO
         !ENDIF
      ENDDO
   ENDIF

100 continue

   need_to_allocate_kc = .true.
   if ( allocated(kc) ) then
     if ( size(kc).ge.numk ) then
        need_to_allocate_kc = .false.
     else
        deallocate(kc)
     end if
   end if

   ALLOCATE(KCK(NUMK) )
   if ( need_to_allocate_kc ) then
      allocate(kc(numk))
      kc  = 0
      kck = 0
   else
      KCK(1:NUMK) = KC(1:NUMK)            ! STORE ORG KC
      kc = 0
   end if

   ALLOCATE(KN2(3,NUML)) ; KN2 = 0     ! RESERVE KN


   L2 = 0 ; L1 = 0
   jathindams = 0
   lc = 0
   nlinkremoved = 0
   DO L=1,NUML                                                   ! LINKS AANSCHUIVEN, 1d EERST
      K1 = KN(1,L)  ; K2 = KN(2,L) ; K3 = KN(3,L)
      if (k3 == 0) then
         jathindams = 1
      end if
      ja = 0
      IF (K1 .NE. 0 .AND. K2 .NE. 0 .AND. K1 .NE. K2 ) THEN
         JA = 1
         IF (XK(K1) == DMISS .OR. XK(K2) == DMISS) THEN          ! EXTRA CHECK: ONE MISSING
            JA = 0
         ELSE                                                    !            : OR BOTH EQUAL
            if ((K3 == 1 .or. k3 == 6) .and. allocated(dxe)) then ! User-defined net link lengths
               if (dxe(L) <= 0d0) then                           ! X/Y of K1, K2 may be equal, as long as length > 0
                  ja = 0
               end if
            else IF (XK(K1) == XK(K2) .AND. YK(K1) == YK(K2) ) THEN
               JA = 0
            end if
         ENDIF
         IF (JA == 1) THEN
            IF (K3 == 0 .or. K3 == 2) THEN
               L2 = L2 + 1
               KN2(1,L2)  = K1 ; KN2(2,L2)  = K2 ; KN2(3,L2)  = K3
               if ( japermout.eq.1 ) then
                  Lperm_new(numL-L2+1) = Lperm(L) ! fill 2D links from the back of the temp. array
               end if
            ELSE IF (K3 == 1 .OR. K3 > 2) THEN
               L1 = L1 + 1
               KN(1,L1) = K1 ; KN(2,L1) = K2 ; KN(3,L1) = K3
               if ( japermout.eq.1 ) then
                  Lperm_new(L1) = Lperm(L) ! fill 1D links from the start of the temp. array
               end if
            ENDIF
            KC(K1)   = 1  ; KC(K2)   = 1
         ENDIF
      ENDIF
      if (ja == 0) then
         ! save removed links, so the flow1d admin can be updated later on
         nlinkremoved = nlinkremoved+1
         LC(nlinkremoved) = L
      endif               
   ENDDO

   if ( japermout.eq.1 ) then
      !     copy 1D and flip 2D values from the temp. to the permutation array
      do L=1,L1
         Lperm(L) = Lperm_new(L)
         Lperminv(Lperm(L)) = L
      end do
      do L=1,L2
         Lperm(L1+L) = Lperm_new(numL-L+1)
         Lperminv(Lperm(L1+L)) = L1+L
      end do
   end if

   NUML1D = L1
   NUML   = L1 + L2

   DO L   = 1,L2
      LL  = NUML1D + L
      KN(:, LL) = KN2(:,L)              ! 2D na 1D
   ENDDO

   ALLOCATE (KC2(NUMK) )
   KK = 0
   DO K = 1,NUMK                        ! NODES AANSCHUIVEN
      IF (KC(K) .NE. 0 ) THEN
         KK = KK + 1
         KC (KK) = K                    ! HIER KWAM IE VANDAAN
         XK (KK) = XK(K)
         YK (KK) = YK(K)
         ZK (KK) = ZK(K)
         KCK(KK) = KCK(K)               ! COPY ORG KC
         KC2(K)  = KK                   ! EN HIER GAAT IE NAARTOE         
         if ( japermout.eq.1 ) then
            nodePermutation(k) = kk     ! k is old node number, kk is new number
         end if
      !else
      !   write (msgbuf, '(a,i0,a)') 'setnodadm: removed mesh node #', K, ' (missing, or not connected in any mesh edge).'
      !   call warn_flush()
      ENDIF
   ENDDO
   !if (numk /= KK) then
   !   write (msgbuf, '(a,i0,a,i0,a,i0,a)') 'setnodadm: ', (NUMK-KK), ' mesh nodes were removed. Input: ', NUMK, ' nodes. New: ', KK, ' nodes. See preceding messages for details.'
   !   call warn_flush()
   !end if
   NUMK = KK

   DO L   = 1,NUML
      K1  = KN(1,L)  ; K2  = KN(2,L) ; K3 = KN(3,L)
      K12 = KC2(K1)  ; K22 = KC2(K2)
      KN2(1,L) = K12 ; KN2(2,L) = K22; KN2(3,L) = K3
   ENDDO

   KN(:,1:NUML)  = KN2(:,1:NUML)       ! TERUGZETTEN
   KC(1:NUMK)    = KCK(1:NUMK)

   DEALLOCATE (KC2, KN2, KCK)          ! WEGWEZEN

   NMK = 0
   DO L   = 1,NUML                     ! TEL LINKS PER NODE
      K1  = KN(1,L)  ; K2  = KN(2,L)
      NMK(K1) = NMK(K1) + 1
      NMK(K2) = NMK(K2) + 1
   ENDDO

   DO K = 1,NUMK                       ! ALLOCEER RUIMTE
      IF (NMK(K) > 0) THEN
         !call REALLOC(NOD(K)%LIN, NMK(K), keepexisting = .false. )
         if (allocated(NOD(K)%LIN)) then
            deallocate(NOD(K)%LIN)
         endif
         allocate( NOD(K)%LIN(nmk(k)) )
      ENDIF
   ENDDO

   NMK = 0
   jDupLinks = 0
   lnk:DO L=1,NUML                         ! EN ZET NODEADMIN (+check/reset dubbele links)
      K1 = KN(1,L) ;
      K2 = KN(2,L) ;
      DO LL = 1,NMK(K1) ! Check all previously added links
         if ( (kn(3,L) /=1) .and. (KN(1,NOD(K1)%LIN(LL)) == K2 .or. KN(2,NOD(K1)%LIN(LL)) == K2)) then
            KN(1,L) = 0
            KN(2,L) = 0
            jDupLinks = 1
            cycle lnk ! Jump to next outer L-loop
         end if
      ENDDO
      NMK(K1) = NMK(K1) + 1
      NMK(K2) = NMK(K2) + 1
      NOD(K1)%LIN(NMK(K1)) = L
      NOD(K2)%LIN(NMK(K2)) = L
   END DO lnk
   if (jDupLinks /= 0) then
      goto 100 ! Er waren duplicate links: opnieuw aanschuiven
   endif



   ! New cross check (two-smallangle check) is always performed
   jOverlapLinks = 0
   costriangleminangle = cos(triangleminangle*degrad_hp)
   if ( triangleminangle > 0 ) then
      lnl:do L=1,NUML
         k1 = kn(1,L) ; k2 = kn(2,L) ; k3 = kn(3,L)

         if (k3 >= 1 .and. k3 <= 6 ) cycle

         jSmallAng = 1
         do ki=1,2 ! Consider links of both nodes in link L
            if (jSmallAng /= 1) exit ! First or second end node did not have small angle between links

            if (ki == 2) then ! Check second node
               k1 = k2 ; k2 = kn(1,L)
            end if

            dmaxcosp = -huge(dmaxcosp)
            do LI=1,NMK(k1)
               LL = NOD(k1)%lin(LI)
               if (LL == L) cycle ! No self-check

               call othernode(k1, LL, kb)
               if (kb == 0) then      ! hk: dit kan hier toch nooit voorkomen?
                  cycle              ! Incomplete link?
               endif

               dcosp = dcosphi(xk(k1), yk(k1), xk(k2), yk(k2), xk(k1), yk(k1), xk(kb), yk(kb), jsferic, jasfer3D, dxymis)
               dmaxcosp = max(dmaxcosp, dcosp)
            end do
            if (dmaxcosp > costriangleminangle) then
               jSmallAng = 1
            else
               jSmallAng = 0
            end if
         end do

         if (jSmallAng == 1) then ! Disable original link L
            kn(1,L) = 0
            kn(2,L) = 0
            jOverlapLinks = 1
            write(msgbuf, '(a,i8, a)') 'Removed link', L, ', because of tiny angles at endpoints.'
            call msg_flush()
            ! cycle lnl ! Jump to next outer L-loop ben je al?
         end if
      end do lnl
   end if

   if (jOverlapLinks /= 0) then
      goto 100 ! Er waren overlapping links: opnieuw aanschuiven
   end if


   ! Sort nod%lin in counterclockwise order
   maxlin = maxval(nmk(1:numk))
   do k=1,numk
      call sort_links_ccw(k, maxlin)
   end do

   ! Reset small link count for linkbadqual (net link based).
   ! Will only be recomputed in flow_geominit.
   nlinkbadortho = 0
   nlinktoosmall = 0
   nlinkcross    = 0

   ! call trace_netlink_polys()

   if ( japermout.eq.1 ) then
      if ( allocated(Lperm_new) ) deallocate(Lperm_new)
   end if

   !  netcell administration out of date
   netstat = NETSTAT_CELLS_DIRTY
   END SUBROUTINE SETNODADM_GRD_OP

   SUBROUTINE INIALLOCnetcell()
   use network_data
   use m_alloc
   implicit none

   integer :: ierr, nx

   NUMP  = 0

   nx = max(1,int(1.5*NUMK))
   call increasenetcells(nx, 1.0, .false.)
   netcell(:)%N = 0

   if (allocated(lnn) ) deallocate(lnn)
   allocate ( lnn(numl) , stat=ierr  )
   call aerr('lnn(numl)', ierr, numl )
   LNN  = 0

   if (allocated(lne) ) deallocate(lne)
   allocate(  lne(2,numl) , stat=ierr )
   call aerr('lne(2,numl)', ierr, 2*numl )
   lne  = 0                                            ! array = 0
   RETURN

   END SUBROUTINE INIALLOCnetcell

   subroutine update_cell_circumcenters()

   use network_data
   use m_alloc
   use m_sferic, only:  jsferic, jasfer3D, dtol_pole
   use m_cell_geometry

   implicit none

   integer :: n, numc, ierr
   double precision :: zzz

   ! Compute (circum)center coordinates now already.
   ! nump is in same rythm as  (future) ndx2d
   if (nump1d2d > 0) then
      !     if ( keepcircumcenters.eq.1 ) call qnerror('updating circumcenter', ' ', ' ')
      ! If ndx>nump, probably already some 1D stuff present.
      ! We can safely ignore it here, but won't, because this saves some
      ! realloc costs for xz, yz in flow_geominit.
      numc = max(ndx,nump1d2d)
      if ((numc > size(xz)).or.(.not.allocated(xz))) then
         call realloc(xz, numc, stat=ierr, keepExisting=.false.)
         call aerr('xz(numc)',IERR, numc)
         call realloc(yz, numc, stat=ierr, keepExisting=.false.)
         call aerr('yz(numc)',IERR, numc)
      end if
      if ((numc > size(xzw)).or.(.not.allocated(xzw))) then
         call realloc(xzw, numc, stat=ierr, keepExisting=.false.)
         call aerr('xzw(numc)',IERR, numc)
         call realloc(yzw, numc, stat=ierr, keepExisting=.false.)
         call aerr('yzw(numc)',IERR, numc)
      end if
      if ((numc > size(ba)).or.(.not.allocated(ba))) then
         call realloc(ba, numc, stat=ierr, keepExisting=.false.)
         call aerr('ba(numc)',IERR, numc)
      endif

      do n = 1,nump                                      ! get cell center coordinates 2D
         CALL GETCELLWEIGHTEDCENTER(n, xz(n) , yz(n) , zzz)
         call getcellsurface(n, ba(n), xzw(n), yzw(n))
         ! call cirr( xzw(n), yzw(n), 211 )
      end do
      do n = nump+1, nump1d2d
         xz(n) = xk(netcell(n)%nod(1))
         yz(n) = yk(netcell(n)%nod(1))
         xzw(n) = xz(n)
         yzw(n) = yz(n)
      enddo
      
   end if
   end subroutine update_cell_circumcenters

   !> Compute circumcenter of a netcell and its average depth value.
   !! See also getcellcircumcenter
   subroutine getcellcircumcenter( n, xz, yz, zz )     ! circumcenter etc, depending on celltype
   use network_data
   use geometry_module, only: getcircumcenter
   use m_missing,       only: dmiss, jins,dxymis
   use m_sferic,        only: jsferic, jasfer3D, jglobe

   implicit none
   integer,          intent(in)       :: n         !< Netcell number
   double precision, intent(out)      :: xz, yz    !< Coordinates of circumcenter point, undefined for void cells.
   double precision, intent(out)      :: zz        !< Depth value at cc point, undefined for void cells.   
   integer,            parameter      :: Msize=10

   double precision, dimension(Msize) :: xv, yv
   integer, dimension(Msize)          :: Lorg
   integer, dimension(Msize)          :: LnnL
   integer                            :: i, k, L, nn

   nn = netcell(n)%n

   if ( nn.lt.1 ) return  ! safety

   call get_cellpolygon(n, Msize, nn, 1d0, xv, yv, LnnL, Lorg, zz)
   call getcircumcenter(nn, xv, yv, LnnL, xz, yz, jsferic, jasfer3D, jglobe, jins, dmiss, dxymis, dcenterinside)

   end subroutine getcellcircumcenter


   !> Finds 2D cells in the unstructured net.
   !! Optionally within a polygon mask.
   ! Resets netcell data and also computes circumcenters in xz (flowgeom)
   SUBROUTINE findcells(jp)
   
   use network_data
   use geometry_module, only: dbpinpol
   use m_polygon,       only: NPL, xpl, ypl, zpl
   use m_missing,       only: dmiss, jins
   use m_cell_geometry
   use m_sferic

   implicit none
   integer, intent(in) :: JP !< Type of cells to find (unfolded: 3: triangle, etc. up to 6=hexa, 0 = all; folded: code+100; no new nodemask (nonzero values will be used as mask here): code+1000, no sednodadm: code+10000, output link permutation array: code+100000)

   integer, allocatable, dimension(:) :: kc_sav  ! save of kc

   integer :: ik
   integer :: k
   integer :: k1
   integer :: k2
   integer :: l
   integer :: jafold, jakeepmask, jasetnodadm, japermout
   integer :: jp_

   jp_ = jp

   ! determine if (setnodm) has to output link permutation array
   if ( jp_.ge.100000) then
      jp_ = jp_-100000
      japermout = 1
   else
      japermout = 0
   end if

   ! determine if setnodm has to be called
   if ( jp_.ge.10000 ) then
      jp_ = jp_-10000
      jasetnodadm = 0
   else
      jasetnodadm = 1
   end if

   ! determine if the nodemask has to be made
   if ( jp_.ge.1000 ) then
      jp_ = jp_-1000
      jakeepmask = 1
   else
      jakeepmask = 0
   end if

   ! determine if folded cells have to be accounted for
   if ( jp_.ge.100 ) then
      jp_ = jp_-100
      jafold = 1
   else
      jafold = 0
   end if

   if ( jasetnodadm.eq.1 ) then
      if ( japermout.eq.1 ) then
         CALL SETNODADM_GRD_OP(10)
      else
         CALL SETNODADM_GRD_OP(0)
      end if
   end if

   CALL INIALLOCnetcell()

   LC   =  0
   IK   = -1

   if ( jakeepmask.ne.1 ) then
      KC   =  0
      DO K = 1,NUMK
         if (NPL > 0) then 
            CALL DBPINPOL(xk(k), yk(k), ik, dmiss, JINS, NPL, xpl, ypl, zpl)
         else
            CALL DBPINPOL(xk(k), yk(k), ik, dmiss, JINS, NPL)
         endif
            
         IF (IK > 0) THEN
            KC(K) = IK
         ENDIF
      ENDDO
   end if

   IF (JP_ .EQ. 0) THEN
      CALL FINDTRIS(0)
      CALL FINDQUADS(0)
      CALL FINDPENTAS(0)
      CALL FINDHEXAS(0)
      if ( jafold.eq.1 ) then
         CALL FINDQUADS(1)
         CALL FINDTRIS(1)
         CALL FINDPENTAS(1)
         CALL FINDHEXAS(1)
      end if
   ELSE IF (JP_ .EQ. 3) THEN
      CALL FINDTRIS(0)
      if ( jafold.eq.1 ) CALL FINDTRIS(1)
   ELSE IF (JP_ .EQ. 4) THEN
      CALL FINDQUADS(0)
      if ( jafold.eq.1 ) CALL FINDQUADS(1)
   ELSE IF (JP_ .EQ. 5) THEN
      CALL FINDPENTAS(0)
      if ( jafold.eq.1 ) CALL FINDPENTAS(1)
   ELSE IF (JP_ .EQ. 6) THEN
      CALL FINDHEXAS(0)
      if ( jafold.eq.1 ) CALL FINDHEXAS(1)
   ELSE IF (JP_ .EQ. 11)THEN
      CALL FINDPENTAS(0)
      CALL FINDHEXAS(0)
      if ( jafold.eq.1 ) then
         CALL FINDPENTAS(1)
         CALL FINDHEXAS(1)
      end if
   ENDIF

   IF (NPL < 3) THEN
      !     LC = 1; KC = 1 ! SPvdP: this gives problems in orthogonalisenet
      LC = 1
      if ( jakeepmask.ne.1 ) then
         KC = 1
      end if
   ELSE
      DO L = 1, NUML
         K1 = KN(1,L) ; K2 = KN(2,L)
         IF (KC(K1) == 1 .or. KC(K2) == 1) LC(L) = 1
      ENDDO
   ENDIF
   nump1d2d = nump 
   call update_cell_circumcenters()

     ! there are no 1D cells yet, safety

   !  If one chooses to add find1dcells to findcells in future, this is how it may look like.
   !    Note however, that:
   !      -lne now has negative entries, which causes problems in various 2d-only subroutines, like orthonogalisenet, at the moment
   !      -kc is detroyed
   !    For these reasons, find1dcells is not included here
   !
   !! find 1D cells, will destroy kc
   !  allocate(kc_sav(numk))
   !  kc_sav = kc(1:numk)
   !
   !  call find1dcells()    ! will destroy kc
   !
   !! restore kc
   !  kc(1:numk) = kc_sav(1:numk)
   !  deallocate(kc_sav)

   ! NR OF 2d CELLS=NUMP
   NDX2D = NUMP                                        

   lasttopology = numk + numl


   ! set network status
   netstat = NETSTAT_OK

   call network_dimensions_message()

   RETURN
   END SUBROUTINE FINDCELLS

   SUBROUTINE FINDTRIS(jafold)

   use network_data
   use m_afmeting
   use m_alloc
   use m_sferic


   integer, intent(in) :: jafold  !< find folded cells (1), or not (0)

   integer :: ierr
   integer :: k1
   integer :: k2
   integer :: k3
   integer :: k4
   integer :: kk
   integer :: kkk
   integer :: kkkk
   integer :: kmod
   integer :: l
   integer :: ll
   integer :: lll
   integer :: i
   integer :: kr(3), Lr(3)
   integer :: kkk_, kkkk_, nmkmax

   !CALL READYY ('FIND TRIS', 0d0)

   nmkmax = 1
   if ( jafold.eq.1 ) nmkmax = 1000

   KMOD = max(1,NUMK/100)
   DO K1 = 1,NUMK

      !IF (MOD(K1,KMOD) == 1) CALL READYY ('FIND TRIS',dble(K1)/dble(NUMK))

      IF (KC(K1) .EQ. 1) THEN

         kklp:DO KK = 1,NMK(K1)
            L  = NOD(K1)%LIN(KK)
            IF (LNN(L) .GE. 2) CYCLE
            CALL OTHERNODECHK(K1,L,K2); IF (K2 == 0) CYCLE

            kkk = 1
            do while ( nod(k2)%lin(kkk).ne.L )
               kkk=kkk+1
            end do
            DO KKK_ = 1,min(NMK(K2),nmkmax)
               kkk = kkk-1
               if ( kkk.lt.1       ) kkk=kkk+nmk(k2)
               if ( kkk.gt.nmk(k2) ) kkk=kkk-nmk(k2)

               LL  = NOD(K2)%LIN(KKK)  ; IF (LL .EQ. L) CYCLE
               IF (LNN(LL) .GE. 2) CYCLE
               CALL OTHERNODECHK(K2,LL,K3) ; IF (K3 == 0) CYCLE
               IF ( rechtsaf(K1,K2,K3) ) CYCLE
               IF (K3 .NE. K1) THEN

                  kkkk = 1
                  do while ( nod(k3)%lin(kkkk).ne.LL )
                     kkkk=kkkk+1
                  end do
                  DO KKKK_ = 1,min(NMK(K3),nmkmax)
                     kkkk = kkkk-1
                     if ( kkkk.lt.1       ) kkkk=kkkk+nmk(k3)
                     if ( kkkk.gt.nmk(k3) ) kkkk=kkkk-nmk(k3)

                     LLL  = NOD(K3)%LIN(KKKK) ; IF (LLL .EQ. LL .OR. LLL .EQ. L) CYCLE
                     IF (LNN(LLL) .GE. 2) CYCLE
                     CALL OTHERNODECHK(K3,LLL,K4)  ; IF (K4 == 0) CYCLE
                     IF ( rechtsaf(K2,K3,K4) ) CYCLE
                     IF (K4 .EQ. K1) THEN  ! TRI GEVONDEN
                        IF (LNN(L)>1 .OR. LNN(LL)>1 .OR. LNN(LLL)>1)  EXIT
                        !                    call setcol(31) ! red
                        !                    call rcirc(xk(k1),yk(k1))
                        !                    call setcol(204) ! green
                        !                    call rcirc(xk(k2),yk(k2))
                        !                    call setcol(211) ! blue
                        !                    call rcirc(xk(k3),yk(k3))
                        !  SPvdP: check and see if cell already exist
                        if ( lnn(L).gt.0 .and. lnn(LL).gt.0 .and. lnn(LLL).gt.0 ) then
                           if ( lne(1,L).eq.lne(1,LL) .and. lne(1,L).eq.lne(1,LLL) ) then
                              cycle
                           else   ! more expensive check
                              kr(1)=k1; kr(2)=k2; kr(3)=k3
                              Lr(1)=L; Lr(2)=LL; Lr(3)=LLL
                              if ( alreadycell(3, kr, Lr) ) cycle

                              !                         do not allow folded cells when all links already have neighboring cells
                              if ( kkk_.ne.1 .or. kkkk_.ne.1 ) then
                                 cycle
                              end if
                           end if
                        end if

                        kr(1)=k1; kr(2)=k2; kr(3)=k3
                        if ( .not.iscounterclockwise(3, kr) ) cycle

                        !CALL ALREADYTRI(K1,K2,K3,JA); IF (JA > 0) EXIT
                        call increasenetcells(NUMP+1, 1.2, .true.)
                        NUMP = NUMP + 1
                        call realloc(netcell(NUMP)%NOD, 3, stat=ierr, keepExisting=.false.)
                        call realloc(netcell(NUMP)%LIN, 3, stat=ierr, keepExisting=.false.)
                        netcell(NUMP)%N = 3
                        netcell(NUMP)%NOD(1) = K1
                        netcell(NUMP)%NOD(2) = K2
                        netcell(NUMP)%NOD(3) = K3
                        netcell(NUMP)%LIN(1) = L
                        netcell(NUMP)%LIN(2) = LL
                        netcell(NUMP)%LIN(3) = LLL
                        LNN(L)            = LNN(L)   + 1
                        LNN(LL)           = LNN(LL)  + 1
                        LNN(LLL)          = LNN(LLL) + 1
                        LNE(LNN(L),L)     = NUMP
                        LNE(LNN(LL),LL)   = NUMP
                        LNE(LNN(LLL),LLL) = NUMP
                        ! SPvdP: linkmask deactivated with the purpose to find folded cells; check if cells already exist instead
                        !                    LC(L)             = 1 ; IF (KN(1,L)   == K2) LC(L)   = -1
                        !                    LC(LL)            = 1 ; IF (KN(1,LL)  == K3) LC(LL)  = -1  ! AvD: re-enabled, in line with new rgfgrid
                        !                    LC(LLL)           = 1 ; IF (KN(1,LLL) == K1) LC(LLL) = -1  !

                        !                    cell found and administered: proceed
                        cycle kklp
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ENDDO kklp
      ENDIF
   ENDDO

   !CALL READYY ( 'FIND TRIS', -1d0 )

   RETURN

   END SUBROUTINE FINDTRIS

   SUBROUTINE FINDQUADS(jafold)
   !LC use m_netw
   use network_data
   use m_afmeting
   use m_alloc
   implicit none

   integer, intent(in) :: jafold  !< find folded cells (1), or not (0)

   double precision :: af
   integer :: ierr
   integer :: k1
   integer :: k2
   integer :: k3
   integer :: k4
   integer :: k5
   integer :: kk
   integer :: kkk
   integer :: kkkk
   integer :: kkkkk
   integer :: kmod
   integer :: l
   integer :: ll
   integer :: lll
   integer :: llll
   integer :: kr(4), Lr(4)
   integer :: kkk_, kkkk_, kkkkk_, nmkmax

   !CALL READYY('FIND QUADS',0d0)

   nmkmax = 1
   if ( jafold.eq.1 ) nmkmax = 1000

   KMOD = max(1,NUMK/100)
   DO K1 = 1,NUMK

      if (mod(k1,KMOD) == 1) then
         af = dble(k1) /dble(numk)
         !CALL READYY('FIND QUADS',AF)
      endif


      IF (KC(K1) .EQ. 1) THEN

         kklp:DO KK = 1,NMK(K1)
            L   = NOD(K1)%LIN(KK)
            IF (LNN(L) .GE. 2) CYCLE
            CALL OTHERNODECHK(K1,L,K2); IF (K2 == 0) CYCLE

            kkk = 1
            do while ( nod(k2)%lin(kkk).ne.L )
               kkk=kkk+1
            end do
            DO KKK_ = 1,min(NMK(K2),nmkmax)
               kkk = kkk-1
               if ( kkk.lt.1       ) kkk=kkk+nmk(k2)
               if ( kkk.gt.nmk(k2) ) kkk=kkk-nmk(k2)

               LL  = NOD(K2)%LIN(KKK)  ; IF (LL .EQ. L) CYCLE
               IF (LNN(LL) .GE. 2) CYCLE
               CALL OTHERNODECHK(K2,LL,K3); IF (K3 == 0) CYCLE
               IF ( rechtsaf(K1,K2,K3) ) CYCLE
               IF (K3 .NE. K1) THEN

                  kkkk = 1
                  do while ( nod(k3)%lin(kkkk).ne.LL )
                     kkkk=kkkk+1
                  end do
                  DO KKKK_ = 1,min(NMK(K3),nmkmax)
                     kkkk = kkkk-1
                     if ( kkkk.lt.1       ) kkkk=kkkk+nmk(k3)
                     if ( kkkk.gt.nmk(k3) ) kkkk=kkkk-nmk(k3)

                     LLL  = NOD(K3)%LIN(KKKK) ; IF (LLL .EQ. LL .OR. LLL .EQ. L) CYCLE
                     IF (LNN(LLL) .GE. 2) CYCLE
                     CALL OTHERNODECHK(K3,LLL,K4); IF (K4 == 0) CYCLE
                     IF ( rechtsaf(K2,K3,K4) ) CYCLE
                     IF (K4 .NE. K2) THEN

                        kkkkk = 1
                        do while ( nod(k4)%lin(kkkkk).ne.LLL )
                           kkkkk=kkkkk+1
                        end do
                        DO KKKKK_ = 1,min(NMK(K4),nmkmax)
                           kkkkk = kkkkk-1
                           if ( kkkkk.lt.1       ) kkkkk=kkkkk+nmk(k4)
                           if ( kkkkk.gt.nmk(k4) ) kkkkk=kkkkk-nmk(k4)

                           LLLL  = NOD(K4)%LIN(KKKKK)
                           IF (LLLL .EQ. LLL .OR. LLLL .EQ. LL .OR. LLLL .EQ. L) CYCLE
                           IF (LNN(LLLL) .GE. 2) CYCLE
                           CALL OTHERNODECHK(K4,LLLL,K5) ; IF (K5 == 0) CYCLE
                           IF ( rechtsaf(K3,K4,K5) ) CYCLE
                           IF (K5 .EQ. K1) THEN  ! PANEEL GEVONDEN
                              IF (LNN(L)>1 .OR. LNN(LL)>1 .OR. LNN(LLL)>1 .OR. LNN(LLLL)>1)  EXIT

                              !  SPvdP: check and see if cell already exist
                              if ( lnn(L).gt.0 .and. lnn(LL).gt.0 .and. lnn(LLL).gt.0 .and. lnn(LLLL).gt.0 ) then
                                 if ( lne(1,L).eq.lne(1,LL) .and. lne(1,L).eq.lne(1,LLL) .and. lne(1,L).eq.lne(1,LLLL) ) then
                                    cycle
                                 else   ! more expensive check
                                    kr(1)=k1; kr(2)=k2; kr(3)=k3; kr(4)=k4
                                    Lr(1)=L; Lr(2)=LL; Lr(3)=LLL; Lr(4)=LLLL
                                    if ( alreadycell(4, kr, Lr) ) cycle
                                    !                               do not allow folded cells when all links already have neighboring cells
                                    if ( kkk_.ne.1 .or. kkkk_.ne.1 .or. kkkkk_.ne.1 ) then
                                       cycle
                                    end if
                                 end if
                              end if

                              !CALL ALREADYQUAD(K1,K2,K3,K4,JA) ; IF (JA > 0 ) EXIT

                              kr(1)=k1; kr(2)=k2; kr(3)=k3; kr(4)=k4
                              if ( .not.iscounterclockwise(4, kr) ) cycle

                              call increasenetcells(NUMP+1, 1.2, .true.)
                              NUMP = NUMP + 1
                              call realloc(netcell(NUMP)%NOD, 4, stat=ierr, keepExisting=.false.)
                              call realloc(netcell(NUMP)%LIN, 4, stat=ierr, keepExisting=.false.)
                              netcell(NUMP)%N = 4
                              netcell(NUMP)%NOD (1)  = K1
                              netcell(NUMP)%NOD (2)  = K2
                              netcell(NUMP)%NOD (3)  = K3
                              netcell(NUMP)%NOD (4)  = K4
                              netcell(NUMP)%LIN(1)   = L
                              netcell(NUMP)%LIN(2)   = LL
                              netcell(NUMP)%LIN(3)   = LLL
                              netcell(NUMP)%LIN(4)   = LLLL
                              LNN(L)              = LNN(L)    + 1
                              LNN(LL)             = LNN(LL)   + 1
                              LNN(LLL)            = LNN(LLL)  + 1
                              LNN(LLLL)           = LNN(LLLL) + 1
                              LNE(LNN(L),L)       = NUMP
                              LNE(LNN(LL),LL)     = NUMP
                              LNE(LNN(LLL),LLL)   = NUMP
                              LNE(LNN(LLLL),LLLL) = NUMP
                              ! SPvdP: linkmask deactivated with the purpose to find folded cells; check if cells already exist instead
                              !                           LC(L)               = 1 ; IF (KN(1,L)    == K2) LC(L)    = -1
                              !                           LC(LL)              = 1 ; IF (KN(1,LL)   == K3) LC(LL)   = -1
                              !                           LC(LLL)             = 1 ; IF (KN(1,LLL)  == K4) LC(LLL)  = -1
                              !                           LC(LLLL)            = 1 ; IF (KN(1,LLLL) == K1) LC(LLLL) = -1

                              !                       cell found and administered: proceed
                              cycle kklp

                           ENDIF
                        ENDDO
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ENDDO kklp
      ENDIF
   ENDDO

   !CALL READYY('FIND QUADS',-1d0)

   RETURN
   END SUBROUTINE FINDQUADS

   SUBROUTINE FINDPENTAS(jafold)

   use network_data
   use m_afmeting
   use m_alloc
   implicit none

   integer, intent(in) :: jafold  !< find folded cells (1), or not (0)

   integer :: ierr
   integer :: k1
   integer :: k2
   integer :: k3
   integer :: k4
   integer :: k5
   integer :: k6
   integer :: kk
   integer :: kkk
   integer :: kkkk
   integer :: kkkkk
   integer :: kkkkkk
   integer :: kmod
   integer :: l
   integer :: ll
   integer :: lll
   integer :: llll
   integer :: lllll
   integer :: kr(5), Lr(5)
   integer :: kkk_, kkkk_, kkkkk_, kkkkkk_, nmkmax
   
   !CALL READYY ('FINDPENTAS',0d0)

   nmkmax = 1
   if ( jafold.eq.1 ) nmkmax = 1000

   KMOD = max(1,NUMK/100)
   DO K1 = 1,NUMK

      !IF (MOD(K1,KMOD) == 1) CALL READYY ('FINDPENTAS',dble(K1)/dble(NUMK))

      IF (KC(K1) == 1) THEN

         kklp:DO KK = 1,NMK(K1)
            L  = NOD(K1)%LIN(KK)
            IF (LNN(L) .GE. 2) CYCLE
            CALL OTHERNODECHK(K1,L,K2); IF (K2 == 0) CYCLE

            kkk = 1
            do while ( nod(k2)%lin(kkk).ne.L )
               kkk=kkk+1
            end do
            DO KKK_ = 1,min(NMK(K2),nmkmax)
               kkk = kkk-1
               if ( kkk.lt.1       ) kkk=kkk+nmk(k2)
               if ( kkk.gt.nmk(k2) ) kkk=kkk-nmk(k2)

               LL  = NOD(K2)%LIN(KKK)
               IF (LL .EQ. L) CYCLE
               IF (LNN(LL) .GE. 2) CYCLE
               CALL OTHERNODECHK(K2,LL,K3); IF (K3 == 0) CYCLE
               IF ( rechtsaf(K1,K2,K3) ) CYCLE
               IF (K3 .NE. K1) THEN

                  kkkk = 1
                  do while ( nod(k3)%lin(kkkk).ne.LL )
                     kkkk=kkkk+1
                  end do
                  DO KKKK_ = 1,min(NMK(K3),nmkmax)
                     kkkk = kkkk-1
                     if ( kkkk.lt.1       ) kkkk=kkkk+nmk(k3)
                     if ( kkkk.gt.nmk(k3) ) kkkk=kkkk-nmk(k3)

                     LLL  = NOD(K3)%LIN(KKKK)
                     IF (LLL .EQ. LL .OR. LLL .EQ. L) CYCLE
                     IF (LNN(LLL) .GE. 2) CYCLE
                     CALL OTHERNODECHK(K3,LLL,K4); IF (K4 == 0) CYCLE
                     IF ( rechtsaf(K2,K3,K4) ) CYCLE
                     IF (K4 .NE. K2 .AND. K4 .NE. K1) THEN

                        kkkkk = 1
                        do while ( nod(k4)%lin(kkkkk).ne.LLL )
                           kkkkk=kkkkk+1
                        end do
                        DO KKKKK_ = 1,min(NMK(K4),nmkmax)
                           kkkkk = kkkkk-1
                           if ( kkkkk.lt.1       ) kkkkk=kkkkk+nmk(k4)
                           if ( kkkkk.gt.nmk(k4) ) kkkkk=kkkkk-nmk(k4)

                           LLLL  = NOD(K4)%LIN(KKKKK)
                           IF (LLLL .EQ. LLL .OR. LLLL .EQ. LL .OR. LLLL .EQ. L) CYCLE
                           IF (LNN(LLLL) .GE. 2) CYCLE
                           CALL OTHERNODECHK(K4,LLLL,K5) ; IF (K5 == 0) CYCLE
                           IF ( rechtsaf(K3,K4,K5) ) CYCLE
                           IF (K5 .NE. K3 .AND. K5 .NE. K2 .AND. K5 .NE. K1) THEN

                              kkkkkk = 1
                              do while ( nod(k5)%lin(kkkkkk).ne.LLLL )
                                 kkkkkk=kkkkkk+1
                              end do
                              DO KKKKKK_ = 1,min(NMK(K5),nmkmax)
                                 kkkkkk = kkkkkk-1
                                 if ( kkkkkk.lt.1       ) kkkkkk=kkkkkk+nmk(k5)
                                 if ( kkkkkk.gt.nmk(k5) ) kkkkkk=kkkkkk-nmk(k5)

                                 LLLLL  = NOD(K5)%LIN(KKKKKK)
                                 IF (LLLLL .EQ. LLLL .OR. LLLLL .EQ. LLL .OR. LLLLL .EQ. LL .OR. LLLLL .EQ. L) CYCLE
                                 IF (LNN(LLLLL) .GE. 2) CYCLE
                                 CALL OTHERNODECHK(K5,LLLLL,K6); IF (K6 == 0) CYCLE
                                 IF ( rechtsaf(K4,K5,K6) ) CYCLE
                                 IF (K6 .EQ. K1) THEN  ! PENTA GEVONDEN
                                    IF (LNN(L)>1 .OR. LNN(LL)>1 .OR. LNN(LLL)>1 .OR.     &
                                       LNN(LLLL)>1 .OR. LNN(LLLLL) > 1 )  EXIT

                                    !  SPvdP: check and see if cell already exist
                                    if ( lnn(L).gt.0 .and. lnn(LL).gt.0 .and. lnn(LLL).gt.0 .and.  &
                                       lnn(LLLL).gt.0  .and. lnn(LLLLL).gt.0 ) then
                                    if ( lne(1,L).eq.lne(1,LL) .and. lne(1,L).eq.lne(1,LLL) .and.  &
                                       lne(1,L).eq.lne(1,LLLL) .and. lne(1,L).eq.lne(1,LLLLL) ) then
                                    cycle
                                    else   ! more expensive check
                                       kr(1)=k1; kr(2)=k2; kr(3)=k3; kr(4)=k4; kr(5)=k5
                                       Lr(1)=L; Lr(2)=LL; lr(3)=LLL; Lr(4)=LLLL; Lr(5)=LLLLL
                                       if ( alreadycell(5, kr, Lr) ) cycle
                                       !                                  do not allow folded cells when all links already have neighboring cells
                                       if ( kkk_.ne.1 .or. kkkk_.ne.1 .or. kkkkk_.ne.1 .or. kkkkkk_.ne.1 ) then
                                          cycle
                                       end if
                                    end if
                                    end if

                                    !CALL ALREADYPENTA(K1,K2,K3,K4,K5,JA) ; IF (JA > 0) EXIT

                                    kr(1)=k1; kr(2)=k2; kr(3)=k3; kr(4)=k4; kr(5)=k5
                                    if ( .not.iscounterclockwise(5, kr) ) cycle

                                    call increasenetcells(NUMP+1, 1.2, .true.)
                                    NUMP = NUMP + 1
                                    call realloc(netcell(NUMP)%NOD, 5, stat=ierr, keepExisting=.false.)
                                    call realloc(netcell(NUMP)%LIN, 5, stat=ierr, keepExisting=.false.)
                                    netcell(NUMP)%N = 5
                                    netcell(NUMP)%NOD(1)     = K1
                                    netcell(NUMP)%NOD(2)     = K2
                                    netcell(NUMP)%NOD(3)     = K3
                                    netcell(NUMP)%NOD(4)     = K4
                                    netcell(NUMP)%NOD(5)     = K5
                                    netcell(NUMP)%LIN(1)     = L
                                    netcell(NUMP)%LIN(2)     = LL
                                    netcell(NUMP)%LIN(3)     = LLL
                                    netcell(NUMP)%LIN(4)     = LLLL
                                    netcell(NUMP)%LIN(5)     = LLLLL
                                    LNN(L)                = LNN(L)     + 1
                                    LNN(LL)               = LNN(LL)    + 1
                                    LNN(LLL)              = LNN(LLL)   + 1
                                    LNN(LLLL)             = LNN(LLLL)  + 1
                                    LNN(LLLLL)            = LNN(LLLLL) + 1
                                    LNE(LNN(L),L)         = NUMP
                                    LNE(LNN(LL),LL)       = NUMP
                                    LNE(LNN(LLL),LLL)     = NUMP
                                    LNE(LNN(LLLL),LLLL)   = NUMP
                                    LNE(LNN(LLLLL),LLLLL) = NUMP
                                    ! SPvdP: linkmask deactivated with the purpose to find folded cells; check if cells already exist instead
                                    !                              LC(L)                 = 1 ; IF (KN(1,L)     == K2) LC(L)     = -1
                                    !                              LC(LL)                = 1 ; IF (KN(1,LL)    == K3) LC(LL)    = -1
                                    !                              LC(LLL)               = 1 ; IF (KN(1,LLL)   == K4) LC(LLL)   = -1
                                    !                              LC(LLLL)              = 1 ; IF (KN(1,LLLL)  == K5) LC(LLLL)  = -1
                                    !                              LC(LLLLL)             = 1 ; IF (KN(1,LLLLL) == K1) LC(LLLLL) = -1

                                    !                       cell found and administered: proceed
                                    cycle kklp

                                 ENDIF
                              ENDDO
                           ENDIF
                        ENDDO
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ENDDO kklp
      ENDIF
   ENDDO

   !CALL READYY ('FINDPENTAS', -1d0)

   RETURN

   END SUBROUTINE FINDPENTAS

   SUBROUTINE FINDHEXAS(jafold)

   use network_data
   use m_afmeting
   use m_alloc
   use m_sferic

   implicit none

   integer, intent(in) :: jafold  !< find folded cells (1), or not (0)

   integer :: ierr
   integer :: k1
   integer :: k2
   integer :: k3
   integer :: k4
   integer :: k5
   integer :: k6
   integer :: k7
   integer :: kk
   integer :: kkk
   integer :: kkkk
   integer :: kkkkk
   integer :: kkkkkk
   integer :: kkkkkkk
   integer :: kmod
   integer :: l
   integer :: ll
   integer :: lll
   integer :: llll
   integer :: lllll
   integer :: llllll
   integer :: kr(6), Lr(6)
   integer :: kkk_, kkkk_, kkkkk_, kkkkkk_, kkkkkkk_, nmkmax

   !LC LOGICAL rechtsaf
   !LC logical :: alreadycell
   !LC logical :: iscounterclockwise

   !  CALL READYY ('FINDHEXAS', 0d0)

   nmkmax = 1
   if ( jafold.eq.1 ) nmkmax = 1000

   KMOD = max(1,NUMK/100)
   DO K1 = 1,NUMK
      !IF (MOD(K1,KMOD) == 1) CALL READYY ('FINDHEXAS',dble(K1)/dble(NUMK))

      IF (KC(K1) == 1) THEN

         kklp:DO KK = 1,NMK(K1)
            L  = NOD(K1)%LIN(KK)
            IF (LNN(L) .GE. 2) CYCLE
            CALL OTHERNODECHK(K1,L,K2); IF (K2 ==0) CYCLE

            kkk = 1
            do while ( nod(k2)%lin(kkk).ne.L )
               kkk=kkk+1
            end do
            DO KKK_ = 1,min(NMK(K2),nmkmax)
               kkk = kkk-1
               if ( kkk.lt.1       ) kkk=kkk+nmk(k2)
               if ( kkk.gt.nmk(k2) ) kkk=kkk-nmk(k2)

               LL  = NOD(K2)%LIN(KKK)
               IF (LL .EQ. L) CYCLE
               IF (LNN(LL) .GE. 2) CYCLE
               CALL OTHERNODECHK(K2,LL,K3); IF (K3 ==0) CYCLE
               IF ( rechtsaf(K1,K2,K3) ) CYCLE
               IF (K3 .NE. K1) THEN

                  kkkk = 1
                  do while ( nod(k3)%lin(kkkk).ne.LL )
                     kkkk=kkkk+1
                  end do
                  DO KKKK_ = 1,min(NMK(K3),nmkmax)
                     kkkk = kkkk-1
                     if ( kkkk.lt.1       ) kkkk=kkkk+nmk(k3)
                     if ( kkkk.gt.nmk(k3) ) kkkk=kkkk-nmk(k3)


                     LLL  = NOD(K3)%LIN(KKKK)
                     IF (LLL .EQ. LL .OR. LLL .EQ. L) CYCLE
                     IF (LNN(LLL) .GE. 2) CYCLE
                     CALL OTHERNODECHK(K3,LLL,K4); IF (K4 ==0) CYCLE
                     IF ( rechtsaf(K2,K3,K4) ) CYCLE
                     IF (K4 .NE. K2 .AND. K4 .NE. K1) THEN

                        kkkkk = 1
                        do while ( nod(k4)%lin(kkkkk).ne.LLL )
                           kkkkk=kkkkk+1
                        end do
                        DO KKKKK_ = 1,min(NMK(K4),nmkmax)
                           kkkkk = kkkkk-1
                           if ( kkkkk.lt.1       ) kkkkk=kkkkk+nmk(k4)
                           if ( kkkkk.gt.nmk(k4) ) kkkkk=kkkkk-nmk(k4)

                           LLLL  = NOD(K4)%LIN(KKKKK)
                           IF (LLLL .EQ. LLL .OR. LLLL .EQ. LL .OR. LLLL .EQ. L) CYCLE
                           IF (LNN(LLLL) .GE. 2) CYCLE
                           CALL OTHERNODECHK(K4,LLLL,K5); IF (K5 ==0) CYCLE
                           IF ( rechtsaf(K3,K4,K5) ) CYCLE
                           IF (K5 .NE. K3 .AND. K5 .NE. K2 .AND. K5 .NE. K1) THEN

                              kkkkkk = 1
                              do while ( nod(k5)%lin(kkkkkk).ne.LLLL )
                                 kkkkkk=kkkkkk+1
                              end do
                              DO KKKKKK_ = 1,min(NMK(K5),nmkmax)
                                 kkkkkk = kkkkkk-1
                                 if ( kkkkkk.lt.1       ) kkkkkk=kkkkkk+nmk(k5)
                                 if ( kkkkkk.gt.nmk(k5) ) kkkkkk=kkkkkk-nmk(k5)

                                 LLLLL  = NOD(K5)%LIN(KKKKKK)
                                 IF (LLLLL .EQ. LLLL .OR. LLLLL .EQ. LLL .OR. LLLLL .EQ. LL .OR. LLLLL .EQ. L) CYCLE
                                 IF (LNN(LLLLL) .GE. 2) CYCLE
                                 CALL OTHERNODECHK(K5,LLLLL,K6); IF (K6 ==0) CYCLE
                                 IF ( rechtsaf(K4,K5,K6) ) CYCLE
                                 IF (K6 .NE. K4 .AND. K6 .NE. K3 .AND. K6 .NE. K2 .AND. K6 .NE. K1) THEN

                                    kkkkkkk = 1
                                    do while ( nod(k6)%lin(kkkkkkk).ne.LLLLL )
                                       kkkkkkk=kkkkkkk+1
                                    end do
                                    DO KKKKKKK_ = 1,min(NMK(K6),nmkmax)
                                       kkkkkkk = kkkkkkk-1
                                       if ( kkkkkkk.lt.1       ) kkkkkkk=kkkkkkk+nmk(k6)
                                       if ( kkkkkkk.gt.nmk(k6) ) kkkkkkk=kkkkkkk-nmk(k6)

                                       LLLLLL  = NOD(K6)%LIN(KKKKKKK)
                                       IF (LLLLLL .EQ. LLLLL .OR. LLLLLL .EQ. LLLL .OR.   &
                                          LLLLLL .EQ. LLL .OR. LLLLLL .EQ. LL .OR. LLLLLL .EQ. L) CYCLE
                                       IF (LNN(LLLLLL) .GE. 2) CYCLE
                                       CALL OTHERNODECHK(K6,LLLLLL,K7); IF (K7 ==0) CYCLE
                                       IF ( rechtsaf(K5,K6,K7) ) CYCLE
                                       IF (K7 .EQ. K1) THEN  ! HEXA GEVONDEN
                                          IF (LNN(L)>1 .OR. LNN(LL)>1 .OR. LNN(LLL)>1 .OR.      &
                                             LNN(LLLL)>1 .OR. LNN(LLLLL)>1 .OR. LNN(LLLLLL)>1)  EXIT

                                          !  SPvdP: check and see if cell already exist
                                          if ( lnn(L).gt.0 .and. lnn(LL).gt.0 .and. lnn(LLL).gt.0 .and.  &
                                             lnn(LLLL).gt.0 .and. lnn(LLLLL).gt.0 .and. lnn(LLLLLL).gt.0 ) then
                                          if ( lne(1,L).eq.lne(1,LL) .and. lne(1,L).eq.lne(1,LLL) .and.  &
                                             lne(1,L).eq.lne(1,LLLL) .and. lne(1,L).eq.lne(1,LLLLL) .and. lne(1,L).eq.lne(1,LLLLLL) ) then
                                          cycle
                                          else   ! more expensive check
                                             kr(1)=k1; kr(2)=k2; kr(3)=k3; kr(4)=k4; kr(5)=k5; kr(6)=k6
                                             Lr(1)=L; Lr(2)=LL; lr(3)=LLL; Lr(4)=LLLL; Lr(5)=LLLLL; Lr(6)=LLLLLL
                                             if ( alreadycell(6, kr, Lr) ) cycle
                                             !                                       do not allow folded cells when all links already have neighboring cells
                                             if ( kkk_.ne.1 .or. kkkk_.ne.1 .or. kkkkk_.ne.1 .or. kkkkkk_.ne.1 .or. kkkkkkk_.ne.1 ) then
                                                cycle
                                             end if
                                          end if
                                          end if

                                          !CALL ALREADYHEXA(K1,K2,K3,K4,K5,K6,JA) ; IF (JA > 0) EXIT

                                          kr(1)=k1; kr(2)=k2; kr(3)=k3; kr(4)=k4; kr(5)=k5; kr(6)=k6
                                          if ( .not.iscounterclockwise(6, kr) ) cycle

                                          call increasenetcells(NUMP+1, 1.2, .true.)
                                          NUMP = NUMP + 1
                                          call realloc(netcell(NUMP)%NOD, 6, stat=ierr, keepExisting=.false.)
                                          call realloc(netcell(NUMP)%LIN, 6, stat=ierr, keepExisting=.false.)
                                          netcell(NUMP)%N = 6
                                          netcell(NUMP)%NOD(1)       = K1
                                          netcell(NUMP)%NOD(2)       = K2
                                          netcell(NUMP)%NOD(3)       = K3
                                          netcell(NUMP)%NOD(4)       = K4
                                          netcell(NUMP)%NOD(5)       = K5
                                          netcell(NUMP)%NOD(6)       = K6
                                          netcell(NUMP)%LIN(1)       = L
                                          netcell(NUMP)%LIN(2)       = LL
                                          netcell(NUMP)%LIN(3)       = LLL
                                          netcell(NUMP)%LIN(4)       = LLLL
                                          netcell(NUMP)%LIN(5)       = LLLLL
                                          netcell(NUMP)%LIN(6)       = LLLLLL
                                          LNN(L)                  = LNN(L)      + 1
                                          LNN(LL)                 = LNN(LL)     + 1
                                          LNN(LLL)                = LNN(LLL)    + 1
                                          LNN(LLLL)               = LNN(LLLL)   + 1
                                          LNN(LLLLL)              = LNN(LLLLL)  + 1
                                          LNN(LLLLLL)             = LNN(LLLLLL) + 1
                                          LNE(LNN(L),L)           = NUMP
                                          LNE(LNN(LL),LL)         = NUMP
                                          LNE(LNN(LLL),LLL)       = NUMP
                                          LNE(LNN(LLLL),LLLL)     = NUMP
                                          LNE(LNN(LLLLL),LLLLL)   = NUMP
                                          LNE(LNN(LLLLLL),LLLLLL) = NUMP

                                          ! SPvdP: linkmask deactivated with the purpose to find folded cells; check if cells already exist instead
                                          !                                   LC(L)                   = 1 ; IF (KN(1,L)      == K2) LC(L)      = -1
                                          !                                   LC(LL)                  = 1 ; IF (KN(1,LL)     == K3) LC(LL)     = -1
                                          !                                   LC(LLL)                 = 1 ; IF (KN(1,LLL)    == K4) LC(LLL)    = -1
                                          !                                   LC(LLLL)                = 1 ; IF (KN(1,LLLL)   == K5) LC(LLLL)   = -1
                                          !                                   LC(LLLLL)               = 1 ; IF (KN(1,LLLLL)  == K6) LC(LLLLL)  = -1
                                          !                                   LC(LLLLLL)              = 1 ; IF (KN(1,LLLLLL) == K1) LC(LLLLLL) = -1

                                          !                                  cell found and administered: proceed
                                          cycle kklp
                                       ENDIF
                                    ENDDO
                                 ENDIF
                              ENDDO
                           ENDIF
                        ENDDO
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ENDDO kklp
      ENDIF
   ENDDO

   !CALL READYY ('FINDHEXAS', -1d0)

   RETURN
   END SUBROUTINE FINDHEXAS

   ! check if cell is counterclockwise
   logical function iscounterclockwise(N, K)
   !LC use m_netw

   use network_data
   use m_missing,  only: dmiss
   use geometry_module, only: comp_masscenter
   use m_sferic, only: jsferic, jasfer3D

   implicit none

   integer,               intent(in) :: N  !< number of links and nodes
   integer, dimension(N), intent(in) :: K  !< node set

   integer,                parameter :: MMAX = 10
   double precision, dimension(MMAX) :: xv, yv

   double precision                  :: xdum, ydum

   double precision                  :: darea
   integer                           :: jacounterclockwise          ! counterclockwise (1) or not (0)
   integer                           :: i, ip1, kk, kkp1

   iscounterclockwise = .true.

   !
   !    darea = 0d0
   !    do i=1,N
   !       ip1 = i+1; if ( ip1.gt.N ) ip1=ip1-N
   !       kk   = K(i)
   !       kkp1 = K(ip1)
   !       darea = darea + xk(kk) * (yk(kkp1)-yk(kk)) - yk(kk) * (xk(kkp1)-xk(kk))
   !    end do
   !    darea = 0.5d0*darea   ! not really necessary
   !
   !    if ( darea.le.0d0 ) then
   !       iscounterclockwise = .false.
   !    end if

   do i=1,N
      kk = K(i)
      xv(i) = xk(kk)
      yv(i) = yk(kk)
   end do

   call comp_masscenter(N, xv, yv, xdum, ydum, darea, jacounterclockwise, jsferic, jasfer3D, dmiss)
   if ( jacounterclockwise.eq.1 ) then
      iscounterclockwise = .true.
   else
      iscounterclockwise = .false.
   end if

   return
   end function iscounterclockwise

   LOGICAL FUNCTION rechtsaf(K1,K2,K3)
   integer, intent(in) :: K1, K2, K3
   rechtsaf = .false.
   end FUNCTION rechtsaf

   SUBROUTINE CONNECTDBN(K1,K2,LNU)
   implicit none
   integer :: K1, K2, LNU
   if (k1 == k2) return
   CALL CONNECTDB(K1,K2,lnu)
   CALL ADDLINKTONODES(K1,K2,LNU)
   RETURN
   END SUBROUTINE CONNECTDBN

   SUBROUTINE CONNECTDB(K1,K2,lnu) ! fast version without refinement

   use network_data
   implicit none
   integer :: K1, K2, LNU

   integer :: l

   DO L = NUML,1,-1
      IF (KN(1,L) .EQ. K1 .AND. KN(2,L) .EQ. K2 .OR.    &
         KN(1,L) .EQ. K2 .AND. KN(2,L) .EQ. K1 ) THEN
      ! CALL CONFRM('POINTS ALREADY CONNECTED, CONTINUE', JA)
      ! IF (JA .NE. 1) RETURN
      LNU = L
      RETURN
      ENDIF
   ENDDO

   LNU  = 0
   DO L = NUML,1,-1
      IF (KN(1,L) .EQ. 0) THEN
         LNU = L
         EXIT
      ENDIF
   ENDDO

   IF (LNU == 0) THEN ! NO FREE NR
      NUML = NUML + 1
      LNU  = NUML
      IF (NUML >= LMAX) THEN
         CALL INCREASENETW(NUMK,NUML)
      ENDIF
   ENDIF

   kn(1,lnu) = k1 ; kn(2,lnu) = k2 ; kn(3,lnu) = KN3TYP  ! cheap version only to be used icm setnodadm
   ! mark link as active
   lc(lnu) = 1

   RETURN
   END SUBROUTINE CONNECTDB

   !> Adds a new netlink, with as little as possible bookkeeping costs.
   !! After a sequence of calls to this subroutine, the network_data must be
   !! updated by calling setnodadm().
   subroutine connectdbfast(k1,k2,lnu)
   use network_data
   implicit none
   integer, intent(in   ) :: k1  !< Index of first netnode (must exist already).
   integer, intent(in   ) :: k2  !< Index of second netnode (must exist already).
   integer, intent(  out) :: lnu !< Result new netlink number (will be placed at end numl+1 of link table).

      if (k1 == k2) return

      numl = numl+1
      if (numl >= lmax) then
         call increasenetw(numk,numl)
      endif
      lnu = numl
            
      call addlinktonodes(k1,k2,lnu)
      lc(lnu) = 1

   end subroutine connectdbfast

   SUBROUTINE ADDLINKTONODES(KL,KR,LNU)
   use network_data
   implicit none
   integer :: KL, KR, LNU

   KN(1,LNU) = KL
   KN(2,LNU) = KR
   NMK(KL)   = NMK(KL) + 1
   NMK(KR)   = NMK(KR) + 1
   CALL SETNODLIN(KL,NMK(KL),LNU)
   CALL SETNODLIN(KR,NMK(KR),LNU)
   IF (KC(KL) .EQ. 0) KC(KL) = 1
   IF (KC(KR) .EQ. 0) KC(KR) = 1
   RETURN

   END SUBROUTINE ADDLINKTONODES

   SUBROUTINE SETNODLIN(K,LK,L)
   !LC use m_netw
   use network_data
   implicit none
   integer :: K, LK, L

   CALL CHKLINSIZTONODE(K)
   NOD(K)%LIN(LK) = L
   RETURN

   END SUBROUTINE SETNODLIN

   SUBROUTINE CHKLINSIZTONODE(KK)
   use network_data
   implicit none
   integer :: KK

   integer :: ierr
   integer :: knxk

   INTEGER, ALLOCATABLE :: IH(:)
   if ( allocated(nod(kk)%lin) ) then   ! SPvdP: nod(kk)%lin may not have been allocated
      KNXK = SIZE(NOD(KK)%LIN)
      IF (NMK(KK) .GT. KNXK) THEN
         ALLOCATE (IH(KNXK),STAT=IERR)
         IH(1:KNXK) = NOD(KK)%LIN(1:KNXK)
         DEALLOCATE (NOD(KK)%LIN)
         ALLOCATE   (NOD(KK)%LIN(KNXK+1),STAT=IERR) ; NOD(KK)%LIN = 0
         NOD(KK)%LIN(1:KNXK) = IH(1:KNXK)
         DEALLOCATE(IH)
      ENDIF
   else
      ALLOCATE (NOD(KK)%LIN(1),STAT=IERR) ; NOD(KK)%LIN = 0
   end if
   RETURN
   END SUBROUTINE CHKLINSIZTONODE


   !> Constructs the connectivity table of netlinks that form each netcell.
   !! Construction is based on given vertex nodes of each cell.
   !! Resulting array could be used on call site to fill netcell%lin(:).
   subroutine ggeo_construct_netcelllin_from_netcellnod(numcell, netcellnod, netcelllin)
      use network_data
      implicit none

      integer, intent(in   ) :: numcell          !< Number of cells in table (may be used to only operate on a piece of the connectivity tables, e.g., only the 2D cells), typically called with global nump.
      integer, intent(in   ) :: netcellnod(:,:)  !< connectivity table with vertex nodes for all netcells (faces)
      integer, intent(  out) :: netcelllin(:,:)  !< Resulting connectivity table with side edges for all netcells (faces), in same order as the nodes.

      integer :: N, ik, inext, kcur, knext, nv
      nv = size(netcellnod, 1)

      do N=1,numcell
         do ik=1,nv
            inext = mod(ik,nv)+1
            if (netcellnod(inext, N) <= 0) then
               inext = 1
            end if

            call getlink_between_nodes(netcellnod(ik, N), netcellnod(inext, N), netcelllin(ik, N))

            if (inext == 1) then
               exit ! Round completed for this cell
            end if
         end do
      end do
   end subroutine ggeo_construct_netcelllin_from_netcellnod

   SUBROUTINE INCELLS(XA,YA,KIN)
   !use m_netw
   use network_data
   use geometry_module, only: pinpok
   use m_missing, only : jins, dmiss

   implicit none
   double precision :: xa
   double precision :: ya
   integer :: kin

   integer :: in
   integer :: k
   integer :: k1
   integer :: n
   integer :: nn
   double precision :: XH(6), YH(6)
   KIN = 0
   DO K = 1,NUMP
      NN = netcell(K)%N
      XH = dmiss
      YH = dmiss
      DO N = 1,NN
         K1 = netcell(K)%NOD(N)
         XH(N) = XK(K1) ; YH(N) = YK(K1)
      ENDDO
      CALL PINPOK(XA, YA , NN, XH, YH, IN, jins, dmiss)
      IF (IN == 1) THEN
         KIN = K
         RETURN
      ENDIF
   ENDDO
   END SUBROUTINE INCELLS

   
   subroutine incells_kdtree(xa, ya, kin, treeinst, searchradiussq)
   !use m_netw
   use network_data
   use geometry_module, only: pinpok
   use m_missing, only : jins, dmiss
   use m_sferic, only: jsferic
   use kdtree2Factory
   implicit none
   double precision,      intent(in   ) :: xa             !< Input/query point x-coordinate
   double precision,      intent(in   ) :: ya             !< Input/query point y-coordinate
   integer,               intent(  out) :: kin            !< netcell index found (0 when not found)
   type(kdtree_instance), intent(inout) :: treeinst       !< k-d tree for searching netcell mass centers
   double precision,      intent(in   ) :: searchradiussq !< Squared search radius for k-d tree searching

   integer :: nCellsInSearchRadius
   integer :: ir

   kin = 0 ! Reset cell finder
 
   call make_queryvector_kdtree(treeinst, Xa, Ya, jsferic)
   nCellsInSearchRadius = kdtree2_r_count(treeinst%tree, treeinst%qv, searchradiussq)

   if (nCellsInSearchRadius == 0) return
   !reallocate if necessary
   call realloc_results_kdtree(treeinst, nCellsInSearchRadius)
   !find nearest cells
   call kdtree2_n_nearest(treeinst%tree,treeinst%qv,nCellsInSearchRadius,treeinst%results)

   do ir = 1, nCellsInSearchRadius
      kin = treeinst%results(ir)%idx
      if (kin <= 0) cycle

      if (is_in_netcell(xa, ya, kin)) then
         ! Cell found, exit
         exit
      end if
   end do
   end subroutine incells_kdtree


   !> Checks whether a given point lies inside one particular given 2D netcell.
   !!
   !! @see incells for the looped/search variant, which searches all netcells.
   function is_in_netcell(x, y, k)
   use network_data, only: nump, netcell, xk, yk
   use geometry_module, only: pinpok
   use m_missing, only : jins, dmiss
   implicit none
   double precision, intent(in   ) :: x !< Query point x coordinate
   double precision, intent(in   ) :: y !< Query point y coordinate
   integer,          intent(in   ) :: k !< Netcell index to check
   logical                         :: is_in_netcell

   integer :: nn, k1, n, in
   double precision :: xh(6), yh(6)

   is_in_netcell = .false.
   
   if (k <= 0 .or. k >= nump) then
      return
   end if

   nn = netcell(k)%n
   xh = dmiss
   yh = dmiss
   do n = 1,nn
      k1 = netcell(k)%nod(n)
      xh(n) = xk(k1) ; yh(n) = yk(k1)
   enddo
   call pinpok(x, y , nn, xh, yh, in, jins, dmiss)
   
   is_in_netcell = (in == 1)
   end function is_in_netcell

   !> sort links in nod%lin counterclockwise (copy-paste from setnodadm)
   subroutine sort_links_ccw(k,maxlin)
   !LC use m_netw
   use network_data
   use m_sferic
   use geometry_module, only: getdxdy, dcosphi, getdx, getdy
   use stdlib_sorting, only: sort_index

   implicit none

   integer,          intent(in)    :: k                           !< node number
   integer,          intent(in)    :: maxlin                      !< array size


   double precision, allocatable   :: arglin(:)                   ! dummy array
   integer,          allocatable   :: linnrs(:), inn(:)           ! dummy arrays

   integer                         :: k1, k2, L, LL

   integer                         :: jDupLinks, jOverlapLinks, jSmallAng
   double precision                :: sl, sm, xcr, ycr, phi0

   double precision                :: phi, dx, dy, dmaxcosp, dcosp, costriangleminangle

   allocate(arglin(maxlin), linnrs(maxlin), inn(maxlin))
   do L=1,NMK(K)
      K1 = KN(1,nod(K)%lin(L)); K2 = KN(2,nod(K)%lin(L))
      if (K2 == K) then
         K2 = K1
         K1 = K
      end if

      !dx = getdx(xk(k1), yk(k1), xk(k2), yk(k2))
      !dy = getdy(xk(k1), yk(k1), xk(k2), yk(k2))
      call getdxdy(xk(k1), yk(k1), xk(k2), yk(k2),dx,dy,jsferic)
      if (abs(dx) < 1d-14 .and. abs(dy) < 1d-14) then
         if (dy < 0) then
            phi = -pi/2
         else
            phi = pi/2
         end if
      else
         phi = atan2(dy, dx)
      end if
      if ( L.eq.1 ) then
         phi0 = phi
      end if

      arglin(L) = phi-phi0
      if ( arglin(L).lt.0d0 ) arglin(L) = arglin(L) + 2d0*pi
   end do

   call sort_index(arglin(1:nmk(k)), inn(1:nmk(k)))

   linnrs(1:nmk(k)) = nod(k)%lin(1:nmk(k))
   do L=1,nmk(k)
      nod(k)%lin(L) = linnrs(inn(L))
   end do

   return
   end subroutine sort_links_ccw

   !> get netcell polygon that is safe for periodic, spherical coordinates and poles
   subroutine get_cellpolygon(n, Msize, nn, rcel, xv, yv, LnnL, Lorg, zz)
   use network_data
   use m_missing, only : dmiss
   use m_sferic
   implicit none

   integer,                            intent(in)  :: n      !< cell number
   integer,                            intent(in)  :: Msize  !< array size
   integer,                            intent(out) :: nn     !< polygon size
   double precision,                   intent(in)  :: rcel   !< cell enlargement factor around cell center
   double precision, dimension(Msize), intent(out) :: xv, yv !< polygon coordinates
   integer,          dimension(Msize), intent(out) :: LnnL   !< original link LnnL
   integer,          dimension(Msize), intent(out) :: Lorg   !< original link number (>0) or added link (0)
   double precision,                   intent(out) :: zz     !< polygon-averaged value

   integer,          dimension(Msize)              :: kpole
   integer                                         :: num, numz, m, mp1, mp2, k1, k2, k3

   !  initialization
   xv   = 0d0
   yv   = 0d0
   LnnL = 0
   Lorg = 0
   zz   = 0d0
   nn   = netcell(n)%n

   if ( nn.lt.3 ) then
      return  ! safety
   end if

   !  check for poles
   kpole = 0
   if ( jsferic.eq.1 ) then
      do m=1,nn
         k1 = netcell(n)%nod(m)
         if ( abs(abs(yk(k1))-90d0).lt.dtol_pole ) then
            kpole(m) = 1
         end if
      end do
   end if

   zz = 0d0
   num = 0 ! number of nodes in polygon
   numz = 0
   do m  = 1,nn
      !num       = num+1
      !k1        = netcell(n)%NOD(m)
      !xv(num)   = xk(k1)
      !yv(num)   = yk(k1)
      !zz        = zz + zk(k1)
      !lnnl(num) = LNN(netcell(n)%lin(m))

      mp1 = m+1; if ( mp1.gt.nn ) mp1=mp1-nn
      k1 = netcell(n)%nod(m)
      k2 = netcell(n)%nod(mp1)

      if ( kpole(m).eq.1 .and. kpole(mp1).ne.1 ) then
         num = num+1
         xv(num) = xk(k2)
         yv(num) = yk(k1)
         if ( zk(k1).ne.DMISS ) then
            numz = numz+1
            zz = zz + zk(k1)
         end if
         lnnl(num) = LNN(netcell(n)%lin(m))
         Lorg(num) = m
      else if (  kpole(m).ne.1 .and. kpole(mp1).eq.1 ) then
         num = num+1
         xv(num) = xk(k1)
         yv(num) = yk(k1)
         if ( zk(k1).ne.DMISS ) then
            numz = numz+1
            zz   = zz + zk(k1)
         end if
         lnnl(num) = LNN(netcell(n)%lin(m))
         Lorg(num) = m

         !        add dummy link on pole ("unmerge")
         num = num+1
         xv(num) = xk(k1)
         yv(num) = yk(k2)
         if ( zk(k2).ne.DMISS ) then
            numz = numz+1
            zz   = zz + zk(k2)
         end if
         !         lnnl(num) = LNN(netcell(n)%lin(m))
         lnnl(num) = 1  ! fictitious boundary netlink

         !        use already existing dummy link if possible
         mp2 = mp1+1; if ( mp2.gt.nn) mp2=mp2-nn
         if ( kpole(mp2).eq.1 ) then
            Lorg(num) = mp1
         else  ! add dummy link
            Lorg(num) = 0
         end if
      else if ( kpole(m).ne.1 .and. kpole(mp1).ne.1 ) then
         num       = num+1
         k1        = netcell(n)%NOD(m)
         xv(num)   = xk(k1)
         yv(num)   = yk(k1)
         if ( zk(k1).ne.DMISS ) then
            numz = numz+1
            zz   = zz + zk(k1)
         end if
         lnnl(num) = LNN(netcell(n)%lin(m))
         Lorg(num) = m
      end if
   enddo
   nn = num

   if (numz.eq.0) then
      zz = DMISS
   else
      zz = zz / numz
   endif

   if (rcel /= 1d0) then
      do m=1,nn
         xv(m) = xzw(n) + rcel*(xv(m) - xzw(n))
         yv(m) = yzw(n) + rcel*(yv(m) - yzw(n))
      end do
   end if
      
   !  check periodicity
   if ( jsferic.eq.1 ) then
      do m=1,nn
         mp1 = m+1; if ( mp1.gt.nn ) mp1=mp1-nn
         if ( xv(mp1)-xv(m).gt.180d0 ) then
            xv(mp1) = xv(mp1) - 360d0
         else if ( xv(mp1)-xv(m).lt.-180d0 ) then
            xv(mp1) = xv(mp1) + 360d0
         end if
         ! TODO: SvdP: need sferic check for y coord as well, after the above new RCEL addition? [AvD]
      end do
   end if

   return
   end subroutine get_cellpolygon
   
   SUBROUTINE CLOSETO1Dnetnode(XP1,YP1,N1,dist,oneDMask) !

   use network_data
   use geometry_module, only: dbdistance
   use m_sferic
   use m_missing

   implicit none
   double precision, intent(in)  :: XP1, YP1
   double precision, intent(out) :: dist     ! find 1D point close to x,y:
   integer         , intent(out) :: n1       !1D point found
   integer, optional, intent(in) :: oneDMask(:)


   double precision :: dismin
   integer          :: ja, k, k1, k2, L, k1ClientIndex, k2ClientIndex
   double precision :: dis,dis1,dis2
   logical                       :: validOneDMask
   
   validOneDMask = .false.
   if(present(oneDMask)) then
      if(size(oneDMask,1).gt.0) then
         validOneDMask = .true.
      endif
   endif
   

   N1 = 0
   DISMIN = 9E+33
   DO L = 1,numl
      IF (kn(3,L) == 1 .or. kn(3,L) == 6) then !  .or. kn(3,L) == 4) THEN
         K1 = kn(1,L) ; K2 = kn(2,L)         
         ! If mask is present we check that the 1d nodes are the nodes I want to connect
         if (validOneDMask) then  
            k1ClientIndex = mesh1dInternalToClientMapping(k1)
            k2ClientIndex = mesh1dInternalToClientMapping(k2)
            if (oneDMask(k1ClientIndex).eq.0) then !! Fortran does not support logical and
               dis1 = DISMIN 
            endif
            if (oneDMask(k2ClientIndex).eq.0) then 
               dis2 = DISMIN 
            endif
            if (oneDMask(k1ClientIndex).eq.1) then 
               dis1 =  dbdistance(XP1,YP1,Xk(K1),Yk(K1),jsferic, jasfer3D, dmiss)    
            endif
            if (oneDMask(k2ClientIndex).eq.1) then 
               dis2 =  dbdistance(XP1,YP1,Xk(K2),Yk(K2),jsferic, jasfer3D, dmiss)    
            endif            
         else
            dis1 = dbdistance(XP1,YP1,Xk(K1),Yk(K1),jsferic, jasfer3D, dmiss)
            dis2 = dbdistance(XP1,YP1,Xk(K2),Yk(K2),jsferic, jasfer3D, dmiss)    
         endif
         if (dis1 < dis2) then
            k = k1 ; dis = dis1
         else
            k = k2 ; dis = dis2
         endif
         if (DIS .LT. DISMIN) then
            N1 = k
            DISMIN = DIS
         endif
      endif
   enddo
   dist = dismin
   END SUBROUTINE CLOSETO1Dnetnode


   SUBROUTINE CLOSETO1Dnetnode_kdtree(XP1,YP1,N1,dist, treeinst, searchradiussq, k1d, oneDMask) !

   use network_data
   use geometry_module, only: dbdistance
   use m_sferic
   use m_missing
   use kdtree2Factory
   use Timers

   implicit none
   double precision,      intent(in   ) :: XP1, YP1 !< Input/query point coordinates
   double precision,      intent(  out) :: dist     !< Distance between query point and the 1D point found
   integer         ,      intent(  out) :: n1       !< 1D point found
   type(kdtree_instance), intent(inout) :: treeinst !< k-d tree for searching 1D net nodes
   double precision,      intent(in   ) :: searchradiussq !< Squared search radius for k-d tree searching
   integer,               intent(in   ) :: k1d(:)   !< Mapping table from purely 1D net nodes to full set of netnodenumbers (1:numk1d) ==> (1:numk)
   integer, optional,     intent(in   ) :: oneDMask(:)


   double precision :: dismin
   integer          :: ja, k, k1, k1ClientIndex
   double precision :: dis
   logical          :: validOneDMask
   integer, save :: timerhandle(2) = 0
   
   integer :: nNodesInSearchRadius
   integer :: ir

   validOneDMask = .false.
   if(present(oneDMask)) then
      if(size(oneDMask,1).gt.0) then
         validOneDMask = .true.
      endif
   endif
   

   if (timon) call timstrt('closeto1dnetnode_kdtree: treeops', timerhandle(1))
   N1 = 0
   DISMIN = 9E+33
   call make_queryvector_kdtree(treeinst, xp1, yp1, jsferic)
   nNodesInSearchRadius = kdtree2_r_count(treeinst%tree, treeinst%qv, searchradiussq)

   if ( nNodesInSearchRadius == 0 ) then
      if (timon) call timstop(timerhandle(1))
      return
   end if
   
   !reallocate if necessary
   call realloc_results_kdtree(treeinst, nNodesInSearchRadius)
   !find nearest cells
   call kdtree2_n_nearest(treeinst%tree,treeinst%qv,nNodesInSearchRadius,treeinst%results)
   if (timon) call timstop(timerhandle(1))
   if (timon) call timstrt('closeto1dnetnode_kdtree: loop', timerhandle(2))

   k = 0 ! Reset node finder
   do ir = 1, nNodesInSearchRadius
      k = treeinst%results(ir)%idx
      if (k <= 0) cycle
      k1 = k1d(k)

      if (validOneDMask) then  
         k1ClientIndex = mesh1dInternalToClientMapping(k1)
         if (oneDMask(k1ClientIndex) == 0) then !! Fortran does not support logical and
            cycle
         endif
         if (oneDMask(k1ClientIndex).eq.1) then 
            dis =  dbdistance(XP1,YP1,Xk(K1),Yk(K1),jsferic, jasfer3D, dmiss)    
         endif
      else
         dis = dbdistance(XP1,YP1,Xk(K1),Yk(K1),jsferic, jasfer3D, dmiss)
      endif
      if (dis < dismin) then
         n1 = k1
         dismin = dis
      endif
   end do
   if (timon) call timstop(timerhandle(2))
   dist = dismin
   END SUBROUTINE CLOSETO1Dnetnode_kdtree

   !-----------------------------------------------------------------!
   ! rest.f90
   !-----------------------------------------------------------------!



   !-----------------------------------------------------------------!
   ! unstruct.F90
   !-----------------------------------------------------------------!

   !> Computes the bottom area of a cell and the center of mass coordinates.
   subroutine getcellsurface( n, ba, xzwr, yzwr) ! bottom area of cell nr n                       ! todo : sferic

   !lc use m_netw
   use network_data
   use m_missing, only : dmiss
   use geometry_module, only: comp_masscenter
   use m_sferic

   implicit none
   double precision :: ba, xzwr, yzwr
   integer          :: n

   ! locals
   integer          :: nn

   integer,                parameter :: MMAX=10   ! maximum cell polygon size
   double precision, dimension(MMAX) :: xh, yh    ! cell polygon node coordinates
   integer,          dimension(MMAX) :: LnnL      ! cell polygon link Lnn (not used here)
   integer,          dimension(MMAX) :: Lorg      ! cell polygon link number (not used here)
   double precision                  :: zz
   integer                           :: jaccw     ! counterclockwise (1) or not (0) (not used here)

   call get_cellpolygon(n,Mmax,nn,1d0,xh,yh,LnnL,Lorg,zz)
   call comp_masscenter(nn, xh , yh, xzwr, yzwr, ba, jaccw,  jsferic, jasfer3D, dmiss)

   end subroutine getcellsurface

   !> computes the cell-weighted center
   subroutine getcellweightedcenter(n, xz, yz, zz)

   use m_ggeo_orthosettings
   use m_missing,  only: jins, dmiss, dxymis
   use geometry_module, only : getcircumcenter, comp_circumcenter3D, comp_masscenter
   use network_data, only: netcell, xk, yk, zk, dcenterinside
   use m_sferic

   implicit none
   double precision   :: xz, yz, zz
   integer            :: n

   integer,                parameter :: MMAX = 10
   double precision, dimension(MMAX) :: xv, yv
   integer,          dimension(MMAX) :: Lorg
   integer,          dimension(MMAX) :: LnnL
   integer                           :: nn
   integer                           :: jaccw  ! counterclockwise (1) or not (0) (not used here)
   integer                           :: i, k, k2, k3, iv, ih

   double precision                  :: ba, xzw, yzw, xh(4), yh(4)

   if ( jsferic.eq.1 .and. jasfer3D.eq.1 ) then
      nn = netcell(n)%N
      do i=1,nn
         k = netcell(n)%nod(i)
         xv(i) = xk(k)
         yv(i) = yk(k)
      end do

      call comp_circumcenter3D(nn, xv, yv, xz, yz, jsferic, dmiss, dcenterinside)

      if (nn == 3) then ! test: triangle both horizontal and vertical edges present => rectangle circumcentre
         iv = 0 ; ih = 0
         do k  = 1,3
            k2 = k+ 1 ; if (k==3) k2 = 1
  !          if ( xv(k) == xv(k2) ) then 
            if ( abs( xv(k) - xv(k2) ) < 1d-10 ) then 
                 iv = k 
  !          else if (yv(k) == yv(k2) ) then 
            else if ( abs( yv(k) - yv(k2) ) < 1d-10 ) then 
                 ih = k
            endif
         enddo
         if ( iv .ne. 0 .and. ih .ne. 0) then 
            xh(1) = minval(xv(1:3)) ; yh(1) = minval(yv(1:3)) 
            xh(2) = maxval(xv(1:3)) ; yh(2) = minval(yv(1:3)) 
            xh(3) = maxval(xv(1:3)) ; yh(3) = maxval(yv(1:3)) 
            xh(4) = minval(xv(1:3)) ; yh(4) = maxval(yv(1:3)) 
            call comp_circumcenter3D(4, xh, yh, xz, yz, jsferic, dmiss, dcenterinside)
         endif

      endif

   else
      !   get the cell polygon that is safe for periodic, spherical coordinates, inluding poles
      call get_cellpolygon(n,Mmax,nn,1d0,xv,yv,LnnL,Lorg,zz)
      call getcircumcenter(nn, xv, yv, lnnl, xz, yz, jsferic, jasfer3D, jglobe, jins, dmiss, dxymis, dcenterinside)
   end if

   if (circumormasscenter .ne. 1d0) then
      !   update with cell mass center
      call comp_masscenter(nn, xv, yv, xzw, yzw, ba, jaccw, jsferic, jasfer3D, dmiss)

      xz = circumormasscenter*xz + (1d0-circumormasscenter)*xzw
      yz = circumormasscenter*yz + (1d0-circumormasscenter)*yzw
   endif
   ! CALL CIRR(XZ,YZ,31)

   end subroutine getcellweightedcenter



   !-----------------------------------------------------------------!
   ! Library public functions
   !-----------------------------------------------------------------!
   function make1D2Dinternalnetlinks(xplLinks, yplLinks, zplLinks, oneDMask, inNet ) result(ierr)

   use m_cell_geometry, only: xz, yz
   use network_data
   use m_alloc
   use m_missing, only:  dmiss, dxymis, jadelnetlinktyp, jins
   use geometry_module, only: dbdistance, normalout, dbpinpol
   use m_sferic, only: jsferic, jasfer3D  
   use kdtree2Factory
   use Timers
  
   implicit none

   !input 
   double precision, optional, intent(in) :: xplLinks(:), yplLinks(:), zplLinks(:) ! optional polygons to reduce the area where the 1D2Dlinks are generated
   integer, optional,          intent(in) :: oneDMask(:)                           !< Masking array for 1d mesh points. This array has the client node ordering
   integer, optional,          intent(in) :: inNet                                 !< Whether or not (1/0) to generate links only for 1D points that lie inside of 2D grid cells. Default: off, 0.
   
   !locals
   integer                                :: K1, K2, K3, L, NC1, NC2, JA, KK2(2), KK, NML, LL
   integer                                :: i, ierr, k, kcell
   double precision                       :: XN, YN, XK2, YK2, WWU
   integer                                :: insidePolygons, Lfound, k1ClientIndex, k2ClientIndex 
   integer                                :: inNet_
   logical                                :: validOneDMask
   integer :: ifil
   integer :: timerhandle(6)
   type(kdtree_instance) :: treeinstcells
   double precision :: searchradiussq

   integer :: jakdtree = 1

   timerhandle = 0

   call init_gridoperations()

   if (timon) call timstrt('make1D2Dinternalnetlinks', timerhandle(1))
   
   validOneDMask = .false.
   if(present(oneDMask)) then
      if(size(oneDMask,1).gt.0) then
         validOneDMask = .true.
      endif
   endif

   ierr = 0   
   if (present(inNet)) then
      inNet_ = inNet
   else
      inNet_ = 0
   end if

   i = size(xk) ; deallocate(kc) ; allocate(kc(i))
   call savenet()

   if (timon) call timstrt('findcells', timerhandle(2))
   call findcells(0)
   if (timon) call timstop(timerhandle(2))

   if (timon) call timstrt('make k-d tree', timerhandle(3))
   if (jakdtree == 1) then
      ierr = constructNetcellKdTree(treeinstcells, searchradiussq)
      searchradiussq = 1.1d0 * searchradiussq**2
      if (ierr /= 0) then
         ! Fall back to without k-d tree
         jakdtree = 0
      end if
   end if
   if (timon) call timstop(timerhandle(3))
   
   if(allocated(connectionIndexes)) then
      deallocate(connectionIndexes)
   endif
   allocate(connectionIndexes(2,nump))
   connectionIndexes = 0
   numValidLinks = 0

   if (timon) call timstrt('mask 1D nodes', timerhandle(6))

   KC = 2
   do L = 1,NUML  ! FLAG TO 1 ANY NODE TOUCHED BY SOMETHING 1D
      K1  = KN(1,L) ; K2  = KN(2,L); K3 = KN(3,L)
      IF (K3 .NE. 4 .AND. K3 .NE. 2 .AND. K3 .NE. 0) THEN ! only for yet-isolated 1D channels with KN(3,L)==1         
         KC(K1) = 1 ; KC(K2) = 1
         if (jadelnetlinktyp == 5 .or. jadelnetlinktyp == 7) then 
            do k  = 1,nmk(k1)
               LL = nod(k1)%lin(k)
               if (kn(3,LL) == 5 .or. kn(3,LL) == 7) then 
                  kc(k1) = 2 ; exit  ! when already connected by pipe forget it
               endif   
            enddo   
            do k  = 1,nmk(k2)
               LL = nod(k2)%lin(k)
               if (kn(3,LL) == 5 .or. kn(3,LL) == 7) then 
                  kc(k2) = 2 ; exit  ! when already connected by pipe forget it
               endif   
            enddo   
         endif
         !Account for oneDMask if present. Assumption is that oneDmask contains 1/0 values.
         if (validOneDMask) then
            k1ClientIndex = mesh1dInternalToClientMapping(k1)
            k2ClientIndex = mesh1dInternalToClientMapping(k2)            
            if (oneDMask(k1ClientIndex).ne.1) then
                kc(k1) = 2
            endif
            if (oneDMask(k2ClientIndex).ne.1) then
                kc(k2) = 2
            endif
         endif 
      endif
      
      ! for 1D channels: do not connect endnodes to 2D, they should remain boundary candidates
      if (k3 == 1) then
          if (nmk(k1) == 1) then
              kc(k1) = 2
          endif
          if (nmk(k2) == 1) then
              kc(k2) = 2
          endif
      endif
   enddo
   if (timon) call timstop(timerhandle(6))

   if (jadelnetlinktyp .ne. 0) then
      kn3typ = jadelnetlinktyp
   else
      kn3typ = 3
   endif
   
   NML  = NUML
   DO K = 1,NUMK

!      IF ((NMK(K) == 2 .and. kc(k) == 1) .or. (kc(k) == 1 .and. present(oneDmask))) THEN ! Do not connect the extreme vertices of the 1d mesh. 
!                                                                                         ! TODO: If oneDmask, calls comes from Delta Shell, remove when this information can be retrived from an extra argument  

      if (kc(k) == 1) then  
     
               if ( present(xplLinks) .and. present(yplLinks) .and. present(zplLinks)) then
                   insidePolygons = - 1 
                   call dbpinpol(XK(K), YK(K), insidePolygons, dmiss, jins, size(xplLinks), xplLinks, yplLinks, zplLinks) 
                   if (insidePolygons .ne. 1) cycle
               endif

         nc1 = 0
         if (jakdtree == 1) then

            if (timon) call timstrt('find cell k-d tree', timerhandle(4))
            call incells_kdtree(xk(k), yk(k), nc1, treeinstcells, searchradiussq)
            if (timon) call timstop(timerhandle(4))
         else ! nokdtree
            if (timon) call timstrt('find cell regular', timerhandle(4))
            call incells(xk(k), yk(k), nc1) ! The old looped variant (pre-kdtree)
            if (timon) call timstop(timerhandle(4))
            endif   

         ! Option for considering only 1d nodes inside 2d net (inNet flag)
         if (inNet_ == 1) then
            if (nc1 .eq. 0) cycle
         endif

         if (timon) call timstrt('create 1d2d link', timerhandle(5))
!         IF (allocated(KC) ) then 
            IF (NC1 > 0) THEN
               CALL dSETNEWPOINT(XZ(NC1),YZ(NC1), NC2)
               if (.true.) then
                  ! Cheap link addition: avoid expensive connectdbn() (setnodadm() will cleanup later)
                  call connectdbfast(NC2, K, L)
               else
                  ! Old link code:
               call connectdbn(NC2, K, L)
               end if
               KN(3,L) = kn3typ
               numValidLinks = numValidLinks + 1
               connectionIndexes(1,numValidLinks) = NC1 !2d
               connectionIndexes(2,numValidLinks) = K   !1d
            ELSE
               ! Note: Code below has not been improved with k-d tree yet (CROSSED2d_BNDCELL, CROSSEDanother1Dlink)
               DO KK = 1, min(2, NMK(K))
                  L  = NOD(K)%LIN(KK)
                  KK2(KK) = KN(1,L) + KN(2,L) - K
               ENDDO
               K1 = KK2(1) 
               IF(NMK(K) == 1) THEN
                  K2 = K
               ELSE
                  K2 = KK2(2)
               ENDIF

               CALL normalout(XK(K1), YK(K1), XK(K2), YK(K2) , XN, YN, jsferic, jasfer3D, dmiss, dxymis)

               WWU = 5D0*DBDISTANCE(XK(K1), YK(K1), XK(K2), YK(K2), jsferic, jasfer3D, dmiss )

               XK2 = XK(K) + XN*WWU
               YK2 = YK(K) + YN*WWU
               CALL CROSSED2d_BNDCELL(NML, XK(K), YK(K), XK2, YK2, NC1, Lfound)

               IF (NC1 > 0) THEN
                  call CROSSEDanother1Dlink(NuML, Xk(k), Yk(k), NC1, Lfound)
                  if (nc1 > 0) then 
                     CALL dSETNEWPOINT(XZ(NC1),YZ(NC1),NC2)
                     call connectdbn(NC2, K, L)
                     KN(3,L) = kn3typ
                     numValidLinks = numValidLinks + 1
                     connectionIndexes(1,numValidLinks) = NC1 !2d
                     connectionIndexes(2,numValidLinks) = K   !1d
                  endif   
               ENDIF

               XK2 = XK(K) - XN*WWU
               YK2 = YK(K) - YN*WWU
               CALL CROSSED2d_BNDCELL(NML, XK(K), YK(K), XK2, YK2, NC1,Lfound)

               IF (NC1 > 0) THEN
                  call CROSSEDanother1Dlink(NuML, Xk(k), Yk(k), NC1, Lfound)
                  if (nc1 > 0) then 
                     CALL dSETNEWPOINT(XZ(NC1),YZ(NC1),NC2)
                     call connectdbn(NC2, K, L)
                     KN(3,L) = 3
                     numValidLinks = numValidLinks + 1
                     connectionIndexes(1,numValidLinks) = NC1 !2d
                     connectionIndexes(2,numValidLinks) = K   !1d
                  endif   
               ENDIF

            ENDIF

         ! ENDIF
         if (timon) call timstop(timerhandle(5))

         ENDIF


   ENDDO

   ! set network status
   netstat = NETSTAT_CELLS_DIRTY

999 continue
   call delete_kdtree2(treeinstcells)

   if (timon) call timstop(timerhandle(1))

   if (timon) call timdump ('timers_make1D2Dinternalnetlinks.txt', .true.)

   end function make1D2Dinternalnetlinks
   
   subroutine make1D2Droofgutterpipes(xplRoofs, yplRoofs, zplRoofs, oneDMask)      !

   use m_missing
   use m_polygon
   use geometry_module
   use m_alloc
   use network_data
   use m_cell_geometry

   implicit none

   !dfm might have already allocated xpl, ypl, zpl
   double precision, optional, intent(in)  :: xplRoofs(:), yplRoofs(:), zplRoofs(:)
   integer, optional, intent(in)           :: oneDMask(:)                !< Masking array for 1d mesh points, unmerged nodes

   integer                                 :: inp, n, n1, ip, i, k1, k2, L, k, numUnMergedNodes
   double precision                        :: XN1, YN1, DIST
   integer,          allocatable           :: nodroof(:), nod1D(:)
   double precision, allocatable           :: dismin(:)
   character(len=5)                        :: sd
   integer                                 :: ierr
   integer                                 :: nInputPolygon
   logical                                 :: validOneDMask
   
   validOneDMask = .false.
   if(present(oneDMask)) then
      if(size(oneDMask,1).gt.0) then
         validOneDMask = .true.
      endif
   endif
   ierr = -1
      
   call findcells(0)

   if(allocated(connectionIndexes)) then
      deallocate(connectionIndexes)
   endif
   allocate(connectionIndexes(2,nump))
   connectionIndexes = 0
   numValidLinks = 0
   
   !allocate and assign polygon if input arrays are present
   !when called from DFM xpl, ypl, and zpl arrays are already allocated in m_polygon
   if (present(xplRoofs)) then
      npl = size(xplRoofs)
      call increasepol(npl, 0)
      xpl(1:npl) = xplRoofs
      ypl(1:npl) = yplRoofs
      zpl(1:npl) = zplRoofs
      inp = -1
   endif

   kc   = 0
   do n = 1,nump
      call inwhichpolygon(xzw(n), yzw(n), inp)
      if (inp > 0) then
         kc(n) = inp
      endif
   enddo

   do i = 1,npoly
      xpl(iiend(i)) = xpl(iistart(i))
      ypl(iiend(i)) = ypl(iistart(i))
      zpl(iiend(i)) = zpl(iistart(i))
   enddo

   allocate( dismin(npoly), nodroof(npoly), nod1D(npoly) )
   dismin = 9d9 ; nodroof = 0 ; nod1D = 0
   do n  = 1,nump
      if (kc(n) > 0) then
         ip = kc(n)
         if (validOneDMask) then
            call CLOSETO1Dnetnode(xzw(n), yzw(n), N1, DIST, oneDMask)
         else
            call CLOSETO1Dnetnode(xzw(n), yzw(n), N1, DIST)
         endif
         if (dist < dismin(ip)) then
            dismin(ip) = dist ; nodroof(ip) = n; nod1D(ip) = n1
         endif
      endif
   enddo

   do ip = 1,npoly
      n1 = nodroof(ip)
      if (n1 > 0) then
         call dsetnewpoint(xz(n1),yz(n1),k1)
         k2 = nod1D(ip)
         call connectdbn(k1,k2,l)
         kn(3,l) = 7
         numValidLinks = numValidLinks + 1
         connectionIndexes(1,numValidLinks) = n1 !2d
         connectionIndexes(2,numValidLinks) = k2 !1d
      endif
   enddo

   deallocate( dismin, nodroof, nod1D )
   
   ierr = 0

   end subroutine make1D2Droofgutterpipes

   subroutine make1D2Dstreetinletpipes(xsStreetInletPipes, ysStreetInletPipes, oneDMask)

   use m_missing
   use m_polygon
   use geometry_module
   use m_alloc
   use network_data
   use m_cell_geometry
   use m_samples
   use m_sferic
   use kdtree2Factory
   use Timers

   implicit none

   !allocate and assign samples if input arrays are present
   !when called from DFM xs, ys are already allocated in m_samples
   double precision, optional, intent(in)  :: xsStreetInletPipes(:), ysStreetInletPipes(:)
   integer, optional, intent(in)           :: oneDMask(:)           !< Masking array for 1d mesh points, unmerged nodes
   integer                                 :: n,k,n1,k1,l, ierr
   double precision                        :: dist
   logical                                 :: validOneDMask

   integer :: timerhandle(5)
   type(kdtree_instance) :: treeinstcells
   type(kdtree_instance) :: treeinst1d
   double precision :: searchradiussq, searchradius1dsq
   integer, allocatable :: k1d(:)

   integer :: isearch
   integer, parameter :: MAXSEARCHIT1D = 10 !< Max number of repeated 1D searches with growing search radius
   integer :: jakdtree = 1
   
   timerhandle = 0

   call init_gridoperations()

   if (timon) call timstrt('make1D2Dstreetinletpipes', timerhandle(1))
   
   validOneDMask = .false.
   if(present(oneDMask)) then
      if(size(oneDMask,1).gt.0) then
         validOneDMask = .true.
      endif
   endif
   ierr = -1

   if (timon) call timstrt('findcells', timerhandle(2))

   call findcells(100)        ! include folded cells
   if (timon) call timstop(timerhandle(2))
   if (timon) call timstrt('make k-d tree', timerhandle(3))
   if (jakdtree == 1) then
      ierr = constructNetcellKdTree(treeinstcells, searchradiussq)
      searchradiussq = 1.1d0 * searchradiussq**2
      if (ierr /= 0) then
         ! Fall back to without k-d tree
         jakdtree = 0
      end if

      
      ierr = constructNetnode1DKdTree(treeinst1d, searchradius1dsq, k1d)
      if (ierr /= 0) then
         if (k1d(1) == 0) then
            ! No 1D nodes at all, no 1D2D links can be generated at all.
            goto 999
         else
            ! Fall back to without k-d tree
            jakdtree = 0
         end if
      else
         searchradius1dsq = 3d0 * searchradius1dsq**2 ! safety factor
      end if
   end if
   if (timon) call timstop(timerhandle(3))

   if(allocated(connectionIndexes)) then
      deallocate(connectionIndexes)
   endif
   allocate(connectionIndexes(2,nump))
   connectionIndexes = 0
   numValidLinks = 0

   if (present(xsStreetInletPipes)) then
      ns = size(xsStreetInletPipes)
      call INCREASESAM(ns)
      Xs(1:ns) = xsStreetInletPipes
      Ys(1:ns) = ysStreetInletPipes
   endif

   do n  = 1,ns
      if (jakdtree == 1) then
         if (timon) call timstrt('find cell k-d tree', timerhandle(4))
         call incells_kdtree(xs(n), ys(n), k, treeinstcells, searchradiussq)
         if (timon) call timstop(timerhandle(4))

      else ! nokdtree
         if (timon) call timstrt('find cell regular', timerhandle(4))
         call incells(Xs(n),Ys(n),K) ! The old looped variant (pre-kdtree)
         if (timon) call timstop(timerhandle(4))
      end if

      call timstrt('create 1d2d link', timerhandle(5))
      if (k > 0) then
         if (jakdtree == 1) then
            do isearch=0,MAXSEARCHIT1D-1
               if(validOneDMask) then
                  call CLOSETO1Dnetnode_kdtree(xzw(k), yzw(k), n1, dist, treeinst1d, 2**isearch * searchradius1dsq, k1d, oneDMask)
               else
                  call CLOSETO1Dnetnode_kdtree(xzw(k), yzw(k), n1, dist, treeinst1d, 2**isearch * searchradius1dsq, k1d)
               end if
               if (n1 > 0) then
                  exit
               else
                  ! Retry with 2x bigger search radius
                  cycle
               end if
            end do
         else
            if(validOneDMask) then
            call CLOSETO1Dnetnode(xzw(k), yzw(k), n1, dist, oneDMask)
         else
            call CLOSETO1Dnetnode(xzw(k), yzw(k), n1, dist)
         endif
         endif
         if (n1 /= 0) then
            call dsetnewpoint(xzw(k),yzw(k),k1)
            if (.true.) then
               ! Cheap link addition: avoid expensive connectdbn() (setnodadm() will cleanup later)
               call connectdbfast(k1,n1,L)
            else
               ! Old link code:
            call connectdbn(k1,n1,l)
            end if

            kn(3,L) = 5
            numValidLinks = numValidLinks + 1
            connectionIndexes(1,numValidLinks) = k   !2d
            connectionIndexes(2,numValidLinks) = n1  !1d
         endif
      endif
      if (timon) call timstop(timerhandle(5))
   enddo
   
   ierr = 0

999 continue
   call delete_kdtree2(treeinstcells)
   call delete_kdtree2(treeinst1d)

   if (timon) call timstop(timerhandle(1))

   if (timon) call timdump ('timers_make1D2Dstreetinletpipes.txt', .true.)
   
   end subroutine make1D2Dstreetinletpipes


   !> Construct a searchable k-d tree instance for all current net cells,
   !! and a recommended maximum search radius.
   !! The representative mass center coordinates of the net cells will be used.
   function constructNetcellKdTree(treeinst, maxsearchradius) result(ierr)
   use kdtree2Factory
   use network_data
   use m_sferic, only: jsferic, jasfer3D
   use m_missing, only: dmiss
   use geometry_module, only: dbdistance
   implicit none

   type(kdtree_instance), intent(inout) :: treeinst        !< k-d tree instance
   double precision,      intent(inout) :: maxsearchradius !< Maximum recommended search radius based on largest distance from any cell corner to its mass center.
   integer                              :: ierr     !< Error status (0 if successful)

   integer :: i, numnetcells, k

   ierr = 0

   numnetcells = size(xzw(:))
   if (numnetcells>0) then
      call build_kdtree(treeinst, size(xzw(:)), xzw(:), yzw(:), ierr, jsferic, dmiss)
   else
      return
   endif
   
   maxsearchradius = 0d0
   do i=1,numnetcells
      do k=1,netcell(i)%N
         maxsearchradius = max(maxsearchradius, dbdistance(xzw(i), yzw(i), xk(netcell(i)%nod(k)), yk(netcell(i)%nod(k)), jsferic, jasfer3D, dmiss))
      end do
   end do

   end function constructNetcellKdTree

   !> Construct a searchable k-d tree instance for all current 1D net nodes,
   !! and a recommended maximum search radius.
   function constructNetnode1DKdTree(treeinst, maxsearchradius, k1d) result(ierr)
   use kdtree2Factory
   use network_data
   use m_sferic, only: jsferic, jasfer3D
   use m_missing, only: dmiss
   use geometry_module, only: dbdistance
   implicit none

   type(kdtree_instance), intent(inout) :: treeinst        !< k-d tree instance
   double precision,      intent(inout) :: maxsearchradius !< Maximum recommended search radius based on average 1d link length.
   integer, allocatable,  intent(inout) :: k1d(:)          !< Mapping table from purely 1D net nodes to full set of netnodenumbers (1:numk1d) ==> (1:numk)
   
   integer                              :: ierr     !< Error status (0 if successful)

   integer          :: k, k1, k2, L!, k1ClientIndex, k2ClientIndex
   double precision :: dis,sumdis

   ! logical                       :: validOneDMask

   double precision, allocatable :: xk1d(:), yk1d(:)
   integer, allocatable :: kcloc(:)
   integer :: numk1d, numl1dregular


   ierr = 0

   ! TODO: consider having the oneDMask here already

   allocate(kcloc(numk), stat = ierr); kcloc = 0
   allocate(k1d(numk), stat = ierr); k1d = 0
   numk1d = 0
   numl1dregular = 0

   sumdis = 0d0
   DO L = 1,numl
      IF (kn(3,L) == 1 .or. kn(3,L) == 6) then !  .or. kn(3,L) == 4) THEN
         numl1dregular = numl1dregular + 1
         K1 = kn(1,L) ; K2 = kn(2,L)         
         if (kcloc(k1) == 0) then
            numk1d = numk1d+1
            k1d(numk1d) = k1
            kcloc(k1) = numk1d ! Store 1D netnode number
         end if
         if (kcloc(k2) == 0) then
            numk1d = numk1d+1
            k1d(numk1d) = k2
            kcloc(k2) = numk1d ! Store 1D netnode number
         end if
         dis = dbdistance(xk(k1), yk(k1), xk(k2), yk(k2),jsferic, jasfer3D, dmiss)
         sumdis = sumdis + dis
      end if
   enddo
   if (numk1d <= 0) then
      ierr = 1
      return
   end if

   ! Make the tree
   allocate(xk1d(numk1d), yk1d(numk1d), stat = ierr); xk1d = dmiss; yk1d = dmiss
   do k=1,numk1d
      xk1d(k) = xk(k1d(k))
      yk1d(k) = yk(k1d(k))
   end do
   call build_kdtree(treeinst, numk1d, xk1d(:), yk1d(:), ierr, jsferic, dmiss)

   ! Suggested search radius
   maxsearchradius = sumdis / numl1dregular ! Average distance

   end function constructNetnode1DKdTree

   



   !> make dual cell polygon around netnode k
   subroutine make_dual_cell(k, N, rcel, xx, yy, num, Wu1Duni)
      
      use network_data
      use m_polygon
      use m_missing
      use m_sferic
      use geometry_module, only: normalout, comp_masscenter, half, xpav, spher2locvec
      
      implicit none

      integer,                        intent(in)  :: k         !< netnode number
      integer,                        intent(in)  :: N         !< array size
      double precision,               intent(in)  :: rcel      !< dual-cell enlargement factor around dual-cell center
      double precision, dimension(N), intent(out) :: xx, yy    !< dual-cell polygon coordinates
      integer,                        intent(out) :: num       !< polygon dimension
      double precision,               intent(in)  :: Wu1Duni   !< 
      
      !locals
      integer                                     :: ierror ! error (1) or not (0)
      integer,          dimension(N)              :: iclist
      double precision                            :: xc, yc, area, w, sn, cs, xh, yh, aa, cs2, cs3, sn2, sn3, f
      double precision, dimension(1)              :: csloc, snloc, csloc2, snloc2, csloc3, snloc3
      double precision                            :: xh2, yh2, xh3, yh3, dist

      integer                                     :: i, ic, k1, k2, L, NN, Nc, ja2D, Lp, k3, ip, is, ncol
      integer                                     :: jacounterclockwise          ! counterclockwise (1) or not (0) (not used here)
      
      integer                                     :: jatolan ! copy cells to landboundary (useful for plotting)
      
      jatolan = 0

      ierror = 1

      !if ( k.eq.4399 ) then
      !   continue
      !end if

      NN = nmk(k)

      ja2D = 1
      do i=1,NN
          L = nod(k)%lin(i)
          if (lnn(L) == 0) then 
              ja2D = 0             ! otherwise incorrectly flagged as a 2D   
          endif
          if (kn(3,L) /= 2 .and. kn(3,L).ne.0 ) ja2D = 0  
      enddo   

      if ( NN.gt.N ) then
         call msgbox('', 'make_dual_cell: array size error', LEVEL_ERROR)
         ! TODO: SvdP: consider adding 'call mess' to stop the simulation.
         goto 1234
      end if

      if (ja2D == 1) then  
   !     get ordered cell list
         call get_celllist(k, N, iclist)
         
   !     construct dual cell polygon
         num = 0
         do i=1,NN
            num = num+1
            L = nod(k)%lin(i)
            k1 = k
            k2 = kn(1,L)+kn(2,L)-k
            xx(num) = 0.5d0*(xk(k1)+xk(k2))
            yy(num) = 0.5d0*(yk(k1)+yk(k2))
            
!           fix for periodic, spherical coordinates   
            if ( jsferic.eq.1 ) then
               if ( xk(k2)-xk(k1).gt.180d0 ) then
                  xx(num) = xx(num) - 180d0
               else if ( xk(k2)-xk(k1).lt.-180d0 ) then
                  xx(num) = xx(num) + 180d0
               end if
            end if
         
            num = num+1
            ic = iclist(i)
            if ( ic.ne.0 ) then
               Nc = netcell(ic)%N
               xx(num) = xzw(ic)
               yy(num) = yzw(ic)
            else
               xx(num) = xk(k)
               yy(num) = yk(k)
            end if
         end do
         
   !     compute dual cell center
         call comp_masscenter(num, xx, yy, xc, yc, area, jacounterclockwise, jsferic, jasfer3D, dmiss)

!        fix for periodic, spherical coordinates           
         if ( xc-xk(k).gt.180d0 ) then
            xc = xc - 360d0
         else if ( xc-xk(k).lt.-180d0 ) then
            xc = xc + 360d0
         end if
         
   !     enlarge dual cell
         do i=1,num
            xx(i) = xc + RCEL*(xx(i)-xc)
            yy(i) = yc + RCEL*(yy(i)-yc)
         end do

      else ! 1D
      
         !w = 0d0 ! check in next time
         !do i=1,NN
         !   L = nod(k)%lin(i) ; call othernode(k,L,k2)
         !   call dbdistancehk( xk(k), yk(k), xk(k2), yk(k2), dist ) ; w = w + dist     
         !enddo
         !w   = w/nn
         !w   = 0.1d0*w* RCEL ! width = 0.2*dx

         w   = 0.5d0*Wu1Duni * RCEL ! RCEL a bit different in 1D

         num = 0
         
         if (nn == 1) then 

            L = nod(k)%lin(1) ; call othernode(k,L,k2)
            
            call normalout( xk(k), yk(k), xk(k2), yk(k2), cs, sn, jsferic, 1, dmiss, dxymis)
            call half(xk(k), yk(k), xk(k2), yk(k2), xh, yh, jsferic, jasfer3D)
            call spher2locvec(xk(k),yk(k),1,(/xh/),(/yh/),(/cs/),(/sn/),csloc,snloc, jsferic, 1, dmiss)
    
            num     = num + 1
            call xpav(xk(k),yk(k),-w,csloc(1),snloc(1),xx(num),yy(num),jsferic,1)
            
            num     = num + 1
            call xpav(xh,yh,-w,cs,sn,xx(num),yy(num),jsferic,1)
            
            num     = num + 1
            call xpav(xh,yh,w,cs,sn,xx(num),yy(num),jsferic,1)
            
            num     = num + 1
            call xpav(xk(k),yk(k),w,csloc(1),snloc(1),xx(num),yy(num),jsferic,1)
            
         else 
            
            if (nn == 2) then 
               is =  1
            else    
               is = -1
            endif  
    
            do i = 1,NN
               L = nod(k)%lin(i) ; call othernode(k,L,k2)
              
               ip = i + 1 ; if (i == nn) ip = 1 
               Lp = nod(k)%lin(ip) ; call othernode(k,Lp,k3)   

               call normalout( xk(k2), yk(k2), xk(k), yk(k),  cs2, sn2, jsferic, 1, dmiss, dxymis) 
               call half(xk(k), yk(k), xk(k2), yk(k2), xh2, yh2, jsferic, jasfer3D)
               call spher2locvec(xk(k),yk(k),1,(/xh2/),(/yh2/),(/cs2/),(/sn2/),csloc2,snloc2, jsferic, 1, dmiss)
               
               call normalout( xk(k) , yk(k), xk(k3), yk(k3), cs3, sn3, jsferic, 1, dmiss, dxymis)
               call half(xk(k), yk(k), xk(k3), yk(k3), xh3, yh3, jsferic, jasfer3D)
               call spher2locvec(xk(k),yk(k),1,(/xh3/),(/yh3/),(/cs3/),(/sn3/),csloc3,snloc3, jsferic, 1, dmiss)
               
               f = w/(1d0+csloc2(1)*csloc3(1)+snloc2(1)*snloc3(1)) 
               
               num     = num + 1
               call xpav(xh2,yh2,w,cs2,sn2,xx(num),yy(num),jsferic,1)

               num     = num + 1
               call xpav(xk(k),yk(k),f,csloc2(1)+csloc3(1),snloc2(1)+snloc3(1),xx(num),yy(num),jsferic,1)

               num    = num + 1
               call xpav(xh3,yh3,w,cs3,sn3,xx(num),yy(num),jsferic,1)

            enddo   
        
                 
         endif   
         
         if (num .ge. 3) then 
             call random_number(aa)
             ncol = 255*aa
             !call DISPF2closed(xx,yy,num,num,ncol)
             
             if ( jatolan.eq.1 ) then
                call INCREASELAN(MXLAN+num+2)
                xlan(MXLAN+1) = dmiss
                ylan(MXLAN+1) = dmiss
                xlan(MXLAN+2:MXLAN+num+2) = (/ xx(1:num), xx(1) /)
                ylan(MXLAN+2:MXLAN+num+2) = (/ yy(1:num), yy(1) /)
                MXLAN = MXLAN+num+2
             end if
         endif   
         
      endif   
         
      ierror = 0
   1234 continue
      return
   end subroutine make_dual_cell
   
   !net.f90, 34233
   !> find netcells surrounding a netnode, order in link direction "nod()%cell"
   !>   cell "0" is a fictious boundary-cell
   subroutine get_celllist(k, N, iclist)
      use network_data
      implicit none

      integer,               intent(in)  :: k         !< netnode
      integer,               intent(in)  :: N         !< array size
      integer, dimension(N), intent(out) :: iclist    !< list of netcells attached to the node

      integer                            :: ierror    ! error (1) or not (0)

      integer                            :: i, ip1, ic1, ic2, j, ja, L, Lp1, NN
      integer                            :: ii, iim1

      ierror = 1

      NN = nmk(k)

      if ( NN.gt.N ) then
         call msgbox('', 'get_celllist: array size error', LEVEL_ERROR)
         ! TODO: SvdP: consider adding 'call mess' to stop the simulation.
         goto 1234
      end if

      do i=1,NN
   !     find cell between ith and (i+1)rst link, 0 indicates no cell (boundary)
         L = nod(k)%lin(i)
         ip1 = i+1; if ( ip1.gt.NN ) ip1=ip1-NN
         Lp1 = nod(k)%lin(ip1)

         ic1 = lne(1,L)
         if ( lnn(L).gt.1) then
            ic2 = lne(2,L)
         else
            ic2 = 0  ! boundary netlink
         end if

   !    check if cell ic1 contains link (i+1)
        ja = 0
        if ( lnn(L).gt.0 ) then
!          find own link index
           ii = 1
           do while ( netcell(ic1)%lin(ii).ne.L .and. ii.lt.netcell(ic1)%N )
              ii = ii+1
           end do
!          check if previous netlink in netcell ic1 is netlink (i+1)
           iim1 = ii-1; if ( iim1.lt.1 ) iim1=iim1+netcell(ic1)%N

              if ( netcell(ic1)%lin(iim1).eq.Lp1 ) then
                 ja = 1
              end if
        end if

        if (ja.eq.1 ) then
   !       cell ic1 is between the ith and (i+1)rst link
           iclist(i) = ic1
        else
   !    if cell ic1 does not contain link (i+1), use ic2 (0 for boundary, or isolated, or 1D links)
           iclist(i) = ic2
        end if
      end do

   !  determine if ic1 or ic2 

      ierror = 0
   1234 continue   
      return
   end subroutine get_celllist

!----------------------------------------------------------------------!
! 
!----------------------------------------------------------------------! 
   function ggeo_initialize()  result(ierr)
   
   use network_data
   integer :: ierr
   
   ierr = 0
   
   if (.not. allocated(xk)) then
      allocate( xk (1), yk (1), zk (1) , NOD (1) , KC (1) , NMK (1) , KN(3,1), LC(1), RNOD(1), RLIN(1))
      allocate(nod(1)%lin(1))
      KMAX = 1
      LMAX = 1
      NMK = 0
   endif
   if (.not. allocated(xk0)) then
      allocate( xk0(1), yk0(1), zk0(1) , NOD0(1) , KC0(1) , NMK0(1), KN0(3,1), LC0(1)  )
      allocate(nod0(1)%lin(1))
      nmk0 = 0
   endif


   CALL INCREASENETW(KMAX, LMAX)
   
   end function ggeo_initialize

   
   !< converter function
   function ggeo_convert(meshgeom, start_index) result(ierr)

   use network_data
   use m_missing, only : dmiss

   implicit none
   type(t_ug_meshgeom), intent(in)      :: meshgeom
   integer, intent(in)                  :: start_index   
   integer                              :: numk_last, numl_last, ierr, numk_read, l, incrementIndex, numl1d_last

   incrementIndex = 0
   ierr = 0

   numk_last = LNUMK
   numl_last = LNUML
   numl1d_last = NUML1D
   numk_read = meshgeom%numnode
   
   if (start_index == 0) incrementIndex = 1;

   ! Prepare net vars for new data and fill with values from file, increases nod, xk, yk, zk, kn if needed

   call increasenetw(numk_last + meshgeom%numnode, numl_last + meshgeom%numedge)
   XK(numk_last+1:numk_last+numk_read) = meshgeom%nodex(1:meshgeom%numnode)
   YK(numk_last+1:numk_last+numk_read) = meshgeom%nodey(1:meshgeom%numnode)
   ZK(numk_last+1:numk_last+numk_read) = dmiss

   do l=1,meshgeom%numedge
      ! Append the netlink table, and also increment netnode numbers in netlink array to ensure unique ids.
      kn(1:2,numl_last+l) = numk_last + meshgeom%edge_nodes(1:2,l) + incrementIndex !to calculate in 1D!
      kn(3,  numl_last+l) = meshgeom%dim
   end do

   ! Increase the number of links
   if (meshgeom%numedge>0) NUML = numl_last + meshgeom%numedge
   if (meshgeom%numnode>0) NUMK = numk_last + meshgeom%numnode
   if (meshgeom%dim==1) numl1d_last = numl1d_last + meshgeom%numedge

   LNUMK = NUMK
   LNUML = NUML
   NUML1D = numl1d_last
   
   ! Save meshgeon 2d as used in the gui to build mappings
   if (meshgeom%dim==2) then
      meshgeom2d = meshgeom
   endif
 
   end function ggeo_convert

   function ggeo_deallocate() result (ierr)

   use network_data
   use m_dimens
   use m_polygon

   integer ierr
   ierr = network_data_destructor()
   ierr = m_dimens_destructor()
   ierr = m_polygon_destructor()

   end function ggeo_deallocate

   !< get the number of created links
   function ggeo_get_links_count(nlinks, linkType) result(ierr)

   use network_data

   integer, intent(inout)  :: nlinks
   integer                 :: l, ierr
   integer                 :: linkType, mesh1dCellIndex, mesh2dCellIndex

   ierr = 0
   nlinks = 0
   do l=1, numl      
      if(kn(3,l).eq.linkType) then
         nlinks = nlinks + 1
      end if
   end do

   end function ggeo_get_links_count

   !< get the links
   function ggeo_get_links(arrayfrom, arrayto, linkType, start_index)  result(ierr)
   use array_module
   use network_data

   integer, intent(inout)  :: arrayfrom(:), arrayto(:)
   integer, intent(in)     :: start_index
   integer                 :: ierr, nlinks, l, nc
   integer                 :: linkType, mesh1dCellIndex, mesh2dCellIndex
   integer, allocatable    :: mesh2dMapping(:)

   ierr     = 0
   nlinks   = 0
   
   ierr  = ggeo_map_2d_cells(meshgeom2d, mesh2dMapping)
   if(ierr.ne.0 .or. (.not.(allocated(mesh1dInternalToClientMapping))).or. (.not.(allocated(mesh2dMapping)))) then
      ierr = -1
      return
   endif
   
   do l=1, numValidLinks

      mesh1dCellIndex=connectionIndexes(2,l)
      mesh2dCellIndex=connectionIndexes(1,l)

      if(mesh1dCellIndex > size(mesh1dInternalToClientMapping)) then
         ierr = -1
         return
      endif
      
      if (mesh2dCellIndex > size(mesh2dMapping)) then
         ierr = -1
         return
      endif
      
      nlinks = nlinks + 1
      arrayfrom(nlinks) = mesh2dMapping(mesh2dCellIndex)
      !1dpoint
      arrayto(nlinks)   = mesh1dInternalToClientMapping(mesh1dCellIndex)
      
   end do

   !convert to required start index, 1 based is assumed
   ierr = convert_start_index(arrayfrom, -999, 1, start_index) 
   ierr = convert_start_index(arrayto, -999, 1, start_index) 
   

   end function ggeo_get_links

   !< create meshgeom from array
   function ggeo_convert_1d_arrays(nodex, nodey, nodeoffset, branchlength, nodebranchidx, sourcenodeid, targetnodeid, meshgeom, startindex) result(ierr)

   use odugrid
   use m_alloc

   double precision, intent(in)          :: nodex(:), nodey(:), nodeoffset(:), branchlength(:)
   integer, intent(in)                   :: nodebranchidx(:), sourcenodeid(:), targetnodeid(:), startindex
   type(t_ug_meshgeom), intent(inout)    :: meshgeom
   integer                               :: ierr, nbranches, branch, numMeshNodes, numk, numl, st, en, stn, enn, stnumk, ennumk, k, numNetworkNodes, numLocalNodes
   integer, allocatable                  :: startEndBranchNodes(:,:), edge_nodes(:,:), correctedNodeBranchidx(:), localNodeIndexses(:), networkNodeIndex(:), branchids(:), meshnodeIndex(:)
   integer                               :: shift, startInternal, endInternal
   double precision, allocatable, target :: xk(:), yk(:)
   double precision                      :: tolerance

   ierr            = -1
   tolerance       = 1e-6
   shift           = 1 - startindex

   nbranches       = size(sourcenodeid)
   numMeshNodes    = size(nodebranchidx,1)
   numNetworkNodes = max(maxval(sourcenodeid) + shift, maxval(targetnodeid) + shift)

   ! allocate enough space for temp arrays
   allocate(xk(numMeshNodes))
   allocate(yk(numMeshNodes))
   allocate(branchids(numMeshNodes))
   allocate(localNodeIndexses(numMeshNodes))

   if(allocated(mesh1dInternalToClientMapping)) then
      deallocate(mesh1dInternalToClientMapping)
   endif
   allocate(mesh1dInternalToClientMapping(numMeshNodes)); mesh1dInternalToClientMapping = 0
   
   allocate(meshnodeIndex(numMeshNodes)); meshnodeIndex = 0
   allocate(networkNodeIndex(numNetworkNodes)); networkNodeIndex = 0

   allocate(edge_nodes(2,numMeshNodes*3)) !rough estimate of the maximum number of edges given a certain amount of nodes
   allocate(correctedNodeBranchidx(numMeshNodes))
   allocate(startEndBranchNodes(2,nbranches)); startEndBranchNodes = -1
   allocate(meshgeom%nodebranchidx(numMeshNodes))


    !map the mesh nodes
    correctedNodeBranchidx = nodebranchidx + shift
    ierr = odu_get_start_end_nodes_of_branches(correctedNodeBranchidx, startEndBranchNodes(1,:), startEndBranchNodes(2,:))

    !start end of each network branch
    do branch = 1, nbranches

       st        = startEndBranchNodes(1,branch)
       en        = startEndBranchNodes(2,branch)
       stn       = sourcenodeid(branch) + shift
       enn       = targetnodeid(branch) + shift

       ! invalid mesh points
       if( st<=0 .or. en <= 0 .or. st> numMeshNodes .or. en > numMeshNodes) then
          cycle
       endif

       ! invalid branch index
       if( stn<=0 .or. enn <= 0 .or. stn> numNetworkNodes .or. enn > numNetworkNodes) then
          cycle
       endif

       !start
       if(nodeoffset(st)<tolerance .and. networkNodeIndex(stn)==0) then
          networkNodeIndex(stn) = st
       endif

       !end
       if(abs(nodeoffset(en) - branchlength(branch)) < tolerance .and. networkNodeIndex(enn)==0) then
          networkNodeIndex(enn) = en
       endif

    enddo
    
   !make the links
   numl = 0
   numk = 0
   do branch = 1, nbranches

      st = startEndBranchNodes(1,branch)
      en = startEndBranchNodes(2,branch)
      stn = sourcenodeid(branch) + shift
      enn = targetnodeid(branch) + shift
      numLocalNodes = 0


      ! invalid branch index
      if( stn<=0 .or. enn <= 0 .or. stn> numNetworkNodes .or. enn > numNetworkNodes) then
         cycle
      endif

      !invalid edge
      if(networkNodeIndex(stn)==networkNodeIndex(enn)) then
        cycle
      endif

      ! start
      if(networkNodeIndex(stn) > 0) then
         if(meshnodeIndex(networkNodeIndex(stn)) ==0) then
            numk                                 = numk + 1
            meshnodeIndex(networkNodeIndex(stn)) = numk
            mesh1dInternalToClientMapping(numk)  = networkNodeIndex(stn)
            xk(numk)                             = nodex(networkNodeIndex(stn))
            yk(numk)                             = nodey(networkNodeIndex(stn))
            branchids(numk)                      = nodebranchidx(networkNodeIndex(stn))
         endif
         numLocalNodes = numLocalNodes + 1
         localNodeIndexses(numLocalNodes) = meshnodeIndex(networkNodeIndex(stn))
      endif


      ! mesh nodes belongs to this branch
      if( st>0 .and. en > 0 .and. st<= numMeshNodes .and. en <= numMeshNodes) then

         ! in case the first node is at the start, skip it, already accounted for
         startInternal = st
         if(nodeoffset(st)<tolerance ) then
            startInternal = st + 1
         endif

         ! in case the first node is at the end, skip it, already accounted for later
         endInternal = en
         if(abs(nodeoffset(en) - branchlength(branch)) < tolerance ) then
            endInternal = en - 1
         endif

         !internal nodes
         do k = startInternal, endInternal
            numLocalNodes                    = numLocalNodes + 1
            if(meshnodeIndex(k)==0 ) then
               numk                                = numk + 1
               meshnodeIndex(k)                    = numk
               mesh1dInternalToClientMapping(numk) = k
               xk(numk)                            = nodex(k)
               yk(numk)                            = nodey(k)
               branchids(numk)                     = nodebranchidx(k)
            endif
            localNodeIndexses(numLocalNodes)       = meshnodeIndex(k)
         enddo
     endif

     !end
     if(networkNodeIndex(enn) > 0) then
         if(meshnodeIndex(networkNodeIndex(enn))==0) then
            numk                                  = numk + 1
            meshnodeIndex(networkNodeIndex(enn))  = numk
            mesh1dInternalToClientMapping(numk)   = networkNodeIndex(enn)
            xk(numk)                              = nodex(networkNodeIndex(enn))
            yk(numk)                              = nodey(networkNodeIndex(enn))
            branchids(numk)                       = nodebranchidx(networkNodeIndex(enn))
         endif                                    
         numLocalNodes = numLocalNodes + 1        
         localNodeIndexses(numLocalNodes)         = meshnodeIndex(networkNodeIndex(enn))
     endif

    !create edge node table
     do k = 1, numLocalNodes - 1
        numl = numl + 1
        edge_nodes(1,numl) = localNodeIndexses(k)
        edge_nodes(2,numl) = localNodeIndexses(k+1)
     enddo

   enddo


   ! assigned merged nodes
   meshgeom%dim     = 1
   meshgeom%numnode = numk
   meshgeom%numedge = numl

   allocate(meshgeom%nodex(numk))
   allocate(meshgeom%nodey(numk))
   allocate(meshgeom%edge_nodes(2, numl))

   meshgeom%nodebranchidx = branchids(1:numk)
   meshgeom%nodex         = xk(1:numk)
   meshgeom%nodey         = yk(1:numk)
   meshgeom%edge_nodes    = edge_nodes(:,1:numl)

   !deallocate
   deallocate(xk)
   deallocate(yk)
   deallocate(localNodeIndexses)
   deallocate(edge_nodes) !rough estimate of the maximum number of edges given a certain amount of nodes
   deallocate(correctedNodeBranchidx)
   deallocate(startEndBranchNodes)

   ierr = 0

   end function ggeo_convert_1d_arrays
   
   
   function ggeo_connect_edges(startEdge, endEdge, connectionMask, numedege, edgenodes) result(ierr)
   
   
   integer, intent(in)                    :: startEdge, endEdge
   
   integer, intent(inout)                 :: connectionMask(:)
   integer, intent(inout)                 :: numedege
   integer, optional, intent(inout)       :: edgenodes(:,:)
   integer                                :: ierr
   logical                                :: createEdgeNodes
   
   ierr = -1
   createEdgeNodes = .false.
   
   if(present(edgenodes)) then
      createEdgeNodes = .true. 
   endif

   if((connectionMask(startEdge).eq.0).or.(connectionMask(endEdge).eq.0)) then
      numedege = numedege + 1 
      connectionMask(startEdge) = connectionMask(startEdge) + 1
      connectionMask(endEdge)   = connectionMask(endEdge) + 1
      if (createEdgeNodes) then
         edgenodes(1,numedege) = startEdge
         edgenodes(2,numedege) = endEdge
      endif
   endif
   
   ierr = 0
   
   end function ggeo_connect_edges


   !< Algorithm to calculate the edgenodes array. The only assumption made here is that the mesh nodes are written consecutively,
   !< in the same direction indicated by the sourcenodeid and the targetnodeid arrays (e.g.
   !< 1  -a-b-c-> 2 and not 1 -c-a-b-> 2 where 1 and 2 are network nodes and a, b, c are mesh nodes )
   function ggeo_count_or_create_edge_nodes(branchidx, branchoffset, sourcenodeid, targetnodeid, branchlength, startindex, numedege, edgenodes) result(ierr) !edge_nodes
   
   use odugrid

   integer, intent(in)                    :: branchidx(:), sourcenodeid(:), targetnodeid(:), startindex
   double precision, intent(in)           :: branchoffset(:),branchlength(:)
   integer, intent(inout)                 :: numedege
   integer, allocatable                   :: meshnodemapping(:,:), internalnodeindexses(:), connectionMask(:), correctedBranchidx(:)
   integer, optional, intent(inout)       :: edgenodes(:,:)
   integer                                :: nnetworknodes, nBranches,nmeshnodes, ierr , k , n, br, st, en, kk, firstvalidarraypos

   ierr = 0
   firstvalidarraypos = 0

   if (startindex.eq.0) then
      firstvalidarraypos = 1
   endif

   !Build mesh mapping: assuming not overlapping mesh nodes
   nnetworknodes = max(maxval(sourcenodeid),maxval(targetnodeid)) + firstvalidarraypos
   nmeshnodes = size(branchidx)
   nbranches = size(sourcenodeid)
   allocate(correctedBranchidx(nmeshnodes))
   allocate(meshnodemapping(2,nbranches))
   allocate(internalnodeindexses(nmeshnodes))
   allocate(connectionMask(nmeshnodes)) !safety mask, to avoid generating too many nodes

   correctedBranchidx = branchidx + firstvalidarraypos
   !map the mesh nodes
   meshnodemapping = -1
   ierr = odu_get_start_end_nodes_of_branches(correctedBranchidx, meshnodemapping(1,:), meshnodemapping(2,:))

   numedege = 0
   connectionMask  =  0
   do br = 1, nbranches
      if ((meshnodemapping(1,br).eq.-1).or.(meshnodemapping(2,br).eq.-1)) then
         ierr = -1
         return
      endif
      ! starting and ending nodes
      st =  meshnodemapping(1,br)
      en =  meshnodemapping(2,br)
      !the nodes between
      kk =  0
      internalnodeindexses = 0
      do n=1, nmeshnodes
         if(correctedBranchidx(n).eq.br.and.(n.ne.st).and.(n.ne.en)) then
            kk = kk + 1
            internalnodeindexses(kk) = n
         endif
      enddo
      !if no internal nodes, connect and cycle
      if (kk.eq.0) then
         ierr = ggeo_connect_edges(st, en, connectionMask, numedege, edgenodes)
         cycle
      endif
      !connect the start node to the first internal node
      ierr = ggeo_connect_edges(st, internalnodeindexses(1), connectionMask, numedege, edgenodes)
      !connect internal nodes
      do n =1, kk - 1
         ierr = ggeo_connect_edges(internalnodeindexses(n), internalnodeindexses(n+1), connectionMask, numedege, edgenodes)
      enddo
      !connect the last internal node to the end node
      ierr = ggeo_connect_edges(internalnodeindexses(kk), en, connectionMask, numedege, edgenodes)   
   enddo
   
   end function ggeo_count_or_create_edge_nodes
   
   !> Makes 1d2d embedded links: 1D is typically overlapping the 2D grid,
   !! and potentially more than one 1d2d link per 1d mesh node is created.
   !! 2D cells are connected if they are intersected by a 1D edge. They are
   !! connected to the nearest of the two endpoints of each 1D edge.
   function ggeo_make1D2Dembeddedlinks(jsferic, jasfer3D, oneDmask)  result(ierr)

   use network_data
   use m_missing,       only: dmiss
   use geometry_module, only: dbdistance, crossinbox
   use kdtree2Factory
   use m_cell_geometry

   implicit none
   
   integer, intent(in)           :: jsferic, jasfer3D 
   integer, optional, intent(in) :: oneDmask(:)
   
   !output
   integer                       :: ierr !< Error status, 0 if success, nonzero in case of error.
   
   !locals
   integer                       :: k, kk, k1, k2, k3, k4, k5, k6, oneDNode, ncellsinSearchRadius, numberCellNetlinks, isCrossing, newPointIndex, newLinkIndex
   integer                       :: l, cellNetLink, cellId, kn3ty, numnetcells,k1ClientIndex, k2ClientIndex
   double precision              :: searchRadiusSquared, ldistance, rdistance, maxdistance, sl, sm, xcr, ycr, crp
   integer, allocatable          :: isInCell(:)
   type(kdtree_instance)         :: treeinst
   logical                       :: validOneDMask 
   
   validOneDMask = .false.
   if(present(oneDMask)) then
      if(size(oneDMask,1).gt.0) then
         validOneDMask = .true.
      endif
   endif
   
   ierr = 0
   !LC: is this the right type?
   kn3ty = 3
   call savenet()
   call findcells(0)
   
   if(allocated(connectionIndexes)) then
      deallocate(connectionIndexes)
   endif
   allocate(connectionIndexes(2,nump))
   connectionIndexes = 0
   numValidLinks = 0

   numnetcells = size(xz(:))
   if (numnetcells>0) then
      call build_kdtree(treeinst, size(xz(:)), xz(:), yz(:), ierr, jsferic, dmiss)
   else
      return
   endif

   allocate(isInCell(numk))
   kc = 0
   isInCell = -1
   do l = 1, numl1d
      k1  = kn(1,l); k2  = kn(2,l); k3 = kn(3,l)
      ! if the link is not 1d do not consider
      if (k3.ne.1) cycle
      !get the left 1d mesh point
      call make_queryvector_kdtree(treeinst, xk(k1), yk(k1), jsferic)
      !compute the search radius
      ldistance = 0d0
      if (l>=2) then
         k4 = kn(1,l-1)
         ldistance = dbdistance(xk(k4),yk(k4),xk(k1),yk(k1), jsferic, jasfer3D, dmiss)
      endif
      rdistance = dbdistance(xk(k1),yk(k1),xk(k2),yk(k2), jsferic, jasfer3D, dmiss)
      maxdistance = max(ldistance, rdistance)
      !check if k1 and k2 are in cell
      if(isInCell(k1).eq.-1) then
         call incells(xk(k1),yk(k1), cellId)
         isInCell(k1) = cellId
      endif
      if(isInCell(k2).eq.-1) then
         call incells(xk(k2),yk(k2), cellId)      
         isInCell(k2) = cellId
      endif
      !1.1d0: safety
      searchRadiusSquared = 1.1d0*maxdistance**2
      !count number of cells in the search area
      nCellsInSearchRadius = kdtree2_r_count(treeinst%tree,treeinst%qv,searchRadiusSquared)
      !no cells found
      if ( nCellsInSearchRadius.eq.0 ) cycle
      !reallocate if necessary
      call realloc_results_kdtree(treeinst, nCellsInSearchRadius)
      !find nearest cells
      call kdtree2_n_nearest(treeinst%tree,treeinst%qv,nCellsInSearchRadius,treeinst%results)
      !connection loop
      isCrossing = 0
      do k = 1, nCellsInSearchRadius
         !check if one of the cell net link crosses the current 1d link
         cellId= treeinst%results(k)%idx
         if (cellId.le.0) cycle
         !this cell has been already explored or is already connected 
         if (kc(cellId).ne.0) cycle
         !check if the cell is already connected
         numberCellNetlinks = size(netcell(cellId)%lin)
         do kk = 1, numberCellNetlinks
            if (kc(cellId).ne.0) cycle
            cellNetLink =  netcell(cellId)%lin(kk)
            k5  = kn(1,cellNetLink); 
            k6  = kn(2,cellNetLink);
            isCrossing = 0
            if (k5<1 .or. k6<1) cycle
            call crossinbox (xk(k5), yk(k5), xk(k6), yk(k6), xk(k1), yk(k1), xk(k2), yk(k2), isCrossing, sl, sm, xcr, ycr, crp, jsferic, dmiss)
            newLinkIndex = -1
            if (isCrossing == 1) then
               ldistance = dbdistance(xk(k1), yk(k1), xz(cellId), yz(cellId), jsferic, jasfer3D, dmiss)
               rdistance = dbdistance(xk(k2), yk(k2), xz(cellId), yz(cellId), jsferic, jasfer3D, dmiss)     
               if (ldistance<=rdistance .and. isInCell(k1).ge.1) then
                  if (validOneDMask) then !again, Fortran does not have logical and two nested if statement are needed
                     k1ClientIndex = mesh1dInternalToClientMapping(k1)
                     if(oneDmask(k1ClientIndex)==1) then
                        !call setnewpoint(xz(cellId),yz(cellId),zk(cellId), newPointIndex)
                        call dsetnewpoint(xz(cellId),yz(cellId), newPointIndex)
                        call connectdbn(newPointIndex, k1, newLinkIndex)
                        oneDNode = k1
                     endif
                  else
                     !call setnewpoint(xz(cellId),yz(cellId),zk(cellId), newPointIndex)
                     call dsetnewpoint(xz(cellId),yz(cellId), newPointIndex)
                     call connectdbn(newPointIndex, k1, newLinkIndex)
                     oneDNode = k1
                  endif
               else if (ldistance > rdistance .and. isInCell(k2).ge.1) then
                  if (validOneDMask) then !again, Fortran does not have logical and two nested if statement are needed
                     k2ClientIndex = mesh1dInternalToClientMapping(k2)
                     if(oneDmask(k2ClientIndex)==1) then
                        !call setnewpoint(xz(cellId),yz(cellId),zk(cellId), newPointIndex)
                        call dsetnewpoint(xz(cellId),yz(cellId), newPointIndex)
                        call connectdbn(newPointIndex, k2, newLinkIndex)
                        oneDNode = k2
                     endif
                  else
                     !call setnewpoint(xz(cellId),yz(cellId),zk(cellId), newPointIndex)
                     call dsetnewpoint(xz(cellId),yz(cellId), newPointIndex)
                     call connectdbn(newPointIndex, k2, newLinkIndex)
                     oneDNode = k2
                  endif
               endif
               if (newLinkIndex.ne.-1) then
                  kn(3,newLinkIndex) = kn3ty
                  numValidLinks = numValidLinks + 1
                  connectionIndexes(1,numValidLinks) = cellId   !2d
                  connectionIndexes(2,numValidLinks) = oneDNode !1d
                  !cell is connected, set kc mask and end cycle
                  kc(cellId) = 2
               endif
            endif
            !loop over numberCellNetlinks
         enddo
       !loop over ncellsInSearchRadius
      enddo
   !loop over numl1d
   enddo

   end function ggeo_make1D2Dembeddedlinks

   !> Makes 1d2d river links: 1D is typically a non-overlapping 2D grid,
   !! and potentially more than one 1d2d link per 1d mesh node is created.
   !! 2D cells are connected if they are at the boundary of the domain and inside the search radius.
   !! They are connected to the nearest 1d point.
   function ggeo_make1D2DRiverLinks(jsferic, jasfer3D, searchRadius, xplRiverLinks, yplRiverLinks, zplRiverLinks, oneDMask) result(ierr)

   use network_data
   use m_missing,       only: dmiss, jins
   use geometry_module, only: dbdistance, crossinbox, dbpinpol
   use kdtree2Factory
   use m_cell_geometry

   implicit none

   !input
   integer, intent(in)              :: jsferic, jasfer3D
   double precision, intent(in)     :: searchRadius
   double precision, optional       :: xplRiverLinks(:), yplRiverLinks(:), zplRiverLinks(:) !polygon to include only selected boundary 2d cells
   integer, optional, intent(in)    :: oneDMask(:)

   !locals
   integer                          :: ierr !< Error status, 0 if success, nonzero in case of error.
   integer                          :: k, kk, k1, k2, k3, k4, ncellsinSearchRadius
   integer                          :: numberCellNetlinks, prevConnected1DNode, newPointIndex, newLinkIndex
   integer                          :: l, cellNetLink, cellId, kn3localType, numnetcells, insidePolygons, clientIndex
   double precision                 :: searchRadiusSquared, maxdistance, prevDistance, currDistance, ldistance, rdistance
   logical                          :: boundaryCell
   type(kdtree_instance)            :: treeinst
   logical                          :: validOneDMask, isPolygonPresent
   integer, allocatable             :: cellTo1DNode(:)
   logical, allocatable             :: isValidCell(:)
   
   ierr = 0
   if (numl1d<=0) then
      return
   endif
   
   ! presence of a valid oneDMask
   validOneDMask = .false.
   if(present(oneDMask)) then
      if(size(oneDMask,1).gt.0) then
         validOneDMask = .true.
      endif
   endif
   
   ! presence of valid polygons 
   isPolygonPresent = present(xplRiverLinks)

   kn3localType = 3 
   call savenet()
   call findcells(0)

   if(allocated(connectionIndexes)) then
      deallocate(connectionIndexes)
   endif
   allocate(connectionIndexes(2,nump))
   connectionIndexes = 0
   numValidLinks = 0
   
   numnetcells = size(xz(:))
   if (numnetcells>0) then
      call build_kdtree(treeinst, size(xz(:)), xz(:), yz(:), ierr, jsferic, dmiss)
   else
      return
   endif

   allocate(cellTo1DNode(nump))
   allocate(isValidCell(nump))
   cellTo1DNode = 0
   isValidCell = .true.
   kc = 0
   
   searchRadiusSquared = searchRadius**2
   do l = 1, numl1d + 1
      !only check the left point
      if( l .eq. numl1d + 1) then
         k1  =  kn(2,l-1)
         k3  =  kn(3,l-1)
      else
         k1  = kn(1,l) 
         k3 = kn(3,l)
      endif
      ! if the link is not 1d cycle
      if (k3.ne.1) cycle
      ! only 1d points outside 2d mesh should be considered 
      cellId = -1
      call incells(xk(k1),yk(k1), cellId)
      if (cellId.ne.0) cycle
      !get the left 1d mesh point
      call make_queryvector_kdtree(treeinst, xk(k1), yk(k1), jsferic)
      !compute the search radius if not provided
      if (searchRadius <= 0.0d0) then
         ldistance = 0.0d0
         rdistance = 0.0d0
         if (l>=2) then
            k4 = kn(1,l-1) 
            ldistance = dbdistance(xk(k4),yk(k4),xk(k1),yk(k1), jsferic, jasfer3D, dmiss)
         endif
         if (l<numl1d + 1) then
            k2 = kn(2,l)
            rdistance = dbdistance(xk(k1),yk(k1),xk(k2),yk(k2), jsferic, jasfer3D, dmiss)
         endif
         searchRadiusSquared = max(ldistance, rdistance) ** 2
      endif
      !count number of cells in the search area
      nCellsInSearchRadius = kdtree2_r_count(treeinst%tree,treeinst%qv,searchRadiusSquared)
      !no cells found
      if ( nCellsInSearchRadius < 1 ) cycle
      !reallocate if necessary
      call realloc_results_kdtree(treeinst, nCellsInSearchRadius)
      !find nearest cells
      call kdtree2_n_nearest(treeinst%tree,treeinst%qv,nCellsInSearchRadius,treeinst%results)
      
      !connection loop
      do k = 1, nCellsInSearchRadius
         ! get the current cell id and the previously connected net node
         cellId= treeinst%results(k)%idx
         ! not a boundary cell
         if (.not.(isValidCell(cellId))) then
            cycle
         endif
	 
         ! check if it is a boundary cell
         boundaryCell = .false.
         do kk =1, size(netcell(cellId)%lin)
            cellNetLink =  netcell(cellId)%lin(kk)
            if(lnn(cellNetLink).eq.1) then
               boundaryCell=.true.
               exit
            endif
         enddo
         if (.not.boundaryCell) then
            isValidCell(cellId) = .false.
            cycle
         endif
         
         ! check if is a valid 2d cell to connect
         if ( isPolygonPresent ) then
            insidePolygons = - 1 
            call dbpinpol(xz(cellId), yz(cellId), insidePolygons, dmiss, jins, size(xplRiverLinks), xplRiverLinks, yplRiverLinks, zplRiverLinks)    
               if (insidePolygons.ne.1) then 
                  isValidCell(cellId) = .false.
                  cycle
               endif
         endif

		 ! a candidate connection already exist
         prevConnected1DNode = cellTo1DNode(cellId)
         if (prevConnected1DNode.eq.0) then
            cellTo1DNode(cellId) = k1
         else if(prevConnected1DNode > 0) then
            prevDistance = dbdistance(xk(prevConnected1DNode), yk(prevConnected1DNode),xz(cellId),yz(cellId), jsferic, jasfer3D, dmiss)
            currDistance = dbdistance(xk(k1), yk(k1), xz(cellId), yz(cellId), jsferic, jasfer3D, dmiss)
            if (currDistance < prevDistance) then
               cellTo1DNode(cellId) = k1
            endif
         endif
      enddo
   enddo

   !make the connections
   kc = 0
   do cellId = 1, nump
      if (cellTo1DNode(cellId) > 0 .and. isValidCell(cellId)) then
         newLinkIndex = -1
         !check presence of oneDMask
         if (validOneDMask) then
            clientIndex =  mesh1dInternalToClientMapping(cellTo1DNode(cellId))
            if (oneDMask(clientIndex)==1) then
               ! call setnewpoint(xz(cellId), yz(cellId), zk(cellId), newPointIndex)
               call dsetnewpoint(xz(cellId), yz(cellId), newPointIndex)
               call connectdbn(newPointIndex, cellTo1DNode(cellId), newLinkIndex)
            endif
         else
            ! call setnewpoint(xz(cellId),yz(cellId),zk(cellId), newPointIndex)
            call dsetnewpoint(xz(cellId),yz(cellId), newPointIndex)
            call connectdbn(newPointIndex, cellTo1DNode(cellId), newLinkIndex)
         endif
         if (newLinkIndex.ne.-1) then
            !cell is connected, set kn
            kn(3,newLinkIndex) = kn3localType
            numValidLinks = numValidLinks + 1
               connectionIndexes(1,numValidLinks) = cellId !2d
               connectionIndexes(2,numValidLinks) = cellTo1DNode(cellId) !1d
         endif
      endif
   enddo

   end function ggeo_make1D2DRiverLinks

   function ggeo_map_2d_cells(meshgeom, mapping) result(ierr)

   use network_data
   use m_missing, only: dmiss, imiss
   use stdlib_sorting, only: radix_sort

   implicit none
   type(t_ug_meshgeom), intent(in)             :: meshgeom
   integer, allocatable,intent(inout)          :: mapping(:)
   double precision, allocatable               :: sorted_faces_x_meshgeom(:,:), sorted_faces_y_meshgeom(:,:)
   double precision, allocatable               :: sorted_faces_x_lib_state(:,:), sorted_faces_y_lib_state(:,:)
   double precision, parameter                 :: tolerance = 1e-8
   integer                                     :: shift
   logical                                     :: isFound
   integer                                     :: i,j,k, ierr

   !for each face sort the x coordinates
   ierr = -1
   
   if(meshgeom%numface < 1)then
      return
   endif
   
   !client 2d mesh, sort coordinate of the cells
   shift = 1 - meshgeom%start_index
   allocate(sorted_faces_x_meshgeom(meshgeom%maxnumfacenodes,meshgeom%numface))
   allocate(sorted_faces_y_meshgeom(meshgeom%maxnumfacenodes,meshgeom%numface))
   sorted_faces_x_meshgeom = dmiss
   sorted_faces_y_meshgeom = dmiss
   do i = 1,meshgeom%numface
      sorted_faces_x_meshgeom(:,i) = meshgeom%nodex(meshgeom%face_nodes(:,i) + shift)
      sorted_faces_y_meshgeom(:,i) = meshgeom%nodey(meshgeom%face_nodes(:,i) + shift)
      call radix_sort(sorted_faces_x_meshgeom(:,i))
      call radix_sort(sorted_faces_y_meshgeom(:,i))
   enddo

   !server mesh, sort coordinate of the cells
   allocate(sorted_faces_x_lib_state(meshgeom%maxnumfacenodes, nump))
   allocate(sorted_faces_y_lib_state(meshgeom%maxnumfacenodes, nump))
   sorted_faces_x_lib_state = dmiss
   sorted_faces_y_lib_state = dmiss
   do i = 1,nump
      sorted_faces_x_lib_state(:,i) = meshgeom%nodex(1)
      sorted_faces_x_lib_state(1:netcell(i)%N,i) = XK(netcell(i)%NOD)
      sorted_faces_y_lib_state(:,i) = meshgeom%nodey(1)
      sorted_faces_y_lib_state(1:netcell(i)%N,i) = YK(netcell(i)%NOD)
      call radix_sort(sorted_faces_x_lib_state(:,i))
      call radix_sort(sorted_faces_y_lib_state(:,i))
   enddo

   !build the mapping2dCells. Note: O(n2) time complexity 
   allocate(mapping(meshgeom%numface))
   mapping = imiss
   do i = 1,meshgeom%numface
      do j= 1, nump
         if(mapping(j).ne.imiss) then
            cycle
         endif
         isFound = .true.
         do k=1,meshgeom%maxnumfacenodes
            if ((abs(sorted_faces_x_meshgeom(k,i)-sorted_faces_x_lib_state(k,j)) > tolerance).or.(abs(sorted_faces_y_meshgeom(k,i)-sorted_faces_y_lib_state(k,j))>tolerance)) then
               isFound = .false.
               exit
            endif
         enddo
         if(isFound) then
            mapping(j) = i
            exit
         endif
      enddo
   enddo

   ierr = 0

   end function ggeo_map_2d_cells

   subroutine network_dimensions_message()
   use network_data
   use MessageHandling
   use m_cell_geometry
   implicit none

   write(msgbuf,'(a,I25)')    'nr of netnodes               ( )  :' , numk                       ; call msg_flush()
   write(msgbuf,'(a,I25)')    'nr of netlinks               ( )  :' , numL                       ; call msg_flush()
   msgbuf = ' ' ; call msg_flush()

   write(msgbuf,'(a,I25)')    'nr of 2D netlinks            ( )  :' , numL - numL1D              ; call msg_flush()
   write(msgbuf,'(a,I25)')    'nr of 1D netlinks            ( )  :' , numL1D                     ; call msg_flush()
   msgbuf = ' ' ; call msg_flush()

   write(msgbuf,'(a,I25)')    'nr of netcells               ( )  :' , nump                       ; call msg_flush()
   write(msgbuf,'(a,I25)')    'nr of netcells 1D2D          ( )  :' , nump1D2D                   ; call msg_flush()
   msgbuf = ' ' ; call msg_flush()

   end subroutine network_dimensions_message
   
   !> Get link angle for a (signed) net link between -pi and pi
   double precision function dLinkangle(L_signed)
      use m_sferic, only: jsferic
      use geometry_module, only: getdxdy 
      use network_data, only: kn, xk, yk
      implicit none
      integer,          intent(in) :: L_signed  !< Signed net link number. May be negative to indicate that this link's orientation is flipped w.r.t. the parent item that it's part of.
      double precision              :: dx, dy
      integer                       :: k1, k2

      if (L_signed > 0) then
         k1 = kn(1,L_signed)
         k2 = kn(2,L_signed)
      else
         k1 = kn(2,-L_signed)
         k2 = kn(1,-L_signed)
      end if
      call getdxdy(xk(k1), yk(k1), xk(k2), yk(k2),dx,dy,jsferic)
      dLinkangle = atan2(dy,dx)
      return
   end function dLinkangle
   
   end module gridoperations