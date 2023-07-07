   !modules from modules.f90
   module m_missing
   implicit none
   double precision                  :: dmiss           = -999d0      !
   double precision                  :: xymis           = -999d0      !
   double precision                  :: dxymis          = -999d0
   !double precision                 :: ieee_negative_inf = -1.7976931348623158e+308 ! IEEE standard for the maximum negative value
   integer                           :: intmiss         = -2147483647 ! integer fillvlue
   integer                           :: imiss           = -999        ! cf_dll missing value 
   integer                           :: LMOD, KMOD                    ! TBV READDY, LC gui related variables can go to unstruc_display
   integer                           :: jins            = 1
   integer                           :: jadelnetlinktyp = 0
   end module m_missing
   
   module m_dimens
   implicit none
   integer                       :: MMAX_old = 3, NMAX_old = 3
   integer                       :: KMAX, LMAX, KNX, MXB

   contains

   function m_dimens_destructor() result (ierr)

   integer ierr
   MMAX_old = 3
   NMAX_old = 3
   KMAX = 0
   LMAX = 0
   KNX   = 0
   MXB  = 0

   ierr = 0
   end function m_dimens_destructor

   end module m_dimens

   module m_landboundary
   implicit none
   double precision, allocatable :: XLAN (:), YLAN(:), ZLAN(:)
   integer, allocatable          :: NCLAN(:)
   integer                       :: MXLAN, MAXLAN

   ! SPvdP: segments
   integer                              :: MXLAN_loc        ! actual MXLAN
   integer                              :: Nlanseg          ! number of land boundary segments
   integer, allocatable, dimension(:,:) :: lanseg_startend  ! segment start and end indices,          dim(2,Nlanseg)
   integer, allocatable, dimension(:)   :: lanseg_map       ! node to land boundary segment mapping,  dim(numk)

   integer                              :: jleft, jright    !< outer land boundary segments in projection

   double precision                     :: rLleft, rLright  !< fractional location of the projected outer nodes (min and max) on the land boundary segment

   double precision                     :: DCLOSE_bound = 5d0 !< close-to-landboundary tolerance, netbound only, measured in number of meshwidths
   double precision                     :: DCLOSE_whole = 1d0 !< close-to-landboundary tolerance, whole network, measured in number of meshwidths

   double precision                     :: DCLOSE = 1d0       ! close-to-landboundary tolerance, measured in number of meshwidths

   logical                              :: Ladd_land = .true. ! add land boundary between land boundary segments that are close to each other

   contains

   subroutine increaselan(n)
   USE m_missing
   !LC TO DO: introduce call back function use unstruc_messages
   use m_alloc
   integer :: n

   integer :: ierr

   IF (N < MAXLAN) RETURN
   MAXLAN = MAX(50000,INT(1.2d0*N))

   call realloc(xlan, MAXLAN, stat=ierr, fill=dxymis)
   !CALL AERR('xlan(maxlan)', IERR, maxlan)
   call realloc(ylan, MAXLAN, stat=ierr, fill=dxymis)
   !CALL AERR('ylan(maxlan)', IERR, maxlan)
   call realloc(zlan, MAXLAN, stat=ierr, fill=dxymis)
   !CALL AERR('zlan(maxlan)', IERR, maxlan)
   call realloc(nclan, MAXLAN, stat=ierr, fill=0)
   !CALL AERR('nclan(maxlan)', IERR, maxlan/2)
   end subroutine increaselan

   end module m_landboundary




   module m_sferic
   implicit none
   integer                           :: jsferic = 0       ! xy pair is in : 0=cart, 1=sferic coordinates
   integer                           :: jsfertek= 0       ! drawn in 0=cart, 1=stereografisch
   integer                           :: jasfer3D = 0      ! 0 = org, 1 = sqrt(dx2+dy2+dz2), 2= greatcircle
   integer                           :: jglobe  = 0       ! if (jsferic==1) do we need extra tests for 360-0 transgression
   integer                           :: jamidlat = 0      ! if (jsferic==1) use middle latitude in refinement
   
   double precision                  :: pi                ! pi
   double precision                  :: twopi             ! 2pi
   double precision                  :: dg2rd             ! degrees to radians
   double precision                  :: rd2dg             ! and vice versa
   double precision                  :: ra = 6378137d0    ! earth radius (m)
   double precision                  :: omega             ! earth angular velocity (rad/s)
   double precision                  :: fcorio            ! 2omegasinfi
   double precision                  :: anglat = 0d0      ! 26.0     ! dubai 52.5     ! angle of latitude  (horizontal)
   double precision                  :: anglon = 0d0      ! 26.0     ! dubai 52.5     ! angle of longitude (vertical)
   double precision                  :: dy2dg             ! from dy in m to lat in degrees
   double precision                  :: csphi             ! cosphi of latest requested
   double precision                  :: xwleft = -180d0   ! leftmost world coordinate degreees


   double precision, parameter       :: dtol_pole = 1d-4   ! pole tolerance in degrees

   contains

   subroutine default_sferic()
      jasfer3D = 1
   end subroutine default_sferic
   end module m_sferic

   module m_polygon

      implicit none
      
      double precision, allocatable  :: XPL (:), YPL (:), ZPL (:), XPH(:), YPH(:), ZPH(:), DZL(:), DZR(:), DCREST(:), DTL(:), DTR(:), DVEG(:)
      integer, allocatable           :: IWEIRT(:)
      integer                        :: NPL, NPH, MAXPOL, MP, MPS, jakol45 = 0
      character(len=64), allocatable :: nampli(:) ! Names of polylines, set in reapol,
      ! not shifted/updated during editpol.
      double precision               :: dxuni=40d0  ! uniform spacing
      integer                        :: MAXPOLY=1000 ! will grow if needed
      double precision, allocatable  :: xpmin(:), ypmin(:), xpmax(:), ypmax(:), zpmin(:), zpmax(:)
      integer                        :: Npoly
      integer,          allocatable  :: iistart(:), iiend(:)
      integer,          allocatable  :: ipsection(:)
   
      contains
      !> Increase size of global polyline array.
      !! Specify new size and whether existing points need to be maintained.
      subroutine increasepol(N, jaKeepExisting)
         use m_missing
         use m_alloc
         implicit none
         integer :: n              !< Desired new minimum size
         integer :: jaKeepExisting !< Whether or not (1/0) to keep existing points.
         logical :: jakeep
         integer :: maxpolcur
         integer :: ierr

         maxpolcur = size(xpl)
         IF (N <= maxpolcur ) THEN
            RETURN
         ENDIF
         MAXPOL = MAX(100000,INT(5d0*N))

         jakeep = jaKeepExisting==1

         call realloc(xpl, maxpol, keepExisting=jakeep, fill=dxymis, stat=ierr)
         call realloc(ypl, maxpol, keepExisting=jakeep, fill=dxymis, stat=ierr)
         call realloc(zpl, maxpol, keepExisting=jakeep, fill=dxymis, stat=ierr)

         if (jakol45 == 1) then
            call realloc(dzl, maxpol, keepExisting=jakeep, fill=dxymis, stat=ierr)
            call realloc(dzr, maxpol, keepExisting=jakeep, fill=dxymis, stat=ierr)
         else if (jakol45 == 2) then
            call realloc(dcrest, maxpol, keepExisting=jakeep, fill=dxymis, stat=ierr)
            call realloc(dzl, maxpol, keepExisting=jakeep, fill=dxymis, stat=ierr)
            call realloc(dzr, maxpol, keepExisting=jakeep, fill=dxymis, stat=ierr)
            call realloc(dtl, maxpol, keepExisting=jakeep, fill=dxymis, stat=ierr)
            call realloc(dtr, maxpol, keepExisting=jakeep, fill=dxymis, stat=ierr)
            call realloc(dveg, maxpol, keepExisting=jakeep, fill=dxymis, stat=ierr)
            call realloc(iweirt, maxpol, keepExisting=jakeep, stat=ierr)
         endif

         !     make sure nampli is allocated
         if ( .not.allocated(nampli) ) then
            allocate(nampli(0))
         end if

      end subroutine increasepol
      
      !> Copies the global polygon into the backup polygon arrays.
      subroutine SAVEPOL()

         use m_alloc
         use m_missing
         implicit none

         call realloc(xph, maxpol, keepExisting=.false.)
         call realloc(yph, maxpol, keepExisting=.false.)
         call realloc(zph, maxpol, keepExisting=.false.)

         IF (NPL > 0) THEN
            XPH(1:NPL) = XPL(1:NPL)
            YPH(1:NPL) = YPL(1:NPL)
            ZPH(1:NPL) = ZPL(1:NPL)
         ENDIF

         MPS = MP
         NPH = NPL

         return
      end subroutine savepol


      !> Puts back a previously saved backup polygon into the global polygon arrays.
      subroutine RESTOREPOL()
         use m_alloc
         use m_missing
         implicit none

         maxpol = max(maxpol, nph)
         call realloc(xpl, maxpol, keepExisting=.false.)
         call realloc(ypl, maxpol, keepExisting=.false.)
         call realloc(zpl, maxpol, keepExisting=.false.)

         IF (NPH > 0) THEN
            XPL(1:NPH) = XPH(1:NPH)
            YPL(1:NPH) = YPH(1:NPH)
            ZPL(1:NPH) = ZPH(1:NPH)
         ENDIF

         MP  = MPS
         NPL = NPH

         return
      end subroutine restorepol
      
      
      function m_polygon_destructor() result (ierr)

      implicit none

      integer :: ierr

      ierr = 0

      if(allocated(XPL).and.ierr==0) deallocate(XPL, stat = ierr)
      if(allocated(YPL).and.ierr==0) deallocate(YPL, stat = ierr)
      if(allocated(ZPL).and.ierr==0) deallocate(ZPL, stat = ierr)
      if(allocated(XPH).and.ierr==0) deallocate(XPH, stat = ierr)

      if(allocated(YPH).and.ierr==0) deallocate(YPH, stat = ierr)
      if(allocated(ZPH).and.ierr==0) deallocate(ZPH, stat = ierr)
      if(allocated(ZPH).and.ierr==0) deallocate(ZPH, stat = ierr)

      if(allocated(DZL).and.ierr==0)    deallocate(DZL, stat = ierr)
      if(allocated(DZR).and.ierr==0)    deallocate(DZR, stat = ierr)
      if(allocated(DCREST).and.ierr==0) deallocate(DCREST, stat = ierr)
      if(allocated(DTL).and.ierr==0)    deallocate(DTL, stat = ierr)
      if(allocated(DTR).and.ierr==0)    deallocate(DTR, stat = ierr)
      if(allocated(DVEG).and.ierr==0)   deallocate(DVEG, stat = ierr)
      if(allocated(IWEIRT).and.ierr==0) deallocate(IWEIRT, stat = ierr)
      
      if(allocated(xpmin).and.ierr==0) deallocate(xpmin, stat = ierr)
      if(allocated(ypmin).and.ierr==0) deallocate(ypmin, stat = ierr)
      if(allocated(xpmax).and.ierr==0) deallocate(xpmax, stat = ierr)
      if(allocated(ypmax).and.ierr==0) deallocate(ypmax, stat = ierr)
      if(allocated(zpmin).and.ierr==0) deallocate(zpmin, stat = ierr)
      if(allocated(zpmax).and.ierr==0) deallocate(zpmax, stat = ierr)
      if(allocated(iistart).and.ierr==0)   deallocate(iistart, stat = ierr)
      if(allocated(iiend).and.ierr==0)     deallocate(iiend, stat = ierr)
      if(allocated(ipsection).and.ierr==0) deallocate(ipsection, stat = ierr)

      jakol45 = 0
      dxuni=40d0
      MAXPOLY=1000
      NPL = 0
      NPH = 0
      MAXPOL = 0
      MP = 0
      MPS = 0
      Npoly = 0

      end function m_polygon_destructor
            
   end module m_polygon

   !
   ! Stores the coordinates of the cells
   !
   module m_cell_geometry
   ! TODO: UNST-1705: LC: I want ndx2d and ndx back into m_flowgeom, as these are flowgeom and not netgeom. Only findcells and update_cell_circumcenters need a change first.
   integer, target                       :: ndx2d      !< [-] Number of 2D flow cells (= NUMP). {"rank": 0}
   integer, target                       :: ndx        !< [-] Number of flow nodes (internal + boundary). {"rank": 0}
   double precision, allocatable, target :: xz (:)     !< [m/degrees_east] waterlevel point / cell centre, x-coordinate (m) {"location": "face", "shape": ["ndx"]}
   double precision, allocatable         :: xz0(:)     !< backup of xz
   double precision, allocatable, target :: yz (:)     !< [m/degrees_north] waterlevel point / cell centre, y-coordinate (m) {"location": "face", "shape": ["ndx"]}
   double precision, allocatable         :: yz0(:)     !< backup of yz
   double precision, allocatable, target :: ba (:)     !< [m2] bottom area, if < 0 use table in node type {"location": "face", "shape": ["ndx"]}
   double precision, allocatable         :: ba0(:)     ! Backup of ba
   ! TODO: UNST-1705: LC: the above variables used to be automatically available in the dflowfm BMI, via the JSON annotated documentation string, this is now broken, needs fixing.

   end module m_cell_geometry


   module M_afmeting
   implicit none
   double precision :: RLENGTH, RWIDTH, RTHICK, RDIAM, RLMIN
   integer :: JVAST, MC, NC, K0, LFAC
   end module M_afmeting

   !> Orthogonalisation settings, both for regular grids and unstructured nets.
   module m_ggeo_orthosettings
   implicit none
   integer          :: ITATP = 25  !< Nr. of outer    iterations in grid/net orthogonalisation.
   integer          :: ITBND = 1   !< Nr. of boundary iterations in grid/net orthogonalisation. (within ITATP)
   integer          :: ITIN  = 25  !< Nr. of inner    iterations in grid/net orthogonalisation. (within ITBND)
   !! Also used within transfinite regular grid generation.
   integer          :: JAPROJECT = 1 !< Project nodes back to boundary (2: yes, all, 1:yes, net bounds only, 0:no)
   double precision :: ATPF = 0.95d0  !< Factor (0.<=ATPF<=1.) between grid smoothing and grid ortho resp.
   double precision :: ATPF_B = 1d0 !< minimum ATPF on the boundary
   double precision :: circumormasscenter = 1d0          !< 1.0 = circumcentre,      0.0 = masscentre, 1.0 -> 0.0 : weighted
   double precision :: smoothorarea    = 1d0   !< Factor between smoother (1d0) and area-homogenizer (0d0)
   integer          :: adapt_method    = 1     !< Mesh-adaptation method; 0: Winslow, 1: arc-length, 2: harmonic map
   double precision :: adapt_beta      = 0.0d0 !< Mesh-refinement factor; between 0d0 and 1d0
   integer          :: adapt_niter_u   = 0     !< number of smoothing iterations of `solution` u in adaptation
   integer          :: adapt_niter_G   = 4     !< number of smoothing iterations of monitor matrix G in adaptation
   double precision :: ortho_pure      = 0.5d0   !< curvi-linear-like (0d0) or pure (1d0) orthogonalisation

   end module m_ggeo_orthosettings

   module m_WEARELT
   double precision :: XMIN,YMIN,XMAX,YMAX,X1,Y1,X2,Y2,RCIR,CR,DSIX
   end module m_WEARELT

   module m_sferzoom
   double precision :: X0,Y0,dyh,FAC,X1W,Y1W,X2W,Y2W
   end module m_sferzoom


   !> Main sample set
   module m_samples
   implicit none
   double precision, ALLOCATABLE  :: XS(:), YS(:), ZS(:)   !< x-coordinate, y-coordinate and value of sample set
   INTEGER,          ALLOCATABLE  :: IPSAM(:)              !< permutation array (increasing x-coordinate)
   integer,          parameter    :: IPSTAT_OK=0           !< permutation array is OK
   integer,          parameter    :: IPSTAT_NOTOK=1        !< permutation array is out of date
   integer                        :: IPSTAT=IPSTAT_NOTOK   !< permutation array status
   INTEGER                        :: NS =0, NSMAX
   integer                        :: MXSAM=0, MYSAM=0      !< structured block sizes (.gt.0), or not structured (0)
   double precision               :: xsammin, ysammin, xsammax, ysammax   !< bounding box corner coordinates

   !> Backup of main sample set
   !! @see savesam()
   double precision, ALLOCATABLE  :: XS2(:), YS2(:), ZS2(:)
   integer                        :: NS2, MXSAM2=0, MYSAM2=0

   !> Alternate sample set.
   !! @see SWAPSAMPLES()
   double precision, ALLOCATABLE  :: XS3(:), YS3(:), ZS3(:)
   integer              :: NS3


   contains
   
   subroutine increasesam(N)
   USE M_MISSING
   use m_alloc
   implicit none
   integer, intent(in) :: n !< New size for sample set #3.

   integer :: ierr
   IF (N < NSMAX) RETURN
   NSMAX = MAX(10000,INT(1.2d0*N))

   call realloc(xs, NSMAX, keepExisting=.true., fill = dmiss, stat=ierr)
   !CALL AERR ('XS(NSMAX)',IERR,NSMAX)
   call realloc(ys, NSMAX, keepExisting=.true., fill = dmiss, stat=ierr)
   !CALL AERR ('YS(NSMAX)',IERR,NSMAX)
   call realloc(zs, NSMAX, keepExisting=.true., fill = dmiss, stat=ierr)
   !CALL AERR ('ZS(NSMAX)',IERR,NSMAX)
   call realloc(ipsam, NSMAX, keepExisting=.false., fill=0, stat=ierr)
   !CALL AERR ('IPSAM',IERR,NSMAX)

   !User is editing samples: mark samples as unstructured
   MXSAM = 0
   MYSAM = 0
   IPSTAT = IPSTAT_NOTOK

   end subroutine increasesam

   subroutine savesam()
   USE M_MISSING
   use m_alloc
   implicit none
   integer :: ierr
   NS2 = NS
   MXSAM2 = MXSAM
   MYSAM2 = MYSAM
   IF (NS .EQ. 0) return

   call realloc(xs2, ns, keepExisting=.false., fill=dmiss, stat=ierr)
   call realloc(ys2, ns, keepExisting=.false., fill=dmiss, stat=ierr)
   call realloc(zs2, ns, keepExisting=.false., fill=dmiss, stat=ierr)

   XS2(1:NS) = XS(1:NS)
   YS2(1:NS) = YS(1:NS)
   ZS2(1:NS) = ZS(1:NS)
   return
   end subroutine savesam

   subroutine restoresam()

   implicit none
   MXSAM = 0   ! unstructured samples by default
   MYSAM = 0
   IPSTAT = IPSTAT_NOTOK
   NS    = NS2
   MXSAM = MXSAM2
   MYSAM = MYSAM2

   if (NS2 == 0) return

   XS2(1:NS2) = XS(1:NS2)
   YS2(1:NS2) = YS(1:NS2)
   ZS2(1:NS2) = ZS(1:NS2)

   end subroutine restoresam
   
   SUBROUTINE SWAPSAMPLES()
      implicit none
      integer :: i
      integer :: nh
      integer :: nn
      DOUBLE PRECISION :: XH, YH, ZH

      IF (NSMAX < NS3) THEN
         CALL increasesam(NS3)
      ELSE IF (NS3 < NS) THEN
         CALL increasesam3(NS)
      ENDIF
      NN = MAX(NS,NS3)
      NH = NS ; NS = NS3 ; NS3 = NH
      DO I = 1, NN
         XH = XS(I) ; XS(I) = XS3(I) ; XS3(I) = XH
         YH = YS(I) ; YS(I) = YS3(I) ; YS3(I) = YH
         ZH = ZS(I) ; ZS(I) = ZS3(I) ; ZS3(I) = ZH
      ENDDO
   END SUBROUTINE SWAPSAMPLES
   
   
   SUBROUTINE INCREASESAM3(N)
      USE M_MISSING
      use m_alloc
      implicit none
      integer, intent(in) :: n !< New size for sample set #3.

      integer :: ierr
      integer :: nsmaxloc

      nsmaxloc = SIZE(XS3)

      IF (N < nsmaxloc) RETURN
      nsmaxloc = MAX(10000,INT(1.2d0*N))
      
      call realloc(xs3, nsmaxloc, keepExisting=.true., fill = dmiss, stat=ierr)
      call realloc(ys3, nsmaxloc, keepExisting=.true., fill = dmiss, stat=ierr)
      call realloc(zs3, nsmaxloc, keepExisting=.true., fill = dmiss, stat=ierr)
      !CALL AERR ('XS3(NSMAX),YS3(NSMAX),ZS3(NSMAX)',IERR,nsmaxloc)
   END SUBROUTINE INCREASESAM3

   end module m_samples
