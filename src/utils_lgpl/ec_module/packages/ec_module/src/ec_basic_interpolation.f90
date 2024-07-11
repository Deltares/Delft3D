!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
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
!  
!  
!  This module contains basic interpolation routines
!
   !Global modules
   module m_ec_triangle           ! original name : m_triangle
   use precision, only : hp
   implicit none
   real(kind=hp)   , allocatable :: XCENT(:), YCENT(:)
   integer, allocatable          :: INDX(:,:)
   integer, allocatable          :: EDGEINDX(:,:)
   integer, allocatable          :: TRIEDGE(:,:)
   integer                       :: NUMTRI
   integer                       :: NUMTRIINPOLYGON
   integer                       :: NUMEDGE
   integer, parameter            :: ITYPE = 2 ! 1 = ORIGINAL FORTRAN ROUTINE, 2 = NEW C ROUTINE

   integer                       :: jagetwf = 0    ! if 1, also assemble weightfactors and indices in:
   integer, allocatable          :: indxx(:,:)     ! to be dimensioned by yourselves 3,*
   real(kind=hp)   , allocatable :: wfxx (:,:)

   type t_nodi
      integer                    :: NUMTRIS       ! total number of TRIANGLES ATtached to this node
      integer, allocatable       :: TRINRS(:)     ! numbers of ATTACHED TRIANGLES
   end type t_nodi

   type (t_nodi), dimension(:), allocatable :: NODE

   !integer, dimension(:,:), allocatable :: trinods ! triangle nodes, dim(3,numtri)
   integer, dimension(:,:), allocatable :: LNtri  ! triangles connected to edges, dim(2,numedges)

   integer                              :: IDENT   ! identifier
   integer, dimension(:),   allocatable :: imask   ! mask array for triangles

   end module m_ec_triangle


   module m_ec_interpolationsettings
   use precision, only : hp
   implicit none
   integer, parameter              :: INTP_INTP = 1
   integer, parameter              :: INTP_AVG  = 2

   !> Averaging methods, when interpolation type == INTP_AVG
   integer, parameter              :: AVGTP_MEAN      = 1 !< Simple mean of all samples in search cell
   integer, parameter              :: AVGTP_NEARESTNB = 2 !< Nearest point to search center
   integer, parameter              :: AVGTP_MAX       = 3 !< Maximum of all samples in search cell, possibly with percentileminmax
   integer, parameter              :: AVGTP_MIN       = 4 !< Minimum of all samples in search cell, possibly with percentileminmax
   integer, parameter              :: AVGTP_INVDIST   = 5 !< Inverse distance weighted average of all samples in search cell
   integer, parameter              :: AVGTP_MINABS    = 6 !< Sample with smallest absolute value in search cell.
   integer, parameter              :: AVGTP_MEDIAN    = 7 !< Median value of all samples in search cell.

   integer                         :: INTERPOLATIONTYPE            ! 1 = TRIANGULATION/BILINEAR INTERPOLATION 2= CELL AVERAGING
   integer                         :: JTEKINTERPOLATIONPROCESS     ! TEKEN INTERPOLATION PROCESS YES/NO 1/0
   integer                         :: IAV                          ! AVERAGING METHOD, One of AVGTP_* parameters: 1 = SIMPLE AVERAGING, 2 = CLOSEST POINT, 3 = MAX, 4 = MIN, 5 = INVERSE WEIGHTED DISTANCE, 6 = MINABS, 7 = MEDIAN
   integer                         :: NUMMIN                       ! MINIMUM NR OF POINTS NEEDED INSIDE CELL TO HANDLE CELL
   real(kind=hp)   , parameter     :: RCEL_DEFAULT = 1.01d0        ! we need a default
   real(kind=hp)                   :: RCEL                         ! RELATIVE SEARCH CELL SIZE, DEFAULT 1D0 = ACTUAL CELL SIZE, 2D0=TWICE AS LARGE
   integer                         :: Interpolate_to               ! 1=bathy, 2=zk, 3=s1, 4=Zc
   real(kind=hp)                   :: percentileminmax             ! if non zero, take average of highest or lowest percentile

   contains

   !> set default interpolation settings
   subroutine default_interpolationsettings()
   implicit none

   INTERPOLATIONTYPE        = INTP_INTP      ! 1 = TRIANGULATION/BILINEAR INTERPOLATION 2= CELL AVERAGING
   JTEKINTERPOLATIONPROCESS = 0              ! TEKEN INTERPOLATION PROCESS YES/NO 1/0
   IAV                      = AVGTP_MEAN     ! AVERAGING METHOD, 1 = SIMPLE AVERAGING, 2 = CLOSEST POINT, 3 = MAX, 4 = MIN, 5 = INVERSE WEIGHTED DISTANCE, 6 = MINABS, 7 = MEDIAN
   NUMMIN                   = 1              ! MINIMUM NR OF POINTS NEEDED INSIDE CELL TO HANDLE CELL
   RCEL                     = RCEL_DEFAULT   ! RELATIVE SEARCH CELL SIZE, DEFAULT 1D0 = ACTUAL CELL SIZE, 2D0=TWICE AS LARGE
   Interpolate_to           = 2              ! 1=bathy, 2=zk, 3=s1, 4=Zc
   percentileminmax         = 0d0
   end subroutine default_interpolationsettings

   end module m_ec_interpolationsettings

   !---------------------------------------------------------------------------!
   !---------------------------------------------------------------------------!
   !   m_ec_basic_interpolation
   !---------------------------------------------------------------------------!
   !---------------------------------------------------------------------------!

   module m_ec_basic_interpolation

   use precision, only : sp, hp
   use MessageHandling, only: msgbox, mess, LEVEL_ERROR
   use geometry_module
   use m_ec_triangle
   use m_ec_interpolationsettings
   use mathconsts,  only: degrad_hp
   use kdtree2Factory
   use m_alloc, only : aerr, realloc
   !use gridgeom
 
   interface triinterp2
      module procedure triinterp2_dbldbl
      module procedure triinterp2_realdbl
      module procedure triinterp2_realreal
   end interface triinterp2

   interface nearest_neighbour
      module procedure nearest_neighbour_dbl
      module procedure nearest_neighbour_real
   end interface nearest_neighbour

   type TerrorInfo
      logical                       :: success
      integer                       :: cntNoSamples
      character(len=:), allocatable :: message
   end type TerrorInfo

   private

   public   ::  bilin_interp
   public   ::  nearest_neighbour
   public   ::  TRIINTfast
   public   ::  AVERAGING2
   public   ::  dlaun
   public   ::  comp_x_dxdxi
   public   ::  bilin_interp_loc
   public   ::  triinterp2
   public   ::  TerrorInfo

   contains

   !> Bilinear interpolation for uniform rectangular.
   !! TODO: move to ec_basic_interpolation or bilin5
   subroutine bilinarc(xk, yk, zk, n)
   use m_missing
   integer      , intent(in)    :: n
   real(kind=hp), intent(in)    :: xk(:), yk(:)
   real(kind=hp), intent(  out) :: zk(:)
   
   integer          :: k
   
   do k = 1,n
      if (zk(k) == dmiss) then 
         call bilinarcinfo( xk(k), yk(k), zk(k))
      endif 
   enddo
   end subroutine bilinarc

   !> Bilinear interpolation for uniform rectangular for 1 point
   !! TODO: move to ec_basic_interpolation or bilin5
   subroutine bilinarcinfo( x, y, z)
   use m_arcinfo
   use m_missing
   real(kind=hp), intent(in)    :: x, y
   real(kind=hp), intent(  out) :: z
   
   real(kind=hp)    :: dm, dn, am, an
   integer          :: m, n
   
   dm = (x - x0)/dxa ; m = int(dm) ; am = dm - m ; m = m + 1
   dn = (y - y0)/dya ; n = int(dn) ; an = dn - n ; n = n + 1
   z  = dmiss
   if (m < mca .and. n < nca .and. m >= 1 .and. n >= 1) then 
      z  =        am  *        an    * d(m+1 , n+1) + &
           (1d0 - am) *        an    * d(m   , n+1) + &
           (1d0 - am) * (1d0 - an)   * d(m   , n  ) + &
                  am  * (1d0 - an)   * d(m+1 , n  ) 
   else 
      z  = dmiss
   endif

   end subroutine bilinarcinfo



   !---------------------------------------------------------------------------!
   !   Triinterp
   !---------------------------------------------------------------------------!

   subroutine triinterp2_dbldbl(XZ, YZ, BL, NDX, JDLA, XS, YS, ZS, NS, dmiss, jsferic, jins, &
                               jasfer3D, NPL, MXSAM, MYSAM, XPL, YPL, ZPL, transformcoef, kcc)
   use m_arcinfo
   implicit none
   !
   ! Parameters
   real(kind=hp), intent(in)            :: XZ(:)
   real(kind=hp), intent(in)            :: YZ(:)
   real(kind=hp), intent(inout)         :: BL(NDX)
   integer,       intent(in)            :: NDX
   integer,       intent(out)           :: JDLA
   integer,       intent(in)            :: NS
   integer,       intent(in)            :: jins
   integer,       intent(in)            :: jasfer3D
   integer,       intent(in)            :: NPL
   integer,       intent(in)            :: MXSAM
   integer,       intent(in)            :: MYSAM
   real(kind=hp), intent(in)            :: XS(:)
   real(kind=hp), intent(in)            :: YS(:)
   real(kind=hp), intent(in)            :: ZS(:)
   real(kind=hp), intent(in)            :: dmiss
   real(kind=hp), intent(in)            :: XPL(:)
   real(kind=hp), intent(in)            :: YPL(:)
   real(kind=hp), intent(in)            :: ZPL(:)
   real(kind=hp), intent(in)            :: transformcoef(:)
   integer,       intent(in), optional  :: kcc(:)            !< index array for elements to be skipped
   !
   ! Locals
   integer :: jakdtree
   integer :: jsferic
   !
   ! Body

   if (ndx < 1) return

   jakdtree = 1

   if ( MXSAM > 0 .and. MYSAM >  0 ) then  ! bi-linear interpolation
      call bilin_interp(NDX, XZ, YZ, BL, dmiss, XS, YS, ZS, MXSAM, MYSAM, jsferic, kcc) ! does not work on globe%
   else if (mca > 0) then 
      call bilinarc(xz, yz, bl, ndx)
   else  ! Delauny
      call TRIINTfast(XS,YS,ZS,NS,1,XZ,YZ,BL,Ndx,JDLA, jakdtree, jsferic, Npl, jins, dmiss, jasfer3D, XPL, YPL, ZPL, transformcoef, kcc)
   end if

   end subroutine triinterp2_dbldbl

   subroutine triinterp2_realdbl(XZ, YZ, BL, NDX, JDLA, XS, YS, ZS, NS, dmiss, jsferic, jins, &
                                jasfer3D, NPL, MXSAM, MYSAM, XPL, YPL, ZPL, transformcoef)
   implicit none
   !
   ! parameters
   real(kind=sp), intent(in)       :: XZ(NDX)
   real(kind=sp), intent(in)       :: YZ(NDX)
   real(kind=hp), intent(inout)    :: BL(NDX)
   integer,       intent(in)       :: NDX
   integer,       intent(out)      :: JDLA
   integer,       intent(in)       :: NS
   integer,       intent(in)       :: jins
   integer,       intent(in)       :: jasfer3D
   integer,       intent(in)       :: NPL
   integer,       intent(in)       :: MXSAM
   integer,       intent(in)       :: MYSAM
   real(kind=hp), intent(in)       :: XS(:)
   real(kind=hp), intent(in)       :: YS(:)
   real(kind=hp), intent(in)       :: ZS(:)
   real(kind=hp), intent(in)       :: dmiss
   integer, intent(in)             :: jsferic
   real(kind=hp), intent(in)       :: XPL(:)
   real(kind=hp), intent(in)       :: YPL(:)
   real(kind=hp), intent(in)       :: ZPL(:)
   real(kind=hp), intent(in)       :: transformcoef(:)
   !
   ! locals
   integer                                   :: ierror
   real(kind=hp), dimension (:), allocatable :: xz_dbl
   real(kind=hp), dimension (:), allocatable :: yz_dbl
   !
   ! body
   allocate (xz_dbl(NDX), stat=ierror)
   allocate (yz_dbl(NDX), stat=ierror)

   xz_dbl = dble(xz)
   yz_dbl = dble(yz)
   call triinterp2_dbldbl(xz_dbl, yz_dbl, bl, NDX, JDLA, XS, YS, ZS, NS, dmiss, jsferic, jins, &
                          jasfer3D, NPL, MXSAM, MYSAM, XPL, YPL, ZPL, transformcoef)
   deallocate (xz_dbl, stat=ierror)
   deallocate (yz_dbl, stat=ierror)
   end subroutine triinterp2_realdbl


   subroutine triinterp2_realreal(XZ, YZ, BL, NDX, JDLA, XS, YS, ZS, NS, dmiss, jsferic, jins, &
                                  jasfer3D, NPL, MXSAM, MYSAM, XPL, YPL, ZPL, transformcoef)
   implicit none
   !
   ! parameters
   real(kind=sp), intent(in)            :: XZ(NDX)
   real(kind=sp), intent(in)            :: YZ(NDX)
   real(kind=sp), intent(inout)         :: BL(NDX)
   integer,       intent(in)            :: NDX
   integer,       intent(out)           :: JDLA
   integer,       intent(in)            :: NS
   integer,       intent(in)            :: jins
   integer,       intent(in)            :: jasfer3D
   integer,       intent(in)            :: NPL
   integer,       intent(in)            :: MXSAM
   integer,       intent(in)            :: MYSAM
   real(kind=hp), intent(in)            :: XS(:)
   real(kind=hp), intent(in)            :: YS(:)
   real(kind=hp), intent(in)            :: ZS(:)
   real(kind=hp), intent(in)            :: dmiss
   integer                              :: jsferic
   real(kind=hp), intent(in)            :: XPL(:)
   real(kind=hp), intent(in)            :: YPL(:)
   real(kind=hp), intent(in)            :: ZPL(:)
   real(kind=hp), intent(in)            :: transformcoef(:)
   !
   ! locals
   integer                                   :: ierror
   real(kind=hp), dimension (:), allocatable :: xz_dbl
   real(kind=hp), dimension (:), allocatable :: yz_dbl
   real(kind=hp), dimension (:), allocatable :: bl_dbl
   !
   ! body
   allocate (xz_dbl(NDX), stat=ierror)
   allocate (yz_dbl(NDX), stat=ierror)
   allocate (bl_dbl(NDX), stat=ierror)

   xz_dbl = dble(xz)
   yz_dbl = dble(yz)
   bl_dbl = dble(bl)
   call triinterp2_dbldbl(xz_dbl, yz_dbl, bl_dbl, NDX, JDLA,&
                        XS, YS, ZS, NS, dmiss, jsferic, jins, jasfer3D, NPL, MXSAM, MYSAM, XPL, YPL, ZPL, transformcoef)
   bl = real(bl_dbl)
   deallocate (xz_dbl, stat=ierror)
   deallocate (yz_dbl, stat=ierror)
   deallocate (bl_dbl, stat=ierror)
   end subroutine triinterp2_realreal

   subroutine TRIINTfast(XS_IN, YS_IN, ZS_IN, NS,NDIM,X,Y,Z,NXY,JDLA,jakdtree, jsferic, NH, jins, dmiss, jasfer3D, &
                         XH, YH, ZH, transformcoef, kc)
   implicit none
   !
   ! parameters
   integer,       intent(inout)        :: jakdtree               !< use kdtree (1) or not (0)
   integer,       intent(in)           :: ns, NDIM               !< sample vector dimension
   integer,       intent(in)           :: nh, jins, jsferic, nxy
   integer,       intent(out)          :: jdla                   ! always 0 on return
   real(kind=hp), intent(in)           :: dmiss
   integer,       intent(in)           :: jasfer3D
   real(kind=hp), intent(in), target   :: XS_IN(ns), YS_IN(ns), ZS_IN(NDIM,ns) !< sample coordinates and values
   real(kind=hp), intent(in)           :: X(:), Y(:)                           !< node coordinates
   real(kind=hp), intent(inout)        :: Z(NDIM,nxy)                          !< node values
   real(kind=hp), intent(in)           :: XH(:), YH(:), ZH(:), transformcoef(:)
   integer,       intent(in), optional :: kc(:)
   !
   ! locals
   real(kind=hp)                       :: af
   integer                             :: i, ierr, k, intri, jslo
   integer                             :: n, in, nxx
   integer                             :: naf
   integer                             :: nbf
   integer                             :: ncf
   integer                             :: ncol
   integer                             :: nrfind
   integer                             :: n2
   integer                             :: idim
   real(kind=hp)                       :: rd
   real(kind=hp)                       :: slo(NDIM)
   real(kind=hp)                       :: xp, yp, zp(NDIM), xpmin, xpmax,ypmin, ypmax
   real(kind=hp)                       :: XL(3),YL(3)
   real(kind=hp), allocatable          :: xx(:), yy(:) , zz(:,:)
   integer      , allocatable          :: ind(:)
   real(kind=hp), allocatable, target  :: xs1(:), ys1(:), zs1(:,:)      ! for store/restore of xs, ys, zs
   real(kind=hp), pointer              :: xs(:), ys(:), zs(:,:)
   real(kind=hp)                       :: xsmin, ysmin, xsmax, ysmax  ! bounding box corner coordinates
   integer                             :: indf(3)
   real(kind=hp)                       :: wf(3)
   integer                             :: NS1           ! for store/restore of NS
   integer                             :: jadum, ierror
   logical                             :: Ldeleteddata  ! for store/restore of xs, ys and zs
   integer                             :: KMOD, jakc
   type(kdtree_instance)               :: sampletree
   integer                             :: numsam, ii, k1, jakdtree2
   real(kind=hp)                       :: R2Search, dist2, cof1, cof2

   jakdtree2 = jakdtree

   jakc = 0
   if (present(kc)) jakc = 1

   if ( jakdtree == 1 ) then
      !       enforce generation of kdtree
      treeglob%itreestat = ITREE_EMPTY
   end if

   !     JSLO=1, OOK SLOPES RD4
   IF (NS  <=  2) RETURN
   JSLO = 0

   Ldeleteddata = .false.

   jdla = 1   ! later, maybe remove this if you want to avoid dlauny every time. This is not working now.
   IF (JDLA == 1) THEN

      IF (Nh > 2 ) THEN ! polygon size reduction

         xpmin = minval(xh(1:nh))
         xpmax = maxval(xh(1:nh))
         ypmin = minval(yh(1:nh))
         ypmax = maxval(yh(1:nh))
         rd    = 0.2*(xpmax-xpmin)
         xpmin = xpmin - rd
         xpmax = xpmax + rd
         rd    = 0.2*(ypmax-ypmin)
         ypmin = ypmin - rd
         ypmax = ypmax + rd

         !           store original data
         allocate(xs1(NS), ys1(NS), zs1(NDIM,NS), stat=ierr)
         call aerr('xs1(NS), ys1(NS), zs1(NDIM,NS)', ierr, (2+NDIM)*NS )
         NS1 = NS
         do i=1,NS
            xs1(i) = xs_in(i)
            ys1(i) = ys_in(i)
            do k=1,NDIM
               zs1(k,i) = zs_in(k,i)
            end do
         end do
         xs => xs1
         ys => ys1
         zs => zs1
         Ldeleteddata = .true.

         n2   = 0
         idim = 1 ! DMISS check on first sample dimension only
         in   = -1
         DO K = 1,Ns
            if (jins == 1) then
               if ( xs(k) > xpmin .and. xs(k) < xpmax .and. ys(k) > ypmin .and. ys(k) < ypmax .and. zs(idim,k) /= DMISS ) then
                  n2 = n2 + 1
                  xs(n2) = xs(k)
                  ys(n2) = ys(k)
                  zs(1:NDIM,n2) = zs(1:NDIM,k)
               endif
            else
               if (zs(idim,k) /= DMISS ) then
                  n2 = n2 + 1
                  xs(n2) = xs(k)
                  ys(n2) = ys(k)
                  zs(1:NDIM,n2) = zs(1:NDIM,k)
               endif
            endif
         ENDDO


         nxx  = 0        ! count net/grid size
         DO K = 1,Nxy
            if (jins == 1) then
               if ( x(k) > xpmin .and. x(k) < xpmax .and. y(k) > ypmin .and. y(k) < ypmax ) then
                  nxx = nxx + 1
               endif
            else
               nxx = nxx + 1
            endif
         ENDDO
         allocate (xx (nxx), yy(nxx), zz(NDIM,nxx), ind(nxx), stat = ierr )
         call aerr('xx (nxx), yy(nxx), zz(NDIM,nxx), ind(nxx)', ierr, (3+NDIM)*nxx)
         ind = 0

         nxx  = 0
         in = -1  ! net/grid size reduction
         DO K = 1,Nxy
            if ( jakc == 1 ) then
               if (kc(k) == 0) then
                  cycle
               endif
            endif
            call dbpinpol(x(k), y(k), in, dmiss, JINS, nh, XH, YH, ZH)
            if (in == 1) then
               nxx = nxx + 1
               ind(nxx) = k
               xx (nxx) = x(k)
               yy(nxx) = y(k)
               zz(1:NDIM,nxx) = z(1:NDIM,k)
            endif
         ENDDO
      ELSE
         NXX = NXY
         N2  = NS
         xs => xs_in
         ys => ys_in
         zs => zs_in
      ENDIF

      !        determine sample bounding box
      xsmin = minval(xs,mask=xs /= DMISS)
      xsmax = maxval(xs,mask=xs /= DMISS)
      ysmin = minval(ys,mask=ys /= DMISS)
      ysmax = maxval(ys,mask=ys /= DMISS)

      ierr = 1
      if ( jakdtree /= 1 ) then
         call dlaun(xs,ys,N2,1,ierr)
      else
         call dlaun(xs,ys,N2,3,ierr)   ! generate edgeindex and triedge
      end if
      if ( ierr /= 0 ) then
         goto 1234
      end if
      JDLA = 0
   ENDIF

   IF (NUMTRI  <  1) THEN
      goto 1234
   ENDIF

   NCOL = 14
   IF (Jtekinterpolationprocess  /=  0) THEN
      DO I = 1,NUMTRI

         NAF   = INDX(1,I)
         NBF   = INDX(2,I)
         NCF   = INDX(3,I)
         XL(1) = XS(NAF)
         XL(2) = XS(NBF)
         XL(3) = XS(NCF)

         YL(1) = YS(NAF)
         YL(2) = YS(NBF)
         YL(3) = YS(NCF)

      ENDDO
   ENDIF

   R2search = 0d0
   if( transformcoef(6) /= dmiss ) R2search = transformcoef(6)**2
   if ( jakdtree2 == 1 .and. R2search > 0d0 ) then             ! build kd-tree for sample points
      sampletree%itreestat = ITREE_EMPTY
      call build_kdtree(sampletree, n2,xs,ys,ierror, jsferic, dmiss)
      if ( ierror /= 0 ) then
         if ( sampletree%itreestat /= ITREE_EMPTY ) call delete_kdtree2(sampletree)
         jakdtree2 = 0
      end if
   end if

   KMOD = MAX(1,NXX/100)
   DO N = 1,NXx

      IF (Jtekinterpolationprocess == 0 .AND. MOD(N,KMOD) == 0 ) THEN
         AF = dble(N-1) / dble(NXX)
      ENDIF

      idim = 1 ! DMISS check on first sample dimension only
      if (nh > 2) then
         XP = xx(N)
         YP = yy(N)
         RD = zz(idim,N)
      else
         if ( jakc == 1 ) then
            if (kc(n) == 0) then
               cycle
            endif
         endif

         XP = x(N)
         YP = y(N)
         RD = z(idim,N)
      endif


      INTRI = 0
      ! For triangulation, only consider points that are inside the sample bounding box
      if ( xp >= xsmin .and. xp <= xsmax .and. yp >= ysmin .and. yp <= ysmax ) then
         IF (RD == dmiss) then
            if ( jakdtree == 1 ) then
               jadum = 0
               call findtri_kdtree(XP,YP,ZP,XS,YS,ZS,N2,NDIM,NRFIND,INTRI,JSLO,SLO, &
                                   Jtekinterpolationprocess,jadum,ierror,indf,wf, dmiss, jsferic, jins, jasfer3D)
               if ( ierror /= 0 ) then
                  !                    deallocate
                  if ( treeglob%itreestat /= ITREE_EMPTY ) call delete_kdtree2(treeglob)
                  if ( allocated(imask) ) deallocate(imask)
                  if ( allocated(LNtri) ) deallocate(LNtri)
                  !                    disable kdtree
                  jakdtree = 0
               end if
            end if

            if ( jakdtree /= 1 ) then
               CALL FINDTRI(XP,YP,ZP,XS,YS,ZS,NDIM,NRFIND,INTRI,JSLO,SLO, &
                            Jtekinterpolationprocess,indf,wf, dmiss, jsferic, jins)
            end if

            IF (INTRI == 1) THEN
               if (nh > 2) then
                  Zz(:,N) = ZP
               else
                  z(:,n)  = zp
               endif

               if (jagetwf == 1) then
                  indxx(1:3,n) = indf(1:3)
                  wfxx (1:3,n) = wf (1:3)
               endif

            ENDIF ! (INTRI == 1)
         ENDIF ! (XP  /=  XYMIS .AND. ... .AND. YP  <=  YMAXS )
      endif


      !!!!!!!!!! give it another try with nearest neighbour opr inverse distance.
      if (intri == 0 .and. R2search > 0d0) then

         if (RD == dmiss) then
            if( jakdtree2 == 1 ) then
               call make_queryvector_kdtree(sampletree, xp, yp, jsferic)
               numsam = kdtree2_r_count( sampletree%tree, sampletree%qv, R2search )
               if ( numsam > 0 ) then
                  call realloc_results_kdtree(sampletree, numsam)
                  call kdtree2_n_nearest(sampletree%tree,sampletree%qv,numsam,sampletree%results)
               end if

               do i = 1,idim
                  ii = 0
                  cof1 = 0d0
                  cof2 = 0d0
                  do k = 1,numsam
                     k1 = sampletree%results(k)%idx
                     if( abs( xp - xs(k1) ) < 1d-6 .and. abs( yp - ys(k1) ) < 1d-6 ) then
                        ii = 1
                        z(i,n) = zs(i,k1)
                        exit
                     endif
                     if (jsferic /= 0) then
                        dist2 = dbdistance( xp, yp, xs(k1), ys(k1), jsferic, jasfer3D, dmiss ) ** 2
                     else
                        dist2 = ( xp - xs(k1) )**2 +  ( yp - ys(k1) )**2
                     endif
                     cof1 = cof1 + zs(i,k1) / dist2
                     cof2 = cof2 + 1d0 / dist2
                  enddo
                  if( ii == 0 .and. numsam > 0 ) then
                     z(i,n) = cof1 / cof2
                  end if
               enddo

            else
               do i = 1,idim
                  ii = 0
                  cof1 = 0d0
                  cof2 = 0d0
                  do k = 1,n2
                     if( abs( xp - xs(k) ) < 1d-6 .and. abs( yp - ys(k) ) < 1d-6 ) then
                        ii = 1
                        z(i,n) = zs(i,k)
                        exit
                     endif
                     if (jsferic /= 0) then
                        dist2 = dbdistance( xp, yp, xs(k), ys(k), jsferic, jasfer3D, dmiss ) ** 2
                     else
                        dist2 = ( xp - xs(k) )**2 +  ( yp - ys(k) )**2
                     endif
                     cof1 = cof1 + zs(i,k) / dist2
                     cof2 = cof2 + 1d0 / dist2
                  enddo
                  if( ii == 0 ) then
                     z(i,n) = cof1 / cof2
                  end if
               enddo
            endif
         endif ! RDMISS

      end if ! intri ==0

   enddo ! DO N = 1,NXx

   if (nh > 2) then
      do k = 1,nxx
         if ( jakc == 1 ) then
            if (kc(k) == 0) then
               cycle
            endif
         endif
         do idim=1,NDIM
            z(idim,ind(k)) = zz(idim,k)
         end do
      enddo
      deallocate (xx, yy, zz, ind)
   endif

1234 continue

   !     deallocate
   if ( jakdtree == 1 ) then
      if ( treeglob%itreestat    /=  ITREE_EMPTY ) call delete_kdtree2(treeglob  )
      if ( sampletree%itreestat  /=  ITREE_EMPTY ) call delete_kdtree2(sampletree)
      if ( allocated(imask) ) deallocate(imask)
      if ( allocated(LNtri) ) deallocate(LNtri)
      if ( allocated(edgeindx) ) deallocate(edgeindx)
      if ( allocated(triedge) ) deallocate(triedge)
      if ( allocated(indx) ) deallocate(indx)
   end if

   if ( Ldeleteddata ) then
      deallocate(xs1, ys1, zs1)
   end if

   end subroutine triintfast


   subroutine DLAUN(XS,YS,NS,jatri,ierr)
   implicit none
   real(kind=hp), intent(in) :: XS(:), YS(:)
   integer, intent(out)      :: ierr
   integer , intent(in)      :: ns
   integer, intent(in)       :: jatri !< Type of DLaun triangulation: 1: just triangulate,
   !! 3: also produce node-edge-triangle mapping tables
   !! for use in Triangulatesamplestonetwork.

   integer              :: maxtri
   integer, parameter   :: nh= 1   ! SPvdP: too small if jatri == 0
   integer              :: nsm
   real(kind=hp)        :: trisize
   real(kind=hp)        :: XH(NH), YH(NH)
   integer, allocatable :: idum(:)

   !     check memory
   allocate ( idum(50*Ns) ,stat=ierr)     ! probably not enough
   call aerr('idum(50*Ns)',ierr,-50*Ns)

   if ( ierr /= 0 ) then
      call msgbox('', 'dlaun: out of memory', LEVEL_ERROR)
      ! TODO: SvdP: consider adding 'call mess' to stop the simulation.
      !         if ( allocated(idum) ) deallocate(idum)  ! gives an error
      return
   end if

   deallocate(idum)


   NUMTRI = 0
   NUMEDGE = 0
   IF (NS  <  3) RETURN

   if (jatri /= 1 .and. jatri /= 3) then
      return
   end if

   numtri = -1
   do while ( numtri < 0 )
      NSM    = 6*NS + 10
      IF (ALLOCATED (INDX) ) DEALLOCATE (INDX)
      ALLOCATE   (INDX(3,NSM),STAT=IERR)
      CALL AERR ('INDX(3,NSM)',IERR,INT(3*NSM))

      if (jatri==3) then
         call realloc(EDGEINDX, (/ 2,2*NSM /), keepExisting=.false., fill=0, stat=ierr)
         call aerr('edgeindx(2,NSM)', ierr, int(2*NSM))
         call realloc(TRIEDGE, (/ 3,NSM /), keepExisting=.false., fill=0, stat=ierr)
         call aerr('triedge(3,NSM)', ierr, int(3*NSM))
      else
         call realloc(EDGEINDX, (/ 2,1 /), keepExisting=.false., fill=0, stat=ierr)
         call realloc(TRIEDGE, (/ 3,1 /), keepExisting=.false., fill=0, stat=ierr)
      end if
      MAXTRI = NSM !?

      numtri = NSM ! Input value should specify max nr of triangles in indx.
      CALL TRICALL(jatri,XS,YS,NS,INDX,NUMTRI,EDGEINDX,NUMEDGE,TRIEDGE,XH,YH,NH,trisize)
      if ( numtri < 0 ) nsm = -numtri
   end do

   end subroutine DLAUN

   !>    find triangle for interpolation with kdtree
   !>       will initialize kdtree and triangulation connectivity
   subroutine findtri_kdtree(XP,YP,ZP,XS,YS,ZS,NS,NDIM,NRFIND,INTRI,JSLO,SLO,JATEK,jadum,ierror,ind, wf, dmiss, jsferic, jins, jasfer3D)
   implicit none

   real(kind=hp)   , intent(in)                :: xp, yp    !< node coordinates
   real(kind=hp)   , intent(out)               :: zp(NDIM)  !< node values
   integer,          intent(in)                :: NS        !< number of samples
   integer,          intent(in)                :: NDIM      !< sample vector dimension
   real(kind=hp)   , intent(in)                :: xs(NS), ys(NS), zs(NDIM,NS) !< sample coordinates and values
   integer,          intent(out)               :: NRFIND    !< triangle index
   integer,          intent(out)               :: intri     !< in triangle (1) or not (0)
   integer,          intent(in)                :: jslo      !< get slope (1) or not (0)
   real(kind=hp)   , intent(out)               :: slo(NDIM) !< slope
   integer,          intent(in)                :: JATEK     !< draw to screen (1) or not (0)
   integer,          intent(in)                :: jadum     !< debug (1) or not (0)
   integer,          intent(out)               :: ierror    !< error (1) or not (0)
   integer,          intent(out)               :: ind(3)    !<
   real(kind=hp)   , intent(out)               :: wf(3)     !<


   real(kind=hp)   , dimension(:), allocatable :: xx, yy !< triangle circumcenter coordinates, dim(numtri)

   real(kind=hp)   , dimension(3)      :: xv, yv    ! triangle node coordinates
   real(kind=hp)   , dimension(NDIM,3) :: zv        ! triangle node vectors


   real(kind=hp)                       :: xz, yz    ! triangle circumcenter
   real(kind=hp)                       :: SL,SM,XCR,YCR,CRP

   integer                             :: i, inod, ii, inext, k
   integer                             :: jacros, iothertriangle
   integer                             :: jsferic_store, ierr
   integer                             :: iedge, k1, k2, numsearched

   real(kind=hp)   , parameter         :: dtol = 1d-8
   integer,          parameter         :: MAXTRICON=100
   integer,          parameter         :: INIT_TRIS_PER_NODE=6

   real(kind=hp)   , external          :: dcosphi
   real(kind=hp)   , intent(in)        :: dmiss
   integer,          intent(in)        :: jins
   integer                             :: jsferic
   integer,          intent(in)        :: jasfer3D

   real(kind=hp)   , parameter         :: dfac = 1.000001d0  ! enlargement factor for pinpok3D

   ierror = 1

   NRFIND = 0

   if ( numtri <= 1 ) then
      !            call qnerror('findtri_kdtree: numtri<=1', ' ', ' ')
      goto 1234
   end if

   ! store jsferic
   jsferic_store = jsferic

   if ( treeglob%itreestat /= ITREE_OK ) then

      !           INITIALIZE kdetree2

      allocate(xx(numtri), yy(numtri), stat=ierr)
      call aerr('xx(numtri), yy(numtri)', ierr, 2*numtri)
      allocate(imask(numtri), stat=ierr)
      call aerr('imask(numtri)', ierr, numtri)
      imask = 0
      IDENT = 0

      allocate(LNtri(2,numedge), stat=ierr)
      call aerr('LNtri(2,numedge)', ierr, 2*numedge)
      LNtri = 0

      !           dlaun is not spherical proof: set global jsferic for circumcenter
      jsferic = 0

      do i=1,numtri
         do ii=1,3
            !                 generate edge-triangle connectivity
            iedge = triedge(ii,i)
            if ( LNtri(1,iedge) == 0 ) then
               LNtri(1,iedge) = i
            else
               LNtri(2,iedge) = i
            end if

            inod = indx(ii,i)

            !                 get triangle polygon
            xv(ii) = xs(inod)
            yv(ii) = ys(inod)
            zv(1:NDIM,ii) = zs(1:NDIM,inod)
         end do

         !              compute triangle circumcenter
         if ( jasfer3D == 1 ) then
            call ave3D(3,xv,yv, xx(i), yy(i),jsferic_store,jasfer3D)
         else
            xx(i) = sum(xv(1:3)) / 3d0
            yy(i) = sum(yv(1:3)) / 3d0
         end if
      end do

      !        restore jsferic
      jsferic = jsferic_store

      call build_kdtree(treeglob, numtri, xx, yy, ierror,jsferic, dmiss)

      !           deallocate
      deallocate(xx, yy)

      if ( ierror /= 0 ) then
         goto 1234
      end if
   end if

   !        update identifier
   IDENT=IDENT+1

   !        fill query vector
   call make_queryvector_kdtree(treeglob,xp,yp,jsferic)

   !        get first triangle
   call kdtree2_n_nearest(treeglob%tree,treeglob%qv,1,treeglob%results)
   inext = treeglob%results(1)%idx

   !        perform a line search
   numsearched = 1
   i = 0

   mainloop:do while ( NRFIND == 0 .and. numsearched <= 2*NUMTRI .and. numsearched > 0 )

      inext = treeglob%results(1)%idx

      numsearched = 0

      do while ( inext > 0 .and. inext <= numtri .and. numsearched <= 2*NUMTRI )   ! numsearched: safety
         i = inext

         !           check current triangle
         do ii=1,3
            inod = indx(ii,i)
            xv(ii) = xs(inod)
            yv(ii) = ys(inod)
            zv(1:NDIM,ii) = zs(1:NDIM,inod)
         end do

!        get a point in the cell
         if ( jasfer3D == 1 ) then
            call ave3D(3,xv,yv,xz,yz,jsferic,jasfer3D)
         else
            xz = sum(xv(1:3)) / 3d0
            yz = sum(yv(1:3)) / 3d0
         end if

         if ( imask(i) == IDENT ) then
            intri = 0
         else
            numsearched = numsearched+1
            if ( jasfer3D == 0 ) then
               call pinpok(xp,yp,3,xv,yv,intri, jins, dmiss)
            else
               call pinpok3D(xp,yp,3,xv,yv,intri, dmiss, jins, 1, 1, dfac=dfac, xz=xz, yz=yz)
            end if

            imask(i) = IDENT
         end if

         if ( intri == 1 ) then
            NRFIND = i
            exit mainloop
         end if

         !           dlaun is not spherical proof: set global jsferic
         jsferic = 0

         !           proceed to next triangle, which is adjacent to the edge that is cut by the line from the current triangle to the query point
         inext = 0

         if ( jadum == 1 ) then
         end if

         do ii=1,3
            iedge=triedge(ii,i)
            if ( LNtri(2,iedge) == 0 ) cycle

            iothertriangle = LNtri(1,iedge) + LNtri(2,iedge) - i
            if ( imask(iothertriangle) == IDENT ) then
               cycle
            end if

            k1 = edgeindx(1,iedge)
            k2 = edgeindx(2,iedge)
            if ( jasfer3D == 0 ) then
               call CROSS(xz, yz, xp, yp, xs(k1), ys(k1), xs(k2), ys(k2), JACROS,SL,SM,XCR,YCR,CRP, jsferic, dmiss)
            else
               call cross3D(xz, yz, xp, yp, xs(k1), ys(k1), xs(k2), ys(k2), jacros, sL, sm, xcr, ycr, jsferic_store, dmiss)
            end if

            !              use tolerance
            if ( jacros == 0 ) then
               if ( sm == dmiss .or. sl == dmiss ) then
                  !                    triangle with small area: sm and sl have not been computed
                  jacros = 1
               else IF (SM  >=  0d0-dtol .AND. SM  <=  1d0+dtol .AND. &
                  SL  >=  0d0-dtol .AND. SL  <=  1d0+dtol) THEN
               JACROS = 1
               ENDIF
            end if

            if ( jadum == 1 .and. jacros == 1 ) then
               call msgbox('', '', LEVEL_ERROR)
               ! TODO: SvdP: consider adding 'call mess' to stop the simulation.
            end if

            if ( jacros == 1 ) then  ! only select this edge if it has a second adjacent triangle
               inext = iothertriangle
               exit
            end if
         end do

         !        restore jsferic
         jsferic = jsferic_store

      end do

   end do mainloop

   if ( intri == 0 .and. inext > 0 ) then
      if ( numsearched > 10 ) write(6,"('numsearched= ', I0)") numsearched
      !            call qnerror('findtri_kdtree: error', ' ', ' ')
   end if

   if (intri == 1) then
      if ( jasfer3D == 0 ) then
         call linear(xv, yv, zv, NDIM, xp, yp, zp, JSLO, SLO, JATEK, wf, dmiss, jsferic)
      else
         call linear3D(xv, yv, zv, NDIM, xp, yp, zp, JSLO, SLO, wf, jsferic, jasfer3D, dmiss)
      end if
      do k = 1,3
         ind(k) = indx(k,nrfind)
      enddo

   endif

   ierror = 0
1234 continue

   return
   end subroutine findtri_kdtree


   subroutine FINDTRI(XP,YP,ZP,XS,YS,ZS,NDIM,NRFIND,INTRI,JSLO,SLO,JATEK, ind, wf, dmiss, jsferic, jins)
   implicit none
   integer      , intent(out)   :: intri
   integer      , intent(in)    :: jatek, jslo
   integer      , intent(out)   :: nrfind
   integer      , intent(in)    :: NDIM   !< sample vector dimension
   integer      , intent(inout) :: ind(:)
   real(kind=hp), intent(inout) :: slo(:)
   real(kind=hp), intent(in)    :: xp
   real(kind=hp), intent(in)    :: yp
   real(kind=hp), intent(out)   :: zp(:)
   real(kind=hp), intent(in)    :: XS(:), YS(:), ZS(:,:)
   real(kind=hp), intent(inout) :: wf(:)
   real(kind=hp), intent(in)    :: dmiss
   integer,       intent(in)    :: jsferic, jins

   integer       :: k, k1, k2, numit
   integer       :: nroldfind, interval
   integer       :: idim
   real(kind=hp) :: xtmax
   real(kind=hp) :: xtmin
   real(kind=hp) :: ytmax
   real(kind=hp) :: ytmin
   real(kind=hp) :: XT(3),YT(3),ZT(NDIM,3)
   integer       :: ik1, ik2, ik3

   DATA NROLDFIND /0/

   ZP       = dmiss
   INTRI    = 0
   interval = 2
   numit = 0
   nrfind = 0
5  CONTINUE
   numit    = numit + 1
   interval = 5*interval

   K1 = MAX(1     ,NROLDFIND-interval)
   K2 = MIN(NUMTRI,NROLDFIND+interval)

   DO K = K1,K2
      ik1 = INDX(1,K)
      ik2 = INDX(2,K)
      ik3 = INDX(3,K)
      XT(1) = XS(ik1)
      XT(2) = XS(ik2)
      XT(3) = XS(ik3)
      YT(1) = YS(ik1)
      YT(2) = YS(ik2)
      YT(3) = YS(ik3)
      XTMAX = MAX(XT(1),MAX( XT(2),XT(3) ) )
      YTMAX = MAX(YT(1),MAX( YT(2),YT(3) ) )
      XTMIN = MIN(XT(1),MIN( XT(2),XT(3) ) )
      YTMIN = MIN(YT(1),MIN( YT(2),YT(3) ) )
      IF (XP  >=  XTMIN .AND. XP  <=  XTMAX .AND.   &
         YP  >=  YTMIN .AND. YP  <=  YTMAX) THEN
      CALL PINPOK(XP,YP,3,XT,YT,INTRI, jins, dmiss)
      IF (INTRI == 1) THEN
         NRFIND    = K
         NROLDFIND = NRFIND
         do idim=1,NDIM
            ZT(idim,1) = ZS(idim,INDX(1,K))
            ZT(idim,2) = ZS(idim,INDX(2,K))
            ZT(idim,3) = ZS(idim,INDX(3,K))
         end do
         CALL LINEAR (XT, YT, ZT, NDIM, XP, YP, ZP, JSLO, SLO, JATEK, wf, dmiss, jsferic)
         ind(1) = ik1
         ind(2) = ik2
         ind(3) = ik3
         RETURN
      ENDIF
      ENDIF
   ENDDO
   if (k1 == 1 .and. k2 == numtri) then
      NROLDFIND = numtri / 2
      return
   endif
   IF (NRfind == 0) THEN
      GOTO 5
   ENDIF
   RETURN
   END subroutine FINDTRI


   subroutine LINEAR ( X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, JATEK, wf, dmiss, jsferic)
   implicit none
   integer      , intent(in)    :: NDIM   !< sample vector dimension
   real(kind=hp), intent(in)    :: X(:),Y(:),Z(:,:)
   real(kind=hp), intent(out)   :: wf(:)
   real(kind=hp), intent(out)   :: zp(:)
   real(kind=hp), intent(inout) :: slo(:)
   real(kind=hp), intent(in)    :: dmiss
   integer,       intent(in)    :: jsferic
   integer,       intent(in)    :: jatek
   integer,       intent(in)    :: jslo
   real(kind=hp), intent(in)    :: xp
   real(kind=hp), intent(in)    :: yp

   integer          :: idim
   real(kind=hp)    :: a11
   real(kind=hp)    :: a12
   real(kind=hp)    :: a21
   real(kind=hp)    :: a22
   real(kind=hp)    :: a31
   real(kind=hp)    :: a32
   real(kind=hp)    :: b1
   real(kind=hp)    :: b2
   real(kind=hp)    :: det
   real(kind=hp)    :: dum
   real(kind=hp)    :: r3
   real(kind=hp)    :: rlam
   real(kind=hp)    :: rmhu
   real(kind=hp)    :: x3
   real(kind=hp)    :: xn
   real(kind=hp)    :: xy
   real(kind=hp)    :: y3
   real(kind=hp)    :: yn
   real(kind=hp)    :: z3
   real(kind=hp)    :: zn

   ZP  = dmiss
   A11 = getdx(x(1),y(1),x(2),y(2),jsferic)   ! X(2) - X(1)
   A21 = getdy(x(1),y(1),x(2),y(2),jsferic)   ! Y(2) - Y(1)
   A12 = getdx(x(1),y(1),x(3),y(3),jsferic)   ! X(3) - X(1)
   A22 = getdy(x(1),y(1),x(3),y(3),jsferic)   ! Y(3) - Y(1)
   B1  = getdx(x(1),y(1),xp  ,yp , jsferic)   ! XP   - X(1)
   B2  = getdy(x(1),y(1),xp  ,yp , jsferic)   ! YP   - Y(1)

   DET  =   A11 * A22 - A12 * A21
   IF (ABS(DET)  <  1E-12) THEN
      RETURN
   ENDIF

   RLAM = ( A22 * B1  - A12 * B2) / DET
   RMHU = (-A21 * B1  + A11 * B2) / DET
   wf(3) = rmhu
   wf(2) = rlam
   wf(1) = 1d0 - rlam - rmhu

   ZP   = Z(:,1) + RLAM * (Z(:,2) - Z(:,1)) + RMHU * (Z(:,3) - Z(:,1))

   IF (JATEK == 1) THEN
      IF (MAX(ABS(A21),ABS(A22))  >  500) THEN
         DUM = 0
      ENDIF
   ENDIF

   IF (JSLO == 1) THEN
      do idim = 1,NDIM
         A31 = Z(idim,2) - Z(idim,1)
         A32 = Z(idim,3) - Z(idim,1)
         X3 =  (A21*A32 - A22*A31)
         Y3 = -(A11*A32 - A12*A31)
         Z3 =  (A11*A22 - A12*A21)
         R3 =  SQRT(X3*X3 + Y3*Y3 + Z3*Z3)
         IF (R3  /=  0) THEN
            XN = X3/R3
            YN = Y3/R3
            ZN = Z3/R3
            XY = SQRT(XN*XN + YN*YN)
            IF (ZN  /=  0) THEN
               SLO(idim) = ABS(XY/ZN)
            ELSE
               SLO(idim) = dmiss
            ENDIF
         ELSE
            SLO(idim) = dmiss
         ENDIF
      end do
   endif
   return
   end subroutine LINEAR

   subroutine linear3D(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, w, jsferic, jasfer3D, dmiss)
      implicit none

      integer,                             intent(in)     :: NDIM       !< sample vector dimension
      real(kind=hp)   , dimension(3),      intent(in)     :: x, y
      real(kind=hp)   , dimension(NDIM,3), intent(in)     :: z
      real(kind=hp)   ,                    intent(in)     :: xp, yp
      real(kind=hp)   ,                    intent(out)    :: zp(NDIM)
      integer,                             intent(in)     :: jslo       !< not supported
      real(kind=hp)   , dimension(NDIM),   intent(out)    :: slo(NDIM)
      real(kind=hp)   , dimension(3),      intent(out)    :: w
      real(kind=hp)   ,                    intent(in)     :: dmiss
      integer         ,                    intent(in)     :: jsferic, jasfer3D

      real(kind=hp)   , dimension(3)                      :: xx1, xx2, xx3, xxp
      real(kind=hp)   , dimension(3)                      :: s123, rhs
      real(kind=hp)   , dimension(3,3)                    :: A
      real(kind=hp)                                       :: D
      integer                                             :: idim
      real(kind=hp)   , parameter                         :: dtol = 0d0
      Double precision :: d1,d2,d3,sd 

      slo = DMISS
      if ( jslo == 1 ) then
         call mess(LEVEL_ERROR, 'linear3D: jslo=1 not supported')
      end if

!     get 3D coordinates of the points
      call sphertocart3D(x(1), y(1), xx1(1), xx1(2), xx1(3))
      call sphertocart3D(x(2), y(2), xx2(1), xx2(2), xx2(3))
      call sphertocart3D(x(3), y(3), xx3(1), xx3(2), xx3(3))
      call sphertocart3D(xp, yp, xxp(1), xxp(2), xxp(3))

!     get (double) area vector
      s123 = vecprod(xx2-xx1,xx3-xx1)

      D = sqrt( inprod(s123,s123) )

      if ( D > dtol ) then
!        build system:
!           gradz . (x2-x1) = z2-z1
!           gradz . (x3-x1) = z3-z1
!           gradz . ((x2-x1) X (x3-x1)) = 0

         A(1,:) = xx2-xx1
         A(2,:) = xx3-xx1
         A(3,:) = s123
         rhs = 0d0   ! not used

!        compute inverse
         call gaussj(A,3,3,rhs,1,1)

!        compute weights
         w(2) = inprod(xxp-xx1, A(:,1))
         w(3) = inprod(xxp-xx1, A(:,2))
         w(1) = 1d0 - w(2) - w(3)

      else
         !zp = DMISS
         !call mess(LEVEL_ERROR, 'linear3D: area too small') 
         ! probably collinear points with at least 2 y = 90  
         d1 = dbdistance( Xp, yp, x(1), y(1), jsferic, jasfer3D, dmiss )
         d2 = dbdistance( Xp, yp, x(2), y(2), jsferic, jasfer3D, dmiss )
         d3 = dbdistance( Xp, yp, x(3), y(3), jsferic, jasfer3D, dmiss )
         w(1) = 1d0/max(d1,1d-3) ;  w(2) = 1d0/max(d2,1d-3) ;  w(3) = 1d0/max(d3,1d-3)
         sd   = w(1) + w(2) + w(3)
         w    = w/sd
      end if

!     interpolate
      do idim=1,NDIM
         zp(idim) = w(1) * z(idim,1) + w(2) * z(idim,2) + w(3) * z(idim,3)
      end do

   end subroutine linear3D

   !---------------------------------------------------------------------------!
   !   nearest_neighbour
   !---------------------------------------------------------------------------!

   !> return the index of the nearest neighbouring source point for each of the target grid points
   subroutine nearest_neighbour_dbl(Nc, xc, yc, kc, Mn, dmiss, XS, YS, MSAM, jsferic, jasfer3D)
   implicit none

   integer,                      intent(in   ) :: Nc       !< number of points to be interpolated
   real(kind=hp), dimension(Nc), intent(in   ) :: xc, yc   !< point coordinates of target grid points
   integer,       pointer      , intent(in   ) :: kc(:)    !< Target mask array-pointer, whether or not (1/0) target points should be included. Pass null() when no masking is wanted.
   integer,       dimension(Nc), intent(  out) :: Mn       !< source index for each target point
   real(kind=hp),                intent(in   ) :: dmiss    !< Missing value inside xc, yx (if any).
   real(kind=hp),                intent(in   ) :: XS(:), YS(:) !< point coordinates of source data points
   integer,                      intent(in   ) :: MSAM     !< Number of points in source data point set.
   integer,                      intent(in   ) :: jsferic  !< Whether or not (1/0) input coordinates are spherical or not.
   integer,                      intent(in   ) :: jasfer3D !< Whether or not 3D distance calculation must be done, across the globe surface.
   integer :: k,m
   real(kind=hp) :: dist, mindist

   Mn = -1
   do k=1,Nc ! Target points
      if (associated(kc)) then
         if (KC(K) == 0) then
            cycle
         end if
      end if

      mindist = Huge(1.d0)
      do m=1,MSAM ! Source points
         dist = dbdistance(xc(k),yc(k),xs(m),ys(m),jsferic,jasfer3D,dmiss)
         if (dist<mindist) then
            mindist = dist
            Mn(k) = m
         end if
      end do
   end do
   end subroutine nearest_neighbour_dbl

   !> return the index of the nearest neighbouring source point for each of the target grid points
   subroutine nearest_neighbour_real(Nc, xc, yc, kc, Mn, dmiss, XS, YS, MSAM, jsferic, jasfer3D)
   implicit none

   integer,                      intent(in   ) :: Nc       !< number of points to be interpolated
   real(kind=sp), dimension(Nc), intent(in   ) :: xc, yc   !< point coordinates of target grid points
   integer,       pointer      , intent(in   ) :: kc(:)    !< Target mask array-pointer, whether or not (1/0) target points should be included. Pass null() when no masking is wanted.
   integer,       dimension(Nc), intent(  out) :: Mn       !< source index for each target point
   real(kind=hp),                intent(in   ) :: dmiss    !< Missing value inside xc, yx (if any).
   real(kind=hp),                intent(in   ) :: XS(:), YS(:) !< point coordinates of source data points
   integer,                      intent(in   ) :: MSAM     !< Number of points in source data point set.
   integer,                      intent(in   ) :: jsferic  !< Whether or not (1/0) input coordinates are spherical or not.
   integer,                      intent(in   ) :: jasfer3D !< Whether or not 3D distance calculation must be done, across the globe surface.

   real(kind=hp), dimension (:), allocatable :: xc_dbl, yc_dbl
   integer :: ierror

   allocate (xc_dbl(Nc), stat=ierror)
   allocate (yc_dbl(Nc), stat=ierror)

   xc_dbl = dble(xc)
   yc_dbl = dble(yc)

   call nearest_neighbour_dbl(Nc, xc_dbl, yc_dbl, kc, Mn, dmiss, XS, YS, MSAM, jsferic, jasfer3D)

   end subroutine nearest_neighbour_real


   !---------------------------------------------------------------------------!
   !   bilin_interp
   !---------------------------------------------------------------------------!

   !> bilinear interpolation of structed sample data at points
   subroutine bilin_interp(Nc, xc, yc, zc, dmiss, XS, YS, ZS, MXSAM, MYSAM, jsferic, kc)

   implicit none

   integer,                         intent(in)  :: Nc       !< number of points to be interpolated
   real(kind=hp)   , dimension(Nc), intent(in)  :: xc, yc   !< point coordinates
   real(kind=hp)   , dimension(Nc), intent(out) :: zc       !< interpolated point values
   real(kind=hp)   , intent(in)                 :: dmiss

   real(kind=hp)   , intent(in)                 ::  XS(:), YS(:), ZS(:)
   integer, intent(in)                          ::  MXSAM, MYSAM, jsferic
   integer, intent(in), optional                ::  kc(nc)

   real(kind=hp)         :: xi, eta
   integer               :: ierror
   integer               :: k, jakc

   jakc = 0
   if (present(kc)) jakc = 1

   ierror = 1

   if ( MXSAM == 0 .or. MYSAM == 0 ) then
      call msgbox('', 'bilin_interp: sample data is unstructured', LEVEL_ERROR)
      ! TODO: SvdP: consider adding 'call mess' to stop the simulation.
      goto 1234
   end if

   xi  = 0d0
   eta = 0d0

   do k=1,Nc
      if ( zc(k) == DMISS ) then
         if (jakc == 0) then
            call bilin_interp_loc(MXSAM, MYSAM, MXSAM, MYSAM, 1, XS, YS, ZS, xc(k), yc(k), xi, eta, zc(k), ierror, dmiss, jsferic)
         else if (kc(k) == 1) then
            call bilin_interp_loc(MXSAM, MYSAM, MXSAM, MYSAM, 1, XS, YS, ZS, xc(k), yc(k), xi, eta, zc(k), ierror, dmiss, jsferic)
         endif
      end if
   end do

   ierror = 0

   !  error handling
1234 continue

   end subroutine bilin_interp


   !> bilinear interpolation between nodes
   subroutine bilin_interp_loc(Nxmax, Nymax, grid_width, grid_depth, NDIM, x, y, z, xp, yp, xi, eta, zp, ierror, dmiss, jsferic)
   implicit none

   integer,                                    intent(in)    :: Nxmax, Nymax !< node array size
   integer,                                    intent(in)    :: grid_width, grid_depth   !< actual sizes
   integer,                                    intent(in)    :: NDIM     !< sample vector dimension
   real(kind=hp)   , dimension(Nxmax,Nymax),   intent(in)    :: x, y     !< node coordinates
   real(kind=hp)   , dimension(NDIM,Nxmax,Nymax), intent(in) :: z        !< node values
   real(kind=hp)   ,                           intent(in)    :: xp, yp   !< interpolant coordinates
   real(kind=hp)   ,                           intent(inout) :: xi, eta  !< interpolant index coordinates (in: first iterate)
   real(kind=hp)   , dimension(NDIM),          intent(out)   :: zp       !< interpolant value
   integer,                                    intent(out)   :: ierror   !< error (1) or not (0)

   real(kind=hp)                                             :: x1, y1
   real(kind=hp)   , dimension(NDIM)                         :: z1
   real(kind=hp)   , dimension(2,2)                          :: DxDxi, DxiDx
   real(kind=hp)                                             :: Dx, Dy   ! residual (in Cartesian coordinates always)
   real(kind=hp)                                             :: Dxi, Deta
   real(kind=hp)                                             :: eps, epsprev
   real(kind=hp)                                             :: det

   integer                                                   :: iter, ierror_loc

   real(kind=hp)   , parameter                               :: dtol    = 1d-6
   real(kind=hp)   , parameter                               :: dmaxincrease = 100
   integer,          parameter                               :: MAXITER = 1000
   real(kind=hp)   , parameter                               :: dmindif = 1d-2
   real(kind=hp)   , intent(in)                              :: dmiss
   integer, intent(in)                                       :: jsferic

   ierror = 1

   zp = DMISS

   !  set realistic start values
   xi  = min(max(xi, 0d0),dble(grid_width)-1d0)
   eta = min(max(eta,0d0),dble(grid_depth)-1d0)

   !  Newton iterations
   eps = 1d99
   epsprev = 2d0*eps
   do iter=1,MAXITER
      call comp_x_DxDxi(Nxmax, Nymax, grid_width, grid_depth, NDIM, x, y, z, xi, eta, x1, y1, z1, DxDxi, ierror_loc, dmiss, jsferic)

      if ( ierror_loc /= 0 ) goto 1234

      !     compute residual
      call getdxdy(x1,y1,xp,yp,dx,dy, jsferic)

      epsprev= eps
      eps = sqrt(Dx**2 + Dy**2)

      if ( eps < dtol .or. (epsprev-eps) < dmindif*epsprev ) exit

      !     invert Jacobian matrix
      det = DxDxi(1,1)*DxDxi(2,2) - DxDxi(2,1)*DxDxi(1,2)
      if ( abs(det) < 1d-9 ) goto 1234

      DxiDx(1,1) =  DxDxi(2,2)/det
      DxiDx(1,2) = -DxDxi(1,2)/det
      DxiDx(2,1) = -DxDxi(2,1)/det
      DxiDx(2,2) =  DxDxi(1,1)/det

      !     compute (xi,eta)-increment
      Dxi  = DxiDx(1,1) * Dx + DxiDx(1,2) * Dy
      Deta = DxiDx(2,1) * Dx + DxiDx(2,2) * Dy

      !     limit (xi,eta)-increment
      if ( Dxi   >   dmaxincrease ) Dxi  =  dmaxincrease
      if ( Dxi   <  -dmaxincrease ) Dxi  = -dmaxincrease
      if ( Deta  >   dmaxincrease ) Deta =  dmaxincrease
      if ( Deta  <  -dmaxincrease ) Deta = -dmaxincrease

      xi  = xi  + Dxi
      eta = eta + Deta

      !     set realistic values
      xi  = min(max(xi, 0d0),dble(grid_width)-1d0)
      eta = min(max(eta,0d0),dble(grid_depth)-1d0)
   end do

   if ( eps > dtol ) then
      !      call qnerror('bilin_interp_loc: no convergence', ' ', ' ')
      goto 1234
   end if

   !  set interpolated node value
   zp = z1

   ierror = 0

   !  error handling
1234 continue

   return
   end subroutine bilin_interp_loc

   !> bilinear interpolation of node coordinates and Jacobian matrix
   subroutine comp_x_DxDxi(Ncx, Ncy, grid_width, grid_depth, NDIM, x, y, z, xi, eta, x1, y1, z1, DxDxi, ierror, dmiss, jsferic)
   implicit none

   integer,                                   intent(in)  :: Ncx, Ncy !< node array sizes
   integer,                                   intent(in)  :: grid_width, grid_depth   !< actual sizes
   integer,                                   intent(in)  :: NDIM     !< sample vector dimension
   real(kind=hp)   , dimension(Ncx,Ncy),      intent(in)  :: x, y     !< node coordinates
   real(kind=hp)   , dimension(NDIM,Ncx,Ncy), intent(in)  :: z        !< node values
   real(kind=hp)   ,                          intent(in)  :: xi, eta  !< interpolant index coordinates
   real(kind=hp)   ,                          intent(out) :: x1, y1   !< interpolant coordinates
   real(kind=hp)   , dimension(NDIM),         intent(out) :: z1       !< interpolant value
   real(kind=hp)   , dimension(2,2),          intent(out) :: DxDxi    !< Jacobian matrix
   integer,                                   intent(out) :: ierror   !< error (1) or not (0)

   real(kind=hp)                                          :: xiL, etaL, xiL1, etaL1

   integer                                                :: i0, i1, j0, j1, k
   real(kind=hp)   , intent(in)                           :: dmiss
   integer                                                ::jsferic

   ierror = 1

   if ( grid_width < 2 .or. grid_depth < 2 ) goto 1234

   !  get the cell indices
   i0 = max(min(int(xi)+1, grid_width-1), 1)
   i1 = i0+1
   j0 = max(min(int(eta)+1, grid_depth-1), 1)
   j1 = j0+1

   !  compute local index coordinates
   xiL  = xi  - dble(i0-1)
   etaL = eta - dble(j0-1)

   xiL1  = 1d0 - xiL
   etaL1 = 1d0 - etaL

   !  check if all values are valid
   if ( x(i0,j0) == DMISS .or. x(i1,j0) == DMISS .or. x(i0,j1) == DMISS .or. x(i1,j1) == DMISS .or.   &
      y(i0,j0) == DMISS .or. y(i1,j0) == DMISS .or. y(i0,j1) == DMISS .or. y(i1,j1) == DMISS ) then
   goto 1234
   end if

   !  bilinear interpolation of node coordinates
   x1 = ( xiL1*x(i0,j0) + xiL*x(i1,j0) )*etaL1 + ( xiL1*x(i0,j1) + xiL*x(i1,j1) )*etaL
   y1 = ( xiL1*y(i0,j0) + xiL*y(i1,j0) )*etaL1 + ( xiL1*y(i0,j1) + xiL*y(i1,j1) )*etaL
   do k=1,NDIM
      if ( z(k,i0,j0) == DMISS .or. z(k,i1,j0) == DMISS .or. z(k,i0,j1) == DMISS .or. z(k,i1,j1) == DMISS ) then
         z1(k) = DMISS
      else
         z1(k) = ( xiL1*z(k,i0,j0) + xiL*z(k,i1,j0) )*etaL1 + ( xiL1*z(k,i0,j1) + xiL*z(k,i1,j1) )*etaL
      end if
   end do

   !  Jacobian matrix
   DxDxi(1,1) = etaL1*getdx(x(i0,j0),y(i0,j0),x(i1,j0),y(i1,j0),jsferic) +   &
      etaL *getdx(x(i0,j1),y(i0,j1),x(i1,j1),y(i1,j1),jsferic)
   DxDxi(1,2) = xiL1 *getdx(x(i0,j0),y(i0,j0),x(i0,j1),y(i0,j1),jsferic) +   &
      xiL  *getdx(x(i1,j0),y(i1,j0),x(i1,j1),y(i1,j1),jsferic)

   DxDxi(2,1) = etaL1*getdy(x(i0,j0),y(i0,j0),x(i1,j0),y(i1,j0),jsferic) +   &
      etaL *getdy(x(i0,j1),y(i0,j1),x(i1,j1),y(i1,j1),jsferic)
   DxDxi(2,2) = xiL1 *getdy(x(i0,j0),y(i0,j0),x(i0,j1),y(i0,j1),jsferic) +   &
      xiL  *getdy(x(i1,j0),y(i1,j0),x(i1,j1),y(i1,j1),jsferic)

   ierror = 0
   !  error handling
1234 continue
   end subroutine comp_x_DxDxi


    !---------------------------------------------------------------------------!
    !   averaging2
    !---------------------------------------------------------------------------!


    !> interpolate/average sample vector data in a polygon (e.g. a netcell)
    !>   note: m_samples is not used
    !>         xs and ys are the sample coordinates, dim(ns)
    !>         zss contains a ndim-dimensional vector for each of the ns samples, dim(ndim,ns)

    subroutine averaging2(ndim,ns,xs,ys,zss,ipsam,xc,yc,zc,grid_width,xx,yy,n6,nnn,jakdtree, &
                          dmiss, jsferic, jasfer3D, jins, npl, xpl, ypl, zpl, errorInfo, kcc) ! Werkt alleen voor cell regions, die zitten in xx en yy
    use stdlib_sorting, only: radix_sort
    implicit none
    integer,                                  intent(in   ) :: ndim                 !< sample vector dimension
    integer,                                  intent(in   ) :: ns                   !< number of samples
    real(kind=hp)   , dimension(ns),          intent(in   ) :: xs, ys               !< sample coordinates
    real(kind=hp)   , dimension(ndim,ns),     intent(in   ) :: zss                  !< sample values
    integer,          dimension(ns),          intent(in   ) :: ipsam                !< sample permutation array (increasing x-coordinate)
    integer,                                  intent(in   ) :: grid_width, n6               !< number of polygons and maximum polygon size
    real(kind=hp)   ,                         intent(in   ) :: xc(grid_width), yc(grid_width)       !< polygon center coordinates
    real(kind=hp)   ,                         intent(inout) :: zc(ndim,grid_width)          !< zc not initialized here
    real(kind=hp)   ,                         intent(in   ) :: xx(n6,grid_width), yy(n6,grid_width) !< polygon coordinates
    integer,                                  intent(in   ) :: nnn(grid_width)              !< polygon sizes
    integer,                                  intent(in   ) :: jakdtree             !< use kdtree (1) or not (0)
    real(kind=hp)   ,                         intent(in   ) :: dmiss                !< missing value
    integer,                                  intent(in   ) :: jsferic              !< spherical or not
    integer,                                  intent(in   ) :: jasfer3D             !< 0 = org, 1 = sqrt(dx2+dy2+dz2), 2= greatcircle
    integer,                                  intent(in   ) :: jins                 !< inside option
    integer,                                  intent(in   ) :: npl                  !< number of polygon points
    double precision, dimension(:),           intent(in   ) :: xpl, ypl, zpl        !< grid coordinates
    type(TerrorInfo),                         intent(  out) :: errorInfo            !< struct holding all error information
    integer,          dimension(:), optional, intent(in   ) :: kcc                  !< Masking array for each of the target points.

    real(kind=hp)   , allocatable     :: xh(:), yh(:)
    real(kind=hp)   , dimension(ndim) :: hparr, rhp
    real(kind=hp)      :: xlow, xhih, ylow, yhih, rmin2, wall, dis2, weight, xdum
    integer            :: n,k,nn,modin, nlowx, nhihx, numxy, ifirs, inhul
    integer            :: ivar
    integer            :: k_, k_start, k_end

    integer            :: japrogressbar, jadoen, in, numsam
    real(kind=hp)      :: R2search, rnn

    real(kind=hp)   , allocatable :: zz(:)
    integer         , allocatable :: kkin(:)
    integer                       :: nin, n1, n2, n12, num

    integer                       :: jakc
    character(len=*), parameter   :: cfmt = "('Unknown averaging method (', i0, '). Must be in range 1 ... 6.')"

    ! default/no samples in cell
    ! zc = dmiss
    ! hk : do not switch off please

    jakc = 0
    if (present(kcc)) jakc = 1

    japrogressbar = 1

    if ( percentileminmax > 0d0) then
       allocate(zz(ns), kkin(ns) )
    endif

    if ( jtekinterpolationprocess == 1 .or. grid_width < 100 ) then
       japrogressbar = 0
    end if

    allocate (xh(n6), yh(n6) )

    modin = max(1.0_hp, real(grid_width, hp)/100.0)
    in = -1

    errorInfo%cntNoSamples = 0

    do n = 1, grid_width

       if (jakc == 1) then
          if (kcc(N) /= 1) then
             cycle
          end if
       end if

       jadoen = 0
       do ivar=1,ndim
          if ( zc(ivar,n) == dmiss ) then
             jadoen = 1
          endif
       enddo
       if (jadoen == 0 .or. nnn(n) == 0) cycle ! Skip undefined cells (0 corners)

       if (npl > 0) then
          call dbpinpol( xc(n), yc(n), in, dmiss, jins, npl, xpl, ypl, zpl)
          if (in == 0) then
             cycle
          endif
       endif

       nn   = nnn(n)
       do k = 1,nn
          xh(k) = xx(k,n)
          yh(k) = yy(k,n)
       enddo
       do k = 1,nn
          xh(k) = rcel*xh(k) + (1d0-rcel)*xc(n)
          yh(k) = rcel*yh(k) + (1d0-rcel)*yc(n)
       enddo

       xlow = minval(xh(1:nn))
       xhih = maxval(xh(1:nn))
       ylow = minval(yh(1:nn))
       yhih = maxval(yh(1:nn))

       !    check for periodic coordinates
       !    it is assumed that the user has provided sufficient sample overlap
       if ( jsferic == 1 ) then

          if ( xhih-xlow > 180d0) then
             xdum = 0.5d0*(xlow+xhih)

             do k=1,nn
                if ( xh(k) < xdum ) then
                   xh(k) = xh(k) + 360d0
                end if
             end do
             xlow = minval(xh(1:nn))
             xhih = maxval(XH(1:NN))

          end if
       end if

       if ( jakdtree == 0 ) then
          call locate(xs, ns, ipsam, xlow, nlowx)
          if (nlowx == 0) nlowx = 1
          call locate(xs, ns, ipsam, xhih, nhihx)
          k_start = nlowx
          k_end   = min(nhihx+1,ns)
       else   ! kdtree
          !       compute cell-bounding circle radius
          R2search = 0d0
          do k=1,nn
             R2search = max(R2search, dbdistance(xc(n), yc(n), xh(k), yh(k), jsferic, jasfer3D, dmiss)**2)
          end do

          !       find all samples in the cell-bounding circle
          call make_queryvector_kdtree(treeglob,xc(n),yc(n), jsferic)

          !       count number of points in search area
          numsam = kdtree2_r_count(treeglob%tree,treeglob%qv,R2search)

          !       set number of points to be queried
          k_start = 1
          k_end   = numsam

          if ( numsam > 0 ) then
             !          resize results array if necessary
             call realloc_results_kdtree(treeglob,numsam)

             !          find samples
             call kdtree2_n_nearest(treeglob%tree, treeglob%qv, numsam, treeglob%results)
          else
             errorInfo%cntNoSamples = errorInfo%cntNoSamples + 1
          end if
       end if

       hparr = 0
       numxy = 0
       rmin2 = dbdistance(XHIH, yhih, xlow, ylow, jsferic, jasfer3D, dmiss)
       ifirs = 0
       wall  = 0
       nin   = 0
       sam:do k_ = k_start, k_end
          if ( jakdtree /= 1 ) then
             k  = ipsam(k_)
          else
             k = treeglob%results(k_)%idx
          end if

          do ivar = 1, ndim
             if ( zss(ivar,k) == dmiss ) cycle sam
          end do

          if (ys(k) >= ylow .and. ys(k) <= yhih) then
             call pinpok(xs(k), ys(k), nn, xh, yh, inhul, jins, dmiss)
             if (inhul == 1) then
                do ivar = 1, ndim
                   select case (iav)
                   case (AVGTP_MEAN)
                      numxy  = numxy + 1 ! NOTE: numxy will be equal to #included samples TIMES ndim!
                      hparr(ivar) = hparr(ivar) + zss(ivar,k)
                   case (AVGTP_NEARESTNB)
                      dis2 = dbdistance(xs(k), ys(k), xc(n), yc(n), jsferic, jasfer3D, dmiss)
                      if (dis2  <  rmin2) then
                         rmin2 = dis2
                         hparr(ivar) = zss(ivar,k)
                         numxy = 1
                      endif
                   case (AVGTP_MAX, AVGTP_MIN, AVGTP_MINABS)
                      if (ifirs == 0) then
                         ifirs = 1
                         hparr(ivar) = zss(ivar,k)
                         numxy = 1
                      endif
                      select case (iav)
                      case (AVGTP_MAX)
                         hparr(ivar) = max(hparr(ivar),zss(ivar,k))
                         if (percentileminmax > 0d0 .and. ivar == 1) then
                            nin = nin + 1
                            kkin(nin) = k
                         endif
                      case (AVGTP_MIN)
                         hparr(ivar) = min(hparr(ivar),zss(ivar,k))
                         if (percentileminmax > 0d0 .and. ivar == 1) then
                            nin = nin + 1
                            kkin(nin) = k
                         endif
                      case (AVGTP_MINABS)
                         hparr(ivar) = min(abs(hparr(ivar)),abs(zss(ivar,k)))
                      end select
                   case (AVGTP_INVDIST)
                      numxy  = numxy + 1
                      dis2   = dbdistance(xs(k), ys(k), xc(n), yc(n), jsferic, jasfer3D, dmiss)
                      dis2   = max(0.01_hp, dis2)
                      weight = 1.0_hp / dis2
                      wall   = wall + weight
                      hparr(ivar) = hparr(ivar) + weight*zss(ivar,k)
                   case (AVGTP_MEDIAN)
                      numxy = 1
                      if (ivar == 1) then ! median only for ndim==1
                         nin = nin + 1
                         kkin(nin) = k
                      endif
                   case default
                      errorInfo%success = .false.
                      errorInfo%message = repeat(' ', len(cfmt) + 10)
                      write(errorInfo%message,cfmt) iav
                      return
                   end select
                end do ! do ivar=1,ndim
             endif
          endif
       enddo sam

       rhp = dmiss
       if (iav == AVGTP_MEAN .or. iav == AVGTP_INVDIST) then
          if (numxy  >=  nummin) then
             if (iav == AVGTP_MEAN) then
                rhp = hparr / real(numxy)
             else if (iav == AVGTP_INVDIST) then
                rhp = hparr/wall
             endif
          endif
       else if (numxy  >=  1) then
          rhp = hparr
          if ((((iav == AVGTP_MAX .or. iav == AVGTP_MIN) .and. percentileminmax > 0d0) &
               .OR. iav == AVGTP_MEDIAN) &
              .and. ndim == 1) then  ! compute percentile or median
             
             ! Sort selected samples
             do nn = 1,nin
                zz(nn) = zss( 1,kkin(nn) )
             enddo
             call radix_sort(zz(1:nin))

             if (iav == AVGTP_MEDIAN) then
                n1 = floor(nin/2d0)
                n2 = ceiling(nin/2d0)
                rhp(1) = (zz(n1) + zz(n2))/2d0

             else if (iav == AVGTP_MAX .or. iav == AVGTP_MIN) then
                rnn   = 0
                rhp(1) = 0d0
                num = nint(0.01d0*percentileminmax*nin)
                if (iav == AVGTP_MIN) then
                   n1 = 1
                   n2 =  num
                   n12 = 1
                else if (iav == AVGTP_MAX) then
                   n1 = nin
                   n2 =  nin - num + 1
                   n12 = -1
                endif
                do nn = n1, n2, n12
                   rnn = rnn + 1d0
                   rhp(1) = rhp(1) + zz(nn)
                enddo
                if (rnn > 0) then
                   rhp(1) = rhp(1) / rnn
                endif
             end if
          endif
       endif

       do ivar=1,ndim
          if (rhp(ivar)  /=  dmiss) then
             zc(ivar,n) = rhp(ivar)
          endif
       end do

    enddo ! n=1,grid_width

    deallocate (XH, YH)

    if ( percentileminmax > 0d0) then
       deallocate( zz, kkin )
    endif

    errorInfo%success = .true.

  end subroutine averaging2

   subroutine LOCATE(XX,N,IPERM,X,J)
   integer,       intent(in)  :: N
   integer,       intent(in)  :: IPERM(:) !< permutation array (increasing xx)
   real(kind=hp), intent(in)  :: XX(:), X
   integer,       intent(out) :: J

   integer          :: JL, JU, JM

   JL=0
   JU=N+1
10 IF(JU-JL > 1)THEN
     JM=(JU+JL)/2
     IF((XX(IPERM(N)) > XX(IPERM(1))).EQV.(X > XX(IPERM(JM))))THEN
       JL=JM
     ELSE
       JU=JM
     ENDIF
     GO TO 10
   ENDIF
   J=JL

   end subroutine LOCATE

   end module m_ec_basic_interpolation
