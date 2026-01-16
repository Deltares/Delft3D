module m_polygon

   implicit none

   double precision, allocatable :: XPL(:), YPL(:), ZPL(:), XPH(:), YPH(:), ZPH(:), DZL(:), DZR(:), DCREST(:), DTL(:), DTR(:), DVEG(:)
   integer, allocatable :: IWEIRT(:)
   integer :: NPL, NPH, MAXPOL, MP, MPS, jakol45 = 0
   character(len=64), allocatable :: nampli(:) ! Names of polylines, set in reapol,
   ! not shifted/updated during editpol.
   double precision :: dxuni = 40d0 ! uniform spacing
   integer :: MAXPOLY = 1000 ! will grow if needed
   double precision, allocatable :: xpmin(:), ypmin(:), xpmax(:), ypmax(:), zpmin(:), zpmax(:)
   integer :: Npoly
   integer, allocatable :: iistart(:), iiend(:)
   integer, allocatable :: ipsection(:)

contains
   !> Increase size of global polyline array.
      !! Specify new size and whether existing points need to be maintained.
   subroutine increasepol(N, jaKeepExisting)
      use m_missing
      use m_alloc
      implicit none
      integer :: n !< Desired new minimum size
      integer :: jaKeepExisting !< Whether or not (1/0) to keep existing points.
      logical :: jakeep
      integer :: maxpolcur
      integer :: ierr

      maxpolcur = size(xpl)
      if (N <= maxpolcur) then
         return
      end if
      MAXPOL = max(100000, int(5d0 * N))

      jakeep = jaKeepExisting == 1

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
      end if

      !     make sure nampli is allocated
      if (.not. allocated(nampli)) then
         allocate (nampli(0))
      end if

   end subroutine increasepol

   !> Copies the global polygon into the backup polygon arrays.
   subroutine SAVEPOL()

      use m_alloc
      use m_missing
      implicit none

      if (NPL > 0) then
         call realloc(xph, maxpol, keepExisting=.false.)
         call realloc(yph, maxpol, keepExisting=.false.)
         call realloc(zph, maxpol, keepExisting=.false.)
         XPH(1:NPL) = XPL(1:NPL)
         YPH(1:NPL) = YPL(1:NPL)
         ZPH(1:NPL) = ZPL(1:NPL)
      end if

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

      if (NPH > 0) then
         XPL(1:NPH) = XPH(1:NPH)
         YPL(1:NPH) = YPH(1:NPH)
         ZPL(1:NPH) = ZPH(1:NPH)
         deallocate(XPH, YPH, ZPH)
      end if
      MP = MPS
      NPL = NPH

      return
   end subroutine restorepol

   function m_polygon_destructor() result(ierr)

      implicit none

      integer :: ierr

      ierr = 0

      if (allocated(XPL) .and. ierr == 0) deallocate (XPL, stat=ierr)
      if (allocated(YPL) .and. ierr == 0) deallocate (YPL, stat=ierr)
      if (allocated(ZPL) .and. ierr == 0) deallocate (ZPL, stat=ierr)
      if (allocated(XPH) .and. ierr == 0) deallocate (XPH, stat=ierr)

      if (allocated(YPH) .and. ierr == 0) deallocate (YPH, stat=ierr)
      if (allocated(ZPH) .and. ierr == 0) deallocate (ZPH, stat=ierr)
      if (allocated(ZPH) .and. ierr == 0) deallocate (ZPH, stat=ierr)

      if (allocated(DZL) .and. ierr == 0) deallocate (DZL, stat=ierr)
      if (allocated(DZR) .and. ierr == 0) deallocate (DZR, stat=ierr)
      if (allocated(DCREST) .and. ierr == 0) deallocate (DCREST, stat=ierr)
      if (allocated(DTL) .and. ierr == 0) deallocate (DTL, stat=ierr)
      if (allocated(DTR) .and. ierr == 0) deallocate (DTR, stat=ierr)
      if (allocated(DVEG) .and. ierr == 0) deallocate (DVEG, stat=ierr)
      if (allocated(IWEIRT) .and. ierr == 0) deallocate (IWEIRT, stat=ierr)

      if (allocated(xpmin) .and. ierr == 0) deallocate (xpmin, stat=ierr)
      if (allocated(ypmin) .and. ierr == 0) deallocate (ypmin, stat=ierr)
      if (allocated(xpmax) .and. ierr == 0) deallocate (xpmax, stat=ierr)
      if (allocated(ypmax) .and. ierr == 0) deallocate (ypmax, stat=ierr)
      if (allocated(zpmin) .and. ierr == 0) deallocate (zpmin, stat=ierr)
      if (allocated(zpmax) .and. ierr == 0) deallocate (zpmax, stat=ierr)
      if (allocated(iistart) .and. ierr == 0) deallocate (iistart, stat=ierr)
      if (allocated(iiend) .and. ierr == 0) deallocate (iiend, stat=ierr)
      if (allocated(ipsection) .and. ierr == 0) deallocate (ipsection, stat=ierr)

      jakol45 = 0
      dxuni = 40d0
      MAXPOLY = 1000
      NPL = 0
      NPH = 0
      MAXPOL = 0
      MP = 0
      MPS = 0
      Npoly = 0

   end function m_polygon_destructor

end module m_polygon