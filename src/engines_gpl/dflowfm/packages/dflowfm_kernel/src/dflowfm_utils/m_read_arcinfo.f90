!> ArcInfo header, raster block, and legacy global-data reading.
module m_read_arcinfo
   implicit none(type, external)
   private

   public :: readarcinfoheader, readarcinfoblock, reaarc

contains

   !> Read ArcInfo dimensions and sample geometry, optionally returning a noninteractive status.
   subroutine READARCINFOHEADER(MINP, MMAX, NMAX, X0, Y0, DX, DY, RMIS, status)
      use precision, only: dp
      use m_filez, only: readerror, eoferror

      real(kind=dp) :: dx, dy
      integer :: jacornerx
      integer :: jacornery
      integer :: minp
      integer :: mmax
      integer :: nmax
      real(kind=dp) :: rmis
      real(kind=dp) :: x0
      real(kind=dp) :: y0
      real(kind=dp) :: DumX, DumY
      character(len=132) :: rec
      integer, intent(out), optional :: status !< Zero on success; when present, return on errors without interactive reporting.
      external :: ilowercase

      if (present(status)) then
         status = 1
      end if
      DumY = -1.0e10_dp

10    continue
      read (minp, *, end=100, err=101) rec, mmax
      call ilowercase(rec)
      if (index(REC, 'ncol') < 1) then
         goto 101 ! wrong format
      end if
      read (minp, *, end=100, err=102) rec, nmax
      read (minp, *, end=100, err=103) rec, x0
      call ilowercase(rec)
      JACORNERX = 0
      if (index(REC, 'cor') >= 1) then
         JACORNERX = 1
      end if
      read (minp, *, end=100, err=104) rec, y0
      call ilowercase(rec)
      JACORNERY = 0
      if (index(REC, 'cor') >= 1) then
         JACORNERY = 1
      end if
      read (minp, '(A)', end=100, err=105) rec
      read (REC(10:), *, ERR=105) DX
      DY = DX
      read (REC(10:), *, end=107, err=105) DumX, DumY
      if (DumY > 0) then
         DY = DumY
      end if

107   continue

      !READ(MINP,'(A)',END = 100) REC
      !READ(REC(13:),*,ERR = 106) RMIS
      read (minp, *, end=100, err=106) rec, rmis
      if (JACORNERX == 1) then
         X0 = X0 + DX / 2
      end if
      if (JACORNERy == 1) then
         Y0 = Y0 + DY / 2.0_dp
      end if
      if (present(status)) then
         status = 0
      end if
      return
100   continue
      if (present(status)) then
         return
      end if
      call EOFERROR(MINP)

101   continue
      if (present(status)) then
         return
      end if
      call READERROR('LOOKING FOR NCOLS (ARC-INFO), BUT GETTING', REC, MINP)
102   continue
      if (present(status)) then
         return
      end if
      call READERROR('LOOKING FOR NROWS (ARC-INFO), BUT GETTING', REC, MINP)
103   continue
      if (present(status)) then
         return
      end if
      call READERROR('LOOKING FOR XLLCORNER (ARC-INFO), BUT GETTING', REC, MINP)
104   continue
      if (present(status)) then
         return
      end if
      call READERROR('LOOKING FOR YLLCORNER (ARCINFO), BUT GETTING', REC, MINP)
105   continue
      if (present(status)) then
         return
      end if
      call READERROR('LOOKING FOR CELLSIZE (ARCINFO), BUT GETTING', REC, MINP)
106   continue
      if (present(status)) then
         return
      end if
      call READERROR('LOOKING FOR MISSING VALUE (ARCINFO), BUT GETTING', REC, MINP)
   end subroutine readarcinfoheader

   !> Read an ArcInfo raster block and normalize its missing-value marker.
   subroutine readarcinfoblock(MINP, D, MC, NC, RMIS, status)
      use m_missing, only: dmiss
      use precision, only: dp
      use m_filez, only: readerror, doclose, eoferror

      integer :: i
      integer :: j
      integer :: mc
      integer :: minp
      integer :: nc
      real(kind=dp) :: rmis
      real(kind=dp), dimension(MC, NC) :: D
      character(len=16) :: tex
      integer, intent(out), optional :: status !< Zero on success; when present, leave the file open and return on errors without reporting.

      if (present(status)) then
         status = 1
      end if
      do J = NC, 1, -1
         read (minp, *, err=101, end=100) (D(I, J), I=1, MC)
      end do
      do i = 1, MC
         do j = 1, NC
            if (D(I, J) == RMIS) then
               D(I, J) = dmiss
            end if
         end do
      end do
      if (present(status)) then
         status = 0
      else
         call doclose(minp)
      end if
      return

100   continue
      if (present(status)) then
         return
      end if
      call EOFERROR(minp)
101   continue
      if (present(status)) then
         return
      end if
      write (tex, '(2I8)') I, J
      call READERROR('ERROR READING ARC-INFO BLOCK IN COLNR, ROWNR :', tex, minp)
      return
   end subroutine readarcinfoblock

   !> Load the legacy ArcInfo globals, retaining optional selection and subsampling.
   subroutine REAARC(MINP, japrompt)
      use precision, only: dp
      use m_getreal, only: getreal
      use m_arcinfo, only: mca, nca, x0, y0, dxa, dya, rmis, d, maxarctile
      use m_polygon, only: npl, xpl, ypl
      use m_missing, only: dmiss
      use m_alloc, only: aerr
      use m_qnerror, only: qnerror
      use m_read_large_arc_info_block, only: readlargearcinfoblock

      integer :: ierr
      integer :: minp

      integer, intent(in) :: japrompt !< prompt for step size (1) or not (0)

      integer :: istep, jstep, MCfile, NCfile
      integer :: istart, iend, jstart, jend !< block to be read in file-index numbering

      real(kind=dp) :: distep, djstep, dsqrtnumcur

      call READARCINFOHEADER(MINP, MCa, NCa, X0, Y0, DXa, DYa, RMIS)

      if (allocated(D)) then
         deallocate (D)
      end if

      if (japrompt == 0 .or. mca * nca < MAXARCTILE) then

         allocate (D(MCa, NCa), STAT=IERR)
         call AERR('D(MCa,NCa)', IERR, MCa * NCa)

         call READARCINFOBLOCK(MINP, D, MCa, NCa, RMIS)

      else
         istep = 1
         jstep = 1
         ierr = 1

         MCfile = MCa
         NCfile = NCa

!         do while ( ierr.ne.0 )

         if (NPL <= 0) then
            istart = 1
            iend = MCa
            jstart = 1
            jend = NCa
         else ! use selecting polygon for dimensions of block to be read
            istart = max(1 + int((minval(xpl(1:NPL), xpl(1:NPL) /= DMISS) - X0) / DXa), 1)
            iend = min(1 + int((maxval(xpl(1:NPL), xpl(1:NPL) /= DMISS) - X0) / DXa), MCa)

            jstart = max(1 + int((minval(ypl(1:NPL), ypl(1:NPL) /= DMISS) - Y0) / DYa), 1)
            jend = min(1 + int((maxval(ypl(1:NPL), ypl(1:NPL) /= DMISS) - Y0) / DYa), NCa)
         end if

         if (japrompt == 1) then
!           automatic istep, jstep
            dsqrtnumcur = sqrt(real(iend - istart + 1, kind=dp)) * sqrt(real(jend - jstart + 1, kind=dp))
            distep = dsqrtnumcur / sqrt(real(MAXARCTILE, kind=dp))
            distep = real(int(distep + 0.5_dp), kind=dp)
            djstep = distep

            if (distep > 1.0_dp) then ! only if necessary
               call getreal("istep = ", distep)
               call getreal("jstep = ", djstep)
            end if

            istep = max(int(distep), 1)
            jstep = max(int(djstep), 1)
         end if

         MCa = (iend - istart + 1) / istep
         NCa = (jend - jstart + 1) / jstep

         allocate (D(MCa, NCa), STAT=IERR)
!            CALL AERR('D(MCa,NCa)',IERR,MCa*NCa)

         !        check for allocation error
         if (IERR /= 0) then
            call qnerror('Sample file too large: increase istep and/or jstep', ' ', ' ')
            MCA = 0
            NCA = 0
            !           we cannot deallocate erroneously allocated arrays directly and need to reallocate it correctly first
            allocate (D(1, 1))
            deallocate (D)
            goto 1234
         end if
!         end do   ! do while ( ierr.ne.0 )

         call ReadLargeArcInfoBlock(MINP, MCfile, NCfile, istart, iend, jstart, jend, MCa, NCa, RMIS, istep, jstep, D)

!        modife arcinfo module data
!         X0 = X0 + dble(istep-1)*0.5d0*DXa
!         Y0 = Y0 + dble(jstep-1)*0.5d0*DYa
         X0 = X0 + (istart - 1) * Dxa + real(istep - 1, kind=dp) * 0.5_dp * DXa
         Y0 = Y0 + (jstart - 1) * Dya + real(jstep - 1, kind=dp) * 0.5_dp * DYa
         DXa = real(istep, kind=dp) * DXa
         DYa = real(jstep, kind=dp) * DYa

      end if ! if ( LdirectReadBlock )

!     error handling
1234  continue

      return
   end subroutine reaarc

end module m_read_arcinfo