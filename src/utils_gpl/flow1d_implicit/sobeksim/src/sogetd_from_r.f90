      subroutine sogetd_from_r(fd_nefis, grpnam, elmnam, nmax, nbytsg, juer, ker)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SOGETR (SObek GET Real variable and store into Double
! Module description: Allocate memory and read contents of variable into
!                     memory  (real pool). See chapter 6 of
!                     [S-DO-000.2SW] for an explanation of the memory
!                     management routines and pools.
!

!
!     Parameters
!
   integer      fd_nefis
   character*16 grpnam, elmnam
   integer      nbytsg, nmax
   integer      juer,   ker
   !
   !     Variables
   !
   integer      start, stop, incr
   integer      uindex(3), usrord(1)
   integer      errcod, pntr, errno
   character*60 txt
   character*8  eno
   parameter    (start=1, stop=2, incr=3)
   real, allocatable, dimension(:)           :: sngl_buffer

   integer, external       :: getrel, mkdpnt
   !
   !     Include memory pool
   !
   include '..\include\memplf90.i'
   include '..\include\errcod.i'
   !
   !     Initialise usrord
   !
   usrord (1) = 1
   !
   uindex ( start ) = 1
   uindex ( stop  ) = 1
   uindex ( incr  ) = 1
   !
   ! Get single data into buffer
   !
   allocate(sngl_buffer(nmax))
   errcod = getrel(fd_nefis, grpnam, elmnam, uindex, usrord, nmax * nbytsg, sngl_buffer)
   !
   !        Check for error
   !
   if (errcod .ne. 0) then
      write(eno,'(i8)') errcod
      txt   = 'SOGETD_FROM_R @' // elmnam // '@ @' // eno // '@'
      ker   = fatal
      errno = egetel
      call error (juer, txt, errno, ker )
   endif
   
   pntr = mkdpnt(elmnam, nmax)
   if (pntr .lt. 0) then
      if (pntr .eq. -1) errno = evrdec
      if (pntr .eq. -2) errno = eoutds
      if (pntr .eq. -3) errno = eoutns

      txt = 'SOGETR Memory error for @' // elmnam // '@'
      ker = fatal
      call error (juer, txt, errno, ker )
   endif
   
   call copy_real2db(nmax, sngl_buffer, dp(pntr))
   
   deallocate(sngl_buffer)
   
end subroutine sogetd_from_r

subroutine copy_real2db(nmax, sngl_buffer, dble_array)

   integer, intent(in)                             :: nmax
   real, dimension(nmax), intent(in)               :: sngl_buffer
   double precision, dimension(nmax), intent(out)  :: dble_array
   
   integer                                   :: i
   
   do i = 1, nmax
      dble_array(i) = dble(sngl_buffer(i))
   enddo

end subroutine copy_real2db

