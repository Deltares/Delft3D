subroutine garesultrq (request  ,juer   ,ker     )
!
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Getij Analyse Module
!
! Programmer:         J.Kuipers
!
! Module:             GARESULTRQ (array GARESULT: request for memory)
!
!                     This subroutine creates/updates array space
!                     of array Garesult. Garesult contains the output
!                     of the tidal analyses.
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 1  request           I  GARESULT should have a size of request
! 2  juer              I  Unit number of error file.
! 3  ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!-----------------------------------------------------------------------
! Subprogram calls:
! error           Error messages
!=======================================================================
!
   use        gadata
   include '../include/errcod.i'
!
!     Declaration of parameters
!
   integer    request,  juer    , ker
!
!     Declaration of local variables
!
   integer  i   ,ierr  ,newsize
   character(len=10) artype,siztxt
   character(len=100) txt
   real               ,allocatable :: rwork(:)
!
!     Allocate or increase array garesult
   if ( request .gt. avail ) then
      artype  = 'garesult'
      newsize = (request/5000+1)*5000
      if ( avail.gt.0 ) then
         allocate ( rwork(avail) , stat=ierr )
         if ( ierr .gt. 0 ) goto 9000
         do i = 1,avail
            rwork(i) = garesult(i)
         enddo
         deallocate ( garesult , stat=ierr )
         if ( ierr .gt. 0 ) goto 9010
      endif
      allocate ( garesult(newsize) , stat=ierr )
      if ( ierr .gt. 0 ) goto 9000
      if ( avail.gt.0 ) then
         do i = 1,avail
            garesult(i) = rwork(i)
         enddo
         do i = avail+1,newsize
            garesult(i) = -9.99e9
         enddo
         deallocate ( rwork , stat=ierr )
         if ( ierr .gt. 0 ) goto 9010
      endif
      avail = newsize
   endif
   return
!
9000 continue
!     Error allocating array space
   write(siztxt,'(i10)') newsize
   txt   = 'GARESULT @' // siztxt // '@ @' // artype // '@'
   ker   = fatal
   call sre_error (juer, txt, ealloc, ker )
   return
!
9010 continue
!     Error deallocating array space'
   txt   = 'GARESULT @' // artype // '@'
   ker   = fatal
   call sre_error (juer, txt, edeall, ker )
!
end
