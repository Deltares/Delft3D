      subroutine garesultrq (request  ,juer   ,ker     )
c
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Getij Analyse Module
c
c Programmer:         J.Kuipers
c
c Module:             GARESULTRQ (array GARESULT: request for memory)
c
c                     This subroutine creates/updates array space 
c                     of array Garesult. Garesult contains the output
c                     of the tidal analyses.
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 1  request           I  GARESULT should have a size of request
c 2  juer              I  Unit number of error file.
c 3  ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c-----------------------------------------------------------------------
c Subprogram calls:
c error           Error messages
c=======================================================================
c
      use        gadata
      include '../include/errcod.i'
c
c     Declaration of parameters
c
      integer    request,  juer    , ker 
c
c     Declaration of local variables
c
      integer  i   ,ierr  ,newsize
      character*10  artype,siztxt
      character*100 txt
      real               ,allocatable :: rwork(:)
c      
c     Allocate or increase array garesult      
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
c
 9000 continue
c     Error allocating array space
      write(siztxt,'(i10)') newsize
      txt   = 'GARESULT @' // siztxt // '@ @' // artype // '@'
      ker   = fatal
      call sre_error (juer, txt, ealloc, ker )
      return
c      
 9010 continue
c     Error deallocating array space'
      txt   = 'GARESULT @' // artype // '@'
      ker   = fatal
      call sre_error (juer, txt, edeall, ker )
c
      end
