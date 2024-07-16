      subroutine gsrest (fd_nefis_rst ,grnams ,nentri ,ngrid ,nfrac ,
     &                   nunlay ,nlayer ,ptrla2 ,pexla2 ,p0la  ,nrdzdl,
     &                   lanrinbt       ,zbave  ,zbfl   ,levunl,dzr   ,
     &                   deff2  ,dmed0  ,depos  ,ncelst ,nameel,neferr)
c
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Graded Sediment module
c
c Programmer:         J.kuipers
c
c Module:             GSREST (Graded Sediment REading of reSTart
c                     information)
c
c Module description: Read restart information into memory
c
c                     Depending on the number of layers used se-
c                     veral variables must be read from the restart
c                     file.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 dafdst            P  -
c  1 defdst            P  -
c  3 grnams            P  -
c  7 hlev              P  -
c  6 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  9 nameel            P  -
c  8 ncelst            I  Actual cell number of a restart block of the
c                         restart file.
c 10 neferr            O  Nefis error code:
c                         0   = No error
c                         <0  = Error:  odd  = fatal
c                                       even = non fatal
c  4 nentri            I  Number of entries (possible report paramaters)
c                         in the tables of a block (i.e.nameel).
c  5 ngrid             I  Number of grid points in network.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c getiel  GET Integer ELement from a nefis file
c getrel  GET Real ELement from a nefis file
c getlel  GET Logical ELement from a nefis file
c=======================================================================
c
c     Parameters
c
      integer       nentri ,ngrid  ,nfrac     ,nunlay ,nlayer ,ncelst,
     &              neferr
      integer       fd_nefis_rst,
     &              nrdzdl(ngrid)             ,lanrinbt(ngrid)
      real          ptrla2 (ngrid,nfrac)      ,pexla2 (ngrid,nfrac)  ,
     &              p0la   (ngrid,nfrac,nunlay)                      ,
     &              zbave  (ngrid)            ,zbfl   (ngrid)        ,
     &              levunl (ngrid)            ,dzr    (ngrid)        ,
     &              deff2  (ngrid)            ,dmed0  (ngrid)        
      logical       depos  (ngrid)            
      character*(*) grnams
      character*(*) nameel(nentri)
c
c     Local variables
c
      integer       error, buflen
      integer       uindex(3), usrord(1)
c
c     Declaration of external functions
c
      integer       getrel ,getiel ,getlel
      external      getrel ,getiel ,getlel
c
      data          usrord /1/
c
      uindex(1) = ncelst
      uindex(2) = ncelst
      uindex(3) = 1
c
      buflen = ngrid * nfrac * 4
      error  = getrel(fd_nefis_rst, grnams, nameel(2),
     +                  uindex, usrord, buflen, ptrla2   )
      if (error .ne. 0) goto 1000
c
      if (nlayer.eq.2) then
        error  = getrel(fd_nefis_rst, grnams, nameel(3),
     +                    uindex, usrord, buflen, pexla2   )
        if (error .ne. 0) goto 1000
      endif

      buflen = ngrid * nfrac * nunlay * 4
      error  = getrel(fd_nefis_rst, grnams, nameel(4),
     +                  uindex, usrord, buflen, p0la      )
      if (error .ne. 0) goto 1000
c     
      buflen = ngrid * 4
      error  = getrel(fd_nefis_rst, grnams, nameel(5),
     +                  uindex, usrord, buflen, dzr       )     
      if (error .ne. 0) goto 1000
c  
      error  = getrel(fd_nefis_rst, grnams, nameel(6),
     +                  uindex, usrord, buflen, deff2     ) 
      if (error .ne. 0) goto 1000
c
      error  = getrel(fd_nefis_rst, grnams, nameel(7),
     +                  uindex, usrord, buflen, levunl    ) 
      if (error .ne. 0) goto 1000
c
      error  = getrel(fd_nefis_rst, grnams, nameel(8),
     +                  uindex, usrord, buflen, zbave     )
      if (error .ne. 0) goto 1000
c
      error  = getrel(fd_nefis_rst, grnams, nameel(9),
     +                  uindex, usrord, buflen, zbfl      ) 
      if (error .ne. 0) goto 1000
c
      error  = getrel(fd_nefis_rst, grnams, nameel(10),
     +                  uindex, usrord, buflen, dmed0     )
      if (error .ne. 0) goto 1000
c
      error  = getiel(fd_nefis_rst, grnams, nameel(11),
     +                  uindex, usrord, buflen, nrdzdl    )
      if (error .ne. 0) goto 1000
c
      error  = getiel(fd_nefis_rst, grnams, nameel(12),
     +                  uindex, usrord, buflen, lanrinbt  ) 
      if (error .ne. 0) goto 1000
c
      error  = getlel(fd_nefis_rst, grnams, nameel(13),
     +                  uindex, usrord, buflen, depos     ) 
      if (error .ne. 0) goto 1000
c     
 1000 continue
      neferr = error
      end
