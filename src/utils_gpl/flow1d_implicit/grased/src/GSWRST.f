      subroutine gswrst (fd_nefis_rst ,grnams ,nentri ,ngrid ,nfrac ,
     &                   nunlay ,nlayer ,ptrla2 ,pexla2 ,p0la  ,nrdzdl,
     &                   lanrinbt       ,zbave  ,zbfl   ,levunl,dzr   ,
     &                   deff2  ,dmed0  ,depos  ,ncelst ,itim  ,nameel,
     &                   neferr )
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
c Module:             GSWRST (Graded Sediment WRiting of reSTart
c                     information)
c
c Module description: Write restart information from memory to restart
c                     file
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  9 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  2 dafdst            P  -
c  1 defdst            P  -
c  3 grnams            P  -
c 10 hlev(ngrid,       IO (i,j) = H at level j in cross section i.
c       maxlev)           - For a circle cross section:
c                         (i,1) = Reference level.
c                         - For a sedredge cross section:
c                         (i,1) = Bed level of main section (i.e. left
c                                 channel).
c                         (i,2) = Bed level of sub section 1 (i.e. right
c                                 channel).
c 14 itim              P  -
c  8 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c 15 nameel            P  -
c  6 nbran             I  Number of branches.
c 13 ncelst            I  Actual cell number of a restart block of the
c                         restart file.
c 16 neferr            O  Nefis error code:
c                         0   = No error
c                         <0  = Error:  odd  = fatal
c                                       even = non fatal
c  4 nentri            I  Number of entries (possible report paramaters)
c                         in the tables of a block (i.e.nameel).
c  5 ngrid             I  Number of grid points in network.
c 11 nlev(ngrid)       I  Number of h-levels for every cross section.
c                         (For a circle cross section   : 1 ;
c                          For a sedredge cross section : 2 )
c  7 ntmpgr            I  Number of scratch arrays with length ngrid
c                         that are packed in tmpgr.
c 12 tmpgr(ngrid,      IO Scratch array for a module:
c                         (i,1) = source(i)
c                         (i,2) = qltgim(i)
c                         (i,3) = distmp(i),dcdx(i),filc(i) or buf(i)
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flsdat  FLuSh buffers of DATa file
c putiel  PUT Integer ELement to nefis file
c putrel  PUT Real ELement to a nefis file
c putlel  PUT Logical ELement to a nefis file
c=======================================================================
c
c     Parameters
c
      integer       nentri ,ngrid  ,nfrac    ,nunlay ,nlayer, itim(2),
     +              ncelst ,neferr
      integer       fd_nefis_rst,
     &              nrdzdl(ngrid)             ,lanrinbt(ngrid)
      real          ptrla2 (ngrid,nfrac)      ,pexla2 (ngrid,nfrac)  ,
     &              p0la   (ngrid,nfrac,nunlay)                      ,
     &              zbave  (ngrid)            ,zbfl   (ngrid)        ,
     &              levunl (ngrid)            ,dzr    (ngrid)        ,
     &              deff2  (ngrid)            ,dmed0  (ngrid)        
      logical       depos  (ngrid)            
c
      character*(*) grnams
      character*(*) nameel(nentri)
c
c     Local variables
c
      integer       error 
      integer       uindex(3), usrord(1)
c
c     Include sobek constants
c
c     Declaration of external functions
c
      integer       flsdat, putrel, putiel, putlel
      external      flsdat, putrel, putiel, putlel 
c
      data          usrord /1/
c
      uindex(1) = ncelst
      uindex(2) = ncelst
      uindex(3) = 1
c
      error = putiel (fd_nefis_rst, grnams, nameel(1),
     +                 uindex, usrord, itim  )
      if (error .ne. 0) goto 1000
c
      error  = putrel (fd_nefis_rst, grnams, nameel(2),
     +                  uindex, usrord, ptrla2   )
      if (error .ne. 0) goto 1000
c
      if (nlayer.eq.2) then
        error  = putrel (fd_nefis_rst, grnams, nameel(3),
     +                    uindex, usrord, pexla2   )
        if (error .ne. 0) goto 1000
      endif
c
      error  = putrel (fd_nefis_rst, grnams, nameel(4),
     +                  uindex, usrord, p0la      )
      if (error .ne. 0) goto 1000
c
      error  = putrel (fd_nefis_rst, grnams, nameel(5),
     +                  uindex, usrord, dzr       )     
      if (error .ne. 0) goto 1000
c   
      error  = putrel (fd_nefis_rst, grnams, nameel(6),
     +                  uindex, usrord, deff2     ) 
      if (error .ne. 0) goto 1000
c
      error  = putrel (fd_nefis_rst, grnams, nameel(7),
     +                  uindex, usrord, levunl    ) 
      if (error .ne. 0) goto 1000
c
      error  = putrel (fd_nefis_rst, grnams, nameel(8),
     +                  uindex, usrord, zbave     )
      if (error .ne. 0) goto 1000
c
      error  = putrel (fd_nefis_rst, grnams, nameel(9),
     +                  uindex, usrord, zbfl      ) 
      if (error .ne. 0) goto 1000
c
      error  = putrel (fd_nefis_rst, grnams, nameel(10),
     +                  uindex, usrord, dmed0     )
      if (error .ne. 0) goto 1000
c
      error  = putiel (fd_nefis_rst, grnams, nameel(11),
     +                  uindex, usrord, nrdzdl    )
      if (error .ne. 0) goto 1000
c
      error  = putiel (fd_nefis_rst, grnams, nameel(12),
     +                  uindex, usrord, lanrinbt  ) 
      if (error .ne. 0) goto 1000
c
      error  = putlel (fd_nefis_rst, grnams, nameel(13),
     +                  uindex, usrord, depos     ) 
      if (error .ne. 0) goto 1000
c
c     Check for error
c
      if (error .ne. 0) goto 1000
c
      error = flsdat (fd_nefis_rst)
c
 1000 continue
      neferr = error
      end
