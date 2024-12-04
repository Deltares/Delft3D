subroutine gswrst (fd_nefis_rst ,grnams ,nentri ,ngrid ,nfrac ,&
&nunlay ,nlayer ,ptrla2 ,pexla2 ,p0la  ,nrdzdl,&
&lanrinbt       ,zbave  ,zbfl   ,levunl,dzr   ,&
&deff2  ,dmed0  ,depos  ,ncelst ,itim  ,nameel,&
&neferr )
!

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Graded Sediment module
!
! Programmer:         J.kuipers
!
! Module:             GSWRST (Graded Sediment WRiting of reSTart
!                     information)
!
! Module description: Write restart information from memory to restart
!                     file
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  9 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  2 dafdst            P  -
!  1 defdst            P  -
!  3 grnams            P  -
! 10 hlev(ngrid,       IO (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
! 14 itim              P  -
!  8 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
! 15 nameel            P  -
!  6 nbran             I  Number of branches.
! 13 ncelst            I  Actual cell number of a restart block of the
!                         restart file.
! 16 neferr            O  Nefis error code:
!                         0   = No error
!                         <0  = Error:  odd  = fatal
!                                       even = non fatal
!  4 nentri            I  Number of entries (possible report paramaters)
!                         in the tables of a block (i.e.nameel).
!  5 ngrid             I  Number of grid points in network.
! 11 nlev(ngrid)       I  Number of h-levels for every cross section.
!                         (For a circle cross section   : 1 ;
!                          For a sedredge cross section : 2 )
!  7 ntmpgr            I  Number of scratch arrays with length ngrid
!                         that are packed in tmpgr.
! 12 tmpgr(ngrid,      IO Scratch array for a module:
!                         (i,1) = source(i)
!                         (i,2) = qltgim(i)
!                         (i,3) = distmp(i),dcdx(i),filc(i) or buf(i)
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flsdat  FLuSh buffers of DATa file
! putiel  PUT Integer ELement to nefis file
! putrel  PUT Real ELement to a nefis file
! putlel  PUT Logical ELement to a nefis file
!=======================================================================
!
!     Parameters
!
   integer       nentri ,ngrid  ,nfrac    ,nunlay ,nlayer, itim(2),&
   &ncelst ,neferr
   integer       fd_nefis_rst,&
   &nrdzdl(ngrid)             ,lanrinbt(ngrid)
   real          ptrla2 (ngrid,nfrac)      ,pexla2 (ngrid,nfrac)  ,&
   &p0la   (ngrid,nfrac,nunlay)                      ,&
   &zbave  (ngrid)            ,zbfl   (ngrid)        ,&
   &levunl (ngrid)            ,dzr    (ngrid)        ,&
   &deff2  (ngrid)            ,dmed0  (ngrid)
   logical       depos  (ngrid)
!
   character*(*) grnams
   character*(*) nameel(nentri)
!
!     Local variables
!
   integer       error
   integer       uindex(3), usrord(1)
!
!     Include sobek constants
!
!     Declaration of external functions
!
   integer       flsdat, putrel, putiel, putlel
   external      flsdat, putrel, putiel, putlel
!
   data          usrord /1/
!
   uindex(1) = ncelst
   uindex(2) = ncelst
   uindex(3) = 1
!
   error = putiel (fd_nefis_rst, grnams, nameel(1),&
   &uindex, usrord, itim  )
   if (error .ne. 0) goto 1000
!
   error  = putrel (fd_nefis_rst, grnams, nameel(2),&
   &uindex, usrord, ptrla2   )
   if (error .ne. 0) goto 1000
!
   if (nlayer.eq.2) then
      error  = putrel (fd_nefis_rst, grnams, nameel(3),&
      &uindex, usrord, pexla2   )
      if (error .ne. 0) goto 1000
   endif
!
   error  = putrel (fd_nefis_rst, grnams, nameel(4),&
   &uindex, usrord, p0la      )
   if (error .ne. 0) goto 1000
!
   error  = putrel (fd_nefis_rst, grnams, nameel(5),&
   &uindex, usrord, dzr       )
   if (error .ne. 0) goto 1000
!
   error  = putrel (fd_nefis_rst, grnams, nameel(6),&
   &uindex, usrord, deff2     )
   if (error .ne. 0) goto 1000
!
   error  = putrel (fd_nefis_rst, grnams, nameel(7),&
   &uindex, usrord, levunl    )
   if (error .ne. 0) goto 1000
!
   error  = putrel (fd_nefis_rst, grnams, nameel(8),&
   &uindex, usrord, zbave     )
   if (error .ne. 0) goto 1000
!
   error  = putrel (fd_nefis_rst, grnams, nameel(9),&
   &uindex, usrord, zbfl      )
   if (error .ne. 0) goto 1000
!
   error  = putrel (fd_nefis_rst, grnams, nameel(10),&
   &uindex, usrord, dmed0     )
   if (error .ne. 0) goto 1000
!
   error  = putiel (fd_nefis_rst, grnams, nameel(11),&
   &uindex, usrord, nrdzdl    )
   if (error .ne. 0) goto 1000
!
   error  = putiel (fd_nefis_rst, grnams, nameel(12),&
   &uindex, usrord, lanrinbt  )
   if (error .ne. 0) goto 1000
!
   error  = putlel (fd_nefis_rst, grnams, nameel(13),&
   &uindex, usrord, depos     )
   if (error .ne. 0) goto 1000
!
!     Check for error
!
   if (error .ne. 0) goto 1000
!
   error = flsdat (fd_nefis_rst)
!
1000 continue
   neferr = error
end
