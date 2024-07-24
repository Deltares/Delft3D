      subroutine mowrst (fd_nefis_rst, grnams, nentri,
     +                    ngrid , maxlev, hlev  , ncelst, itim  ,
     +                    nameel, neferr
     +                  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         S.L. van der Woude
c
c Module:             MOWRST (MOrphology WRiting of reSTart information)
c
c Module description: Write restart information from memory to restart
c                     file
c
c                     Write cross sections to the restart file.
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
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: mowrst.pf,v $
c Revision 1.8  1997/02/17  10:23:20  kuipe_j
c Lateral Q in m3/s in cont. equation now
c
c Revision 1.7  1997/01/23  08:29:53  kuipe_j
c Make flow module robust
c
c Revision 1.6  1996/11/05  13:48:21  kuipe_j
c Error in declaration hlev fixed
c
c Revision 1.5  1996/01/17  13:18:24  kuipe_j
c header update
c
c Revision 1.4  1995/10/18  09:00:10  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.3  1995/05/30  09:55:59  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:05:03  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:28  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:33:05  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:10  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      integer       nentri, ngrid, maxlev, itim(2),
     +              ncelst, neferr

      integer       fd_nefis_rst

      double precision hlev(ngrid,maxlev)

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
      integer       flsdat, putrel, putiel
      external      flsdat, putrel, putiel
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
c     Write h-levels
c
      error = putrel (fd_nefis_rst, grnams, nameel(2),
     +                 uindex, usrord, hlev  )
c
c     Check for error
c
      if (error .ne. 0) goto 1000
c
      error = flsdat (fd_nefis_rst)
c
 1000 continue
      neferr = error

      return
      end
