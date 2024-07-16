      subroutine moini (ngrid  ,nbran  ,nmlat  ,maxlev ,itim   ,
     +                  branch ,mltpar ,hlev   ,hlev0  ,grid   ,
     +                  newres  ,fd_nefis_rst, fd_nefis_new, juer,
     +                  ncelmo ,ker    ,morini ,sumda  ,lgrad  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         S.L. van der Woude
c
c Module:             MOINI (MOrphology INItialisation)
c
c Module description: Initialise datastructures for the sediment trans-
c                     port module.
c
c                     Before the morphology routine can be activated
c                     first the datastructures have to be initialised.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  6 branch            P  -
c 14 dafdrn            P  -
c 12 dafdst            P  -
c 13 defdrn            P  -
c 11 defdst            P  -
c  8 hlev              P  -
c  5 itim              P  -
c 15 juer              P  -
c 17 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  4 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  2 nbran             I  Number of branches.
c 16 ncelmo            P  -
c 10 newres            P  -
c  1 ngrid             I  Number of grid points in network.
c  3 ntmpgr            I  Number of scratch arrays with length ngrid
c                         that are packed in tmpgr.
c  9 tmpgr             P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c morsta  MOrphology read or write ReSTart information
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: moini.pf,v $
c Revision 1.8  1997/06/17  11:26:57  kuipe_j
c output in history format
c
c Revision 1.7  1997/02/17  10:23:15  kuipe_j
c Lateral Q in m3/s in cont. equation now
c
c Revision 1.6  1997/01/23  08:29:48  kuipe_j
c Make flow module robust
c
c Revision 1.5  1996/01/17  13:18:21  kuipe_j
c header update
c
c Revision 1.4  1996/01/16  15:01:37  kuipe_j
c Restart improvements
c
c Revision 1.3  1995/05/30  09:55:52  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:04:48  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:16  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:32:44  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:08  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      integer   maxlev, ngrid, nbran, nmlat, itim(2), juer, ker
      integer   ncelmo(3)

      integer   fd_nefis_rst, fd_nefis_new, 
     +          branch (4,nbran),
     +          morini(*)       ,
     +          grid(ngrid)

      real      mltpar(9,nmlat),
     +          sumda  (ngrid)        ,hlev0 (ngrid)

      double precision hlev   (ngrid,maxlev)

      logical   inires, first, newres, lgrad
c
      integer   ifil,i
c
c     Include sobek error file
c
      include '..\include\errcod.i'
c
c     Check and update administration of lateral stations
c
      call CHLATA (ngrid  ,nmlat ,nbran, mltpar ,grid  ,branch)
c
      ker   = ok
      first = .true.
c
c     Set initial flags for HIS files
c
      do ifil=1,2
         morini(ifil)=0
      enddo
      do i=1,ngrid
         sumda(i) = 0.0
         hlev0(i) = min(hlev(i,1), hlev(i,2))
      enddo
c
      call morsta ( ngrid , maxlev, itim  ,
     +              hlev  , newres, fd_nefis_rst, fd_nefis_new,
     +              juer  , first , ncelmo(3)     ,
     +              inires, ker   , lgrad )

      return
      end

