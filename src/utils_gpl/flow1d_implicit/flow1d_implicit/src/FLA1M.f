      subroutine FLA1M(ngrid  ,nbran  ,branch  ,typcr  ,
     +                 h1      ,h     ,maxlev  ,nlev   ,hlev   ,
     +                 wft    ,af     ,izwft   ,a1m    ,theta2 )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLA1M (FLow A1M)
c
c Module description: Compute first order momentum cross section A1m for
c                     each grid point in network.
c
c                     Notice that several formulations for cross secti-
c                     ons are possible. Each type will be processed by a
c                     different routine.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 14 a1m               P  -
c 12 af                P  -
c  3 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  6 h1                P  -
c  7 h                 P  -
c 10 hlev              P  -
c 13 izwft             P  -
c  8 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  2 nbran             I  Number of branches.
c  1 ngrid             I  Number of grid points in network.
c  9 nlev              P  -
c 15 theta2            P  -
c  4 typcr(nbran)      I  Type of cross section used in every branch:
c                         ccrtab (1) : Tabulated cross sections
c                         ccrcir (2) : Circle as cross section
c                         ccrsed (3) : Sedredge cross sections
c 11 wft               P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c fla1mc  FLow A1M Circle
c fla1ms  FLow A1M Sedredge
c fla1mt  FLow A1M Table
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: fla1m.pf,v $
c Revision 1.7  1997/09/30  09:25:21  kuipe_j
c density term improved for Preisman slot
c
c Revision 1.6  1997/01/23  08:28:51  kuipe_j
c Make flow module robust
c
c Revision 1.5  1995/10/18  08:59:12  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.4  1995/09/12  08:10:39  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.2  1993/11/26  15:30:18  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:45  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer   ngrid, nbran, maxlev
      integer   branch(4,nbran), typcr(nbran), nlev(ngrid)
      double precision      hlev(ngrid,maxlev)
      double precision      h1(ngrid), h(ngrid)
      real      wft  (ngrid,maxlev), af (ngrid)
      real      izwft(ngrid,maxlev), a1m(ngrid)
      real      theta2
c
c     Declaration of local variables:
c
      integer  ibr, i1, i2
c
c     Include sobek constants
c
      include '../include/sobcon.i'
c
c     Loop over branches
c
      do 100 ibr = 1, nbran
c
c        i1 = global grid point number at node n1
c        i2 = global grid point number at node n2
c
         i1 = branch (3,ibr)
         i2 = branch (4,ibr)
c
c        Normal branch with tabulated cross sections
c
        if ( typcr(ibr) .eq. ccrtab ) then
            call FLA1MT(i1     ,i2     ,ngrid  ,h1     ,
     +                  h      ,maxlev ,nlev   ,hlev   ,wft    ,af     ,
     +                  izwft  ,a1m    ,theta2 )
c
c        Circle cross section in branch
c
        else if ( typcr(ibr) .eq. ccrcir ) then
           call FLA1MC(i1     ,i2     ,ngrid  ,h1     ,h      ,maxlev ,
     +                  hlev   ,wft    ,a1m   ,theta2)
c
c        Sedredge branch
c
        else if ( typcr(ibr) .eq. ccrsed ) then
            call FLA1MS(i1     ,i2     ,ngrid  ,h1     ,h      ,maxlev ,
     +                  hlev   ,af     ,a1m    ,theta2 )
        endif
 100  continue
c
      end
