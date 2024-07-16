      subroutine CSSECT(ngrid  ,nbran  ,branch ,typcr  ,
     +                  maxlev ,nlev   ,hlev   ,
     +                  wft    ,aft    ,of     ,
     +                  subsec ,secth0 ,secth1 ,
     +                  wfh0   ,afh0   ,oh0    ,
     +                  wfh1   ,afh1   ,oh1    ,
     +                  psltvr ,secths ,ws     ,lsedt  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Cross Sectional Table Module
c
c Programmer:         J.Brouwer
c
c Module:             CSSECT (Cross Section SECTional parameters)
c
c Module description: In this subroutine the time-independent parameters
c                     Af(h0) = flow area in the main section for h=h0,
c                     and
c                     Af(h1) = flow area in sub section 1 for h=h1, will
c                     be computed.
c
c                     In subroutine CSSECT for each cross section the
c                     constant flow area Af(h0) and the wetted perimeter
c                     O0 are computed for the main section. The parame-
c                     ters Af(h1) and O1 are computed for sub section 1.
c                     Parameters computed in this subroutine are applied
c                     in the computation of Boussinesq's constant and
c                     the Chezy friction (See subroutine FLBOCH in flow
c                     module).
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 15 afh0              P  -
c 18 afh1              P  -
c  9 aft               P  -
c  3 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  7 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
c       maxlev)           - For a circle cross section:
c                         (i,1) = Reference level.
c                         - For a sedredge cross section:
c                         (i,1) = Bed level of main section (i.e. left
c                                 channel).
c                         (i,2) = Bed level of sub section 1 (i.e. right
c                                 channel).
c  5 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  2 nbran             I  Number of branches.
c  1 ngrid             I  Number of grid points in network.
c  6 nlev(ngrid)       I  Number of h-levels for every cross section.
c                         (For a circle cross section   : 1 ;
c                          For a sedredge cross section : 2 )
c 10 of                P  -
c 16 oh0(ngrid)        I  Wetted perimeter Ot at water level h=h0 for
c                         every grid point.
c 19 oh1(ngrid)        IO Wetted perimeter Ot at water level h=h1 for
c                         every grid point.
c 20 psltvr            P  -
c 12 secth0(ngrid)     O  H0-value (for 1 or 2 sub sections) for every
c                         grid point.
c 13 secth1            P  -
c 11 subsec(ngrid)     I  Defines the number of sub sections for every
c                         cross section:
c                         c1sec (0) : Main section only (0 sub sections)
c                         c2sec (1) : 1 sub section
c                         c3sec (2) : 2 sub sections
c                         (For a circle cross section   : 0 ;
c                          For a sedredge cross section : 1 )
c  4 typcr(nbran)      I  Type of cross section used in every branch:
c                         ccrtab (1) : Tabulated cross sections
c                         ccrcir (2) : Circle as cross section
c                         ccrsed (3) : Sedredge cross sections
c 14 wfh0              P  -
c 17 wfh1              P  -
c  8 wft               P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c csseci  Cross Section SECtion I
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: cssect.pf,v $
c Revision 1.5  1997/01/23  08:28:47  kuipe_j
c Make flow module robust
c
c Revision 1.4  1996/05/30  09:59:54  kuipe_j
c comment char
c
c Revision 1.3  1995/05/30  09:54:25  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:55:31  hoeks_a
c files changed from dos-file to unix-files
c
c Revision 1.1  1995/04/13  06:58:34  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:29:54  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:41  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer   ngrid, maxlev, nbran, branch(4,nbran), typcr(nbran)
      integer   nlev(ngrid)
      real      subsec(ngrid)
      real      wft (ngrid,maxlev) ,aft(ngrid,maxlev)
      real      of  (ngrid,maxlev)
      real      secth0(ngrid), secth1(ngrid)
      real      wfh0(ngrid), afh0(ngrid), oh0(ngrid)
      real      wfh1(ngrid), afh1(ngrid), oh1(ngrid)
      real      psltvr(7,ngrid)
      real      secths(ngrid), ws(ngrid)
      double precision hlev(ngrid,maxlev)
      logical   lsedt
c
c     Local variables
c
      integer   i, i1, i2 ,ibr
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
c     Computation for all grid points
c
      do i=1,ngrid
         secths(i) = 0.
      enddo  
c
      do 20 ibr = 1, nbran
c
c        i1 = global grid point number at node n1
c        i2 = global grid point number at node n2
c
         i1 = branch (3,ibr)
         i2 = branch (4,ibr)
c
         if (typcr(ibr) .eq. ccrtab) then
c
            do 10 i = i1, i2
c
c              For rivers with only main section secth0 = hlev(maxlev)
c
               if ( int( subsec(i) ) .eq. 0 ) then
                  secth0(i) = hlev(i,nlev(i))
                  afh0  (i) = aft (i,nlev(i))
               endif
c
               if ( int( subsec(i) ) .ge. 1 ) then
c
c                 Calculate water level, flow area and wetted perimeter
c
                  call CSSECI (ngrid   ,i        ,maxlev  ,nlev   ,
     +                         wft     ,hlev     ,aft     ,of     ,
     +                         wfh0(i) ,secth0(i),afh0(i) ,oh0(i) ,
     +                         psltvr )
               endif
c
               if ( int( subsec(i) ) .eq. 2 ) then
c
c                 Calculate water level, flow area and wetted perimeter
c
                  call CSSECI (ngrid   ,i        ,maxlev  ,nlev   ,
     +                         wft     ,hlev     ,aft     ,of     ,
     +                         wfh1(i) ,secth1(i),afh1(i) ,oh1(i) ,
     +                         psltvr )
c
c                 Calculate O for section 1
c
                  oh1 (i) = oh1 (i) - oh0 (i)
               endif
               if (lsedt) then
                  call cswsed ( ngrid  ,i    ,maxlev ,nlev     ,
     +                          wft    ,hlev ,ws(i)  ,secths(i))
               endif  
   10       continue   
         endif
   20 continue
c
      end
