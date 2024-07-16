      subroutine MOITST ( igp    ,icel   ,isec   ,ngrid  ,nbran  ,
     +                    nboun  ,nnode  ,maxlev ,branch ,ibr    ,
     +                    node   ,ibrtyp ,mbdpar ,hlev   ,grid   ,
     +                    maxtab ,ntabm  ,ntab   ,table  ,h      ,
     +                    wf     ,wfh0   ,ws     ,wft    ,afs    ,
     +                    x      ,dtm    ,alphac ,alphad ,
     +                    celer  ,sedtr  ,dissed ,time   ,flwdir ,
     +                    mopta  ,moptb  , moptc ,moptd  ,mopte  ,
     +                    moptf  ,intstr ,int1   ,int2
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
c Module:             MOITST (MOrphology InTegral for STructure point)
c
c Module description: Calculate structure integral
c
c                     The integral for a structure  is determined at
c                     the outflowing side of the structure. This
c                     because the sum of the integrals must be zero.
c                     Notice that a structure has a begin and end point
c                     This routine will be called with the left grid
c                     point number igp and the right point number icel.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 25 afs               P  -
c 28 alphac            P  -
c 29 alphad            P  -
c  9 branch            P  -
c 30 celer             P  -
c 32 dissed            P  -
c 27 dtm               P  -
c 34 flwdir(ngrid)     I  Indicator for flow direction at each grid
c                         point      1 = positive flow
c                                    0 = zero flow
c                                   -1 = negative flow
c 15 grid              P  -
c 20 h                 P  -
c 14 hlev              P  -
c 10 ibr               P  -
c 12 ibrtyp            P  -
c  2 icel              I  First non structure point after structure
c  1 igp               I  Gridpoint number
c 42 int1              P  -
c 43 int2              P  -
c 41 intstr            O  Calculated integral value for a structure
c  3 isec              P  -
c  8 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c 16 maxtab            I  Maximum number of defined tables.
c 13 mbdpar            P  -
c 35 mopta             P  -
c 36 moptb             P  -
c 37 moptc             P  -
c 38 moptd             P  -
c 39 mopte             P  -
c 40 moptf             P  -
c  6 nboun             I  Number of boundary nodes.
c  5 nbran             I  Number of branches.
c  4 ngrid             I  Number of grid points in network.
c  7 nnode             I  Number of nodes.
c 11 node              P  -
c 18 ntab              P  -
c 17 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 31 sedtr             P  -
c 19 table             P  -
c 33 time              P  -
c 21 wf                P  -
c 22 wfh0              P  -
c 24 wft               P  -
c 23 ws                P  -
c 26 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c moitbp  MORPHology InTegral on Begin Point
c moitep  MOrphology InTegral on End Point
c moitno  MOrphology InTegral at a NOde
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: moitst.pf,v $
c Revision 1.3  1998/06/11  11:47:18  kuipe_j
c Estuary special integrated
c
c Revision 1.2  1996/03/08  09:39:14  kuipe_j
c Headers + moptf temporarily = false
c
c Revision 1.1  1996/03/07  10:44:23  kuipe_j
c Bottom scema acc. to Lax Wendroff with flux limitter
c
c Revision 1.3  1995/10/18  09:00:01  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.2  1995/05/30  07:04:51  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:19  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  08:52:39  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:32:49  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:08  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c
c     Parameters
c
      integer     igp    ,icel   ,isec   ,ngrid  ,ibr    ,nbran  ,
     +            nboun  ,nnode  ,maxtab ,ntabm  ,ibrtyp ,
     +            maxlev ,juer   ,ker

      integer     branch (4,nbran),
     +            grid   (ngrid),
     +            node   (4,nnode),
     +            mbdpar (5,nboun),
     +            ntab   (4,maxtab),
     +            flwdir (ngrid)

      real        alphac ,alphad ,intstr ,int1   ,int2

      real        x      (ngrid),
     +            table  (ntabm),
     +            celer  (ngrid,*),
     +            sedtr  (ngrid,*),
     +            dissed (4,nbran),
     +            wf     (ngrid),
     +            wfh0   (ngrid),
     +            ws     (ngrid),
     +            wft    (ngrid),
     +            afs    (ngrid)

      double  precision   time, dtm, hlev(ngrid,maxlev), h(ngrid)


      logical mopta ,moptb ,moptc ,moptd ,mopte ,moptf
c
c     Local variables
c
      real    iextra
c
c     Determine flow direction at i=istruct
c 
c wijziging zie MOITEP
c
c     if ( flwdir(igp) .ge. 0) then
      if ( flwdir(igp) .gt. 0) then
c
c        Left side of structure is inflowing
c        ------------------------------------

c
c     Calculate Istr
c
         if (flwdir(igp) .eq. 0) then
            intstr = 0.
         else
            CALL MOITNO ( igp    ,isec   ,ngrid  ,x      ,
     +                    dtm    ,alphac ,0      ,branch ,
     +                    ibr    ,nbran  ,grid   ,
     +                    mopta  ,moptb  ,moptc  ,moptd  ,
     +                    celer  ,sedtr  ,alphad ,flwdir ,
     +                    juer   ,ker    ,intstr
     +                  )
         endif
c
c     Calculate I-extra
c
         call MOIEXT ( igp    ,-1     ,isec   ,ngrid  ,
     +                 dtm    ,x      ,celer  ,sedtr  ,iextra )
c
c     Calculate Istr-1/2
c
         CALL MOITEP ( igp    ,igp-1  ,isec   ,ngrid  ,nbran  ,
     +                 nboun  ,nnode  ,maxlev ,branch ,ibr    ,
     +                 node   ,ibrtyp ,mbdpar ,hlev   ,
     +                 grid   ,maxtab ,ntabm  ,ntab   ,
     +                 table  ,h      ,wf     ,wfh0   ,
     +                 ws     ,wft    ,afs    ,dissed ,
     +                 x      ,time   ,dtm    ,alphac ,
     +                 celer  ,sedtr  ,intstr ,flwdir ,
     +                 alphad ,moptc  ,moptf  ,int1   ,
     +                 iextra ,juer   ,ker    )
c
c     Calculate Istr+1/2
c
         CALL MOITBP ( icel   ,icel+1 ,isec   ,ngrid  ,nbran  ,
     +                 nboun  ,nnode  ,maxlev ,branch ,ibr    ,
     +                 node   ,ibrtyp ,mbdpar ,hlev   ,
     +                 grid   ,maxtab ,ntabm  ,ntab   ,
     +                 table  ,h      ,wf     ,wfh0   ,
     +                 ws     ,wft    ,afs    ,dissed ,
     +                 x      ,time   ,dtm    ,alphac ,
     +                 celer  ,sedtr  ,intstr ,flwdir ,
     +                 alphad ,moptd  ,mopte  ,int2   ,
     +                 iextra ,juer   ,ker    )

      else
c
c        Left side of structure is outflowing
c        -------------------------------------
c
c
c     Calculate Istr
c
         CALL MOITNO ( icel   ,isec   ,ngrid  ,x      ,
     +                 dtm    ,alphac ,0      ,branch ,
     +                 ibr    ,nbran  ,grid   ,
     +                 mopta  ,moptb  ,moptc  ,moptd  ,
     +                 celer  ,sedtr  ,alphad ,flwdir ,
     +                 juer   ,ker    ,intstr
     +               )
c
c     Calculate I-extra
c
         call MOIEXT ( icel   , 1     ,isec   ,ngrid  ,
     +                 dtm    ,x      ,celer  ,sedtr  ,iextra )
c
c     Calculate Istr-1/2
c
         CALL MOITEP ( igp    ,igp-1  ,isec   ,ngrid  ,nbran  ,
     +                 nboun  ,nnode  ,maxlev ,branch ,ibr    ,
     +                 node   ,ibrtyp ,mbdpar ,hlev   ,
     +                 grid   ,maxtab ,ntabm  ,ntab   ,
     +                 table  ,h      ,wf     ,wfh0   ,
     +                 ws     ,wft    ,afs    ,dissed ,
     +                 x      ,time   ,dtm    ,alphac ,
     +                 celer  ,sedtr  ,intstr ,flwdir ,
     +                 alphad ,moptc  ,moptf  ,int1   ,
     +                 iextra ,juer   ,ker    )
c
         CALL MOITBP ( icel   ,icel+1 ,isec   ,ngrid  ,nbran  ,
     +                 nboun  ,nnode  ,maxlev ,branch ,ibr    ,
     +                 node   ,ibrtyp ,mbdpar ,hlev   ,
     +                 grid   ,maxtab ,ntabm  ,ntab   ,
     +                 table  ,h      ,wf     ,wfh0   ,
     +                 ws     ,wft    ,afs    ,dissed ,
     +                 x      ,time   ,dtm    ,alphac ,
     +                 celer  ,sedtr  ,intstr ,flwdir ,
     +                 alphad ,moptd  ,mopte  ,int2   ,
     +                 iextra ,juer   ,ker    )

      endif
c
      return
      end
