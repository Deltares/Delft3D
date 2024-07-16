      subroutine MOAREA ( isec   ,ngrid  ,nbran  ,
     +                    nboun  ,nnode  ,maxlev ,branch ,ibr    ,
     +                    node   ,ibrtyp ,mbdpar ,hlev   ,nlev   ,
     +                    grid   ,maxtab ,ntabm  ,ntab   ,
     +                    table  ,h      ,wf     ,wfh0   ,
     +                    ws     ,wft    ,afs    ,dissed ,
     +                    x      ,time   ,dtm    ,alphac ,
     +                    celer  ,sedtr  ,intgr  ,flwdir ,sumda  ,
     +                    alphad ,mopta  ,moptb  ,moptc  ,moptd  ,
     +                    mopte  ,moptf  ,slat   ,moropt ,
     +                    juer   ,ker
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
c Module:             MOAREA (MOrphology AREA)
c
c Module description: Calculate delta A by solving a continuity equation
c                     for each gridpoint in the branch and adapt cross
c                     sectional dimensions. For sedredge branches this
c                     routine will be called once for the left channel
c                     and once for the right channel.
c
c                     First routine MOADBP is called to determine the
c                     change in area for grid point 1. This routine also
c                     delivers the value of the integral at point x =
c                     3/2. The integral is used as a starting point for
c                     the calculation of the internal grid points. The
c                     internal grid points from i1+1 until i2-2 are pro-
c                     cessed by routine MODAIP. The last point is calcu-
c                     lated by routine MOADEP. Point i2-1 is calculated
c                     by routine MODALI using the last results from
c                     routines MODAIP and MODAEP. See also figure one in
c                     chapter three of [S-DO-004].
c
c                     In case a structure is found, the last point
c                     before the structure will be calculated as the
c                     last point of a branch. The structure grid points
c                     are processed by MODAST.
c
c                     Each of the above described routines return a
c                     change in the area. The calculated changes are
c                     used to adapt the cross sectional tables in routi-
c                     ne MOADCS.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 24 afs               P  -
c 29 alphac            P  -
c 34 alphad            P  -
c  7 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 30 celer             P  -
c 25 dissed            P  -
c 28 dtm               P  -
c 33 flwdir            P  -
c 14 grid(ngrid)       I  Grid cell type definition:
c                         cgrdcl (1) : Normal grid cell
c                         cstrcl (2) : Structure cell
c 19 h                 P  -
c 12 hlev              P  -
c  8 ibr               I  Branch number
c 10 ibrtyp            P  -
c 32 intgr(ngrid,*)    I  Integral values for grid
c  1 isec              I  Section number (1 or 2)
c 44 juer              P  -
c 45 ker               P  -
c  6 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c 15 maxtab            I  Maximum number of defined tables.
c 11 mbdpar            P  -
c 35 mopta             P  -
c 36 moptb             P  -
c 37 moptc             P  -
c 38 moptd             P  -
c 39 mopte             P  -
c 40 moptf             P  -
c 42 moropt            P  -
c  4 nboun             I  Number of boundary nodes.
c  3 nbran             I  Number of branches.
c  2 ngrid             I  Number of grid points in network.
c 13 nlev              P  -
c  5 nnode             I  Number of nodes.
c  9 node              P  -
c 17 ntab              P  -
c 16 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 31 sedtr             P  -
c 41 slat              P  -
c    sumda             IO Increase in cross sectional area since start
c 18 table             P  -
c 27 time              P  -
c 20 wf                P  -
c 21 wfh0              P  -
c 23 wft               P  -
c 22 ws                P  -
c 26 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c moadcs  MORPHology Adapt Cross Sections
c modabp  MOrphology Delta Area for Begin Point
c modaep  MOrphology Delta Area for End Point
c modaip  MOrphology Delta Area for Internal Points
c modali  MOrphology Delta Area for Last Internal point
c modast  MOrphology Delta Area for STructures
c modast  MOrphology Delta Area for STructures
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: moarea.pf,v $
c Revision 1.8  1998/06/11  11:47:06  kuipe_j
c Estuary special integrated
c
c Revision 1.7  1997/06/17  11:18:26  kuipe_j
c Remove undefined vars
c
c Revision 1.6  1996/09/03  14:48:47  kuipe_j
c frequency time hist,Improved sed distribution at nodes
c
c Revision 1.5  1996/03/08  09:38:57  kuipe_j
c Headers + moptf temporarily = false
c
c Revision 1.4  1996/03/07  10:44:10  kuipe_j
c Bottom scema acc. to Lax Wendroff with flux limitter
c
c Revision 1.3  1995/05/30  09:55:46  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:04:33  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:02  hoeks_a
c Initial check-in
c
c Revision 1.4  1994/12/02  13:26:23  kuipe_j
c Branches with 2 or 3 grid points are also possible now.
c
c Revision 1.3  1994/11/28  08:52:21  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:32:26  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:05  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c
c     Parameters
c
      integer    isec   ,ngrid  ,ibr    ,nbran  ,
     +           nboun  ,nnode  ,maxtab ,ntabm  ,ibrtyp ,moropt ,
     +           maxlev ,juer   ,ker

      integer    branch (4,nbran),
     +           grid   (ngrid),
     +           node   (4,nnode),
     +           mbdpar (5,nboun),
     +           ntab   (4,maxtab),
     +           nlev   (ngrid),
     +           flwdir (ngrid)

      real       alphac ,alphad

      real       x      (ngrid),
     +           intgr  (ngrid,2,*),
     +           table  (ntabm),
     +           celer  (ngrid,*),
     +           sedtr  (ngrid,*),
     +           dissed (4,nbran),
     +           wf     (ngrid),
     +           wfh0   (ngrid),
     +           ws     (ngrid),
     +           wft    (ngrid,maxlev),
     +           slat   (ngrid,*),
     +           afs    (ngrid)  ,sumda (ngrid)

      double precision  time, dtm, hlev   (ngrid,maxlev)
      double precision  h(ngrid)

      logical    mopta ,moptb ,moptc ,moptd ,mopte ,moptf

c     Local variables
c
      integer   i1     ,i2     ,igp    ,icel   ,istep

      real      intbou ,intimh ,intiph ,intstr ,
     +          int1   ,int2   ,intnmh ,
     +          iextra

      double precision deltaa, delta1, delta2
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
c     Read first and last grid point of branch
c
      i1 = branch(3,ibr)
      i2 = branch(4,ibr)
c
c     Determine delta area on point 1
c
      intbou = intgr(i1,1,isec)
      iextra = intgr(i1,2,isec)
c
      call MODABP ( i1 ,i1+1 ,isec   ,ngrid  ,nbran  ,
     +              nboun  ,nnode  ,maxlev ,branch ,ibr    ,
     +              node   ,ibrtyp ,mbdpar ,hlev   ,
     +              grid   ,maxtab ,ntabm  ,ntab   ,
     +              table  ,h      ,wf     ,wfh0   ,
     +              ws     ,wft    ,afs    ,dissed ,
     +              x      ,time   ,dtm    ,alphac ,
     +              celer  ,sedtr  ,intbou ,flwdir ,
     +              alphad ,moptd  ,mopte  ,slat   ,
     +              intiph ,iextra ,deltaa ,juer   ,ker    )
c
c  Aanpassing Kees Sloff 8-8-1997: ibr toegvoegd aan MODACS
c     Adapt cross section
c
      call MOADCS ( i1     ,isec   ,ibrtyp ,
     +              deltaa ,time   ,moropt ,
     +              nboun  ,ngrid  ,nnode  ,
     +              branch ,ibr    ,node   ,mbdpar ,
     +              maxtab ,ntabm  ,ntab   ,table  ,
     +              maxlev ,nlev   ,hlev   ,
     +              wft    ,ws     ,
     +              flwdir
     +            )
      sumda(i1) = sumda(i1) + deltaa
c
c     Determine first internal point and save integral on halve point
c
      igp = i1 + 1
      intimh = intiph
c
c     Loop over internal points
c
 100  continue
c
c     Stop if igp >= i2 - 1
c
      if ( igp .lt. i2-1 ) then
c
c        Check if next grid point = structure point
c
         if ( grid (igp+1) .eq. cstrcl) then
c
c           Calculate I_n-1/2 , I_structure and I_n+1/2
c
c
c           locate end point of structure
c
            istep = 1
  200       continue
            if (grid(igp+1+istep) .eq. cstrcl) then
               istep=istep+1
               goto 200
            endif
            icel = igp + istep + 1
            call MODAST ( igp+1  ,icel   ,isec   ,ngrid  ,nbran  ,
     +                    nboun  ,nnode  ,maxlev ,branch ,ibr    ,
     +                    node   ,ibrtyp ,mbdpar ,hlev   ,grid   ,
     +                    maxtab ,ntabm  ,ntab   ,table  ,h      ,
     +                    wf     ,wfh0   ,ws     ,wft    ,afs    ,
     +                    x      ,dtm    ,alphac ,alphad ,
     +                    celer  ,sedtr  ,dissed ,time   ,flwdir ,
     +                    mopta  ,moptb  ,moptc  ,moptd  ,mopte  ,
     +                    moptf  ,slat   ,intstr ,int1   ,int2   ,
     +                    delta1 ,delta2
     +                  )
c
c           Adapt cross section for left side of structure
c
            call MOADCS ( igp+1  ,isec   ,ibrtyp ,
     +                    delta1 ,time   ,moropt ,
     +                    nboun  ,ngrid  ,nnode  ,
     +                    branch ,ibr    ,node   ,mbdpar ,
     +                    maxtab ,ntabm  ,ntab   ,table  ,
     +                    maxlev ,nlev   ,hlev   ,
     +                    wft    ,ws     ,
     +                    flwdir
     +                  )
            sumda(igp+1) = sumda(igp+1) + real(delta1)
c
c           Adapt cross section for right side of structure
c
            call MOADCS ( icel   ,isec   ,ibrtyp ,
     +                    delta2 ,time   ,moropt ,
     +                    nboun  ,ngrid  ,nnode  ,
     +                    branch ,ibr    ,node   ,mbdpar ,
     +                    maxtab ,ntabm  ,ntab   ,table  ,
     +                    maxlev ,nlev   ,hlev   ,
     +                    wft    ,ws     ,
     +                    flwdir
     +                  )
            sumda(icel) = sumda(icel) + real(delta2)
c
c           Process last point before structure
c
            intnmh = int1
            call MODALI ( igp-1  ,igp    ,igp+1  ,
     +                    isec   ,ngrid  ,
     +                    dtm    ,
     +                    intimh ,intnmh ,
     +                    slat   ,x      ,
     +                    deltaa
     +                  )
c
c           Adapt cross section
c
            call MOADCS ( igp    ,isec   ,ibrtyp ,
     +                    deltaa ,time   ,moropt ,
     +                    nboun  ,ngrid  ,nnode  ,
     +                    branch ,ibr    ,node   ,mbdpar ,
     +                    maxtab ,ntabm  ,ntab   ,table  ,
     +                    maxlev ,nlev   ,hlev   ,
     +                    wft    ,ws     ,
     +                    flwdir
     +                  )
            sumda(igp) = sumda(igp) + real(deltaa)
c
c           Determine next grid point and save integral on halve point
c
            igp   = icel + 1
            intimh = int2
         else
c
c           Next point is a normal grid cell
c
c           intiph [I(i+1/2)] from previous point is used
c
            intiph = intimh
            call MODAIP ( igp    ,isec   ,ngrid  ,
     +                    alphac ,dtm    ,alphad ,
     +                    celer  ,sedtr  ,slat   ,x     ,
     +                    intiph ,deltaa
     +                  )
c
c           Adapt cross section
c
            call MOADCS ( igp    ,isec   ,ibrtyp ,
     +                    deltaa ,time   ,moropt ,
     +                    nboun  ,ngrid  ,nnode  ,
     +                    branch ,ibr    ,node   ,mbdpar ,
     +                    maxtab ,ntabm  ,ntab   ,table  ,
     +                    maxlev ,nlev   ,hlev   ,
     +                    wft    ,ws     ,
     +                    flwdir
     +                  )
            sumda(igp) = sumda(igp) + deltaa
c
c           Determine next grid point and save integral at halve point
c
            igp   = igp + 1
            intimh = intiph
c
         endif
         goto 100
      endif
c
c     Last point of branch
c
c       * intnmh [I(n-1/2)] will be calculated
c
      intbou = intgr(i2,1,isec)
      iextra = intgr(i2,2,isec)
c
      call MODAEP ( i2     ,i2-1   ,isec   ,ngrid  ,nbran  ,
     +              nboun  ,nnode  ,maxlev ,branch ,ibr    ,
     +              node   ,ibrtyp ,mbdpar ,hlev   ,
     +              grid   ,maxtab ,ntabm  ,ntab   ,
     +              table  ,h      ,wf     ,wfh0   ,
     +              ws     ,wft    ,afs    ,dissed ,
     +              x      ,time   ,dtm    ,alphac ,
     +              celer  ,sedtr  ,intbou ,flwdir ,
     +              alphad ,moptc  ,moptf  ,slat   ,
     +              intnmh ,iextra ,deltaa ,juer   ,ker    )
c
c     Adapt cross section
c
      call MOADCS ( i2     ,isec   ,ibrtyp ,
     +              deltaa ,time   ,moropt ,
     +              nboun  ,ngrid  ,nnode  ,
     +              branch ,ibr    ,node   ,mbdpar ,
     +              maxtab ,ntabm  ,ntab   ,table  ,
     +              maxlev ,nlev   ,hlev   ,
     +              wft    ,ws     ,
     +              flwdir
     +            )
      sumda(i2) = sumda(i2) + real( deltaa )
c
c     One but last point of branch
c
c       * intimh [I(i+1/2)] from previous point is used
c       * intnmh [I(n-1/2)] from end point is used
c
      if (igp .lt. i2) then
         call MODALI ( igp-1  ,igp    ,igp+1  ,
     +                 isec   ,ngrid  ,
     +                 dtm    ,
     +                 intimh ,intnmh ,
     +                 slat   ,x      ,
     +                 deltaa
     +                  )
c
c        Adapt cross section
c
         call MOADCS ( igp    ,isec   ,ibrtyp ,
     +                 deltaa ,time   ,moropt ,
     +                 nboun  ,ngrid  ,nnode  ,
     +                 branch ,ibr    ,node   ,mbdpar ,
     +                 maxtab ,ntabm  ,ntab   ,table  ,
     +                 maxlev ,nlev   ,hlev   ,
     +                 wft    ,ws     ,
     +                 flwdir
     +               )
         sumda(igp) = sumda(igp) + real( deltaa )
      endif

      return
      end
