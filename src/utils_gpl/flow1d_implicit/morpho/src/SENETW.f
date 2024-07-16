      subroutine senetw (nbran  ,nnode  ,ngrid  ,maxlev ,maxtab ,ntabm ,
     &                   time   ,g      ,pacfac ,relden ,kinvis ,juer  ,
     &                   branch ,node   ,sdrdbf ,sedinf ,ntab   ,table ,
     &                   e      ,bfrict ,engpar ,rc     ,nucoef ,uscoef,
     &                   trform ,prslot ,af     ,afs    ,wf     ,wfs   ,
     &                   nlev   ,hlev   ,wft    ,ws     ,secths ,asubsc,
     &                   alfab  ,rs     ,cs     ,x      ,q2     ,qs    ,
     &                   h2     ,grsize ,forcon ,sedtr  ,celer  ,lrivr ,
     &                   psltvr ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SENETW (SEdiment NETWork)
c
c Module description: Calculation of the sediment transport in the net-
c                     work.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 27 af                P  -
c 28 afs               P  -
c 37 alfab             P  -
c 36 asubsc            P  -
c 21 bfrict            P  -
c 14 branch            P  -
c 47 celer             P  -
c 39 cs                P  -
c 20 e                 P  -
c 22 engpar            P  -
c 45 forcon            P  -
c  8 g                 P  -
c 44 grsize            P  -
c 43 h2                P  -
c 32 hlev              P  -
c 13 juer              P  -
c 48 ker               P  -
c 12 kinvis            P  -
c  4 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  5 maxtab            I  Maximum number of defined tables.
c  1 nbran             I  Number of branches.
c  3 ngrid             I  Number of grid points in network.
c 31 nlev              P  -
c  2 nnode             I  Number of nodes.
c 15 node              P  -
c 18 ntab              P  -
c  6 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 10 pacfac            P  -
c 26 prslot            P  -
c 49 psltvr            P  -
c 41 q2                P  -
c 42 qs                P  -
c 23 rc                P  -
c 11 relden            P  -
c 38 rs                P  -
c 16 sdrdbf            P  -
c 35 secths            P  -
c 17 sedinf(2,nbran)   I  Defines for each branch whether a branch is a
c                         sedredge branch or not.
c                         (1,j) = Sedredge branch number (1..nsedrd)
c                                 else 0.
c                         (2,j) = Starting index in Rc array (River bend
c                                 curvature).
c 46 sedtr             P  -
c 19 table             P  -
c  7 time              P  -
c 25 trform            P  -
c 24 uscoef            P  -
c 29 wf                P  -
c 30 wfs               P  -
c 33 wft               P  -
c 34 ws                P  -
c 40 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c senorb  SEdiment NORmal Branch
c sesedb  SEdiment SEDredge Branch
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: senetw.pf,v $
c Revision 1.5  1998/06/11  11:47:28  kuipe_j
c Estuary special integrated
c
c Revision 1.4  1997/01/23  08:29:58  kuipe_j
c Make flow module robust
c
c Revision 1.3  1995/10/18  09:00:41  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.2  1995/05/30  07:07:26  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:26  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  08:47:38  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:34:57  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:22  kuipe_j
c Initial version
c
c
c***********************************************************************
c
      include '..\include\sobdim.i'
c
c     Declaration of parameters
c
      integer    nbran  ,nnode ,ngrid ,maxlev  ,maxtab ,ntabm ,juer  ,
     &           ker    ,nucoef
      integer    branch(4,nbran)  ,node(4,nnode)    ,sdrdbf(2,*)      ,
     &           sedinf(2,nbran)  ,bfrict(3,nbran)  ,ntab  (4,maxtab) ,
     &           nlev  (ngrid,maxlev)
      real       g     ,pacfac    ,relden,kinvis
      real       table (ntabm)    ,rc    (*)        ,e     (7,*)      ,
     &           uscoef(nrcoefs,nucoef)             ,trform(3,nbran)  ,
     &           engpar(9)        ,prslot(3,nbran)  ,psltvr(7,ngrid)  ,
     &           af    (ngrid)    ,afs   (ngrid,2 ) ,secths(ngrid)    ,
     &           wf    (ngrid)    ,wfs   (ngrid,2)  ,ws    (ngrid)    ,
     &           wft   (ngrid,maxlev)               ,
     &           asubsc(ngrid)    ,alfab (ngrid)    ,
     &           rs    (ngrid,3)  ,x     (ngrid)    ,
     &           cs    (ngrid,3)  ,qs    (ngrid,2)  ,
     &           grsize(4,ngrid,*),forcon(4,ngrid,*),
     &           sedtr (ngrid,*)  ,celer (ngrid,*)
      logical    lrivr

      double     precision  time, hlev (ngrid,maxlev), 
     &           h2(ngrid), q2(ngrid)
c
c     Declaration of local parameters
c
      integer    ibr

c
      do 10 ibr = 1, nbran
c
         if (sedinf(1,ibr) .gt. 0) then
c
c           Sedredge branch
c
            call sesedb(nbran  ,nnode  ,ngrid  ,maxlev ,maxtab ,ntabm ,
     &                  time   ,g      ,pacfac ,relden ,kinvis ,ibr   ,
     &                  juer   ,branch ,node   ,sdrdbf ,sedinf ,ntab  ,
     &                  table  ,e      ,rc     ,nucoef ,uscoef ,trform,
     &                  afs    ,wf     ,wfs    ,hlev   ,rs     ,cs    ,
     &                  x      ,q2     ,qs     ,h2     ,grsize ,forcon,
     &                  sedtr  ,celer  ,ker    )
         else
c
c           Normal branch
c
            call senorb (nbran  ,ngrid  ,maxlev ,g      ,pacfac ,relden,
     &                   kinvis ,ibr    ,juer   ,branch ,bfrict ,engpar,
     &                   nucoef ,uscoef ,trform ,prslot ,af     ,afs   ,
     &                   wf     ,wfs    ,nlev   ,hlev   ,wft    ,ws    ,
     &                   secths ,rs     ,cs     ,asubsc ,alfab  ,q2    ,
     &                   qs     ,h2     ,grsize ,forcon ,sedtr  ,celer ,
     &                   lrivr  ,psltvr ,ker    )
         endif
   10 continue
c
      end
