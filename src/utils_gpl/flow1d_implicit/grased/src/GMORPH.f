      subroutine GMORPH  (ngrid  ,nbran  ,nboun  ,nnode  ,
     +                    nbrnod ,ntmpfr ,nfrac  ,time   ,
     +                    dtm    ,prslot ,
     +                    sedpar ,morpar ,gsopts ,flwdir ,
     +                    grid   ,branch ,node   ,
     +                    brnode ,bgout  ,tmpfr  ,
     +                    maxtab ,ntabm  ,ntab   ,table  ,
     +                    mbdpar ,x      ,typcr  ,maxlev ,
     +                    nlev   ,hlev   ,h      ,afwfqs ,
     +                    wft    ,ws     ,celer  ,
     +                    celert ,sedtr  ,disgse ,source ,
     +                    deltaa ,dfrac  ,cela1  ,intgr  ,
     +                    itim   ,juer   ,ker    ,wfsold ,
     +                    deltar ,zbave  ,zbfl   ,jugralg,
     +                    sumda  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         S.L. van der Woude
c
c Module:             MORPH (MORPHology)
c
c Module description: Calculate the change in cross sectional area by
c                     solving a continuity equation and adapt the cross
c                     sectional dimensions for each gridpoint in the
c                     network. If a branch is of type sedredge the con-
c                     tinuity equation is solved for the left and right
c                     side of the channel.
c
c                     For each branch in the network routine MOAREA is
c                     called. If a branch is of type sedredge the routi-
c                     ne is called twice. One time for the left side of
c                     the channel and one time for the right side of the
c                     channel.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 36 afwfqs            P  -
c 13 alphac            P  -
c 19 bgout             P  -
c 15 branch            P  -
c 18 brnode            P  -
c 37 celer             P  -
c 39 disgse            P  -
c  8 dtm               P  -
c 14 grid              P  -
c 30 h                 P  -
c 29 hlev              P  -
c 41 itim(2)           I  Actual time level tn+1 expressed in date and
c                         time. Format (integer):
c                         itim(1) = YYYYMMDD (year,month,day)
c                         itim(2) = HHMMSSHH (hour,minute,second,
c                                   hundredth of a second)
c 42 juer              P  -
c 43 ker               I  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 27 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c 21 maxtab            I  Maximum number of defined tables.
c 25 mbdpar            P  -
c 11 moropt            P  -
c  3 nboun             I  Number of boundary nodes.
c  2 nbran             I  Number of branches.
c  5 nbrnod            I  Maximum number of connected branches to one
c                         node.
c  1 ngrid             I  Number of grid points in network.
c 28 nlev              P  -
c  4 nnode             I  Number of nodes.
c 16 node              P  -
c 23 ntab              P  -
c 22 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c  6 ntmpgr            I  Number of scratch arrays with length ngrid
c                         that are packed in tmpgr.
c  9 prslot            P  -
c 35 sectc             P  -
c 38 sedtr             P  -
c 40 slat              P  -
c 24 table             P  -
c  7 time              P  -
c 20 tmpgr             P  -
c 17 typcr(nbran)      I  Type of cross section used in every branch:
c                         ccrtab (1) : Tabulated cross sections
c                         ccrcir (2) : Circle as cross section
c                         ccrsed (3) : Sedredge cross sections
c 31 waoft             P  -
c 32 wft               P  -
c 34 ws                P  -
c 33 wtt               P  -
c 26 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c moarea  MOrphology AREA
c moczio  MOrphology Check Z Increasing Order
c modslt  MOrphology Delete Preissmann SloT
c mointn  MOrphology INTegrals for Nodes
c=======================================================================
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gmorph.F,v $
c Revision 1.4  1996/06/07  11:55:14  kuipe_j
c multi  +  fixed layer
c
c Revision 1.3  1996/01/08  13:29:36  kuipe_j
c Multi layer option for under layer added
c
c Revision 1.2  1995/09/27  10:11:40  kuipe_j
c Maintenance
c
c
c***********************************************************************
c
c
c     Parameters
c
      integer   ngrid  ,nbran  ,nboun  ,nnode  ,nfrac ,
     +          nbrnod ,ntmpfr ,maxtab ,
     +          ntabm  ,maxlev ,juer   ,ker    ,jugralg
      integer   grid   (ngrid) ,flwdir (ngrid) ,
     +          branch (4,nbran),
     +          node   (4,nnode),
     +          brnode (nbrnod+1,nnode),
     +          bgout  (3,nbrnod),
     +          typcr  (nbran),
     +          ntab   (4,maxtab),
     +          mbdpar (5,nboun),
     +          nlev   (ngrid),
     +          itim   (2)    ,
     +          gsopts (*)
      real      prslot (3,nbran)       ,
     +          table  (ntabm)         ,
     +          x      (ngrid)         ,
     +          wft    (ngrid,maxlev)  ,
     +          afwfqs (ngrid,8)     ,
     +          celer  (ngrid,nfrac,5) ,celert (ngrid)       ,
     +          sedtr  (ngrid,nfrac+2) ,
     +          source (ngrid,nfrac+2) ,
     +          deltar (ngrid,nfrac)   ,
     +          disgse (nfrac,2,nbran) ,
     +          intgr  (nfrac,2,nbran) ,
     +          tmpfr  (nfrac+2,ntmpfr),
     +          dfrac  (nfrac)         ,
     +          cela1  (nfrac,nfrac)   ,
     +          wfsold (ngrid)         ,ws     (ngrid)       ,
     +          zbave  (ngrid)         ,zbfl   (ngrid)       ,
     +          sumda  (ngrid)         ,
     +          sedpar (*)             ,morpar  (*)
      double precision  time   ,dtm ,  hlev(ngrid,maxlev) ,
     +                  deltaa (ngrid,nfrac+1)
     
      double precision  h(ngrid,3)
c
c     Local variables
c
      integer      ibr   ,i      ,nonngp ,nvast  ,moropt
      real         zbeps ,alphac ,alphad ,alphae
      character*18 txt
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
c      
      moropt = INT(morpar(4))
      nonngp = gsopts(8)
      nvast  = gsopts(9)
      zbeps  = sedpar(13)
      alphad = morpar(13) 
      alphae = morpar(14) 
      alphac = morpar(15) 
c      
c     Calculate integral values for nodes and boundaries
c
      call GMINTN (nnode ,nbran  ,nbrnod ,ngrid  ,nboun ,maxtab  ,
     +             ntabm ,nfrac  ,time   ,dtm    ,alphac,alphad  ,
     +             alphae,branch ,brnode ,bgout  ,
c                 <itot>          <stot>
     +             tmpfr(1,1)    ,tmpfr(1,2)     ,
     +             node  ,mbdpar ,ntab   ,table  ,disgse ,intgr  ,
     +             sedtr ,celer  ,celert ,source ,dfrac  ,
c                  <ds>            <spredc>
     +             tmpfr(1,3)    ,tmpfr(1,4)     ,cela1  ,flwdir ,
     +             x      ,juer  ,jugralg        ,ker    )
c     Check for error
c
      if (ker .eq. fatal) goto 1000
c
c     Do for each branch in network
c
      do 100 ibr = 1, nbran
c
c        Process continuity equation for whole channel
c
         call GMAREA (ibr    ,ngrid  ,nbran  ,nfrac  ,dtm    ,alphac ,
     +                alphad ,alphae ,grid   ,branch ,x      ,intgr  ,
     +                celer  ,celert ,sedtr  ,source ,flwdir ,dfrac  ,
c                    <ds>                    <spredc>
     +                tmpfr(1,1)     ,cela1  ,tmpfr(1,2)     ,
c                    <intimh>        <intiph>        <intstr>
     +                tmpfr(1,3)     ,tmpfr(1,4)     ,tmpfr(1,5)     ,
     +                deltaa         ,jugralg        )
  
         call GMCROS (ibr    ,ngrid  ,nbran  ,nboun  ,nnode  ,nfrac ,
     +                time   ,moropt ,nonngp ,nvast  ,zbeps ,
     +                grid   ,branch ,node   ,maxtab ,ntabm  ,ntab  ,
     +                table  ,mbdpar ,x      ,maxlev ,nlev   ,hlev  ,
c                    <wf>    
     +                afwfqs(1,3)    ,wft    ,ws     ,flwdir ,deltaa,
c                            <daacor>
     +                deltar ,tmpfr(1,1)     ,zbave  ,zbfl   ,jugralg,
     +                sumda  )

 100  continue
c
c     Remember wfsold in case of more sublayers
c
      do 200 i=1,ngrid
        wfsold(i) = afwfqs(i,3)
  200 continue
c
c     Check for z in increasing order
c
      call MOCZIO ( nbran  ,ngrid  ,maxlev ,branch ,nlev   ,
c                                  <h(n+1)>
     +              hlev   ,typcr  ,h(1,3) ,
     +              juer   ,ker    ,prslot )
c
      if (ker .eq. fatal) goto 1000
c
      return
c
c     Error label
c
 1000 continue
c
      write (txt,'(2(1x,i8))') itim
      call ERROR (juer,'GMORPH timestep@'//txt//'@',emomes,info)
c
      end
