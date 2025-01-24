subroutine GMORPH  (ngrid  ,nbran  ,nboun  ,nnode  ,&
&nbrnod ,ntmpfr ,nfrac  ,time   ,&
&dtm    ,prslot ,&
&sedpar ,morpar ,gsopts ,flwdir ,&
&grid   ,branch ,node   ,&
&brnode ,bgout  ,tmpfr  ,&
&maxtab ,ntabm  ,ntab   ,table  ,&
&mbdpar ,x      ,typcr  ,maxlev ,&
&nlev   ,hlev   ,h      ,afwfqs ,&
&wft    ,ws     ,celer  ,&
&celert ,sedtr  ,disgse ,source ,&
&deltaa ,dfrac  ,cela1  ,intgr  ,&
&itim   ,juer   ,ker    ,wfsold ,&
&deltar ,zbave  ,zbfl   ,jugralg,&
&sumda  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             MORPH (MORPHology)
!
! Module description: Calculate the change in cross sectional area by
!                     solving a continuity equation and adapt the cross
!                     sectional dimensions for each gridpoint in the
!                     network. If a branch is of type sedredge the con-
!                     tinuity equation is solved for the left and right
!                     side of the channel.
!
!                     For each branch in the network routine MOAREA is
!                     called. If a branch is of type sedredge the routi-
!                     ne is called twice. One time for the left side of
!                     the channel and one time for the right side of the
!                     channel.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 36 afwfqs            P  -
! 13 alphac            P  -
! 19 bgout             P  -
! 15 branch            P  -
! 18 brnode            P  -
! 37 celer             P  -
! 39 disgse            P  -
!  8 dtm               P  -
! 14 grid              P  -
! 30 h                 P  -
! 29 hlev              P  -
! 41 itim(2)           I  Actual time level tn+1 expressed in date and
!                         time. Format (integer):
!                         itim(1) = YYYYMMDD (year,month,day)
!                         itim(2) = HHMMSSHH (hour,minute,second,
!                                   hundredth of a second)
! 42 juer              P  -
! 43 ker               I  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 27 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
! 21 maxtab            I  Maximum number of defined tables.
! 25 mbdpar            P  -
! 11 moropt            P  -
!  3 nboun             I  Number of boundary nodes.
!  2 nbran             I  Number of branches.
!  5 nbrnod            I  Maximum number of connected branches to one
!                         node.
!  1 ngrid             I  Number of grid points in network.
! 28 nlev              P  -
!  4 nnode             I  Number of nodes.
! 16 node              P  -
! 23 ntab              P  -
! 22 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
!  6 ntmpgr            I  Number of scratch arrays with length ngrid
!                         that are packed in tmpgr.
!  9 prslot            P  -
! 35 sectc             P  -
! 38 sedtr             P  -
! 40 slat              P  -
! 24 table             P  -
!  7 time              P  -
! 20 tmpgr             P  -
! 17 typcr(nbran)      I  Type of cross section used in every branch:
!                         ccrtab (1) : Tabulated cross sections
!                         ccrcir (2) : Circle as cross section
!                         ccrsed (3) : Sedredge cross sections
! 31 waoft             P  -
! 32 wft               P  -
! 34 ws                P  -
! 33 wtt               P  -
! 26 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! moarea  MOrphology AREA
! moczio  MOrphology Check Z Increasing Order
! modslt  MOrphology Delete Preissmann SloT
! mointn  MOrphology INTegrals for Nodes
!=======================================================================
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gmorph.F,v $
! Revision 1.4  1996/06/07  11:55:14  kuipe_j
! multi  +  fixed layer
!
! Revision 1.3  1996/01/08  13:29:36  kuipe_j
! Multi layer option for under layer added
!
! Revision 1.2  1995/09/27  10:11:40  kuipe_j
! Maintenance
!
!
!***********************************************************************
!
!
!     Parameters
!
   integer   ngrid  ,nbran  ,nboun  ,nnode  ,nfrac ,&
   &nbrnod ,ntmpfr ,maxtab ,&
   &ntabm  ,maxlev ,juer   ,ker    ,jugralg
   integer   grid   (ngrid) ,flwdir (ngrid) ,&
   &branch (4,nbran),&
   &node   (4,nnode),&
   &brnode (nbrnod+1,nnode),&
   &bgout  (3,nbrnod),&
   &typcr  (nbran),&
   &ntab   (4,maxtab),&
   &mbdpar (5,nboun),&
   &nlev   (ngrid),&
   &itim   (2)    ,&
   &gsopts (*)
   real      prslot (3,nbran)       ,&
   &table  (ntabm)         ,&
   &x      (ngrid)         ,&
   &wft    (ngrid,maxlev)  ,&
   &afwfqs (ngrid,8)     ,&
   &celer  (ngrid,nfrac,5) ,celert (ngrid)       ,&
   &sedtr  (ngrid,nfrac+2) ,&
   &source (ngrid,nfrac+2) ,&
   &deltar (ngrid,nfrac)   ,&
   &disgse (nfrac,2,nbran) ,&
   &intgr  (nfrac,2,nbran) ,&
   &tmpfr  (nfrac+2,ntmpfr),&
   &dfrac  (nfrac)         ,&
   &cela1  (nfrac,nfrac)   ,&
   &wfsold (ngrid)         ,ws     (ngrid)       ,&
   &zbave  (ngrid)         ,zbfl   (ngrid)       ,&
   &sumda  (ngrid)         ,&
   &sedpar (*)             ,morpar  (*)
   double precision  time   ,dtm ,  hlev(ngrid,maxlev) ,&
   &deltaa (ngrid,nfrac+1)

   double precision  h(ngrid,3)
!
!     Local variables
!
   integer      ibr   ,i      ,nonngp ,nvast  ,moropt
   real         zbeps ,alphac ,alphad ,alphae
   character*18 txt
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
   moropt = INT(morpar(4))
   nonngp = gsopts(8)
   nvast  = gsopts(9)
   zbeps  = sedpar(13)
   alphad = morpar(13)
   alphae = morpar(14)
   alphac = morpar(15)
!
!     Calculate integral values for nodes and boundaries
!
   call GMINTN (nnode ,nbran  ,nbrnod ,ngrid  ,nboun ,maxtab  ,&
   &ntabm ,nfrac  ,time   ,dtm    ,alphac,alphad  ,&
   &alphae,branch ,brnode ,bgout  ,&
!                 <itot>          <stot>
   &tmpfr(1,1)    ,tmpfr(1,2)     ,&
   &node  ,mbdpar ,ntab   ,table  ,disgse ,intgr  ,&
   &sedtr ,celer  ,celert ,source ,dfrac  ,&
!                  <ds>            <spredc>
   &tmpfr(1,3)    ,tmpfr(1,4)     ,cela1  ,flwdir ,&
   &x      ,juer  ,jugralg        ,ker    )
!     Check for error
!
   if (ker .eq. fatal) goto 1000
!
!     Do for each branch in network
!
   do 100 ibr = 1, nbran
!
!        Process continuity equation for whole channel
!
      call GMAREA (ibr    ,ngrid  ,nbran  ,nfrac  ,dtm    ,alphac ,&
      &alphad ,alphae ,grid   ,branch ,x      ,intgr  ,&
      &celer  ,celert ,sedtr  ,source ,flwdir ,dfrac  ,&
!                    <ds>                    <spredc>
      &tmpfr(1,1)     ,cela1  ,tmpfr(1,2)     ,&
!                    <intimh>        <intiph>        <intstr>
      &tmpfr(1,3)     ,tmpfr(1,4)     ,tmpfr(1,5)     ,&
      &deltaa         ,jugralg        )

      call GMCROS (ibr    ,ngrid  ,nbran  ,nboun  ,nnode  ,nfrac ,&
      &time   ,moropt ,nonngp ,nvast  ,zbeps ,&
      &grid   ,branch ,node   ,maxtab ,ntabm  ,ntab  ,&
      &table  ,mbdpar ,x      ,maxlev ,nlev   ,hlev  ,&
!                    <wf>
      &afwfqs(1,3)    ,wft    ,ws     ,flwdir ,deltaa,&
!                            <daacor>
      &deltar ,tmpfr(1,1)     ,zbave  ,zbfl   ,jugralg,&
      &sumda  )

100 continue
!
!     Remember wfsold in case of more sublayers
!
   do 200 i=1,ngrid
      wfsold(i) = afwfqs(i,3)
200 continue
!
!     Check for z in increasing order
!
   call MOCZIO ( nbran  ,ngrid  ,maxlev ,branch ,nlev   ,&
!                                  <h(n+1)>
   &hlev   ,typcr  ,h(1,3) ,&
   &juer   ,ker    ,prslot )
!
   if (ker .eq. fatal) goto 1000
!
   return
!
!     Error label
!
1000 continue
!
   write (txt,'(2(1x,i8))') itim
   call ERROR (juer,'GMORPH timestep@'//txt//'@',emomes,info)
!
end
