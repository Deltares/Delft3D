      subroutine MORPH  ( ngrid  ,nbran  ,nboun  ,nnode  ,
     +                    nbrnod ,ntmpgr ,time   ,dtm    ,
     +                    morpar ,flwdir ,sumda  ,
     +                    grid   ,branch ,node   ,typcr  ,
     +                    brnode ,bgout  ,tmpgr  ,
     +                    maxtab ,ntabm  ,ntab   ,table  ,
     +                    mbdpar ,x      ,
     +                    maxlev ,nlev   ,hlev   ,h2     ,
     +                    waoft  ,wft    ,ws     ,
     +                    sectc  ,afwfqs ,
     +                    celer  ,sedtr  ,dissed ,slat   ,
     +                    itim   ,juer   ,ker    ,prslot )

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
c 33 afwfqs            P  -
c 16 bgout             P  -
c 12 branch            P  -
c 15 brnode            P  -
c 34 celer             P  -
c 36 dissed            P  -
c  8 dtm               P  -
c 10 flwdir            P  -
c 11 grid              P  -
c 27 h                 P  -
c 26 hlev              P  -
c 38 itim(2)           I  Actual time level tn+1 expressed in date and
c                         time. Format (integer):
c                         itim(1) = YYYYMMDD (year,month,day)
c                         itim(2) = HHMMSSHH (hour,minute,second,
c                                   hundredth of a second)
c 39 juer              P  -
c 40 ker               IO Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 24 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c 18 maxtab            I  Maximum number of defined tables.
c 22 mbdpar            P  -
c  9 morpar(20)        I  (1) = Reduction parameter for actual tr.width
c                         (2) = Maximum number of iteration steps allowed
c                         (3) = Stability factor (>1)
c                         (4) = Method of adapting cross sections
c                               ceqows (1) : Equally over the actual
c                                            sediment tr. width of the
c                                            cross section
c                               cprodp (2) : Proportional to the local
c                                            water depth across the cross
c                                            section
c                          (5) = Limiter constant
c                          (6) = Switch used in formulas (mopta)
c                          (7) = Switch used in formulas (moptb)
c                          (8) = Switch used in formulas (moptc)
c                          (9) = Switch used in formulas (moptd)
c                          (10)= Switch used in formulas (mopte)
c                          (11)= Switch used in formulas (moptf)
c  3 nboun             I  Number of boundary nodes.
c  2 nbran             I  Number of branches.
c  5 nbrnod            I  Maximum number of connected branches to one
c                         node.
c  1 ngrid             I  Number of grid points in network.
c 25 nlev              P  -
c  4 nnode             I  Number of nodes.
c 13 node              P  -
c 20 ntab              P  -
c 19 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c  6 ntmpgr            I  Number of scratch arrays with length ngrid
c                         that are packed in tmpgr.
c 41 prslot            P  -
c 32 sectc             P  -
c 35 sedtr             P  -
c 37 slat              P  -
c 21 table             P  -
c  7 time              P  -
c 17 tmpgr             P  -
c 14 typcr(nbran)      I  Type of cross section used in every branch:
c                         ccrtab (1) : Tabulated cross sections
c                         ccrcir (2) : Circle as cross section
c                         ccrsed (3) : Sedredge cross sections
c 28 waoft             P  -
c 29 wft               P  -
c 31 ws                P  -
c 30 wtt               P  -
c 23 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c epsequ  EQUal test with interval EPSilon
c error   write an ERROR to the error file.
c moarea  MOrphology AREA
c moczio  MOrphology Check Z Increasing Order
c mointn  MOrphology INTegrals for Nodes
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: morph.pf,v $
c Revision 1.11  1999/03/15  15:53:03  kuipe_j
c tabs removed
c
c Revision 1.10  1998/06/11  11:47:20  kuipe_j
c Estuary special integrated
c
c Revision 1.9  1997/06/17  11:26:59  kuipe_j
c output in history format
c
c Revision 1.8  1997/02/17  10:23:17  kuipe_j
c Lateral Q in m3/s in cont. equation now
c
c Revision 1.7  1997/01/23  08:29:50  kuipe_j
c Make flow module robust
c
c Revision 1.6  1996/03/08  09:39:15  kuipe_j
c Headers + moptf temporarily = false
c
c Revision 1.5  1996/03/07  10:44:24  kuipe_j
c Bottom scema acc. to Lax Wendroff with flux limitter
c
c Revision 1.4  1995/10/18  09:00:04  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.3  1995/05/30  09:55:56  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:04:59  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:25  hoeks_a
c Initial check-in
c
c Revision 1.4  1994/12/02  13:03:09  kuipe_j
c Wrong water level was used for checking for negative depth.
c Error handling improved.
c
c Revision 1.3  1994/11/28  08:52:41  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:32:58  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:09  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c
c     Parameters
c
      integer   ngrid  ,nbran  ,nboun  ,nnode  ,
     +          nbrnod ,ntmpgr ,maxtab ,
     +          ntabm  ,maxlev ,juer   ,ker

      integer   grid   (ngrid),
     +          branch (4,nbran),
     +          flwdir (ngrid),
     +          node   (4,nnode),
     +          brnode (nbrnod+1,nnode),
     +          bgout  (3,nbrnod),
     +          typcr  (nbran),
     +          ntab   (4,maxtab),
     +          mbdpar (5,nboun),
     +          nlev   (ngrid),
     +          itim   (2)

      real      morpar (20),
     +          table  (ntabm),
     +          x      (ngrid),
     +          waoft  (ngrid,6),
     +          wft    (ngrid,maxlev),
     +          ws     (ngrid),
     +          sectc  (ngrid,3),
     +          afwfqs (ngrid,8),
     +          celer  (ngrid,*),
     +          sedtr  (ngrid,*),
     +          dissed (4,nbran),
     +          slat   (ngrid,*),
     +          tmpgr  (ngrid,ntmpgr),
     +          prslot (3,nbran),sumda (ngrid)

      double    precision  time ,dtm, hlev (ngrid,maxlev)
      double    precision  h2(ngrid)

c
c     Local variables
c
      integer ibr, isec,moropt
c
c
      real    alphac,alphad

      logical mopta, moptb, moptc, moptd, mopte, moptf
      logical epsequ

      character*18 txt
      external epsequ
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
      include '..\include\sobcon.i'
c
      alphac = morpar(3)
      moropt = INT(morpar(4))
      alphad = morpar(5)
      mopta  = (INT(morpar(6)) .eq. 1)
      moptb  = (INT(morpar(7)) .eq. 1)
      moptc  = (INT(morpar(8)) .eq. 1)
      moptd  = (INT(morpar(9)) .eq. 1)
      mopte  = (INT(morpar(10)) .eq. 1)
c
c     Totdat het UI is aangepast op langer array morpar (20 ipv 10)
c     moet moptf hard op false gezet worden.
c
c      moptf  = (INT(morpar(11)) .eq. 1)
      moptf = .FALSE.
c
c     set default alphad =1
c
      if (epsequ(alphad ,0. ,cdchk)) then
         alphad = 1.
      endif
c
c     Calculate integral values for nodes and boundaries
c
      call MOINTN (nnode  ,nbran  ,nbrnod ,ngrid  ,nboun ,maxtab ,
     +             ntabm  ,time   ,dtm    ,alphac ,branch,brnode ,
     +             typcr  ,bgout  ,grid   ,node   ,mbdpar,ntab   ,
c                         <intgr>
     +             dissed ,tmpgr  ,x      ,table  ,celer ,sedtr  ,
     +             mopta  ,moptb  ,moptc  ,moptd  ,alphad,flwdir ,
     +             juer   ,ker    )

c
c     Check for error
c
      if (ker .eq. fatal) goto 1000
c
c     Do for each branch in network
c
      do 200 ibr = 1, nbran

         if (typcr(ibr) .eq. ccrtab) then
c
c           Process continuity equation for whole channel
c
            isec = 1
      call MOAREA ( isec   ,ngrid  ,nbran  ,
     +              nboun  ,nnode  ,maxlev ,branch ,ibr    ,
     +              node   ,typcr(ibr) ,mbdpar ,hlev ,nlev ,
     +              grid   ,maxtab ,ntabm  ,ntab   ,
c                          <h_n+1> <wf>        <wfh0>
     +              table  ,h2     ,waoft(1,1),sectc(1,2)  ,
c                                  <A-main>
     +              ws     ,wft    ,afwfqs(1,1),dissed ,
     +              x      ,time   ,dtm    ,alphac ,
c                                  <intgr>
     +              celer  ,sedtr  ,tmpgr  ,flwdir ,sumda  ,
     +              alphad ,mopta  ,moptb  ,moptc  ,moptd  ,
     +              mopte  ,moptf  ,slat   ,moropt ,
     +              juer   ,ker
     +            )

         elseif (typcr(ibr) .eq. ccrsed) then
c
c           Process continuity equation for left and right channel
c
            do 100 isec = 1, 2
          call MOAREA ( isec   ,ngrid  ,nbran  ,
     +                  nboun  ,nnode  ,maxlev ,branch ,ibr    ,
     +                  node   ,typcr(ibr) ,mbdpar ,hlev,nlev ,
     +                  grid   ,maxtab ,ntabm  ,ntab   ,
c                              <h_n+1> <wf>        <wfh0>
     +                  table  ,h2     ,waoft(1,1),sectc(1,2)  ,
c                                       <A-main>
     +                  ws     ,wft    ,afwfqs(1,1),dissed ,
     +                  x      ,time   ,dtm    ,alphac ,
c                                      <intgr>
     +                  celer  ,sedtr  ,tmpgr  ,flwdir ,sumda  ,
     +                  alphad ,mopta  ,moptb  ,moptc  ,moptd  ,
     +                  mopte  ,moptf  ,slat   ,moropt ,
     +                  juer   ,ker
     +             )

 100        continue
         endif
 200  continue
c
c     Check for z in increasing order
c
      call MOCZIO ( nbran  ,ngrid  ,maxlev ,branch ,nlev   ,
c                                  <h_n+1>
     +              hlev   ,typcr  ,h2     ,
     +              juer   ,ker    ,prslot )
c
c     Error label
c
 1000 continue
c
      if (ker .ne. ok) then
         write (txt,'(2(1x,i8))') itim
         call ERROR (juer,'MORP timestep@'//txt//'@',emomes,info)
         if (ker .ne. fatal) ker = ok
      endif
c
      end
