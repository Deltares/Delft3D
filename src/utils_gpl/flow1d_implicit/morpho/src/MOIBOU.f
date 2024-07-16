      subroutine MOIBOU ( ibr    ,igp    ,isec   ,
     +                    ngrid  ,nbran  ,nboun  ,nnode  ,
     +                    grid   ,branch ,node   ,
     +                    mbdpar ,x      ,
     +                    maxtab ,ntabm  ,ntab   ,table  ,
     +                    time   ,dtm    ,alphac ,
     +                    celer  ,sedtr  ,dissed ,
     +                    mopta  ,moptb  ,moptc  ,moptd  ,
     +                    alphad ,flwdir ,
     +                    juer   ,ker    ,intbou
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
c Module:             MOIBOU (MOrphology Integral BOUndary)
c
c Module description: Calculate time integral for a boundary of a
c                     branch.
c
c                     This routine processes the calculation of the time
c                     integral at a boundary point.
c                     Here several situations are possible:
c
c                     a)  inflowing boundary with condition S = f(t);
c                     b)  inflowing boundary with condition S = f(Q);
c                     c)  inflowing boundary with condition z = f(t);
c                     d)  outflowing from branch to node.
c
c                     These situations are processed as follows:
c
c                     a)  Integral is calculated by using the boundary
c                         condition for the river case.
c                     b)  Integral is calculated using the avaraged
c                         sediment transports calculated in the sediment
c                         module;
c                     c)  The Integral is not calculated, the boundary
c                         condition will be used to adapt the cross
c                         section; However the integral value is someti-
c                         mes needed for calculation of the halve point
c                         (i=1.5 or n-.5). Therefore the integral is
c                         estimated with the calculated sediment trans-
c                         ports from the sediment module.
c                     d)  Integral is calculated by using the calculated
c                         sediment transports.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 19 alphac            P  -
c 27 alphad            P  -
c  9 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 20 celer             P  -
c 22 dissed(4,nbran)   I  Redistributed sediment transport at begin and
c                         end of branches. At the outflow side of the
c                         branch the calculated transports are stored.
c                         At the inflow side the redistributed trans-
c                         ports are stored.
c                         (1,i)   Transport at section 1 (main or left
c                                 channel)  at begin of branch.
c                         (2,i)   Transport at section 2 (right channel)
c                                 at begin of branch.
c                         (3,i)   Transport at section 1 (main or left
c                                 channel)  at end of branch.
c                         (4,i)   Transport at section 2 (right channel)
c                                 at end of branch.
c 18 dtm               I  Morphology time step.
c 28 flwdir(ngrid)     I  Indicator for flow direction at each grid
c                         point      1 = positive flow
c                                    0 = zero flow
c                                   -1 = negative flow
c  8 grid              P  -
c  1 ibr               I  Branch number
c  2 igp               I  Gridpoint number
c 31 intbou            O  Integral value for begin or end point of a
c                         branch
c  3 isec              I  Section number (1 or 2)
c 29 juer              P  -
c 30 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 13 maxtab            I  Maximum number of defined tables.
c 11 mbdpar(5,nboun)   I  Morphodynamic boundary conditions:
c                         (1,i) = Type of boundary condition:
c                                 cmbsft (1) : Condition S=f(t).
c                                 cmbsfq (2) : Condition S=f(Q).
c                                 cmbzft (3) : Condition z=f(t).
c                         (2,i) = Location (node number).
c                         (3,i) = Branch number that is connected.
c                         (4,i) = Table pointer for boundary table. In
c                                 case of a connected sedredge branch
c                                 the pointer will be assigned to the
c                                 left channel.
c                         (5,i) = Table pointer for boundary table. In
c                                 case of a connected sedredge branch
c                                 the pointer will be assigned to the
c                                 right channel. In other cases undefi-
c                                 ned.
c 23 mopta             P  -
c 24 moptb             P  -
c 25 moptc             P  -
c 26 moptd             P  -
c  6 nboun             I  Number of boundary nodes.
c  5 nbran             I  Number of branches.
c  4 ngrid             I  Number of grid points in network.
c  7 nnode             I  Number of nodes.
c 10 node(4,nnode)     I  Definition of nodes:
c                         (1,i) = Type of node i:
c                                 cintnd (1) : Internal node
c                                 chbou  (2) : H-boundary
c                                 cqbou  (3) : Q-boundary
c                                 cqhbou (4) : QH-boundary
c                                 chqbou (5) : HQ-boundary
c                         (2,i) = Gridpoint in case of boundary, else
c                                 undefined.
c                         (3,i) = Station number for boundary, undefined
c                                 for internal nodes:
c                                 HQ, H-boundary: station nr H-station.
c                                 QH, Q-boundary: station nr Q-station.
c                         (4,i) = Boundary number in case of boundary.
c 15 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
c                         to maxtab. For a specific table number k and
c                         function Y = f (X) the following definitions
c                         exist:
c                         (1,k) = Length of table k.
c                         (2,k) = Start address X in table.
c                         (3,k) = Start address Y in table.
c                         (4,k) = Access method and period control: xy
c                                 x = ctbnpf (0) : No period defined
c                                 x = ctbpfu (1) : Period defined
c                                 y = ctbico (0) : Continue interpltn
c                                 y = ctbidi (1) : Discrete interpltn
c 14 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 21 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
c               1|2       (At first transports per unit width, finaly
c                         total transports)
c                         - Normal branches:
c                         (i,1) = Sediment transport in gridpoint i of
c                                 main section.
c                         - Sedredge branches:
c                         (i,1) = Sediment transport in gridpoint i,
c                                 left channel.
c                         (i,2) = Sediment transport in gridpoint i,
c                                 right channel.
c                         (i,3) = Sediment exchange in gridpoint i.
c 16 table             P  -
c 17 time              I  Actual time level tn+1. in sec.
c 12 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c inttab  INTerpolate in TABle
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
c $Log: moibou.pf,v $
c Revision 1.7  1999/06/01  13:42:32  kuipe_j
c names in messages substituted + message template
c
c Revision 1.6  1996/03/08  09:39:05  kuipe_j
c Headers + moptf temporarily = false
c
c Revision 1.5  1996/03/07  10:44:14  kuipe_j
c Bottom scema acc. to Lax Wendroff with flux limitter
c
c Revision 1.4  1995/10/18  08:59:58  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.3  1995/05/30  09:55:51  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:04:46  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:14  hoeks_a
c Initial check-in
c
c Revision 1.5  1994/12/14  08:09:32  kuipe_j
c Release V0_06
c
c Revision 1.4  1994/12/05  13:49:26  kuipe_j
c Release 0.06
c
c Revision 1.3  1994/11/28  08:52:32  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:32:41  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:07  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      integer     ibr    ,igp    ,isec   ,ngrid  ,nbran  ,nboun  ,nnode,
     +            maxtab ,ntabm  ,juer   ,ker

      integer     grid   (ngrid)  ,
     +            flwdir (ngrid)  ,
     +            branch (4,nbran),
     +            node   (4,nnode),
     +            mbdpar (5,nboun),
     +            ntab   (4,maxtab)

      real        alphac ,intbou ,alphad

      real        x      (ngrid),
     +            table  (ntabm),
     +            celer  (ngrid,*),
     +            sedtr  (ngrid,*),
     +            dissed (4,nbran)

      double      precision  time ,dtm

      logical     mopta, moptb ,moptc ,moptd
c
c     Local variables
c
      integer     indir, ixdis, inode, iboun, itab, lbrnam
      real        s1g1, s1g2, dtms
      character*40      branam
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
      include '..\include\sobcon.i'
c
c     Determine inflowing or outflowing and connected node number
c
      dtms = sngl(dtm)
      if (branch(3,ibr) .eq. igp) then
c
c        Begin of branch: inflowing is positive
c
         indir = +1
         ixdis = isec
         inode = branch(1,ibr)
      else
c
c        End of branch  : inflowing is negative
c
         indir = -1
         ixdis = isec+2
         inode = branch(2,ibr)
      endif
c
c     Check for inflowing:
c
         if (indir * flwdir(igp) .ge. 0) then
c
c        Test made consistent with test in MOINOD
c        This makes a difference with estuary morphologie, as
c        transports and distributed transports are integrated.
c
c        Read boundary number
c
         iboun = node(4,inode)

         if (iboun .gt. 0) then
c
c           Inflowing boundary
c
            if     (mbdpar(1,iboun) .eq. cmbsft) then
c
c              S = f(t), fetch table number (index 4 or 5)
c
               itab = mbdpar(3+isec,iboun)
c
c              Interpolate on time level time
c
               call inttab ( ntab(1,itab),
     +                       ntab(4,itab),
     +                       table(ntab(2,itab)),
     +                       table(ntab(3,itab)),
     +                       time,
     +                       s1g1
     +                     )
c
c              Interpolate on time level time+dtm
c
               call inttab ( ntab(1,itab),
     +                       ntab(4,itab),
     +                       table(ntab(2,itab)),
     +                       table(ntab(3,itab)),
     +                       time+dtm,
     +                       s1g2
     +                     )
c
c              Calculate time integral
c
               intbou = ( s1g1 + s1g2 ) * dtms / 2.

            elseif (mbdpar(1,iboun) .eq. cmbsfq) then
c
c              S = f(Q), time integral is distributed sediment transport
c
               itab = mbdpar(3+isec,iboun)
               intbou = dissed(ixdis,ibr) * dtms

            elseif (mbdpar(1,iboun) .eq. cmbzft) then
c
c              z = f(t), I will not be used, set to fictive value
c
               intbou = sedtr(igp,isec) * dtms

            endif
         else
            ker = fatal
            call getbrn (ibr,branam,lbrnam)
            call ERROR ( juer, 'MOIBOU @' //branam(:lbrnam)//'@',
     &                   emobou, ker )
         endif
c
      else
c
c        Outflowing
c
          call MOITNO ( igp    ,isec   ,ngrid  ,x      ,
     +                  dtm    ,alphac ,inode  ,branch ,
     +                  ibr    ,nbran  ,grid   ,
     +                  mopta  ,moptb  ,moptc  ,moptd  ,
     +                  celer  ,sedtr  ,alphad ,flwdir ,
     +                  juer   ,ker    ,intbou
     +                )
      endif

      return
      end
