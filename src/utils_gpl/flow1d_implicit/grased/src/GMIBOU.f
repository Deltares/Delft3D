      subroutine GMIBOU ( ibr    ,igp    ,juer   ,time   ,dtm    ,
     +                    ngrid  ,nbran  ,nboun  ,nnode  ,nfrac  ,
     +                    maxtab ,ntabm  ,branch ,node   ,mbdpar ,
     +                    ntab   ,table  ,alphac ,alphad ,alphae ,
     +                    x      ,celer  ,celert ,sedtr  ,disgse ,
     +                    source ,dfrac  ,ds     ,spredc ,cela1  ,
     +                    flwdir ,intbou ,jugralg,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         S.L. van der Woude
c
c Module:             GMIBOU (Graded Morphology Integral BOUndary)
c
c Module description: Calculate time integral for a boundary of a
c                     branch.
c
c                     This routine processes the calculation of the time
c                     integral at the begin or end point of a branch.
c                     Here several situations are possible:
c
c                     a)  inflowing boundary with condition S = f(t);
c                     b)  inflowing boundary with condition S = f(Q);
c                     c)  inflowing boundary with condition z = f(t);
c                     d)  inflowing from node to branch;
c                     e)  outflowing from branch to node.
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
c                     d)  Integral is calculated by using the distribu-
c                         ted sediment transports; The sediment trans-
c                         ports are distributed each flow step in the
c                         sediment module. So the sum of the time inte-
c                         grals will be zero.
c                     e)  Integral is calculated by using the calculated
c                         sediment transports.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 19 alphac            I  Stability factor for bottom scheme (>1)
c  9 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 20 celer(ngrid,*)    I  Sediment celerity for each gridpoint.
c               1|2       - Normal branches:
c                         (i,1) = Celerity in gridpoint i in main secti-
c                                 on.
c                         - Sedredge branches:
c                         (i,1) = Celerity in gridpoint i,
c                                 left channel.
c                         (i,2) = Celerity in gridpoint i,
c                                 right channel.
c 22 disgse(4,nbran)   I  Redistributed sediment transport at begin and
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
c 18 dtm               I  Morphology time step
c  8 grid(ngrid)       I  Grid cell type definition:
c                         cgrdcl (1) : Normal grid cell
c                         cstrcl (2) : Structure cell
c  1 ibr               I  Branch number
c  2 igp               I  Gridpoint number
c 25 intbou            O  Integral value for begin or end point of a
c                         branch
c  3 isec              I  Section number (1 or 2)
c 23 juer              P  -
c 24 ker               O  Error code:
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
c 12 x(ngrid)          I  x(i) = X-coordinate of grid point i.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c inttab  INTerpolate in TABle
c=======================================================================
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gmibou.F,v $
c Revision 1.2  1995/09/27  10:11:33  kuipe_j
c Maintenance
c
c
c***********************************************************************
c
c     Parameters
c
      integer     ibr    ,igp    ,ngrid  ,nbran  ,nboun  ,nnode,
     +            nfrac  ,maxtab ,ntabm  ,juer   ,jugralg,ker
      integer     branch (4,nbran) ,
     +            node   (4,nnode) ,
     +            mbdpar (5,nboun) ,
     +            ntab   (4,maxtab),
     +            flwdir (ngrid)   
      real        alphac ,alphad   ,alphae
      real        intbou (nfrac,2)       ,table  (ntabm)        ,
     +            sedtr  (ngrid,nfrac+2) ,disgse (nfrac,2,nbran),
     +            source (ngrid,nfrac+2) ,celer  (ngrid,nfrac,5),
     +            celert (ngrid)         ,
     +            x      (ngrid)         ,cela1  (nfrac,nfrac)  ,
     +            ds     (nfrac)         ,spredc (nfrac)        ,
     +            dfrac  (nfrac)
      double precision    time   ,dtm
c
c     Local variables
c
      integer     indir,  ixdir ,inode, iboun, itab, itabtb ,jf  ,
     +            igpcel, lbrnam
      real        s1g1 , s1g2
      character*40       branam
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
      include '..\include\sobcon.i'
c
c     Determine inflowing or outflowing and connected node number
c
      if (branch(3,ibr) .eq. igp) then
c
c        Begin of branch: inflowing is positive
c
         indir = +1
         ixdir =  1
         inode = branch(1,ibr)
      else
c
c        End of branch  : inflowing is negative
c
         indir = -1
         ixdir =  2
         inode = branch(2,ibr)
      endif
c
c     Check for inflowing: S*dir >= 0
c
      if (indir * flwdir(igp) .ge. 0) then
c
c        Read boundary number
c
         iboun = node(4,inode)

         if (iboun .gt. 0) then
c
c           Inflowing boundary
c
c           S = f(t)
c
            if (mbdpar(1,iboun) .eq. cmbsft) then
c
c              User defined sediment table S=f(t).
c              First get table with table numbers
c
c              itabtb     : TABLE number of table with
c                            table numbers

               itabtb  = mbdpar(4,iboun)

               do 10 jf=1,nfrac

                  itab = int(table(ntab(3,itabtb) + jf - 1 ))

c                 itab       : TABLE number sedtra=f(t) for
c                              fraction jf
c
c                 Interpolate on time level time
c
                  call inttab (ntab (1,itab)      ,ntab(4,itab),
     &                         table(ntab(2,itab)),
     &                         table(ntab(3,itab)),
     &                         time               ,s1g1        )
c
c                 Interpolate on time level time+dtm
c
                  call inttab (ntab (1,itab)      ,ntab(4,itab),
     &                         table(ntab(2,itab)),
     &                         table(ntab(3,itab)),
     &                         time + dtm         ,s1g2        )
c
c                 Calculate time integral
c
                  intbou(jf,ixdir) = ( s1g1 + s1g2 ) * sngl(dtm) / 2.
c
   10          continue
c

            elseif (mbdpar(1,iboun) .eq. cmbsfq) then
c
c              S = f(Q), time integral is distributed sediment transport
c
               do 20 jf=1,nfrac
                  intbou(jf,ixdir) = disgse(jf,ixdir,ibr) * sngl(dtm)
   20          continue

            else
c
c              z = f(t), I will not be used, set to fictive value
c                  or
c              undefined boundary (should be outflowing but
c              a zero transport will be treated as inflowing)
c
               do 30 jf=1,nfrac
                  intbou(jf,ixdir) = sedtr(igp,jf) * sngl(dtm)
   30          continue

            endif
         else
            ker = fatal
            call getbrn (ibr,branam,lbrnam)
            call ERROR ( juer, 'GMIBOU @' //branam(:lbrnam)//'@',
     &                   emobou, ker )        
         endif
c
      else
c
c        Calculate sediment integral for outflowing boundary
c
         igpcel = igp+indir
c
         call gmiflp (igp    ,igpcel ,ngrid ,nfrac  ,alphac ,alphad ,
     +                alphae ,sngl(dtm)     ,celer  ,celert ,sedtr  ,
     +                source ,x      ,dfrac ,ds     ,spredc ,cela1  ,
     +                intbou(1,ixdir)       ,jugralg)

      endif

      end
