subroutine GMIBOU ( ibr    ,igp    ,juer   ,time   ,dtm    ,&
&ngrid  ,nbran  ,nboun  ,nnode  ,nfrac  ,&
&maxtab ,ntabm  ,branch ,node   ,mbdpar ,&
&ntab   ,table  ,alphac ,alphad ,alphae ,&
&x      ,celer  ,celert ,sedtr  ,disgse ,&
&source ,dfrac  ,ds     ,spredc ,cela1  ,&
&flwdir ,intbou ,jugralg,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             GMIBOU (Graded Morphology Integral BOUndary)
!
! Module description: Calculate time integral for a boundary of a
!                     branch.
!
!                     This routine processes the calculation of the time
!                     integral at the begin or end point of a branch.
!                     Here several situations are possible:
!
!                     a)  inflowing boundary with condition S = f(t);
!                     b)  inflowing boundary with condition S = f(Q);
!                     c)  inflowing boundary with condition z = f(t);
!                     d)  inflowing from node to branch;
!                     e)  outflowing from branch to node.
!
!                     These situations are processed as follows:
!
!                     a)  Integral is calculated by using the boundary
!                         condition for the river case.
!                     b)  Integral is calculated using the avaraged
!                         sediment transports calculated in the sediment
!                         module;
!                     c)  The Integral is not calculated, the boundary
!                         condition will be used to adapt the cross
!                         section; However the integral value is someti-
!                         mes needed for calculation of the halve point
!                         (i=1.5 or n-.5). Therefore the integral is
!                         estimated with the calculated sediment trans-
!                         ports from the sediment module.
!                     d)  Integral is calculated by using the distribu-
!                         ted sediment transports; The sediment trans-
!                         ports are distributed each flow step in the
!                         sediment module. So the sum of the time inte-
!                         grals will be zero.
!                     e)  Integral is calculated by using the calculated
!                         sediment transports.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 19 alphac            I  Stability factor for bottom scheme (>1)
!  9 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 20 celer(ngrid,*)    I  Sediment celerity for each gridpoint.
!               1|2       - Normal branches:
!                         (i,1) = Celerity in gridpoint i in main secti-
!                                 on.
!                         - Sedredge branches:
!                         (i,1) = Celerity in gridpoint i,
!                                 left channel.
!                         (i,2) = Celerity in gridpoint i,
!                                 right channel.
! 22 disgse(4,nbran)   I  Redistributed sediment transport at begin and
!                         end of branches. At the outflow side of the
!                         branch the calculated transports are stored.
!                         At the inflow side the redistributed trans-
!                         ports are stored.
!                         (1,i)   Transport at section 1 (main or left
!                                 channel)  at begin of branch.
!                         (2,i)   Transport at section 2 (right channel)
!                                 at begin of branch.
!                         (3,i)   Transport at section 1 (main or left
!                                 channel)  at end of branch.
!                         (4,i)   Transport at section 2 (right channel)
!                                 at end of branch.
! 18 dtm               I  Morphology time step
!  8 grid(ngrid)       I  Grid cell type definition:
!                         cgrdcl (1) : Normal grid cell
!                         cstrcl (2) : Structure cell
!  1 ibr               I  Branch number
!  2 igp               I  Gridpoint number
! 25 intbou            O  Integral value for begin or end point of a
!                         branch
!  3 isec              I  Section number (1 or 2)
! 23 juer              P  -
! 24 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 13 maxtab            I  Maximum number of defined tables.
! 11 mbdpar(5,nboun)   I  Morphodynamic boundary conditions:
!                         (1,i) = Type of boundary condition:
!                                 cmbsft (1) : Condition S=f(t).
!                                 cmbsfq (2) : Condition S=f(Q).
!                                 cmbzft (3) : Condition z=f(t).
!                         (2,i) = Location (node number).
!                         (3,i) = Branch number that is connected.
!                         (4,i) = Table pointer for boundary table. In
!                                 case of a connected sedredge branch
!                                 the pointer will be assigned to the
!                                 left channel.
!                         (5,i) = Table pointer for boundary table. In
!                                 case of a connected sedredge branch
!                                 the pointer will be assigned to the
!                                 right channel. In other cases undefi-
!                                 ned.
!  6 nboun             I  Number of boundary nodes.
!  5 nbran             I  Number of branches.
!  4 ngrid             I  Number of grid points in network.
!  7 nnode             I  Number of nodes.
! 10 node(4,nnode)     I  Definition of nodes:
!                         (1,i) = Type of node i:
!                                 cintnd (1) : Internal node
!                                 chbou  (2) : H-boundary
!                                 cqbou  (3) : Q-boundary
!                                 cqhbou (4) : QH-boundary
!                                 chqbou (5) : HQ-boundary
!                         (2,i) = Gridpoint in case of boundary, else
!                                 undefined.
!                         (3,i) = Station number for boundary, undefined
!                                 for internal nodes:
!                                 HQ, H-boundary: station nr H-station.
!                                 QH, Q-boundary: station nr Q-station.
!                         (4,i) = Boundary number in case of boundary.
! 15 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
!                         to maxtab. For a specific table number k and
!                         function Y = f (X) the following definitions
!                         exist:
!                         (1,k) = Length of table k.
!                         (2,k) = Start address X in table.
!                         (3,k) = Start address Y in table.
!                         (4,k) = Access method and period control: xy
!                                 x = ctbnpf (0) : No period defined
!                                 x = ctbpfu (1) : Period defined
!                                 y = ctbico (0) : Continue interpltn
!                                 y = ctbidi (1) : Discrete interpltn
! 14 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 21 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
!               1|2       (At first transports per unit width, finaly
!                         total transports)
!                         - Normal branches:
!                         (i,1) = Sediment transport in gridpoint i of
!                                 main section.
!                         - Sedredge branches:
!                         (i,1) = Sediment transport in gridpoint i,
!                                 left channel.
!                         (i,2) = Sediment transport in gridpoint i,
!                                 right channel.
!                         (i,3) = Sediment exchange in gridpoint i.
! 16 table             P  -
! 17 time              I  Actual time level tn+1. in sec.
! 12 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! inttab  INTerpolate in TABle
!=======================================================================
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gmibou.F,v $
! Revision 1.2  1995/09/27  10:11:33  kuipe_j
! Maintenance
!
!
!***********************************************************************
!
!     Parameters
!
   integer     ibr    ,igp    ,ngrid  ,nbran  ,nboun  ,nnode,&
   &nfrac  ,maxtab ,ntabm  ,juer   ,jugralg,ker
   integer     branch (4,nbran) ,&
   &node   (4,nnode) ,&
   &mbdpar (5,nboun) ,&
   &ntab   (4,maxtab),&
   &flwdir (ngrid)
   real        alphac ,alphad   ,alphae
   real        intbou (nfrac,2)       ,table  (ntabm)        ,&
   &sedtr  (ngrid,nfrac+2) ,disgse (nfrac,2,nbran),&
   &source (ngrid,nfrac+2) ,celer  (ngrid,nfrac,5),&
   &celert (ngrid)         ,&
   &x      (ngrid)         ,cela1  (nfrac,nfrac)  ,&
   &ds     (nfrac)         ,spredc (nfrac)        ,&
   &dfrac  (nfrac)
   double precision    time   ,dtm
!
!     Local variables
!
   integer     indir,  ixdir ,inode, iboun, itab, itabtb ,jf  ,&
   &igpcel, lbrnam
   real        s1g1 , s1g2
   character*40       branam
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
   include '..\include\sobcon.i'
!
!     Determine inflowing or outflowing and connected node number
!
   if (branch(3,ibr) .eq. igp) then
!
!        Begin of branch: inflowing is positive
!
      indir = +1
      ixdir =  1
      inode = branch(1,ibr)
   else
!
!        End of branch  : inflowing is negative
!
      indir = -1
      ixdir =  2
      inode = branch(2,ibr)
   endif
!
!     Check for inflowing: S*dir >= 0
!
   if (indir * flwdir(igp) .ge. 0) then
!
!        Read boundary number
!
      iboun = node(4,inode)

      if (iboun .gt. 0) then
!
!           Inflowing boundary
!
!           S = f(t)
!
         if (mbdpar(1,iboun) .eq. cmbsft) then
!
!              User defined sediment table S=f(t).
!              First get table with table numbers
!
!              itabtb     : TABLE number of table with
!                            table numbers

            itabtb  = mbdpar(4,iboun)

            do 10 jf=1,nfrac

               itab = int(table(ntab(3,itabtb) + jf - 1 ))

!                 itab       : TABLE number sedtra=f(t) for
!                              fraction jf
!
!                 Interpolate on time level time
!
               call inttab (ntab (1,itab)      ,ntab(4,itab),&
               &table(ntab(2,itab)),&
               &table(ntab(3,itab)),&
               &time               ,s1g1        )
!
!                 Interpolate on time level time+dtm
!
               call inttab (ntab (1,itab)      ,ntab(4,itab),&
               &table(ntab(2,itab)),&
               &table(ntab(3,itab)),&
               &time + dtm         ,s1g2        )
!
!                 Calculate time integral
!
               intbou(jf,ixdir) = ( s1g1 + s1g2 ) * sngl(dtm) / 2.
!
10          continue
!

         elseif (mbdpar(1,iboun) .eq. cmbsfq) then
!
!              S = f(Q), time integral is distributed sediment transport
!
            do 20 jf=1,nfrac
               intbou(jf,ixdir) = disgse(jf,ixdir,ibr) * sngl(dtm)
20          continue

         else
!
!              z = f(t), I will not be used, set to fictive value
!                  or
!              undefined boundary (should be outflowing but
!              a zero transport will be treated as inflowing)
!
            do 30 jf=1,nfrac
               intbou(jf,ixdir) = sedtr(igp,jf) * sngl(dtm)
30          continue

         endif
      else
         ker = fatal
         call getbrn (ibr,branam,lbrnam)
         call ERROR ( juer, 'GMIBOU @' //branam(:lbrnam)//'@',&
         &emobou, ker )
      endif
!
   else
!
!        Calculate sediment integral for outflowing boundary
!
      igpcel = igp+indir
!
      call gmiflp (igp    ,igpcel ,ngrid ,nfrac  ,alphac ,alphad ,&
      &alphae ,sngl(dtm)     ,celer  ,celert ,sedtr  ,&
      &source ,x      ,dfrac ,ds     ,spredc ,cela1  ,&
      &intbou(1,ixdir)       ,jugralg)

   endif

end
