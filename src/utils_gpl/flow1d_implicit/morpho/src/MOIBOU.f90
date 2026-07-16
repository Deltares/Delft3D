subroutine MOIBOU ( ibr    ,igp    ,isec   ,&
&ngrid  ,nbran  ,nboun  ,nnode  ,&
&grid   ,branch ,node   ,&
&mbdpar ,x      ,&
&maxtab ,ntabm  ,ntab   ,table  ,&
&time   ,dtm    ,alphac ,&
&celer  ,sedtr  ,dissed ,&
&mopta  ,moptb  ,moptc  ,moptd  ,&
&alphad ,flwdir ,&
&juer   ,ker    ,intbou&
&)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             MOIBOU (MOrphology Integral BOUndary)
!
! Module description: Calculate time integral for a boundary of a
!                     branch.
!
!                     This routine processes the calculation of the time
!                     integral at a boundary point.
!                     Here several situations are possible:
!
!                     a)  inflowing boundary with condition S = f(t);
!                     b)  inflowing boundary with condition S = f(Q);
!                     c)  inflowing boundary with condition z = f(t);
!                     d)  outflowing from branch to node.
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
!                     d)  Integral is calculated by using the calculated
!                         sediment transports.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 19 alphac            P  -
! 27 alphad            P  -
!  9 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 20 celer             P  -
! 22 dissed(4,nbran)   I  Redistributed sediment transport at begin and
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
! 18 dtm               I  Morphology time step.
! 28 flwdir(ngrid)     I  Indicator for flow direction at each grid
!                         point      1 = positive flow
!                                    0 = zero flow
!                                   -1 = negative flow
!  8 grid              P  -
!  1 ibr               I  Branch number
!  2 igp               I  Gridpoint number
! 31 intbou            O  Integral value for begin or end point of a
!                         branch
!  3 isec              I  Section number (1 or 2)
! 29 juer              P  -
! 30 ker               O  Error code:
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
! 23 mopta             P  -
! 24 moptb             P  -
! 25 moptc             P  -
! 26 moptd             P  -
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
! 12 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! inttab  INTerpolate in TABle
! moitno  MOrphology InTegral at a NOde
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: moibou.pf,v $
! Revision 1.7  1999/06/01  13:42:32  kuipe_j
! names in messages substituted + message template
!
! Revision 1.6  1996/03/08  09:39:05  kuipe_j
! Headers + moptf temporarily = false
!
! Revision 1.5  1996/03/07  10:44:14  kuipe_j
! Bottom scema acc. to Lax Wendroff with flux limitter
!
! Revision 1.4  1995/10/18  08:59:58  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.3  1995/05/30  09:55:51  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:04:46  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:14  hoeks_a
! Initial check-in
!
! Revision 1.5  1994/12/14  08:09:32  kuipe_j
! Release V0_06
!
! Revision 1.4  1994/12/05  13:49:26  kuipe_j
! Release 0.06
!
! Revision 1.3  1994/11/28  08:52:32  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:32:41  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:07  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer     ibr    ,igp    ,isec   ,ngrid  ,nbran  ,nboun  ,nnode,&
   &maxtab ,ntabm  ,juer   ,ker

   integer     grid   (ngrid)  ,&
   &flwdir (ngrid)  ,&
   &branch (4,nbran),&
   &node   (4,nnode),&
   &mbdpar (5,nboun),&
   &ntab   (4,maxtab)

   real        alphac ,intbou ,alphad

   real        x      (ngrid),&
   &table  (ntabm),&
   &celer  (ngrid,*),&
   &sedtr  (ngrid,*),&
   &dissed (4,nbran)

   double      precision  time ,dtm

   logical     mopta, moptb ,moptc ,moptd
!
!     Local variables
!
   integer     indir, ixdis, inode, iboun, itab, lbrnam
   real        s1g1, s1g2, dtms
   character*40      branam
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
   include '..\include\sobcon.i'
!
!     Determine inflowing or outflowing and connected node number
!
   dtms = sngl(dtm)
   if (branch(3,ibr) .eq. igp) then
!
!        Begin of branch: inflowing is positive
!
      indir = +1
      ixdis = isec
      inode = branch(1,ibr)
   else
!
!        End of branch  : inflowing is negative
!
      indir = -1
      ixdis = isec+2
      inode = branch(2,ibr)
   endif
!
!     Check for inflowing:
!
   if (indir * flwdir(igp) .ge. 0) then
!
!        Test made consistent with test in MOINOD
!        This makes a difference with estuary morphologie, as
!        transports and distributed transports are integrated.
!
!        Read boundary number
!
      iboun = node(4,inode)

      if (iboun .gt. 0) then
!
!           Inflowing boundary
!
         if     (mbdpar(1,iboun) .eq. cmbsft) then
!
!              S = f(t), fetch table number (index 4 or 5)
!
            itab = mbdpar(3+isec,iboun)
!
!              Interpolate on time level time
!
            call inttab ( ntab(1,itab),&
            &ntab(4,itab),&
            &table(ntab(2,itab)),&
            &table(ntab(3,itab)),&
            &time,&
            &s1g1&
            &)
!
!              Interpolate on time level time+dtm
!
            call inttab ( ntab(1,itab),&
            &ntab(4,itab),&
            &table(ntab(2,itab)),&
            &table(ntab(3,itab)),&
            &time+dtm,&
            &s1g2&
            &)
!
!              Calculate time integral
!
            intbou = ( s1g1 + s1g2 ) * dtms / 2.

         elseif (mbdpar(1,iboun) .eq. cmbsfq) then
!
!              S = f(Q), time integral is distributed sediment transport
!
            itab = mbdpar(3+isec,iboun)
            intbou = dissed(ixdis,ibr) * dtms

         elseif (mbdpar(1,iboun) .eq. cmbzft) then
!
!              z = f(t), I will not be used, set to fictive value
!
            intbou = sedtr(igp,isec) * dtms

         endif
      else
         ker = fatal
         call getbrn (ibr,branam,lbrnam)
         call ERROR ( juer, 'MOIBOU @' //branam(:lbrnam)//'@',&
         &emobou, ker )
      endif
!
   else
!
!        Outflowing
!
      call MOITNO ( igp    ,isec   ,ngrid  ,x      ,&
      &dtm    ,alphac ,inode  ,branch ,&
      &ibr    ,nbran  ,grid   ,&
      &mopta  ,moptb  ,moptc  ,moptd  ,&
      &celer  ,sedtr  ,alphad ,flwdir ,&
      &juer   ,ker    ,intbou&
      &)
   endif

   return
end
