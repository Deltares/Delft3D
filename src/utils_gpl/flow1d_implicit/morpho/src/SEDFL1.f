      subroutine sedfl1 (ibr    ,nbran  ,nnode  ,ngrid  ,maxtab ,ntabm ,
     &                   juer   ,time   ,branch ,node   ,sedinf ,sdrdbf,
     &                   ntab   ,table  ,q2     ,qs     ,linc   ,ker   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SEDFL1 (SEdiment Distribute FLow on upstream point 1)
c
c Module description: Distribute flow on first upstream grid point ac-
c                     cording to boundary condition or nodal function.
c
c                     At first the upstream side of the branch will be
c                     determined. Then a check is made on changes in
c                     flow direction.
c
c                     When the upstream gridpoint is located on a boun-
c                     dary the boundary condition for the right channel
c                     is read. In case the gridpoint is located in a
c                     node the discharges in both channels are calcula-
c                     ted using a distribution function.
c                     [ Doc. S-FO-002.2KV / Eq. 6.18 and 6.22 ]
c
c Precondition:       For the distribution functions:
c                     The arguments (discharges) are all positive or all
c                     negative.
c                     The Function value Alpha-w is positive.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  9 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  1 ibr               I  Number of actual branch.
c  7 juer              P  -
c 18 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 17 linc              O  Indicates the direction of looping through a
c                         branch:
c                         +1 : Positive
c                         -1 : Negative
c  5 maxtab            I  Maximum number of defined tables.
c  2 nbran             I  Number of branches.
c  4 ngrid             I  Number of grid points in network.
c  3 nnode             I  Number of nodes.
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
c 13 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
c  6 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 15 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
c 16 qs(ngrid,2)       IO Flow in every grid point per section:
c                         (i,1) = Through grid point i of main channel.
c                         (i,2) = Through grid point i of sub section 1.
c 12 sdrdbf(2,nsedrd)  I  Defines for each sedredge branch the pointer
c                         to the sediment distribution function and the
c                         flow distribution function.
c                         (1,j) = Table pointer Q distribution table for
c                                 sedredge branch in case the sedredge
c                                 branch starts at an internal node. If
c                                 the sedredge branch starts at a boun-
c                                 dary the table pointer points to a
c                                 table with boundary condition for the
c                                 right channel.
c                         (2,j) = Table pointer S distribution table for
c                                 sedredge branch j.
c 11 sedinf(2,nbran)   I  Defines for each branch whether a branch is a
c                         sedredge branch or not.
c                         (1,j) = Sedredge branch number (1..nsedrd)
c                                 else 0.
c                         (2,j) = Starting index in Rc array (River bend
c                                 curvature).
c 14 table             P  -
c  8 time              P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c inttab  INTerpolate in TABle
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sedfl1.pf,v $
c Revision 1.4  1999/06/01  13:42:41  kuipe_j
c names in messages substituted + message template
c
c Revision 1.3  1995/05/30  09:56:28  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:07:16  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:18  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  08:47:30  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:34:41  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:19  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer    nbran ,nnode ,maxtab,ntabm ,ngrid  ,ibr   ,
     &           juer  ,linc  ,ker
      integer    branch(4,nbran) ,node  (4,nnode) ,ntab(4,maxtab),
     &           sedinf(2,nbran) ,sdrdbf(2,*)
      real       table (ntabm)   ,qs  (ngrid,2)
      double     precision  time, q2(ngrid)
c
c     Declaration of local parameters
c
      integer           ig1   ,ig2   ,igr   ,ind   ,itab ,lbrnam
      real              alphaw
      logical           isnode
      character*40      branam
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
      include '..\include\sobcon.i'
c
c     Check if all discharges in the branch are either positive
c     or negative. Determine process direction.
c
      ig1 = branch(3,ibr)
      ig2 = branch(4,ibr)
c
      if (q2(ig1) .gt. 0.) then
         do 10 igr = ig1+1,ig2
            if (q2(igr) .le. 0.) goto 100
  10     continue
         ind = 1
      else if (q2(ig1) .lt. 0.) then
         do 20 igr = ig1+1,ig2
            if (q2(igr) .ge. 0.) goto 100
  20     continue
         ind = 2
      else
         goto 100
      endif
c
      linc   = 3-2*ind
      isnode = node(1,branch(ind,ibr)) .eq. cintnd
      itab   = sdrdbf(1,sedinf(1,ibr))
      igr    = branch(2+ind,ibr)
c
      if (isnode) then
c
c        Node: Interpolate in table and get alpha-w = f(Q2)
c
         call inttab (ntab (1,itab)      ,ntab(4,itab),
     &                table(ntab(2,itab)),
     &                table(ntab(3,itab)),
     &                dble(q2(igr))      ,alphaw    )
         qs(igr,1) = q2(igr) * alphaw
         qs(igr,2) = q2(igr) * (1.-alphaw)
      else
c
c        Boundary: Get boundary conditions for right channel.
c
         call inttab (ntab (1,itab)      ,ntab(4,itab),
     &                table(ntab(2,itab)),
     &                table(ntab(3,itab)),
     &                time    ,qs(igr,2) )
         qs(igr,1) = q2(igr) - qs(igr,2)
      endif
c
      return
c
 100  continue
c
c     Message in case of change in discharge direction.
c
      ker = fatal
      call getbrn (ibr,branam,lbrnam)
      call error (juer,'SEDFL1 @'//branam(:lbrnam)//'@',eseqdr,ker)
c
      end
