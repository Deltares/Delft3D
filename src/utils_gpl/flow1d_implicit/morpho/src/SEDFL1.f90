subroutine sedfl1 (ibr    ,nbran  ,nnode  ,ngrid  ,maxtab ,ntabm ,&
&juer   ,time   ,branch ,node   ,sedinf ,sdrdbf,&
&ntab   ,table  ,q2     ,qs     ,linc   ,ker   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SEDFL1 (SEdiment Distribute FLow on upstream point 1)
!
! Module description: Distribute flow on first upstream grid point ac-
!                     cording to boundary condition or nodal function.
!
!                     At first the upstream side of the branch will be
!                     determined. Then a check is made on changes in
!                     flow direction.
!
!                     When the upstream gridpoint is located on a boun-
!                     dary the boundary condition for the right channel
!                     is read. In case the gridpoint is located in a
!                     node the discharges in both channels are calcula-
!                     ted using a distribution function.
!                     [ Doc. S-FO-002.2KV / Eq. 6.18 and 6.22 ]
!
! Precondition:       For the distribution functions:
!                     The arguments (discharges) are all positive or all
!                     negative.
!                     The Function value Alpha-w is positive.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  9 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  1 ibr               I  Number of actual branch.
!  7 juer              P  -
! 18 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 17 linc              O  Indicates the direction of looping through a
!                         branch:
!                         +1 : Positive
!                         -1 : Negative
!  5 maxtab            I  Maximum number of defined tables.
!  2 nbran             I  Number of branches.
!  4 ngrid             I  Number of grid points in network.
!  3 nnode             I  Number of nodes.
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
! 13 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
!  6 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 15 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
! 16 qs(ngrid,2)       IO Flow in every grid point per section:
!                         (i,1) = Through grid point i of main channel.
!                         (i,2) = Through grid point i of sub section 1.
! 12 sdrdbf(2,nsedrd)  I  Defines for each sedredge branch the pointer
!                         to the sediment distribution function and the
!                         flow distribution function.
!                         (1,j) = Table pointer Q distribution table for
!                                 sedredge branch in case the sedredge
!                                 branch starts at an internal node. If
!                                 the sedredge branch starts at a boun-
!                                 dary the table pointer points to a
!                                 table with boundary condition for the
!                                 right channel.
!                         (2,j) = Table pointer S distribution table for
!                                 sedredge branch j.
! 11 sedinf(2,nbran)   I  Defines for each branch whether a branch is a
!                         sedredge branch or not.
!                         (1,j) = Sedredge branch number (1..nsedrd)
!                                 else 0.
!                         (2,j) = Starting index in Rc array (River bend
!                                 curvature).
! 14 table             P  -
!  8 time              P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! inttab  INTerpolate in TABle
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sedfl1.pf,v $
! Revision 1.4  1999/06/01  13:42:41  kuipe_j
! names in messages substituted + message template
!
! Revision 1.3  1995/05/30  09:56:28  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:07:16  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:18  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:47:30  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:34:41  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:19  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    nbran ,nnode ,maxtab,ntabm ,ngrid  ,ibr   ,&
   &juer  ,linc  ,ker
   integer    branch(4,nbran) ,node  (4,nnode) ,ntab(4,maxtab),&
   &sedinf(2,nbran) ,sdrdbf(2,*)
   real       table (ntabm)   ,qs  (ngrid,2)
   double     precision  time, q2(ngrid)
!
!     Declaration of local parameters
!
   integer           ig1   ,ig2   ,igr   ,ind   ,itab ,lbrnam
   real              alphaw
   logical           isnode
   character*40      branam
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
   include '..\include\sobcon.i'
!
!     Check if all discharges in the branch are either positive
!     or negative. Determine process direction.
!
   ig1 = branch(3,ibr)
   ig2 = branch(4,ibr)
!
   if (q2(ig1) .gt. 0.) then
      do 10 igr = ig1+1,ig2
         if (q2(igr) .le. 0.) goto 100
10    continue
      ind = 1
   else if (q2(ig1) .lt. 0.) then
      do 20 igr = ig1+1,ig2
         if (q2(igr) .ge. 0.) goto 100
20    continue
      ind = 2
   else
      goto 100
   endif
!
   linc   = 3-2*ind
   isnode = node(1,branch(ind,ibr)) .eq. cintnd
   itab   = sdrdbf(1,sedinf(1,ibr))
   igr    = branch(2+ind,ibr)
!
   if (isnode) then
!
!        Node: Interpolate in table and get alpha-w = f(Q2)
!
      call inttab (ntab (1,itab)      ,ntab(4,itab),&
      &table(ntab(2,itab)),&
      &table(ntab(3,itab)),&
      &dble(q2(igr))      ,alphaw    )
      qs(igr,1) = q2(igr) * alphaw
      qs(igr,2) = q2(igr) * (1.-alphaw)
   else
!
!        Boundary: Get boundary conditions for right channel.
!
      call inttab (ntab (1,itab)      ,ntab(4,itab),&
      &table(ntab(2,itab)),&
      &table(ntab(3,itab)),&
      &time    ,qs(igr,2) )
      qs(igr,1) = q2(igr) - qs(igr,2)
   endif
!
   return
!
100 continue
!
!     Message in case of change in discharge direction.
!
   ker = fatal
   call getbrn (ibr,branam,lbrnam)
   call error (juer,'SEDFL1 @'//branam(:lbrnam)//'@',eseqdr,ker)
!
end
