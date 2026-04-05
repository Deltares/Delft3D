subroutine wqfind ( ngrid  ,nbran  ,nbrnod ,branch ,&
&brnode ,qaggr  ,inode  ,igpfrm ,&
&igpto  ,isecfr ,isecto ,qex    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         S.L. van der Woude
!
! Module:             WQFIND (Water Quality Flow In a NoDe)
!
! Module description: This routine calculates the exchange flows between
!                     two segments in a node.
!
!                     First the sum of all leaving flows in the node is
!                     calculated. After this the flows for the two defi-
!                     ned gridpoints which have an exchange are calcula-
!                     ted followed by a calculation of the exchange
!                     flow.
!
!                     The "from" flow should be entering the node. This
!                     is true if the gridpoint is located at a branch
!                     begin and the flow is less then 0 or the gridpoint
!                     is located at a branch end and the flow is greater
!                     then 0. In all other cases the from flow will be
!                     zero.
!
!                     The "to" flow should be leaving the node. This is
!                     true if the gridpoint is located at a branch begin
!                     and the flow is greater then 0 or the gridpoint is
!                     located at a branch end and the flow is less then
!                     0. In other cases the to flow will be zero.
!
!                     The resulting sign of Qex is shown in the follo-
!                     wing table:
!
!                     location of from gp  sign of from Q  sign Qex
!                     -------------------  --------------  --------
!                     branch begin            1            -1
!                     branch begin           -1             1
!                     branch end              1             1
!                     branch end             -1            -1
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  4 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  5 brnode(nbrnod+1,  I  Node-Branch relation table. The first index
!        ,nnode)          contains the number of connected branches
!                         (index 1) for each node. The second index
!                         contains the first connected branch number
!                         etc.
!  8 igpfrm            I  Gridpoint from.
!  9 igpto             I  Gridpoint to.
!  7 inode             I  Node number.
! 10 isecfr            P  -
! 11 isecto            P  -
!  2 nbran             I  Number of branches.
!  3 nbrnod            I  Maximum number of connected branches to one
!                         node.
!  1 ngrid             I  Number of grid points in network.
!  6 qaggr             P  -
! 12 qex               IO Calculated exchange flow.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! wqfdir  Water Quality Flow DIRection
! wqgpfl  Water Quality GridPoint FLow
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: wqfind.pf,v $
! Revision 1.3  1999/03/15  15:53:57  kuipe_j
! tabs removed
!
! Revision 1.2  1995/05/30  07:08:30  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:52  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:27  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!       Parameters
!
   integer igpfrm,&
   &igpto,&
   &inode,&
   &isecfr,&
   &isecto,&
   &ngrid,&
   &nbran,&
   &nbrnod

   integer branch(4,nbran),&
   &brnode(nbrnod+1)

   real    qex

   real    qaggr(ngrid,3)

!
!       Variables
!
   integer abra  , i    , ibr   , igp  , isecwq
   integer frdir , todir, qfdir , qtdir
   real    qleave, q    , q1    , q2   , qsign

!
!     Initialise from dir and to dir
!
   frdir = 0
   todir = 0

!
!     Calculate sum of leaving flows in node number node. Add for
!     all gridpoints in the node. Also determine the locations of
!     the "from" and "to" gridpoints (begin or end of branch)
!
   qleave = 0
   abra   = brnode (1)

   do 100 i = 2, abra + 1
      ibr = brnode (i)
!
!        Branch starts at node
!
      if (branch (1,ibr) .eq. inode) then
         igp  = branch (3,ibr)

!
!           Check if igpfrm is located at branch begin
!
         if (igpfrm .eq. igp) then
            frdir = 1
         endif

!
!           Check if igpto is located at branch begin
!
         if (igpto .eq. igp) then
            todir = 1
         endif

         isecwq = 0
         call wqgpfl ( ngrid ,qaggr ,igp   ,isecwq  ,q    )

         if ( q .gt. 0) then
            qleave = qleave + q
         endif
!
!        Branch ends at node
!
      else
         igp  = branch (4,ibr)

!
!           Check if igpfrm is located at branch end
!
         if (igpfrm .eq. igp) then
            frdir = 2
         endif

!
!           Check if igpto is located at branch end
!
         if (igpto .eq. igp) then
            todir = 2
         endif

         isecwq = 0
         call wqgpfl ( ngrid ,qaggr ,igp   ,isecwq  ,q    )

         if (q .lt. 0) then
            qleave = qleave + abs (q)
         endif
      endif
100 continue

!
!     Read flow values for "from" and "to" grid points
!
   call wqgpfl ( ngrid  ,qaggr  ,igpfrm ,isecfr ,q1  )
   call wqgpfl ( ngrid  ,qaggr  ,igpto  ,isecto ,q2  )
!
!     Determine entering or leaving flow (1 = Entering, 2 = Leaving)
!
   call wqfdir ( frdir, q1, qfdir )
   call wqfdir ( todir, q2, qtdir )

!
!     One of the flow should be entering, the other leaving
!
   if (((qfdir .eq. 1) .and. (qtdir .eq. 2)) .or.&
   &((qfdir .eq. 2) .and. (qtdir .eq. 1)))&
   &then

!
!        Exchange flow is Q1 * Q2 / Sum Q_leaving
!
      if (abs(qleave) .lt. 1.0E-9) then
         qex = 0
      else
         qex = (abs (q1) * abs (q2)) / abs(qleave)
      endif

!
!        Determine sign of exchange flow
!
      if     (frdir .eq. 1) then
         if (q1 .gt. 0) then
            qsign = -1.0
         else
            qsign =  1.0
         endif
      elseif (frdir .eq. 2) then
         if (q1 .gt. 0) then
            qsign =  1.0
         else
            qsign = -1.0
         endif
      endif

!
!        Set exchange sign
!
      qex = qsign * qex

   else
!
!        Set exchange flow to zero
!
      qex = 0
   endif

   return
end
