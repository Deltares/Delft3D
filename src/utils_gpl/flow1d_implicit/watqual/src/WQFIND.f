      subroutine wqfind ( ngrid  ,nbran  ,nbrnod ,branch ,
     +                    brnode ,qaggr  ,inode  ,igpfrm ,
     +                    igpto  ,isecfr ,isecto ,qex    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         S.L. van der Woude
c
c Module:             WQFIND (Water Quality Flow In a NoDe)
c
c Module description: This routine calculates the exchange flows between
c                     two segments in a node.
c
c                     First the sum of all leaving flows in the node is
c                     calculated. After this the flows for the two defi-
c                     ned gridpoints which have an exchange are calcula-
c                     ted followed by a calculation of the exchange
c                     flow.
c
c                     The "from" flow should be entering the node. This
c                     is true if the gridpoint is located at a branch
c                     begin and the flow is less then 0 or the gridpoint
c                     is located at a branch end and the flow is greater
c                     then 0. In all other cases the from flow will be
c                     zero.
c
c                     The "to" flow should be leaving the node. This is
c                     true if the gridpoint is located at a branch begin
c                     and the flow is greater then 0 or the gridpoint is
c                     located at a branch end and the flow is less then
c                     0. In other cases the to flow will be zero.
c
c                     The resulting sign of Qex is shown in the follo-
c                     wing table:
c
c                     location of from gp  sign of from Q  sign Qex
c                     -------------------  --------------  --------
c                     branch begin            1            -1
c                     branch begin           -1             1
c                     branch end              1             1
c                     branch end             -1            -1
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  4 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  5 brnode(nbrnod+1,  I  Node-Branch relation table. The first index
c        ,nnode)          contains the number of connected branches
c                         (index 1) for each node. The second index
c                         contains the first connected branch number
c                         etc.
c  8 igpfrm            I  Gridpoint from.
c  9 igpto             I  Gridpoint to.
c  7 inode             I  Node number.
c 10 isecfr            P  -
c 11 isecto            P  -
c  2 nbran             I  Number of branches.
c  3 nbrnod            I  Maximum number of connected branches to one
c                         node.
c  1 ngrid             I  Number of grid points in network.
c  6 qaggr             P  -
c 12 qex               IO Calculated exchange flow.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c wqfdir  Water Quality Flow DIRection
c wqgpfl  Water Quality GridPoint FLow
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: wqfind.pf,v $
c Revision 1.3  1999/03/15  15:53:57  kuipe_j
c tabs removed
c
c Revision 1.2  1995/05/30  07:08:30  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:52  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:27  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c       Parameters
c
      integer igpfrm,
     +        igpto,
     +        inode,
     +        isecfr,
     +        isecto,
     +        ngrid,
     +        nbran,
     +        nbrnod

      integer branch(4,nbran),
     +        brnode(nbrnod+1)

      real    qex

      real    qaggr(ngrid,3)

c
c       Variables
c
      integer abra  , i    , ibr   , igp  , isecwq
      integer frdir , todir, qfdir , qtdir
      real    qleave, q    , q1    , q2   , qsign

c
c     Initialise from dir and to dir
c
      frdir = 0
      todir = 0

c
c     Calculate sum of leaving flows in node number node. Add for
c     all gridpoints in the node. Also determine the locations of
c     the "from" and "to" gridpoints (begin or end of branch)
c
      qleave = 0
      abra   = brnode (1)

      do 100 i = 2, abra + 1
         ibr = brnode (i)
c
c        Branch starts at node
c
         if (branch (1,ibr) .eq. inode) then
            igp  = branch (3,ibr)

c
c           Check if igpfrm is located at branch begin
c
            if (igpfrm .eq. igp) then
               frdir = 1
            endif

c
c           Check if igpto is located at branch begin
c
            if (igpto .eq. igp) then
               todir = 1
            endif

            isecwq = 0
            call wqgpfl ( ngrid ,qaggr ,igp   ,isecwq  ,q    )

            if ( q .gt. 0) then
               qleave = qleave + q
            endif
c
c        Branch ends at node
c
         else
            igp  = branch (4,ibr)

c
c           Check if igpfrm is located at branch end
c
            if (igpfrm .eq. igp) then
               frdir = 2
            endif

c
c           Check if igpto is located at branch end
c
            if (igpto .eq. igp) then
               todir = 2
            endif

            isecwq = 0
            call wqgpfl ( ngrid ,qaggr ,igp   ,isecwq  ,q    )

            if (q .lt. 0) then
               qleave = qleave + abs (q)
            endif
         endif
 100  continue

c
c     Read flow values for "from" and "to" grid points
c
      call wqgpfl ( ngrid  ,qaggr  ,igpfrm ,isecfr ,q1  )
      call wqgpfl ( ngrid  ,qaggr  ,igpto  ,isecto ,q2  )
c
c     Determine entering or leaving flow (1 = Entering, 2 = Leaving)
c
      call wqfdir ( frdir, q1, qfdir )
      call wqfdir ( todir, q2, qtdir )

c
c     One of the flow should be entering, the other leaving
c
      if (((qfdir .eq. 1) .and. (qtdir .eq. 2)) .or.
     +    ((qfdir .eq. 2) .and. (qtdir .eq. 1)))
     +then

c
c        Exchange flow is Q1 * Q2 / Sum Q_leaving
c
         if (abs(qleave) .lt. 1.0E-9) then
            qex = 0
         else
            qex = (abs (q1) * abs (q2)) / abs(qleave)
         endif

c
c        Determine sign of exchange flow
c
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

c
c        Set exchange sign
c
         qex = qsign * qex

      else
c
c        Set exchange flow to zero
c
         qex = 0
      endif

      return
      end
