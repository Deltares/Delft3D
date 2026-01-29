subroutine wqexfl ( npntr  ,pntr   ,nexdef ,exdef  ,&
&ngrid  ,nbran  ,branch ,nbrnod ,&
&nnode  ,brnode ,qltpar ,x      ,&
&qaggr  ,qlaggr ,sexfl  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         S.L. van der Woude
!
! Module:             WQEXFL (Water Quality EXchange FLows)
!
! Module description: This routine calculates the exchange flows for one
!                     time step.
!
!                     The routine uses the pointer table and the exchan-
!                     ge definition table. The pointer table gives in-
!                     formation about segment exchanges in the form
!                     segment i -> segment j. Each exchange is defined
!                     by one or more exchange definitions. Four types
!                     are possible:
!
!                     -   Exchange in a gridpoint;
!                     -   Exchange between two gridpoints;
!                     -   Exchange in a node;
!                     -   Exchange from a lateral discharge station to a
!                         segment
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  7 branch            P  -
! 10 brnode            P  -
!  4 exdef(6,nexdef)   I  This table contains the elementary exchange
!                         definitions. Each exchange between segments
!                         can be described by one or more elementary
!                         definitions in this table.
!                         (1,i) = Exchange type:
!                                 cexigp (1) : Exchange in gridpoint.
!                                 cexbgp (2) : Exchange between gpoints.
!                                 cexind (3) : Exchange in a node.
!                                 cexqlt (4) : Exchange from Qlat to seg
!                         - Exchange in a gridpoint (Type 1):
!                         (2,i) = Gridpoint
!                         (3,i) = Section from
!                         (4,i) = Section to
!                         (5,i) = Direction of "from" --> "to":
!                                 cpopnt (+1) : pos branch direction.
!                                 cnepnt (-1) : neg branch direction.
!                         - Exchange between gridpoints (Type 2):
!                         (2,i) = Gridpoint from
!                         (3,i) = Gridpoint to
!                         (4,i) = Section
!                         (5,i) = Length factor (0< length factor <1)
!                         - Exchange in a node (Type 3):
!                         (2,i) = Node number
!                         (3,i) = Gridpoint from
!                         (4,i) = Gridpoint to
!                         (5,i) = Section from
!                         (6,i) = Section to
!                         - Exchange from Qlat stat to segment (Type 4)
!                         (2,i) = Gridpoint
!                         (3,i) = Length factor (0 < factor <= 1).
!                         (4,i) = Lateral station number.
!  6 nbran             I  Number of branches.
!  8 nbrnod            I  Maximum number of connected branches to one
!                         node.
!  3 nexdef            I  Number of entries in exdef table.
!  5 ngrid             I  Number of grid points in network.
!  9 nnode             I  Number of nodes.
!  1 npntr             I  Number of entries in pntr table.
!  2 pntr(4,npntr)     I  Definition of the pointer table. From here it
!                         is possible to find the exchanges between the
!                         segments and the starting location in the
!                         exdef array.
!                         (1,j) = From segment number.
!                         (2,j) = To segment number.
!                         (3,j) = Pointer to exchange table exdef.
!                         (4,j) = Number of exchange definitions in ex-
!                                 def.
! 13 qaggr             P  -
! 14 qlaggr            P  -
! 11 qltpar            P  -
! 15 sexfl(npntr)      IO Table with segment exchange flows.
! 12 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! wqfbgp  Water Quality Flow Between GridPoints
! wqfigp  Water Quality Flow In GridPoint
! wqfind  Water Quality Flow In a NoDe
! wqfqlt  Water Quality Flow Q LaTeral
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
! $Log: wqexfl.pf,v $
! Revision 1.4  1999/03/12  12:34:59  kuipe_j
! parallel segments added
!
! Revision 1.3  1995/05/30  09:56:36  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:08:25  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:45  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:35:30  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:27  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer nbran,&
   &nbrnod,&
   &nexdef,&
   &ngrid,&
   &nnode,&
   &npntr
!
   integer pntr   (4,npntr),&
   &branch (4,nbran),&
   &brnode (nbrnod+1,nnode)
!
   real    exdef  (6,nexdef),&
   &qaggr  (ngrid,3),&
   &qlaggr (*),&
   &qltpar (9,*),&
   &sexfl  (npntr),&
   &x      (ngrid)
!
!     Variables
!
   integer idef, iexch, inode, istat, itype, ndef,  pdef
   real    qex
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
!     Loop over segment exchanges
!
   do 400 iexch = 1, npntr
!
      pdef = pntr (3,iexch)
      ndef = pntr (4,iexch)
!
!        Loop over exchange definitions and calculate exchange flows
!
      sexfl (iexch) = 0

      do 300 idef = pdef, pdef + ndef - 1

         itype = int (exdef (1,idef))

!           if type = 1 : Exchange in a gridpoint

         if (itype .eq. cexigp) then

            call wqfigp ( ngrid  ,qaggr   ,&
            &int (exdef (2,idef)),&
            &int (exdef (3,idef)),&
            &int (exdef (4,idef)),&
            &int (exdef (5,idef)),&
            &qex&
            &)


!           if type = 2 : Exchange between gridpoints

         else if (itype .eq. cexbgp) then

            call wqfbgp ( ngrid  ,qaggr  ,&
            &int (exdef (2,idef)),&
            &int (exdef (3,idef)),&
            &int (exdef (4,idef)),&
            &exdef (5,idef),&
            &qex&
            &)


!           if type = 3 : Exchange in a node

         elseif (itype .eq. cexind) then

            inode = int (exdef (2,idef))

            call wqfind ( ngrid  ,nbran  ,nbrnod ,&
            &branch ,brnode (1,inode),&
            &qaggr  ,inode  ,&
            &int (exdef (3,idef)),&
            &int (exdef (4,idef)),&
            &int (exdef (5,idef)),&
            &int (exdef (6,idef)),&
            &qex&
            &)


!           if type = 4 : Exchange from Qlat station to segment

         elseif (itype .eq. cexqlt) then

            istat  = int ( exdef(4,idef))

            call wqfqlt ( ngrid ,x     ,&
            &qlaggr (istat),&
            &qltpar (1,istat),&
            &int (exdef(2,idef)),&
            &exdef(3,idef),&
            &qex&
            &)
         else

            qex = 0.

         endif

!           Add Qex to exchange flows

         sexfl (iexch) = sexfl (iexch) + qex

300   continue
400 continue
!
   return
end
