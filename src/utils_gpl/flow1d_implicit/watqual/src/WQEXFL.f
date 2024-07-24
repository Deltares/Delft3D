      subroutine wqexfl ( npntr  ,pntr   ,nexdef ,exdef  ,
     +                    ngrid  ,nbran  ,branch ,nbrnod ,
     +                    nnode  ,brnode ,qltpar ,x      ,
     +                    qaggr  ,qlaggr ,sexfl  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         S.L. van der Woude
c
c Module:             WQEXFL (Water Quality EXchange FLows)
c
c Module description: This routine calculates the exchange flows for one
c                     time step.
c
c                     The routine uses the pointer table and the exchan-
c                     ge definition table. The pointer table gives in-
c                     formation about segment exchanges in the form
c                     segment i -> segment j. Each exchange is defined
c                     by one or more exchange definitions. Four types
c                     are possible:
c
c                     -   Exchange in a gridpoint;
c                     -   Exchange between two gridpoints;
c                     -   Exchange in a node;
c                     -   Exchange from a lateral discharge station to a
c                         segment
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  7 branch            P  -
c 10 brnode            P  -
c  4 exdef(6,nexdef)   I  This table contains the elementary exchange
c                         definitions. Each exchange between segments
c                         can be described by one or more elementary
c                         definitions in this table.
c                         (1,i) = Exchange type:
c                                 cexigp (1) : Exchange in gridpoint.
c                                 cexbgp (2) : Exchange between gpoints.
c                                 cexind (3) : Exchange in a node.
c                                 cexqlt (4) : Exchange from Qlat to seg
c                         - Exchange in a gridpoint (Type 1):
c                         (2,i) = Gridpoint
c                         (3,i) = Section from
c                         (4,i) = Section to
c                         (5,i) = Direction of "from" --> "to":
c                                 cpopnt (+1) : pos branch direction.
c                                 cnepnt (-1) : neg branch direction.
c                         - Exchange between gridpoints (Type 2):
c                         (2,i) = Gridpoint from
c                         (3,i) = Gridpoint to
c                         (4,i) = Section
c                         (5,i) = Length factor (0< length factor <1)
c                         - Exchange in a node (Type 3):
c                         (2,i) = Node number
c                         (3,i) = Gridpoint from
c                         (4,i) = Gridpoint to
c                         (5,i) = Section from
c                         (6,i) = Section to
c                         - Exchange from Qlat stat to segment (Type 4)
c                         (2,i) = Gridpoint
c                         (3,i) = Length factor (0 < factor <= 1).
c                         (4,i) = Lateral station number.
c  6 nbran             I  Number of branches.
c  8 nbrnod            I  Maximum number of connected branches to one
c                         node.
c  3 nexdef            I  Number of entries in exdef table.
c  5 ngrid             I  Number of grid points in network.
c  9 nnode             I  Number of nodes.
c  1 npntr             I  Number of entries in pntr table.
c  2 pntr(4,npntr)     I  Definition of the pointer table. From here it
c                         is possible to find the exchanges between the
c                         segments and the starting location in the
c                         exdef array.
c                         (1,j) = From segment number.
c                         (2,j) = To segment number.
c                         (3,j) = Pointer to exchange table exdef.
c                         (4,j) = Number of exchange definitions in ex-
c                                 def.
c 13 qaggr             P  -
c 14 qlaggr            P  -
c 11 qltpar            P  -
c 15 sexfl(npntr)      IO Table with segment exchange flows.
c 12 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c wqfbgp  Water Quality Flow Between GridPoints
c wqfigp  Water Quality Flow In GridPoint
c wqfind  Water Quality Flow In a NoDe
c wqfqlt  Water Quality Flow Q LaTeral
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
c $Log: wqexfl.pf,v $
c Revision 1.4  1999/03/12  12:34:59  kuipe_j
c parallel segments added
c
c Revision 1.3  1995/05/30  09:56:36  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:08:25  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:45  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:35:30  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:27  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      integer nbran,
     +        nbrnod,
     +        nexdef,
     +        ngrid,
     +        nnode,
     +        npntr
c
      integer pntr   (4,npntr),
     +        branch (4,nbran),
     +        brnode (nbrnod+1,nnode)
c
      real    exdef  (6,nexdef),
     +        qaggr  (ngrid,3),
     +        qlaggr (*),
     +        qltpar (9,*),
     +        sexfl  (npntr),
     +        x      (ngrid)
c
c     Variables
c
      integer idef, iexch, inode, istat, itype, ndef,  pdef
      real    qex
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
c     Loop over segment exchanges
c
      do 400 iexch = 1, npntr
c
         pdef = pntr (3,iexch)
         ndef = pntr (4,iexch)
c
c        Loop over exchange definitions and calculate exchange flows
c
         sexfl (iexch) = 0

         do 300 idef = pdef, pdef + ndef - 1

            itype = int (exdef (1,idef))

c           if type = 1 : Exchange in a gridpoint

            if (itype .eq. cexigp) then

                call wqfigp ( ngrid  ,qaggr   ,
     +                       int (exdef (2,idef)),
     +                       int (exdef (3,idef)),
     +                       int (exdef (4,idef)),
     +                       int (exdef (5,idef)),
     +                       qex
     +                     )


c           if type = 2 : Exchange between gridpoints

            else if (itype .eq. cexbgp) then

               call wqfbgp ( ngrid  ,qaggr  ,
     +                       int (exdef (2,idef)),
     +                       int (exdef (3,idef)),
     +                       int (exdef (4,idef)),
     +                       exdef (5,idef),
     +                       qex
     +                     )


c           if type = 3 : Exchange in a node

            elseif (itype .eq. cexind) then

               inode = int (exdef (2,idef))

               call wqfind ( ngrid  ,nbran  ,nbrnod ,
     +                       branch ,brnode (1,inode),
     +                       qaggr  ,inode  ,
     +                       int (exdef (3,idef)),
     +                       int (exdef (4,idef)),
     +                       int (exdef (5,idef)),
     +                       int (exdef (6,idef)),
     +                       qex
     +                     )


c           if type = 4 : Exchange from Qlat station to segment

            elseif (itype .eq. cexqlt) then

               istat  = int ( exdef(4,idef))

               call wqfqlt ( ngrid ,x     ,
     +                       qlaggr (istat),
     +                       qltpar (1,istat),
     +                       int (exdef(2,idef)),
     +                       exdef(3,idef),
     +                       qex
     +                     )
            else

               qex = 0.

            endif

c           Add Qex to exchange flows

            sexfl (iexch) = sexfl (iexch) + qex

 300     continue
 400  continue
c
      return
      end
