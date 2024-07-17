      subroutine wqexar ( npntr  ,pntr   ,nexdef ,exdef  ,
     +                    ngrid  ,af     ,afs    ,sexar
     +                  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         S.L. van der Woude
c
c Module:             WQEXAR (Water Quality EXchange AReas)
c
c Module description: This routine calculates the exchange areas for one
c                     time step.
c
c                     The routine uses the pointer table and the exchan-
c                     ge definition table. The pointer table gives in-
c                     formation about segment exchanges in the form
c                     segment i -> segment j. Each exchange is defined
c                     by one or more exchange definitions. Three types
c                     are possible:
c
c                     -   Exchange in a gridpoint (I);
c                     -   Exchange between two gridpoints (II);
c                     -   Exchange in a node (III);
c                     -   Exchange between lateral station and grid-cell
c                         (IV).
c
c                     For these types the following information will be
c                     available:
c
c                     Type I:
c                     -   gridpoint
c                     -   section from
c                     -   section to
c
c                     Type II:
c                     -   gridpoint from
c                     -   gridpoint to
c                     -   section
c                     -   length factor (0 < length factor < 1)
c
c                     Type III:
c                     -   node number
c                     -   gridpoint from
c                     -   gridpoint to
c                     -   section from
c                     -   section to
c
c                     For each type of exchange a different algorithm
c                     must be used. An exchange area for the first ex-
c                     change type is calculated by determination of the
c                     minimum area. An exchange area for the second type
c                     is determined by interpolating between the two
c                     areas known at the two gridpoints.
c                     The last one is more complex. First the total area
c                     of each of the two segments is determined by sum-
c                     ming the areas of the working units connected to
c                     this node. Subsequently, the exchange area for
c                     DELWAQ is found by taking the minimum (likewise
c                     for type 1) of these two areas.
c
c                     An exchange area always must have a value > 0. In
c                     case a zero area is calculated the area will be
c                     set to 1 cm2. For exchanges from a negative to a
c                     positive segment (Lateral discharge) the exchange
c                     area will be set to 1 cm2.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  6 af                P  -
c  7 afs               P  -
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
c  3 nexdef            I  Number of entries in exdef table.
c  5 ngrid             I  Number of grid points in network.
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
c  8 sexar(npntr)      IO Table with segment exchange areas.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c wqabgp  Water Quality Area Between GridPoints
c wqaigp  Water Quality Area In GridPoint
c wqaind  Water Quality Area In a NoDe
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
c $Log: wqexar.pf,v $
c Revision 1.4  1999/03/12  12:34:56  kuipe_j
c parallel segments added
c
c Revision 1.3  1995/05/30  09:56:35  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:08:23  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:44  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:35:28  kuipe_j
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
      integer  nexdef,
     +         ngrid,
     +         npntr
c
      integer  pntr  (4,npntr)
c
      real     af    (ngrid),
     +         afs   (ngrid,2),
     +         exdef (6,nexdef),
     +         sexar (npntr)
c
c     Variables
c
      integer idef, itype, iexch, ndef, pdef
      real    area
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
c        Loop over exchange definitions and calculate area 1 and 2
c
        sexar (iexch) = 0
c
        do 300 idef = pdef, pdef + ndef - 1
c
           itype = int (exdef (1,idef))
c
c           if type = 1 : Exchange in a gridpoint
c
           if (itype .eq. cexigp) then
              call wqaigp ( ngrid ,af    ,afs   ,
     +                       int ( exdef (2,idef)),
     +                       int ( exdef (3,idef)),
     +                       int ( exdef (4,idef)),
     +                       area
     +                     )

c
c           if type = 2 : Exchange between gridpoints
c                         Exchange area calculation by interpolation
c
           else if (itype .eq. cexbgp) then
              call wqabgp ( ngrid ,af    ,afs   ,
     +                       int (exdef (2,idef)),
     +                       int (exdef (3,idef)),
     +                       int (exdef (4,idef)),
     +                       exdef ( 5, idef ),
     +                       area
     +                     )

c
c           if type = 3 : Exchange in a node
c                         Exchange area calculation by minimum surface
c
           elseif (itype .eq. cexind) then
              call wqaind ( ngrid  ,af    ,afs    ,
     +                       nexdef ,exdef ,int (exdef (2,idef)),
     +                       pdef   ,ndef  ,idef   ,area
     +                      )

c
c           if type = 4 : Exchange from qlat station to segment
c                                                  2
c                         Exchange area always 1 cm
c
           elseif (itype .eq. cexqlt) then
              area = 1.0E-4
           else
c
c           if type = 5 : Exchange between parallel segments

              area = 1.

           endif
c
c           Add calculated area to total exchange area
c
           sexar (iexch) = sexar (iexch) + area
c
 300     continue
c
c        Calculated exchange area should not be zero !
c
        if (abs(sexar (iexch)) .lt. 1.0E-9) then
           sexar (iexch) = 1.0E-4
        endif
c
 400  continue
c
      return
      end
