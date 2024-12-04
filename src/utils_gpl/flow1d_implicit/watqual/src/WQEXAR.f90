subroutine wqexar ( npntr  ,pntr   ,nexdef ,exdef  ,&
&ngrid  ,af     ,afs    ,sexar&
&)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         S.L. van der Woude
!
! Module:             WQEXAR (Water Quality EXchange AReas)
!
! Module description: This routine calculates the exchange areas for one
!                     time step.
!
!                     The routine uses the pointer table and the exchan-
!                     ge definition table. The pointer table gives in-
!                     formation about segment exchanges in the form
!                     segment i -> segment j. Each exchange is defined
!                     by one or more exchange definitions. Three types
!                     are possible:
!
!                     -   Exchange in a gridpoint (I);
!                     -   Exchange between two gridpoints (II);
!                     -   Exchange in a node (III);
!                     -   Exchange between lateral station and grid-cell
!                         (IV).
!
!                     For these types the following information will be
!                     available:
!
!                     Type I:
!                     -   gridpoint
!                     -   section from
!                     -   section to
!
!                     Type II:
!                     -   gridpoint from
!                     -   gridpoint to
!                     -   section
!                     -   length factor (0 < length factor < 1)
!
!                     Type III:
!                     -   node number
!                     -   gridpoint from
!                     -   gridpoint to
!                     -   section from
!                     -   section to
!
!                     For each type of exchange a different algorithm
!                     must be used. An exchange area for the first ex-
!                     change type is calculated by determination of the
!                     minimum area. An exchange area for the second type
!                     is determined by interpolating between the two
!                     areas known at the two gridpoints.
!                     The last one is more complex. First the total area
!                     of each of the two segments is determined by sum-
!                     ming the areas of the working units connected to
!                     this node. Subsequently, the exchange area for
!                     DELWAQ is found by taking the minimum (likewise
!                     for type 1) of these two areas.
!
!                     An exchange area always must have a value > 0. In
!                     case a zero area is calculated the area will be
!                     set to 1 cm2. For exchanges from a negative to a
!                     positive segment (Lateral discharge) the exchange
!                     area will be set to 1 cm2.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  6 af                P  -
!  7 afs               P  -
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
!  3 nexdef            I  Number of entries in exdef table.
!  5 ngrid             I  Number of grid points in network.
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
!  8 sexar(npntr)      IO Table with segment exchange areas.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! wqabgp  Water Quality Area Between GridPoints
! wqaigp  Water Quality Area In GridPoint
! wqaind  Water Quality Area In a NoDe
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
! $Log: wqexar.pf,v $
! Revision 1.4  1999/03/12  12:34:56  kuipe_j
! parallel segments added
!
! Revision 1.3  1995/05/30  09:56:35  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:08:23  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:44  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:35:28  kuipe_j
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
   integer  nexdef,&
   &ngrid,&
   &npntr
!
   integer  pntr  (4,npntr)
!
   real     af    (ngrid),&
   &afs   (ngrid,2),&
   &exdef (6,nexdef),&
   &sexar (npntr)
!
!     Variables
!
   integer idef, itype, iexch, ndef, pdef
   real    area
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
!        Loop over exchange definitions and calculate area 1 and 2
!
      sexar (iexch) = 0
!
      do 300 idef = pdef, pdef + ndef - 1
!
         itype = int (exdef (1,idef))
!
!           if type = 1 : Exchange in a gridpoint
!
         if (itype .eq. cexigp) then
            call wqaigp ( ngrid ,af    ,afs   ,&
            &int ( exdef (2,idef)),&
            &int ( exdef (3,idef)),&
            &int ( exdef (4,idef)),&
            &area&
            &)

!
!           if type = 2 : Exchange between gridpoints
!                         Exchange area calculation by interpolation
!
         else if (itype .eq. cexbgp) then
            call wqabgp ( ngrid ,af    ,afs   ,&
            &int (exdef (2,idef)),&
            &int (exdef (3,idef)),&
            &int (exdef (4,idef)),&
            &exdef ( 5, idef ),&
            &area&
            &)

!
!           if type = 3 : Exchange in a node
!                         Exchange area calculation by minimum surface
!
         elseif (itype .eq. cexind) then
            call wqaind ( ngrid  ,af    ,afs    ,&
            &nexdef ,exdef ,int (exdef (2,idef)),&
            &pdef   ,ndef  ,idef   ,area&
            &)

!
!           if type = 4 : Exchange from qlat station to segment
!                                                  2
!                         Exchange area always 1 cm
!
         elseif (itype .eq. cexqlt) then
            area = 1.0E-4
         else
!
!           if type = 5 : Exchange between parallel segments

            area = 1.

         endif
!
!           Add calculated area to total exchange area
!
         sexar (iexch) = sexar (iexch) + area
!
300   continue
!
!        Calculated exchange area should not be zero !
!
      if (abs(sexar (iexch)) .lt. 1.0E-9) then
         sexar (iexch) = 1.0E-4
      endif
!
400 continue
!
   return
end
