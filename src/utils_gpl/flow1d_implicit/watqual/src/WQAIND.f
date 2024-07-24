      subroutine wqaind ( ngrid  ,af     ,afs    ,nexdef ,
     +                    exdef  ,nodenr ,pdef   ,ndef   ,
     +                    idef   ,exarea )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         S.L. van der Woude
c
c Module:             WQAIND (Water Quality Area In a NoDe)
c
c Module description: This routine calculates the exchange area between
c                     two segments in a node.
c
c                     This routine is more complex and works in a diffe-
c                     rent manner as the routines WQAIGP and WQABGP. The
c                     problem is that more than one exchange is needed
c                     from the list of exchanges. This list is used to
c                     determine exchange flows. For instance the follow-
c                     ing problem:
c
c                                      ------------
c                                      |          |
c                               1 -----|-|      |-|----- 1
c                     Seg I     2 -----|-|1    2|-|----- 2      Seg II
c                               3 -----|-|      |-|----- 3
c                                      |          |
c                                      |  node 1  |
c                                      ------------
c
c                     This results in a list of exchanges from segment I
c                     to segment II (node,gridpoint/section):
c
c                     I,11 -> 21 and I,11 -> 22 and I,11 -> 23
c                     I,12 -> 21 and I,12 -> 22 and I,12 -> 23
c                     I,13 -> 21 and I,13 -> 22 and I,13 -> 23
c
c                     To calculate the exchange area between the two
c                     segments the section areas of grid point 1 should
c                     be added and the section areas for grid point 2.
c                     As can be seen in the list the gridpoint/section
c                     combination can occur more then once. Therefore
c                     the combination of gridpoint and section will be
c                     processed only once.
c
c                     Each time routine WQABGP and WQAIGP is called one
c                     definition will be processed. In case the exchange
c                     is in a node WQAIND will process all definitions
c                     which have the same node number.
c                     Therefore when the routine is called with a node
c                     number first a check is made if the node had been
c                     processed before.
c
c                     When all definitions are processed two segment
c                     areas are known in the node. The exchange area is
c                     then calculated by taking the minimum of both
c                     areas.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 af                P  -
c  3 afs               P  -
c 10 exarea            O  Calculated exchange area.
c  5 exdef(6,nexdef)   I  This table contains the elementary exchange
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
c  9 idef              I  Current exchange definition.
c  8 ndef              I  Number of definitions for this exchange.
c  4 nexdef            I  Number of entries in exdef table.
c  1 ngrid             I  Number of grid points in network.
c  6 nodenr            I  Node number to be processed.
c  7 pdef              I  Pointer to exchange table starting definition.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c wqgpar  Water Quality GridPoint ARea
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
c $Log: wqaind.pf,v $
c Revision 1.4  1999/03/15  15:53:47  kuipe_j
c tabs removed
c
c Revision 1.3  1995/05/30  09:56:35  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:08:20  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:41  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:35:22  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:26  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c       Parameters
c
      integer   idef   ,nexdef ,ndef   ,ngrid  ,nodenr ,pdef

      real      exarea

      real      af    (ngrid),
     +          afs   (ngrid,2),
     +          exdef (6,nexdef)
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
c     Variables
c
      integer   i, igp, inode, isecwq, iseg, ixgp, ixsec

      logical   found

      real      area, sega(2)

c
c     First determine if this node has already been calculated. Seach
c     from begin of this definition up until the last one processed.
c
      found = .false.
      do 100 i = pdef, idef - 1
         if ( int(exdef (1,i)) .eq. cexind ) then
            found = ( found ) .or. ( nodenr .eq. int(exdef (2,i)))
         endif
 100  continue

c
c     If not found process this node from here up until the last
c     definition. For each unique node+section combination the area
c     should be calculated and so the total area for one segment is
c     constructed. This is executed for segment 1 and segment 2. At
c     the end of the calculation two areas are known. The exchange
c     area between these segments is then calculated by taking the
c     minimum area.
c
      if (.not. found) then

c
c        From current position to end of definition of exchange for both
c        segments
c
         do 400 iseg = 1, 2

c
c           Select index segment 1 or segment 2
c
            if (iseg .eq. 1) then
               ixgp  = 3
               ixsec = 5
            else
               ixgp  = 4
               ixsec = 6
            endif

            sega (iseg) = 0

            do 300 inode = idef, pdef + ndef - 1
               if (( int(exdef (1,inode)) .eq. cexind ) .and.
     +             ( int(exdef (2,inode)) .eq. nodenr ))
     +         then

                  igp    = int (exdef (ixgp, inode))
                  isecwq = int (exdef (ixsec,inode))

c
c                 Search back until begin to check whether [gp,sec]
c                 combination is unique.
c
                  found = .false.
                  do 200 i = idef, inode - 1
                     if ( int( exdef (1,i)) .eq. cexind ) then
                        found = ( found ) .or.
     +                          (( int(exdef (ixgp, i)) .eq. igp ) .and.
     +                           ( int(exdef (ixsec,i)) .eq. isecwq))
                     endif
 200              continue

c
c                 If not found then add area to total segment area
c
                  if (.not. found) then
                     call wqgpar ( ngrid ,af    ,afs   ,igp   ,
     +                             isecwq,area  )

                     sega (iseg) = sega (iseg) + area
                  endif
               endif
 300        continue
 400     continue

c
c        Area is minimum of totalised surfaces per node
c
         exarea = min ( sega (1), sega (2))
      else
         exarea = 0
      endif

      return
      end
