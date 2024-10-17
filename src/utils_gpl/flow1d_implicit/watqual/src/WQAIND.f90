subroutine wqaind ( ngrid  ,af     ,afs    ,nexdef ,&
&exdef  ,nodenr ,pdef   ,ndef   ,&
&idef   ,exarea )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         S.L. van der Woude
!
! Module:             WQAIND (Water Quality Area In a NoDe)
!
! Module description: This routine calculates the exchange area between
!                     two segments in a node.
!
!                     This routine is more complex and works in a diffe-
!                     rent manner as the routines WQAIGP and WQABGP. The
!                     problem is that more than one exchange is needed
!                     from the list of exchanges. This list is used to
!                     determine exchange flows. For instance the follow-
!                     ing problem:
!
!                                      ------------
!                                      |          |
!                               1 -----|-|      |-|----- 1
!                     Seg I     2 -----|-|1    2|-|----- 2      Seg II
!                               3 -----|-|      |-|----- 3
!                                      |          |
!                                      |  node 1  |
!                                      ------------
!
!                     This results in a list of exchanges from segment I
!                     to segment II (node,gridpoint/section):
!
!                     I,11 -> 21 and I,11 -> 22 and I,11 -> 23
!                     I,12 -> 21 and I,12 -> 22 and I,12 -> 23
!                     I,13 -> 21 and I,13 -> 22 and I,13 -> 23
!
!                     To calculate the exchange area between the two
!                     segments the section areas of grid point 1 should
!                     be added and the section areas for grid point 2.
!                     As can be seen in the list the gridpoint/section
!                     combination can occur more then once. Therefore
!                     the combination of gridpoint and section will be
!                     processed only once.
!
!                     Each time routine WQABGP and WQAIGP is called one
!                     definition will be processed. In case the exchange
!                     is in a node WQAIND will process all definitions
!                     which have the same node number.
!                     Therefore when the routine is called with a node
!                     number first a check is made if the node had been
!                     processed before.
!
!                     When all definitions are processed two segment
!                     areas are known in the node. The exchange area is
!                     then calculated by taking the minimum of both
!                     areas.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 af                P  -
!  3 afs               P  -
! 10 exarea            O  Calculated exchange area.
!  5 exdef(6,nexdef)   I  This table contains the elementary exchange
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
!  9 idef              I  Current exchange definition.
!  8 ndef              I  Number of definitions for this exchange.
!  4 nexdef            I  Number of entries in exdef table.
!  1 ngrid             I  Number of grid points in network.
!  6 nodenr            I  Node number to be processed.
!  7 pdef              I  Pointer to exchange table starting definition.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! wqgpar  Water Quality GridPoint ARea
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
! $Log: wqaind.pf,v $
! Revision 1.4  1999/03/15  15:53:47  kuipe_j
! tabs removed
!
! Revision 1.3  1995/05/30  09:56:35  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:08:20  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:41  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:35:22  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:26  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!       Parameters
!
   integer   idef   ,nexdef ,ndef   ,ngrid  ,nodenr ,pdef

   real      exarea

   real      af    (ngrid),&
   &afs   (ngrid,2),&
   &exdef (6,nexdef)
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
!     Variables
!
   integer   i, igp, inode, isecwq, iseg, ixgp, ixsec

   logical   found

   real      area, sega(2)

!
!     First determine if this node has already been calculated. Seach
!     from begin of this definition up until the last one processed.
!
   found = .false.
   do 100 i = pdef, idef - 1
      if ( int(exdef (1,i)) .eq. cexind ) then
         found = ( found ) .or. ( nodenr .eq. int(exdef (2,i)))
      endif
100 continue

!
!     If not found process this node from here up until the last
!     definition. For each unique node+section combination the area
!     should be calculated and so the total area for one segment is
!     constructed. This is executed for segment 1 and segment 2. At
!     the end of the calculation two areas are known. The exchange
!     area between these segments is then calculated by taking the
!     minimum area.
!
   if (.not. found) then

!
!        From current position to end of definition of exchange for both
!        segments
!
      do 400 iseg = 1, 2

!
!           Select index segment 1 or segment 2
!
         if (iseg .eq. 1) then
            ixgp  = 3
            ixsec = 5
         else
            ixgp  = 4
            ixsec = 6
         endif

         sega (iseg) = 0

         do 300 inode = idef, pdef + ndef - 1
            if (( int(exdef (1,inode)) .eq. cexind ) .and.&
            &( int(exdef (2,inode)) .eq. nodenr ))&
            &then

               igp    = int (exdef (ixgp, inode))
               isecwq = int (exdef (ixsec,inode))

!
!                 Search back until begin to check whether [gp,sec]
!                 combination is unique.
!
               found = .false.
               do 200 i = idef, inode - 1
                  if ( int( exdef (1,i)) .eq. cexind ) then
                     found = ( found ) .or.&
                     &(( int(exdef (ixgp, i)) .eq. igp ) .and.&
                     &( int(exdef (ixsec,i)) .eq. isecwq))
                  endif
200            continue

!
!                 If not found then add area to total segment area
!
               if (.not. found) then
                  call wqgpar ( ngrid ,af    ,afs   ,igp   ,&
                  &isecwq,area  )

                  sega (iseg) = sega (iseg) + area
               endif
            endif
300      continue
400   continue

!
!        Area is minimum of totalised surfaces per node
!
      exarea = min ( sega (1), sega (2))
   else
      exarea = 0
   endif

   return
end
