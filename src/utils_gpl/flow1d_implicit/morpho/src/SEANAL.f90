subroutine seanal (nbran  ,ngrid  ,maxlev ,bdhrat ,g     ,branch ,&
&nonall ,sedinf ,afs    ,wfs    ,hlev  ,nellvl ,&
&qs     ,sedtr  ,celer )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SEANAL (SEdiment Adapt Non-Alluvial Layers)
!
! Module description: Adapt sediment transport for non-alluvial layers.
!
!                     When a non-alluvial layer has been defined for a
!                     branch a check is made whether the sediment trans-
!                     port is "feeling" the non-erodible layer. For
!                     sedredge branches this is done for both channels.
!                     For normal branches this is done for the main
!                     channel (largest depth). In routine SENLAY the
!                     reduction will take place.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  9 afs(ngrid,2)      I  Actual flow area per section:
!                         (i,1) = For main section of grid point i.
!                         (i,2) = For sub section 1 of grid point i.
!  4 bdhrat            I  bed form height ratio
!  6 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 16 celer             P  -
!  5 g                 P  -
! 11 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
!  3 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  1 nbran             I  Number of branches.
! 12 nellvl(nnelvl)    I  Level of non erodible layer. For sedredge
!                         branches 2 levels will be defined in this
!                         array on indixes j and j+1.
!  2 ngrid             I  Number of grid points in network.
!  7 nonall(3,nbran)   I  Non alluvial layers info for each branch:
!                         (1,i) = Indicates layer defined for branch i:
!                                 cnlayd (0) : No layer defined.
!                                 cylayd (1) : Layer defined.
!                         (2,i) = Reduction function to use:
!                                 crdstr (1) : Straight reduction.
!                                 crdsin (2) : Sinus reduction.
!                         (3,i) = Start address in nellvl.
! 13 qs(ngrid,2)       I  Flow in every grid point per section:
!                         (i,1) = Through grid point i of main channel.
!                         (i,2) = Through grid point i of sub section 1.
!  8 sedinf(2,nbran)   I  Defines for each branch whether a branch is a
!                         sedredge branch or not.
!                         (1,j) = Sedredge branch number (1..nsedrd)
!                                 else 0.
!                         (2,j) = Starting index in Rc array (River bend
!                                 curvature).
! 14 sedtr             P  -
! 10 wfs(ngrid,2)      I  Actual flow width per section:
!                         (i,1) = For main section of grid point i.
!                         (i,2) = For sub section 1 of grid point i.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! senlay  SEdiment Non-alluvial LAYer
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: seanal.pf,v $
! Revision 1.4  1997/01/23  08:29:56  kuipe_j
! Make flow module robust
!
! Revision 1.3  1995/05/30  09:56:23  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:07:09  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:12  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:34:29  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:18  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    nbran ,ngrid    ,maxlev
   integer    branch(4,nbran) ,nonall(3,nbran),sedinf(2,nbran)
   real       bdhrat,g
   real       afs   (ngrid,2)      ,wfs  (ngrid,2) ,&
   &qs   (ngrid,2)       ,nellvl(*) ,&
   &sedtr (ngrid,*)      ,celer(ngrid,*)
   double precision hlev (ngrid,maxlev)
!
!     Declaration of local parameters
!
   integer    ibr   ,isec   ,igr    ,ixlvl ,redfun
   real       depth ,laythn ,distnc ,u     ,pi2
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
   pi2 = atan(1.)*2.
!
   do 40 ibr = 1,nbran
!
!        Non-Erodible layer defined ?
!
      if (nonall(1,ibr) .eq. cylayd) then
!
!           Get reduction function number for branch.
!           Get starting index of levels for branch.
!
         redfun = nonall(2,ibr)
         ixlvl  = nonall(3,ibr)
!
!           Is this a Sedredge branch ?
!
         if (sedinf(1,ibr) .ne. 0) then
!
!              Call function for each side of channel.
!
            do 20 igr = branch(3,ibr),branch(4,ibr)
!
               do 10 isec = 1,2
!
!                    Calculate layer thickness and distance between
!                    bottom level and level of non-erodible layer.
!
                  depth  = afs(igr,isec) / wfs(igr,isec)
                  laythn = bdhrat * depth
                  distnc = hlev(igr,isec) - nellvl(ixlvl)
!
!                    Do we feel the layer ? If so then reduce
!                    transport and celerity.
!
                  if (distnc .lt. laythn ) then
                     u = qs(igr,isec) / afs(igr,isec)
!
                     call senlay (redfun ,distnc ,laythn ,depth ,u  ,&
                     &pi2    ,g      ,sedtr(igr,isec)   ,&
                     &celer(igr,isec))
                  endif
!
                  ixlvl = ixlvl + 1
10             continue
20          continue
!
!           Normal branch Reduction for largest depth (main).
!
         else
            do 30 igr = branch(3,ibr),branch(4,ibr)
!
!                 Calculate layer thickness and distance between
!                 bottom level and level of non-erodible layer.
!
               depth  = afs(igr,1) / wfs(igr,1)
               laythn = bdhrat * depth
!
               distnc = hlev(igr,1) - nellvl(ixlvl)
!
!                 Do we feel the layer ? If so then reduce
!                 transport and celerity.
!
               if (distnc .lt. laythn ) then
                  u = qs(igr,1) / afs(igr,1)
!
                  call senlay (redfun ,distnc ,laythn ,depth ,u   ,&
                  &pi2    ,g      ,sedtr(igr,1)  ,&
                  &celer(igr,1)   )
               endif
!
               ixlvl = ixlvl + 1
30          continue
         endif
      endif
40 continue
!
end
