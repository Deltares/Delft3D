      subroutine seanal (nbran  ,ngrid  ,maxlev ,bdhrat ,g     ,branch ,
     &                   nonall ,sedinf ,afs    ,wfs    ,hlev  ,nellvl ,
     &                   qs     ,sedtr  ,celer )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SEANAL (SEdiment Adapt Non-Alluvial Layers)
c
c Module description: Adapt sediment transport for non-alluvial layers.
c
c                     When a non-alluvial layer has been defined for a
c                     branch a check is made whether the sediment trans-
c                     port is "feeling" the non-erodible layer. For
c                     sedredge branches this is done for both channels.
c                     For normal branches this is done for the main
c                     channel (largest depth). In routine SENLAY the
c                     reduction will take place.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  9 afs(ngrid,2)      I  Actual flow area per section:
c                         (i,1) = For main section of grid point i.
c                         (i,2) = For sub section 1 of grid point i.
c  4 bdhrat            I  bed form height ratio
c  6 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 16 celer             P  -
c  5 g                 P  -
c 11 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
c       maxlev)           - For a circle cross section:
c                         (i,1) = Reference level.
c                         - For a sedredge cross section:
c                         (i,1) = Bed level of main section (i.e. left
c                                 channel).
c                         (i,2) = Bed level of sub section 1 (i.e. right
c                                 channel).
c  3 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  1 nbran             I  Number of branches.
c 12 nellvl(nnelvl)    I  Level of non erodible layer. For sedredge
c                         branches 2 levels will be defined in this
c                         array on indixes j and j+1.
c  2 ngrid             I  Number of grid points in network.
c  7 nonall(3,nbran)   I  Non alluvial layers info for each branch:
c                         (1,i) = Indicates layer defined for branch i:
c                                 cnlayd (0) : No layer defined.
c                                 cylayd (1) : Layer defined.
c                         (2,i) = Reduction function to use:
c                                 crdstr (1) : Straight reduction.
c                                 crdsin (2) : Sinus reduction.
c                         (3,i) = Start address in nellvl.
c 13 qs(ngrid,2)       I  Flow in every grid point per section:
c                         (i,1) = Through grid point i of main channel.
c                         (i,2) = Through grid point i of sub section 1.
c  8 sedinf(2,nbran)   I  Defines for each branch whether a branch is a
c                         sedredge branch or not.
c                         (1,j) = Sedredge branch number (1..nsedrd)
c                                 else 0.
c                         (2,j) = Starting index in Rc array (River bend
c                                 curvature).
c 14 sedtr             P  -
c 10 wfs(ngrid,2)      I  Actual flow width per section:
c                         (i,1) = For main section of grid point i.
c                         (i,2) = For sub section 1 of grid point i.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c senlay  SEdiment Non-alluvial LAYer
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: seanal.pf,v $
c Revision 1.4  1997/01/23  08:29:56  kuipe_j
c Make flow module robust
c
c Revision 1.3  1995/05/30  09:56:23  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:07:09  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:12  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:34:29  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:18  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer    nbran ,ngrid    ,maxlev
      integer    branch(4,nbran) ,nonall(3,nbran),sedinf(2,nbran)
      real       bdhrat,g
      real       afs   (ngrid,2)      ,wfs  (ngrid,2) ,
     &           qs   (ngrid,2)       ,nellvl(*) ,
     &           sedtr (ngrid,*)      ,celer(ngrid,*)
      double precision hlev (ngrid,maxlev)
c
c     Declaration of local parameters
c
      integer    ibr   ,isec   ,igr    ,ixlvl ,redfun
      real       depth ,laythn ,distnc ,u     ,pi2
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
      pi2 = atan(1.)*2.
c
      do 40 ibr = 1,nbran
c
c        Non-Erodible layer defined ?
c
         if (nonall(1,ibr) .eq. cylayd) then
c
c           Get reduction function number for branch.
c           Get starting index of levels for branch.
c
            redfun = nonall(2,ibr)
            ixlvl  = nonall(3,ibr)
c
c           Is this a Sedredge branch ?
c
            if (sedinf(1,ibr) .ne. 0) then
c
c              Call function for each side of channel.
c
               do 20 igr = branch(3,ibr),branch(4,ibr)
c
                  do 10 isec = 1,2
c
c                    Calculate layer thickness and distance between
c                    bottom level and level of non-erodible layer.
c
                     depth  = afs(igr,isec) / wfs(igr,isec)
                     laythn = bdhrat * depth
                     distnc = hlev(igr,isec) - nellvl(ixlvl)
c
c                    Do we feel the layer ? If so then reduce
c                    transport and celerity.
c
                     if (distnc .lt. laythn ) then
                        u = qs(igr,isec) / afs(igr,isec)
c
                        call senlay (redfun ,distnc ,laythn ,depth ,u  ,
     &                               pi2    ,g      ,sedtr(igr,isec)   ,
     &                               celer(igr,isec))
                     endif
c
                     ixlvl = ixlvl + 1
   10             continue
   20          continue
c
c           Normal branch Reduction for largest depth (main).
c
            else
               do 30 igr = branch(3,ibr),branch(4,ibr)
c
c                 Calculate layer thickness and distance between
c                 bottom level and level of non-erodible layer.
c
                  depth  = afs(igr,1) / wfs(igr,1)
                  laythn = bdhrat * depth
c
                  distnc = hlev(igr,1) - nellvl(ixlvl)
c
c                 Do we feel the layer ? If so then reduce
c                 transport and celerity.
c
                  if (distnc .lt. laythn ) then
                     u = qs(igr,1) / afs(igr,1)
c
                     call senlay (redfun ,distnc ,laythn ,depth ,u   ,
     &                            pi2    ,g      ,sedtr(igr,1)  ,
     &                            celer(igr,1)   )
                  endif
c
                  ixlvl = ixlvl + 1
   30          continue
            endif
         endif
   40 continue
c
      end
