cu    subroutine seds1b (nbran  ,ngrid  ,maxtab ,ntabm ,stot   ,bgout1 ,
ci3
      subroutine seds1b (nbran  ,ngrid  ,maxtab ,ntabm ,stotfr ,bgout1 ,
     &                   sedinf ,sdrdbf ,ntab   ,q2    ,table  ,dissed ,
     &                   nfrac  ,nfracb )
cu   &                   sedinf ,sdrdbf ,ntab   ,q2    ,table  ,dissed )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SEDS1B (SEdiment Distribute Sediment for 1 Branch)
c
c Module description: Distribute sediment transport for one outflowing
c                     branch.
c
c                     The total sediment transport is stored in the
c                     outflowing branch if it is a normal branch. In
c                     case the branch is a Sedredge branch routine SEDI-
c                     SE is called. Routine SEDISE distributes the sedi-
c                     ment transport over the left and right channel.
c
c Precondition:       The total incoming transport in node is positive.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  6 bgout1(3)         I  Contains info for outflowing branch:
c                         (1) =   Branch number
c                         (2) =   Grid point number
c                         (3) =   Direction in branch:
c                                 +1 : First grid point is in node.
c                                 -1 : Last grid point is in node.
c 12 dissed(4,nbran)   O  Redistributed sediment transport at begin and
c                         end of branches. At the outflow side of the
c                         branch the calculated transports are stored.
c                         At the inflow side the redistributed trans-
c                         ports are stored.
c                         (1,i)   Transport at section 1 (main or left
c                                 channel)  at begin of branch.
c                         (2,i)   Transport at section 2 (right channel)
c                                 at begin of branch.
c                         (3,i)   Transport at section 1 (main or left
c                                 channel)  at end of branch.
c                         (4,i)   Transport at section 2 (right channel)
c                                 at end of branch.
c  3 maxtab            I  Maximum number of defined tables.
c  1 nbran             I  Number of branches.
c  2 ngrid             I  Number of grid points in network.
c  9 ntab              P  -
c  4 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 10 q2                P  -
c  8 sdrdbf            P  -
c  7 sedinf(2,nbran)   I  Defines for each branch whether a branch is a
c                         sedredge branch or not.
c                         (1,j) = Sedredge branch number (1..nsedrd)
c                                 else 0.
c                         (2,j) = Starting index in Rc array (River bend
c                                 curvature).
c  5 stot              I  Total sediment transport going towards node.
c 11 table             P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c sedise  SEdiment DIstribute SEdredge
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: seds1b.pf,v $
c Revision 1.2  1995/05/30  07:07:20  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:21  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:34:47  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:21  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
ci1
      integer    nbran ,ngrid     ,maxtab  ,ntabm  ,nfrac ,nfracb
cu    integer    nbran ,ngrid     ,maxtab  ,ntabm
      integer    sedinf(2,nbran)  ,sdrdbf(2,*) ,
     &           ntab  (4,maxtab) ,bgout1(3)
cu    real       stot
cu    real       q2    (ngrid)    ,table (ntabm)    ,dissed(4,nbran)
ci2
      real       table (ntabm)    ,stotfr(nfrac)  ,
     &           dissed(nfracb,2,nbran)

      double precision q2(ngrid)
c
c     Declaration of local parameters
c
ci1
      integer    ibr    ,igr    ,dir  ,sedbr ,iside, ifrac
cu    integer    ibr    ,igr    ,dir  ,sedbr
      real       s
      real       dissdb (2)
c
      ibr = bgout1(1)
      igr = bgout1(2)
      dir = bgout1(3)
c
ci5
      iside             = (3-dir)/2
      do 10 ifrac = 1,nfrac
         s                       = stotfr(ifrac) * real(dir)
         dissed(ifrac,iside,ibr) = s
   10 continue
cu    s                 = stot * real(dir)
cu    dissed(2-dir,ibr) = s
c
c     Check for sedredge branches and if nessecary distribute sediment
c     over both channels.
c
c     Get sedredge branch number
c
      sedbr = sedinf(1,ibr)
      if (sedbr .ne. 0) then
c
c        Get total sediment and total discharge of the sedredge
c        branch.
c
         call sedise (sedbr  ,maxtab ,ntabm  ,s    ,q2(igr),
     &                sdrdbf ,ntab   ,table  ,dissdb )
c
ci2
         dissed(1,iside,ibr) = dissdb(1)
         dissed(2,iside,ibr) = dissdb(2)
cu       dissed(2-dir,ibr) = dissdb(1)
cu       dissed(3-dir,ibr) = dissdb(2)
c
      endif
c
      end
