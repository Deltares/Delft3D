      subroutine sedsli (inode ,nnode  ,nbran  ,ngrid  ,maxtab ,ntabm  ,
ci3
     &                   qtot  ,stotfr ,bgout  ,sedinf ,sdrdbf ,seddb  ,
     &                   ntab  ,morcon ,q2     ,table  ,dissed ,nfrac  ,
     &                   nfracb)
cu   &                   qtot  ,stot   ,bgout  ,sedinf ,sdrdbf ,seddb  ,
cu   &                   ntab  ,morcon ,q2     ,table  ,dissed )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SEDSLI (SEdiment Distribute Sediment LInear)
c
c Module description: Calculate sediment transport according to a linear
c                     distribution function.
c
c                     The total sediment transport is divided over the
c                     two outflowing branches (i.e. that are branches
c                     that recieve transport from the node) by using the
c                     two discharges through the branches and the two
c                     function coefficients. The user must define the
c                     coefficients for each pair of outflowing branches.
c                     [S-FO-002.2KV / Eq. 4.9 ]
c                     If the definition is not present for the currently
c                     outflowing branches the proportional method will
c                     be used.
c                     [S-FO-002.2KV / Eq. 4.8 ]
c                     After this routine SEDISE is called in case one or
c                     both branches are sedredge branches. Routine SEDI-
c                     SE distributes the sediment transport over the
c                     left and right channel.
c
c Precondition:       The total incoming discharge and transport in the
c                     node are positive.
c                     The coefficients ALPHA and BETHA are positive. For
c                     the calculation of the linear distribution functi-
c                     on the absolute value of the discharge ratio will
c                     be used.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  9 bgout(3,nrout)    I  Contains info of every outflowing branch i
c                         from the current node:
c                         (1,i) = Branch number
c                         (2,i) = Grid point number
c                         (3,i) = Direction in branch:
c                                 +1 : First grid point is in node.
c                                 -1 : Last grid point is in node.
c 17 dissed(4,nbran)   IO Redistributed sediment transport at begin and
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
c  1 inode             I  Number of actual node.
c  5 maxtab            I  Maximum number of defined tables.
c 14 morcon(4,nmorcn)  I  Morphodynamic conditions in nodes:
c                         (1,i) = Branch number 1.
c                         (2,i) = Branch number 2.
c                         (3,i) = Table pointer to distribution function
c                                 (ratio) or coefficient ALPHA (linear).
c                         (4,i) = Undefined for ratio or coefficient
c                                 BETA (linear).
c  3 nbran             I  Number of branches.
c  3 nfrac             I  Number of sediment fractions
c  3 nfracb            I  Array bound max(nfrac,2)
c  4 ngrid             I  Number of grid points in network.
c  2 nnode             I  Number of nodes.
c 13 ntab              P  -
c  6 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 15 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
c  7 qtot              I  Total discharge flowing towards node.
c 11 sdrdbf            P  -
c 12 seddb(2,nnode)    I  Sediment distribution as function of the di-
c                         scharge:
c                         (1,j) = Type of distribution function:
c                                 cdbpro (1) : Proportional
c                                 cdblin (2) : Linear
c                                 cdbrat (3) : Ratio
c                         Options 2 and 3 are only available for nodes
c                         with three connected branches.
c                         (2,j) = Starting index in morcon for distribu-
c                                 tion functions 2 and 3. If a starting
c                                 index has been specified a seddn(3,j)
c                                 pairs of branches will be found in
c                                 array morcon on position index, in-
c                                 dex+1 , index+2 etc.
c                         (3,j) = Number of pairs with defined linear or
c                                 ratio distribution function.
c 10 sedinf(2,nbran)   I  Defines for each branch whether a branch is a
c                         sedredge branch or not.
c                         (1,j) = Sedredge branch number (1..nsedrd)
c                                 else 0.
c                         (2,j) = Starting index in Rc array (River bend
c                                 curvature).
c  8 stot              I  Total sediment transport going towards node.
c 16 table             P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c equal   EQUAL test of two real variables
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
c $Log: sedsli.pf,v $
c Revision 1.3  1995/10/18  09:00:39  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.2  1995/05/30  07:07:21  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:22  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:34:49  kuipe_j
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
ci2
      integer    inode ,nnode    ,nbran ,ngrid  ,maxtab ,ntabm,
     &           nfrac ,nfracb
cu    integer    inode ,nnode    ,nbran ,ngrid  ,maxtab ,ntabm
      integer    sedinf(2,nbran) ,sdrdbf(2,*)   ,seddb(3,nnode),
     &           ntab  (4,maxtab),bgout (3,2)
ci3
      real       qtot
      real       morcon(2+2*nfrac,*),stotfr(nfrac)   ,
     &           table (ntabm)      ,dissed(nfracb,2,nbran)
cu    real       qtot  ,stot
cu    real       morcon(4,*)     ,q2    (ngrid)    ,
cu   &           table (ntabm)   ,dissed(4,nbran)
      double precision q2(ngrid), q
      logical    equal
      external   equal
c
c     Declaration of local parameters
c
      integer    ibr    ,ibr1  ,ibr2   ,igr    ,igr1  ,igr2   ,dir  ,
     &           dir1   ,dir2  ,ibrmax ,ibrmin ,ind   ,ind1   ,ind2 ,
ci1
     &           indm   ,indmax,sedbr  ,i      ,iside ,ifrac
cu   &           indm   ,indmax,sedbr  ,i
      real       qratio ,sedt2 ,s
      real       dissdb (2)
c
c     Determine which branch is branch-1 and which is branch-2.
c     Also get the index in array Morcon that refers to the 2 branches
c     with incoming sediment transport.
c
      ibr1   = bgout(1,1)
      ibr2   = bgout(1,2)
      ibrmax = max  (ibr1,ibr2)
      ibrmin = min  (ibr1,ibr2)
      ind    = seddb(2,inode)
      indmax = ind + seddb(3,inode)
      i      = ind
      indm   = 0
c
c ----do while index in array Morcon found --->
   10 continue
      if (i .lt. indmax) then
         if (max(int(morcon(1,i)),int(morcon(2,i))) .eq. ibrmax) then
            if (min(int(morcon(1,i)),int(morcon(2,i))) .eq. ibrmin) then
               if (int(morcon(1,i)) .eq. ibr1) then
                  ind1 = 1
                  ind2 = 2
                  indm = i
               else
                  ind1 = 2
                  ind2 = 1
                  indm = i
               endif
               i = indmax
            endif
         endif
         i = i+1
         goto 10
      endif
c <---while end ------------------------------
c
      if (indm .ne. 0) then
c
c        Pair of branches with user defined distribution found.
c
         ibr1  = bgout(1,ind1)
         ibr2  = bgout(1,ind2)
         igr1  = bgout(2,ind1)
         igr2  = bgout(2,ind2)
         dir1  = bgout(3,ind1)
         dir2  = bgout(3,ind2)
c
c        Index Ind1 points to branch 1
c        Index Ind2 points to branch 2
c        Now distribute incoming sediment over the 2 branches
c
ci2
         do 12 ifrac=1,nfrac
c
         if(equal(sngl(q2(igr2)),0.)) then
ci2
            dissed(ifrac,(3-dir1)/2,ibr1) = stotfr(ifrac) * real(dir1)
            dissed(ifrac,(3-dir2)/2,ibr2) = 0.
cu          dissed(2-dir1,ibr1) = stot * real(dir1)
cu          dissed(2-dir2,ibr2) = 0.
         else
            qratio = abs(q2(igr1) / q2(igr2))
            if (qratio .lt. .001) then
ci3
               dissed(ifrac,(3-dir1)/2,ibr1) = 0.
               dissed(ifrac,(3-dir2)/2,ibr2) =
     &                             stotfr(ifrac) * real(dir2)
cu             dissed(2-dir1,ibr1) = 0.
cu             dissed(2-dir2,ibr2) = stot * real(dir2)
            else if (qratio .gt. 1000.) then
ci3
               dissed(ifrac,(3-dir1)/2,ibr1) =
     &                             stotfr(ifrac) * real(dir1)
               dissed(ifrac,(3-dir2)/2,ibr2) = 0.
cu             dissed(2-dir1,ibr1) = stot * real(dir1)
cu             dissed(2-dir2,ibr2) = 0.
            else
ci6
               sedt2 = stotfr(ifrac) /
     &                 (morcon(1+2*ifrac,indm)*qratio +
     &                  morcon(2+2*ifrac,indm) + 1.)
               dissed(ifrac,(3-dir1)/2,ibr1) =
     &                       (stotfr(ifrac) - sedt2) * real(dir1)
               dissed(ifrac,(3-dir2)/2,ibr2) = sedt2 * real(dir2)
cu             sedt2 = stot /
cu   &                 (morcon(3,indm)*qratio + morcon(4,indm) + 1.)
cu             dissed(2-dir1,ibr1) = (stot - sedt2) * real(dir1)
cu             dissed(2-dir2,ibr2) = sedt2 * real(dir2)
            endif
         endif
c
ci2
   12    continue
c
      else
c
c        No pair of branches with user defined distribution found.
c        The proportional method will be used yet.
c
         do 20 i = 1,2
c
            ibr = bgout(1,i)
            igr = bgout(2,i)
            dir = bgout(3,i)
            if (qtot .gt. 1.e-10) then
               qratio = abs(q2(igr)) / qtot
            else
               qratio = .5
            endif
ci5
            iside = (3-dir)/2
            do 18 ifrac=1,nfrac
               dissed(ifrac,iside,ibr) =
     &                qratio * stotfr(ifrac) * real(dir)
   18       continue
cu          dissed(2-dir,ibr) = qratio * stot * real(dir)
   20    continue
c
      endif
c
c     Check for sedredge branches and if nessecary distribute sediment
c     over both channels.
c
      do 30 i = 1,2
c
         ibr = bgout(1,i)
         igr = bgout(2,i)
         dir = bgout(3,i)
c
c        Get sedredge branch number.
c
         sedbr = sedinf(1,ibr)
         if (sedbr .ne. 0) then
c
c           Get total sediment and total discharge of the sedredge
c           branch.
c
ci2
            iside = (3-dir)/2
            s = dissed(1,iside,ibr)
cu          s = dissed(2-dir,ibr)
            q = q2(igr)
c
            call sedise (sedbr  ,maxtab ,ntabm  ,s   ,q   ,
     &                   sdrdbf ,ntab   ,table  ,dissdb )
c
ci2
            dissed(1,iside,ibr) = dissdb(1)
            dissed(2,iside,ibr) = dissdb(2)
cu          dissed(2-dir,ibr) = dissdb(1)
cu          dissed(3-dir,ibr) = dissdb(2)
c
         endif
c
  30  continue
c
      end
