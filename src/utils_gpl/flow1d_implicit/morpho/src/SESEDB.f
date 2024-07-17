      subroutine sesedb (nbran  ,nnode  ,ngrid  ,maxlev ,maxtab ,ntabm ,
     &                   time   ,g      ,pacfac ,relden ,kinvis ,ibr   ,
     &                   juer   ,branch ,node   ,sdrdbf ,sedinf ,ntab  ,
     &                   table  ,e      ,rc     ,nucoef ,uscoef ,trform,
     &                   afs    ,wf     ,wfs    ,hlev   ,rs     ,cs    ,
     &                   x      ,q2     ,qs     ,h2     ,grsize ,forcon,
     &                   sedtr  ,celer  ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SESEDB (SEdiment SEDredge Branch)
c
c Module description: Calculation of the sediment transport in a Sedred-
c                     ge branch.
c
c                     First the flow distribution over the left and
c                     right channel is calculated. For the first upstre-
c                     am gridpoint routine SEDFL1 is called because the
c                     flow is calculated starting from either a boundary
c                     condition or a nodal distribution. For all other
c                     gridpoints SEDFL2 is called which uses the flow
c                     distribution of the first upstream grid point.
c
c                     After this, for both channels the sediment trans-
c                     port and celerity are calculated by calling SETRFO
c                     and SETRCY. Sediment transports and celerities are
c                     corrected in routine SESCED for longitudinal bed
c                     slope effects. The next step is the calculation of
c                     the sediment exchange between the two channels.
c                     This includes transport calculation in the exchan-
c                     ge region (SETRFO) and the calculation of the
c                     direction (SEALPH).
c
c                     It is required that the flow direction in a se-
c                     dredge branch does not change!
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 24 afs(ngrid,2)      I  Actual flow area per section:
c                         (i,1) = For main section of grid point i.
c                         (i,2) = For sub section 1 of grid point i.
c 14 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 37 celer(ngrid,*)    O  Sediment celerity for each gridpoint.
c               1|2       - Normal branches:
c                         (i,1) = Celerity in gridpoint i in main secti-
c                                 on.
c                         - Sedredge branches:
c                         (i,1) = Celerity in gridpoint i,
c                                 left channel.
c                         (i,2) = Celerity in gridpoint i,
c                                 right channel.
c 29 cs(ngrid,3)       I  Actual Chezy coefficient in a section (main,
c                         sub 1, sub 2) for every grid point.
c 20 e                 P  -
c 35 forcon            P  -
c  8 g                 P  -
c 34 grsize(4,ngrid,*) I  Grain sizes for each gridpoint and section.
c                  1|2    Depending on the transport formula chosen one
c                         or more of the grain sizes will be defined.
c                         When a branch has been marked as a sedredge
c                         branch the D50 value should always be defined,
c                         no matter what transport formula has been
c                         chosen. For normal branches only section 1
c                         will be used being the D size in the main and
c                         sub sections. For sedredge branches section 1
c                         will be the D size in the left channel and
c                         section 2 will be the D size in the right
c                         channel.
c                         (1,i,j) =     D35 value for section j, grid-
c                                       point i.
c                         (2,i,j) =     D50 value for section j, grid-
c                                       point i.
c                         (3,i,j) =     D90 value for section j, grid-
c                                       point i.
c                         (4,i,j) =     Dmedium value for section j,
c                                       gridpoint i.
c 33 h2(ngrid)         I  Water level in every grid point at time
c                         t(n+1).
c 27 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
c       maxlev)           - For a circle cross section:
c                         (i,1) = Reference level.
c                         - For a sedredge cross section:
c                         (i,1) = Bed level of main section (i.e. left
c                                 channel).
c                         (i,2) = Bed level of sub section 1 (i.e. right
c                                 channel).
c 12 ibr               I  Number of actual branch.
c 13 juer              P  -
c 38 ker               I  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 11 kinvis            P  -
c  4 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  5 maxtab            I  Maximum number of defined tables.
c  1 nbran             I  Number of branches.
c  3 ngrid             I  Number of grid points in network.
c  2 nnode             I  Number of nodes.
c 15 node              P  -
c 18 ntab              P  -
c  6 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c  9 pacfac            P  -
c 31 q2                P  -
c 32 qs(ngrid,2)       I  Flow in every grid point per section:
c                         (i,1) = Through grid point i of main channel.
c                         (i,2) = Through grid point i of sub section 1.
c 21 rc                P  -
c 10 relden            P  -
c 28 rs(ngrid,3)       I  Actual hydraulic radius in a section (main,
c                         sub 1, sub 2) for every grid point.
c 16 sdrdbf            P  -
c 17 sedinf(2,nbran)   I  Defines for each branch whether a branch is a
c                         sedredge branch or not.
c                         (1,j) = Sedredge branch number (1..nsedrd)
c                                 else 0.
c                         (2,j) = Starting index in Rc array (River bend
c                                 curvature).
c 36 sedtr(ngrid,*)    IO Sediment transport results for each gridpoint.
c               1|2       (At first transports per unit width, finaly
c                         total transports)
c                         - Normal branches:
c                         (i,1) = Sediment transport in gridpoint i of
c                                 main section.
c                         - Sedredge branches:
c                         (i,1) = Sediment transport in gridpoint i,
c                                 left channel.
c                         (i,2) = Sediment transport in gridpoint i,
c                                 right channel.
c                         (i,3) = Sediment exchange in gridpoint i.
c 19 table             P  -
c  7 time              P  -
c 23 trform(3,nbran)   I  Defines for each branch a transport formula:
c                         (1,j) = Transport formula number:
c                                 ctrfeh (1) : Engelund & Hansen
c                                 ctrfmm (2) : Meyer-Peter & Muller
c                                 ctrfaw (3) : Ackers & White
c                                 ctrfvr (4) : Van Rijn
c                                 ctrfpk (5) : Parker & Klingeman
c                                 ctrfud (6) : User Defined Formula
c                         (2,j) = Starting index of the user coeffi-
c                                 cients for this branch.
c                         (3,j) = Calibration factor for transport for-
c                                 mula.
c 22 uscoef            P  -
c 25 wf                P  -
c 26 wfs               P  -
c 30 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c equal   EQUAL test of two real variables
c sealph  SEdiment ALPHa calculation
c secsed  SEdiment Correct SEDiment transport
c secysb  SEdiment CeleritY Sedredge Branch
c sedfl1  SEdiment Distribute FLow on upstream point 1
c sedfl2  SEdiment Distribute FLows 2
c setrfo  SEdiment TRansport FOrmula
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sesedb.pf,v $
c Revision 1.5  1999/03/15  13:47:32  kuipe_j
c coordinate in message
c
c Revision 1.4  1995/10/18  09:00:44  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.3  1995/05/30  09:56:31  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:07:30  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:29  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  08:47:40  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:35:04  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:22  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
      include '..\include\sobdim.i'
c
c     Declaration of parameters
c
      integer    nbran  ,nnode  ,ngrid  ,maxlev ,juer ,nucoef ,
     &           maxtab ,ntabm  ,ibr    ,ker
      integer    branch(4,nbran)  ,node  (4,nnode)  ,sdrdbf(2,*)      ,
     &           sedinf(2,nbran)  ,ntab  (4,maxtab)
      real       g         ,pacfac,relden    ,kinvis
      real       table (ntabm)    ,rc    (*)        ,e     (7,*)      ,
     &           uscoef(nrcoefs,nucoef)             ,trform(3,nbran)  ,
     &           afs   (ngrid,2 ) ,wf    (ngrid)    ,wfs   (ngrid,2)  ,
     &           rs(ngrid,3)  ,x     (ngrid)    ,
     &           cs    (ngrid,3)  ,qs    (ngrid,2)  ,
     &           grsize(4,ngrid,*),forcon(4,ngrid,*),
     &           sedtr (ngrid,*)  ,celer (ngrid,*)
      double     precision   time, hlev  (ngrid,maxlev) 
      double     precision   h2(ngrid), q2(ngrid)
c
c     Declaration of local parameters
c
      integer    igr   ,ig1  ,ig2   ,igp  ,igm  ,linc  ,ibrl ,ind   ,
     &           irc   ,isec ,i     ,ised
      real       depth ,u    ,d12  ,u12  ,c12  ,r12
      real       grn(4),factor,velo,velo1,sedpdu,dvelo
      logical    incall
c
      double precision tanalf
c
      logical    equal
      external   equal
c
c     Constants
c
      integer    d50
      real       diff
      parameter  (d50  = 2)
      parameter  (diff = .001)
c
      incall = .false.
c
c     Calculate Q distribution using boundary condition or
c     flow distribution function on first gridpoint in branch
c     with inflow.
c
      call sedfl1 (ibr    ,nbran  ,nnode  ,ngrid  ,maxtab ,ntabm ,
     &             juer   ,time   ,branch ,node   ,sedinf ,sdrdbf,
     &             ntab   ,table  ,q2     ,qs     ,linc   ,ker   )
c
      if (ker .eq. fatal) goto 100
c
      call sedfl2 (ibr    ,linc   ,nbran  ,ngrid  ,maxlev ,g     ,
     &             branch ,afs    ,wfs    ,hlev   ,x      ,h2    ,
     &             q2     ,cs     ,rs     ,qs     )
c
c     Loop through branch.
c
      ig1  = branch(3,ibr)
      ig2  = branch(4,ibr)
      ind  = max(int(trform(2,ibr)),1)
      ibrl = ibr
      irc  = sedinf(2,ibr) - 1
      ised = sedinf(1,ibr)
c
c     ISED = Sedredge branch number
c     IRC  = Index in RC-array of current grid point
c
      do 30 igr = ig1,ig2
c
c        Calculate sediment transport through channels 1 and 2.
c
         irc = irc + 1
         igp = min(igr+1,ig2)
         igm = max(igr-1,ig1)
         u12 = 0.
         d12 = 0.
         do 10 isec = 1,2
c
c           Calculate velocity and depth
c
            u     = qs(igr,isec) / afs(igr,isec)
            depth = h2(igr) - hlev(igr,isec)
c
c           Calculate correction factor
c
            call secsed (isec  ,igp     ,igm  ,ngrid    ,maxlev,relden ,
     &                   grsize(d50,igr,isec) ,e(1,ised),cs(igr,isec)  ,
     &                   u     ,depth   ,hlev ,x        ,factor        )

c
c           Calculate sediment transport.
c
            call setrfo
     &           (incall ,g  ,pacfac,relden,kinvis ,grsize(1,igr,isec),
     &            cs     (igr,isec) ,u     ,depth  ,rs(igr,isec)      ,
     &            uscoef (1,ind)    ,trform(1,ibr) ,forcon(1,igr,isec),
     &            sedtr  (igr,isec) )
c
c           Multiply sediment transport with correction factor
c
            sedtr(igr,isec) = sedtr(igr,isec) * factor
c
c           Calculate celerity by numerical differentiation.
c
            if (.not.equal(sedtr(igr,isec),0.)) then
c
c              Calculate |u+du|
c
               velo  = abs(u)
               dvelo = velo * diff
               velo1 = velo + dvelo
c
c              Calculate correction factor
c
               call secsed
     &                  (isec  ,igp     ,igm  ,ngrid    ,maxlev,relden ,
     &                   grsize(d50,igr,isec) ,e(1,ised),cs(igr,isec)  ,
     &                   velo1 ,depth   ,hlev ,x        ,factor        )

c
c              Calculate sediment transport based on u+du
c
               call setrfo
     &           (incall ,g  ,pacfac,relden,kinvis ,grsize(1,igr,isec),
     &            cs     (igr,isec) ,velo1 ,depth  ,rs(igr,isec)      ,
     &            uscoef (1,ind)    ,trform(1,ibr) ,forcon(1,igr,isec),
     &            sedpdu )
c
c              Multiply s with correction factor
c
               sedpdu = sedpdu * factor
c
c              Calculate celerity for sedredge branch
c
               call  secysb (g     ,u     ,dvelo ,depth ,ibrl  ,juer  ,
     &                       igr   ,sedtr(igr,isec)     ,sedpdu,
     &                       celer(igr,isec)     ,ker   )
c
               if (ker .ge. warnng) then
                  if (ker .eq. fatal) then
                     goto 100
                  else
                     ibrl = 0
                  endif
               endif
            else
               celer (igr,isec) = 0.
            endif
c
            u12 = u12 + u
            d12 = d12 + depth
c
   10    continue
c
c        Calculate exchange of transport between the 2 channels.
c        At First calculate avaraged parameters.
c        [Doc. S-FO-002.2KV  Eq. 6.13 ]
c
         u12 = u12 *.5
         d12 = d12 *.5
         c12 = sqrt(2. / (1./cs(igr,1)**2 + 1./cs(igr,2)**2))
         r12 = (rs(igr,1) + rs(igr,2)) *.5
c
         do 20 i = 1, 4
            grn(i) = (grsize(i,igr,1) + grsize(i,igr,2)) *.5
   20    continue
c
c        Calculate longitudinal sediment transport in exchange region.
c
         call setrfo (incall        ,g     ,pacfac  ,relden,kinvis  ,
     &                grn   ,c12    ,u12   ,d12     ,r12   ,
     &                uscoef(1,ind) ,trform(1,ibr)  ,forcon(1,igr,3),
     &                sedtr (igr,3) )
c
c        Calculate the (tangent of) angle of transport in exchange
c        region.
c
         call sealph (igr    ,igp   ,igm   ,ngrid   ,maxlev   ,g     ,
     &                relden ,u12   ,c12   ,grn(d50),rc(irc)  ,x     ,
     &                wf     ,hlev  ,h2    ,qs      ,e(1,ised),tanalf)
c
c        Store exchange transport.
c
         sedtr(igr,3) = abs(sedtr(igr,3)) * real(tanalf)
c
  30  continue
      return
c
 100  continue
      end
