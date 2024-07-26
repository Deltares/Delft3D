      subroutine senorb (nbran  ,ngrid  ,maxlev ,g      ,pacfac ,relden,
     &                   kinvis ,ibr    ,juer   ,branch ,bfrict ,engpar,
     &                   nucoef ,uscoef ,trform ,prslot ,af     ,afs   ,
     &                   wf     ,wfs    ,nlev   ,hlev   ,wft    ,ws    ,
     &                   secths ,rs     ,cs     ,asubsc ,alfab  ,q2    ,
     &                   qs     ,h2     ,grsize ,forcon ,sedtr  ,celer ,
     &                   lrivr  ,psltvr ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SENORB (SEdiment NORmal Branch)
c
c Module description: Calculation of the sediment transport in a normal
c                     branch.
c
c                     First the sediment transport is calculated in
c                     every gridpoint of the main section (subroutine
c                     SETRFO). After that the celerity is calculated
c                     with subroutine SETRCY. In this routine the cele-
c                     rity is calculated by numerical differentiation of
c                     the transport. Therefore, in case of the Engelund
c                     roughness predictor the Chezy value must also be
c                     calculated for U+.001 (subroutine ENGRPR).
c
c
c                     If the Preissmann slot is present and the water
c                     level drops below the original bottom the sediment
c                     transport and celerity are set to zero.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 17 af                P  -
c 18 afs(ngrid,2)      I  Actual flow area per section:
c                         (i,1) = For main section of grid point i.
c                         (i,2) = For sub section 1 of grid point i.
c 29 alfab             P  -
c 28 asubsc(ngrid)     I  Defines the actual number of sub sections for
c                         avery cross section (depending on the actual
c                         water level):
c                         c1sec (0) : Main section only (0 sub sections)
c                         c2sec (1) : 1 sub section
c                         c3sec (2) : 2 sub sections
c 12 bfrict(3,nbran)   I  Bed friction in sections of branch.
c                         For each branch and section a different fric-
c                         tion type can be defined.
c                         (1,i) = Friction type in main section in
c                                 branch i:
c                                 cfrchc (1) : Chezy constant
c                                 cfrchq (2) : Chezy function of Q
c                                 cfrchh (3) : Chezy function of h
c                                 cfrman (4) : Manning constant
c                                 cfrskn (5) : Strickler 1 constant Kn
c                                 cfrsks (6) : Strickler 2 constant Ks
c                                 cfrnik (7) : Nikuradze constant
c                                 cfreng (8) : Engelund predictor
c                         (2,i) = Friction type in sub section 1 in
c                                 branch i:
c                                 cfrchc (1) : Chezy constant
c                                 cfrman (4) : Manning constant
c                                 cfrskn (5) : Strickler 1 constant Kn
c                                 cfrsks (6) : Strickler 2 constant Ks
c                                 cfrnik (7) : Nikuradze constant
c                         (3,i) = Friction type in sub section 2 in
c                                 branch i:
c                                 Same definitions as bfrict(2,i)
c 11 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 36 celer(ngrid,*)    O  Sediment celerity for each gridpoint.
c               1|2       - Normal branches:
c                         (i,1) = Celerity in gridpoint i in main secti-
c                                 on.
c                         - Sedredge branches:
c                         (i,1) = Celerity in gridpoint i,
c                                 left channel.
c                         (i,2) = Celerity in gridpoint i,
c                                 right channel.
c 27 cs(ngrid,3)       I  Actual Chezy coefficient in a section (main,
c                         sub 1, sub 2) for every grid point.
c 13 engpar            P  -
c 34 forcon            P  -
c  4 g                 P  -
c 33 grsize            P  -
c 32 h2(ngrid)         I  Water level in every grid point at time
c                         t(n+1).
c 22 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
c       maxlev)           - For a circle cross section:
c                         (i,1) = Reference level.
c                         - For a sedredge cross section:
c                         (i,1) = Bed level of main section (i.e. left
c                                 channel).
c                         (i,2) = Bed level of sub section 1 (i.e. right
c                                 channel).
c  9 ibr               I  Number of actual branch.
c 10 juer              P  -
c 37 ker               I  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  8 kinvis            P  -
c  3 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  1 nbran             I  Number of branches.
c  2 ngrid             I  Number of grid points in network.
c 21 nlev              P  -
c  6 pacfac            P  -
c 16 prslot(3,nbran)   I  Contains information concerning Preissmann
c                         slot (assuring positive water depths).
c                         (1,i) = Create slot indicator
c                                 csldis (0) : No slot in this branch.
c                                 cslena (1) : Create slot in branch.
c                         (2,i) = Reference area for slot generation.
c                         (3,i) = Depth of slot below lowest bottom of
c                                 branch.
c 38 psltvr(7,ngrid)   I  Preissmann slot variables for every grid point
c                         i (assuring positive water depths):
c                         (1,i) = Value for C**2*R for positive flow.
c                         (2,i) = Value for C**2*R for negative flow.
c                         (3,i) = Bottom of slot (funnel)
c                         (4,i) = Division level between trapezium and
c                                 rectangle of slot (top of rectangle
c                                 and bottom of trapezium)
c                         (5,i) = Top of slot
c                         (6,i) = Bottom width of slot (width of
c                                 rectangle)
c                         (7,i) = Top width of slot (top of trapezium)
c 30 q2                P  -
c 31 qs(ngrid,2)       I  Flow in every grid point per section:
c                         (i,1) = Through grid point i of main channel.
c                         (i,2) = Through grid point i of sub section 1.
c  7 relden            P  -
c 26 rs                P  -
c 25 secths            P  -
c 35 sedtr(ngrid,*)    O  Sediment transport results for each gridpoint.
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
c 15 trform(3,nbran)   I  Defines for each branch a transport formula:
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
c 14 uscoef            P  -
c 19 wf                P  -
c 20 wfs(ngrid,2)      I  Actual flow width per section:
c                         (i,1) = For main section of grid point i.
c                         (i,2) = For sub section 1 of grid point i.
c 23 wft               P  -
c 24 ws                P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c engrpr  ENGelund Roughness PRedictor
c equal   EQUAL test of two real variables
c secynb  SEdiment CeleritY Normal Branch
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
c $Log: senorb.pf,v $
c Revision 1.7  1998/06/11  11:47:29  kuipe_j
c Estuary special integrated
c
c Revision 1.6  1998/02/13  12:09:58  kuipe_j
c Avoid pointer toempty array
c
c Revision 1.5  1997/01/23  08:29:59  kuipe_j
c Make flow module robust
c
c Revision 1.4  1995/10/18  09:00:42  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.3  1995/05/30  09:56:30  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:07:29  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:28  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:35:01  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:22  kuipe_j
c Initial version
c
c
c***********************************************************************
c
      include '..\include\sobdim.i'
c
c     Declaration of parameters
c
      integer    nbran ,ngrid     ,maxlev  ,juer  ,ibr  ,ker ,nucoef
      integer    branch(4,nbran)  ,bfrict(3,nbran)  ,nlev(ngrid)
      real       g     ,pacfac    ,relden  ,kinvis
      real       uscoef(nrcoefs,nucoef),trform(3,nbran)     ,
     &           engpar(9)             ,prslot(3,nbran)     ,
     &           af    (ngrid)         ,afs   (ngrid,2 )    ,
     &           wf    (ngrid)         ,wfs   (ngrid,2)     ,
     &           rs    (ngrid,3)       ,cs    (ngrid,3)     ,
     &           qs    (ngrid,2)       ,alfab (ngrid)       ,
     &           wft   (ngrid,maxlev)  ,ws    (ngrid)         ,
     &           asubsc(ngrid)         ,secths(ngrid)       ,
     &           grsize(4,ngrid,*)     ,forcon(4,ngrid,*)   ,
     &           sedtr (ngrid,*)       ,celer (ngrid,*)     ,
     &           psltvr(7,ngrid)
      double precision hlev  (ngrid,maxlev), h2(ngrid), q2(ngrid)
c
c     Declaration of local variables
c
      integer    igr   ,ibrl ,ind  ,nsect
      real       depth ,u    ,u1   ,chezy
      real       velo  ,velo1,dvelo,sedpdu
      logical    equal, lwet, incall , lrivr
      external   equal
c
c     Constants
c
      real       diff
      parameter  (diff=.001)
      integer    d90
      parameter  (d90=3)
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
      include '..\include\sobcon.i'
c
      incall = .false.
c
      ind   = max(int(trform(2,ibr)),1)
      ibrl  = ibr

      do 10 igr = branch(3,ibr),branch(4,ibr)
c
         if (int(prslot(1,ibr)) .eq. cslena) then
            if (h2(igr) .lt. psltvr(5,igr) ) then
c
c              Dry point in case of Preisman slot.
c
               sedtr(igr,1) = 0.
               celer(igr,1) = 0.
               lwet         = .false.
            else
               lwet         = .true.
            endif
         else
            lwet = .true.
         endif
c
         if (lwet) then
c
c           Wet point.
c           Calculate velocity and depth.
c
            u     = qs(igr,1) / afs(igr,1)
            depth = afs(igr,1) / wfs(igr,1)
c
c           Calculate sediment transport.
c
            call setrfo
     &             (incall    ,g   ,pacfac  ,relden,kinvis,
     &              grsize(1,igr,1),cs(igr,1)   ,u ,depth ,rs(igr,1),
     &              uscoef(1,ind)  ,trform(1,ibr)  ,forcon(1,igr,1) ,
     &              sedtr (igr,1)  )
c
c           Calculate celerity by numerical differentiation,
c           if transport is not zero.
c
            if (.not.equal(sedtr(igr,1),0.)) then
               if (bfrict(1,ibr) .eq. cfreng) then
                  u1 = abs(u) * (1.+diff)
c
c                 In case of the Engelund roughness predictor the Chezy
c                 value is velocity dependent.
c
                  call engrpr(engpar    ,grsize(d90,igr,1) ,u1 ,
     &                        rs(igr,1) ,chezy )
               else
                  chezy = cs(igr,1)
               endif
c
c              Calculate |u+du|
c
               velo  = abs(u)
               dvelo = velo * diff
               velo1 = velo + dvelo
c
c              Calculate sediment transport based on u+du
c
               call setrfo
     &           (incall ,g  ,pacfac,relden,kinvis ,grsize(1,igr,1),
     &            chezy      ,velo1 ,depth ,rs(igr,1)              ,
     &            uscoef (1,ind)    ,trform(1,ibr) ,forcon(1,igr,1),
     &            sedpdu )
c
c              Calculate number of sections in use
c
               nsect = int(asubsc(igr)) + 1
c
c              Calculate celerity including correction for flow in
c              sub sections
c
               call secynb (g     ,igr   ,nsect ,u     ,dvelo ,
     &                      depth ,ibrl  ,sedtr(igr,1) ,sedpdu,maxlev,
     &                      ngrid ,nlev  ,hlev  ,wft   ,ws    ,secths,
     &                      h2    ,q2    ,qs    ,wf    ,wfs   ,af    ,
     &                      afs   ,rs    ,alfab ,juer  ,celer(igr,1) ,
     &                      lrivr ,ker   )
c
               if (ker .ge. warnng) then
                  if (ker .eq. fatal) then
                     goto 100
                  else
                     ibrl = 0
                  endif
               endif
            else
c
c              No sediment transport occured.
c
               celer (igr,1) = 0.
            endif
         endif
c
  10  continue
      return
c
 100  continue
c
      end
