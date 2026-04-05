subroutine senorb (nbran  ,ngrid  ,maxlev ,g      ,pacfac ,relden,&
&kinvis ,ibr    ,juer   ,branch ,bfrict ,engpar,&
&nucoef ,uscoef ,trform ,prslot ,af     ,afs   ,&
&wf     ,wfs    ,nlev   ,hlev   ,wft    ,ws    ,&
&secths ,rs     ,cs     ,asubsc ,alfab  ,q2    ,&
&qs     ,h2     ,grsize ,forcon ,sedtr  ,celer ,&
&lrivr  ,psltvr ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SENORB (SEdiment NORmal Branch)
!
! Module description: Calculation of the sediment transport in a normal
!                     branch.
!
!                     First the sediment transport is calculated in
!                     every gridpoint of the main section (subroutine
!                     SETRFO). After that the celerity is calculated
!                     with subroutine SETRCY. In this routine the cele-
!                     rity is calculated by numerical differentiation of
!                     the transport. Therefore, in case of the Engelund
!                     roughness predictor the Chezy value must also be
!                     calculated for U+.001 (subroutine ENGRPR).
!
!
!                     If the Preissmann slot is present and the water
!                     level drops below the original bottom the sediment
!                     transport and celerity are set to zero.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 17 af                P  -
! 18 afs(ngrid,2)      I  Actual flow area per section:
!                         (i,1) = For main section of grid point i.
!                         (i,2) = For sub section 1 of grid point i.
! 29 alfab             P  -
! 28 asubsc(ngrid)     I  Defines the actual number of sub sections for
!                         avery cross section (depending on the actual
!                         water level):
!                         c1sec (0) : Main section only (0 sub sections)
!                         c2sec (1) : 1 sub section
!                         c3sec (2) : 2 sub sections
! 12 bfrict(3,nbran)   I  Bed friction in sections of branch.
!                         For each branch and section a different fric-
!                         tion type can be defined.
!                         (1,i) = Friction type in main section in
!                                 branch i:
!                                 cfrchc (1) : Chezy constant
!                                 cfrchq (2) : Chezy function of Q
!                                 cfrchh (3) : Chezy function of h
!                                 cfrman (4) : Manning constant
!                                 cfrskn (5) : Strickler 1 constant Kn
!                                 cfrsks (6) : Strickler 2 constant Ks
!                                 cfrnik (7) : Nikuradze constant
!                                 cfreng (8) : Engelund predictor
!                         (2,i) = Friction type in sub section 1 in
!                                 branch i:
!                                 cfrchc (1) : Chezy constant
!                                 cfrman (4) : Manning constant
!                                 cfrskn (5) : Strickler 1 constant Kn
!                                 cfrsks (6) : Strickler 2 constant Ks
!                                 cfrnik (7) : Nikuradze constant
!                         (3,i) = Friction type in sub section 2 in
!                                 branch i:
!                                 Same definitions as bfrict(2,i)
! 11 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 36 celer(ngrid,*)    O  Sediment celerity for each gridpoint.
!               1|2       - Normal branches:
!                         (i,1) = Celerity in gridpoint i in main secti-
!                                 on.
!                         - Sedredge branches:
!                         (i,1) = Celerity in gridpoint i,
!                                 left channel.
!                         (i,2) = Celerity in gridpoint i,
!                                 right channel.
! 27 cs(ngrid,3)       I  Actual Chezy coefficient in a section (main,
!                         sub 1, sub 2) for every grid point.
! 13 engpar            P  -
! 34 forcon            P  -
!  4 g                 P  -
! 33 grsize            P  -
! 32 h2(ngrid)         I  Water level in every grid point at time
!                         t(n+1).
! 22 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
!  9 ibr               I  Number of actual branch.
! 10 juer              P  -
! 37 ker               I  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  8 kinvis            P  -
!  3 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  1 nbran             I  Number of branches.
!  2 ngrid             I  Number of grid points in network.
! 21 nlev              P  -
!  6 pacfac            P  -
! 16 prslot(3,nbran)   I  Contains information concerning Preissmann
!                         slot (assuring positive water depths).
!                         (1,i) = Create slot indicator
!                                 csldis (0) : No slot in this branch.
!                                 cslena (1) : Create slot in branch.
!                         (2,i) = Reference area for slot generation.
!                         (3,i) = Depth of slot below lowest bottom of
!                                 branch.
! 38 psltvr(7,ngrid)   I  Preissmann slot variables for every grid point
!                         i (assuring positive water depths):
!                         (1,i) = Value for C**2*R for positive flow.
!                         (2,i) = Value for C**2*R for negative flow.
!                         (3,i) = Bottom of slot (funnel)
!                         (4,i) = Division level between trapezium and
!                                 rectangle of slot (top of rectangle
!                                 and bottom of trapezium)
!                         (5,i) = Top of slot
!                         (6,i) = Bottom width of slot (width of
!                                 rectangle)
!                         (7,i) = Top width of slot (top of trapezium)
! 30 q2                P  -
! 31 qs(ngrid,2)       I  Flow in every grid point per section:
!                         (i,1) = Through grid point i of main channel.
!                         (i,2) = Through grid point i of sub section 1.
!  7 relden            P  -
! 26 rs                P  -
! 25 secths            P  -
! 35 sedtr(ngrid,*)    O  Sediment transport results for each gridpoint.
!               1|2       (At first transports per unit width, finaly
!                         total transports)
!                         - Normal branches:
!                         (i,1) = Sediment transport in gridpoint i of
!                                 main section.
!                         - Sedredge branches:
!                         (i,1) = Sediment transport in gridpoint i,
!                                 left channel.
!                         (i,2) = Sediment transport in gridpoint i,
!                                 right channel.
!                         (i,3) = Sediment exchange in gridpoint i.
! 15 trform(3,nbran)   I  Defines for each branch a transport formula:
!                         (1,j) = Transport formula number:
!                                 ctrfeh (1) : Engelund & Hansen
!                                 ctrfmm (2) : Meyer-Peter & Muller
!                                 ctrfaw (3) : Ackers & White
!                                 ctrfvr (4) : Van Rijn
!                                 ctrfpk (5) : Parker & Klingeman
!                                 ctrfud (6) : User Defined Formula
!                         (2,j) = Starting index of the user coeffi-
!                                 cients for this branch.
!                         (3,j) = Calibration factor for transport for-
!                                 mula.
! 14 uscoef            P  -
! 19 wf                P  -
! 20 wfs(ngrid,2)      I  Actual flow width per section:
!                         (i,1) = For main section of grid point i.
!                         (i,2) = For sub section 1 of grid point i.
! 23 wft               P  -
! 24 ws                P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! engrpr  ENGelund Roughness PRedictor
! equal   EQUAL test of two real variables
! secynb  SEdiment CeleritY Normal Branch
! setrfo  SEdiment TRansport FOrmula
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: senorb.pf,v $
! Revision 1.7  1998/06/11  11:47:29  kuipe_j
! Estuary special integrated
!
! Revision 1.6  1998/02/13  12:09:58  kuipe_j
! Avoid pointer toempty array
!
! Revision 1.5  1997/01/23  08:29:59  kuipe_j
! Make flow module robust
!
! Revision 1.4  1995/10/18  09:00:42  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.3  1995/05/30  09:56:30  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:07:29  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:28  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:35:01  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:22  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   include '..\include\sobdim.i'
!
!     Declaration of parameters
!
   integer    nbran ,ngrid     ,maxlev  ,juer  ,ibr  ,ker ,nucoef
   integer    branch(4,nbran)  ,bfrict(3,nbran)  ,nlev(ngrid)
   real       g     ,pacfac    ,relden  ,kinvis
   real       uscoef(nrcoefs,nucoef),trform(3,nbran)     ,&
   &engpar(9)             ,prslot(3,nbran)     ,&
   &af    (ngrid)         ,afs   (ngrid,2 )    ,&
   &wf    (ngrid)         ,wfs   (ngrid,2)     ,&
   &rs    (ngrid,3)       ,cs    (ngrid,3)     ,&
   &qs    (ngrid,2)       ,alfab (ngrid)       ,&
   &wft   (ngrid,maxlev)  ,ws    (ngrid)         ,&
   &asubsc(ngrid)         ,secths(ngrid)       ,&
   &grsize(4,ngrid,*)     ,forcon(4,ngrid,*)   ,&
   &sedtr (ngrid,*)       ,celer (ngrid,*)     ,&
   &psltvr(7,ngrid)
   double precision hlev  (ngrid,maxlev), h2(ngrid), q2(ngrid)
!
!     Declaration of local variables
!
   integer    igr   ,ibrl ,ind  ,nsect
   real       depth ,u    ,u1   ,chezy
   real       velo  ,velo1,dvelo,sedpdu
   logical    equal, lwet, incall , lrivr
   external   equal
!
!     Constants
!
   real       diff
   parameter  (diff=.001)
   integer    d90
   parameter  (d90=3)
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
   include '..\include\sobcon.i'
!
   incall = .false.
!
   ind   = max(int(trform(2,ibr)),1)
   ibrl  = ibr

   do 10 igr = branch(3,ibr),branch(4,ibr)
!
      if (int(prslot(1,ibr)) .eq. cslena) then
         if (h2(igr) .lt. psltvr(5,igr) ) then
!
!              Dry point in case of Preisman slot.
!
            sedtr(igr,1) = 0.
            celer(igr,1) = 0.
            lwet         = .false.
         else
            lwet         = .true.
         endif
      else
         lwet = .true.
      endif
!
      if (lwet) then
!
!           Wet point.
!           Calculate velocity and depth.
!
         u     = qs(igr,1) / afs(igr,1)
         depth = afs(igr,1) / wfs(igr,1)
!
!           Calculate sediment transport.
!
         call setrfo&
         &(incall    ,g   ,pacfac  ,relden,kinvis,&
         &grsize(1,igr,1),cs(igr,1)   ,u ,depth ,rs(igr,1),&
         &uscoef(1,ind)  ,trform(1,ibr)  ,forcon(1,igr,1) ,&
         &sedtr (igr,1)  )
!
!           Calculate celerity by numerical differentiation,
!           if transport is not zero.
!
         if (.not.equal(sedtr(igr,1),0.)) then
            if (bfrict(1,ibr) .eq. cfreng) then
               u1 = abs(u) * (1.+diff)
!
!                 In case of the Engelund roughness predictor the Chezy
!                 value is velocity dependent.
!
               call engrpr(engpar    ,grsize(d90,igr,1) ,u1 ,&
               &rs(igr,1) ,chezy )
            else
               chezy = cs(igr,1)
            endif
!
!              Calculate |u+du|
!
            velo  = abs(u)
            dvelo = velo * diff
            velo1 = velo + dvelo
!
!              Calculate sediment transport based on u+du
!
            call setrfo&
            &(incall ,g  ,pacfac,relden,kinvis ,grsize(1,igr,1),&
            &chezy      ,velo1 ,depth ,rs(igr,1)              ,&
            &uscoef (1,ind)    ,trform(1,ibr) ,forcon(1,igr,1),&
            &sedpdu )
!
!              Calculate number of sections in use
!
            nsect = int(asubsc(igr)) + 1
!
!              Calculate celerity including correction for flow in
!              sub sections
!
            call secynb (g     ,igr   ,nsect ,u     ,dvelo ,&
            &depth ,ibrl  ,sedtr(igr,1) ,sedpdu,maxlev,&
            &ngrid ,nlev  ,hlev  ,wft   ,ws    ,secths,&
            &h2    ,q2    ,qs    ,wf    ,wfs   ,af    ,&
            &afs   ,rs    ,alfab ,juer  ,celer(igr,1) ,&
            &lrivr ,ker   )
!
            if (ker .ge. warnng) then
               if (ker .eq. fatal) then
                  goto 100
               else
                  ibrl = 0
               endif
            endif
         else
!
!              No sediment transport occured.
!
            celer (igr,1) = 0.
         endif
      endif
!
10 continue
   return
!
100 continue
!
end
