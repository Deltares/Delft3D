subroutine sesedb (nbran  ,nnode  ,ngrid  ,maxlev ,maxtab ,ntabm ,&
&time   ,g      ,pacfac ,relden ,kinvis ,ibr   ,&
&juer   ,branch ,node   ,sdrdbf ,sedinf ,ntab  ,&
&table  ,e      ,rc     ,nucoef ,uscoef ,trform,&
&afs    ,wf     ,wfs    ,hlev   ,rs     ,cs    ,&
&x      ,q2     ,qs     ,h2     ,grsize ,forcon,&
&sedtr  ,celer  ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SESEDB (SEdiment SEDredge Branch)
!
! Module description: Calculation of the sediment transport in a Sedred-
!                     ge branch.
!
!                     First the flow distribution over the left and
!                     right channel is calculated. For the first upstre-
!                     am gridpoint routine SEDFL1 is called because the
!                     flow is calculated starting from either a boundary
!                     condition or a nodal distribution. For all other
!                     gridpoints SEDFL2 is called which uses the flow
!                     distribution of the first upstream grid point.
!
!                     After this, for both channels the sediment trans-
!                     port and celerity are calculated by calling SETRFO
!                     and SETRCY. Sediment transports and celerities are
!                     corrected in routine SESCED for longitudinal bed
!                     slope effects. The next step is the calculation of
!                     the sediment exchange between the two channels.
!                     This includes transport calculation in the exchan-
!                     ge region (SETRFO) and the calculation of the
!                     direction (SEALPH).
!
!                     It is required that the flow direction in a se-
!                     dredge branch does not change!
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 24 afs(ngrid,2)      I  Actual flow area per section:
!                         (i,1) = For main section of grid point i.
!                         (i,2) = For sub section 1 of grid point i.
! 14 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 37 celer(ngrid,*)    O  Sediment celerity for each gridpoint.
!               1|2       - Normal branches:
!                         (i,1) = Celerity in gridpoint i in main secti-
!                                 on.
!                         - Sedredge branches:
!                         (i,1) = Celerity in gridpoint i,
!                                 left channel.
!                         (i,2) = Celerity in gridpoint i,
!                                 right channel.
! 29 cs(ngrid,3)       I  Actual Chezy coefficient in a section (main,
!                         sub 1, sub 2) for every grid point.
! 20 e                 P  -
! 35 forcon            P  -
!  8 g                 P  -
! 34 grsize(4,ngrid,*) I  Grain sizes for each gridpoint and section.
!                  1|2    Depending on the transport formula chosen one
!                         or more of the grain sizes will be defined.
!                         When a branch has been marked as a sedredge
!                         branch the D50 value should always be defined,
!                         no matter what transport formula has been
!                         chosen. For normal branches only section 1
!                         will be used being the D size in the main and
!                         sub sections. For sedredge branches section 1
!                         will be the D size in the left channel and
!                         section 2 will be the D size in the right
!                         channel.
!                         (1,i,j) =     D35 value for section j, grid-
!                                       point i.
!                         (2,i,j) =     D50 value for section j, grid-
!                                       point i.
!                         (3,i,j) =     D90 value for section j, grid-
!                                       point i.
!                         (4,i,j) =     Dmedium value for section j,
!                                       gridpoint i.
! 33 h2(ngrid)         I  Water level in every grid point at time
!                         t(n+1).
! 27 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
! 12 ibr               I  Number of actual branch.
! 13 juer              P  -
! 38 ker               I  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 11 kinvis            P  -
!  4 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  5 maxtab            I  Maximum number of defined tables.
!  1 nbran             I  Number of branches.
!  3 ngrid             I  Number of grid points in network.
!  2 nnode             I  Number of nodes.
! 15 node              P  -
! 18 ntab              P  -
!  6 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
!  9 pacfac            P  -
! 31 q2                P  -
! 32 qs(ngrid,2)       I  Flow in every grid point per section:
!                         (i,1) = Through grid point i of main channel.
!                         (i,2) = Through grid point i of sub section 1.
! 21 rc                P  -
! 10 relden            P  -
! 28 rs(ngrid,3)       I  Actual hydraulic radius in a section (main,
!                         sub 1, sub 2) for every grid point.
! 16 sdrdbf            P  -
! 17 sedinf(2,nbran)   I  Defines for each branch whether a branch is a
!                         sedredge branch or not.
!                         (1,j) = Sedredge branch number (1..nsedrd)
!                                 else 0.
!                         (2,j) = Starting index in Rc array (River bend
!                                 curvature).
! 36 sedtr(ngrid,*)    IO Sediment transport results for each gridpoint.
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
! 19 table             P  -
!  7 time              P  -
! 23 trform(3,nbran)   I  Defines for each branch a transport formula:
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
! 22 uscoef            P  -
! 25 wf                P  -
! 26 wfs               P  -
! 30 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! equal   EQUAL test of two real variables
! sealph  SEdiment ALPHa calculation
! secsed  SEdiment Correct SEDiment transport
! secysb  SEdiment CeleritY Sedredge Branch
! sedfl1  SEdiment Distribute FLow on upstream point 1
! sedfl2  SEdiment Distribute FLows 2
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
! $Log: sesedb.pf,v $
! Revision 1.5  1999/03/15  13:47:32  kuipe_j
! coordinate in message
!
! Revision 1.4  1995/10/18  09:00:44  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.3  1995/05/30  09:56:31  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:07:30  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:29  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:47:40  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:35:04  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:22  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
   include '..\include\sobdim.i'
!
!     Declaration of parameters
!
   integer    nbran  ,nnode  ,ngrid  ,maxlev ,juer ,nucoef ,&
   &maxtab ,ntabm  ,ibr    ,ker
   integer    branch(4,nbran)  ,node  (4,nnode)  ,sdrdbf(2,*)      ,&
   &sedinf(2,nbran)  ,ntab  (4,maxtab)
   real       g         ,pacfac,relden    ,kinvis
   real       table (ntabm)    ,rc    (*)        ,e     (7,*)      ,&
   &uscoef(nrcoefs,nucoef)             ,trform(3,nbran)  ,&
   &afs   (ngrid,2 ) ,wf    (ngrid)    ,wfs   (ngrid,2)  ,&
   &rs(ngrid,3)  ,x     (ngrid)    ,&
   &cs    (ngrid,3)  ,qs    (ngrid,2)  ,&
   &grsize(4,ngrid,*),forcon(4,ngrid,*),&
   &sedtr (ngrid,*)  ,celer (ngrid,*)
   double     precision   time, hlev  (ngrid,maxlev)
   double     precision   h2(ngrid), q2(ngrid)
!
!     Declaration of local parameters
!
   integer    igr   ,ig1  ,ig2   ,igp  ,igm  ,linc  ,ibrl ,ind   ,&
   &irc   ,isec ,i     ,ised
   real       depth ,u    ,d12  ,u12  ,c12  ,r12
   real       grn(4),factor,velo,velo1,sedpdu,dvelo
   logical    incall
!
   double precision tanalf
!
   logical    equal
   external   equal
!
!     Constants
!
   integer    d50
   real       diff
   parameter  (d50  = 2)
   parameter  (diff = .001)
!
   incall = .false.
!
!     Calculate Q distribution using boundary condition or
!     flow distribution function on first gridpoint in branch
!     with inflow.
!
   call sedfl1 (ibr    ,nbran  ,nnode  ,ngrid  ,maxtab ,ntabm ,&
   &juer   ,time   ,branch ,node   ,sedinf ,sdrdbf,&
   &ntab   ,table  ,q2     ,qs     ,linc   ,ker   )
!
   if (ker .eq. fatal) goto 100
!
   call sedfl2 (ibr    ,linc   ,nbran  ,ngrid  ,maxlev ,g     ,&
   &branch ,afs    ,wfs    ,hlev   ,x      ,h2    ,&
   &q2     ,cs     ,rs     ,qs     )
!
!     Loop through branch.
!
   ig1  = branch(3,ibr)
   ig2  = branch(4,ibr)
   ind  = max(int(trform(2,ibr)),1)
   ibrl = ibr
   irc  = sedinf(2,ibr) - 1
   ised = sedinf(1,ibr)
!
!     ISED = Sedredge branch number
!     IRC  = Index in RC-array of current grid point
!
   do 30 igr = ig1,ig2
!
!        Calculate sediment transport through channels 1 and 2.
!
      irc = irc + 1
      igp = min(igr+1,ig2)
      igm = max(igr-1,ig1)
      u12 = 0.
      d12 = 0.
      do 10 isec = 1,2
!
!           Calculate velocity and depth
!
         u     = qs(igr,isec) / afs(igr,isec)
         depth = h2(igr) - hlev(igr,isec)
!
!           Calculate correction factor
!
         call secsed (isec  ,igp     ,igm  ,ngrid    ,maxlev,relden ,&
         &grsize(d50,igr,isec) ,e(1,ised),cs(igr,isec)  ,&
         &u     ,depth   ,hlev ,x        ,factor        )

!
!           Calculate sediment transport.
!
         call setrfo&
         &(incall ,g  ,pacfac,relden,kinvis ,grsize(1,igr,isec),&
         &cs     (igr,isec) ,u     ,depth  ,rs(igr,isec)      ,&
         &uscoef (1,ind)    ,trform(1,ibr) ,forcon(1,igr,isec),&
         &sedtr  (igr,isec) )
!
!           Multiply sediment transport with correction factor
!
         sedtr(igr,isec) = sedtr(igr,isec) * factor
!
!           Calculate celerity by numerical differentiation.
!
         if (.not.equal(sedtr(igr,isec),0.)) then
!
!              Calculate |u+du|
!
            velo  = abs(u)
            dvelo = velo * diff
            velo1 = velo + dvelo
!
!              Calculate correction factor
!
            call secsed&
            &(isec  ,igp     ,igm  ,ngrid    ,maxlev,relden ,&
            &grsize(d50,igr,isec) ,e(1,ised),cs(igr,isec)  ,&
            &velo1 ,depth   ,hlev ,x        ,factor        )

!
!              Calculate sediment transport based on u+du
!
            call setrfo&
            &(incall ,g  ,pacfac,relden,kinvis ,grsize(1,igr,isec),&
            &cs     (igr,isec) ,velo1 ,depth  ,rs(igr,isec)      ,&
            &uscoef (1,ind)    ,trform(1,ibr) ,forcon(1,igr,isec),&
            &sedpdu )
!
!              Multiply s with correction factor
!
            sedpdu = sedpdu * factor
!
!              Calculate celerity for sedredge branch
!
            call  secysb (g     ,u     ,dvelo ,depth ,ibrl  ,juer  ,&
            &igr   ,sedtr(igr,isec)     ,sedpdu,&
            &celer(igr,isec)     ,ker   )
!
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
!
         u12 = u12 + u
         d12 = d12 + depth
!
10    continue
!
!        Calculate exchange of transport between the 2 channels.
!        At First calculate avaraged parameters.
!        [Doc. S-FO-002.2KV  Eq. 6.13 ]
!
      u12 = u12 *.5
      d12 = d12 *.5
      c12 = sqrt(2. / (1./cs(igr,1)**2 + 1./cs(igr,2)**2))
      r12 = (rs(igr,1) + rs(igr,2)) *.5
!
      do 20 i = 1, 4
         grn(i) = (grsize(i,igr,1) + grsize(i,igr,2)) *.5
20    continue
!
!        Calculate longitudinal sediment transport in exchange region.
!
      call setrfo (incall        ,g     ,pacfac  ,relden,kinvis  ,&
      &grn   ,c12    ,u12   ,d12     ,r12   ,&
      &uscoef(1,ind) ,trform(1,ibr)  ,forcon(1,igr,3),&
      &sedtr (igr,3) )
!
!        Calculate the (tangent of) angle of transport in exchange
!        region.
!
      call sealph (igr    ,igp   ,igm   ,ngrid   ,maxlev   ,g     ,&
      &relden ,u12   ,c12   ,grn(d50),rc(irc)  ,x     ,&
      &wf     ,hlev  ,h2    ,qs      ,e(1,ised),tanalf)
!
!        Store exchange transport.
!
      sedtr(igr,3) = abs(sedtr(igr,3)) * real(tanalf)
!
30 continue
   return
!
100 continue
end
