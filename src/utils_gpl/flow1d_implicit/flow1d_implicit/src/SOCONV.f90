subroutine SOCONV(ngrid ,epsh  ,epsq  ,h     ,q     ,miniter     ,&
&conv  ,juresi,iter  ,epsqrl,qtyp  ,juer ,ker   ,&
&flitmx,lconv ,inocon,ibuf ,resbuf,itstat)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SOCONV (SObek CONVergence)
!
! Module description: Routine SOCONV will check whether the water levels
!                     and discharges for the two latest iteration levels
!                     are almost identical.
!
!                     Routine SOCONV checks whether all grid points
!                     satisfy the convergence criteria:
!
!                     h(*) - h(n+1) <= eps_h
!
!                     Q(*) - Q(n+1) <= eps_Q
!                                       +
!                                      eps_Qrel * Qtyp
!
!                     for user selected parameters eps_h , eps_Q and
!                     eps_Qrel
!
! Pre condition:      At least one of the parameters eps_h , eps_Q or
!                     eps_Qrel must ne non-zero.
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  8 conv              O  Switch to indicate convergented solution
!                         = 0 nog niet geconvergeerd
!                         = 1 geconvergeerd
!                         = 2 niet geconvergeerd, alle iteraties
!                             verbruikt en doorgaan
!  3 epsh              I  Convergence criterium for water levels
!  4 epsq              I  Convergence criterium for discharges(absolute)
! 12 epsqrl            I  Convergence criterium for discharges(relative)
! 16 flitmx            I  maximum number of iterationsteps
!  5 h(ngrid)          I  Water level in every grid point at the latest
!                         iteration.
! 19 ibuf1             IO Pointer in circular buffers with residues and
!                         structure data
! 21 ibuf2             IO Pointer in circular buffers with solutions
! 18 inocon            IO Number of timesteps continuated without
!                         convergence
! 10 istep             I  Current time step number (t(n+1)).
! 11 iter              I  -
! 22 itstat            IO Statistical information on iterationsteps
!                         during simulation
! 14 juer              P  -
!  9 juresi            I  Unit number of file residu
! 15 ker               IO Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 17 lconv             I  Flag if simulation should continue without
!                         conververgence if max.number of iterations
!                         is reached
!    miniter           I  minimum number of iterations that will be
!                         carried out
!  1 ngrid             I  Number of grid points in network.
!  6 q(ngrid)          I  Discharge in every grid point at the latest
!                         iteration.
! 13 qtyp              I  Typical value for the discharges (maximum|Q|)
! 20 resbuf(dmbuf1,6)  IO Buffer with latest residues
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! epsequ  EQUal test with interval EPSilon
! error   write an ERROR to the error file.
! getloc  GET LOCation of gridpoint
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
! $Log: soconv.pf,v $
! Revision 1.18  1999/06/01  13:42:46  kuipe_j
! names in messages substituted + message template
!
! Revision 1.17  1999/03/15  15:03:26  kuipe_j
! Improve Froude file and Dumpfiles
!
! Revision 1.16  1998/12/11  13:10:04  kuipe_j
! improve annotation in dumps
!
! Revision 1.15  1998/06/11  11:47:36  kuipe_j
! Estuary special integrated
!
! Revision 1.14  1997/05/26  07:36:11  kuipe_j
! discharge stop criterium changed
!
! Revision 1.13  1997/01/23  08:30:06  kuipe_j
! Make flow module robust
!
! Revision 1.12  1996/04/12  13:05:56  kuipe_j
! headers, minor changes
!
! Revision 1.11  1996/04/11  08:16:22  kuipe_j
! Kalman module added
!
! Revision 1.10  1995/11/21  11:09:08  kuipe_j
! Added features are: Special morphology output for IVR; Improvement of
!     auxilliary output; Automatic speudo time stepping; general structure
!     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
!     relation); removal of grid points in messages; etc.
!
! Revision 1.9  1995/10/18  09:00:56  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.8  1995/10/11  12:24:12  kuipe_j
! Remove aux output temp
!
! Revision 1.7  1995/09/22  10:03:28  kuipe_j
! variable dimensions, new headers
!
! Revision 1.6  1995/09/12  08:11:27  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.5  1995/08/30  12:37:29  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.4  1995/08/23  14:29:50  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.3  1995/05/30  09:56:43  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:09:35  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:12:01  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:39:40  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Include constants for array dimensions
!
   include '../include/sobdim.i'
!
!     Parameters
!
   integer  ngrid ,juresi, iter ,miniter
   integer  juer , ker   ,flitmx
   integer  inocon,ibuf(*),itstat(4)

   real     epsh,&
   &epsq,&
   &epsqrl,&
   &qtyp
!
   double precision h(ngrid,3), q(ngrid,3)
   real     resbuf(dmbuf1,6)
!
   logical  lconv
   integer  conv
!
!     conv = 0 nog niet geconvergeerd
!     conv = 1 geconvergeerd
!     conv = 2 niet geconvergeerd, alle iteraties verbruikt en doorgaan
!
!     Local variables
!
   integer  igp  ,igph  ,igpq  ,juis
   real     afwh ,afwq   ,epsq2
   logical  EPSEQU
   logical  hcrit  , qcrit2
!
!     Include error codes
!
   include '../include/errcod.i'
!
!     In case of fatal error write step annotation as there is probably
!     written a buffer. After that return to caller.
!
   if (ker.eq.fatal) then
      if (ibuf(1).lt.0) then
         resbuf(abs(ibuf(1)),1) = itstat(4)+1
         resbuf(abs(ibuf(1)),2) = iter
      endif
   else
      hcrit  = .not. EPSEQU(epsh,.0,1E-10)
      qcrit2 = .not. EPSEQU(epsq,.0,1E-10) .or.&
      &.not. EPSEQU(epsqrl,.0,1E-10)
      epsq2  = epsq + epsqrl*qtyp
!
      if (EPSEQU(epsq2,.0,1E-10) .and. .not.hcrit ) then
!        Error when qtyp=0 and epsqrl only criterium specified
         ker = fatal
         call sre_error (juer , 'SOCONV no convergence-criterion left'&
         &, eflncc , ker )
      endif
!
      conv   = 1
      igp    = 1
!
!     Determine if convergence has been obtained.
!     Repeat until last gridpoint processed or not convergence
!
      if ( ker .ne. fatal ) then
10       continue

         if (hcrit .and. abs(h(igp,2) - h(igp,3)) .gt. epsh ) then
            conv = 0
         endif

         if (qcrit2 .and. abs(q(igp,2) - q(igp,3)) .gt. epsq2) then
            conv = 0
         endif
!
         igp = igp + 1
!
         if ((igp .le. ngrid) .and. (conv .eq. 1)) then
            goto 10
         endif
      endif
!
!     Determine maximum of residuals
!
      afwh = 0.
      afwq = 0.
      igph = 1
      igpq = 1
      do 20 igp=1,ngrid
         if (abs(h(igp,2) - h(igp,3)).gt.abs(afwh))then
            afwh = h(igp,2) - h(igp,3)
            igph = igp
         endif
         if (abs(q(igp,2) - q(igp,3)).gt.abs(afwq))then
            afwq = q(igp,2) - q(igp,3)
            igpq = igp
         endif
20    continue
!
!     A negative sign means that in Flow in this iteration step
!     buffers are written
      ibuf(1) = abs(ibuf(1))
      resbuf(ibuf(1),1) = itstat(4)+1
      resbuf(ibuf(1),2) = iter
      resbuf(ibuf(1),3) = afwh
      resbuf(ibuf(1),4) = igph
      resbuf(ibuf(1),5) = afwq
      resbuf(ibuf(1),6) = igpq
      ibuf(1) = ibuf(1) + 1
      if (ibuf(1) .eq. 21) ibuf(1) = 1
      ibuf(2) = ibuf(2) + 1
      if (ibuf(2) .eq. 5) ibuf(2) = 1
!      call getloc (igph,ibrh,xh)
!      call getloc (igpq,ibrq,xq)
!
!     Write maximum residuals to file DUMPRES
!
!    FM1DIMP2DO: manage error message
      if ( iter .ge. flitmx .and. conv .eq. 0) then
         if ( lconv ) then
            inocon  = inocon + 1
            conv    = 2
            juis    = juresi
            ibuf(4) = inocon
         else
            ker = fatal
            call sre_error (juer,'SOFLOW No convergence', eflncv, ker)
            juis = juer
         endif
!         write(juis,100) itstat(4)+1
!         call getbrn (ibrh,branam,lbrnam)
!         l1 = max(1,lbrnam-29)
!         l2 = l1+29
!         write(juis,101) branam(l1:l2),xh,afwh
!         call getbrn (ibrq,branam,lbrnam)
!         l1 = max(1,lbrnam-29)
!         l2 = l1+29
!         write(juis,102) branam(l1:l2),xq,afwq
!         if (hcrit .and.
!     +        abs(afwh) .ge. epsh ) then
!            write(juis,104) abs(afwh)-epsh
!            write(juis,105) abs(afwh)/epsh
!         endif
!         if (qcrit2 .and.
!     +        abs(afwq) .ge. epsq2 ) then
!            write(juis,106) abs(afwq)-epsq2
!            if (ker.ne.fatal)
!c           fatal means epsq2=0 in this case
!     +      write(juis,107) abs(afwq)/epsq2
!         endif
      endif
!
!     Always a minimum number of iterations will be
!     carried out
!
      if (iter.lt.miniter) then
         if (conv.eq.1) conv = 0
      endif
!
!     Update statistics
!
      if ( conv .gt. 0 .and. ker .ne. fatal ) then
         if ( iter .lt. itstat(1) ) itstat(1) = iter
         if ( iter .gt. itstat(2) ) itstat(2) = iter
         itstat(3) = itstat(3) + iter
         itstat(4) = itstat(4) + 1
      endif
!        WRITE (*,*) 'Timestep=',itstat(4),' Nr iterations=',iter
   endif
!
   return

100 format (' Time step =',i6)
101 format (4x,a30,' X=',f10.2,' DH=',f10.5)
102 format (4x,a30,' X=',f10.2,' DQ=',f10.3)
104 format (4x,'Waterlevel convergence error=',f10.5)
105 format (4x,'Waterlevel convergence error factor=',f10.3)
106 format (4x,'Discharge convergence error=',f10.5)
107 format (4x,'Discharge convergence error factor=',f10.3)

end

