      subroutine SOCONV(ngrid ,epsh  ,epsq  ,h     ,q     ,miniter     ,
     +                  conv  ,juresi,iter  ,epsqrl,qtyp  ,juer ,ker   ,
     +                  flitmx,lconv ,inocon,ibuf ,resbuf,itstat)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SOCONV (SObek CONVergence)
c
c Module description: Routine SOCONV will check whether the water levels
c                     and discharges for the two latest iteration levels
c                     are almost identical.
c
c                     Routine SOCONV checks whether all grid points
c                     satisfy the convergence criteria:
c
c                     h(*) - h(n+1) <= eps_h
c
c                     Q(*) - Q(n+1) <= eps_Q
c                                       +
c                                      eps_Qrel * Qtyp
c
c                     for user selected parameters eps_h , eps_Q and
c                     eps_Qrel
c
c Pre condition:      At least one of the parameters eps_h , eps_Q or
c                     eps_Qrel must ne non-zero.
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  8 conv              O  Switch to indicate convergented solution
c                         = 0 nog niet geconvergeerd
c                         = 1 geconvergeerd
c                         = 2 niet geconvergeerd, alle iteraties
c                             verbruikt en doorgaan
c  3 epsh              I  Convergence criterium for water levels
c  4 epsq              I  Convergence criterium for discharges(absolute)
c 12 epsqrl            I  Convergence criterium for discharges(relative)
c 16 flitmx            I  maximum number of iterationsteps
c  5 h(ngrid)          I  Water level in every grid point at the latest
c                         iteration.
c 19 ibuf1             IO Pointer in circular buffers with residues and
c                         structure data
c 21 ibuf2             IO Pointer in circular buffers with solutions
c 18 inocon            IO Number of timesteps continuated without
c                         convergence
c 10 istep             I  Current time step number (t(n+1)).
c 11 iter              I  -
c 22 itstat            IO Statistical information on iterationsteps
c                         during simulation
c 14 juer              P  -
c  9 juresi            I  Unit number of file residu
c 15 ker               IO Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 17 lconv             I  Flag if simulation should continue without
c                         conververgence if max.number of iterations
c                         is reached
c    miniter           I  minimum number of iterations that will be
c                         carried out
c  1 ngrid             I  Number of grid points in network.
c  6 q(ngrid)          I  Discharge in every grid point at the latest
c                         iteration.
c 13 qtyp              I  Typical value for the discharges (maximum|Q|)
c 20 resbuf(dmbuf1,6)  IO Buffer with latest residues
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c epsequ  EQUal test with interval EPSilon
c error   write an ERROR to the error file.
c getloc  GET LOCation of gridpoint
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: soconv.pf,v $
c Revision 1.18  1999/06/01  13:42:46  kuipe_j
c names in messages substituted + message template
c
c Revision 1.17  1999/03/15  15:03:26  kuipe_j
c Improve Froude file and Dumpfiles
c
c Revision 1.16  1998/12/11  13:10:04  kuipe_j
c improve annotation in dumps
c
c Revision 1.15  1998/06/11  11:47:36  kuipe_j
c Estuary special integrated
c
c Revision 1.14  1997/05/26  07:36:11  kuipe_j
c discharge stop criterium changed
c
c Revision 1.13  1997/01/23  08:30:06  kuipe_j
c Make flow module robust
c
c Revision 1.12  1996/04/12  13:05:56  kuipe_j
c headers, minor changes
c
c Revision 1.11  1996/04/11  08:16:22  kuipe_j
c Kalman module added
c
c Revision 1.10  1995/11/21  11:09:08  kuipe_j
c Added features are: Special morphology output for IVR; Improvement of
c     auxilliary output; Automatic speudo time stepping; general structure
c     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
c     relation); removal of grid points in messages; etc.
c
c Revision 1.9  1995/10/18  09:00:56  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.8  1995/10/11  12:24:12  kuipe_j
c Remove aux output temp
c
c Revision 1.7  1995/09/22  10:03:28  kuipe_j
c variable dimensions, new headers
c
c Revision 1.6  1995/09/12  08:11:27  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.5  1995/08/30  12:37:29  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.4  1995/08/23  14:29:50  overmar
c Lelystad juli 95 ingebracht
c
c Revision 1.3  1995/05/30  09:56:43  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:09:35  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:01  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:39:40  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Include constants for array dimensions
c
      include '../include/sobdim.i'
c
c     Parameters
c
      integer  ngrid ,juresi, iter ,miniter
      integer  juer , ker   ,flitmx
      integer  inocon,ibuf(*),itstat(4)

      real     epsh,
     +         epsq,
     +         epsqrl,
     +         qtyp
c
      double precision h(ngrid,3), q(ngrid,3)
      real     resbuf(dmbuf1,6)
c
      logical  lconv
      integer  conv
c
c     conv = 0 nog niet geconvergeerd
c     conv = 1 geconvergeerd
c     conv = 2 niet geconvergeerd, alle iteraties verbruikt en doorgaan
c
c     Local variables
c
      integer  igp  ,igph  ,igpq  ,juis
      real     afwh ,afwq   ,epsq2
      logical  EPSEQU
      logical  hcrit  , qcrit2
c
c     Include error codes
c
      include '../include/errcod.i'
c
c     In case of fatal error write step annotation as there is probably
c     written a buffer. After that return to caller.
c
      if (ker.eq.fatal) then
         if (ibuf(1).lt.0) then
            resbuf(abs(ibuf(1)),1) = itstat(4)+1
            resbuf(abs(ibuf(1)),2) = iter
         endif
      else
      hcrit  = .not. EPSEQU(epsh,.0,1E-10)
      qcrit2 = .not. EPSEQU(epsq,.0,1E-10) .or.
     +         .not. EPSEQU(epsqrl,.0,1E-10)
      epsq2  = epsq + epsqrl*qtyp
c
      if (EPSEQU(epsq2,.0,1E-10) .and. .not.hcrit ) then
c        Error when qtyp=0 and epsqrl only criterium specified
         ker = fatal
         call sre_error (juer , 'SOCONV no convergence-criterion left'
     +               , eflncc , ker )
      endif
c
      conv   = 1
      igp    = 1
c
c     Determine if convergence has been obtained.
c     Repeat until last gridpoint processed or not convergence
c
      if ( ker .ne. fatal ) then
  10  continue

      if (hcrit .and. abs(h(igp,2) - h(igp,3)) .gt. epsh ) then
         conv = 0
      endif

      if (qcrit2 .and. abs(q(igp,2) - q(igp,3)) .gt. epsq2) then
        conv = 0
      endif
c
      igp = igp + 1
c
      if ((igp .le. ngrid) .and. (conv .eq. 1)) then
         goto 10
      endif
      endif
c
c     Determine maximum of residuals
c
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
  20  continue
c
c     A negative sign means that in Flow in this iteration step
c     buffers are written
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
c      call getloc (igph,ibrh,xh)
c      call getloc (igpq,ibrq,xq)
c
c     Write maximum residuals to file DUMPRES
c
c    FM1DIMP2DO: manage error message
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
c         write(juis,100) itstat(4)+1        
c         call getbrn (ibrh,branam,lbrnam)
c         l1 = max(1,lbrnam-29)
c         l2 = l1+29         
c         write(juis,101) branam(l1:l2),xh,afwh
c         call getbrn (ibrq,branam,lbrnam)
c         l1 = max(1,lbrnam-29)
c         l2 = l1+29 
c         write(juis,102) branam(l1:l2),xq,afwq
c         if (hcrit .and.
c     +        abs(afwh) .ge. epsh ) then
c            write(juis,104) abs(afwh)-epsh
c            write(juis,105) abs(afwh)/epsh
c         endif
c         if (qcrit2 .and.
c     +        abs(afwq) .ge. epsq2 ) then
c            write(juis,106) abs(afwq)-epsq2
c            if (ker.ne.fatal)
cc           fatal means epsq2=0 in this case
c     +      write(juis,107) abs(afwq)/epsq2
c         endif
      endif
c
c     Always a minimum number of iterations will be
c     carried out
c
      if (iter.lt.miniter) then
         if (conv.eq.1) conv = 0
      endif
c
c     Update statistics
c
      if ( conv .gt. 0 .and. ker .ne. fatal ) then
         if ( iter .lt. itstat(1) ) itstat(1) = iter
         if ( iter .gt. itstat(2) ) itstat(2) = iter
         itstat(3) = itstat(3) + iter
         itstat(4) = itstat(4) + 1
      endif
C        WRITE (*,*) 'Timestep=',itstat(4),' Nr iterations=',iter
      endif
c
      return

 100  format (' Time step =',i6)
 101  format (4x,a30,' X=',f10.2,' DH=',f10.5)
 102  format (4x,a30,' X=',f10.2,' DQ=',f10.3)
 104  format (4x,'Waterlevel convergence error=',f10.5)
 105  format (4x,'Waterlevel convergence error factor=',f10.3)
 106  format (4x,'Discharge convergence error=',f10.5)
 107  format (4x,'Discharge convergence error factor=',f10.3)

      end

