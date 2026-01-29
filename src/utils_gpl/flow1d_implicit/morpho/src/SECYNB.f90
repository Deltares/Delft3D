subroutine secynb (g      ,igp    ,nsect  ,u      ,dvelo ,&
&depth  ,ibrl   ,sedtra ,sedpdu ,maxlev ,ngrid ,&
&nlev   ,hlev   ,wft    ,ws     ,secths ,h2    ,&
&q2     ,qs     ,wf     ,wfs    ,af     ,afs   ,&
&rs     ,alfab  ,juer   ,celeri ,lrivr  ,ker   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SECYNB (SEdiment CeleritY Normal Branch)
!
! Module description: Calculate celerity for a normal branch
!
!                     The Froude number is calculated and tested for
!                     extreme values. The celerity is calculated using
!                     numerical differentiation. For this the transport
!                     on U+.001 is calculated. The celerity is adapted
!                     if parts of another section are included in the
!                     sediment transporting channel.
!
! Precondition:       Froude number < 1.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 23 af(ngrid)         I  Flow area at every grid point at time t(n+1)
! 24 afs(ngrid,2)      I  Actual flow area per section:
!                         (i,1) = For main section of grid point i.
!                         (i,2) = For sub section 1 of grid point i.
! 26 alfab(ngrid)      I  Actual Bousinessq coefficient in grid point i.
! 28 celeri            IO celerity
!  7 depth             I  avarage depth
!  6 dvelo             I  Difference in velocity u+du
!  1 g                 I  Acceleration of gravity.
! 18 h2                P  -
! 14 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
!  8 ibrl              IO Number of actual branch. Will be set to zero
!                         when a message is generated.
!  3 igp               I  igr+1 (at branch end = igr)
! 27 juer              P  -
! 29 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 11 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
! 12 ngrid             I  Number of grid points in network.
! 13 nlev(ngrid)       I  Number of h-levels for every cross section.
!                         (For a circle cross section   : 1 ;
!                          For a sedredge cross section : 2 )
!  4 nsect             IO Actual number of sections in use
!                         1 = Main section
!                         2 = Main + sub 1 section
!                         3 = Main + sub 1 + sub 2 section
! 19 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
! 20 qs(ngrid,2)       I  Flow in every grid point per section:
!                         (i,1) = Through grid point i of main channel.
!                         (i,2) = Through grid point i of sub section 1.
! 25 rs(ngrid,3)       I  Actual hydraulic radius in a section (main,
!                         sub 1, sub 2) for every grid point.
! 17 secths            P  -
! 10 sedpdu            I  sediment transport as f(u+du)
!  9 sedtra            I  calculated sediment transport
!  5 u                 I  velocity
! 21 wf(ngrid)         I  Actual flow width at every grid point.
! 22 wfs(ngrid,2)      I  Actual flow width per section:
!                         (i,1) = For main section of grid point i.
!                         (i,2) = For sub section 1 of grid point i.
! 15 wft(ngrid,maxlev) I  (i,j) = flow width at h = hlev(i,j) for grid
!                                 point i.
!                         - For a circle cross section:
!                         (i,1) = Radius of the circle.
!                         - For a sedredge cross section:
!                         (i,1) = Width of main section (i.e. left chan-
!                                 nel).
!                         (i,2) = Width of sub section 1 (i.e. right
!                                 channel).
! 16 ws(ngrid)         I  Sediment transporting width for each grid
!                         point.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! epsequ  EQUal test with interval EPSilon
! error   write an ERROR to the error file.
! indwgh  Compute INDex and WeiGHt factor
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: secynb.pf,v $
! Revision 1.6  1999/06/01  13:42:39  kuipe_j
! names in messages substituted + message template
!
! Revision 1.5  1999/03/15  13:47:29  kuipe_j
! coordinate in message
!
! Revision 1.4  1998/06/11  11:47:24  kuipe_j
! Estuary special integrated
!
! Revision 1.3  1995/05/30  09:56:26  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:07:14  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:16  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/12/02  12:50:56  kuipe_j
! Test concerning actual number of sections was wrong.
!
! Revision 1.2  1993/11/26  15:34:38  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:19  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer      igp    ,nsect  ,maxlev ,ngrid  ,juer   ,ibrl   ,&
   &ker
!
   real         g      ,u      ,dvelo  ,depth  ,sedtra ,&
   &sedpdu ,celeri
!
   integer      nlev   (ngrid)
!
   real         wft    (ngrid,maxlev)  ,&
   &ws     (ngrid)         ,secths (ngrid)         ,&
   &qs     (ngrid,2)       ,wf     (ngrid)         ,&
   &wfs    (ngrid,2)       ,af     (ngrid)         ,&
   &afs    (ngrid,2)       ,rs     (ngrid,3)       ,&
   &alfab  (ngrid)
!
   double precision hlev (ngrid,maxlev), h2(ngrid), q2(ngrid)
!
!     Declaration of local variables
!
   real         velo   ,frou2  ,dsdu   ,wsact  ,wj     ,wjp1   ,&
   &wsum   ,term1  ,term2  ,gamma  ,dw     ,&
   &dz     ,sdelta ,sumws1 ,sumws2 ,ws1    ,ws2    ,&
   &qf     ,uf     ,uiouf  ,unouf  ,umouf  ,rmgam  ,&
   &f      ,he1    ,wsacth
!
   double precision wght
!
   integer      j      ,isect, ibrd, lbrnam
!
   real         asect(3), qsect(3), usect(3), wsect(3),xc
!
   character*40 branam
   character*5  txt
   character*11 xtxt
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
   logical      EPSEQU
   external     EPSEQU
!
   logical      lrivr
!
   integer iostat
   logical exist
   real froumx, froum
!
!     Determine froude^2
!
   velo  = abs(u)
   frou2 = velo*velo / (g*depth)
!
!     Only check when river morphology
!
   if (lrivr) then
      if (frou2 .gt. .64) then
!+++++++++++++++++++++++++++++++
! start rev 1.3:
!+++++++++++++++++++++++++++++++
         froumx = .98
         inquire(file='../FROUDMAX',iostat=iostat,exist=exist)
         if (iostat.eq.0 .and.  exist) then
            open(98,file='../FROUDMAX',iostat=iostat,status='old')
            if (iostat.eq.0) then
               read(98,'(f10.0)',iostat=iostat)froum
            endif
            if (iostat.eq.0) then
               froumx = max(froumx,froum)
            endif
            close (98)
         endif
         if (frou2 .gt. froumx) then
            write(*,*)'+++SECYNB froumx**2=',froumx,&
            &' exceeded, froude**2 value=',frou2
!-orig      if (frou2 .gt. .98) then
!+++++++++++++++++++++++++++++++
! end rev 1.3:
!+++++++++++++++++++++++++++++++
            ker = fatal
            call getloc (igp,ibrd,xc)
            write (txt,'(f5.2)') sqrt(frou2)
            call getbrn (ibrd,branam,lbrnam)
            write (xtxt,'(f10.2)') xc
            call error (juer,&
            &'SECYNB Froude number too large(@'//txt//&
            &'@) in branch @'//branam(:lbrnam)//'@ at X= @'&
            &//xtxt//'@',esefro,ker)
            goto 1000
         else if (ibrl .ne. 0) then
!
!              Only one message will be given in a branch.
!
            ker = warnng
            call getloc (igp,ibrd,xc)
            call getbrn (ibrd,branam,lbrnam)
            write (xtxt,'(f10.2)') xc
            call error (juer,&
            &'SECYNB Froude numbers > .8 in branch @'&
            &//branam(:lbrnam)//'@ starting at X=@'//&
            &xtxt//'@',esefrw,ker)
            ibrl = 0
         endif
      endif
   endif
!
   dsdu = (sedpdu - abs(sedtra)) / dvelo
!
!     Calculate actual sediment transporting width
!
   if (wf(igp) .lt. ws(igp)) then
      wsact  = wf(igp)
      wsacth = h2(igp)
   else
      wsact  = ws(igp)
      wsacth = secths(igp)
   endif
!
!     Determine gamma for water level Wmain, Wj and Wj+1
!
   call INDWGH ( ngrid  ,igp    ,maxlev ,nlev   ,hlev   ,&
   &dble(wsacth)   ,j      ,wght   )
!
   if (j .eq. -1) then
      gamma = 2.
!
      wj    = wft(igp,nlev(igp))
      wjp1  = wj
   else
      dz = hlev(igp,j+1) - hlev(igp,j)
      dw = wft (igp,j+1) - wft (igp,j)
      gamma = ( 2. * dz ) / sqrt ( (dw/2.)**2 + dz**2 )
!
      wj   = wft(igp,j)
      if (j .eq. nlev(igp)) then
         wjp1  = wj
      else
         wjp1 = wft(igp,j+1)
      endif
   endif
!
!     Add Wj and Wj+1
!
   wsum = wj + wjp1
!
!     Check if A last section(s) is not too small
!
   if (nsect .gt. 1) then
      asect(1) = afs(igp,1)
      asect(2) = afs(igp,2)
      if (nsect .eq. 3) then
         asect(3) = af(igp) - asect(1) - asect(2)
!
         if (epsequ ( asect(3)/af(igp) , 0., 1.0E-4 )) then
            nsect = 2
         endif
      endif
      if (epsequ ( asect(2)/af(igp) , 0., 1.0E-4 )) then
         nsect = 1
      endif
   endif
!
!     Check for flow in only main section
!
   if (nsect .eq. 1) then
!
!        Check if Wj and Wj+1 are equal
!
      if (epsequ ( wj, wjp1, 1.0E-4 )) then
!
         celeri = dsdu * velo / ((1.-frou2) * depth)
!
         TERM1 = 0.
      else
!
         term1  = (( (wf(igp)-wj)**2 ) / ( wjp1-wj)) / 2.
!
         celeri = ( (2.*wsact) / wsum ) *&
         &( velo / ((1.-frou2) * af(igp))) *&
         &( wf(igp) - term1 ) * dsdu
      endif
!
   else
!
!        Determine Qf and Uflow
!
      qf = abs(q2(igp))
      uf = qf / af(igp)
!
!        Number of sections is 2 or 3, determine Q, W and U section
!
      qsect(1) = abs(qs(igp,1))
      qsect(2) = abs(qs(igp,2))
!
      wsect(1) = wfs(igp,1)
      wsect(2) = wfs(igp,2)
!
      usect(1) = qsect(1) / asect(1)
      usect(2) = qsect(2) / asect(2)
!
      if (nsect .eq. 3) then
         qsect(3) = qf      - qsect(1) - qsect(2)
         wsect(3) = wf(igp) - wsect(1) - wsect(2)
         usect(3) = qsect(3) / asect(3)
      endif
!
!        determine sin(delta) for actual water level
!
      call INDWGH ( ngrid  ,igp    ,&
      &maxlev ,nlev   ,hlev   ,&
      &h2(igp),j      ,wght   )
!
      if (j .eq. -1) then
         sdelta = 1.
      else
         dz = hlev(igp,j+1) - hlev(igp,j)
         dw = wft (igp,j+1) - wft (igp,j)
         sdelta =  dz / sqrt ( (dw/2.)**2 + dz**2 )
      endif
!
!        Calculate Ws1 and Ws2
!
      sumws1 = 0.
      sumws2 = 0.
!
      do 300 isect = 1, nsect
!
!           Calculate Ui over Uf
!
         uiouf  = usect(isect) / uf
         sumws1 = sumws1 + uiouf * wsect(isect)
         sumws2 = sumws2 + ( uiouf * uiouf ) * wsect(isect)
300   continue
!
!        Calculate Un over Uf
!
      unouf = usect(nsect) / uf
!
      ws1 = (-2./3.) * unouf * (rs(igp,nsect) / sdelta) + sumws1
      ws2 = -unouf   * unouf * (rs(igp,nsect) / sdelta) + sumws2
!
!        Calculate Um over Uf and Rm*gamma
!
      umouf = usect(1) / uf
      rmgam = rs(igp,1) * gamma
!
!        Calculate F
!
      term1 = umouf * umouf * ( wsum - rmgam ) +&
      &alfab(igp) * umouf * ( -3./2. * wsum + rmgam )
!
      term2 = 2. * ws2 - 3. * alfab(igp) * ws1 +&
      &(g * af(igp)**3) / (qf * qf)
!
      f = term1 / term2
!
!        Calculate 1/He
!
      term1 = (wsect(1) / (3. * asect(1))) - (ws1/af(igp))
!
      term2 = (rmgam/3.-wsum/2.) * (1.-qsect(1)/qf) + (wsum/3.)
!
      he1   = (3./2.) * ( (term1 * f) + (term2 / af(igp)) )
!
      celeri = ((2.*wsact) / wsum) * (qsect(1)/asect(1)) * he1 * dsdu
   endif
!
!     Assign sign to celerity
!
   celeri = sign ( celeri, sedtra )
!
1000 continue
!
end
