      subroutine secynb (g      ,igp    ,nsect  ,u      ,dvelo ,
     &                   depth  ,ibrl   ,sedtra ,sedpdu ,maxlev ,ngrid ,
     &                   nlev   ,hlev   ,wft    ,ws     ,secths ,h2    ,
     &                   q2     ,qs     ,wf     ,wfs    ,af     ,afs   ,
     &                   rs     ,alfab  ,juer   ,celeri ,lrivr  ,ker   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SECYNB (SEdiment CeleritY Normal Branch)
c
c Module description: Calculate celerity for a normal branch
c
c                     The Froude number is calculated and tested for
c                     extreme values. The celerity is calculated using
c                     numerical differentiation. For this the transport
c                     on U+.001 is calculated. The celerity is adapted
c                     if parts of another section are included in the
c                     sediment transporting channel.
c
c Precondition:       Froude number < 1.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 23 af(ngrid)         I  Flow area at every grid point at time t(n+1)
c 24 afs(ngrid,2)      I  Actual flow area per section:
c                         (i,1) = For main section of grid point i.
c                         (i,2) = For sub section 1 of grid point i.
c 26 alfab(ngrid)      I  Actual Bousinessq coefficient in grid point i.
c 28 celeri            IO celerity
c  7 depth             I  avarage depth
c  6 dvelo             I  Difference in velocity u+du
c  1 g                 I  Acceleration of gravity.
c 18 h2                P  -
c 14 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
c       maxlev)           - For a circle cross section:
c                         (i,1) = Reference level.
c                         - For a sedredge cross section:
c                         (i,1) = Bed level of main section (i.e. left
c                                 channel).
c                         (i,2) = Bed level of sub section 1 (i.e. right
c                                 channel).
c  8 ibrl              IO Number of actual branch. Will be set to zero
c                         when a message is generated.
c  3 igp               I  igr+1 (at branch end = igr)
c 27 juer              P  -
c 29 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 11 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c 12 ngrid             I  Number of grid points in network.
c 13 nlev(ngrid)       I  Number of h-levels for every cross section.
c                         (For a circle cross section   : 1 ;
c                          For a sedredge cross section : 2 )
c  4 nsect             IO Actual number of sections in use
c                         1 = Main section
c                         2 = Main + sub 1 section
c                         3 = Main + sub 1 + sub 2 section
c 19 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
c 20 qs(ngrid,2)       I  Flow in every grid point per section:
c                         (i,1) = Through grid point i of main channel.
c                         (i,2) = Through grid point i of sub section 1.
c 25 rs(ngrid,3)       I  Actual hydraulic radius in a section (main,
c                         sub 1, sub 2) for every grid point.
c 17 secths            P  -
c 10 sedpdu            I  sediment transport as f(u+du)
c  9 sedtra            I  calculated sediment transport
c  5 u                 I  velocity
c 21 wf(ngrid)         I  Actual flow width at every grid point.
c 22 wfs(ngrid,2)      I  Actual flow width per section:
c                         (i,1) = For main section of grid point i.
c                         (i,2) = For sub section 1 of grid point i.
c 15 wft(ngrid,maxlev) I  (i,j) = flow width at h = hlev(i,j) for grid
c                                 point i.
c                         - For a circle cross section:
c                         (i,1) = Radius of the circle.
c                         - For a sedredge cross section:
c                         (i,1) = Width of main section (i.e. left chan-
c                                 nel).
c                         (i,2) = Width of sub section 1 (i.e. right
c                                 channel).
c 16 ws(ngrid)         I  Sediment transporting width for each grid
c                         point.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c epsequ  EQUal test with interval EPSilon
c error   write an ERROR to the error file.
c indwgh  Compute INDex and WeiGHt factor
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: secynb.pf,v $
c Revision 1.6  1999/06/01  13:42:39  kuipe_j
c names in messages substituted + message template
c
c Revision 1.5  1999/03/15  13:47:29  kuipe_j
c coordinate in message
c
c Revision 1.4  1998/06/11  11:47:24  kuipe_j
c Estuary special integrated
c
c Revision 1.3  1995/05/30  09:56:26  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:07:14  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:16  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/12/02  12:50:56  kuipe_j
c Test concerning actual number of sections was wrong.
c
c Revision 1.2  1993/11/26  15:34:38  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:19  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer      igp    ,nsect  ,maxlev ,ngrid  ,juer   ,ibrl   ,
     +             ker
c
      real         g      ,u      ,dvelo  ,depth  ,sedtra ,
     +             sedpdu ,celeri
c
      integer      nlev   (ngrid)
c
      real         wft    (ngrid,maxlev)  ,
     +             ws     (ngrid)         ,secths (ngrid)         ,
     +             qs     (ngrid,2)       ,wf     (ngrid)         ,
     +             wfs    (ngrid,2)       ,af     (ngrid)         ,
     +             afs    (ngrid,2)       ,rs     (ngrid,3)       ,
     +             alfab  (ngrid)
c
      double precision hlev (ngrid,maxlev), h2(ngrid), q2(ngrid)
c
c     Declaration of local variables
c
      real         velo   ,frou2  ,dsdu   ,wsact  ,wj     ,wjp1   ,
     +             wsum   ,term1  ,term2  ,gamma  ,dw     ,
     +             dz     ,sdelta ,sumws1 ,sumws2 ,ws1    ,ws2    ,
     +             qf     ,uf     ,uiouf  ,unouf  ,umouf  ,rmgam  ,
     +             f      ,he1    ,wsacth
c
      double precision wght
c
      integer      j      ,isect, ibrd, lbrnam
c
      real         asect(3), qsect(3), usect(3), wsect(3),xc
c
      character*40 branam
      character*5  txt
      character*11 xtxt
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
c
      logical      EPSEQU
      external     EPSEQU
c
      logical      lrivr
c
      integer iostat
      logical exist
      real froumx, froum
c
c     Determine froude^2
c
      velo  = abs(u)
      frou2 = velo*velo / (g*depth)
c
c     Only check when river morphology
c
      if (lrivr) then
         if (frou2 .gt. .64) then
c+++++++++++++++++++++++++++++++
c start rev 1.3:
c+++++++++++++++++++++++++++++++
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
            write(*,*)'+++SECYNB froumx**2=',froumx,
     +                ' exceeded, froude**2 value=',frou2
c-orig      if (frou2 .gt. .98) then
c+++++++++++++++++++++++++++++++
c end rev 1.3:
c+++++++++++++++++++++++++++++++
               ker = fatal
               call getloc (igp,ibrd,xc)
               write (txt,'(f5.2)') sqrt(frou2)
               call getbrn (ibrd,branam,lbrnam)
               write (xtxt,'(f10.2)') xc
               call error (juer,
     +            'SECYNB Froude number too large(@'//txt//
     +            '@) in branch @'//branam(:lbrnam)//'@ at X= @'
     +            //xtxt//'@',esefro,ker)
               goto 1000
            else if (ibrl .ne. 0) then
c
c              Only one message will be given in a branch.
c
               ker = warnng
               call getloc (igp,ibrd,xc)
               call getbrn (ibrd,branam,lbrnam)
               write (xtxt,'(f10.2)') xc
               call error (juer,
     +            'SECYNB Froude numbers > .8 in branch @'
     +            //branam(:lbrnam)//'@ starting at X=@'//
     +            xtxt//'@',esefrw,ker)
               ibrl = 0
            endif
         endif
      endif
c
      dsdu = (sedpdu - abs(sedtra)) / dvelo
c
c     Calculate actual sediment transporting width
c
      if (wf(igp) .lt. ws(igp)) then
         wsact  = wf(igp)
         wsacth = h2(igp)
      else
         wsact  = ws(igp)
         wsacth = secths(igp)
      endif
c
c     Determine gamma for water level Wmain, Wj and Wj+1
c
      call INDWGH ( ngrid  ,igp    ,maxlev ,nlev   ,hlev   ,
     +              dble(wsacth)   ,j      ,wght   )
c
      if (j .eq. -1) then
         gamma = 2.
c
         wj    = wft(igp,nlev(igp))
         wjp1  = wj
      else
         dz = hlev(igp,j+1) - hlev(igp,j)
         dw = wft (igp,j+1) - wft (igp,j)
         gamma = ( 2. * dz ) / sqrt ( (dw/2.)**2 + dz**2 )
c
         wj   = wft(igp,j)
         if (j .eq. nlev(igp)) then
            wjp1  = wj
         else
            wjp1 = wft(igp,j+1)
         endif
      endif
c
c     Add Wj and Wj+1
c
      wsum = wj + wjp1
c
c     Check if A last section(s) is not too small
c
      if (nsect .gt. 1) then
         asect(1) = afs(igp,1)
         asect(2) = afs(igp,2)
         if (nsect .eq. 3) then
            asect(3) = af(igp) - asect(1) - asect(2)
c
            if (epsequ ( asect(3)/af(igp) , 0., 1.0E-4 )) then
               nsect = 2
            endif
         endif 
         if (epsequ ( asect(2)/af(igp) , 0., 1.0E-4 )) then
            nsect = 1
         endif
      endif
c
c     Check for flow in only main section
c
      if (nsect .eq. 1) then
c
c        Check if Wj and Wj+1 are equal
c
         if (epsequ ( wj, wjp1, 1.0E-4 )) then
c
            celeri = dsdu * velo / ((1.-frou2) * depth)
c
            TERM1 = 0.
         else
c
            term1  = (( (wf(igp)-wj)**2 ) / ( wjp1-wj)) / 2.
c
            celeri = ( (2.*wsact) / wsum ) *
     +               ( velo / ((1.-frou2) * af(igp))) *
     +               ( wf(igp) - term1 ) * dsdu
         endif
c
      else
c
c        Determine Qf and Uflow
c
         qf = abs(q2(igp))
         uf = qf / af(igp)
c
c        Number of sections is 2 or 3, determine Q, W and U section
c
         qsect(1) = abs(qs(igp,1))
         qsect(2) = abs(qs(igp,2))
c
         wsect(1) = wfs(igp,1)
         wsect(2) = wfs(igp,2)
c
         usect(1) = qsect(1) / asect(1)
         usect(2) = qsect(2) / asect(2)
c
         if (nsect .eq. 3) then
            qsect(3) = qf      - qsect(1) - qsect(2)
            wsect(3) = wf(igp) - wsect(1) - wsect(2)
            usect(3) = qsect(3) / asect(3)
         endif
c
c        determine sin(delta) for actual water level
c
         call INDWGH ( ngrid  ,igp    ,
     +                 maxlev ,nlev   ,hlev   ,
     +                 h2(igp),j      ,wght   )
c
         if (j .eq. -1) then
            sdelta = 1.
         else
            dz = hlev(igp,j+1) - hlev(igp,j)
            dw = wft (igp,j+1) - wft (igp,j)
            sdelta =  dz / sqrt ( (dw/2.)**2 + dz**2 )
         endif
c
c        Calculate Ws1 and Ws2
c
         sumws1 = 0.
         sumws2 = 0.
c
         do 300 isect = 1, nsect
c
c           Calculate Ui over Uf
c
            uiouf  = usect(isect) / uf
            sumws1 = sumws1 + uiouf * wsect(isect)
            sumws2 = sumws2 + ( uiouf * uiouf ) * wsect(isect)
 300     continue
c
c        Calculate Un over Uf
c
         unouf = usect(nsect) / uf
c
         ws1 = (-2./3.) * unouf * (rs(igp,nsect) / sdelta) + sumws1
         ws2 = -unouf   * unouf * (rs(igp,nsect) / sdelta) + sumws2
c
c        Calculate Um over Uf and Rm*gamma
c
         umouf = usect(1) / uf
         rmgam = rs(igp,1) * gamma
c
c        Calculate F
c
         term1 = umouf * umouf * ( wsum - rmgam ) +
     +           alfab(igp) * umouf * ( -3./2. * wsum + rmgam )
c
         term2 = 2. * ws2 - 3. * alfab(igp) * ws1 +
     +           (g * af(igp)**3) / (qf * qf)
c
         f = term1 / term2
c
c        Calculate 1/He
c
         term1 = (wsect(1) / (3. * asect(1))) - (ws1/af(igp))
c
         term2 = (rmgam/3.-wsum/2.) * (1.-qsect(1)/qf) + (wsum/3.)
c
         he1   = (3./2.) * ( (term1 * f) + (term2 / af(igp)) )
c
         celeri = ((2.*wsact) / wsum) * (qsect(1)/asect(1)) * he1 * dsdu
      endif
c
c     Assign sign to celerity
c
      celeri = sign ( celeri, sedtra )
c
 1000 continue
c
      end
