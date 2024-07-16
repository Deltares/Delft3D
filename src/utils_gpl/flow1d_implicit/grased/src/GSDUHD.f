      subroutine gsduhd (g      ,igp    ,nsect  ,u      ,
     &                   depth  ,ibrl   ,maxlev ,ngrid  ,nlev   ,hlev  ,
     &                   wft    ,ws     ,secths ,h2     ,q2     ,qs    ,
     &                   wf     ,wfs    ,af     ,afs    ,rs     ,alfab ,
     &                   juer   ,duda   ,dhda   ,ker    )

c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gsduhd.F,v $
c Revision 1.3  2003/03/25  14:30:00  Andre Staakman
c special for Meander: intro FROUDMAX control-file,
c                      containing 1 real number: maxfroud**2.
c                      When this number is exceeded sobeksim
c                      will stop with a fatal error.
c                      (Andre Staakman, MX)  
c Revision 1.2  1995/09/27  10:12:16  kuipe_j
c Maintenance
c
c
c***********************************************************************
c
c Module:             GSDUHD (Graded Sediment calculate DU/da, dH/Da)
c
c     Declaration of parameters
c
      integer      igp    ,nsect  ,maxlev ,ngrid  ,juer   ,ibrl   ,
     +             ker
c
      real         g      ,u      ,depth  ,duda   ,dhda
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
      double precision hlev   (ngrid,maxlev)
      double precision h2(ngrid) ,q2(ngrid) 

c
c     Declaration of local variables
c
      real         frou2  ,wsact  ,wj     ,wjp1   ,f      ,he1    ,
     +             wsum   ,term1  ,term2  ,gamma  ,dw     ,
     +             dz     ,sdelta ,sumws1 ,sumws2 ,ws1    ,ws2    ,
     +             qf     ,uf     ,uiouf  ,unouf  ,umouf  ,rmgam  ,
     +             velo   ,xc     ,wsacth
c
      double precision wght
c
      integer      j      ,isect  ,ibrd   ,lbrnam
c
      real         asect(3), qsect(3), usect(3), wsect(3)
c
      character*40 branam
      character*5  txt
      character*11 xtxt
c
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
c
      logical      EPSEQU
      external     EPSEQU
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
            write(*,*)'+++GSDUHD froumx**2=',froumx,
     +                ' exceeded, froude**2 value=',frou2
c-orig   if (frou2 .gt. .98) then
c+++++++++++++++++++++++++++++++
c end rev 1.3:
c+++++++++++++++++++++++++++++++
            ker = fatal
c overrule  ker = warnng
            call getloc (igp,ibrd,xc)
            write (txt,'(f5.2)') sqrt(frou2)
            call getbrn (ibrd,branam,lbrnam)
            write (xtxt,'(f10.2)') xc
            call error (juer,
     +           'GSDUHD Froude number too large (@'//txt//
     +           '@) in branch @'//branam(:lbrnam)//'@ at X= @'
     +           //xtxt//'@',esefro,ker)
c overrule  frou2 = .98
            goto 100
         else if (ibrl .ne. 0) then
c
c           Only one message will be given in a branch.
c
            ker = warnng
            call getloc (igp,ibrd,xc)
            call getbrn (ibrd,branam,lbrnam)
            write (xtxt,'(f10.2)') xc
            call error (juer,
     +           'GSDUHD Froude numbers > .8 in branch @'
     +           //branam(:lbrnam)//'@ starting at X=@'//
     +           xtxt//'@',esefrw,ker)
            ibrl = 0
         endif
      endif
c
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
         dz = hlev (igp,j+1) - hlev (igp,j)
         dw = wft  (igp,j+1) - wft  (igp,j)
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
            duda = 1. / ((1.-frou2) * af(igp))
c
         else
c
            term1  = (( (wf(igp)-wj)**2 ) / ( wjp1-wj)) / 2.
c
            duda = ( 2. / wsum ) /
     +             ((1.-frou2) * af(igp)) *
     +             ( wf(igp) - term1 )
         endif
c
         dhda = - depth * duda 
         duda = u * duda 
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
            dz = hlev (igp,j+1) - hlev (igp,j)
            dw = wft  (igp,j+1) - wft  (igp,j)
            sdelta =  dz / sqrt ( (dw/2.)**2 + dz**2 )
         endif
c
c        Calculate Ws1 and Ws2
c
         sumws1 = 0.
         sumws2 = 0.
c
         do 10 isect = 1, nsect
c
c           Calculate Ui over Uf
c
            uiouf  = usect(isect) / uf
            sumws1 = sumws1 + uiouf * wsect(isect)
            sumws2 = sumws2 + ( uiouf * uiouf ) * wsect(isect)
  10     continue
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
         duda  = (2. / wsum) * (qsect(1)/asect(1)) * he1
c
         dhda  = -1. / wsact + 2. / wsum * f
c
      endif
c
 100  continue
c
      end
