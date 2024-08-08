      subroutine mozcontrol ( istep  ,ngrid  ,nqlat  ,dtf    ,
     +                        juer   ,istmoz ,idmoz  ,itim   ,
     +                        qltpar ,qlatid ,qlatgr ,hpack  ) 
c
c     Control module of Mozart coupling
c
      integer    istep   ,ngrid  ,nqlat  ,istmoz  ,idmoz,juer
      integer    itim(2)
      double     precision        dtf
      real       qltpar(9,nqlat) ,qlatgr(ngrid)
      double precision hpack(ngrid,3) 
      character*20                qlatid(nqlat)
c
      character*120     cntrlnam, cmdline, ininam
      integer           nfm,istk,ipnt,nd,nh,nm,ns,nstmoz,nE,i
      integer           itim_new(2)
      integer(2) ::     istcnt
      real              dtfm
      double precision  dattim, dattim_new  
      logical           FirstProc, Initmode, Crashed
c
c Koppeling Mozart    
c
c     initialiseer koppeling      
c     schrijf initiele waterstand (istep = 1)
      if (istep .eq. 1) then
           istcnt = -1

           nfm = 0
           open (120, file = 'Sbmzkop.fnm')
   10      read (120,'(a)',end=20) cmdline
           if(cmdline(1:1) .eq. '*') then
           else
              read (cmdline,*) cntrlnam
              if(nfm .eq. 0) ininam = cntrlnam
              nfm = nfm + 1
           endif
           goto 10
   20      close (120)

c sbmzkop.ini voor overall tijdstap
           open (120, file = ininam)
   30      read (120,'(a)',end=40) ininam 
           if(ininam(1:6) .eq. 'Deltat') goto 40
           goto 30

   40      close (120)
           istk = index (ininam,'=')
           ipnt = index (ininam,'.')
           read (ininam(istk+1:ipnt-1),*) nd
           read (ininam(ipnt+1:ipnt+6),'(3i2)') nh,nm,ns
           dtfm = real(nd) * 86400. + real(nh) * 3600. + 
     *            real(nm) * 60. + real(ns)
           nstmoz =  int (dtfm / dtf)
             
           cmdline='Sbmzsob.exe '//cntrlnam

           write(juer,'(1x,a,1x,f10.0, i5)') cmdline(:40),dtfm, nstmoz 
           call INITCT (cmdline, idmoz, istcnt)
           call INITFP (FirstProc, Initmode)
c memory allocation initialisation stekkerdoos
           call swalloc(nE)
           if (nE .ne. 0) then
              call ErrOut('SWALLOC',nE)
           end if
      endif

c tijdstapverschil Sobek - Mozart, doorrekenen tot volgende Mozart-tijdstip
        istmoz = 1
c JC 12/4/2000 istep-1 ipv istep; of dat moet is nog de vraag
        if(istep.eq.1.or.mod(istep-1,nstmoz).eq.0) then
          istmoz = 0
c JC 12/4/2000 schrijven waterstanden 
c  (verplaatst, komt van na call soconf)
          call MOZWRITEH (istep  ,  ngrid  , nqlat  ,
     +                    qltpar ,  qlatid , qlatgr , hpack )
c check proces, wacht
c indien ok, lees QA in FLQLAT
          Initmode = .false.
          dattim = itim(1) + itim(2)/100000000.D0
          do i = 1, 2
             itim_new(i) = itim(i)
          end do
          call SOTIME(itim_new,dtf)
          dattim_new = itim_new(1) + itim_new(2)/100000000.D0
          write(juer,*) ' voor Stepct', dattim, dattim_new

          call STEPCT (dattim ,dattim_new ,idmoz ,istcnt ,initmode,
     +                 crashed)
          If (crashed)  Call CrashCt (Idmoz, .false. )
c         write(juer,*) ' na Stepct'
      endif
c
      end
      integer function lentrim(string)
      character*(*)    string
      integer          len, ld, lact, i
c 
      ld   = len (string)
      lact = ld
      do i=ld,1,-1
         lact = i
         if (string(i:i).ne.' ') exit
      enddo 
      lentrim = lact
c
      end
