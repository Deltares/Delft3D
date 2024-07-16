      subroutine FLMINMAX (kode  ,ngrid ,istep ,nstep ,dt    ,itim  ,
     &                     h     ,hmin  ,hmax  ,tminh ,tmaxh ,
     &                     q     ,qmin  ,qmax  ,tminq ,tmaxq ,gridnm) 
c
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Kuipers
c
c Module:             FLMINMMAX(FLow determine MINinum and MAXimum)
c
c Module description: The minimu and maximum water level and discharge
c                     in every grid
c                     point will be calculated. As well as the times of
c                     the maximum and minimum.
c                     At the end of the computation these will be
c                     written to file MINMAX.HIS
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 kode              I  1 = Calculate maximum on first step and
c                             initialize.
c                         2 = Calculate maximum on next steps
c                         3 = Calculate maximum on last step and
c                             write to file.
c  2 ngrid             I  Number of grid points in network.
c  4 gridnm(ngrid)     I  Name of every grid point.
c  5 h(ngrid)          I  Water level in every grid point at t=n+1
c                         (TIME).
c  6 hmax(ngrid)      IO  Maximum water level in every grid point.
c
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c
c=======================================================================
c
      include '..\include\filsim.i'
c      
c     Declaration of Parameters:
c
      integer      ngrid   ,istep,   nstep,  kode
      integer      itim  (2)             
      real         hmin(ngrid)    ,hmax(ngrid)  ,
     &             tminh(ngrid)   ,tmaxh(ngrid) ,
     &             qmin(ngrid)    ,qmax(ngrid)  ,
     &             tminq(ngrid)   ,tmaxq(ngrid)
      character*40 gridnm(ngrid)
      double precision dt, h(ngrid), q(ngrid)
c
c     Declaration of local variables
c
      integer      nentri
c      parameter   (nentri=4) change Ars07786
      parameter    (nentri=8)
      integer      i     ,istepf ,istepl,itim0(2)
      integer      ijaar ,imaand ,idag ,iuur ,imin ,isec, iscu
      integer      lun   ,nvar ,igr
      real         time   
      double precision    scudt
      character*40 idmap(4) 
      character*20 parnam(nentri)
      logical      writef
c
      save         istepf, istepl
c
      if (kode .eq. 1 ) then
         do igr=1,ngrid
            hmin (igr) =  1.0e10
            hmax (igr) = -1.0e10
            tminh(igr) = 0.
            tmaxh(igr) = 0.
            qmin (igr) =  1.0e10
            qmax (igr) = -1.0e10
            tminq(igr) = 0.
            tmaxq(igr) = 0.
         enddo
         istepf = istep
         itim0(1) = itim(1)
         itim0(2) = itim(2)
      endif
  
      time = (istep-istepf)*dt /3600.
      if (kode .le. 2 ) then
         do igr=1,ngrid
            if (hmax(igr) .lt. h(igr)) then
               hmax (igr) = h(igr)
               tmaxh(igr) = time
            endif   
            if (hmin(igr) .gt. h(igr)) then
               hmin (igr) = h(igr)
               tminh(igr) = time 
            endif  
            if (qmax(igr) .lt. q(igr)) then
               qmax (igr) = q(igr)
               tmaxq(igr) = time
            endif   
            if (qmin(igr) .gt. q(igr)) then
               qmin (igr) = q(igr)
               tminq(igr) = time
            endif  
         enddo
         istepl = istep
      endif   
c
      if (kode .eq. 3 ) then
         writef = .true.
         do igr=1,ngrid
            if (hmin(igr) .ge.  1.0e10) writef = .false.
         enddo
         if (writef) then
c
c           Initialise
c
            idmap(1)  = 'SOBEK                                   '
            idmap(2)  = 'Maps results at gridpoints              '
            idmap(3)  = '                                        '
            idmap(4)  = '                                        '
            parnam(1) = 'Minimum Water level '
            parnam(2) = 'Maximum Water level '
            parnam(3) = 'Time min. WaterL(hr)'
            parnam(4) = 'Time max. WaterL(hr)'
            parnam(5) = 'Minimum Discharge   '
            parnam(6) = 'Maximum Discharge   '
            parnam(7) = 'Time min. Disch (hr)'
            parnam(8) = 'Time max. Disch (hr)'
c
c           Write Map results
c         
            lun  = 31
#if defined (USE_MSWINDOWS)
            open(lun , file = minmax , form = 'binary')      
#else
#if defined (USE_HPUX)
            open(lun , file = minmax , form = 'unformatted') 
#else
            open(lun , file = minmax , form = 'unformatted') 
#endif
#endif
            nvar = nentri
            iscu = nstep*dt/1.0d9+1.d0 
            scudt  = dt/dble(iscu)
            call parsdt(itim0,ijaar,imaand,idag,iuur,imin,isec)
            write ( idmap(4),1000 ) ijaar,imaand,idag,
     +                              iuur,imin,isec,iscu
            write(lun) (idmap(i) , i = 1,4)
            write(lun) nvar,ngrid
            write(lun) (parnam(i),i = 1 , nvar)
            write(lun) (igr , gridnm(igr)(:20),
     +                  igr = 1 , ngrid )
            write(lun) nint((istepl-istepf)*scudt),
     +                 (hmin(igr),hmax(igr),tminh(igr),tmaxh(igr),
     +                  qmin(igr),qmax(igr),tminq(igr),tmaxq(igr),
     +                  igr = 1 , ngrid)
cARS 7786 hierboven vier parameters toegevoegd voor q en tijd
c         i.e. voor de debieten en de debiettijden
            close (lun)
         endif 
      endif
c
 1000 format ('T0: ',I4.4,'.',I2.2,'.',I2.2,' ',I2.2,':',I2.2,':',I2.2,
     +        '  (scu=',i8,'s)')
c
      return
      end
