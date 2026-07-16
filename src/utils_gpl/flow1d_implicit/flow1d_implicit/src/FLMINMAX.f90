subroutine FLMINMAX (kode  ,ngrid ,istep ,nstep ,dt    ,itim  ,&
&h     ,hmin  ,hmax  ,tminh ,tmaxh ,&
&q     ,qmin  ,qmax  ,tminq ,tmaxq ,gridnm)
!
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Kuipers
!
! Module:             FLMINMMAX(FLow determine MINinum and MAXimum)
!
! Module description: The minimu and maximum water level and discharge
!                     in every grid
!                     point will be calculated. As well as the times of
!                     the maximum and minimum.
!                     At the end of the computation these will be
!                     written to file MINMAX.HIS
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 kode              I  1 = Calculate maximum on first step and
!                             initialize.
!                         2 = Calculate maximum on next steps
!                         3 = Calculate maximum on last step and
!                             write to file.
!  2 ngrid             I  Number of grid points in network.
!  4 gridnm(ngrid)     I  Name of every grid point.
!  5 h(ngrid)          I  Water level in every grid point at t=n+1
!                         (TIME).
!  6 hmax(ngrid)      IO  Maximum water level in every grid point.
!
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
!
!=======================================================================
!
   include '../include/filsim.i'
!
!     Declaration of Parameters:
!
   integer      ngrid   ,istep,   nstep,  kode
   integer      itim  (2)
   real         hmin(ngrid)    ,hmax(ngrid)  ,&
   &tminh(ngrid)   ,tmaxh(ngrid) ,&
   &qmin(ngrid)    ,qmax(ngrid)  ,&
   &tminq(ngrid)   ,tmaxq(ngrid)
   character(len=40) gridnm(ngrid)
   double precision dt, h(ngrid), q(ngrid)
!
!     Declaration of local variables
!
   integer      nentri
!      parameter   (nentri=4) change Ars07786
   parameter    (nentri=8)
   integer      i     ,istepf ,istepl,itim0(2)
   integer      ijaar ,imaand ,idag ,iuur ,imin ,isec, iscu
   integer      lun   ,nvar ,igr
   real         time
   double precision    scudt
   character(len=40) idmap(4)
   character(len=20) parnam(nentri)
   logical      writef
!
   save         istepf, istepl
!
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
!
   if (kode .eq. 3 ) then
      writef = .true.
      do igr=1,ngrid
         if (hmin(igr) .ge.  1.0e10) writef = .false.
      enddo
      if (writef) then
!
!           Initialise
!
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
!
!           Write Map results
!
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
         write ( idmap(4),1000 ) ijaar,imaand,idag,&
         &iuur,imin,isec,iscu
         write(lun) (idmap(i) , i = 1,4)
         write(lun) nvar,ngrid
         write(lun) (parnam(i),i = 1 , nvar)
         write(lun) (igr , gridnm(igr)(:20),&
         &igr = 1 , ngrid )
         write(lun) nint((istepl-istepf)*scudt),&
         &(hmin(igr),hmax(igr),tminh(igr),tmaxh(igr),&
         &qmin(igr),qmax(igr),tminq(igr),tmaxq(igr),&
         &igr = 1 , ngrid)
!ARS 7786 hierboven vier parameters toegevoegd voor q en tijd
!         i.e. voor de debieten en de debiettijden
         close (lun)
      endif
   endif
!
1000 format ('T0: ',I4.4,'.',I2.2,'.',I2.2,' ',I2.2,':',I2.2,':',I2.2,&
   &'  (scu=',i8,'s)')
!
   return
end
