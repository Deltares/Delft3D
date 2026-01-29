subroutine sawrhi(nsaman ,nsamap ,nsatim ,ngrid  ,itim   ,istep  ,&
&nstep  ,salmap ,saltim ,csa2   ,disgr  ,&
&sacpre ,rho    ,dt     ,salini ,buffer ,gridnm )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         A.W.J.Koster
!
! Module:             SAWRHI(SAlt WRite salt HIs files)
!
! Module description: Writing of user selected salt results to HIS file
!
!                     The user selected salt results at user supplied
!                     locations and time levels will be stored on the
!                     result file. The stored data can be processed
!                     further by the User Interface.
!
!                     The user can select functions of place and of
!                     time.
!
!                     Writing can start on a new file or in case the
!                     data model is unchanged an existing file can be
!                     extended.
!
!
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 25 buffer(nvar,ngrid)O  Buffer for results to be written to HIS file
! 15 csa2(ngrid)       I  Salt concentration in every grid point at time
!                         t(n+1).
! 16 disgr(ngrid)      I  Dispersion coefficient in every grid point at
!                         time t(n+1).
!  8 istep             I  Current time step number (t(n+1)).
!  7 itim              P  -
!  6 ngrid             I  Number of grid points in network.
!  3 nsaman            I  Number of main codes of salt results.
!  4 nsamap            I  Number of entries in salmap.
!  5 nsatim            I  Number of entries in saltim.
! 20 rho(ngrid)        I  Density of diluted water per grid point.
! 19 sacpre(nsaman)    I  sacpre(i) = index in block tabel (1..nentri)
!                         for main code i of salt results.
! 13 salmap(nsamap)    I  Parameter list for MAP block with salt
!                         results:
!                         (1)      = Report begin time
!                         (2)      = Report end time
!                         (3)      = Report time step
!                         (4)      = Report parameter 1 main code
!                         (5)      = Report parameter 1 sub code
!                         (i)      = Report parameter 1 main code
!                         (i+1)    = Report parameter 1 sub code
!                         (nsamap) = Report parameter n sub code
! 14 saltim(nsatim)    I  Parameter list for HIST block with salt re-
!                         sults:
!                         (1)      = Number of places
!                         (2)      = Report grid point 1
!                         (3)      = Report grid point 2
!                         (i)      = Report grid point n
!                         (i+1)    = Report parameter code 1
!                         (i+2)    = Report parameter sub code 1
!                         (nsatim) = Report parameter n sub code
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
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
! $Log: sawrhi.pf,v $
! Revision 1.4  1998/02/13  12:12:54  kuipe_j
! Adapt to CMT
!
! Revision 1.3  1997/06/18  08:21:27  kuipe_j
! remove unreferences
!
! Revision 1.2  1997/06/18  07:52:12  kuipe_j
! Remove unreferenced vars
!
!
!***********************************************************************
!
   include '..\include\filsim.i'
!
!     Declaration of parameters
!
   integer      nentri
   parameter   (nentri=5)
   integer      nsaman  ,nsamap,nsatim ,ngrid ,istep, nstep
   integer      salmap(nsamap),&
   &saltim(nsatim) ,sacpre(nsaman),itim  (2) ,&
   &salini (*)
   real         csa2(ngrid)    ,disgr(ngrid)  ,rho(ngrid),&
   &buffer(nentri,ngrid)
   character*40 gridnm(*)
   double precision dt
!
!     Declaration of local variables
!
   integer      i ,j ,ie  ,nlc , nsk
   integer      ijaar ,imaand ,idag ,iuur ,imin ,isec, iscu
   integer      lun   ,ivar   ,nvar ,igrid,ifil ,istepf, istphf ,&
   &skip
   double precision    scudt
   character*40 idmap(4) , idhis(4)
   character*20 parnam(nentri),parnam1(nentri)
   logical      new      , newuit
!
!     Declaration of external functions
!
   logical      yesmap
   external     yesmap
   save         istepf,istphf,scudt
!     Initialize.
!
!
!    Initialise
!
   idmap(1)  = 'SOBEK                                   '
   idmap(2)  = 'Maps results at gridpoints              '
   idmap(3)  = '                                        '
   idmap(4)  = '                                        '
   parnam(1) = 'Salt concentration  '
   parnam(2) = 'Disp. coefficient   '
   parnam(3) = 'Density             '
   parnam(4) = 'Salinity            '
   parnam(5) = 'Chloride concen.    '
   idhis(1)  = 'SOBEK                                   '
   idhis(2)  = 'History results at gridpoints           '
   idhis(3)  = '                                        '
   idhis(4)  = '                                        '
!
!     Write Map results
!
   if (nsamap .gt. 3) then
      if (yesmap(salmap(1),salmap(2),salmap(3),istep)) then
         nvar = nint(0.5*(nsamap-3))
         ifil = 1
         lun  = 54
         if ( salini(ifil) .eq. 0 ) then
#if defined (USE_MSWINDOWS)
            open(lun , file = sltmap , form = 'binary')
#else
#if defined (USE_HPUX)
            open(lun , file = sltmap , form = 'unformatted')
#else
            open(lun , file = sltmap , form = 'unformatted')
#endif
#endif
            iscu = nstep*dt/1.0d9+1.d0
            scudt  = dt/dble(iscu)
            call parsdt(itim,ijaar,imaand,idag,iuur,imin,isec)
            write ( idmap(4),1000 ) ijaar,imaand,idag,&
            &iuur,imin,isec,iscu
            write(lun) (idmap(i) , i = 1,4)
            write(lun) nvar,ngrid
            write(lun) ( parnam(sacpre(salmap(i))+&
            &salmap(i+1)) ,  i = 4,nsamap,2 )
            write(lun) (igrid , gridnm(igrid)(:20),&
            &igrid = 1 , ngrid )
            salini(ifil) = 1
            istepf       = istep
         endif
!
         ivar = 0
         do 30 i = 4,nsamap,2
            ivar = ivar + 1
            ie = sacpre(salmap(i))+salmap(i+1)
!
            do 20 igrid = 1 , ngrid
               if      (ie.eq.1) then
                  buffer(ivar,igrid) = csa2(igrid)
               else if (ie.eq.2) then
                  buffer(ivar,igrid) = disgr(igrid)
               else if (ie.eq.3) then
                  buffer(ivar,igrid) = rho(igrid)
               else if (ie.eq.4) then
                  buffer(ivar,igrid) = csa2(igrid) / rho(igrid)&
                  &* 1000.
               else if (ie.eq.5) then
                  buffer(ivar,igrid) = csa2(igrid) / 1.80655
               endif
20          continue
30       continue
         write(lun) nint((istep-istepf)*scudt),&
         &((buffer(ivar,igrid),&
         &ivar = 1 , nvar),&
         &igrid = 1 , ngrid)
      endif
   endif
!
!     Write History results.
!
   nlc = saltim(1)
   new = mod(nsatim-nlc,2) .eq. 0
   if (new) then
      nsk = 3
   else
      nsk = 0
   endif
!
   if (nsatim .gt. 1+nsk) then
      if (new) then
         newuit = yesmap(saltim(nlc+2),saltim(nlc+3),saltim(nlc+4),&
         &istep)
      else
         newuit = .false.
      endif
      if ( (saltim(1).gt.0 .and. .not. new) .or. newuit ) then
         ifil = 2
         lun  = 55
         if ( salini(ifil) .eq. 0 ) then
#if defined (USE_MSWINDOWS)
            open(lun , file = slthis , form = 'binary')
#else
#if defined (USE_HPUX)
            open(lun , file = slthis , form = 'unformatted')
#else
            open(lun , file = slthis , form = 'unformatted')
#endif
#endif
            ivar = 0
            do i = saltim(1)+2+nsk,nsatim,2
               ie = sacpre(saltim(i))+saltim(i+1)
               if (ie.le.nentri) then
                  ivar = ivar + 1
                  parnam1(ivar) = parnam(ie)
               endif
            enddo
            nvar   = ivar
            iscu   = nstep*dt/1.0d9+1.d0
            scudt  = dt/dble(iscu)
            call parsdt(itim,ijaar,imaand,idag,iuur,imin,isec)
            write ( idhis(4),1000 ) ijaar,imaand,idag,&
            &iuur,imin,isec,iscu
            write(lun) (idhis(i) , i = 1,4)
            write(lun) nvar , saltim(1)
            write(lun) (parnam1(ivar),ivar=1,nvar)
            write(lun) (saltim(igrid),gridnm(saltim(igrid))(:20),&
            &igrid = 2 , saltim(1)+1)
            salini(ifil) = 1
            istphf       = istep
         endif
!
         ivar = 0
         do  60 i = saltim(1)+2+nsk,nsatim,2
            ivar = ivar + 1
            skip = 0
            ie = sacpre(saltim(i))+saltim(i+1)
            do 50 j=1,saltim(1)
               if (ie.eq.1) then
                  buffer(ivar,j) = csa2(saltim(j+1))
               else if (ie.eq.2) then
                  buffer(ivar,j) = disgr(saltim(j+1))
               else if (ie.eq.3) then
                  buffer(ivar,j) = rho(saltim(j+1))
               else if (ie.eq.4) then
                  buffer(ivar,j) = csa2(saltim(j+1)) /&
                  &rho(saltim(j+1)) * 1000.
               else if (ie.eq.5) then
                  buffer(ivar,j) = csa2(saltim(j+1)) / 1.80655
               else
                  skip = 1
               endif
50          continue
            ivar = ivar - skip
60       continue
         nvar = ivar
         write(lun) nint((istep-istphf)*scudt),&
         &((buffer(ivar,j),&
         &ivar = 1,nvar),&
         &j    = 1,saltim(1))
      endif
   endif
!
1000 format ('T0: ',I4.4,'.',I2.2,'.',I2.2,' ',I2.2,':',I2.2,':',I2.2,&
   &'  (scu=',i8,'s)')
!
   return
end
