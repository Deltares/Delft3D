      subroutine sawrhi(nsaman ,nsamap ,nsatim ,ngrid  ,itim   ,istep  ,
     &                  nstep  ,salmap ,saltim ,csa2   ,disgr  ,
     &                  sacpre ,rho    ,dt     ,salini ,buffer ,gridnm )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         A.W.J.Koster 
c
c Module:             SAWRHI(SAlt WRite salt HIs files)
c
c Module description: Writing of user selected salt results to HIS file
c
c                     The user selected salt results at user supplied
c                     locations and time levels will be stored on the
c                     result file. The stored data can be processed
c                     further by the User Interface.
c
c                     The user can select functions of place and of
c                     time.
c
c                     Writing can start on a new file or in case the
c                     data model is unchanged an existing file can be
c                     extended.
c
c
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 25 buffer(nvar,ngrid)O  Buffer for results to be written to HIS file
c 15 csa2(ngrid)       I  Salt concentration in every grid point at time
c                         t(n+1).
c 16 disgr(ngrid)      I  Dispersion coefficient in every grid point at
c                         time t(n+1).
c  8 istep             I  Current time step number (t(n+1)).
c  7 itim              P  -
c  6 ngrid             I  Number of grid points in network.
c  3 nsaman            I  Number of main codes of salt results.
c  4 nsamap            I  Number of entries in salmap.
c  5 nsatim            I  Number of entries in saltim.
c 20 rho(ngrid)        I  Density of diluted water per grid point.
c 19 sacpre(nsaman)    I  sacpre(i) = index in block tabel (1..nentri)
c                         for main code i of salt results.
c 13 salmap(nsamap)    I  Parameter list for MAP block with salt
c                         results:
c                         (1)      = Report begin time
c                         (2)      = Report end time
c                         (3)      = Report time step
c                         (4)      = Report parameter 1 main code
c                         (5)      = Report parameter 1 sub code
c                         (i)      = Report parameter 1 main code
c                         (i+1)    = Report parameter 1 sub code
c                         (nsamap) = Report parameter n sub code
c 14 saltim(nsatim)    I  Parameter list for HIST block with salt re-
c                         sults:
c                         (1)      = Number of places
c                         (2)      = Report grid point 1
c                         (3)      = Report grid point 2
c                         (i)      = Report grid point n
c                         (i+1)    = Report parameter code 1
c                         (i+2)    = Report parameter sub code 1
c                         (nsatim) = Report parameter n sub code
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
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
c $Log: sawrhi.pf,v $
c Revision 1.4  1998/02/13  12:12:54  kuipe_j
c Adapt to CMT
c
c Revision 1.3  1997/06/18  08:21:27  kuipe_j
c remove unreferences
c
c Revision 1.2  1997/06/18  07:52:12  kuipe_j
c Remove unreferenced vars
c
c
c***********************************************************************
c
      include '..\include\filsim.i'
c
c     Declaration of parameters
c
      integer      nentri
      parameter   (nentri=5)
      integer      nsaman  ,nsamap,nsatim ,ngrid ,istep, nstep
      integer      salmap(nsamap),
     &             saltim(nsatim) ,sacpre(nsaman),itim  (2) ,
     &             salini (*)            
      real         csa2(ngrid)    ,disgr(ngrid)  ,rho(ngrid),
     &             buffer(nentri,ngrid)
      character*40 gridnm(*)
      double precision dt
c
c     Declaration of local variables
c
      integer      i ,j ,ie  ,nlc , nsk
      integer      ijaar ,imaand ,idag ,iuur ,imin ,isec, iscu
      integer      lun   ,ivar   ,nvar ,igrid,ifil ,istepf, istphf ,
     +             skip
      double precision    scudt
      character*40 idmap(4) , idhis(4)
      character*20 parnam(nentri),parnam1(nentri)  
      logical      new      , newuit
c
c     Declaration of external functions
c
      logical      yesmap
      external     yesmap
      save         istepf,istphf,scudt
c     Initialize.
c
c
c    Initialise
c
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
c
c     Write Map results
c
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
               write ( idmap(4),1000 ) ijaar,imaand,idag,
     +                                   iuur,imin,isec,iscu
               write(lun) (idmap(i) , i = 1,4)
               write(lun) nvar,ngrid
               write(lun) ( parnam(sacpre(salmap(i))+
     +                      salmap(i+1)) ,  i = 4,nsamap,2 )
               write(lun) (igrid , gridnm(igrid)(:20),
     +                     igrid = 1 , ngrid )
               salini(ifil) = 1
               istepf       = istep
            endif
c
            ivar = 0
            do 30 i = 4,nsamap,2
               ivar = ivar + 1
               ie = sacpre(salmap(i))+salmap(i+1)
c
               do 20 igrid = 1 , ngrid
                  if      (ie.eq.1) then  
                     buffer(ivar,igrid) = csa2(igrid)
                  else if (ie.eq.2) then  
                     buffer(ivar,igrid) = disgr(igrid)
                  else if (ie.eq.3) then  
                     buffer(ivar,igrid) = rho(igrid)
                  else if (ie.eq.4) then
                     buffer(ivar,igrid) = csa2(igrid) / rho(igrid)
     +                                    * 1000.                  
                  else if (ie.eq.5) then
                     buffer(ivar,igrid) = csa2(igrid) / 1.80655 
                  endif  
 20            continue
 30         continue
            write(lun) nint((istep-istepf)*scudt),
     +                 ((buffer(ivar,igrid),
     +                 ivar = 1 , nvar),
     +                 igrid = 1 , ngrid)
         endif
      endif
c
c     Write History results.
c
      nlc = saltim(1)
      new = mod(nsatim-nlc,2) .eq. 0
      if (new) then
         nsk = 3
      else
         nsk = 0
      endif
c
      if (nsatim .gt. 1+nsk) then
         if (new) then
            newuit = yesmap(saltim(nlc+2),saltim(nlc+3),saltim(nlc+4),
     &                      istep)
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
               write ( idhis(4),1000 ) ijaar,imaand,idag,
     +                                   iuur,imin,isec,iscu
               write(lun) (idhis(i) , i = 1,4)
               write(lun) nvar , saltim(1)
               write(lun) (parnam1(ivar),ivar=1,nvar)
               write(lun) (saltim(igrid),gridnm(saltim(igrid))(:20),
     +                     igrid = 2 , saltim(1)+1)
               salini(ifil) = 1
               istphf       = istep
            endif
c
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
                     buffer(ivar,j) = csa2(saltim(j+1)) / 
     +                                    rho(saltim(j+1)) * 1000.                  
                  else if (ie.eq.5) then
                     buffer(ivar,j) = csa2(saltim(j+1)) / 1.80655 
                  else   
                     skip = 1
                  endif  
   50          continue
               ivar = ivar - skip
   60       continue
            nvar = ivar            
            write(lun) nint((istep-istphf)*scudt),
     +                 ((buffer(ivar,j),
     +                 ivar = 1,nvar),
     +                 j    = 1,saltim(1))
         endif
      endif
c
 1000 format ('T0: ',I4.4,'.',I2.2,'.',I2.2,' ',I2.2,':',I2.2,':',I2.2,
     +        '  (scu=',i8,'s)')
c
      return
      end
