      subroutine KAFREH(nfremn ,nkfrmp ,nkfrtm ,nsamp  ,
     &                  itim   ,istep  ,nstep  ,first  ,lfilt  ,
     &                  kfrmap ,kfrtim ,res    ,scares ,rescov ,
     &                  frecpr ,buffer ,smploc ,gridnm ,ngrid  ,kalini ,
     &                  dt     )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAFRER (KAlman Filter REsidual results to
c                             HIS file)
c
c Module description: Write the user selected Kalman module results to
c                     the result file. The results processed in this
c                     module are scaled residuals and covariances of
c                     residuals.
c
c                     The stored data can be processed further by the
c                     User Interface. The user can select functions of
c                     place or functions of time. See [S-DD-002.1MR] for
c                     a specification of the nefis names.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 22 buffer(ngrid)     O  Buffer for results to be written to NEFIS.
c 10 first             I  True in case of first call.
c 21 frecpr(nfremn)    I  frecpr(i) = index in block table (1...nentri)
c                         for main code i of the residuals.
c  8 istep             I  Current time step number (t(n+1)).
c  7 itim              P  -
c 14 kfrmap(*)         I  Parameter list for residuals MAP block
c                         (1/2/3) Report step of begin, end, resp.
c                         increment.
c                         (4) Main code parameter 1.
c                         (5) Sub code parameter 1.
c                         (.) Etc.
c 15 kfrtim(*)         I  Parameter list for  residuals HIST
c                         block
c                         (1) Number of places.
c                         (2) Location 1.
c                         (i) Location n.
c                         (i+1) Main code parameter 1.
c                         (i+2) Sub code parameter 1.
c                         (.) Etc.
c 12 lfilt             I  = True if a filter step must be performed.
c  3 nfremn            I  Number of main codes of residuals.
c  4 nkfrmp            I  Number of entries in kfrmap.
c  5 nkfrtm            I  Number of entries in kfrtim.
c  6 nsamp             I  Number of hydrodynamic samples (measurements)
c 16 res(nsamp)        I  Residual vector
c 18 rescov(nsamp,     I  Matrix with covariances of residuals.
c      ,nsamp)
c 17 scares(nsamp)     I  Scaled residual vector
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c flsdat  FLuSh buffers of DATa file
c putrel  PUT Real ELement to a nefis file
c resadm  RESults; make ADMinistration for element selection
c resdes  RESults; DEScription group is defined
c resini  RESults; writing is INItialized
c restim  RESults; writing of current TIMe
c yesmap  YES MAP results are written
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kafreh.pf,v $
c Revision 1.2  1998/02/13  12:12:43  kuipe_j
c Adapt to CMT
c
c Revision 1.1  1997/06/17  11:26:48  kuipe_j
c output in history format
c
c Revision 1.4  1996/12/03  07:59:06  kuipe_j
c dimension of element names improved
c
c Revision 1.3  1996/09/03  14:54:24  kuipe_j
c frequency time hist,etc
c
c Revision 1.2  1996/04/12  13:04:56  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:34  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
      include '..\include\filsim.i'
c
c     Declaration of parameters
c
      integer      nentri
      parameter   (nentri=3)
      integer      nfremn ,nkfrmp ,nkfrtm ,nsamp  ,istep  ,nstep,
     &             ngrid
      integer      kfrmap(nkfrmp)  ,
     &             kfrtim(nkfrtm) ,frecpr(nfremn),itim  (2)
      integer      smploc(nsamp)  ,kalini(*)
      real         buffer(nentri,ngrid)   ,res(nsamp)    ,scares(nsamp),
     &             rescov(nsamp,nsamp)
      logical      first  ,lfilt
      double       precision dt
c
c     Declaration of local variables
c
      integer      i ,j ,ie , ismp, nsk, nlc , isamp 
      integer      ijaar ,imaand ,idag ,iuur ,imin ,isec,iscu
      integer      lun   ,ivar   ,nvar ,igrid,ifil ,istepf, istphf
      integer      code(nentri)
      integer      modgrd
      double precision    scudt
      character*40 idmap(4) , idhis(4)
      character*40 gridnm(ngrid)
      character*20 parnam(3)  
      logical      new      ,newuit 
c
c     Declaration of external functions
c
      logical      yesmap
      external     yesmap
      save         istepf, istphf,scudt
c
      modgrd (i) = mod(i-1,ngrid) + 1
c
      data (code(i),i=1,nentri) / 1, 1, 1/
c
c     Initialize.
c
      if (first) call resadm (nentri ,code   ,frecpr )
c
      idmap(1)  = 'SOBEK                                   '
      idmap(2)  = 'Maps results at gridpoints              '
      idmap(3)  = '                                        '
      idmap(4)  = '                                        '
      parnam(1) = 'Residual            '
      parnam(2) = 'Scaled residual     '
      parnam(3) = 'Residual variance   '
      idhis(1)  = 'SOBEK                                   '
      idhis(2)  = 'History results at gridpoints           '
      idhis(3)  = '                                        '
      idhis(4)  = '                                        '
c
      if (lfilt) then
c
c       Write Map results
c
        if (nkfrmp .gt. 3) then
          if (yesmap(kfrmap(1),kfrmap(2),kfrmap(3),istep)) then
            nvar = nint(0.5*(nkfrmp-3))
            ifil = 9 
            lun  = 68
            if ( kalini(ifil) .eq. 0 ) then
#if defined (USE_MSWINDOWS)
               open(lun , file = firmap , form = 'binary')      
#else
#if defined (USE_HPUX)
               open(lun , file = firmap , form = 'unformatted') 
#else
               open(lun , file = firmap , form = 'unformatted') 
#endif
#endif
               iscu   = nstep*dt/1.0d9+1.d0
               scudt  = dt/dble(iscu)
               call parsdt(itim,ijaar,imaand,idag,iuur,imin,isec)
               write ( idmap(4),1000 ) ijaar,imaand,idag,
     +                                   iuur,imin,isec,iscu
               write(lun) (idmap(i) , i = 1,4)
               write(lun) nvar,nsamp 
               write(lun) ( parnam(frecpr(kfrmap(i))+
     +                      kfrmap(i+1)) ,  i = 4,nkfrmp,2 )
               write(lun) (isamp , 
     +                     gridnm(modgrd(smploc(isamp)))(:20),
     +                     isamp = 1 , nsamp )
               kalini(ifil) = 1
               istepf = istep
            endif
c
            ivar=0
            do 50 i = 4,nkfrmp,2
               ivar = ivar + 1
               ie = frecpr(kfrmap(i))+kfrmap(i+1)
c
               if     (ie.eq.1) then
                  do 15 j = 1, nsamp
                     buffer(ivar,j) = res(j)
   15             continue
               elseif (ie.eq.2) then
                  do 25 j = 1, nsamp
                     buffer(ivar,j) = scares(j)
   25             continue
               elseif (ie.eq.3) then
                  do 35 j = 1, nsamp
                     buffer(ivar,j) = rescov(j,j)
   35             continue
               endif
   50       continue
            write(lun) nint((istep-istepf)*scudt),
     +                 ((buffer(ivar,isamp),
     +                 ivar  = 1 , nvar),
     +                 isamp = 1 , nsamp)
          endif
        endif
c
c       Write History results.
c
        nlc = kfrtim(1)
        new = mod(nkfrtm-nlc,2) .eq. 0

        if ( new ) then
           nsk = 3
        else
           nsk = 0
        endif
c
        if (nkfrtm .gt. 1+nsk) then
          if (new) then
            newuit = yesmap(kfrtim(nlc+2),kfrtim(nlc+3),kfrtim(nlc+4),
     &                      istep)
          else
            newuit = .false.
          endif
          if ( (kfrtim(1).gt.0 .and. .not. new) .or. newuit ) then
            nvar = nint(.5*(nkfrtm-kfrtim(1)-2-nsk+1))
            ifil = 10
            lun  = 69
            if ( kalini(ifil) .eq. 0 ) then
#if defined (USE_MSWINDOWS)
               open(lun , file = firhis , form = 'binary')      
#else
#if defined (USE_HPUX)
               open(lun , file = firhis , form = 'unformatted') 
#else
               open(lun , file = firhis , form = 'unformatted') 
#endif
#endif
               iscu   = nstep*dt/1.0d9+1.d0
               scudt  = dt/dble(iscu)
               call parsdt(itim,ijaar,imaand,idag,iuur,imin,isec)
               write ( idhis(4),1000 ) ijaar,imaand,idag,
     +                                   iuur,imin,isec,iscu
               write(lun) (idhis(i) , i = 1,4)
               write(lun) nvar , kfrtim(1)
               write(lun) (parnam(frecpr(kfrtim(i))+kfrtim(i+1)),
     +                     i = kfrtim(1)+2+nsk,nkfrtm,2)
               write(lun) (kfrtim(igrid),
     +                     gridnm(modgrd(smploc(kfrtim(igrid))))(:20),
     +                     igrid = 2 , kfrtim(1)+1)
               kalini(ifil) = 1
               istphf       = istep
            endif
c
            ivar=0
            do 100 i = kfrtim(1)+2+nsk,nkfrtm,2
               ivar = ivar + 1
               ie   = frecpr(kfrtim(i))+kfrtim(i+1)
               if     (ie.eq.1) then
                  do 65 j=1,kfrtim(1)
                     ismp = kfrtim(j+1)
                     buffer(ivar,j) = res(ismp)
   65             continue
               elseif (ie.eq.2) then
                  do 75 j=1,kfrtim(1)
                     ismp = kfrtim(j+1)
                     buffer(ivar,j) = scares(ismp)
   75             continue
               elseif (ie.eq.3) then
                  do 85 j=1,kfrtim(1)
                     ismp = kfrtim(j+1)
                     buffer(ivar,j) = rescov(ismp,ismp)
   85             continue
               endif
  100       continue
            write(lun) nint((istep-istphf)*scudt),
     +                 ((buffer(ivar,j),
     +                 ivar = 1,nvar),
     +                 j    = 1,kfrtim(1))
         endif
       endif
      endif
c
 1000 format ('T0: ',I4.4,'.',I2.2,'.',I2.2,' ',I2.2,':',I2.2,':',I2.2,
     +        '  (scu=',i8,'s)')
c
      end
