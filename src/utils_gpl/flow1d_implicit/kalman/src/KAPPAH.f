      subroutine KAPPAH(nppamn ,nkppmp ,nkpptm ,ngrid  ,
     &                  itim   ,istep  ,nstep  ,first  ,
     &                  kppmap ,kpptim ,pi     ,np     ,p2     ,nkapar ,
     &                  ppacpr ,buffer ,corrnm ,kalini ,dt     )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         A.W.J.Koster             
c
c Module:             KAPPAH (KAlman Predicted PArameter results to HIS
c                             file)
c
c Module description: Write the user selected Kalman module results to
c                     the HIS file. The results processed in this
c                     module are predicted covariances of the correction
c                     parameters.
c
c                     The stored data can be processed further by the
c                     User Interface. The user can select functions of
c                     place or functions of time.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 22 buffer(ngrid)     O  Buffer for results to be written to NEFIS.
c  2 dafdrs            P  -
c 10 first             I  True in case of first call.
c  8 istep             I  Current time step number (t(n+1)).
c  7 itim              P  -
c 13 kppmap(*)         I  Parameter list for prediction results of para-
c                         meters MAP block.
c                         (1/2/3) Report step of begin, end, resp.
c                         increment.
c                         (4) Main code parameter 1.
c                         (5) Sub code parameter 1.
c                         (.) Etc.
c 14 kpptim(*)         I  Parameter list for prediction results of para-
c                         meters HIST block.
c                         (1) Number of places.
c                         (2) Location 1.
c                         (i) Location n.
c                         (i+1) Main code parameter 1.
c                         (i+2) Sub code parameter 1.
c                         (.) Etc.
c  6 ngrid             I  Number of grid points in network.
c 18 nkapar            I  = nnf + nnmu + 1
c  4 nkppmp            I  Number of entries in kppmap.
c  5 nkpptm            I  Number of entries in kpptim.
c 16 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
c  3 nppamn            I  Number of main codes of parameter prediction
c 17 p2(np,np)         I  Matrix with covariances of waterlevels,
c                         discharges and uncertain correction parameters
c                         (bed friction, contraction in case of free gate
c                         flow and wind) on time level n+1|n (predicted
c                         values).
c 15 pi                I  prediction interval (number of prediction steps)
c 21 ppacpr(nppamn)    I  ppacpr(i) = index in block table (1...nentri)
c                         for main code i of the parameter prediction
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c flsdat  FLuSh buffers of DATa file
c putiel  PUT Integer ELement to nefis file
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
c $Log: kappah.pf,v $
c Revision 1.2  1998/02/13  12:12:45  kuipe_j
c Adapt to CMT
c
c Revision 1.1  1997/06/17  11:26:50  kuipe_j
c output in history format
c
c Revision 1.4  1996/12/03  07:59:08  kuipe_j
c dimension of element names improved
c
c Revision 1.3  1996/09/03  14:54:27  kuipe_j
c frequency time hist,etc
c
c Revision 1.2  1996/04/12  13:05:15  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:48  kuipe_j
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
      parameter   (nentri=2)
      integer      nppamn ,nkppmp ,nkpptm ,ngrid  ,istep  ,nstep ,
     &             pi     ,np     ,nkapar   
      integer      kppmap(nkppmp),kpptim(nkpptm) ,ppacpr(nppamn),
     &             itim  (2)     ,kalini(*) 
      real         p2(np,np)     ,buffer(nentri,ngrid)
      logical      first   
      double       precision dt
c
c     Declaration of local variables
c
      integer      i ,j ,k ,ie , igr, nlc, nsk ,  ipar
      integer      ijaar ,imaand ,idag ,iuur ,imin ,isec,iscu
      integer      lun   ,ivar   ,nvar ,ifil ,istepf, istphf
      integer      code(2)
      double       precision scudt
      character*40 idmap(4) , idhis(4)
      character*40 corrnm(*)  
      character*20 parnam(3)  
      logical      new      , newuit
c
c     Declaration of external functions
c
      logical      yesmap
      external     yesmap
      save         istepf, istphf,scudt
c
      data (code(i),i=1,nentri) / 1, 2 /
c
c     Initialize.
c
      if (first) call resadm (nentri ,code   ,ppacpr )
c
      idmap(1)  = 'SOBEK                                   '
      idmap(2)  = 'Maps results at gridpoints              '
      idmap(3)  = '                                        '
      idmap(4)  = '                                        '
      parnam(1) = 'Prediction interval '
      parnam(2) = 'Predict. param. var.'
      idhis(1)  = 'SOBEK                                   '
      idhis(2)  = 'History results at gridpoints           '
      idhis(3)  = '                                        '
      idhis(4)  = '                                        '
c
c     Write Map results
c
      if (nkppmp .gt. 3) then
         if (yesmap(kppmap(1),kppmap(2),kppmap(3),istep)) then
            nvar = nint(0.5*(nkppmp-3))
            ifil = 3 
            lun  = 62
            if ( kalini(ifil) .eq. 0 ) then
#if defined (USE_MSWINDOWS)
               open(lun , file = prpmap , form = 'binary')      
#else
#if defined (USE_HPUX)
               open(lun , file = prpmap , form = 'unformatted') 
#else
               open(lun , file = prpmap , form = 'unformatted') 
#endif
#endif
               iscu   = nstep*dt/1.0d9+1.d0
               scudt  = dt/dble(iscu) 
               call parsdt(itim,ijaar,imaand,idag,iuur,imin,isec)
               write ( idmap(4),1000 ) ijaar,imaand,idag,
     +                                 iuur,imin,isec,iscu
               write(lun) (idmap(i) , i = 1,4)
               write(lun) nvar,nkapar
               write(lun) ( parnam(ppacpr(kppmap(i))+
     +                      kppmap(i+1)) ,  i = 4,nkppmp,2 )
               write(lun) (ipar , corrnm(ipar)(:20),
     +                     ipar = 1 , nkapar)
               kalini(ifil) = 1
               istepf = istep
            endif
c
            ivar = 0
            do 40 i = 4,nkppmp,2
               ivar = ivar + 1
               ie = ppacpr(kppmap(i))+kppmap(i+1)
               if      (ie.eq.1) then
                  do 20 k = 1,nkapar
                     buffer(ivar,k) = pi      
   20             continue
                     k = k + 1
               else if (ie.eq.2) then
                  k = 1
                  do 25 j = ngrid*2+1, np
                     buffer(ivar,k) = p2(j,j)
                     k = k + 1
   25             continue
               endif
   40       continue
            write(lun) nint((istep-istepf)*scudt),
     +                 ((buffer(ivar,ipar),
     +                 ivar = 1 , nvar),
     +                 ipar = 1 , nkapar)
         endif
      endif
c
c     Write History results.
c
      nlc = kpptim(1)
      new = mod(nkpptm-nlc,2) .eq. 0

      if ( new ) then
         nsk = 3
      else
         nsk = 0
      endif
c
      if (nkpptm .gt. 1+nsk) then
         if (new) then
            newuit = yesmap(kpptim(nlc+2),kpptim(nlc+3),kpptim(nlc+4),
     &                      istep)
         else
            newuit = .false.
         endif
         if ( (kpptim(1).gt.0 .and. .not. new) .or. newuit ) then
            nvar = nint(.5*(nkpptm-kpptim(1)-2-nsk+1))
            ifil = 4
            lun  = 63
            if ( kalini(ifil) .eq. 0 ) then
#if defined (USE_MSWINDOWS)
               open(lun , file = prphis , form = 'binary')      
#else
#if defined (USE_HPUX)
               open(lun , file = prphis , form = 'unformatted') 
#else
               open(lun , file = prphis , form = 'unformatted') 
#endif
#endif
               iscu   = nstep*dt/1.0d9+1.d0
               scudt  = dt/dble(iscu) 
               call parsdt(itim,ijaar,imaand,idag,iuur,imin,isec)
               write ( idhis(4),1000 ) ijaar,imaand,idag,
     +                                   iuur,imin,isec,iscu
               write(lun) (idhis(i) , i = 1,4)
               write(lun) nvar , kpptim(1)
               write(lun) (parnam(ppacpr(kpptim(i))+kpptim(i+1)),
     +                     i = kpptim(1)+2+nsk,nkpptm,2)
               write(lun) (kpptim(ipar),corrnm(kpptim(ipar))(:20),
     +                     ipar = 2 , kpptim(1)+1)
               kalini(ifil) = 1
               istphf       = istep
            endif
c
            ivar = 0
            do 80 i = kpptim(1)+2+nsk,nkpptm,2
               ivar = ivar + 1
               ie   = ppacpr(kpptim(i))+kpptim(i+1)
               if      (ie.eq.1) then
                  do 62 j=1,kpptim(1)
                     buffer(ivar,j) = pi                
   62             continue
               else if (ie.eq.2) then
                  do 65 j=1,kpptim(1)
                     igr = kpptim(j+1) + 2*ngrid
                     buffer(ivar,j) = p2(igr,igr)
   65             continue
               endif
   80       continue
            write(lun) nint((istep-istphf)*scudt),
     +                 ((buffer(ivar,j),
     +                 ivar = 1,nvar),
     +                 j    = 1,kpptim(1))
         endif
      endif
c
 1000 format ('T0: ',I4.4,'.',I2.2,'.',I2.2,' ',I2.2,':',I2.2,':',I2.2,
     +        '  (scu=',i8,'s)')
c
      return
      end
