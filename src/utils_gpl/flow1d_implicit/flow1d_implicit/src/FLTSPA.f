      subroutine fltspa(strpar ,istru  ,nstru  ,stdbq ,nstdb1 ,s     ,
     &                  xnod   ,ynod   ,n      ,m     ,deltah ,inttyp,
     &                  zs     ,zero   ,juer   ,ker   )
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
c Module:             FLTSPA (FLow Tabulated Structure get PAramaters)
c
c Module description: Parameters for the data base are extracted
c                     from the packed array strpar.
c
c                     Also at the first call the data in data base 
c                     STDBQ containing the Q-h relation is reordered.
c
c-----------------------------------------------------------------------
c Parameters:
c NAME             IO DESCRIPTION
c deltah            O switch for type of arguments ynod:
c                     true:  ynod = h1-h2
c                     false: ynod = h2
c inttyp            O 1 = linear inerpolation
c                     2 = spline     
c istru             I structure number 
c m                 O number of columns (h2-values) in the Q-h table-1
c n                 O number of rows (h1-values) in the Q-h table-1
c nstdb1            I number of elements of data base for this structure
c nstru               number of structures
c strpar(21,nstru) IO Parameters for data base structure: 
c                     (1,i) interpolation:
c                           1 = linear
c                           2 = spline
c                     (2,i) sequence number of data base structure 
c                           (1,2,…), i.e. data base structure number
c                     (3,i) number of dimensions of data base (2 or 3)
c                     (4,i) number of H1-values nh1
c                     (5,i) number of H2-values nh2
c                     (6,i) number of values of gate height, crest level 
c                           or width
c                           (extension in future; 1 in Sobek 2.51)
c                     (7,i) dummy
c                     (8,i) dummy
c                     (9,i) index of first element in array with 
c                           discharges stdbq for this structure
c                     (10,i) type of values of first row in stdbq for 
c                           this structure:
c                           1 : H2-values
c                           2 : dH-values (dH=H2-H1)
c                     (11,i) number of structure where the data base 
c                           of this structure is defined. This is
c                           the structure with the first occurrence 
c                           of a link to this data base table.
c                     (12,i) indicator that identifies if this data 
c                           base table is initialised:
c                           0 : no
c                           1 : yes
c                     (13,i) level of sill of structure or reference
c                           level
c                     (14,i) indicator that defines if discharge is zero
c                           if sill is dry (0), otherwise 1.
c                     (15,i) minimum H1 (wrt sill) in case of underflow 
c                           (otherwise 1E20)  
c                     (16,i) minimum H2/Dh in case of underflow 
c                           (otherwise 1E20)  
c                     (17,i) maximum H1 (wrt sill) in case of overflow 
c                           (otherwise -1E20)  
c                     (18,i) maximum H2/DH in case of overflow
c                           (otherwise -1E20)
c                     (19,i) 4-digit indicater for under or overflow in
c                           an iteration step.
c                           From right to left a digit corresponds for:
c                           H1-underflow, H2/DH-underflow,
c                           H1-overflow, H2/DH-overflow.
c                           A digit can be 0 (inside) or 1 (outside 
c                           data base).
c stdbq            IO Table containing discrete Q-h relation, h1 values      
c                     and h2 (or h1-h2) values at initial input:
c                     stdbq(-1,-1)= dummy / gate value
c                     stdbq(i,-1) = h1(i)                   for i=0(1)n
c                     stdbq(-1,j) = (h1-h2)(j)  or
c                                   h2(j)                   for j=0(1)m
c                     stdbq(i,j)  = Q(i,j)      for i=0(1)n and j=0(1)m
c                     At output and following calls:
c                     stdbq(1)      dummy / gate value
c                     stdbq(2..)    h1(i)                   for i=0(1)n
c                     stdbq(3+n..)  (h1-h2)(j) or 
c                                   h2(j)                   for j=0(1)m
c                     stdbq(4+m+n..)Q(i,j)      for i=0(1)n and j=0(1)m
c s                 O pointer to array s (contains Q)
c xnod              O pointer to array xnod (contains h1)
c ynod              O pointer to array ynod (contains h2 or h1-h2)
c zs                O level of sill
c-----------------------------------------------------------------------
c
c     Include constants for array dimensions
c
      include '../include/sobdim.i'
c
c     Declaration of parameters:
c
      integer    istru ,nstru     ,nstdb1 ,s     ,xnod  ,ynod ,
     &           n     ,m         ,inttyp ,juer  ,ker
      real       zs
      real       strpar(dmstrpar,nstru) ,stdbq  (nstdb1)
      logical    deltah,zero
c
c     Declaration of external
c
      external   wlcrest0
      logical    wlcrest0
c      
c     Declaration of local variables:
c      
      integer    nh1   ,nh2       ,sdbdef ,j    ,ijget  ,ijput ,
     &           ind   ,ij1       ,ij2    ,m2   ,lstnam
      real       h2 
      logical    inidb ,initial
      character  strnam*40
c
c     Include sobek constants and error codes
c
      include '../include/errcod.i'
c      
      nh1     = nint(strpar(4,istru))
      nh2     = nint(strpar(5,istru))
      n       = nh1 - 1
      m       = nh2 - 1
      deltah  = nint(strpar(10,istru)).eq.2
      inttyp  = nint(strpar(1 ,istru))
      sdbdef  = nint(strpar(11,istru))
      initial = nint(strpar(12,istru)).eq.0
      inidb   = nint(strpar(12,sdbdef)).eq.0
      zs      =      strpar(13,istru)
      zero    = nint(strpar(14,sdbdef)).eq.0
c      
      xnod    = 2
      ynod    = 2+nh1
      s       = 2+nh1+nh2  
c
c     Initialize limits
c
      if (initial) then
c        for current structure      
         strpar(15,istru) = 1.e20
         strpar(16,istru) = 1.e20
         strpar(17,istru) = -1.e20
         strpar(18,istru) = -1.e20
         strpar(12,istru) = 1.
      endif
      if (inidb) then
c        for structure that contains data base definition
         strpar(15,sdbdef) = 1.e20
         strpar(16,sdbdef) = 1.e20
         strpar(17,sdbdef) = -1.e20
         strpar(18,sdbdef) = -1.e20
      endif
c
      if (inidb) then
c
c       Reorder structure data base
c       j is index in s from 0..m      
c       First shift h2-values from columns to second row
c
        do j=1,m
           ijget = (m+2-j)*(n+2)+j
           ijput = n+3+j
           h2    = stdbq(ijget)
           do ind = ijget-1,ijput,-1
              stdbq(ind+1) = stdbq(ind)
           enddo
           stdbq(ijput) = h2
        enddo
c
c       h2-values in second row are in reverse order,
c       so they are put in increasing order.
c
        m2=m/2
        do j=1,m2
          ij1 = n+3+j
          ij2 = n+m+4-j
          h2  = stdbq(ij1)
          stdbq(ij1) = stdbq(ij2)
          stdbq(ij2) = h2
        enddo   
c
c       Set indicator of structure where data base was defined 
c       (first occurence at input) at 'initialized'.

        strpar(12,sdbdef) = 1.0
        
        zero = wlcrest0 (stdbq(s) ,stdbq(xnod) ,stdbq(ynod) ,n ,m ,
     +                   deltah   )
        if (zero) then
           strpar(14,sdbdef)= 0
        else
           strpar(14,sdbdef)= 1
        endif   

      endif
c
c     Warning if discharge is not zero at sill
c
      if (initial .and. .not.zero) then
c         call getstr(istru,strnam,lstnam)
c         ker = warnng
c         call sre_error (juer,'FLTSPA Q not zero in structure @'
c     +               //strnam(:lstnam)//'@ if sill dry', eflcr0, ker)         
      endif   
      if (inidb .and. .not.zero) then
c         call getstr(sdbdef,strnam,lstnam)
         if (sdbdef.ne.istru) then   
c            call getstr(sdbdef,strnam,lstnam)
c            ker = warnng
c            call sre_error (juer,'FLTSPA Q not zero in structure @'
c     +                  //strnam(:lstnam)//'@ if sill dry', eflcr0, ker)
         endif
      endif
c   
      return
c
      end
      function wlcrest0 (s ,xnod, ynod ,n ,m ,deltah)
c
c     Function determines if discharge is zero when crest becomes
c     dry.
c
      integer  n         ,m     
      real     s(0:n,0:m), xnod(0:n) ,ynod(0:m)
      logical  wlcrest0  ,deltah
c    
      logical  nul
      integer  j    ,jj
c
c     Declaration of external
c
      external   epsequ
      logical    epsequ
c
      if (deltah) then
         nul = .true.
         j = 0
         do while (.not.EPSEQU (ynod(j),0.,1.e-8))
            j = j+1
         enddo
         do jj=j,m
            nul = nul .and. (EPSEQU (s(0,jj),0.,1.e-6))
         enddo   
      else 
         if (EPSEQU (xnod(0),0.,1.e-8) .and. EPSEQU (ynod(0),0.,1.e-8) 
     +       .and. EPSEQU (s(0,0),0.,1.e-6)) then
            nul = .true.
         else 
            nul = .false.
         endif
      endif
      wlcrest0 = nul
c
      end
