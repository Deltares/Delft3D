      module gadata
c
c     arrays with length nreeksf
      real         ,allocatable, save :: tijdenf(:)   ,reeksf(:)
c     arrays with length nreeks / nreeksi
      real         ,allocatable, save :: tijden(:)    ,reeks(:) ,
     +                                   coreeks(:)
      double precision ,allocatable, save ::  y2(:)   ,u(:)
c     arrays with length lrenbuf
      real         ,allocatable, save :: reeksen(:)   
c     arrays with length nvar*nloc
      real         ,allocatable, save :: buffer(:)   
c     arrays with length nvar
      character*20 ,allocatable, save :: parnam(:)
c     arrays with length nloc
      character*20 ,allocatable, save :: locnam(:)
      integer, parameter :: hreeks=1 ,qreeks=2 ,vreeks=3 ,creeks=4 ,
     +                      sreeks=5 ,greeks=6
      
c     array with unknown length garesult
      real         ,allocatable, save :: garesult(:) 
      integer     ::  avail=0
c 
      integer, parameter :: type    = 1  ,grooth  = 2  ,locat   = 3  ,
     +                      ngetij  = 4  ,   
     +                      igetij  = 1  ,begin   = 2  ,eind    = 3  , 
     +                      duur    = 4  ,gemidd  = 5  ,slag    = 6  ,
     +                      mintijd = 7  ,minw    = 8  ,maxtijd = 9  ,
     +                      maxw    =10  ,vloed   =11  ,eb      =12  ,
     +                      nummer  =13     
c      
c                   type    type van de groorheid
c 1                 grooth  grootheidsnummer
c 2                 locat   locatie nummer
c 3                 ngetij  aantal getijden
c 3+(igetij-1) +1   igetij  getijnummer = 1
c 3+(igetij-1) +2   begin   begintijd getij
c 3+(igetij-1) +3   eind    eindtijd getij
c 3+(igetij-1) +4   duur    duur getij
c 3+(igetij-1) +5   gemidd  middenstand / reststroom / gemiddelde 
c 3+(igetij-1) +6   slag    getijslag / delta
c 3+(igetij-1) +7   mintijd tijd minimum 
c 3+(igetij-1) +8   minw    minimum waarde
c 3+(igetij-1) +9   maxtijd tijd maximum
c 3+(igetij-1) +10  maxw    maximum waarde
c 3+(igetij-1) +11  vloed   vloedvolume / gemiddelde (positief)
c 3+(igetij-1) +12  eb      ebvolume / gemiddelde (negatief)
c 3+(igetij-1) +1   nummer  getijnummer = 2      
      end module
