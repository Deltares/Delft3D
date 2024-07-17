! Test Fortran interface RIZA OpenMI buffer
! Use Rob functions from DLL !!! (instead of lib)
PROGRAM RobTest
  use IRob
  use Frob  

  character (len=25)              :: ModelID = 'RobTest'
  character(len=2),dimension(1)   :: QuantitySet= (/'q2'/)
  character(len=4),dimension(5)   :: Quantity = (/'q1  ','q2  ','q2_1', 'q2_2','q2_3'/)
  character(len=16),dimension(5)  :: Description = (/'q1_description  ','q2_description  ','q2_1_description', 'q2_2_description', 'q2_3_description'/)
  character(len=5),dimension(3)   :: Unit = (/'m3/s','kg/m3','m'/)
  character(len=10),dimension(3)   :: ElementID = (/'Element1','Element2','Element3'/)
  integer                         :: fp, n,j,k,m
  character(len=132)              :: s,s0,s1
  real(kind=8),dimension(1)       :: D0,D1
  real(kind=8),pointer            :: pD0(:), pD1(:)

  fp = 99
  open(fp,file='test.txt')

  n = FRobModelAdd (ModelID, ModelID);

  ! Add some Units
  do j=1,3
    n = FRobUnitAdd(Unit(j), 'Unit description', real(1.,8), real(2.,8))
  end do

  ! Quantities (with properties)
  do j=1,2
    n=  FRobQuantityAdd (Quantity(j) ,  Description(j), Unit(1));
    n = FRobQuantityAddProperty('', Quantity(j),'Property 1', 'Property Value 1');
    n = FRobQuantityAddProperty('', Quantity(j),'Property 2', 'Property Value 2');
  end do

  ! Get the value for some properties
  s = FRobQuantityPropertyValue('','q2','Property 1');
  s = FRobQuantityPropertyValue('','q2','Property 2');
  s = FRobQuantityPropertyValue('','q1','Property 2');

  ! Create a QuantitySet by adding Quantities to an existing Quantity
  n=  FRobQuantityAddQuantity (Quantity(2), Quantity(3) , Description(3), Unit(2));
  n=  FRobQuantityAddQuantity (Quantity(2), Quantity(4) , Description(4), Unit(3));
  n=  FRobQuantityAddQuantity (Quantity(2), Quantity(5) , Description(5), Unit(3));
  n=  FrobQuantityGetInfo(Quantity(2), s0,s1,NumberOf)

  ! Add properties to Quantities in set
  do j=3,5
    n = FRobQuantityAddProperty(Quantity(2),Quantity(j),"Property 1", "Property Value 1")
    n = FRobQuantityAddProperty(Quantity(2),Quantity(j),"Property 2", "Property Value 2")
  end do

  n=FrobQuantityGetInfo(Quantity(2), s0,s1,NumberOf)
  do j=1,NumberOf
    s= FRobQuantitySetGetQuantityID(Quantity(2),j)
  end do

  ! Set a new value for property
  write(fp,'(a)') 'Change property value for the second quantity in the set'
  n = FRobQuantitySetPropertyValue("q2",Quantity(4),"Property 2","New Property Value")
  s = FRobQuantityPropertyValue("q2",Quantity(4),"Property 2")
  write(fp,'(a)') 'Value now '//s(1:len_trim(s))


  ! Remove second, first & last entry
  n = FRobQuantitySetRemoveQuantity(Quantity(2),Quantity(4))
  n=FrobQuantityGetInfo(Quantity(2), s0,s1,NumberOf)

  write(fp,'(a)') 'Remove : '//Quantity(2)
  n = FRobQuantitySetRemoveQuantity(Quantity(2),Quantity(3))
  n = FrobQuantityGetInfo(Quantity(2), s0,s1,NumberOf)  
  
  write(fp,'(a)') 'Remove : '//Quantity(5)
  n = FRobQuantitySetRemoveQuantity(Quantity(2),Quantity(5))
  n=FrobQuantityGetInfo(Quantity(2), s0,s1,NumberOf)

  ! (Re)Create the QuantitySet
  n=  FRobQuantityAddQuantity (Quantity(2), Quantity(3) , Description(3), Unit(2));
  n=  FRobQuantityAddQuantity (Quantity(2), Quantity(4) , Description(4), Unit(3));
  n=  FRobQuantityAddQuantity (Quantity(2), Quantity(5) , Description(5), Unit(3));

  ! Create an ElementSet
  do j=1,3
    n = FRobElementAdd(ElementID(j));
    n = FRobElementsetAdd('TestSet ID','TestSet Description' ,ELEMENTTYPE_IDBASED,ElementID(j));
  end do

  ! Create the ExchangeItems for the Quantity and QuantitySet
  do k=1,2
    n = FRobExchangeitemAdd (ModelID, Quantity(k), 'TestSet ID', PROVIDING_ROLE);
    n = FRobExchangeitemAdd (ModelID, Quantity(k), 'TestSet ID', ACCEPTING_ROLE);
  end do

  ! Check RobPutDouble (single Quantity)
  D0(1) = 10.; D1(1) = 20.
  write(fp,'(a)') 'RobPutDouble :'
  n = FRobPutDouble(ModelID, '', Quantity(1),'TestSet ID', PROVIDING_ROLE, 1, D0 );
  write(fp,'(2a,f8.2)') 'FRobPutDouble (all elements same value) : Quantity '//Quantity(1), ' ElementSetID '//'TestSet ID PROVIDING ',D0
  n = FRobPutDouble(ModelID, '', Quantity(1),'TestSet ID', ACCEPTING_ROLE, 1, D1 );
  write(fp,'(2a,f8.2)') 'FRobPutDouble (all elements same value) : Quantity '//Quantity(1), ' ElementSetID '//'TestSet ID ACCEPTING ',D1

  ! Check RobPutDouble (QuantitySet)
  n= FRobElementsetCount('TestSet ID');
  allocate(pD0(n),pD1(n),stat=i)


  do j=3,5
    do m=1,n
      pD0(m) = m*10 + m+1 * j+1;
      pD1(m) = 2*pD0(m)
    end do

    m = FRobPutDouble(ModelID, Quantity(2), Quantity(j), 'TestSet ID', PROVIDING_ROLE, n, pD0 );
    write(fp,'(2a)',advance='no') 'RobPutDouble : QuantitySet '//Quantity(2),' Quantity '//Quantity(j)//' ElementID PROVIDING TestSet ID'
    write(fp,'(1000f8.2)') (pD0(m),m=1,n)

    m = FRobPutDouble(ModelID, Quantity(2), Quantity(j), 'TestSet ID', ACCEPTING_ROLE, n, pD1 );
    write(fp,'(2a)',advance='no') 'RobPutDouble : QuantitySet '//Quantity(2),' Quantity '//Quantity(j)//' ElementID ACCEPTING TestSet ID'
    write(fp,'(1000f8.2)') (pD1(m),m=1,n)
  end do

  ! Check RobGetDouble (single Quantity)
  write(fp,'(a)')'RobGetDouble :'
  m = FRobGetDouble(ModelID, '', Quantity(1),'TestSet ID', PROVIDING_ROLE,n, pD0 );
  write(fp,'(a,i3,a)', advance='no')'RobGetDouble Quantity '//Quantity(1)//' Role = PROVIDING, QuantitySet =TestSet ID (result=',m,')'
  write(fp,'(1000f8.2)') (pD0(m),m=1,n)

  m = FRobGetDouble(ModelID, '', Quantity(1),'TestSet ID', ACCEPTING_ROLE, n, pD1 );
  write(fp,'(a,i3,a)', advance='no')'RobGetDouble Quantity '//Quantity(1)//' Role = ACCEPTING, QuantitySet =TestSet ID (result=',m,')'
  write(fp,'(1000f8.2)') (pD1(m),m=1,n)

  ! Check RobGetDouble (QuantitySet)
  do j=3,5
     m = FRobGetDouble(ModelID, Quantity(2), Quantity(j), 'TestSet ID', PROVIDING_ROLE, n, pD0 );
     write(fp,'(a,i3)', advance='no')'RobGetDouble QuantitySet '//Quantity(2)(1:len_trim(Quantity(2)))//' Quantity='//Quantity(j)(1:len_trim(Quantity(j)))//' Role = PROVIDING,result=',m
     write(fp,'(1000f8.2)') (pD0(m),m=1,n)

     m = FRobGetDouble(ModelID, Quantity(2), Quantity(j), 'TestSet ID', ACCEPTING_ROLE, n, pD1 );
     write(fp,'(a,i3)', advance='no')'RobGetDouble QuantitySet '//Quantity(2)(1:len_trim(Quantity(2)))//' Quantity='//Quantity(j)(1:len_trim(Quantity(j)))//' Role = ACCEPTING,result=',m
     write(fp,'(1000f8.2)') (pD1(m),m=1,n)
  end do

  call RobFinalize();
  close(fp);

end program
