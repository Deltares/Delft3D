#include "DimrTests.h"
#include <direct.h>
#include <stdio.h>
#include "../../dimr_lib/include/dimr_unit.h"
#include "../../dimr_lib/include/dimr.h"

/*
class MyXmlTree : public IXmlTree
{
public:
	MyXmlTree() {  };
	void AddAttrib(const char* name, const char* value) override {};
	void AddChild(IXmlTree* child) override{}
	void ExpandEnvironmentVariables() override{}
	void ExpandEnvironmentVariables(int instance) override{}
	const char* GetAttrib(const char* name) override { return "\0"; }
	bool GetBoolAttrib(const char* name) override { return true; }
	long GetIntegerAttrib(const char* name) override { return 0; }
	double GetFloatAttrib(const char* name) override { return 0.0; }
	IXmlTree* Lookup(const char* pathname) override { return NULL; }
	IXmlTree* Lookup(const char* pathname, int instance) override{ return NULL; }
	int Lookup(const char* pathname, int instance, keyValueLL*& kvlist) override { return 0; }
	const char* GetElement(const char* name) override{ return "\0"; }
	bool GetBoolElement(const char* name, bool defaultValue) override{ return true; }
	void Print() override{}
};
*/

TEST_CLASS(DimrGlobalTests)
{

public:
	
    TEST_METHOD(initializeWithoutFileNameTest)
	{
		Assert::AreEqual(initialize(""), (int)Exception::ERR_OS);
	}
	
	TEST_METHOD(initializeWithFileNameTest)
	{
		Assert::AreEqual(initialize("dimr.xml"), (int)Exception::ERR_INVALID_INPUT);
	}

	TEST_METHOD(initializeWithEmptyFileNameAndFileTest)
	{
		std::ofstream file{ "dimr.xml" };

		Assert::AreEqual(initialize("dimr.xml"), (int)Exception::ERR_INVALID_INPUT);
	}
};


TEST_CLASS(DimrTests)
{

public:
	
	TEST_METHOD(ConstructDimr)
	{
		Dimr * dimr = Dimr::GetInstance();
        //thisDimr = dimr;
		Assert::AreEqual((int)WARNING, (int)dimr->logLevel);
		Assert::AreEqual((int)INFO,(int)dimr->feedbackLevel);
        Assert::AreEqual(false,dimr->ready);
        Assert::AreEqual(NULL, dimr->exePath);
        Assert::AreEqual(NULL, (int)dimr->config);
        Assert::AreEqual(NULL, dimr->mainArgs);
        Assert::AreEqual(NULL, dimr->slaveArg);
        Assert::AreEqual(NULL, (int)dimr->control);
        Assert::AreEqual(0,(int)dimr->componentsList.numComponents);
        Assert::AreEqual(0,(int)dimr->couplersList.numCouplers);
        Assert::AreEqual(false,dimr->use_mpi );
        Assert::AreEqual(0,dimr->my_rank );
        Assert::AreEqual(1,dimr->numranks);
        Assert::AreEqual(NULL, dimr->configfile);
        Assert::AreEqual(false, dimr->done);
        Assert::AreEqual("dimr_redirected.log", dimr->redirectFile);
        //Can not delete, destructor refers to the static global dimr instance, which is only assigned in dimr initialize
        //auto func2 = [dimr] { delete dimr; };
        //Assert::ExpectException<std::exception>(func2);
	}

	
    TEST_METHOD(scanConfigIsEmptyTest)
    {
        /*Dimr* dimr = Dimr::GetInstance();
		char buffer[] = "foobar";
		FILE* memFile = fmemopen()
		
		dimr->config = new XmlTree();
        //thisDimr = dimr;
        
        auto func1 = [dimr] {dimr->scanConfigFile(); };
        Assert::ExpectException<Exception>(func1);*/
        //Can not delete, destructor refers to the static global dimr instance, which is only assigned in dimr initialize
        //auto func2 = [dimr] { delete dimr; };
        //Assert::ExpectException<std::exception>(func2);
    }
    
    TEST_METHOD(scanConfigIsNotEmptyButFileVersionIsNullTest)
    {
        Dimr* dimr = Dimr::GetInstance();
        //dimr->config = new XmlTree();
        auto func1= [dimr] {dimr->scanConfigFile(); };
        Assert::ExpectException<Exception>(func1);
        //Can not delete, destructor refers to the static global dimr instance, which is only assigned in dimr initialize
        //auto func2 = [dimr] { delete dimr; };
        //Assert::ExpectException<std::exception>(func2);
    }
    
    TEST_METHOD(WhenInvalidLibIsUsedIn_ConnectLibsThrowsAnException)
    {
        // Set up
        Dimr* dimr = Dimr::GetInstance();
        dimr_component component;
        component.onThisRank = true;
        component.library = "invalidLib";
        dimr_components componentsList;
        componentsList.components = &component;
        componentsList.numComponents = 1;
        dimr->componentsList = componentsList;

        // Act
	    auto func1 = [&] {dimr->connectLibs(); };
        Assert::ExpectException<Exception>(func1);
        //Can not delete, destructor refers to the static global dimr instance, which is only assigned in dimr initialize
        //auto func2 = [dimr] { delete dimr; };
        //Assert::ExpectException<std::exception>(func2);
    }

    TEST_METHOD(WhenInvalidLibPathIsUsedIn_ConnectLibsThrowsAnException)
    {
        // Set up
	    Dimr* dimr = Dimr::GetInstance();
        dimr_component component;
        component.onThisRank = true;
        component.library = "\dimr_testcomponent.dll";

        // set the componentsList
        dimr_components componentsList;
        componentsList.components = &component;
        componentsList.numComponents = 1;
        dimr->componentsList = componentsList;

        // Act
        auto func1 = [&] {dimr->connectLibs(); };
        Assert::ExpectException<Exception>(func1);
        //Can not delete, destructor refers to the static global dimr instance, which is only assigned in dimr initialize
        //auto func2 = [dimr] { delete dimr; };
        //Assert::ExpectException<std::exception>(func2);
    }

    //cannot go further, need a valid dll to test the rest of the connectLibs

    TEST_METHOD(WhenRunParallelInitIsUsedWithValidMasterComponent_EverythingIsFine)
    {
        // Set up
        Dimr* dimr = Dimr::GetInstance();
        Clock clock;
        dimr->clock = &clock;
        dimr_control_block cb;
        cb.numSubBlocks = 1;

        dimr_control_block controlBlock;
        dimr_unit dimrUnit;
        dimr_component masterComponent;
        masterComponent.name = "masterComponent";
        char cCurrentPath[FILENAME_MAX];
        _getcwd(cCurrentPath, sizeof(cCurrentPath));
        masterComponent.workingDir = cCurrentPath;
        masterComponent.inputFile = "";
        masterComponent.dllInitialize = [](const char * c) { return 0; };
        masterComponent.dllGetStartTime = [](double * time) { };
        masterComponent.dllGetEndTime = [](double * time) {};
        masterComponent.dllGetTimeStep = [](double * time) {};
        masterComponent.dllGetCurrentTime = [](double * time) {};

        dimrUnit.component = &masterComponent;
        controlBlock.unit = dimrUnit;

        controlBlock.type = CT_START;
        cb.subBlocks = &controlBlock;
        cb.masterSubBlockId = -1;

        // Act
        dimr->runParallelInit(&cb);
        //Can not delete, destructor refers to the static global dimr instance, which is only assigned in dimr initialize
        //auto func2 = [dimr] { delete dimr; };
        //Assert::ExpectException<std::exception>(func2);
    }

    TEST_METHOD(WhenRunParallelFinishIsUsedWithValidMasterComponent_EverythingIsFine)
    {
        Dimr* dimr = Dimr::GetInstance();
        Clock clock;
        dimr->clock = &clock;

        // Set up
        dimr_control_block cb;
        cb.numSubBlocks = 1;

        dimr_control_block controlBlock;
        dimr_unit dimrUnit;
        dimr_component masterComponent;
        masterComponent.name = "masterComponent";
        char cCurrentPath[FILENAME_MAX];
        _getcwd(cCurrentPath, sizeof(cCurrentPath));
        masterComponent.workingDir = cCurrentPath;
        masterComponent.inputFile = "";
        masterComponent.dllFinalize = []() {};

        dimrUnit.component = &masterComponent;
        controlBlock.unit = dimrUnit;

        controlBlock.type = CT_START;
        cb.subBlocks = &controlBlock;
        cb.masterSubBlockId = -1;

        // Act
        dimr->runParallelFinish(&cb);

        //Can not delete, destructor refers to the static global dimr instance, which is only assigned in dimr initialize
        //auto func2 = [dimr] { delete dimr; };
        //Assert::ExpectException<std::exception>(func2);
    }

    TEST_METHOD(WhenTimersInitIsCalled_TimersAreSetToZero)
    {
        Dimr* dimr = Dimr::GetInstance();
        Clock clock;
        dimr->clock = &clock;
        
	    // Set up
        dimr_components componentList;
        dimr_component childComponent;
        componentList.components = &childComponent;
	    dimr->componentsList = componentList;
        dimr->componentsList.numComponents = 1;

        //Can not call timersInit() because thisDimr is a global static instance in dimr.cpp
	    // dimr->timersInit();
    }

    TEST_METHOD(WhenTimerStartIsCalled_timerStartIsSet)
    {
        Dimr* dimr = Dimr::GetInstance();
        Clock clock;
        dimr->clock = &clock;

        // Set up
        dimr_component component;
        dimr->timerStart(&component);
        Assert::AreNotEqual(Clock::Timestamp(), component.timerStart);
    }



    TEST_METHOD(WhenTimerEndIsCalled_timerEndIsSet)
    {
        Dimr* dimr = Dimr::GetInstance();
        Clock clock;
        dimr->clock = &clock;

        // Set up
        dimr_component component;
        dimr->timerEnd(&component);
        Assert::AreNotEqual(Clock::Timestamp(0), component.timerSum);
	    Assert::AreEqual(Clock::Timestamp(0), component.timerStart);
    }
};

/*
void           scanConfigFile(void);
void           connectLibs(void);
void           printComponentVersionStrings(Level);
void           freeLibs(void);
void           processWaitFile(void);
void           runControlBlock(dimr_control_block *, double, int);
void           runParallelInit(dimr_control_block *);
void           runParallelFinish(dimr_control_block *);
void           timersInit(void);
void           timerStart(dimr_component *);
void           timerEnd(dimr_component *);
void           timersFinish(void);
void           receive(const char *, int, BMI_SETVAR, BMI_GETVAR, double *, int *, int, int, const void *);
void           getAddress(const char * name, int compType, BMI_GETVAR dllGetVar, double ** sourceVarPtr, int * processes, int nProc, double * transfer);
double *       send(const char * name, int compType, double* sourceVarPtr, int* processes, int nProc, double* transfer);
*/

/*TEST_CLASS(DimrExeTests)
{

public:

	TEST_METHOD(InitDll)
	{
		Dimr * dimr = &Dimr::GetInstance();
		//thisDimr = dimr;
		Assert::AreEqual((int)WARNING, (int)dimr->logLevel);
		Assert::AreEqual((int)INFO, (int)dimr->feedbackLevel);
		Assert::AreEqual(false, dimr->ready);
		Assert::AreEqual(NULL, dimr->exePath);
		Assert::AreEqual(NULL, (int)dimr->config);
		Assert::AreEqual(NULL, dimr->mainArgs);
		Assert::AreEqual(NULL, dimr->slaveArg);
		Assert::AreEqual(NULL, (int)dimr->control);
		Assert::AreEqual(0, (int)dimr->componentsList.numComponents);
		Assert::AreEqual(0, (int)dimr->couplersList.numCouplers);
		Assert::AreEqual(false, dimr->use_mpi);
		Assert::AreEqual(0, dimr->my_rank);
		Assert::AreEqual(1, dimr->numranks);
		Assert::AreEqual(NULL, dimr->configfile);
		Assert::AreEqual(false, dimr->done);
		Assert::AreEqual("dimr_redirected.log", dimr->redirectFile);
		//Can not delete, destructor refers to the static global dimr instance, which is only assigned in dimr initialize
		//auto func2 = [dimr] { delete dimr; };
		//Assert::ExpectException<std::exception>(func2);
	}
}*/