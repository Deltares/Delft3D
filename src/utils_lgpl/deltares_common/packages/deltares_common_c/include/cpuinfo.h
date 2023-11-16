#pragma once

#ifdef WIN32

#include <WTYPES.h>

class CpuInfo
{
	public:
		unsigned __int64 GetClockSpeed();
		int GetCores();
};

#else

#include <sys/time.h>
#include <sched.h>
#include <time.h>
#include <unistd.h>

class CpuInfo
{
	public:
		unsigned long long GetClockSpeed();
		int GetCores();
};

#endif
