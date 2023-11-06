#pragma once

#ifdef WIN32

#include <windows.h>

class MemInfo
{
	public:
		unsigned __int64 GetTotalMemSize();
};

#else

#include <unistd.h>

class MemInfo
{
	public:
		unsigned long long GetTotalMemSize();
};

#endif
