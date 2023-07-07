#include "cpuinfo.h"

#ifdef WIN32

unsigned __int64 usCountOverhead, CPUClockSpeed;

unsigned __int64 GetUsCount()
{
    static LARGE_INTEGER ticksPerSec;
    static double scalefactor;
    LARGE_INTEGER val;
    if(!scalefactor)
    {
        if(QueryPerformanceFrequency(&ticksPerSec))
        {
            scalefactor=ticksPerSec.QuadPart/1000000000000.0;
        }
        else
        {
            scalefactor=1;
        }
    }
    if(!QueryPerformanceCounter(&val))
    {
        return GetTickCount() * 1000000000;
    }
    return unsigned (__int64 (val.QuadPart/scalefactor));
}

unsigned __int64 CpuInfo::GetClockSpeed()
{
	int n;
	unsigned __int64 start, end, start_tsc, end_tsc;
	if(!usCountOverhead)
	{
		unsigned __int64 foo = 0;
		start = GetUsCount();
		for(n = 0; n < 1000000; n++)
		{
			foo += GetUsCount();
		}
		end = GetUsCount();
		usCountOverhead = (end-start)/n;
	}
	start = GetUsCount();
	start_tsc = __rdtsc();
	for(n = 0; n < 1000; n++)
	{
		Sleep(0);
	}
	end_tsc = __rdtsc();
	end = GetUsCount();
	return unsigned (__int64 ((1000000000000.0 * (end_tsc-start_tsc)) / (end-start-usCountOverhead)));
}

int CpuInfo::GetCores()
{
	SYSTEM_INFO sysinfo;
	GetSystemInfo( &sysinfo );
	return sysinfo.dwNumberOfProcessors;
};

#else

unsigned long long GetUsCount()
{
#ifdef CLOCK_MONOTONIC
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (ts.tv_sec*1000000000000LL)+ts.tv_nsec*1000LL;
#else
    struct timeval tv;
    gettimeofday(&tv, 0);
    return (tv.tv_sec*1000000000000LL)+tv.tv_usec*1000000LL;
#endif
}

unsigned long long usCountOverhead, CPUClockSpeed;

#if defined(__i386__)

static __inline__ unsigned long long rdtsc(void)
{
    unsigned long long int x;
    __asm__ volatile (".byte 0x0f, 0x31" : "=A" (x));
    return x;
}

#elif defined(__x86_64__)

static __inline__ unsigned long long rdtsc(void)
{
    unsigned hi, lo;
    __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
    return ( (unsigned long long)lo)|( ((unsigned long long)hi)<<32 );
}

#endif

unsigned long long CpuInfo::GetClockSpeed()
{
	int n;
	unsigned long long start, end, start_tsc, end_tsc;
	if(!usCountOverhead)
	{
		unsigned long long foo=0;
		start = GetUsCount();
		for(n = 0; n < 1000000; n++)
		{
			foo += GetUsCount();
		}
		end = GetUsCount();
		usCountOverhead = (end-start)/n;
	}
	start = GetUsCount();
	start_tsc = rdtsc();
	for(n = 0; n < 1000; n++)
	{
		sched_yield();
	}
	end_tsc = rdtsc();
	end = GetUsCount();
	return (1000000000000.0*(end_tsc-start_tsc))/(end-start-usCountOverhead);
}

int CpuInfo::GetCores()
{
	return sysconf(_SC_NPROCESSORS_ONLN);
};

#endif
