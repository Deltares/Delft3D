/*
 * main.cpp
 *
 *  Created on: Aug 21, 2012
 *      Author: witlox
 */

#include <stdio.h>
#include "cpuinfo.h"
#include "meminfo.h"

int main(int argc, char *argv[])
{
	CpuInfo *cpuinfo = new CpuInfo();
	MemInfo *meminfo = new MemInfo();
	FILE * fp;

	fp = fopen("system_information.txt", "w");
	
	fprintf(fp, "Clockspeed\t: %llu\n", cpuinfo->GetClockSpeed());
	fprintf(fp, "Cores\t\t: %d\n", cpuinfo->GetCores());
	fprintf(fp, "Memory\t\t: %llu\n", meminfo->GetTotalMemSize());
	fclose(fp);
}



