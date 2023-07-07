#pragma once
using namespace std;
#include <list>

struct keyValueLL {
	char * key;
	char * val;
	keyValueLL * nextkv;
};
struct keyValue {
	char * key;
	char * val;
};

typedef list<keyValue>	keyValueList;
