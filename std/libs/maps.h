#include "../hl.h"

typedef struct {
	unsigned int hash;
} hl_hb_entry;

typedef struct {
	uchar *key;
	vdynamic *value;
} hl_hb_value;

typedef struct {
	int pos;
	int count;
} hl_free_bucket;

typedef struct {
	hl_free_bucket *buckets;
	int head;
	int nbuckets;
} hl_free_list;

typedef struct {
	void *cells;
	void *nexts;
	hl_hb_entry *entries;
	hl_hb_value *values;
	hl_free_list lfree;
	int ncells;
	int nentries;
	int maxentries;
} hl_hb_map;

typedef struct {
	void *key;
} hl_mlookup__entry;

typedef struct {
	int value;
} hl_mlookup__value;

