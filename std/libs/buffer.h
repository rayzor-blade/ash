#include "../hl.h"

typedef struct _stringitem {
	uchar *str;
	int size;
	int len;
	struct _stringitem *next;
} * stringitem;

struct hl_buffer {
	int totlen;
	int blen;
	stringitem data;
};

typedef struct vlist {
	vdynamic *v;
	struct vlist *next;
} vlist;

typedef struct tlist {
	hl_type *t;
	struct tlist *next;
} tlist;