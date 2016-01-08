/*
 * Configuration file management routines.
 * Copyright 2003 Leonid Broukhis
 */
#include <stdio.h>
#include <string.h>
#include "conf.h"

typedef struct {
	char * name;
	int * val;
} iconf_t;

typedef struct {
	char * name;
	flag_t * val;
} bconf_t;

typedef struct {
	char * name;
	char ** val;
} sconf_t;

/*
 * Attribute names are case-insensitive.
 */
sconf_t sconf[] = {
	{ "floppyA", &floppyA },
	{ "floppyB", &floppyB },
	{ "floppyC", &floppyC },
	{ "floppyD", &floppyD },
	{ "monit10.rom", &monitor10rom }
};

iconf_t iconf[] = {
	{ "UpperPorch", &upper_porch },
	{ "LowerPorch", &lower_porch }
};

bconf_t bconf[] = {
	{ "FakeTape", &fake_tape },
	{ "Telegraph", &telegraph_enabled }
};

#define NUM_SATTR (sizeof(sconf)/sizeof(sconf_t))
#define NUM_IATTR (sizeof(iconf)/sizeof(iconf_t))
#define NUM_BATTR (sizeof(bconf)/sizeof(bconf_t))

init_config() {
	FILE * bkrc = popen("cat $HOME/.bkrc", "r");
	char buf[1024];
	char name[1024];
	char sval[1024];
	int ival;
	if (!bkrc) return;
	while (fgets(buf, 1024, bkrc)) {
		int n, i;
		/* # in the first non-blank position marks a comment */
		n = strspn(buf, " \t\n");
		if (buf[n] == '\0' || buf[n] == '#')
			continue;
		
		if (1 > sscanf(buf, " %[^= ] =%n", name, &n)) {
			fprintf(stderr, "Bad configuration line: %s\n", buf);
			continue;
		}
		
		for (i = 0; i < NUM_SATTR; i++) {
			if (!strcasecmp(name, sconf[i].name)) {
				n = sscanf(buf+n, " %s", sval);
				if (n < 1) {
					fprintf(stderr, "String value for %s is required\n", sconf[i].name);
					break;
				}
				*sconf[i].val = strdup(sval);
				break;
			}
		}
		if (i != NUM_SATTR) continue;

		for (i = 0; i < NUM_IATTR; i++) {
			if (!strcasecmp(name, iconf[i].name)) {
				n = sscanf(buf+n, " %d", &ival);
				if (n < 1) {
					fprintf(stderr, "Integer value for %s is required\n", iconf[i].name);
					break;
				}
				*iconf[i].val = ival;
				break;
			}
		}
		if (i != NUM_IATTR) continue;
		for (i = 0; i < NUM_BATTR; i++) {
			if (!strcasecmp(name, bconf[i].name)) {
				n = sscanf(buf+n, " %s", sval);
				if (n < 1) {
					fprintf(stderr, "Boolean value for %s is required\n", bconf[i].name);
					break;
				}
				switch (sval[0]) {
				case '1':
				case 'T':
				case 't':
				case 'Y':
				case 'y':
					*bconf[i].val = 1;
					break;
				case '0':
				case 'F':
				case 'f':
				case 'N':
				case 'n':
					*bconf[i].val = 0;
					break;
				default:
					fprintf(stderr, "Boolean value for %s is required (got %c)\n",
						 bconf[i].name, ival);
				}
				break;
			}
		}
		if (i != NUM_BATTR) continue;

		fprintf(stderr, "Unknown attribute %s\n", name);
	}
	pclose(bkrc);
}