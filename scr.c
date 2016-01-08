#include "defines.h"
#include "SDL/SDL.h"
#include "scr.h"
#include <libintl.h>
#define _(String) gettext (String)

SDL_Surface * screen;
flag_t cflag = 0, dblflag = 0;
int cur_shift = 0;
int cur_width = 0;	/* 0 = narrow, !0 = wide */

/* Scan lines */
unsigned char dirty[512];
SDL_Surface * lines[512];

/*
 * The colors are ordered in the HW order, not in the "Basic" order.
 * To convert, switch colors 1 and 3.
 */
SDL_Color palettes[16][5] = {
{ Black, Blue, Green, Red, White },   /* BK0010 Colors */
{ Black, Yellow, Magenta, Red, White },
{ Black, Cyan, Blue, Magenta, White },
{ Black, Green, Cyan, Yellow, White },
{ Black, Magenta, Cyan, White, White },
{ Black, White, White, White, White },
{ Black, DarkRed, Brown, Red, White },
{ Black, Green, Cyan, Yellow, White },
{ Black, Violet, LightBlue, Magenta, White },
{ Black, Yellow, LightBlue, DarkRed, White },  /* Almost the same as the next one */
{ Black, Yellow, Violet, Red, White },
{ Black, Cyan, Yellow, Red, White },          /* CSI-DOS colors */
{ Black, Red, Green, Cyan, White },
{ Black, Cyan, Yellow, White, White },
{ Black, Yellow, Green, White, White },
{ Black, Cyan, Green, White, White },
};

unsigned scr_dirty = 0;

unsigned char req_page[512], req_palette[512];
unsigned char active_palette, active_page;
unsigned char half_frame = 0;

int upper_porch = 0;	/* Default */
int lower_porch = 3;	/* Default */

#define LINES_TOTAL     (256+upper_porch+lower_porch)

/*
 * Set the pixel at (x, y) to the given value
 * NOTE: The surface must be locked before calling this!
 */
static inline void putpixel1(SDL_Surface * s, int x, Uint32 pixel)
{
    Uint8 *p = (Uint8 *)s->pixels + x;
    *p = pixel;
}

/* Puts 2 pixels at once for double width mode */
static inline void putpixel2(SDL_Surface * s, int x, Uint32 pixel)
{
    Uint16 *p = (Uint16 *)s->pixels + x;
    *p = pixel<<8|pixel;
}

static void lock() {
	/* Lock the screen for direct access to the pixels */
	if ( SDL_MUSTLOCK(screen) ) {
		if ( SDL_LockSurface(screen) < 0 ) {
			fprintf(stderr, _("Can't lock screen: %s\n"), SDL_GetError());
			return;
		}
	}
}

static void unlock() {
    if ( SDL_MUSTLOCK(screen) ) {
	SDL_UnlockSurface(screen);
    }
}

/*
 * Flushes a word into a statically defined scan line;
 * scrolling and line doubling are done during blitting. bufno is 0 or 1,
 * address is relative to the video buffer.
 */
int scr_write(int bufno, c_addr addr, d_word wrd)
{
	int offset, dest_x, dest_y;
	SDL_Surface * s;
	if (!cflag) {
		int i;
		offset = addr * 8;
		dest_y = offset / 512;
		dest_x = offset & 511;
		i = 256*bufno+dest_y;
		s = lines[i];
		if (!dirty[i]) {
			dirty[i] = 1;
			scr_dirty++;
		}
		for (i = 16; i; i--, dest_x++, wrd >>= 1) {
			putpixel1(s, dest_x, (wrd & 1) << 2);
		}
	} else {
		int i;
		offset = addr * 4;
		dest_y = offset / 256;
		dest_x = offset & 255;
		i = 256*bufno+dest_y;
		s = lines[i];
		if (!dirty[i]) {
			dirty[i] = 1;
			scr_dirty++;
		}
		for(i = 8; i; i--, dest_x++, wrd >>= 2) {
			(dblflag?putpixel2:putpixel1)(s, dest_x, wrd & 3);
		}
	}
	return OK;
}

/* In the bk_icon array, each word is in RGBA format;
 * A = 0 is transparent, A = 255 is opaque
 */

extern Uint32 bk_icon_width, bk_icon_height, bk_icon[];

Uint8 *
compute_icon_mask() {
        int i;
	static Uint8 * mask = 0;
	if (mask) return mask;

	mask = calloc(1, bk_icon_width*bk_icon_height/8);
	if (!mask) return NULL;

        for ( i=0; i<bk_icon_width*bk_icon_height; ) {
                /* More than half opaque is opaque */
                if ( (bk_icon[i] & 0xFF) >= 128)
                        mask[i/8] |= 0x01;
                ++i;
                if ( (i%8) != 0 )
                        mask[i/8] <<= 1;
        }
        return mask;
}

unsigned scan_line_duration;

/* BK-0010 screen refresh - no palettes */
extern void scr_refresh_bk0010(unsigned shift, unsigned full);

/* BK-0011 screen refresh - single size, lossy interlacing */
extern void scr_refresh_bk0011_1(unsigned shift, unsigned full);

/* BK-0011 screen refresh - double size, exact interlacing */
extern void scr_refresh_bk0011_2(unsigned shift, unsigned full);

void (*scr_refresh)(unsigned, unsigned);

scr_init() {

    extern unsigned bk_icon[];
    extern unsigned char * compute_icon_mask();
    static char init_done = 0;
    int i;
    Uint32 bpp, rmask, gmask, bmask;
    if (init_done) return;
    init_done = 1;

    SDL_WM_SetIcon(SDL_CreateRGBSurfaceFrom(bk_icon, bk_icon_width,
			bk_icon_height, 32, bk_icon_width*4,
			0xff000000, 0xff0000, 0xff00, 0xff),
	compute_icon_mask());
	
    screen = SDL_SetVideoMode(cflag && !dblflag ? 256 : 512,
	 dblflag ? 512 : 256, 0,
	 SDL_HWSURFACE|SDL_DOUBLEBUF|SDL_ANYFORMAT|SDL_HWPALETTE);
    if (screen == NULL) {
        fprintf(stderr, _("Couldn't set up video: %s\n"),
                        SDL_GetError());
        exit(1);
    }

    /* Translation disabled because of an SDL bug */
    if (bkmodel == 0) {
    	SDL_WM_SetCaption("BK-0010", "BK-0010"); }
    else {
    	SDL_WM_SetCaption("BK-0011M", "BK-0011M"); 
    }

    active_palette = bkmodel ? 15 : 0;

    if (screen->format->BitsPerPixel == 8) {
	SDL_SetColors(screen, palettes[active_palette], 0, 5);
    }

    /* Create palettized surfaces for scan lines for the highest possible
     * resolution (so far width 512). 
     */
    for (i = 0; i < 512; i++) {
	lines[i] = SDL_CreateRGBSurface(SDL_SWSURFACE, 512, 1,
		8, 0, 0, 0, 0);
	if (!lines[i]) {
		fprintf(stderr, "Couldn't set up video: %s\n",
			SDL_GetError());
		exit(1);
	}
	SDL_SetPalette(lines[i], SDL_LOGPAL,
		palettes[active_palette], 0, 5);
	dirty[i] = 0;
    }
    SDL_ShowCursor(SDL_DISABLE);
    scan_line_duration = TICK_RATE/(LINES_TOTAL*50);
    scr_refresh = bkmodel ?
	dblflag ? scr_refresh_bk0011_2 :
		scr_refresh_bk0011_1 :
	scr_refresh_bk0010;
}

/* Cyclically switches between color/b-w and single/double size modes:
 * single color -> double color -> double b/w -> single b/w
 */
scr_switch() {
    int i;
    if (cflag)
	if (dblflag)
		cflag = 0;
	else
		dblflag = 1;
    else if (dblflag)
		dblflag = 0;
	else
		cflag = 1; 

    screen = SDL_SetVideoMode(cflag && !dblflag ? 256 : 512,
         dblflag ? 512 : 256, 0,
	 SDL_SWSURFACE|SDL_ANYFORMAT|SDL_HWPALETTE|SDL_DOUBLEBUF);

    /* Re-flush both video pages */
    for (i = 0; i < 040000; i+=2) {
	scr_write(0, i, ram[1][i >> 1]);
	scr_write(1, i, ram[7][i >> 1]);
    }
    if (bkmodel) scr_refresh = dblflag ? scr_refresh_bk0011_2 : scr_refresh_bk0011_1;
}

/* Returns the scan line number that is supposedly being displayed "now".
 * Each half frame is TICK_RATE/50 long and consists of 128 lines.
 */
unsigned current_scan_line() {
	extern double half_frame_delay;
	unsigned nframes = ticks/half_frame_delay;
	unsigned frame_ticks = ticks - half_frame_delay * nframes;
	unsigned line = frame_ticks / scan_line_duration;
	if (line < upper_porch) return 0;
	line -= upper_porch;
	if (line < 256) return line; else return 256;
}

unsigned char param_change_line;
unsigned char change_req;

void scr_param_change(int pal, int buf) {
	int cur = current_scan_line();
	uint i;
	for (i = param_change_line; i < cur; i++) {
		req_palette[2 * i + half_frame] = active_palette;
		req_page[2 * i + half_frame] = active_page;
	}
	active_palette = pal;
	active_page = buf;
	param_change_line = cur;
	change_req = 3;	/* For 2 half-frames */
	fprintf(stderr, "l=%d\n", cur); 
}

/*
 * Just before a half frame ends, fill up the buffer and palette
 * requests to the end with the current values.
 */
void scr_sync() {
	uint i;
	for (i = param_change_line; i < 256; i++) {
		req_palette[2 * i + half_frame] = active_palette;
		req_page[2 * i + half_frame] = active_page;
	}
	half_frame ^= 1;
	param_change_line = 0;
}

/*
 * Screen refresh for BK-0010 does not need to know about buffers or
 * palettes.
 */
void
scr_refresh_bk0010(unsigned shift, unsigned full) {
	int blit_all = shift != cur_shift || cur_width != full;
	int i;
	int doublebuf =
		screen->flags & (SDL_DOUBLEBUF|SDL_HWSURFACE) ==
		(SDL_DOUBLEBUF|SDL_HWSURFACE);

	/* If more than a few lines changed, no point
	 * doing separate UpdateRect's for each line.
	 */
	int update_all = blit_all || doublebuf || scr_dirty >= 4;
	int width = cflag && !dblflag ? 256 : 512;
	int nlines = full ? 256 : 64;
	static SDL_Rect srcrect = {0, 0, 0, 1};
	static SDL_Rect dstrect = {0, 0, 0, 0};
	srcrect.w = width;
	for (i = 0; i < nlines; i++) {
		int line = (i + shift) & 0xFF;
		SDL_Surface * l = lines[line];
		dstrect.y = i << dblflag;
		if (dirty[line] | blit_all) {
			SDL_BlitSurface(l, &srcrect, screen, &dstrect);
			if (!update_all) {
				SDL_UpdateRect(screen, 0, dstrect.y, width, 1);
			}
			if (dblflag) {
				dstrect.y++;
				SDL_BlitSurface(l, &srcrect, screen, &dstrect);
				if (!update_all) {
					SDL_UpdateRect(screen, 0, dstrect.y, width, 1);
				}
			}
		}
	}
	// Only the first 256 lines are used
	memset(dirty, 0, 256);
	if (!full && cur_width) {
		/* Black out the low 3/4 of the screen */
		dstrect.x = 0; dstrect.y = 64<<dblflag;
		dstrect.w = width;
		dstrect.h = 192<<dblflag;
		SDL_FillRect(screen, &dstrect, 0);
	}
	cur_width = full;
	cur_shift = shift;
	if (update_all) {
		SDL_Flip(screen);
	}
	scr_dirty = 0;
}

/*
 * For single-sized BK-0011 screen we do crude interlacing:
 * even lines are taken from the first half-frame,
 * odd lines - from the second.
 */
void
scr_refresh_bk0011_1(unsigned shift, unsigned full) {
	int blit_all = change_req || shift != cur_shift || cur_width != full;
	int i;
	int doublebuf =
		screen->flags & (SDL_DOUBLEBUF|SDL_HWSURFACE) ==
		(SDL_DOUBLEBUF|SDL_HWSURFACE);

	/* If more than a few lines changed, no point
	 * doing separate UpdateRect's for each line.
	 */
	int update_all = blit_all || doublebuf || scr_dirty >= 4;
	int width = cflag ? 256 : 512;
	int do_palette = change_req || shift != cur_shift;
	int nlines = full ? 256 : 64;
	static SDL_Rect srcrect = {0, 0, 0, 1};
	static SDL_Rect dstrect = {0, 0, 0, 0};
	srcrect.w = width;
	for (i = 0; i < nlines; i++) {
		int line = (i + shift) & 0xFF;
		unsigned physline = 256*req_page[2*i+(i&1)] + line;
		SDL_Surface * l = lines[physline];
		dstrect.y = i;
		if (dirty[physline] | blit_all) {
			if (do_palette) {
				SDL_SetPalette(l, SDL_LOGPAL,
					palettes[req_palette[2*i+(i&1)]], 0, 5);
			}
			SDL_BlitSurface(l, &srcrect, screen, &dstrect);
			if (!update_all) {
				SDL_UpdateRect(screen, 0, dstrect.y, width, 1);
			}
		}
	}
	memset(dirty, 0, 512);
	if (!full && cur_width) {
		/* Black out the low 3/4 of the screen */
		dstrect.x = 0; dstrect.y = 64;
		dstrect.w = width;
		dstrect.h = 192;
		SDL_FillRect(screen, &dstrect, 0);
	}
	cur_width = full;
	cur_shift = shift;
	if (update_all) {
		SDL_Flip(screen);
	}
	scr_dirty = 0;
	change_req >>= 1;
}

void
scr_refresh_bk0011_2(unsigned shift, unsigned full) {
	int blit_all = change_req || shift != cur_shift || cur_width != full;
	int i;
	int doublebuf =
		screen->flags & (SDL_DOUBLEBUF|SDL_HWSURFACE) ==
		(SDL_DOUBLEBUF|SDL_HWSURFACE);

	/* If more than a few lines changed, no point
	 * doing separate UpdateRect's for each line.
	 */
	int update_all = blit_all || doublebuf || scr_dirty >= 4;
	int do_palette = change_req || shift != cur_shift;
	int nlines = full ? 512 : 128;
	static SDL_Rect srcrect = {0, 0, 512, 1};
	static SDL_Rect dstrect = {0, 0, 0, 0};
	for (i = 0; i < nlines; i++) {
		int line = (i/2 + shift) & 0xFF;
		unsigned physline = 256*req_page[i]+line;
		SDL_Surface * l = lines[physline];
		dstrect.y = i;
		if (dirty[physline] | blit_all) {
			if (do_palette) {
				SDL_SetPalette(l, SDL_LOGPAL,
					palettes[req_palette[i]], 0, 5);
			}
			SDL_BlitSurface(l, &srcrect, screen, &dstrect);
			if (!update_all) {
				SDL_UpdateRect(screen, 0, dstrect.y, 512, 1);
			}
		}
	}
	memset(dirty, 0, 512);
	if (!full && cur_width) {
		/* Black out the low 3/4 of the screen */
		dstrect.x = 0; dstrect.y = 128;
		dstrect.w = 512;
		dstrect.h = 384;
		SDL_FillRect(screen, &dstrect, 0);
	}
	cur_width = full;
	cur_shift = shift;
	if (update_all) {
		SDL_Flip(screen);
	}
	scr_dirty = 0;
	change_req >>= 1;
}

void scr_flush() {
	if (scr_dirty || change_req)
		scr_refresh((tty_scroll - 0330) & 0377, tty_scroll & 01000);
}
