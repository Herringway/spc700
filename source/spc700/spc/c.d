module spc700.spc.c;

import spc700.memory;

import spc700.spc.filter;
import spc700.spc.cpu;
import spc700.dsp.dsp;

import core.stdc.stdlib;
import core.lifetime;

extern(C) nothrow:

/* Creates new SPC emulator. NULL if out of memory. */

enum spc_file_size = 0x10200; /* spc_out must have this many bytes allocated */
SNES_SPC* spc_new() {
	// be sure constants match
	assert(spc_sample_rate == cast(int) SNES_SPC.sample_rate);
	assert(spc_rom_size == cast(int) SNES_SPC.rom_size);
	assert(spc_clock_rate == cast(int) SNES_SPC.clock_rate);
	assert(spc_clocks_per_sample == cast(int) SNES_SPC.clocks_per_sample);
	assert(spc_port_count == cast(int) SNES_SPC.port_count);
	assert(spc_voice_count == cast(int) SNES_SPC.voice_count);
	assert(spc_tempo_unit == cast(int) SNES_SPC.tempo_unit);
	assert(spc_file_size == cast(int) SNES_SPC.spc_file_size);
	version (SPC_NO_COPY_STATE_FUNCS) {
	} else {
		assert(spc_state_size == cast(int) SNES_SPC.state_size);
	}

	SNES_SPC* s = manualAlloc!SNES_SPC();
	if (s && s.initialize()) {
		free(s);
		s = null;
	}
	return s;
}

/* Frees SPC emulator */
void spc_delete(SNES_SPC* s) {
	manualFree(s);
}

/* Sample pairs generated per second */
enum spc_sample_rate = 32000;

/**** Emulator use ****/

/* Sets IPL ROM data. Library does not include ROM data. Most SPC music files
don't need ROM, but a full emulator must provide this. */
enum spc_rom_size = 0x40;
void spc_init_rom(SNES_SPC* s, const(ubyte)[64] r) {
	s.init_rom(r);
}

/* Sets destination for output samples */
alias spc_sample_t = short;
void spc_set_output(SNES_SPC* s, spc_sample_t* p, int n) {
	s.set_output(p[0 .. n]);
}

/* Number of samples written to output since last set */
int spc_sample_count(const(SNES_SPC)* s) {
	return s.sample_count();
}

/* Resets SPC to power-on state. This resets your output buffer, so you must
call spc_set_output() after this. */
void spc_reset(SNES_SPC* s) {
	s.reset();
}

/* Emulates pressing reset switch on SNES. This resets your output buffer, so
you must call spc_set_output() after this. */
void spc_soft_reset(SNES_SPC* s) {
	s.soft_reset();
}

/* 1024000 SPC clocks per second, sample pair every 32 clocks */
alias spc_time_t = int;
enum spc_clock_rate = 1024000;
enum spc_clocks_per_sample = 32;

/* Reads/writes port at specified time */
enum spc_port_count = 4;
int spc_read_port(SNES_SPC* s, spc_time_t t, int p) {
	return s.read_port(t, p);
}

void spc_write_port(SNES_SPC* s, spc_time_t t, int p, int d) {
	s.write_port(t, p, d);
}

/* Runs SPC to end_time and starts a new time frame at 0 */
void spc_end_frame(SNES_SPC* s, spc_time_t t) {
	s.end_frame(t);
}

/**** Sound control ****/

/*Mutes voices corresponding to non-zero bits in mask. Reduces emulation accuracy. */
enum spc_voice_count = 8;
void spc_mute_voices(SNES_SPC* s, int mask) {
	s.mute_voices(mask);
}

/* If true, prevents channels and global volumes from being phase-negated.
Only supported by fast DSP; has no effect on accurate DSP. */
void spc_disable_surround(SNES_SPC* s, bool disable) {
	s.disable_surround(disable);
}

/* Sets tempo, where spc_tempo_unit = normal, spc_tempo_unit / 2 = half speed, etc. */
enum spc_tempo_unit = 0x100;
void spc_set_tempo(SNES_SPC* s, int tempo) {
	s.set_tempo(tempo);
}

/**** SPC music playback *****/

/* Loads SPC data into emulator. Returns NULL on success, otherwise error string. */
const(char*) spc_load_spc(SNES_SPC* s, const(void)* p, long n) {
	return s.load_spc(p[0 .. cast(size_t)n]);
}

/* Clears echo region. Useful after loading an SPC as many have garbage in echo. */
void spc_clear_echo(SNES_SPC* s) {
	s.clear_echo();
}

/* Plays for count samples and write samples to out. Discards samples if out
is NULL. Count must be a multiple of 2 since output is stereo. */
const(char*) spc_play(SNES_SPC* s, int count, short* out_) {
	return s.play(out_[0 .. count]).ptr;
}

/* Skips count samples. Several times faster than spc_play(). */
const(char*) spc_skip(SNES_SPC* s, int count) {
	return s.skip(count).ptr;
}

/**** State save/load (only available with accurate DSP) ****/

version(SPC_NO_COPY_STATE_FUNCS) {} else {
	/* Saves/loads exact emulator state */
	enum spc_state_size = 67 * 1024L; /* maximum space needed when saving */
	alias spc_copy_func_t = void function(ubyte** io, void* state, size_t) @safe nothrow;
	void spc_copy_state(SNES_SPC* s, ubyte** p, spc_copy_func_t f) {
		void delegate(ubyte**, void[]) @safe nothrow dg = (ubyte** io, void[] state) { f(io, &state[0], state.length); };
		s.copy_state(p, dg);
	}
	/* Saves emulator state as SPC file data. Writes spc_file_size bytes to spc_out.
	Does not set up SPC header; use spc_init_header() for that. */
	void spc_save_spc(SNES_SPC* s, void* spc_out) {
		s.save_spc((cast(ubyte*)spc_out)[0 .. spc_file_size]);
	}
	/* Returns non-zero if new key-on events occurred since last check. Useful for
	trimming silence while saving an SPC. */
	int spc_check_kon(SNES_SPC* s) {
		return s.check_kon();
	}
	/* Writes minimal SPC file header to spc_out */
	void spc_init_header(void* spc_out) {
		SNES_SPC.init_header((cast(ubyte*)spc_out)[0 .. spc_file_size]);
	}
}




/**** SPC_Filter ****/

/* Creates new filter. NULL if out of memory. */
SPC_Filter* spc_filter_new() {
	auto filter = cast(SPC_Filter*) malloc(SPC_Filter.sizeof);
	emplace(filter);
	return filter;
}

/* Frees filter */
void spc_filter_delete(SPC_Filter* f) {
	free(f);
}

/* Filters count samples of stereo sound in place. Count must be a multiple of 2. */
void spc_filter_run(SPC_Filter* f, spc_sample_t* io, int count) {
	f.run(io[0 .. count]);
}

/* Clears filter to silence */
void spc_filter_clear(SPC_Filter* f) {
	f.clear();
}

/* Sets gain (volume), where spc_filter_gain_unit is normal. Gains greater than
spc_filter_gain_unit are fine, since output is clamped to 16-bit sample range. */
enum spc_filter_gain_unit = 0x100;
void spc_filter_set_gain(SPC_Filter* f, int gain) {
	f.set_gain(gain);
}

/* Sets amount of bass (logarithmic scale) */
enum spc_filter_bass_none = 0;
enum spc_filter_bass_norm = 8; /* normal amount */
enum spc_filter_bass_max = 31;
void spc_filter_set_bass(SPC_Filter* f, int bass) {
	f.set_bass(bass);
}
