module spc700.dsp.c;

import spc700.memory;
import spc700.dsp.dsp;

import core.stdc.stdlib;

/* Copyright (C) 2007 Shay Green. This module is free software; you
can redistribute it and/or modify it under the terms of the GNU Lesser
General Public License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version. This
module is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
details. You should have received a copy of the GNU Lesser General Public
License along with this module; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA */

extern(C) nothrow:

/* Creates new DSP emulator. NULL if out of memory. */
SPC_DSP* spc_dsp_new() {
	// be sure constants match
	assert(spc_dsp_voice_count == cast(int) SPC_DSP.voice_count);
	assert(spc_dsp_register_count == cast(int) SPC_DSP.register_count);
	version (SPC_NO_COPY_STATE_FUNCS) {
	} else {
		assert(spc_dsp_state_size == cast(int) SPC_DSP.state_size);
	}
	return manualAlloc!SPC_DSP();
}

/* Frees DSP emulator */
void spc_dsp_delete(SPC_DSP* s) {
	manualFree(s);
}

/* Initializes DSP and has it use the 64K RAM provided */
void spc_dsp_init(SPC_DSP* s, void* ram_64k) {
	s.initialize((cast(ubyte*)ram_64k)[0 .. 0x10000]);
}

/* Sets destination for output samples. If out is NULL or out_size is 0,
doesn't generate any. */
alias spc_dsp_sample_t = short;
void spc_dsp_set_output(SPC_DSP* s, spc_dsp_sample_t* p, int n) {
	s.set_output(p[0 .. n]);
}

/* Number of samples written to output since it was last set, always
a multiple of 2. Undefined if more samples were generated than
output buffer could hold. */
int spc_dsp_sample_count(const(SPC_DSP)* s) {
	return s.sample_count();
}

/**** Emulation *****/

/* Resets DSP to power-on state */
void spc_dsp_reset(SPC_DSP* s) {
	s.reset();
}

/* Emulates pressing reset switch on SNES */
void spc_dsp_soft_reset(SPC_DSP* s) {
	s.soft_reset();
}

/* Reads/writes DSP registers. For accuracy, you must first call spc_dsp_run() */
/* to catch the DSP up to present. */
int spc_dsp_read(const(SPC_DSP)* s, int addr) {
	return s.read(addr);
}

void spc_dsp_write(SPC_DSP* s, int addr, int data) {
	s.write(addr, data);
}

/* Runs DSP for specified number of clocks (~1024000 per second). Every 32 clocks */
/* a pair of samples is be generated. */
void spc_dsp_run(SPC_DSP* s, int clock_count) {
	s.run(clock_count);
}

/**** Sound control *****/

/* Mutes voices corresponding to non-zero bits in mask. Reduces emulation accuracy. */
enum spc_dsp_voice_count = 8;
void spc_dsp_mute_voices(SPC_DSP* s, int mask) {
	s.mute_voices(mask);
}

/* If true, prevents channels and global volumes from being phase-negated.
Only supported by fast DSP; has no effect on accurate DSP. */
void spc_dsp_disable_surround(SPC_DSP* s, int disable) {
	s.disable_surround(!!disable);
}

/**** State save/load *****/

/* Resets DSP and uses supplied values to initialize registers */
enum spc_dsp_register_count = 128;
void spc_dsp_load(SPC_DSP* s, ref const(ubyte)[spc_dsp_register_count] regs) {
	s.load(regs);
}

version (SPC_NO_COPY_STATE_FUNCS) {
} else {
	/* Saves/loads exact emulator state (accurate DSP only) */
	enum spc_dsp_state_size = 640; /* maximum space needed when saving */
	alias spc_dsp_copy_func_t = void function(ubyte** io, void* state, size_t) @trusted nothrow;
	void spc_dsp_copy_state(SPC_DSP* s, ubyte** p, spc_dsp_copy_func_t f) {
		void delegate(ubyte**, void[]) @safe nothrow dg = (ubyte** io, void[] state) { f(io, &state[0], state.length); };
		s.copy_state(p, dg);
	}

	/* Returns non-zero if new key-on events occurred since last call (accurate DSP only) */
	int spc_dsp_check_kon(SPC_DSP* s) {
		return s.check_kon();
	}
}
