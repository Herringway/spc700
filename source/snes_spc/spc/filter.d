// snes_spc 0.9.0. http://www.slack.net/~ant/

module snes_spc.spc.filter;

import core.stdc.string;

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

struct SPC_Filter {
nothrow:
public:

	// Filters count samples of stereo sound in place. Count must be a multiple of 2.
	alias sample_t = short;
	void run(sample_t[] io) @safe {
		assert((io.length & 1) == 0); // must be even

		const int gain = this.gain;
		const int bass = this.bass;
		chan_t[] c = ch[0 .. $];
		do {
			// cache in registers
			int sum = c[$ - 1].sum;
			int pp1 = c[$ - 1].pp1;
			int p1 = c[$ - 1].p1;

			for (int i = 0; i < io.length; i += 2) {
				// Low-pass filter (two point FIR with coeffs 0.25, 0.75)
				int f = io[i] + p1;
				p1 = io[i] * 3;

				// High-pass filter ("leaky integrator")
				int delta = f - pp1;
				pp1 = f;
				int s = sum >> (gain_bits + 2);
				sum += (delta * gain) - (sum >> bass);

				// Clamp to 16 bits
				if (cast(short) s != s)
					s = (s >> 31) ^ 0x7FFF;

				io[i] = cast(short) s;
			}

			c[$ - 1].p1 = p1;
			c[$ - 1].pp1 = pp1;
			c[$ - 1].sum = sum;
			io = io[1 .. $];
			c = c[0 .. $ - 1];
		}
		while (c.length > 0);
	}

	// Optional features

	// Clears filter to silence
	void clear() @safe {
		ch = ch.init;
	}

	// Sets gain (volume), where gain_unit is normal. Gains greater than gain_unit
	// are fine, since output is clamped to 16-bit sample range.
	enum gain_unit = 0x100;
	void set_gain(int g) @safe {
		gain = g;
	}

	// Sets amount of bass (logarithmic scale)
	enum bass_none = 0;
	enum bass_norm = 8; // normal amount
	enum bass_max = 31;
	void set_bass(int b) @safe {
		bass = b;
	}

private:
	enum gain_bits = 8;
	int gain = gain_unit;
	int bass = bass_norm;
	struct chan_t {
		int p1, pp1, sum;
	}
	chan_t[2] ch;
}
