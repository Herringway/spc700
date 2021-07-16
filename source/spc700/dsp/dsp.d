// Highly accurate SNES SPC-700 DSP emulator
module spc700.dsp.dsp;

import core.stdc.string;

void CLAMP16(ref int io) nothrow @safe {
	if (cast(short) io != io)
		io = (io >> 31) ^ 0x7FFF;
}

immutable ubyte[SPC_DSP.register_count] initial_regs = [
	0x45, 0x8B, 0x5A, 0x9A, 0xE4, 0x82, 0x1B, 0x78, 0x00, 0x00, 0xAA, 0x96, 0x89, 0x0E, 0xE0, 0x80,
	0x2A, 0x49, 0x3D, 0xBA, 0x14, 0xA0, 0xAC, 0xC5, 0x00, 0x00, 0x51, 0xBB, 0x9C, 0x4E, 0x7B, 0xFF,
	0xF4, 0xFD, 0x57, 0x32, 0x37, 0xD9, 0x42, 0x22, 0x00, 0x00, 0x5B, 0x3C, 0x9F, 0x1B, 0x87, 0x9A,
	0x6F, 0x27, 0xAF, 0x7B, 0xE5, 0x68, 0x0A, 0xD9, 0x00, 0x00, 0x9A, 0xC5, 0x9C, 0x4E, 0x7B, 0xFF,
	0xEA, 0x21, 0x78, 0x4F, 0xDD, 0xED, 0x24, 0x14, 0x00, 0x00, 0x77, 0xB1, 0xD1, 0x36, 0xC1, 0x67,
	0x52, 0x57, 0x46, 0x3D, 0x59, 0xF4, 0x87, 0xA4, 0x00, 0x00, 0x7E, 0x44, 0x9C, 0x4E, 0x7B, 0xFF,
	0x75, 0xF5, 0x06, 0x97, 0x10, 0xC3, 0x24, 0xBB, 0x00, 0x00, 0x7B, 0x7A, 0xE0, 0x60, 0x12, 0x0F,
	0xF7, 0x74, 0x1C, 0xE5, 0x39, 0x3D, 0x73, 0xC1, 0x00, 0x00, 0x7A, 0xB3, 0xFF, 0x4E, 0x7B, 0xFF
];

immutable short[512] gauss = [
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2,
	2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5,
	6, 6, 6, 6, 7, 7, 7, 8, 8, 8, 9, 9, 9, 10, 10, 10,
	11, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15, 15, 16, 16, 17, 17,
	18, 19, 19, 20, 20, 21, 21, 22, 23, 23, 24, 24, 25, 26, 27, 27,
	28, 29, 29, 30, 31, 32, 32, 33, 34, 35, 36, 36, 37, 38, 39, 40,
	41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56,
	58, 59, 60, 61, 62, 64, 65, 66, 67, 69, 70, 71, 73, 74, 76, 77,
	78, 80, 81, 83, 84, 86, 87, 89, 90, 92, 94, 95, 97, 99, 100, 102,
	104, 106, 107, 109, 111, 113, 115, 117, 118, 120, 122, 124, 126, 128, 130, 132,
	134, 137, 139, 141, 143, 145, 147, 150, 152, 154, 156, 159, 161, 163, 166, 168,
	171, 173, 175, 178, 180, 183, 186, 188, 191, 193, 196, 199, 201, 204, 207, 210,
	212, 215, 218, 221, 224, 227, 230, 233, 236, 239, 242, 245, 248, 251, 254, 257,
	260, 263, 267, 270, 273, 276, 280, 283, 286, 290, 293, 297, 300, 304, 307, 311,
	314, 318, 321, 325, 328, 332, 336, 339, 343, 347, 351, 354, 358, 362, 366, 370,
	374, 378, 381, 385, 389, 393, 397, 401, 405, 410, 414, 418, 422, 426, 430, 434,
	439, 443, 447, 451, 456, 460, 464, 469, 473, 477, 482, 486, 491, 495, 499, 504,
	508, 513, 517, 522, 527, 531, 536, 540, 545, 550, 554, 559, 563, 568, 573, 577,
	582, 587, 592, 596, 601, 606, 611, 615, 620, 625, 630, 635, 640, 644, 649, 654,
	659, 664, 669, 674, 678, 683, 688, 693, 698, 703, 708, 713, 718, 723, 728, 732,
	737, 742, 747, 752, 757, 762, 767, 772, 777, 782, 787, 792, 797, 802, 806, 811,
	816, 821, 826, 831, 836, 841, 846, 851, 855, 860, 865, 870, 875, 880, 884, 889,
	894, 899, 904, 908, 913, 918, 923, 927, 932, 937, 941, 946, 951, 955, 960, 965,
	969, 974, 978, 983, 988, 992, 997, 1001, 1005, 1010, 1014, 1019, 1023, 1027, 1032, 1036,
	1040, 1045, 1049, 1053, 1057, 1061, 1066, 1070, 1074, 1078, 1082, 1086, 1090, 1094, 1098, 1102,
	1106, 1109, 1113, 1117, 1121, 1125, 1128, 1132, 1136, 1139, 1143, 1146, 1150, 1153, 1157, 1160,
	1164, 1167, 1170, 1174, 1177, 1180, 1183, 1186, 1190, 1193, 1196, 1199, 1202, 1205, 1207, 1210,
	1213, 1216, 1219, 1221, 1224, 1227, 1229, 1232, 1234, 1237, 1239, 1241, 1244, 1246, 1248, 1251,
	1253, 1255, 1257, 1259, 1261, 1263, 1265, 1267, 1269, 1270, 1272, 1274, 1275, 1277, 1279, 1280,
	1282, 1283, 1284, 1286, 1287, 1288, 1290, 1291, 1292, 1293, 1294, 1295, 1296, 1297, 1297, 1298,
	1299, 1300, 1300, 1301, 1302, 1302, 1303, 1303, 1303, 1304, 1304, 1304, 1304, 1304, 1305, 1305
];

enum int simple_counter_range = 2048 * 5 * 3; // 30720

immutable uint[32] counter_rates = [simple_counter_range + 1, // never fires
	2048, 1536,
	1280, 1024, 768,
	640, 512, 384,
	320, 256, 192,
	160, 128, 96,
	80, 64, 48,
	40, 32, 24,
	20, 16, 12,
	10, 8, 6,
	5, 4, 3,
	2,
	1
];

immutable uint[32] counter_offsets = [
	1, 0, 1040,
	536, 0, 1040,
	536, 0, 1040,
	536, 0, 1040,
	536, 0, 1040,
	536, 0, 1040,
	536, 0, 1040,
	536, 0, 1040,
	536, 0, 1040,
	536, 0, 1040,
	0,
	0
];
struct SPC_DSP {
nothrow:
public:

	// Setup

	// Initializes DSP and has it use the 64K RAM provided
	void init_(ubyte[] ram_64k) @system {
		m.ram = cast(ubyte[]) ram_64k;
		mute_voices(0);
		disable_surround(false);
		set_output(null);
		reset();

		version (NDEBUG) {
		} else {
			// be sure this sign-extends
			assert(cast(short) 0x8000 == -0x8000);

			// be sure right shift preserves sign
			assert((-1 >> 1) == -1);

			// check clamp macro
			int i;
			i = +0x8000;
			if (cast(short) i != i)
				i = (i >> 31) ^ 0x7FFF;
			assert(i == +0x7FFF);
			i = -0x8001;
			if (cast(short) i != i)
				i = (i >> 31) ^ 0x7FFF;
			assert(i == -0x8000);
		}
	}

	// Sets destination for output samples. If out is NULL or out_size is 0,
	// doesn't generate any.
	alias sample_t = short;
	void set_output(sample_t[] out_) @system {
		assert((out_.length & 1) == 0); // must be even
		if (!out_) {
			out_ = m.extra;
		}
		m.out_begin = out_.ptr;
		m.out_ = out_.ptr;
		m.out_end = out_.ptr + out_.length;
	}

	// Number of samples written to output since it was last set, always
	// a multiple of 2. Undefined if more samples were generated than
	// output buffer could hold.
	int sample_count() const @safe {
		return cast(int)(m.out_ - m.out_begin);
	}

	// Emulation

	// Resets DSP to power-on state
	void reset() @safe {
		load(initial_regs);
	}

	// Emulates pressing reset switch on SNES
	void soft_reset() @safe {
		m.regs[r_flg] = 0xE0;
		soft_reset_common();
	}

	// Reads/writes DSP registers. For accuracy, you must first call run()
	// to catch the DSP up to present.
	int read(int addr) const @safe {
		assert(cast(uint) addr < register_count);
		return m.regs[addr];
	}

	void write(int addr, int data) @safe {
		assert(cast(uint) addr < register_count);

		m.regs[addr] = cast(ubyte) data;
		switch (addr & 0x0F) {
		case v_envx:
			m.envx_buf = cast(ubyte) data;
			break;

		case v_outx:
			m.outx_buf = cast(ubyte) data;
			break;

		case 0x0C:
			if (addr == r_kon)
				m.new_kon = cast(ubyte) data;

			if (addr == r_endx) // always cleared, regardless of data written
			{
				m.endx_buf = 0;
				m.regs[r_endx] = 0;
			}
			break;
		default:
			break;
		}
	}

	// Runs DSP for specified number of clocks (~1024000 per second). Every 32 clocks
	// a pair of samples is be generated.
	void run(int clocks_remain) @system {
		assert(clocks_remain > 0);

		const int phase = m.phase;
		m.phase = (phase + clocks_remain) & 31;
		switch (phase) {
		loop:
		case 0:
			voice_V5(&m.voices[0]);
			voice_V2(&m.voices[1]);
			if (1 && !--clocks_remain)
				break;
			goto case;
		case 1:
			voice_V6(&m.voices[0]);
			voice_V3(&m.voices[1]);
			if (2 && !--clocks_remain)
				break;
			goto case;
		case 2:
			voice_V7_V4_V1(&m.voices[0]);
			if (3 && !--clocks_remain)
				break;
			goto case;
		case 3:
			voice_V8_V5_V2(&m.voices[0]);
			if (4 && !--clocks_remain)
				break;
			goto case;
		case 4:
			voice_V9_V6_V3(&m.voices[0]);
			if (5 && !--clocks_remain)
				break;
			goto case;
		case 5:
			voice_V7_V4_V1(&m.voices[1]);
			if (6 && !--clocks_remain)
				break;
			goto case;
		case 6:
			voice_V8_V5_V2(&m.voices[1]);
			if (7 && !--clocks_remain)
				break;
			goto case;
		case 7:
			voice_V9_V6_V3(&m.voices[1]);
			if (8 && !--clocks_remain)
				break;
			goto case;
		case 8:
			voice_V7_V4_V1(&m.voices[2]);
			if (9 && !--clocks_remain)
				break;
			goto case;
		case 9:
			voice_V8_V5_V2(&m.voices[2]);
			if (10 && !--clocks_remain)
				break;
			goto case;
		case 10:
			voice_V9_V6_V3(&m.voices[2]);
			if (11 && !--clocks_remain)
				break;
			goto case;
		case 11:
			voice_V7_V4_V1(&m.voices[3]);
			if (12 && !--clocks_remain)
				break;
			goto case;
		case 12:
			voice_V8_V5_V2(&m.voices[3]);
			if (13 && !--clocks_remain)
				break;
			goto case;
		case 13:
			voice_V9_V6_V3(&m.voices[3]);
			if (14 && !--clocks_remain)
				break;
			goto case;
		case 14:
			voice_V7_V4_V1(&m.voices[4]);
			if (15 && !--clocks_remain)
				break;
			goto case;
		case 15:
			voice_V8_V5_V2(&m.voices[4]);
			if (16 && !--clocks_remain)
				break;
			goto case;
		case 16:
			voice_V9_V6_V3(&m.voices[4]);
			if (17 && !--clocks_remain)
				break;
			goto case;
		case 17:
			voice_V1(&m.voices[0]);
			voice_V7(&m.voices[5]);
			voice_V4(&m.voices[6]);
			if (18 && !--clocks_remain)
				break;
			goto case;
		case 18:
			voice_V8_V5_V2(&m.voices[5]);
			if (19 && !--clocks_remain)
				break;
			goto case;
		case 19:
			voice_V9_V6_V3(&m.voices[5]);
			if (20 && !--clocks_remain)
				break;
			goto case;
		case 20:
			voice_V1(&m.voices[1]);
			voice_V7(&m.voices[6]);
			voice_V4(&m.voices[7]);
			if (21 && !--clocks_remain)
				break;
			goto case;
		case 21:
			voice_V8(&m.voices[6]);
			voice_V5(&m.voices[7]);
			voice_V2(&m.voices[0]); /* t_brr_next_addr order dependency */
			if (22 && !--clocks_remain)
				break;
			goto case;
		case 22:
			voice_V3a(&m.voices[0]);
			voice_V9(&m.voices[6]);
			voice_V6(&m.voices[7]);
			echo_22();
			if (23 && !--clocks_remain)
				break;
			goto case;
		case 23:
			voice_V7(&m.voices[7]);
			echo_23();
			if (24 && !--clocks_remain)
				break;
			goto case;
		case 24:
			voice_V8(&m.voices[7]);
			echo_24();
			if (25 && !--clocks_remain)
				break;
			goto case;
		case 25:
			voice_V3b(&m.voices[0]);
			voice_V9(&m.voices[7]);
			echo_25();
			if (26 && !--clocks_remain)
				break;
			goto case;
		case 26:
			echo_26();
			if (27 && !--clocks_remain)
				break;
			goto case;
		case 27:
			misc_27();
			echo_27();
			if (28 && !--clocks_remain)
				break;
			goto case;
		case 28:
			misc_28();
			echo_28();
			if (29 && !--clocks_remain)
				break;
			goto case;
		case 29:
			misc_29();
			echo_29();
			if (30 && !--clocks_remain)
				break;
			goto case;
		case 30:
			misc_30();
			voice_V3c(&m.voices[0]);
			echo_30();
			if (31 && !--clocks_remain)
				break;
			goto case;
		case 31:
			voice_V4(&m.voices[0]);
			voice_V1(&m.voices[2]);
			goto default;
		default:
			if (--clocks_remain)
				goto loop;
		}
	}

	// Sound control

	// Mutes voices corresponding to non-zero bits in mask (issues repeated KOFF events).
	// Reduces emulation accuracy.
	enum voice_count = 8;
	void mute_voices(int mask) @safe {
		m.mute_mask = mask;
	}

	// State

	// Resets DSP and uses supplied values to initialize registers
	enum register_count = 128;
	void load( in ubyte[register_count]  regs ) @safe {
		//reset everything but ram
		auto ram = m.ram;
		m = m.init;
		m.ram = ram;

		m.regs = regs;

		// Internal state
		for (int i = voice_count; --i >= 0;) {
			voice_t* v = &m.voices[i];
			v.brr_offset = 1;
			v.vbit = 1 << i;
			v.regs = m.regs[i * 0x10 .. (i + 1) * 0x10];
		}
		m.new_kon = m.regs[r_kon];
		m.t_dir = m.regs[r_dir];
		m.t_esa = m.regs[r_esa];

		soft_reset_common();
	}

	// Saves/loads exact emulator state
	enum state_size = 640; // maximum space needed when saving
	version(SPC_NO_COPY_STATE_FUNCS) {} else {
		alias copy_func_t = void delegate(ubyte** io, void[] state) @safe nothrow;
		void copy_state(ubyte** io, copy_func_t copy) @safe {
			SPC_State_Copier copier = SPC_State_Copier(io, copy);

			auto SPC_COPY(T, U)(ref U state) {
				state = cast(T) copier.copy_int(state, T.sizeof);
				assert(cast(T) state == state);
			}
			// DSP registers
			copier.copy(m.regs[0 .. register_count]);

			// Internal state

			// Voices
			int i;
			for (i = 0; i < voice_count; i++) {
				voice_t* v = &m.voices[i];

				// BRR buffer
				int i2;
				for (i2 = 0; i2 < brr_buf_size; i2++) {
					int s = v.buf[i2];
					SPC_COPY!short(s);
					v.buf[i2] = v.buf[i2 + brr_buf_size] = s;
				}

				SPC_COPY!ushort(v.interp_pos);
				SPC_COPY!ushort(v.brr_addr);
				SPC_COPY!ushort(v.env);
				SPC_COPY!short(v.hidden_env);
				SPC_COPY!ubyte(v.buf_pos);
				SPC_COPY!ubyte(v.brr_offset);
				SPC_COPY!ubyte(v.kon_delay);
				{
					int m = v.env_mode;
					SPC_COPY!ubyte(m);
					v.env_mode = cast(env_mode_t) m;
				}
				SPC_COPY!ubyte(v.t_envx_out);

				copier.extra();
			}

			// Echo history
			for (i = 0; i < echo_hist_size; i++) {
				int j;
				for (j = 0; j < 2; j++) {
					int s = m.echo_hist[i + m.echo_hist_pos][j];
					SPC_COPY!short(s);
					m.echo_hist[i][j] = s; // write back at offset 0
				}
			}
			m.echo_hist_pos = 0;
			m.echo_hist[echo_hist_size .. echo_hist_size * 2] = m.echo_hist[0 .. echo_hist_size];

			// Misc
			SPC_COPY!ubyte(m.every_other_sample);
			SPC_COPY!ubyte(m.kon);

			SPC_COPY!ushort(m.noise);
			SPC_COPY!ushort(m.counter);
			SPC_COPY!ushort(m.echo_offset);
			SPC_COPY!ushort(m.echo_length);
			SPC_COPY!ubyte(m.phase);

			SPC_COPY!ubyte(m.new_kon);
			SPC_COPY!ubyte(m.endx_buf);
			SPC_COPY!ubyte(m.envx_buf);
			SPC_COPY!ubyte(m.outx_buf);

			SPC_COPY!ubyte(m.t_pmon);
			SPC_COPY!ubyte(m.t_non);
			SPC_COPY!ubyte(m.t_eon);
			SPC_COPY!ubyte(m.t_dir);
			SPC_COPY!ubyte(m.t_koff);

			SPC_COPY!ushort(m.t_brr_next_addr);
			SPC_COPY!ubyte(m.t_adsr0);
			SPC_COPY!ubyte(m.t_brr_header);
			SPC_COPY!ubyte(m.t_brr_byte);
			SPC_COPY!ubyte(m.t_srcn);
			SPC_COPY!ubyte(m.t_esa);
			SPC_COPY!ubyte(m.t_echo_enabled);

			SPC_COPY!short(m.t_main_out[0]);
			SPC_COPY!short(m.t_main_out[1]);
			SPC_COPY!short(m.t_echo_out[0]);
			SPC_COPY!short(m.t_echo_out[1]);
			SPC_COPY!short(m.t_echo_in[0]);
			SPC_COPY!short(m.t_echo_in[1]);

			SPC_COPY!ushort(m.t_dir_addr);
			SPC_COPY!ushort(m.t_pitch);
			SPC_COPY!short(m.t_output);
			SPC_COPY!ushort(m.t_echo_ptr);
			SPC_COPY!ubyte(m.t_looped);

			copier.extra();
		}
	}
	// Returns non-zero if new key-on events occurred since last call
	bool check_kon() @safe {
		bool old = m.kon_check;
		m.kon_check = 0;
		return old;
	}

	// DSP register addresses

	// Global registers
	enum {
		r_mvoll = 0x0C,
		r_mvolr = 0x1C,
		r_evoll = 0x2C,
		r_evolr = 0x3C,
		r_kon = 0x4C,
		r_koff = 0x5C,
		r_flg = 0x6C,
		r_endx = 0x7C,
		r_efb = 0x0D,
		r_pmon = 0x2D,
		r_non = 0x3D,
		r_eon = 0x4D,
		r_dir = 0x5D,
		r_esa = 0x6D,
		r_edl = 0x7D,
		r_fir = 0x0F // 8 coefficients at 0x0F, 0x1F ... 0x7F
	}

	// Voice registers
	enum {
		v_voll = 0x00,
		v_volr = 0x01,
		v_pitchl = 0x02,
		v_pitchh = 0x03,
		v_srcn = 0x04,
		v_adsr0 = 0x05,
		v_adsr1 = 0x06,
		v_gain = 0x07,
		v_envx = 0x08,
		v_outx = 0x09
	}

public:
	enum extra_size = 16;
	sample_t[] extra() return @safe  {
		return m.extra;
	}

	const(sample_t)* out_pos() const @safe {
		return m.out_;
	}

	void disable_surround(bool) @safe{
	} // not supported
public:

	enum echo_hist_size = 8;

	enum env_mode_t {
		env_release,
		env_attack,
		env_decay,
		env_sustain
	};
	enum brr_buf_size = 12;
	struct voice_t {
		int[brr_buf_size * 2] buf; // decoded samples (twice the size to simplify wrap handling)
		int buf_pos; // place in buffer where next samples will be decoded
		int interp_pos; // relative fractional position in sample (0x1000 = 1.0)
		int brr_addr; // address of current BRR block
		int brr_offset; // current decoding offset in BRR block
		ubyte[] regs; // pointer to voice's DSP registers
		int vbit; // bitmask for voice: 0x01 for voice 0, 0x02 for voice 1, etc.
		int kon_delay; // KON delay/current setup phase
		env_mode_t env_mode;
		int env; // current envelope level
		int hidden_env; // used by GAIN mode 7, very obscure quirk
		ubyte t_envx_out;
	};
private:
	enum brr_block_size = 9;

	struct state_t {
		ubyte[register_count] regs;

		// Echo history keeps most recent 8 samples (twice the size to simplify wrap handling)
		int[2][echo_hist_size * 2] echo_hist;
		size_t echo_hist_pos; // &echo_hist [0 to 7]

		int every_other_sample; // toggles every sample
		int kon; // KON value when last checked
		int noise;
		int counter;
		int echo_offset; // offset from ESA in echo buffer
		int echo_length; // number of bytes that echo_offset will stop at
		int phase; // next clock cycle to run (0-31)
		bool kon_check; // set when a new KON occurs

		// Hidden registers also written to when main register is written to
		int new_kon;
		ubyte endx_buf;
		ubyte envx_buf;
		ubyte outx_buf;

		// Temporary state between clocks

		// read once per sample
		int t_pmon;
		int t_non;
		int t_eon;
		int t_dir;
		int t_koff;

		// read a few clocks ahead then used
		int t_brr_next_addr;
		int t_adsr0;
		int t_brr_header;
		int t_brr_byte;
		int t_srcn;
		int t_esa;
		int t_echo_enabled;

		// internal state that is recalculated every sample
		int t_dir_addr;
		int t_pitch;
		int t_output;
		int t_looped;
		int t_echo_ptr;

		// left/right sums
		int[2] t_main_out;
		int[2] t_echo_out;
		int[2] t_echo_in;

		voice_t[voice_count] voices;

		// non-emulation state
		ubyte[] ram; // 64K shared RAM between DSP and SMP
		int mute_mask;
		sample_t* out_;
		sample_t* out_end;
		sample_t* out_begin;
		sample_t[extra_size] extra;
	}

	state_t m;

	void init_counter() @safe {
		m.counter = 0;
	}

	void run_counters() @safe {
		if (--m.counter < 0)
			m.counter = simple_counter_range - 1;
	}

	uint read_counter(int rate) @safe {
		return (cast(uint) m.counter + counter_offsets[rate]) % counter_rates[rate];
	}

	int interpolate(const(voice_t)* v) @system {
		// Make pointers into gaussian based on fractional position between samples
		int offset = v.interp_pos >> 4 & 0xFF;
		const(short)* fwd = &gauss[255 - offset];
		const(short)* rev = &gauss[offset]; // mirror left half of gaussian

		const(int)* in_ = &v.buf[(v.interp_pos >> 12) + v.buf_pos];
		int out_;
		out_ = (fwd[0] * in_[0]) >> 11;
		out_ += (fwd[256] * in_[1]) >> 11;
		out_ += (rev[256] * in_[2]) >> 11;
		out_ = cast(short) out_;
		out_ += (rev[0] * in_[3]) >> 11;

		CLAMP16(out_);
		out_ &= ~1;
		return out_;
	}

	void run_envelope(voice_t* v) @safe {
		int env = v.env;
		if (v.env_mode == env_mode_t.env_release) // 60%
		{
			if ((env -= 0x8) < 0)
				env = 0;
			v.env = env;
		} else {
			int rate;
			int env_data = v.regs[v_adsr1];
			if (m.t_adsr0 & 0x80) // 99% ADSR
			{
				if (v.env_mode >= env_mode_t.env_decay) // 99%
				{
					env--;
					env -= env >> 8;
					rate = env_data & 0x1F;
					if (v.env_mode == env_mode_t.env_decay) // 1%
						rate = (m.t_adsr0 >> 3 & 0x0E) + 0x10;
				} else // env_attack
				{
					rate = (m.t_adsr0 & 0x0F) * 2 + 1;
					env += rate < 31 ? 0x20 : 0x400;
				}
			} else // GAIN
			{
				int mode;
				env_data = v.regs[v_gain];
				mode = env_data >> 5;
				if (mode < 4) // direct
				{
					env = env_data * 0x10;
					rate = 31;
				} else {
					rate = env_data & 0x1F;
					if (mode == 4) // 4: linear decrease
					{
						env -= 0x20;
					} else if (mode < 6) // 5: exponential decrease
					{
						env--;
						env -= env >> 8;
					} else // 6,7: linear increase
					{
						env += 0x20;
						if (mode > 6 && cast(uint) v.hidden_env >= 0x600)
							env += 0x8 - 0x20; // 7: two-slope linear increase
					}
				}
			}

			// Sustain level
			if ((env >> 8) == (env_data >> 5) && v.env_mode == env_mode_t.env_decay)
				v.env_mode = env_mode_t.env_sustain;

			v.hidden_env = env;

			// unsigned cast because linear decrease going negative also triggers this
			if (cast(uint) env > 0x7FF) {
				env = (env < 0 ? 0 : 0x7FF);
				if (v.env_mode == env_mode_t.env_attack)
					v.env_mode = env_mode_t.env_decay;
			}

			if (!read_counter(rate))
				v.env = env; // nothing else is controlled by the counter
		}
	}

	void decode_brr(voice_t* v) @system {
		// Arrange the four input nybbles in 0xABCD order for easy decoding
		int nybbles = m.t_brr_byte * 0x100 + m.ram[(v.brr_addr + v.brr_offset + 1) & 0xFFFF];

		const int header = m.t_brr_header;

		// Write to next four samples in circular buffer
		int* pos = &v.buf[v.buf_pos];
		int* end;
		if ((v.buf_pos += 4) >= brr_buf_size)
			v.buf_pos = 0;

		// Decode four samples
		for (end = pos + 4; pos < end; pos++, nybbles <<= 4) {
			// Extract nybble and sign-extend
			int s = cast(short) nybbles >> 12;

			// Shift sample based on header
			const int shift = header >> 4;
			s = (s << shift) >> 1;
			if (shift >= 0xD) // handle invalid range
				s = (s >> 25) << 11; // same as: s = (s < 0 ? -0x800 : 0)

			// Apply IIR filter (8 is the most commonly used)
			const int filter = header & 0x0C;
			const int p1 = pos[brr_buf_size - 1];
			const int p2 = pos[brr_buf_size - 2] >> 1;
			if (filter >= 8) {
				s += p1;
				s -= p2;
				if (filter == 8) // s += p1 * 0.953125 - p2 * 0.46875
				{
					s += p2 >> 4;
					s += (p1 * -3) >> 6;
				} else // s += p1 * 0.8984375 - p2 * 0.40625
				{
					s += (p1 * -13) >> 7;
					s += (p2 * 3) >> 4;
				}
			} else if (filter) // s += p1 * 0.46875
			{
				s += p1 >> 1;
				s += (-p1) >> 5;
			}

			// Adjust and write sample
			CLAMP16(s);
			s = cast(short)(s * 2);
			pos[brr_buf_size] = pos[0] = s; // second copy simplifies wrap-around
		}
	}

	void misc_27() @safe {
		m.t_pmon = m.regs[r_pmon] & 0xFE; // voice 0 doesn't support PMON
	}

	void misc_28() @safe {
		m.t_non = m.regs[r_non];
		m.t_eon = m.regs[r_eon];
		m.t_dir = m.regs[r_dir];
	}

	void misc_29() @safe {
		if ((m.every_other_sample ^= 1) != 0)
			m.new_kon &= ~m.kon; // clears KON 63 clocks after it was last read
	}

	void misc_30() @safe {
		if (m.every_other_sample) {
			m.kon = m.new_kon;
			m.t_koff = m.regs[r_koff] | m.mute_mask;
		}

		run_counters();

		// Noise
		if (!read_counter(m.regs[r_flg] & 0x1F)) {
			int feedback = (m.noise << 13) ^ (m.noise << 14);
			m.noise = (feedback & 0x4000) ^ (m.noise >> 1);
		}
	}

	void voice_output(const(voice_t)* v, int ch) @safe {
		// Apply left/right volume
		int amp = (m.t_output * cast(byte) v.regs[v_voll + ch]) >> 7;

		// Add to output total
		m.t_main_out[ch] += amp;
		CLAMP16(m.t_main_out[ch]);

		// Optionally add to echo total
		if (m.t_eon & v.vbit) {
			m.t_echo_out[ch] += amp;
			CLAMP16(m.t_echo_out[ch]);
		}
	}

	void voice_V1(voice_t* v) @safe {
		m.t_dir_addr = m.t_dir * 0x100 + m.t_srcn * 4;
		m.t_srcn = v.regs[v_srcn];
	}

	void voice_V2(voice_t* v) {
		// Read sample pointer (ignored if not needed)
		const(ubyte)* entry = &m.ram[m.t_dir_addr];
		if (!v.kon_delay)
			entry += 2;
		m.t_brr_next_addr = *cast(ushort*)(entry);

		m.t_adsr0 = v.regs[v_adsr0];

		// Read pitch, spread over two clocks
		m.t_pitch = v.regs[v_pitchl];
	}
	// Most voices do all these in one clock, so make a handy composite
	void voice_V3(voice_t* v) @system {
		voice_V3a(v);
		voice_V3b(v);
		voice_V3c(v);
	}

	void voice_V3a(voice_t* v) @safe {
		m.t_pitch += (v.regs[v_pitchh] & 0x3F) << 8;
	}

	void voice_V3b(voice_t* v) @safe {
		// Read BRR header and byte
		m.t_brr_byte = m.ram[(v.brr_addr + v.brr_offset) & 0xFFFF];
		m.t_brr_header = m.ram[v.brr_addr]; // brr_addr doesn't need masking
	}

	void voice_V3c(voice_t* v) @system {
		// Pitch modulation using previous voice's output
		if (m.t_pmon & v.vbit)
			m.t_pitch += ((m.t_output >> 5) * m.t_pitch) >> 10;

		if (v.kon_delay) {
			// Get ready to start BRR decoding on next sample
			if (v.kon_delay == 5) {
				v.brr_addr = m.t_brr_next_addr;
				v.brr_offset = 1;
				v.buf_pos = 0;
				m.t_brr_header = 0; // header is ignored on this sample
				m.kon_check = true;
			}

			// Envelope is never run during KON
			v.env = 0;
			v.hidden_env = 0;

			// Disable BRR decoding until last three samples
			v.interp_pos = 0;
			if (--v.kon_delay & 3)
				v.interp_pos = 0x4000;

			// Pitch is never added during KON
			m.t_pitch = 0;
		}

		// Gaussian interpolation
		{
			int output = interpolate(v);

			// Noise
			if (m.t_non & v.vbit)
				output = cast(short)(m.noise * 2);

			// Apply envelope
			m.t_output = (output * v.env) >> 11 & ~1;
			v.t_envx_out = cast(ubyte)(v.env >> 4);
		}

		// Immediate silence due to end of sample or soft reset
		if (m.regs[r_flg] & 0x80 || (m.t_brr_header & 3) == 1) {
			v.env_mode = env_mode_t.env_release;
			v.env = 0;
		}

		if (m.every_other_sample) {
			// KOFF
			if (m.t_koff & v.vbit)
				v.env_mode = env_mode_t.env_release;

			// KON
			if (m.kon & v.vbit) {
				v.kon_delay = 5;
				v.env_mode = env_mode_t.env_attack;
			}
		}

		// Run envelope for next sample
		if (!v.kon_delay)
			run_envelope(v);
	}

	void voice_V4(voice_t* v) @system {
		// Decode BRR
		m.t_looped = 0;
		if (v.interp_pos >= 0x4000) {
			decode_brr(v);

			if ((v.brr_offset += 2) >= brr_block_size) {
				// Start decoding next BRR block
				assert(v.brr_offset == brr_block_size);
				v.brr_addr = (v.brr_addr + brr_block_size) & 0xFFFF;
				if (m.t_brr_header & 1) {
					v.brr_addr = m.t_brr_next_addr;
					m.t_looped = v.vbit;
				}
				v.brr_offset = 1;
			}
		}

		// Apply pitch
		v.interp_pos = (v.interp_pos & 0x3FFF) + m.t_pitch;

		// Keep from getting too far ahead (when using pitch modulation)
		if (v.interp_pos > 0x7FFF)
			v.interp_pos = 0x7FFF;

		// Output left
		voice_output(v, 0);
	}

	void voice_V5(voice_t* v) @safe {
		// Output right
		voice_output(v, 1);

		// ENDX, OUTX, and ENVX won't update if you wrote to them 1-2 clocks earlier
		int endx_buf = m.regs[r_endx] | m.t_looped;

		// Clear bit in ENDX if KON just began
		if (v.kon_delay == 5)
			endx_buf &= ~v.vbit;
		m.endx_buf = cast(ubyte) endx_buf;
	}

	void voice_V6(voice_t*) @safe {
		m.outx_buf = cast(ubyte)(m.t_output >> 8);
	}

	void voice_V7(voice_t* v) @safe {
		// Update ENDX
		m.regs[r_endx] = m.endx_buf;

		m.envx_buf = v.t_envx_out;
	}

	void voice_V8(voice_t* v) @safe {
		// Update OUTX
		v.regs[v_outx] = m.outx_buf;
	}

	void voice_V9(voice_t* v) @safe {
		// Update ENVX
		v.regs[v_envx] = m.envx_buf;
	}
	// Common combinations of voice steps on different voices. This greatly reduces
	// code size and allows everything to be inlined in these functions.
	void voice_V7_V4_V1(voice_t* v) @system {
		voice_V7(v);
		voice_V1(v + 3);
		voice_V4(v + 1);
	}

	void voice_V8_V5_V2(voice_t* v) @system {
		voice_V8(v);
		voice_V5(v + 1);
		voice_V2(v + 2);
	}

	void voice_V9_V6_V3(voice_t* v) @system {
		voice_V9(v);
		voice_V6(v + 1);
		voice_V3(v + 2);
	}

	// Current echo buffer pointer for left/right channel
	auto ECHO_PTR(int ch) @safe {
		return &m.ram[m.t_echo_ptr + ch * 2];
	}

	// Sample in echo history buffer, where 0 is the oldest
	auto ref ECHO_FIR(size_t i) @safe {
		return m.echo_hist[i + m.echo_hist_pos];
	}

	// Calculate FIR point for left/right channel
	auto CALC_FIR(size_t i, int ch) @safe {
		return (ECHO_FIR(i + 1)[ch] * cast(byte) m.regs[r_fir + i * 0x10]) >> 6;
	}

	void echo_read(int ch) @system {
		int s = cast(short)*cast(ushort*)(ECHO_PTR(ch));
		// second copy simplifies wrap-around handling
		ECHO_FIR(0)[ch] = ECHO_FIR(8)[ch] = s >> 1;
	}

	int echo_output(int ch) @safe {
		int out_ = cast(short)((m.t_main_out[ch] * cast(byte) m.regs[r_mvoll + ch * 0x10]) >> 7) + cast(short)((m.t_echo_in[ch] * cast(byte) m.regs[r_evoll + ch * 0x10]) >> 7);
		CLAMP16(out_);
		return out_;
	}

	void echo_write(int ch) @system {
		if (!(m.t_echo_enabled & 0x20))
			*cast(ushort*) ECHO_PTR(ch) = cast(ushort) m.t_echo_out[ch];
		m.t_echo_out[ch] = 0;
	}

	void echo_22() @system {
		// History
		if (++m.echo_hist_pos >= echo_hist_size)
			m.echo_hist_pos = 0;

		m.t_echo_ptr = (m.t_esa * 0x100 + m.echo_offset) & 0xFFFF;
		echo_read(0);

		// FIR (using l and r temporaries below helps compiler optimize)
		int l = CALC_FIR(0, 0);
		int r = CALC_FIR(0, 1);

		m.t_echo_in[0] = l;
		m.t_echo_in[1] = r;
	}

	void echo_23() @system {
		int l = CALC_FIR(1, 0) + CALC_FIR(2, 0);
		int r = CALC_FIR(1, 1) + CALC_FIR(2, 1);

		m.t_echo_in[0] += l;
		m.t_echo_in[1] += r;

		echo_read(1);
	}

	void echo_24() @safe {
		int l = CALC_FIR(3, 0) + CALC_FIR(4, 0) + CALC_FIR(5, 0);
		int r = CALC_FIR(3, 1) + CALC_FIR(4, 1) + CALC_FIR(5, 1);

		m.t_echo_in[0] += l;
		m.t_echo_in[1] += r;
	}

	void echo_25() @safe {
		int l = m.t_echo_in[0] + CALC_FIR(6, 0);
		int r = m.t_echo_in[1] + CALC_FIR(6, 1);

		l = cast(short) l;
		r = cast(short) r;

		l += cast(short) CALC_FIR(7, 0);
		r += cast(short) CALC_FIR(7, 1);

		CLAMP16(l);
		CLAMP16(r);

		m.t_echo_in[0] = l & ~1;
		m.t_echo_in[1] = r & ~1;
	}

	void echo_26() @safe {
		// Left output volumes
		// (save sample for next clock so we can output both together)
		m.t_main_out[0] = echo_output(0);

		// Echo feedback
		int l = m.t_echo_out[0] + cast(short)((m.t_echo_in[0] * cast(byte) m.regs[r_efb]) >> 7);
		int r = m.t_echo_out[1] + cast(short)((m.t_echo_in[1] * cast(byte) m.regs[r_efb]) >> 7);

		CLAMP16(l);
		CLAMP16(r);

		m.t_echo_out[0] = l & ~1;
		m.t_echo_out[1] = r & ~1;
	}

	void echo_27() @system {
		// Output
		int l = m.t_main_out[0];
		int r = echo_output(1);
		m.t_main_out[0] = 0;
		m.t_main_out[1] = 0;

		// TODO: global muting isn't this simple (turns DAC on and off
		// or something, causing small ~37-sample pulse when first muted)
		if (m.regs[r_flg] & 0x40) {
			l = 0;
			r = 0;
		}

		// Output sample to DAC
		static if (is(SPC_DSP_OUT_HOOK)) {
			SPC_DSP_OUT_HOOK(l, r);
		} else {
			sample_t* out_ = m.out_;

			{
				out_[0] = cast(short) l;
				out_[1] = cast(short) r;
				out_ += 2;
				if (out_ >= m.out_end) {
					assert(out_ == m.out_end);
					assert(m.out_end != &m.extra.ptr[extra_size] || (m.extra.ptr <= m.out_begin && m.extra.ptr < &m.extra.ptr[extra_size]));
					out_ = m.extra.ptr;
					m.out_end = &m.extra.ptr[extra_size];
				}
			}
			m.out_ = out_;
		}
	}

	void echo_28() @safe {
		m.t_echo_enabled = m.regs[r_flg];
	}

	void echo_29() @system {
		m.t_esa = m.regs[r_esa];

		if (!m.echo_offset)
			m.echo_length = (m.regs[r_edl] & 0x0F) * 0x800;

		m.echo_offset += 4;
		if (m.echo_offset >= m.echo_length)
			m.echo_offset = 0;

		// Write left echo
		echo_write(0);

		m.t_echo_enabled = m.regs[r_flg];
	}

	void echo_30() @system {
		// Write right echo
		echo_write(1);
	}

	void soft_reset_common() @safe {
		assert(m.ram); // init() must have been called already

		m.noise = 0x4000;
		m.echo_hist_pos = 0;
		m.every_other_sample = 1;
		m.echo_offset = 0;
		m.phase = 0;

		init_counter();
	}
}

version (SPC_NO_COPY_STATE_FUNCS) {
} else {

	struct SPC_State_Copier {
	nothrow:
		SPC_DSP.copy_func_t func;
		ubyte** buf;
	public:
		this(ubyte** p, SPC_DSP.copy_func_t f) @safe {
			func = f;
			buf = p;
		}

		final void copy(void[] state) @safe {
			func(buf, state);
		}

		final int copy_int(int state, int size) @safe {
			ubyte[2] s;
			assert(s.length >= size);
			(cast(ushort[]) s)[0] = cast(ushort) state;
			func(buf, s[0 .. size]);
			return (cast(ushort[]) s)[0];
		}

		final void skip(int count) @safe {
			if (count > 0) {
				char[64] temp = 0;
				do {
					int n = temp.sizeof;
					if (n > count)
						n = count;
					count -= n;
					func(buf, temp[0 .. n]);
				}
				while (count);
			}
		}

		final void extra() @safe {
			int n = 0;
			n = cast(ubyte) copy_int(n, ubyte.sizeof);
			assert(cast(ubyte) n == n);
			skip(n);
		}
	}
}
