// Core SPC emulation: CPU, timers, SMP registers, memory

// snes_spc 0.9.0. http://www.slack.net/~ant/

module spc700.spc.cpu;

import core.stdc.config;
import core.stdc.string;

import spc700.dsp.dsp;

version(unittest) {
	version = opcodeHook;
}

const int cpu_lag_max = 12 - 1; // DIV YA,X takes 12 clocks

enum SPC_MORE_ACCURACY = 0;

uint get_le16(scope const ubyte[] p) @safe nothrow {
	return cast(uint)p[1] << 8 | cast(uint)p[0];
}

immutable int bits_in_int = 8 * int.sizeof;

// If write isn't preceded by read, data has this added to it
immutable int no_read_before_write = 0x2000;

// Hex value in name to clarify code and bit shifting.
// Flag stored in indicated variable during emulation
immutable int n80 = 0x80; // nz
immutable int v40 = 0x40; // psw
immutable int p20 = 0x20; // dp
immutable int b10 = 0x10; // psw
immutable int h08 = 0x08; // psw
immutable int i04 = 0x04; // psw
immutable int z02 = 0x02; // nz
immutable int c01 = 0x01; // c

immutable int nz_neg_mask = 0x880; // either bit set indicates N flag set

immutable ubyte[256][3] glitch_probs = [
	[
		0xC3, 0x92, 0x5B, 0x1C, 0xD1, 0x92, 0x5B, 0x1C, 0xDB, 0x9C, 0x72, 0x18, 0xCD, 0x5C, 0x38, 0x0B,
		0xE1, 0x9C, 0x74, 0x17, 0xCF, 0x75, 0x45, 0x0C, 0xCF, 0x6E, 0x4A, 0x0D, 0xA3, 0x3A, 0x1D, 0x08,
		0xDB, 0xA0, 0x82, 0x19, 0xD9, 0x73, 0x3C, 0x0E, 0xCB, 0x76, 0x52, 0x0B, 0xA5, 0x46, 0x1D, 0x09,
		0xDA, 0x74, 0x55, 0x0F, 0xA2, 0x3F, 0x21, 0x05, 0x9A, 0x40, 0x20, 0x07, 0x63, 0x1E, 0x10, 0x01,
		0xDF, 0xA9, 0x85, 0x1D, 0xD3, 0x84, 0x4B, 0x0E, 0xCF, 0x6F, 0x49, 0x0F, 0xB3, 0x48, 0x1E, 0x05,
		0xD8, 0x77, 0x52, 0x12, 0xB7, 0x49, 0x23, 0x06, 0xAA, 0x45, 0x28, 0x07, 0x7D, 0x28, 0x0F, 0x07,
		0xCC, 0x7B, 0x4A, 0x0E, 0xB2, 0x4F, 0x24, 0x07, 0xAD, 0x43, 0x2C, 0x06, 0x86, 0x29, 0x11, 0x07,
		0xAE, 0x48, 0x1F, 0x0A, 0x76, 0x21, 0x19, 0x05, 0x76, 0x21, 0x14, 0x05, 0x44, 0x11, 0x0B, 0x01,
		0xE7, 0xAD, 0x96, 0x23, 0xDC, 0x86, 0x59, 0x0E, 0xDC, 0x7C, 0x5F, 0x15, 0xBB, 0x53, 0x2E, 0x09,
		0xD6, 0x7C, 0x4A, 0x16, 0xBB, 0x4A, 0x25, 0x08, 0xB3, 0x4F, 0x28, 0x0B, 0x8E, 0x23, 0x15, 0x08,
		0xCF, 0x7F, 0x57, 0x11, 0xB5, 0x4A, 0x23, 0x0A, 0xAA, 0x42, 0x28, 0x05, 0x7D, 0x22, 0x12, 0x03,
		0xA6, 0x49, 0x28, 0x09, 0x82, 0x2B, 0x0D, 0x04, 0x7A, 0x20, 0x0F, 0x04, 0x3D, 0x0F, 0x09, 0x03,
		0xD1, 0x7C, 0x4C, 0x0F, 0xAF, 0x4E, 0x21, 0x09, 0xA8, 0x46, 0x2A, 0x07, 0x85, 0x1F, 0x0E, 0x07,
		0xA6, 0x3F, 0x26, 0x07, 0x7C, 0x24, 0x14, 0x07, 0x78, 0x22, 0x16, 0x04, 0x46, 0x12, 0x0A, 0x02,
		0xA6, 0x41, 0x2C, 0x0A, 0x7E, 0x28, 0x11, 0x05, 0x73, 0x1B, 0x14, 0x05, 0x3D, 0x11, 0x0A, 0x02,
		0x70, 0x22, 0x17, 0x05, 0x48, 0x13, 0x08, 0x03, 0x3C, 0x07, 0x0D, 0x07, 0x26, 0x07, 0x06, 0x01
	],[
		0xE0, 0x9F, 0xDA, 0x7C, 0x4F, 0x18, 0x28, 0x0D, 0xE9, 0x9F, 0xDA, 0x7C, 0x4F, 0x18, 0x1F, 0x07,
		0xE6, 0x97, 0xD8, 0x72, 0x64, 0x13, 0x26, 0x09, 0xDC, 0x67, 0xA9, 0x38, 0x21, 0x07, 0x15, 0x06,
		0xE9, 0x91, 0xD2, 0x6B, 0x63, 0x14, 0x2B, 0x0E, 0xD6, 0x61, 0xB7, 0x41, 0x2B, 0x0E, 0x10, 0x09,
		0xCF, 0x59, 0xB0, 0x2F, 0x35, 0x08, 0x0F, 0x07, 0xB6, 0x30, 0x7A, 0x21, 0x17, 0x07, 0x09, 0x03,
		0xE7, 0xA3, 0xE5, 0x6B, 0x65, 0x1F, 0x34, 0x09, 0xD8, 0x6B, 0xBE, 0x45, 0x27, 0x07, 0x10, 0x07,
		0xDA, 0x54, 0xB1, 0x39, 0x2E, 0x0E, 0x17, 0x08, 0xA9, 0x3C, 0x86, 0x22, 0x16, 0x06, 0x07, 0x03,
		0xD4, 0x51, 0xBC, 0x3D, 0x38, 0x0A, 0x13, 0x06, 0xB2, 0x37, 0x79, 0x1C, 0x17, 0x05, 0x0E, 0x06,
		0xA7, 0x31, 0x74, 0x1C, 0x11, 0x06, 0x0C, 0x02, 0x6D, 0x1A, 0x38, 0x10, 0x0B, 0x05, 0x06, 0x03,
		0xEB, 0x9A, 0xE1, 0x7A, 0x6F, 0x13, 0x34, 0x0E, 0xE6, 0x75, 0xC5, 0x45, 0x3E, 0x0B, 0x1A, 0x05,
		0xD8, 0x63, 0xC1, 0x40, 0x3C, 0x1B, 0x19, 0x06, 0xB3, 0x42, 0x83, 0x29, 0x18, 0x0A, 0x08, 0x04,
		0xD4, 0x58, 0xBA, 0x43, 0x3F, 0x0A, 0x1F, 0x09, 0xB1, 0x33, 0x8A, 0x1F, 0x1F, 0x06, 0x0D, 0x05,
		0xAF, 0x3C, 0x7A, 0x1F, 0x16, 0x08, 0x0A, 0x01, 0x72, 0x1B, 0x52, 0x0D, 0x0B, 0x09, 0x06, 0x01,
		0xCF, 0x63, 0xB7, 0x47, 0x40, 0x10, 0x14, 0x06, 0xC0, 0x41, 0x96, 0x20, 0x1C, 0x09, 0x10, 0x05,
		0xA6, 0x35, 0x82, 0x1A, 0x20, 0x0C, 0x0E, 0x04, 0x80, 0x1F, 0x53, 0x0F, 0x0B, 0x02, 0x06, 0x01,
		0xA6, 0x31, 0x81, 0x1B, 0x1D, 0x01, 0x08, 0x08, 0x7B, 0x20, 0x4D, 0x19, 0x0E, 0x05, 0x07, 0x03,
		0x6B, 0x17, 0x49, 0x07, 0x0E, 0x03, 0x0A, 0x05, 0x37, 0x0B, 0x1F, 0x06, 0x04, 0x02, 0x07, 0x01
	],[
		0xF0, 0xD6, 0xED, 0xAD, 0xEC, 0xB1, 0xEB, 0x79, 0xAC, 0x22, 0x47, 0x1E, 0x6E, 0x1B, 0x32, 0x0A,
		0xF0, 0xD6, 0xEA, 0xA4, 0xED, 0xC4, 0xDE, 0x82, 0x98, 0x1F, 0x50, 0x13, 0x52, 0x15, 0x2A, 0x0A,
		0xF1, 0xD1, 0xEB, 0xA2, 0xEB, 0xB7, 0xD8, 0x69, 0xA2, 0x1F, 0x5B, 0x18, 0x55, 0x18, 0x2C, 0x0A,
		0xED, 0xB5, 0xDE, 0x7E, 0xE6, 0x85, 0xD3, 0x59, 0x59, 0x0F, 0x2C, 0x09, 0x24, 0x07, 0x15, 0x09,
		0xF1, 0xD6, 0xEA, 0xA0, 0xEC, 0xBB, 0xDA, 0x77, 0xA9, 0x23, 0x58, 0x14, 0x5D, 0x12, 0x2F, 0x09,
		0xF1, 0xC1, 0xE3, 0x86, 0xE4, 0x87, 0xD2, 0x4E, 0x68, 0x15, 0x26, 0x0B, 0x27, 0x09, 0x15, 0x02,
		0xEE, 0xA6, 0xE0, 0x5C, 0xE0, 0x77, 0xC3, 0x41, 0x67, 0x1B, 0x3C, 0x07, 0x2A, 0x06, 0x19, 0x07,
		0xE4, 0x75, 0xC6, 0x43, 0xCC, 0x50, 0x95, 0x23, 0x35, 0x09, 0x14, 0x04, 0x15, 0x05, 0x0B, 0x04,
		0xEE, 0xD6, 0xED, 0xAD, 0xEC, 0xB1, 0xEB, 0x79, 0xAC, 0x22, 0x56, 0x14, 0x5A, 0x12, 0x26, 0x0A,
		0xEE, 0xBB, 0xE7, 0x7E, 0xE9, 0x8D, 0xCB, 0x49, 0x67, 0x11, 0x34, 0x07, 0x2B, 0x0B, 0x14, 0x07,
		0xED, 0xA7, 0xE5, 0x76, 0xE3, 0x7E, 0xC4, 0x4B, 0x77, 0x14, 0x34, 0x08, 0x27, 0x07, 0x14, 0x04,
		0xE7, 0x8B, 0xD2, 0x4C, 0xCA, 0x56, 0x9E, 0x31, 0x36, 0x0C, 0x11, 0x07, 0x14, 0x04, 0x0A, 0x02,
		0xF0, 0x9B, 0xEA, 0x6F, 0xE5, 0x81, 0xC4, 0x43, 0x74, 0x10, 0x30, 0x0B, 0x2D, 0x08, 0x1B, 0x06,
		0xE6, 0x83, 0xCA, 0x48, 0xD9, 0x56, 0xA7, 0x23, 0x3B, 0x09, 0x12, 0x09, 0x15, 0x07, 0x0A, 0x03,
		0xE5, 0x5F, 0xCB, 0x3C, 0xCF, 0x48, 0x91, 0x22, 0x31, 0x0A, 0x17, 0x08, 0x15, 0x04, 0x0D, 0x02,
		0xD1, 0x43, 0x91, 0x20, 0xA9, 0x2D, 0x54, 0x12, 0x17, 0x07, 0x09, 0x02, 0x0C, 0x04, 0x05, 0x03
	]
];

immutable opcodeLengths = [
	1, 1, 2, 3, 2, 3, 1, 2, 2, 3, 3, 2, 3, 1, 3, 1,
	2, 1, 2, 3, 2, 3, 3, 2, 3, 1, 2, 2, 1, 1, 3, 3,
	1, 1, 2, 3, 2, 3, 1, 2, 2, 3, 3, 2, 3, 1, 3, 2,
	2, 1, 2, 3, 2, 3, 3, 2, 3, 1, 2, 2, 1, 1, 2, 3,
	1, 1, 2, 3, 2, 3, 1, 2, 2, 3, 3, 2, 3, 1, 3, 2,
	2, 1, 2, 3, 2, 3, 3, 2, 3, 1, 2, 2, 1, 1, 3, 3,
	1, 1, 2, 3, 2, 3, 1, 2, 2, 3, 3, 2, 3, 1, 3, 1,
	2, 1, 2, 3, 2, 3, 3, 2, 3, 1, 2, 2, 1, 1, 2, 1,
	1, 1, 2, 3, 2, 3, 1, 2, 2, 3, 3, 2, 3, 2, 1, 3,
	2, 1, 2, 3, 2, 3, 3, 2, 3, 1, 2, 2, 1, 1, 1, 1,
	1, 1, 2, 3, 2, 3, 1, 2, 2, 3, 3, 2, 3, 2, 1, 1,
	2, 1, 2, 3, 2, 3, 3, 2, 3, 1, 2, 2, 1, 1, 1, 1,
	1, 1, 2, 3, 2, 3, 1, 2, 2, 3, 3, 2, 3, 2, 1, 1,
	2, 1, 2, 3, 2, 3, 3, 2, 2, 2, 2, 2, 1, 1, 3, 1,
	1, 1, 2, 3, 2, 3, 1, 2, 2, 3, 3, 2, 3, 1, 1, 1,
	2, 1, 2, 3, 2, 3, 3, 2, 2, 2, 3, 2, 1, 1, 2, 1,
];
immutable mnemonics = [
	"NOP", "TCALL", "SET0", "BBS0", "ORZ", "OR", "OR", "OR", "OR", "OR", "OR1", "ASLZ", "ASL", "PUSH", "TSET1", "BRK",
	"BPL", "TCALL", "CLR0", "BBC0", "ORZ", "OR", "OR", "OR", "OR", "OR", "DECW", "ASL", "ASL", "DEC", "CMP", "JMP",
	"CLRP", "TCALL", "SET1", "BBS1", "ANDZ", "AND", "AND", "AND", "AND", "AND", "OR1", "ROLZ", "ROL", "PUSH", "CBNE", "BRA",
	"BMI", "TCALL", "CLR1", "BBC1", "ANDZ", "AND", "AND", "AND", "AND", "AND", "INCW", "ROL", "ROL", "INC", "CMP", "CALL",
	"SETP", "TCALL", "SET2", "BBS2", "EORZ", "EOR", "EOR", "EOR", "EOR", "EOR", "AND1", "LSRZ", "LSR", "PUSH", "TCLR1", "PCALL",
	"BVC", "TCALL", "CLR2", "BBC2", "EORZ", "EOR", "EOR", "EOR", "EOR", "EOR", "CLRW", "LSR", "LSR", "MOV", "CMP", "JMP",
	"CLRC", "TCALL", "SET3", "BBS3", "CMPZ", "CMP", "CMP", "CMP", "CMP", "CMP", "AND1", "ROR", "ROR", "PUSH", "DBNZ", "RET",
	"BVS", "TCALL", "CLR3", "BBC3", "CMPZ", "CMP", "CMP", "CMP", "CMP", "CMP", "ADDW", "ROR", "ROR", "MOV", "CMP", "RETI",
	"SETC", "TCALL", "SET4", "BBS4", "ADCZ", "ADC", "ADC", "ADC", "ADC", "ADC", "EOR1", "DECZ", "DEC", "MOV", "POP", "MOV",
	"BCC", "TCALL", "CLR4", "BBC4", "ADCZ", "ADC", "ADC", "ADC", "ADC", "MOV", "SUBW", "DEC", "DEC", "MOV", "DIV", "XCN",
	"EI", "TCALL", "SET5", "BBS5", "SBCZ", "SBC", "SBC", "SBC", "SBC", "SBC", "MOV1", "INCZ", "INC", "CMP", "POP", "MOV",
	"BCS", "TCALL", "CLR5", "BBC5", "SBCZ", "SBC", "SBC", "SBC", "SBC", "SBC", "MOVW", "INC", "INC", "MOV", "DAS", "MOV",
	"DI", "TCALL", "SET6", "BBS6", "MOV", "MOV", "MOV", "MOV", "CMP", "MOV", "MOV1", "MOV", "MOV", "MOV", "POP", "MUL",
	"BNE", "TCALL", "CLR6", "BBC6", "MOV", "MOV", "MOV", "MOV", "MOV", "MOV", "MOVW", "MOV", "DEC", "MOV", "CBNE", "DAA",
	"CLRV", "TCALL", "SET7", "BBS7", "MOV", "MOV", "MOV", "MOV", "MOV", "MOV", "NOT1", "MOV", "MOV", "NOTC", "POP", "SLEEP",
	"BEQ", "TCALL", "CLR7", "BBC7", "MOV", "MOV", "MOV", "MOV", "MOV", "MOV", "MOV", "MOV", "INC", "MOV", "DBNZ", "STOP",
];
struct CPUState {
	ushort pc;
	ushort sp;
	int a;
	int x;
	int y;
	ubyte psw;
}
struct SNES_SPC {
	version(opcodeHook) {
		void delegate(scope const ubyte[] op, CPUState state) @safe pure nothrow hook;
	}
nothrow:
public:
	// Must be called once before using
	const(char)* initialize() @safe {
		m = m.init;
		dsp.initialize(m.ram);

		version(opcodeHook) {
			if (!hook) {
				hook = (_, __) {};
			}
		}
		m.tempo = tempo_unit;

		// Most SPC music doesn't need ROM, and almost all the rest only rely
		// on these two bytes
		m.rom[0x3E] = 0xFF;
		m.rom[0x3F] = 0xC0;

		reset();
		return null;
	}

	// Sample pairs generated per second
	enum sample_rate = 32000;

	// Emulator use

	// Sets IPL ROM data. Library does not include ROM data. Most SPC music files
	// don't need ROM, but a full emulator must provide this.
	enum rom_size = 0x40;
	void init_rom(ref const(ubyte)[rom_size] rom) @safe {
		m.rom = rom;
	}

	// Sets destination for output samples
	alias sample_t = short;
	void set_output(sample_t[] out_) @safe {
		assert((out_.length & 1) == 0); // size must be even

		m.extra_clocks &= clocks_per_sample - 1;
		if (out_) {
			m.buf_begin = out_;

			// Copy extra to output
			const(sample_t)[] in_ = m.extra_buf;
			while (in_.ptr < &m.extra_buf[m.extra_pos] && out_.length > 0) {
				out_[0] = in_[0];
				out_ = out_[1 .. $];
				in_ = in_[1 .. $];
			}

			// Handle output being full already
			if (out_.length == 0) {
				// Have DSP write to remaining extra space
				out_ = dsp.extra();

				// Copy any remaining extra samples as if DSP wrote them
				while (in_.ptr < &m.extra_buf[m.extra_pos]) {
					out_[0] = in_[0];
					out_ = out_[1 .. $];
					in_ = in_[1 .. $];
				}
				assert(out_.length == 0);
			}

			dsp.set_output(out_);
		} else {
			reset_buf();
		}
	}

	// Number of samples written to output since last set
	int sample_count() const @safe {
		return (m.extra_clocks >> 5) * 2;
	}

	// Resets SPC to power-on state. This resets your output buffer, so you must
	// call set_output() after this.
	void reset() @safe {
		m.ram = state_t.init.ram;
		ram_loaded();
		reset_common(0x0F);
		dsp.reset();
	}

	// Emulates pressing reset switch on SNES. This resets your output buffer, so
	// you must call set_output() after this.
	void soft_reset() @safe {
		reset_common(0);
		dsp.soft_reset();
	}

	// 1024000 SPC clocks per second, sample pair every 32 clocks
	alias time_t = int;
	enum clock_rate = 1024000;
	enum clocks_per_sample = 32;

	// Emulated port read/write at specified time
	enum port_count = 4;
	int read_port(time_t t, int port) @safe {
		assert(cast(uint) port < port_count);
		run_until_(t);
		return m.smp_regs[0][r_cpuio0 + port];
	}
	int read_port_now(int port) @safe {
		assert(cast(uint) port < port_count);
		return m.smp_regs[0][r_cpuio0 + port];
	}

	void write_port(time_t t, int port, int data) @safe {
		assert(cast(uint) port < port_count);
		run_until_(t);
		m.smp_regs[1][r_cpuio0 + port] = cast(ubyte) data;
	}
	void write_port_now(int port, int data) @safe {
		assert(cast(uint) port < port_count);
		m.smp_regs[0][r_cpuio0 + port] = cast(ubyte) data;
		m.smp_regs[1][r_cpuio0 + port] = cast(ubyte) data;
	}

	// Runs SPC to end_time and starts a new time frame at 0
	void end_frame(time_t end_time) @safe {
		// Catch CPU up to as close to end as possible. If final instruction
		// would exceed end, does NOT execute it and leaves m.spc_time < end.
		if (end_time > m.spc_time)
			run_until_(end_time);

		m.spc_time -= end_time;
		m.extra_clocks += end_time;

		// Greatest number of clocks early that emulation can stop early due to
		// not being able to execute current instruction without going over
		// allowed time.
		assert(-cpu_lag_max <= m.spc_time && m.spc_time <= 0);

		// Catch timers up to CPU
		for (int i = 0; i < timer_count; i++)
			run_timer(&m.timers[i], 0);

		// Catch DSP up to CPU
		if (m.dsp_time < 0) {
			int count = (0) - m.dsp_time;
			if (!SPC_MORE_ACCURACY || count) {
				assert(count > 0);
				m.dsp_time = (0);
				dsp.run(count);
			}
		}

		// Save any extra samples beyond what should be generated
		if (m.buf_begin)
			save_extra();
	}

	// Sound control

	// Mutes voices corresponding to non-zero bits in mask (issues repeated KOFF events).
	// Reduces emulation accuracy.
	enum voice_count = 8;
	void mute_voices(int mask) @safe {
		dsp.mute_voices(mask);
	}

	// If true, prevents channels and global volumes from being phase-negated.
	// Only supported by fast DSP.
	void disable_surround(bool disable = true) @safe {
		dsp.disable_surround(disable);
	}

	// Sets tempo, where tempo_unit = normal, tempo_unit / 2 = half speed, etc.
	enum tempo_unit = 0x100;
	void set_tempo(int t) @safe {
		m.tempo = t;
		const int timer2_shift = 4; // 64 kHz
		const int other_shift = 3; //  8 kHz

		if (!t)
			t = 1;
		const int timer2_rate = 1 << timer2_shift;
		int rate = (timer2_rate * tempo_unit + (t >> 1)) / t;
		if (rate < timer2_rate / 4)
			rate = timer2_rate / 4; // max 4x tempo
		m.timers[2].prescaler = rate;
		m.timers[1].prescaler = rate << other_shift;
		m.timers[0].prescaler = rate << other_shift;
	}

	// SPC music files

	// Loads SPC data into emulator
	enum spc_min_file_size = 0x10180;
	enum spc_file_size = 0x10200;
	const(char)* load_spc(const(void)[] data) @safe {
		if (data.length < spc_min_file_size)
			return "Corrupt SPC file";
		const spc_file_t* spc = &(cast(const(spc_file_t)[])data[0 .. spc_file_t.sizeof])[0];

		// be sure compiler didn't insert any padding into fle_t
		assert(spc_file_t.sizeof == spc_min_file_size + 0x80);

		// Check signature and file size
		if (spc.signature != spcSignature)
			return "Not an SPC file";


		// CPU registers
		m.cpu_regs.pc = spc.pch * 0x100 + spc.pcl;
		m.cpu_regs.a = spc.a;
		m.cpu_regs.x = spc.x;
		m.cpu_regs.y = spc.y;
		m.cpu_regs.psw = spc.psw;
		m.cpu_regs.sp = spc.sp;

		// RAM and registers
		m.ram[] = spc.ram;
		ram_loaded();

		// DSP registers
		dsp.load(spc.dsp);

		reset_time_regs();

		return null;
	}
	void load_buffer(scope const(ubyte)[] data, ushort start) @safe {
		// CPU registers
		m.cpu_regs.pc = start;
		m.cpu_regs.a = 0;
		m.cpu_regs.x = 0;
		m.cpu_regs.y = 0;
		m.cpu_regs.psw = 0;
		m.cpu_regs.sp = 0xEF;

		// RAM and registers
		m.ram[0 .. data.length] = data;
		m.ram[2 .. 0x100] = 0;
		(cast(ushort[])m.ram[0 .. 2])[0] = start;
		m.ram[0xF2] = 0x2C;
		ram_loaded();

		// DSP registers
		dsp.reset();

		reset_time_regs();
	}

	// Clears echo region. Useful after loading an SPC as many have garbage in echo.
	void clear_echo() @safe {
		if (!(dsp.read(SPC_DSP.r_flg) & 0x20)) {
			int addr = 0x100 * dsp.read(SPC_DSP.r_esa);
			int end = addr + 0x800 * (dsp.read(SPC_DSP.r_edl) & 0x0F);
			if (end > 0x10000)
				end = 0x10000;
			m.ram[addr .. end] = 0xFF;
		}
	}

	// Plays for count samples and write samples to out. Discards samples if out
	// is NULL. Count must be a multiple of 2 since output is stereo.
	string play(sample_t[] out_) @safe {
		assert((out_.length & 1) == 0); // must be even
		if (out_) {
			set_output(out_);
			end_frame(cast(int)(out_.length * (clocks_per_sample / 2)));
		}

		string err = m.cpu_error;
		m.cpu_error = null;
		return err;
	}

	string play(int count) @safe {
		assert((count & 1) == 0); // must be even
		if (count) {
			m.extra_clocks &= clocks_per_sample - 1;
			reset_buf();
			end_frame(count * (clocks_per_sample / 2));
		}

		string err = m.cpu_error;
		m.cpu_error = null;
		return err;
	}

	// Skips count samples. Several times faster than play() when using fast DSP.
	string skip(int count) @safe {
		return play(count);
	}

	// State save/load (only available with accurate DSP)

	// Saves/loads state
	enum state_size = 67 * 1024L; // maximum space needed when saving
	alias copy_func_t = SPC_DSP.copy_func_t;
	void copy_state(ubyte** io, copy_func_t copy) @safe {
		auto copier = SPC_State_Copier(io, copy);
		auto SPC_COPY(T, U)(ref U state) {
			state = cast(T) copier.copy_int(state, T.sizeof);
			assert(cast(T) state == state);
		}

		// Make state data more readable by putting 64K RAM, 16 SMP registers,
		// then DSP (with its 128 registers) first

		// RAM
		enable_rom(0); // will get re-enabled if necessary in regs_loaded() below
		copier.copy(m.ram);

		{
			// SMP registers
			ubyte[port_count] out_ports;
			ubyte[reg_count] regs;
			out_ports = m.smp_regs[0][r_cpuio0 .. r_cpuio0 + out_ports.length];
			save_regs(regs);
			copier.copy(regs);
			copier.copy(out_ports);
			load_regs(regs);
			regs_loaded();
			m.smp_regs[0][r_cpuio0 .. r_cpuio0 + out_ports.length] = out_ports;
		}

		// CPU registers
		SPC_COPY!ushort(m.cpu_regs.pc);
		SPC_COPY!ubyte(m.cpu_regs.a);
		SPC_COPY!ubyte(m.cpu_regs.x);
		SPC_COPY!ubyte(m.cpu_regs.y);
		SPC_COPY!ubyte(m.cpu_regs.psw);
		SPC_COPY!ubyte(m.cpu_regs.sp);
		copier.extra();

		SPC_COPY!short(m.spc_time);
		SPC_COPY!short(m.dsp_time);

		// DSP
		dsp.copy_state(io, copy);

		// Timers
		for (int i = 0; i < timer_count; i++) {
			Timer* t = &m.timers[i];
			SPC_COPY!short(t.next_time);
			SPC_COPY!ubyte(t.divider);
			copier.extra();
		}
		copier.extra();
	}

	void saveFullSPC(ubyte[] output) @safe {
		init_header(output);
		save_spc(output);
	}

	// Writes minimal header to spc_out
	static void init_header(ubyte[] spc_out) @safe {
		with((cast(spc_file_t[])spc_out[0 .. spc_file_t.sizeof])[0]) {
			has_id666 = 26; // has none
			version_ = 30;
			signature[] = spcSignature;
			text[] = 0;
		}
	}

	// Saves emulator state as SPC file data. Writes spc_file_size bytes to spc_out.
	// Does not set up SPC header; use init_header() for that.
	void save_spc(ubyte[] spc_out) @safe {
		spc_file_t* spc = &(cast(spc_file_t[]) spc_out[0 .. spc_file_t.sizeof])[0];

		// CPU
		spc.pcl = cast(ubyte)(m.cpu_regs.pc >> 0);
		spc.pch = cast(ubyte)(m.cpu_regs.pc >> 8);
		spc.a = cast(ubyte) m.cpu_regs.a;
		spc.x = cast(ubyte) m.cpu_regs.x;
		spc.y = cast(ubyte) m.cpu_regs.y;
		spc.psw = cast(ubyte) m.cpu_regs.psw;
		spc.sp = cast(ubyte) m.cpu_regs.sp;

		// RAM, ROM
		spc.ram[] = m.ram;
		if (m.rom_enabled)
			spc.ram[rom_addr .. rom_addr + m.hi_ram.sizeof] = m.hi_ram;
		spc.unused[] = 0;
		spc.ipl_rom[] = m.rom;

		// SMP registers
		save_regs(spc.ram[0xF0 .. 0x100]);
		int i;
		for (i = 0; i < port_count; i++)
			spc.ram[0xF0 + r_cpuio0 + i] = m.smp_regs[1][r_cpuio0 + i];

		// DSP registers
		for (i = 0; i < SPC_DSP.register_count; i++)
			spc.dsp[i] = cast(ubyte) dsp.read(i);
	}

	// Returns true if new key-on events occurred since last check. Useful for
	// trimming silence while saving an SPC.
	bool check_kon() @safe {
		return dsp.check_kon();
	}

public:

	// Time relative to m_spc_time. Speeds up code a bit by eliminating need to
	// constantly add m_spc_time to time from CPU. CPU uses time that ends at
	// 0 to eliminate reloading end time every instruction. It pays off.
	alias rel_time_t = int;

	struct Timer {
		rel_time_t next_time; // time of next event
		int prescaler;
		int period;
		int divider;
		int enabled;
		int counter;
	};
	enum reg_count = 0x10;
	enum timer_count = 3;
	enum extra_size = SPC_DSP.extra_size;

	enum signature_size = 35;

private:
	SPC_DSP dsp;

	struct state_t {
		Timer[timer_count] timers;

		ubyte[reg_count][2] smp_regs;

		CPUState cpu_regs;

		rel_time_t dsp_time;
		time_t spc_time;
		bool echo_accessed;

		int tempo;
		int skipped_kon;
		int skipped_koff;
		string cpu_error;

		int extra_clocks;
		sample_t[] buf_begin;
		size_t extra_pos;
		sample_t[extra_size] extra_buf;

		int rom_enabled;
		ubyte[rom_size] rom;
		ubyte[rom_size] hi_ram;

		ubyte[256] cycle_table = [
			0x2, 0x8, 0x4, 0x7, 0x3, 0x4, 0x3, 0x6, 0x2, 0x6, 0x5, 0x4, 0x5, 0x4, 0x6, 0x8,
			0x4, 0x8, 0x4, 0x7, 0x4, 0x5, 0x5, 0x6, 0x5, 0x5, 0x6, 0x5, 0x2, 0x2, 0x4, 0x6,
			0x2, 0x8, 0x4, 0x7, 0x3, 0x4, 0x3, 0x6, 0x2, 0x6, 0x5, 0x4, 0x5, 0x4, 0x7, 0x4,
			0x4, 0x8, 0x4, 0x7, 0x4, 0x5, 0x5, 0x6, 0x5, 0x5, 0x6, 0x5, 0x2, 0x2, 0x3, 0x8,
			0x2, 0x8, 0x4, 0x7, 0x3, 0x4, 0x3, 0x6, 0x2, 0x6, 0x4, 0x4, 0x5, 0x4, 0x6, 0x6,
			0x4, 0x8, 0x4, 0x7, 0x4, 0x5, 0x5, 0x6, 0x5, 0x5, 0x4, 0x5, 0x2, 0x2, 0x4, 0x3,
			0x2, 0x8, 0x4, 0x7, 0x3, 0x4, 0x3, 0x6, 0x2, 0x6, 0x4, 0x4, 0x5, 0x4, 0x7, 0x5,
			0x4, 0x8, 0x4, 0x7, 0x4, 0x5, 0x5, 0x6, 0x5, 0x5, 0x5, 0x5, 0x2, 0x2, 0x3, 0x6,
			0x2, 0x8, 0x4, 0x7, 0x3, 0x4, 0x3, 0x6, 0x2, 0x6, 0x5, 0x4, 0x5, 0x2, 0x4, 0x5,
			0x4, 0x8, 0x4, 0x7, 0x4, 0x5, 0x5, 0x6, 0x5, 0x5, 0x5, 0x5, 0x2, 0x2, 0xC, 0x5,
			0x3, 0x8, 0x4, 0x7, 0x3, 0x4, 0x3, 0x6, 0x2, 0x6, 0x4, 0x4, 0x5, 0x2, 0x4, 0x4,
			0x4, 0x8, 0x4, 0x7, 0x4, 0x5, 0x5, 0x6, 0x5, 0x5, 0x5, 0x5, 0x2, 0x2, 0x3, 0x4,
			0x3, 0x8, 0x4, 0x7, 0x4, 0x5, 0x4, 0x7, 0x2, 0x5, 0x6, 0x4, 0x5, 0x2, 0x4, 0x9,
			0x4, 0x8, 0x4, 0x7, 0x5, 0x6, 0x6, 0x7, 0x4, 0x5, 0x5, 0x5, 0x2, 0x2, 0x8, 0x3,
			0x2, 0x8, 0x4, 0x7, 0x3, 0x4, 0x3, 0x6, 0x2, 0x4, 0x5, 0x3, 0x4, 0x3, 0x4, 0x0,
			0x4, 0x8, 0x4, 0x7, 0x4, 0x5, 0x5, 0x6, 0x3, 0x4, 0x5, 0x4, 0x2, 0x2, 0x6, 0x0,
		];

		ubyte[0x10000] ram;
	}

	state_t m;

	enum rom_addr = 0xFFC0;

	enum skipping_time = 127;

	// Value that padding should be filled with
	enum ubyte cpu_pad_fill = 0xFF;

	enum {
		r_test = 0x0,
		r_control = 0x1,
		r_dspaddr = 0x2,
		r_dspdata = 0x3,
		r_cpuio0 = 0x4,
		r_cpuio1 = 0x5,
		r_cpuio2 = 0x6,
		r_cpuio3 = 0x7,
		r_f8 = 0x8,
		r_f9 = 0x9,
		r_t0target = 0xA,
		r_t1target = 0xB,
		r_t2target = 0xC,
		r_t0out = 0xD,
		r_t1out = 0xE,
		r_t2out = 0xF
	};

	// Timer registers have been loaded. Applies these to the timers. Does not
	// reset timer prescalers or dividers.
	void timers_loaded() @safe {
		int i;
		for (i = 0; i < timer_count; i++) {
			Timer* t = &m.timers[i];
			t.period = (cast(ubyte)((m.smp_regs[0][r_t0target + i]) - 1) + 1);
			t.enabled = m.smp_regs[0][r_control] >> i & 1;
			t.counter = m.smp_regs[1][r_t0out + i] & 0x0F;
		}

		set_tempo(m.tempo);
	}

	void enable_rom(int enable) @safe {
		if (m.rom_enabled != enable) {
			m.rom_enabled = enable;
			if (enable)
				m.hi_ram[] = m.ram[rom_addr .. rom_addr + m.hi_ram.sizeof];
			m.ram[rom_addr .. rom_addr + rom_size] = (enable ? m.rom[0 .. rom_size] : m.hi_ram[0 .. rom_size]);
			// TODO: ROM can still get overwritten when DSP writes to echo buffer
		}
	}

	void reset_buf() @safe {
		// Start with half extra buffer of silence
		sample_t[] out_ = m.extra_buf[0 .. extra_size / 2];
		while (out_.length > 0) {
			out_[0] = 0;
			out_ = out_[1 .. $];
		}

		m.extra_pos = extra_size / 2;
		m.buf_begin = null;

		dsp.set_output(null);
	}

	void save_extra() @safe {
		// Copy any extra samples at these ends into extra_buf
		sample_t[] out_ = m.extra_buf[];
		foreach (sample; m.buf_begin[sample_count() .. $]) { // end of data written to buf
			out_[0] = sample;
			out_ = out_[1 .. $];
		}
		foreach (sample; dsp.extra()[0 .. $ - dsp.out_pos.length]) { // end of data written to dsp.extra()
			out_[0] = sample;
			out_ = out_[1 .. $];
		}

		m.extra_pos = &out_[0] - &m.extra_buf[0];
	}
	// Loads registers from unified 16-byte format
	//void load_regs( const(ubyte)[reg_count] in_ ) {
	void load_regs(in ubyte[16] in_) @safe {
		m.smp_regs[0] = in_;
		m.smp_regs[1] = m.smp_regs[0];

		// These always read back as 0
		m.smp_regs[1][r_test] = 0;
		m.smp_regs[1][r_control] = 0;
		m.smp_regs[1][r_t0target] = 0;
		m.smp_regs[1][r_t1target] = 0;
		m.smp_regs[1][r_t2target] = 0;
	}
	// RAM was just loaded from SPC, with $F0-$FF containing SMP registers
	// and timer counts. Copies these to proper registers.
	void ram_loaded() @safe {
		m.rom_enabled = 0;
		load_regs(m.ram[0xF0 .. 0x100]);
	}
	// Registers were just loaded. Applies these new values.
	void regs_loaded() @safe {
		enable_rom(m.smp_regs[0][r_control] & 0x80);
		timers_loaded();
	}

	void reset_time_regs() @safe {
		m.cpu_error = null;
		m.echo_accessed = 0;
		m.spc_time = 0;
		m.dsp_time = 0;

		for (int i = 0; i < timer_count; i++) {
			Timer* t = &m.timers[i];
			t.next_time = 1;
			t.divider = 0;
		}

		regs_loaded();

		m.extra_clocks = 0;
		reset_buf();
	}

	void reset_common(int timer_counter_init) @safe {
		int i;
		for (i = 0; i < timer_count; i++)
			m.smp_regs[1][r_t0out + i] = cast(ubyte) timer_counter_init;

		// Run IPL ROM
		m.cpu_regs = m.cpu_regs.init;
		m.cpu_regs.pc = rom_addr;

		m.smp_regs[0][r_test] = 0x0A;
		m.smp_regs[0][r_control] = 0xB0; // ROM enabled, clear ports
		for (i = 0; i < port_count; i++)
			m.smp_regs[1][r_cpuio0 + i] = 0;

		reset_time_regs();
	}

	Timer* run_timer(Timer* t, rel_time_t time) @safe {
		if (time >= t.next_time) {
			assert(t.prescaler, "Division by zero");
			int elapsed = ((time - t.next_time) / t.prescaler) + 1;
			t.next_time += elapsed * t.prescaler;

			if (t.enabled) {
				int remain = (cast(ubyte)((t.period - t.divider) - 1) + 1);
				int divider = t.divider + elapsed;
				int over = elapsed - remain;
				if (over >= 0) {
					int n = over / t.period;
					t.counter = (t.counter + 1 + n) & 0x0F;
					divider = over - n * t.period;
				}
				t.divider = cast(ubyte) divider;
			}
		}
		return t;
	}

	int dsp_read(rel_time_t time) @safe {
		{
			int count = (time) - m.dsp_time;
			if (count) {
				assert(count > 0);
				m.dsp_time = (time);
				dsp.run(count);
			}
		}

		int result = dsp.read(m.smp_regs[0][r_dspaddr] & 0x7F);

		return result;
	}

	void dsp_write(int data, rel_time_t time) @safe {
		int count = (time) - m.dsp_time;
		if (count) {
			assert(count > 0);
			m.dsp_time = (time);
			dsp.run(count);
		}
		static if (is(SPC_DSP_WRITE_HOOK)) {
			SPC_DSP_WRITE_HOOK(m.spc_time + time, m.smp_regs[0][r_dspaddr], cast(ubyte) data);
		}

		if (m.smp_regs[0][r_dspaddr] <= 0x7F)
			dsp.write(m.smp_regs[0][r_dspaddr], data);
	}

	void cpu_write_smp_reg_(int data, rel_time_t time, int addr) @safe {
		switch (addr) {
		case r_t0target:
		case r_t1target:
		case r_t2target: {
				Timer* t = &m.timers[addr - r_t0target];
				int period = (cast(ubyte)((data) - 1) + 1);
				if (t.period != period) {
					t = run_timer(t, time);
					version (SPC_MORE_ACCURACY) {
						// Insane behavior when target is written just after counter is
						// clocked and counter matches new period and new period isn't 1, 2, 4, or 8
						if (t.divider == (period & 0xFF) && t.next_time == time + TIMER_MUL(t, 1) && ((period - 1) | ~0x0F) & period) {
							//dprintf( "SPC pathological timer target write\n" );

							// If the period is 3, 5, or 9, there's a probability this behavior won't occur,
							// based on the previous period
							int prob = 0xFF;
							int old_period = t.period & 0xFF;
							if (period == 3)
								prob = glitch_probs[0][old_period];
							if (period == 5)
								prob = glitch_probs[1][old_period];
							if (period == 9)
								prob = glitch_probs[2][old_period];

							// The glitch suppresses incrementing of one of the counter bits, based on
							// the lowest set bit in the new period
							int b = 1;
							while (!(period & b))
								b <<= 1;

							if ((rand() >> 4 & 0xFF) <= prob)
								t.divider = (t.divider - b) & 0xFF;
						}
					}
					t.period = period;
				}
				break;
			}

		case r_t0out:
		case r_t1out:
		case r_t2out:
			//if ( !SPC_MORE_ACCURACY )
			//	dprintf( "SPC wrote to counter %d\n", cast(int) addr - r_t0out );

			if (data < no_read_before_write / 2)
				run_timer(&m.timers[addr - r_t0out], time - 1).counter = 0;
			break;

			// Registers that act like RAM
		case 0x8:
		case 0x9:
			m.smp_regs[1][addr] = cast(ubyte) data;
			break;

		case r_test:
			//if ( cast(ubyte) data != 0x0A )
			//	dprintf( "SPC wrote to test register\n" );
			break;

		case r_control:
			// port clears
			if (data & 0x10) {
				m.smp_regs[1][r_cpuio0] = 0;
				m.smp_regs[1][r_cpuio1] = 0;
			}
			if (data & 0x20) {
				m.smp_regs[1][r_cpuio2] = 0;
				m.smp_regs[1][r_cpuio3] = 0;
			}

			// timers
			{
				for (int i = 0; i < timer_count; i++) {
					Timer* t = &m.timers[i];
					int enabled = data >> i & 1;
					if (t.enabled != enabled) {
						t = run_timer(t, time);
						t.enabled = enabled;
						if (enabled) {
							t.divider = 0;
							t.counter = 0;
						}
					}
				}
			}
			enable_rom(data & 0x80);
			break;
		default:
			break;
		}
	}

	void cpu_write_smp_reg(int data, rel_time_t time, int addr) @safe {
		if (addr == r_dspdata) // 99%
			dsp_write(data, time);
		else
			cpu_write_smp_reg_(data, time, addr);
	}

	void cpu_write_high(int data, int i, rel_time_t time) @safe {
		if (i < rom_size) {
			m.hi_ram[i] = cast(ubyte) data;
			if (m.rom_enabled)
				m.ram[i + rom_addr] = m.rom[i]; // restore overwritten ROM
		} else {
			assert(m.ram[i + rom_addr] == cast(ubyte) data);
			m.ram[i + rom_addr] = cpu_pad_fill; // restore overwritten padding
			cpu_write(data, i + rom_addr - 0x10000, time);
		}
	}

	void cpu_write(int data, int addr, rel_time_t time) @safe {
		version (SPC_MORE_ACCURACY) {
			if (time >= m.dsp_time) {
				int count = (time) - m.dsp_time;
				if (count) {
					assert(count > 0);
					m.dsp_time = (time);
					dsp.run(count);
				}
			}
		} else {
			auto a = !check_echo_access(cast(ubyte) addr);
			assert(a);
		}

		// RAM
		m.ram[addr] = cast(ubyte) data;
		int reg = addr - 0xF0;
		if (reg >= 0) // 64%
		{
			// $F0-$FF
			if (reg < reg_count) // 87%
			{
				m.smp_regs[0][reg] = cast(ubyte) data;

				// Ports
				static if (is(SPC_PORT_WRITE_HOOK)) {
					if (cast(uint)(reg - r_cpuio0) < port_count)
						SPC_PORT_WRITE_HOOK(m.spc_time + time, (reg - r_cpuio0), cast(ubyte) data, &m.smp_regs[0][r_cpuio0]);
				}

				// Registers other than $F2 and $F4-$F7
				//if ( reg != 2 && reg != 4 && reg != 5 && reg != 6 && reg != 7 )
				// TODO: this is a bit on the fragile side
				if (((~0x2F00 << (bits_in_int - 16)) << reg) < 0) // 36%
					cpu_write_smp_reg(data, time, reg);
			}  // High mem/address wrap-around
			else {
				reg -= rom_addr - 0xF0;
				if (reg >= 0) // 1% in IPL ROM area or address wrapped around
					cpu_write_high(data, reg, time);
			}
		}
	}

	int cpu_read_smp_reg(int reg, rel_time_t time) @safe {
		int result = m.smp_regs[1][reg];
		reg -= r_dspaddr;
		// DSP addr and data
		if (cast(uint) reg <= 1) // 4% 0xF2 and 0xF3
		{
			result = m.smp_regs[0][r_dspaddr];
			if (cast(uint) reg == 1)
				result = dsp_read(time); // 0xF3
		}
		return result;
	}

	int cpu_read(int addr, rel_time_t time) @safe {
		version (SPC_MORE_ACCURACY) {
			if (time >= m.dsp_time) {
				int count = (time) - m.dsp_time;
				if (count) {
					assert(count > 0);
					m.dsp_time = (time);
					dsp.run(count);
				}
			}
		} else {
			auto a = !check_echo_access(cast(ubyte) addr);
			assert(a);
		}

		// RAM
		int result = m.ram[addr];
		int reg = addr - 0xF0;
		if (reg >= 0) // 40%
		{
			reg -= 0x10;
			if (cast(uint) reg >= 0xFF00) // 21%
			{
				reg += 0x10 - r_t0out;

				// Timers
				if (cast(uint) reg < timer_count) // 90%
				{
					Timer* t = run_timer(&m.timers[reg], time);
					result = t.counter;
					t.counter = 0;
				}  // Other registers
				else if (reg < 0) // 10%
				{
					result = cpu_read_smp_reg(reg + r_t0out, time);
				} else // 1%
				{
					assert(reg + (r_t0out + 0xF0 - 0x10000) < 0x100);
					result = cpu_read(reg + (r_t0out + 0xF0 - 0x10000), time);
				}
			}
		}

		return result;
	}

	uint CPU_mem_bit(scope const(ubyte)[] pc, rel_time_t rel_time) @safe {
		uint addr = get_le16(pc);
		uint t = cpu_read((addr & 0x1FFF), rel_time + (0)) >> (addr >> 13);
		return t << 8 & 0x100;
	}

	bool check_echo_access(int addr) @safe {
		if (!(dsp.read(SPC_DSP.r_flg) & 0x20)) {
			int start = 0x100 * dsp.read(SPC_DSP.r_esa);
			int size = 0x800 * (dsp.read(SPC_DSP.r_edl) & 0x0F);
			int end = start + (size ? size : 4);
			if (start <= addr && addr < end) {
				if (!m.echo_accessed) {
					m.echo_accessed = 1;
					return true;
				}
			}
		}
		return false;
	}

	ubyte[] run_until_(time_t end_time) return nothrow @safe {
		rel_time_t rel_time = m.spc_time - end_time;
		assert(rel_time <= 0);
		m.spc_time = end_time;
		m.dsp_time += rel_time;
		m.timers[0].next_time += rel_time;
		m.timers[1].next_time += rel_time;
		m.timers[2].next_time += rel_time;

		{
			ubyte[] ram = m.ram;
			CPUState cpuState;
			cpuState.a = m.cpu_regs.a;
			cpuState.x = m.cpu_regs.x;
			cpuState.y = m.cpu_regs.y;
			cpuState.psw = cast(ubyte)m.cpu_regs.psw;
			int c;
			int nz;
			int dp;

			cpuState.pc = cast(ushort)m.cpu_regs.pc;
			cpuState.sp = cast(ushort)(0x101 + m.cpu_regs.sp);
			{
				c = m.cpu_regs.psw << 8;
				dp = m.cpu_regs.psw << 3 & 0x100;
				nz = (m.cpu_regs.psw << 4 & 0x800) | (~m.cpu_regs.psw & z02);
			}

			goto loop;

			// Main loop

		cbranch_taken_loop:
			cpuState.pc += cast(byte)ram[cpuState.pc];
		inc_pc_loop:
			cpuState.pc++;
		loop: while (true) {
				uint opcode;
				uint data;

				assert(cast(uint) cpuState.a < 0x100);
				assert(cast(uint) cpuState.x < 0x100);
				assert(cast(uint) cpuState.y < 0x100);

				opcode = ram[cpuState.pc];
				if ((rel_time += m.cycle_table[opcode]) > 0)
					break;

				version(opcodeHook) {
					hook(ram[cpuState.pc .. cpuState.pc + opcodeLengths[opcode]], cpuState);
				}
				/*
		//SUB_CASE_COUNTER( 1 );
		#define PROFILE_TIMER_LOOP( op, addr, len )\
		if ( opcode == op )\
		{\
			int cond = (uint) ((addr) - 0xFD) < 3 &&\
					pc [len] == 0xF0 && pc [len+1] == 0xFE - len;\
			SUB_CASE_COUNTER( op && cond );\
		}

		PROFILE_TIMER_LOOP( 0xEC, (*cast(ushort*) (pc + 1)), 3 );
		PROFILE_TIMER_LOOP( 0xEB, pc [1], 2 );
		PROFILE_TIMER_LOOP( 0xE4, pc [1], 2 );
		*/

				// TODO: if PC is at end of memory, this will get wrong operand (very obscure)
				++cpuState.pc;
				data = ram[cpuState.pc];
				uint addr;
				int addr2;
				int temp;
				switch (opcode) {

					// Common instructions

				case 0xF0: // BEQ
				{
						cpuState.pc++;
						cpuState.pc += cast(byte) data;
						if (!cast(ubyte) nz)
							break;
						cpuState.pc -= cast(byte) data;
						rel_time -= 2;
						break;
					}
					// BRANCH( !(ubyte) nz ) // 89% taken

				case 0xD0: // BNE
				{
						cpuState.pc++;
						cpuState.pc += cast(byte) data;
						if (cast(ubyte) nz)
							break;
						cpuState.pc -= cast(byte) data;
						rel_time -= 2;
						break;
					}
					// BRANCH( (ubyte) nz )

				case 0x3F: { // CALL
						int old_addr = cast(int)(cpuState.pc + 2);
						cpuState.pc = (cast(ushort[])(ram[cpuState.pc .. cpuState.pc + 2]))[0];
						{ // PUSH16( old_addr );
							addr2 = cast(int)((cpuState.sp -= 2));
							if (addr2 > 0x100) {
								(cast(ushort[])(ram[cpuState.sp .. cpuState.sp + 2]))[0] = cast(ushort) old_addr;
							} else {
								ram[cast(ubyte) addr2 + 0x100] = cast(ubyte) old_addr;
								ram[cpuState.sp + 1] = cast(ubyte)(old_addr >> 8);
								cpuState.sp += 0x100;
							}
						}
						break;
					}

				case 0x6F: // RET
					addr2 = cast(int)(cpuState.sp);
					cpuState.pc = (cast(ushort[])(ram[cpuState.sp .. cpuState.sp + 2]))[0];
					cpuState.sp += 2;
					if (addr2 < 0x1FF)
						break;

					(cpuState.pc = (ram[cpuState.sp -0x101] * 0x100 + ram[cast(ubyte) addr2 + 0x100]));
					cpuState.sp -= 0x100;
					break;

				case 0xE4: // MOV a,dp
					++cpuState.pc;
					// 80% from timer
					{
						cpuState.a = nz = cpu_read(dp + data, rel_time);
					}
					break;

				case 0xFA: { // MOV dp,dp
					{
							temp = cpu_read(dp + data, rel_time - 2);
						}
						data = temp + no_read_before_write;
					}
					goto case;
				case 0x8F: { // MOV dp,#imm
						temp = ram[cpuState.pc + 1];
						cpuState.pc += 2;

						version (SPC_MORE_ACCURACY) {
							cpu_write(data, dp + temp, rel_time);
						} else {
							int i = dp + temp;
							ram[i] = cast(ubyte) data;
							i -= 0xF0;
							if (cast(uint) i < 0x10) // 76%
							{
								m.smp_regs[0][i] = cast(ubyte) data;

								// Registers other than $F2 and $F4-$F7
								//if ( i != 2 && i != 4 && i != 5 && i != 6 && i != 7 )
								if (((~0x2F00 << (bits_in_int - 16)) << i) < 0) // 12%
									cpu_write_smp_reg(data, rel_time, i);
							}
						}
						break;
					}

				case 0xC4: // MOV dp,a
					++cpuState.pc;
					version (SPC_MORE_ACCURACY) {
						cpu_write(a, dp + data, rel_time);
					} else {
						int i = dp + data;
						ram[i] = cast(ubyte) cpuState.a;
						i -= 0xF0;
						if (cast(uint) i < 0x10) // 39%
						{
							uint sel = i - 2;
							m.smp_regs[0][i] = cast(ubyte) cpuState.a;

							if (sel == 1) // 51% $F3
								dsp_write(cpuState.a, rel_time);
							else if (sel > 1) // 1% not $F2 or $F3
								cpu_write_smp_reg_(cpuState.a, rel_time, i);
						}
					}
					break;

					// 1. 8-bit Data Transmission Commands. Group I

				case 0xE8 - 0x02: /* (X) */
					data = cpuState.x + dp;
					cpuState.a = nz = cpu_read(data, rel_time);
					break;
				case 0xE8 + 0x0F: /* (dp)+Y */
					data = (cast(ushort[])((ram[data + dp .. data + dp + 2])))[0] + cpuState.y;
					goto end_0xE8;
				case 0xE8 - 0x01: /* (dp+X) */
					const offset = cast(ubyte)(data + cpuState.x) + dp;
					data = (cast(ushort[])((ram[offset .. offset + 2])))[0];
					goto end_0xE8;
				case 0xE8 + 0x0E: /* abs+Y */
					data += cpuState.y;
					goto case 0xE8 - 0x03;
				case 0xE8 + 0x0D: /* abs+X */
					data += cpuState.x;
					goto case 0xE8 - 0x03;
				case 0xE8 - 0x03: /* abs */
				abs_0xE8:
					++cpuState.pc;
					data += 0x100 * ram[cpuState.pc];
					goto end_0xE8;
				case 0xE8 + 0x0C: /* dp+X */
					data = cast(ubyte)(data + cpuState.x);
					data += dp;
				end_0xE8: // MOV A,addr
					cpuState.a = nz = cpu_read(data, rel_time);
					cpuState.pc++;
					break;

				case 0xBF: { // MOV A,(X)+
						temp = cpuState.x + dp;
						cpuState.x = cast(ubyte)(cpuState.x + 1);
						cpuState.a = nz = cpu_read(temp, rel_time - 1);
						break;
					}

				case 0xE8: // MOV A,imm
					cpuState.a = data;
					nz = data;
					cpuState.pc++;
					break;

				case 0xF9: // MOV X,dp+Y
					data = cast(ubyte)(data + cpuState.y);
					goto case;
				case 0xF8: // MOV X,dp
				{
						cpuState.x = nz = cpu_read(dp + data, rel_time);
					}
					cpuState.pc++;
					break;

				case 0xE9: // MOV X,abs
					data = (cast(ushort[])(ram[cpuState.pc .. cpuState.pc + 2]))[0];
					++cpuState.pc;
					data = cpu_read(data, rel_time);
					goto case;
				case 0xCD: // MOV X,imm
					cpuState.x = data;
					nz = data;
					goto inc_pc_loop;

				case 0xFB: // MOV Y,dp+X
					data = cast(ubyte)(data + cpuState.x);
					goto case;
				case 0xEB: // MOV Y,dp
					// 70% from timer
					cpuState.pc++;
					{
						cpuState.y = nz = cpu_read(dp + data, rel_time);
					}
					break;

				case 0xEC: { // MOV Y,abs
						temp = (cast(ushort[])(ram[cpuState.pc .. cpuState.pc + 2]))[0];
						cpuState.pc += 2;
						{
							cpuState.y = nz = cpu_read(temp, rel_time);
						}
						break;
					}

				case 0x8D: // MOV Y,imm
					cpuState.y = data;
					nz = data;
					goto inc_pc_loop;

					// 2. 8-BIT DATA TRANSMISSION COMMANDS, GROUP 2

				case 0xC8 - 0x02: /* (X) */
					data = cpuState.x + dp;
					cpuState.pc--;
					goto end_0xC8;
				case 0xC8 + 0x0F: /* (dp)+Y */
					data = (cast(ushort[])((ram[data + dp .. data + dp + 2])))[0] + cpuState.y;
					goto end_0xC8;
				case 0xC8 - 0x01: /* (dp+X) */
					const offset = cast(ubyte)(data + cpuState.x) + dp;
					data = (cast(ushort[])((ram[offset .. offset + 2])))[0];
					goto end_0xC8;
				case 0xC8 + 0x0E: /* abs+Y */
					data += cpuState.y;
					goto abs_0xC8;
				case 0xC8 + 0x0D: /* abs+X */
					data += cpuState.x;
					goto case;
				case 0xC8 - 0x03: /* abs */
				abs_0xC8:
					++cpuState.pc;
					data += 0x100 * ram[cpuState.pc];
					goto end_0xC8;
				case 0xC8 + 0x0C: /* dp+X */
					data = cast(ubyte)(data + cpuState.x);
					data += dp;
				end_0xC8: // MOV addr,A
					cpu_write(cpuState.a, data, rel_time);
					goto inc_pc_loop;

					{
						// int temp;
				case 0xCC: // MOV abs,Y
						temp = cpuState.y;
						goto mov_abs_temp;
				case 0xC9: // MOV abs,X
						temp = cpuState.x;
				mov_abs_temp:
						cpu_write(temp, (cast(ushort[])(ram[cpuState.pc .. cpuState.pc + 2]))[0], rel_time);
						cpuState.pc += 2;
						break;
					}

				case 0xD9: // MOV dp+Y,X
					data = cast(ubyte)(data + cpuState.y);
					goto case;
				case 0xD8: // MOV dp,X
					cpu_write(cpuState.x, data + dp, rel_time);
					goto inc_pc_loop;

				case 0xDB: // MOV dp+X,Y
					data = cast(ubyte)(data + cpuState.x);
					goto case;
				case 0xCB: // MOV dp,Y
					cpu_write(cpuState.y, data + dp, rel_time);
					goto inc_pc_loop;

					// 3. 8-BIT DATA TRANSMISSIN COMMANDS, GROUP 3.

				case 0x7D: // MOV A,X
					cpuState.a = cpuState.x;
					nz = cpuState.x;
					break;

				case 0xDD: // MOV A,Y
					cpuState.a = cpuState.y;
					nz = cpuState.y;
					break;

				case 0x5D: // MOV X,A
					cpuState.x = cpuState.a;
					nz = cpuState.a;
					break;

				case 0xFD: // MOV Y,A
					cpuState.y = cpuState.a;
					nz = cpuState.a;
					break;

				case 0x9D: // MOV X,SP
					cpuState.x = nz = cast(int)(cpuState.sp - 0x101);
					break;

				case 0xBD: // MOV SP,X
					cpuState.sp = cast(ushort)(0x101 + cpuState.x);
					break;

					//case 0xC6: // MOV (X),A (handled by MOV addr,A in group 2)

				case 0xAF: // MOV (X)+,A
					cpu_write(cpuState.a + no_read_before_write, dp + cpuState.x, rel_time);
					cpuState.x++;
					break;

					// 5. 8-BIT LOGIC OPERATION COMMANDS

				case 0x28 - 0x02: /* (X) */
					data = cpuState.x + dp;
					cpuState.pc--;
					goto end_0x28;
				case 0x28 + 0x0F: /* (dp)+Y */
					data = (cast(ushort[])((ram[data + dp .. data + dp + 2])))[0] + cpuState.y;
					goto end_0x28;
				case 0x28 - 0x01: /* (dp+X) */
					const offset = cast(ubyte)(data + cpuState.x) + dp;
					data = (cast(ushort[])((ram[offset .. offset + 2])))[0];
					goto end_0x28;
				case 0x28 + 0x0E: /* abs+Y */
					data += cpuState.y;
					goto abs_0x28;
				case 0x28 + 0x0D: /* abs+X */
					data += cpuState.x;
					goto case;
				case 0x28 - 0x03: /* abs */
				abs_0x28:
					++cpuState.pc;
					data += 0x100 * ram[cpuState.pc];
					goto end_0x28;
				case 0x28 + 0x0C: /* dp+X */
					data = cast(ubyte)(data + cpuState.x);
					goto case;
				case 0x28 - 0x04: /* dp */
					data += dp;
				end_0x28: /* addr */
					data = cpu_read(data, rel_time);
					goto case;
				case 0x28: /* imm */
					nz = cpuState.a &= data;
					goto inc_pc_loop;
					{ //uint addr;
				case 0x28 + 0x11: /* X,Y */
						data = cpu_read(dp + cpuState.y, rel_time - 2);
						addr = cpuState.x + dp;
						goto addr_0x28;
				case 0x28 + 0x01: /* dp,dp */
						data = cpu_read(dp + data, rel_time - 3);
						goto case;
				case 0x28 + 0x10: { /*dp,imm*/
							const(ubyte) addr3 = ram[cpuState.pc + 1];
							cpuState.pc += 2;
							addr = addr3 + dp;
						}
				addr_0x28:
						nz = data & cpu_read(addr, rel_time - 1);
						cpu_write(nz, addr, rel_time);
						break;
					}

					// LOGICAL_OP( 0x28, & ); // AND

				case 0x08 - 0x02: /* (X) */
					data = cpuState.x + dp;
					cpuState.pc--;
					goto end_0x08;
				case 0x08 + 0x0F: /* (dp)+Y */
					data = (cast(ushort[])((ram[data + dp .. data + dp + 2])))[0] + cpuState.y;
					goto end_0x08;
				case 0x08 - 0x01: /* (dp+X) */
					const offset = cast(ubyte)(data + cpuState.x) + dp;
					data = (cast(ushort[])((ram[offset .. offset + 2])))[0];
					goto end_0x08;
				case 0x08 + 0x0E: /* abs+Y */
					data += cpuState.y;
					goto abs_0x08;
				case 0x08 + 0x0D: /* abs+X */
					data += cpuState.x;
					goto case;
				case 0x08 - 0x03: /* abs */
				abs_0x08:
					++cpuState.pc;
					data += 0x100 * ram[cpuState.pc];
					goto end_0x08;
				case 0x08 + 0x0C: /* dp+X */
					data = cast(ubyte)(data + cpuState.x);
					goto case;
				case 0x08 - 0x04: /* dp */
					data += dp;
				end_0x08: /* addr */
					data = cpu_read(data, rel_time);
					goto case;
				case 0x08: /* imm */
					nz = cpuState.a |= data;
					goto inc_pc_loop;
					{
				case 0x08 + 0x11: /* X,Y */
						data = cpu_read(dp + cpuState.y, rel_time - 2);
						addr = cpuState.x + dp;
						goto addr_0x08;
				case 0x08 + 0x01: /* dp,dp */
						data = cpu_read(dp + data, rel_time - 3);
						goto case;
				case 0x08 + 0x10: { /*dp,imm*/
							const(ubyte) addr3 = ram[cpuState.pc + 1];
							cpuState.pc += 2;
							addr = addr3 + dp;
						}
				addr_0x08:
						nz = data | cpu_read(addr, rel_time - 1);
						cpu_write(nz, addr, rel_time);
						break;
					}
					// LOGICAL_OP( 0x08, | ); // OR

				case 0x48 - 0x02: /* (X) */
					data = cpuState.x + dp;
					cpuState.pc--;
					goto end_0x48;
				case 0x48 + 0x0F: /* (dp)+Y */
					data = (cast(ushort[])((ram[data + dp .. data + dp + 2])))[0] + cpuState.y;
					goto end_0x48;
				case 0x48 - 0x01: /* (dp+X) */
					const offset = cast(ubyte)(data + cpuState.x) + dp;
					data = (cast(ushort[])((ram[offset .. offset + 2])))[0];
					goto end_0x48;
				case 0x48 + 0x0E: /* abs+Y */
					data += cpuState.y;
					goto abs_0x48;
				case 0x48 + 0x0D: /* abs+X */
					data += cpuState.x;
					goto case;
				case 0x48 - 0x03: /* abs */
				abs_0x48:
					++cpuState.pc;
					data += 0x100 * ram[cpuState.pc];
					goto end_0x48;
				case 0x48 + 0x0C: /* dp+X */
					data = cast(ubyte)(data + cpuState.x);
					goto case;
				case 0x48 - 0x04: /* dp */
					data += dp;
				end_0x48: /* addr */
					data = cpu_read(data, rel_time);
					goto case;
				case 0x48: /* imm */
					nz = cpuState.a ^= data;
					goto inc_pc_loop;
					{
				case 0x48 + 0x11: /* X,Y */
						data = cpu_read(dp + cpuState.y, rel_time - 2);
						addr = cpuState.x + dp;
						goto addr_0x48;
				case 0x48 + 0x01: /* dp,dp */
						data = cpu_read(dp + data, rel_time - 3);
						goto case;
				case 0x48 + 0x10: { /*dp,imm*/
							const(ubyte) addr3 = ram[cpuState.pc + 1];
							cpuState.pc += 2;
							addr = addr3 + dp;
						}
				addr_0x48:
						nz = data ^ cpu_read(addr, rel_time - 1);
						cpu_write(nz, addr, rel_time);
						break;
					}
					// LOGICAL_OP( 0x48, ^ ); // EOR

					// 4. 8-BIT ARITHMETIC OPERATION COMMANDS

				case 0x68 - 0x02: /* (X) */
					data = cpuState.x + dp;
					cpuState.pc--;
					goto end_0x68;
				case 0x68 + 0x0F: /* (dp)+Y */
					data = (cast(ushort[])((ram[data + dp .. data + dp + 2])))[0] + cpuState.y;
					goto end_0x68;
				case 0x68 - 0x01: /* (dp+X) */
					const offset = cast(ubyte)(data + cpuState.x) + dp;
					data = (cast(ushort[])((ram[offset .. offset + 2])))[0];
					goto end_0x68;
				case 0x68 + 0x0E: /* abs+Y */
					data += cpuState.y;
					goto abs_0x68;
				case 0x68 + 0x0D: /* abs+X */
					data += cpuState.x;
					goto case;
				case 0x68 - 0x03: /* abs */
				abs_0x68:
					++cpuState.pc;
					data += 0x100 * ram[cpuState.pc];
					goto end_0x68;
				case 0x68 + 0x0C: /* dp+X */
					data = cast(ubyte)(data + cpuState.x);
					goto case;
				case 0x68 - 0x04: /* dp */
					data += dp;
				end_0x68: // CMP addr
					data = cpu_read(data, rel_time);
					goto case;
				case 0x68: // CMP imm
					nz = cpuState.a - data;
					c = ~nz;
					nz &= 0xFF;
					goto inc_pc_loop;

				case 0x79: // CMP (X),(Y)
					data = cpu_read(dp + cpuState.y, rel_time - 2);
					nz = cpu_read(dp + cpuState.x, rel_time - 1) - data;
					c = ~nz;
					nz &= 0xFF;
					break;

				case 0x69: // CMP dp,dp
					data = cpu_read(dp + data, rel_time - 3);
					goto case;
				case 0x78: // CMP dp,imm
					++cpuState.pc;
					nz = cpu_read(dp + ram[cpuState.pc], rel_time - 1) - data;
					c = ~nz;
					nz &= 0xFF;
					goto inc_pc_loop;

				case 0x3E: // CMP X,dp
					data += dp;
					goto cmp_x_addr;
				case 0x1E: // CMP X,abs
					data = (cast(ushort[])(ram[cpuState.pc .. cpuState.pc + 2]))[0];
					cpuState.pc++;
				cmp_x_addr:
					data = cpu_read(data, rel_time);
					goto case;
				case 0xC8: // CMP X,imm
					nz = cpuState.x - data;
					c = ~nz;
					nz &= 0xFF;
					goto inc_pc_loop;

				case 0x7E: // CMP Y,dp
					data += dp;
					goto cmp_y_addr;
				case 0x5E: // CMP Y,abs
					data = (cast(ushort[])(ram[cpuState.pc .. cpuState.pc + 2]))[0];
					cpuState.pc++;
				cmp_y_addr:
					data = cpu_read(data, rel_time);
					goto case;
				case 0xAD: // CMP Y,imm
					nz = cpuState.y - data;
					c = ~nz;
					nz &= 0xFF;
					goto inc_pc_loop;

					{
				case 0xB9: // SBC (x),(y)
				case 0x99: // ADC (x),(y)
						cpuState.pc--; // compensate for inc later
						data = cpu_read(dp + cpuState.y, rel_time - 2);
						addr2 = cpuState.x + dp;
						goto adc_addr;
				case 0xA9: // SBC dp,dp
				case 0x89: // ADC dp,dp
						data = cpu_read(dp + data, rel_time - 3);
						goto case;
				case 0xB8: // SBC dp,imm
				case 0x98: // ADC dp,imm
						++cpuState.pc;
						addr2 = ram[cpuState.pc] + dp;
				adc_addr:
						nz = cpu_read(addr2, rel_time - 1);
						goto adc_data;

						// catch ADC and SBC together, then decode later based on operand
				case 0x88 - 0x02:
				case (0x88 - 0x02) + 0x20: /* (X) */
						data = cpuState.x + dp;
						cpuState.pc--;
						goto end_0x88;
				case 0x88 + 0x0F:
				case (0x88 + 0x0F) + 0x20: /* (dp)+Y */
					data = (cast(ushort[])((ram[data + dp .. data + dp + 2])))[0] + cpuState.y;
						goto end_0x88;
				case 0x88 - 0x01:
				case (0x88 - 0x01) + 0x20: /* (dp+X) */
					const offset = cast(ubyte)(data + cpuState.x) + dp;
					data = (cast(ushort[])((ram[offset .. offset + 2])))[0];
						goto end_0x88;
				case 0x88 + 0x0E:
				case (0x88 + 0x0E) + 0x20: /* abs+Y */
						data += cpuState.y;
						goto abs_0x88;
				case 0x88 + 0x0D:
				case (0x88 + 0x0D) + 0x20: /* abs+X */
						data += cpuState.x;
						goto case;
				case 0x88 - 0x03:
				case (0x88 - 0x03) + 0x20: /* abs */
				abs_0x88:
						++cpuState.pc;
						data += 0x100 * ram[cpuState.pc];
						goto end_0x88;
				case 0x88 + 0x0C:
				case (0x88 + 0x0C) + 0x20: /* dp+X */
						data = cast(ubyte)(data + cpuState.x);
						goto case;
				case 0x88 - 0x04:
				case (0x88 - 0x04) + 0x20: /* dp */
						data += dp;
				end_0x88: // ADC/SBC addr
						data = cpu_read(data, rel_time);
						goto case;
				case 0xA8: // SBC imm
				case 0x88: // ADC imm
						addr2 = -1; // A
						nz = cpuState.a;
				adc_data: {
							int flags;
							if (opcode >= 0xA0) // SBC
								data ^= 0xFF;

							flags = data ^ nz;
							nz += data + (c >> 8 & 1);
							flags ^= nz;

							cpuState.psw = (cpuState.psw & ~(v40 | h08)) | (flags >> 1 & h08) | ((flags + 0x80) >> 2 & v40);
							c = nz;
							if (addr2 < 0) {
								cpuState.a = cast(ubyte) nz;
								goto inc_pc_loop;
							}
							cpu_write(nz, addr2, rel_time);
							goto inc_pc_loop;
						}

					}

					// 6. ADDITION & SUBTRACTION COMMANDS

				case 0xBC:
					nz = cpuState.a + 1;
					cpuState.a = cast(ubyte) nz;
					break;
					// INC_DEC_REG( a, + 1 ) // INC A
				case 0x3D:
					nz = cpuState.x + 1;
					cpuState.x = cast(ubyte) nz;
					break;
					// INC_DEC_REG( x, + 1 ) // INC X
				case 0xFC:
					nz = cpuState.y + 1;
					cpuState.y = cast(ubyte) nz;
					break;
					// INC_DEC_REG( y, + 1 ) // INC Y

				case 0x9C:
					nz = cpuState.a - 1;
					cpuState.a = cast(ubyte) nz;
					break;
					// INC_DEC_REG( a, - 1 ) // DEC A
				case 0x1D:
					nz = cpuState.x - 1;
					cpuState.x = cast(ubyte) nz;
					break;
					// INC_DEC_REG( x, - 1 ) // DEC X
				case 0xDC:
					nz = cpuState.y - 1;
					cpuState.y = cast(ubyte) nz;
					break;
					// INC_DEC_REG( y, - 1 ) // DEC Y

				case 0x9B: // DEC dp+X
				case 0xBB: // INC dp+X
					data = cast(ubyte)(data + cpuState.x);
					goto case;
				case 0x8B: // DEC dp
				case 0xAB: // INC dp
					data += dp;
					goto inc_abs;
				case 0x8C: // DEC abs
				case 0xAC: // INC abs
					data = (cast(ushort[])(ram[cpuState.pc .. cpuState.pc + 2]))[0];
					cpuState.pc++;
				inc_abs:
					nz = (opcode >> 4 & 2) - 1;
					nz += cpu_read(data, rel_time - 1);
					cpu_write((nz), (data), rel_time);
					goto inc_pc_loop;

					// 7. SHIFT, ROTATION COMMANDS

				case 0x5C: // LSR A
					c = 0;
					goto case;
				case 0x7C: { // ROR A
						nz = (c >> 1 & 0x80) | (cpuState.a >> 1);
						c = cpuState.a << 8;
						cpuState.a = nz;
						break;
					}

				case 0x1C: // ASL A
					c = 0;
					goto case;
				case 0x3C: { // ROL A
						temp = c >> 8 & 1;
						c = cpuState.a << 1;
						nz = c | temp;
						cpuState.a = cast(ubyte) nz;
						break;
					}

				case 0x0B: // ASL dp
					c = 0;
					data += dp;
					goto rol_mem;
				case 0x1B: // ASL dp+X
					c = 0;
					goto case;
				case 0x3B: // ROL dp+X
					data = cast(ubyte)(data + cpuState.x);
					goto case;
				case 0x2B: // ROL dp
					data += dp;
					goto rol_mem;
				case 0x0C: // ASL abs
					c = 0;
					goto case;
				case 0x2C: // ROL abs
					data = (cast(ushort[])(ram[cpuState.pc .. cpuState.pc + 2]))[0];
					cpuState.pc++;
				rol_mem:
					nz = c >> 8 & 1;
					nz |= (c = cpu_read(data, rel_time - 1) << 1);
					cpu_write((nz), (data), rel_time);
					goto inc_pc_loop;

				case 0x4B: // LSR dp
					c = 0;
					data += dp;
					goto ror_mem;
				case 0x5B: // LSR dp+X
					c = 0;
					goto case;
				case 0x7B: // ROR dp+X
					data = cast(ubyte)(data + cpuState.x);
					goto case;
				case 0x6B: // ROR dp
					data += dp;
					goto ror_mem;
				case 0x4C: // LSR abs
					c = 0;
					goto case;
				case 0x6C: // ROR abs
					data = (cast(ushort[])(ram[cpuState.pc .. cpuState.pc + 2]))[0];
					cpuState.pc++;
				ror_mem: {
						temp = cpu_read(data, rel_time - 1);
						nz = (c >> 1 & 0x80) | (temp >> 1);
						c = temp << 8;
						cpu_write(nz, data, rel_time);
						goto inc_pc_loop;
					}

				case 0x9F: // XCN
					nz = cpuState.a = (cpuState.a >> 4) | cast(ubyte)(cpuState.a << 4);
					break;

					// 8. 16-BIT TRANSMISION COMMANDS

				case 0xBA: // MOVW YA,dp
					cpuState.a = cpu_read(dp + data, rel_time - 2);
					nz = (cpuState.a & 0x7F) | (cpuState.a >> 1);
					cpuState.y = cpu_read(dp + cast(ubyte)(data + 1), rel_time);
					nz |= cpuState.y;
					goto inc_pc_loop;

				case 0xDA: // MOVW dp,YA
					cpu_write(cpuState.a, dp + data, rel_time - 1);
					cpu_write(cpuState.y + no_read_before_write, cast(ubyte)(data + 1), rel_time);
					goto inc_pc_loop;

					// 9. 16-BIT OPERATION COMMANDS

				case 0x3A: // INCW dp
				case 0x1A: { // DECW dp
						// low byte
						data += dp;
						temp = cpu_read(data, rel_time - 3);
						temp += (opcode >> 4 & 2) - 1; // +1 for INCW, -1 for DECW
						nz = ((temp >> 1) | temp) & 0x7F;
						cpu_write(temp, data, rel_time - 2);

						// high byte
						data = cast(ubyte)(data + 1) + dp;
						temp = cast(ubyte)((temp >> 8) + cpu_read(data, rel_time - 1));
						nz |= temp;
						cpu_write(temp, data, rel_time);

						goto inc_pc_loop;
					}

				case 0x7A: // ADDW YA,dp
				case 0x9A: { // SUBW YA,dp
						int lo = cpu_read(dp + data, rel_time - 2);
						int hi = cpu_read(dp + cast(ubyte)(data + 1), rel_time);
						int result;
						int flags;

						if (opcode == 0x9A) // SUBW
						{
							lo = (lo ^ 0xFF) + 1;
							hi ^= 0xFF;
						}

						lo += cpuState.a;
						result = cpuState.y + hi + (lo >> 8);
						flags = hi ^ cpuState.y ^ result;

						cpuState.psw = (cpuState.psw & ~(v40 | h08)) | (flags >> 1 & h08) | ((flags + 0x80) >> 2 & v40);
						c = result;
						cpuState.a = cast(ubyte) lo;
						result = cast(ubyte) result;
						cpuState.y = result;
						nz = (((lo >> 1) | lo) & 0x7F) | result;

						goto inc_pc_loop;
					}

				case 0x5A: { // CMPW YA,dp
						temp = cpuState.a - cpu_read(dp + data, rel_time - 1);
						nz = ((temp >> 1) | temp) & 0x7F;
						temp = cpuState.y + (temp >> 8);
						temp -= cpu_read(dp + cast(ubyte)(data + 1), rel_time);
						nz |= temp;
						c = ~temp;
						nz &= 0xFF;
						goto inc_pc_loop;
					}

					// 10. MULTIPLICATION & DIVISON COMMANDS

				case 0xCF: { // MUL YA
						uint temp2 = cpuState.y * cpuState.a;
						cpuState.a = cast(ubyte) temp2;
						nz = ((temp2 >> 1) | temp2) & 0x7F;
						cpuState.y = temp2 >> 8;
						nz |= cpuState.y;
						break;
					}

				case 0x9E: // DIV YA,X
				{
						uint ya = cpuState.y * 0x100 + cpuState.a;

						cpuState.psw &= ~(h08 | v40);

						if (cpuState.y >= cpuState.x)
							cpuState.psw |= v40;

						if ((cpuState.y & 15) >= (cpuState.x & 15))
							cpuState.psw |= h08;

						if (cpuState.y < cpuState.x * 2) {
							cpuState.a = ya / cpuState.x;
							cpuState.y = ya - cpuState.a * cpuState.x;
						} else {
							cpuState.a = 255 - (ya - cpuState.x * 0x200) / (256 - cpuState.x);
							cpuState.y = cpuState.x + (ya - cpuState.x * 0x200) % (256 - cpuState.x);
						}

						nz = cast(ubyte) cpuState.a;
						cpuState.a = cast(ubyte) cpuState.a;

						break;
					}

					// 11. DECIMAL COMPENSATION COMMANDS

				case 0xDF: // DAA
					dprintf("SPC: suspicious opcode: DAA\n");
					if (cpuState.a > 0x99 || c & 0x100) {
						cpuState.a += 0x60;
						c = 0x100;
					}

					if ((cpuState.a & 0x0F) > 9 || cpuState.psw & h08)
						cpuState.a += 0x06;

					nz = cpuState.a;
					cpuState.a = cast(ubyte) cpuState.a;
					break;

				case 0xBE: // DAS
					dprintf("SPC: suspicious opcode: DAS\n");
					if (cpuState.a > 0x99 || !(c & 0x100)) {
						cpuState.a -= 0x60;
						c = 0;
					}

					if ((cpuState.a & 0x0F) > 9 || !(cpuState.psw & h08))
						cpuState.a -= 0x06;

					nz = cpuState.a;
					cpuState.a = cast(ubyte) cpuState.a;
					break;

					// 12. BRANCHING COMMANDS

				case 0x2F: // BRA rel
					cpuState.pc += cast(byte) data;
					goto inc_pc_loop;

				case 0x30: // BMI
				{
						cpuState.pc++;
						cpuState.pc += cast(byte) data;
						if (nz & nz_neg_mask)
							break;
						cpuState.pc -= cast(byte) data;
						rel_time -= 2;
						break;
					}
					// BRANCH( (nz & nz_neg_mask) )

				case 0x10: // BPL
				{
						cpuState.pc++;
						cpuState.pc += cast(byte) data;
						if (!(nz & nz_neg_mask))
							break;
						cpuState.pc -= cast(byte) data;
						rel_time -= 2;
						break;
					}
					// BRANCH( !(nz & nz_neg_mask) )

				case 0xB0: // BCS
				{
						cpuState.pc++;
						cpuState.pc += cast(byte) data;
						if (c & 0x100)
							break;
						cpuState.pc -= cast(byte) data;
						rel_time -= 2;
						break;
					}
					// BRANCH( c & 0x100 )

				case 0x90: // BCC
				{
						cpuState.pc++;
						cpuState.pc += cast(byte) data;
						if (!(c & 0x100))
							break;
						cpuState.pc -= cast(byte) data;
						rel_time -= 2;
						break;
					}
					// BRANCH( !(c & 0x100) )

				case 0x70: // BVS
				{
						cpuState.pc++;
						cpuState.pc += cast(byte) data;
						if (cpuState.psw & v40)
							break;
						cpuState.pc -= cast(byte) data;
						rel_time -= 2;
						break;
					}
					// BRANCH( psw & v40 )

				case 0x50: // BVC
				{
						cpuState.pc++;
						cpuState.pc += cast(byte) data;
						if (!(cpuState.psw & v40))
							break;
						cpuState.pc -= cast(byte) data;
						rel_time -= 2;
						break;
					}
					// BRANCH( !(psw & v40) )

				case 0x03: // BBS dp.bit,rel
				case 0x23:
				case 0x43:
				case 0x63:
				case 0x83:
				case 0xA3:
				case 0xC3:
				case 0xE3: {
						cpuState.pc++;
						if (cpu_read(dp + data, rel_time - 4) >> (opcode >> 5) & 1)
							goto cbranch_taken_loop;
						rel_time -= 2;
						goto inc_pc_loop;
					}
					// CBRANCH( cpu_read ( dp + data, rel_time - 4 ) >> (opcode >> 5) & 1 )

				case 0x13: // BBC dp.bit,rel
				case 0x33:
				case 0x53:
				case 0x73:
				case 0x93:
				case 0xB3:
				case 0xD3:
				case 0xF3: {
						cpuState.pc++;
						if (!(cpu_read(dp + data, rel_time - 4) >> (opcode >> 5) & 1))
							goto cbranch_taken_loop;
						rel_time -= 2;
						goto inc_pc_loop;
					}
					// CBRANCH( !(cpu_read ( dp + data, rel_time - 4 ) >> (opcode >> 5) & 1) )

				case 0xDE: // CBNE dp+X,rel
					data = cast(ubyte)(data + cpuState.x);
					goto case;
				case 0x2E: { // CBNE dp,rel
						// int temp;
						// 61% from timer
						{
							temp = cpu_read(dp + data, rel_time - 4);
						}
						{
							cpuState.pc++;
							if (temp != cpuState.a)
								goto cbranch_taken_loop;
							rel_time -= 2;
							goto inc_pc_loop;
						}
						// CBRANCH( temp != a )
					}

				case 0x6E: { // DBNZ dp,rel
						uint temp2 = cpu_read(dp + data, rel_time - 4) - 1;
						cpu_write(temp2 + no_read_before_write, dp + cast(ubyte) data, rel_time - 3);
						{
							cpuState.pc++;
							if (temp2)
								goto cbranch_taken_loop;
							rel_time -= 2;
							goto inc_pc_loop;
						}
						// CBRANCH( temp2 )
					}

				case 0xFE: // DBNZ Y,rel
					cpuState.y = cast(ubyte)(cpuState.y - 1);
					{
						cpuState.pc++;
						cpuState.pc += cast(byte) data;
						if (cpuState.y)
							break;
						cpuState.pc -= cast(byte) data;
						rel_time -= 2;
						break;
					}
					// BRANCH( y )

				case 0x1F: // JMP [abs+X]
					cpuState.pc = cast(ushort)((cast(const(ushort)[])(ram[cpuState.pc .. cpuState.pc + 2]))[0] + cpuState.x);
					goto case;
				case 0x5F: // JMP abs
					cpuState.pc = ((cast(const(ushort)[])(ram[cpuState.pc .. cpuState.pc + 2]))[0]);
					break;

					// 13. SUB-ROUTINE CALL RETURN COMMANDS

				case 0x0F: { // BRK
						// int temp;
						int ret_addr = cast(int)(cpuState.pc);
						dprintf("SPC: suspicious opcode: BRK\n");
						cpuState.pc = (cast(ushort[])(ram[0xFFDE .. 0xFFE0]))[0]; // vector address verified
						{ // PUSH16( ret_addr );
							cpuState.sp -= 2;
							addr2 = cast(int)(cpuState.sp);
							if (addr2 > 0x100) {
								(cast(ushort[])(ram[cpuState.sp .. cpuState.sp + 2]))[0] = cast(ushort) ret_addr;
							} else {
								ram[cast(ubyte) addr2 + 0x100] = cast(ubyte) ret_addr;
								ram[cpuState.sp + 1] = cast(ubyte)(ret_addr >> 8);
								cpuState.sp += 0x100;
							}
						}

						{
							temp = cpuState.psw & ~(n80 | p20 | z02 | c01);
							temp |= c >> 8 & c01;
							temp |= dp >> 3 & p20;
							temp |= ((nz >> 4) | nz) & n80;
							if (!cast(ubyte) nz)
								temp |= z02;
						}
						cpuState.psw = (cpuState.psw | b10) & ~i04;
						{ // PUSH( temp );
							--cpuState.sp;
							ram[cpuState.sp] = cast(ubyte)(temp);
							if (cpuState.sp == 0x100)
								cpuState.sp += 0x100;
						}
						break;
					}

				case 0x4F: { // PCALL offset
						int ret_addr = cast(int)(cpuState.pc + 1);
						(cpuState.pc = cast(ushort)(0xFF00 | data));
						{ //PUSH16(ret_addr)
							addr2 = cast(int)((cpuState.sp -= 2));
							if (addr2 > 0x100) {
								(cast(ushort[])(ram[cpuState.sp .. cpuState.sp + 2]))[0] = cast(ushort) ret_addr;
							} else {
								ram[cast(ubyte) addr2 + 0x100] = cast(ubyte) ret_addr;
								ram[cpuState.sp + 1] = cast(ubyte)(ret_addr >> 8);
								cpuState.sp += 0x100;
							}
						}
						break;
					}

				case 0x01: // TCALL n
				case 0x11:
				case 0x21:
				case 0x31:
				case 0x41:
				case 0x51:
				case 0x61:
				case 0x71:
				case 0x81:
				case 0x91:
				case 0xA1:
				case 0xB1:
				case 0xC1:
				case 0xD1:
				case 0xE1:
				case 0xF1: {
						int ret_addr = cast(int)(cpuState.pc);
						cpuState.pc = (cast(ushort[])(ram[0xFFDE - (opcode >> 3) .. 0xFFE0 - (opcode >> 3)]))[0];
						{ //PUSH16( ret_addr );
							addr2 = cast(int)((cpuState.sp -= 2));
							if (addr2 > 0x100) {
								(cast(ushort[])(ram[cpuState.sp .. cpuState.sp + 2]))[0] = cast(ushort) ret_addr;
							} else {
								ram[cast(ubyte) addr2 + 0x100] = cast(ubyte) ret_addr;
								ram[cpuState.sp + 1] = cast(ubyte)(ret_addr >> 8);
								cpuState.sp += 0x100;
							}
						}
						break;
					}

					// 14. STACK OPERATION COMMANDS

					{
						// int temp;
				case 0x7F: // RET1
						temp = ram[cpuState.sp];
						cpuState.pc = (cast(ushort[])(ram[cpuState.sp + 1 .. cpuState.sp + 3]))[0];
						cpuState.sp += 3;
						goto set_psw;
				case 0x8E: // POP PSW
				{ // POP( temp );
							temp = ram[cpuState.sp];
							cpuState.sp++;
							if (cpuState.sp == 0x201) {
								temp = ram[cpuState.sp -0x101];
								cpuState.sp -= 0x100;
							}
						}
				set_psw: {
							cpuState.psw = cast(ubyte)temp;
							c = temp << 8;
							dp = temp << 3 & 0x100;
							nz = (temp << 4 & 0x800) | (~temp & z02);
						}
						break;
					}

				case 0x0D: { // PUSH PSW
						// int temp;
						{
							temp = cpuState.psw & ~(n80 | p20 | z02 | c01);
							temp |= c >> 8 & c01;
							temp |= dp >> 3 & p20;
							temp |= ((nz >> 4) | nz) & n80;
							if (!cast(ubyte) nz)
								temp |= z02;
						}
						{ // PUSH( temp );
							--cpuState.sp;
							ram[cpuState.sp] = cast(ubyte)(temp);
							if (cpuState.sp == 0x100)
								cpuState.sp += 0x100;
						}
						break;
					}

				case 0x2D: // PUSH A
				{ // PUSH( a );
						--cpuState.sp;
						ram[cpuState.sp] = cast(ubyte)(cpuState.a);
						if (cpuState.sp == 0x100)
							cpuState.sp += 0x100;
					}
					break;

				case 0x4D: // PUSH X
				{ // PUSH( x );
						--cpuState.sp;
						ram[cpuState.sp] = cast(ubyte)(cpuState.x);
						if (cpuState.sp == 0x100)
							cpuState.sp += 0x100;
					}
					break;

				case 0x6D: // PUSH Y
				{ // PUSH( y );
						--cpuState.sp;
						ram[cpuState.sp] = cast(ubyte)(cpuState.y);
						if (cpuState.sp == 0x100)
							cpuState.sp += 0x100;
					}
					break;

				case 0xAE: // POP A
				{ // POP( a );
						cpuState.a = ram[cpuState.sp];
						cpuState.sp++;
						if (cpuState.sp == 0x201) {
							cpuState.a = ram[cpuState.sp -0x101];
							cpuState.sp -= 0x100;
						}
					}
					break;

				case 0xCE: // POP X
				{ // POP( x );
						cpuState.x = ram[cpuState.sp];
						cpuState.sp++;
						if (cpuState.sp == 0x201) {
							cpuState.x = ram[cpuState.sp -0x101];
							cpuState.sp -= 0x100;
						}
					}
					break;

				case 0xEE: // POP Y
				{ // POP( y );
						cpuState.y = ram[cpuState.sp];
						cpuState.sp++;
						if (cpuState.sp == 0x201) {
							cpuState.y = ram[cpuState.sp -0x101];
							cpuState.sp -= 0x100;
						}
					}
					break;

					// 15. BIT OPERATION COMMANDS

				case 0x02: // SET1
				case 0x22:
				case 0x42:
				case 0x62:
				case 0x82:
				case 0xA2:
				case 0xC2:
				case 0xE2:
				case 0x12: // CLR1
				case 0x32:
				case 0x52:
				case 0x72:
				case 0x92:
				case 0xB2:
				case 0xD2:
				case 0xF2: {
						int bit = 1 << (opcode >> 5);
						int mask = ~bit;
						if (opcode & 0x10)
							bit = 0;
						data += dp;
						cpu_write(cpu_read(data, rel_time - 1) & mask | bit, data, rel_time);
						goto inc_pc_loop;
					}

				case 0x0E: // TSET1 abs
				case 0x4E: // TCLR1 abs
					data = (cast(ushort[])(ram[cpuState.pc .. cpuState.pc + 2]))[0];
					cpuState.pc += 2;
					{
						uint temp2 = cpu_read(data, rel_time - 2);
						nz = cast(ubyte)(cpuState.a - temp2);
						temp2 &= ~cpuState.a;
						if (opcode == 0x0E)
							temp2 |= cpuState.a;
						cpu_write(temp2, data, rel_time);
					}
					break;

				case 0x4A: // AND1 C,mem.bit
					c &= CPU_mem_bit(ram[cpuState.pc .. cpuState.pc + 2], rel_time + 0);
					cpuState.pc += 2;
					break;

				case 0x6A: // AND1 C,/mem.bit
					c &= ~CPU_mem_bit(ram[cpuState.pc .. cpuState.pc + 2], rel_time + 0);
					cpuState.pc += 2;
					break;

				case 0x0A: // OR1 C,mem.bit
					c |= CPU_mem_bit(ram[cpuState.pc .. cpuState.pc + 2], rel_time - 1);
					cpuState.pc += 2;
					break;

				case 0x2A: // OR1 C,/mem.bit
					c |= ~CPU_mem_bit(ram[cpuState.pc .. cpuState.pc + 2], rel_time - 1);
					cpuState.pc += 2;
					break;

				case 0x8A: // EOR1 C,mem.bit
					c ^= CPU_mem_bit(ram[cpuState.pc .. cpuState.pc + 2], rel_time - 1);
					cpuState.pc += 2;
					break;

				case 0xEA: // NOT1 mem.bit
					data = (cast(ushort[])(ram[cpuState.pc .. cpuState.pc + 2]))[0];
					cpuState.pc += 2;
					{
						uint temp2 = cpu_read(data & 0x1FFF, rel_time - 1);
						temp2 ^= 1 << (data >> 13);
						cpu_write(temp2, data & 0x1FFF, rel_time);
					}
					break;

				case 0xCA: // MOV1 mem.bit,C
					data = (cast(ushort[])(ram[cpuState.pc .. cpuState.pc + 2]))[0];
					cpuState.pc += 2;
					{
						uint temp2 = cpu_read(data & 0x1FFF, rel_time - 2);
						uint bit = data >> 13;
						temp2 = (temp2 & ~(1 << bit)) | ((c >> 8 & 1) << bit);
						cpu_write(temp2 + no_read_before_write, data & 0x1FFF, rel_time);
					}
					break;

				case 0xAA: // MOV1 C,mem.bit
					c = CPU_mem_bit(ram[cpuState.pc .. cpuState.pc + 2], rel_time);
					cpuState.pc += 2;
					break;

					// 16. PROGRAM PSW FLAG OPERATION COMMANDS

				case 0x60: // CLRC
					c = 0;
					break;

				case 0x80: // SETC
					c = ~0;
					break;

				case 0xED: // NOTC
					c ^= 0x100;
					break;

				case 0xE0: // CLRV
					cpuState.psw &= ~(v40 | h08);
					break;

				case 0x20: // CLRP
					dp = 0;
					break;

				case 0x40: // SETP
					dp = 0x100;
					break;

				case 0xA0: // EI
					dprintf("SPC: suspicious opcode: EI\n");
					cpuState.psw |= i04;
					break;

				case 0xC0: // DI
					dprintf("SPC: suspicious opcode: DI\n");
					cpuState.psw &= ~i04;
					break;

					// 17. OTHER COMMANDS

				case 0x00: // NOP
					break;

				case 0xFF: { // STOP
						// handle PC wrap-around
						addr = cast(uint)(cpuState.pc - 1);
						if (addr >= 0x10000) {
							addr &= 0xFFFF;
							cpuState.pc = cast(ushort)addr;
							dprintf("SPC: PC wrapped around\n");
							break;
						}
					}
					goto case;
				case 0xEF: // SLEEP
					dprintf("SPC: suspicious opcode: STOP/SLEEP\n");
					--cpuState.pc;
					rel_time = 0;
					m.cpu_error = "SPC emulation error";
					goto stop;
				default:
					assert(0); // catch any unhandled instructions
				}
			}
			rel_time -= m.cycle_table[ram[cpuState.pc]]; // undo partial execution of opcode
		stop:

			// Uncache registers
			if (cpuState.pc >= 0x10000)
				dprintf("SPC: PC wrapped around\n");
			m.cpu_regs.pc = cast(ushort)(cpuState.pc);
			m.cpu_regs.sp = cast(ubyte)(cpuState.sp - 0x101);
			m.cpu_regs.a = cast(ubyte) cpuState.a;
			m.cpu_regs.x = cast(ubyte) cpuState.x;
			m.cpu_regs.y = cast(ubyte) cpuState.y;
			{
				int temp;
				{
					temp = cpuState.psw & ~(n80 | p20 | z02 | c01);
					temp |= c >> 8 & c01;
					temp |= dp >> 3 & p20;
					temp |= ((nz >> 4) | nz) & n80;
					if (!cast(ubyte) nz)
						temp |= z02;
				}
				m.cpu_regs.psw = cast(ubyte) temp;
			}
		}
		m.spc_time += rel_time;
		m.dsp_time -= rel_time;
		m.timers[0].next_time -= rel_time;
		m.timers[1].next_time -= rel_time;
		m.timers[2].next_time -= rel_time;
		assert(m.spc_time <= end_time);
		return m.smp_regs[0][r_cpuio0 .. $];
	}

	struct spc_file_t {
		char[signature_size] signature;
		ubyte has_id666;
		ubyte version_;
		ubyte pcl, pch;
		ubyte a;
		ubyte x;
		ubyte y;
		ubyte psw;
		ubyte sp;
		char[212] text;
		ubyte[0x10000] ram;
		ubyte[128] dsp;
		ubyte[0x40] unused;
		ubyte[0x40] ipl_rom;
	}

	static immutable char[signature_size] spcSignature = "SNES-SPC700 Sound File Data v0.30\x1A\x1A";

	void save_regs(ref ubyte[reg_count] out_) @safe {
		// Use current timer counter values
		for (int i = 0; i < timer_count; i++)
			out_[r_t0out + i] = cast(ubyte) m.timers[i].counter;

		// Last written values
		out_[0 .. r_t0out] = m.smp_regs[0][0 .. r_t0out];
	}
}

unittest {
	import core.exception : AssertError;
	import std.format : sformat;
	import std.stdio : writefln;
	string pswString = "nvpbhizc";
	void runTest(const ubyte[] program, ushort loadAddress, ushort startAddress, string file = __FILE__, ulong line = __LINE__) {
		ubyte[0x10000] buffer;
		buffer[loadAddress .. loadAddress + program.length] = program;
		SNES_SPC spc;
		static struct Result {
			const(ubyte)[] op;
			CPUState state;
		}
		Result[] results;
		spc.hook = (op, state) {
			results ~= Result(op, state);
		};
		spc.initialize();
		spc.load_buffer(buffer, startAddress);
		spc.run_until_(0x100);
		if (spc.m.ram[0x8000] != 1) {
			foreach (result; results) {
				char flag(ubyte idx) {
					return cast(char)(pswString[idx] - (result.state.psw & (1 << idx)) * 0x20);
				}
				char[50] buf;
				const ops = sformat!"%(%02X %)"(buf[], result.op);
				writefln!"%04X: % -12s % -6s (A: %02X, X: %02X, Y: %02X %s%s%s%s%s%s%s%s)"(result.state.pc, ops, mnemonics[result.op[0]], result.state.a, result.state.x, result.state.y, flag(0), flag(1), flag(2), flag(3), flag(4), flag(5), flag(6), flag(7));
			}
			throw new AssertError("Program failed!", file, line);
		}
	}
	void runTestSimple(const ubyte[] program, ushort loadAddress, string file = __FILE__, ulong line = __LINE__) {
		runTest(program, loadAddress, loadAddress, file, line);
	}
	runTestSimple([ // basic MOV
		0xE8, 0x01, // MOV A, #1
		0xC5, 0x00, 0x80, // MOV $8000, A
		0xFF, // STOP
	], 0x100);
	runTestSimple([ // MOV X
		0xCD, 0x01, // MOV X, #1
		0xC9, 0x00, 0x80, // MOV $8000, X
		0xFF, // STOP
	], 0x100);
	runTestSimple([ // MOV Y
		0x8D, 0x01, // MOV Y, #1
		0xCC, 0x00, 0x80, // MOV $8000, Y
		0xFF, // STOP
	], 0x100);
	runTestSimple([ // PUSH/POP A
		0xE8, 0x01, // MOV A, #1
		0x2D, // PUSH A
		0xE8, 0x00, // MOV A, #0
		0xAE, // POP A
		0xC5, 0x00, 0x80, // MOV $8000, A
		0xFF, // STOP
	], 0x100);
	runTestSimple([ // PUSH/POP X
		0xCD, 0x01, // MOV X, #1
		0x4D, // PUSH X
		0xCD, 0x00, // MOV X, #0
		0xCE, // POP X
		0xC9, 0x00, 0x80, // MOV $8000, X
		0xFF, // STOP
	], 0x100);
	runTestSimple([ // PUSH/POP Y
		0x8D, 0x01, // MOV Y, #1
		0x6D, // PUSH Y
		0x8D, 0x00, // MOV Y, #0
		0xEE, // POP Y
		0xCC, 0x00, 0x80, // MOV $8000, Y
		0xFF, // STOP
	], 0x100);
	runTestSimple([ // basic BRA
		0xE8, 0x01, // MOV A, #1
		0x2F, 0x02, // BRA +2
		0xE8, 0x00, // MOV A, #0
		0xC5, 0x00, 0x80, // MOV $8000, A
		0xFF, // STOP
	], 0x100);
	runTestSimple([ // basic BEQ
		0xE8, 0x01, // MOV A, #1
		0x68, 0x00, // CMP A #0
		0xF0, 0x04, // BEQ +4
		0xE8, 0x01, // MOV A, #1
		0x2F, 0x02, // BRA +2
		0xE8, 0x00, // MOV A, #0
		0xC5, 0x00, 0x80, // MOV $8000, A
		0xFF, // STOP
	], 0x100);
	runTestSimple([ // basic BNE
		0xE8, 0x01, // MOV A, #1
		0x68, 0x00, // CMP A #0
		0xD0, 0x04, // BNE +4
		0xE8, 0x00, // MOV A, #0
		0x2F, 0x02, // BRA +2
		0xE8, 0x01, // MOV A, #1
		0xC5, 0x00, 0x80, // MOV $8000, A
		0xFF, // STOP
	], 0x100);
	runTestSimple([ // basic BCC
		0xE8, 0x01, // MOV A, #1
		0x68, 0x10, // CMP A #16
		0x90, 0x04, // BCC +4
		0xE8, 0x00, // MOV A, #0
		0x2F, 0x02, // BRA +2
		0xE8, 0x01, // MOV A, #1
		0xC5, 0x00, 0x80, // MOV $8000, A
		0xFF, // STOP
	], 0x100);
	runTestSimple([ // BCC with negative A
		0xE8, 0xFF, // MOV A, #FF
		0x68, 0x10, // CMP A #16
		0x90, 0x04, // BCC +4
		0xE8, 0x01, // MOV A, #1
		0x2F, 0x02, // BRA +2
		0xE8, 0x00, // MOV A, #0
		0xC5, 0x00, 0x80, // MOV $8000, A
		0xFF, // STOP
	], 0x100);
	runTestSimple([ // basic BCS
		0xE8, 0x01, // MOV A, #1
		0x68, 0x10, // CMP A #16
		0xB0, 0x04, // BCS +4
		0xE8, 0x01, // MOV A, #1
		0x2F, 0x02, // BRA +2
		0xE8, 0x00, // MOV A, #0
		0xC5, 0x00, 0x80, // MOV $8000, A
		0xFF, // STOP
	], 0x100);
	runTestSimple([ // basic BMI
		0xE8, 0x01, // MOV A, #1
		0x68, 0x02, // CMP A #2
		0x30, 0x04, // BMI +4
		0xE8, 0x00, // MOV A, #0
		0x2F, 0x02, // BRA +2
		0xE8, 0x01, // MOV A, #1
		0xC5, 0x00, 0x80, // MOV $8000, A
		0xFF, // STOP
	], 0x100);
	runTestSimple([ // BMI with negative A
		0xE8, 0xFF, // MOV A, #FF
		0x68, 0x02, // CMP A #2
		0x30, 0x04, // BMI +4
		0xE8, 0x00, // MOV A, #0
		0x2F, 0x02, // BRA +2
		0xE8, 0x01, // MOV A, #1
		0xC5, 0x00, 0x80, // MOV $8000, A
		0xFF, // STOP
	], 0x100);
	runTestSimple([ // basic BPL
		0xE8, 0x01, // MOV A, #1
		0x68, 0x02, // CMP A #2
		0x10, 0x04, // BPL +4
		0xE8, 0x01, // MOV A, #1
		0x2F, 0x02, // BRA +2
		0xE8, 0x00, // MOV A, #0
		0xC5, 0x00, 0x80, // MOV $8000, A
		0xFF, // STOP
	], 0x100);
	runTestSimple([ // BPL with negative A
		0xE8, 0xFF, // MOV A, #FF
		0x68, 0x02, // CMP A #2
		0x10, 0x04, // BPL +4
		0xE8, 0x01, // MOV A, #1
		0x2F, 0x02, // BRA +2
		0xE8, 0x00, // MOV A, #0
		0xC5, 0x00, 0x80, // MOV $8000, A
		0xFF, // STOP
	], 0x100);
	runTestSimple([ // PUSH PSW/Z
		0xE8, 0x01, // MOV A, #1
		0x68, 0x00, // CMP A #0
		0x0D, // PUSH PSW
		0x68, 0x01, // CMP A #1
		0x8E, // POP PSW
		0xF0, 0x04, // BEQ +4
		0xE8, 0x01, // MOV A, #1
		0x2F, 0x02, // BRA +2
		0xE8, 0x00, // MOV A, #0
		0xC5, 0x00, 0x80, // MOV $8000, A
		0xFF, // STOP
	], 0x100);
	runTestSimple([ // ROL
		0xE8, 0x80, // MOV A, #80
		0x3C, // ROL
		0x3C, // ROL
		0xC5, 0x00, 0x80, // MOV $8000, A
		0xFF, // STOP
	], 0x100);
	runTestSimple([ // ROL ABS
		0xE8, 0x80, // MOV A, #80
		0xC5, 0x00, 0x80, // MOV $8000, A
		0x2C, 0x00, 0x80, // ROL $8000
		0x2C, 0x00, 0x80, // ROL $8000
		0xFF, // STOP
	], 0x100);
	runTestSimple([ // ROR
		0xE8, 0x02, // MOV A, #2
		0x7C, // ROR
		0xC5, 0x00, 0x80, // MOV $8000, A
		0xFF, // STOP
	], 0x100);
	runTestSimple([ // ROR ABS
		0xE8, 0x02, // MOV A, #2
		0xC5, 0x00, 0x80, // MOV $8000, A
		0x6C, 0x00, 0x80, // ROR $8000
		0xFF, // STOP
	], 0x100);
	runTestSimple([ // AND
		0xE8, 0xFF, // MOV A, #FF
		0x28, 0x01, // AND A, #1
		0xC5, 0x00, 0x80, // MOV $8000, A
		0xFF, // STOP
	], 0x100);
	runTestSimple([ // AND X
		0x8F, 0x01, 0x20, // MOV $20, #1
		0xE8, 0xFF, // MOV A, #FF
		0xCD, 0x20, // MOV X, #20
		0x26, // AND A, (X)
		0xC5, 0x00, 0x80, // MOV $8000, A
		0xFF, // STOP
	], 0x100);
}

void dprintf(const char*) nothrow @safe {
}
