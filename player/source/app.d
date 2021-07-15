import spc700;

import core.stdc.stdlib;
import std.experimental.logger;
import std.string;
import std.stdio;
import std.file;

import bindbc.sdl : SDL_AudioCallback, SDL_AudioDeviceID;

__gshared SDL_AudioDeviceID dev;

bool initAudio(SDL_AudioCallback fun, ubyte channels, uint sampleRate, void* userdata = null) {
	import bindbc.sdl;

	assert(loadSDL() == sdlSupport);
	if (SDL_Init(SDL_INIT_AUDIO) != 0) {
		criticalf("SDL init failed: %s", SDL_GetError().fromStringz);
		return false;
	}
	SDL_AudioSpec want, have;
	want.freq = sampleRate;
	want.format = SDL_AudioFormat.AUDIO_S16;
	want.channels = channels;
	want.samples = 512;
	want.callback = fun;
	want.userdata = userdata;
	dev = SDL_OpenAudioDevice(null, 0, &want, &have, 0);
	if (dev == 0) {
		criticalf("SDL_OpenAudioDevice failed: %s", SDL_GetError().fromStringz);
		return false;
	}
	SDL_PauseAudioDevice(dev, 0);
	return true;
}

extern (C) void _sampling_func(void* user, ubyte* buf, int bufSize) nothrow {
	auto snes_spc = cast(SNES_SPC*)user;
	/* Play into buffer */
	snes_spc.play(bufSize / 2, cast(short*)buf );

	/* Filter samples */
	filter.run((cast(short*)buf)[0 .. bufSize /2]);
}

__gshared SPC_Filter filter;
int main(string[] args)
{
	/* Create emulator and filter */
	SNES_SPC snes_spc;
	snes_spc.init_();
	filter = SPC_Filter();

	/* Load SPC */
	{
		/* Load file into memory */
		long spc_size;
		void[] spc = std.file.read( (args.length > 1) ? args[1] : "test.spc" );

		/* Load SPC data into emulator */
		snes_spc.load_spc(spc);

		/* Most SPC files have garbage data in the echo buffer, so clear that */
		snes_spc.clear_echo();

		/* Clear filter before playing */
		filter.clear();
	}
	if (!initAudio(&_sampling_func, 2, SNES_SPC.sample_rate, &snes_spc)) {
		return 1;
	}

	writeln("Press enter to exit");
	readln();

	return 0;
}
