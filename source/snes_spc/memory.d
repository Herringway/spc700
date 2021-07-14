module snes_spc.memory;

import core.stdc.stdlib;
import core.lifetime;

T* manualAlloc(T)() {
	auto result = cast(T*) malloc(T.sizeof);
	emplace(result);
	return result;
}