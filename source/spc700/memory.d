module spc700.memory;

import std.experimental.allocator;

T* manualAlloc(T)() {
	return theAllocator.make!T();
}
void manualFree(T)(T* ptr) {
	theAllocator.dispose(ptr);
}