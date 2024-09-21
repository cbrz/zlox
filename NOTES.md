# Notes

Some notes/thoughts while going through this project

## TODO

* Add optimizations (30)
  * Might not do this since it would require changing places where Value is declared
* String handling when debugging is kind of hard, is there a better way to do this with `std.log`?
* Look for places to use keywords like `comptime` and `inline`
* Add comments in case there's a possibility to port again to a different language

## Differences

Some general differences that could be used instead of the C version

* Table uses a HashMap as the backing. Also the function signatures are different.
* Scanner is using slices instead of char pointers/length
* Compiler had sentinel values using `-1` for scope depth and jump instructions. Changed these using a nullable. Reason for this is the "ip" is pointing to different instructions as a `usize` (unsigned int) and using `-1` will cause an integer overflow.
* After adding GC (implemented after print statement), found a lot of more places (calling GC on each allocation) that needed pushing to vm.stack (to mark the objects) than the book.

## Zig

* Can replace `#ifdef` using build options, need different options for runtime/test
* Memory management in Zig seems to involve getting an allocator (rolling your own or using existing one), and applying it to every struct/function that needs it on the way down. Kind of cool... (can be a bit annoying). It does help make lifetimes someone obvious. Defer keyword is neat way to cleanup.
* Lots of `try` littered everywhere in compiler.zig, could be annoying... I do like the explicit error handling
* `null` needs special handling which is also nice
* Zig has implicit copying of values, need to be explicit when you want to modify a value (or pass a pointer)
  * https://ziglang.org/documentation/master/#Pass-by-value-Parameters
  * https://zig.news/gowind/beware-the-copy-32l4
* The Zig language is changing all the time which makes referring to articles difficult depending on which version was used. Best examples were found in the source code.
