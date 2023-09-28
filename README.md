# fluent-wasm

an ergonomic interface for building and executing wasm bytecode. check out the
tests in `src/main.zig` if you'd like to see what it looks like :)

## using as a library

this was made with **zig 0.11.0** though it may work for newer versions:

add to your build.zig.zon dependencies:
```zig
.fluent-wasm = .{
    .url = "https://github.com/garrisonhh/fluent-wasm/tarball/<COMMIT-HASH>",
    .hash = "<TARBALL-HASH>",
},
```

add to your build.zig:
```zig
const wasm_dep = b.dependency("fluent-wasm", .{});
const wasm_mod = wasm3_dep.module("fluent-wasm");
const wasm_lib = wasm3_dep.artifact("fluent-wasm");

const exe = // ...

exe.linkLibC();
exe.linkLibrary(wasm_lib);
try exe.include_dirs.appendSlice(wasm_lib.include_dirs.items);
exe.addModule(wasm_mod);
```