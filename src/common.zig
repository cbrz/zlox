const std = @import("std");

pub const Byte = u8;
pub const Short = u16;
pub const Line = u16;

pub const FRAME_MAX: usize = 64;
pub const BYTE_MAX: usize = std.math.maxInt(Byte) + 1;
pub const SHORT_MAX: usize = BYTE_MAX * BYTE_MAX;
pub const STACK_MAX: usize = FRAME_MAX * 4;

const config = @import("config");

pub const DEBUG_PRINT_CODE = config.DEBUG_PRINT_CODE;
pub const DEBUG_TRACE_EXECUTION = config.DEBUG_TRACE_EXECUTION;
pub const DEBUG_STRESS_GC = config.DEBUG_STRESS_GC;
pub const DEBUG_TRACE_GC = config.DEBUG_TRACE_GC;
