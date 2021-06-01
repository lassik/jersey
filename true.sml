structure TrueCommand :> COMMAND = struct
type invocation = unit;
val invocationFromOpts = Command.reduceNoNonOption;
val opts = [];
fun main (run: invocation) = OS.Process.success;
val command = (main, invocationFromOpts, opts);
end;

val _ = Command.main TrueCommand.command;
