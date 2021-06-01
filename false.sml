structure FalseCommand :> COMMAND = struct
type invocation = unit;
val invocationFromOpts = Command.reduceNoNonOption;
val opts = [];
fun main (run: invocation) = OS.Process.failure;
val command = (main, invocationFromOpts, opts);
end;

val _ = Command.main FalseCommand.command;
