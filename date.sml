structure DateCommand :> COMMAND = struct

type invocation = {
    flag_u: bool,
    date: string option
};

fun invocationFromOpts opts = {
    flag_u = Command.reducePresent "u" opts,
    date = Command.reduceOptionalArg opts
};

val opts = Command.flagChars "u";

val defaultDateFormat = "";

fun formatDate format date =
    Date.toString date;

fun writeCurrentDate utc format =
    let val time = Posix.ProcEnv.time ()
        val date = Date.fromTimeUniv time
    in print ((formatDate format date) ^ "\n") end;

fun main (run: invocation) =
    ((case #date run
       of NONE => writeCurrentDate (#flag_u run) defaultDateFormat
        | SOME string =>
          if string = "" then
              raise Command.Usage "string argument is blank"
          else if String.sub (string, 0) = #"+" then
              writeCurrentDate (#flag_u run)
                               (String.extract (string, 1, NONE))
          else
              raise Command.Usage
                    "string argument does not start with + sign");
     OS.Process.success);

val command = (main, invocationFromOpts, opts);

end;

val _ = Command.main DateCommand.command;
