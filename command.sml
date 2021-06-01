signature COMMAND = sig
    type invocation;
    val invocationFromOpts : (string * (string list list)) list -> invocation;
    val opts : (string list * int) list;
    val main : invocation -> OS.Process.status;
    val command : ((invocation -> OS.Process.status) *
                   ((string * (string list list)) list -> invocation) *
                   (string list * int) list);
end

structure Command = struct

exception CannotHappen of string;
exception Usage of string;

fun stringInList string [] = false
  | stringInList string (s::ss) =
    (s = string) orelse stringInList string ss;

fun flagChars string =
    let fun loop flags i =
            if i = 0 then flags
            else let val flag = String.substring (string, (i - 1), 1)
                 in loop (([flag], 0) :: flags) (i - 1) end
    in loop [] (String.size string) end;

fun optionSetFold merge state [] = state
  | optionSetFold merge state ((optName,optValues)::opts) =
    optionSetFold merge (merge (state, optName, optValues)) opts;

type optset = (string * string list list) list;

fun reduceNoNonOption (set: optset) =
    optionSetFold
        (fn (_, name, _) => if name = "" then
                                raise Usage "No arguments expected"
                            else ())
        () set;

fun reducePresent optName (set: optset) =
    optionSetFold
        (fn (present, name, _) => if name = optName then true else present)
        false set;

fun reduceCount optName (set: optset) =
    optionSetFold
        (fn (count, name, _) => if name = optName then count + 1 else count)
        0 set;

fun reduceChoice optNames (set: optset) =
    optionSetFold
        (fn (match, name, _) =>
            if stringInList name optNames then
                case match
                 of NONE => SOME name
                  | SOME matchName =>
                    if matchName = name then
                        match
                    else
                        raise Usage "Conflicting flags"
            else
                match)
        NONE set;

fun reduceArg optName (set: optset) =
    optionSetFold
        (fn (lastValue, name, values) =>
            if name = optName then
                case values
                 of [[value]] =>
                    (case lastValue
                      of NONE => SOME value
                       | SOME _ => raise Usage "Option given more than once")
                  | _ => raise CannotHappen "Bad option spec"
            else lastValue)
        NONE set;

fun reduceZeroOrMoreArgs optName (set: optset) =
    optionSetFold
        (fn (prevValues, name, values) =>
            if name = optName then List.concat values else prevValues)
        [] set;

fun foldCommandLine merge state opts args =
    let fun takeDrop (list, n) =
            let fun loop head tail n =
                    case (if n > 0 then List.getItem tail else NONE)
                     of NONE => ((List.rev head), tail)
                      | SOME (x, tail) => loop (x::head) tail (n - 1)
            in loop [] list n end
        fun countDashes s =
            let fun loop i =
                    if (i < String.size s) andalso
                       (String.sub (s, i) = #"-") then loop (i + 1) else i
            in loop 0 end
        fun findOption name [] = raise Usage "No such option"
          | findOption name ((optNames, argCount)::opts) =
            if stringInList name optNames then
                ((List.hd optNames), argCount)
            else findOption name opts
        fun handleOption (state, args) (string, i, j) =
            let val optName = String.substring (string, i, j)
                val (canonicalName, argCount) = findOption optName opts
                val (optArgs, args) = takeDrop (args, argCount)
            in if List.length optArgs = argCount then
                   ((merge (state, canonicalName, optArgs)), args)
               else
                   raise Usage "Not enough arguments for option"
            end
        fun nonOptionArg state arg = merge (state, "", [arg])
        fun loopArgsOnly (state, []) = state
          | loopArgsOnly (state, (arg :: args)) =
            loopArgsOnly ((nonOptionArg state arg), args)
        fun loopOpts (state, []) = state
          | loopOpts (state, (arg :: args)) =
            let val d = countDashes arg
                val n = String.size arg
            in if d > 2 then
                   raise Usage "Too many dashes"
               else if d = 2 andalso d < n then
                   loopOpts (handleOption (state, args) (arg, d, (n - d)))
               else if d = 2 then
                   loopArgsOnly (state, args)
               else if d = 1 andalso d < n then
                   loopOptChars (state, args) (arg, d)
               else
                   loopOpts ((nonOptionArg state arg), args)
            end
        and loopOptChars (state, args) (arg, i) =
            if i = String.size arg then
                loopOpts (state, args)
            else
                loopOptChars (handleOption (state, args) (arg, i, 1))
                             (arg, (i + 1))
    in loopOpts (state, args) end;

fun optionSetFromArgs (opts: (string list * int) list)
                      (args: string list)
    : ((string * string list list) list) =
    foldCommandLine
        (fn (optSet, optName, optArgs) =>
            let fun loop acc [] =
                    List.rev ((optName, [optArgs]) :: acc)
                  | loop acc ((name, argss) :: optSet) =
                    if name = optName then
                        List.revAppend (((name, (argss @ [optArgs])) :: acc),
                                        optSet)
                    else
                        loop ((name, argss) :: acc) optSet
            in loop [] optSet end)
        []
        opts
        args;

fun exitFailure message = (print ("Cannot happen: " ^ message ^ "\n");
                           OS.Process.failure);

fun mainWithArgs (mainOnInvocation, invocationFromOpts, opts) args =
    mainOnInvocation (invocationFromOpts (optionSetFromArgs opts args))
    handle Usage e => exitFailure e
         | CannotHappen e => exitFailure e;

fun main command =
    mainWithArgs command (CommandLine.arguments ());

end;
