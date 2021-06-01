structure CatCommand :> COMMAND = struct

type invocation = {
    flag_u: bool,
    filenames: string list
};

fun invocationFromOpts opts = {
    flag_u = Command.reducePresent "u" opts,
    filenames = Command.reduceZeroOrMoreArgs "" opts
};

val opts = Command.flagChars "u";

fun catFileDesc fileDesc =
    let val buffer = Word8Array.array (4096, 0w0)
        val slice = Word8ArraySlice.full buffer
        fun loop () =
            let val nRead = Posix.IO.readArr (fileDesc, slice)
            in if nRead = 0 then
                   ()
               else
                   let val nWritten =
                           Posix.IO.writeArr
                               (Posix.FileSys.stdout,
                                (Word8ArraySlice.slice (buffer, 0,
                                                        SOME nRead)))
                   in if nWritten = nRead then
                          loop ()
                      else
                          raise Command.CannotHappen "not enough written"
                   end
            end
    in loop () end;

fun catFile filename =
    let val fileDesc = Posix.FileSys.openf (filename,
                                            Posix.FileSys.O_RDONLY,
                                            (Posix.FileSys.O.flags []))
    in catFileDesc fileDesc end;

fun main (run: invocation) =
    ((if (#filenames run) = [] then
          catFileDesc Posix.FileSys.stdin
      else
          List.app (fn file => if file = "-" then
                                   catFileDesc Posix.FileSys.stdin
                               else
                                   catFile file)
                   (#filenames run));
     OS.Process.success);

val command = (main, invocationFromOpts, opts);

end;

val _ = Command.main CatCommand.command;
