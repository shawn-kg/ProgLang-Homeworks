-module(fileServer).
-export([fileServer/1,storeFile/2,fileServerMessageHandler/0]).

fileServer(DirUAL) ->
    {DirUAL, DirUAL} ! {fileServer, {node(),self()}},
    {DirUAL, DirUAL} ! {sort, print},
    Node = atom_to_list(node()),
    file:make_dir("servers/" ++ Node),
    fileServerMessageHandler().

fileServerMessageHandler() ->
    receive
        {chunk, {PartName, FileContents}} ->
            io:format("received message~n",[]),
            storeFile(PartName, FileContents),
            fileServerMessageHandler();
        % {get, {Filename,DirUAL}} -> 
            
        quit ->
            ok
    end.

storeFile(PartName, FileContents) ->
    Node = atom_to_list(node()),
    util:saveFile("servers/" ++ Node ++ "/" ++ PartName, FileContents).
    