-module(dirService).
-export([dirService/2,printPIDS/1]).

dirService(FileServers,Files) ->
    receive
        {fileServer, {FileNode,FilePID}} ->
            dirService(add(FilePID, FileNode, FileServers),Files);
        {sort, print} ->
            printPIDS(lists:sort(fun({FileNode1, _}, {FileNode2, _}) -> FileNode1 < FileNode2 end, FileServers)),
            dirService(lists:sort(fun({FileNode1, _}, {FileNode2, _}) -> FileNode1 < FileNode2 end,FileServers),Files);
        {create, Filename, Filecontents} ->
            dirService(FileServers,splitAndSend(Filename, Filecontents, FileServers, FileServers, Files, 1,1));
        printFiles ->
            printFiles(Files),
            dirService(FileServers,Files);
        {get, Filename, ClientPID} ->
            sendFilesData(Filename,ClientPID, Files), 
            dirService(FileServers,Files);
        quit ->
            quit(FileServers),
            init:stop()
    end.


add(FilePID, FileNode, FileServers) ->
    [{FileNode, FilePID} | FileServers].

printPIDS([]) -> 
    ok;

printPIDS([{FileNode, FilePID} | Rest]) ->
    io:format("~w ~w~n", [FileNode,FilePID]),
    printPIDS(Rest).

printFiles([]) ->
    ok;
printFiles([{Filename, {ActualName, FilePID}} | Rest]) ->
    io:format("~w~n~w~n~w~n",[Filename,ActualName,FilePID]),
    printFiles(Rest).

quit ([]) ->
    ok;

quit([{_, FilePID} | Rest]) ->
    FilePID ! quit,
    quit(Rest).

splitAndSend(Filename, Filecontents, FileServers, [{_, FilePID} | Rest ], Files, Val, Filepart) ->
    if 
        Val > length(Filecontents) ->
            Files;
        length(Filecontents) - (Val-1) >= 64 ->
            io:format("Pass through~n",[]),
            io:format("~w~n",[FilePID]),
            ActualName = lists:sublist(Filename, 1, string:str(Filename,".")-1) ++ "_" ++ integer_to_list(Filepart) ++ lists:sublist(Filename,string:str(Filename,"."),length(Filename)-(string:str(Filename,".")-1)),
            FilePID ! {chunk, {ActualName, lists:sublist(Filecontents, Val, 64)} },
            splitAndSend(Filename, Filecontents, FileServers, checkServers(Rest, FileServers), [{Filename, {ActualName, FilePID}} | Files], Val+64, Filepart+1);
        length(Filecontents) - (Val-1) < 64 -> 
            io:format("Pass through~n",[]),
            io:format("~w~n",[FilePID]),
            ActualName = lists:sublist(Filename, 1, string:str(Filename,".")-1) ++ "_" ++ integer_to_list(Filepart) ++ lists:sublist(Filename,string:str(Filename,"."),length(Filename)-(string:str(Filename,".")-1)),
            FilePID ! {chunk, {ActualName, lists:sublist(Filecontents, Val, length(Filecontents) - (Val-1))} },
            [{Filename, {ActualName, FilePID}} | Files]
    end.


checkServers([],FileServers) ->
    io:format("sent back full list~n",[]),
    FileServers;
checkServers(RestofServers, _) ->
    RestofServers.

sendFilesData(_,ClientPID,[]) ->
    ClientPID ! donewithfileparts;
sendFilesData(RequestName, ClientPID, [{Filename, {ActualName, FilePID}} | Rest]) ->
    if 
        RequestName == Filename ->
            ClientPID ! {filepart, FilePID, ActualName},
            sendFilesData(RequestName, ClientPID, Rest);
        RequestName /= Filename ->
            sendFilesData(RequestName, ClientPID, Rest)
    end.


