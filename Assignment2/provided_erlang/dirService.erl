-module(dirService).
-export([dirService/2,printPIDS/1,splitAndSendLocalTest/6]).

dirService(FileServers,Files) ->
    receive
        {fileServer, {FileNode,FilePID}} ->
            dirService(add(FilePID, FileNode, FileServers),Files);
        {sort, print} ->
            printPIDS(lists:sort(fun({FileNode1, _}, {FileNode2, _}) -> FileNode1 < FileNode2 end, FileServers)),
            dirService(lists:sort(fun({FileNode1, _}, {FileNode2, _}) -> FileNode1 < FileNode2 end,FileServers),Files);
        {create, Filename, Filecontents} ->
            dirService(FileServers,splitAndSend(Filename, Filecontents, FileServers, FileServers, Files, 1,1));
        {get, {Filename,ClientPID}} ->
            getBlocks(Filename,ClientPID,FileServers, Files), 
            dirService(FileServers,Files);
        quit ->
            quit(FileServers),
            ok
    end.


add(FilePID, FileNode, FileServers) ->
    [{FileNode, FilePID} | FileServers].

printPIDS([]) -> 
    ok;

printPIDS([{FileNode, FilePID} | Rest]) ->
    io:format("~w ~w~n", [FileNode,FilePID]),
    printPIDS(Rest).

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

splitAndSendLocalTest(Filename, Filecontents, FileServers, [Head | Rest ],Val, Filepart) ->
    if 
        Val > length(Filecontents) ->
            ok;
        length(Filecontents) - (Val-1) >= 64 ->
            io:fwrite("~w~n",[Head]),
            util:saveFile("downloads/" ++ Filename ++ integer_to_list(Filepart) ++ ".txt",lists:sublist(Filecontents, Val, 64)),
            splitAndSendLocalTest(Filename, Filecontents, FileServers, checkServers(Rest, FileServers), Val+64, Filepart+1);
        length(Filecontents) - (Val-1) < 64 -> 
            io:fwrite("~w~n",[Head]),
            util:saveFile("downloads/" ++ Filename ++ integer_to_list(Filepart) ++ ".txt",lists:sublist(Filecontents, Val, length(Filecontents) - (Val-1))),
            ok
    end.
getBlocks(Filename, ClientPID, FileServers, Files) ->
    



% counter(Val) ->
%     receive
%         increment -> 
%             counter(Val+1);
%         {From,get} ->
%             From ! {self(), Val},
%             counter(Val);
%         reset -> counter(1);
%         stop -> true
%     end.
