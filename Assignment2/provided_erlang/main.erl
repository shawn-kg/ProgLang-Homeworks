-module(main).

% main functions
-export([start_file_server/1, start_dir_service/0, get/2, create/2, quit/1, print/1, getInfo/2]).

% can access own ual w/ node()
% can access own PID w/ self()

% you are free (and encouraged) to create helper functions
% but note that the functions that will be called when
% grading will be those below

% when starting the Directory Service and File Servers, you will need
% to register a process name and spawn a process in another node

% starts a directory service
start_dir_service() -> 
	io:format("~w~n",[node()]),
	register(node(),spawn(dirService,dirService,[[],[]])).
	% CODE THIS

% starts a file server with the UAL of the Directory Service
start_file_server(DirUAL) ->
	spawn(fileServer,fileServer,[DirUAL]).
	% CODE THIS

% requests file information from the Directory Service (DirUAL) on File
% then requests file parts from the locations retrieved from Dir Service
% then combines the file and saves to downloads folder
get(DirUAL, File) ->
	spawn(main,getInfo,[DirUAL,File]).	
	% CODE THIS

% gives Directory Service (DirUAL) the name/contents of File to create
create(DirUAL, File) ->
	Contents = util:readFile("input/" ++ File),
	{DirUAL, DirUAL} ! {create, File, Contents}.
	% CODE THIS

% sends shutdown message to the Directory Service (DirUAL)
quit(DirUAL) ->
	{DirUAL, DirUAL} ! quit.
	% CODE THIS

print(DirUAL) ->
	{DirUAL, DirUAL} ! printFiles.

getInfo(DirUAL, File) ->
	{DirUAL, DirUAL} ! {get, File, self()},
	getReceive(File,[]).

getReceive(File, Fileparts) ->
	receive 
		{filepart, FilePID, part_name} ->
			
	end.
% inp() ->
% 	[Head | Tail] = string:tokens(io:get_line(""),[$\s]),
% 	case Head of 
% 		"d" ->
% 			%io:format("Directory creation~n",[]),
% 			%[T] = Tail,
% 			%T1 = lists:sublist(T,1,length(T)-1),
% 			%io:format("~w~n",[T1]),
% 			start_dir_service(),
% 			inp();
% 		"f" ->
% 			io:format("Fileserver creation~n"),
% 			[H1 | T1] = Tail,
% 			%start_file_server(H1),
% 			[T] = T1,
% 			T2 = lists:sublist(T,1,length(T)-1),
% 			io:format("~w~n~w~n",[H1,T2]),
% 			inp();
% 		"c" ->
% 			io:format("Create command~n"),
% 			[H1|T1] = Tail,
% 			[T] = T1,
% 			T2 = lists:sublist(T,2,length(T)-3),
% 			%create(H1,T2),
% 			io:format("~w~n~w~n",[H1,T2]),
% 			inp();
% 		"g" ->
% 			io:format("Get command~n"),
% 			[H1|T1] = Tail,
% 			[T] = T1,
% 			T2 = lists:sublist(T,2,length(T)-3),
% 			io:format("~w~n~w~n",[H1,T2]),
% 			%get(H1,T2),
% 			inp()
% 	end. 
	
	% receive

	% end.


