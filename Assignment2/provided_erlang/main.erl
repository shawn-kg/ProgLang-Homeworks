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
	file:make_dir("servers"),
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
	file:make_dir("downloads"),
	spawn(main,getInfo,[DirUAL,atom_to_list(File)]).	
	% CODE THIS

% gives Directory Service (DirUAL) the name/contents of File to create
create(DirUAL, File) ->
	Contents = util:readFile("input/" ++ atom_to_list(File)),
	{DirUAL, DirUAL} ! {create, atom_to_list(File), Contents}.
	% CODE THIS

% sends shutdown message to the Directory Service (DirUAL)
quit(DirUAL) ->
	{DirUAL, DirUAL} ! quit,
	init:stop().
	% CODE THIS
printList([]) ->
	ok;
printList([{Partname, Filecontents} | Rest]) ->
	io:format("~w~n~w~n",[Partname, Filecontents]),
	printList(Rest).


print(DirUAL) ->
	{DirUAL, DirUAL} ! printFiles.




getInfo(DirUAL, File) ->
	{DirUAL, DirUAL} ! {get, File, self()},
	getReceive(File,[], []).

getReceive(File, Fileparts, FileList) ->
	receive 
		{filepart, FilePID, PartName} ->
			getReceive(File, addFilePart(Fileparts, PartName, FilePID), FileList);
		donewithfileparts ->
			printList(Fileparts),
			askForParts(Fileparts),
			getReceive(File, Fileparts, FileList);
		{filecontents, _, PartName, FileContents} ->
			NewFileList = addFileContent(FileList, FileContents, PartName),
			checkLength(File, Fileparts,NewFileList)
	end.

checkLength(File, Fileparts, FileList) ->
	if 
		length(Fileparts)==length(FileList) ->
			storeFileDriver(File, FileList);
		length(Fileparts) /= length(FileList) ->
			getReceive(File, Fileparts, FileList)
	end.

addFileContent(FileList, FileContents, PartName) ->
	[{PartName, FileContents} | FileList].

addFilePart(Fileparts, PartName, FilePID) ->
	[{FilePID, PartName} | Fileparts].

askForParts([]) ->
	ok;
askForParts([{FilePID, PartName}| Rest]) ->
	FilePID ! {clientAsk, PartName, self()},
	askForParts(Rest).

storeFileDriver(File, FileList) ->
	NewFileList = lists:sort(fun({PartName1, _ }, {PartName2, _}) -> PartName1 < PartName2 end, FileList),
	printList(NewFileList),
	
	util:saveFile("downloads/" ++ File, storeFile(NewFileList, [])).
storeFile([],FileToStore) ->
	FileToStore;
storeFile([{_, FileContents} | Rest] , FileToStore) ->
	storeFile(Rest, FileToStore ++ FileContents).



