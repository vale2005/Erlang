-module(matrices).

-export([start/0]).

%% main entry point.
start() ->
	MainThread = self(), 
	spawn(fun() -> readPoints(file:open("input_points.txt", read), MainThread) end),
	Matrices = readMatrices(file:open("input_matrices.txt", read)),
	{Count, Points} = receive A -> A end,
	Pids = spawnThreads(self(), Matrices, Count, MainThread),
	[StartPid|ToPids] = Pids,
	lists:zipwith(fun(Pid, ToPid) -> Pid ! {self(), ToPid} end, Pids, ToPids++[self()]),
	lists:map(fun(P) -> StartPid ! {self(), P} end, Points),
	Results = getResults(Count, lists:last(Pids)),
	toFile(file:open("output.txt", write), Results).

%%reads the number of input points and the points themselves
readPoints({ok, FileDescriptor}, MainThread) ->
    case io:get_line(FileDescriptor, '') of
        eof  ->
            file:close(FileDescriptor),
            [];
        Line ->
            case string:tokens(Line, " ") of
                [P1, P2, P3] ->
                    [[toInt(P1), toInt(P2), toInt(P3), 1]| readPoints({ok, FileDescriptor}, MainThread)];
                [Count] ->
                    MainThread ! {toInt(Count), readPoints({ok, FileDescriptor}, MainThread)}
            end
    end.

%%reads the matrices for transformation, ignoring the first line(which includes one number)
readMatrices({ok, FileDescriptor}) ->
    case io:get_line(FileDescriptor, '') of
        eof  ->
            file:close(FileDescriptor),
            [];
        Line ->
            case string:tokens(Line, " ") of
                [P1, P2, P3, P4] ->
                    [[toInt(P1), toInt(P2), toInt(P3), toInt(P4)]| readMatrices({ok, FileDescriptor})];
                _ ->
                    readMatrices({ok, FileDescriptor})
            end
    end.

%%spawns the processes for each 'atom' of the transformation
%%the processes now the sender, and they are waiting for the main thread to send the forward adress
%%then invoke the transform method
spawnThreads(_, [], _, _) ->
	[];
spawnThreads(From,Mtx, Count, MainThread) ->
	case lists:split(4, Mtx) of
		{H, T} ->
			NewPid = spawn(fun() -> receive {MainThread, To} -> transform(From, To, H, Count) end end),
			[NewPid | spawnThreads(NewPid,T,Count, MainThread)]
	end.

%%multiples 'Count' nomber of points with 'Mtx'
transform(From, To, Mtx, Count) ->
	if 
		Count =< 0 ->
			ok;
		true ->
			receive 
				{From, V} ->
					Result = lists:map(fun(M) -> lists:sum(lists:zipwith(fun(X, Y) -> X*Y end, M, V)) end ,Mtx),
					To ! {self(), Result},
					transform(From, To, Mtx, Count - 1)
			end
	end.

%%receives 'Count' number of transformed points
getResults(Count, Pid) ->
	if 
		Count == 0 ->
			[];
		Count == 1 ->
			receive
				{Pid, [P1, P2, P3, _]} ->
					[[P1, P2, P3]]
			end;
		Count > 0 ->
			receive
				{Pid,[P1, P2, P3, _]} ->
					[[P1, P2, P3]| getResults(Count-1, Pid)]
			end
	end.

%%writes the output into a file
toFile({ok, FileDescriptor}, [[P1, P2, P3]]) ->
    io:format(FileDescriptor, "~B ~B ~B\n", [P1, P2, P3]),
    file:close(FileDescriptor);
toFile({ok, FileDescriptor}, [[P1, P2, P3]|Tail]) ->
    io:format(FileDescriptor, "~B ~B ~B\n", [P1, P2, P3]),
    toFile({ok, FileDescriptor}, Tail).

%%converts input string to intger
toInt(Str) ->
    {Result, _} = string:to_integer(Str), 
    Result.