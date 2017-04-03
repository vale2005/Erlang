-module(orsi_bead1).
-export([start/0]).
 
%% main entry point.
start() ->
    [_|Input] = from_file(file:open("input.txt", read)),
    Data = lists:foldl(fun(X, Dict) -> addToDict(X, Dict) end, ordDict:new() , Input),
    Pids = ordDict:map(fun(X)-> start_process(X) end, Data),
    %%OrderedPids = lists:sort(fun(X, Y)-> case {X,Y} of {{_, A1}, {_, B1}} -> A1 < B1 end end, Pids),
    Results = ordDict:map(fun(X) -> end_process(X) end, Pids),
    to_file(file:open("output.txt", write), Results).


%% spawn processes and send given data
start_process({Game, GameData}) ->
    Pid = spawn(fun() -> process() end),
    Pid ! {self(), GameData},
    {Pid, Game}.
 
%% get data from given pids
end_process({Pid, Game}) ->
    receive
        {Pid, Pair} ->
            {Game, Pair}
    end.
 
%% read data from file
from_file({ok, FileDescriptor}) ->
    case io:get_line(FileDescriptor, '') of
        eof  ->
            file:close(FileDescriptor),
            [];
        Line ->
            case string:tokens(Line, " ") of
                [_, Game, Time] ->
                    [{Game, toInt(Time)}| from_file({ok, FileDescriptor})];
                _ ->
                    [[]|from_file({ok, FileDescriptor})]
            end
    end.

	
addToDict({Game, Time} , Dict) ->
	ordDict:update(Game, fun(List) -> [Time:List], [Time],Dict) .
	
	
%%groups and sorts input datas by game
groupAndSortByGame({Game, Time}, [{Key, List}|T]) ->
    case string:equal(Game, Key) of
        true -> 
            [{Key, [Time|List]}|T];
        false ->
            if 
                Game > Key ->
                    [{Key,List}|groupAndSortByGame({Game, Time}, T)];
                true ->
                    [{Game, [Time]}| [{Key, List}|T] ]
            end
    end;
groupAndSortByGame({Game, Time}, []) ->
    [{Game, [Time]}].


 
%% write data to file
to_file({ok, FileDescriptor}, [{Game, {Sum, Avg}}]) ->
    io:format(FileDescriptor, "~s ~B ~B\n", [Game, Sum, Avg]),
    file:close(FileDescriptor);
to_file({ok, FileDescriptor}, [{Game, {Sum, Avg}}|Tail]) ->
    io:format(FileDescriptor, "~s ~B ~B\n", [Game, Sum, Avg]),
    to_file({ok, FileDescriptor}, Tail).
 
%% "time consuming" process
process() ->
    receive
        { Sender, List } when is_list(List) ->
            Sender ! {self(), addAndAvg(List)};
        { Sender, _ } ->
            io:format("Something went wrong.\n"),
            Sender ! {self(), -1}
    end.

%%converts input string to intger
toInt(Str) ->
    {Result, _} = string:to_integer(Str), 
    Result.

%%creates a tuple with the summation and avareges of input lists' elements
addAndAvg(List) ->
    case lists:sum(List) of
        S ->
            {S, S div length(List)}
    end.