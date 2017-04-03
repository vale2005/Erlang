-module(merge_sort2).

-export([start/0]).

-define(MergeLimit, 0). %%merge till this length

%% main entry point
start() ->
    [_|Input] = from_file(file:open("input.txt", read)),
    Result = merge_sort(Input),
    to_file(file:open("output.txt", write), Result).


%% read data from file
from_file({ok, FileDescriptor}) ->
    case io:get_line(FileDescriptor, '') of
        eof  ->
            file:close(FileDescriptor),
            [];
        Line ->
            case string:strip(Line,right, $\n) of
                Word ->
                    [Word|from_file({ok, FileDescriptor})]
            end
    end.

%% write data to file
to_file({ok, FileDescriptor}, []) ->
    file:close(FileDescriptor);
to_file({ok, FileDescriptor}, [Word|Tail]) ->
    io:format(FileDescriptor, "~s\n", [Word]),
    to_file({ok, FileDescriptor}, Tail).

%%merge sort on a list
merge_sort(List)->
	Len = length(List),
	if 
		Len < 2 ->
			List;
		Len =< ?MergeLimit ->
			bubble_sort(List);
		true ->
			{L1, L2} = lists:split(Len div 2, List),
			Pid1 = spawn(fun() -> receive {ToSend, L} -> ToSend ! {self(), merge_sort(L)} end end),
			Pid1 ! {self(), L1},
			OL2 = merge_sort(L2),
			receive 
				{Pid1, OL1} ->
					zip(OL1, OL2)
			end
	end.

%%zips 2 ordered lists
zip([], L) ->
	L;
zip(L, []) ->
	L;
zip([H1|T1],[H2|T2]) ->
	if
		H1 =< H2 ->
			[H1|zip(T1,[H2|T2])];
		true ->
			[H2|zip([H1|T1], T2)]
	end.

bubble_sort(L)->
case lists:reverse(iterate_cmp(L)) of
	[H|T] ->
		[H|bubble_sort(T)];
	[] ->
		[]
end.

%%iterates compare operator through a list
%%smallest element at the end after execution
iterate_cmp([])->
	[];
iterate_cmp([H1|T]) ->
case T of
	[H2|TI] ->
		if 
			H1 >= H2 ->
				[H1|iterate_cmp([H2|TI])];
			true ->
				[H2|iterate_cmp([H1|TI])]
		end;
	[] ->
		[H1]
end.


