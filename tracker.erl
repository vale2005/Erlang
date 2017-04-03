-module(tracker).
-export([main/0]).

loop(PeerList) ->
	receive
		{get, Peer} ->
			case lists:keyfind(Peer, 1, PeerList) of
				false ->
					monitor(process, Peer),
					NewList = [Peer|PeerList];
				_ ->
					NewList = PeerList
			end,
			Peer ! {tracker_response, NewList},
			loop(NewList);
		{'DOWN', MonitorRef, _, Peer, _} ->
			loop(lists:keydelete(Peer, 1, PeerList));
		Unexp ->
			io:format("Unexpected message: ~s~n", [Unexp]),
			loop(PeerList)
	end.

main() ->
	Tracker = spawn(fun() -> loop([]) end),
	register(tracker, Tracker).

