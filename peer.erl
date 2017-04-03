-module(peer).

-record(torrent, {announce, info}).

-record(metainfo, {name, piece_length, pieces, length}).

-export([peer/1, seeder/1]).

check_hash(FilePiece, Hash) ->
	crypto:hash(sha, FilePiece) == Hash.
		

download_piece(PieceIndex, Peer) ->
	Peer ! {request, PieceIndex},
	receive
		{piece, Piece} -> Piece
	after
		2000 -> exit({download_failed, PieceIndex})
	end.

save_piece(FilePiece, Offset, File) ->
	 file:pwrite(File, Offset, FilePiece).

download_next_piece(PieceIndex, Hash, Peer) ->
	Piece = download_piece(PieceIndex, Peer),
	case check_hash(Piece, Hash) of 
		true ->
			Piece;
		false ->
			exit({download_failed, PieceIndex})
	end.

select_next_piece(PeerStatuses, RemainingPieces,_) ->
	case length(RemainingPieces) of
		0 ->
			[];
		L1 ->
			NextIndex = random:uniform(L1),
			NextPieceIndex = lists:nth(NextIndex, RemainingPieces),
			case lists:keyfind(NextPieceIndex, 1, PeerStatuses) of
				{_, PeerList} ->
					case length(PeerList) of
						0 ->
							[];
						L2 ->
							NextPeerIndex = random:uniform(L2),
							NextPeer = lists:nth(NextPeerIndex, PeerList),
							[{NextPieceIndex, NextPeer, lists:keydelete(NextPieceIndex, 1, RemainingPieces)}]
					end;
				false ->
					[]
			end
	end.

file_offset(PieceInd, Metainfo) ->
	PieceInd * Metainfo#metainfo.piece_length.	


download_random_piece(MetaInfo, PeerStatuses, RemainingPieces, File) ->
	case select_next_piece(PeerStatuses, RemainingPieces,File) of
		[{NextPieceIndex, NextPeer, NewRemainingPieces}] ->
			ActPeer = self(),
			spawn_monitor(
				fun() -> 
					Piece = download_piece(NextPieceIndex, NextPeer),
					Offset = file_offset(NextPieceIndex, MetaInfo),
					save_piece(Piece, Offset, File), 
					ActPeer ! {downloaded, NextPieceIndex} 
				end),
			NewRemainingPieces;
		[] ->
		  	ok
	end.

notify_peers(PeerList, PieceIndex) ->
	lists:map(fun(Peer) -> Peer ! {have, PieceIndex, self()}  end, PeerList).

read_piece(Offset, PieceSize, File) ->
	file:pread(File, Offset, PieceSize).

update_piece_status(PieceInd, Peer, PeerStatuses) ->
	{_, HavingPeers} = lists:keyfind(PieceInd, 1, PeerStatuses),
	case lists:keyfind(Peer, 1, HavingPeers) of
		false ->
			lists:keyreplace(PieceInd, 1, PeerStatuses, {PieceInd, [Peer|HavingPeers]})
	end.

update_pieces_status(Pieces, Peer, PeerStatuses) -> 
	lists:map(fun(Piece) -> update_piece_status(Piece, Peer, PeerStatuses) end, Pieces).


loop(MetaInfo, PeerList, PeerStatuses, DownLoadedPieces, RemainingPieces, File) ->
	receive 
		{tracker_response, NewPeers} ->
			NewSts = lists:map(fun(Pair)->update_statuses(Pair, NewPeers) end, PeerStatuses),
			loop(MetaInfo, NewPeers, NewSts , DownLoadedPieces, RemainingPieces, File);
		{have_pieces, PieceIndices, Peer} ->
			NewSts = update_pieces_status(PieceIndices, Peer, PeerStatuses),
			Peer ! {have_pieces_response, DownLoadedPieces, self()},
			NewRem = download_random_piece(MetaInfo, NewSts, RemainingPieces, File),
			loop(MetaInfo, PeerList, NewSts, DownLoadedPieces, NewRem, File);
		{have_pieces_response, PieceIndices, Peer} ->
			NewSts = update_pieces_status(PieceIndices, Peer, PeerStatuses),
			NewRem = download_random_piece(MetaInfo, NewSts, RemainingPieces, File),
			loop(MetaInfo, PeerList, NewSts, DownLoadedPieces, NewRem, File);
		{have, PieceIndex, Peer} ->
			loop(MetaInfo, PeerList, update_piece_status(PieceIndex, Peer, PeerStatuses), DownLoadedPieces, RemainingPieces, File);
		{request, PieceIndex, Peer} ->
			spawn(fun() -> Peer ! {piece, read_piece(file_offset(MetaInfo,PieceIndex),MetaInfo#metainfo.piece_length,File)} end),
			loop(MetaInfo, PeerList, RemainingPieces, DownLoadedPieces, RemainingPieces, File);
		{downloaded, PieceIndex} ->
			notify_peers(PeerList, PieceIndex),
			NewRem = download_random_piece(MetaInfo, PeerStatuses, RemainingPieces, File),
			loop(MetaInfo, PeerList, PeerStatuses, [PieceIndex|DownLoadedPieces], NewRem, File);
		{'DOWN', _, _, _, {download_failed, PieceIndex}} ->
			NewRem = download_random_piece(MetaInfo, PeerStatuses, [PieceIndex|RemainingPieces], File),
			loop(MetaInfo, PeerList, PeerStatuses, DownLoadedPieces, NewRem, File)
	end.

peer({torrent, _, [MInfo]}) ->
	tracker ! {get, self()},
	receive
		{tracker_response, PeerList} ->
			CreateFile = file:make_dir("nf"),
			Pieces = MInfo#metainfo.pieces,
			PeerSt = lists:map(fun(Piece)-> {Piece, []} end, Pieces),
			loop(MInfo, PeerList, PeerSt, [], Pieces, CreateFile)
			
	end,
	timer:send_interval(2000, tracker, {get, self()}).
	

seeder({torrent, _, [MInfo]}) ->
	tracker ! {get, self()},
	receive
		{tracker_response, PeerList} ->
			ReadFile = file:open(MInfo#metainfo.name, [read]),
			Pieces = MInfo#metainfo.pieces,
			PeerSt = lists:map(fun(Piece)-> {Piece, [self()]} end, Pieces),
			loop(MInfo, PeerList, PeerSt, Pieces, [], ReadFile)
	end,
	timer:send_interval(2000, tracker, {get, self()}).

%%egy piece státuszából torol
update_statuses({Piece, PeerList}, NewPeers) ->
	lists:foldl(
		fun(Peer, {TempPiece, TempList}) ->
			case lists:keyfind(Peer, 1, NewPeers) of 
				false -> {TempPiece, lists:keydelete(Peer, 1, TempList)};
				_ -> {TempPiece, TempList}
			end
		end,
		{Piece, PeerList},
		PeerList
	).
