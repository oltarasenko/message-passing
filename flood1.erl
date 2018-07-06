-module(flood1).
-author('roberto@widetag.com').
-compile(export_all).


% starts pong
start_pong() ->
	register(flood_pong, spawn(fun() -> pong_loop() end)).

pong_loop() ->
	receive
		{{Sender, SenderNode}, Any} ->
			% pong back
			{Sender, SenderNode} ! {pong, Any},
			pong_loop();
		shutdown ->
			io:format("pong shutdown~n",[]);
		_Ignore ->
			pong_loop()
	after 60000 ->
		io:format("pong timeout, shutdown~n",[])
	end.


start(PongNode, Msg, Num) ->
	true = rpc:call(PongNode, flood1, start_pong, []),
	io:format("Started reciever~n"),
	start_responses_receiver(Num),
	io:format("Sending~n"),
	send(PongNode, Msg, Num).

% start ping
start_responses_receiver(Num) ->
	register(flood_ping, spawn(fun() -> ping_loop(Num, erlang:timestamp(), Num) end)).

send(_PongNode, _Msg, 0) ->
	ok;
send(PongNode, Msg, Num) ->
	% send a spawned ping
	spawn(fun() -> {flood_pong, PongNode} ! {{flood_ping, node()}, Msg} end),
	send(PongNode, Msg, Num - 1).

ping_loop(Num, Start, 0) ->
	T = timer:now_diff(erlang:timestamp(), Start),
	io:format("RECEIVED ALL ~p in ~p ms [~p/min]~n",[Num, T, (Num*60000000/T)]);
ping_loop(Num, Start, Count) ->
	receive
		{pong, _PingBack} ->
			ping_loop(Num, Start, Count-1);
		_Received ->
			ping_loop(Num, Start, Count)
	after 10000 ->
		io:format("ping timeout, missing ~p pong, shutdown~n",[Count])
	end.
