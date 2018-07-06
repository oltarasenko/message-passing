-module(flood2).
-author('roberto@widetag.com').
-compile(export_all).


% starts pong
start_pong() ->
	register(flood_pong, spawn(fun() -> pong_loop() end)).

pong_loop() ->
	receive
		{Sender, Any} ->
			% pong back
			qr:send(Sender, {pong, Any}),
			pong_loop();
		shutdown ->
			io:format("pong shutdown~n",[]);
		_Ignore ->
			pong_loop()
	after 30000 ->
		io:format("pong timeout, shutdown~n",[])
	end.

start(PongNode, Msg, Num) ->
	Res = rpc:call(PongNode, qr, start_link, []),
	io:format("QR, started: ~p~n", [Res]),
	true = rpc:call(PongNode, flood2, start_pong, []),
	{ok, _} = qr:start_link(),
	io:format("Started reciever~n"),
	start_responses_receiver(Num),
	io:format("Sending~n"),
	send(PongNode, Msg, Num).

% start ping
start_responses_receiver(Num) ->
	register(flood_ping, spawn(fun() -> ping_loop(Num, erlang:timestamp(), Num) end)).

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

send(_PongNode, _Msg, 0) ->
	ok;
send(PongNode, Msg, Num) ->
	% send a spawned ping
	spawn(fun() -> qr:send({flood_pong, PongNode}, {{flood_ping, node()}, Msg}) end),
	send(PongNode, Msg, Num - 1).
