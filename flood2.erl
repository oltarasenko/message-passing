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


% start ping
start_ping(PongNode, Num) ->
	register(flood_ping, spawn(fun() -> ping_loop(Num, now(), Num) end)),
	send(PongNode, Num).

send(_PongNode, 0) ->
	ok;
send(PongNode, Num) ->
	% send a spawned ping
	% Message = ping_request,
	Message = "a message of a string content of 128 bytes long---------------------------------------------------------------------------------------------------------------------------------------------------------===============================================================================================================================",
	spawn(fun() -> qr:send({flood_pong, PongNode}, {{flood_ping, node()}, Message}) end),
	send(PongNode, Num - 1).

ping_loop(Num, Start, 0) ->
	T = timer:now_diff(now(), Start),
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
