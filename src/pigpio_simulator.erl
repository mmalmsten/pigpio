-module(pigpio_simulator).

-export([start/0]).

start() ->
    Pid = spawn_link(fun () ->
                             {ok, Listen} = gen_tcp:listen(8888,
                                                           [{active, false}]),
                             spawn(fun () -> acceptor(Listen) end),
                             timer:sleep(infinity)
                     end),
    {ok, Pid}.

acceptor(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun () -> acceptor(ListenSocket) end),
    handle(Socket).

%% Echoing back whatever was obtained
handle(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, <<"quit", _/binary>>} ->
            gen_tcp:close(Socket);
        {tcp, Socket, Msg} ->
            io:format("Msg ~p~n", [Msg]),
            gen_tcp:send(Socket, rand:uniform(2) - 1),
            handle(Socket);
        {_, _, _} -> handle(Socket)
    end.
