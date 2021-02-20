-module(pigpio_simulator).

-export([start/0]).

-define(TCP_PORT, 8888).

start() ->
    Pid = spawn_link(fun () ->
                             {ok, Listen} = gen_tcp:listen(?TCP_PORT,
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
            Status = rand:uniform(2) - 1,
            gen_tcp:send(Socket,
                         <<3:32/little, 0, 0, Status:32/little, <<>>/binary>>),
            handle(Socket);
        {_, _, _} -> handle(Socket)
    end.
