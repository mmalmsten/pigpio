-module(pigpio).

-author(skvamme).

-author(mmalmsten).

-behaviour(gen_server).

-export([connect/1, connect/2, connect/3, read/1, write/2]).

-export([handle_call/3, handle_cast/2, handle_info/2,
     init/1, start_link/3]).

-define(UINT, 32/little).

%%
%% pigpio commands
%%

connect(Gpio) -> connect(Gpio, input).
connect(Gpio, Mode) -> connect(Gpio, Mode, {"127.0.0.1", 8888}).
connect(Gpio, input, Conn) -> start_link(Conn, Gpio, 0);
connect(Gpio, output, Conn) -> start_link(Conn, Gpio, 1).
    
read(Pid) -> gen_server:call(Pid, read).

write(Pid, Msg) -> gen_server:call(Pid, {write, Msg}).

%%
%% Handle gen_server and tcp connection
%%

start_link({Ip, Port}, Gpio, Mode) ->
    {ok, Pid} = gen_server:start_link(?MODULE,
                      {Ip, Port, Gpio, Mode}, []),
    {ok, Pid}.

init({Ip, Port, Gpio, Mode}) ->
    application:start(inets),
    {ok, Socket} = gen_tcp:connect(Ip, Port, [binary, {packet, 0}]),
    ok = gen_tcp:send(Socket, <<0:?UINT,Gpio:?UINT,Mode:?UINT,0:?UINT>>),
    {ok, #{socket => Socket, gpio => Gpio}}.

handle_call(read, _, State) ->
    #{gpio := Gpio, socket := Socket} = State,
    ok = gen_tcp:send(Socket, <<3:?UINT,Gpio:?UINT,0:?UINT,0:?UINT>>),
    {reply, ok, State};

handle_call({write, Msg}, _, State) ->
    {reply, Msg, State};
handle_call(terminate, _, State) ->
    {stop, normal, ok, State}.

handle_cast(Msg, State) ->
    io:format("handle_cast: ~p~n", [Msg]),
    {noreply, State}.

handle_info(Msg, {Socket, Gpio}) ->
    io:format("handle_info parse: ~p~n", [parse(Msg)]),
    {noreply, {Socket, Gpio}}.

%%
% pigpio response
%%

parse(<<>>) -> ok;
parse(<<0:?UINT, P1:?UINT, P2:?UINT, 0:?UINT, Rest/binary>>) ->
    io:format("setmode pin ~p mode ~p~n", [P1, P2]),
    parse(Rest);
parse(<<1:?UINT, P1:?UINT, _P2:?UINT, P3:?UINT, Rest/binary>>) ->
    io:format("getmode pin ~p mode ~p~n", [P1, P3]),
    parse(Rest);
parse(<<2:?UINT, P1:?UINT, P2:?UINT, 0:?UINT, Rest/binary>>) ->
    io:format("setpullupdown pin ~p pud ~p~n", [P1, P2]),
    parse(Rest);
parse(<<3:?UINT, _P1:?UINT, _P2:?UINT, P3:?UINT, Rest/binary>>) ->
    io:format("read ~p~n", [P3]), parse(Rest);
parse(<<4:?UINT, P1:?UINT, P2:?UINT, 0:?UINT, Rest/binary>>) ->
    io:format("write pin ~p level ~p~n", [P1, P2]),
    parse(Rest);
parse(<<10:?UINT, _P1:?UINT, _P2:?UINT, P3:?UINT, Rest/binary>>) ->
    io:format("readbits 0-31 ~.2B~n", [P3]),
    parse(Rest);
parse(<<17:?UINT, _P1:?UINT, _P2:?UINT, P3:?UINT, Rest/binary>>) ->
    io:format("hver ~p~n", [P3]), parse(Rest);
parse(Response) ->
    io:format("Error: ~w~n", [Response]).

