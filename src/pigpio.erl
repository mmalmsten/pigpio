-module(pigpio).

-author(skvamme).

-author(mmalmsten).

-behaviour(gen_server).

-export([call/2, cast/2, start_link/1]).

-export([handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1]).

-define(TCP_ADDRESS, "localhost").

-define(TCP_PORT, 8888).

start_link(Gpio) ->
    io:format("start gen_server with Gpio ~p~n", [Gpio]),
    gen_server:start_link(?MODULE, Gpio, []).

call(Pid, Msg) -> gen_server:call(Pid, Msg).

cast(Pid, Msg) -> gen_server:cast(Pid, Msg).

init(Gpio) ->
    application:start(inets),
    {ok, Socket} = gen_tcp:connect(?TCP_ADDRESS,
                                   ?TCP_PORT,
                                   [binary, {packet, 0}]),
    {ok, #{socket => Socket, gpio => Gpio, data => false}}.

handle_call(read, _, State) ->
    #{data := Data} = State,
    {reply, Data, State};
handle_call(Msg, _From, State) ->
    io:format("Msg ~p~n", [Msg]),
    {reply, Msg, State}.

handle_cast({command, Cmd, Input}, State) ->
    #{gpio := Gpio, socket := Socket} = State,
    gen_tcp:send(Socket, command({Cmd, Gpio, Input})),
    {noreply, State};
handle_cast({read, once}, State) ->
    #{gpio := Gpio, socket := Socket} = State,
    gen_tcp:send(Socket, command({read, Gpio})),
    {noreply, State};
handle_cast({read, Time}, State) ->
    #{gpio := Gpio, socket := Socket} = State,
    timer:apply_interval(Time,
                         gen_tcp,
                         send,
                         [Socket, command({read, Gpio})]),
    {noreply, State};
handle_cast(Msg, State) ->
    io:format("Msg ~p~n", [Msg]),
    {noreply, State}.

%% Receive sensor data
handle_info({tcp, _, Msg}, State) ->
    {noreply, maps:put(data, parse(Msg), State)};
handle_info(Msg, State) ->
    io:format("Msg ~p~n", [Msg]),
    {noreply, State}.

%%
%% pigpio commands
%%

command({hver}) ->
    <<17:32/little, 0:32/little, 0:32/little, 0:32/little>>;
command({br1}) ->
    <<10:32/little, 0:32/little, 0:32/little, 0:32/little>>;
command({read, Gpio}) ->
    <<3:32/little, Gpio:32/little, 0:32/little,
      0:32/little>>;
command({getmode, Gpio}) ->
    <<1:32/little, Gpio:32/little, 0:32/little,
      0:32/little>>;
command({setmode, Gpio, 0}) -> % Input
    <<0:32/little, Gpio:32/little, 0:32/little,
      0:32/little>>;
command({setmode, Gpio, 1}) -> % Output
    <<0:32/little, Gpio:32/little, 1:32/little,
      0:32/little>>;
command({write, Gpio, Level}) ->
    <<4:32/little, Gpio:32/little, Level:32/little,
      0:32/little>>;
command({setpullupdown, Gpio, 0}) -> % Off
    <<2:32/little, Gpio:32/little, 0:32/little,
      0:32/little>>;
command({setpullupdown, Gpio, 1}) -> % Down
    <<2:32/little, Gpio:32/little, 1:32/little,
      0:32/little>>;
command({setpullupdown, Gpio, 2}) -> % Up (3.3v)
    <<2:32/little, Gpio:32/little, 2:32/little,
      0:32/little>>.

%%
%% pigpio response
%%

parse(Msg) -> parse(#{}, Msg).

parse(Map, <<>>) -> Map;
parse(Map,
      <<0:32/little, _, P2:32/little, 0:32/little,
        Rest/binary>>) ->
    parse(maps:put(setmode, P2, Map), Rest);
parse(Map,
      <<1:32/little, _, _, P3:32/little, Rest/binary>>) ->
    parse(maps:put(getmode, P3, Map), Rest);
parse(Map,
      <<2:32/little, _, P2:32/little, 0:32/little,
        Rest/binary>>) ->
    parse(maps:put(setpullupdown, P2, Map), Rest);
parse(Map,
      <<3:32/little, _, _, P3:32/little, Rest/binary>>) ->
    parse(maps:put(read, P3, Map), Rest);
parse(Map,
      <<4:32/little, _, P2:32/little, 0:32/little,
        Rest/binary>>) ->
    parse(maps:put(write, P2, Map), Rest);
parse(Map,
      <<10:32/little, _, _, P3:32/little, Rest/binary>>) ->
    parse(maps:put(readbits, P3, Map), Rest);
parse(Map,
      <<17:32/little, _, _, P3:32/little, Rest/binary>>) ->
    parse(maps:put(hver, P3, Map), Rest);
parse(Map, Response) -> maps:put(error, Response, Map).

%%----------------------------------------------------------------------
%% Unit tests
%%----------------------------------------------------------------------
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

command_test() ->
    <<17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>> =
        command({hver}),
    <<10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>> =
        command({br1}),
    <<3, 0, 0, 0, 25, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>> =
        command({read, 25}),
    <<1, 0, 0, 0, 25, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>> =
        command({getmode, 25}),
    <<0, 0, 0, 0, 25, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>> =
        command({setmode, 25, 0}),
    <<0, 0, 0, 0, 25, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0>> =
        command({setmode, 25, 1}),
    <<4, 0, 0, 0, 25, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>> =
        command({write, 25, 0}),
    <<2, 0, 0, 0, 25, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>> =
        command({setpullupdown, 25, 0}),
    <<2, 0, 0, 0, 25, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0>> =
        command({setpullupdown, 25, 1}),
    <<2, 0, 0, 0, 25, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0>> =
        command({setpullupdown, 25, 2}).

parse_test() ->
    #{} = parse(<<>>),
    #{setmode := 1} = parse(<<0:32/little, "_", 1:32/little,
                              0:32/little>>),
    #{getmode := 1} = parse(<<1:32/little, "_", "_",
                              1:32/little>>),
    #{setpullupdown := 1} = parse(<<2:32/little, "_",
                                    1:32/little, 0:32/little>>),
    #{read := 1} = parse(<<3:32/little, "_", "_",
                           1:32/little>>),
    #{write := 1} = parse(<<4:32/little, "_", 1:32/little,
                            0:32/little>>),
    #{readbits := 1} = parse(<<10:32/little, "_", "_",
                               1:32/little>>),
    #{hver := 1} = parse(<<17:32/little, "_", "_",
                           1:32/little>>),
    #{error := test} = parse(test).

-endif.
