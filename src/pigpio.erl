-module(pigpio).

-author(skvamme).

-author(mmalmsten).

-behaviour(gen_server).

-export([start_link/1]).

-export([handle_call/3, handle_cast/2, handle_info/2,
	 init/1]).

start_link(Gpio) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Gpio,
			  []).

init(Gpio) ->
    application:start(inets),
    {ok, Socket} = gen_tcp:connect("127.0.0.1", 8888,
				   [binary, {packet, 0}]),
    Socket = false,
    {ok, #{socket => Socket, gpio => Gpio, data => false}}.

handle_call(read, _, State) ->
    #{data := Data} = State, {reply, Data, State};
handle_call(Msg, _From, State) ->
    io:format("Msg ~p~n", [Msg]), {reply, Msg, State}.

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
    timer:apply_interval(Time, gen_tcp, send,
			 [Socket, command({read, Gpio})]),
    {noreply, State};
handle_cast(Msg, State) ->
    io:format("Msg ~p~n", [Msg]), {noreply, State}.

%% Receive sensor data
handle_info(Msg, State) ->
    {noreply, maps:put(data, parse(Msg), State)}.

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
command({setmode, Gpio, Mode}) ->
    % Input = 0, Output = 1
    <<0:32/little, Gpio:32/little, Mode:32/little,
      0:32/little>>;
command({write, Gpio, Level}) ->
    <<4:32/little, Gpio:32/little, Level:32/little,
      0:32/little>>;
command({setpullupdown, Gpio, Pud}) ->
    % Off = 0, Down = 1, Up (3.3v) = 2 - high/low
    <<2:32/little, Gpio:32/little, Pud:32/little,
      0:32/little>>.

%%
%% pigpio response
%%

parse(<<>>) -> parse(#{}, <<>>).

parse(Map, <<>>) -> Map;
parse(Map,
      <<0:32/little, _, P2:32/little, 0:32/little,
	Rest/binary>>) ->
    parse(maps:put(Map, setmode, P2), Rest);
parse(Map,
      <<1:32/little, _, _, P3:32/little, Rest/binary>>) ->
    parse(maps:put(Map, getmode, P3), Rest);
parse(Map,
      <<2:32/little, _, P2:32/little, 0:32/little,
	Rest/binary>>) ->
    parse(maps:put(Map, setpullupdown, P2), Rest);
parse(Map,
      <<3:32/little, _, _, P3:32/little, Rest/binary>>) ->
    parse(maps:put(Map, read, P3), Rest);
parse(Map,
      <<4:32/little, _, P2:32/little, 0:32/little,
	Rest/binary>>) ->
    parse(maps:put(Map, write, P2), Rest);
parse(Map,
      <<10:32/little, _, _, P3:32/little, Rest/binary>>) ->
    parse(maps:put(Map, readbits, P3), Rest);
parse(Map,
      <<17:32/little, _, _, P3:32/little, Rest/binary>>) ->
    parse(maps:put(Map, hver, P3), Rest);
parse(Map, Response) -> maps:put(Map, error, Response).
