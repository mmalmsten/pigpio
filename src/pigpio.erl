-module(pigpio).

-author(skvamme).

-author(mmalmsten).

-export([read/1, start/2, write/2]).

-export([command/1, command/2, command/3, init/2,
	 loop/1]).

-define(UINT, 32 / little).

%%
%% pigpio commands
%%

% -spec start(Gpio::integer(), Type::atom() = button) -> {ok, Pid::pid()}.
start(Gpio, Type) ->
    Pid = spawn_link(?MODULE, init, [Gpio, Type]),
    register(list_to_atom(integer_to_list(Gpio)), Pid),
    {ok, Pid}.

init(Gpio, Type) ->
    application:start(inets),
    {ok, Socket} = gen_tcp:connect("127.0.0.1", 8888,
				   [binary, {packet, 0}]),
    self() ! {init, Type},
    loop(#{socket => Socket, gpio => Gpio, type => Type}).

loop(State) ->
    io:format("State ~p~n", [State]),
    #{gpio := Gpio, socket := Socket} = State,
    receive
      {init, button} ->
	  gen_tcp:send(Socket, command(setmode, Gpio, 0)),
	  gen_tcp:send(Socket, command(setpullupdown, Gpio, 2)),
	  timer:send_interval(1000, read),
	  loop(State);
      {read, Response_pid} ->
	  Response_pid ! maps:get(msg, State), loop(State);
      {tcp, _, Msg} -> parse(Msg), loop(State);
      {msg, Key, Value} -> loop(maps:put(Key, Value, State));
      read ->
	  gen_tcp:send(Socket, command(read, Gpio)), loop(State);
      X ->
	  io:format("Unexpected message ~p~n", [X]), loop(State)
    end.

% -spec read(Gpio::integer()) -> {ok, Msg}.
read(Gpio) ->
    whereis(list_to_atom(integer_to_list(Gpio))) !
      {read, self()},
    receive Msg -> Msg end.

% -spec write(Gpio::integer(), Msg:atom()) -> ok.
write(Gpio, Msg) ->
    whereis(list_to_atom(integer_to_list(Gpio))) !
      {write, Msg}.

%%
%% pigpio commands
%%

command(hver) ->
    <<17:(?UINT), 0:(?UINT), 0:(?UINT), 0:(?UINT)>>;
command(br1) ->
    <<10:(?UINT), 0:(?UINT), 0:(?UINT), 0:(?UINT)>>.

command(read, Gpio) ->
    <<3:(?UINT), Gpio:(?UINT), 0:(?UINT), 0:(?UINT)>>;
command(getmode, Gpio) ->
    <<1:(?UINT), Gpio:(?UINT), 0:(?UINT), 0:(?UINT)>>.

command(setmode, Gpio, Mode) ->
    <<0:(?UINT), Gpio:(?UINT), Mode:(?UINT),
      0:(?UINT)>>; % Input = 0, Output = 1
command(write, Gpio, Level) ->
    <<4:(?UINT), Gpio:(?UINT), Level:(?UINT), 0:(?UINT)>>;
command(setpullupdown, Gpio, Pud) ->
    <<2:(?UINT), Gpio:(?UINT), Pud:(?UINT),
      0:(?UINT)>>. % Off = 0, Down = 1, Up (3.3v) = 2 - high/low

%%
%% pigpio response
%%

parse(<<>>) -> ok;
parse(<<0:(?UINT), _P1:(?UINT), P2:(?UINT), 0:(?UINT),
	Rest/binary>>) ->
    self() ! {msg, setmode, P2}, parse(Rest);
parse(<<1:(?UINT), _P1:(?UINT), _P2:(?UINT), P3:(?UINT),
	Rest/binary>>) ->
    self() ! {msg, getmode, P3}, parse(Rest);
parse(<<2:(?UINT), _P1:(?UINT), P2:(?UINT), 0:(?UINT),
	Rest/binary>>) ->
    self() ! {msg, setpullupdown, P2}, parse(Rest);
parse(<<3:(?UINT), _P1:(?UINT), _P2:(?UINT), P3:(?UINT),
	Rest/binary>>) ->
    self() ! {msg, read, P3}, parse(Rest);
parse(<<4:(?UINT), _P1:(?UINT), P2:(?UINT), 0:(?UINT),
	Rest/binary>>) ->
    self() ! {msg, write, P2}, parse(Rest);
parse(<<10:(?UINT), _P1:(?UINT), _P2:(?UINT),
	P3:(?UINT), Rest/binary>>) ->
    self() ! {msg, readbits, P3}, parse(Rest);
parse(<<17:(?UINT), _P1:(?UINT), _P2:(?UINT),
	P3:(?UINT), Rest/binary>>) ->
    self() ! {msg, hver, P3}, parse(Rest);
parse(Response) -> self() ! {msg, error, Response}.
