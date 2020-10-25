-module(pigpio).

-author(skvamme).

-author(mmalmsten).

-behaviour(gen_server).

-export([connect/3, read/1, write/2]).

-export([handle_call/3, handle_cast/2, handle_info/2,
	 init/1, start_link/3]).

%%
%% pigpio commands
%%

connect(Ip, Port, Gpio) ->
    {ok, Pid} = start_link(Ip, Port, Gpio),
    register(list_to_atom(integer_to_list(Gpio)), Pid),
    {ok, Pid}.

read(Pid) -> gen_server:call(Pid, read).

write(Pid, Msg) -> gen_server:call(Pid, {write, Msg}).

%%
%% Handle gen_server and tcp connection
%%

start_link(Ip, Port, Gpio) ->
    {ok, Pid} = gen_server:start_link(?MODULE,
				      {Ip, Port, Gpio}, []),
    {ok, Pid}.

init(_) -> {ok, []}.

% init({_Ip, _Port, _Gpio}) ->
    % application:start(inets),
    % TODO: calculate magic number from "Gpio" here
    % Bits = 33554432,
    % {ok, Socket} = gen_tcp:connect(Ip, Port,

        % 			   [binary, {packet, 0}]),

    % ok = gen_tcp:send(Socket,
        % 	      <<0:32, Gpio:32, 0:32, 0:32>>),

    % ok = gen_tcp:send(Socket,
        % 	      <<1:32, Gpio:32, 0:32, 0:32>>),

    % ok = gen_tcp:send(Socket, <<99:32, 0:32, 0:32, 0:32>>),
    % ok = gen_tcp:send(Socket,
        % 	      <<19:32, 0:32, Bits:32, 0:32>>),

    % {ok, [Socket]}.

handle_call(read, _, State) ->
    {reply, round(rand:uniform()) == 1,
     State}; % Simulator for now
handle_call({write, Msg}, _, State) ->
    {reply, Msg, State};
handle_call(terminate, _, State) ->
    {stop, normal, ok, State}.

handle_cast(Msg, State) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, State}.

%%
% pigpio response
%%

% parse(_Socket, <<>>) -> ok;
% parse(Socket,
%       <<0:32, P1:32, P2:32, 0:32, Rest/binary>>) ->
%     io:format("setmode pin ~p mode ~p~n", [P1, P2]),
%     parse(Socket, Rest);
% parse(Socket,
%       <<1:32, P1:32, _P2:32, P3:32, Rest/binary>>) ->
%     io:format("getmode pin ~p mode ~p~n", [P1, P3]),
%     parse(Socket, Rest);
% parse(Socket,
%       <<2:32, P1:32, P2:32, 0:32, Rest/binary>>) ->
%     io:format("setpullupdown pin ~p pud ~p~n", [P1, P2]),
%     parse(Socket, Rest);
% parse(Socket,
%       <<3:32, _P1:32, _P2:32, P3:32, Rest/binary>>) ->
%     io:format("read ~p~n", [P3]), parse(Socket, Rest);
% parse(Socket,
%       <<4:32, P1:32, P2:32, 0:32, Rest/binary>>) ->
%     io:format("write pin ~p level ~p~n", [P1, P2]),
%     parse(Socket, Rest);
% parse(Socket,
%       <<10:32, _P1:32, _P2:32, P3:32, Rest/binary>>) ->
%     io:format("readbits 0-31 ~.2B~n", [P3]),
%     parse(Socket, Rest);
% parse(Socket,
%       <<17:32, _P1:32, _P2:32, P3:32, Rest/binary>>) ->
%     io:format("hver ~p~n", [P3]), parse(Socket, Rest);
% parse(_Socket, Response) ->
%     io:format("Error: ~w~n", [Response]).

