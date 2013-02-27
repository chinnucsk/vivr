%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(hello_resource).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> 
  start(),
  {ok, undefined}.

to_html(ReqData, State) ->
  {hello(), ReqData, State}.

start() ->
  spawn(fun() ->
        register(ruby, self()),
        process_flag(trap_exit, true),
        Cmd = "ruby ./lib/hello_resource.rb",
        Port = open_port({spawn, Cmd}, [{packet, 4}, nouse_stdio, exit_status, binary]),
        port_loop(Port)
    end).

stop() -> ruby ! stop.

hello() ->
  ruby ! {hello, self()},
  receive
    {result, Text} -> Text
  end.

port_loop(Port) ->
  receive
    {hello, Caller} ->
      Payload = term_to_binary({hello}),
      Port ! {self(), {command, Payload}},
      Result = get_result(Port),
      Caller ! {result, Result },

      port_loop(Port);

    stop ->
      Port ! {self(), close},
      receive
        {Port, closed} -> exit(normal)
      end
  end.

get_result(Port) ->
  receive
    {Port, {data, Data}} ->
      {result, Text} = binary_to_term(Data),
      io_lib:format("~p~n", [Text]);
    {'EXIT', Port, Reason} ->
      exit({port_terminated,Reason})
  end.
