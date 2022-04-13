-module(simple_web_server).

-compile([export_all, nowarn_export_all]).

-define(EXAMPLE_PAGE,
    "HTTP/1.1 200 OK
Date: Mon, 23 May 2005 22:38:34 GMT
Content-Type: text/html; charset=UTF-8
Content-Length: 155
Last-Modified: Wed, 08 Jan 2003 23:11:55 GMT
Server: Apache/1.3.3.7 (Unix) (Red-Hat/Linux)
ETag: \"3f80f-1b6-3e1cb03b\"
Accept-Ranges: bytes
Connection: close

<html>
  <head>
    <title>An Example Page</title>
  </head>
  <body>
    <p>Hello World, this is a very simple HTML document.</p>
  </body>
</html>"
).

start_simple_server() ->
    {ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4},
                                         {reuseaddr, true},
                                         {active, true}]),

    {ok, Socket} = gen_tcp:accept(Listen),
    gen_tcp:close(Listen),
    loop(Socket).

loop(Socket) ->
    io:format("loop"),
    receive
            X ->
                io:format("X = ~p~n", X);
            {tcp, Socket, Bin} ->
            % {tcp, Socket, Bin} ->
                io:format("Server received binary = ~p~n", [Bin]),
                Str = binary_to_term(Bin),
                io:format("Server (unpacked) ~p~n", [Str]),
                _Reply = lib_misc:string2value(Str),

                Reply2 = ?EXAMPLE_PAGE,

                io:format("Server replyíng = ~p~n", [Reply2]),
                gen_tcp:send(Socket, term_to_binary(Reply2)),
                io:format("Closing");
                %loop(Socket);
            {tcp_closed, Socket} ->
                io:format("Server socket closed~n")
    end.