-module(rabbit_net_app).

-behaviour(application).

%% Application callbacks
%%-export([start/2, stop/1]).
-compile(export_all).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, MainSupPid} = rabbit_net_sup:start_link(),
    boot_net(),
    {ok, MainSupPid}.


stop(_State) ->
    ok.


boot_net() ->
    record_distribution_listener(),
    ok = start(),
    ok = boot_tcp().

record_distribution_listener() ->
    {Name, Host} = node_parts(erlang:node()),
    {port, Port, _} = erl_epmd:port_please(Name, Host),
    tcp_listener_started(clustering, {0, 0, 0, 0, 0, 0, 0, 0}, Port).

start() ->
    rabbit_net_sup:start_supervisor_child(rabbit_tcp_client_sup, rabbit_client_sup,
                                         [{local, rabbit_tcp_client_sup},
                                          {rabbit_connection_sup, start_link, []}]).

boot_tcp() ->
    TcpListeners = application:get_env(rabbit_net, tcp_listeners, [19001]),
    [ok = start_tcp_listener(L) || L <- TcpListeners],
    ok.

%% 'redink@127.0.0.1' => {"redink", "127.0.0.1"}
node_parts(Node) when erlang:is_atom(Node)->
    node_parts(erlang:atom_to_list(Node));
node_parts(Node) ->
    case lists:splitwith(fun(E) -> E =/= $@ end, Node) of
        {Prefix, []} ->
            {_, Suffix} = node_parts(erlang:node()),
            {Prefix, Suffix};
        {Prefix, Suffix} ->
            {Prefix, erlang:tl(Suffix)}
    end.

tcp_listener_started(_Protocol, _IPAddress, _Port) ->
    ingore.

tcp_listener_stopped(_Protocol, _IPAddress, _Port) ->
    ingore.

%% only [19001]
start_tcp_listener(Listener) ->
    start_listener(Listener, amqp, "TCP Listener", {?MODULE, start_client, []}).

%% OnConnect : after connect, execute `OnConnect` function
start_listener(Listener, Protocol, Label, OnConnect) ->
    [start_listener_do(Address, Protocol, Label, OnConnect)
     || Address <- tcp_listener_addresses(Listener)],
    ok.

%% Address = {{0,0,0,0,0,0,0,0}, 19000, inet6}
%% Protocol = amqp
%% Label = "TCP Listener"
%% OnConnect = {?MODULE, start_client, []}
start_listener_do(Address, Protocol, Label, OnConnect) ->
    Spec = tcp_listener_spec(rabbit_tcp_listener_sup, Address, tcp_opts(), Protocol, Label, OnConnect),
    case supervisor:start_child(rabbit_net_sup, Spec) of
        {ok, _} ->
            ok;
        {error, {shutdown, _}} ->
            exit({could_not_start_tcp_listener, Address})
    end.

start_client(Sock) ->
    start_client(Sock, fun(S) -> {ok, S} end).

start_client(Sock, _SockTramsfrom) ->
    {ok, Pid} = supervisor:start_child(rabbit_tcp_client_sup, []),
    ok = gen_tcp:controlling_process(Sock, Pid),
    ok = inet:setopts(Sock, [{active, once}]).


%% 19000 => [{{0,0,0,0,0,0,0,0},19000,inet6}]

tcp_listener_addresses(Port) when is_integer(Port) ->
    tcp_listener_addresses_auto(Port);
tcp_listener_addresses({"auto", Port}) ->
    tcp_listener_addresses_auto(Port);
tcp_listener_addresses({Host, Port}) ->
    tcp_listener_addresses({Host, Port, auto});
tcp_listener_addresses({Host, Port, Family0})
  when is_integer(Port) andalso (Port >= 0) andalso (Port =< 65535) ->
    [{IPAddress, Port, Family} ||
        {IPAddress, Family} <- getaddr(Host, Family0)];
tcp_listener_addresses({_Host, Port, _Family0}) ->
    throw({error, {invalid_port, Port}}).

tcp_listener_addresses_auto(Port) ->
    lists:append([tcp_listener_addresses(Listener) ||
                     Listener <- port_to_listeners(Port)]).

%% NamePrefix = rabbit_tcp_listener_sup
tcp_listener_spec(NamePrefix, {IPAddress, Port, Family}, SocketOpts,
                  Protocol, Label, OnConnect) ->
    {tcp_name(NamePrefix, IPAddress, Port),
     {tcp_listener_sup, start_link,
      [IPAddress, Port, [Family | SocketOpts],
       {?MODULE, tcp_listener_started, [Protocol]},
       {?MODULE, tcp_listener_stopped, [Protocol]},
       OnConnect, Label]},
     transient, infinity, supervisor, [tcp_listener_sup]}.

tcp_opts() ->
    [].

%% NamePrefix = rabbit_tcp_listener_sup
%% IPAddress  = {0,0,0,0,0,0,0,0}
%% Port       = 19000

%% Result = 'rabbit_tcp_listener_sup_:::19000' (atom)
tcp_name(NamePrefix, IPAddress, Port) when is_atom(NamePrefix) andalso is_number(Port) ->
    list_to_atom(format("~w_~s:~w", [NamePrefix, inet_parse:ntoa(IPAddress), Port])).

format(Fmt, Args) -> lists:flatten(io_lib:format(Fmt, Args)).

getaddr(Host, Family) ->
    case inet_parse:address(Host) of
        {ok, IPAddress} -> [{IPAddress, resolve_family(IPAddress, Family)}];
        {error, _}      -> gethostaddr(Host, Family)
    end.

port_to_listeners(Port) ->
    IPv4 = {"0.0.0.0", Port, inet},
    IPv6 = {"::",      Port, inet6},
    case ipv6_status(10000) of
        single_stack -> [IPv6];
        ipv6_only    -> [IPv6];
        dual_stack   -> [IPv6, IPv4];
        ipv4_only    -> [IPv4]
    end.

ipv6_status(TestPort) ->
    IPv4 = [inet,  {ip, {0,0,0,0}}],
    IPv6 = [inet6, {ip, {0,0,0,0,0,0,0,0}}],
    case gen_tcp:listen(TestPort, IPv6) of
        {ok, LSock6} ->
            case gen_tcp:listen(TestPort, IPv4) of
                {ok, LSock4} ->
                    %% Dual stack
                    gen_tcp:close(LSock6),
                    gen_tcp:close(LSock4),
                    dual_stack;
                %% Checking the error here would only let us
                %% distinguish single stack IPv6 / IPv4 vs IPv6 only,
                %% which we figure out below anyway.
                {error, _} ->
                    gen_tcp:close(LSock6),
                    case gen_tcp:listen(TestPort, IPv4) of
                        %% Single stack
                        {ok, LSock4}            -> gen_tcp:close(LSock4),
                                                   single_stack;
                        %% IPv6-only machine. Welcome to the future.
                        {error, eafnosupport}   -> ipv6_only; %% Linux
                        {error, eprotonosupport}-> ipv6_only; %% FreeBSD
                        %% Dual stack machine with something already
                        %% on IPv4.
                        {error, _}              -> ipv6_status(TestPort + 1)
                    end
            end;
        %% IPv4-only machine. Welcome to the 90s.
        {error, eafnosupport} -> %% Linux
            ipv4_only;
        {error, eprotonosupport} -> %% FreeBSD
            ipv4_only;
        %% Port in use
        {error, _} ->
            ipv6_status(TestPort + 1)
    end.

resolve_family({_,_,_,_},         auto) -> inet;
resolve_family({_,_,_,_,_,_,_,_}, auto) -> inet6;
resolve_family(IP,                auto) -> throw({error, {strange_family, IP}});
resolve_family(_,                 F)    -> F.

gethostaddr(Host, auto) ->
    Lookups = [{Family, inet:getaddr(Host, Family)} || Family <- [inet, inet6]],
    case [{IP, Family} || {Family, {ok, IP}} <- Lookups] of
        []  -> host_lookup_error(Host, Lookups);
        IPs -> IPs
    end;

gethostaddr(Host, Family) ->
    case inet:getaddr(Host, Family) of
        {ok, IPAddress} -> [{IPAddress, Family}];
        {error, Reason} -> host_lookup_error(Host, Reason)
    end.

host_lookup_error(Host, Reason) ->
    rabbit_log:error("invalid host ~p - ~p~n", [Host, Reason]),
    throw({error, {invalid_host, Host, Reason}}).

    