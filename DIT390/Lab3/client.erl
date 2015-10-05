-module(client).
-export([loop/2, initial_state/2]).
-include_lib("./defs.hrl").

%% Produce initial state
initial_state(Nick, GUIName) ->
    #client_st { gui = GUIName, name = Nick, channels=[] }.

%% ---------------------------------------------------------------------------

%% loop handles each kind of request from GUI

%% Connect to server
loop(St, {connect, Server}) ->
    case catch(genserver:request(list_to_atom(Server), {connect, St#client_st.name, self()})) of
        ok -> {ok, St#client_st{server=list_to_atom(Server)}};
        user_already_connected -> {{error, user_already_connected, "You are already connected."}, St};
        server_not_reached     -> {{error, server_not_reached, "Server couldn't be reached."}, St};
        name_taken             -> {{error, user_already_connected, "Your name is already taken."}, St};
        {'EXIT', {badarg, _}}  -> {{error, server_not_reached, "Server couldn't be reached."}, St}
    end;

%% Disconnect from server
loop(St, disconnect) -> 
    case St#client_st.server of
        not_connected   -> {{error,user_not_connected,"Not connected to a server"},St};
        Server          -> 
            case genserver:request(Server, {disconnect, St#client_st.name}) of
                ok -> {ok, St#client_st{server=not_connected}};
                user_not_connected -> {{error, user_not_connected, "You are not connected."}, St};
                leave_channels_first -> {{error, leave_channels_first, "Leave channels first."}, St}
            end
    end;

% Join channel
loop(St, {join, Channel}) ->
    case St#client_st.server of
        not_connected   -> {{error,user_not_connected,"Not connected to a server"},St};
        Server          -> 
            case genserver:request(Server, {join, Channel, self()}) of
                {ok, Pid} -> {
                    ok, 
                    St#client_st{
                        channels=[
                            #channel{
                                name = Channel,
                                pid = Pid
                            } | St#client_st.channels
                        ]
                    }
                };
                user_already_joined -> {{
                    error, 
                    user_already_joined, 
                    "You have already joined that channel."
                }, St}
            end
    end;

%% Leave channel
loop(St, {leave, Channel}) ->
    case St#client_st.server of
            not_connected   -> {{error,user_not_connected,"Not connected to a server"},St};
            Server          -> 
                case genserver:request(Server, {leave, Channel, self()}) of
                    ok -> {
                        ok, 
                        St#client_st{
                            channels=lists:keydelete(Channel,3 ,St#client_st.channels)
                        }
                    };
                    user_not_joined -> {{error, user_not_joined, "You havn't joined that channel."}, St}
            end
    end;

% Sending messages
loop(St, {msg_from_GUI, Channel, Msg}) ->
    case lists:keyfind(Channel, 2, St#client_st.channels) of
        false -> {{error, user_not_joined, "You are not in that channel."}, St};
        Chan  -> 
            ChanPid = Chan#channel.pid,
            case catch(genserver:request(ChanPid, {send_message, #user{name=St#client_st.name,pid=self()}, Msg})) of
                ok                 -> {ok,St};
                {"EXIT","Timeout"} -> {{error, timeout, "Request timed out"}, St};
                {"EXIT",Reason}    -> {{error, error, Reason}, St}
            end
    end;




%% Get current nick
loop(St, whoami) ->
    case St#client_st.server of
        not_connected   -> {St#client_st.name, St};
        Server          -> 
            case catch(genserver:request(Server, {whoami, self()})) of
                {"EXIT","Timeout"}  -> {{error,timeout,"Request timed out"}, St};
                {"EXIT",Reason}     -> {{error,error,Reason}, St};
                {nick,Nick}         -> {Nick,St};
                Response            -> {{error,unrecognised_response,Response}}
            end
    end;

%% Change nick
loop(St, {nick, Nick}) ->
    case St#client_st.server of
        not_connected   -> {ok,St#client_st{name=Nick}};
        Server          -> 
            case catch(genserver:request(Server, {nick, Nick, self()})) of
                ok                  -> {ok,St#client_st{name=Nick}};
                nick_taken          -> {{error,nick_taken,"That nick is already taken"}, St};
                {"EXIT","Timeout"}  -> {{error,timeout,"Request timed out"}, St};
                {"EXIT",Reason}     -> {{error,error,Reason}, St}
            end
    end;



%% Incoming message
loop(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.
