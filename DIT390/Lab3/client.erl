-module(client).
-export([loop/2, initial_state/2]).
-include_lib("./defs.hrl").

%% Produce initial state
initial_state(Nick, GUIName) ->
    #client_st { gui = GUIName, name = Nick }.

%% ---------------------------------------------------------------------------

%% loop handles each kind of request from GUI

%% Connect to server
loop(St, {connect, Server}) ->
    % {ok, St} ;
    {{error, not_implemented, "Not implemented"}, St} ;

%% Disconnect from server
loop(St, disconnect) ->
    % {ok, St} ;
    {{error, not_implemented, "Not implemented"}, St} ;

% Join channel
loop(St, {join, Channel}) ->
    % {ok, St} ;
    {{error, not_implemented, "Not implemented"}, St} ;

%% Leave channel
loop(St, {leave, Channel}) ->
    % {ok, St} ;
    {{error, not_implemented, "Not implemented"}, St} ;

% Sending messages
loop(St, {msg_from_GUI, Channel, Msg}) ->
    case St#client_st.server of
        not_connected   -> {{error,not_connected,"Not connected to a server"},St};
        Server          -> 
            case catch(gen_server:request(Server, {St#client_st.name, Channel, Msg})) of
                {"EXIT","Timeout"} -> {{error,timeout,"Request timed out"}, St};
                {"EXIT",Reason}    -> {{error,error,Reason}, St};
                ok                 -> {ok,St}
            end
    end;
    % {ok, St} ;
    % {{error, not_implemented, "Not implemented"}, St} ;

%% Get current nick
loop(St, whoami) ->
    case St#client_st.server of
        not_connected   -> {{error,not_connected,"Not connected to a server"},St};
        Server          -> 
            case catch(gen_server:request(Server, whoami)) of
                {"EXIT","Timeout"}  -> {{error,timeout,"Request timed out"}, St};
                {"EXIT",Reason}     -> {{error,error,Reason}, St};
                {nick,Nick}         -> {Nick,St}
            end
    end;

            

    % {"nick", St} ;
    % {{error, not_implemented, "Not implemented"}, St} ;

%% Change nick
loop(St, {nick, Nick}) ->
    case St#client_st.server of
        not_connected   -> {{error,not_connected,"Not connected to a server"},St};
        Server          -> 
            case catch(gen_server:request(Server, {nick, Nick})) of
                {"EXIT","Timeout"}  -> {{error,timeout,"Request timed out"}, St};
                {"EXIT",Reason}     -> {{error,error,Reason}, St};
                {nick,Nick}         -> {ok,St#client_st{name=Nick}}
            end
    end;
    % {ok, St} ;
    % {{error, not_implemented, "Not implemented"}, St} ;

%% Incoming message
loop(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.
