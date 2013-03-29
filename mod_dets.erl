-module(mod_dets).
-author("Arjan Scherpenisse").

-mod_title("DETS term storage").
-mod_description("Implement Erlang key/value storage through DETS tables").
-mod_prio(20).

-include_lib("zotonic.hrl").

-export([init/1]).

%%====================================================================
%% support functions go here
%%====================================================================

init(_Context) ->
    ok.
