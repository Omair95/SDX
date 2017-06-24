-module(gui).
-export([start/2]).
-define(width, 200).
-define(height, 200).
-include_lib("wx/include/wx.hrl").

start(Name, Master) ->
    spawn_link(fun() -> init(Name, Master) end).

init(Name, Master) ->
    Frame = make_frame(Name),
    loop(Frame, Master).

make_frame(Name) ->       %Name is the window title
    Server = wx:new(),  %Server will be the parent for the Frame
    Frame = wxFrame:new(Server, -1, Name, [{size,{?width, ?height}}]),
    wxFrame:setBackgroundColour(Frame, ?wxBLACK),
    wxFrame:show(Frame),
    %monitor closing window event
    wxFrame:connect(Frame, close_window),
    Frame.

loop(Frame, Master)->
    receive
        %check if the window was closed by the user
        #wx{event=#wxClose{}} ->
            wxWindow:destroy(Frame),  
            Master ! stop,
            ok;
        {color, Color} ->
            color(Frame, Color),
            loop(Frame, Master);
        stop ->
            ok;
        Error ->
            io:format("gui: strange message ~w ~n", [Error]),
            loop(Frame, Master)
    end.

color(Frame, Color) ->
    wxFrame:setBackgroundColour(Frame, Color),
    wxFrame:refresh(Frame).