%% Author: Ionut Trancioveanu
%% This module displays simple GUI of a software

-module(gui).
-export([start/0, new_window/0, loop/1]).
-include_lib("wx/include/wx.hrl").

start()->
    State = new_window(),
    loop(State).

new_window()->
    Server = wx:new(),
    Frame = wxFrame:new(Server,-1,"F-torrent",[{pos,{300,150}},{size,{800,555}}]),    
    Panel = wxPanel:new(Frame),

%% Creating the Widgets which will be showed on the frame.   
  
    Image = wxBitmap:new("Ftorrent.jpg",[{type,?wxBITMAP_TYPE_JPEG}]), 
    StaticBitmap = wxStaticBitmap:new(Panel,5,Image),
    FilePickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,[{label, "File-Open"}]),
    FilePicker = wxFilePickerCtrl:new(Panel, 2, [{path, "/"}]),
    wxFilePickerCtrl:connect(FilePicker, command_filepicker_changed, []),
    StaticText = wxStaticText:new(Panel, 2,"Download status", []),
    ButtonStart  = wxButton:new(Panel, 3, [{label, "&Play"}]),
    ButtonStop  = wxButton:new(Panel, ?wxID_EXIT, [{label, "&Stop"}]),
    Gauge = wxGauge:new(Panel,1,100,[{size,{200,-1}},{style,?wxGA_HORIZONTAL}]),
    wxGauge:setValue(Gauge, 80),

%% Create Sizers.    
   
    MainSizer  = wxBoxSizer:new(?wxVERTICAL),
    InputSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "File type"}]),
    ButtonSizer = wxBoxSizer:new(?wxVERTICAL),
    OuterSizer   = wxBoxSizer:new(?wxHORIZONTAL),
    PickerOptions = [{border, 4},{flag, ?wxALL bor ?wxEXPAND}],

%% Adding the Widgets, using the Variable, to Sizers and Spacers.

    wxSizer:add(FilePickerSizer, FilePicker, PickerOptions),
    wxSizer:add(InputSizer,StaticBitmap,[]),
    wxSizer:add(InputSizer, FilePickerSizer, [{flag,?wxEXPAND bor ?wxALL}]),
    wxSizer:add(InputSizer, 250,20,[]),
    wxSizer:add(InputSizer, StaticText, []),
    wxSizer:add(InputSizer,Gauge,[]),
    wxSizer:addSpacer(MainSizer,50),
    wxSizer:add(MainSizer, InputSizer,[]),
    wxSizer:addSpacer(MainSizer, 10),
    wxSizer:add(ButtonSizer, ButtonStart,[]),
    wxSizer:addSpacer(MainSizer, 10),
    wxSizer:add(ButtonSizer, ButtonStop,[]),
    wxSizer:addSpacer(OuterSizer, 200),
    wxSizer:add(OuterSizer, MainSizer, []),
    wxSizer:add(MainSizer, ButtonSizer, []),

%% Setting the OuterSizer into the Panel and show the Frame.
    
    wxPanel:setSizer(Panel,OuterSizer),
    wxFrame:show(Frame).

%% Create a loop which receives messages and respond to them.

loop(State)->
    State,
    ok.
