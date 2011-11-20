-module(gui).
-author("Ionut Trancioveanu").
-export([start/1,new_window/0,loop/2]).
-import(info,[about/2,help/2]).
-include_lib("wx/include/wx.hrl").

start(Manager)->
    State = new_window(),
    loop(State,Manager).

%%%%%%% Creating a new window/frame and a panel. 

new_window()->
    Server = wx:new(),
    Frame  = wxFrame:new(Server,-1,"F-torrent",[{pos,{300,150}},{size,{650,500}}]),
    Icon = wxIcon:new("bg.ico",[{type,?wxBITMAP_TYPE_ICO}]),
    wxFrame:setIcon(Frame,Icon),
    Panel  = wxPanel:new(Frame),
    wxPanel:setBackgroundColour(Panel,{204,204,204}),

%%%%%% Creating the Widgets which will be showed on the frame/panel. 

    BitmapPause  = wxBitmap:new("Pause.png",    [{type,?wxBITMAP_TYPE_PNG}]),
    BitmapCancel = wxBitmap:new("Cancel.png",   [{type,?wxBITMAP_TYPE_PNG}]),
    BitmapOpen   = wxBitmap:new("Openfile.png", [{type,?wxBITMAP_TYPE_PNG}]),
    Logo         = wxBitmap:new("Logo.png",     [{type,?wxBITMAP_TYPE_PNG}]),
    %% Image        = wxBitmap:new("image.jpg",   [{type,?wxBITMAP_TYPE_JPEG}]),
    %% Audio        = wxBitmap:new("audio.jpg",   [{type,?wxBITMAP_TYPE_JPEG}]),
    %% Video        = wxBitmap:new("video.jpg",   [{type,?wxBITMAP_TYPE_JPEG}]),
    BitmapAbout  = wxBitmap:new("about.png",    [{type,?wxBITMAP_TYPE_PNG}]),
    BitmapHelp   = wxBitmap:new("help.png",     [{type,?wxBITMAP_TYPE_PNG}]),
    Type         = Logo,
    StaticBitmap = wxStaticBitmap:new(Panel,12,Type),
    StaticText   = wxStaticText:new  (Panel, 21,"Download status", []),
    StaticText1  = wxStaticText:new  (Panel, 31,"Name: ", []),
    StaticText11 = wxStaticText:new  (Panel, 41,"     Music.mp3 ", []),
    StaticText2  = wxStaticText:new  (Panel, 51,"Size: ", []),
    StaticText22 = wxStaticText:new  (Panel, 61,"        1,2 Mb ", []),
    ButtonOpen   = wxBitmapButton:new(Panel, 1 , BitmapOpen),
    ButtonPause  = wxBitmapButton:new(Panel, 2 ,BitmapPause),
    ButtonCancel = wxBitmapButton:new(Panel, 3,BitmapCancel),
    ButtonAbout  = wxBitmapButton:new(Panel, 4 ,BitmapAbout),
    ButtonHelp   = wxBitmapButton:new(Panel, 5 , BitmapHelp),

%%%%%% Creating listeners for the buttons.

    wxBitmapButton:connect(ButtonOpen,  command_button_clicked),
    wxBitmapButton:connect(ButtonPause, command_button_clicked),
    wxBitmapButton:connect(ButtonCancel,command_button_clicked),
    wxBitmapButton:setToolTip(ButtonOpen,  "Add torrent file "),
    wxBitmapButton:setToolTip(ButtonPause,     "Start torrent"),
    wxBitmapButton:setToolTip(ButtonCancel,     "Stop torrent"),
    Range = 100,
    Gauge = wxGauge:new(Panel,1,Range,[{size,{260,-1}},{style,?wxGA_HORIZONTAL}]),
    Value = 0,
    wxGauge:setValue(Gauge, Value),

    Font1 = wxFont:new(15, ?wxMODERN, ?wxNORMAL, ?wxBOLD),
    Font2 = wxFont:new(10, ?wxNORMAL, ?wxNORMAL, ?wxBOLD),
    wxStaticText:setFont(StaticText1,Font1),
    wxStaticText:setFont(StaticText2,Font1),
    wxStaticText:setFont(StaticText,Font2),

%%%%%% Creating Notebook in which the Info about torrent file will be shown.

    Notebook = wxNotebook:new(Panel, 1, [{style, ?wxBK_DEFAULT}]),
    %% wxImageList is for displaying icons in the tab field
    IL = wxImageList:new(16,16),
    wxImageList:add(IL, wxArtProvider:getBitmap("wxART_TICK_MARK",       [{size, {16,16}}])),
    wxImageList:add(IL, wxArtProvider:getBitmap("wxART_INFORMATION",     [{size, {16,16}}])),
    wxImageList:add(IL, wxArtProvider:getBitmap("wxART_LIST_VIEW",       [{size, {16,16}}])),
    wxImageList:add(IL, wxArtProvider:getBitmap("wxART_FILE_OPEN",       [{size, {16,16}}])),
    wxImageList:add(IL, wxArtProvider:getBitmap("wxART_EXECUTABLE_FILE", [{size, {16,16}}])),
    wxNotebook:assignImageList(Notebook, IL),
 %% First tab field ("General").
    Win1 = wxPanel:new(Notebook, []),
    wxPanel:setBackgroundColour(Win1, {128,128,128}),
    Win1Text = wxStaticText:new(Win1, ?wxID_ANY, "        
       Download:     ...
       Availability: ... ",[{pos, {50, 100}}]),
    wxStaticText:setForegroundColour(Win1Text, ?wxWHITE),
    Sizer1 = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(Sizer1, Win1Text),
    wxPanel:setSizer(Win1, Sizer1),
    wxNotebook:addPage(Notebook, Win1, "General", []),
    wxNotebook:setPageImage(Notebook, 0, 1),   
 %% Second tab field ("Tracker").
    Win2 = wxPanel:new(Notebook, []),
    wxPanel:setBackgroundColour(Win2, {128,128,128}),
    Win2Text = wxStaticText:new(Win2, ?wxID_ANY, "
    Information.",[{pos, {50, 100}}]),
    wxStaticText:setForegroundColour(Win2Text, ?wxWHITE),
    Sizer2 = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(Sizer2, Win2Text),
    wxPanel:setSizer(Win2, Sizer2),
    wxNotebook:addPage(Notebook, Win2, "Tracker", []),
    wxNotebook:setPageImage(Notebook, 1,4),
 %% Third tab field ("Peers").
    Win3 = wxPanel:new(Notebook, []),
    wxPanel:setBackgroundColour(Win3, {128,128,128}),
    Win3Text = wxStaticText:new(Win3, ?wxID_ANY, "    
     Peers List: ",[{pos, {50, 100}}]),
    wxStaticText:setForegroundColour(Win3Text, ?wxWHITE),
    Sizer3 = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(Sizer3, Win3Text),
    wxPanel:setSizer(Win3, Sizer3),
    wxNotebook:addPage(Notebook, Win3, "Peers", []),
    wxNotebook:setPageImage(Notebook, 2,2),
 %% Fourth tab field ("File").
    Win4 = wxPanel:new(Notebook, []),
    wxPanel:setBackgroundColour(Win4, {128,128,128}),
    Win4Text = wxStaticText:new(Win4, ?wxID_ANY, " 
     File Name : .mp3",[{pos, {50, 100}}]),
    wxStaticText:setForegroundColour(Win4Text, ?wxWHITE),
    Sizer4 = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(Sizer4, Win4Text),
    wxPanel:setSizer(Win4, Sizer4),
    wxNotebook:addPage(Notebook, Win4, "File", []),
    wxNotebook:setPageImage(Notebook, 3, 3),
 %% Fifth tab field ("Speed").
    Win5 = wxPanel:new(Notebook, []),
    wxPanel:setBackgroundColour(Win5, {128,128,128}),
    Win5Text = wxStaticText:new(Win5, ?wxID_ANY, "
    speed",[{pos, {50, 100}}]),
    wxStaticText:setForegroundColour(Win5Text, ?wxWHITE),
    Sizer5 = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(Sizer5, Win5Text),
    wxPanel:setSizer(Win5, Sizer5),
    wxNotebook:addPage(Notebook, Win5, "Speed",[]),
    wxNotebook:setPageImage(Notebook, 4, 0),

%%%%%% Create Sizers.
    
    MainSizer     = wxBoxSizer:new(?wxHORIZONTAL),
    InputSizer    = wxBoxSizer:new(?wxHORIZONTAL),
    InputSizer1   = wxBoxSizer:new  (?wxVERTICAL),
    TextSizer     = wxBoxSizer:new  (?wxVERTICAL),
    TextSizer1    = wxBoxSizer:new(?wxHORIZONTAL),
    ButtonSizer   = wxBoxSizer:new  (?wxVERTICAL),
    ButtonSizer1  = wxBoxSizer:new(?wxHORIZONTAL),
    SpaceSizer    = wxBoxSizer:new(?wxHORIZONTAL),
    OuterSizer    = wxBoxSizer:new  (?wxVERTICAL),
    
%%%%%% Adding the Widgets, using the Variable, to Sizers and Spacers.
     
    wxSizer:add(MainSizer, InputSizer,   []),
    wxSizer:add(ButtonSizer1,ButtonSizer,[]),
    wxSizer:add(TextSizer1,TextSizer,    []),
    wxSizer:add(InputSizer,SpaceSizer ,  []),
    wxSizer:add(InputSizer,ButtonSizer1 ,[]),
    wxSizer:add(InputSizer,TextSizer1,   []),
    wxSizer:add(InputSizer,InputSizer1,  []),
    wxSizer:addSpacer(SpaceSizer ,  40),
    wxSizer:addSpacer(ButtonSizer1, 50),
    wxSizer:addSpacer(ButtonSizer,  40),
    wxSizer:add(ButtonSizer,ButtonOpen , []),
    wxSizer:addSpacer(ButtonSizer,   5),
    wxSizer:addSpacer(ButtonSizer,  30),
    wxSizer:add(ButtonSizer,ButtonPause, []),
    wxSizer:addSpacer(ButtonSizer,   5),
    wxSizer:add(ButtonSizer,ButtonCancel,[]),
    wxSizer:addSpacer(ButtonSizer,  40),
    wxSizer:addSpacer(InputSizer , -50),
    wxSizer:add(InputSizer,ButtonAbout , []),
    wxSizer:addSpacer(InputSizer ,   5),
    wxSizer:add(InputSizer,ButtonHelp  , []),
    wxSizer:addSpacer(TextSizer1 ,  30),
    wxSizer:addSpacer(TextSizer  ,  40),
    wxSizer:add(TextSizer, StaticText1 , []),
    wxSizer:addSpacer(TextSizer  ,  20),
    wxSizer:add(TextSizer, StaticText11, []),
    wxSizer:addSpacer(TextSizer  ,  20),
    wxSizer:add(TextSizer, StaticText2 , []),
    wxSizer:addSpacer(TextSizer  ,  20),
    wxSizer:add(TextSizer, StaticText22, []),
    wxSizer:addSpacer(TextSizer  ,  40),
    wxSizer:add(TextSizer, StaticText  , []),
    wxSizer:addSpacer(TextSizer  ,  60),
    wxSizer:addSpacer(InputSizer ,  30),
    wxSizer:addSpacer(InputSizer1,  40),
    wxSizer:add(InputSizer1,StaticBitmap,[]),
    wxSizer:addSpacer(InputSizer1,  20),
    wxSizer:add(InputSizer1,Gauge      , []),
    wxSizer:addSpacer(OuterSizer ,  10),
    wxSizer:addSpacer(MainSizer  ,  40),
    wxSizer:add(OuterSizer,MainSizer   , []),
    wxSizer:add(OuterSizer,Notebook,[{proportion, 1},{flag, ?wxEXPAND}]),
    wxNotebook:connect(Notebook, command_notebook_page_changed,
		       [{skip, true}]),

%%%%%% Setting the OuterSizer into the Panel and show the Frame.
    
    wxPanel:setSizer(Panel,OuterSizer),
    wxFrame:show(Frame),

%%%%%% Create the listeners.

     wxFrame:connect( Frame, close_window),
     wxPanel:connect(Panel, command_button_clicked),

%%%%%% Returned value from the State. 

    {Frame, StaticText11, StaticText22,  Win1Text, Win2Text, Win3Text}.

%%%%%% Create a loop which receives messages and respond to them.

loop(State,Manager)->
    {Frame, StaticText11, StaticText22,  Win1Text, Win2Text, Win3Text}= State,

    receive      %% Receiving the close message which is sent to server.
	#wx{event=#wxClose{}} ->   
	    Manager ! {stop},
            io:format("~p Frame destroyed, memory released.. ~n",[self()]),
             wxFrame:destroy(Frame),  %% Close the window.
            ok;       %% Exit the loop.
        #wx{id= 1, event=#wxCommand{type=command_button_clicked}} ->
	    FD=wxFileDialog:new(Frame,[{message,"   Select torrent file to open  "}]),
	    case wxFileDialog:showModal(FD) of
		?wxID_OK ->      %% Open is clicked show the Dialog.
		    Filename = wxFileDialog:getFilename(FD),
		    Path = wxFileDialog:getPath(FD),     
		    io:format("File Path: ~p~n ", [Path]),
		    io:format(" File name: ~p~n " , [Filename] ),
		    Manager ! {start_manager, Filename, self()},
		    wxFileDialog:destroy(FD);
	   	_ ->             %% Cancel is clicked close Dialog.
		    wxFileDialog:destroy(FD),
		    cancel
	    end,
            loop(State, Manager);
	#wx{id= 2, event=#wxCommand{type=command_button_clicked}} ->
	    %%  do something ..
            io:format("ButtonStart clicked ~n",[]),
	    Manager ! {connect, self()},
            loop(State, Manager);
	#wx{id= 3, event=#wxCommand{type=command_button_clicked}} ->
	    %%  do something ..
            io:format("ButtonStop clicked ~n", []),
            loop(State, Manager);
	#wx{id= 4, event=#wxCommand{type=command_button_clicked}} ->
	    about(4, Frame),   %% Event when button About is clicked.
            loop(State, Manager);
	#wx{id= 5, event=#wxCommand{type=command_button_clicked}} ->
	    help(5, Frame),    %% Event when button Help is clicked.
            loop(State, Manager);
	{table, Torrent_info} ->
	    wxStaticText:setLabel(StaticText11, db:read("FileName")),
	    wxStaticText:setLabel(StaticText22, integer_to_list(db:read("length") div 1048576) ++ " MB"),
	    wxStaticText:setLabel(Win1Text, "\n   File: " ++ db:read("FileName") ++ "\n   Size: " ++ integer_to_list(db:read("length") div 1048576) ++ " MB" ++"\n   Total pieces: " ++ integer_to_list(db:read("NoOfPieces"))),
	    wxStaticText:setLabel(Win2Text, "\n   Tracker: " ++ db:read("announce")),
	    loop(State, Manager);
	{peer_list, Peer_list} ->
	    wxStaticText:setLabel(Win3Text, "\n   Peers: \n" ++ Peer_list),
	    loop(State, Manager)
    end.

