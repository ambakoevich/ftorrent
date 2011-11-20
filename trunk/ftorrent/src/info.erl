-module(info).
-author("Ionut Trancioveanu").
-compile(export_all).
-include_lib("wx/include/wx.hrl").


about(4,  Frame) ->
    Str = string:join(["                                  --~~ FrameWork Studio group ~~--                \n\n",
		               "  ©2011 F-torrent,Inc.
    All Rights Reserved.\n\n","                   Thanks to: \n\n"

		                 " Batbilig Bavuudorj\n",
		                 "David Giorgidze\n",
		                 "Ionut Trancioveanu\n",
		                 "Paulius Vysniauskas\n",
		                 "Rashid Darwish\n",
		                 "Zarif Jawad\n\n\n","F-torrent Project\n",
		                 "System: ",    wx_misc:getOsDescription(),"."], " "),
    MD = wxMessageDialog:new(Frame,
                             Str,
                             [{style, ?wxOK bor ?wxICON_INFORMATION}, 
                              {caption, "About"}]),

    wxDialog:showModal(MD),
    wxDialog:destroy(MD).

help(5,  Frame) ->
    Str = string:join(["                                 --~~  Help! ~~--                 \n\n",
		               "  ©2011 F-torrent,Inc.
    All Rights Reserved.\n\n","    The buttons on the Window at the left of the F-Torrent interface allows you perform some basic functions, most of which apply to the currently selected torrent.
    The order of the list here reflects the order of the buttons needs. 
    If you are unsure, you can place your mouse cursor over a button, and a tooltip should pop up providing the description of the button.
     > Start will start the selected torrent job.
     || Pause will pause the selected torrent job. "], " "),

    MDP = wxMessageDialog:new(Frame,
                             Str,
                             [{style, ?wxOK bor ?wxICON_INFORMATION}, 
                              {caption, "Help!"}]),

    wxDialog:showModal(MDP),
    wxDialog:destroy(MDP).
