%% @author Ionut Trancioveanu,
%% @copyright Framework Studio
%% @version v0.1
%% @doc Created: 15-Nov-2011, Gui_info displays messages dialogs of corresponding buttons.

-module(gui_info).
-author("Ionut Trancioveanu").
-compile(export_all).
-include_lib("wx/include/wx.hrl").

%% @doc About dialog box 
about(4, Frame) ->
    Str = string:join(["\t\tF-torrent\n\n\n\n",
		       "Framework Studio © 2011\n",
		       "Some Rights Reserved.\n\n",
		       "F-torrent is a university project, peer to peer file sharing software which is used to download torrents over the BitTorrent protocol.\n\n\n",
		       "Credits:\n",
		       "Batbilig Bavuudorj\n",
		       "David Giorgidze\n",
		       "Ionut Trancioveanu\n",
		       "Paulius Vysniauskas\n",
		       "Rashid Darwish\n",
		       "Zarif Jawad\n\n\n",
		       "F-torrent Project\n",
		       "System: ", wx_misc:getOsDescription(),"."], " "),

    MD = wxMessageDialog:new(Frame,
                             Str,
                             [{style, ?wxOK bor ?wxICON_INFORMATION}, 
                              {caption, "About"}]),
    wxDialog:showModal(MD),
    wxDialog:destroy(MD).

%% @doc Help dialog box
help(5, Frame) ->
    Str = string:join(["\t\tHelp\n\n",
		       "To use a software:\n\n",
		       "1. Click Open to load the torrent file.\n\n",
		       "2. Click Start to start downloading.\n\n",
		       "3. Wait untill download finishes.\n\n",
		       "File will be placed in the same directory as .torrent file.\n\n\n",
		       "Click Cancel to stop downloading the current torrent.\n",
		       "And start all over again from Step 1.\n\n",
		       "Note: You can open and play the file, even before it is downloaded.\n",
		       "(Not all video/audio players support this).\n"], " "),

    MDP = wxMessageDialog:new(Frame,
			      Str,
			      [{style, ?wxOK bor ?wxICON_INFORMATION}, 
			       {caption, "Help!"}]),
    wxDialog:showModal(MDP),
    wxDialog:destroy(MDP).

%% @doc Displaying content of error messages.
error_message(2,  Frame) ->
    Str = string:join(["Please select a new torrent file "], "Previous torrent file canceled !"),
    MDE = wxMessageDialog:new(Frame,
			      Str,
			      [{style, ?wxOK bor ?wxICON_ERROR},
			       {caption, " Error Message!"}]),
    wxDialog:showModal(MDE),
    wxDialog:destroy(MDE).
