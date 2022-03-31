-- |

module XMonad.MyCfg.Workspaces (
  myWsList,
  ws1 , ws2 , ws3 , ws4 , ws5,
  ws6 , ws7 , ws8 , ws9 , ws10,
  wsDownloads,
  wsTerminal,
  wsWebpages,
  wsSystem,
  wsMusic,
  wsEmacs,)
  where

ws1  = "term"
ws2  = "web"
ws3  = "sys"
ws4  = "mus"
ws5  = "dow"
ws6  = "ws6"
ws7  = "ws7"
ws8  = "ws8"
ws9  = "ws9"
ws10 = "emacs"

wsTerminal   = ws1
wsWebpages   = ws2
wsSystem     = ws3
wsMusic      = ws4
wsDownloads  = ws5
wsEmacs      = ws10

myWsList = [
    ws1 , ws2 , ws3 , ws4 , ws5
  , ws6 , ws7 , ws8 , ws9 , ws10
  ]
