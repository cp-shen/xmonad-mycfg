--

module XMonad.MyCfg.Workspaces
  ( myWsList,
    ws1, ws2, ws3, ws4, ws5,
    ws6, ws7, ws8, ws9, ws10,
    wsCode,
    wsDownloads,
    wsMusic,
    wsTerminal,
    wsWebpages,
  )
where

ws1 = "term"
ws2 = "web"
ws3 = "code"
ws4 = "mus"
ws5 = "dow"
ws6 = "ws6"
ws7 = "ws7"
ws8 = "ws8"
ws9 = "ws9"
ws10 = "ws10"

wsTerminal = ws1
wsWebpages = ws2
wsCode = ws3
wsMusic = ws4
wsDownloads = ws5

myWsList =
  [ ws1, ws2, ws3, ws4, ws5,
    ws6, ws7, ws8, ws9, ws10
  ]
