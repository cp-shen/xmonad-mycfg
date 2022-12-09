--

module XMonad.MyCfg.Workspaces
  ( myWsList,
    ws1, ws2, ws3, ws4, ws5,
    ws6, ws7, ws8, ws9, ws10,
    wsCode,
    wsDownloads,
    wsEntertain,
    wsTerminal,
    wsWebpages,
  )
where

ws1 = "代码"
ws2 = "计时"
ws3 = "网页"
ws4 = "娱乐"
ws5 = "下载"
ws6 = "其他"
ws7 = "7"
ws8 = "8"
ws9 = "9"
ws10 = "10"

wsTerminal = ws1
wsWebpages = ws3
wsCode = ws1
wsEntertain = ws4
wsDownloads = ws5

myWsList =
  [ ws1, ws2, ws3, ws4, ws5, ws6]
