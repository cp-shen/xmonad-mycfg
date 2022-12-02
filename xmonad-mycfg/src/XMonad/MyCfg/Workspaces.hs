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

ws1 = "终端"
ws2 = "网页"
ws3 = "代码"
ws4 = "娱乐"
ws5 = "下载"
ws6 = "其他"
ws7 = "第七"
ws8 = "第八"
ws9 = "第九"
ws10 = "第十"

wsTerminal = ws1
wsWebpages = ws2
wsCode = ws3
wsEntertain = ws4
wsDownloads = ws5

myWsList =
  [ ws1, ws2, ws3, ws4, ws5, ws6]
