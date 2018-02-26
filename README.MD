## quick-open

快速打开/切换程序，在Terminal中运行。Mac上。

## 编译 & 使用

```bash
stack build
```

如果不想下载源码编译，可以直接下载根目录的t文件

```bash
t qq # 切换或者打开qq界面
t chrome # 切换或者打开Chrome
t baidu # 打开默认浏览器并进入百度
t baidu quick-open # 打开网页并用百度搜索quick-open
t :google.com 打开网址(前面不能加协议）
```

## 配置

默认的配置 ["/Applications"]

自定义配置文件 ~/.tconfig

配置需要快速打开应用所在的文件夹
```haskell
["/your/abs/path1"]
```


