# sys
## 僕の環境について
```
macOS 10.12.6 (Sierra)
GNU Fortran (Homebrew GCC 7.3.0_1) 7.3.0
```

## fileの実行について
compileされた実行ファイルはgithub上に共有する必要がないので gitignore される ```bin``` 以下に置かれるようにします。
gfortranは -o オプションで実行ファイルの名前を指定できますが、pathは指定できず、カレントディレクトリに置かれるので、bin内で作業をする必要がある

例）
```
$ pwd
/Users/kosuke_machida/School/sys/lecture1/bin

$ gfortran -o main_ex1 ../main_ex1.f90
// これでカレントディレクトリに main_ex1 という名前の実行ファイルが出来る

$ ./main_ex1
// 実行される
```
