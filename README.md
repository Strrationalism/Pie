# Pie 构建系统

一个基于S-表达式语法的构建系统。

## 编译

编译先决条件：

* Haskell Stack

执行编译：

```shell
stack build
```

## 安装

### 从源代码安装

编译后执行：

```shell
stack install
```

### 从二进制文件安装

从[Release页面](https://github.com/Strrationalism/Pie/releases)下载二进制文件，
并放入`$PATH`中。

## 例子

```lisp
(task (copy-file in-file out-file)  ;; 定义任务copy-file，接受两个参数in-file和out-file
    (in in-file)                    ;; 标记为输入文件in-file
    (out out-file)                  ;; 标记为输出文件out-file
    (make                           ;; 构建时执行
        (copy in-file out-file)))   ;; 复制文件

(action build
    (ensure-dir "./build")          ;; 确保目录存在
    (copy-file "./Pie.cabal" "./build/Pie.cabal"))  ;; 创建一个copy-file任务

;; 任务的make部分将会在action build执行结束之后并行执行

(export build)
;; 导出build任务，使得可以通过命令行访问，最后一个导出的任务为命令行默认任务
```

## 快速开始

### 基本

Pie的构建脚本应当命名为`./build.pie`，并且至少定义并导出一个action，例子如下：

```lisp
(action build
    (display "Hello, world!"))

(export build)
```

之后即可使用`pie`命令或`pie build`命令来执行该脚本。

当使用`pie`命令时，pie将会执行最后一个导出的action，即`build`。

pie构建脚本中可以定义变量、函数、task、action，并且这些内容均可export，export的内容可以被
其它pie文件import到。

action不可具有返回值，所有的返回值都将被替换为`nil`。

pie将会首先执行`_init` action，之后执行命令行给定的或默认的action，每个action的执行都将会
遵循以下步骤：

1. 确保上一个action及其task都已经执行完毕
1. 执行当前action内容，执行task的do块，并且收集task的make块并构建依赖图
1. 当action执行结束后，将会对所有的task按照输入输出文件依赖进行拓扑排序
1. 根据拓扑序并行执行所有task的make块执行构建（如果pie的第一个参数为`-s`，则为单线程执行）

### 类型

Pie构建脚本中每个值都具有一个类型，值可能具有以下类型：

* `nil`类型，值写作`nil`
* `bool`类型，值写作`true`或`false`
* `string`类型，使用双引号包裹的字符串
* `number`类型，使用数字表示的值
* `list`类型，列表，可以通过`list`函数构造
* `var`类型，可变变量，可以被重新赋值
* `lambda`类型，表示一个函数

### 变量

你可以通过以下两种语法来定义变量，并在定义之后变量可用，这种语法可以写在文件顶部：

```lisp
(define <变量名> <值>)
```

```lisp
(defines
    (<变量名1> <值1>)
    (<变量名2> <值2>)
    (<变量名3> <值3>))
```

以下这种定义变量的语法可以将变量限制在一个表达式范围内：

```lisp
(let
    (<变量名1> <值1>)
    (<变量名2> <值2>)
    (<变量名3> <值3>)
    (<可以访问这些变量的表达式>))
```

如：

```lisp

(define a 1)
(define b "2")
(define c true)

(defines
    (x "X")
    (y "Y")
    (z "Z"))

(action main
    (let
        (h "Hello ")
        (w "World!")
        (display h w)))

(export main)
```

### 函数

对于具有名字的函数，可以通过`define`、`defines`中对变量名添加括号和参数来定义，例子：

```lisp
(define (f) (display "Function F!"))
(define (g x) (display "Function G! Argument:" x))
(define (h x y) (display "Function H! Arguments:" x y))
```

```lisp
(defines
    ((f) (display "Function F!"))
    ((g x) (display "Function G! Argument:" x))
    ((h x y) (display "Function H! Arguments:" x y)))
```

对于不具有名字的函数，可以使用`lambda`语法来定义，如：

```lisp
(define f (lambda () (display "Function F!")))
(define g (lambda (x) (display "Function G! Argument:" x)))
(define h (lambda (x y) (display "Function H! Arguments:" x y)))
```

函数可以具有多个语句，最后一个语句将会被作为返回值：

```lisp
(define (f)
    (display "Function F!")
    1)  ;; 返回1
```

```lisp
(lambda (x y)
    (display "Add " x " " y)
    (+ x y))    ;; 返回x和y的和
```

调用函数则必须使用`()`来包裹函数和参数，如：

```lisp

(define (f) (display "Function F!"))
(define (g x) (display "Function G! Argument:" x))

(action main
    (f)         ;; 调用了f函数
    (g 1))      ;; 调用了g函数

(export main)
```

如果在函数上不写括号，则表示为取这个函数的引用：

```lisp
(define (f) (display "Function F!"))

(define (g1)
    f)      ;; 没有包裹()，表示g1的返回值为f函数

(define (g2)
    (f))    ;; 包裹了()，表示调用f后，将f的返回值作为g2的返回值

```

### 任务

任务是构建系统的基本单元，定义的语法如下：

```lisp
(task (<任务名>)
    <任务实现>)

(task (<任务名> <参数>...)
    <任务实现>)
```

其中任务实现可以放置以下块：

* define - 定义变量
* defines - 定义变量
* in - 参数可以是任意个文件路径，表示输入文件
* out - 参数可以是任意个文件路径，表示输出文件
* do - 表示在构建任务图过程中要执行的操作，并且这个块的返回值将会被作为调用task的返回值
* make - 表示action执行完毕之后要执行的构建操作，任务将会被按照依赖项拓扑排序后并行执行

例如以下是一个任务定义：

```lisp
(task (compile-c-file c-file)
    (define o-file (change-ext c-file "o"))
    (in c-file)
    (out o-file)
    (do o-file)         ;; 通过do块将o-file作为返回值
    (make
        (shell "gcc" "-c" c-file o-file)))
```

之后便可在action中调用：

```lisp
(action build
    (display "o-file: " (compile-c-file "./main.c")))
```

### 模块

构建脚本可以由不同的模块组成，模块之间用import来导入其已经export的内容：

`./test.pie`:

```lisp
(define test-module-name "./test-module")   ;; 定义变量`test-module-name`

(action test                                ;; 定义动作`test`
    (display "Test " test-module-name))

(export                                     ;; 导出
    test-module-name
    test)
```

`./clean.pie`:

```lisp
(action clean                               ;; 定义clean动作
    (display "Clean!"))

(export clean)                              ;; 导出`clean`动作
```

`./build.pie`:

```lisp
(import                                     ;; 导入这两个模块导出的内容
    "./test.pie"
    "./clean.pie")

(action build
    (display "Build!"))

(action build-test
    (build)
    (test)
    (display test-module-name " Tested!"))

(action rebuild
    (clean)
    (build))

(export
    test    ;; 动作`test`和`clean`仅仅只是导入到了./build.pie中，它需要从./build.pie中
    clean   ;; 重新导出才可以被命令行或其他模块调用
    build)  ;; 最后一个导出的action将会被作为默认action，在命令行中不指定action时执行
```

### 查看所有语法、内置的常量与函数

你可以到[Pie Wiki](https://github.com/Strrationalism/Pie/wiki)上找到所有的语法、内置
的常量与函数。
