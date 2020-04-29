
# 概要

Goによるごく限られた機能しかないLISPインタプリタ。

実用にはならないので注意。

# 使い方

    cd src
    go run golisp.go ../test-scripts/fib.lsp


REPL

    $ go run golisp.go 
    #[1] (+ 1 2)
    3
    #[2] 

# Goからの呼び出し

Goからインタプリタを呼び出す例。

```
package main

import (
	"fmt"
	"log"

	"github.com/kztomita/golisp/interpreter"
)

func main() {
	interpreter.Initialize()

	// LISP構文を字句解析、構文解析して構文木を構築
	node, err := interpreter.Parse("(+ 1 2)")
	if err != nil {
		log.Fatalf("%v\n", err)
	}

	// 作成した構文木を評価
	ev := interpreter.NewEvaluator()
	result, err := ev.Eval(node)
	if err != nil {
		log.Fatalf("%v", err)
	}

	fmt.Println(result.ToString());
}
```

# 現状使用可能な関数/マクロ

- \+
- \-
- \*
- \/
- =
- \/=
- &gt;
- &gt;=
- &lt;
- &lt;=
- and
- append
- apply
- cadr
- car
- cddr
- cdr
- cons
- defmacro (*1)
- defun (*2)
- do
- funcall
- function (#')
- if
- lambda
- length
- let
- list
- not
- or
- prin1
- print
- progn
- quote
- setq

(*1) lambda list keywordは&optional,&rest,&bodyのみ対応。<br />
(*2) lambda list keywordは&optional,&restのみ対応。

# TODO

- 対応関数の充実
- テストケースの充実
- わかりやすいエラー表示
