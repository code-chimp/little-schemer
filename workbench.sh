#!/bin/bash

# Common Lisp
tmux new-session -s test -n lisp -d
tmux send-keys -t test 'vim tls.lisp' C-m
tmux split-window -h -t test
tmux split-window -v -t test:2
tmux send-keys -t test:1.2 'clisp' C-m
tmux send-keys -t test:1.3 'sbcl' C-m
tmux select-pane -t test:1.1

# Scheme
tmux new-window -n scheme -t test
tmux send-keys -t test:2 'vim tls.scm' C-m
tmux split-window -h -t test:2
tmux split-window -v -t test:2.2
tmux split-window -v -t test:2.3
tmux select-layout main-vertical -t test:2
tmux send-keys -t test:2.2 'mit-scheme' C-m
tmux send-keys -t test:2.3 'guile' C-m
tmux send-keys -t test:2.4 'gsi' C-m
tmux select-pane -t test:2.1

# Clojure
tmux new-window -n clojure -t test
tmux send-keys -t test:3 'vim tls.clj' C-m
tmux split-window -h -t test:3
tmux send-keys -t test:3.2 'lein repl' C-m

# Racket
tmux new-window -n racket -t test
tmux send-keys -t test:4 'vim tls.rkts' C-m
tmux split-window -h -t test:4
tmux send-keys -t test:4.2 'racket' C-m

# Drive!
tmux select-window -t test:1
tmux select-pane -t test:1.1
tmux attach -t test
