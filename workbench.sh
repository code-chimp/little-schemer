#!/bin/bash

# Common Lisp
tmux new-session -s litsch -n lisp -d
tmux send-keys -t litsch 'vim tls.lisp' C-m
tmux split-window -h -t litsch
tmux split-window -v -t litsch
tmux send-keys -t litsch:0.1 'clisp' C-m
tmux send-keys -t litsch:0.2 'sbcl' C-m
tmux select-pane -t litsch:0.0

# Scheme
tmux new-window -n scheme -t litsch
tmux send-keys -t litsch:1 'vim tls.scm' C-m
tmux split-window -h -t litsch:1
tmux split-window -v -t litsch:1
tmux split-window -v -t litsch:1
tmux select-layout -t litsch:1 main-vertical
tmux send-keys -t litsch:1.1 'mit-scheme' C-m
tmux send-keys -t litsch:1.2 'guile' C-m
tmux send-keys -t litsch:1.3 'gsi' C-m
tmux select-pane -t litsch:1.0

# Clojure
tmux new-window -n clojure -t litsch
tmux send-keys -t litsch:2 'vim tls.clj' C-m
tmux split-window -h -t litsch:2
tmux send-keys -t litsch:2.1 'lein repl' C-m
tmux select-pane -t litsch:2.0

# Racket
tmux new-window -n racket -t litsch
tmux send-keys -t litsch:3 'vim tls.rkts' C-m
tmux split-window -h -t litsch:3
tmux send-keys -t litsch:3.1 'racket' C-m
tmux select-pane -t litsch:3.0

# Drive!
tmux select-window -t litsch:0
tmux select-pane -t litsch:0.0
tmux attach -t litsch
