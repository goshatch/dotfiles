# Personal configuration files

Following the "[$HOME as a git repository](https://drewdevault.com/2019/12/30/dotfiles.html)" pattern.

Always a work in progress.

## Setting up a new system

In the home directory:

```bash
$ git init .
$ git remote add origin git@github.com:gueorgui/dotfiles.git
$ git fetch
$ git checkout -f master
$ git submodule update --init --remote .emacs.d
$ git submodule update --init --remote .tmux
```
