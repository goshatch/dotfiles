if [[ "$OSTYPE" == "linux-gnu" ]]; then
  source /usr/share/nvm/init-nvm.sh
else
  export NVM_DIR="$HOME/.nvm"
  [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
fi

export GOPATH="$HOME/go"
export PATH="$GOPATH/bin:$HOME/.bin:$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$HOME/.rbenv/bin:$PATH"

