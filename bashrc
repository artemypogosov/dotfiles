#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export HISTCONTROL=ignoreboth:erasedups

# Default editor

export EDITOR='emacs'
export VISUAL='emacs'

PS1='[\u@\h \W]\$ '

if [ -d "$HOME/.bin" ] ;
  then PATH="$HOME/.bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ;
  then PATH="$HOME/.local/bin:$PATH"
fi

# Ignore upper and lowercase when TAB completion
bind "set completion-ignore-case on"

### ALIASES

## START OF PFM ALIASES --------------------------------------------------

##alias pfm-figwheel-clear-port='sudo kill -9 `sudo lsof -t -i:3451`'
##alias pfm-figwheel-start-repl='FIGWHEEL_SERVER_PORT=3499 NREPL_PORT=8333 lein repl'

alias pfm-hard-rebuild='make quick_stop down volumes/delete/all volumes/delete/config init_env_file up/from_scratch bdm/setup frontend2/setup frontend2/clean frontend2/install switch_to_clojure_repl'
alias pfm-soft-rebuild='make quick_stop down init_env_file up/from_scratch frontend2/setup frontend2/clean frontend2/install switch_to_clojure_repl'

alias pfm-setup-frontend='make frontend2/setup frontend2/clean frontend2/install'
alias pfm-run-frontend-repl='make frontend2/repl'
alias pfm-run-backend-repl='make quick_stop down up switch_to_clojure_repl'

##alias pfm-delete-db-start-repl='make quick_stop down volumes/delete/db volumes/delete/storage up switch_to_clojure_repl'
alias pfm-setup-db='(cd ../demo ; make bdm/setup ; cd ../frontend ; pfm-figwheel-start-repl)'


alias pfm-pull-revamp='git pull origin qb-java-revamp-ui'

## END OF PFM ALIASES --------------------------------------------------

## ls
alias ls='ls --color=auto'
alias la='ls -a'
alias ll='ls -la'
alias l='ls'
alias l.="ls -A | egrep '^\.'"


## Colorize the grep command output for ease of use (good for log files)##
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

## Readable output
alias df='df -h'

## Free
alias free="free -mt"

## Userlist
alias userlist="cut -d: -f1 /etc/passwd"

## Aliases for software managment
alias update='sudo pacman -Syyu'

## Cleanup orphaned packages
alias cleanup='sudo pacman -Rns $(pacman -Qtdq)'

## ps
alias psgrep="ps aux | grep -v grep | grep -i -e VSZ -e"

## Add new fonts
alias update-fc='sudo fc-cache -fv'

## Switch between bash and zsh
alias tobash="sudo chsh $USER -s /bin/bash && echo 'Now log out.'"
alias tozsh="sudo chsh $USER -s /bin/zsh && echo 'Now log out.'"

## Hardware info --short
alias hw="hwinfo --short"

## Get fastest mirrors in your neighborhood
alias mirror="sudo reflector -f 30 -l 30 --number 10 --verbose --save /etc/pacman.d/mirrorlist"
alias mirrord="sudo reflector --latest 30 --number 10 --sort delay --save /etc/pacman.d/mirrorlist"
alias mirrors="sudo reflector --latest 30 --number 10 --sort score --save /etc/pacman.d/mirrorlist"
alias mirrora="sudo reflector --latest 30 --number 10 --sort age --save /etc/pacman.d/mirrorlist"

## Our experimental - best option for the moment
alias mirrorx="sudo reflector --age 6 --latest 20  --fastest 20 --threads 5 --sort rate --protocol https --save /etc/pacman.d/mirrorlist"
alias mirrorxx="sudo reflector --age 6 --latest 20  --fastest 20 --threads 20 --sort rate --protocol https --save /etc/pacman.d/mirrorlist"

## Mounting the folder Public for exchange between host and guest on virtualbox
alias vbm="sudo /usr/local/bin/arcolinux-vbox-share"

#shopt
shopt -s autocd # change to named directory
shopt -s cdspell # autocorrects cd misspellings
shopt -s cmdhist # save multi-line commands in history as single line
shopt -s dotglob
shopt -s histappend # do not overwrite history
shopt -s expand_aliases # expand aliases


#Recent Installed Packages
alias rip="expac --timefmt='%Y-%m-%d %T' '%l\t%n %v' | sort | tail -200 | nl"
alias riplong="expac --timefmt='%Y-%m-%d %T' '%l\t%n %v' | sort | tail -3000 | nl"

#iso and version used to install ArcoLinux
alias iso="cat /etc/dev-rel | awk -F '=' '/ISO/ {print $2}'"

#search content with ripgrep
alias rg="rg --sort path"

#get the error messages from journalctl
alias jctl="journalctl -p 3 -xb"

# Quick open config files
alias nb="$EDITOR ~/.dotfiles/bashrc"
alias nz="$EDITOR ~/.dotfiles/zshrc"
alias oxmonad="$EDITOR ~/.dotfiles/xmonad/xmonad.hs"

#systeminfo
alias probe="sudo -E hw-probe -all -upload"

#shutdown or reboot
alias ssn="sudo shutdown now"
alias sr="sudo reboot"

#give the list of all installed desktops - xsessions desktops
alias xs="ls /usr/share/xsessions"

# # ex = EXtractor for all kinds of archives
# # usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1   ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *.deb)       ar x $1      ;;
      *.tar.xz)    tar xf $1    ;;
      *.tar.zst)   tar xf $1    ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

# Run fm6000
fm6000 -r -c "random"

# Temp fix for layouts
setxkbmap us

# Doom emacs
 export PATH="$HOME/.emacs.d/bin:$PATH"

# Set PFM NPM_AUTH_TOKEN
export NPM_AUTH_TOKEN=jkSufCaf73J3LY3uXbVM

