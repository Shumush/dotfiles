if [ -f /etc/bash_completion ]; then
	    . /etc/bash_completion
fi

# this makes it so only one TAB is required to get a list of all options
set show-all-if-ambiguous on

complete -cf sudo

shopt -s cdspell
shopt -s checkwinsize
shopt -s cmdhist
shopt -s dotglob
shopt -s expand_aliases
shopt -s extglob
shopt -s histappend
shopt -s hostcomplete
shopt -s nocaseglob

export HISTSIZE=10000
export HISTFILESIZE=${HISTSIZE}
export HISTCONTROL=ignoreboth
export LESS="-erX"  # so less displays ANSI colors  correctly

###############################################################################
# PATH stuff

prepend() { [ -d "$2" ] && eval $1=\"$2\$\{$1:+':'\$$1\}\" && export $1 ; }

prepend PATH /opt/java/jre/bin
prepend PATH $HOME/.gem/ruby/*/bin
prepend PATH $HOME/bin

###############################################################################
# ex - archive extractor
# usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1     ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}
###############################################################################
# SSH SETTINGS
SSH_ENV="$HOME/.ssh/environment"
function start_agent {
     echo "Initialising new SSH agent..."
     /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
     echo succeeded
     chmod 600 "${SSH_ENV}"
     . "${SSH_ENV}" > /dev/null
     /usr/bin/ssh-add;
}

# Source SSH settings, if applicable

if [ -f "${SSH_ENV}" ]; then
     . "${SSH_ENV}" > /dev/null
     #ps ${SSH_AGENT_PID} doesn't work under cywgin
     ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
         start_agent;
     }
else
     start_agent;
fi
###############################################################################
# vim stuff
export EDITOR=vim
#export VISUAL=vim
alias vi=vim
setterm -blength 0


###############################################################################
# prompt
PS1='[\u@\h \W]\$ '
export PCOLOR='\e[0;32m'
if [ -f $HOME/.bash.prompt ] ; then
	source $HOME/.bash.prompt
fi
###############################################################################
# global aliases
alias ..="cd .."
alias ...="cd ../../"
alias ls='ls --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias ll='ls -l --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias la='ls -la --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias grep='grep --color=tty -d skip'
alias cp="cp -i"                          # confirm before overwriting something
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB
alias sch="cd $HOME/docs/School"
alias mi="make && make install"
alias f="find . -iname"

###############################################################################
# git alises
alias gs="git status"
alias gap="git add --patch"
alias gpo="git push origin master"
alias g="git"

###############################################################################
# archlinux aliases
alias vp='vim PKGBUILD'
alias vs='vim SPLITBUILD'
alias y="yaourt"
alias yupdate="yaourt -Syu"


###############################################################################
# suse aliases
alias z="sudo zypper"



###############################################################################
# misc aliases
alias amarokup="git checkout master && git remote update && git rebase amarok/master && git pull amarok master"
alias fanhi="sudo bash -c \"echo level full-speed > /proc/acpi/ibm/fan\""

###############################################################################
# source local settings
if [ -f $HOME/.bashrc.local ] ; then
    source $HOME/.bashrc.local
fi
 
