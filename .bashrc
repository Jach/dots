# /etc/skel/.bashrc
#
# This file is sourced by all *interactive* bash shells on startup,
# including some apparently interactive shells such as scp and rcp
# that can't tolerate any output.  So make sure this doesn't display
# anything or bad things will happen !


# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
	# Shell is non-interactive.  Be done now!
	return
fi


# Put your fun stuff here.
alias l='ls'
alias lm='ls -lh'
alias gccd='gcc -Wall -Wextra -ansi -pedantic'
alias gccd32='gcc -m32 -Wall -Wextra -ansi -pedantic'
alias g++d='g++ -g -Wall -Wextra -ansi -pedantic'
alias g++d32='g++ -m32 -Wall -Wextra -ansi -pedantic'
export LD_LIBRARY_PATH="/usr/lib64/panda3d/:/usr/local/lib/"
export PATH="/usr/local/bin:/usr/local/sbin/:/bin:/sbin:/usr/bin:/usr/sbin:$PATH"
export PATH="$PATH:/home/kevin/j64-602/bin/"
export PATH="$PATH:/home/kevin/Desktop/flex_sdk_4/bin/:/opt/jswat-4.5/bin/"
export PATH="$PATH:/home/kevin/Desktop/flash_player_10_linux_dev/standalone/debugger/"
export PATH="$PATH:/home/kevin/luciddb-0.0.0/bin/:/home/kevin/p4s/"
export CLASSPATH="$CLASSPATH:.:/usr/share/jdbc-mysql/lib/jdbc-mysql.jar"
export PYTHONSTARTUP=$HOME'/.pythonrc.py'

function ltxm() {
  latexmk -silent -pdf $@
}
alias ltxc='latexmk -silent -c -CF'
function ltx() {
  latexmk -silent -pdf $@ && ltxc
}
# To latex a paper with a .bib file:
# pdflatex -interaction=batchmode file.tex
# bibtex file
# and rerun pdflatex twice, use ltxc to cleanup if you want

alias ip='curl www.nincheats.net/ip.php'
alias py_prof='python -m cProfile'
alias lucid.d="lucidDbServer"
alias lucid="sqllineClient"
alias lucida="sqllineClient -n sa -p sa"

alias mxmlc_themed="mxmlc -theme=/home/kevin/Desktop/flex_sdk_4/frameworks/themes/Halo/halo.swc -theme=/home/kevin/Desktop/flex_sdk_4/frameworks/themes/Spark/spark.css"

export ANT_OPTS=-Xmx1400m

export PATH="/usr/lib64/ccache/bin:$PATH" # ccache
export PATH=/home/kevin/:$PATH

C() { cd `$HOME/path-selector.sh "$@"`; }
E() { $EDITOR `$HOME/path-selector.sh "$@"`; }
MP() { /usr/bin/mplayer `$HOME/path-selector.sh "$@"`; }

alias ant_flex="ant; cp ../dynamodb-services/wars/adminui.war ../dynamodb-services/tmp/dynamodb-services/webapps/"
alias ant_ws="ant deploy; cp ../dynamodb-services/wars/adminws.war ../dynamodb-services/tmp/dynamodb-services/webapps/"

fortune -a
alias vgrind="valgrind --tool=memcheck --leak-check=yes --show-reachable=yes --num-callers=20 --track-fds=yes --track-origins=yes -v"

# also works with hw:0 make sure capture on gnome is up and record
alias recordwebcamsound='ffmpeg -f alsa -i hw:1,0 -acodec libmp3lame -ab 96k output.mp3'
alias recordinternal='parec --format=s16le --rate=44100 -r -d 1  | ffmpeg -y -ac 2 -f s16le -ar 44100 -i - out.mp3'
alias pytime='python -m timeit ' # snippet

unset ANT_HOME
alias asmer="gcc -S -masm=intel -fverbose=asm"
alias gccsec='gcc -fmudflap -fstack-check -gnato'
# pointer arith errs,
# stack overflow prot., int overflow
#define likely(x) __builtin_expect((x),1)
#define unlikely(x) __builtin_expect((x),0)
export CLASSPATH=$CLASSPATH:/usr/share/jline/lib/jline.jar:/home/kevin/clojure-1.2.1/clojure.jar
export PATH=$PATH:$HOME/git_repos/clojurescript/script/repl:$HOME/git_repos/clojurescript/script/repljs:$HOME/git_repos/clojurescript/bin/
export CLOJURESCRIPT_HOME=$HOME/git_repos/clojurescript
#alias clj="java -cp $HOME/clojure-1.3.0/jline-0.9.94.jar:$HOME/clojure-1.3.0/math.combinatorics.jar:$HOME/clojure-1.3.0/clojure.jar jline.ConsoleRunner clojure.main"
#alias clj="rlwrap java -cp $HOME/clojure-1.3.0/math.combinatorics.jar:$HOME/clojure-1.3.0/clojure.jar clojure.main"
#alias clj="rlwrap java -cp $HOME/clojure-1.4.0/math.combinatorics.jar:$HOME/clojure-1.4.0/clojure-1.4.0.jar:. clojure.main"
alias clj="rlwrap java -cp $HOME/clojure-1.6.0/clojure-1.6.0.jar:. clojure.main"
export PATH=$PATH:$HOME/bin:$HOME/Nim/bin:$HOME/.nimble/bin
alias camcam="gphoto2 --get-all-files"
# 10 requests, 5 concurrent
alias servertest='ab -n 10 -c 5 http://www.thejach.com/'

source /usr/share/bash-completion/git-prompt.sh
export PS1="\$(if [ \$? = 0 ]; then echo \[\e[32m\]':)'\[\e[0m\]; else echo \[\e[31m\]':('\[\e[0m\]; fi) \[\033[01;32m\]\u@\h\[\033[01;34m\] \w\$(__git_ps1) \$\[\033[00m\] "

# trying this out
alias python='pypy'
# rlwrap
# perf stat -B cmd
alias minecraft='export LD_LIBRARY_PATH=/usr/lib/jvm/oracle-jre-bin-1.7/lib/amd64/ && java -jar minecraft.jar'
alias pyserver='python -m SimpleHTTPServer'
# python3: python -m http.server 8000
# Note to self: traceroute -I is important (as root)
alias pylint='pylint -r n -f colorized'
function aesenc() {
  gpg --output $2 --symmetric --cipher-algo AES256 $1
  #openssl aes-256-cbc -in $1 -out $2 # don't use, insecure...
}
function aesdec() {
  gpg --output $2 --decrypt $1
  #openssl aes-256-cbc -d -in $1 -out $2
}
# extract2end filename starttime outputname
# starttime is in hr:min:sec
function extract2end() {
  ffmpeg -ss $2 -i $1 -vcodec copy -acodec copy $3
}

#PERL_MB_OPT="--install_base \"/home/kevin/perl5\""; export PERL_MB_OPT;
#PERL_MM_OPT="INSTALL_BASE=/home/kevin/perl5"; export PERL_MM_OPT;
shopt -s histappend
export HISTSIZE=100000
export HISTFILESIZE=100000
export PATH=$PATH:$HOME/.roswell/bin

stairon() {
  curl 192.168.0.44:8080/CMD?Stairway_Lights=ON &> /dev/null
}
stairoff() {
  curl 192.168.0.44:8080/CMD?Stairway_Lights=OFF &> /dev/null
}

sign() {
  gpg --output $1.signed --clearsign $1
  echo "Check $1"
}

verify() {
  gpg --verify $1
}

movie2gif() {
  # $1: movie file
  # $2: output file
  fps=10
  ffmpeg -i $1  -r $fps -f image2pipe -vcodec ppm - | convert -delay $fps -loop 0 - $2
}

export AMDAPPSDKROOT="/home/kevin/AMDAPPSDK-3.0"
export ANT_HOME=/usr/share/ant

i2pstart() {
  /usr/share/i2p/runplain.sh
}

alias sbcl='rlwrap sbcl'

. $HOME/git_repos/not_mine/git-issue/gi-completion.sh

# have less print to stdout if less than one screen of text
#export PAGER="$PAGER -FX"
# breaks the new man-db...
alias less='less -FX'

flac-eac-split() {
  cuefile=$1
  flacfile=$2
  # shntool
  shnsplit -f "$cuefile" -t %n-%t -o flac "$flacfile"
  # cuetools
  cuetag.sh "$cuefile" [0-9]*.flac
}

export PATH=$PATH:$HOME/git_repos/not_mine/srt-resync

. $HOME/.bashrc.secret


[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/kevin/google-cloud-sdk/path.bash.inc' ]; then . '/home/kevin/google-cloud-sdk/path.bash.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/home/kevin/google-cloud-sdk/completion.bash.inc' ]; then . '/home/kevin/google-cloud-sdk/completion.bash.inc'; fi
PATH=$PATH:$HOME/.local/bin
