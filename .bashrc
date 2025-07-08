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
export EDITOR=vim
alias l='ls'
alias lm='ls -lh'
alias gccd='gcc -Wall -Wextra -ansi -pedantic'
alias gccd32='gcc -m32 -Wall -Wextra -ansi -pedantic'
alias g++d='g++ -g -Wall -Wextra -ansi -pedantic'
alias g++d32='g++ -m32 -Wall -Wextra -ansi -pedantic'
#export LD_LIBRARY_PATH="/usr/lib64/panda3d/:/usr/local/lib/"
export PATH="/usr/local/bin:/usr/local/sbin/:/bin:/sbin:/usr/bin:/usr/sbin:$PATH"
export PATH="$PATH:/home/kevin/j64-602/bin/"
export PATH="$PATH:/home/kevin/Desktop/flex_sdk_4/bin/:/opt/jswat-4.5/bin/"
export PATH="$PATH:/home/kevin/Desktop/flash_player_10_linux_dev/standalone/debugger/"
export PATH="$PATH:/home/kevin/luciddb-0.0.0/bin/:/home/kevin/p4s/"
export PYTHONSTARTUP=$HOME'/.pythonrc.py'

alias rot13="tr 'A-Za-z' 'N-ZA-Mn-za-m'"

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

cdl () {
      cd "$(dirname "$(readlink "$1")")";
}

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
export PS1="\[\033]0;\u@\h:\w\007\]\$(if [ \$? = 0 ]; then echo \[\e[32m\]':)'\[\e[0m\]; else echo \[\e[31m\]':('\[\e[0m\]; fi) \[\033[01;32m\]\u@\h\[\033[01;34m\] \w\$(__git_ps1) \$\[\033[00m\] "

# trying this out
#alias python='pypy'
# rlwrap
# perf stat -B cmd
#alias minecraft='export LD_LIBRARY_PATH=/usr/lib/jvm/oracle-jre-bin-1.7/lib/amd64/ && java -jar minecraft.jar'
alias pyserver='python -m SimpleHTTPServer'
alias pyserver3='python3 -m http.server 8000'
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

alias sbcl='rlwrap sbcl --dynamic-space-size 11gb --core ~/sbcl-core'

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


#export FZF_DEFAULT_COMMAND="ag . -l --ignore '*.fasl' --ignore '*.pyc' --nocolor --hidden"
export FZF_DEFAULT_COMMAND="ag -l"
[ -f /home/kevin/.fzf.bash ] && source /home/kevin/.fzf.bash

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/kevin/google-cloud-sdk/path.bash.inc' ]; then . '/home/kevin/google-cloud-sdk/path.bash.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/home/kevin/google-cloud-sdk/completion.bash.inc' ]; then . '/home/kevin/google-cloud-sdk/completion.bash.inc'; fi
PATH=$PATH:$HOME/.local/bin

confirm() {
  read -r -p "$1 [Y/n] " response
  response=${response,,} # toLower
  if [[ "$response" =~ ^(no|n)$ ]]; then
    false
  else
    true
  fi
}

webcam() {
  # does this still work?
  while true; do
    fswebcam -r 1280x720 --png -1 -D 1 -S 30 --no-banner "/webcam/Webcam-`date +%s`".png
    sleep 1200
  done
}

# set -o noclobber # forbids x > foo; protect self from overwriting foo...

export ua_googlebot='Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)'

alias glxgears='__GL_SYNC_TO_VBLANK=0 glxgears'

alias github_mimic='grip'

export PATH=$PATH:$HOME/git_repos/not_mine/vimpager/standalone/

tricks() {
  # automate/remember e.g.
  #WINEPREFIX=/mnt/ssd/rootfs/SteamLibrary/steamapps/compatdata/1031480/pfx/ winetricks dotnet20 dotnet461 xact gdiplus allfonts
  game_id="$1"
  WINEPREFIX=/mnt/ssd/rootfs/SteamLibrary/steamapps/compatdata/$game_id/pfx/ winetricks "${@:2}"
}

# Make certain directories, especially on ntfs mounted systems,
# not have an ugly green bg color when using ls and use no bg-color.
# ow = other, writable
# tw = sticky, writable.
# Recall color code has three columns: bold;font-color;bg-color.
export LS_COLORS="$LS_COLORS:ow=1;34:tw=1;34:"

#export GTK_IM_MODULE=ibus
#export XMODIFIERS=@im=ibus
#export QT_IM_MODULE=ibus

trace_env_vars() {
  PS4='+$BASH_SOURCE> ' BASH_XTRACEFD=7 bash -xl 7> /tmp/file.log
}

vid2cam() {
#https://rmsol.de/2020/04/25/v4l2/
  ffmpeg -stream_loop -1 -re -i "$1" -f v4l2 /dev/video2
}

#alias ocr='xclip -selection clipboard <<< $(gazou -h 2> /dev/null)'
alias ocr='scrot -s -F - | gazou'

nicer() {
  ionice -c3 nice "$@"
}

curltime() {
  curl -L -w "time_namelookup: %{time_namelookup}\ntime_connect: %{time_connect}\ntime_appconnect: %{time_appconnect}\ntime_pretransfer: %{time_pretransfer}\ntime_redirect: %{time_redirect}\ntime_starttransfer: %{time_starttransfer}\ntime_total: %{time_total}\n" "$1"
}

livetl() {
  whisper-ctranslate2 --language Japanese --task translate --live_transcribe True --model large-v3 --live_volume_threshold 0.01
}

livetl-en() {
  whisper-ctranslate2 --language English --live_transcribe True --model large-v3 --live_volume_threshold 0.01
}

whisper-stdout() {
  lang='Japanese'
  task='translate'
  if [ "$2" != "" ]; then
    # meant to just transcribe e.g. english
    lang=$2
    task='transcribe'
  fi
  whisper-ctranslate2 --language "$lang" --task $task --model large-v3 --condition_on_previous_text False -f txt "$1"
  bn="${1%.*}"
  rm "$bn".txt
}

whisper-jp() {
  #whisper-ctranslate2 --language Japanese --task translate --model large-v3 -f srt --vad_filter True --condition_on_previous_text False "$1"
  # vad filter was causing 0 output on a song...
  whisper-ctranslate2 --language Japanese --task translate --model large-v3 -f srt --condition_on_previous_text False "$1"
}

whisper-jp-transcribe() {
  #whisper-ctranslate2 --language Japanese --task translate --model large-v3 -f srt --vad_filter True --condition_on_previous_text False "$1"
  # vad filter was causing 0 output on a song...
  whisper-ctranslate2 --language Japanese --model large-v3 -f srt --condition_on_previous_text False "$1"
}

whisper-en() {
  whisper-ctranslate2 --language English --model large-v3 -f srt --condition_on_previous_text False "$1"
}

list-aspect-ratios() {
  for e in *.JPG; do echo -n "$e "; identify -ping -format 'scale=2; %w/%h\n' $e | bc; done
}

list-proton() {
  dirs="$HOME/.local/share/Steam/steamapps/compatdata/"
  list() {
    for i in $dirs*; do
      appid=$(basename "$i")
      vers=$([ -f "$i/version" ] && cat "$i/version" || echo "N/A")
      printf "%s\t%s\n" "$appid" "$vers"
    done
  }

  id_versions=$(list | sort -k1,1)
  name_ids=$(grep -n "name" "$HOME/.local/share/Steam/steamapps/"*.acf |
                  sed -e 's/^.*_//;s/\.acf:.:/ /;s/name//;s/"//g;s/\t//g;s/ /-/' |
                  awk -F"-" '{printf "%s\t%s\n", $1, $2}' |
                  sort -k1,1)

  join -t $'\t' -o '2.2,1.1,1.2' <(echo "$id_versions") <(echo "$name_ids") |
    awk -F'\t' '{printf "%-55s\t%s\t%s\n", $1, $2, $3}' | sort -k1,1
}

alias vi=nvim

ups-load() {
  # multiply by 10 for approx. wattage
  load=$(upsc myups@localhost ups.load)
  echo $(( $load * 10 ))W
}

tekken-rng() {
  curl -s 'https://www.random.org/integers/?num=1&min=1&max=35&col=1&base=10&format=html&rnd=new' | grep 'class="data"' | awk -F '>' '{print $2}'
}

export PATH=$HOME/git_repos/not_mine/gamescope/build/src/:$PATH

fortune -a

# leave commented out?
#export PATH="/home/kevin/apps/miniconda3/bin:$PATH"  # commented out by conda initialize

