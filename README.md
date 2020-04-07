# cfvc
 cfvc: CodeForces contests Virtual Clone with single line of command (on Virtual Judge)

## Installation

### Install with Homebrew (macOS, Linux)

[Homebrew](https://brew.sh/) is one of the most famous package managers on macOS. It is also available on Linux, detailed information of which can be found at [Homebrew on Linux](https://docs.brew.sh/Homebrew-on-Linux).

macOS
```shell
brew install linyxus/tools/cfvc
```

Linux
```shell
brew install linyxus/tools/cfvc-linux
```

If you do not want to bother yourself by installing Homebrew on Linux, prebuilt binaries for Linux can also be found on the release page.

### Manual installation
Prerequisites:
- make
- nix
- cabal

First, build the project
```
make build
```

A symlink named result will be generated if the project get built successfully.
Move `cfvc` binary in `{project_root}/result/bin/cfvc` into your path.

## Usage

### Configuration

At the first run of `cfvc`, a template configuration will be written to `{home_dir}/.cfvc/config.yaml`. You can find the exact path of the configuration in the log.

The template configuration file looks like this:
```yaml
# Title of the contest on Vjudge
title: "A Good Contest on CodeForces"
# Id of the CodeForces contest to be cloned, optional
contest: 1328
# Additional problems, optional
problems:
  - "1212a"
# Begin time of the contest. It can either be absolute or relative.
# Absolute: 13:30:00 means the nearest 13:00:00 in the future
# Relative: +1:00:00 means 1 hour later
begin-time: 13:30:00
# Length of the contest
length: 24:0:0
# Description of the contest
desc: ""
# Annoucement of the contest
announcement: ""
# 0: Public
# 1: Protected
# 2: Private
access-level: 2
# Password of the contest if access level is either protected or private
password: "password"
# Show peers
show-peers: true
# VJudge username
auth-username:
# VJudge password
auth-password:
```

The configuration is self-documented and straightforward. A similar example can be found in the project's
`example` dir.

> Note that the `problems` field provides extra problems. In other words, problems that will be added into the VJudge contest consists of all problems of the specified CodeForces contest and those provided in `problems` field.

### Command line

Command line arguments are almost the same with configuration. The existence of configuration can save you from typing the same information again and again. You can check the help of `cfvc` command line options by executing `cfvc --help`:
```
cfvc - CodeForces Virtual Cloner

Usage: cfvc [CONTEST_ID] [-t|--title TITLE] [-b|--begin-time TIME]
            [-l|--length TIME] [-d|--description TEXT] [-a|--announcement TEXT]
            [-u|--username USERNAME] [-p|--password PASSWORD]
  Clone CodeForces contest onto Virtual Judge

Available options:
  -t,--title TITLE         Title of VJudge contest
  -b,--begin-time TIME     Begin time: 18:00:00, +1:30:00 ...
  -l,--length TIME         Contest length: 3:00:00
  -d,--description TEXT    Contest description
  -a,--announcement TEXT   Contest announcement
  -u,--username USERNAME   Virtual Judge username
  -p,--password PASSWORD   Virtual Judge password
  -h,--help                Show this help text
```

> The options provided in CLI will override the corresponding field in configuration. So it is handy to fill all common information in the configuration and override them with CLI options if necessary.

Example:
A simple usage example.
```shell
# The following command will clone Clone Codeforces Round #631 (Div. 1), https://codeforces.com/contest/1329. The option `-t` provides a proper title.
cfvc 1329 -t 'Codeforces Div. 1 #631'
```

Once the contest is successfully created, the URL will be displayed.
```
...
[Info] Creating contest ...
[Info] Done! Contest URL: https://vjudge.net/contest/366773
```

You can also set begin time and length of the contest.
```shell
# The created contest will start 1h later, and last 10 hours.
cfvc 1329 -t 'Another Contest!' -b +1:0:0 -l 10:0:0
```

You can specify begin time in two formats, either absolute or relative.
The absolute way, will start the contest at the nearest time provided. To be specific, consider the following command:
```shell
cfvc 1329 -t 'name' -b 20:00:00
```
If you run the command at 19:59:00, the contest will begin 1 hour later. While if the command is executed at 20:00:01, the contest will begin at 20:00:00 tomorrow.

The relative way, is much more straightforward. You specify the begin time of the contest to be hh:mm:ss later.