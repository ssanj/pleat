# pleat

Tired of wrangling Bash scripts to create my Bash prompt, I turned to the power of Haskell to make this more enjoyable. "Haskell your Bash" whenever possible.

[![Build Status](https://travis-ci.org/ssanj/pleat.svg?branch=master)](https://travis-ci.org/ssanj/pleat) [![Latest Version](https://img.shields.io/github/v/release/ssanj/pleat)](https://github.com/ssanj/pleat/releases/latest)

## Installing

### Mac

Download the [latest release](https://github.com/ssanj/pleat/releases/download/v0.2.0.5/pleat-v0.2.0.5-osx.tar.gz) from the releases page.

### Linux


Download the [latest release](https://github.com/ssanj/pleat/releases/download/v0.2.0.5/pleat-v0.2.0.5-linux.tar.gz) from the releases page.

In addition you can build it from source and install it locally with Stack:

```
stack build --fast && stack install
```

## Configuring

Once you have the `pleat` executable on your path, you need to tell Bash to use it to render its prompt. You can do this via the following scripts or use another technique that works for you.


1. Define the [~/.bash_prompt](https://stackoverflow.com/questions/3058325/what-is-the-difference-between-ps1-and-prompt-command) script that specifies how the prompt should be created:


```
function prompt_command {
  export PS1=$(PATH_TO_pleat)
}
export PROMPT_COMMAND=prompt_command
```

2. Source the above script from your `bash_profile`:

```
source ~/.bash_prompt
```

If all goes according to plan you should see something like:

![Pleat](pleat.png)

## Using

```
pleat - bash prompt

Usage: pleat ([--no-hostname] | [--hostname HOSTNAME]) [--no-path]
             [--max-path-length INT] [--no-git] [--no-timestamp]
             [--prompt PROMPT] [--prompt-separator SEP] [-v|--version]
  writes out a bash prompt with useful information

Available options:
  --no-hostname            don't display hostname
  --hostname HOSTNAME      override hostname
  --no-path                don't display path
  --max-path-length INT    maximum length for the path displayed (default: 50)
  --no-git                 don't display git
  --no-timestamp           don't display timestamp
  --prompt PROMPT          override prompt (default: "> ")
  --prompt-separator SEP   override prompt separator (default: ":")
  -v,--version             Show pleat version
  -h,--help                Show this help text

--no-feature options take precedence over other options
```


## Releasing

- Bump version in package.yaml: X.Y.Z
- make changes
- build
- commit changes
- tag changes to match version: git tag 'vX.Y.Z'
- push commit
- push tags: git push --tags
- update README (this file) with latest version link
- push commit