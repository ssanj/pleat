[![Build Status](https://travis-ci.org/ssanj/pleat.svg?branch=master)](https://travis-ci.org/ssanj/pleat)

# pleat

## Usage

Run `stack install` to install `pleat` to your local bin directory. Ensure the local bin directory is on your PATH.
Define a [file](https://stackoverflow.com/questions/3058325/what-is-the-difference-between-ps1-and-prompt-command) to update the prompt:

_~/.bash_prompt_: 

```
function prompt_command {
  export PS1=$(~/.local/bin/pleat)
}
export PROMPT_COMMAND=prompt_command
```

and source it from your `bash_profile`:

```
source ~/.bash_prompt
```

which should yield something like:

![Pleat](pleat.png)