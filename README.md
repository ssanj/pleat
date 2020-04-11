# pleat

## Usage


Define a [file](https://stackoverflow.com/questions/3058325/what-is-the-difference-between-ps1-and-prompt-command) to update the prompt:

_~/.bash_prompt_: 

```
function prompt_command {
  export PS1=$(~/bin/pleat)
}
export PROMPT_COMMAND=prompt_command
```

and source it from your `bash_profile`:

```
source ~/.bash_prompt
```