# gh-issues-copy

A simple scala-cli script to help you copy github issues from one repo to the other. This is
NOT [issue transfer](https://docs.github.com/en/issues/tracking-your-work-with-issues/transferring-an-issue-to-another-repository).

First try and see if that works for you before using this!

# requirements

- Things that you can manage with [sdkman.io](https://sdkman.io/):
    - Java 11+ (ideally +) — we use `String.isBlank`
    - [scala-cli](https://scala-cli.virtuslab.org/)
- github [cli](https://cli.github.com/)
    - the script assumes that you already did `gh auth`
        - tested only with
          personal [access token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens)
          setup

# run

```shell
./gh-issues-copy.scala -- lorandszakacs/gh-issues-copy lorandszakacs/test-target 40
```

# semantics

- it only copies open issues. Can be easily changed
- copies comments as well
- :warning: it cannot maintain authorship of issues and comments. The person who did `gh auth` and running the script
  will be the author!
- it will add footer to the issue description with a link to the original issue
- it will add a header to the copied comment with a link to the original issue, and a reference to the person who made
  the comment

## what it does not do

- transfer tags

# testing

The issues on this repo prefixed with `Test: ` will never be closed and are types of issues that this script ought to
handle well.

So I don't have to remember how to invoke script everytime I test it :shrug:

```shell
./test.sh
```
